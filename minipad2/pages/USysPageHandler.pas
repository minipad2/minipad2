unit USysPageHandler;

interface

uses UxlClasses, UxlExtClasses, UPageSuper, UTypeDef, UxlList, UxlWinClasses, UPasteHandler;

type
   TSysPageHandlerSuper = class (TxlInterfacedObject, ICommandExecutor, ILangObserver)
   protected
   	FPage: TPageSuper;
      FParent: TPageSuper;
   protected
   	function PageType (): TPageType; virtual; abstract;
      function NameResource (): integer; virtual; abstract;
      function MenuItem_Manage (): integer; virtual; abstract;
   public
   	constructor Create (AParent: TPageSuper); virtual;
      destructor Destroy (); override;

      function CheckCommand (opr: word): boolean; virtual;
      procedure ExecuteCommand (opr: word); virtual;
      procedure LanguageChanged ();
      property Page: TPageSuper read FPage;
   end;

   TSysListPageHandler = class (TSysPageHandlerSuper, IListProvider)
   private
   protected
   	function MenuItem_List (): integer; virtual; abstract;
   	procedure SubItemCalled (index: integer); virtual; abstract;
   public
   	constructor Create (AParent: TPageSuper); override;
      destructor Destroy (); override;

      procedure ExecuteCommand (opr: word); override;
      procedure FillList (id: integer; o_list: TxlStrList);
      function GetSubItem (id: integer; var i_index: integer; var s_result: widestring; i_maxchar: integer = -1): boolean;
	end;

   TSysPageContainerHandler = class (TSysListPageHandler)
   protected
   	procedure SubItemCalled (index: integer); override;
   end;

   THandlerWithSubHotKey = class (TSysListPageHandler, IOptionObserver, IHotkeyOwner)    // FastLink, Clipboard, Template
   private
   	FPopupHandler: TPopupHandler;
   protected
		procedure AddHotkeyAndSubKey (hk: THotKey; enablesub: boolean);
   	procedure SubItemCalled (index: integer); override;
      function PopupAtCaretPos (): boolean; virtual; 
      procedure ExecuteItem (id: integer); virtual; abstract;
   public
   	constructor Create (AParent: TPageSuper); override;
      destructor Destroy (); override;
      procedure OptionChanged (); virtual; abstract;
      procedure OnHotkey (id: integer; hk: THotkey); virtual;
      property PopupHandler: TPopupHandler write FPopupHandler;
   end;

   THandlerWithPaste = class (THandlerWithSubHotKey)
   protected
   	FPasteHandler: TPasteHandler;
      function PopupAtCaretPos (): boolean; override;
	public
		function CheckCommand (opr: word): boolean; override;
      property PasteHandler: TPasteHandler write FPasteHandler;
	end;

implementation

uses Windows, UPageStore, UDeleteHandler, UGlobalObj, ULangManager, UPageFactory, UxlMath, UOptionManager, UxlFunctions, UxlWindow,
	Resource;

constructor TSysPageHandlerSuper.Create (AParent: TPageSuper);
var id: integer;
begin
   FParent := AParent;
	id := PageStore.NewPage (FParent.id, PageType);
   FPage := PageStore[id];

   CommandMan.AddExecutor (self);
   LangMan.AddObserver (self);
end;

destructor TSysPageHandlerSuper.Destroy ();
begin
	LangMan.RemoveObserver (self);
   CommandMan.RemoveExecutor (self);
   inherited;
end;

procedure TSysPageHandlerSuper.LanguageChanged ();
begin
   FPage.Name := LangMan.GetItem(NameResource);
end;

function TSysPageHandlerSuper.CheckCommand (opr: word): boolean;
begin
   result := true;
end;

procedure TSysPageHandlerSuper.ExecuteCommand (opr: word);
begin
	if opr = MenuItem_Manage then
   begin
      FParent.Childs.AddChild (FPage.id);
      PageCenter.ActivePage := FPage;
   end;
end;

//-----------------------

constructor TSysListPageHandler.Create (AParent: TPageSuper);
begin
	inherited Create (AParent);
   ListCenter.AddProvider (self);
end;

destructor TSysListPageHandler.Destroy ();
begin
	ListCenter.RemoveProvider (self);
	inherited;
end;

procedure TSysListPageHandler.ExecuteCommand (opr: word);
begin
	if InRange (opr, MenuItem_List + 1, MenuItem_List + 499) then
		SubItemCalled (opr - MenuItem_List - 1)
   else
      inherited ExecuteCommand (opr);
end;

procedure TSysListPageHandler.FillList (id: integer; o_list: TxlStrList);
	function AddSortNo (const p: TPageSuper; i: integer): widestring;
   begin
      result := p.GetColText (sr_abbrev);
      if result = '' then
      begin
         result := IntToStr(i);
         if i <= 9 then
            result := result + '.'
         else
            result := '.' + result;
      end
      else
         result := result + '.';
      result := result + #9 + p.name;
	end;
var i: integer;
	o_list2: TxlIntList;
begin
	if id <> MenuItem_List then exit;
   o_list2 := TxlIntList.Create;
   FPage.GetChildList (o_list2);
   o_list.clear;
   for i := o_list2.Low to o_list2.High do
		o_list.Add ( AddSortNo(PageStore[o_list2[i]], i + 1));
   o_list2.free;
end;

function TSysListPageHandler.GetSubItem (id: integer; var i_index: integer; var s_result: widestring; i_maxchar: integer = -1): boolean;
var i_pageid: integer;
begin
	result := false;
   if FPage.Childs = nil then exit;
   i_index := id - MenuItem_List - 1;
	result := InRange (i_index, 0, FPage.Childs.Count - 1);
   if result then
   begin
   	i_pageid := FPage.Childs.ChildId (i_index);
   	s_result := PageStore[i_pageid].GetColText (sr_Abstract);
   end;
end;

//------------------

procedure TSysPageContainerHandler.SubItemCalled (index: integer);
var o_list: TxlIntList;
begin
   o_list := TxlIntList.Create;
   FPage.GetChildList (o_list);
   PageCenter.ActivePage := PageStore[o_list[index]];
   o_list.free;
end;

//---------------------

constructor THandlerWithSubHotKey.Create (AParent: TPageSuper);
begin
	inherited Create (AParent);
   OptionMan.AddObserver (self);
end;

destructor THandlerWithSubHotKey.Destroy ();
begin
	OptionMan.RemoveObserver (self);
   HotkeyCenter.RemoveOwner (self);
   inherited;
end;

procedure THandlerWithSubHotKey.SubItemCalled (index: integer);   // index: 1 .. 9
var o_list: TxlIntList;
	i, n: integer;
begin
   o_list := TxlIntList.Create;
   FPage.Childs.GetChildList (o_list);
   if index <= o_list.High then
   begin
      if KeyPressed (VK_SHIFT) and (not (KeyPressed(VK_CONTROL) or KeyPressed(VK_MENU))) then
      begin
         for i := index downto 0 do
         begin
         	n := o_list[i];
            ExecuteItem (n);
         end;
      end
      else
         ExecuteItem (o_list[index]);
   end;
   o_list.free;
end;

procedure THandlerWithSubHotKey.OnHotkey (id: integer; hk: THotkey);
begin
	case id of
      0:
      	FPopupHandler.PopupMenu (MenuItem_List, PopupAtCaretPos);
      1..9:
      	begin
         	ReleaseHotkey (hk);     // 对于粘帖非常重要！！！否则alt+n式的粘帖会失败！！！
	      	SubItemCalled (id - 1);
         end;
   end;
end;

procedure THandlerWithSubHotKey.AddHotkeyAndSubKey (hk: THotKey; enablesub: boolean);
var i, md: byte;
begin
  	HotkeyCenter.AddHotKey (self, 0, hk);
   md := HiByte (hk);
   if (md <> 0) and enablesub then
   begin
      for i := 1 to 9 do
         HotkeyCenter.AddHotKey (self, i, MakeWord(i + 48, md));
   end
   else
   begin
      for i := 1 to 9 do
         HotkeyCenter.RemoveHotKey (self, i);
   end;
end;

function THandlerWithSubHotKey.PopupAtCaretPos (): boolean;
begin
	result := false;
end;

//------------------------

function THandlerWithPaste.PopupAtCaretPos (): boolean;
begin
	result := true;
end;

function THandlerWithPaste.CheckCommand (opr: word): boolean;
begin
	if opr = MenuItem_List then
   begin
      if GetForeGroundWindow = MainWinHandle then
         result := PageCenter.ActivePage.PageControl = pcEdit
      else
         result := true;
	end
   else
      result := true;
end;

end.
