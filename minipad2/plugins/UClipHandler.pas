unit UClipHandler;

interface

uses Windows, UPageSuper, USysPageHandler, UTypeDef, UxlClasses, UxlExtClasses, UxlWinClasses, UxlWindow, UxlList, UPasteHandler,
   UxlWinControl, UCommonClasses;

type
   TClipHandler = class (THandlerWithPaste, IClipObserver)
   private
		FOptions: TClipOptions;
      procedure f_ClipboardChanged ();
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
      procedure ExecuteItem (id: integer); override;
   public
   	constructor Create (AParent: TPageSuper; AWindow: TxlWindow);

      procedure OptionChanged (); override;
      function CheckCommand (opr: word): boolean; override;
      procedure ExecuteCommand (opr: word); override;
      procedure ClipNotify (const value: widestring);
   end;

implementation

uses Messages, UGlobalObj, UOptionManager, UPageStore, ULangManager, UPageFactory, UxlCommDlgs, UxlFunctions, UxlStrUtils, UClipPage,
	Resource;

function TClipHandler.PageType (): TPageType;
begin
	result := ptClip;
end;

function TClipHandler.NameResource (): integer;
begin
	result := sr_Clipboard;
end;

function TClipHandler.MenuItem_Manage (): integer;
begin
	result := m_Clipboard;
end;

function TClipHandler.MenuItem_List (): integer;
begin
	result := m_InsertClipText;
end;

function TClipHandler.CheckCommand (opr: word): boolean;
begin
	case opr of
   	m_clearclipboard:
      	result := FPage.Childs.Count > 0
      else
		   result := inherited CheckCommand (opr);
   end;
end;

procedure TClipHandler.ExecuteCommand (opr: word);
var  i: integer;
   o_list: TxlIntList;
   b: boolean;
begin
	case opr of
   	m_watchclipboard:
      	begin
            b := not CommandMan.ItemChecked[m_watchclipboard];
            CommandMan.ItemChecked[m_watchclipboard] := b;
            if b then
               ClipWatcher.AddObserver (self)
            else
               ClipWatcher.RemoveObserver (self);
            MemoryMan.WatchClipboard := b;
         end;
      m_clearclipboard:
      	begin
				if ShowMessage (LangMan.GetItem (sr_clearclipboardprompt), mtQuestion,
      			LangMan.GetItem(sr_prompt)) = mrCancel then exit;
            TClipPage(FPage).Clear;
            f_ClipboardChanged;
         end;
      else
			inherited ExecuteCommand (opr);
   end;
end;

procedure TClipHandler.ExecuteItem (id: integer);
var firstid: integer;
	s: widestring;
begin
	s := PageStore[id].GetColText (sr_Text);
   FPasteHandler.Paste (s, false);

   if FOptions.NewPasteToTop and (FPage.Childs.Count > 1) then
   begin
      firstid := FPage.Childs.ChildId(0);
      if firstid <> id then
      begin
      	FPage.Childs.RemoveChild (id);
         FPage.Childs.AddChild (id, firstid);
      end;
   end;
end;

procedure TClipHandler.OptionChanged ();
begin
   FOptions := OptionMan.Options.ClipOptions;
	with OptionMan.Options do
   	AddHotkeyAndSubKey (Clipboardhotkey, EnableClipItemHotkey);
end;

constructor TClipHandler.Create (AParent: TPageSuper; AWindow: TxlWindow);
begin
	inherited Create (AParent);

	if MemoryMan.WatchClipboard then
   	ExecuteCommand (m_watchclipboard);
   f_ClipboardChanged;
end;

procedure TClipHandler.f_ClipboardChanged ();
begin
	CommandMan.CheckCommand (m_clearclipboard);
end;

procedure TClipHandler.ClipNotify (const value: widestring);      // 总是插入在最前端
	function FirstItemId (): integer;
   begin
      result := FPage.Childs.ChildId (0);
   end;
   function LastItemId (): integer;
   begin
   	result := FPage.Childs.ChildId (FPage.Childs.Count - 1);
   end;

var id, i, n, i_pos: integer;
   o_list: TxlIntList;
   o_page: TClipPage;
begin
   o_page := TClipPage (FPage);
   if Length(value) > (FOptions.MaxItemByte div 2) then exit;
   if (not o_page.Childs.IsEmpty) then
   begin
      if FOptions.FilterType = cftFilterNeighboring then
      begin
         if PageStore[FirstItemId].Text = value then exit;
      end
      else if FOptions.FilterType = cftFilterAll then
         for i := 0 to o_page.Childs.Count - 1 do
         begin
            id := o_page.Childs.ChildId(i);
            if PageStore[id].Text = value then exit;
         end;
   end;

   o_list := TxlIntList.Create;

   // 移去固定项
   n := o_page.Childs.Count;
   for i := n - 1 downto 0 do
      if o_page.IsFixed (i) then
      begin
         o_list.AddByIndex (o_page.Childs.ChildId(i), i);
         o_page.Childs.RemoveChildByPos (i);
      end;

   // 如果尚有多余位置的话，创建新剪贴板项目
   if o_list.Count < FOptions.MaxClipNum then
   begin
      id := PageStore.NewPage (o_page.id, ptClipItem);
      PageStore[id].Name := LeftStr (value, FOptions.MenuWidth);
      PageStore[id].TExt := value;
      o_page.Childs.AddChildByPos (id, 0);
   end;

   // 移去多余项
   n := o_page.Childs.Count;
   if (n > 0) and (n + o_list.Count > FOptions.MaxClipNum) then
      for i := n - 1 downto FOptions.MaxClipNum - o_list.Count do
         o_page.Delete (i);

   // 恢复固定项在原来位置
   for i := o_list.High downto o_list.Low do
      o_page.Childs.AddChildByPos (o_list.Indexes[i], o_list[i]);

   o_list.free;
   f_ClipboardChanged;
end;

end.
