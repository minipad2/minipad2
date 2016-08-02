unit UxlExtClasses;

interface

uses UxlList, UxlClasses, UxlWinControl;

type
   ISaver = interface
   	procedure Save ();
   end;

   TxlSaveCenter = class (TxlInterfacedObject, ISaver)
   private
      FObservers: TxlInterfaceList;
   protected
   	procedure OnCreate(); virtual;
      procedure OnDestroy(); virtual;
   public
      constructor Create (); virtual;
      destructor Destroy (); override;
      procedure AddObserver (o_observer: ISaver);
		procedure RemoveObserver(o_observer: ISaver);
      procedure Save (); virtual;
   end;

type
   IOptionObserver = Interface
   	procedure OptionChanged ();
   end;

   TxlOption = class
   private
      FObservers: TxlInterfaceList;
   protected
   	procedure OnCreate (); virtual;
      procedure OnDestroy (); virtual;
      procedure DoLoad (); virtual; abstract;
      procedure DoSave (); virtual; abstract;
      function DoSetOptions (WndParent: TxlWinControl): boolean; virtual; abstract;
      procedure NotifyObservers ();
   public
      constructor Create ();
      destructor Destroy (); override;
      procedure SetOptions (WndParent: TxlWinControl);
      procedure AddObserver (o_obj: IOptionObserver);
      procedure RemoveObserver (o_obj: IOptionObserver);
   end;

type
   IMemorizer = Interface
   	procedure SaveMemory ();
      procedure RestoreMemory ();
   end;

   TxlMemory = class (TxlInterfacedObject, ISaver)
   private
      FObservers: TxlInterfaceList;
   protected
   	procedure OnCreate (); virtual;
      procedure OnDestroy (); virtual;
   	procedure DoLoad (); virtual; abstract;
      procedure DoSave (); virtual; abstract;
   public
      constructor Create ();
      destructor Destroy (); override;
      procedure Save ();
      procedure AddObserver (o_obj: IMemorizer);
      procedure RemoveObserver (o_obj: IMemorizer);
   end;

type
	ILangObserver = Interface
   	procedure LanguageChanged();
   end;

   TxlLanguage = class (TxlInterfacedObject)
   private
      FItemList: TxlStrList;
      FObservers: TxlInterfaceList;
      FLanguage: widestring;     // 当前语言
      FInnerLanguage: widestring;    // 字符串嵌入代码之中的语言
      function LoadResourceString (id: integer): widestring;
   protected
      procedure FillItems (var lang: widestring; itemlist: TxlStrList); virtual; abstract;
   public
      constructor Create ();
      destructor Destroy (); override;

      procedure SetLanguage (value: widestring = '');
      procedure SetInnerLanguage (const value: widestring);
      property Language: widestring read FLanguage write SetLanguage;

      // 优先搜索 FItemList，其次 s_defvalue，再次 LoadResourceString；
      // 允许某些语言 fillitems，某些直接写在代码（s_defvalue）中，某些写在资源文件里
      function GetItem (i_index: integer; const s_defvalue: widestring = ''): widestring;

      procedure AddObserver (o_observer: ILangObserver);
		procedure RemoveObserver(o_observer: ILangObserver);
   end;

type
   IEventObserver = interface
   	procedure EventNotify (event, wparam, lparam: integer);
   end;

   TxlEventCenter = class (TxlInterfacedObject)
   private
   	FObservers: TxlInterfaceList;
//      FMessages: TxlStrList;
		FMessage: widestring;
   public
      constructor Create ();
      destructor Destroy (); override;
      
      procedure AddObserver (o_observer: IEventObserver);
		procedure RemoveObserver(o_observer: IEventObserver);
      procedure EventNotify (event: integer; wparam: integer = 0; lparam: integer = 0);

      property Message: widestring read FMessage write FMessage;
//      function AddMessage (const s_msg: widestring): integer;    // returns message id
//		function GetMessage (id: integer): widestring;
   end;

type
	IListProvider = interface
   	procedure FillList (id: integer; o_list: TxlStrList);
		function GetSubItem (id: integer; var i_index: integer; var s_result: widestring; i_maxchar: integer = -1): boolean;
   end;

   TxlListCenter = class
   private
   	FProviders: TxlInterfaceList;
   public
   	constructor Create ();
      destructor Destroy (); override;
      procedure AddProvider (o_provider: IListProvider);
      procedure RemoveProvider (o_provider: IListProvider);
      procedure FillList (id: integer; o_list: TxlStrList);
		function GetSubItem (id: integer; var i_index: integer; var s_result: widestring; i_maxchar: integer = -1): boolean;
   end;

type
   ICommandSender = interface
      procedure SetItemChecked (id: word; value: boolean);
      function GetItemChecked (id: word): boolean;

      procedure SetItemEnabled (id: word; value: boolean);
      function GetItemEnabled (id: word): boolean;

      procedure SetItemVisible (id: word; value: boolean);
      function GetItemVisible (id: word): boolean;

      procedure SetItemText (id: word; const value: widestring);
      function GetItemText (id: word): widestring;

      procedure SetCommandProc (value: TCommandEvent);
      function ItemExists (id: word): boolean;
      procedure GetItemList (o_list: TxlIntList);
   end;

	ICommandExecutor = interface
   	function CheckCommand (opr: word): boolean;
      procedure ExecuteCommand (opr: word);
   end;

   TxlCommandCenter = class (TxlInterfacedObject)
   private
   	FSenderList: TxlInterfaceList;
      FExecutorList: TxlInterfaceList;
		FOnCommand: TCommandEvent;
   protected
      procedure SetItemChecked (id: word; value: boolean);
      procedure SetItemEnabled (id: word; value: boolean);
      function GetItemChecked (id: word): boolean;
      function GetItemEnabled (id: word): boolean;
      procedure SetItemVisible (id: word; value: boolean);
      function GetItemVisible (id: word): boolean;
      procedure SetItemText (id: word; const value: widestring);
      function GetItemText (id: word): widestring;
      procedure SetCommandProc (value: TCommandEvent);
   public
   	constructor Create ();
      destructor Destroy (); override;
   	procedure AddSender (obj: ICommandSender);
      procedure RemoveSender (obj: ICommandSender);
      procedure AddExecutor (obj: ICommandExecutor);
      procedure RemoveExecutor (obj: ICommandExecutor);

      function ItemExists (id: word): boolean;
      procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
      procedure CheckCommands (); virtual;
      procedure SwitchCheck (id: word);

      property ItemText[id: word]: widestring read GetItemText write SetItemText;
      property ItemEnabled[id: word]: boolean read GetItemEnabled write SetItemEnabled;
      property ItemChecked[id: word]: boolean read GetItemChecked write SetItemChecked;
      property ItemVisible[id: word]: boolean read GetItemVisible write SetItemVisible;
      property OnCommand: TCommandEvent read FOnCommand write SetCommandProc;
   end;

implementation

uses Windows, UxlFunctions, UxlStrUtils;

constructor TxlSaveCenter.Create ();
begin
	FObservers := TxlInterfaceList.Create();
   OnCreate;
end;

destructor TxlSaveCenter.Destroy ();
begin
	OnDestroy;
	FObservers.free;
   inherited;
end;

procedure TxlSaveCenter.OnCreate();
begin
end;

procedure TxlSaveCenter.OnDestroy();
begin
end;

procedure TxlSaveCenter.AddObserver (o_observer: ISaver);
begin
	FObservers.Add(o_observer);
end;

procedure TxlSaveCenter.RemoveObserver(o_observer: ISaver);
begin
	FObservers.Remove (o_observer);
end;

procedure TxlSaveCenter.Save ();
var i: integer;
begin
	i := FObservers.Low;
   while i <= FObservers.High do
   begin
   	if FObservers[i] <> nil then
   		ISaver (FObservers[i]).Save;
      inc (i);
   end;
end;

//-------------------------

constructor TxlOption.Create ();
begin
   FObservers := TxlInterfaceList.Create();
   OnCreate;
	DoLoad;
end;

destructor TxlOption.Destroy ();
begin
	FObservers.Free;
   OnDestroy;
   inherited;
end;

procedure TxlOption.OnCreate ();
begin
end;

procedure TxlOption.OnDestroy ();
begin
end;

procedure TxlOption.AddObserver (o_obj: IOptionObserver);
begin
	FObservers.Add (o_obj);
  	o_obj.OptionChanged ();
end;

procedure TxlOption.RemoveObserver (o_obj: IOptionObserver);
begin
	FObservers.Remove (o_obj);
end;

procedure TxlOption.SetOptions (WndParent: TxlWinControl);
begin
	if DoSetOptions (WndParent) then
   begin
      DoSave;
      NotifyObservers;
   end;
end;

procedure TxlOption.NotifyObservers ();
var i: integer;
begin
   i := FObservers.Low;
   while i <= FObservers.High do  // SetOption 的过程中可能有些对象 RemoveObserver 导致 High 变化。因此用 for 循环不妥
   begin
      if FObservers[i] <> nil then
         IOptionObserver(FObservers[i]).OptionChanged ();
      inc (i);
   end;
end;

//----------------------------

constructor TxlMemory.Create ();
begin
   FObservers := TxlInterfaceList.Create;
   OnCreate;
	DoLoad;
end;

destructor TxlMemory.Destroy ();
begin
	OnDestroy;
	FObservers.Free;
   inherited;
end;

procedure TxlMemory.AddObserver (o_obj: IMemorizer);
begin
	FObservers.Add (o_obj);
  	o_obj.RestoreMemory ();
end;

procedure TxlMemory.RemoveObserver(o_obj: IMemorizer);
begin
	FObservers.Remove (o_obj);
end;

procedure TxlMemory.Save ();
var i: integer;
begin
	i := FObservers.Low;
   while i <= FObservers.High do
   begin
   	if FObservers[i] <> nil then
   		IMemorizer(FObservers[i]).SaveMemory ();
      inc (i);
   end;
   DoSave;
end;

procedure TxlMemory.OnCreate ();
begin
end;

procedure TxlMemory.OnDestroy ();
begin
end;

//------------------

constructor TxlLanguage.Create ();
begin
	FItemList := TxlStrList.create;
   FObservers := TxlInterfaceList.Create();
   FLanguage := '';
end;

destructor TxlLanguage.Destroy ();
begin
	FItemList.free;
   FObservers.Free;
   inherited;
end;

procedure TxlLanguage.SetLanguage (value: widestring = '');
var i: integer;
begin
	if value = '' then
   	value := IfThen (IsLocaleChinese(), 'Chinese', 'English');

   if value <> FLanguage then
   begin
   	FLanguage := value;
      FillItems (FLanguage, FItemList);
      i := FObservers.Low;
      while i <= FObservers.High do
      begin
      	if FObservers[i] <> nil then
         	ILangObserver(FObservers[i]).LanguageChanged ;
         inc (i);
      end;
   end;
end;

procedure TxlLanguage.SetInnerLanguage(const value: widestring);
begin
	FInnerLanguage := value;
end;

procedure TxlLanguage.AddObserver(o_observer: ILangObserver);
begin
	FObservers.Add(o_observer);
   if FLanguage <> '' then
	   o_observer.LanguageChanged;
end;

procedure TxlLanguage.RemoveObserver(o_observer: ILangObserver);
begin
	FObservers.Remove (o_observer);
end;

function TxlLanguage.GetItem (i_index: integer; const s_defvalue: widestring = ''): widestring;
begin
	result := FItemList.ItemsByIndex [i_index];
   if result = '' then
   begin
   	if IsSameStr(FLanguage, FInnerLanguage) and (s_defvalue <> '') then
      	result := s_defvalue
      else
      	result := LoadResourceString (i_index);
   end;
   result := ReplaceStrings (result, ['\t', '\n'], [#9, #13#10]);
end;

var FResStringBuffer: array[0..2000] of ansichar;

function TxlLanguage.LoadResourceString (id: integer): widestring;
var s: pansichar;
begin
	LoadStringA (system.MainInstance, id, FResStringBuffer, 2000);
   s := FResSTringBuffer;
   result := AnsiToUnicode (s);
end;

//------------------------

constructor TxlEventCenter.Create ();
begin
	FObservers := TxlInterfaceList.Create();
end;

destructor TxlEventCenter.Destroy ();
begin
	FObservers.free;
   inherited;
end;

procedure TxlEventCenter.AddObserver (o_observer: IEventObserver);
begin
	FObservers.Add(o_observer);
end;

procedure TxlEventCenter.RemoveObserver(o_observer: IEventObserver);
begin
	FObservers.Remove (o_observer);
end;

procedure TxlEventCenter.EventNotify (event: integer; wparam: integer = 0; lparam: integer = 0);
var i: integer;
begin
	i :=  FObservers.Low;
   while i <= FObservers.High do
   begin
		if FObservers[i] <> nil then
   		IEventObserver(FObservers[i]).EventNotify (event, wparam, lparam);
   	inc (i);
   end;
end;

//---------------------

constructor TxlListCenter.Create ();
begin
	FProviders := TxlInterfaceList.Create();
end;

destructor TxlListCenter.Destroy ();
begin
	FProviders.Free;
   inherited;
end;

procedure TxlListCenter.AddProvider (o_provider: IListProvider);
begin
	FProviders.Add (o_provider);
end;

procedure TxlListCenter.RemoveProvider (o_provider: IListProvider);
begin
	FProviders.Remove (o_provider);
end;

procedure TxlListCenter.FillList (id: integer; o_list: TxlStrList);
var i: integer;
begin
	i := FProviders.Low;
   while i <= FProviders.High do
   begin
   	if FProviders[i] <> nil then
   		IListProvider(FProviders[i]).FillList (id, o_list);
      inc (i);
   end;
end;

function TxlListCenter.GetSubItem (id: integer; var i_index: integer; var s_result: widestring; i_maxchar: integer = -1): boolean;
var i: integer;
begin
	result := false;
   for i := FProviders.Low to FProviders.High do
   	if (FProviders[i] <> nil) then
      begin
      	result := IListProvider(FProviders[i]).GetSubItem (id, i_index, s_result, i_maxchar);
         if result then exit;
      end;
end;

//---------------------

constructor TxlCommandCenter.Create ();
begin
	FSenderList := TxlInterfaceList.Create();
   FExecutorList := TxlInterfaceList.Create();
   FOnCommand := ExecuteCommand;
end;

destructor TxlCommandCenter.Destroy ();
begin
	FSenderList.Free;
   FExecutorList.Free;
   inherited;
end;

procedure TxlCommandCenter.AddSender (obj: ICommandSender);
begin
	FSenderList.Add(obj);
   obj.SetCommandProc (FOnCommand);
end;

procedure TxlCommandCenter.RemoveSender (obj: ICommandSender);
begin
	FSenderList.Remove (obj);
end;

procedure TxlCommandCenter.AddExecutor (obj: ICommandExecutor);
begin
	FExecutorList.Add(obj);
end;

procedure TxlCommandCenter.RemoveExecutor (obj: ICommandExecutor);
begin
	FExecutorList.Remove(obj);
end;

procedure TxlCommandCenter.ExecuteCommand (opr: word);
var i: integer;
begin
   i := FExecutorList.Low;
   while i <= FExecutorList.High do
   begin
		if FExecutorList[i] <> nil then
	   	ICommandExecutor(FExecutorList[i]).ExecuteCommand(opr);
      inc (i);
   end;
end;

function TxlCommandCenter.CheckCommand (opr: word): boolean;
var i: integer;
begin
   i := FExecutorList.Low;
   while i <= FExecutorList.High do
   begin
   	if (FExecutorList[i] <> nil) and (not ICommandExecutor(FExecutorList[i]).CheckCommand (opr)) then
      begin
   		if ItemEnabled[opr] then
         	ItemEnabled[opr] := false;
         result := false;
         exit;
      end;
      inc (i);
   end;
   if not ItemEnabled[opr] then   // 性能优化，防止闪烁
   	ItemEnabled[opr] := true;
   result := true;
end;

procedure TxlCommandCenter.CheckCommands ();
var i, j: integer;
	o_list: TxlIntList;
begin
	o_list := TxlIntList.Create();
   i := FSenderList.Low;
   while i <= FSenderList.High do
   begin
   	if FSenderList[i] <> nil then
      begin
         ICommandSender(FSenderList[i]).GetItemList(o_list);
         for j := o_list.Low to o_list.High do
            CheckCommand (o_list[j]);
      end;
      inc (i);
   end;
   o_list.free;
end;

procedure TxlCommandCenter.SetItemChecked (id: word; value: boolean);
var i: integer;
begin
	for i := FSenderList.Low to FSenderList.High do
   	ICommandSender(FSenderList[i]).SetItemChecked (id, value);
end;

function TxlCommandCenter.GetItemChecked (id: word): boolean;
var i: integer;
begin
	result := false;
	for i := FSenderList.Low to FSenderList.High do
   	if ICommandSender(FSenderList[i]).itemExists (id) then
      begin
      	result := ICommandSender(FSenderList[i]).GetItemchecked (id);
         exit;
      end;
end;

procedure TxlCommandCenter.SwitchCheck (id: word);
var i: integer;
	ic: ICommandSender;
begin
	for i := FSenderList.Low to FSenderList.High do
   begin
   	ic := ICommandSender(FSenderList[i]);
   	if ic.itemExists (id) then
			ic.SetItemChecked ( id, not ic.GetItemChecked(id) );
   end;
end;

procedure TxlCommandCenter.SetItemEnabled (id: word; value: boolean);
var i: integer;
begin
	for i := FSenderList.Low to FSenderList.High do
   	ICommandSender(FSenderList[i]).SetItemEnabled (id, value);
end;

function TxlCommandCenter.GetItemEnabled (id: word): boolean;
var i: integer;
begin
	result := false;
	for i := FSenderList.Low to FSenderList.High do
   	if ICommandSender(FSenderList[i]).itemExists (id) then
      begin
      	result := ICommandSender(FSenderList[i]).GetItemEnabled (id);
         exit;
      end;
end;

procedure TxlCommandCenter.SetItemVisible (id: word; value: boolean);
var i: integer;
begin
	for i := FSenderList.Low to FSenderList.High do
   	ICommandSender(FSenderList[i]).SetItemVisible (id, value);
end;

function TxlCommandCenter.GetItemVisible (id: word): boolean;
var i: integer;
begin
	result := false;
	for i := FSenderList.Low to FSenderList.High do
   	if ICommandSender(FSenderList[i]).itemExists (id) then
      begin
      	result := ICommandSender(FSenderList[i]).GetItemVisible (id);
         exit;
      end;
end;

procedure TxlCommandCenter.SetItemText (id: word; const value: widestring);
var i: integer;
begin
	for i := FSenderList.Low to FSenderList.High do
   	ICommandSender(FSenderList[i]).SetItemText (id, value);
end;

function TxlCommandCenter.GetItemText (id: word): widestring;
var i: integer;
begin
	result := '';
	for i := FSenderList.Low to FSenderList.High do
   	if ICommandSender(FSenderList[i]).itemExists (id) then
      begin
      	result := ICommandSender(FSenderList[i]).GetItemText (id);
         exit;
      end;
end;

function TxlCommandCenter.ItemExists (id: word): boolean;
var i: integer;
begin
	result := false;
	for i := FSenderList.Low to FSenderList.High do
   	if ICommandSender(FSenderList[i]).itemExists (id) then
      begin
      	result := true;
         exit;
      end;
end;

procedure TxlCommandCenter.SetCommandProc (value: TCommandEvent);
var i: integer;
begin
	FOnCommand := value;
	for i := FSenderList.Low to FSenderList.High do
   	ICommandSender(FSenderList[i]).SetCommandProc (value);
end;

end.




