unit UxlMenu;

interface

uses Windows, Messages, UxlWindow, UxlClasses, UxlFunctions, UxlList, UxlStrUtils, UxlWinControl, UxlImageList, UxlExtClasses, CommCtrl;

type
   TMenuItem = record
      id: word;
      text: widestring;
      data: ulong;
      popup: boolean;
      checked: boolean;
      enabled: boolean;
   end;
   PMenuItem = ^TMenuItem;

   TPopupDirection = (pdLeftTop, pdRightTop, pdRightBottom, pdLeftBottom);

   TxlToolBar = class;
   TxlAccelTable = class;

   TxlMenu = class (TxlInterfacedObject, ICommandSender)
   private
      Ftoolbar: TxlToolBar;
      FAccelTable: TxlAccelTable;
      FIDList: TxlIntList;
      FOnCommand: TCommandEvent;
      FNeedDestroyMenu: boolean;

      procedure f_SetItem (i_id: word; const o_item: TMenuItem);
      function f_GetItem (i_id: word): TMenuItem;

      procedure SetItemChecked (id: word; value: boolean);
      procedure SetItemEnabled (id: word; value: boolean);
      function GetItemChecked (id: word): boolean;
      function GetItemEnabled (id: word): boolean;
      procedure SetItemVisible (id: word; value: boolean);
      function GetItemVisible (id: word): boolean;
      procedure SetItemText (id: word; const value: widestring);
      function GetItemText (id: word): widestring;
      procedure SetCommandProc (value: TCommandEvent);
   protected
      Fhandle: HMENU;
      FWndParent: TxlWindow;
   	procedure OnInitialize (); virtual;
   	procedure OnCreate (); virtual;
      procedure OnDestroy (); virtual;
      procedure SetTemplate (id_template: word);

      property ToolBar: TxlToolBar read FToolBar write FToolBar;
      property AccelTable: TxlAccelTable read FAccelTable write FAccelTable;
   public
      constructor Create (Wndparent: TxlWindow; h_root: HMenu = 0);
      destructor Destroy (); override;
      procedure ProcessCommand (ItemID: WORD);
      procedure InitMenuPopup (h_menu: HMenu); virtual;

      function AddPopup (h_parent: HMENU; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU;
      function InsertPopup (h_parent: HMENU; i_pos: integer; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU;
      procedure ClearPopup (h_popup: HMenu);
      function AddItem (h_parent: HMENU; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU; overload;
      function InsertItem (h_parent: HMENU; i_pos: integer; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU; overload;
      function AddItem (h_parent: HMENU; const o_item: TMenuItem): HMENU; overload;
      function InsertItem (h_parent: HMENU; i_pos: integer; const o_item: TMenuItem): HMENU; overload;
      procedure AddSeparator (h_parent: HMENU);
      procedure DeleteItem (id_item: word);

      procedure Popup (posid: integer = 0; x: integer = -1; y: integer = -1; pdDir: TPopupDirection = pdLeftTop; b_bypos: boolean = false);
      function GetMenuHandle (id: word): HMENU;
      procedure GetItemList (h_parent: HMENU; var o_list: TxlIntList); overload; // 如 h_parent = Fhandle，则得到所有id列表；否则得到下属菜单的id列表
      procedure GetItemList (o_list: TxlIntList);  overload;
      function GetItemCount (h_parent: HMENU = 0): integer;

      property Handle: HMENU read Fhandle;
      property OnCommand: TCommandEvent read FOnCommand write SetCommandProc;

      property Items[id: word]: TMenuItem read f_GetItem write f_SetItem; default;
      property ItemText[id: word]: widestring read GetItemText write SetItemText;
      property ItemEnabled[id: word]: boolean read GetItemEnabled write SetItemEnabled;
      property ItemChecked[id: word]: boolean read GetItemChecked write SetItemChecked;
      procedure SwitchCheck (id: word);
      function ItemExists (id: Word): boolean;
   end;

   TxlToolBar = class (TxlControl, ICommandSender)
   private
      FMenu: TxlMenu;
      FImages: TxlImageList;
      FDisabledImages: TxlImageList;
      FSepCount: integer;
      FSepImage: integer;
      FOnContextMenu: TNotifyEvent;
      FOnCommand: TCommandEvent;
      FIDList: TxlIntList;

      procedure f_AddButton (id_item: integer; const s_text: widestring = '');

      procedure SetItemEnabled (id: word; value: boolean);
      procedure SetItemChecked (id: word; value: boolean);
      function GetItemEnabled (id: word): boolean;
      function GetItemChecked (id: word): boolean;
      procedure SetItemText (id: word; const value: widestring);
      function GetItemText (id: word): widestring;
      procedure SetItemVisible (id: word; value: boolean);
      function GetItemVisible (id: word): boolean;
      procedure SetCommandProc (value: TCommandEvent);
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
   	procedure OnCreate (); virtual;
      procedure OnDestroy (); virtual;
      procedure SetPos (const value: TPos); override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
   public
      procedure AssociateMenu (o_menu: TxlMenu);
      procedure Clear ();
      procedure AddButton (id_item: integer; const s_text: widestring = '');
      procedure AddSeparator ();
      procedure SetButtons (o_list: TxlIntList);  // id list
      procedure GetButtons (var o_list: TxlIntList);

      procedure PopupMenu (id_menuitem: integer);
		function ProcessCommand (dwEvent: WORD): DWORD; override;
//   	function ProcessNotify (code: integer; lParam: dword): dword; override;

      property ItemText[id: word]: widestring read GetItemText write SetItemText;
      property ItemEnabled[id: word]: boolean read GetItemEnabled write SetItemEnabled;
      property ItemChecked[id: word]: boolean read GetItemChecked write SetItemChecked;
      function ItemExists (id: Word): boolean;
      procedure GetItemList (o_list: TxlIntList);

      property OnCommand: TCommandEvent read FOnCommand write SetCommandProc;
      property OnContextMenu: TNotifyEvent read FOnContextMenu write FOnContextMenu;
   end;

   TxlAccelTable = class
   private
      Fhandle: HACCEL;
      FKeyList: TxlStrList;
      FEnableArray: array of boolean;
      FMenu: TxlMenu;
      FOnCommand: TCommandEvent;
		function f_TextToAccel (s_key: widestring; var i_mod: byte; var i_key: word): boolean;
	protected
   	procedure OnInitialize (); virtual;
      procedure SetTemplate (id_template: integer);
		procedure AddKey (id: cardinal; const s_key: widestring; b_enabled: boolean = true); // 当false时仅在菜单项中显示，不实际注册。如ctrl+c等编辑器的内置快捷方式即如此处理
      procedure LoadFromFile (const s_file: widestring);
      procedure CreateAccelTable ();
   public
   	constructor Create (id_template: integer = 0);
      destructor Destroy (); override;
      procedure AssociateMenu (o_menu: TxlMenu);
      function ItemExists (id: word): boolean;
      function GetKey (id: word): widestring;
      procedure ProcessCommand (ItemID: WORD);
      property Handle: HAccel read Fhandle;
      property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
   end;

function GetSimpleText (const s_text: widestring): widestring;

implementation

uses UxlConst, UxlCommDlgs;

constructor TxlMenu.Create (Wndparent: TxlWindow; h_root: HMenu = 0);
begin
   FWndParent := WndParent;
   FIDList := TxlIntList.Create();
   FToolbar := nil;
   FAccelTable := nil;
   if h_root <> 0 then
   	Fhandle := h_root
   else
   	OnInitialize ();  // settemplate in the inherited function
   FNeedDestroyMenu := (h_root <> 0);
   FWndParent.RegisterMenu (self);
   OnCreate;
end;

destructor TxlMenu.Destroy ();
begin
	OnDestroy;
  	FWndParent.UnRegisterMenu (self);
	if FNeedDestroyMenu then
   	DestroyMenu (Fhandle);     // 自动销毁所有子条目
   FIDList.Free;
   inherited;
end;

procedure TxlMenu.OnInitialize ();
begin
   Fhandle := CreateMenu ();
end;

procedure Txlmenu.SetTemplate(id_template: word);
begin
   Fhandle := LoadMenu (system.MainInstance, MakeIntResource(id_template));
end;

procedure TxlMenu.OnCreate ();
begin
end;

procedure TxlMenu.OnDestroy ();
begin
end;

procedure TxlMenu.ProcessCommand (ItemID: WORD);
begin
	if ItemExists(itemID) then
   begin
      if not ItemEnabled[itemID] then exit;
      if Items[ItemID].popup then
      begin
         if assigned (Ftoolbar) then Ftoolbar.popupMenu (ItemID)
      end
      else
         if assigned (FOnCommand) then FOnCommand (ItemID);
   end
   else
      if assigned (FOnCommand) then FOnCommand (ItemID);
end;

procedure TxlMenu.InitMenuPopup (h_menu: HMenu);
begin
end;

function TxlMenu.ItemExists (id: Word): boolean;
var lpmii: TMENUITEMINFOW;
begin
	with lpmii do
   begin
		cbsize := sizeof (lpmii);
      fMask := MIIM_DATA;
   end;
   result := GetMenuItemInfoW (Fhandle, id, false, lpmii);
end;

procedure TxlMenu.SetCommandProc (value: TCommandEvent);
begin
	FOnCommand := value;
end;

//----------

function TxlMenu.AddItem (h_parent: HMENU; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU;
begin
   result := InsertItem (h_parent, GetMenuItemCount(h_parent), id_item, s_text, i_data);
end;

function TxlMenu.InsertItem (h_parent: HMENU; i_pos: integer; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU;
var o_item: TMenuItem;
begin
   with o_item do
   begin
   	id := id_item;
      text := s_text;
      data := i_data;
      popup := false;
      checked := false;
      enabled := true;
   end;
   result := InsertItem (h_parent, i_pos, o_item);
end;

function TxlMenu.AddPopup (h_parent: HMENU; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU;
begin
	result := InsertPopup (h_parent, GetMenuItemCount(h_parent), id_item, s_text, i_data);
end;

function TxlMenu.InsertPopup (h_parent: HMENU; i_pos: integer; id_item: word; const s_text: widestring = ''; i_data: ulong = 0): HMENU;
var o_item: TMenuItem;
begin
	with o_item do
   begin
   	id := id_item;
      text := s_text;
      data := i_data;
      popup := true;
      checked := false;
      enabled := true;
   end;
	result := InsertItem (h_parent, i_pos, o_item);
end;

procedure TxlMenu.ClearPopup (h_popup: HMenu);
var i, n, id: integer;
begin
	n := GetItemCount (h_popup) - 1;
   for i := n downto 0 do
   begin
   	id := GetMenuItemID (h_popup, i);
   	DeleteMenu (h_popup, i, MF_BYPOSITION);
      with FIDList do
      	Delete (Find (id));
   end;
end;

function TxlMenu.AddItem (h_parent: HMENU; const o_item: TMenuItem): HMENU;
begin
	result := InsertItem (h_parent, GetMenuItemCount(h_parent), o_item);
end;

function TxlMenu.InsertItem (h_parent: HMENU; i_pos: integer; const o_item: TMenuItem): HMENU;
var lpmii: TMENUITEMINFOW;
begin
   with lpmii do
   begin
   	cbsize := sizeof (lpmii);
      fMask := MIIM_DATA or MIIM_FTYPE or MIIM_ID or MIIM_STATE or MIIM_STRING or MIIM_SUBMENU;
      fType := MFT_STRING;
		fState := 0;
      if o_item.checked then fState := fState or MF_CHECKED;
      if not o_item.enabled then fState := fState or MF_GRAYED;
      wID := o_item.id;
      dwItemData := o_item.data;
      dwTypeData := pwidechar(o_item.text);
		cch := length(o_item.text) * 2;
      if o_item.popup then
         hSubmenu := CreatePopupMenu()
      else
      	hSubmenu := 0;
   end;
	InsertMenuItemW (h_parent, i_pos, true, lpmii);
	result := lpmii.hSubmenu;
   FIDList.Add(o_item.id);
end;

procedure TxlMenu.AddSeparator (h_parent: HMENU);
begin
	AppendMenu (h_parent, MF_SEPARATOR, 0, nil);
end;

procedure TxlMenu.DeleteItem (id_item: word);
begin
   DeleteMenu (Fhandle, id_item, MF_BYCOMMAND);
   with FIDList do
   	Delete (Find(id_item));
end;

//----------

function TxlMenu.f_GetItem (i_id: word): TMenuItem;
var lpmii: TMENUITEMINFOW;
	n: integer;
   buffer: array [0..255] of widechar;
begin
	with lpmii do
   begin
		cbsize := sizeof (lpmii);
      fMask := MIIM_DATA or MIIM_STATE or MIIM_SUBMENU;
   end;
   if not GetMenuItemInfoW (Fhandle, i_id, false, lpmii) then exit;
   n := GetMenuStringW (Fhandle, i_id, buffer, length(buffer), MF_BYCOMMAND);
   buffer[n] := #0;
   with result do
   begin
   	id := i_id;
      text := buffer;
      text := leftstr(text, firstpos(#9, text) - 1);
      data := lpmii.dwItemData;
      popup := (lpmii.hSubMenu <> 0);
      checked := IntToBool(lpmii.fState and MFS_CHECKED);
      enabled := not IntToBool(lpmii.fState and MFS_DISABLED);
   end;
end;

function GetSimpleText (const s_text: widestring): widestring;
var k, l: integer;
begin
   result := s_text;
   k := firstpos('(', result);
   if k > 1 then
   begin
      l := lastpos (')', result);
      if l > k then
         result := leftstr(result, k - 1) + midstr (result, l + 1);
   end;
   result := replacestr (result, '&', '');
end;

procedure TxlMenu.f_SetItem (i_id: word; const o_item: TMenuItem);
var lpmii: TMENUITEMINFOW;
	s_text: widestring;
begin
	with lpmii do
   begin
   	cbsize := sizeof (lpmii);
      fMask := MIIM_DATA or MIIM_STATE or MIIM_STRING;
      dwItemData := o_item.data;
      s_text := o_item.text;
      if assigned (AccelTable) and AccelTable.itemExists (i_id) then
      	s_text := s_text + #9 + AccelTable.GetKey(i_id);
      dwTypeData := pwidechar(s_text);
      if o_item.checked then
      	fState := MFS_CHECKED
      else
      	fState := MFS_UNCHECKED;
      if o_item.enabled then
      	fState := fState or MFS_ENABLED
      else
      	fState := fState or MFS_DISABLED;
   end;
   SetMenuItemInfoW (Fhandle, i_id, false, lpmii);
   if assigned (FToolbar) then
   begin
   	s_text := GetSimpleText(o_item.text);
   	FToolbar.ItemText[i_id] := s_text;
      FToolbar.ItemEnabled[i_id] := o_item.enabled;
      FToolbar.ItemChecked[i_id] := o_item.checked;
   end;
end;

procedure TxlMenu.SetItemText (id: word; const value: widestring);
var item: TMenuItem;
begin
	item := f_getitem (id);
   item.text := value;
   f_setitem (id, item);
end;

procedure TxlMenu.SetItemChecked (id: word; value: boolean);
var item: TMenuItem;
begin
	item := f_getitem (id);
   item.checked := value;
   f_setitem (id, item);
end;

procedure TxlMenu.SetItemEnabled (id: word; value: boolean);
var item: TMenuItem;
begin
	item := f_getitem (id);
   item.enabled := value;
   f_setitem (id, item);
end;

procedure TxlMenu.SetItemVisible(id: word; value: boolean);
begin
//   if assigned (Ftoolbar) then Ftoolbar.ItemVisible[id] := value;
end;

function TxlMenu.GetItemText (id: word): widestring;
begin
	result := Items[id].text;
end;

function TxlMenu.GetItemChecked (id: word): boolean;
begin
	result := Items[id].checked;
end;

function TxlMenu.GetItemEnabled (id: word): boolean;
begin
	result := Items[id].enabled;
end;

function TxlMenu.GetItemVisible(id: word): boolean;
begin
	result := true;  //Items[id].visible;
end;

procedure TxlMenu.SwitchCheck (id: word);
begin
	ItemChecked[id] := not ItemChecked[id];
end;

procedure TxlMenu.Popup (posid: integer = 0; x: integer = -1; y: integer = -1; pdDir: TPopupDirection = pdLeftTop; b_bypos: boolean = false);
var uFlags: cardinal;
	itemid: integer;
   hsub: HMENU;
   o_pos: TPoint;
begin
	if not assigned (FWndParent) then exit;
   case pdDir of
   	pdLeftTop: uFlags := TPM_LEFTALIGN or TPM_TOPALIGN;
      pdRightTop: uFlags := TPM_RIGHTALIGN or TPM_TOPALIGN;
      pdRightBottom: uFlags := TPM_RIGHTALIGN or TPM_BOTTOMALIGN;
      pdLeftBottom: uFlags := TPM_LEFTALIGN or TPM_BOTTOMALIGN;
      else uFlags := 0;
   end;
	uFlags := uFlags or TPM_LEFTBUTTON or TPM_NOANIMATION or TPM_NONOTIFY or TPM_RETURNCMD or TPM_RECURSE;
   if b_bypos or (posid = 0) then
   	hsub := GetSubMenu(Fhandle, posid)
//   else if posid < 0 then
//   	hsub := self.Handle
   else
   	hsub := GetmenuHandle (posid);
   if (x < 0) or (y < 0) then
   begin
   	GetCursorPos (o_pos);
      x := o_pos.x;
      y := o_pos.y;
   end;
   if self <> FWndparent.Mainmenu then    // 不是主菜单，则补充发送以下消息。对于主菜单会自动发送该消息。
   	SendMessageW (FWndParent.handle, WM_INITMENUPOPUP, wParam (hsub), 0);
	itemid := cardinal (TrackPopupMenu (hsub, uFlags, x, y, 0, FWndParent.handle, nil));
   if itemid > 0 then ProcessCommand (itemid);
end;

function TxlMenu.GetMenuHandle (id: word): HMENU;
var lpmii: TMENUITEMINFOW;
begin
	with lpmii do
   begin
		cbsize := sizeof (lpmii);
      fMask := MIIM_SUBMENU;
   end;
   if GetMenuItemInfoW (Fhandle, id, false, lpmii) then
   	result := lpmii.hSubMenu
   else
    	result := 0;
end;

procedure TxlMenu.GetItemList (h_parent: HMenu; var o_list: TxlIntList);
var i, n, id: integer;
begin
	o_list.clear;
	if h_parent = Fhandle then
   begin
      for i := FIDList.Low to FIDList.High do
      	o_list.add (FIDList[i]);
   end
   else
   begin
      n := GetItemCount (h_parent);
      for i := 0 to n - 1 do
      begin
         id := GetMenuItemID(h_parent, i);
         if id >= 0 then o_list.add (id);
      end;
   end;
end;

procedure TxlMenu.GetItemList (o_list: TxlIntList);
begin
	GetItemList (Fhandle, o_list);
end;

function TxlMenu.GetItemCount (h_parent: HMENU = 0): integer;
begin
	if h_parent = 0 then
   	result := FIDList.Count
   else
		result := windows.GetMenuItemCount (h_parent);
end;

//----------------------------

const CCM_SETVERSION = 8199;

function TxlToolbar.DoCreateControl (HParent: HWND): HWND;
var i_style: DWord;
begin
   i_Style := TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_WRAPABLE;  //or TBSTYLE_CUSTOMERASE;  // or CCS_ADJUSTABLE ;
   InitCommonControl (ICC_BAR_CLASSES);
   result := CreateWin32Control (HParent, TOOLBARCLASSNAME, i_style);
end;

procedure TxlToolBar.OnCreateControl ();
var i_width: integer;
begin
	Perform (CCM_SETVERSION, 5, 0);
   i_width := GetSystemMetrics (SM_CXSMICON);
	FImages := TxlImageList.create (i_width, i_width);
   Perform (TB_SETIMAGELIST, 0, FImages.handle);
   FDisabledImages := TxlImageList.create (i_width, i_width);
   Perform (TB_SETDISABLEDIMAGELIST, 0, FDisabledImages.handle);
   Perform (TB_SETMAXTEXTROWS, 0, 0);
   FIDLIst := TxlIntList.Create();
   Clear;

   SetWndProc(@WindowProc);
   OnCreate;
//   SetClassLong (Fhandle, GCL_HBRBACKGROUND, CreateSolidBrush (RGB(255,0,0)));
end;

procedure TxlToolBar.OnDestroyControl ();
begin
	OnDestroy;
   FIDList.Free;
	FImages.free;
   FDisabledImages.free;
end;

procedure TxlToolBar.OnCreate ();
begin
end;

procedure TxlToolBar.OnDestroy ();
begin
end;

procedure TxlToolBar.SetPos (const value: TPos);
begin
	inherited;
   Perform (TB_AUTOSIZE, 0, 0);
end;

//-------------------

procedure TxlToolBar.SetCommandProc(value: TCommandEvent);
begin
	FOnCommand := value;
end;

procedure TxlToolBar.AssociateMenu (o_menu: TxlMenu);
begin
	FMenu := o_menu;
   FMenu.ToolBar := self;
end;

procedure TxlToolBar.Clear ();
var i, n: integer;
begin
	n := Perform (TB_BUTTONCOUNT, 0, 0);
   for i := n - 1 downto 0 do
   	Perform (TB_DELETEBUTTON, i, 0);
   FImages.Clear;
   FDisabledImages.clear;
   FSepCount := 0;
   FImages.addicon (tb_sep);
   FSepImage := FDisabledImages.addicon (tb_sep);
   FIDList.Clear;
end;

procedure TxlToolBar.AddButton (id_item: integer; const s_text: widestring = '');
begin
	if id_item = 0 then
   	AddSeparator
   else
   begin
      f_AddButton (id_item, s_text);
      FIDList.Add(id_item);
   end;
end;

procedure TxlToolBar.AddSeparator ();
begin
	if (tb_sep + FSepCount < tb_sepmax) then inc (FSepCount);
   f_AddButton (tb_sep + FSepCount);
   FIDList.Add(0);
end;

procedure TxlToolBar.f_AddButton (id_item: integer; const s_text: widestring = '');
var o_tbstruct: array [0..0] of TTBButton;
	i_image: integer;
   tbbi: TBBUTTONINFOW;
   s: widestring;
   b_Ismenuitem: boolean;
begin
	b_Ismenuitem := (id_item < tb_sep) or (id_item > tb_sepmax);
   if b_IsMenuItem then
   begin
      i_image := FImages.AddIcon(id_item);
      FDisabledImages.AddIconGrayed(id_item);
   end;
   with o_tbstruct[0] do
   begin
   	fsStyle := BTNS_BUTTON;  // or BTNS_SHOWTEXT;
      if b_IsMenuItem then
      begin
         iBitmap := makelong (i_image, 0);
         if s_text <> '' then
         	iString := dword(pwidechar(s_text))
         else if assigned (FMenu) then
         begin
            s := FMenu.ItemText[id_item];
         	iString := dword(pwidechar(GetSimpleText(s)));
         end
         else
         	iString := 0;
         fsState := TBSTATE_ENABLED;
      end
      else    //separator
		begin
         iBitmap := makelong (FSepImage, 0);
         iString := 0;
         fsState := 0;
      end;
      idCommand := id_item;
      dwData := 0;
   end;
   Perform (TB_ADDBUTTONSW, 1, DWORD(@o_tbstruct));
   if (id_item < tb_sep) or (id_item > tb_sepmax) then
   begin
   end
   else    // separators
   begin
      with tbbi do
      begin
         cbSize := sizeof (tbbi);
         dwMask := TBIF_SIZE;
         cx := 9;
      end;
      Perform (TB_SETBUTTONINFOW, id_item, dword(@tbbi));
   end;
end;

procedure TxlToolBar.SetButtons (o_list: TxlIntList);
var i, i_menu: integer;
begin
	Clear;
   for i := 0 to o_list.count - 1 do
   begin
   	i_menu := o_list[i];
      if i_menu > 0 then
      begin
      	AddButton (i_menu);
         if assigned (FMenu) then
         begin
         	ItemEnabled[i_menu] := FMenu.ItemEnabled[i_menu];
            ItemChecked[i_menu] := FMenu.ItemChecked[i_menu];
         end;
      end
      else
      	AddSeparator ();
   end;
end;

procedure TxlToolBar.SetItemEnabled (id: word; value: boolean);
begin
	Perform (TB_ENABLEBUTTON, id, MakeLong(BoolToInt(value), 0));
end;

procedure TxlToolBar.SetItemChecked (id: word; value: boolean);
begin
	Perform (TB_CHECKBUTTON, id, MakeLong(BoolToInt(value), 0));
end;

function TxlToolbar.GetItemEnabled(id: word): boolean;
begin
	result := true;
end;

function TxlToolbar.GetItemChecked(id: word): boolean;
begin
	result := false;
end;

procedure TxlToolbar.SetItemVisible (id: word; value: boolean);
begin
end;

function TxlToolbar.GetItemVisible (id: word): boolean;
begin
	result := true;
end;

function TxlToolBar.GetItemText(id: word): widestring;
begin
	result := '';
end;

procedure TxlToolBar.SetItemText(id: word; const value: widestring);
var tbbi: TBBUTTONINFOW;
begin
   with tbbi do
   begin
      cbSize := sizeof (tbbi);
      dwMask := TBIF_TEXT;
      pszText := pwidechar(value); //pansichar(UnicodeToAnsi(value));
   end;
   Perform (TB_SETBUTTONINFOW, id, dword(@tbbi));
end;

function TxlToolBar.ItemExists(id: word): boolean;
var i: integer;
begin
	result := false;
   for i := FIDList.Low to FIDLIst.High do
   	if FIDList[i] = id then
      begin
      	result := true;
         exit;
      end;
end;

procedure TxlToolBar.GetItemList (o_list: TxlIntList);
begin
end;

procedure TxlToolBar.GetButtons (var o_list: TxlIntList);
var i: integer;
begin
	o_list.Clear;
   for i := FIDList.Low to FIDList.High do
   	o_list.Add(FIDList[i]);
end;

procedure TxlToolBar.PopupMenu (id_menuitem: integer);
var rect: TRect;
   pt: TPoint;
begin
   SendMessageW (Parent.handle, WM_INITMENUPOPUP, wparam(FMenu.GetMenuHandle(id_menuitem)), 0);
   Perform (TB_GETRECT, id_menuitem, dword(@rect));
   ItemChecked[id_menuitem] := true;
   pt.x := rect.left;
   pt.y := rect.bottom;
   ClientToScreen (parent.handle, pt);
   FMenu.popup (id_menuitem, pt.x, pt.y, pdLeftTop, false);
   ItemChecked [id_menuitem] := false;
end;

function TxlToolbar.ProcessCommand (dwEvent: WORD): DWORD;
begin
	result := 0;
   if assigned (FMenu) and FMenu.ItemExists(dwEvent) then
   	FMenu.ProcessCommand(dwEvent)
   else if assigned (FOnCommand) then
		FOnCommand (dwEvent);
end;

function TxlToolbar.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
begin
   case AMessage of
      WM_CONTEXTMENU:
         if assigned (FOnContextMenu) then
         begin
            FOnContextMenu (self);
            result := 0;
         end;
      else
      	result := inherited ProcessMessage (AMessage, wParam, lParam);
   end;
end;

//-----------------------

constructor TxlAccelTable.Create (id_template: integer = 0);
begin
   FKeyList := TxlStrList.Create;
   if id_template = 0 then
   	OnInitialize ()
   else
   	SetTemplate (id_template);
end;

destructor TxlAccelTable.Destroy ();
begin
	FKeyList.Free;
   DestroyAcceleratorTable (Fhandle);
   inherited;
end;

procedure TxlAccelTable.OnInitialize ();
begin
end;

procedure TxlAccelTable.SetTemplate (id_template: integer);
begin
  	Fhandle := LoadAccelerators (system.mainInstance, MakeIntResource(id_template));
end;

procedure TxlAccelTable.LoadFromFile (const s_file: widestring);
var o_list: TxlStrList;
   i: integer;
begin
   if not PathFileExists (s_file) then exit;
   o_list := TxlStrList.Create();
   o_list.IndexDeli := '=';
   o_list.LoadFromFile (s_file);
   for i := o_list.Low to o_list.High do
      AddKey (o_list.Indexes[i], o_list[i]);
   o_list.free;
end;

procedure TxlAccelTable.AddKey (id: cardinal; const s_key: widestring; b_enabled: boolean = true);
var i: integer;
begin
   i := FKeyList.FindByIndex (id);
   if FKeyList.PosValid (i) then  // override the previous accelerator
   begin
      FKeyList.Items[i] := s_key;
      FEnableArray[i] := b_enabled;
   end
   else
   begin
      FKeyList.AddByIndex (id, s_key);
      SetLength (FEnableArray, FKeyList.Count);
      FEnableArray[FKeyList.Count - 1] := b_enabled;
   end;
end;

procedure TxlAccelTable.CreateAccelTable ();
var accarray: array of TAccel;
	i_mod: byte;
   i_key: word;
	i, n: integer;
begin
	n := 0;
   for i := FKeyList.Low to FKeyList.High do
   begin
   	if FEnableArray[i] and f_TextToAccel (FKeyList.Items[i], i_mod, i_key) then
      begin
         setlength (accarray, n + 1);
      	accarray[n].fVirt := i_mod;
         accarray[n].key := i_key;
      	accarray[n].cmd := FKeyList.Indexes[i];
         inc (n);
      end;
   end;
   Fhandle := CreateAcceleratorTable (accarray[0], n);
end;

function TxlAccelTable.f_TextToAccel (s_key: widestring; var i_mod: byte; var i_key: word): boolean;
var i, j: integer;
	s: widestring;
begin
	result := s_key <> '';
   if not result then exit;

   i_mod := FNOINVERT or FVirtKey;
   s_key := uppercase(triminside(s_key));
   if firstpos('CTRL', s_key) > 0 then i_mod := i_mod or FControl;
   if firstpos('SHIFT', s_key) > 0 then i_mod := i_mod or FShift;
   if firstpos('ALT', s_key) > 0 then i_mod := i_mod or FAlt;
   i := lastpos ('+', s_key);
   s := midstr (s_key, i + 1);
   if (s[1] = 'F') and (s <> 'F') then
   begin
   	j := StrToIntDef(midstr(s, 2));
      if j in [1..12] then
      	i_key := 111 + j
      else
      	result := false;
   end
   else if s = 'ESC' then
      i_key := VK_ESCAPE
   else if s = 'DEL' then
   	i_key := VK_Delete
   else if s = 'INS' then
   	i_key := VK_Insert
   else if s = 'TAB' then
   	i_key := VK_TAB
   else if s = 'NEXT' then
   	i_key := VK_NEXT
   else if s = 'PRIOR' then
   	i_key := VK_PRIOR
   else if s = 'LEFT' then
   	i_key := VK_LEFT
   else if s = 'RIGHT' then
   	i_key := VK_RIGHT
   else if s = 'UP' then
   	i_key := VK_UP
   else if s = 'DOWN' then
   	i_key := VK_DOWN
   else if length(s) = 1 then
      i_key := ord(s[1])
   else
   	result := false;
end;

function TxlAccelTable.GetKey (id: word): widestring;
begin
	if assigned (FKeyList) then
		result := FKeyList.ItemsByIndex[id]
   else
   	result := '';
end;

function TxlAccelTable.ItemExists (id: word): boolean;
begin
	result := assigned(FKeyList) and FKeyList.PosValid (FKeyList.FindByIndex(id));
end;

procedure TxlAccelTable.AssociateMenu (o_menu: TxlMenu);
begin
   FMenu := o_menu;
   FMenu.AccelTable := self;
end;

procedure TxlAccelTable.ProcessCommand (ItemID: word);
begin
   if assigned (FMenu) then
   	FMenu.ProcessCommand(ItemID)
   else if assigned (FOnCommand) then
   	FOnCommand (ItemID);
end;

end.





