unit UxlTabControl;

interface

uses Windows, Messages, CommCtrl, UxlWinControl, UxlDragControl, UxlImageList, UxlFunctions, UxlClasses, UxlList;

type
	TTabPosition = (tpTop, tpLeft, tpRight, tpBottom);

	TTabStyle = record
      MultiLine: boolean;
      TabWidth: 0..10;
   end;

   TTabItem = record
      text: widestring;
      data: ulong;
      image: integer;
      highlight: boolean;
   end;

   TTabItems = class (TxlCollection)
   private
   	FOwner: TxlWinControl;
      FOnItemChange: TNotifyEvent;
      FOnSelChanging: TItemEvent;
      FOnSelChanged: TItemEvent;
      FSelChangeEvent: boolean;

      procedure f_OnItemChange ();
      procedure f_selchanged (i_index: integer);
      function f_GetCurSel (): integer;
      procedure f_SetCurSel (value: integer);

      function f_TabItem2TCITEM (const o_item: TTabItem): TTCITEMW;
      function f_GetSelItem (): TTabItem;
      procedure f_SetSelItem (const value: TTabItem);
      procedure SetItem (i_index: integer; const o_item: TTabItem);
      function GetItem (i_index: integer): TTabItem;
      function GetItemText (i_index: integer): widestring;
      procedure SetItemText (i_index: integer; const s_text: widestring);
      function GetItemHighlight (index: integer): boolean;
      procedure SetItemHighlight (index: integer; value: boolean);

      function ProcessNotify (code: integer; lParam: dword): dword;
      property OnItemChange: TNotifyEvent read FOnItemChange write FOnItemChange;
   public
   	constructor Create (AOwner: TxlWinControl);
   	function Count (): integer; override;

      function Add (const o_item: TTabItem): integer; overload;
      function Add (const s_text: widestring; i_image: integer = -1): integer; overload;
      function Insert (i_index: integer; const o_item: TTabItem): integer; overload;
      function Insert (i_index: integer; const s_text: widestring; i_image: integer = -1): integer; overload;
      function Delete (i_index: integer; b_selchangeevent: boolean = true): boolean;
      procedure Clear ();
      function Move (i, j: integer): boolean;

      procedure Select (value: integer; b_selchangeevent: boolean = true);
      property SelIndex: integer read f_GetCurSel write f_SetCurSel;
      function FindByData (i_data: integer): integer; // return item index

      property Items[i_index: integer]: TTabItem read GetItem write SetItem; default;
      property ItemText[i_index: integer]: widestring read GetItemText write SetItemText;
      property ItemHighlight[i_index: integer]: boolean read GetItemHighlight write SetItemHighlight;
      property SelItem: TTabItem read f_GetSelItem write f_SetSelItem;

      property OnSelChanging: TItemEvent read FOnSelChanging write FOnSelChanging;
      property OnSelChanged: TItemEvent read FOnSelChanged write FOnSelChanged;
	end;

   TxlTabControlSuper = class (TxlControlWithImages)
   private
      FStyle: TTabStyle;
      FTabPosition: TTabPosition;
      FOnDblClick: TItemEvent;
      FOnContextMenu: TItemEvent;

      procedure f_OnItemsChange (Sender: TObject);
   protected
      FItems: TTabItems;
   	function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure WinSized (); override;
      function ProcessNotify (code: integer; lParam: dword): dword; override;
      procedure OnFontChange (Sender: TObject); override;
      procedure SetTabPosition (tp: TTabPosition);
      procedure SetShowImages (value: boolean); override;

// 		function IsUserClass(): boolean; override;
      procedure SetDropHighlight (hidx: integer; b_highlight: boolean); override;
      function HitTestItem (): integer; override;
      procedure GetDragData (o_list: TxlIntList); override;
      procedure DeleteDragItems (); override;

      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
   public
      constructor create (WndParent: TxlWinContainer; h_handle: HWND = 0); override;
      destructor destroy (); override;
      procedure SetStyle (const value: TTabStyle);
      function ClientRect (): TRect; override;

      property Style: TTabStyle read FStyle write SetStyle;
      property TabPosition: TTabPosition read FTabPosition write SetTabPosition;
      property OnDblClick: TItemEvent read FOnDblClick write FOnDblClick;
      property OnContextMenu: TItemEvent read FOnContextMenu write FOnContextMenu;
   end;

   TxlTabControl = class (TxlTabControlSuper)
   private
   public
      property Items: TTabItems read FItems write FItems;
   end;

   TxlPageControl = class (TxlTabControlSuper)
   private
      FPageList: TxlObjList;
      FOnPageChanged: TItemEvent;
      procedure f_OnTabSelChanged (index: integer);
		function f_FindPage (page: TxlControl; var i_index: integer): boolean;
   protected
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
   public
		procedure AddPage (const s_text: widestring; page: TxlControl; id_image: integer = -1);
      procedure RemovePage (page: TxlControl);
		procedure SelectPage (page: TxlControl); overload;
      procedure SelectPage (index: integer); overload;
      property OnPageChanged: TItemEvent read FOnPageChanged write FOnPageChanged;
   end;

implementation

constructor TTabItems.Create (AOwner: TxlWinControl);
begin
	FOwner := AOwner;
   FSelChangeEvent := true;
end;

procedure TTabItems.f_OnItemChange ();
begin
	if assigned (FOnItemChange) then
   	FOnItemChange (self);
end;

function TTabItems.ProcessNotify (code: integer; lParam: dword): dword;
begin
	result := 0;
   case code of
   	TCN_SelChanging:
      	if FSelChangeEvent and assigned (FOnSelChanging) then
            FOnSelChanging (SelIndex);
      TCN_SelChange:
      	f_SelChanged (SelIndex);
   end;
end;

procedure TTabItems.f_SelChanged (i_index: integer);
begin
   if FSelChangeEvent and PosValid (i_index) and assigned (FOnSelChanged) then
      FOnSelChanged (i_index);
end;

function TTabItems.f_GetCurSel (): integer;
begin
	result := FOwner.Perform (TCM_GETCURSEL, 0, 0);
end;

procedure TTabItems.f_SetCurSel (value: integer);
begin
	Select (value);
end;

function TTabItems.Count (): integer;
begin
	result := FOwner.Perform (TCM_GetItemCount, 0, 0);
end;

procedure TTabItems.Select (value: integer; b_selchangeevent: boolean = true);
begin
   FSelChangeEvent := b_selchangeevent;
	FOwner.Perform (TCM_SETCURSEL, value, 0);
   FSelChangeEvent := true;
end;

//----------------

function TTabItems.Add (const o_item: TTabItem): integer;
begin
	result := Insert (Count, o_item);
end;

function TTabItems.Add (const s_text: widestring; i_image: integer = -1): integer;
begin
	result := Insert (Count, s_text, i_image);
end;

function TTabItems.Insert (i_index: integer; const s_text: widestring; i_image: integer = -1): integer;
var o_item: TTabItem;
begin
	with o_item do
   begin
   	text := s_text;
      data := 0;
      image := i_image;
      highlight := false;
   end;
   result := Insert (i_index, o_item);
end;

function TTabItems.Insert (i_index: integer; const o_item: TTabItem): integer;
var tci: TTCITEMW;
begin
	tci := f_TabItem2TCITEM (o_item);
   result := FOwner.Perform (TCM_INSERTITEMW, i_index, DWORD(@tci));
	if o_item.highlight then SetItem (i_index, o_item);
end;

procedure TTabItems.SetItem (i_index: integer; const o_item: TTabItem);
var tci: TTCITEMW;
begin
	tci := f_TabItem2TCITEM (o_item);
	FOwner.Perform (TCM_SETITEMW, i_index, DWORD(@tci));
   f_OnItemChange;
end;

function TTabItems.GetItem (i_index: integer): TTabItem;
var tci: TTCITEMW;
begin
	tci.mask := TCIF_IMAGE or TCIF_PARAM or TCIF_STATE;
	FOwner.Perform (TCM_GETITEMW, i_index, DWORD(@tci));
   with result do
   begin
      data := tci.lParam;
      image := tci.iImage;
      highlight := tci.dwState = TCIS_HIGHLIGHTED;
   	text := GetItemText (i_index);
   end;
end;

procedure TTabItems.SetItemText (i_index: integer; const s_text: widestring);
var tci: TTCITEMW;
begin
   tci.mask := TCIF_TEXT;
   tci.pszText := pwidechar(s_text);
   FOwner.Perform (TCM_SETITEMW, i_index, DWORD(@tci));
   f_OnItemChange;
end;

function TTabItems.GetItemText (i_index: integer): widestring;
var tci: TTCITEMW;
begin
	with tci do
   begin
   	mask := TCIF_TEXT;
      pszText := sharedbuffer;
      cchTextMax := sizeof(sharedbuffer);
   end;
   FOwner.Perform (TCM_GETITEMW, i_index, DWORD(@tci));
	result := sharedbuffer;
end;

procedure TTabItems.SetItemHighlight (index: integer; value: boolean);
var o_item: TTabItem;
begin
	o_item := Items[index];
   o_item.highlight := value;
   Items[index] := o_item;
end;

function TTabItems.GetItemHighlight (index: integer): boolean;
begin
	result := Items[index].highlight;
end;

function TTabItems.f_GetSelItem (): TTabItem;
begin
   if SelIndex >= 0 then
      result := Items[SelIndex];
end;

procedure TTabItems.f_SetSelItem (const value: TTabItem);
begin
   if SelIndex >= 0 then
      Items[SelIndex] := value;
end;

function TTabItems.Move (i, j: integer): boolean;
var o_item: TTabItem;
begin
	result := false;
   if (not PosValid (i)) or (not PosValid(j)) or (i = j) then exit;
   o_item := Items[i];
   Delete (i);
   Insert (j, o_item);
   result := true;
end;

function TTabItems.Delete (i_index: integer; b_selchangeevent: boolean = true): boolean;
begin
	result := IntToBool (FOwner.Perform(TCM_DELETEITEM, i_index, 0));
   if not result then exit;

   if i_index >= count then dec(i_index);
	if b_selchangeevent then f_SelChanged (i_index);
   f_OnItemChange;
end;

procedure TTabItems.Clear ();
begin
	if Count > 0 then
		FOwner.Perform (TCM_DELETEALLITEMS, 0, 0);
end;

function TTabItems.f_TabItem2TCITEM (const o_item: TTabItem): TTCITEMW;
begin
   with result do
   begin
      mask := TCIF_IMAGE or TCIF_PARAM or TCIF_TEXT or TCIF_STATE;
      pszText := pwidechar(o_item.text);
     	iImage := o_item.image;
      lParam := o_item.data;
      if o_item.highlight then
	      dwState := TCIS_HIGHLIGHTED
		else
      	dwState := 0;
   end;
end;

function TTabItems.FindByData (i_data: integer): integer; // return item index
var i: integer;
begin
	result := -1;
	for i := Low to High do
   	if Items[i].data = i_data then
      begin
      	result := i;
         exit;
      end;
end;

//---------------------------- TxlTabControl ----------------------------------------

constructor TxlTabControlSuper.create (WndParent: TxlWinContainer; h_handle: HWND = 0);
begin
   FItems := TTabItems.Create (self);
   FItems.OnItemChange := f_OnItemsChange;
	inherited Create (WndParent, h_handle);
end;

destructor TxlTabControlSuper.destroy ();
begin
	FItems.Free;
   inherited;
end;

function TxlTabControlSuper.DoCreateControl (HParent: HWND): HWND;
var i_style: dword;
begin
   i_style := WS_CLIPSIBLINGS;
   if FStyle.MultiLine or (FTabPosition in [tpLeft, tpRight]) then
      i_style := i_style or TCS_RIGHTJUSTIFY or TCS_MULTILINE;
   if FTabPosition in [tpLeft, tpRight] then
   	i_style := i_style or TCS_VERTICAL;
   if FTabPosition in [tpRight, tpBottom] then
   	i_Style := i_Style or TCS_RIGHT;
   result := CreateWin32Control (HParent, WC_TABCONTROL, i_style);
end;

procedure TxlTabControlSuper.OnCreateControl ();
begin
	inherited;
   SetWndProc (@WindowProc);
end;

procedure TxlTabControlSuper.SetStyle (const value: TTabStyle);
begin
   SetWndStyle (TCS_MULTILINE, value.MultiLine);
	SetWndStyle (TCS_FIXEDWIDTH, value.TabWidth > 0);
   if value.TabWidth > 0 then
   	TabCtrl_SetItemSize (Fhandle, value.TabWidth * 5, 20);
   FStyle := value;
   WinSized;
end;

procedure TxlTabControlSuper.SetShowImages (value: boolean);
begin
	if value then
      Perform (TCM_SetImageList, 0, Images.handle)
	else
      Perform (TCM_SetImageList, 0, 0);
   WinSized;
   inherited SetShowImages (value);
end;

procedure TxlTabControlSuper.SetTabPosition (tp: TTabPosition);
begin
	if tp <> FTabPosition then
   begin
   	FTabPosition := tp;
      Recreate;
   end;
end;

procedure TxlTabControlSuper.f_OnItemsChange (Sender: TObject);
begin
	WinSized;
end;

procedure TxlTabControlSuper.WinSized ();
begin
   SetWndStyle (TCS_MULTILINE, not FStyle.MultiLine);
   SetWndStyle (TCS_MULTILINE, FStyle.MultiLine);
   inherited;
end;

function TxlTabControlSuper.ClientRect (): TRect;
var o_rect: TRect;
	i_rowcount, i_rowheight, i_total: integer;
begin
	result := inherited ClientRect;
   if FItems.Count > 0 then
   begin
      i_rowcount := Perform (TCM_GETROWCOUNT, 0, 0);
      Perform (TCM_GETITEMRECT, 0, dword(@o_rect));
      if FTabPosition in [tpLeft, tpRight] then
      	i_rowheight := o_rect.right - o_rect.left
      else
      	i_rowheight := o_rect.bottom - o_rect.top;
      i_total := i_rowheight * i_rowcount + 4;
      case FTabPosition of
			tpTop: inc (result.Top, i_total);
         tpLeft: inc (result.Left, i_total);
         tpRight: dec (result.Right, i_total);
         tpBottom: dec (result.Bottom, i_total);
      end;
   end;
end;

function TxlTabControlSuper.ProcessNotify (code: integer; lParam: dword): dword;
begin
	result := FItems.ProcessNotify(code, lParam);
end;

procedure TxlTabControlSuper.OnFontChange (Sender: TObject);
begin
	inherited OnFontChange (Sender);
   WinSized;
end;

// 此段代码暂时屏蔽，勿删。否则setcolor无法成功。另或可尝试截获 WM_ERASEBKD, 用 SelectObject (hdc(wparam), Brush)
//function TxlTabControlSuper.IsUserClass(): boolean;
//begin
//	result := true;
//end;

//-----------------------

procedure TxlTabControlSuper.SetDropHighlight (hidx: integer; b_highlight: boolean);
var o_item: TTabItem;
begin
	o_item := FItems[hidx];
   o_item.highlight := b_highlight;
   FItems[hidx] := o_item;
end;

function TxlTabControlSuper.HitTestItem (): integer;
var tchit: TTCHITTESTINFO;
begin
	tchit.pt := CursorPos;
   tchit.flags := TCHT_ONITEMLABEL;
	result := TabCtrl_HitTest (Fhandle, @tchit);
end;

procedure TxlTabControlSuper.GetDragData (o_list: TxlIntList);
begin
	o_list.Clear;
   if FItems.SelIndex >= 0 then
   	o_list.Add(FItems.SelItem.Data);
end;

procedure TxlTabControlSuper.DeleteDragItems ();
begin
	FItems.Delete (FItems.SelIndex, false);
end;

//------------------------

function TxlTabControlSuper.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var index: integer;
begin
  	result := 0;
  	case AMessage of
      WM_LBUTTONDOWN:
         if DragDetect (Fhandle, MakePoints(lparam)) then
            CheckBeginDrag;
   	WM_LBUTTONDBLCLK:
  			if assigned (FOnDblClick) then
         	FOnDblClick (FItems.SelIndex);
      WM_CONTEXTMENU:
         if assigned (FOnContextMenu) then
         begin
            index := HitTestItem;
            if index >= 0 then
               FOnContextMenu (index);
            exit;
         end;
  	end;
   result := inherited ProcessMessage (AMessage, WParam, LParam);
end;

//-----------------------

procedure TxlPageControl.OnCreateControl ();
begin
	inherited;
   FItems.OnSelChanged := f_OnTabSelChanged;
   FPageList := TxlObjList.Create;
//   Update;
end;

procedure TxlPageControl.OnDestroyControl ();
begin
	FPageList.Free;
   inherited;
end;

procedure TxlPageControl.AddPage (const s_text: widestring; page: TxlControl; id_image: integer = -1);
begin
	if id_image >= 0 then
   	FItems.Add (s_text, Images.AddIcon (id_image))
   else
   	FItems.Add (s_text);
//   page.Parent := self;
//   page.Align := alClient;
	AddChild (page, alClient);
   FPageList.Add (page);
   Update;
   SelectPage (page);
end;

function TxlPageControl.f_FindPage (page: TxlControl; var i_index: integer): boolean;
begin
	i_index := FPageList.Find (page);
   result := FPageList.PosValid (i_index);
end;

procedure TxlPageControl.RemovePage (page: TxlControl);
var i: integer;
begin
	if f_FindPage (page, i) then
   begin
//      page.Align := alNone;
//      page.Parent := nil;
		RemoveChild (page);
      FItems.Delete (i);
      FPageList.Delete (i);
   end;
end;

procedure TxlPageControl.SelectPage (page: TxlControl);
var i: integer;
begin
	if f_FindPage (page, i) then
		SelectPage (i);
end;

procedure TxlPageControl.SelectPage(index: integer);
begin
   FItems.Select(index, false);
   f_OnTabSelChanged (index);
end;

procedure TxlPageControl.f_OnTabSelChanged (index: integer);
var i: integer;
begin
	for i := FPageList.Low to FPageList.High do
   	TxlControl(FPageList[i]).Visible := (i = index);
   TxlControl(FPageList[index]).BringToTop;
   if assigned (FOnPageChanged) then
   	FOnPageChanged (index);
end;

//-----------------------

initialization
   InitCommonControl (ICC_TAB_CLASSES);

finalization
   DeInitCommonControl();

end.

