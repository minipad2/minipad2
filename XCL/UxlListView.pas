unit UxlListView;

interface

uses Windows, Messages, CommCtrl, UxlWinControl, UxlDragControl, UxlFunctions, UxlClasses, UxlImageList, UxlList, UxlMiscCtrls;

type
	TListViewColumn = record
      text: widestring;
      width: integer;
      align: TAlign;
   end;

   TListViewColumns = class (TxlCollection)
   private
   	FOwner: TxlWinControl;
   	FCount: integer;
      FOnClick: TItemEvent;

      procedure SetColumn (i_index: integer; const o_col: TListViewColumn);
      function GetColumn (i_index: integer): TListViewColumn;
      procedure SetColText (i_index: integer; const s_text: widestring);
      function GetColText (i_index: integer): widestring;
      procedure SetColWidth (i_index: integer; i_width: integer);
      function GetColWidth (i_index: integer): integer;
      function GetTExt (): widestring;
      procedure SetTExt (const value: widestring);
      function f_ColRecordToLVColumn (const o_col: TListViewColumn): TLVColumnW;
   public
   	constructor Create (AOwner: TxlWinControl);
      function Count (): integer; override;
      procedure Clear ();

      procedure Add (const s_text: widestring; i_width: integer = 200; o_align: Talign = alLeft); overload;
      procedure Add (const o_col: TListViewColumn); overload;
      function Delete (i_index: integer): boolean;
      procedure GetHeaderOrder (o_list: TxlIntList);

      function ProcessNotify (code: integer; lParam: dword): dword;
      property OnClick: TItemEvent read FOnClick write FOnClick;

      property Cols[i_index: integer]: TListViewColumn read GetColumn write SetColumn; default;
      property ColText[i_index: integer]: widestring read GetColText write SetColText;
      property Width[i_index: integer]: integer read GetColWidth write SetColWidth;
      property Text: widestring read GetText write SetText;
   end;

type
   TSubItemEvent = procedure (i_row, i_col: integer) of object;
   TToolTipDemand = procedure (index: integer; const rtItem: TRect) of object;

   TListViewItem = record
      text: widestring;
      data: ulong;
      image: integer;
      state: integer;
      Checked: boolean;
      Selected: boolean;
   end;
   PListViewItem = ^TListViewItem;

   TListViewItems = class (TxlCollection)
   private
   	FOwner: TxlWinControl;
      FCols: TListViewColumns;
      FOnSelect: TSelItemEvent;
      FOnBeginLabelEdit: TLabelEditEvent;
      FOnEndLabelEdit: TLabelEditEvent;
      FSelChangeEvent: boolean;

      function f_GetSelIndex (): integer;
      procedure f_SetSelIndex (value: integer);
      function f_GetSelItem (): TListViewItem;
      procedure f_SetSelItem (const value: TListViewItem);

      function f_ItemRecordToLVItem (i_index: integer; const o_item: TListviewItem): TLVITEMW;
      procedure SetItem (i_index: integer; const o_item: TListViewItem);
      function GetItem (i_index: integer): TListViewItem;
      procedure SetItemText (i_index: integer; const s_text: widestring);
      function GetItemText (i_index: integer): widestring;
//      procedure SetState (i_index: integer; i_state: integer);
//      function Getstate (i_index: integer): integer;
      procedure SetChecked (i_index: integer; value: boolean);
      function GetChecked (i_index: integer): boolean;
      function GetSelected (i_index: integer): boolean;
      procedure SetText (const s_text: widestring);
      function GetText (): widestring;
   public
   	constructor Create (AOwner: TxlWinControl; ACols: TListViewColumns);
      function Count (): integer; override;
      procedure Clear();
      procedure CheckAll (value: boolean = true);
      function FindByData (i_data: integer): integer; // return item index

      function Add (const o_item: TListViewItem): integer; overload;
      function Add (const s_text: widestring; i_image: integer = 0): integer; overload;
      function Insert (i_index: integer; const o_item: TListViewItem): integer; overload;
      function Insert (i_index: integer; const s_text: widestring; i_image: integer = 0): integer; overload;
      function Delete (i_index: integer): boolean;
      function Move (i, j: integer): boolean;

      function SelCount (): integer;
      procedure Select (i_index: integer; b_sel: boolean = true);
      procedure SelectAll (b_sel: boolean = true);
      property SelIndex: integer read f_GetSelIndex write f_SetSelIndex;
      property SelItem: TListViewItem read f_GetSelItem write f_SetSelItem;

      function ProcessNotify (code: integer; lParam: dword): dword;

      property OnBeginLabelEdit: TLabelEditEvent read FOnBeginLabelEdit write FOnBeginLabelEdit;
      property OnEndLabelEdit: TLabelEditEvent read FOnEndLabelEdit write FOnEndLabelEdit;
      property OnSelect: TSelItemEvent read FOnSelect write FOnSelect;
      property SelChangeEvent: boolean write FSelChangeEvent;

      property Items[i_index: integer]: TListViewItem read GetItem write SetItem; default;
      property ItemText[i_index: integer]: widestring read GetItemText write SetItemText;
//      property State[i_index: integer]: integer read GetState write SetState;
      property Checked[i_index: integer]: boolean read GetChecked write SetChecked;
      property Selected[i_index: integer]: boolean read GetSelected write Select;
      property Text: widestring read GetTExt write SetText;
   end;

type
   TViewType = (lvsIcon, lvsSmallIcon, lvsList, lvsReport);

	TListViewStyle = record
      ViewStyle: TViewType;
      MultiSelect: boolean;
      FullRowSelect: boolean;
      GridLines: boolean;
      EditLabels: boolean;
      ShowHeader: boolean;
      HeaderDragDrop: boolean;
      CheckBoxes: boolean;
   end;

   TxlListView = class (TxlDragControl)
   private
      FOnBeginDrag: TItemEvent;
      FOnContextMenu: TItemEvent;
      FOnItemDblClick: TItemEvent;
      FOnItemRightClick: TItemEvent;
      FOnSubItemClick: TSubItemEvent;
      FOnItemReturn: TItemEvent;
      FOnDeleteItemDemand: TNotifyEvent;

      FStyle: TListViewStyle;
      FOnToolTipDemand: TToolTipDemand;

      FLargeImages: TxlImageList;
      FSmallImages: TxlImageList;
      FLargeImagesCreated: boolean;
      FSmallImagesCreated: boolean;
      FShowImages: boolean;

      FCols: TListViewColumns;
      FItems: TListViewItems;

      function f_GetViewStyle (): TViewType;
      procedure f_SetViewStyle (vs: TViewType);

      procedure SetSmallImages (value: TxlImageList);
      function GetSmallImages (): TxlImageList;
      procedure SetLargeImages (value: TxlImageList);
      function GetLargeImages (): TxlImageList;
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;

      procedure OnFontChange (Sender: TObject); override;
      procedure SetColor (i_color: TColor); override;
      function GetText (): widestring; override;
      procedure SetText (const value: widestring); override;

      procedure SetDropHighlight (hidx: integer; b_highlight: boolean); override;
      function HitTestItem (): integer; override;
      procedure GetDragData (o_list: TxlIntList); override;
      procedure DeleteDragItems (); override;
      procedure SetShowImages (value: boolean);
   public
      procedure SetStyle (const value: TListViewStyle);
      procedure Clear(); // 删除所有列和项目
      procedure Sort (i_col: integer; b_asc: boolean);

      function ProcessNotify (code: integer; lParam: dword): dword; override;

      property OnBeginDrag: TItemEvent read FOnBeginDrag write FOnBeginDrag;
      property OnContextMenu: TItemEvent read FOnContextMenu write FOnContextMenu;
      property OnToolTipDemand: TTooltipDemand read FOnToolTipDemand write FOnToolTipDemand;
      property OnItemDblClick: TItemEvent read FOnItemDblClick write FOnItemDblClick;
      property OnItemRightClick: TItemEvent read FOnItemRightClick write FOnItemRightClick;
      property OnSubItemClick: TSubItemEvent read FOnSubItemClick write FOnSubItemClick;
      property OnItemReturn: TItemEvent read FOnItemReturn write FOnItemReturn;
      property OnDeleteItemDemand: TNotifyEvent read FOnDeleteItemDemand write FOnDeleteItemDemand;

      property Style: TListViewStyle read FStyle write SetStyle;
      property ViewStyle: TViewType read f_GetViewStyle write f_SetViewStyle;

      property LargeImages: TxlImageList read GetLargeImages write SetLargeImages;
      property SmallImages: TxlImageList read GetSmallImages write SetSmallImages;
      property ShowImages: boolean read FShowImages write SetShowImages;

      property Cols: TListViewColumns read FCols;
      property Items: TListViewItems read FItems;
   end;

implementation

uses UxlWinDef, UxlStrUtils;

const c_SubItemSep = #9;
	c_ItemSep = #13#10;

//------------------------------------ TListViewColumns ---------------------------------

constructor TListViewColumns.Create (AOwner: TxlWinControl);
begin
	FOwner := AOwner;
   FCount := 0;
end;

function TListViewColumns.Count (): integer;
begin
	result := FCount;
end;

procedure TListViewColumns.Add (const s_text: widestring; i_width: integer = 200; o_align: Talign = alLeft);
var o_col: TListViewColumn;
begin
	with o_col do
   begin
   	text := s_text;
      width := i_width;
      align := o_align;
   end;
	Add (o_col);
end;

procedure TListViewColumns.Add (const o_col: TListViewColumn);
var lvc: TLVCOLUMNW;
begin
	lvc := f_ColRecordtoLVColumn (o_col);
	FOwner.Perform (LVM_InsertColumnW, FCount, DWORD(@lvc));
   inc (FCount);
end;

function TListViewColumns.f_ColRecordToLVColumn (const o_col: TListViewColumn): TLVColumnW;
begin
	with result do
   begin
      mask := LVCF_FMT or LVCF_WIDTH or LVCF_TEXT;
      case o_col.align of
      	alleft: fmt := LVCFMT_LEFT;
         alcenter: fmt := LVCFMT_Center;
         alright: fmt := LVCFMT_Right;
      end;
      pszText := pwidechar(o_col.text);
      cx := o_col.width;
   end;
end;

function TListViewColumns.Delete (i_index: integer): boolean;
begin
	result := PosValid (i_index);
   if result then
   begin
      FOwner.Perform (LVM_DELETECOLUMN, i_index, 0);
      dec (FCount);
   end;
end;

procedure TListViewColumns.Clear ();
var i: integer;
begin
   for i := High downto Low do
      Delete (i);
end;

procedure TListViewColumns.SetColumn (i_index: integer; const o_col: TListViewColumn);
var lvc: TLVColumnW;
begin
	lvc := f_ColRecordToLVColumn (o_col);
   FOwner.Perform (LVM_SETCOLUMNW, i_index, dword(@lvc));
end;

function TListViewColumns.GetColumn (i_index: integer): TListViewColumn;
var lvc: TLVCOLUMNW;
begin
	with lvc do
   begin
   	mask := LVCF_FMT or LVCF_WIDTH or LVCF_TEXT;
//   	iSubItem := i_index;
		pszText := sharedbuffer;
      cchTextMax := sharedbuffersize;
	end;
	FOwner.Perform (LVM_GETCOLUMNW, i_index, dword(@lvc));
   with result do
   begin
      case lvc.fmt of
         LVCFMT_LEFT: align := alLeft;
         LVCFMT_Center: align := alCenter;
         LVCFMT_Right: align := alRight;
      end;
      text := lvc.pszText;
      width := lvc.cx;
   end;
end;

procedure TListViewColumns.SetColText (i_index: integer; const s_text: widestring);
var o_col: TListViewColumn;
begin
   o_col := GetColumn(i_index);
   o_col.text := s_text;
   SetColumn (i_index, o_col);
end;

function TListViewColumns.GetColText (i_index: integer): widestring;
begin
	result := Cols[i_index].text;
end;

procedure TListViewColumns.SetColWidth (i_index: integer; i_width: integer);
var o_col: TListViewColumn;
begin
   o_col := GetColumn(i_index);
   o_col.width := i_width;
   SetColumn (i_index, o_col);
end;

function TListViewColumns.GetColWidth (i_index: integer): integer;
begin
	result := Cols[i_index].width;
end;

function TListViewColumns.GetText (): widestring;
var i: integer;
	o_list: TxlStrList;
begin
	o_list := TxlStrList.Create;
   o_list.SEparator := c_SubItemSep;
	for i := Low to High do
      o_list.Add (ColText[i]);
   result := o_list.Text;
	o_list.free;
end;

procedure TListViewColumns.SetTExt (const value: widestring);
var i: integer;
	o_list: TxlStrList;
begin
	o_list := TxlStrList.Create;
   o_list.SEparator := c_SubItemSep;
   o_list.Text := value;
   Clear;
   for i := o_list.Low to o_list.High do
   	Add (o_list[i]);
   o_list.free;
end;

procedure TListViewColumns.GetHeaderOrder (o_list: TxlIntList);
var h: HWND;
	i, n: integer;
   arr: array [0..1000] of integer;
begin
	h := FOwner.Perform (LVM_GETHEADER, 0, 0);
	n := SendMessageW (h, HDM_GETITEMCOUNT, 0, 0);
   SendMessageW (h, HDM_GETORDERARRAY, n, lparam(@arr));
   for i := 0 to n - 1 do
   	o_list.Add (arr[i]);
end;

function TListViewColumns.ProcessNotify (code: integer; lParam: dword): dword;
var nmlv: TNMLISTVIEW;
begin
	result := 0;
   case code of
      LVN_COLUMNCLICK:
      	begin
         	nmlv := PNMLISTVIEW (lParam)^;
            if assigned (FOnClick) then
            	FOnClick (nmlv.iSubItem);
         end;
   end;
end;

//------------------------------- TListViewItems ---------------------------------------

constructor TListViewItems.Create (AOwner: TxlWinControl; ACols: TListViewColumns);
begin
	FOwner := AOwner;
   FCols := ACols;
   FSelChangeEvent := true;
end;

function TListViewItems.Count (): integer;
begin
	result := LISTVIEW_GETITEMCOUNT (FOwner.handle);
end;

procedure TListViewItems.Clear ();
begin
	FOwner.perform (LVM_DELETEALLITEMS, 0, 0);
end;

function TListViewItems.Add (const s_text: widestring; i_image: integer = 0): integer;
begin
	result := Insert (Count, s_text, i_image);
end;

function TListViewItems.Add (const o_item: TListViewItem): integer;
begin
	result := Insert (Count, o_item);
end;

function TListViewItems.Insert (i_index: integer; const s_text: widestring; i_image: integer = 0): integer;
var o_item: TListViewItem;
begin
	with o_item do
   begin
   	text := s_text;
      data := 0;
      image := i_image;
      state := 0;
      checked := false;
      Selected := false;
   end;
   result := Insert (i_index, o_item);
end;

function TListViewItems.Insert (i_index: integer; const o_item: TListViewItem): integer;
var lvi: TLVITEMW;
begin
//	showmessage (IntToStr(i_index) + ' ' + o_item.TExt);
	lvi := f_itemRecordToLVItem (i_index, o_item);
   result := Fowner.Perform (LVM_INSERTITEMW, 0, DWORD(@lvi));
   SetItemText (i_index, o_item.text);
   SetChecked (i_index, o_item.checked);
   Select (i_index, o_item.selected);
end;

function TListViewItems.f_ItemRecordToLVItem (i_index: integer; const o_item: TListviewItem): TLVITEMW;
begin
   with result do
   begin
      mask := LVIF_ITEMINFO;
      iItem := i_index;
      iSubItem := 0;
      iImage := o_item.image;
      lParam := o_item.data;
      State := o_item.state shl 8;
      StateMask := LVIS_OVERLAYMASK;
   end;
end;

function TListViewItems.SelCount (): integer;
begin
	result := ListView_GetSelectedCount (FOwner.handle);
end;

procedure TListViewItems.SetItem (i_index: integer; const o_item: TListViewItem);
var lvi: TLVITEMW;
begin
	if not PosValid (i_index) then exit;
	lvi := f_ItemRecordToLVItem (i_index, o_item);
   FOwner.Perform (LVM_SETITEMW, 0, DWORD(@lvi));
   SetItemText (i_index, o_item.text);
   SetChecked (i_index, o_item.checked);
   Select (i_index, o_item.Selected);
end;

function TListViewItems.GetItem (i_index: integer): TListViewItem;
var lvi: TLVITEMW;
begin
	if not PosValid (i_index) then exit;
	lvi.mask := LVIF_ITEMINFO;
   lvi.iItem := i_index;
   lvi.iSubItem := 0;
   lvi.StateMask := LVIF_OVERLAYMASK;
   FOwner.Perform (LVM_GETITEMW, 0, DWORD(@lvi));
   with result do
   begin
   	text := GetItemText (i_index);
   	data := lvi.lParam;
   	image := lvi.iImage;
      state := lvi.state shr 8;
   end;
   result.Checked := GetChecked (i_index);
   result.Selected := GetSelected (i_index);
end;

procedure TListViewItems.SetItemText (i_index: integer; const s_text: widestring);
var lvi: TLVITEMW;
	i: integer;
   o_list: TxlStrList;
begin
	if not PosValid (i_index) then exit;
   lvi.mask := LVIF_TEXT;
   lvi.iItem := i_index;
   o_list := TxlStrList.Create;
   o_list.Separator := c_SubItemSep;
   o_list.Text := s_text;
   for i := o_list.Low to o_list.High do
   begin
      lvi.iSubItem := i;
      lvi.pszText := pwidechar(o_list[i]);
      FOwner.Perform (LVM_SetItemTextW, i_index, DWORD(@lvi));
   end;
   o_list.Free;
end;

function TListViewItems.GetItemText (i_index: integer): widestring;
var lvi: TLVITEMW;
	i, n: integer;
//   buffer: array [0..1000] of widechar;
	o_list: TxlStrList;
begin
	if not PosValid (i_index) then exit;
	with lvi do
   begin
      mask := LVIF_TEXT;
   	iItem := i_index;
   	pszText := sharedbuffer;
   	cchTextMax := sharedbuffersize;
   end;
   o_list := TxlStrList.Create();
   o_list.Separator := c_SubItemSep;
   for i := FCols.Low to FCols.High do
   begin
      lvi.iSubItem := i;
      n := FOwner.Perform (LVM_GETITEMTEXTW, i_index, DWORD(@lvi));
      sharedbuffer[n] := #0;
      o_list.Add(sharedbuffer);
   end;
   result := o_list.text;
   o_list.free;
end;

procedure TListViewItems.SetText (const s_text: widestring);
var o_list: TxlStrList;
	s_item: widestring;
begin
	o_list := TxlStrList.create ();
   o_list.Separator := c_ItemSep;
   o_list.Text := s_text;
   self.clear;
   while o_list.getnext (s_item) do
   	Add (s_item, 0);
   o_list.free;
end;

function TListViewItems.GetText (): widestring;
var o_list: TxlStrList;
	i: integer;
begin
   o_list := TxlStrList.Create();
   o_list.Separator := c_ItemSep;
   for i := Low to High do
   	o_list.add (items[i].text);
   result := o_list.text;
   o_list.free;
end;

//因暂时不用而屏蔽，勿删
//procedure TListViewItems.SetState (i_index: integer; i_state: integer);
//var lvi: TLVITEMW;
//begin
//	lvi.state := i_state shl 8;
//   lvi.statemask := LVIS_OVERLAYMASK;
//	Perform (LVM_SETITEMSTATE, i_index, dword(@lvi));
//end;
//
//function TListViewItems.Getstate (i_index: integer): integer;
//begin
//	result := Perform (LVM_GETITEMSTATE, i_index, LVIS_OVERLAYMASK);
//   result := result shr 8;
//end;

procedure TListViewItems.SetChecked (i_index: integer; value: boolean);
begin
   ListView_SetCheckState (FOwner.handle, i_index, value);
end;

function TListViewItems.GetChecked (i_index: integer): boolean;
begin
   result := Boolean(ListView_GetCheckState (FOwner.handle, i_index));
end;

procedure TListViewItems.CheckAll (value: boolean = true);
var i: integer;
begin
   for i := Low to High do
      Checked[i] := value;
end;

function TListViewItems.FindByData (i_data: integer): integer; // return item index
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

function TListViewItems.GetSelected (i_index: integer): boolean;
begin
   result := (ListView_GetItemState (FOwner.handle, i_index, LVIS_SELECTED) = LVIS_SELECTED);
end;

procedure TListViewItems.Select (i_index: integer; b_sel: boolean = true);
var i_state: cardinal;
begin
	if not PosValid (i_index) then exit;
   if b_sel then
   	i_state := LVIS_SELECTED
   else
   	i_state := 0;
	ListView_SetItemState (FOwner.handle, i_index, i_state, LVIS_SELECTED);
end;

procedure TListViewItems.SelectAll (b_sel: boolean = true);
var i: integer;
begin
	for i := Low to High do
   	Select (i, b_sel);
end;

function TListViewItems.f_GetSelIndex (): integer;
begin
	result := Low;
   while result <= High do
   begin
   	if Selected[result] then exit;
      inc (result);
   end;
   result := Low - 1;
end;

procedure TListViewItems.f_SetSelIndex (value: integer);
begin
	Select (value, true);
end;

function TListViewItems.f_GetSelItem (): TListViewItem;
begin
   if SelCount > 0 then
      result := Items[SelIndex];
end;

procedure TListViewItems.f_SetSelItem (const value: TListViewItem);
begin
   if SelCount > 0 then
      items[SelIndex] := value;
end;

function TListViewItems.Delete (i_index: integer): boolean;
begin
	if not PosValid (i_index) then
      result := false
   else
      result := IntToBool (FOwner.Perform (LVM_DELETEITEM, i_index, 0));
end;

function TListViewItems.Move (i, j: integer): boolean;
var o_item: TListViewItem;
begin
	result := false;
   if (not PosValid (i)) or (not PosValid(j)) or (i = j) then exit;
   o_item := Items[i];
   Delete (i);
   Insert (j, o_item);
   result := true;
end;

function TListViewItems.ProcessNotify (code: integer; lParam: dword): dword;
var nmldi: TNMLVDISPINFOW;
   nmlv: TNMLISTVIEW;
begin
	result := 0;
   case code of
   	LVN_BEGINLABELEDITW:
      	if assigned (FOnBeginLabelEdit) then
         begin
         	nmldi := PNMLVDISPINFOW (lParam)^;
         	result := FOnBeginLabelEdit (nmldi.item.iItem, nmldi.item.pszText);
         end;
   	LVN_ENDLABELEDITW:
      	if assigned (FOnEndLabelEdit) then
         begin
         	nmldi := PNMLVDISPINFOW (lParam)^;
         	result := FOnEndLabelEdit (nmldi.item.iItem, nmldi.item.pszText);
         end;
      LVN_ITEMCHANGED:
      	if FSelChangeEvent and assigned (FOnSelect) then
      	begin
            nmlv := PNMLISTVIEW (lParam)^;
            if ((nmlv.uOldState and LVIS_SELECTED) = 0) and ((nmlv.uNewState and LVIS_SELECTED) <> 0) then
            	FOnSelect (nmlv.iItem, true)
            else if ((nmlv.uOldState and LVIS_SELECTED) <> 0) and ((nmlv.uNewState and LVIS_SELECTED) = 0) then
               FOnSelect (nmlv.iItem, false);
         end;
   end;
end;

//------------------------------------ TxlListView -------------------------------------

function TxlListView.DoCreateControl (HParent: HWND): HWND;
var i_style: DWord;
begin
   i_Style := LVS_List or LVS_ALIGNTOP or LVS_AUTOARRANGE or WS_HScroll or LVS_SHOWSELALWAYS ;
   InitCommonControl (ICC_LISTVIEW_CLASSES);
	result := CreateWin32Control (HParent, WC_LISTVIEW, i_style, WS_EX_CLIENTEDGE);
end;

procedure TxlListView.OnCreateControl ();
begin
	inherited;
	FCols := TListViewColumns.Create (self);
   FItems := TListViewItems.Create (self, FCols);
   SetWndProc (@WindowProc);
end;

procedure TxlListView.OnDestroyControl ();
begin
	LargeImages := nil;
   SmallImages := nil;
   FCols.Free;
   FItems.Free;
   DeInitCommonControl();
   inherited;
end;

procedure TxlListView.Clear ();
begin
   Items.Clear;
   Cols.Clear;
end;

procedure TxlListView.SetSmallImages (value: TxlImageList);
begin
	if FSmallImagesCreated then
   begin
   	FSmallImages.free;
      FSmallImagesCreated := false;
   end;
   FSmallImages := value;
end;

function TxlListView.GetSmallImages (): TxlImageList;
var i_width: integer;
begin
	if FSmallImages = nil then
   begin
      i_width := GetSystemMetrics (SM_CXSMICON);
      FSmallImages := TxlImageList.create (i_width, i_width);
      FSmallImagesCreated := true;
   end;
   result := FSmallImages;
end;

procedure TxlListView.SetLargeImages (value: TxlImageList);
begin
	if FLargeImagesCreated then
   begin
   	FLargeImages.free;
      FLargeImagesCreated := false;
   end;
   FLargeImages := value;
end;

function TxlListView.GetLargeImages (): TxlImageList;
var i_width: integer;
begin
	if FLargeImages = nil then
   begin
      i_width := GetSystemMetrics (SM_CXICON);
      FLargeImages := TxlImageList.create (i_width, i_width);
      FLargeImagesCreated := true;
   end;
   result := FLargeImages;
end;

procedure TxlListView.SetShowImages (value: boolean);
var sh, lh: HWND;
begin
	sh := IfThen (value, SmallImages.handle, 0);
   lh := IfThen (value, LargeImages.handle, 0);
   ListView_SetImageList(Fhandle, lh, LVSIL_NORMAL);
   ListView_SetImageList(Fhandle, sh, LVSIL_SMALL);
	FShowImages := value;
end;

function TxlListView.GetText (): widestring;
begin
	result := Cols.TExt + #13#10 + Items.Text;
end;

procedure TxlListView.SetText (const value: widestring);
var i: integer;
begin
	i := FirstPos (#13#10, value);
   if i <= 0 then exit;
   Cols.Text := LeftStr (value, i - 1);
   Items.Text := MidStr (value, i + 2);
end;

procedure TxlListView.SetStyle (const value: TListViewStyle);
var dwExstyle: DWORD;
begin
   f_SetViewStyle (value.viewstyle);
	SetWndStyle (LVS_EDITLABELS, value.EditLabels);
   SetWndStyle (LVS_SINGLESEL, not value.MultiSelect);
   SetWndStyle (LVS_NOCOLUMNHEADER, not value.ShowHeader);

   dwExstyle := LISTVIEW_GETEXTENDEDLISTVIEWSTYLE (Fhandle);
   dwExStyle := dwExStyle or LVS_EX_INFOTIP;
   if value.FullRowSelect then
   	dwExstyle := dwExstyle or LVS_EX_FULLROWSELECT
   else
   	dwExstyle := dwExstyle and (not LVS_EX_FULLROWSELECT);
   if value.GridLines then
   	dwExstyle := dwExstyle or LVS_EX_GRIDLINES
   else
   	dwExstyle := dwExstyle and (not LVS_EX_GRIDLINES);
   if value.HeaderDragDrop then
   	dwExstyle := dwExstyle or LVS_EX_HEADERDRAGDROP
   else
   	dwExstyle := dwExstyle and (not LVS_EX_HEADERDRAGDROP);
   if value.CheckBoxes then
   	dwExstyle := dwExstyle or LVS_EX_CHECKBOXES
   else
   	dwExstyle := dwExstyle and (not LVS_EX_CHECKBOXES);

   ListView_SetExtendedListViewStyle (Fhandle, dwExstyle);

	FStyle := value;
end;

function TxlListView.f_GetViewStyle (): TViewType;
var i: dword;
begin
	i := GetWndLong (GWL_STYLE) and LVS_TYPEMASK;
	case i of
   	LVS_ICON: result := lvsIcon;
      LVS_SMALLICON: result := lvsSmallIcon;
      LVS_LIST: result := lvsList;
      else result := lvsReport;
   end;
end;

procedure TxlListView.f_SetViewStyle (vs: TViewType);
var dwView, dwStyle: dword;
begin
   case vs of
      lvsIcon: dwView := LVS_Icon;
      lvsSmallIcon: dwView := LVS_SmallIcon;
      lvsList: dwView := LVS_List;
      else dwView := LVS_Report;
   end;
	dwStyle := GetWndLong (GWL_STYLE);
   dwStyle := (dwStyle and not LVS_TYPEMASK) or dwView;
   SetWndLong (GWL_STYLE, dwStyle);
end;

procedure TxlListView.OnFontChange (Sender: TObject);
begin
   inherited OnFontChange (Sender);
   Perform (LVM_SETTEXTCOLOR, 0, lParam(Font.color));
end;

procedure TxlListView.SetColor (i_color: TColor);
begin
   Perform (LVM_SETBKCOLOR, 0, lparam(i_color));
   Perform (LVM_SETTEXTBKCOLOR, 0, lParam(i_color));
end;

procedure TxlListView.Sort (i_col: integer; b_asc: boolean);
var o_items: array of TListViewItem;
	i, n: integer;
   o_list: TxlObjList;
   o_list2: TxlStrList;
begin
   o_list := TxlObjList.Create;
   o_list2 := TxlStrList.Create;
   o_list2.Separator := #9;
	SetLength (o_items, Items.Count);
   for i := Items.Low to Items.High do
   begin
   	o_items[i] := Items[i];
      o_list2.Text := o_items[i].Text;
      n := o_list.Add (@o_items[i]);      // 不能用 AddByKey，因Key不允许重复
      o_list.Keys[n] := o_list2[i_col];
   end;
   o_list.SortByKey;
   Items.Clear;
   if b_asc then
   begin
   	for i := o_list.Low to o_list.High do
      	Items.Add (PListViewItem(o_list[i])^);
   end
   else
   begin
   	for i := o_list.High downto o_list.Low do
      	Items.Add (PListViewItem(o_list[i])^);
   end;
   o_list.Free;
   o_list2.Free;
end;

//----------------------

function TxlListView.ProcessNotify (code: integer; lParam: dword): dword;
var nmitem: TNMITEMACTIVATE;
   nmlv: TNMLISTVIEW;
//   iItem: integer;
   o_info: TLVHITTESTINFO;
   o_tipinfo: TNMLVGETINFOTIPW;
   rt: TRect;
begin
	result := 0;
   case code of
   	LVN_BEGINLABELEDITW, LVN_ENDLABELEDITW, LVN_ITEMCHANGED:
			result := FItems.ProcessNotify (code, lparam);
      LVN_COLUMNCLICK:
			result := FCols.ProcessNotify (code, lparam);
      NM_CLICK:
      	if assigned (FOnSubItemClick) then
         begin
            nmitem := PNMITEMACTIVATE (lParam)^;
            o_info.pt := CursorPos;
            o_info.flags := 0;
           	Perform(LVM_SUBITEMHITTEST, 0, dword(@o_info));
         	FOnSubItemClick (o_info.iItem, nmitem.iSubItem);
         end;
      NM_DBLCLK:
      	if assigned (FOnItemDblClick) then
         begin
            nmitem := PNMITEMACTIVATE (lParam)^;
            FOnItemDblClick (nmitem.iItem);
         end;
      NM_RETURN:
      	if assigned (FOnitemReturn) and (Items.SelIndex >= 0) then
				FOnItemReturn (Items.SelIndex);
      NM_RCLICK:
      	if assigned (FOnItemRightClick) then
         begin
            nmitem := PNMITEMACTIVATE (lParam)^;
            FOnItemRightClick (nmitem.iItem);
         end;
      LVN_BEGINDRAG:
      	begin
            nmlv := PNMLISTVIEW (lParam)^;
            Items.Select (nmlv.iItem);
				CheckBeginDrag;
         end;
      LVN_GETINFOTIPW:
      	begin
         	o_tipinfo := PNMLVGETINFOTIPW (lparam)^;
            rt.Left := LVIR_BOUNDS;
            Perform (LVM_GETITEMRECT, o_tipinfo.iItem, dword(@rt));
//				FTip.AddTool (0, Fhandle, rt, 'haha');
            if assigned (FOnToolTipDemand) then
            	OnToolTipDemand (o_tipinfo.iItem, rt);
         end;
   end;
end;

function TxlListView.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var b_processed: boolean;
begin
   result := 0;
   b_processed := false;
   case AMessage of
      WM_CONTEXTMENU:
         if assigned (FOnContextMenu) then
         begin
            b_processed := true;
            FOnContextMenu (HitTestItem);
         end;
      WM_KEYDOWN:
         if (wparam = VK_DELETE) and assigned (FOnDeleteItemDemand) and (Items.SelCount > 0) then
            FOnDeleteItemDemand (self);
   end;
   if not b_processed then
      result := inherited ProcessMessage (AMessage, WParam, LParam);
end;

//---------------

procedure TxlListView.SetDropHighlight (hidx: integer; b_highlight: boolean);
var i_state: cardinal;
begin
	if not Items.PosValid (hidx) then exit;
   if b_highlight then
   	i_state := LVIS_DROPHILITED
   else
   	i_state := 0;
	ListView_SetItemState (handle, hidx, i_state, LVIS_DROPHILITED);
end;

function TxlListView.HitTestItem (): integer;
var lvhit: TLVHITTESTINFO;
begin
   with lvhit do
   begin
      pt := CursorPos;
      flags := LVHT_ONITEMLABEL;
      iSubItem := 0;
   end;
   result := ListView_HitTest(handle, lvhit);
end;

procedure TxlListView.GetDragData (o_list: TxlIntList);
var i: integer;
begin
	o_list.Clear;
	for i := Items.Low to Items.High do
   	if Items[i].Selected then
      	o_list.Add(Items[i].data);
end;

procedure TxlListView.DeleteDragItems ();
var i: integer;
begin
	for i := Items.High downto Items.Low do
   	if Items[i].Selected then
      	Items.Delete (i);
end;

end.



