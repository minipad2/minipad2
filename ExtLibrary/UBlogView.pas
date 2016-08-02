unit UBlogView;

interface         // 待解决问题：Redraw （局部Item的InvalidateREct，避免全局闪烁）

uses Windows, UxlWinControl, UxlClasses, UxlPanel, UxlList, UxlEdit;

type
	TBlogItem = record
   	Title: widestring;
		Data: integer;
      DateTime: widestring;
      Text: widestring;
      IsVirtual: boolean;
      TitleRect: TRect;
      TextRect: TRect;
      Selected: boolean;
   end;
   TPopulateItemEvent = procedure (index: integer; var o_item: TBlogItem) of object;

   TBlogItems = class (TxlCollection)
   private
      FParent: TxlWinControl;
   	FItems: array of TBlogItem;
//      FSelIndex: integer;
      FOnPopulateItem: TPopulateItemEvent;
      FOnSelect: TSelItemEvent;
      FOnItemChange: TNotifyEvent;

   	function GetItem (index: integer): TBlogItem;
      procedure SetItem (index: integer; const value: TBlogItem);
      function f_GetSelIndex (): integer;
      procedure f_SetSelIndex (value: integer);
      function f_GetSelItem (): TBlogItem;
      procedure f_SetSelItem (const value: TBlogItem);
      function GetSelected (i_index: integer): boolean;
      procedure f_OnChange ();
   public
      constructor Create (AParent: TxlWinControl);
   	function Add (const value: TBlogItem): integer;
      function Insert (index: integer; const value: TBlogItem): integer;
      procedure Delete (index: integer);
      procedure Clear();
      function Count (): integer; override;
      function FindByData (i_data: integer): integer; // return item index

      function SelCount (): integer;
      procedure Select (i_index: integer; b_sel: boolean = true);
      procedure SelectAll (b_sel: boolean = true);
      property SelIndex: integer read f_GetSelIndex write f_SetSelindex;
      property SelItem: TBlogItem read f_GetSelItem write f_SetSelItem;

      property Items[index: integer]: TBlogItem read GetItem write SetItem; default;
      property Selected[i_index: integer]: boolean read GetSelected write Select;
      property OnPopulateItem: TPopulateItemEvent read FOnPopulateItem write FOnPopulateItem;
      property OnSelect: TSelItemEvent read FOnSelect write FOnSelect;
   end;

   TItemEdit = class (TxlEditControl)
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
   end;

	TBlogView = class (TxlCustomControl)
   private
      FItems: TBlogItems;
      FEdit: TItemEdit;
      FFirstRow, FPageRowCount: integer;
      FOnItemDblClick: TItemEvent;
      FOnItemReturn: TItemEvent;
      FOnDeleteItemDemand: TNotifyEvent;
      FOnContextMenu: TItemEvent;
      FMultiSelect: boolean;

      FTitleFont: TxlFont;
      FTitleSelFont: TxlFont;
      FDateTimeFont: TxlFont;
      FDateTimeSelFont: TxlFont;
      FTitleBrush: TxlBrush;
      FTitleSelBrush: TxlBrush;

      procedure SetTitleFont (const value: TxlFont);
      procedure SetTitleColor (value: TColor);
      function GetTitleColor (): TColor;
      procedure SetTitleSelFont (const value: TxlFont);
      procedure SetTitleSelColor (value: TColor);
      function GetTitleSelColor (): TColor;

      procedure f_OnVertScroll (i_request, i_thumbpos: word);
      procedure f_Draw ();
		function f_ProcessEditMessage (AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
      procedure f_OnItemChange (Sender: TObject);
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
   	procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
		function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
//      procedure OnSize (clpos: TPos); override;
   public
      property FirstRow: integer read FFirstRow write FFirstRow;
      property Items: TBlogItems read FItems;
      property OnItemDblClick: TItemEvent read FOnItemDblClick write FOnItemDblClick;
      property OnItemReturn: TItemEvent read FOnItemReturn write FOnItemReturn;
      property OnDeleteItemDemand: TNotifyEvent read FOnDeleteItemDemand write FOnDeleteItemDemand;
      property OnContextMenu: TItemEvent read FOnContextMenu write FOnContextMenu;
      property MultiSelect: boolean read FMultiSelect write FMultiSelect;

      property TitleFont: TxlFont read FTitleFont write SetTitleFont;
      property TitleColor: TColor read GetTitleColor write SetTitleColor;
      property TitleSelFont: TxlFont read FTitleSelFont write SetTitleSelFont;
      property TitleSelColor: TColor read GetTitleSelColor write SetTitleSelColor;
   end;

implementation

uses Messages, UxlMath, UxlFunctions, UxlCommDlgs;

const c_space = 5;

constructor TBlogItems.Create (AParent: TxlWinControl);
begin
   FParent := AParent;
end;

procedure TBlogItems.f_OnChange ();
begin
	if assigned (FOnItemChange) then
   	FOnItemChange (self);
end;

function TBlogItems.GetItem (index: integer): TBlogItem;
begin
	if InRange (index, Low, High()) then
   begin
   	if FItems[index].IsVirtual and assigned (FOnPopulateItem) then
         FOnPopulateItem (index, FItems[index]);
		result := FItems[index];
   end;
end;

procedure TBlogItems.SetItem (index: integer; const value: TBlogItem);
begin
	if InRange (index, Low, High) then
   begin
   	FItems[index] := value;
      f_OnChange ();
   end;
end;

function TBlogItems.Add (const value: TBlogItem): integer;
begin
	result := Insert (Count, value);
end;

function TBlogItems.Insert (index: integer; const value: TBlogItem): integer;
var i: integer;
begin
	if index < 0 then exit;
   SetLength (FItems, Count + 1);
	for i := High downto index + 1 do
      FItems[i] := FItems[i-1];
   FItems[index] := value;
   result := index;
   f_OnChange ();
end;

procedure TBlogItems.Delete (index: integer);
var i: integer;
begin
   Select (index, false);
	for i := index to High - 1 do
   	FItems[i] := FItems[i + 1];
   SetLength (FItems, Count - 1);
   f_OnChange ();
end;

procedure TBlogItems.Clear();
begin
	SetLength (FItems, 0);
   f_OnChange;
end;

function TBlogItems.Count (): integer;
begin
	result := Length (FItems);
end;

function TBlogItems.FindByData (i_data: integer): integer; // return item index
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

//----------------------

function TBlogItems.SelCount (): integer;
var i: integer;
begin
	result := 0;
   for i := Low to High do
   	if FItems[i].Selected then
      	inc (result);
end;

function TBlogItems.GetSelected (i_index: integer): boolean;
begin
	if PosValid (i_index) then
   	result := FItems[i_index].Selected;
end;

procedure TBlogItems.Select (i_index: integer; b_sel: boolean = true);
var rc: TRect;
begin
	if not PosValid (i_index) then exit;
   if b_sel <> FItems[i_index].Selected then
   begin
      FItems[i_index].Selected := b_sel;
      rc := FItems[i_index].TitleRect;
      InvalidateRect (FParent.handle, @rc, true);
      if assigned (FOnSelect) then
         FOnSelect (i_index, b_sel);
   end;
end;

procedure TBlogItems.SelectAll (b_sel: boolean = true);
var i: integer;
begin
	for i := Low to High do
   	Select (i, b_sel);
end;

function TBlogItems.f_GetSelIndex (): integer;
begin
	result := Low;
   while result <= High do
   begin
   	if Selected[result] then exit;
      inc (result);
   end;
   result := Low - 1;
end;

procedure TBlogItems.f_SetSelIndex (value: integer);
var i: integer;
begin
	for i := Low to High do
   	Select (i, i = value);
end;

function TBlogItems.f_GetSelItem (): TBlogItem;
begin
   if SelCount > 0 then
      result := Items[SelIndex];
end;

procedure TBlogItems.f_SetSelItem (const value: TBlogItem);
begin
   if SelCount > 0 then
      items[SelIndex] := value;
end;

//-------------------

function TItemEdit.DoCreateControl (HParent: HWND): HWND;
var i_style: DWord;
begin
   i_Style := ES_LEFT or ES_MULTILINE or ES_AUTOVSCROLL or ES_WANTRETURN;
   result := CreateWin32Control (HParent, 'edit', i_style, WS_EX_CLIENTEDGE);
end;

procedure TItemEdit.OnCreateControl ();
begin
	inherited;
   SetWndProc (@WindowProc);
end;

//-------------------

function TBlogView.DoCreateControl (HParent: HWND): HWND;
begin
   RegisterControlClass ('TBlogView', GetStockObject(WHITE_BRUSH));
  	result := CreateWin32Control (HParent, 'TBlogView', WS_VSCROLL);
end;

procedure TBlogView.OnCreateControl ();
begin
	inherited;

   FTitleFont := TxlFont.Create;
   FTitleFont.Assign(self.Font);
   FTitleFont.Bold := true;
   FDateTimeFont := Txlfont.Create;
   FDateTimeFont.Assign (FTitleFont);
   FDateTimeFont.Italic := true;

   FTitleSelFont := TxlFont.Create;
   FDAteTimeSelFont := TxlFont.Create;
   SetTitleSelFont (FTitleFont);

   FTitleBrush := TxlBrush.Create;
   FTitleBrush.Color := RGB (165,219,181);
   FTitleSelBrush := TxlBrush.Create;
   FTitleSelBrush.Color := RGB (255, 190, 173); //(200, 180, 200);

   FEdit := TItemEdit.Create (self);
   with FEdit do
   begin
   	ParentFont := true;
   	ParentColor := true;
   	ReadOnly := true;
   	Border := false;
      MessageHandler := f_ProcessEditMessage;
   end;
   FItems := TBlogItems.Create (self);
   FItems.FOnItemChange := f_OnItemChange;
end;

procedure TBlogView.OnDestroyControl ();
begin
	FItems.Free;
   FEdit.Free;
   
   FTitleFont.Free;
   FDateTimeFont.free;
   FTitleSelFont.free;
   FDateTimeSelFont.free;
   FTitleBrush.Free;
   FTitleSelBrush.free;
	inherited;
end;

procedure TBlogView.f_OnItemChange (Sender: TObject);
begin
	FEdit.Hide;
end;

//-------------------------------

procedure TBlogView.SetTitleFont(const value: TxlFont);
begin
   FTitleFont.Assign (value);
   FDateTimeFont.Assign (value);
   FDateTimeFont.Italic := true;
end;

procedure TBlogView.SetTitleColor (value: TColor);
begin
	FTitleBrush.Color := value;
end;

function TBlogView.GetTitleColor (): TColor;
begin
	result := FTitleBrush.Color;
end;

procedure TBlogView.SetTitleSelFont(const value: TxlFont);
begin
   FTitleSelFont.Assign (value);
   FDateTimeSelFont.Assign (value);
   FDateTimeSelFont.Italic := true;
end;

procedure TBlogView.SetTitleSelColor (value: TColor);
begin
	FTitleSelBrush.Color := value;
end;

function TBlogView.GetTitleSelColor (): TColor;
begin
	result := FTitleSelBrush.Color;
end;

//--------------------

type TBlogItemHitTest = (htTitle, htText, htFull);

function TBlogView.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
	function f_HitTestItem (x, y: integer; ht: TBlogItemHitTest): integer;
   var i: integer;
   	p: TPoint;
      b: boolean;
      rc1, rc2: TRect;
   begin
   	p.x := x;
      p.y := y;
      result := -1;
      for i := FirstRow to FirstRow + FPageRowCount do
      begin
         rc1 := Items[i].TitleRect;
         rc2 := Items[i].TextRect;
      	case ht of
         	htTitle:	b := PointInRect (p, rc1);
         	htText: b := PointInRect(p, rc2);
            else b := PointInRect (p, rc1) or PointInRect(p, rc2);
         end;
         if b then
         begin
         	result := i;
            exit;
         end;
      end;
   end;
var index, i, i_sel: integer;
	pt: TPoint;
   o_rect: TRect;
   b_ctrl, b_shift, b_processed: boolean;
begin
	result := 0;
   b_processed := true;
   case AMessage of
   	WM_PAINT:
      	f_Draw ();
      WM_VSCROLL:
      	f_OnVertScroll (LoWord(wparam), HiWord(wparam));
      WM_MOUSEWHEEL:
         if GetWheelDelta (wParam) > 0 then
            f_OnVertScroll (SB_LINEUP, 0)
         else
            f_OnVertScroll (SB_LINEDOWN, 0);
      WM_MBUTTONDOWN:    // 编辑模式
      	begin
         	index := f_HitTestItem (LoWord(lParam), HiWord(lParam), htText);
            FEdit.Hide;
            if index >= 0 then
				begin
            	FEdit.Rect := items[index].TextRect;
					o_rect := FEdit.ClientRect;
               inc (o_rect.Left, c_space - 3);
               inc (o_rect.Top, c_space - 3);
               FEdit.Perform (EM_SETRECT, 0, dword(@o_rect));
               FEdit.Text := items[index].Text;
               FEdit.Show;
               pt := FEdit.CursorPos;
               FEdit.Post (WM_LBUTTONDOWN, 0, Makelparam(pt.x, pt.y));
            end
            else
               b_processed := false;
         end;
		WM_LBUTTONDOWN, WM_RBUTTONDOWN:
      	begin
         	FEdit.Hide;
            self.SetFocus;
         	index := f_HitTestItem (LoWord(lParam), HiWord(lParam), htFull);
            if index < 0 then
               Items.SelectAll(false)
            else
            begin
               b_ctrl := KeyPressed (VK_CONTROL);
               b_shift := KeyPressed (VK_SHIFT);
               if (AMessage = WM_LBUTTONDOWN) and ((not MultiSelect) or (not (b_ctrl or b_shift))) then
                  Items.SelectAll (false);

               if MultiSelect and b_shift then
               begin
                  i_sel := FItems.SelIndex;
                  if i_sel < index then
                     for i := i_sel + 1 to index - 1 do
                        FItems.Select(i, true)
                  else
                     for i := index + 1 to i_sel - 1 do
                        Items.Select(i, true);
               end;
               Items.Select (index, true);
//                  Redraw;
            end;
            b_processed := false;
         end;
      WM_LBUTTONDBLCLK:
      	if assigned (FOnItemDblClick) then
         begin
         	index := f_HitTestItem (LoWord(lParam), HiWord(lParam), htFull);
//            if index >= 0 then
            	FOnItemDblclick (index);
         end;
      WM_KEYDOWN:
      	if (wparam = VK_RETURN) and assigned (FOnItemReturn) and (Items.SelIndex >= 0) then
         	FOnItemReturn (Items.SelIndex)
         else if (wparam = VK_DELETE) and assigned (FOnDeleteItemDemand) and (Items.SelCount > 0) then
            FOnDeleteItemDemand (self);
      WM_CONTEXTMENU:
      	begin
            if assigned (FOnContextMenu) then
            begin
               pt := CursorPos;
               index := f_HitTestItem (pt.x, pt.y, htFull);
               FOnContextMenu (index);
            end;
         end;
   	else
      	b_processed := false;
   end;
   if not b_processed then
      result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

function TBlogView.f_ProcessEditMessage (AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
begin
	result := 0;
   case AMessage of
   	WM_MOUSEWHEEL:
      	begin
	      	ProcessMessage (AMessage, wParam, lParam);
            b_processed := true;
         end
      else
      	b_processed := false;
   end;
end;

procedure TBlogView.f_OnVertScroll (i_request, i_thumbpos: word);
var i: integer;
begin
	i := FirstRow;
	case i_request of
   	SB_LINEDOWN: inc (i);
      SB_LINEUP: dec (i);
      SB_PAGERIGHT: inc (i, FPageRowCount);
      SB_PAGELEFT: dec (i, FPageRowCount);
      SB_THUMBTRACK: i := i_thumbpos;
   end;

   i := ConfineRange (i, Items.Low, Items.High - FPageRowcount + 1);
	if i <> FirstRow then
   begin
   	FirstRow := i;
      FEdit.Hide;
      Redraw;
   end;
end;

procedure TBlogView.f_Draw ();
	procedure f_DrawSepLine (h_dc: HDC; const o_rect: TRect);
   begin
      SelectObject (h_dc, GetStockObject(DC_PEN));
      SetDCPenColor (h_dc, GetSysColor(COLOR_BTNSHADOW));
      MoveToEx (h_dc, o_rect.left, o_rect.top, nil);
      LineTo (h_dc, o_rect.right, o_rect.top);
   end;

	procedure f_DrawItem (h_dc: HDC; var o_rect: TRect; index: integer);
   var o_rect2: TRect;
   	s: widestring;
      o_item: TBlogItem;
      i_titleflag, i_textflag: DWORD;
   begin
   	o_item := Items[index];
      o_rect2 := o_rect;
      o_rect2.Top := o_rect.Top + 1;

      i_titleflag := DT_SINGLELINE or DT_NOPREFIX;
      i_textflag := DT_LEFT or DT_EDITCONTROL or DT_WORDBREAK or DT_NOPREFIX or DT_EXPANDTABS;

      if o_item.Selected then
      begin
         SelectObject (h_dc, TitleSelFont.Handle );
         SetTextColor (h_dc, TitleSelFont.Color);
      end
      else
      begin
         SelectObject (h_dc, TitleFont.Handle );
         SetTextColor (h_dc, TitleFont.Color);
      end;
   	s := IntToStr(index + 1) + '. ' + o_item.Title;
      zoomrect (o_rect2, -1 * c_space);
   	DrawTextW (h_dc, pwidechar(s), Length(s), o_rect2, i_titleflag or DT_LEFT or DT_CALCRECT);
      zoomrect (o_rect2, c_space);
      o_rect2.Right := o_rect.right;
      o_item.TitleRect := o_rect2;

      if o_item.Selected then
      	FillRect (h_dc, o_rect2, FTitleSelBrush.handle)
      else
	      FillRect (h_dc, o_rect2, FTitleBrush.handle);
		zoomrect (o_rect2, -1 * c_space);
      DrawTextW (h_dc, pwidechar(s), Length(s), o_rect2, i_titleflag or DT_LEFT);

      if o_item.Selected then
         SelectObject (h_dc, FDateTimeSelFont.Handle )
      else
         SelectObject (h_dc, FDateTimeFont.Handle );
      s := o_item.DateTime;
      DrawTextW (h_dc, pwidechar(s), Length(s), o_rect2, i_titleflag or DT_RIGHT);
      zoomrect (o_rect2, c_space);
      o_rect.Top := o_rect2.Bottom;

      SelectObject (h_dc, Font.handle);
      SetTextColor (h_dc, Font.Color);
      s := o_item.Text;
      o_rect2 := o_rect;
      zoomrect (o_rect2, -1 * c_space);
   	DrawTextW (h_dc, pwidechar(s), Length(s), o_rect2, i_textflag or DT_CALCRECT);
      DrawTextW (h_dc, pwidechar(s), Length(s), o_rect2, i_textflag);
      zoomrect (o_rect2, c_space);
      o_rect2.Right := o_rect.Right;
      o_item.TextRect := o_rect2;
      Items.Fitems[index] := o_item;

      o_rect.Top := o_rect2.Bottom;
   end;
var o_rect: TRect;
   i: integer;
   h_dc: HDC;
   ps: PaintStruct;
	si: TScrollInfo;
begin
   h_dc := Beginpaint ( Fhandle, ps );
   SetBkMode( h_dc, TRANSPARENT );

   o_rect := ClientRect;
   for i := FFirstRow to Items.High do
   begin
   	f_DrawItem (h_dc, o_rect, i);

      if o_rect.Top >= o_rect.Bottom then
      	break
      else
      	f_DrawSepLine (h_dc, o_rect);
   end;

   EndPaint ( Fhandle, ps );

   FPageRowCount := i - FFirstRow;
	with si do
   begin
   	cbSize := sizeof (si);
      fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
      nMin := Items.Low;
      nMax := Items.High;
      nPos := FFirstRow;
      nPage := FPageRowCount; //Min (ClientRect.Bottom * RowCount div (TableRect.Bottom - TableRect.Top), RowCount);
   end;
  	SetScrollInfo (Fhandle, SB_VERT, si, true);
   ShowScrollBar (Fhandle, SB_VERT, (FFirstRow > Items.Low) or (i < Items.High));
end;

end.

