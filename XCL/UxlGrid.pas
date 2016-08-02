unit UxlGrid;

interface

uses Windows, UxlClasses, UxlWinControl, UxlPanel, UxlListView, UxlEdit, UxlList, UxlComboBox, UxlMiscCtrls;

type
	TGridStatus = (gsNormal, gsSizeCol, gsSizeRow, gsMoveCol, gsMoveRow);
   TFixedRowCol = (frcNone, frcAuto, frcUser);
   TDrawCellEvent = function (h_dc: HDC; const o_rect: TRect; row, col: integer): boolean of object;

 	TxlGrid = class (TxlControl)
   private
      FEdit: TxlEdit;
      FComboBox: TxlComboBox;
      FColSlide: TxlVertSlide;
      FRowSlide: TxlHorzSlide;
		FCells: array of array of widestring;    // 行，列
      FColWidth, FRowHeight: array of integer;
      FFixedRow, FFixedCol: TFixedRowCol;

      FFixedColor: TColor;
      FFixedBrush: HBrush;
      FStatus: TGridStatus;
      FSizeRowCol: integer;
      FFirstRow, FFirstCol: integer;

      FReadOnly: boolean;
      FGridResizable: boolean;
      FOnDrawCell: TDrawCellEvent;

      function GetRowHeight (row: integer): integer;
      procedure SetRowHeight (row: integer; value: integer);
      function GetColWidth (col: integer): integer;
      procedure SetColWidth (col: integer; value: integer);
      function GetRowCount (): integer;
      procedure SetRowcount (value: integer);
      function GetColCount (): integer;
      procedure SetColCount (value: integer);
		procedure f_GetPageRowColCount (var i_rowcount, i_colcount: integer);
   	function PageRowCount (): integer;
      function PageColCount (): integer;
      function GetCell (row, col: integer): widestring;
      procedure SetCell (row, col: integer; const value: widestring);

      procedure SetFirstRow (value: integer);
      procedure SetFirstCol (value: integer);
      procedure MoveRow (i_source, i_target: integer);
      procedure MoveCol (i_source, i_target: integer);

      procedure f_OnClick (o_point: TPoint);
      procedure f_OnMouseMove (o_point: TPoint);
      procedure f_Draw ();
      procedure f_SetCursor (o_status: TGridStatus);
      procedure f_DetermineStatus ();
      procedure f_DetermineScrollBar ();
      procedure f_OnVertScroll (i_request, i_thumbpos: word);
      procedure f_OnHorzScroll (i_request, i_thumbpos: word);
      procedure f_OnStartMove (Sender: TObject);
      procedure f_OnRowMove (Sender: TObject);
      procedure f_OncolMove (Sender: TObject);
		procedure f_ExitFromEditMode ();

      function TableRect (): TRect;
      function TableWidth (): integer;
      function TableHeight (): integer;
      function GridRect (i_row, i_col: integer): TRect;
      function GetGridByPoint (pt: TPoint; var i_row, i_col: integer): boolean;
      function NoGrid (): boolean;

      procedure SetFixedColor (i_color: TColor);
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
   	procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
      procedure OnSize (clpos: TPos); override;
      function IsUserClass (): boolean; override;
   public
      property RowCount: integer read GetRowCount write SetRowCount;  // 包括表头行
      property ColCount: integer read GetColCount write SetColCount;
      property FixedRow: TFixedRowCol read FFixedRow write FFixedRow;
      property FixedCol: TFixedRowCol read FFixedCol write FFixedCol;
      property FirstRow: integer read FFirstRow write SetFirstRow;
      property FirstCol: integer read FFirstCol write SetFirstCol;

      procedure SaveGrid;
      function GridValid (row, col: integer): boolean;
      property Cells[row, col: integer]: widestring read GetCell write SetCell; default;
      property RowHeight[row: integer]: integer read GetRowHeight write SetRowHeight;
      property ColWidth[col: integer]: integer read GetColWidth write SetColWidth;
      property FixedColor: TColor read FFixedColor write SetFixedColor;
      property ReadOnly: boolean read FReadOnly write FReadOnly;
      property GridResizable: boolean read FGridResizable write FGridResizable;
      property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
   end;

implementation

uses CommCtrl, Messages, UxlStrUtils, UxlFunctions, UxlMath, UxlCommDlgs;

function TxlGrid.DoCreateControl (HParent: HWND): HWND;
begin
   RegisterControlClass ('TxlGrid', GetStockObject(WHITE_BRUSH));
  	result := CreateWin32Control (HParent, 'TxlGrid', WS_VSCROLL or WS_HSCROLL);
end;

procedure TxlGrid.OnCreateControl ();
begin
	inherited;

//   FGrayBrush := GetStockObject (LTGRAY_BRUSH);  //GetSysColorBrush(COLOR_BTNFACE);  //
	FixedColor := RGB (198, 195, 198);
   FStatus := gsNormal;

   FEdit := TxlEdit.create(self);
   with FEdit do
   begin
   	Align := alNone;
   	Border := false;
//		ParentColor := true;
      ParentFont := true;
   end;

   FComboBox := TxlComboBox.Create (self);
   with FComboBox do
   begin
   	AllowEdit := true;
      Align := alNone;
      Border := false;
      parentColor := true;
      ParentFont := true;
   end;

   FColSlide := TxlVertSlide.Create (self);
   with FColSlide do
   begin
   	Width := 8;
   	Border := true;
   	OnStartSlide := f_OnStartMove;
   	OnEndSlide := f_OnColMove;
   end;
   FRowSlide := TxlHorzSlide.Create (self);
   with FRowSlide do
   begin
   	Height := 8;
   	Border := true;
      OnStartSlide := f_OnStartMove;
   	OnEndSlide := f_OnRowMove;
   end;
   
   FFirstRow := 0;
   FFirstCol := 0;
end;

procedure TxlGrid.OnDestroyControl ();
begin
   FEdit.Free;
   FComboBox.Free;
   FColSlide.Free;
   FRowSlide.Free;
   if FFixedBrush <> 0 then
   	DeleteObject (FFixedBrush);
	inherited;
end;

function TxlGrid.IsUserClass (): boolean;
begin
	result := true;
end;

procedure TxlGrid.SetFixedColor (i_color: TColor);
begin
	FFixedColor := i_color;
   if FFixedBrush <> 0 then
   	DeleteObject (FFixedBrush);
   FFixedBrush := CreateSolidBrush (i_color);
end;

function TxlGrid.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var b_processed: boolean;
begin
	result := 0;
   b_processed := true;
   case AMessage of
   	WM_PAINT:
      	f_Draw ();
      WM_LBUTTONDOWN:
     		f_OnClick (MakePoints(lParam));
      WM_MOUSEMOVE:
      	f_OnMouseMove (MakePoints(lParam));
      WM_LBUTTONDBLCLK:
      	SetWndStyle (WS_VSCROLL or WS_HSCROLL, true);
      WM_VSCROLL:
      	f_OnVertScroll (LoWord(wparam), HiWord(wparam));
      WM_HSCROLL:
      	f_OnHorzScroll (LoWord(wparam), HiWord(wparam));
   	else
      	b_processed := false;
   end;
   if not b_processed then
      result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

procedure TxlGrid.OnSize (clpos: TPos);
begin
	inherited OnSize (clpos);
   f_DetermineScrollBar;
end;

procedure TxlGrid.f_OnVertScroll (i_request, i_thumbpos: word);
var i: integer;
begin
	i := FirstRow;
	case i_request of
   	SB_LINEDOWN: FirstRow := i + 1;
      SB_LINEUP: FirstRow := i - 1;
      SB_PAGERIGHT: FirstRow := i + PageRowCount;
      SB_PAGELEFT: FirstRow := i - PageRowCount;
      SB_THUMBTRACK: FirstRow := i_thumbpos;
   end;

	if FirstRow <> i then
   begin
   	FEdit.Hide;
      FComboBox.Hide;
      f_DetermineScrollBar;
      Redraw;
   end;
end;

procedure TxlGrid.f_OnHorzScroll (i_request, i_thumbpos: word);
var i: integer;
begin
	i := FirstCol;
	case i_request of
   	SB_LINERIGHT: FirstCol := FirstCol + 1;
      SB_LINELEFT: FirstCol := FirstCol - 1;
      SB_PAGERIGHT: FirstCol := FirstCol + PageColCount;
      SB_PAGELEFT: FirstCol := FirstCol - PageColCount;
      SB_THUMBTRACK: FirstCol := i_thumbpos;
   end;

   if FirstCol <> i then
   begin
   	FEdit.Hide;
      FComboBox.Hide;
      f_DetermineScrollBar;
      Redraw;
   end;
end;

procedure TxlGrid.f_DetermineScrollBar ();
var si: TScrollInfo;
begin
	with si do
   begin
   	cbSize := sizeof (si);
      fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
      nMin := 0;
      nMax := ColCount - 1;
      nPos := FFirstCol;
      nPage := PageColCount; //Min (ClientRect.Right * ColCount div (TableRect.Right - TableRect.Left), ColCount);
   end;
   SetScrollInfo (Fhandle, SB_HORZ, si, true);
   ShowScrollBar (Fhandle, SB_HORZ, (FirstCol > 0) or (ClientPos.Width < TableWidth));

   si.nMax := RowCount - 1;
   si.nPage := PageRowCount;  //Min (ClientRect.Bottom * RowCount div (TableRect.Bottom - TableRect.Top), RowCount);
   si.nPos := FFirstRow;
  	SetScrollInfo (Fhandle, SB_VERT, si, true);
   ShowScrollBar (Fhandle, SB_VERT, (FirstRow > 0) or (ClientPos.Height < TableHeight));
end;

//------------------------

procedure TxlGrid.SaveGrid ();
var i_oldrow, i_oldcol: integer;
	o_ctrl: TxlControl;
begin
	if FEdit.Visible then
   	o_ctrl := FEdit
   else if FComboBox.Visible then
   	o_ctrl := FComboBox
   else
   	exit;
   GetGridByPoint (RectToPoint(o_ctrl.Rect), i_oldrow, i_oldcol);
   if GridValid (i_oldrow, i_oldcol) then
      Cells[i_oldrow, i_oldcol] := o_ctrl.Text;
end;

procedure TxlGrid.f_OnClick (o_point: TPoint);
var i, i_row, i_col: integer;
	o_rect: TRect;
   o_list: TxlStrList;
begin
   if FStatus = gsNormal then // edit
   begin
   	if ReadOnly then exit;
      SaveGrid;
      GetGridByPoint (o_point, i_row, i_col);
      if not GridValid (i_row, i_col) then exit;
      o_rect := GridRect (i_row, i_col);
      inc(o_rect.Left, 1);
      inc (o_rect.Top, 1);

      if ((i_row = 0) and (FixedRow = frcAuto)) or ((i_col=0) and (FixedCol = frcAuto)) then
      begin
      	FEdit.Hide;
         FComboBox.Hide;
      end
      else if (FixedRow = frcUser) and (i_row > 0) and IsSubStr (':', Cells[0, i_col]) then
      begin
      	o_list := TxlStrList.Create;
         o_list.Separator := ',';
         o_list.Text := Trim(MidStr (Cells[0, i_col], FirstPos(':', Cells[0, i_col]) + 1));
         FComboBox.Items.Clear;
         for i := o_list.Low to o_list.High do
            FComboBox.Items.Add (o_list[i]);
         o_list.Free;

         FEdit.Hide;
         inc (o_rect.Bottom, 120);
         FComboBox.rect := o_rect;
         FComboBox.Show;
         FComboBox.Text := Cells[i_row, i_col];
         FComboBox.SetFocus;
      end
      else
      begin
         if ((FixedRow <> frcNone) and (i_row = 0)) or ((FixedCol <> frcNone) and (i_col = 0)) then
         	FEdit.Color := FixedColor
         else
         	FEdit.Color := self.Color;
      	FComboBox.Hide;
         FEdit.Show;
         FEdit.rect := o_rect;
         FEdit.Text := Cells[i_row, i_col];
         FEdit.SelectAll;
         FEdit.SetFocus;
      end;
   end;
//   else if FStatus in [gsMoveRow, gsMoveCol] then
//   	FDragButton.Show;
   f_SetCursor (FStatus);
end;

procedure TxlGrid.f_ExitFromEditMode ();
begin
   if FEdit.Visible or FComboBox.Visible then
   begin
      SaveGrid;
      FEdit.Hide;
      FComboBox.Hide;
   end;
end;

procedure TxlGrid.f_OnMouseMove (o_point: TPoint);
	procedure f_SetNormalStatus ();
   begin
      FStatus := gsNormal;
      f_SetCursor (gsNormal);
   end;
var n, m: integer;
begin
   f_DetermineStatus;
   if (FStatus in [gsSizeCol, gsSizeRow]) and KeyPressed(VK_LBUTTON) then     // size grid
   begin
		f_ExitFromEditMode;
   	if FStatus = gsSizeCol then
      begin
      	m := FColWidth [FSizeRowCol];
      	n := o_point.x - GridRect(0, FSizeRowCol).Left;
         if (m <> n) and InRange (n, 20, ClientRect.Right) then
         	FColWidth [FSizeRowCol] := n
         else if ABS (n - m) > 6 then
         	f_SetNormalStatus;
      end
      else if FStatus = gsSizeRow then
      begin
      	m := FRowHeight [FSizeRowCol];
      	n := o_point.y - GridRect(FSizeRowCol, 0).Top;
         if (m <> n) and InRange (n, 20, ClientRect.Bottom) then
           	FRowHeight [FSizeRowCol] := n
         else if ABS (n - m) > 6 then
         	f_SetNormalStatus;
      end;
      Redraw;
   end;
end;

procedure TxlGrid.f_DetermineStatus ();
	function f_IsSizeRow (const o_point: TPoint; var i_row: integer): boolean;
   var i, i_half: integer;
   begin
   	result := false;
      if not GridResizable then exit;
      i_half := (GridRect(0,0).Right - GridRect(0,0).Left) div 2 + GridRect(0,0).Left;
      if InRange (o_point.X, i_half, GridRect(0,0).Right) then
      begin
      	for i := 0 to RowCount - 1 do
         	if ABS(o_point.y - GridRect(i, 0).Bottom) <= 6 then
            begin
            	i_row := i;
               result := true;
               exit;
            end;
      end;
   end;

   function f_IsMoveRow (const o_point: TPoint; var i_row: integer): boolean;
   var i, i_half: integer;
   begin
   	result := false;
      if (RowCount = 1) or ((RowCount = 2) and (FixedRow <> frcNOne)) then exit;
      i_half := (GridRect(0,0).Right - GridRect(0,0).Left) div 2 + GridRect(0,0).Left;
      if InRange (o_point.X, GridRect(0,0).Left, i_half) then
      begin
      	for i := 0 to RowCount - 1 do
         	if InRange(GridRect(i, 0).Bottom - o_point.y, 0, FRowSlide.Height) then
            begin
            	i_row := i;
               result := true;
               exit;
            end;
      end;
   end;

   function f_IsSizeCol (const o_point: TPoint; var i_col: integer): boolean;
   var i, i_half: integer;
   begin
   	result := false;
      if not GridResizable then exit;
      i_half := (GridRect(0,0).Bottom - GridRect(0,0).Top) div 2 + GridRect(0,0).Top;
      if InRange (o_point.y, i_half, GridRect(0,0).Bottom) then
      begin
      	for i := 0 to ColCount - 1 do
         	if ABS(o_point.x - GridRect(0, i).Right) <= 6 then
            begin
            	i_col := i;
               result := true;
               exit;
            end;
      end;
   end;

   function f_IsMoveCol (const o_point: TPoint; var i_col: integer): boolean;
   var i, i_half: integer;
   begin
   	result := false;
      if (ColCount = 1) or ((ColCount = 2) and (FixedCol <> frcNOne)) then exit;
      i_half := (GridRect(0,0).Bottom - GridRect(0,0).Top) div 2 + GridRect(0,0).Top;
      if InRange (o_point.y, GridRect(0,0).Top, i_half) then
      begin
      	for i := 0 to ColCount - 1 do
         	if InRange (GridRect(0, i).Right - o_point.x, 0, FColSlide.Width) then
            begin
            	i_col := i;
               result := true;
               exit;
            end;
      end;
   end;

var pt: TPoint;
	o_rect: TRect;
begin
	pt := CursorPos;
	if KeyPressed(VK_LBUTTON) then exit;
   if f_IsSizeRow (pt, FSizeRowCol) then
      FStatus := gsSizeRow
   else if f_IsMoveRow (pt, FSizeRowCol) then
   	FStatus := gsMoveRow
   else if f_IsSizeCol (pt, FSizeRowCol) then
      FStatus := gsSizeCol
   else if f_IsMoveCol (pt, FSizeRowCol) then
   	FStatus := gsMoveCol
   else
      FStatus := gsNormal;
   f_SetCursor (FStatus);

	FColSlide.Visible := FStatus = gsMoveCol;
   FRowSlide.Visible := FStatus = gsMoveRow;
   if FStatus = gsMoveRow then
   begin
   	o_rect := GridRect (FSizeRowCol, 0);
   	with FRowSlide do
      begin
         Left := o_rect.Left;
         Top := o_rect.Bottom - Height;
         Width := RectToPos(o_rect).Width div 2;
//         Height := 10;
      end;
   end
   else
   begin
   	o_rect := GridRect (0, FSizeRowCol);
   	with FColSlide do
      begin
         Top := o_rect.Top;
         Left := o_rect.Right - Width;
//         Width := 10;
         Height := RectToPos(o_rect).Height div 2;
      end;
   end;
end;

procedure TxlGrid.f_SetCursor (o_status: TGridStatus);
   procedure f_DoSetCursor (pc: pChar);
   begin
      SetCursor (LoadCursor(0, pc));
   end;
begin
	case o_status of
   	gsNormal: f_DoSetCursor (IDC_ARROW);
      gsSizeCol: f_DoSetCursor (IDC_SIZEWE);
      gsSizeRow: f_DoSetCursor (IDC_SIZENS);
      gsMoveCol: f_DoSetCursor (IDC_HAND);
      gsMoveRow: f_DoSetCursor (IDC_HAND);
   end;
end;

procedure TxlGrid.f_OnStartMove (Sender: TObject);
begin
	f_ExitFromEditMode;
end;

procedure TxlGrid.f_OnRowMove (Sender: TObject);
var i_row, i_col: integer;
begin
	GetGridByPoint (CursorPos, i_row, i_col);
	MoveRow (FSizeRowCol, i_row);
   FStatus := gsNormal;
   FRowSlide.Hide;
   f_SetCursor (FStatus);
   Redraw;
end;

procedure TxlGrid.f_OnColMove (Sender: TObject);
var i_row, i_col: integer;
begin
	GetGridByPoint (CursorPos, i_row, i_col);
	MoveCol (FSizeRowCol, i_col);
   FStatus := gsNormal;
   FColSlide.Hide;
   f_SetCursor (FStatus);
   Redraw;
end;

//---------------

function TxlGrid.GetRowCount (): integer;
begin
	result := Length(FRowHeight);
end;

procedure TxlGrid.SetRowcount (value: integer);
var i, j, n: integer;
begin
	if value = RowCount then exit;
	n := RowCount;
   SetLength (FRowHeight, value);
   SetLength (FCells, value, ColCount);
	if value > n then
   	for i := n to value - 1 do
      begin
      	FRowHeight[i] := 25;
         for j := 0 to ColCount - 1 do
         	FCells[i, j] := '';
      end;
   f_DetermineScrollBar;
end;

function TxlGrid.GetColCount (): integer;
begin
	result := Length(FColWidth);
end;

procedure TxlGrid.SetColCount (value: integer);
var i, j, n: integer;
begin
	if value = ColCount then exit;
	n := ColCount;
   SetLength (FColWidth, value);
   SetLength (FCells, RowCount, value);
	if value > n then
   	for i := n to value - 1 do
      begin
      	FColWidth[i] := 120;
         for j := 0 to rowCount - 1 do
         	FCells[j, i] := '';
      end;
	f_DetermineScrollBar;
end;

function TxlGrid.GetRowHeight (row: integer): integer;
begin
	result := IfThen(InRange(row, 0, RowCount - 1), FRowHeight[row], 0);
end;

procedure TxlGrid.SetRowHeight (row: integer; value: integer);
var rc: TRect;
begin
	if row < 0 then exit;
   if row > RowCount - 1 then
   	RowCount := row + 1;
   GetScreenRect (rc, false);
   FRowHeight[row] := ConfineRange (value, 0, rc.Bottom);
end;

function TxlGrid.GetColWidth (col: integer): integer;
begin
	result := IfThen(InRange(col, 0, ColCount - 1), FColWidth[col], 0);
end;

procedure TxlGrid.SetColWidth (col: integer; value: integer);
var rc: TRect;
begin
   if col < 0 then exit;
   if col > ColCount - 1 then
   	ColCount := col + 1;
   GetScreenRect (rc, false);
   FColWidth[col] := ConfineRange (value, 0, rc.Right);
end;

procedure TxlGrid.f_GetPageRowColCount (var i_rowcount, i_colcount: integer);
var pt: TPoint;
	i_row, i_col: integer;
begin
   pt.x := ClientRect.Right;
   pt.y := ClientRect.Bottom;
   GetGridByPoint (pt, i_row, i_col);
   i_rowcount := i_row - FirstRow + 1;
   if pt.Y < GridRect(i_row, i_col).Bottom then  // 最后一行不完整，不计入内
   	dec(i_rowcount);
   i_colcount := i_col - FirstCol + 1;
   if pt.x < GridRect(i_row, i_col).Right then  // 最后一列不完整，不计入内。
   	dec(i_colcount);
end;

function TxlGrid.PageRowCount (): integer;
var i: integer;
begin
   f_GetPageRowColCount (result, i);
end;

function TxlGrid.PageColCount (): integer;
var i: integer;
begin
   f_GetPageRowColCount (i, result);
end;

procedure TxlGrid.SetFirstRow (value: integer);
var i, n: integer;
begin
   n := 0;
   for i := RowCount - 1 downto 0 do
   begin
   	inc (n, RowHeight[i]);
      if n > ClientPos.Height then
      	break;
   end;
   FFirstRow := ConfineRange (value, 0, i + 1);
   ReDraw;
end;

procedure TxlGrid.SetFirstCol (value: integer);
var i, n: integer;
begin
   n := 0;
   for i := ColCount - 1 downto 0 do
   begin
   	inc (n, ColWidth[i]);
      if n > ClientPos.Width then
      	break;
   end;
	FFirstCol := ConfineRange (value, 0, i + 1);
   ReDraw;
end;

procedure TxlGrid.MoveRow (i_source, i_target: integer);
var s: widestring;
	i, j, i_height: integer;
begin
	if i_source = i_target then exit;
   if (FixedRow <> frcNOne) and ((i_source = 0) or (i_target = 0)) then exit;
   for i := 0 to ColCount - 1 do
   begin
   	s := Cells[i_source, i];
   	if i_source < i_target then
      begin
         for j := i_source to i_target - 1 do
         	Cells[j, i] := Cells[j + 1, i];
      end
      else
      begin
      	for j := i_source downto i_target + 1 do
         	Cells[j, i] := Cells[j - 1, i];
      end;
      Cells[i_target, i] := s;
   end;

   i_height := RowHeight[i_source];
   if i_source < i_target then
   begin
      for j := i_source to i_target - 1 do
         RowHeight[j] := RowHeight[j + 1];
   end
   else
   begin
      for j := i_source downto i_target + 1 do
         RowHeight[j] := RowHeight[j - 1];
   end;
   RowHeight[i_target] := i_height;
//	parent.Text := InttoStr(i_source) + '-->' + IntToStr(i_target);
end;

procedure TxlGrid.MoveCol (i_source, i_target: integer);
var s: widestring;
	i, j, i_width: integer;
begin
	if i_source = i_target then exit;
   if (FixedCol <> frcNOne) and ((i_source = 0) or (i_target = 0)) then exit;
   for i := 0 to RowCount - 1 do
   begin
   	s := Cells[i, i_source];
   	if i_source < i_target then
      begin
         for j := i_source to i_target - 1 do
         	Cells[i, j] := Cells[i, j + 1];
      end
      else
      begin
      	for j := i_source downto i_target + 1 do
         	Cells[i, j] := Cells[i, j - 1];
      end;
      Cells[i, i_target] := s;
   end;

   i_width := ColWidth[i_source];
   if i_source < i_target then
   begin
      for j := i_source to i_target - 1 do
         ColWidth[j] := ColWidth[j + 1];
   end
   else
   begin
      for j := i_source downto i_target + 1 do
         ColWidth[j] := ColWidth[j - 1];
   end;
   ColWidth[i_target] := i_width;
//	parent.Text := InttoStr(i_source) + '-->' + IntToStr(i_target);
end;

//---------------------

function TxlGrid.GridValid (row, col: integer): boolean;
begin
	result := InRange (row, 0, RowCount - 1) and InRange (col, 0, ColCount - 1);
end;

function TxlGrid.NoGrid (): boolean;
begin
	result := (RowCount = 0) or (ColCount = 0);
end;

function TxlGrid.GridRect (i_row, i_col: integer): TRect;
var i: integer;
begin
	if not GridValid (i_row, i_col) then exit;

   result.Top := 0;
   if i_row >= FirstRow then
      for i := FirstRow to i_row - 1 do
         inc (result.Top, FRowHeight[i])
   else
   	for i := FirstRow - 1 downto i_row do
      	dec (result.Top, FRowHeight[i]);
   result.Bottom := result.Top + FRowHeight[i_row];

   result.Left := 0;
   if i_col >= FirstCol then
      for i := FirstCol to i_col - 1 do
         inc (result.Left, FColWidth[i])
   else
   	for i := FirstCol - 1 downto i_col do
      	dec (result.Right, FColWidth[i]);
   result.Right := result.Left + FColWidth[i_col];
end;

function TxlGrid.TableRect (): TRect;
begin
   result := GridRect (0, 0);
	result.Right := GridRect (RowCount - 1, ColCount - 1).Right;
   result.Bottom := GridRect (RowCount - 1, ColCount - 1).Bottom;
end;

function TxlGrid.TableWidth (): integer;
begin
	result := TableRect.Right - TableRect.Left;
end;

function TxlGrid.TableHeight (): integer;
begin
	result := TableRect.Bottom - TableRect.Top;
end;

function TxlGrid.GetGridByPoint (pt: TPoint; var i_row, i_col: integer): boolean;
var i, n: integer;
begin
	n := -1;
	for i := FirstRow to RowCount - 1 do
	begin
   	if n >= pt.y then break;
      inc (n, FRowHeight[i]);
   end;
   i_row := i - 1;

	n := -1;
	for i := FirstCol to ColCount - 1 do
	begin
   	if n >= pt.x then break;
      inc (n, FColWidth[i]);
   end;
   i_col := i - 1;
   result := GridValid (i_row, i_col);
end;

//---------------------

function TxlGrid.GetCell (row, col: integer): widestring;
	function f_GetLabel (i: integer; b_aschar: boolean): widestring;
   begin
   	if i = 0 then
      	result := ''
      else if not b_aschar then
      	result := IntToStr(i)
      else
      begin
      	result := '';
      	repeat
   			result := WideChar((i - 1) mod 26 + 65) + result;
            i := i div 26;
         until i = 0;
      end;
   end;
begin
	if (row = 0) and (FixedRow = frcAuto) then
   	result := IfThen (FixedCol = frcNone, f_GetLabel(col + 1, true), f_GetLabel(col, true))
   else if (col = 0) and (FixedCol = frcAuto) then
   	result := IfThen (FixedRow = frcNone, f_GetLabel(row + 1, false), f_GetLabel(row, false))
   else
		result := IfThen (GridValid (row, col), FCells[row, col], '');
end;

procedure TxlGrid.SetCell (row, col: integer; const value: widestring);
begin
	if row >= RowCount then
   	RowCount := row + 1;
   if col >= ColCount then ColCount := col + 1;
	FCells[row, col] := value;
end;

//---------------

procedure TxlGrid.f_Draw ();
var i, j, m, n: integer;
	h_dc: HDC;
	o_rect, o_rect2, o_tablerect: TRect;
   s: widestring;
   i_flag: dword;
   ps: PaintStruct;
begin
	if NoGrid then exit;

   o_rect := ClientRect;
   o_tablerect := TableRect;
   h_dc := Beginpaint ( Fhandle, ps );
   SetBkMode( h_dc, TRANSPARENT );
   SelectObject (h_dc, Font.Handle );
   SetTextColor (h_dc, Font.Color);
   f_SetCursor (FStatus);

   o_rect2.Left := 0;
   o_rect2.Top := 0;
   if (FixedRow <> frcNone) and (FirstRow = 0) then
   begin
   	o_rect2.Right := Min(o_rect.right, o_tablerect.Right);
      o_rect2.Bottom := FRowHeight[0];
      FillRect (h_dc, o_rect2, FFixedBrush);
   end;
   if (FixedCol <> frcNone) and (FirstCol = 0) then
   begin
   	o_rect2.Right := FColWidth[0];
      o_rect2.Bottom := Min (o_rect.Bottom, o_tablerect.Bottom);
      FillRect (h_dc, o_rect2, FFixedBrush);
   end;

   SelectObject (h_dc, GetStockObject(DC_PEN));
   SetDCPenColor (h_dc, GetSysColor(COLOR_BTNSHADOW));
//   SelectObject (h_dc, GetStockObject (DKGRAY_BRUSH));
	n := 0;
   for i := FirstCol to ColCount - 1 do
   begin
   	if (n >= o_rect.right) then break;
   	inc (n, FColWidth[i]);
//      Rectangle (h_dc, n - 3, 0, n + 3, RowHeight[0] div 3);
      MoveToEx (h_dc, n, 0, nil);
      LineTo (h_dc, n, Min(o_rect.bottom, o_tablerect.bottom));
   end;

	n := 0;
   for i := FirstRow to RowCount - 1 do
   begin
   	if n >= o_rect.bottom then break;
      m := 0;
      for j := FirstCol to ColCount - 1 do
      begin
      	if m >= o_rect.right then break;
         o_rect2.Left := m + 3;
         o_rect2.Top := n + 1;
         o_rect2.Right := m + FColWidth[j] - 6;
         o_rect2.Bottom := n + FRowHeight[i] - 2;
         if (not assigned (FOnDrawCell)) or (not FOnDrawCell (h_dc, o_rect2, i, j)) then
         begin
            s := Cells[i, j];
            if (i = 0) and (FixedRow = frcUser) and IsSubStr(':', s) then
               s := LeftStr (s, FirstPos(':', s) - 1);
            if s <> '' then
            begin
               i_flag := IfThen ( ((FixedRow <> frcNone) and (i = 0)) or ((FixedCol <> frcNone) and (j = 0)), DT_CENTER, DT_LEFT);
               DrawTextW (h_dc, pwidechar(s), Length(s), o_rect2, i_flag or DT_SINGLELINE or DT_NOPREFIX);
            end;
         end;
         inc (m, FColWidth[j]);
      end;

   	inc (n, Frowheight[i]);
      MoveToEx (h_dc, 0, n, nil);
      LineTo (h_dc, Min(o_rect.right, o_tablerect.right), n);
   end;

   EndPaint ( Fhandle, ps );
//   ValidateRect (Fhandle, @o_Rect);
end;

end.

