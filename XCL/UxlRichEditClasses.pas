unit UxlRichEditClasses;

interface

uses Windows, UxlClasses, UxlRichEdit, UxlStack;

type
   TxlRichEditDecorator = class (TxlInterfacedObject, IRichEditDecorator)
   private
      procedure SetColor (value: TColor);
      function GetColor (): TColor;
   protected
   	FEditor: TxlRichEdit;
      FBrush: TxlBrush;
   	FhdcCpb: HDC;   //与RichEdit兼容的Dc
      FEnabled: boolean;
      procedure DoBeforePaint (); virtual;
      procedure DoPaint (); virtual;
      procedure DoOnNotify (code, lparam: dword); virtual;
      procedure AfterSwitchEnable (); virtual;
      procedure SetEnable (value: boolean); virtual;
   public
      constructor Create (AEditor: TxlRichEdit); virtual;
      destructor Destroy (); override;
      procedure BeforePaint ();
      procedure Paint ();
      procedure OnNotify (code, lparam: dword);
      procedure EditorRecreated (); virtual;

   	property Color: TColor read GetColor write SetColor;
   	property Enabled: boolean read FEnabled write SetEnable;
	end;

   TxlRichEditLineNumber = class (TxlRichEditDecorator)
   private
      FFont: TxlFont;
      FOldLeftMargin: integer;
      procedure SetFont (value: TxlFont);
		procedure f_CalcBarWidth (var i_barwidth, i_linecount, i_halfcharwidth: integer);
   protected
//      procedure AfterSwitchEnable (); override;
      procedure SetEnable (value: boolean); override;
      procedure DoBeforePaint (); override;
   public
      constructor Create (AEditor: TxlRichEdit); override;
      destructor Destroy (); override;
      procedure EditorRecreated (); override;
      property Font: TxlFont read FFont write SetFont;
	end;

type
   TxlRichEditHLSuper = class (TxlRichEditDecorator)
   protected
   	procedure f_HighlightText (hdcEdit: HDC; i_start, i_end, i_paintheight: integer);
		function f_CreateBmp (hdcEdit: HDC; var o_pos: TPos): HBitMap;
		procedure f_GetVisibleText (var s_text: widestring; var i_offset: cardinal);
	end;

   TxlRichEditHighlightSelLine = class (TxlRichEditHLSuper)
   private
   	FRect: TRect;
      FErasedOnce: boolean;
   protected
      procedure DoBeforePaint (); override;
      procedure DoPaint (); override;
      procedure DoOnNotify (code, lparam: dword); override;
   public
   end;

   TxlRichEditHighlightText = class (TxlRichEditHLSuper)
   private
   	FText: widestring;
      FMatchCase: boolean;
      FThickness: integer;
      procedure SetText (const value: widestring);
      procedure SetMatchCase (value: boolean);
   protected
      procedure DoPaint (); override;
   public
   	property Text: widestring read FText write SetText;
      property MatchCase: boolean read FMatchCase write SetMatchCase;
      property Thickness: integer read FThickness write FThickness;   // 0 为背景色高亮，否则为下划线高亮
   end;

   TULScheme = record
      RightChar: widechar;      // LeftChar 固定为 #28. RightChar建议值：#29, #30, #31, #127
      Color: TColor;
      Thickness: integer;    // 为 0 时为背景色高亮
   end;

   TxlRichEditHighLightTextBlock = class (TxlRichEditHLSuper)
   private
   	FSchemes: array of TULScheme;
      FStack: TxlIntStack;
      procedure SetScheme (index: integer; const value: TULScheme);
      function GetScheme (index: integer): TULScheme;
//      procedure SetColor (index: integer; value: TColor);
//      function GetColor (index: integer): TColor;
      function IndexValid (index: integer): boolean;
   protected
      procedure DoPaint (); override;
   public
      constructor Create (AEditor: TxlRichEdit); override;
      destructor Destroy (); override;
		function AddScheme (const value: TULScheme): integer;  overload; // return index
      function AddScheme (rc: widechar; cl: TColor; tn: integer = 1): integer; overload;
      property Schemes[index: integer]: TULScheme read GetScheme write SetScheme;
//      property Colors[index: integer]: TColor read GetColor write SetColor;
      class function LeftChar (): widechar;
   end;

type
   TxlRichEditFindHandler = class
   private
   	FEditor: TxlRichEdit;
   	FHLText: TxlRichEditHighlightText;

      Ffindtext: widestring;
      Fdirup: boolean;
      Fmatchcase: boolean;
      Fwholeword: boolean;
      Frollback: boolean;
      FWithinSelRange: boolean;
      FAllowReplace: boolean;
      FReplaceText: widestring;
//      FSelectFirstMatch: boolean;
		procedure SetFindText (const value: widestring);
      procedure SetMatchCase (value: boolean);
      function GetHLMatch (): boolean;
      procedure SetHLMatch (value: boolean);
      function GetHLColor (): TColor;
      procedure SetHLColor (value: TColor);
      function GetHLThickness (): integer;
      procedure SetHLThickness (value: integer);
		function DoFind (i_min, i_max: integer; b_dirup: boolean): integer;  // returns pos of the first match
   public
      constructor Create (AEditor: TxlRichEdit);
      destructor Destroy (); override;
      function Find (b_reverse: boolean = false): boolean;    //
      function Replace (): boolean;
      function ReplaceAll (): boolean;

      function CanFind (): boolean;
      function CanReplace (): boolean;

      property FindText: widestring read FFindText write SetFindText;
      property DirUp: boolean read FDirUp write FDirUp;
      property MatchCase: boolean read FMatchCase write SetMatchCase;
      property WholeWord: boolean read FWholeWord write FWholeWord;
      property RollBack: boolean read FRollBack write FRollBack;
      property WithinSelRange: boolean read FWithinSelRange write FWithinSelRange;

      property AllowReplace: boolean read FAllowReplace write FAllowReplace;
      property ReplaceText: widestring read FReplaceText write FReplaceText;

      property HighlightMatch: boolean read GetHLMatch write SetHLMatch;
      property HighlightColor: TColor read GetHLColor write SetHLColor;
      property HighlightThickness: integer read GetHLThickness write SetHLThickness;
//      property SelectFirstMatch: boolean read FSelectFirstMatch write FSelectFirstMatch;
   end;

implementation

uses UxlWinDef, UxlWindow, Messages, RichEdit, CommDlg, UxlFunctions, UxlMath, UxlStrUtils, UxlCommDlgs;

constructor TxlRichEditFindHandler.Create (AEditor: TxlRichEdit);
begin
   FEditor := AEditor;
   FHLText := TxlRichEditHighlightText.Create (AEditor);
   FHLText.Color := RGB (100, 100, 100);
end;

destructor TxlRichEditFindHandler.Destroy ();
begin
	FHLText.free;
   inherited;
end;

procedure TxlRichEditFindHandler.SetFindText (const value: widestring);
begin
	FFindText := value;
   FHLText.Text := value;
end;

procedure TxlRichEditFindHandler.SetMatchCase (value: boolean);
begin
   FMatchCase := value;
   FHLText.MatchCase := value;
end;

function TxlRichEditFindHandler.GetHLMatch (): boolean;
begin
	result := FHLText.Enabled;
end;

procedure TxlRichEditFindHandler.SetHLMatch (value: boolean);
begin
	FHLText.Enabled := value;
end;

function TxlRichEditFindHandler.GetHLColor (): TColor;
begin
	result := FHLText.Color;
end;

procedure TxlRichEditFindHandler.SetHLColor (value: TColor);
begin
	FHLText.Color := value;
end;

function TxlRichEditFindHandler.GetHLThickness (): integer;
begin
	result := FHLText.Thickness;
end;

procedure TxlRichEditFindHandler.SetHLThickness (value: integer);
begin
	FHLText.Thickness := value;
end;

function TxlRichEditFindHandler.CanFind (): boolean;
begin
	result := (FFindText <> '');
end;

function TxlRichEditFindHandler.CanReplace (): boolean;
begin
	result := AllowReplace and CanFind;
end;

function TxlRichEditFindHandler.DoFind (i_min, i_max: integer; b_dirup: boolean): integer;
var wp: WPARAM;
	ft: TFindTextW;
begin
   wp := 0;
   if not b_dirup then wp := wp or FR_DOWN;
   if wholeword then wp := wp or FR_WHOLEWORD;
   if matchcase then wp := wp or FR_MatchCase;

   if (i_max > i_min) and b_dirup then    // 在选区内查询
   begin
      ft.chrg.cpMin := i_max;
      ft.chrg.cpMax := i_min;
   end
   else
   begin
      ft.chrg.cpMin := i_min;
      ft.chrg.cpMax := i_max;
   end;
   ft.lpstrtext := pwidechar(FFindText);

   result := FEditor.Perform (EM_FINDTEXTW, wp, lparam(@ft));
end;

function TxlRichEditFindHandler.Find (b_reverse: boolean = false): boolean;
var i_pos, j, i_start, i_len: integer;
	b_dirup: boolean;
begin
	result := false;
   if not CanFind then exit;
   b_dirup := dirup xor b_reverse;

   if (not WithinSelRange) and IsSameStr (FEditor.SelText, findtext, matchcase) then
   begin
      FEditor.GetSel (i_pos, j);
      if b_dirup then
         FEditor.SetSel (i_pos, 0)
      else
         FEditor.SetSel (i_pos + 1, 0);
   end;

   FEditor.GetSel (i_start, i_len);
   if WithinSelRange and (i_len > 0) then
   	i_pos := DoFind (i_start, i_start + i_len, b_dirup)
   else
   	i_pos := DoFind (i_start, -1, b_dirup);

   if (i_pos < 0) and (not WithinSelRange) and rollback then   // 循环查找
	begin
   	if b_dirup then
      	i_pos := DoFind (FEditor.TextLength, -1, b_dirup)
      else
      	i_pos := DoFind (0, -1, b_dirup);
   end;

   result := i_pos >= 0;
   if result then
   begin
   	FEditor.SetFocus;
      FEditor.SetSel (i_pos, length(findtext));
      WithinSelRange := false;
   end;
end;

function TxlRichEditFindHandler.Replace (): boolean;
var i, j: integer;
begin
	result := CanReplace;
   if result then
   begin
      result := IsSameStr (FEditor.SelText, findtext, matchcase);
      if result then
      begin
         FEditor.GetSel (i, j);
         FEditor.SelText := replacetext;
         FEditor.SetSel (i + length(replacetext), 0);
      end;
   end;
end;

function TxlRichEditFindHandler.ReplaceAll (): boolean;
var b_dirup: boolean;
	i_pos, i_start, j, i_len, i_len2, i_last: integer;
begin
	result := false;
   if not CanReplace then exit;

   b_dirup := Dirup;
   if rollback and (not WithinSelRange) then
   begin
      b_dirup := false;
      FEditor.SetSel (0, 0);
   end;

   i_len := Length(FindText);
   i_len2 := Length(ReplaceText);
   FEditor.GetSel (i_start, j);
   if WithinSelRange then
   	i_last := i_start + j
   else
   	i_last := -1;
	while true do
   begin
     	i_pos := DoFind (i_start, i_last, b_dirup);
      if i_pos < 0 then break;

      FEditor.SetSel (i_pos, i_len);
      FEditor.SelText := ReplaceText;
   	result := true;
      i_start := i_pos + i_len2;
      if WithinSelRange then
      begin
      	i_last := i_last + i_len2 - i_len;
      	if i_start >= i_last then break;
      end;
   end;
end;

//------------------------

constructor TxlRichEditDecorator.Create (AEditor: TxlRichEdit);
var hdcEdit: HDC;
begin
	FEditor := AEditor;
   FBrush := TxlBrush.Create;

   hdcEdit := GetDC ( FEditor.handle );      //获取RichEdit的Dc
   FhdcCpb := CreateCompatibleDC( hdcEdit );     //创建与RichEdit兼容的Dc
   SetBkMode( FhdcCpb, TRANSPARENT );
   SetTextColor( FhdcCpb, $000000 );  //设置显示行号的前景色
   SetTextAlign (FhdcCpb, TA_RIGHT);
   ReleaseDC (FEditor.handle, hdcEdit);

   FEditor.AddDecorator (self);
end;

destructor TxlRichEditDecorator.Destroy ();
begin
	FEditor.RemoveDecorator (self);
	FBrush.free;
   DeleteDC( FhdcCpb );
   inherited;
end;

procedure TxlRichEditDecorator.BeforePaint ();
begin
	if Enabled then
   	DoBeforePaint;
end;

procedure TxlRichEditDecorator.Paint ();
begin
	if Enabled then
   	DoPaint;
end;

procedure TxlRichEditDecorator.EditorRecreated ();
begin
end;

procedure TxlRichEditDecorator.OnNotify (code, lparam: dword);
begin
	if Enabled then
   	DoOnNotify (code, lparam);
end;

procedure TxlRichEditDecorator.DoBeforePaint ();
begin
end;

procedure TxlRichEditDecorator.DoPaint ();
begin
end;

procedure TxlRichEditDecorator.DoOnNotify (code, lparam: dword);
begin
end;

procedure TxlRichEditDecorator.SetColor (value: TColor);
begin
   FBrush.Color := value;
   FEditor.Redraw;
end;

function TxlRichEditDecorator.GetColor (): TColor;
begin
   result := FBrush.Color;
end;

procedure TxlRichEditDecorator.SetEnable (value: boolean);
begin
	if value <> FEnabled then
   begin
		FEnabled := value;
      AfterSwitchenable;
      FEditor.Redraw;
   end;
end;

procedure TxlRichEditDecorator.AfterSwitchEnable ();
begin
end;

//----------------------

constructor TxlRichEditLineNumber.Create (AEditor: TxlRichEdit);
begin
	inherited Create (AEditor);
   FFont := TxlFont.Create;
   FOldLeftMargin := FEditor.LeftMargin;
end;

destructor TxlRichEditLineNumber.Destroy ();
begin
   FFont.free;
	inherited;
end;

procedure TxlRichEditLineNumber.SetEnable (value: boolean);
begin
	inherited SetEnable (value);
	FOldLeftMargin := FEditor.LeftMargin;
end;

procedure TxlRichEditLineNumber.SetFont (value: TxlFont);
begin
	FFont.Assign(value);
end;

procedure TxlRichEditLineNumber.f_CalcBarWidth (var i_barwidth, i_linecount, i_halfcharwidth: integer);
var i, i_charcount: integer;
begin
   i_LineCount := FEditor.LineCount;
   i_charcount := Length(IntToStr(i_LineCount));
   GetCharWidth32 (FhdcCpb, 57, 57, i);
   i_barwidth := (i_charcount + 1) * i + 4;
   i_halfcharwidth := i div 2 + 2;
end;

procedure TxlRichEditLineNumber.EditorRecreated ();
begin
	FEditor.Perform (EM_SETMARGINS, EC_LEFTMARGIN, MakeLParam(FOldLeftMargin, 0));
end;

procedure TxlRichEditLineNumber.DoBeforePaint ();
   procedure f_DetectFirstChar (var i_y: integer; var i_linenumber: integer);
   var o_point: TPoint;
      i_index: cardinal;
   begin
   	i_index := FEditor.GetCharIndex (0, 0);
      i_linenumber := FEditor.Perform (EM_LINEFROMCHAR, i_index, 0);
      FEditor.Perform (EM_POSFROMCHAR, dword(@o_point), i_index);
      i_y := o_point.y;
   end;
var cr: TRect;     //RichEdit的客户区大小
   BarRect: TRect;        // 行号区
   hdcBmp: HBITMAP;      //RichEdit兼容的位图dc
   chHeight: integer;        //字符的高度，常量
   ClientHeight: integer;    //RichEdit的客户区高度
   i_line: integer;
   i_FirstLine: integer;       //文本框中的第一个可见行的行号。
   i_LineCount: integer;       //文本的总行数
   hdcEdit: HDC;
   i_barwidth, i_halfcharwidth, i_y, i_right, i_leftmargin: integer;
   s: widestring;
begin
   // 获取行高与第一行位置
   f_DetectFirstChar (i_y, i_FirstLine);
   chHeight := FEditor.LineHeight;

   FFont.Height := chHeight;
   SelectObject (FhdcCpb, Ffont.Handle);

   f_CalcBarWidth (i_barwidth, i_linecount, i_halfcharwidth);
   i_leftmargin := i_barwidth + FEditor.LeftMargin;
//   i_currentleftmargin := LoWord (FEditor.Perform (EM_GETMARGINS, 0, 0));       // 执行速度的瓶颈，千万不可！
   if i_leftmargin <> FOldLeftMargin then
   begin
//   	f_AdjustLeftMargin;       // 执行速度的瓶颈，千万不可！
   	FEditor.Perform (EM_SETMARGINS, EC_LEFTMARGIN, MakeLParam(i_leftmargin, 0));
      FOldLeftMargin := i_leftmargin;
      FEditor.Redraw;          // 必须，否则在行号区会残留一些如光标一类的幻影
   end;

   GetClientRect( FEditor.handle, cr);
   BarRect := cr;
   BarRect.Right := BarRect.Left + i_barwidth;
   ClientHeight := cr.bottom - cr.top;     //获取RichEdit的客户区高度

   hdcEdit := GetDC ( FEditor.handle );      //获取RichEdit的Dc
   hdcBmp := CreateCompatibleBitmap ( hdcEdit, i_barwidth, ClientHeight );     //创建与RichEdit兼容的位图Dc，用来显示行号。
   SelectObject( FhdcCpb, hdcBmp );      //将位图dc选入RichEdit环境中
   FillRect ( FhdcCpb, BarRect, FBrush.handle );      //填充显示行号dc的背景颜色。

   i_right := BarRect.Right - i_halfcharwidth; // 行号右侧适当留空
   for i_line := i_FirstLine + 1 to i_LineCount do    //在位图dc中循环输出行号
   begin
      s := IntToStr(i_line);
      TextOutW (FhdcCpb, i_right, i_y, pwidechar(s), Length(s));
      Inc (i_y, chHeight);
      if ( i_y > ClientHeight) then break;
   end;

   // 将已"画好"的位图真正"贴"到RichEdit中
   BitBlt ( hdcEdit, 0, 0, i_barwidth, ClientHeight, FhdcCpb, 0, 0, SRCCOPY );
   DeleteObject( hdcBmp );
   cr.Right := i_barwidth;
   ValidateRect (FEditor.handle, @cr);
   ReleaseDC( FEditor.handle, hdcEdit );
end;

//------------------------

procedure TxlRichEditHighlightSelLine.DoOnNotify (code, lparam: dword);
begin
	if code = EN_SelChange then
   	DoBeforePaint;
end;

procedure TxlRichEditHighlightSelLine.DoBeforePaint ();
	procedure f_Erase ();
   begin
   	InvalidateRect (FEditor.handle, @FRect, true);     // 擦除先前的高亮痕迹
	end;
var o_point: TPoint;
	i, j: integer;
begin
   GetCaretPos (o_point);
   if o_point.y <> FRect.Top then    // 防止频繁erase而闪烁
   	f_Erase
   else
   begin
   	i := FEditor.Perform (EM_LINEINDEX, FEditor.LineNumber, 0);
      j := FEditor.GetCharIndex (o_point.x, o_point.y);
      if j - i <= 1 then    // 无此在一行中输入第一字符时会出现两行高亮的问题
      	f_Erase;
   end;
end;

procedure TxlRichEditHighlightSelLine.DoPaint ();
var o_TextRect: TRect;
   hdcBmp: HBITMAP;      //RichEdit兼容的位图dc
   hdcEdit: HDC;
   o_pos: TPos;
   o_point: TPoint;
begin
   o_textrect := FEditor.TextRect;
   GetCaretPos (o_point);
   if not InRange (o_point.y, o_textrect.top, o_textrect.bottom) then exit;

   hdcEdit := GetDC (FEditor.handle);
   hdcBmp := f_CreateBmp (hdcEdit, o_pos);
   SelectObject( FhdcCpb, hdcBmp );      //将位图dc选入RichEdit环境中
   FillRect( FhdcCpb, PosToRect (o_pos), FBrush.handle );

   o_pos.x := o_TextRect.left;
   o_pos.y := o_point.y;
   BitBlt ( hdcEdit, o_pos.x, o_pos.y, o_pos.width, o_pos.height, FhdcCpb, 0, 0, SRCAND );
   DeleteObject( hdcBmp );
   o_pos.height := o_pos.height * 3;     // 不乘3则擦不干净
   FRect := PosToRect (o_pos);
   ReleaseDC (FEditor.handle, hdcEdit);
end;

//----------------------

procedure TxlRichEditHLSuper.f_HighlightText (hdcEdit: HDC; i_start, i_end, i_paintheight: integer);
	function f_GetRightX (i_y: integer): integer;
   var i, j, i_width: integer;
   	s: widestring;
   begin
   	j := i_y + FEditor.LineHeight;
      i := FEditor.GetCharIndex (0, j);
      while j > i_y do
      begin
      	dec (i);
         FEditor.GetCharXY (i, result, j);
      end;
      s := FEditor.GetTextBlock (i, 1);

//      GetTextExtentPoint32W (hdcEdit, pwidechar(s), 1, sz);
      GetCharWidth32W (hdcEdit, Ord(s[1]), Ord(s[1]), i_width);
      inc (result, i_width + 5);
   end;
var i_x, i_y, i_width, i_firstx, i_firsty, i_lastx, i_lasty, i_lineheight: integer;
	o_textrect: TRect;
   tm: TTextMetric;
begin
	o_textrect := FEditor.TextRect;
   i_lineheight := FEditor.LineHeight;
   if i_paintheight <= 0 then
      i_paintheight := i_lineheight;
   FEditor.GetCharXY (i_start, i_firstx, i_firsty);
   FEditor.GetCharXY (i_end, i_lastx, i_lasty);

   GetTextMetrics (hdcEdit, tm);

   i_y := i_firsty;
   while i_y <= i_lasty do
   begin
      if i_y = i_firsty then // 第一行
      begin
         i_x := i_firstx;
         if i_firsty = i_lasty then
         	i_Width := i_lastx - i_firstx
         else
         	i_width := f_GetRightX (i_y) - i_firstx; // + tm.tmMaxCharWidth *6 div 5;      // o_textrect.right
      end
      else if i_y = i_lasty then   // 最后一行
      begin
         i_x := o_textrect.Left;
         i_width := i_lastx - i_x;
      end
      else
      begin
         i_x := o_textrect.Left;
         i_width := f_GetRightX (i_y) - i_x; // + tm.tmMaxCharWidth  *6 div 5;
      end;
      BitBlt ( hdcEdit, i_x, i_y + i_lineheight - i_paintheight, i_width, i_paintheight, FhdcCpb, 0, 0, SRCAND );
      inc (i_y, i_lineheight);
   end;
end;

function TxlRichEditHLSuper.f_CreateBmp (hdcEdit: HDC; var o_pos: TPos): HBitMap;
var o_textrect: TRect;
begin
	o_textrect := FEditor.TextRect;
   o_pos.x := 0;
   o_pos.y := 0;
   o_pos.width := o_TextREct.Right - o_textrect.Left;
   o_pos.height := FEditor.LineHeight;
	result := CreateCompatibleBitmap( hdcEdit, o_pos.width, o_pos.height );     //创建与RichEdit兼容的位图Dc。
end;

procedure TxlRichEditHLSuper.f_GetVisibleText (var s_text: widestring; var i_offset: cardinal);
var i_end: cardinal;
begin
	FEditor.GetVisibleTextRange (i_offset, i_end);
   s_text := FEditor.GetTextBlock (i_offset, i_end - i_offset + 1);
end;

//----------------------

procedure TxlRichEditHighlightText.SetText (const value: widestring);
begin
	if value <> FText then
   begin
      FText := value;
      FEditor.Redraw;
   end;
end;

procedure TxlRichEditHighlightText.SetMatchCase (value: boolean);
begin
   if value <> FMatchCase then
   begin
      FMatchCase := value;
      FEditor.Redraw;
   end;
end;

procedure TxlRichEditHighlightText.DoPaint ();
var i_offset, i_len, i_pos: cardinal;
   o_pos: TPos;
   s_text, s_tar: widestring;
   hdcEdit: HDC;
   hdcBmp: HBITMAP;      //RichEdit兼容的位图dc
begin
   hdcEdit := GetDC (FEditor.handle);
   hdcBmp := f_CreateBmp (hdcEdit, o_pos);
   SelectObject( FhdcCpb, hdcBmp );      //将位图dc选入RichEdit环境中
   FillRect( FhdcCpb, PosToRect(o_pos), FBrush.handle );

	f_GetVisibleText (s_text, i_offset);

   s_tar := FText;
   i_len := length(s_tar);
   if not FMatchCase then
   begin
      s_tar := lowercase (s_tar);
      s_text := lowercase (s_text);
   end;

   i_pos := FirstPos (s_tar, s_text);
   while i_pos > 0 do
   begin
   	f_HighlightText (hdcEdit, i_pos + i_offset - 1, i_pos + i_offset + i_len - 1, FThickness);
      i_pos := FirstPos (s_tar, s_text, i_pos + i_len);
   end;

   DeleteObject( hdcBmp );
   ReleaseDC (FEditor.handle, hdcEdit);
end;

//---------------------------

constructor TxlRichEditHighlightTextBlock.Create (AEditor: TxlRichEdit);
begin
	inherited Create (AEditor);
   FStack := TxlIntStack.Create;
end;

destructor TxlRichEditHighlightTextBlock.Destroy ();
begin
	FStack.free;
   inherited;
end;

function TxlRichEditHighlightTextBlock.IndexValid (index: integer): boolean;
begin
	result := InRange (index, 0, Length(FSchemes) - 1);
end;

procedure TxlRichEditHighlightTextBlock.SetScheme (index: integer; const value: TULScheme);
begin
	if IndexValid (index) then
      FSchemes[index] := value
   else
   	AddScheme (value);
end;

function TxlRichEditHighlightTextBlock.GetScheme (index: integer): TULScheme;
begin
	if IndexValid (index) then
		result := FSchemes[index];
end;

function TxlRichEditHighlightTextBlock.AddScheme (const value: TULScheme): integer;  // return index
var n: integer;
begin
	n := Length (FSchemes);
	SetLength (FSchemes, n + 1);
   FSchemes[n] := value;
end;

function TxlRichEditHighlightTextBlock.AddScheme (rc: widechar; cl: TColor; tn: integer = 1): integer;
var o_scheme: TULScheme;
begin
	with o_scheme do
   begin
      RightChar := rc;
      Color := cl;
      Thickness := tn;
   end;
   result := AddScheme (o_scheme);
end;

procedure TxlRichEditHighlightTextBlock.DoPaint ();
   function f_IsRightChar (c: widechar; var sc: integer): boolean;
   var i: integer;
   begin
   	result := false;
      for i := Low(FSchemes) to High(FSchemes) do
      	if c = FSchemes[i].RightChar then
         begin
         	result := true;
            sc := i;
            break;
         end;
   end;
var i_offset: cardinal;
	i, sc, lp: integer;
   o_pos: TPos;
   s_text: widestring;
   hdcEdit: HDC;
   hdcBmp: HBITMAP;      //RichEdit兼容的位图dc
begin
   hdcEdit := GetDC (FEditor.handle);
   hdcBmp := f_CreateBmp (hdcEdit, o_pos);
   SelectObject( FhdcCpb, hdcBmp );      //将位图dc选入RichEdit环境中

   f_GetVisibleText (s_text, i_offset);
   FStack.Clear;

   for i := 1 to Length(s_text) do
   	if s_text[i] = LeftChar then
      	FStack.Push (i)
      else if (f_IsRightChar (s_text[i], sc) and FStack.Pop (lp)) then
      begin
         FBrush.Color := FSchemes[sc].Color;
         FillRect( FhdcCpb, PosToRect(o_pos), FBrush.handle );
         f_HighlightText (hdcEdit, lp + i_offset, i + i_offset - 1, FSchemes[sc].Thickness);
      end;

   DeleteObject( hdcBmp );
   ReleaseDC (FEditor.handle, hdcEdit);
end;

class function TxlRichEditHighlightTextBlock.LeftChar (): widechar;
begin
	result := #28; //#1, #127;
end;

end.

