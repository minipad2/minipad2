unit UxlRichEdit;

interface

uses Windows, Messages, RichEdit, UxlEdit, UxlClasses, UxlList, UxlWinControl, CommDlg;

type
   TTextStruct = record
      pText: Pwidechar;
      nSize: longint;
      StreamIn: boolean;
   end;
   PTextStruct = ^TTextStruct;

	TKeyEvent = function (key: DWORD): boolean of object;

	IRichEditDecorator = interface
   	procedure BeforePaint ();
      procedure Paint ();
      procedure OnNotify (code, lparam: dword);
      procedure EditorRecreated ();
   end;

   TxlRichEdit = class (TxlEditControl)
   private
      FProtected: boolean;
      FProtectedCount: cardinal;

      FUndoLimit: integer;
      FAutoIndent: boolean;
      FAutoEmptyLine: boolean;
      FLeftMargin: cardinal;
      FRightMargin: cardinal;
      FSmoothScroll: boolean;
      FOneClickOpenLink: boolean;
      
   	FDecorators: TxlInterfaceList;

      FOnContextMenu: TDemandEvent;
      FOnKeyDown: TKeyEvent;
      FOnKeyPress: TKeyEvent;
      FOnLink: TStringEvent;

      procedure f_SetUndoLimit (value: integer);
      procedure f_SetProtected (value: boolean);
      procedure SetLeftMargin (value: cardinal);
      procedure SetRightMargin (value: cardinal);
      procedure SetScrollPos (value: TPoint);
      function GetScrollPos (): TPoint;
      procedure SetFirstVisibleLine (value: dword);
      function GetFirstVisibleLine (): dword;
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure ReCreate (); override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
      function f_GetSelText (): widestring; override;
      function GetText (): widestring; override;
      procedure SetText (const s_text: widestring); override;
      procedure OnFontChange (Sender: TObject); override;
      procedure SetColor (i_color: TColor); override;
   public
      constructor create (WndParent: TxlWinContainer; h_handle: HWND = 0); override;
      destructor destroy (); override;
      function ProcessNOtify (code: integer; lParam: dword): dword; override;

      procedure Redo (); override;
      function CanRedo (): boolean; override;
      procedure GetSel (var i_start, i_length: integer); override;
      procedure SetSel (i_start, i_length: integer); override;
      function TextCount (b_withcrlf: boolean = true): cardinal; override;
      function GetTextBlock (i_start, i_length: cardinal): widestring;
      procedure GetVisibleTextRange (var i_start, i_end: cardinal);
		procedure GetCharXY (index: integer; var i_x, i_y: integer);
      function GetCharIndex (i_x, i_y: integer): integer;
      function TextRect (): TRect;
      function LineHeight (): integer;

      procedure AddDecorator (value: IRichEditDecorator);
      procedure RemoveDecorator (value: IRichEditDecorator);

      function CanIndent (): boolean;
      procedure Indent (b_indent: boolean = true; s_prefix: widestring = '');

      property LeftMargin: cardinal read FLeftMargin write SetLeftMargin;
      property RightMargin: cardinal read FRightMargin write SetRightMargin;
      property TabStops: integer read FTabStops write f_SetTabStops;
      property LineNumber: integer read f_GetLineNumber;
      property LineText: widestring read f_GetSelLineText write f_SetSelLineText;
      property Lines[index: integer]: widestring read f_GetLine; default;
      property UndoLimit: integer read FUndoLimit write f_SetUndoLimit;
      property AutoIndent: boolean read FAutoIndent write FAutoIndent;
      property AutoEmptyLine: boolean read FAutoEmptyLine write FAutoEmptyLine;
      property Protected: boolean read FProtected write f_SetProtected;
      property ScrollPos: TPoint read GetScrollPos write SetScrollPos;
      property FirstVisibleLine: dword read GetFirstVisibleLine write SetFirstVisibleLine;
      property SmoothScroll: boolean read FSmoothScroll write FSmoothScroll;
      property OneClickOpenLink: boolean read FOneClickOpenLink write FOneClickOpenLink;

      property OnContextMenu: TDemandEvent read FOnContextMenu write FOnContextMenu;
      property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
      property OnKeyPress: TKeyEvent read FOnKeyPress write FOnKeyPress;
      property OnLink: TStringEvent read FOnLink write FOnLink;
   end;

function EditStreamCallBack (dwCookie: DWORD; pbBuff: PBYTE; cb: longint; var pcb: longint): dword; stdcall;

implementation

uses SysUtils, UxlFunctions, UxlStrUtils, UxlCommDlgs, UxlMath, UxlWinDef, UxlDateTimeUtils;

constructor TxlRichEdit.create (WndParent: TxlWinContainer; h_handle: HWND = 0);
begin
	inherited Create (WndParent, h_handle);
   FDecorators := TxlInterfaceList.Create;
end;

destructor TxlRichEdit.destroy ();
begin
	FDecorators.free;
   inherited;
end;

function TxlRichEdit.DoCreateControl (HParent: HWND): HWND;
var i_style: DWord;
begin
   i_Style := ES_LEFT or ES_MULTILINE or ES_AUTOVSCROLL or ES_WANTRETURN or WS_VSCROLL or ES_NOHIDESEL;
   if not FWordWrap then i_style := i_style or ES_AUTOHSCROLL or WS_HSCROLL;
   result := CreateWin32Control (HParent, 'RichEdit20W', i_style, WS_EX_CLIENTEDGE);
end;

procedure TxlRichEdit.OnCreateControl ();
begin
	inherited OnCreateControl;

  	Perform (EM_SETTEXTMODE, TM_PLAINTEXT, 0);
   Perform (EM_AUTOURLDETECT, 1, 0);

	Perform (EM_SETEVENTMASK, 0, ENM_SELCHANGE or ENM_Change or ENM_LINK or ENM_SCROLL or ENM_PROTECTED);  // or ENM_DROPFILES );
   Perform (EM_EXLIMITTEXT, 0, dword(-1));

   SetWndProc (@WindowProc);
   Redraw;
end;

procedure TxlRichEdit.ReCreate ();
var s_text: widestring;
	i: integer;
begin
   s_text := Text;    // 针对由wordwrap导致的recreate, 此时控件中可能已有文字，执行settextmode可能失败
   Text := '';
	inherited ReCreate ();
   LeftMargin := FLeftMargin;
   RightMargin := FRightMargin;
   for i := FDecorators.Low to FDecorators.High do
      IRichEditDecorator(FDecorators[i]).EditorReCreated ();

   Text := s_text;
end;

procedure TxlRichEdit.AddDecorator (value: IRichEditDecorator);
begin
	FDecorators.Add(value);
end;

procedure TxlRichEdit.RemoveDecorator (value: IRichEditDecorator);
begin
	FDecorators.Remove(value);
end;

procedure TxlRichEdit.Redo ();
begin
	Perform (EM_REDO, 0, 0);
end;

function TxlRichEdit.CanRedo (): boolean;
begin
	result := (Perform(EM_CANREDO, 0, 0) <> 0);
end;

procedure TxlRichEdit.f_SetUndoLimit (value: integer);
begin
	FUndoLimit := value;
   Perform (EM_SETUNDOLIMIT, value, 0);
end;

function TxlRichEdit.f_GetSelText (): widestring;
var i_start, i_length: integer;
begin
	GetSel (i_start, i_length);
   if i_length <= 0 then
   	result := ''
   else
   begin
      SetLength (result, i_length);
      Perform (EM_GETSELTEXT, 0, lparam(pwidechar(result)));
   end;
end;

procedure TxlRichEdit.GetSel (var i_start, i_length: integer);
var cr: TCharRange;
begin
   Perform (EM_EXGETSEL, 0, lparam(@cr));
   i_start := cr.cpMin;
   i_length := cr.cpMax - cr.cpMin;
end;

procedure TxlRichEdit.SetSel (i_start, i_length: integer);
var cr: TCharRange;
begin
   cr.cpMin := i_start;
   cr.cpMax := i_start + i_length;
   Perform (EM_EXSETSEL, 0, lparam(@cr));
end;

function TxlRichEdit.TextCount (b_withcrlf: boolean = true): cardinal;
var o_lenex: TGETTEXTLENGTHEX;
begin
   o_lenex.flags := GTL_PRECISE or GTL_NUMCHARS;
   if b_withcrlf then o_lenex.flags := o_lenex.flags or GTL_USECRLF;
   o_lenex.codepage := 1200;   // unicode
   result := Perform (EM_GETTEXTLENGTHEX, wparam(@o_lenex), 0);
end;

function TxlRichEdit.GetTextBlock (i_start, i_length: cardinal): widestring;
var o_range: TTextRangeW;
begin
   SetLength (result, i_length + 1);
   o_range.chrg.cpMin := i_start;
   o_range.chrg.cpMax := i_start + i_length;
   o_range.lpstrText := pwidechar(result);
   Perform (EM_GETTEXTRANGE, 0, lparam(@o_range));
   result := pwidechar(result);
end;

procedure TxlRichEdit.GetVisibleTextRange (var i_start, i_end: cardinal);
var o_textrect: TRect;
	o_point: TPoint;
begin
   o_textrect := TextRect;
   o_point := RectToPoint (o_textRect);
   i_start := Perform (EM_CHARFROMPOS, 0, dword(@o_point));
   o_point.x := o_TextRect.right;
   o_point.y := o_TextRect.Bottom;
   i_end := Perform (EM_CHARFROMPOS, 0, dword(@o_point));
end;

procedure TxlRichEdit.SetLeftMargin (value: cardinal);
begin
   FLeftMargin := value;
   Perform (EM_SETMARGINS, EC_LEFTMARGIN, MakeLParam(value, 0));
end;

procedure TxlRichEdit.SetRightMargin (value: cardinal);
begin
   FRightMargin := value;
  	Perform (EM_SETMARGINS, EC_RIGHTMARGIN, MakeLParam(0, value));
end;

procedure TxlRichEdit.SetScrollPos (value: TPoint);
begin
	value.Y := ConfineRange (value.y, 0, LineCount * LineHeight - textrect.Bottom);
   Perform (EM_SETSCROLLPOS, 0, lparam(@value));
end;

function TxlRichEdit.GetScrollPos (): TPoint;
begin
	Perform (EM_GETSCROLLPOS, 0, lparam(@result));
end;

procedure TxlRichEdit.SetFirstVisibleLine (value: dword);
begin
	SetSel (0, 0);
   Perform ( EM_GETLINECOUNT, 0, 0 );  // 无此句，在载入文字后恢复滚动位置会出问题，可能是因为文字未载入完全系统就尝试滚动的缘故
	Perform (EM_LINESCROLL, 0, value);
end;

function TxlRichEdit.GetFirstVisibleLine (): dword;
begin
	result := Perform (EM_GETFIRSTVISIBLELINE, 0, 0);
end;

function TxlRichEdit.TextRect (): TRect;
begin
	Perform (EM_GETRECT, 0, dword(@result));
end;

procedure TxlRichEdit.GetCharXY (index: integer; var i_x, i_y: integer);
var o_point: TPoint;
begin
   Perform (EM_POSFROMCHAR, dword(@o_point), index);
   i_x := o_point.X;
   i_y := o_point.y;
end;

function TxlRichEdit.GetCharIndex(i_x, i_y: integer): integer;
var o_point: TPoint;
begin
   o_point.x := i_x;
   o_point.y := i_y;
   result := Perform (EM_CHARFROMPOS, 0, dword(@o_point));
end;

function TxlRichEdit.LineHeight (): integer;
var i, j, k: integer;
begin
	GetCharXY (0, k, i);
   GetCharXY (f_GetLineCharIndex(1), k, j);
   result := j - i;
   if result <= 0 then
      result := -1 * Font.Height;
end;

type PEnLink = ^TEnLink;

function TxlRichEdit.ProcessNotify (code: integer; lParam: dword): dword;
var p: PEnLink;
	s: widestring;
   i, n: integer;
   enp: TENPROTECTED;
begin
	result := 0;
   case code of
	   EN_SelChange, EN_Change:
      	begin
         	f_OnChange;
            for i := FDecorators.Low to FDecorators.High do
               IRichEditDecorator(FDecorators[i]).OnNotify (code, lparam);
         end;
      EN_LINK:
      	begin
      		p := PEnLink(lParam);
            if (p^.msg = WM_LBUTTONDBLCLK) or ((p^.msg = WM_LBUTTONDOWN) and (FOneClickOpenLink or KeyPressed(VK_CONTROL))) then
            begin
            	s := GetTextBlock (p^.chrg.cpMin, p^.chrg.cpMax - p^.chrg.cpMin);
               if assigned (FOnLink) then
                  FOnLink (s)
               else
                  try
                     ExecuteLink (s);
                  except
                  end;
            end;
         end;
      EN_PROTECTED:
      	begin
         	if not FProtected then exit;
            enp := PENPROTECTED(lparam)^;
            n := FProtectedCount;
            if (enp.msg = WM_KEYDOWN) and (enp.wParam = VK_BACK) then
               inc (n);
      		result := ifThen ( enp.chrg.cpMin < n, 1, 0);
      	end;
      else
         result := inherited ProcessNotify (code, lparam);
	end;
end;

//---------------------

procedure TxlRichEdit.SetColor (i_color: TColor);
begin
	inherited SetColor (i_color);
	Perform (EM_SETBKGNDCOLOR, 0, i_color);
end;

procedure TxlRichEdit.OnFontChange (Sender: TObject);
var n: integer;
   FCharFormat: TCharFormatW;
   s_name: widestring;
begin
   with FCharFormat do
   begin
   	cbSize := sizeof (FCharFormat);
      dwMask := CFM_BOLD or CFM_COLOR or CFM_ITALIC or CFM_SIZE or CFM_STRIKEOUT or CFm_UNDERLINE or CFM_FACE or CFM_OFFSET or CFM_PROTECTED;
      dwEffects := 0;
      if font.bold then
      	dwEffects := dwEffects or CFE_BOLD;
      if font.italic then dwEffects := dwEffects or CFE_ITALIC;
      if font.strikeout then dwEffects := dwEffects or CFE_STRIKEOUT;
      if font.underline then dwEffects := dwEffects or CFE_UNDERLINE;
      if FProtected then dwEffects := dwEffects or CFE_PROTECTED;
//      bCharSet := DEFAULT_CHARSET;   // 千万不能用！！！
      yHeight := font.size * 20;
      crTextColor := font.Color;
      yOffset := 0;
//      bPitchAndFamily := o_logfont.lfPitchAndFamily;
   end;
   s_name := Font.Name;
   n := length(s_name)+1;
   copymemory (@FCharFormat.szFaceName, pwidechar(s_name), n*2);

   Perform (EM_SETCHARFORMAT, SCF_ALL, lparam(@FCharFormat));
end;

procedure TxlRichEdit.f_SetProtected (value: boolean);
begin
	FProtected := value;
   if value then
   	FProtectedCount := TextCount (false)
   else
   	FProtectedCount := 0;
	OnFontChange (self);        // 不可省略！
end;

function TxlRichEdit.CanIndent (): boolean;
begin
   result := IsSubStr (#13, SelText);
end;

procedure TxlRichEdit.Indent (b_indent: boolean = true; s_prefix: widestring = '');
var s: widestring;
   i_start, i_len, i_start2, i_line, i_pos, n, i_prefixlen: integer;
   b_numberprefix: boolean;
begin
   s := SelText;
   b_numberprefix := s_prefix = '';

   GetSel (i_start, i_len);
   i_line := Perform (EM_LINEFROMCHAR, i_start, 0);
   i_start2 := Perform (EM_LINEINDEX, i_line, 0);
   if i_start > i_start2 then
   begin
      inc (i_len, i_start - i_start2);
      i_start := i_start2;
      SetSel (i_start, i_len);
      s := SelText;
   end;

   if i_start = i_start2 then
   	i_pos := 1
   else
   	i_pos := FirstPos (#13, s) + 1;
   n := 0;
   while InRange (i_pos, 1, i_len)  do
   begin
   	inc (n);
      if (n > 1) and (i_pos = 1) then break;

   	if b_numberprefix then
      begin
         s_prefix := IntToStr(n) + '. ';
      end;
      i_prefixlen := Length(s_prefix);

      while (i_pos < i_len) and (Ord(s[i_pos]) <= 32) do
         inc (i_pos);
         
      if b_indent then
      begin
         Insert (s, i_pos, s_prefix);
         inc (i_len, i_prefixlen);
      end
      else
      begin
      	if MidStr(s, i_pos, i_prefixlen) = s_prefix then
         begin
            Delete (s, i_pos, i_prefixlen);
            dec (i_len, i_prefixlen);
         end
         else if (i_pos > 1) and (i_prefixlen = 1) and (Ord(s_prefix[1]) <= 32) and (Ord(s[i_pos - 1]) <= 32) and (Ord(s[i_pos - 1]) <> 13) then  // shift_tab 时连空格等一并回撤
         begin
         	dec (i_pos);
            Delete (s, i_pos, 1);
            dec (i_len, 1);
         end;
      end;
      i_pos := FirstPos (#13, s, i_pos) + 1;
   end;
   SelText := s;
   SetSel (i_start, i_len);
end;

function TxlRichEdit.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var i, n: integer;
	s: widestring;
   b, b_processed: boolean;
   i_time: Int64;
begin
	result := 0;
   b_processed := false;
   case AMessage of
	   WM_KEYDOWN:
         begin
            if assigned (FOnKeyDown) and FOnKeyDown (wParam) then
               b_processed := true
            else if wParam = VK_RETURN then
            begin
               if FAutoIndent then s := LineText;
               result := inherited ProcessMessage (WM_KeyDown, WParam, LParam);
               if FAutoEmptyLine then
                  inherited ProcessMessage (WM_KeyDown, WParam, LParam);
               if s = '' then exit;
               i := 1;
               n := length(s);
               while (i <= n) and ((s[i] = #9) or (s[i] = #32)) or (s[i] = '　') do   // 包含中文空格
               begin
                  Perform (WM_CHAR, dword(s[i]), 0);
                  inc (i);
               end;
               b_processed := true;
            end;
         end;
      WM_CHAR:
         if (wparam = 9) and CanIndent then
         begin
         	Indent (not KeyPressed(VK_SHIFT), #9);
         	b_processed := true;
         end
         else if assigned (FOnKeyPress) then
            b_processed := FOnKeyPress (wParam);
      WM_CONTEXTMENU:
         if assigned (FOnContextMenu) then
         begin
            i_time := SystemTimeToInt64 (Now, ittMilliSecond);
            result := inherited ProcessMessage (AMessage, WParam, LParam);
            if SystemTimeToInt64 (Now, ittMilliSecond) - i_time <= 100 then  // 并未弹出系统本身的默认菜单。针对scrollbar
               FOnContextMenu (self);
            b_processed := true;
         end;
      WM_MBUTTONDOWN:
         if (wparam and MK_CONTROL) <> 0 then   // ctrl+中键单击：撤销 zoom
         begin
            Post (EM_SETZOOM, 0, 0);
            b_processed := true;
         end;
      WM_MOUSEWHEEL:
      	if not (KeyPressed(VK_CONTROL) or SmoothScroll) then
      	begin
				b_processed := true;
            b := GetWheelDelta (wParam) > 0;
            for i := 0 to 2 do
               Perform (WM_VSCROLL, IfThen (b, SB_LINEUP, SB_LINEDOWN), 0);
         END;
      WM_PAINT:
         begin
            for i := FDecorators.Low to FDecorators.High do
            	IRichEditDecorator(FDecorators[i]).BeforePaint;
            result := inherited ProcessMessage (AMessage, WParam, LParam);
            for i := FDecorators.Low to FDecorators.High do
               IRichEditDecorator(FDecorators[i]).Paint;
            b_processed := true;
//            ValidateRect (Fhandle, nil);
         end;
      end;
   if not b_processed then
   	result := inherited ProcessMessage (AMessage, WParam, LParam);
end;

//------------------

function TxlRichEdit.GetText (): widestring;
var es: EditStream;
	ess: TTextStruct;
	i_len: integer;
begin
	i_len := TextLength;
	setlength (result, i_len);

   ess.pText := pwidechar(result);
   ess.nSize := i_len * 2;
   ess.StreamIn := false;

   with es do
   begin
   	dwCookie := dword(@ess);
   	dwError := 0;
   	pfnCallback := @EditStreamCallback;
   end;

   Perform (EM_STREAMOUT, SF_TEXT or SF_UNICODE, LParam(@es));
end;

procedure TxlRichEdit.SetText (const s_text: widestring);
var es: EditStream;
	ess: TTextStruct;
   b: boolean;
begin
	b := Protected;
   FProtected := false;

   ess.pText := pwidechar(s_text);
   ess.nSize := length(s_text) * 2;
   ess.streamIn := true;

   with es do
   begin
   	dwCookie := dword(@ess);
   	dwError := 0;
   	pfnCallback := @EditStreamCallback;
   end;

   SetRedraw (false);
  	Perform (EM_SETTEXTMODE, TM_PLAINTEXT, 0);
   Perform (EM_STREAMIN, SF_TEXT or SF_UNICODE, LParam(@es));
   SetRedraw (true);

   FProtected := b;
end;

function EditStreamCallBack (dwCookie: DWORD; pbBuff: PBYTE; cb: longint; var pcb: longint): dword; stdcall;
var pess: PTextStruct;
begin
	pess := PTextStruct(dwCookie);
   if cb > pess^.nSize then
   	pcb := pess^.nSize
   else
   	pcb := (cb div 2) * 2;

   if pcb > 0 then
   begin
      if pess^.StreamIn then
      	copymemory (pbBuff, pess^.pText, pcb)
      else
      	copymemory (pess^.pText, pbbuff, pcb);
      inc (pess^.pText, pcb div 2);
      dec (pess^.nSize, pcb);
   end;

   result := 0;
end;

//------------------

var Riched20: HModule;

initialization
	Riched20 := LoadLibraryW ('riched20.dll');

finalization
	FreeLibrary (Riched20);

end.

//      procedure SetSelColor (i_color: TColor);   // 需要把PlainText风格去除才能生效。如需实现语法高亮，实现OnPaint事件，在事件代码中逐个SetSelColor即可。

//procedure TxlRichEdit.SetSelColor (i_color: TColor);
//var FCharFormat: TCharFormatW;
//begin
//   with FCharFormat do
//   begin
//      cbSize := sizeof (FCharFormat);
//      dwMask := CFM_COLOR;
//      dwEffects := 0;
//      crTextColor := i_color;
//   end;
//   Perform (EM_SETCHARFORMAT, SCF_SELECTION, dword(@FCharFormat));
//end;
