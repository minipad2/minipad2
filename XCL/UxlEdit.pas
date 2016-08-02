unit UxlEdit;

interface

uses Windows, Messages, UxlWinControl, UxlClasses, UxlFunctions, UxlStrUtils, CommDlg;

type TxlEditControl = class (TxlControl)
private
   procedure f_SetReadOnly (b_readonly: boolean);
   procedure f_SetModify (value: boolean);
   function f_GetModify (): boolean;
   procedure f_SetSelText (const value: widestring);
	function f_PeekLineText (l_line: integer; var s: widestring; var c: widechar; var m: integer): integer;
protected
	FOnChange: TNotifyEvent;
   FReadOnly: boolean;
	FTabStops: integer;
   FWordWrap: boolean;
   procedure OnCreateControl (); override;
   procedure ReCreate (); override;
   procedure f_SetWordWrap (value: boolean);
   procedure f_SetTabStops (value: integer);
   function f_GetLineNumber (): integer; 
   function f_GetLineCharIndex (i_line: integer): integer;
   function f_GetSelText (): widestring; virtual;
   function f_GetSelLineText (): widestring;
   procedure f_SetSelLineText (const value: widestring);
   function f_GetLine (i_linenumber: integer): widestring;
   procedure f_OnChange ();

   property TabStops: integer read FTabStops write f_SetTabStops;
   property LineNumber: integer read f_GetLineNumber;     // 包括wordwrap造成的非物理行
   property LineText: widestring read f_GetSelLineText write f_SetSelLineText;      // 物理行
   property Lines[index: integer]: widestring read f_GetLine; default;
public
   procedure Clear ();
   procedure Undo ();
   procedure Redo (); virtual;
   procedure SelectAll ();
   procedure Cut ();
   procedure Copy ();
   procedure Paste ();
   procedure DeleteSel ();
   
   function CanClear (): boolean;
   function CanUndo (): boolean;
   function CanRedo (): boolean; virtual;
//   function CanSelAll (): boolean;
   function CanCut (): boolean;
   function CanCopy (): boolean;
   function CanPaste (): boolean;
   function IsEmpty (): boolean;
   procedure GetSel (var i_start, i_length: integer); virtual;
   procedure SetSel (i_start, i_length: integer); virtual;
   procedure LocateLine (var i_start, i_end: integer; i_linenumber: integer = -1);
   function TextCount (b_withcrlf: boolean = true): cardinal; virtual;
   function LineCount (): cardinal;

   procedure ClearUndoBuffer ();
   procedure AddLine (const s: widestring = ''; b_noemptyline: boolean = false);
   procedure LoadFromFile (const s_file: widestring);
   function ProcessCommand (dwEvent: WORD): DWORD; override;

	property WordWrap: boolean read FWordWrap write f_setwordwrap;
   property OnChange: TNotifyEvent read FOnChange write FOnchange;
   property ReadOnly: boolean read FReadOnly write f_setreadonly;
   property MOdified: boolean read f_getmodify write f_setmodify;
   property SelText: widestring read f_GetSelText write f_SetSelText;
end;

type TxlEdit = class (TxlEditControl)
protected
	function DoCreateControl (HParent: HWND): HWND; override;
public
end;

type TMaskType = (msNumber, msPassword);

type TxlMaskEdit = class (TxlEdit)
public
   procedure SetMaskType (ms: TMaskType);
end;

type TxlMemo = class (TxlEditControl)
private
protected
	function DoCreateControl (HParent: HWND): HWND; override;
public
//	property WordWrap: boolean read FWordWrap write f_setwordwrap;
   property TabStops: integer read FTabStops write f_SetTabStops;
   property LineNumber: integer read f_GetLineNumber;
   property LineText: widestring read f_GetSelLineText write f_SetSelLineText;
   property Lines[index: integer]: widestring read f_GetLine; default;
end;

implementation

uses UxlCommDlgs, UxlMath, UxlList;

procedure TxlEditControl.OnCreateControl ();
begin
	FTabStops := 8;
end;

procedure TxlEditControl.ReCreate ();
var b_modified: boolean;
	b_readonly: boolean;
   i_tabstops: integer;
begin
   b_modified := Modified;
   b_readonly := ReadOnly;
   i_tabstops := TabStops;

   inherited ReCreate ();

   ReadOnly := b_ReadOnly;
   TabStops := i_TabStops;
   Modified := b_modified;
end;

procedure TxlEditControl.f_SetWordWrap (value: boolean);
begin
	if FWordWrap <> value then
   begin
      FWordWrap := value;
      ReCreate();
   end;
end;

procedure TxlEditControl.f_SetReadOnly (b_readonly: boolean);
begin
	Perform (EM_SETREADONLY, BoolToInt(b_readonly), 0);
   FReadOnly := b_readonly;
   f_OnChange;
end;

procedure TxlEditControl.f_SetModify (value: boolean);
begin
	Perform (EM_SETMODIFY, BoolToInt(value), 0);
end;

function TxlEditControl.f_GetModify (): boolean;
begin
   result := IntToBool (Perform (EM_GETMODIFY, 0, 0));
end;

function TxlEditControl.f_PeekLineText (l_line: integer; var s: widestring; var c: widechar; var m: integer): integer;
var n: integer;
begin
   m := f_GetLineCharIndex (l_line);
   n := 0;
   if m < TextCount (false) then
      n := Perform (EM_LINELENGTH, m, 0);
   setlength (s, n + 1);
   pword(s)^ := n + 1;
   Perform (EM_GETLINE, l_line, dword(pwidechar(s)));
   c := s[n + 1];
   setlength (s, n);
   result := n;
end;

function TxlEditControl.f_GetSelLineText (): widestring;
begin
	result := f_GetLine (LineNumber);
end;

procedure TxlEditControl.f_SetSelLineText (const value: widestring);
var i_start, i_end: integer;
begin
	LocateLine (i_start, i_end);
	SetSel (i_start, i_end - i_start);
   SelText := value;
end;

function TxlEditControl.f_GetLine (i_linenumber: integer): widestring;
var i, j, m, n: integer;
	s: widestring;
   c: widechar;
begin
	result := '';
	if i_linenumber >= LineCount then exit;

   n := Perform (EM_GETLINECOUNT, 0, 0);
   i := i_linenumber;
   j := i;
   repeat
      f_PeekLineText (j, s, c, m);
      result := result + s;
      inc (j);
   until (c = #13) or (c = #11) or (j = n);
   while i > 0 do
   begin
      dec (i);
      f_PeekLineText (i, s, c, m);
      if (c = #13) or (c = #11) then break;
      result := s + result;
   end;
end;

procedure TxlEditControl.LocateLine (var i_start, i_end: integer; i_linenumber: integer = -1);
var i, j, n, o, i_len: integer;
	s: widestring;
   c: widechar;
begin
   n := Perform (EM_GETLINECOUNT, 0, 0);
   if i_linenumber < 0 then i_linenumber := LineNumber;
   i := i_linenumber;
   j := i;
   repeat
      i_len := f_PeekLineText (j, s, c, o);
      if j = i then i_start := o;
      i_end := o + i_len;
      inc (j);
   until (c = #13)  or (c = #11) or (j = n);
	while i > 0 do
   begin
   	dec (i);
      f_PeekLineText (i, s, c, o);
      if (c = #13) or (c = #11) then break;
      i_start := o;
   end;
end;

function TxlEditControl.f_GetLineNumber (): integer;
var o_point, o_point2: TPoint;
	i_char: integer;
begin
//   result := Perform (EM_LINEFROMCHAR, dword(-1), 0);    // 对于最后一行，会有问题
	GetCaretPos (o_point);
   i_char := Perform (EM_CHARFROMPOS, 0, lparam(@o_point));
   result := Perform (EM_LINEFROMCHAR, i_char, 0);
   Perform (EM_POSFROMCHAR, wparam(@o_point2), i_char);
   if o_point2.y < o_point.y then inc (result);
end;

function TxlEditControl.f_GetLineCharIndex (i_line: integer): integer;
begin
	result := Perform (EM_LINEINDEX, i_line, 0);
end;

procedure TxlEditControl.f_SetTabStops (value: integer);
var pi: pdword;
begin
	new (pi);
	pi^ := value * 5;
	Perform (EM_SETTABSTOPS, 1, dword(pi));
   dispose (pi);
   FTabStops := value;
end;

procedure TxlEditControl.Clear ();
begin
	SetRedraw (false);
	SelectAll ();
   Perform (WM_CLEAR, 0, 0);
   SetRedraw (true);
end;

procedure TxlEditControl.Undo ();
begin
	Perform (EM_UNDO, 0, 0);
end;

procedure TxlEditControl.Redo ();
begin
	Undo ();
end;

procedure TxlEditControl.SelectAll ();
begin
   SetFocus;
	SetSel (0, TextLength);
end;

procedure TxlEditControl.Cut ();
begin
	Perform (WM_CUT, 0, 0);
end;

procedure TxlEditControl.Copy ();
begin
	Perform (WM_COPY, 0, 0);
end;

procedure TxlEditControl.Paste ();
begin
	Perform (WM_PASTE, 0, 0);
end;

procedure TxlEditControl.DeleteSel ();
begin
	Perform (WM_CLEAR);
end;

function TxlEditControl.CanClear (): boolean;
begin
	result := (not readonly) and (not IsEmpty);
end;

function TxlEditControl.CanUndo (): boolean;
begin
	result := (Perform(EM_CANUNDO, 0, 0) <> 0);
end;

function TxlEditControl.CanRedo (): boolean;
begin
	result := CanUndo ();
end;

//function TxlEditControl.CanSelAll (): boolean;
//var i_start, i_sellength: integer;
//begin
//   if IsEmpty then
//   	result := false
//   else
//   begin
//      GetSel (i_start, i_sellength);
//      if i_start > 0 then
//         result := true
//      else
//         result := (i_sellength < TextCount);
//   end;
//end;

function TxlEditControl.CanCut (): boolean;
begin
	result := (not ReadOnly) and CanCopy;
end;

function TxlEditControl.CanCopy (): boolean;
var i, j: integer;
begin
   GetSel (i, j);
   result := not (j = 0);
end;

function TxlEditControl.CanPaste (): boolean;
begin
	result := (not ReadOnly) and IsClipboardFormatAvailable (CF_UNICODETEXT);
end;

function TxlEditControl.IsEmpty (): boolean;
begin
	result := (TextLength <= 0);
end;

function TxlEditControl.TextCount (b_withcrlf: boolean = true): cardinal;
begin
   result := length (Text);
end;

function TxlEditControl.LineCount (): cardinal;
begin
	result := Perform ( EM_GETLINECOUNT, 0, 0 );
end;

procedure TxlEditControl.ClearUndoBuffer ();
begin
	Perform (EM_EMPTYUNDOBUFFER, 0, 0);
end;

procedure TxlEditControl.GetSel (var i_start, i_length: integer);
var i_end: integer;
begin
   Perform (EM_GETSEL, dword(@i_start), dword(@i_end));
   i_length := i_end - i_start;
end;

procedure TxlEditControl.SetSel (i_start, i_length: integer);
begin
   Perform (EM_SETSEL, i_start, i_start + i_length);
end;

function TxlEditControl.f_GetSelText (): widestring;
var i_start, i_length: integer;
begin
   GetSel (i_start, i_length);
   result := MidStr (text, i_start, i_length);
end;

procedure TxlEditControl.f_SetSelText (const value: widestring);
begin
   Perform (EM_REPLACESEL, 1, dword(pwidechar(value)));
end;

procedure TxlEditcontrol.AddLine (const s: widestring = ''; b_noemptyline: boolean = false);
var s_text: widestring;
begin
	s_text := Text;
   if b_noemptyline then
   	while rightstr(s_text, 2) = #13#10 do
      	s_text := leftstr (s_text, length(s_text) - 2);
	if (s_text <> '') and (rightstr(s_text, 2) <> #13#10) then
      s_text := s_text + #13#10;
   s_text := s_text + s;
   Text := s_text;
end;

procedure TxlEditControl.LoadFromFile (const s_file: widestring);
var sl: TxlStrList;
begin
   sl := TxlStrList.Create;
   sl.LoadFromFile (s_file);
   self.Text := sl.Text;
   sl.free;
end;

function TxlEditControl.ProcessCommand (dwEvent: WORD): DWORD;
begin
	result := 0;
	case dwEvent of
   	EN_UPDATE:
      	f_OnChange;
   end;
end;

procedure TxlEditControl.f_OnChange ();
begin
	if assigned (FOnChange) then FOnChange (self);
end;

//-----------------------

function TxlEdit.DoCreateControl (HParent: HWND): HWND;
begin
   result := CreateWin32Control (HParent, 'edit', ES_LEFT or ES_AUTOHSCROLL, WS_EX_CLIENTEDGE);
end;

//------------------------

procedure TxlMaskEdit.SetMaskType (ms: TMaskType);
begin
	SetWndStyle (ES_Number, (ms = msNumber));
   SetWndStyle (ES_Password, (ms = msPassword));
end;

//------------------------

function TxlMemo.DoCreateControl (HParent: HWND): HWND;
var i_style: DWord;
begin
   i_Style := ES_LEFT or ES_MULTILINE or ES_AUTOVSCROLL or ES_WANTRETURN or WS_VSCROLL;
   if not FWordWrap then i_style := i_style or ES_AUTOHSCROLL or WS_HSCROLL;
   result := CreateWin32Control (HParent, 'edit', i_style, WS_EX_CLIENTEDGE);
end;

end.







