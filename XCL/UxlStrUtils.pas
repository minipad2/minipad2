unit UxlStrUtils;

interface

uses windows, UxlClasses, UxlMath;

type TCompareResult = (crSmaller = -1, crEqual, crLarger);

function StrAlloc(Size: Cardinal): PWideChar;
function StrBufSize(const Str: PWideChar): Cardinal;
procedure StrDispose(Str: PWideChar);

function IsSameStr (const S1, s2: widestring; matchcase: boolean = false): boolean;
function CompareStr (const s1, s2: widestring): TCompareResult;   // return 1 if >, -1 if <, 0 if =
function FirstPos(const SubStr, S: widestring; Offset: Cardinal = 1): Cardinal;  // the first pos is 1
function LastPos (const SubStr, s: widestring; Offset: Cardinal = 0): Cardinal;
function IsSubStr (const SubStr, S: widestring): boolean;
function StrScan(const Str: PWideChar; Chr: WideChar): PWideChar;

function LeftStr(const s: widestring; i_count: integer): widestring;
function MidStr(const s: widestring; i_offset: integer; i_count: integer = -1): widestring;
function SubStr(const s: widestring; i_start: integer; i_end: integer = -1): widestring;
function RightStr(const s: widestring; i_count: integer): widestring;
procedure Insert (var s: widestring; Index: integer; const s_ins: widestring);
procedure Delete(var S: widestring; Index, Count: Integer); overload;
procedure Delete(var s: widestring; const s_del: widestring); overload;
procedure StrLCopy (Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal);
function ReplaceStr (const s_source, s_from, s_to: widestring): widestring;
function ReplaceStrings (const s_source: widestring; const s_from, s_to: array of widestring): widestring;
function CutText (const s: widestring; i_count: integer): widestring;     // xxx...

function Trim(const S: WideString): WideString;
function TrimLeft(const S: WideString): WideString;
function TrimRight(const S: WideString): WideString;
function TrimInside (const s: widestring): WideString;
function UpperCase(const S: widestring): widestring;
function LowerCase(const S: widestring): widestring;
function DuplicateStr (const s: widestring; i_count: cardinal): widestring;
function Space (i_count: cardinal): widestring;

function MultiLineToSingleLine (const s: widestring; b_useblank: boolean = false): widestring;
function SingleLineToMultiLine (const s: widestring): widestring;

function AnsiToUnicode (const s: AnsiString): widestring;
function UnicodeToAnsi (const s: widestring): AnsiString;

function EncryptString (const s: widestring): widestring;
function DecryptString (const s: widestring): widestring;

function FormatStr (const s_source: widestring; const StrArray: array of widestring): widestring;
function FormatInt (const s_source: widestring; IntArray: array of integer): widestring;

function IsWordBreak (c: word): boolean; overload;
function IsWordBreak (c: widechar): boolean; overload;

implementation

//---------------------

uses UxlFunctions;

function StrAlloc(Size: Cardinal): PWideChar;
begin
  Inc(Size, SizeOf(Cardinal));
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(Result, SizeOf(Cardinal));
end;

function StrBufSize(const Str: PWideChar): Cardinal;
var
  P: PWideChar;
begin
  P := Str;
  Dec(P, SizeOf(Cardinal));
  Result := Cardinal(Pointer(P)^) - SizeOf(Cardinal);
end;

procedure StrDispose(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(Str, SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

function IsSameStr (const S1, s2: widestring; matchcase: boolean = false): boolean;
var dwCompFlags: DWORD;
begin
	if matchcase then
      dwCompFlags := 0
   else
   	dwCompFlags := NORM_IGNORECASE;
  result := (CompareStringW(LOCALE_USER_DEFAULT, dwCompFlags, PWideChar(S1), Length(S1), PWideChar(S2), Length(S2)) = CSTR_EQUAL);
end;

function CompareStr (const s1, s2: widestring): TCompareResult;
//   function IsAscii (const s: widestring): boolean;
//   var i: integer;
//   begin
//      result := true;
//      for i := 1 to length(s) do
//         if Ord(s[i]) > 255 then
//         begin
//            result := false;
//            exit;
//         end;
//   end;
begin
   // 对于英文单词。如果使用comparestring函数，大写将被视为 > 小写；对于unicode中文，不能直接比较。
//   if IsAscii (s1) and IsAscii (s2) then
//   begin
//      if s1 < s2 then
//         result := crSmaller
//      else if s1 > s2 then
//         result := crLarger
//      else
//         result := crEqual;
//   end
//   else
	   result := TCompareResult (CompareStringW (LOCALE_USER_DEFAULT, 0, pwidechar(s1), length(s1), pwidechar(s2), length(s2)) - 2);
end;

function StrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function FirstPos(const SubStr, S: widestring; Offset: Cardinal = 1): Cardinal;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
   Result := 0;
   if (SubStr = '') or (S = '') then exit;
   I := Offset;
   LenSubStr := Length(SubStr);
   Len := Length(S) - LenSubStr + 1;
   while I <= Len do
   begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
   end;
end;

function LastPos (const SubStr, s: widestring; Offset: Cardinal = 0): Cardinal;
var i, k, n, x: integer;
begin
   k := length (SubStr);
   if offset <= 0 then offset := length(s);
   n := Min (offset, length(s) - k + 1);
   for i := n downto 1 do
      if S[i] = SubStr[1] then
      begin
        X := 1;
        while (X < k) and (S[i + X] = SubStr[1 + X]) do
          Inc(X);
        if (X = k) then
        begin
          Result := I;
          exit;
        end;
      end;
   result := 0;
end;

function IsSubStr (const SubStr, S: widestring): boolean;
begin
	result := (Pos(SubStr, S) > 0);
end;

function LeftStr(const s: widestring; i_count: integer): widestring;
begin
	if i_count >= 0 then
		result := copy(s, 1, i_count)
   else
   	result := s;
end;

function MidStr(const s: widestring; i_offset: integer; i_count: integer = -1): widestring;
begin
	if i_count >= 0 then
		result := copy(s, i_offset, i_count)
   else
   	result := copy(s, i_offset, length(s));
end;

function SubStr(const s: widestring; i_start: integer; i_end: integer = -1): widestring;
begin
	if InRange(i_start, 1, Length(s)) and (i_end >= i_start) then
   	result := copy (s, i_start, i_end - i_start + 1)
   else
   	result := '';
end;

function RightStr(const s: widestring; i_count: integer): widestring;
begin
	result := copy(s, length(s) - i_count + 1, i_count);
end;

function Trim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function TrimRight(const S: WideString): WideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function TrimInside (const s: widestring): WideString;
begin
	result := ReplaceStr (s, ' ', '');
   result := ReplaceStr (result, #9, '');
end;

procedure Insert (var s: widestring; Index: integer; const s_ins: widestring);
begin
	s := LeftStr (s, index - 1) + s_ins + MidStr (s, index);
end;

procedure Delete(var S: widestring; Index, Count: Integer);
begin
	s := leftstr (s, index - 1) + midstr (s, index + count);
end;

procedure Delete(var s: widestring; const s_del: widestring);
begin
	s := ReplaceStr (s, s_del, '');
end;

procedure StrLCopy (Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal);
var i: integer;
begin
   for i := 0 to MaxLen - 1 do
      Dest[i] := Source[i];
   Dest[MaxLen] := #0;
end;

function ReplaceStr (const s_source, s_from, s_to: widestring): widestring;
var i, j: integer;
begin
//   n := length(s_source) - length(s_from) + 1;
   i := 1;
   result := '';
   j := firstpos (s_from, s_source, i);
   while j > 0 do
   begin
      result := result + midstr(s_source, i, j - i) + s_to;
      i := j + length(s_from);
      j := firstpos (s_from, s_source, i);
   end;
   result := result + midstr (s_source, i);
end;

function ReplaceStrings (const s_source: widestring; const s_from, s_to: array of widestring): widestring;
var i: integer;
begin
	result := s_source;
	for i := Low(s_from) to High(s_from) do
   	result := ReplaceStr (result, s_from[i], s_to[i]);
end;

function CutText (const s: widestring; i_count: integer): widestring;     // xxx...
begin
	if length(s) > i_count then
   	result := LeftStr(s, i_count) + ' ...'
   else
   	result := s;
end;

function UpperCase(const S: widestring): widestring;
var
  Ch: WideChar;
  L: Integer;
  Source, Dest: PWideChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function LowerCase(const S: widestring): widestring;
var
  Ch: WideChar;
  L: Integer;
  Source, Dest: PWideChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function DuplicateStr (const s: widestring; i_count: cardinal): widestring;
var i: cardinal;
begin
   result := '';
   for i := 1 to i_count do
      result := result + s;
end;

function Space (i_count: cardinal): widestring;
begin
   result := DuplicateStr (' ', i_count);
end;

const s_ml: array [0..3] of widestring = (#9, #11, #13#10, #13);
   s_sl1: array [0..3] of widestring = (#1, #2, #2, #2);
   s_sl2: array [0..3] of widestring = ('  ', '  ', '  ', '  ');

function MultiLineToSingleLine (const s: widestring; b_useblank: boolean = false): widestring;
var i: integer;
begin
   result := s;
   for i := Low(s_ml) to High(s_ml) do
      if b_useblank then
         result := ReplaceStr (result, s_ml[i], s_sl2[i])
      else
         result := ReplaceStr (result, s_ml[i], s_sl1[i]);
end;

function SingleLineToMultiLine (const s: widestring): widestring;
begin
   result := ReplaceStr (s, #1, #9);
   result := ReplaceStr (result, #2, #13#10);
end;

function AnsiToUnicode (const s: AnsiString): widestring;
var p: pwidechar;
	n: integer;
begin
	n := length(s) + 1;
   GetMem (p, n * 2);
   StringToWideChar (s, p, n);
   result := p;
   Freemem (p, n * 2);
end;

function UnicodeToAnsi (const s: widestring): AnsiString;
begin
   result := WideCharToString (pwidechar(s));
end;

function f_ConvertString (const s: widestring; i_offset1, i_offset2: integer): widestring;
var i, n: integer;
begin
	n := length(s);
	setlength (result, n);
	for i := 1 to n do
   	if i div 2 = 0 then
      	result[i] := WideChar(Ord(s[i]) + i_offset2)
      else
         result[i] := WideChar(Ord(s[i]) + i_offset1)
end;

function EncryptString (const s: widestring): widestring;
begin
	result := f_ConvertString (s, -2, 3);
end;

function DecryptString (const s: widestring): widestring;
begin
	result := f_ConvertString (s, 2, -3);
end;

function FormatStr (const s_source: widestring; const strArray: array of widestring): widestring;
var i: integer;
begin
	result := s_source;
   for i := length(StrArray) - 1 downto 0 do
   	result := ReplaceStr (result, '%' + IntToStr(i), StrArray[i]);
end;

function FormatInt (const s_source: widestring; IntArray: array of integer): widestring;
var i: integer;
begin
	result := s_source;
   for i := length(IntArray) - 1 downto 0 do
   	result := ReplaceStr (result, '%' + IntToStr(i), IntToStr(IntArray[i]));
end;

const set_wordbreak: set of byte = [9, 10, 13, 32, 33, 44, 46, 58, 59, 63];

function IsWordBreak (c: widechar): boolean;
begin
	result := word(c) in set_wordbreak;
end;

function IsWordBreak (c: word): boolean;
begin
	result := c in set_wordbreak;
end;

end.

