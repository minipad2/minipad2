unit UxlMath;

interface

function IntPower(X: Extended; I: Integer): Extended;
function Power (const Base, Exponent: Extended): Extended;
//function PowerDef (const Base, Exponent: Extended; DefVal: Extended = 0): Extended;
function SetDecimal (d_expr: Extended; i_decimal: integer): Extended;
procedure GetBaseExpo (d_expr: Extended; var base: Extended; var expo: integer);
function Sign (i: integer): integer;
function RandomInt (i_min, i_max: integer): integer;
function Ceil (const x: Extended): Integer;
function Floor (const x: Extended): Integer;

function Max (a, b: Integer): Integer;
function Min (a, b: Integer): Integer;
function ConfineRange (x, i_min, i_max: Integer): Integer;
function InRange (x, i_min, i_max: Integer): boolean;
function MaxF (a, b: Extended): Extended;
function MinF (a, b: Extended): Extended;
function ConfineRangeF (x, d_min, d_max: Extended): Extended;

function Rad (const X: Extended): Extended;   // 角度转换为弧度
function Deg (const X: Extended): Extended;   // 弧度转换为角度
function ArcSin(const X: Extended): Extended;
function ArcCos(const X: Extended): Extended;
function ArcCot(const X: Extended): Extended;
function Log10(const X: Extended): Extended;

function IsZero (d_value: Extended): boolean;
function IsInfinite(const AValue: Double): Boolean;
function IsNan(const AValue: Extended): Boolean;
function sDot(): widechar;

const pi: Extended = 3.141592653589793238;
      e: Extended = 2.718281828459045;

implementation

uses SysUtils, Windows, UxlClasses;

function IntPower(X: Extended; I: Integer): Extended;
var
  Y: Integer;
begin
  Y := Abs(I);
  Result := 1.0;
  while Y > 0 do begin
    while not Odd(Y) do
    begin
      Y := Y shr 1;
      X := X * X
    end;
    Dec(Y);
    Result := Result * X
  end;
  if I < 0 then Result := 1.0 / Result
end;

function Power (const Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0               { n**0 = 1 }
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0               { 0**n = 0, n > 0 }
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, Integer(Trunc(Exponent)))
  else
  begin
//    Result := Ln(Base);
//    Result := Exponent * Result;
//    Result := Exp(Result);
    Result := Exp(Exponent * Ln(Base));
  end;
  if IsNan (Result) or IsInfinite(Result) then
  		Raise Exception.Create ('Invalid result!');
end;

function Ceil (const x: Extended): Integer;
begin
	result := Round (x);
   if x > result then
   	Inc (result);
end;

function Floor (const x: Extended): Integer;
begin
   result := Round (x);
   if result > x then
      Dec (result);
end;

function SetDecimal (d_expr: Extended; i_decimal: integer): Extended;
var i, n: integer;
begin
  if IsNan (d_expr) or IsInfinite(d_expr) then
  		Raise Exception.Create ('Invalid result!');
	result := d_expr;
   if (d_expr = 0) or (not i_decimal in [0..19]) then exit;

   n := 0;
   while ABS(result * 10) < 1 do
   begin
      if n > 19 then exit;
      result := result * 10;
      inc (n);
   end;
   for i := 1 to i_decimal do
   begin
      if (ABS(result) >= 1E20) or (n > 19) then exit;
      result := result * 10;
      inc(n);
   end;
   result := Round(result) / IntPower(10, n);
end;

procedure GetBaseExpo (d_expr: Extended; var base: Extended; var expo: integer);
begin
   expo := 0;
   base := d_expr;
   if abs(base) >= 10 then
   begin
      repeat
         inc (expo);
         base := base / 10;
      until abs(base) < 10;
   end
   else if abs(base) < 1 then
   begin
      repeat
         dec (expo);
         base := base * 10;
      until abs(base) >= 1;
   end;
end;

function MaxF (a, b: Extended): Extended;
begin
	if a > b then
   	result := a
   else
      result := b;
end;

function MinF (a, b: Extended): Extended;
begin
	if a < b then
   	result := a
   else
   	result := b;
end;

function ConfineRangeF (x, d_min, d_max: Extended): Extended;
begin
	if x < d_min then
   	result := d_min
   else if x > d_max then
   	result := d_max
   else
   	result := x;
end;

function Max (a, b: Integer): Integer;
begin
	if a > b then
   	result := a
   else
      result := b;
end;

function Min (a, b: Integer): Integer;
begin
	if a < b then
   	result := a
   else
   	result := b;
end;

function ConfineRange (x, i_min, i_max: Integer): integer;
begin
	if x < i_min then
   	result := i_min
   else if x > i_max then
   	result := i_max
   else
   	result := x;
end;

function InRange (x, i_min, i_max: Integer): boolean;
begin
	result := (x >= i_min) and (x <= i_max);
end;

function Rad (const X: Extended): Extended;   // 角度转换为弧度
begin
   result := X * pi / 180;
end;

function Deg (const X: Extended): Extended;   // 弧度转换为角度
begin
   result := X * 180 / pi;
end;

function f_ArcTan2(const Y, X: Extended): Extended;
asm
   FLD     Y
   FLD     X
   FPATAN
   FWAIT
end;

function ArcCos(const X: Extended): Extended;
begin
  Result := f_ArcTan2(Sqrt(1 - X * X), X);
end;

function ArcSin(const X: Extended): Extended;
begin
  Result := f_ArcTan2(X, Sqrt(1 - X * X))
end;

function ArcCot(const X: Extended): Extended;
begin
  if X = 0 then
    Result := PI / 2
  else
    Result := ArcTan(1 / X);
end;

function Log10(const X: Extended): Extended;
  { Log.10(X) := Log.2(X) * Log.10(2) }
asm
   FLDLG2     { Log base ten of 2 }
   FLD     X
   FYL2X
   FWAIT
end;

function IsNan(const AValue: Extended): Boolean;
type
  TExtented = packed record
    Mantissa: Int64;
    Exponent: Word;
  end;
  PExtended = ^TExtented;
begin
  Result := ((PExtended(@AValue)^.Exponent and $7FFF)  = $7FFF) and
            ((PExtended(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0);
end;

function IsInfinite(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000);
end;

function Sign (i: integer): integer;
begin
	if i = 0 then
      result := 0
   else if i > 0 then
      result := 1
   else
   	result := -1;
end;

function RandomInt (i_min, i_max: integer): integer;
begin
	if i_min <= i_max then
   	result := Random (i_max + 1 - i_min) + i_min
   else
   	result := Random (i_min + 1 - i_max) + i_max;
end;

function IsZero (d_value: Extended): boolean;
begin
	result := (abs(d_value) <= 1E-14);      // 若按Extended的标准：1E-19来判断，对于减法会有问题。因 - 操作符精度为Double， 17584.07-17583.94-0.13 会得到 1E-15 的结果
end;

function sDot (): widechar;
var sdectmp: array[0..4] of widechar;
begin
	GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SDECIMAL, sdectmp, 5);
  	result := sdectmp[0];
end;

Initialization
	Randomize;

end.

