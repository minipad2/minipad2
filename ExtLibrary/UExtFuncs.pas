unit UExtFuncs;

interface

function DecodeTemplate (const s: widestring): widestring;

implementation

uses UxlStrUtils, UxlDateTimeUtils, UxlFunctions;

function DecodeTemplate (const s: widestring): widestring;
	function f_Decode (const s, s_sub: widestring; var s1, s2, s3: widestring): boolean;
	var i_pos, i_len, i, n, k: integer;
   begin
   	result := false;
      i_len := Length(s_sub) + 1;
      i_pos := Firstpos (s_sub + '(', s);
      if i_pos <= 0 then exit;

      n := length (s);
      k := 0;
      for i := i_pos + 2 to n do
         if s[i] = '(' then
            inc (k)
         else if s[i] = ')' then
         begin
            dec (k);
            if k = 0 then
            begin
               result := true;
               s1 := leftstr (s, i_pos - 1);
               s2 := MidStr(s, i_pos + i_len, i - i_pos - i_len);
               s3 := midstr (s, i + 1);
               exit;
            end;
         end;
   end;
var s1, s2, s3, s_dup: widestring;
	i_pos3, i, n: integer;
const s_keys: array[0..1] of widestring = ('%n', '%t');
	s_vals: array[0..1] of widestring = (#13#10, #9);
begin
   result := ReplaceStr (s, '%d', SystemTimeToLocaleStr(Now, dtmDate));
   result := ReplaceStr (result, '%i', SystemTimeToLocaleStr(Now, dtmTime));

   n := 0;
   while f_Decode (result, '%c', s1, s2, s3) and (n < 1000) do
   begin
      result := s1 + widechar(StrToIntDef(s2, 32)) + s3;
      inc (n);
   end;

   n := 0;   // ·ÀÖ¹ËÀÑ­»·
   while f_Decode (result, '%u', s1, s2, s3) and (n < 1000) do
   begin
      result := s1 + SystemTimeToFormatStr (Now, s2) + s3;
      inc (n);
   end;

   n := 0;
	while f_Decode (result, '%s', s1, s2, s3) and (n < 1000) do
   begin
      i_pos3 := FirstPos (',', s2);
      if i_pos3 > 0 then
      begin
         s_dup := LeftStr(s2, i_pos3 - 1);
         n := StrToIntDef (MidStr(s2, i_pos3 + 1));
      end
      else
      begin
      	s_dup := '-';
      	n := StrToIntDef (s2);
      end;
      result := s1 + DuplicateStr(s_dup, n) + s3;
      inc (n);
   end;

   for i := Low(s_keys) to High(s_keys) do
	   result := ReplaceStr (result, s_keys[i], s_vals[i]);
end;

end.
