unit UxlDateTimeUtils;

interface

uses Windows;

type
	TDateTimeMode = (dtmDateTime, dtmDate, dtmTime, dtmDateTimeWithSecond);
	TIntTimeType = (ittMinute, ittSecond, ittMilliSecond, ittFull);
   TTimeCompareResult = (ctrEarlier, ctrEqual, ctrLater);

// 用于数据存储与读取
function StringToSystemTime (const s_datetime: widestring; b_defnow: boolean = true): TSystemTime;
function SystemTimeToString (const systime: TSystemTime; dtm: TDateTimeMode): widestring;
// 用于显示
function SystemTimeToLocaleStr (const systime: TSystemTime; dtm: TDateTimeMode = dtmDateTime): widestring;
function SystemTimeToFormatStr (const systime: TSystemTime; const s_format: widestring): widestring;
// 用于计算
function SystemTimeToInt64 (const systime: TSystemTime; itt: TIntTimeType = ittFull): Int64;
function Int64ToSystemTime (i_time: Int64; itt: TIntTimeType = ittFull): TSystemTime;
function CompareTime (t1, t2: TSystemTime; itt: TIntTimeType = ittFull): TTimeCompareResult;

function CombineDateTime (const o_date, o_time: TSystemTime): TSystemTime;
function Now (): TSystemTime;
procedure IncTime (var o_time: TSystemTime; i_inc: Int64 = 1; itt: TIntTimeType = ittMinute);
function GetDatePeriod (o_start, o_end: TSystemTime): widestring;
function ConvertMinute (s_timestr: widestring): integer;   // min, h, d --> min

implementation

uses UxlStrUtils, UxlFunctions, UxlList;

function SystemTimeToString (const systime: TSystemTime; dtm: TDateTimeMode): widestring;
var s_format: widestring;
begin
   case dtm of
   	dtmDateTime: s_format := 'yyyy-MM-dd HH:mm';
      dtmDate: s_format := 'yyyy-MM-dd';
      dtmTime: s_format := 'HH:mm:ss';
      dtmDateTimeWithSecond: s_format := 'yyyy-MM-dd HH:mm:ss';
   end;
   result := SystemTimeToFormatStr (systime, s_format);
end;

function SystemTimeToLocaleStr (const systime: TSystemTime; dtm: TDateTimeMode = dtmDateTime): widestring;
var s1: array[0..80] of widechar;
	s2: array[0..80] of widechar;
   s_format, s3, s4: widestring;
begin
	GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SSHORTDATE, s1, Length(s1));
   GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_STIMEFORMAT, s2, Length(s2));
   s3 := s1;
   s4 := s2;
   if dtm <> dtmDateTimeWithSecond then
   	Delete (s4, ':ss');

   case dtm of
   	dtmDateTime, dtmDateTimeWithSecond: s_format := s3 + ' ' + s4;
      dtmDate: s_format := s3;
      dtmTime: s_format := s4;
   end;
   result := SystemTimeToFormatStr (systime, s_format);
end;

function SystemTimeToFormatStr (const systime: TSystemTime; const s_format: widestring): widestring;
var s: widestring;
	s1: array[0..9] of widechar;
   h: integer;
   i: integer;
   o_list: TxlStrList;

   function f_Decode (const s_source: widestring): widestring;
   var i, i_pos: integer;
      s: widestring;
   begin
      result := s_source;
      if length(s_source) = 0 then exit;
      for i := o_list.Low to o_list.High do
      begin
         s := o_list.Keys[i];
         i_pos := FirstPos (s, s_source);
         if i_pos > 0 then
         begin
            result := f_Decode(LeftStr (s_source, i_pos - 1)) + o_list[i] + f_Decode (MidStr (s_source, i_pos + length (s)));
            exit;
         end;
      end;
   end;
begin
   o_list := TxlStrList.Create();

   s := IntToStr(systime.wYear, 4);
	o_list.AddByKey ('yyyy', s);
	o_list.AddByKey ('yy', RightStr(s, 2));
   o_list.AddByKey ('y', s[4]);

   GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SMONTHNAME1 + systime.wMonth - 1, s1, Length(s1));
   o_list.AddByKey ('MMMM', s1);
   GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SABBREVMONTHNAME1 + systime.wMonth - 1, s1, Length(s1));
   o_list.AddByKey ('MMM', s1);
   o_list.AddByKey ('MM', IntToStr(systime.wMonth, 2));
   o_list.AddByKey ('M', IntToStr(systime.wMonth));

   if systime.wDayOfWeek = 0 then
   	i := 6
   else
   	i := systime.wDayOfWeek - 1;
   GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SDAYNAME1 + i, s1, Length(s1));
   o_list.AddByKey ('dddd', s1);
   GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SABBREVDAYNAME1 + i, s1, Length(s1));
   o_list.AddByKey ('ddd', s1);
   o_list.AddByKey ('dd', IntToStr(systime.wDay, 2));
   o_list.AddByKey ('d', IntToStr(systime.wDay));

   o_list.AddByKey ('HH', IntToStr(systime.wHour, 2));
   o_list.AddByKey ('H', IntToStr(systime.wHour));

   h := systime.wHour;
   if h >= 12 then
   	GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_S2359, s1, Length(s1))
   else
   	GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_S1159, s1, Length(s1));
   if h > 12 then
   	dec (h, 12)
   else if h = 0 then
   	h := 12;

   o_list.AddByKey ('tt', s1);
   o_list.AddByKey ('hh', IntToStr(h, 2));
   o_list.AddByKey ('h', IntToStr(h));

   o_list.AddByKey ('mm', IntToStr(systime.wMinute, 2));
   o_list.AddByKey ('m', IntToStr(systime.wMinute));

   o_list.AddByKey ('ss', IntToStr(systime.wSecond, 2));
   o_list.AddByKey ('s', IntToStr(systime.wSecond));

   result := f_Decode (s_format);    // 使用递归算法，以替换的部分不纳入解析。不能简单地用ReplaceStr 逐个地去替换，因可能发生类似上次替换产生了s字符，下次解析时s被误当成指代符的问题。
   o_list.free;
end;

function StringToSystemTime (const s_datetime: widestring; b_defnow: boolean = true): TSystemTime;
var i_pos, i, j: integer;
	s_date, s_time: widestring;
   o_list: TxlIntList;
begin
   s_date := '';
   s_time := '';
   if b_defnow then
   	result := Now;
	i_pos := FirstPos (' ', s_datetime);
   if i_pos > 1 then
   begin
      s_date := Trim (LeftStr (s_datetime, i_pos - 1));
      s_time := Trim (MidStr (s_datetime, i_pos + 1));
   end
   else if IsSubStr ('-', s_datetime) then
   	s_date := Trim (s_datetime)
   else
   	s_time := Trim (s_datetime);

   o_list := TxlIntList.Create (3);
   if s_date <> '' then
   begin
   	o_list.Separator := '-';
      o_list.Text := s_date;
      with result do
      begin
      	wYear := o_list[0];
         wMOnth := o_list[1];
         wDay := o_list[2];
      end;
   end;

   if s_time <> '' then
   begin
   	o_list.Separator := ':';
   	o_list.Text := s_time;
      result.wHour := o_list[0];
      result.wMinute := o_list[1];
      result.wSecond := o_list[2];
   end;
   o_list.free;
end;

function CombineDateTime (const o_date, o_time: TSystemTime): TSystemTime;
begin
	result := o_date;
   result.wHour := o_time.wHour;
   result.wMinute := o_time.wMinute;
end;

function SystemTimeToInt64 (const systime: TSystemTime; itt: TIntTimeType = ittFull): Int64;
var ft: TFileTime;
begin
   SystemTimeToFileTime (systime, ft);
   result := Int64(ft.dwHighDateTime) * High(Cardinal);
   result := result + ft.dwLowDateTime;
   case itt of
   	ittMinute:
   		result := result div 600000000;
      ittSecond:
      	result := result div 10000000;
      ittMilliSecond:
         result := result div 10000;
   end;
end;

function Int64ToSystemTime (i_time: Int64; itt: TIntTimeType = ittFull): TSystemTime;
var ft: TFileTime;
begin
	case itt of
   	ittMinute:
      	i_time := i_time * 600000000;
      ittSecond:
      	i_time := i_time * 10000000;
   end;
   ft.dwLowDateTime := i_time mod High(Cardinal);
   ft.dwHighDateTime := i_time div High(Cardinal);
   FiletimeToSystemTime (ft, result);
end;

function CompareTime (t1, t2: TSystemTime; itt: TIntTimeType = ittFull): TTimeCompareResult;
var i, j: Int64;
begin
	i := SystemTimeToInt64 (t1, itt);
   j := SystemTimeToInt64 (t2, itt);
   if i < j then
   	result := ctrEarlier
   else if i > j then
   	result := ctrLater
   else
   	result := ctrEqual;
end;

function Now (): TSystemTime;
begin
   GetLocalTime (result);
end;

procedure IncTime (var o_time: TSystemTime; i_inc: Int64 = 1; itt: TIntTimeType = ittMinute);
var dt: Int64;
begin
   dt := SystemTimeToInt64 (o_time, itt);
   dt := dt + i_inc;
   o_time := Int64ToSystemTime (dt, itt);
end;

function GetDatePeriod (o_start, o_end: TSystemTime): widestring;
var s1, s2: widestring;
begin
   s1 := SystemTimeToLocaleStr (o_start, dtmDate);
   s2 := SystemTimeToLocaleStr (o_end, dtmDate);
   if s1 <> s2 then
   begin
//      if o_start.wYear = o_start.wYear then
//      begin
//      	GetLocaleInfoW (LOCALE_SYSTEM_DEFAULT, LOCALE_SSHORTDATE, s3, Length(s3));
//         for i := Length(s3) downto 0 do
//         	if
//			s2 := MidStr (s2, 6);
      result := s1 + ' .. ' + s2;
   end
   else
      result := s1;
end;

function ConvertMinute (s_timestr: widestring): integer;
begin
	result := 0;
   if IsSubStr ('d', s_timestr) then
   begin
      Delete (s_timestr, 'd');
      result := StrToIntDef (s_timestr) * 60 * 24;
   end
   else if IsSubStr ('min', s_timestr) then
   begin
      Delete (s_timestr, 'min');
      result := StrToIntDef (s_timestr);
   end
   else
   begin
      Delete (s_timestr, 'h');
      result := StrToIntDef (s_timestr) * 60;
   end;
end;

end.
