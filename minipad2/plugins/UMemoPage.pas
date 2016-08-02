unit UMemoPage;

interface

uses Windows, UPageSuper, UPageProperty, UxlList, UxlDateTimeUtils, UTypeDef, UEditBox, UxlComboBox, UxlMiscCtrls;

type
	TMemoPage = class (TChildItemContainer)
   private
   public
   	class function PageType (): TPageType; override;
      class function DefChildType (): TPageType; override;
      class procedure GetListShowCols (o_list: TxlIntList); override;
		class procedure InitialListProperty (lp: TListProperty); override;
   end;

   TTimeMode = (tmThisDay, tmDaily, tmWeekly, tmMonthly, tmYearly, tmPeriod, tmNoTime);
   TAction = (mmReminder, mmExecuteLink, mmNoAction);

   TMemoProperty = class (TClonableProperty)
   private
      function EncryptDateTime (TimeMode: TTimeMode; const s_days: widestring; const o_date, o_time: TSystemTime): widestring;
      procedure DecryptDateTime (s_datetime: widestring; var TimeMode: TTimeMode; var s_days: widestring; var o_date, o_time: TSystemTime);
   public
      TimeMode: TTimeMode;
      Days: widestring;
      Date: TSystemTime;
      Time: TSystemTime;
      Action: TAction;
      Description: widestring;
      UseSound: boolean;
      SoundFile: widestring;
      AgainTime: Int64;
//      PreTime: Int64;
      Reminded: boolean;
      Remark: widestring;

   	procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      class procedure GetShowCols (o_list: TxlIntList); 
      function GetColText (id_col: integer; var s_result: widestring): boolean; override;
      function CheckTime (ctime: TSystemTime): boolean;
      function IsObsolete (): boolean;
   end;

   TMemoItem = class (TChildItemSuper)
   private
   	FMemo: TMemoProperty;
   protected
      function GetImageIndex (): integer; override;
   public
      constructor Create (i_id: integer); override;
      destructor Destroy (); override;

   	class function PageType (): TPageType; override;
		class procedure GetSearchCols (o_list: TxlIntList); override;
      property Memo: TMemoProperty read FMemo;
   end;

   TMemoBox = class (TEditBoxSuper)
   private
      FDatePicker: TxlDatePicker;
      FTimePicker: TxlTimePicker;
      FDatePicker2: TxlDatePicker;
      FCMBTimeMode: TxlComboBox;

      procedure f_SwitchTimeMode (Sender: TObject);
      procedure f_CheckSoundEnable ();
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;

      procedure LoadItem (value: TPageSuper); override;
      procedure ClearAndNew (); override;
      function SaveItem (value: TPageSuper): integer; override;
   public
   	class function PageType (): TPageType; override;
   end;

implementation

uses CommCtrl, UxlWinControl, UxlFunctions, UxlListView, UxlStrUtils, UxlCommDlgs, UPageFactory, UPageStore,
	UGlobalObj, ULangManager, UOptionManager, Resource;

class	function TMemoPage.PageType (): TPageType;
begin
	result := ptMemo;
end;

class function TMemoPage.DefChildType (): TPageType;
begin
	result := ptMemoItem;
end;

class procedure TMemoPage.InitialListProperty (lp: TListProperty);
const c_cols: array[0..2] of integer = (sr_Title, sr_Time, sr_Description);
	c_widths: array[0..2] of integer = (100, 100, 200);
begin
	with lp do
   begin
		ColList.Populate (c_cols);
		WidthList.Populate (c_widths);
      CheckBoxes := false;
		View := lpvReport;
   	FullrowSelect := true;
   	GridLines := false;
   end;
end;

class procedure TMemoPage.GetListShowCols (o_list: TxlIntList);
begin
	TMemoProperty.GetShowCols (o_list);
end;

//-----------------------

constructor TMemoItem.Create (i_id: integer);
begin
	inherited Create (i_id);
	FMemo := TMemoProperty.Create (i_id);
   FItemProperty := FMemo;
   AddProperty (FMemo);
end;

destructor TMemoItem.Destroy ();
begin
	FMemo.free;
   inherited;
end;

class function TMemoItem.PageType (): TPageType;
begin
	result := ptMemoItem;
end;

function TMemoItem.GetImageIndex (): integer;
begin
	result := PageImageList.IndexOf (PageType) + Ord(Memo.Action) * 2;
	if Memo.IsObsolete then
		inc (result);
end;

class procedure TMemoItem.GetSearchCols (o_list: TxlIntList);
begin
   TMemoProperty.GetShowCols (o_list);
end;

//----------------

procedure TMemoProperty.Load (o_list: TxlStrList);
begin
   DecryptDateTime (o_list[0], TimeMode, Days, Date, Time);
   Action := TAction(StrToIntDef(o_list[1]));
   Description := SingleLineToMultiLine (o_list[2]);
   UseSound := StrToBool (o_list[3]);
   SoundFile := o_list[4];
//   PreTime := o_list[5];
   AgainTime := StrToIntDef(o_list[5]);
   Reminded := StrToBool(o_list[6]);
   Remark := SingleLineToMultiLine(o_list[7]);
   o_list.Delete (0, 8);
end;

procedure TMemoProperty.Save (o_list: TxlStrList);
begin
	with o_list do
   begin
      Add (EncryptDateTime (TimeMode, Days, Date, Time));
		Add (IntToStr(Ord(Action)));
      Add (MultiLineToSingleLine (Description));
		Add (BoolToStr (UseSound));
      Add (SoundFile);
//      Add (IntToStr (PreTime));
      Add (IntToStr (AgainTime));
      Add (BoolToStr (Reminded));
      Add (MultiLineToSingleLine(Remark));
   end;
end;

class procedure TMemoProperty.GetShowCols (o_list: TxlIntList);
const c_cols: array[0..6] of integer = (sr_Title, sr_Time, sr_Action, sr_Description, sr_UseSound, sr_SoundFile, sr_Remark);
var i: integer;
begin
   for i := Low(c_cols) to High(c_cols) do
		o_list.Add (c_cols[i]);
end;

function TMemoProperty.GetColText (id_col: integer; var s_result: widestring): boolean;
begin
	result := true;
	case id_col of
   	sr_Title:
      	s_result := PageStore[FPageId].Name;
   	sr_Time:
      	s_result := EncryptDateTime (TimeMode, Days, Date, Time);
      sr_Action:
      	s_result := LangMan.GetItem (sr_MemoAction + Ord(Action));
      sr_Description:
      	s_result := MultiLineToSingleLine (Description, true);
      sr_UseSound:
      	s_result := BoolToCheckMark (UseSound);
      sr_SoundFile:
      	s_result := SoundFile;
      sr_Remark:
      	s_result := MultiLineToSingleLine (Remark, true);
      else
      	result := false;
   end;
end;

function TMemoProperty.EncryptDateTime (TimeMode: TTimeMode; const s_days: widestring; const o_date, o_time: TSystemTime): widestring;
const s_prefix: array[0..2] of widestring = ('w', 'm', 'y');
var s1, s2: widestring;
begin
	case TimeMOde of
   	tmWeekly, tmMonthly, tmYearly:
      	result := '@' + s_prefix[Ord(TimeMode) - Ord(tmWeekly)] + trim(s_days) + ' ' + SystemTimeToString (o_time, dtmTime);
      tmDaily:
      	result := '@' + SystemTimeToString (o_time, dtmTime);
      tmThisDay:
      	result := SystemTimeToString (CombineDateTime(o_date, o_time), dtmDateTime);
      tmNoTime:
      	result := ''
      else   // tmPeriod
      begin
         s1 := SystemTimeToString (o_date, dtmDate);
         s2 := SystemTimeToString (o_time, dtmDate);
         if s1 <> s2 then
         begin
            if o_date.wYear = o_time.wYear then  s2 := MidStr (s2, 6);
            result := s1 + '...' + s2;
         end
         else
         	result := s1;
      end;
   end;
end;

procedure TMemoProperty.DecryptDateTime (s_datetime: widestring; var TimeMode: TTimeMode; var s_days: widestring; var o_date, o_time: TSystemTime);
var i: integer;
   s: widestring;
begin
   s_datetime := LowerCase(Trim(s_datetime));
   if s_datetime = '' then
   	TimeMode := tmNoTime
   else if s_datetime[1] = '@' then
   begin
   	if (s_datetime[2] = 'w') or (s_datetime[2] = 'm') or (s_datetime[2] = 'y') then
      begin
         if s_datetime[2] = 'w' then
            TimeMode := tmWeekly
         else if s_datetime[2] = 'm' then
            TimeMOde := tmMonthly
         else
            TimeMode := tmYearly;
         i := FirstPos (' ', s_datetime);
         s_days := MidStr (s_datetime, 3, i - 3);
         o_time := StringToSystemTime (MidStr(s_datetime, i + 1));
      end
      else
      begin
      	TimeMOde := tmDaily;
         s_days := '';
         o_time := StringToSystemTime (MidStr(s_datetime, 2));
      end;
      o_date := Now;
   end
   else if IsSubStr ('...', s_datetime) or (Length(s_datetime) <= 10) then
   begin
      TimeMode := tmPeriod;
      s_days := '';
      if IsSubStr ('...', s_datetime) then
      begin
         i := FirstPos ('...', s_datetime);
         o_date := StringToSystemTime (LeftStr (s_datetime, i - 1));
         s := MidStr (s_datetime, i + 3);
         if length(s) < 6 then s := LeftStr(s_datetime, 4) + '-' + s;
         o_time := StringToSystemTime (s);
      end
      else
      begin
      	o_date := StringToSystemTime (s_datetime);
         o_time := o_date;
      end;
   end
   else
   begin
   	TimeMode := tmThisday;
      s_days := '';
      o_date := StringToSystemTime (s_datetime);
      o_time := o_date;
   end;
end;

function TMemoProperty.CheckTime (ctime: TSystemTime): boolean;
   function f_daysin (i_day: cardinal; const s_days: widestring): boolean;
   var i, i_pos, i_start, i_end: cardinal;
      s: widestring;
      o_list: TxlStrList;
   begin
      result := false;
      o_list := TxlStrList.Create;
      o_list.Separator := ',';
      o_list.Text := s_days;
      for i := o_list.Low to o_list.High do
      begin
         s := o_list[i];
         i_pos := FirstPos ('-', s);
         if i_pos > 0 then
         begin
            i_start := StrToIntDef(LeftStr(s, i_pos - 1), 100);
            i_end := StrToIntDef(MidStr(s, i_pos + 1), 0);
            result := (i_day >= i_start) and (i_day <= i_end);
         end
         else
            result := (i_day = StrToIntDef(s));
         if result then break;
      end;
      o_list.free;
   end;
var m, d, i, j: integer;
   o_list: TxlStrList;
   s_day: widestring;
begin
	result := false;
   if TimeMode = tmNoTime then exit;

   if (SystemTimeToInt64(ctime, ittMinute) = AgainTime) then
   begin
   	result := true;
      AgainTime := 0;
   end
   else if (TimeMode = tmPeriod) then
      result := (not Reminded) and (CompareTime (Date, ctime, ittMinute) in [ctrEarlier, ctrEqual])
   else if TimeMode = tmThisDay then
   	result := (not Reminded) and (CompareTime (CombineDateTime(Date, Time), ctime, ittMinute) in [ctrEarlier, ctrEqual])
   else
   begin
   	if not (ctime.wMinute = Time.wMinute) then exit;
      if not (ctime.wHour = Time.wHour) then exit;
      case TimeMOde of
         tmDaily:
            result := true;
         tmWeekly:
            if ctime.wDayOfWeek = 0 then   // Sunday 可为 0，也可为 7
               result := f_daysin (0, Days) or f_daysin (7, Days)
            else
               result := f_daysin (ctime.wDayOfWeek, Days);
         tmMonthly:
            result := f_daysin (ctime.wDay, Days);
         tmYearly:
            begin
               o_list := TxlStrList.Create;
               o_list.Separator := ',';
               o_list.Text := Days;
               for i := o_list.Low to o_list.High do
               begin
                  s_day := o_list[i];
                  j := FirstPos ('-', s_day);
                  m := StrToIntDef (LeftStr (s_day, j - 1));
                  d := StrToIntDef (MidStr (s_day, j + 1));
                  if (ctime.wMonth = m) and (ctime.wDay = d) then
                  begin
                     result := true;
                     break;
                  end;
               end;
               o_list.free;
            end;
      end;
   end;
end;

function TMemoProperty.IsObsolete (): boolean;
begin
	if (TimeMode in [tmThisDay, tmPeriod]) and (Action in [mmReminder, mmExecuteLink]) and (not Reminded) then
   	result := false
   else
      case TimeMode of
         tmThisDay:
            result := CompareTime (CombineDateTime(Date, Time), Now, ittMinute) = ctrEarlier;
         tmPeriod:
            result := SystemTimeToInt64 (Now, ittMinute) - SystemTimeToInt64 (Time, ittMinute) >= 1440;
         else
            result := false;
      end;
end;

//--------------------

procedure TMemoBox.OnInitialize ();
begin
	InitCommonControl (ICC_DATE_CLASSES);
	SetTemplate (Memo_Box, m_newmemo);
end;

const c_timemodes: array[0..6] of word = (sr_today, sr_daily, sr_weekly, sr_monthly, sr_yearly, sr_timespan, sr_notime);
const c_memobox: array[0..14] of word = (st_description, st_action, st_title, rb_reminder, rb_executelink, rb_noaction, st_sound, rb_nosound, rb_beep, rb_soundfile, st_time, cb_new, cb_add, IDOK, IDCANCEL);

procedure TMemoBox.OnOpen ();
var i: integer;
begin
   FCMBTimeMode := TxlComboBox.create (self, ItemHandle[cmb_timemode]);
   for i := Low(c_timemodes) to High(c_timemodes) do
   	FCMBTimeMOde.Items.Add (LangMan.GetItem(c_timemodes[i]));
   FCmbTimeMode.Items.OnSelChange := f_SwitchTimeMode;

   FTimePicker := TxlTimePicker.Create (self, ItemHandle[dtp_time]);
	FDatePicker := TxlDatePicker.Create (self, ItemHandle[dtp_date]);
   FDatePicker2 := TxlDatePicker.Create (self, ItemHandle[dtp_date2]);

   inherited;
   refreshItemText (self, c_memobox);
end;

procedure TMemoBox.OnClose ();
begin
	FCMBTimeMode.free;
	FDatePicker.free;
   FTimePicker.free;
   FDatePicker2.free;
	inherited;
end;

procedure TMemoBox.OnCommand (ctrlID: integer);
var s_files: widestring;
begin
	case CtrlID of
   	cb_add:
         begin
            s_files := ItemText[mle_description];
            if AddFiles (s_files) then
            begin
               ItemTExt[mle_description] := s_files;
               MoveCursorLast (mle_description);
            end;
         end;
      rb_nosound, rb_beep, rb_soundfile:      //rb_reminder, rb_executelink, rb_noaction
      	begin
         	f_CheckSoundEnable ();
            if ItemEnabled[sle_soundfile] then
            	FocusControl (sle_soundfile);
         end;
      cb_browse:
         with TxlOpenDialog.create do
         begin
            Title := LangMan.GetItem (sr_selectsoundfile);
            Filter := LangMan.GetItem (sr_soundfilefilter);
            FilterIndex := 1;
            Path := SoundDir;
            FileName := ItemText[sle_soundfile];
            MultiSelect := false;
            if Execute then
            begin
               ItemText[sle_soundfile] := FileName;
               if Path <> SoundDir then
                  CopyFile (Path + FileName, SoundDir + FileName, true);
            end;
            free;
         end;
      else
         inherited;
   end;
end;

class function TMemoBox.PageType (): TPageType;
begin
	result := ptMemoItem;
end;

procedure TMemoBox.LoadItem (value: TPageSuper);
var p: TMemoProperty;
begin
	self.Text := LangMan.GetItem(sr_EditReminder);

   ItemText[sle_title] := TMemoItem(value).name;
   p := TMemoItem(value).Memo;

   FCMBTimeMode.Items.Select (Ord(p.TimeMode));
   ItemText[sle_days] := p.Days;
   FDatePicker.Date := p.Date;
	FTimePicker.Time := p.Time;
   FDatePicker2.Date := p.Time;

   ItemChecked[rb_reminder] := (p.Action = mmReminder);
   ItemChecked[rb_executelink] := (p.Action = mmExecuteLink);
	ItemChecked[rb_noaction] := (p.Action = mmNoAction);
   ItemText[mle_description] := p.Description;
   ItemText[mle_remark] := p.Remark;

   ItemChecked[rb_nosound] := not p.UseSound;
   Itemchecked[rb_beep] := p.UseSound and (p.Soundfile = '');
   ItemChecked[rb_soundfile] := not (ItemChecked[rb_nosound] or ItemChecked[rb_beep]);
   ItemTExt[sle_soundfile] := p.SoundFile;

   f_SwitchTimeMode (self);
   f_CheckSoundEnable ();
   FocusControl (sle_title);
end;

procedure TMemoBox.ClearAndNew ();
var o_deftime: TSystemTime;
begin
	self.Text := LangMan.GetItem(sr_NewReminder);

  	o_deftime := Int64ToSystemTime(SystemTimeToInt64(Now, ittMinute) + OptionMan.Options.DefRemindTime, ittMinute);
   ItemText[sle_title] := '';

   FCmbTimeMode.Items.SelIndex := 0;
   ItemText[sle_days] := '';
   FDatePicker.Date := o_deftime;
	FTimePicker.Time := o_deftime;
   FDatePicker2.Date := o_deftime;

   ItemChecked[rb_reminder] := MemoryMan.MemoAction = mmReminder;
   ItemChecked[rb_executelink] := (MemoryMan.MemoAction = mmExecuteLink);
	ItemChecked[rb_noaction] := (MemoryMan.MemoAction = mmNoAction);

   ItemText[mle_Description] := '';
   ItemText[mle_remark] := '';

   ItemChecked[rb_nosound] := not MemoryMan.MemoUseSound;
   Itemchecked[rb_beep] := MemoryMan.MemoUseSound and (MemoryMan.MemoSoundFile = '');
   ItemChecked[rb_soundfile] := not (ItemChecked[rb_nosound] or ItemChecked[rb_beep]);
   itemText[sle_SoundFile] := MemoryMan.MemoSoundFile;

   f_SwitchTimeMode (self);
   f_CheckSoundEnable ();
   FocusControl (sle_title);
end;

function TMemoBox.SaveItem (value: TPageSuper): integer;
begin
   TMemoItem (value).name := ItemText[sle_title];
   with TMemoItem (value).Memo do
   begin
      TimeMode := TTimeMode (FCMBTimeMode.Items.SelIndex);
      Days := ItemText[sle_days];
     	Date := FDatePicker.Date;
      if TimeMode in [tmPeriod, tmNoTime] then
         Time := FDatePicker2.Date
      else
         Time := FTimePicker.Time;
      if ItemChecked[rb_reminder] then
         Action := mmReminder
      else if ItemChecked[rb_executelink] then
         Action := mmExecuteLink
      else
         Action := mmNoAction;
      if ItemText[mle_description] <> '' then
      	Description := ItemText[mle_description]
      else
      	Description := ItemText[sle_title];
      UseSound := not ItemChecked[rb_nosound];
      if ItemChecked[rb_soundfile] then
      	SoundFile := ItemTExt[sle_soundfile]
      else
      	SoundFile := '';
      AgainTime := 0;
      Reminded := false;
      remark := ItemText[mle_remark];
   end;
end;

procedure TMemoBox.f_SwitchTimeMode (Sender: TObject);
var tm: TTimeMode;
   b: boolean;
begin
   tm := TTimeMOde (FCmbtimemode.Items.SelIndex);
   ItemEnabled[sle_days] := (tm = tmWeekly) or (tm = tmMonthly) or (tm = tmYearly);
   ItemEnabled[dtp_date] := (tm in [tmThisDay, tmPeriod]);

   b := (tm in [tmPeriod, tmNoTime]);
   ItemVisible[st_to] := b;
   ItemVisible[dtp_time] := not b;
   ItemVisible[dtp_date2] := b;
   ItemEnabled[dtp_time] := not b;
   ItemEnabled[dtp_date2] := tm = tmPeriod;

   b := tm = tmNoTime;
   ItemEnabled[rb_reminder] := not b;
   ItemEnabled[rb_executelink] := not b;

   ItemEnabled[rb_nosound] := not b;
   Itemenabled[rb_beep] := not b;
   ItemEnabled[rb_soundfile] := not b;
   if b then
   begin
      ItemChecked[rb_reminder] := false;
      ItemChecked[rb_executelink] := false;
      ItemChecked[rb_noaction] := true;
      ItemChecked[rb_nosound] := true;
      Itemchecked[rb_beep] := false;
      ItemChecked[rb_soundfile] := false;
   end;
   f_CheckSoundEnable;
   if ItemEnabled[sle_days] then
   	FocusControl (sle_days);
end;

procedure TMemoBox.f_CheckSoundEnable ();
begin
   ItemEnabled[sle_soundfile] := ItemChecked[rb_soundfile];
   ItemEnabled[cb_browse] := ItemEnabled[sle_soundfile];
end;

//--------------------

initialization
	PageFactory.RegisterClass (TMemoPage);
   PageImageList.AddImageWithOverlay (ptMemo, m_newMemo);
	PageNameMan.RegisterDefName (ptMemo, sr_defMemoname);

	PageFactory.RegisterClass (TMemoItem);
   PageImageList.AddImages (ptMemoItem, [ic_reminder, ic_batchlink, ic_noaction], true);
//	PageNameMan.RegisterDefName (ptMemoItem, sr_defMemoItemname);
   EditBoxFactory.RegisterClass (TMemoBox);

finalization

end.
