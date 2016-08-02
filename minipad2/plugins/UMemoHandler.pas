unit UMemoHandler;

interface

uses UxlDialog, UMemoPage, UxlList, UxlClasses, UPageFactory, UxlWinclasses, UTypeDef;

type
	TReminderPopupBox = class (TxlDialogML)
   private
		FItem: TMemoItem;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
   	procedure OnCommand (ctrlID: integer); override;
      procedure OnClose (); override;
   public
   	property Item: TMemoItem read FItem write FItem;
   end;

   TPopupClosedEvent = procedure (id: cardinal; s_againtime: widestring; b_noremind, b_deleteafterclose: boolean) of object;

   TReminderPopupManager = class
   private
      FPopupList: TxlObjList;
      procedure f_OnPopupClosed (sender: TObject);
   public
      constructor Create ();
      destructor Destroy (); override;
		procedure PopupReminder (o_item: TMemoItem);
   end;

	TMemoHandler = class (TxlInterfacedObject, IPageObserver)
   private
   	FMemoList: TxlIntList;
      FTimer: TxlTimer;
      FPopupManager: TReminderPopupManager;
      procedure f_OnTimer (Sender: TObject);
   public
      constructor Create ();
      destructor Destroy (); override;
   	procedure OnPageEvent (pct: TPageEvent; id, id2: integer);
   end;

implementation

uses Windows, Mmsystem, Resource, ULangManager, UGlobalObj, UPageStore, UxlDateTimeUtils, UxlComboBox, UxlStrUtils, UxlFunctions,
	ULinkHandler;

const c_ReminderPopupBox: array[0..3] of word = (chk_popupagain, chk_noremind, chk_deleteafterclose, st_minuteslater);

procedure TReminderPopupBox.OnInitialize ();
begin
   SetTemplate (ReminderPopup_Box, ic_reminder);
end;

procedure TReminderPopupBox.OnOpen ();
begin
   RefreshItemText (self, c_ReminderPopupBox, ReminderPopup_Box);

   Text := SystemTimeToLocaleStr (Now, dtmTime) + ' ' + Fitem.Name;
   ItemText[st_remindertext] := FItem.Memo.Description;
   ItemEnabled[chk_noremind] := FItem.Memo.TimeMode <> tmThisDay;
//   FTimeMode := tmThisDay;

	with TxlComboBox.Create (self, ItemHandle[cmb_time]) do
   begin
   	Items.Populate (['5min', '10min', '20min', '30min', '45min', '60min', '90min', '2h', '3h', '4h', '6h', '8h']);
      if FItem.Memo.timemode = tmThisDay then
      	Items.Populate (['1d', '2d', '3d', '5d', '7d']);
      AllowEdit := true;
      Text := '30min';
      free;
   end;
end;

procedure TReminderPopupBox.OnCommand (ctrlID: integer);
var b: boolean;
begin
	case ctrlID of
   	chk_popupagain:
      	begin
         	b := ItemChecked[chk_popupagain];
      		ItemEnabled[cmb_time] := b;
            ItemEnabled[chk_noremind] := (not b) and (FItem.Memo.TimeMode <> tmThisDay);
            ItemEnabled[chk_deleteafterclose] := not b;
         end;
      chk_noremind, chk_deleteafterclose:
      	begin
      		b := ItemEnabled[ctrlID];
            ItemEnabled[chk_popupagain] := not b;
            Itemenabled[cmb_time] := not b;
         end;
   end;
end;

procedure TReminderPopupBox.OnClose ();
var i_againtime: integer;
   dt: TSystemTime;
begin
   if ItemChecked[chk_deleteafterclose] then
   begin
   	FItem.Owner.Childs.RemoveChild (FItem.id);
      FItem.Delete;
   end
   else if ItemChecked[chk_noremind] then
   	FItem.Memo.TimeMOde := tmThisDay
   else if ItemChecked[chk_popupagain] and (itemText[cmb_time] <> '') then
   begin
      i_againtime := ConvertMinute (itemText[cmb_time]);
      if Fitem.Memo.TimeMOde = tmThisDay then
      begin
         dt := Now;
         IncTime (dt, i_againtime);
         Fitem.Memo.Date := dt;
         Fitem.Memo.Time := dt;
         Fitem.Memo.Reminded := false;
      end
      else
         Fitem.Memo.AgainTime := SystemTimeToInt64 (Now, ittMinute) + i_againtime;
      PageCenter.EventNotify (pctFieldModified, FItem.id);
   end;
end;

//----------------------

constructor TReminderPopupManager.Create ();
begin
   FPopupList := TxlObjList.Create();
end;

destructor TReminderPopupManager.Destroy ();
var i: integer;
begin
   for i := FPopupList.Low to FPopupList.High do
      with TReminderPopupBox(FPopupList[i]) do
      begin
         Close;
         Free;
      end;
   FPopupList.Free;
   inherited;
end;

procedure TReminderPopupManager.f_OnPopupClosed (Sender: TObject);
var o_box: TReminderPopupBox;
begin
	o_box := Sender as TReminderPopupBox;
   FPopupList.Remove (o_box);
   o_box.Free;
   PlaySoundW (nil, 0, 0);
end;

procedure TReminderPopupManager.PopupReminder (o_item: TMemoItem);
var o_pos: TPos;
   i: cardinal;
   o_box: TReminderPopupBox;
   rc: TRect;
const mhz: array [0 .. 3] of cardinal = (523, 659, 784, 1047);
begin
   o_box := TReminderPopupBox.Create;
   with o_box do
   begin
   	Item := o_item;
      Open (false);
      OnDialogClosed := f_OnPopupClosed;
   end;
   FPopupList.Add(o_box);

   o_pos := o_box.Pos;
   GetScreenRect (rc, false);
   o_pos.x := rc.right - o_pos.width - 5;
   o_pos.y := rc.bottom - o_pos.height - 5;
   with o_box do
   begin
   	Pos := o_pos;
   	Show;
		BringToTop;
   	StayOnTop := true;
   end;

   if o_item.Memo.usesound then
   	if (o_item.Memo.soundfile <> '') and PathFileExists(SoundDir + o_item.Memo.soundfile) then
			PlaySoundW (pwidechar(SoundDir + o_item.memo.soundfile), 0, SND_FILENAME or SND_ASYNC)
      else
      begin
      	for i := low(mhz) to high(mhz) do
         	Windows.beep (mhz[i], 500);
      end;
end;

//------------------------------

constructor TMemoHandler.Create ();
var o_now: TSystemTime;
begin
	FMemoList := TxlIntList.Create;
   PageStore.GetPageList (ptMemoItem, FMemoList);

   FPopupManager := TReminderPopupManager.Create ();
   PageCenter.AddObserver (self);

   FTimer := TimerCenter.NewTimer;
   o_now := Now;
   if o_now.wSecond > 0 then
   	FTimer.Interval := (60 - o_now.wSecond) * 1000
   else
   	FTimer.Interval := 60000;
   FTimer.OnTimer := f_OnTimer;
   FTimer.Enabled := true;
end;

destructor TMemoHandler.Destroy ();
begin
   TimerCenter.ReleaseTimer (FTimer);
   PageCenter.RemoveObserver (self);
   FPopupManager.Free;
   FMemoList.Free;
   inherited;
end;

procedure TMemoHandler.OnPageEvent (pct: TPageEvent; id, id2: integer);
begin
   case pct of
   	pctAddChild:
      	if (PageStore[id2].PageType = ptMemoItem) then
      		FMemoList.Add (id2);
      pctRemoveChild:
      	if (PageStore[id2].PageType = ptMemoItem) then
      		FMemoList.DeleteByValue (id2);
   end;
end;

procedure TMemoHandler.f_OnTimer (sender: TObject);
var i: integer;
   o_item: TMemoItem;
   ctime: TSystemTime;
begin
	if FTimer.Interval <> 60000 then
   	FTimer.Interval := 60000;
   ctime := Now;
   for i := FMemoList.Low to Fmemolist.High do
   begin
      o_item := TMemoItem(PageStore[FMemoList[i]]);
      if o_item.Memo.CheckTime (ctime) then
      begin
      	o_item.Memo.Reminded := true;
         SaveMan.Save;   // 防止对于自动关机等脚本，关机后来不及保存已经执行的记号。
         case o_item.Memo.Action of
            mmReminder:
               FPopupManager.PopupReminder (o_item);
            mmExecuteLink:
            	ExecBatchLink (o_item.Memo.Description);
         end;
         PageCenter.EventNotify (pctFieldModified, o_item.id);
      end;
	end;
end;

end.
