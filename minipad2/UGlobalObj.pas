unit UGlobalObj;

interface

uses Windows, UxlClasses, UxlWinClasses, UxlExtClasses, UxlFunctions, UxlList, UxlIniFile, UxlWinControl, UxlWindow, UxlStrUtils,
	UxlFile, UxlMiscCtrls, UxlDialog, UTypeDef, UCommonClasses;

type
   ISizeObserver = interface
   	procedure AdjustAppearance ();
   end;

   TSizeManager = class (TxlInterfacedObject, IOptionObserver)
	private
      FAutoAdjust: boolean;
      FLevel: integer;
      FObservers: TxlInterfaceList;
      FWidth, FHeight: integer;

   	constructor Create ();
		procedure NotifyObservers ();
		function f_CompareVal (i_val, i_baseval: integer): boolean;
   public
      destructor Destroy (); override;
   	procedure AddObserver (observer: ISizeObserver);
      procedure RemoveObserver (observer: ISizeObserver);

      procedure OptionChanged ();
      procedure WinSized (i_width, i_height: integer);

      function ShowTree (): boolean;
      function ShowTab (): boolean;
      function MultiLineTab (): boolean;
      function ShowToolbar (): boolean;
      function ShowMenu (): boolean;
   end;

   TBackupHandler = class (TxlInterfacedObject, IOptionObserver)
   private
      FTimer: TxlTimer;
      FIntervalType: TBackupIntervalType;
      FInterval: integer;
      FTotalBackup: integer;
      FBackupPath: widestring;

      procedure f_DoBackup ();
   	procedure f_OnTimer (Sender: TObject);
      procedure OptionChanged ();
   public
      constructor Create ();
      destructor Destroy (); override;
   end;

   TSaveManager = class (TxlSaveCenter, IOptionObserver)
   private
      FTimer: TxlTimer;
      FActive: boolean;
      
      procedure OptionChanged ();
   	procedure f_OnSaveTimer (Sender: TObject);
   public
      constructor Create (); override;
      destructor Destroy (); override;
   	procedure Save (); override;
      property Active: boolean read FActive write FActive;
   end;

   TLogInHandler = class
   private
      FTime: Int64;
      FDialogShown: boolean;
      FValidatedOnce: boolean;
   public
   	constructor Create ();
      function ValidateLogIn (): boolean;
      procedure NeedReValidate ();
   end;

function GetFullLink (const s_link: widestring; linktype: TLinkType; b_addprefix, b_fullpath: boolean): widestring;

function DataDir(): widestring;
function SoundDir(): widestring;
function DictDir(): widestring;
function LangDir(): widestring;
function Version(): widestring;

function EventMan: TxlEventCenter;
function CommandMan: TxlCommandCenter;
function SaveMan: TSaveManager;
function SizeMan: TSizeManager;
function ListCenter: TxlListCenter;
function LogInHandler (): TLogInHandler;

var
   ClipWatcher: TClipWatcher;
   ProgTip: TxlTrackingTip;
   GSearchText: widestring;     // 记忆搜索文字的全局变量
   GMatchCase: boolean;
   
implementation

uses CommCtrl, Resource, UOptionBox, UxlMath, UOptionManager, UxlDateTimeUtils, UDialogs;

var FEventMan: TxlEventCenter;
    FCommandMan: TxlCommandCenter;
    FSaveMan: TSaveManager;
    FSizeMan: TSizeManager;
	 FListCenter: TxlListCenter;
	 FLogInHandler: TLogInHandler;

function EventMan(): TxlEventCenter;
begin
	if not assigned (FEventMan) then
   	FEventMan := TxlEventCenter.Create;
   result := FEventMan;
end;

function CommandMan(): TxlCommandCenter;
begin
	if FCommandMan = nil then
	   FCommandMan := TxlCommandCenter.Create;
   result := FCommandMan;
end;

function ListCenter(): TxlListCenter;
begin
	if not assigned (FListCenter) then
		FListCenter := TxlListCenter.Create;
   result := FListCenter;
end;

function SaveMan(): TSaveManager;
begin
	if FSaveMan = nil then
      FSaveMan := TSaveManager.Create;
   result := FSaveman;
end;

function SizeMan(): TSizeManager;
begin
	if not assigned (FSizeMan) then
   begin
   	FSizeMan := TSizeManager.Create;
   	OptionMan.AddObserver(FSizeMan);
   end;
   result := FSizeMan;
end;

//--------------------------

constructor TSizeManager.Create ();
begin
	FObservers := TxlInterfaceList.Create;
end;

destructor TSizeManager.Destroy ();
begin
	FObservers.Free;
   inherited;
end;

procedure TSizeManager.AddObserver (observer: ISizeObserver);
begin
	FObservers.Add (observer);
   observer.AdjustAppearance;
end;

procedure TSizeManager.RemoveObserver (observer: ISizeObserver);
begin
	FObservers.Remove (observer);
end;

procedure TSizeManager.NotifyObservers ();
var i: integer;
begin
   for i := FObservers.Low to FObservers.High do
   	ISizeObserver(FObservers[i]).AdjustAppearance;
end;

procedure TSizeManager.OptionChanged ();
begin
	FAutoAdjust := OptionMan.Options.AutoAdjust;
   FLevel := ConfineRange (OptionMan.Options.AutoAdjustLevel, 1, 5);
   NotifyObservers;
end;

procedure TSizeManager.WinSized (i_width, i_height: integer);
begin
	if (i_width <> FWidth) or (i_height <> FHeight) then
   begin
      FWidth := i_width;
      FHeight := i_height;
      NotifyObservers;
   end;
end;

function TSizeManager.f_CompareVal (i_val, i_baseval: integer): boolean;
begin
	if FAutoAdjust then
   	result := i_val > (i_baseval + FLevel * 20)
   else
		result := true;
end;

function TSizeManager.ShowTree (): boolean;
begin
	result := f_CompareVal (FWidth, 320);
end;

function TSizeManager.MultiLineTab (): boolean;
begin
   result := f_CompareVal (FHeight, 280);
end;

function TSizeManager.ShowToolbar (): boolean;
begin
	result := f_CompareVal (FHeight, 240);
end;

function TSizeManager.ShowMenu (): boolean;
begin
	result := f_CompareVal (FHeight, 200);
end;

function TSizeManager.ShowTab (): boolean;
begin
   result := f_CompareVal (FHeight, 160);
end;

//------------------

constructor TBackupHandler.Create ();
begin
   FTimer := TimerCenter.NewTimer;
   FTimer.OnTimer := f_OnTimer;
   FTimer.Interval := 60000;     // 每分钟检查一次是否该备份
   OptionMan.AddObserver (self);
end;

destructor TBackupHandler.Destroy ();
begin
	TimerCenter.ReleaseTimer (FTimer);
   OptionMan.RemoveObserver (self);
   inherited;
end;

procedure TBackupHandler.OptionChanged ();
begin
	FIntervalType := OptionMan.Options.BackupIntervalType;
   FInterval := OptionMan.Options.BackupInterval;
   if FIntervalType = btHour then
      Finterval := Finterval * 60
   else if FIntervalType = btDay then
      Finterval := Finterval * 60 * 24;
   FTimer.Enabled := (FInterval > 0);

   FTotalBackup := OptionMan.Options.TotalBackup;
   FBackupPath := RelToFullPath (OptionMan.Options.BackupPath, ProgDir);
end;

procedure TBackupHandler.f_OnTimer (Sender: TObject);
begin
	if FIntervalType = btSaveCount then      // 即使在这种情况下，最快也就是一分钟备份一次
   begin
      if MemoryMan.SaveCount - MemoryMan.LastBackupSaveCount >= FInterval then
         f_DoBackup
   end
   else
   begin
      if SystemTimeToInt64 (Now, ittMinute) - MemoryMan.LastBackupTime >= FInterval then
      	f_DoBackup;
   end;
end;

procedure TBackupHandler.f_DoBackup ();
var s_path2, s_delpath: widestring;
	o_list, o_list2: TxlStrList;
   i, j, i_total: integer;
begin
   if FBackupPath = '' then exit;
   if not PathFileExists (FBackupPath) then
   	CreateDir (FBackupPath);
   s_path2 := FBackupPath + 'mpbak ' + ReplaceStr(SystemTimeToString (Now, dtmDateTimeWithSecond), ':', '-') + '\';
	CopyFiles (DataDir, s_path2);
   MemoryMan.LastBackupTime := SystemTimeToInt64 (Now, ittMinute);
   MemoryMan.LastBackupSaveCount := MemoryMan.SaveCount;

   i_total := FTotalBackup;
   o_list := TxlStrList.Create;
   FindSubDir (FBackupPath, o_list);
   for i := o_list.High downto o_list.low do
      if not IsSameStr(LeftStr(LowerCase(o_list[i]), 6), 'mpbak ') then
         o_list.Delete(i);
   if o_list.count > i_total then
   begin
   	for i := o_list.Low to o_list.High do
	   	o_list.Keys[i] := o_list[i];
      o_list.SortByKey();
      o_list2 := TxlStrList.Create;
      for i := 0 to o_list.count - i_total - 1 do
      begin
      	s_delpath := FBackupPath + o_list[i];
      	FindFiles (s_delpath + '*.*', o_list2);
         for j := o_list2.Low to o_list2.High do
         	DeleteFile (s_delpath + o_list2[j]);
         RemoveDirectory (s_delpath);
      end;
      o_list2.free;
   end;
   o_list.free;
end;

//---------------------

constructor TSaveManager.Create ();
begin
	inherited Create;
   FTimer := TimerCenter.NewTimer;
   FTimer.OnTimer := f_OnSaveTimer;
   OptionMan.AddObserver (self);
end;

destructor TSaveManager.Destroy ();
begin
	TimerCenter.ReleaseTimer (FTimer);
   OptionMan.RemoveObserver (self);
   inherited;
end;

procedure TSaveManager.OptionChanged ();
begin
   FTimer.Interval := OptionMan.Options.AutoSaveTime * 60000;
   FTimer.Enabled := (OptionMan.Options.AutoSaveTime > 0);
end;

procedure TSaveManager.f_OnSaveTimer (Sender: TObject);
begin
	if Active then
		Save;
end;

procedure TSaveManager.Save ();
begin
//   EventMan.EventNotify ()
   inherited;
   inc (MemoryMan.SaveCount);
   CommandMan.ItemEnabled [m_save] := false;
end;

//--------------------

function LogInHandler (): TLogInHandler;
begin
   if not assigned (FLogInHandler) then
   	FLogInHandler := TLogInHandler.Create;
   result := FLogInHandler;
end;

constructor TLogInHandler.Create ();
begin
	FTime := 0;
   FDialogShown := false;
   FValidatedOnce := false;
end;

function TLogInHandler.ValidateLogIn (): boolean;
var i_time: Int64;
begin
	if FDialogShown then     // 按了一次托盘区后，有可能再按一次再次弹出登录窗口
   	result := false
   else
   begin
      result := true;
      if not OptionMan.Options.NeedLogIn then exit;
      if FValidatedOnce then  // 从托盘区恢复时再次确认密码
      begin
      	if OptionMan.Options.LockTrayTime < 0 then exit;     // 托盘区恢复时不需要输入密码
         i_time := SystemTimeToInt64(Now, ittMinute);
         if (i_time - FTime) < OptionMan.Options.LockTrayTime then exit;  // 恢复时，时间尚未达到设定锁定时间
      end;

      FDialogShown := true;
      with TLogInBox.Create () do
      begin
         Password := OptionMan.Options.Password;
         result := Execute;
         free;
      end;
      FDialogShown := false;
      FValidatedOnce := true;
   end;
end;

procedure TLogInHandler.NeedReValidate ();
begin
	FTime := SystemTimeToInt64 (Now, ittMinute);
end;

//------------------

function GetFullLink (const s_link: widestring; linktype: TLinkType; b_addprefix, b_fullpath: boolean): widestring;
var s, s_prefix: widestring;
begin
	result := Trim (s_link);
   if result = '' then exit;
	s := LowerCase(result);
	s_prefix := '';
   case linktype of
   	ltFile, ltFolder:
      	begin
         	if b_fullpath then
               result := RelToFullPath (result, ProgDir);
            if b_addprefix and (LeftStr(s, 5) <> 'file:') then
               s_prefix := 'file://';
         end;
      ltPage:
      	if (leftstr(s, 5) <> 'http:') and (leftstr(s, 6) <> 'https:') and (leftstr(s, 4) <> 'ftp:') then
         	s_prefix := 'http://';
		ltEmail:
      	if LeftStr(s, 7) <> 'mailto:' then
            s_prefix := 'mailto:';
      ltNode:
      	if leftstr(s, 8) <> 'outlook:' then
         	s_prefix := 'outlook://';
   end;
   result := s_prefix + result;
end;

function f_GetSubDir (const s: widestring; b_autocreate: boolean = true): widestring;
begin
   result := ProgDir + s;
   if (not PathFileExists (result)) and b_autocreate then
      CreateDir (result);
end;

function DataDir(): widestring;
begin
	result := f_GetSubDir ('data\');
end;

function SoundDir (): widestring;
begin
   result := f_GetSubDir ('sound\');
end;

function DictDir (): widestring;
begin
   result := f_GetSubDir ('dict\');
end;

function LangDir (): widestring;
begin
   result := f_GetSubDir ('lang\', not IsLocaleChinese);
end;

function Version (): widestring;
begin
	result := '3.2.0';
end;

//----------------

initialization

finalization
//   OptionMan.RemoveObserver(SizeMan);      // 不能加，因optionman等对象可能已经销毁
//   OptionMan.RemoveObserver (SaveMan);
//   OptionMan.RemoveObserver(LangMan);

//   MemoryMan.RemoveObserver (SaveMan);
//   SaveMan.RemoveObserver (MemoryMan);

   FreeAndNil (FSaveMan);
   FreeAndNil (FSizeMan);
   FreeAndNil (FEventMan);
   FreeAndNil (FCommandMan);
   FreeAndNil (FListCenter);
	FreeAndNil (FLogInHandler);

end.


























