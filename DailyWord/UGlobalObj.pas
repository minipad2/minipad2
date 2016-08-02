unit UGlobalObj;

interface

uses Windows, UxlClasses, UxlExtClasses, UxlWinControl, UxlMiscCtrls, UDisplay, UDisplayManager;

type
	TOptions = record
      DisplayMode: TDisplayMode;
      TwoLines: boolean;
   	WinColor: TColor;
      WinFont: TxlFont;
      Transparency: TTransparency;
   	DictFile: widestring;
      StartHotKey: THotKey;
      FullScreenHotKey: THotKey;
      TrackMouseHotKey: THotKey;
      AutoRun: boolean;
      TrackXSpace: integer;
      TrackYSpace: integer;

      BoxSize: integer;
      AutoNewBox: boolean;
      NewBoxCycle: integer;
      AutoTestMode: boolean;
      TestModeCycle: integer;
      RandomNewBox: boolean;
      RandomSwitchWord: boolean;
      SwitchWordInterval: integer;
      TestModeType: TTestModeType;
      TestModeDelay: integer;
	end;

   TOptionManager = class (TxlOption)
   private
   	FOptions: TOptions;
   protected
   	procedure OnCreate (); override;
      procedure OnDestroy (); override;
   	procedure DoLoad (); override;
      procedure DoSave (); override;
      function DoSetOptions (WndParent: TxlWinControl): boolean; override;
   public
      property Options: TOptions read FOptions write FOptions;
   end;

   TMemoryManager = class (TxlMemory)
   protected
   	procedure DoLoad (); override;
      procedure DoSave (); override;
   public
   	WinPos: TPoint;
      SwitchCount: integer;
      BoxIndex: integer;
      WordIndex: integer;
      Started: boolean;
      TestMode: boolean;
      TrackMouse: boolean;
	end;

var
   OptionMan: TOptionManager;
   MemoryMan: TMemoryManager;
   CommandMan: TxlCommandCenter;
   ProgTip: TxlTrackingTip;

implementation

uses UxlIniFile, UxlFunctions, UDialogs, UxlStrUtils, UxlMath, UxlDialog;

procedure TOptionManager.OnCreate ();
begin
	FOptions.WinFont := TxlFont.Create;
end;

procedure TOptionManager.OnDestroy ();
begin
	FOptions.WinFont.free;
end;

procedure TOptionManager.DoLoad ();
var o_inifile: TxlIniFile;
	s_seg: widestring;
begin
	o_inifile := TxlIniFile.Create (ProgIni);

   o_inifile.Cache.ReadSection ('Program');
	with FOptions do
   begin
   	DictFile := o_inifile.Cache.GetString ('DictFile', '');
      if (DictFile = '') and PathFileExists (ProgDir + 'Demo.dwd') then
      	DictFile := 'Demo.dwd';
      StartHotKey := o_inifile.Cache.GetInteger ('StartHotKey', 0);
      FullScreenHotKey := o_inifile.Cache.GetInteger ('FullScreenHotKey', 0);
      TrackMouseHotKey := o_inifile.Cache.GetInteger ('TrackMouseHotKey', 0);
   end;

   o_inifile.Cache.ReadSection ('Appearance');
   with FOptions do
   begin
      DisplayMode := TDisplayMode(o_inifile.Cache.GetInteger ('DisplayMode', Ord(dmFixed)));
      TwoLines := o_inifile.Cache.GetBool ('TwoLines', false);
   	WinColor := TColor (o_inifile.Cache.GetInteger ('WinColor', rgb(0,0,0)));     //12319917
      Transparency := o_inifile.Cache.GetInteger ('Transparency', 4);
      TrackXSpace := o_inifile.Cache.GetInteger ('TrackXSpace', 20);
      TrackYSpace := o_inifile.Cache.GetInteger ('TrackYSpace', 15);
   end;

   o_inifile.Cache.ReadSection ('WordBox');
   with FOptions do
   begin
      BoxSize := o_inifile.Cache.GetInteger ('BoxSize', 50);
      AutoNewBox := o_inifile.Cache.GetBool ('AutoNewBox', true);
      NewBoxCycle := o_inifile.Cache.GetInteger ('NewBoxCycle', 3);
      AutoTestMode := o_inifile.Cache.GetBool ('AutoTestMode', false);
      TestModeCycle := o_inifile.Cache.GetInteger ('TestModeCycle', 2);
      RandomNewBox := o_inifile.Cache.GetBool ('RandomNewBox', false);
      RandomSwitchWord := o_inifile.Cache.GetBool ('RandomSwitchWord', false);
      SwitchWordInterval := o_inifile.Cache.GetInteger ('SwitchWordInterval', 5);
      TestModeType := TTestModeType (o_inifile.Cache.GetInteger ('TestModeType', Ord(tmtRandom)));
      TestModeDelay := o_inifile.Cache.GetInteger ('TestModeDelay', 2);
   end;

   o_inifile.Cache.ReadSection ('Font');
   with FOptions.WinFont do
   begin
      Name := o_inifile.Cache.GetString ('FontName', 'Arial');
      color := o_inifile.Cache.GetInteger('FontColor', RGB(255, 255, 255));
      Size := o_inifile.Cache.GetInteger('FontSize', 9);
      Bold := o_inifile.Cache.GetBool ('FontBold', false);
      Italic := o_inifile.Cache.GetBool ('FontItalic', false);
      Underline := o_inifile.Cache.GetBool ('FontUnderline', false);
      StrikeOut := o_inifile.Cache.GetBool ('FontStrikeOut', false);
	end;

	o_inifile.Free;
end;

procedure TOptionManager.DoSave ();
var s_seg: widestring;
	o_inifile: TxlIniFile;
begin
	o_inifile := TxlIniFile.Create (ProgIni);
	with o_inifile.Cache do
   begin
      AddString ('DictFile', FOptions.DictFile);
      AddInteger ('StartHotKey', FOptions.StartHotkey);
      AddInteger ('FullScreenHotKey', FOptions.FullScreenHotkey);
      AddInteger ('TrackMouseHotKey', FOptions.TrackMouseHotKey);
      WriteSection ('Program');

      AddInteger ('DisplayMode', Ord(FOptions.DisplayMode));
      AddBool ('TwoLines', FOptions.TwoLines);
   	AddInteger ('WinColor', Ord(FOptions.WinColor));
      AddInteger ('Transparency', FOptions.Transparency);
      AddInteger ('TrackXSpace', FOptions.TrackXSpace);
      AddInteger ('TrackYSpace', FOptions.TrackYSpace);
      WriteSection ('Appearance');
      
      AddInteger ('BoxSize', FOptions.BoxSize);
      AddBool ('AutoNewBox', FOptions.AutoNewBox);
      AddInteger ('NewBoxCycle', FOptions.NewBoxCycle);
      AddBool ('AutoTestMode', FOptions.AutoTestMode);
      AddInteger ('TestModeCycle', FOptions.TestModeCycle);
      AddBool ('RandomNewBox', FOptions.RandomNewBox);
      AddBool ('RandomSwitchWord', FOptions.RandomSwitchWord);
      AddInteger ('SwitchWordInterval', FOptions.SwitchWordInterval);
      AddInteger ('TestModeType', Ord(FOptions.TestModeType));
      AddInteger ('TestModeDelay', FOptions.TestModeDelay);
      WriteSection ('WordBox');

      AddString ('FontName', FOptions.WinFont.name);
      AddInteger ('FontColor', FOptions.WinFont.color);
      AddInteger ('FontSize', FOptions.WinFont.size);
      AddBool ('FontBold', FOptions.WinFont.bold);
      AddBool ('FontItalic', FOptions.WinFont.italic);
      AddBool ('FontUnderline', FOptions.WinFont.Underline );
      AddBool ('FontStrikeOut', FOptions.WinFont.strikeout);
      WriteSection ('Font');
   end;
   o_inifile.Free;
end;

function TOptionManager.DoSetOptions (WndParent: TxlWinControl): boolean;
var o_box: TOptionBox;
	o_reg: TxlRegistry;
   s: widestring;
begin
   o_reg := Txlregistry.create (HKEY_LOCAL_MACHINE);
   if o_reg.openkey ('Software\Microsoft\Windows\CurrentVersion\Run', false) then
   begin
      s := o_reg.ReadString ('DailyWord');
   	o_reg.closekey;
      FOptions.AutoRun := IsSameStr (ProgExe, s);
   end;

   o_box := TOptionBox.Create (WndParent, dpScreenCenter);
   o_box.Options := FOptions;
   result := o_box.Execute();

   if result then
   begin
      if o_box.Options.AutoRun <> FOptions.AutoRun then
      begin
         if o_reg.openkey ('Software\Microsoft\Windows\CurrentVersion\Run', false) then
         begin
            if o_box.Options.AutoRun then
               o_reg.WriteString ('DailyWord', ProgExe)
            else
               o_reg.deletevalue ('DailyWord');
            o_reg.closekey;
         end;
      end;
      FOptions := o_box.options;
   end;
   o_box.Free;
   o_reg.free;
end;

//--------------

procedure TMemoryManager.DoLoad ();
var o_inifile: TxlIniFile;
begin
	o_inifile := TxlIniFile.Create (ProgIni);
	with o_inifile.Cache do
   begin
   	ReadSection ('Memory');
   	WinPos.X := GetInteger ('WinPosX', 200);
      WinPos.Y := GetInteger ('WinPosY', 0);
      SwitchCount := GetInteger ('SwitchCount', 0);
      BoxIndex := GetInteger ('BoxIndex', -1);
      WordIndex := GetInteger ('WordIndex', -1);
      Started := GetBool ('Started', true);
		TrackMouse := GetBool ('TrackMouse', false);
      TestMode := GetBool ('TestMode', false);
   end;
   o_inifile.Free;
end;

procedure TMemoryManager.DoSave ();
var o_inifile: TxlIniFile;
begin
	o_inifile := TxlIniFile.Create (ProgIni);
	with o_inifile.Cache do
   begin
   	AddInteger ('WinPosX', WinPos.X);
      AddInteger ('WinPosY', WinPos.Y);
      AddInteger ('SwitchCount', SwitchCount);
      AddInteger ('BoxIndex', BoxIndex);
      AddInteger ('WordIndex', WordIndex);
      AddBool ('Started', Started);
      AddBool ('TrackMouse', TrackMouse);
      AddBool ('TestMode', TestMode);
      WriteSection ('Memory');
   end;
   o_inifile.Free;
end;

initialization
	OptionMan := TOptionManager.Create;
   MemoryMan := TMemoryManager.Create;
   CommandMan := TxlCommandCenter.Create;

finalization
	OptionMan.Free;
   MemoryMan.Free;
   CommandMan.Free;
   
end.








