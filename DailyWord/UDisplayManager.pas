unit UDisplayManager;

interface

uses SysUtils, Windows, UxlWindow, UxlExtClasses, UxlClasses, UxlWinClasses, UQueryManager, UDisplay, UWordBox;

type
	TTestModeType = (tmtExpDelay, tmtWordDelay, tmtRandom);

	TDisplayManager = class (TxlInterfacedObject, IOptionObserver, ICommandExecutor, IMemorizer)
   private
      FQueryMan: TQueryManager;
		FDisplayFactory: TDisplayFactory;
      FDisplay: TDisplaySuper;

   	FSwitchTimer: TxlTimer;
      FDelayTimer: TxlTimer;

      FDisplayMode: TDisplayMode;
      FFullScreen: boolean;
      FTrackMouse: boolean;
      FActive: boolean;
      FTestMode: boolean;
      FTestModeCycle: integer;
      FTestModeType: TTestModeType;
      FWord: TWord;

      procedure f_DetermineDisplay ();
		procedure f_OnSwitchTimer (Sender: TObject);
      procedure f_OnDelayTimer (Sender: TObject);
   public
   	constructor Create (AWin: TxlWindow);
      destructor Destroy (); override;
      procedure Pause ();
      procedure Resume ();

      procedure OptionChanged ();
      procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
      procedure SaveMemory ();
      procedure RestoreMemory ();
   end;

implementation

uses Messages, UxlMath, UGlobalObj, Resource;

constructor TDisplayManager.Create (AWin: TxlWindow);
begin
   FQueryMan := TQueryManager.Create (AWin);
   FQueryMan.OnBoxChange := f_OnSwitchTimer;

   FDisplayFactory := TDisplayFactory.Create (AWin);

   FSwitchTimer := TimerCenter.NewTimer;
   FSwitchTimer.OnTimer := f_OnSwitchTimer;
   FDelayTimer := TimerCenter.NewTimer;
	FDelayTimer.OnTimer := f_OnDelayTimer;

   OptionMan.AddObserver (self);
   CommandMan.AddExecutor (self);
   MemoryMan.AddObserver (self);
end;

destructor TDisplayManager.Destroy ();
begin
	MemoryMan.RemoveObserver (self);
	CommandMan.RemoveExecutor (self);
	OptionMan.RemoveObserver (self);

   TimerCenter.ReleaseTimer (FSwitchTimer);
   TimerCenter.ReleaseTimer (FDelayTimer);
   FDisplayFactory.Free;
   FQueryMan.Free;

   inherited;
end;

procedure TDisplayManager.Pause ();
begin
	FSwitchTimer.Stop;
end;

procedure TDisplayManager.Resume ();
begin
	if FActive then
		FSwitchTimer.Start;
end;

procedure TDisplayManager.OptionChanged ();
begin
   FDisplayMode := OptionMan.Options.DisplayMode;
	f_DetermineDisplay;

   FSwitchTimer.Interval := OptionMan.Options.SwitchWordInterval * 1000;
   FDelayTimer.Interval := OptionMan.Options.TestModeDelay * 1000;
   FTestModeType := OptionMan.Options.TestModeType;
   if OptionMan.Options.AutoTestMode then
   	FTestModeCycle := OptionMan.Options.TestModeCycle
   else
   	FTestModeCycle := 0;
end;

procedure TDisplayManager.f_DetermineDisplay ();
var dm: TDisplayMode;
begin
	if not FActive then
   	dm := dmNone
   else if FDisplayMode = dmEmbed then
   	dm := dmEmbed
   else if FFullScreen then
   	dm := dmFullScreen
   else if FTrackMouse then
   	dm := dmTrack
   else
   	dm := dmFixed;
   FDisplay := FDisplayFactory.GetDisplay (dm);
//   CommandMan.CheckCommands;
	CommandMan.ItemEnabled[m_fullscreen] := CheckCommand (m_fullscreen);
   CommandMan.ItemEnabled[m_trackmouse] := CheckCommand (m_trackmouse);
   if FActive then
   begin
   	FSwitchTimer.Start;
      f_OnSwitchTimer (self);
   end;
end;

procedure TDisplayManager.ExecuteCommand (opr: word);
begin
	case opr of
   	m_start:
      	begin
         	FActive := not CommandMan.ItemChecked [m_start];
            CommandMan.ItemChecked [m_start] := FActive;
				FSwitchTimer.Enabled := FActive;
            f_DetermineDisplay;
         end;
      m_testmode:
      	begin
         	FTestMode := not CommandMan.ItemChecked [m_testmode];
            CommandMan.ItemChecked [m_testmode] := FTestMode;
         end;
      m_fullscreen:
      	begin
         	FFullScreen := not CommandMan.ItemChecked [m_fullscreen];
            CommandMan.ItemChecked [m_fullscreen] := FFullScreen;
            f_DetermineDisplay;
         end;
      m_trackmouse:
      	begin
         	FTrackMouse := not CommandMan.ItemChecked [m_trackmouse];
            CommandMan.ItemChecked [m_trackmouse] := FTrackMouse;
            f_DetermineDisplay;
         end;
      m_next:
      	if FDelayTimer.Enabled then
         	FDelayTimer.TimerEvent
         else
         begin
         	FSwitchTimer.TimerEvent;
      		FSwitchTimer.Reset;   // 重新计时
         end;
      m_Copy:
      	begin
            Clipboard.TExt := FDisplay.ExportWord;
            ProgTip.ShowTip ('当前项已复制到剪贴板!');
         end;
   end;
end;

function TDisplayManager.CheckCommand (opr: word): boolean;
begin
	case opr of
      m_fullscreen:
			result := FDisplayMode <> dmEmbed;
      m_trackmouse:
      	result := (FDisplayMode <> dmEmbed) and (not FFullScreen);
      else
			result := true;
   end;
end;

procedure TDisplayManager.SaveMemory ();
begin
	MemoryMan.TestMode := FTestMode;
   MemoryMan.Started := FActive;
   MemoryMan.TrackMouse := FTrackMouse;
end;

procedure TDisplayManager.RestoreMemory ();
begin
	if MemoryMan.TestMode then
   	ExecuteCommand (m_testmode);
   if MemoryMan.Started then
   	ExecuteCommand (m_start);
   if MemoryMan.TrackMouse then
   	ExecuteCommand (m_trackmouse);
end;

procedure TDisplayManager.f_OnSwitchTimer (Sender: TObject);
var o_word: TWord;
	i: integer;
{$J+} const cCycleCount: integer = 0; {$J-}   // 记忆前一次切换时的 Cycle 数
begin
	try
      FWord := FQueryMan.GetWord;
      o_word := FWord;

      if (FTestModeCycle > 0) then
      begin
      	if (cCycleCount < FTestModeCycle) and (FQueryMan.CycleCount >= FTestModeCycle) then  // 自动进入测验模式
         	FTestMode := true
         else if (cCycleCount >= FTestModeCycle) and (FQueryMan.CycleCount < FTestModeCycle) then // 自动退出测验模式
         	FTestMode := false;
         CommandMan.ItemChecked [m_testmode] := FTestMode;
      end;
      cCycleCount := FQueryMan.CycleCount;
      
      if FTestMode and (o_word.word <> '') and (o_word.exp <> '') then
      begin
      	case FTestModeType of
         	tmtExpDelay:
            	o_word.Exp := '?';
            tmtWordDelay:
            	o_Word.Word := '? ';
            else
               begin
                  i := RandomInt (0, 1);
                  if i = 0 then
                     o_word.Word := '? '
                  else
                     o_word.Exp := '?';
               end;
         end;
         if FDelayTimer.Interval < FSwitchTimer.Interval then
         	FDelayTimer.Start;
      end;
      FDisplay.ShowWord (o_word);
   except
   	On E: Exception do
   		FDisplay.ShowString (E.Message);
   end;
end;

procedure TDisplayManager.f_OnDelayTimer (Sender: TObject);
begin
	FDelayTimer.Stop;
   FDisplay.ShowWord (FWord);
end;

end.

