unit USpecialMode;

interface

uses Windows, UxlWindow, UxlWinClasses, UxlClasses, UxlWinControl, UxlExtClasses, UTypeDef;

type 
   TSpecialMode = class
   private
      FTargetWin: Txlwindow;
		FDirection: TSMDirection;
      FAnimationTime: cardinal;
      FEdgeWidth: cardinal;
      FHideTimer: TxlTimer;
      FShowTimer: TxlTimer;
      FPaused: boolean;
      FEnabled: boolean;
      FShow: boolean;
      FOnHide: TNotifyEvent;
      FOnShow: TNotifyEvent;

      procedure f_OnHideTimer (sender: TObject);
      procedure f_OnShowTimer (sender: TObject);
      function f_ShowWindow (value: boolean): boolean;  // return whether executed;
      procedure SetEnable (value: boolean);
   public
      constructor Create (ATargetWin: TxlWindow);
      destructor Destroy (); override;
      procedure SetOptions (const value: TSMOptions);
      function ShowWindowDemand (): boolean;
//      procedure HideWindowDemand ();  // 由 TPasteManager 触发
		function CursorInWindowRect (): boolean;

      property OnHide: TNotifyEvent read FOnHide write FOnHide;
      property OnShow: TNotifyEvent read FOnShow write FOnShow;
      property Enabled: boolean read FEnabled write SetEnable;
      property WinShow: boolean read FShow;
      property Pause: boolean read FPaused write FPaused;
   end;

   TSpModeManager = class (TxlInterfacedObject, IOptionObserver, IMemorizer, ICommandExecutor, IMessageObserver)
   private
   	FSpecialMode: TSpecialMode;
      FParent: TxlWindow;
      FWinPos: TPos;
      FWinHide: boolean;
      procedure f_OnHide (Sender: TObject);
      procedure f_OnShow (Sender: TObject);
	public
   	constructor Create (ATargetWin: TxlWindow);
      destructor Destroy (); override;
      function ShowWindowDemand (): boolean;
      function WinShow (): boolean;

      procedure OptionChanged ();
      procedure SaveMemory ();
      procedure RestoreMemory ();
      procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
   end;

implementation

uses Messages, UxlDialog, UxlFunctions, UxlCommDlgs, UxlStrUtils, UxlWinDef, UxlMath, UGlobalObj, Resource, UOptionManager;

constructor TSpecialMode.Create (ATargetWin: TxlWindow);
begin
	FTargetWin := ATargetWin;
   FHideTimer := TimerCenter.NewTimer;
   FHideTimer.OnTimer := f_OnHideTimer;
   FShowTimer := TimerCenter.NewTimer;
   FShowTimer.OnTimer := f_OnShowTimer;
   FPaused := false;
   FShow := true;
   FEnabled := false;
end;

destructor TSpecialMode.Destroy ();
begin
	TimerCenter.ReleaseTimer (FHideTimer);
   TimerCenter.ReleaseTimer (FShowTimer);
   inherited;
end;

procedure TSpecialMode.SetOptions (const value: TSMOptions);
begin
   FHideTimer.Interval := Max(value.HideDelay, 100);
   FShowTimer.Interval := Max(value.ShowDelay, 100);
   FDirection := value.Direction;
   FAnimationTime := value.AnimationTime;
   FEdgeWidth := value.EdgeWidth;
end;

procedure TSpecialMode.SetEnable (value: boolean);
begin
   FHideTimer.Enabled := value;
	FShowTimer.Enabled := value;
   FEnabled := value;
end;

procedure TSpecialMode.f_OnHideTimer (sender: TObject);
begin
   if not CursorInWindowRect () then
      f_ShowWindow (false);
end;

procedure TSpecialMode.f_OnShowTimer (sender: TObject);
begin
   if CursorInWindowRect () then
      f_ShowWindow (true);
end;

function TSpecialMode.CursorInWindowRect (): boolean;
var o_point: TPoint;
	rc: TRect;
begin
	GetCursorPos (o_point);
   GetWindowRect (FTargetWin.handle, rc);
	result := PointInRect (o_point, rc);
end;

function TSpecialMode.ShowWindowDemand (): boolean;
begin
   if (FTargetWin.Status = wsMinimize) then
   begin
      FShow := true;
      result := false;
   end
   else
   begin
      result := f_ShowWindow (true);
   end;
end;

function TSpecialMode.f_ShowWindow (value: boolean): boolean;
   function f_findnearest(o_win: TxlWindow): TSMDirection;
   var i_distance: array[1 .. 4] of integer;
      i, j: cardinal;
      rcs: TRect;
   begin
   	GetScreenRect (rcs);
      with o_win.pos do
      begin
         i_distance[1] := x;
         i_distance[2] := y;
         i_distance[3] := rcs.right - (x + width);
         i_distance[4] := rcs.bottom - (y + height);
      end;
      i := 1;
      for j := 2 to 4 do
         if i_distance[j] < i_distance[i] then i := j;
      result := TSMDirection(i);
   end;

   procedure f_MoveWindow (o_win: Txlwindow; o_targetpos: TPos; i_anitime: cardinal);
   var i, i_step_x, i_step_y, i_time: cardinal;
      o_pos: TPos;
   const c_stepcount = 20;
   begin
      FHideTimer.Enabled := false;
      FShowTimer.Enabled := false;
      if i_anitime >= 20 then
      begin
         i_time := i_anitime div c_stepcount;
         o_pos := o_win.pos;
         i_step_x := (o_targetpos.x - o_pos.x) div c_stepcount;
         i_step_y := (o_targetpos.y - o_pos.y) div c_stepcount;
         for i := 1 to c_stepcount do
         begin
            inc (o_pos.x, i_step_x);
            inc (o_pos.y, i_step_y);
            o_win.pos := o_pos;
            sleep (i_time);
         end;
      end;
      o_win.pos := o_targetpos;  // 确保不受div误差的影响，精确地到达设定位置。
      FHideTimer.Enabled := true;
      FShowTimer.Enabled := true;
   end;

var o_dir: TSMDirection;
	o_pos: TPos;
   rcs: TRect;
begin
   result := false;
   if FPaused then exit;
   if FShow = value then exit;

   SendKeyPress (VK_LBUTTON, false);
   o_pos := FTargetWin.Pos;
   GetScreenRect (rcs);
   if value then
   begin
      if o_pos.x = Fedgewidth - o_pos.width then  // hdLeft
         o_pos.x := 0
      else if o_pos.y = Fedgewidth - o_pos.height then   //hdTop
         o_pos.y := 0
      else if o_pos.x = rcs.right - Fedgewidth then  //hdRight
         o_pos.x := rcs.right - o_pos.width
      else    // hdBottom
         o_pos.y := rcs.bottom - o_pos.height;
      f_MoveWindow (FTargetWin, o_pos, FAnimationTime);
      FTargetWin.Redraw;
   end
   else
   begin
      o_dir := FDirection;
      if o_dir = hdFree then
         o_dir := f_findnearest (FTargetWin);
      case o_dir of
      	hdLeft: o_pos.x := Fedgewidth - o_pos.width;
         hdTop: o_pos.y := Fedgewidth - o_pos.height;
         hdRight: o_pos.x := rcs.right - Fedgewidth;
         hdBottom: o_pos.y := rcs.bottom - Fedgewidth;
      end;
   	f_MoveWindow (FTargetWin, o_pos, FAnimationTime);
   end;
   FShow := value;
   result := true;
   if value and assigned(FOnShow) then
		FOnShow (self)
   else if (not value) and assigned(FOnHide) then
   	FOnHide (self);
end;

//-----------------------

constructor TSPModeManager.Create (ATargetWin: TxlWindow);
begin
   FParent := ATargetWin;
	FSpecialMode := TSpecialMode.Create (ATargetWin);
   FSpecialMode.OnHide := f_OnHide;
   FSpecialMode.OnShow := f_OnShow;

   OptionMan.AddObserver(self);
   CommandMan.AddExecutor(self);
   MemoryMan.AddObserver(self);
   FParent.AddMessageObserver (self);
end;

destructor TSPModeManager.Destroy ();
begin
	FParent.RemoveMessageObserver (self);
	OptionMan.RemoveObserver(self);
   MemoryMan.RemoveObserver(self);
   CommandMan.RemoveExecutor(self);
	FSpecialMode.Free;
   inherited;
end;

function TSPModeManager.ShowWindowDemand (): boolean;
begin
	result := FSpecialMode.ShowWindowDemand;
end;

procedure TSPModeManager.OptionChanged ();
begin
	FSpecialMode.SetOptions (OptionMan.Options.SMOptions);
   if not FSpecialMode.Enabled then
   	FParent.ToolWindowStyle := OptionMan.options.ToolWindowStyle;
end;

procedure TSPModeManager.SaveMemory ();
begin
	MemoryMan.specialmode := CommandMan.ItemChecked [m_specialmode];
	MemoryMan.StayOnTop := CommandMan.ItemChecked[m_stayontop];
end;

procedure TSPModeManager.RestoreMemory ();
begin
	if MemoryMan.SpecialMode then
   	CommandMan.ExecuteCommand (m_specialmode);
	if MemoryMan.stayontop then
   	CommandMan.ExecuteCommand (m_stayontop);
end;

procedure TSPModeManager.ExecuteCommand (opr: word);
var h: HWND;
begin
	case opr of
   	m_specialmode:
         begin
         	h := GetFocus ();
            CommandMan.SwitchCheck(m_specialmode);
            FSpecialMode.Enabled := CommandMan.ItemChecked [m_specialmode];
            if FSpecialMode.Enabled then
            begin
            	FParent.StayOnTop := true;
               FParent.ToolWindowStyle := true;
            end
            else
            begin
            	FParent.StayOnTop := CommandMan.ItemChecked[m_stayontop];
               FParent.ToolWindowStyle := OptionMan.options.ToolWindowStyle;
            end;
            SetFocus (h);
         end;
      m_stayontop:
      	begin
            CommandMan.SwitchCheck (m_stayontop);
            if not FSpecialMode.Enabled then
            	FParent.StayOnTop := CommandMan.ItemChecked[m_stayontop];
         end;
	end;
end;

function TSPModeManager.CheckCommand (opr: word): boolean;
begin
	result := true;
end;

function TSPModeManager.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
begin
	b_processed := false;
	case AMessage of
   	WM_MOVING, WM_SIZING:
      	if (not FSpecialMode.Pause) and (not FSpecialMode.CursorInWindowRect) then
           	FSpecialMode.Pause := true;
      WM_DIALOGOPENED, WM_ENTERMENULOOP:
      	FSpecialMode.Pause := true;
   	WM_MOVE, WM_SIZE, WM_DIALOGCLOSED, WM_EXITMENULOOP:
         FSpecialMode.Pause := false;
      WM_ENTERSIZEMOVE:
      	begin
	      	FWinPos := FParent.Pos;
            FWinHide := not FSpecialMode.WinShow;
         end;
      WM_EXITSIZEMOVE:
      	if FWinHide then
         begin
         	FParent.width := FWinPos.width;
            FParent.height := FWinPos.height;
         end;
   end;
end;

procedure TSPModeManager.f_OnHide (Sender: TObject);
begin
	SaveMan.Save;
   SaveMan.Active := false;
   MemoryMan.Save;
   CommandMan.ItemEnabled[m_specialmode] := false;
   ClearMemory;
end;

procedure TSPModeManager.f_OnShow (Sender: TObject);
begin
	SaveMan.Active := true;
   CommandMan.ItemEnabled[m_specialmode] := true;
	FParent.BringToTop;
   EventMan.EventNotify(e_ShowFromSPMode);
end;

function TSPModeManager.WinShow (): boolean;
begin
	result := FSpecialMode.WinShow;
end;

end.



//procedure TSpecialMode.HideWindowDemand ();  // 由 TPasteManager 触发
//begin
//	if Enabled then
//   begin
//   	if WinShow then f_ShowWindow (false);
//   end
//   else
//      FTargetWin.Status := wsMinimize;
//end;


