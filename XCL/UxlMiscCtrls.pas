unit UxlMiscCtrls;

interface

uses Windows, Messages, CommCtrl, UxlWinControl, UxlFunctions, UxlClasses, UxlList, UxlStdCtrls, UxlWinClasses;

type
   TUpDownStruct = record
      Min: integer;
      Max: integer;
      Position: integer;
      Increment: integer;
      Buddy: TxlControl;
   end;

   TUDChangeEvent = procedure (newpos: integer) of object;

   TxlUpDown = class (TxlControl)
   private
      FBuddy: TxlWinControl;
      FMin, FMax, FIncrement, FPosition: integer;
      FOnChange: TUDChangeEvent;

      procedure SetMin (i_min: integer);
      procedure SetMax (i_max: integer);
   protected
   	procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
		function DoCreateControl (HParent: HWND): HWND; override;
   public
		function ProcessNotify (code: integer; lParam: DWORD): DWORD; override;
      procedure SetBuddy (o_buddy: TxlWinControl);
      procedure SetPosition (i_pos: integer);
      procedure SetRange (i_min, i_max: integer);
      procedure SetIncrement (i_inc: integer);

      property Min: integer read FMin write SetMin;
      property Max: integer read FMax write SetMax;
      property Position: integer read FPosition write SetPosition;
      property Buddy: TxlWinControl read FBuddy write SetBuddy;
      property Increment: integer read FIncrement write SetIncrement;
      property OnChange: TUDChangeEvent read FOnChange write FOnChange;
   end;

type
   TxlProgressBar = class (TxlControl)
   protected
		function DoCreateControl (HParent: HWND): HWND; override;
   public
      procedure SetRange (i_min, i_max: cardinal);
      procedure SetPosition (i_pos: cardinal);
      procedure IncPosition (i_inc: cardinal);
      procedure GoStart ();
      procedure GoEnd ();
   end;

type
	TxlSlideSuper = class (TxlControl)
   private
      FDragMode: boolean;
      FOnStartSlide: TNotifyEvent;
      FOnEndSlide: TNotifyEvent;
      FSlideColor: TColor;
   protected
      procedure OnCreateControl (); override;
   	function IsUserClass(): boolean; override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
      function IsVertStyle (): boolean; virtual; abstract;
      procedure f_OnEndSlide (); virtual;
   public
   	property OnStartSlide: TNotifyEvent read FOnStartSlide write FOnStartSlide;
   	property OnEndSlide: TNotifyEvent read FOnEndSlide write FOnEndSlide;
	end;

   TxlVertSlide = class (TxlSlideSuper)
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
   	function IsVertStyle (): boolean; override;
	end;

   TxlHorzSlide = class (TxlSlideSuper)
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
   	function IsVertStyle (): boolean; override;
	end;

   TxlSplitterSuper = class (TxlSlideSuper)
   private
      FList_Buddy1, FList_Buddy2: TxlObjList;
      FButton: TxlButton;
      FOnButtonClick, FOnButtonRightClick: TNotifyEvent;

      procedure Slide (i_newpos: integer);
      function GetSlidePos (): integer;
      procedure f_OnButtonclick (Sender: TObject);
      procedure f_OnButtonRightclick (Sender: TObject);
      procedure SetShowButton (value: boolean);
   protected
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
      procedure OnSize (clpos: TPos); override;
      procedure f_OnEndSlide (); override;
   public
      procedure SetBuddy (buddy1, buddy2: array of TxlWinControl); overload;
      procedure SetBuddy (buddy1, buddy2: TxlObjList); overload;
      procedure ClearBuddy ();
   	property SlidePos: integer read GetSlidePos write Slide;

      property ShowButton: boolean write SetShowButton;
      property OnButtonclick: TNotifyEvent read FOnButtonClick write FOnButtonclick;
      property OnButtonRightClick: TNOtifyEvent read FOnButtonRightclick write FOnButtonRightClick;
   end;

   TxlVertSplitter = class (TxlSplitterSuper)
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
   	function IsVertStyle (): boolean; override;
	end;

   TxlHorzSplitter = class (TxlSplitterSuper)
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
   	function IsVertStyle (): boolean; override;
	end;

type
   TxlHotKey = class (TxlControl)
   private
   	FOnchange: TNotifyEvent;
      procedure f_SetHotKey (value: THotKey);
      function f_GetHotKey (): THotKey;
   protected
		function DoCreateControl (HParent: HWND): HWND; override;
      function ProcessCommand (dwEvent: word): dword; override;
   public
      property HotKey: THotKey read f_GetHotKey write f_SetHotKey;
      property Onchange: TNotifyEvent read FOnChange write FOnChange;
   end;

type
   TxlDateTimePicker = class (TxlControl)
   private
      procedure f_SetDateTime (const value: TSystemTime);
   protected
      FFormat: widestring;
		function DoCreateControl (HParent: HWND): HWND; override;
      function f_datetimestyle (): DWORD; virtual;
      function f_GetDateTime (): TSystemTime;
   public
      procedure SetFormat (const s_format: widestring);
      property DateTime: TSystemTime read f_GetDateTime write f_SetDateTime;
   end;

   TxlDatePicker = class (TxlDateTimePicker)
   private
      procedure f_SetDate (const value: TSystemTime);
   protected
   public
      property Date: TSystemTime read f_GetDateTime write f_SetDate;
   end;

   TxlTimePicker = class (TxlDateTimePicker)
   private
      procedure f_SetTime (const value: TSystemTime);
   protected
      function f_datetimestyle (): DWORD; override;
   public
      property Time: TSystemTime read f_GetDateTime write f_SetTime;
   end;

type
	TTextAlign = (taLeft, taCenter, taRight);

	TxlStatusBar = class (TxlControl)
   private
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
   public
      procedure SetParts (i_widths: array of integer);
      procedure SetStatusText (i_part: integer; const s_text: widestring; i_align: TTextAlign = taLeft);
   end;

type
	TToolTipStyle = record
   	TipWidth: integer;
      InitialTime: integer;
      AutoPopTime: integer;
   end;
   TTipIcon = (tiNone, tiInfo, tiWarning, tiError);

	TxlToolTipSuper = class (TxlControl)
   private
   	FStyle: TToolTipStyle;
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
      procedure BringTipToTop ();
		procedure SetStyle (const value: TToolTipStyle); virtual;
   	procedure SetTipWidth (value: integer);
      procedure SetColor (i_color: TColor); override;
      function GetColor (): TColor; override;
   public
      property Style: TToolTipStyle read FStyle write SetStyle;
      property TipWidth: integer write SetTipWidth;
   end;

   TxlToolTip = class (TxlToolTipSuper)
   private
      FTipList: TxlIntList;
		procedure f_DoDeleteTool (id: integer);
      function ToolExists (id: integer): boolean;
   protected
   	procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
   public
   	procedure AddTool (id: integer; h_owner: HWND; const rt: TRect; const s_text: widestring);
		procedure ResetTool (id: integer; const rt: TRect; const s_text: widestring);
      procedure DeleteTool (id: integer);
      procedure Clear ();
	end;

   TxlTrackingTip = class (TxlToolTipSuper)    // parent must be TxlWindow
   private
   	FToolInfo: ToolInfoW;
      FInitialTimer: TxlTimer;
      FAutoPopTimer: Txltimer;
      FHideTimer: TxlTimer;
      FHideWhenCursorMove: boolean;
      FTipPos, FCursorPos: TPoint;
      procedure f_OnInitialTimer (Sender: TObject);
      procedure f_OnAutoPopTimer (Sender: TObject);
      procedure f_OnHideTimer (Sender: TObject);
   protected
   	procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
		procedure SetStyle (const value: TToolTipStyle); override;
	public
   	procedure ShowTip (const s_text: widestring; const s_title: widestring = ''; o_icon: TTipIcon = tiNone); overload;
      procedure ShowTip (const pt: TPoint; const s_text: widestring; const s_title: widestring = ''; o_icon: TTipIcon = tiNone); overload;
      procedure HideTip ();
      property HideWhenCursorMove: boolean read FHideWhenCursorMove write FHideWhenCursorMove;
   end;

type
	TxlCalendar = class (TxlControl)
   private
   protected
		function DoCreateControl (HParent: HWND): HWND; override;
	public
   end;

implementation

uses SysUtils, UxlCommDlgs, UxlStrUtils, UxlWinDef, UxlWindow;

function TxlUpDown.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_UPDOWN_CLASS);
	result := CreateWin32Control (HParent, UPDOWN_CLASS, UDS_ALIGNRIGHT or UDS_ARROWKEYS or UDS_SETBUDDYINT);
end;

procedure TxlUpDown.OnCreateControl ();
begin
	SetRange (0, 10);
   SetPosition (1);
   SetIncrement (1);
   FBuddy := nil;
end;

procedure TxlUpDown.OnDestroyControl ();
begin
   DeInitCommonControl();
end;

procedure TxlUpDown.SetMin (i_min: integer);
begin
   SetRange (i_min, Max);
end;

procedure TxlUpDown.SetMax (i_max: integer);
begin
   SetRange (Min, i_max);
end;

procedure TxlUpDown.SetRange(i_min, i_max: integer);
begin
   Perform (UDM_SETRANGE, 0, MAKELONG(i_max, i_min));
	FMin := i_min;
   FMax := i_max;
end;

procedure TxlUpDown.SetPosition (i_pos: integer);
begin
	if (i_pos < FMin) or (i_pos > FMax) then exit;
   Perform (UDM_SETPOS, 0, MakeLong(i_pos, 0));
   FPosition := i_pos;
   if assigned(FBuddy) then  // SetBuddy 有时失效，因此此处手动读取与设置 Buddy 的内容
   	FBuddy.Text := IntToStr (i_pos);
end;

procedure TxlUpDown.SetBuddy (o_buddy: TxlWinControl);
begin
   FBuddy := o_buddy;
   Perform (UDM_SETBUDDY, o_buddy.handle, 0);
end;

procedure TxlUpDown.SetIncrement (i_inc: integer);
begin
	FIncrement := i_inc;
end;

function TxlUpDown.ProcessNotify (code: integer; lParam: DWORD): DWORD;
var pnm: PNMUPDOWN;
	i_newpos, i_oldpos: integer;
begin
	result := 0;
   if (code = UDN_DELTAPOS) then
   begin
   	pnm := PNMUpDown (lParam);
      if assigned (FBuddy) then   // 根据 Buddy 的内容刷新 Position
      	SetPosition (StrToIntDef(FBuddy.text, Position));
      i_oldpos := Position;
      i_newpos := Position + pnm.iDelta * FIncrement;
      SetPosition (i_newpos);
		if (Position <> i_oldpos) and assigned (FOnChange) then
      	FOnChange (Position);
      result := 1;
   end;
end;

//-------------------------

function TxlProgressBar.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_PROGRESS_CLASS);
	result := CreateWin32Control (HParent, PROGRESS_CLASS, PBS_SMOOTH);
end;

procedure TxlProgressBar.SetPosition (i_pos: cardinal);
begin
   Perform (PBM_SETPOS, i_pos, 0);
end;

procedure TxlProgressBar.IncPosition (i_inc: cardinal);
begin
	perform (PBM_DELTAPOS, i_inc, 0);
end;

procedure TxlProgressBar.SetRange (i_min, i_max: cardinal);
begin
	Perform (PBM_SETRANGE, 0, MakeLparam(i_min, i_max));
end;

procedure TxlProgressBar.GoStart ();
begin
   SetPosition (Perform (PBM_GETRANGE, wparam(true), 0));
end;

procedure TxlProgressBar.GoEnd ();
begin
	SetPosition (Perform (PBM_GETRANGE, wparam(false), 0));
end;

//--------------------------

function TxlHotKey.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_HOTKEY_CLASS);
	result := CreateWin32Control (HParent, 'msctls_hotkey32');
end;

procedure TxlHotKey.f_SetHotKey (value: THotKey);
begin
	Perform (HKM_SETHOTKEY, value, 0);
   if assigned (FOnChange) then FOnChange (self);
end;

function TxlHotKey.f_GetHotKey (): THotKey;
begin
	result := Perform (HKM_GETHOTKEY, 0, 0);
end;

function TxlHotKey.ProcessCommand (dwEvent: word): dword;
begin
	result := 0;
   case dwEvent of
      EN_CHANGE:
         if assigned (FOnChange) then FOnChange (self);
	end;
end;

//--------------------------

function TxlDateTimePicker.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_DATE_CLASSES);
   result := CreateWin32Control (HParent, DATETIMEPICK_CLASS, f_datetimestyle);
   FFormat := '';
end;

procedure TxlDatePicker.f_SetDate (const value: TSystemTime);
begin
	if FFormat = '' then
   	SetFormat ('yyyy-MM-dd');
   f_SetDateTime (value);
end;

procedure TxlTimePicker.f_SetTime (const value: TSystemTime);
begin
	if FFormat = '' then
   	SetFormat ('HH:mm');
   f_SetDateTime (value);
end;

procedure TxlDateTimePicker.f_SetDateTime (const value: TSystemTime);
begin
   if FFormat = '' then
      SetFormat ('yyyy-MM-dd HH:mm');
  	DateTime_SetSystemtime (Fhandle, GDT_VALID, value);
end;

function TxlDateTimePicker.f_GetDateTime (): TSystemTime;
begin
	DateTime_GetSystemtime (Fhandle, result);
end;

procedure TxlDateTimePicker.SetFormat(const s_format: widestring);
begin
	FFormat := s_format;
   DateTime_SetFormatW (Fhandle, pwidechar(s_format));
end;

function TxlDateTimePicker.f_datetimestyle (): DWORD;
begin
	result := 0;
end;

function TxlTimePicker.f_datetimestyle (): DWORD;
begin
	result := DTS_UPDOWN;
end;

//-----------------------------

function TxlCalendar.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_DATE_CLASSES);
   result := CreateWin32Control (HParent, MONTHCAL_CLASS, MCS_DAYSTATE);
end;

//---------------------------------

const DefBarWidth = 3;

procedure TxlSlideSuper.OnCreateControl ();
begin
	inherited;
   Move (-1, -1, DefBarWidth, DefBarWidth);
   FSlideColor := GetSysColor (COLOR_BTNFACE);
   Color := FSlideColor;
   SetWndStyle (WS_CLIPSIBLINGS, false);
end;

function TxlSlideSuper.IsUserClass(): boolean;
begin
	result := true;
end;

function TxlSlideSuper.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var b_processed: boolean;
begin
	result := 0;
   b_processed := true;
  	case AMessage of
   	WM_LBUTTONDOWN:
      	begin
         	FDragMode := true;
            self.Color := GetSysColor (COLOR_GRAYTEXT);
			   SetCapture (Fhandle);
            if assigned (FOnStartSlide) then
            	FOnStartSlide (self);
         end;
      WM_MOUSEMOVE:
      	begin
            if not FDragMode then exit;
            if IsVertStyle then
               Left := Parent.CursorPos.x
            else
               Top := Parent.CursorPos.y;
         end;
      WM_LBUTTONUP:
         begin
         	self.Hide;    // 否则会留下残影
            FDragMode := false;
            ReleaseCapture ();
            self.Color := FSlideColor;
            self.Show;
            f_OnEndSlide;
         end;
      else
         b_processed := false;
   end;
   if not b_processed then
      result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

procedure TxlSlideSuper.f_OnEndSlide ();
begin
   if assigned (FOnEndSlide) then
      FOnEndSlide (self);
end;

//----------------------

procedure TxlSplitterSuper.OnCreateControl ();
begin
	inherited;
   FList_Buddy1 := TxlObjList.create;
   FList_Buddy2 := TxlObjList.create;
end;

procedure TxlSplitterSuper.OnDestroyControl ();
begin
	if assigned (FButton) then
   	FButton.Free;
	FList_Buddy1.free;
   FList_Buddy2.free;
   inherited;
end;

procedure TxlSplitterSuper.OnSize (clpos: TPos);
begin
	inherited OnSize (clpos);
   if not assigned (FButton) then exit;
   with FButton do
   begin
   	Left := 0;
      Top := clpos.height div 2 - 10;
      Width := clpos.Width;
      Height := 20;
   end;
end;

procedure TxlSplitterSuper.SetShowButton(value: boolean);
begin
	if value then
   begin
   	if assigned (FButton) then exit;
      FButton := TxlButton.create(self);
      FButton.OnClick := f_OnButtonClick;
      FButton.OnRightClick := f_OnButtonRightClick;
   end
   else
   	if assigned (FButton) then
   		FreeAndNil (FButton);
end;

procedure TxlSplitterSuper.f_OnButtonClick (Sender: TObject);
begin
	if assigned (FOnButtonClick) then
   	FOnButtonClick (self);
end;

procedure TxlSplitterSuper.f_OnButtonRightClick (Sender: TObject);
begin
	if assigned (FOnButtonRightClick) then
   	FOnButtonRightClick (self);
end;

//-----------------------

procedure TxlSplitterSuper.SetBuddy (buddy1, buddy2: array of TxlWinControl);
var i: integer;
begin
	ClearBuddy;
   for i := Low(buddy1) to High(buddy1) do
   	FList_Buddy1.Add (buddy1[i]);
   for i := Low(buddy2) to High(buddy2) do
   	FList_Buddy2.Add (buddy2[i]);
end;

procedure TxlSplitterSuper.SetBuddy (buddy1, buddy2: TxlObjList);
var i: integer;
begin
   ClearBuddy;
   for i := buddy1.Low to buddy1.High do
   	FList_Buddy1.Add (buddy1[i]);
   for i := buddy2.Low to buddy2.High do
   	FList_Buddy2.Add (buddy2[i]);
end;

procedure TxlSplitterSuper.ClearBuddy ();
begin
	FList_Buddy1.clear;
   FList_Buddy2.Clear;
end;

procedure TxlSplitterSuper.Slide (i_newpos: integer);
begin
	if IsVertStyle then
      self.Left := i_newpos
   else
      self.Top := i_newpos;
   f_OnEndSlide;
end;

function TxlSplitterSuper.GetSlidePos (): integer;
begin
	if IsVertStyle then
   	result := self.Left
   else
   	result := self.Top;
end;

procedure TxlSplitterSuper.f_OnEndSlide ();
var i: integer;
	ctrl: TxlWinControl;
begin
	inherited;
	if IsVertStyle then
   begin
      for i := FList_Buddy1.Low to FList_Buddy1.High do
      begin
      	ctrl := TxlWinControl(FList_Buddy1[i]);
         ctrl.Width := self.Left - ctrl.Left;
         ctrl.Redraw;
      end;
      for i := FList_Buddy2.Low to FList_Buddy2.High do
      begin
      	ctrl := TxlWinControl(FList_Buddy2[i]);
         ctrl.Width := ctrl.Right - self.Right;
         ctrl.Left := self.Right;
         ctrl.Redraw;
      end;
   end
   else
   begin
      for i := FList_Buddy1.Low to FList_Buddy1.High do
      begin
      	ctrl := TxlWinControl(FList_Buddy1[i]);
         ctrl.Height := self.Top - ctrl.Top;
         ctrl.Redraw;
      end;
      for i := FList_Buddy2.Low to FList_Buddy2.High do
      begin
      	ctrl := TxlWinControl(FList_Buddy2[i]);
         ctrl.Height := ctrl.Bottom - self.Bottom;
         ctrl.Top := self.Bottom;
         ctrl.Redraw;
      end;
   end;

//   Parent.Update();
end;

//-------------------------

function TxlVertSlide.DoCreateControl (HParent: HWND): HWND;
begin
  	RegisterControlClass ('TxlVertSlide', COLOR_BTNFACE, IDC_HAND);
  	result := CreateWin32Control (HParent, 'TxlVertSlide');
end;

function TxlVertSlide.IsVertStyle (): boolean;
begin
	result := true;
end;

function TxlHorzSlide.DoCreateControl (HParent: HWND): HWND;
begin
   RegisterControlClass ('TxlHorzSlide', COLOR_BTNFACE, IDC_HAND);
   result := CreateWin32Control (HParent, 'TxlHorzSlide');
end;

function TxlHorzSlide.IsVertStyle (): boolean;
begin
	result := false;
end;

function TxlVertSplitter.DoCreateControl (HParent: HWND): HWND;
begin
  	RegisterControlClass ('TxlVertSplitter', COLOR_BTNFACE, IDC_SIZEWE);
  	result := CreateWin32Control (HParent, 'TxlVertSplitter');
end;

function TxlVertSplitter.IsVertStyle (): boolean;
begin
	result := true;
end;

function TxlHorzSplitter.DoCreateControl (HParent: HWND): HWND;
begin
   RegisterControlClass ('TxlHorzSplitter', COLOR_BTNFACE, IDC_SIZENS);
   result := CreateWin32Control (HParent, 'TxlHorzSplitter');
end;

function TxlHorzSplitter.IsVertStyle (): boolean;
begin
	result := false;
end;

//--------------------------

function TxlStatusBar.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_BAR_CLASSES);
	result := CreateWin32Control (HParent, STATUSCLASSNAME, 0);
end;

procedure TxlStatusBar.SetParts (i_widths: array of integer);
var i: integer;
begin
	for i := Low(i_widths) + 1 to High(i_widths) do
   begin
   	if i_widths[i] = -1 then break;
   	i_widths[i] := i_widths[i] + i_widths[i-1];
   end;
	perform (SB_SETPARTS, length(i_widths), lparam(@i_widths));
end;

procedure TxlStatusBar.SetStatusText (i_part: integer; const s_text: widestring; i_align: TTextAlign = taLeft);
var s: widestring;
begin
	case i_align of
   	taCenter: s := #9 + s_text;
      taRight: s := #9#9 + s_text;
      else s := s_text;
   end;
	Perform (SB_SETTEXTW, i_part, lparam(pwidechar(s)));
end;

//-----------------------------

function TxlToolTipSuper.DoCreateControl (HParent: HWND): HWND;
begin
   InitCommonControl (ICC_BAR_CLASSES);
	result := CreateWindowExW (WS_EX_TOPMOST, TOOLTIPS_CLASS, nil, WS_POPUP or TTS_NOPREFIX or TTS_ALWAYSTIP,
   CW_USEDEFAULT, CW_USEDEFAULT,	CW_USEDEFAULT, CW_USEDEFAULT, HParent, 0, system.MainInstance, nil);
end;

procedure TxlToolTipSuper.BringTipToTop ();
begin
  	SetWindowPos (Fhandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TxlToolTipSuper.SetStyle (const value: TToolTipStyle);
begin
   Perform (TTM_SETDELAYTIME, TTDT_INITIAL, value.InitialTime);
   Perform (TTM_SETDELAYTIME, TTDT_AUTOPOP, value.AutoPopTime);
   FStyle := value;
   SetTipWidth (value.TipWidth);
end;

procedure TxlToolTipSuper.SetTipWidth (value: integer);
begin
   Perform (TTM_SETMAXTIPWIDTH, 0, value);
   FStyle.TipWidth := value;
end;

procedure TxlToolTipSuper.SetColor (i_color: TColor);
begin
	Perform (TTM_SETTIPBKCOLOR, i_color, 0);
end;

function TxlToolTipSuper.GetColor (): TColor;
begin
	result := Perform (TTM_GETTIPBKCOLOR, 0, 0);
end;

//------------------

procedure TxlToolTip.OnCreateControl ();
begin
	FTipList := TxlIntList.Create;
end;

procedure TxlToolTip.OnDestroyControl ();
begin
	FTipList.Free;
end;

procedure TxlToolTip.AddTool (id: integer; h_owner: HWND; const rt: TRect; const s_text: widestring);
var ti: TOOLINFOW;
begin
	if ToolExists (id) then
		ResetTool (id, rt, s_text)
   else
   begin
      with ti do
      begin
         cbSize := sizeof (ti);
         uFlags := TTF_SUBCLASS;
         hwnd := h_owner;
         uId := id;
         hinst := hInstance;
         lpszText := pwidechar(s_text);
         rect := rt;
      end;
      Perform (TTM_ADDTOOLW, 0, LPARAM(@ti));
   	BringTipToTop;
   	FTipList.AddByIndex (id, h_owner);
	end;
end;

procedure TxlToolTip.DeleteTool (id: integer);
begin
   if not ToolExists (id) then exit;
   f_DoDeleteTool (id);
   FTipList.DeleteByIndex (id);
end;

procedure TxlToolTip.f_DoDeleteTool (id: integer);
var ti: ToolInfoW;
begin
	with ti do
   begin
   	cbSize := sizeof (ti);
      hwnd := FTipList.ItemsByIndex [id];
      uId := id;
   end;
   Perform (TTM_DELTOOL, 0, lparam(@ti));
end;

function TxlToolTip.ToolExists (id: integer): boolean;
begin
	result := FTipList.IndexValid (id);
end;

procedure TxlToolTip.ResetTool (id: integer; const rt: TRect; const s_text: widestring);
var ti: TOOLINFOW;
begin
	if not ToolExists (id) then exit;
   with ti do
   begin
      cbSize := sizeof (ti);
      uFlags := TTF_SUBCLASS;
      hwnd := FTipList.ItemsByIndex [id];
      uId := id;
      rect := rt;
      lpszText := pwidechar (s_Text);
   end;
   Perform (TTM_NEWTOOLRECT, 0, lparam(@ti));
   Perform (TTM_UPDATETIPTEXTW, 0, lparam(@ti));
end;

procedure TxlToolTip.Clear ();
var i: integer;
begin
	for i := FTipList.Low to FTipList.High do
   	f_DoDeleteTool (FTipList.Indexes[i]);
   FTipList.Clear;
end;

//-----------------------

procedure TxlTrackingTip.OnCreateControl ();
begin
   inherited;
   with FToolInfo do
   begin
      cbSize := sizeof (FToolInfo);
      uFlags := TTF_TRACK or TTF_ABSOLUTE;
      hwnd := Fhandle;
      uId := 0;
      hinst := hInstance;
      lpszText := nil;
      rect.Left := 0;
      rect.Top := 0;
      rect.Right := 0;
      rect.Bottom := 0;
   end;
   Perform (TTM_ADDTOOLW, 0, LPARAM(@FToolInfo));
   FInitialTimer := TimerCenter.NewTimer;
   FInitialTimer.OnTimer := f_OnInitialtimer;
   FAutoPoptimer := TimerCenter.NewTimer;
   FAutoPopTimer.OnTimer := f_OnAutoPopTimer;
   FHideTimer := TimerCenter.NewTimer;
   FHideTimer.Interval := 300;
   FHideTimer.OnTimer := f_OnHideTimer;
end;

procedure TxlTrackingTip.OnDestroyControl ();
begin
   TimerCenter.ReleaseTimer (FInitialTimer);
   TimerCenter.ReleaseTimer (FAutoPopTimer);
   TimerCenter.ReleaseTimer (FHideTimer);
   inherited;
end;

procedure TxlTrackingTip.SetStyle (const value: TToolTipStyle);
begin
	inherited;
  	FInitialTimer.Interval := value.Initialtime;
  	FAutoPopTimer.Interval := value.AutoPoptime;
end;

procedure TxlTrackingTip.ShowTip (const s_text: widestring; const s_title: widestring = ''; o_icon: TTipIcon = tiNone);
begin
   FTipPos.X := -500;
	ShowTip (FTipPos, s_text, s_title, o_icon);
end;

procedure TxlTrackingTip.ShowTip (const pt: TPoint; const s_text: widestring; const s_title: widestring = ''; o_icon: TTipIcon = tiNone);
begin
	FTipPos := pt;
   HideTip;

	Perform (TTM_SETTITLEW, Ord(o_icon), lparam(pwidechar(s_title)));
	FToolInfo.lpszText := pwidechar (s_Text);
   Perform (TTM_UPDATETIPTEXTW, 0, lparam(@FToolInfo));

   if FInitialTimer.Interval > 0 then
   	FInitialTimer.Start
   else
   	f_OnInitialtimer (self);
end;

procedure TxlTrackingTip.HideTip ();
begin
	FInitialTimer.Stop;
	FAutoPopTimer.Stop;
   FHideTimer.Stop;
   Perform (TTM_TRACKACTIVATE, BoolToInt(false), lparam(@FToolInfo));
end;

procedure TxlTrackingTip.f_OnInitialTimer (Sender: TObject);
var rt: TRect;
	xdif, ydif: integer;
begin
	FInitialTimer.Stop;

   if FTipPos.X <= -500 then
   begin
      GetCursorPos (FTipPos);
      inc (FTipPos.Y, 20);

      GetScreenRect (rt);
      xdif := rt.right - FTipPos.x;
      ydif := rt.bottom - FTipPos.y;
      if xdif < FStyle.TipWidth then
      	dec (FTipPos.x, FStyle.TipWidth - xdif);
      if ydif < 200 then
      	dec (FTipPos.Y, 200 - ydif);
   end;

   Perform (TTM_TRACKPOSITION, 0, MAKELPARAM(FTipPos.x, FTipPos.y));
   Perform (TTM_TRACKACTIVATE, BoolToInt(true), lparam(@FToolInfo));
	if (not Parent.StayOnTop) or ((Parent as TxlWindow).Status = wsMinimize) then
   	BringTipToTop;
   if FAutoPopTimer.Interval > 0 then
   	FAutoPoptimer.Start;
   if FHideWhenCursorMove then
   begin
   	GetCursorPos (FCursorPos);
   	FHideTimer.Start;
   end;
end;

procedure TxlTrackingTip.f_OnAutoPopTimer (Sender: TObject);
begin
   HideTip;
end;

procedure TxlTrackingTip.f_OnHideTimer(Sender: TObject);
var pt: TPoint;
begin
	GetCursorPos (pt);
   if (ABS (pt.x - FCursorPos.x) > 20) or (ABS (pt.Y - FCursorPos.y) > 20) then
      HideTip;
end;

end.

// 请勿删除!
//	function f_CalcPosition (const rt: TRect; const ti: ToolInfoW): TPoint;
//   var scrt, rt2: TRect;
//   	cp: TPoint;
//   	bubblewidth, bubbleheight, dw: integer;
//   begin
//      GetCursorPos (cp);
//      GetScreenRect (scrt);
////      dw := SendMessageW (Fhandle, TTM_GETBUBBLESIZE, 0, lparam(@ti));
//		rt2 := self.Rect;
//		Perform (TTM_ADJUSTRECT, BoolToInt(true), lparam(@rt2));
//      bubblewidth := rt2.Right - rt2.Left;
//      bubbleheight := rt2.Bottom - rt2.Top;  //  HiWord (dw);
//
//      result.x := cp.X;
//      if PointInRect (cp, rt) then
//      begin
//      	result.x := cp.x;
//         result.Y := cp.Y + 20;
//      end
//      else
//      begin
//      	result.X := rt.Left + (rt.Right - rt.Left) div 2;
//         result.Y := rt.Bottom;
//      end;
//
//      if result.x + bubblewidth > scrt.Right then
//      	dec (result.x, bubblewidth);
//      if result.y + bubbleheight > scrt.Bottom then
//      	result.Y := rt.Top - bubbleheight;
//   end;

