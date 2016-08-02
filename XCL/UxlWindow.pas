unit UxlWindow;

interface

uses SysUtils, windows, messages, UxlWinControl, UxlClasses, UxlFunctions, UxlList, UxlStrUtils;

type
   TWindowStruct = record
      Caption: WideString;
      Icon: cardinal;
      x, y, clwidth, clheight: integer;
      MinBox, MaxBox, Sizable: boolean;
   end;

   TxlWindow = class (TxlWinContainer)
   private
      FMinToTray: boolean;
      FCloseMinimize: boolean;
//      FLastWinStatus: TWindowStatus;

      FMainMenu: pointer;
      FMenus: TxlObjList;
      FToolbar: pointer;
      FAccelTable: pointer;
      FTrayIcon: pointer;
      FToolWindowStyle: boolean;

      procedure f_SetWindowStatus (value: TWindowStatus);
      function f_GetWindowStatus (): TWindowStatus;
      procedure f_ToolWindowStyle (value: boolean);
      procedure SetTitleBar (value: boolean);
   protected
		function IsUserClass(): boolean; override;
      procedure OnCreate (); virtual;
      procedure OnCloseQuery (var CanClose: boolean); virtual;
      procedure OnClose (); virtual;
      procedure OnActive (); virtual;
      procedure OnStatusChanged (); virtual;
      function OnSysCommand (wParam, lParam: DWORD): DWORD; virtual;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
   public
      constructor Create (const WndStrc: TWindowStruct);
      procedure Run();
      procedure Close();
      procedure BringToTop (); override;

      procedure SetMainMenu (menu: pointer);
      procedure SetToolbar (toolbar: pointer);
      procedure SetAccelTable (accel: pointer);
      procedure SetTrayIcon (trayicon: pointer);

      procedure RegisterMenu (value: pointer);
      procedure UnRegisterMenu (value: pointer);

      property MinimizeToTray: boolean read FMinToTray write FMinToTray;
      property CloseMinimize: boolean read FCloseMinimize write FCloseMinimize;
      property Status: TWindowStatus read f_GetwindowStatus write f_SetWindowStatus;
      property ToolWindowStyle: boolean read FToolWindowStyle write f_ToolWindowStyle;
      property TitleBar: boolean write SetTitleBar;

      property MainMenu: pointer read FMainMenu write SetMainMenu;
   end;

function MainWinHandle (): HWND;
function MainWindow (): TxlWindow;
function ProgClass (): widestring;
procedure ProcessMessages ();
function CheckSingleton (b_activate: boolean = true): boolean;

implementation

uses UxlMenu, UxlWinClasses, UxlWinDef, UxlMath;

var FMainWinHandle: HWND;
    WM_TaskBarRestart: DWORD;
    FAccel: HACCEL;
    FMainWindow: TxlWindow;

function MainWinHandle (): HWND;
begin
  	result := FMainWinHandle;
end;

function MainWindow (): TxlWindow;
begin
	result := FMainWindow;
end;

function ProgClass (): widestring;
begin
	result := paramstr(0);
end;

constructor TxlWindow.Create (const WndStrc: TWindowStruct);
var WindowClass: WndClassA;
   ExStyle, WndStyle: DWord;
   i_width, i_height: integer;
begin
	with WindowClass do
   begin
      Style := cs_hRedraw or cs_vRedraw or CS_DBLCLKS;
      lpfnWndProc := @WindowProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := system.MainInstance;
      if WndStrc.Icon = 0 then
      	hIcon := LoadIcon (0, idi_Application)
      else
      	hIcon := LoadIcon (hInstance, MakeIntResource(WndStrc.Icon));
      hCursor := LoadCursor (0, IDC_Arrow);
      hbrBackground := HBRUSH ((COLOR_BTNFACE + 1));
      lpszMenuName := '';
      lpszClassName := PAnsiChar(UnicodeToAnsi(ProgClass));  //pansichar(UnicodeToAnsi(ProgClass));
   end;

   if RegisterClassA (WindowClass) = 0 then raise Exception.create ('Window Class Register Failed!');

   WndStyle := WS_CLIPCHILDREN or WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU;
   if WndStrc.Sizable then WndStyle := WndStyle or WS_THICKFRAME;
   if WndStrc.MaxBox then WndStyle := WndStyle or WS_MAXIMIZEBOX;
   if WndStrc.MinBox then WndStyle := WndStyle or WS_MINIMIZEBOX;
   ExStyle := WS_EX_CONTROLPARENT;

   i_width := 0;
   if WndStrc.clwidth > 0 then
   begin
      i_width := WndStrc.clwidth + 2 * GetSystemMetrics(SM_CXEDGE);
      if WndStrc.Sizable then inc (i_width, 2 * GetSystemMetrics(SM_CXSIZEFRAME));
   end;
   i_height := 0;
   if WndStrc.clheight > 0 then
   begin
      i_height := WndStrc.clheight + GetsystemMetrics (SM_CYCAPTION) + 2 * GetSystemMetrics(SM_CYEDGE);
      if WndStrc.Sizable then inc (i_height, 2 * GetSystemMetrics(SM_CXSIZEFRAME));
   end;
   
   Fhandle := CreateWindowExW (ExStyle, pwidechar(ProgClass), pwidechar(WndStrc.Caption), WndStyle, WndStrc.x, WndStrc.y,
         i_width, i_height, 0, 0, system.MainInstance, nil);
   FMainWinHandle := Fhandle;
   FMainWindow := self;

   WM_TaskBarRestart := RegisterWindowMessageW('TaskbarCreated');

   FMainMenu := nil;
   FAccel := 0;
   FToolbar := nil;
   FTrayIcon := nil;
   FMenus := TxlObjList.Create();
   FToolWindowStyle := false;
//   FHotkeyCenter := TxlHotkeyCenter.Create (self.handle);

   SuperOnCreate;
   OnCreate;
end;

procedure TxlWindow.Run ();
var AMessage: Msg;
begin
   while GetMessage(AMessage, 0, 0, 0) do
      try
         if (FAccel = 0) or (TranslateAccelerator (Fhandle, FAccel, AMessage) = 0) then
         begin
            TranslateMessage(AMessage);
            DispatchMessage(AMessage);
         end;
      except
      	on E: Exception do
         	continue;
      end;
end;

procedure ProcessMessages ();
var AMessage: Msg;
begin
  	while PeekMessage (AMessage, 0, 0, 0, PM_REMOVE) do
      try
         if (FAccel = 0) or (TranslateAccelerator (MainWinhandle, FAccel, AMessage) = 0) then
         begin
            TranslateMessage(AMessage);
            DispatchMessage(AMessage);
         end;
      except
      end;
end;

procedure TxlWindow.Close ();
var canClose: boolean;
begin
	OnCloseQuery (CanClose);
	if CanClose then
   begin
   	OnClose;
      Perform (WM_CLOSE, 0, 0);
      FMenus.Free;
      if assigned (FTrayIcon) then
         TxlTrayIcon(FTrayIcon).DrawIcon (false);
      SuperOnDestroy;
//      TMHCenter.GetInstance.UnRegisterMH(Fhandle);
      FreeAndNil (self);
      halt;
   end;
end;

procedure TxlWindow.BringToTop ();
var hFGWinHandle: HWND;
	tid, cid: dword;
begin
   hFGWinHandle := GetForegroundWindow ();
   if hFGWinHandle <> Fhandle then
   begin
      tid := getwindowthreadprocessid (hFGWinHandle, nil);
      cid := getwindowthreadprocessid (Fhandle, nil);
      attachthreadinput (cid, tid, true);
      SetForeGroundWindow (FHandle);
      attachthreadinput (cid, tid, false);
   end;
end;

function TxlWindow.IsUserClass(): boolean;
begin
	result := true;
end;

procedure TxlWindow.OnCreate ();
begin
	Show;
   ClearMemory;
end;

procedure TxlWindow.OnCloseQuery (var CanClose: boolean);
begin
	CanClose := true;
end;

procedure TxlWindow.OnClose ();
begin
end;

procedure TxlWindow.OnActive ();
begin
end;

procedure TxlWindow.OnStatusChanged ();
begin
end;

//-------------------

procedure TxlWindow.f_ToolWindowStyle (value: boolean);
var b_visible: boolean;
begin
   if value = FToolWindowStyle then exit;

   b_visible := self.Visible;
	if b_visible then
   	ShowWindow (Fhandle, SW_Hide);   // 不能用 Hide!

   SetWndStyleEx (WS_EX_TOOLWINDOW, value);
   SetWndStyleEx (WS_EX_APPWINDOW, not value);

   self.Height := self.Height - 1;  // 非如此，不足以刷新显示
   self.Height := self.Height + 1;
   if b_visible then
      ShowWindow (Fhandle, SW_SHOW);

   FToolWindowStyle := value;
end;

procedure TxlWindow.f_SetWindowStatus (value: TWindowStatus);
var wparam: dword;
begin
   case value of
      wsMinimize: wParam := SC_MINIMIZE;
      wsMaximize: wParam := SC_MAXIMIZE;
      else wParam := SC_RESTORE
   end;
   Perform (WM_SYSCOMMAND, wParam, 0);
end;

function TxlWindow.f_GetWindowStatus (): TWindowStatus;
var wp: TWindowPlacement;
begin
   wp.length := sizeof (wp);
   GetWindowPlacement (Fhandle, @wp);
   if wp.showCmd in [SW_HIDE, SW_SHOWMINIMIZED, SW_MINIMIZE] then
   	result := wsMinimize
   else if wp.showCmd in [SW_SHOWMAXIMIZED, SW_MAXIMIZE] then
   	result := wsMaximize
   else
   	result := wsNormal;
end;

procedure TxlWindow.SetTitleBar (value: boolean);
begin
	SetWndStyle (WS_CAPTION, value);
end;

//-------------------

procedure TxlWindow.RegisterMenu (value: pointer);
begin
   FMenus.Add (value);
end;

procedure TxlWindow.UnRegisterMenu (value: pointer);
begin
   FMenus.Remove (value);
end;

procedure TxlWindow.SetMainMenu (menu: pointer);
begin
	FMainMenu := menu;
   if assigned (FMainMenu) then
   	windows.SetMenu (Fhandle, TxlMenu(FMainMenu).handle)
   else
      windows.SetMenu (Fhandle, 0);
end;

procedure TxlWindow.SetToolbar (toolbar: pointer);
begin
	FToolbar := toolbar;
   if toolbar <> nil then
   	TxlToolbar(FToolbar).Align := alTop;
end;

procedure TxlWindow.SetAccelTable (accel: pointer);
begin
	FAccelTable := accel;
   if accel <> nil then
   	FAccel := TxlAccelTable(accel).Handle
   else
   	FAccel := 0;
end;

procedure TxlWindow.SetTrayIcon (trayicon: pointer);
begin
	if trayicon = FTrayIcon then exit;
	if assigned(FTrayIcon) then
   	TxlTrayIcon (FTrayIcon).DrawIcon(false);
	FTrayIcon := trayicon;
	if assigned(FTrayIcon) then
   	TxlTrayIcon (FTrayIcon).DrawIcon(true);
end;

// ------------- Message Handlers --------------

function TxlWindow.OnSysCommand(wParam, lParam: DWORD): DWORD;
var b_mintotray: boolean;
begin
	result := 0;
   b_mintotray := FMintoTray;
   if (wParam = SC_CLOSE) and CloseMinimize then
   begin
      wParam := SC_MINIMIZE;
      b_mintotray := true;
   end;

   if wParam = SC_CLOSE then
      close
   else
   begin
      result := DefWindowProc(Fhandle, WM_SYSCOMMAND, WParam, LParam);
      if (wParam = SC_MINIMIZE) and b_mintotray then
         ShowWindow (Fhandle, SW_HIDE);
      if (wparam = SC_MAXIMIZE) or (wparam = SC_RESTORE) then
         SetForeGroundWindow (Fhandle);
      if (wparam = SC_MAXIMIZE) or (wParam = SC_MINIMIZE) or (wParam = SC_RESTORE) then
         OnStatusChanged ();
   end;
end;

// Window Procedure
function TxlWindow.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var i: integer;
	i_lo, i_hi: word;
   b_processed: boolean;
begin
  	result := 0;
   b_processed := true;
  	case AMessage of
   	WM_COMMAND:
      	begin
            i_lo := LoWord(wParam);
            i_hi := HiWord(wParam);

            if (lParam = 0) then
            begin
               if (i_hi = 0) and assigned (FMainMenu) then  // from menu
                  TxlMenu(FMainMenu).ProcessCommand (i_lo)
               else if (i_hi = 1) and assigned(FAcceltable) then // from accelerator
                  TxlAccelTable(FAcceltable).ProcessCommand (i_lo);
            end
            else if assigned (FToolbar) and (lParam = TxlToolbar(FToolbar).handle) then  // from toolbar
               TxlToolbar(FToolbar).ProcessCommand(i_lo)
            else  // from control
               b_processed := false;
         end;
      WM_ACTIVATE:
      	if wParam in [WA_ACTIVE, WA_CLICKACTIVE] then
         	OnActive ();
      WM_CALLWINDOWDEMAND:
         begin
            ShowWindow (Fhandle, SW_SHOWNORMAL);
            SetForeGroundWindow (Fhandle);
         end;
      WM_HOTKEY:
			HotkeyCenter.OnHotkey (wparam);
      WM_TIMER:
         TimerCenter.OnTimer (wparam);
      WM_TRAYICONMESSAGE:
         if assigned (FTrayIcon) then
            TxlTrayIcon (FTrayIcon).ProcessCommand (lParam);
      WM_SYSCOMMAND:
      	result := OnSysCommand (wparam, lParam);
      WM_INITMENUPOPUP:
         for i := FMenus.Low to FMenus.High do
         	if FMenus[i] <> nil then
            	TxlMenu(FMenus[i]).InitMenuPopup (wParam);
      WM_Destroy :
         PostQuitMessage(0);
      else
      	if AMessage = WM_TaskBarRestart then    // else 需要 constant value，不能用 var
         begin
            if assigned (FTrayIcon) then
               TxlTrayIcon(FTrayIcon).DrawIcon (true);
         end
         else
         	b_processed := false;
   end;
	if not b_processed then
      result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

//--------------------------

var Findhwnd: HWND;

function EnumWndProc (hwnd:HWND; lparam: dword): boolean; stdcall;
var s_buffer: widestring;
   WinInstance: DWord;
begin
   result := true;
   SetLength (s_buffer, 2000);
   if GetClassNameW (hwnd, pwidechar(s_buffer), 2000) = 0 then exit;   //获得当前遍历窗口的类名
   if not IsSameStr (pwidechar(s_buffer), ProgClass) then exit;

   WinInstance := GetWindowLong (hwnd, GWL_HINSTANCE);  //获得当前遍历窗口的实例
   if GetModuleFileNameW (WinInstance, pwidechar(s_buffer), 2000) = 0 then exit;
   if not IsSameStr (pwidechar(s_buffer), ProgExe) then exit;

   FindHwnd := hwnd;	//使用全局变量 FindHwnd 保存找到的句炳
   result := false;	//找到以后就结束遍历
end;

function CheckSingleton (b_activate: boolean = true): boolean;
var mymutex: DWord;
begin
   mymutex := createmutexW (nil, false, pwidechar(ReplaceStr (progClass, '\', '/')));
  	result := waitforsingleobject (mymutex, 0) <> wait_timeout;
	if (not result) and b_activate then    //同一exe只允许运行一个实例。第二次运行时restore以前的实例。
  	begin
      FindHwnd := 0;
     	EnumWindows (@EnumWndProc, 0);    //回调函数，在return false或所有窗口遍历完成后返回。
      if FindHwnd <> 0 then
      begin
//			ShowWindow (Findhwnd, SW_SHOWNORMAL);
         SendMessageW (Findhwnd, WM_CALLWINDOWDEMAND, 0, 0);
//         SetForeGroundWindow (Findhwnd);
      end;
  	end;
end;

end.


//type
//   TLabel = record
//      Pos: TPoint;
//      Text: widestring;
//      Font: TxlFont;
//      Visible: boolean;
//   end;
//
//   TxlLabeledWindow = class (TxlWindow)
//   private
//      FLabels: array of TLabel;
//
//      function f_GetLabel (id: integer): TLabel;
//      procedure f_SetLabelText (id: integer; const value: widestring);
//      function f_GetLabelText (id: integer): widestring;
//      procedure f_SetLabelVisible (id: integer; value: boolean);
//      function f_GetLabelVisible (id: integer): boolean;
//   protected
//      function OnWinMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
//   public
//      function AddLabel (i_x, i_y: integer; const s_text: widestring = ''): integer; overload;
//      function AddLabel (const o_label: TLabel): integer; overload;
//      procedure SetLabel (id: integer; const value: TLabel);
//      procedure RemoveLabel (id: integer);
//
//      property Labels[id: integer]: TLabel read f_GetLabel write SetLabel;
//      property LabelText[id: integer]: widestring read f_GetLabelText write f_SetLabelText;
//      property LabelVisible[id: integer]: boolean read f_GetLabelVisible write f_SetLabelVisible;
//   end;

//function TxlLabeledWindow.AddLabel (i_x, i_y: integer; const s_text: widestring = ''): integer;
//var o_label: TLabel;
//begin
//   with o_label do
//   begin
//   	Pos.x := i_x;
//      Pos.y := i_y;
//      text := s_text;
//      font.name := '';
//      visible := true;
//   end;
//   result := AddLabel (o_label);
//end;
//
//function TxlLabeledWindow.AddLabel (const o_label: TLabel): integer;
//begin
//	result := length (FLabels);
//	setlength (Flabels, result + 1);
//   FLabels[result] := o_label;
//end;
//
//procedure TxlLabeledWindow.RemoveLabel (id: integer);
//var i, n: integer;
//begin
//	n := Length(FLabels);
//	if not InRange (id, 0, n - 1) then exit;
//	for i := id to n - 2 do
//   	FLabels[i] := FLabels[i + 1];
//   SetLength (FLabels, n - 1);
//end;
//
//procedure TxlLabeledWindow.SetLabel (id: integer; const value: TLabel);
//begin
//	FLabels[id] := value;
//end;
//
//function TxlLabeledWindow.f_GetLabel (id: integer): TLabel;
//begin
//	result := FLabels[id];
//end;
//
//procedure TxlLabeledWindow.f_SetLabelText (id: integer; const value: widestring);
//begin
//	FLabels[id].text := value;
//end;
//
//function TxlLabeledWindow.f_GetLabelText (id: integer): widestring;
//begin
//	result := FLabels[id].text;
//end;
//
//procedure TxlLabeledWindow.f_SetLabelVisible (id: integer; value: boolean);
//begin
//   FLabels[id].visible := value;
//end;
//
//function TxlLabeledWindow.f_GetLabelVisible (id: integer): boolean;
//begin
//	result := FLabels[id].visible;
//end;
//
//function TxlLabeledWindow.OnWinMessage (AMessage, wParam, lParam: DWORD): DWORD;
//   procedure f_drawLabel (h_DC: HDC; o_label: TLabel);
//   var h_OldFont: HFONT;
//   begin
//      if not o_label.visible then exit;
//      if o_label.Font.Name = '' then
//         h_OldFont := SelectObject (h_DC, GetStockObject (DEFAULT_GUI_FONT))
//      else
//         h_OldFont := SelectObject (h_DC, CreateFontHandle(o_label.Font));
//      with o_label do
//         TextOutW (h_DC, Pos.x, Pos.y, pwidechar(text), length(text));
//      h_oldfont := SelectObject (h_DC, h_OldFont);
//      DeleteObject (h_oldfont);
//   end;
//var i: integer;
//   h_DC: HDC;
//   ps: PaintStruct;
//begin
//   if (AMessage = WM_PAINT) then
//   begin
//      h_DC := BeginPaint (Fhandle, ps);
//      SetBkMode (h_DC, TRANSPARENT);
//      for i := 0 to length(FLabels) - 1 do
//         f_drawLabel (h_DC, FLabels[i]);
//      EndPaint (Fhandle, ps);
//   end;
//   result := inherited OnWinMessage (AMessage, wParam, lParam);
//end;

//   IMessagePoster = interface
//   	procedure ReceiveMessage (AMessage, wParam, lParam: DWORD);
//   end;
//
//   TxlMessagePoster = class
//   private
//   	FOwner: TxlWindow;
//   	FMsgs: TxlInterfaceList;
//   public
//      constructor Create (AOwner: TxlWindow);
//      destructor Destroy (); override;
//      procedure Post (AOwner: IMessagePoster;
//   end;

//------------------------


