unit UxlWinControl;

interface

uses Windows, Messages, UxlClasses, UxlList;

type
	TxlWinControl = class;
	TxlWinContainer = class;
	TMsgHandler = function (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWORD of Object;
   IMessageObserver = interface
   	function ProcessMessage (ASender: TxlWinControl; AMessage, wparam, lparam: DWORD; var b_processed: boolean): DWORD;
   end;

   TxlWinControl = class (TxlInterfacedObject)
   private
      FVisible: boolean;
   	FStayOnTop: boolean;
      FTransparency: TTransparency;
   	FData: integer;
      FWndParent: TxlWinContainer;
      FMessageHandler: TMsgHandler;
      FMsgObservers: TxlInterfaceList;

   	procedure f_StayOnTop (b_top: boolean);
      procedure f_SetTransparency (value: TTransparency);

      function GetVisible (): boolean;
      procedure SetVisible (value: boolean); virtual;
      function GetEnabled (): boolean;
      procedure SetEnabled (value: boolean); virtual;

      procedure SetLeft (value: integer);
      function GetLeft (): integer;
      procedure SetTop (value: integer);
      function GetTop (): integer;
      procedure SetWidth (value: integer);
      function GetWidth (): integer;
      procedure SetHeight (value: integer);
      function GetHeight (): integer;
      procedure SetFont (value: TxlFont);

      procedure SetWndParent (value: TxlWinContainer); virtual;
   protected
      Fhandle: HWND;
      FFont: TxlFont;
      FColor: TColor;
      FHBrush: HBrush;
   	FDefWndProc: pointer;

      procedure SuperOnCreate (); virtual;
      procedure SuperOnDestroy (); virtual;
      function OnWinMessage (AMessage, wParam, lParam: DWORD): DWORD;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; virtual;
		function DefMessageProcess (AMessage, wParam, lParam: DWORD): DWORD; virtual;
      procedure SetWndProc (WndProc: pointer);

      procedure SetPos (const value: TPos); virtual;
      function GetPos (): TPos;
      procedure SetRect (const value: TRect);
      function GetRect (): TRect; virtual;
      procedure SetIcon (id_icon: cardinal);
      function GetText (): widestring; virtual;
      procedure SetText (const s_text: widestring); virtual;

      procedure OnFontChange (Sender: TObject); virtual;
      procedure SetColor (i_color: TColor); virtual;
		function GetColor (): TColor; virtual;

      function IsUserClass(): boolean; virtual;  // default is false, 用于 SetColor
//      function IsChildWindow(): boolean; virtual;  // default is false
   public
      procedure SetWndStyle (i_style: DWORD; b_set: boolean = true);
      procedure SetWndStyleEx (i_style: DWORD; b_set: boolean = true);

      function Perform (msg: DWORD; wParam: dword = 0; lParam: DWORD = 0): DWORD;
      procedure Post (msg: DWord; wParam: dword = 0; lParam: DWord = 0);
      function SetWndLong (nIndex: integer; dwNewLong: DWORD): DWORD;
      function GetWndLong (nIndex: integer): DWORD;
      procedure SetRedraw (value: boolean); virtual;
      procedure BringToTop (); virtual;
      procedure SetFocus ();
      function HasFocus (): boolean;
      procedure Redraw ();
      procedure Show();
      procedure Hide();
      procedure Move (i_x: integer = -1; i_y: integer = -1; i_width: integer = -1; i_height: integer = -1);
      function CursorPos (): TPoint;
      function CursorInWindow (): boolean;
      function TextLength (): cardinal;

      function Handle(): HWND;
      property Parent: TxlWinContainer read FWndParent write SetWndParent;
      property Font: TxlFont read FFont write SetFont;
      property Color: TColor read GetColor write SetColor;
      property Brush: HBrush read FHBrush write FHBrush;
      property Visible: boolean read GetVisible write SetVisible;
      property Enabled: boolean read GetEnabled write SetEnabled;
      property Text: widestring read GetText write SetText;
   	property StayOnTop: boolean read FStayOnTop write f_stayOnTop;
      property Transparency: TTransparency read FTransparency write f_SetTransparency;   // 0..12
   	property Data: integer read FData write FData;  // used to store anything

      property MessageHandler: TMsgHandler read FMessageHandler write FMessageHandler;
      procedure AddMessageObserver (value: IMessageObserver);
      procedure RemoveMessageObserver (value: IMessageObserver);

      property Pos: TPos read GetPos write SetPos;
      property Rect: TRect read GetRect write SetRect;
      function ClientPos (): TPos; virtual;
      function ClientRect (): TRect; virtual;
      property Left: integer read GetLeft write SetLeft;
      property Top: integer read GetTop write SetTop;
      property Width: integer read GetWidth write SetWidth;
      property Height: integer read GetHeight write SetHeight;
      function Right(): integer;
      function Bottom(): integer;
   end;

   TxlControl = class;

   TxlWinContainer = class (TxlWinControl)
   private
		function f_FindControl (h: HWND; var o_ctrl: TxlControl): boolean;
      function Control (i: integer): TxlControl;

      function OnControlCommand (l_handle: HWND; dwCommand: WORD): boolean;
      function OnControlNotify (l_handle: HWND; code: integer; lParam: DWORD; var dwResult: dword): boolean;
	protected
   	FControls: TxlObjList;
   	procedure SuperOnCreate (); override;
      procedure SuperOnDestroy (); override;

      procedure SetVisible (value: boolean); override;
      procedure SetEnabled (value: boolean); override;
      procedure SetColor (i_color: TColor); override;
      procedure OnFontChange (Sender: TObject); override;

      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
   	procedure OnMove (pt: TPoint); virtual;
      procedure OnSize (clpos: TPos); virtual;
      procedure WinSized (); dynamic;
      function ProcessCommand (dwEvent: WORD): DWORD; virtual;
      function ProcessNotify (code: integer; lParam: dword): dword; virtual;
   public
   	procedure AddChild (ctrl: TxlControl; o_align: TAlign = alNoChange);
      procedure RemoveChild (ctrl: TxlControl);
		procedure ClearChildList ();
      procedure Update ();
   end;

   TxlControl = class (TxlWinContainer)
   private
      FNeedDestroyHandle: boolean;
      FAlign: TAlign;
      FParentColor: boolean;
      FParentFont: boolean;
   protected
   	function DoCreateControl (HParent: HWND): HWND; virtual; abstract;
		procedure ReCreate (); virtual;
      procedure OnCreateControl (); virtual;
      procedure OnDestroyControl (); virtual;
		procedure SetWndParent (value: TxlWinContainer); override;
		function GetRect (): TRect; override;
      procedure SetBorder (value: boolean); virtual;
   public
      constructor create (WndParent: TxlWinContainer; h_handle: HWND = 0); virtual;
      destructor destroy (); override;
      property Align: TAlign read FAlign write FAlign default alNone;
      property Border: boolean write SetBorder;
      property ParentColor: boolean read FParentColor write FParentColor;
      property ParentFont: boolean read FParentFont write FParentFont;
   end;

	TMHCenter = class
   private
      FList: TxlIntList;
      FMHArray: array of TMessageHandler;
      constructor Create ();
   public
      class function GetInstance (): TMHCenter;
      Destructor Destroy (); override;
      procedure RegisterMH (hwin: HWND; mh: TMessageHandler);
      procedure UnRegisterMH (hwin: HWND);
      function GetMH (hwin: HWND): TMessageHandler;
   end;

function MakePoints (lp: LParam): TPoint;
procedure InitCommonControl (CC: integer);
procedure DeInitCommonControl ();
function CreateWin32Control (HParent: HWND; const s_controltype: widestring; i_Style: DWORD = 0; i_ExStyle: DWORD = 0): HWND;
procedure RegisterControlClass (const s_classname: widestring; h_backcolor: HBrush; i_Cursor: pchar = nil);
function WindowProc (hWindow: HWnd; AMessage, WParam, LParam: DWORD): DWORD; stdcall;  // 必须在同一单元中，不能多个单元共用

const wColorEvent = 100; sharedbuffersize = 2000;
var PauseEscapeMessage: boolean = false;
var sharedbuffer: array [0..2000] of widechar;

implementation

uses SysUtils, UxlFunctions, commctrl, UxlMath, UxlWindow, UxlWinDef;

procedure TxlWinControl.SuperOnCreate ();
var mh: TMessageHandler;
begin
	FDefWndProc := nil;
   FData := 0;

   FFont := TxlFont.Create;
   FFont.OnChange := OnFontChange;
	Perform (WM_SETFONT, GetStockObject (DEFAULT_GUI_FONT), 0);

   FColor := -1;
   FHBrush := GetClassLong (Fhandle, GCL_HBRBACKGROUND);
   FMsgObservers := TxlInterfaceList.Create;

	mh := OnWinMessage;
   TMHCenter.GetInstance.RegisterMH (Fhandle, mh);
end;

procedure TxlWinControl.SuperOnDestroy ();
begin
   FMsgObservers.free;
	FFont.Free;
   if FHBrush <> 0 then
   	DeleteObject (FHBrush);
   TMHCenter.GetInstance.UnRegisterMH (Fhandle);
end;

function TxlWinControl.IsUserClass(): boolean;
begin
	result := false;
end;

function TxlWinControl.Handle(): HWND;
begin
	result := Fhandle;
end;

//-----------------------

function TxlWinControl.GetColor (): TColor;
var h_dc: HDC;
begin
	if FColor >= 0 then
   	result := FColor
   else
   begin
      h_dc := GetDC (Fhandle);
      result := GetBkColor (h_dc);
      ReleaseDC (Fhandle, h_dc);
   end;
end;

procedure TxlWinControl.SetColor (i_color: TColor);
begin
	FColor := i_color;
   if FHBrush <> 0 then
   	DeleteObject (FHBrush);
   if i_color = -1 then
   	FHBrush := 0
   else
      FHBrush := CreateSolidBrush (FColor);
   if IsUserClass then
   	SetClassLong (Fhandle, GCL_HBRBACKGROUND, FHBrush);
   Redraw ();
end;

procedure TxlWinControl.SetFont (value: TxlFont);
begin
   FFont.Assign (value);
end;

procedure TxlWinControl.OnFontChange (Sender: TObject);
var h_dc: HDC;
begin
   h_dc := GetDC (Fhandle);
   SetTextColor (h_dc, Ffont.color);
	ReleaseDC (Fhandle, h_dc);
   Perform (WM_SETFONT, FFont.handle, 1);
end;

procedure TxlWinControl.SetIcon (id_icon: cardinal);
var h_icon: HICON;
begin
   h_icon := LoadIconFromResource (id_icon);
   Perform (WM_SETICON, ICON_BIG, h_icon);
   Perform (WM_SETICON, ICON_SMALL, h_icon);
end;

//----------------------

procedure TxlWinControl.Move (i_x: integer = -1; i_y: integer = -1; i_width: integer = -1; i_height: integer = -1);
var o_pos: TPos;
begin
   o_pos := Pos;
   if i_x >= 0 then o_pos.x := i_x;
   if i_y >=0 then o_pos.y := i_y;
   if i_width >=0 then o_pos.width := i_width;
   if i_height >= 0 then o_pos.height := i_height;
   SetPos (o_pos);
end;

procedure TxlWinControl.SetPos (const value: TPos);
begin
	MoveWindow (Fhandle, value.x, value.y, value.width, value.height, true);
end;

function TxlWinControl.GetPos (): TPos;
begin
   result := RectToPos (GetRect);
end;

procedure TxlWinControl.SetRect (const value: TRect);
begin
	SetPos (RectToPos(value));
end;

function TxlWinControl.GetRect (): TRect;
begin
	GetWindowRect (Fhandle, result);
end;

procedure TxlWinControl.SetLeft (value: integer);
begin
	Move (value);
end;

function TxlWinControl.GetLeft (): integer;
begin
	result := Rect.Left;
end;

procedure TxlWinControl.SetTop (value: integer);
begin
	Move (-1, value);
end;

function TxlWinControl.GetTop (): integer;
begin
	result := Rect.Top;
end;

procedure TxlWinControl.SetWidth (value: integer);
begin
	Move (-1, -1, value);
end;

function TxlWinControl.GetWidth (): integer;
begin
	result := Pos.width;
end;

procedure TxlWinControl.SetHeight (value: integer);
begin
	Move (-1, -1, -1, value);
end;

function TxlWinControl.GetHeight (): integer;
begin
	result := Pos.height;
end;

function TxlWinControl.Right(): integer;
begin
	result := Rect.Right;
end;

function TxlWinControl.Bottom(): integer;
begin
	result := Rect.Bottom;
end;

function TxlWinControl.ClientRect (): TRect;
begin
   GetClientRect (Fhandle, result);
end;

function TxlWinControl.ClientPos (): TPos;
begin
   result := RectToPos (ClientRect);
end;

//----------------

procedure TxlWinControl.SetVisible (value: boolean);
begin
	if value then
   	ShowWindow (Fhandle, SW_Show)
   else
   	ShowWindow (Fhandle, SW_Hide);
   FVisible := value;
end;

function TxlWinControl.GetVisible (): boolean;
begin
	result := FVisible or IsWindowVisible (Fhandle);
end;

procedure TxlWinControl.Show();
begin
	Visible := true;
end;

procedure TxlWinControl.Hide();
begin
   Visible := false;
end;

function TxlWinControl.GetEnabled (): boolean;
begin
	result := IsWindowEnabled (Fhandle);
end;

procedure TxlWinControl.SetEnabled (value: boolean);
begin
	EnableWindow (Fhandle, value);
end;

function TxlWinControl.GetText (): widestring;
var i_length: integer;
begin
	i_length := TextLength;
	setlength (result, i_length);
   GetWindowTextW (Fhandle, pwidechar(result), i_length + 1);    // WM_GETTEXT
end;

procedure TxlWinControl.SetText (const s_text: widestring);
begin
   SetWindowTextW (Fhandle, pwidechar(s_text));
end;

function TxlWincontrol.TextLength (): cardinal;
begin
   result := GetWindowTextLengthW (Fhandle);
end;

//---------------------

function TxlWinControl.CursorPos (): TPoint;
begin
   GetCursorPos (result);
   ScreenToClient (Fhandle, result);
end;

function TxlWinControl.CursorInWindow (): boolean;
var pt: TPoint;
begin
	pt := CursorPos;
   result := (pt.X >= 0) and (pt.x < Width) and (pt.Y >= 0) and (pt.y < Height);
end;

procedure TxlWinControl.f_StayOnTop (b_top: boolean);
var l_top: HWND;
	o_pos: TPos;
begin
   if b_top then
   	l_top := HWND_TOPMost
   else
   	l_top := HWND_NoTOPMost;
   o_pos := self.Pos;
   SetWindowPos (FHandle, l_top, o_pos.x, o_pos.y, o_pos.width, o_pos.height, 0);
   FStayOnTop := b_top;
end;

procedure TxlWinControl.f_SetTransparency (value: TTransparency);
begin
//	value := ConfineRange (value, 0, 12);
   SetWndStyleEx (WS_EX_LAYERED, (value > 0));
   SetLayeredWindowAttributes (Fhandle, 0, 255 - 20* value, LWA_ALPHA);
   FTransparency := value;
end;

procedure TxlWinControl.AddMessageObserver (value: IMessageObserver);
begin
	FMsgObservers.Add (value);
end;

procedure TxlWinControl.RemoveMessageObserver (value: IMessageObserver);
begin
	FMsgObservers.Remove (value);
end;

function TxlWinControl.OnWinMessage (AMessage, wParam, lParam: DWORD): DWORD;
var i: integer;
	b_processed: boolean;
begin
	if (AMessage = WM_KEYDOWN) and (wparam = VK_ESCAPE) and (not PauseEscapeMessage) then
   	PostMessageW (MainWinHandle, WM_ESCAPE, 0, 0)
   else if (AMessage = WM_KEYUP) and (wparam = VK_ESCAPE) then
      PauseEscapeMessage := false;
      
   i :=  FMsgObservers.Low;
   while i <= FMsgObservers.High do
   begin
		if FMsgObservers[i] <> nil then
      begin
   		result := IMessageObserver(FMsgObservers[i]).ProcessMessage (self, AMessage, wParam, lparam, b_processed);
         if b_processed then exit;
      end;
   	inc (i);
   end;

	if assigned (FMessageHandler) then
   begin
   	result := FMessageHandler (AMessage, wParam, lParam, b_processed);
		if b_processed then exit;
   end;

   result := ProcessMessage (AMessage, wParam, lParam);
end;

function TxlWinControl.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
begin
	if FDefWndProc <> nil then
		result := CallWindowProcW (FDefWndProc, Fhandle, AMessage, WParam, LParam)
   else
      result := DefMessageProcess (AMessage, wParam, lParam);
end;

function TxlWinControl.DefMessageProcess (AMessage, wParam, lParam: DWORD): DWORD;
begin
   result := DefWindowProc (Fhandle, AMessage, WParam, LParam);
end;

//-----------------------

function TxlWinControl.Perform (msg: DWORD; wParam: DWORD = 0; lParam: DWORD = 0): DWORD;
begin
	result := SendMessageW (handle, msg, wParam, lParam);
end;

procedure TxlWinControl.Post (msg: DWord; wParam, lParam: DWord);
begin
	PostMessageW (Fhandle, msg, wParam, lParam);
end;

procedure TxlWinControl.SetWndProc (WndProc: pointer);
begin
   FDefWndProc := pointer (SetWndLong (GWL_WNDPROC, dword(WndProc)));
end;

procedure TxlWinControl.SetWndStyle (i_style: DWORD; b_set: boolean = true);
begin
	if b_set then
      i_style := i_style or DWORD(GetWndLong (GWL_STYLE))
   else
      i_style := DWORD(GetWndLong (GWL_STYLE)) and (not i_style);
	SetWndLong (GWL_STYLE, i_style);
end;

procedure TxlWinControl.SetWndStyleEx (i_style: DWORD; b_set: boolean = true);
begin
	if b_set then
      i_style := i_style or DWORD(GetWndLong (GWL_EXSTYLE))
   else
      i_style := DWORD(GetWndLong (GWL_EXSTYLE)) and (not i_style);
	SetWndLong (GWL_EXSTYLE, i_style);
end;

function TxlWinControl.SetWndLong (nIndex: integer; dwNewLong: DWORD): DWORD;
begin
	result := SetWindowLongW (Fhandle, nIndex, dwNewLong);
end;

function TxlWinControl.GetWndLong (nIndex: integer): DWORD;
begin
	result := GetWindowLongW (Fhandle, nIndex);
end;

procedure TxlWinControl.SetRedraw (value: boolean);
begin
	Perform (WM_SETREDRAW, booltoint(value), 0);
   if value then Redraw;
end;

procedure TxlWinControl.Redraw ();
begin
  	InvalidateRect (Fhandle, nil, true);
   UpdateWindow (Fhandle);
   Perform (WM_NCPAINT, 1, 0);  // 否则语言切换后，菜单栏不更新
end;

procedure TxlWinControl.BringToTop ();
begin
   SetForeGroundWindow (Fhandle);
   SetFocus ();
end;

procedure TxlWinControl.SetFocus ();
begin
	windows.SetFocus (Fhandle);
end;

function TxlWinControl.HasFocus (): boolean;
begin
   result := GetFocus = Fhandle;
end;

procedure TxlWinControl.SetWndParent (value: TxlWinContainer);
begin
   FWndParent := value;
end;

//---------------

procedure TxlWinContainer.SuperOnCreate ();
begin
	inherited;
	FControls := TxlObjList.Create;
end;

procedure TxlWinContainer.SuperOnDestroy ();
begin
	FControls.Free;
   inherited;
end;

function TxlWinContainer.f_FindControl (h: HWND; var o_ctrl: TxlControl): boolean;
var i: integer;
begin
	result := false;
	for i := FControls.Low to FControls.High do
   	if Control(i).handle = h then
      begin
      	o_ctrl := Control(i);
         result := true;
         break;
      end;
end;

function TxlWinContainer.Control (i: integer): TxlControl;
begin
	result := TxlControl(FControls[i]);
end;

function TxlWinContainer.OnControlCommand (l_handle: HWND; dwCommand: WORD): boolean;
var i: integer;
begin
	result := true;
	if l_handle = handle then
   begin
   	ProcessCommand (dwCommand);
      exit;
   end;

   for i := FControls.Low to FControls.High do
      if Control(i).OnControlCommand (l_handle, dwCommand) then exit;

   result := false;
end;

function TxlWinContainer.OnControlNotify (l_handle: HWND; code: integer; lParam: DWORD; var dwResult: dword): boolean;
var i: integer;
begin
	result := true;
	if l_handle = handle then
   begin
   	dwResult := ProcessNotify (code, lParam);
      exit;
   end;

   for i := FControls.Low to FControls.High do
      if Control(i).OnControlNotify (l_handle, code, lParam, dwResult) then exit;

   result := false;
end;

function TxlWinContainer.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
   function OnControlColor (wParam, lParam: DWORD): DWORD;
   var ctrl: TxlControl;
   begin
      if f_FindControl (lParam, ctrl) then
      begin
         SetTextColor (wParam, ctrl.font.Color);
         SetBkColor (wParam, ctrl.color);
         result := ctrl.Brush;
      end
      else
         result := 0;
   end;
   function OnControlScroll (wParam, lParam: DWord): DWord;
   var ctrl: TxlControl;
   begin
      if f_FindControl (lParam, ctrl) then
         result := ctrl.ProcessCommand (wParam)
      else
         result := 0;
   end;
var nmhdr: TNMHDR;
	b_processed: boolean;
begin
	result := 0;
   b_processed := false;
   case AMessage of
   	WM_SIZE:
         if wParam <> Size_Minimized then
      		WinSized ();
      WM_MOVE:
         OnMove (MakePoints(lParam));
     	WM_COMMAND:
     		if OnControlCommand (lParam, HiWord(wParam)) then
         	b_processed := true;
      WM_NOTIFY:     // 执行完后不能 exit!!
      	begin
            nmhdr := PNMHDR(lParam)^;
      		if OnControlNotify (nmhdr.hwndFrom, nmhdr.code, lParam, result) then
            	b_processed := true;
         end;
      WM_CTLCOLORSTATIC, WM_CTLCOLOREDIT, WM_CTLCOLORLISTBOX:
      	begin
	     		result := OnControlColor (wparam, lParam);
            b_processed := true;  // 必须 exit, 因为 return brush!!
         end;
      WM_VSCROLL, WM_HSCROLL:
      	OnControlScroll (wParam, lParam);
   end;
   if not b_processed then
   	result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

function TxlWinContainer.ProcessCommand (dwEvent: word): dword;
begin
	result := 0;
end;

function TxlWinContainer.ProcessNotify (code: integer; lParam: dword): dword;
begin
	result := 0;
end;

procedure TxlWinContainer.WinSized ();
var i: integer;
	ctrl: TxlControl;
   clpos: TPos;
begin         // 切蛋糕过程
   clpos := ClientPos;
//   for i := FControls.Low to FControls.High do
//   	TxlControl(FControls[i]).SetRedraw (false);
   for i := FControls.Low to FControls.High do
	begin
      ctrl := FControls[i];
      if (ctrl.align <> alClient) and (not ctrl.Visible) then continue;
      case ctrl.Align of
      	alLeft:
         	begin
            	ctrl.Move (clpos.x, clpos.y, -1, clpos.height);
               inc (clpos.x, ctrl.width);
               dec (clpos.width, ctrl.width);
            end;
         alTop:
            begin
            	ctrl.Move (clpos.x, clpos.y, clpos.width, -1);
               inc (clpos.y, ctrl.height);
               dec (clpos.height, ctrl.height);
            end;
         alRight:
         	begin
            	ctrl.Move (clpos.width - ctrl.width, clpos.y, -1, clpos.height);
               dec (clpos.width, ctrl.width);
            end;
         alBottom:
         	begin
            	ctrl.Move (clpos.x, clpos.height - ctrl.height, clpos.width, -1);
               dec (clpos.height, ctrl.height);
            end;
         alClient:    // 不调整clleft等参数，允许有一个以上控件均为alclient，占据相同的位置。
         	begin
            	ctrl.Move (clpos.x, clpos.y, clpos.width, clpos.height);
            end;
      end;
      ctrl.Update;
   end;
//   for i := FControls.Low to FControls.High do
//   	TxlControl(FControls[i]).SetRedraw (true);

   OnSize (ClientPos);
end;

procedure TxlWinContainer.OnMove (pt: TPoint);
begin
end;

procedure TxlWinContainer.OnSize (clpos: TPos);
begin
end;

procedure TxlWinContainer.SetVisible (value: boolean);
var i: integer;
begin
	inherited;
	for i := FControls.Low to FControls.High do
   	Control(i).Visible := value;
end;

procedure TxlWinContainer.SetEnabled (value: boolean);
var i: integer;
begin
	inherited;
	for i := FControls.Low to FControls.High do
   	Control(i).Enabled := value;
end;

procedure TxlWinContainer.OnFontChange (Sender: TObject);
var i: integer;
begin
	inherited OnFontChange (Sender);
	for i := FControls.Low to FControls.High do
   	if Control(i).ParentFont then
   		Control(i).Font := self.Font;
end;

procedure TxlWinContainer.SetColor (i_color: TColor);
var i: integer;
begin
	inherited;
	for i := FControls.Low to FControls.High do
   	if Control(i).ParentColor then
   		Control(i).Color := i_color;
end;

//procedure TxlWinContainer.SetRedraw (value: boolean);
//var i: integer;
//begin
//	for i := FControls.Low to FControls.High do
//   	Control(i).SetRedraw(value);
//   inherited SetRedraw (value);
//end;

//--------------

procedure TxlWinContainer.AddChild (ctrl: TxlControl; o_align: TAlign = alNoChange);
begin
   if ctrl.Parent <> nil then
      ctrl.Parent.RemoveChild (ctrl);
   ctrl.FWndParent := self;    //  不能为 ctrl.Parent := self!
   FControls.Add (ctrl);
   if GetParent (ctrl.Handle) <> handle then
   	SetParent (ctrl.handle, handle);
   if o_align <> alNoChange then
   	ctrl.Align := o_align;
   ctrl.BringToTop;
end;

procedure TxlWinContainer.RemoveChild (ctrl: TxlControl);
begin
//	SetParent (ctrl.handle, 0);
	FControls.Remove (ctrl);
end;

procedure TxlWinContainer.ClearChildList ();
//var i: integer;
begin
//	for i := FControls.High downto FControls.Low do
//		RemoveChild (TxlControl(FControls[i]));
   if FControls <> nil then
	   FControls.Clear;
end;

procedure TxlWinContainer.Update ();
begin
	WinSized;
   Redraw;
end;

//--------------

constructor TxlControl.Create (Wndparent: TxlWinContainer; h_handle: HWND = 0);
begin
   FAlign := alNone;
   if h_handle <> 0 then
   	FHandle := h_handle
   else
   	FHandle := DoCreateControl (WndParent.Handle);
   FNeedDestroyHandle := (h_handle = 0);
   SuperOnCreate;
  	WndParent.AddChild (self);
   OnCreateControl;
end;

destructor TxlControl.Destroy ();
begin
	if Parent <> nil then
   	Parent.RemoveChild (self);
	OnDestroyControl;
  	if FNeedDestroyHandle then
   	DestroyWindow (Fhandle);
   SuperOnDestroy;
   inherited;
end;

procedure TxlControl.ReCreate ();
var o_pos: TPos;
	s_text: widestring;
   i: integer;
   o_font: TxlFont;
   o_color: TColor;
   mh: TMessageHandler;
begin
   if not FNeedDestroyHandle then exit;  // 对于对话框中的已有句柄的控件不允许重创建。

	for i := FControls.High downto FControls.Low do
   	SetParent(TxlControl(FControls[i]).Handle, FWndParent.handle);

	o_pos := self.Pos;
   s_text := self.text;
   o_font := TxlFont.Create;
   o_font.assign (self.Font);
   o_color := self.Color;
   TMHCenter.GetInstance.UnRegisterMH (Fhandle);
   DestroyWindow (Fhandle);
   FHandle := DoCreateControl (Parent.handle);
	mh := OnWinMessage;
   TMHCenter.GetInstance.RegisterMH (Fhandle, mh);
   OnCreateControl;
   self.Color := o_color;
   self.Font := o_font;
   o_font.free;
   self.Pos := o_pos;
   self.Text := s_text;

	for i := FControls.High downto FControls.Low do
   	SetParent (TxlControl(FControls[i]).handle, self.handle);
end;

function TxlControl.GetRect (): TRect;
var o_point: TPoint;
	xoffset, yoffset: integer;
begin
	GetWindowRect (Fhandle, result);
   o_point := RectToPoint (result);
   ScreenToClient (FWndParent.handle, o_point);
   xoffset := o_point.x - result.Left;
   yoffset := o_point.y - result.Top;
   with result do
   begin
      Left := o_point.x;
      Top := o_point.y;
      inc (Right, xoffset);
      inc (Bottom, yoffset);
   end;
end;

procedure TxlControl.OnCreateControl ();
begin
end;

procedure TxlControl.OnDestroyControl ();
begin
end;

procedure TxlControl.SetWndParent (value: TxlWinContainer);
begin
	value.AddChild (self);
end;

procedure TxlControl.SetBorder (value: boolean);
begin
	SetWndStyle (WS_BORDER, value);
end;

//-----------------------

var ComCtl32Dll: HMODULE = 0;
  f_InitCommonControlsEx: function (var ICC: TInitCommonControlsEx): Bool stdcall;
//  RefCount: integer = 0;

procedure InitCommonControl (CC: integer);
var icc: TInitCommonControlsEx;
begin
//   if ComCtl32DLL = 0 then
//   begin
      ComCtl32DLL := LoadLibraryW ('comctl32.dll');    // Auto-maintained reference count
      if ComCtl32DLL = 0 then exit;
      @f_InitCommonControlsEx := GetProcAddress (ComCtl32DLL, 'InitCommonControlsEx');
//   end;

	icc.dwsize := sizeof (TInitCommonControlsEx);
   icc.dwICC := CC;
   if not f_InitCommonControlsEx (ICC) then
		raise Exception.create ('Load Common Controls Failed!');
//   inc (RefCount);
end;

procedure DeInitCommonControl ();
begin
//	dec (RefCount);
//   if RefCount = 0 then
   	FreeLibrary (ComCtl32DLL);    // decrement the reference count
end;

function CreateWin32Control (HParent: HWND; const s_controltype: widestring; i_Style: DWORD = 0; i_ExStyle: DWORD = 0): HWND;
begin
   result := CreateWindowExW (i_ExStyle, pwidechar(s_ControlType), nil, WS_CHILD or WS_VISIBLE or WS_TABSTOP or WS_CLIPCHILDREN or i_Style,
      0, 0, 0, 0, HParent, 0, system.MainInstance, nil);
end;

var FClassList: TxlStrList;

procedure RegisterControlClass (const s_classname: widestring; h_backcolor: HBrush; i_Cursor: pchar = nil);
var o_WndClass: WndClassW;
begin
   if FClassList.ItemExists (s_classname) then exit;
   if i_cursor = nil then i_cursor := IDC_Arrow;
   with o_WndClass do
   begin
      Style := CS_DBLCLKS or CS_PARENTDC;
      lpfnWndProc := @WindowProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := system.MainInstance;
      hIcon := 0;
      hCursor := LoadCursor (0, i_cursor);
      hbrBackground := h_backcolor;
      lpszMenuName := nil;
      lpszClassName := pwidechar(s_classname);
   end;
   if RegisterClassW (o_WndClass) = 0 then
      raise Exception.create ('Window Class ' + s_classname + ' Register Failed!');
   FClassList.Add (s_classname);
end;

//---------------------

var MHCenter: TMHCenter;

constructor TMHCenter.Create ();
begin
	FList := TxlIntList.Create;
end;

class function TMHCenter.GetInstance (): TMHCenter;
begin
	result := MHCenter;
end;

Destructor TMHCenter.Destroy ();
begin
	FList.Free;
   inherited;
end;

procedure TMHCenter.RegisterMH (hwin: HWND; mh: TMessageHandler);
var n: integer;
begin
	n := FList.Add (hwin);
   setlength (FMHArray, n + 1);
   FMHArray[n] := mh;
end;

procedure TMHCenter.UnRegisterMH(hwin: HWND);
var n: integer;
begin
   n := FList.Find(hwin);
   if n >= 0 then
   	FMHArray [n] := nil;
end;

function TMHCenter.GetMH (hwin: HWND): TMessageHandler;
var n: integer;
begin
	n := FList.Find (hwin);
	if n >= 0 then
   	result := FMHArray [n]
   else
   	result := nil;
end;

//---------------

function MakePoints (lp: LParam): TPoint;
begin
	result.x := LoWord(lp);
   result.y := HiWord(lp);
end;

function WindowProc(hWindow: HWnd; AMessage, WParam, LParam: DWORD): DWORD; stdcall;
var mh: TMessageHandler;
begin
	mh := TMHCenter.GetInstance.GetMH (hWindow);
   if assigned (mh) then
	   result := mh (AMessage, wParam, lParam)
   else
     	result := DefWindowProc(hWindow, AMessage, WParam, LParam);
end;

//--------------------

initialization
	MHCenter := TMHCenter.Create;
   FClassList := TxlStrList.Create();

finalization
	MHCenter.free;
   FClassList.Free;
   
end.






