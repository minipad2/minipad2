unit UxlWinClasses;

interface

uses Windows, Messages, ShellAPI, UxlClasses, UxlWindow, UxlWinControl, UxlMenu, UxlStrUtils, UxlMath, UxlFunctions, UxlList;

type
   TxlTrayIcon = class
   private
      FWndParent: TxlWindow;
      FPopMenu: TxlMenu;
      FIcon: cardinal;
      FTip: widestring;
      FOnClick: TNotifyEvent;
   public
      constructor Create (WndParent: TxlWindow; i_icon: cardinal; const s_tip: widestring);
      procedure ProcessCommand (dwEvent: DWORD);
      procedure DrawIcon (b_draw: boolean);

      property Menu: TxlMenu read FPopMenu write FPopMenu;
      property OnClick: TNotifyEvent read FOnClick write FOnClick;
   end;

type
   TxlTimer = class
   private
      FInterval: cardinal;
      FHParent: HWND;
      FOnTimer: TNotifyEvent;
      FStarted: boolean;
      FID: cardinal;

      procedure f_SetEnable (value: boolean);
      constructor Create (HParent: HWND; id: integer);
      procedure SetInterval (i_interval: cardinal);
   public
      destructor Destroy (); override;
      procedure Start ();
      procedure Stop ();
      procedure Reset ();
      procedure TimerEvent ();

      property Interval: cardinal read FInterval write SetInterval;
      property Enabled: boolean read FStarted write f_SetEnable;
      property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
   end;

   TxlTimerCenter = class
   private
      FTimers: TxlObjList;
      FHParent: HWND;
      constructor Create (HParent: HWND);
   public
      destructor Destroy (); override;
   	function NewTimer (): TxlTimer;
      procedure ReleaseTimer (value: TxlTimer);
      procedure OnTimer (id: integer);
   end;

type
   IHotkeyOwner = interface
   	procedure OnHotkey (id: integer; hk: THotkey);
   end;

   THotkeyRecord = record
   	Id: integer;
      Owner: IHotkeyOwner;
      Hotkey: THotkey;
   end;

   TxlHotKeyCenter = class
   private
      FHotkeys: array of THotkeyRecord;
      FHParent: HWND;
      constructor Create (HParent: HWND);
   public
      procedure AddHotkey (owner: IHotkeyOwner; id: integer; hk: THotKey);
      procedure RemoveHotkey (owner: IHotkeyOwner; id: integer);
      procedure RemoveOwner (value: IHotkeyOwner);

      procedure OnHotkey (hkid: integer);
   end;

type
   TxlClipboard = class
   private
      FHOwner: HWND;
      function f_gettext (): widestring;
      procedure f_settext (const value: widestring);
   public
      constructor Create (HOwner: HWND);
      function GetTextSize (): integer;
      property Text: widestring read f_gettext write f_settext;
   end;

function Clipboard(): TxlClipboard;
function HotkeyCenter (): TxlHotkeyCenter;
function TimerCenter (): TxlTimerCenter;

implementation

uses UxlCommDlgs, UxlWinDef, commctrl;

var
	FClipbrd: TxlClipboard;
   FHotkeyCenter: TxlHotkeyCenter;
   FTimerCenter: TxlTimerCenter;

function Clipboard(): TxlClipboard;
begin
	if FClipbrd = nil then
   	FClipbrd := TxlClipboard.Create (MainWinHandle);
   result := FClipbrd;
end;

function HotkeyCenter(): TxlHotkeyCenter;
begin
	if FHotkeyCenter = nil then
   	FHotkeyCenter := TxlHotkeyCenter.Create (MainWinHandle);
   result := FHotkeyCenter;
end;

function TimerCenter(): TxlTimerCenter;
begin
	if FTimerCenter = nil then
   	FTimerCenter := TxlTimerCenter.Create (MainWinHandle);
   result := FTimerCenter;
end;

//------------------

constructor TxlTrayIcon.Create (WndParent: TxlWindow; i_icon: cardinal; const s_tip: widestring);
begin
   FWndParent := WndParent;
   FIcon := i_icon;
   FTip := s_tip;
end;

procedure TxlTrayIcon.DrawIcon(b_draw: boolean);
var o_nid: TNotifyIconDataW;
begin
  o_nid.cbSize := sizeof (o_nid);
   with o_nid do
   begin
 //     cbSize := sizeof (o_nid);
      Wnd := FWndParent.handle;
      uID := cardinal(self);
   end;
   if b_draw then
   begin
   	with o_nid do
      begin
         uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
         hIcon := LoadIconFromResource (Ficon);
         uCallBackMessage := WM_TRAYICONMESSAGE;
         StrLCopy (szTip, pwidechar(Ftip), Min (length(Ftip), length(szTip)));
      end;
      Shell_NotifyIconW (NIM_ADD, @o_nid);
   end
   else
      Shell_NotifyIcon (NIM_DELETE, @o_nid);
end;

procedure TxlTrayIcon.ProcessCommand (dwEvent: DWORD);
var h: HWND;
begin
	case dwEvent of
      WM_LBUTTONDOWN:
         if assigned (FOnClick) then FOnClick (self);
      WM_RBUTTONDOWN:
      	begin
            if Assigned(FPopMenu) then
            begin
               h := FWndParent.handle;
            	SetForegroundWindow(h);
               FPopMenu.Popup(0, -1, -1, pdRightBottom, true);
               SendMessageW(h, WM_TRAYICONAFTERPOPUPMENU, 0, 0);
               PostMessageW(h, WM_NULL, 0, 0);
            end;
        	end;
      WM_MOUSEMOVE:
      	SendMessageW (FWndParent.Handle, WM_TRAYICONMOUSEMOVE, 0, 0);
   end;
end;

//----------------------

constructor TxlClipboard.Create (HOwner: HWND);
begin
	FHOwner := HOwner;
end;

function TxlClipboard.f_gettext (): widestring;
var hg: HGLOBAL;
	p: pointer;
   pt: pwidechar;
   n: integer;
begin
   result := '';
   if IsClipboardFormatAvailable (CF_UNICODETEXT) then
   begin
   	OpenClipboard (FHOwner);
      hg := GetClipboardData (CF_UNICODETEXT);
      n := globalsize (hg);
      p := GlobalLock (hg);
      getmem (pt, n);
      copymemory (pt, p, n);
      GlobalUnlock (hg);
      closeclipboard ();
      result := pt;
      freemem (pt);
   end;
end;

procedure TxlClipboard.f_settext (const value: widestring);
var hg: HGLOBAL;
	p: pointer;
   n: integer;
begin
	n := length(value);
   OpenClipboard (FHOwner);
   EmptyClipboard ();

	hg := GlobalAlloc (GHND or GMEM_SHARE, (n + 1)*2);
   p := GlobalLock (hg);
   copymemory (p, pwidechar(value), n * 2);
   GlobalUnlock (hg);

   SetClipboardData (CF_UNICODETEXT, hg);
   CloseClipboard ();
end;

function TxlClipboard.GetTextSize (): integer;
var hg: HGlobal;
begin
	result := 0;
   if IsClipboardFormatAvailable (CF_UNICODETEXT) then
   begin
   	OpenClipboard (FHOwner);
      hg := GetClipboardData (CF_UNICODETEXT);
      result := globalsize (hg) div 2;
      CloseClipboard ();
   end;
end;

//------------------------

constructor TxlTimer.Create (HParent: HWND; id: integer);
begin
   FHParent := HParent;
   FID := id;
   FStarted := false;
end;

destructor TxlTimer.Destroy ();
begin
   Stop;
   inherited;
end;

procedure TxlTimer.Start ();
begin
	Stop;
	if FInterval > 0 then
   begin
      SetTimer (FHParent, FID, FInterval, nil);
      FStarted := true;
   end;
end;

procedure TxlTimer.Stop ();
begin
   if FStarted then
      KillTimer (FHparent, FID);
   FStarted := false;
end;

procedure TxlTimer.Reset ();
begin
	Stop;
   Start;
end;

procedure TxlTimer.f_SetEnable (value: boolean);
begin
	if value then
   	Start
   else
   	Stop;
end;

procedure TxlTimer.SetInterval(i_interval: cardinal);
begin
	if i_interval <> FInterval then
   begin
   	FInterval := i_interval;
      if FStarted then
         Start;
   end;
end;

procedure TxlTimer.TimerEvent ();
begin
   if assigned (FOnTimer) then
      FOnTimer (self);
end;

//-------------------------

constructor TxlTimerCenter.Create (HParent: HWND);
begin
   FHParent := HParent;
   FTimers := TxlObjList.Create;
end;

destructor TxlTimerCenter.Destroy ();
begin
	FTimers.Free;
   inherited;
end;

function TxlTimerCenter.NewTimer (): TxlTimer;
var id: integer;
begin
	id := FTimers.GetNewIndex;
   result := TxlTimer.Create (FHParent, id);
   FTimers.AddByIndex (id, result);
end;

procedure TxlTimerCenter.ReleaseTimer (value: TxlTimer);
begin
	FTimers.Remove (value);
   value.free;
end;

procedure TxlTimerCenter.OnTimer (id: integer);
var o_timer: TxlTimer;
begin
   o_timer := FTimers.ItemsByINdex [id];
   if o_timer <> nil then
   	o_timer.TimerEvent ();
end;

//-------------------------

const cHotkeyOffset = 1000;

constructor TxlHotkeyCenter.Create (HParent: HWND);
begin
   FHParent := HParent;
end;

procedure TxlHotkeyCenter.AddHotkey (owner: IHotkeyOwner; id: integer; hk: THotKey);
var i, n: integer;
   modifier, vk: cardinal;
begin
	RemoveHotkey (owner, id);
	if hk <> 0 then
   begin
   	n := -1;
      for i := Low(FHotkeys) to High(FHotkeys) do
      	if FHotkeys[i].Owner = nil then
         begin
         	n := i;
            break;
         end;
      if n < 0 then
      begin
      	n := Length (FHotkeys);
         SetLength (FHotkeys, n + 1);
      end;

      SplitHotkey (hk, modifier, vk);
   	RegisterHotkey (FHParent, n + cHotkeyOffset, modifier, vk);
      FHotkeys[n].Owner := owner;
      FHotkeys[n].Id := id;
      FHotkeys[n].Hotkey := hk;
   end;
end;

procedure TxlHotkeyCenter.RemoveHotkey (owner: IHotkeyOwner; id: integer);
var i: integer;
begin
	for i := Low(FHotkeys) to High(FHotkeys) do
   	if (FHotkeys[i].id = id) and (FHotkeys[i].Owner = owner) then
      begin
			UnRegisterHotkey (FHParent, i + cHotkeyOffset);
   		FHotkeys[i].Owner := nil;
         exit;
      end;
end;

procedure TxlHotkeyCenter.RemoveOwner (value: IHotkeyOwner);
var i: integer;
begin
	for i := Low(FHotkeys) to High(FHotkeys) do
   	if (FHotkeys[i].Owner = value) then
      begin
			UnRegisterHotkey (FHParent, i + cHotkeyOffset);
   		FHotkeys[i].Owner := nil;
      end;
end;

procedure TxlHotkeyCenter.OnHotkey (hkid: integer);
var n: integer;
begin
	n := hkid - cHotkeyOffset;
   if InRange (n, 0, Length(FHotkeys) - 1) and (FHotkeys[n].Owner <> nil) then
   	FHotkeys[n].Owner.OnHotKey (FHotkeys[n].Id, FHotkeys[n].hotkey);
end;

//------------------------

initialization

finalization
 	FreeAndNil (FClipbrd);
   FreeAndNil (FHotkeyCenter);
   FreeAndNil (FTimerCenter);

end.

