unit UPasteHandler;

interface

uses Windows, UxlClasses, UxlExtClasses, UxlMenu, UxlWindow, UxlWinControl, UxlList;

type
   TExtPopupMenu = class (TxlMenu)
   private
      FhPopup: HMenu;
   protected
      procedure OnCreate (); override;
   public
      procedure PopupMenu (pid: integer; x, y: integer);
   end;

   TPopupHandler = class (TxlInterfacedObject, IOptionObserver)
   private
      FParent: TxlWindow;
      FMenu: TExtPopupMenu;
      FFocusPopupMenu: boolean;
	public
      constructor Create (AOwner: TxlWindow);
      destructor Destroy (); override;
      procedure OptionChanged ();
      procedure PopupMenu (id_menu: cardinal; b_caretpos: boolean);
   end;

	TPasteHandler = class (TxlInterfacedObject, IMessageObserver)
	private
      FParent: TxlWindow;
      FPrepared: boolean;
      FQueue: TxlStrList;
      FSimKeyPress: boolean;
      procedure DoPaste ();
   public
   	constructor Create (AOwner: TxlWindow);
      destructor Destroy (); override;
      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
      procedure Paste (const s: widestring; b_simkeypress: boolean);
   end;

implementation

uses Messages, UGlobalObj, UOptionManager, UxlWinDef, UxlFunctions, UxlWinClasses, UxlStrUtils, UxlCommDlgs, Resource;

const WM_EXTERNALPASTE = WM_APP + 105;

procedure TExtPopupMenu.OnCreate ();
begin
	inherited;
	FhPopup := AddPopup (self.handle, 0);
end;

procedure TExtPopupMenu.PopupMenu (pid: integer; x, y: integer);
var i: integer;
	o_list: TxlStrList;
begin
   o_list := TxlStrList.Create;
   ListCenter.FillList (pid, o_list);

   ClearPopup (FhPopup);
   for i := o_list.Low to o_list.High do
   	AddItem(FhPopup, pid + i + 1, o_list[i]);

   o_list.free;
   Popup (0, x, y);
end;

//-------------------------

constructor TPopupHandler.Create (AOwner: TxlWindow);
begin
   FParent := AOwner;
   FMenu := TExtPopupMenu.Create (AOwner);
   CommandMan.AddSender (FMenu);
   OptionMan.AddObserver(self);
end;

destructor TPopupHandler.Destroy ();
begin
   OptionMan.RemoveObserver(self);
   CommandMan.RemoveSender (FMenu);
   FMenu.free;
	inherited;
end;

procedure TPopupHandler.OptionChanged ();
begin
	FFocusPopupMenu := not OptionMan.Options.PopupMenuNoFocus;
end;

procedure TPopupHandler.PopupMenu (id_menu: cardinal; b_caretpos: boolean);
var cp: TPoint;
	tid, cid: dword;
   hFocus: HWND;
   hFGWinHandle: HWND;
begin
	hFGWinHandle := GetForegroundWindow ();
	if (hFGWinHandle <> FParent.handle) and (not FParent.StayOnTop) and (Fparent.Status <> wsMinimize) then  // 以免焦点突然切换到 minipad2
      FParent.Status := wsMinimize;

	if b_caretpos then
   begin
      if hFGWinHandle <> FParent.handle then
      begin
         tid := getwindowthreadprocessid (hFGWinHandle, nil);
         cid := getwindowthreadprocessid (FParent.handle, nil);
         attachthreadinput (cid, tid, true);
      end;
      hFocus := GetFocus;

      GetCaretPos (cp);
      if (cp.x < 0) and (cp.y < 0) then GetCursorPos(cp);
      ClientToScreen (hFocus, cp);

      if FFocusPopupMenu then SetForeGroundWindow (FParent.handle);
      FMenu.PopupMenu (id_menu, cp.x, cp.y);
      if FFocusPopupMenu then SetForeGroundWindow (hFGWinHandle);
      SetFocus (hFocus);
      if hFGWinHandle <> FParent.handle then
         attachthreadinput (cid, tid, false);    // 只能加在popup之后，否则菜单无法消失  // 加了后经常粘入v，因此去除 09-05-10	end;
   end
   else
   begin
      GetCursorPos (cp);
      SetForeGroundWindow (FParent.handle);
   	FMenu.PopupMenu (id_menu, cp.x, cp.y);
   end;
end;

//-------------------------

constructor TPasteHandler.Create (AOwner: TxlWindow);
begin
   FParent := AOwner;
   FQueue := TxlStrList.Create;
   AOwner.AddMessageObserver (self);
	FPrepared := true;
end;

destructor TPasteHandler.Destroy ();
begin
	FParent.RemoveMessageObserver (self);
   FQueue.free;
   inherited;
end;

function TPasteHandler.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
var hwin1: HWND;
{$J+} const	hFGWinHandle: HWND = 0; bAfter: boolean = false;{$J-}
begin
	b_processed := false;
   case AMessage of
      WM_TRAYICONMOUSEMOVE:
      	begin
            hwin1 := GetForeGroundWindow;
            if (hwin1 <> FParent.handle) then
               hFGWinHandle := hwin1;
            bAfter := false;
         end;
      WM_TRAYICONAFTERPOPUPMENU:
      	bAfter := true;
      WM_EXTERNALPASTE:
      	begin
            if bAfter and (hFGWinHandle <> 0) then
            begin
               setforegroundwindow (hFGWinHandle);
               hFGWinHandle := 0;
               bAfter := false;
            end;
            DoPaste ();
         end;
   end;
end;

procedure TPasteHandler.Paste(const s: widestring; b_simkeypress: boolean);
begin
	FQueue.Add (s);
   FSimKeyPress := b_simkeypress;
   FParent.Post (WM_EXTERNALPASTE, 0, 0);
end;

procedure TPasteHandler.DoPaste ();
	procedure f_Paste ();
   begin
   	PressCombineKey ('V', VK_CONTROL);
	end;
   procedure f_Paste2 (const s: widestring; o_clip: TxlClipboard);
   begin
   	if s = '' then exit;
      ClipWatcher.PassNext;
   	o_clip.Text := s;
      f_Paste;
   end;
   procedure f_Paste3 (var s: widestring; i_pos: integer; vk: integer; o_clip: TxlClipboard);
   var i, n: integer;
      i_lb, i_rb, i_repeat: integer;
   begin
      f_Paste2 (LeftStr(s, i_pos - 1), o_clip);

      i_repeat := 1;         // 解析重复数
      s := MidStr (s, i_pos + 2);
      i_lb := FirstPos ('(', s);
      if i_lb = 1 then
      begin
      	i_rb := FirstPos (')', s, 3);
         if i_rb >= 3 then
         begin
         	n := StrToIntDef (MidStr(s, 2, i_rb - 2));
            if n > 0 then
            begin
            	i_repeat := n;
               s := MidStr (s, i_rb + 1);
            end;
         end;
      end;

      for i := 1 to i_repeat do
      begin
         SendKeyPress (vk, true, 2);
         SendKeyPress (vk, false, 2);
      end;
	end;
var tid, cid: dword;
   hFGWinHandle: HWND;
   i, i_min, j, n, m: integer;
   s: widestring;
   i_pos: array[0..7] of integer;
   o_clip: TxlClipboard;
const a_modkey: array[0..2] of integer = (VK_MENU, VK_CONTROL, VK_SHIFT);
const a_simkey: array[0..7] of integer = (VK_RETURN, VK_TAB, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END);
const s_simkey: array[0..7] of widestring = ('~n', '~t', '~l', '~r', '~u', '~d', '~h', '~e');
begin
   if FQueue.IsEmpty then exit;
   for i := 0 to 2 do
   begin
		if KeyPressed (a_modkey[i]) then
      	SendKeyPress (a_modkey[i], false);
	end;

	hFGWinHandle := GetForegroundWindow ();
   if hFGWinHandle <> FParent.handle then
   begin
      tid := getwindowthreadprocessid (hFGWinHandle, nil);
      cid := getwindowthreadprocessid (FParent.handle, nil);
      attachthreadinput (cid, tid, true);
   end;

   s := FQueue.Text;
   FQueue.Clear;
   o_clip := TxlClipboard.Create (hFGWinHandle);
   if FSimkeypress then
   begin
      j := FirstPos ('~s', s);   //  解析目标编辑器中选中的文字
      if j > 0 then
      begin
         ClipWatcher.PassNext;
         PressCombineKey ('C', VK_CONTROL);
         s := ReplaceStr (s, '~s', o_clip.Text);
      end;

      j := FirstPos ('~c', s);    //  将定位符转换为VK_LEFT
      if j > 0 then
      begin
      	Delete (s, j, 2);
         m := Length(s);
         n := m - j + 1;
         for i := j to m do
            if s[i] = #13 then dec (n);   // 对于#13#10只需一次按VK_LEFT
      	s := s + '~l(' + IntToStr(n) + ')';
      end;

   	while true do            // 解析自动填表
      begin
      	i_min := 0;
      	for i := Low(a_simkey) to High(a_simkey) do
         begin
         	i_pos[i] := FirstPos (s_simkey[i], s);
            if (i_pos[i] > 0) and ((i_pos[i_min] = 0) or (i_pos[i] < i_pos[i_min])) then i_min := i;
         end;
         if i_pos[i_min] = 0 then break;

  			f_Paste3 (s, i_pos[i_min], a_simkey[i_min], o_clip);
      end;
   end;
   f_Paste2 (s, o_clip);

   if hFGWinHandle <> FParent.handle then
   	attachthreadinput (cid, tid, false);
   o_clip.free;
end;


end.
