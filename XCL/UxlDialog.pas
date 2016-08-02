unit UxlDialog;
// 在资源编辑器中创建的Dialog资源建议一律去除 WS_VISIBLE，由 TxlDialogSuper 来决定其 visible，否则会有闪烁

interface

uses Windows, Messages, UxlWinControl, UxlClasses, UxlStrUtils, UxlFunctions, UxlList;

type
	TDialogPos = (dpParentCenter, dpScreenCenter);

   TxlDialogSuper = class (TxlWinContainer)
   private
      FLinkObjList: TxlObjList;
      FLinkStrList: TxlStrList;
      FDialogPos: TDialogPos;

      procedure f_SetItemText (ctrlID: integer; const value: widestring);
      function f_GetItemText (ctrlID: integer): widestring;
      procedure f_SetItemChecked (ctrlID: integer; value: boolean);
      function f_GetItemChecked (ctrlID: integer): boolean;
      procedure f_SetItemEnabled (ctrlID: integer; value: boolean);
      function f_GetItemEnabled (ctrlID: integer): boolean;
      procedure f_SetItemVisible (ctrlID: integer; value: boolean);
      function f_GetItemVisible (ctrlID: integer): boolean;

      procedure f_OnLinkClick (sender: TObject);
   protected
      FHParent: HWND;
      FTemplate: pwidechar;
      FIcon: integer;

      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
      function DefMessageProcess (AMessage, wParam, lParam: DWORD): DWORD; override;

      procedure OnInitialize (); virtual; abstract;
      procedure OnDestroy (); virtual;
      procedure OnOpen (); virtual;
      procedure OnClose (); virtual;
      procedure OnCommand (ctrlID: integer); virtual;

      procedure SetTemplate (id_template: integer; ic_icon: integer = 0);
      procedure AddLink (id_link: cardinal; const s_link: widestring = '');

      procedure FocusControl (CtrlID: integer);
      procedure MoveCursorLast (EditCtrlID: integer);

      function GetItemHandle (ctrlID: integer): HWND;
      property ItemHandle[ctrlID: integer]: HWND read GetItemHandle;
      procedure Close (b_ok: boolean = true); virtual;
   public
      constructor Create (WndParent: TxlWinControl = nil; dp: TDialogPos = dpParentCenter);
      destructor Destroy (); override;

      property ItemText[ctrlID: integer]: widestring read f_GetItemText write f_SetItemText;
      property ItemChecked[ctrlID: integer]: boolean read f_GetItemChecked write f_SetItemChecked;
      property ItemEnabled[ctrlID: integer]: boolean read f_GetItemEnabled write f_SetItemEnabled;
      property ItemVisible[ctrlID: integer]: boolean read f_GetItemVisible write f_SetItemVisible;
      property DialogPos: TDialogPos read FDialogPos write FDialogPos;
   end;

   TxlDialog = class (TxlDialogSuper)
   private
      procedure f_OnInitDialog (hDlg: HWND);
   protected
      procedure Close (b_ok: boolean = true); override;
   public
      function Execute (): boolean;
   end;

   TxlDialogML = class (TxlDialogSuper)
   private
      FOnDialogClosed: TNotifyEvent;
   protected
   public
      procedure Open (b_show: boolean = true);
      procedure Close (b_ok: boolean = true); override;

      property OnDialogClosed: TNOtifyEvent read FOnDialogClosed write FOnDialogClosed;
   end;
   TDialogMLClass = class of TxlDialogML;
   
function DialogProcML(hDlg: HWnd; AMessage, WParam, LParam: DWORD): BOOL; stdcall;
function DialogProc(hDlg: HWnd; AMessage, WParam, LParam: DWORD): BOOL; stdcall;    // 不能用boolean作为返回值！否则无法处理wm_ctlcolorstatic

implementation

uses SysUtils, UxlWindow, UxlStdCtrls, UxlCommDlgs, UxlWinDef;

constructor TxlDialogSuper.Create (WndParent: TxlWinControl = nil; dp: TDialogPos = dpParentCenter);
begin
   SuperOnCreate;
   if WndParent <> nil then
	   FHParent := WndParent.handle
   else
      FHParent := MainWinHandle;
   FLinkObjList := TxlObjList.Create();
   FLinkStrList := TxlStrList.Create();
   Fhandle := 0;
   FDialogPos := dp;
   OnInitialize ();
end;

destructor TxlDialogSuper.Destroy ();
begin
	OnDestroy;
   FLinkObjList.Free;
   FLinkStrList.Free;
   SuperOnDestroy;
   inherited;
end;

procedure TxlDialogSuper.Close(b_ok: boolean);
begin
	FLinkObjList.ClearAndFreeAll;
	OnClose ();
end;

procedure TxlDialogSuper.SetTemplate (id_template: integer; ic_icon: integer = 0);
begin
	Ftemplate := MakeIntResourceW (id_template);
   FIcon := ic_icon;
end;

procedure TxlDialogSuper.AddLink (id_link: cardinal; const s_link: widestring = '');
var o_link: TxlStaticLink;
begin
   o_link := TxlStaticLink.Create (self, Itemhandle[id_link]);
   with o_link do
   begin
   	OnClick := f_OnLinkClick;
   	SetLinkFont('Arial', 9);
      Color := GetSysColor(COLOR_BTNFACE);
   end;
   FLinkObjList.Add (o_link);
   if s_link <> '' then
   	FLinkStrList.Add (s_link)
   else
   	FLinkStrList.Add (ItemText[id_link]);
end;

procedure TxlDialogSuper.f_OnLinkClick (sender: TObject);
var i: integer;
begin
   i := FLinkObjList.Find(sender);
   try
		Executelink (FLinkStrList[i]);
   except
      on E: Exception do ShowMessage (E.message);
   end;
end;

procedure TxlDialogSuper.FocusControl (CtrlID: integer);
begin
	Post (WM_NEXTDLGCTL, ItemHandle[CtrlID], 1);
end;

procedure TxlDialogSuper.MoveCursorLast (EditCtrlID: integer);
begin
   FocusControl (EditCtrlID);
   PostMessageW (GetItemHandle(EditCtrlID), EM_SETSEL, 10000000, 10000000);
end;

function TxlDialogSuper.GetItemHandle (ctrlID: integer): HWND;
begin
	result := GetDlgItem (Fhandle, ctrlID);
end;

procedure TxlDialogSuper.OnOpen ();
var rc, rcs: TRect;
	ps, o_pos: TPos;
begin
   GetScreenRect (rcs);
	if (FHParent <> 0) and (FDialogPos = dpParentCenter) then
   begin
      GetWindowRect (FHParent, rc);
      if RectInRect (rc, rcs) then
         ps := RectToPos (rc)
      else
      	ps := RectToPos (rcs);
   end
   else
   	ps := RectToPos (rcs);
   o_pos := self.Pos;
   o_pos.x := ps.x + (ps.width - o_pos.Width) div 2;
   o_pos.y := ps.y + (ps.height - o_pos.Height) div 2;
   ValidateWindowPos (o_pos, false);
   self.Pos := o_pos;
   self.BringToTop;
end;

procedure TxlDialogSuper.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	IDOK: close (true);
      IDCANCEL: close (false);
   end;
end;

procedure TxlDialogSuper.OnClose ();
begin
end;

procedure TxlDialogSuper.OnDestroy ();
begin
end;

//--------- Message Handler ------------

procedure TxlDialogSuper.f_SetItemText (ctrlID: integer; const value: widestring);
begin
	SetDlgItemTextW (Fhandle, ctrlID, pwidechar(value));
end;

function TxlDialogSuper.f_GetItemText (ctrlID: integer): widestring;
var p: array [0..5000] of widechar;
	n: integer;
begin
   n := GetDlgItemTextW (Fhandle, ctrlID, p, 5000);
   p[n] := #0;
   result := p;
end;

procedure TxlDialogSuper.f_SetItemChecked (ctrlID: integer; value: boolean);
var h: HWND;
begin
	h := GetDlgItem (Fhandle, ctrlID);
	if value then
   	SendMessage (h, BM_SETCHECK, BST_CHECKED, 0)
   else
   	SendMessage (h, BM_SETCHECK, BST_UNCHECKED, 0);
end;

function TxlDialogSuper.f_GetItemChecked (ctrlID: integer): boolean;
var h: HWND;
begin
	h := GetDlgItem (Fhandle, ctrlID);
	result := (SendMessage (h, BM_GETCHECK, 0, 0) = BST_CHECKED);
end;

procedure TxlDialogSuper.f_SetItemEnabled (ctrlID: integer; value: boolean);
var h: HWND;
begin
   h := GetDlgItem (Fhandle, ctrlID);
   EnableWindow (h, value);
end;

function TxlDialogSuper.f_GetItemEnabled (ctrlID: integer): boolean;
var h: HWND;
begin
   h := GetDlgItem (Fhandle, ctrlID);
   result := IsWindowEnabled (h);
end;

procedure TxlDialogSuper.f_SetItemVisible (ctrlID: integer; value: boolean);
var h: HWND;
begin
   h := GetDlgItem (Fhandle, ctrlID);
	if value then
   	ShowWindow (h, SW_Show)
   else
   	ShowWindow (h, SW_Hide);
end;

function TxlDialogSuper.f_GetItemVisible (ctrlID: integer): boolean;
var h: HWND;
begin
   h := GetDlgItem (Fhandle, ctrlID);
   result := IsWindowVisible (h);
end;

//---------------------

function TxlDialogSuper.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
begin
	result := 1;
   if ((AMessage = WM_SYSCOMMAND) and (wParam = SC_Close)) or ((AMessage = WM_COMMAND) and (wParam = IDCANCEL)) then
      close (false)
   else
   begin
      case AMessage of
         WM_COMMAND:
            begin
               inherited ProcessMessage (AMessage, wParam, lParam);
               OnCommand (loword(wParam));
            end;
         WM_Notify:
            begin
               result := inherited ProcessMessage (AMessage, wParam, lParam);
               SetWindowLong (Fhandle, DWL_MSGRESULT, result);
            end;
         WM_CTLCOLORDLG:
         	begin
               SetTextColor (wParam, font.Color);
               SetBkColor (wParam, self.color);
         		result := self.Brush;
            end;
         else
      		result := inherited ProcessMessage (AMessage, wParam, lParam);
      end;
   end;
end;

function TxlDialogSuper.DefMessageProcess (AMessage, wParam, lParam: DWORD): DWORD;
begin
	result := 0;
end;

//-------------------

var FOnInitDialog: procedure (hDlg: HWND) of object;

function TxlDialog.Execute (): boolean;
begin
	Fhandle := 0;
   FOnInitDialog := f_OnInitDialog;
   result := IntToBool ( DialogBoxW (system.MainInstance, Ftemplate, FHParent, @DialogProc) );
end;

procedure TxlDialog.close (b_ok: boolean = true);
begin
   inherited close(b_ok);
	EndDialog (Fhandle, BooltoInt(b_ok));
   SendMessageW (FHParent, WM_DIALOGCLOSED, 0, 0);
end;

procedure TxlDialog.f_OnInitDialog (hDlg: HWND);
var mh: TMessageHandler;
begin
   Fhandle := hDlg;
   if FIcon <> 0 then
      SetIcon (FIcon);
   mh := OnWinMessage;
   TMHCenter.GetInstance.RegisterMH (hDlg, mh);
   SendMessageW (FHParent, WM_DIALOGOPENED, 0, 0);
   OnOpen ();
end;

function DialogProc(hDlg: HWnd; AMessage, WParam, LParam: DWORD): BOOL; stdcall;
var mh: TMessageHandler;
begin
   if AMessage = WM_INITDIALOG then
   begin
      FOnInitDialog (hDlg);
      result := true;
   end
   else
   begin
      mh := TMHCenter.GetInstance.GetMH (hDlg);
      if assigned (mh) then
         result := Bool ( mh (AMessage, wParam, lParam))
      else
         result := false;
   end;
end;

//----------------------

procedure TxlDialogML.Open (b_show: boolean = true);
var mh: TMessageHandler;
begin
	Fhandle := CreateDialogW (system.MainInstance, Ftemplate, FHParent, @DialogProcML);
   if FIcon <> 0 then SetIcon (FIcon);
   mh := OnWinMessage;
   TMHCenter.GetInstance.RegisterMH (Fhandle, mh);
//   SendMessageW (FHParent, WM_DIALOGOPENED, 0, 0);
   OnOpen ();
   if b_show then
   	Show;
   ClearMemory;
end;

procedure TxlDialogML.Close (b_ok: boolean = true);
begin
	if Fhandle <> 0 then
   begin
   	inherited close(b_ok);
      DestroyWindow (Fhandle);
      TMHCenter.GetInstance.UnRegisterMH(Fhandle);
      Fhandle := 0;
      if assigned (FOnDialogClosed) then
         FOnDialogClosed (self);
   end;
end;

function DialogProcML (hDlg: HWnd; AMessage, WParam, LParam: DWORD): BOOL; stdcall;
var mh: TMessageHandler;
begin
   mh := TMHCenter.GetInstance.GetMH (hDlg);
   if assigned(mh) then
      result := BOOL (mh (AMessage, WParam, lParam))
   else
      result := false;
end;

//-----------------------

var FComCtl32Dll: HMODULE;

initialization
	FComCtl32Dll := LoadLibraryW ('comctl32.dll');   // 无此类库调用的话，对话框资源中的某些common control无法正常加载。

finalization
   FreeLibrary (FComCtl32DLL);

end.


