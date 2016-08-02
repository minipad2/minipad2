unit UxlStdCtrls;

interface

uses windows, Messages, UxlWinControl, UxlClasses, UxlFunctions, UxlList;

type
   TxlStaticControl = class (TxlControl)
   private
      FOnClick: TNotifyEvent;
      FOnDblClick: TNotifyEvent;
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      function f_staticstyle (): DWORD; virtual; abstract;
      function ProcessCommand (dwEvent: word): dword; override;
   public
      property OnClick: TNotifyEvent read FonClick write FOnClick;
   	property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
   end;

   TxlStaticText = class (TxlStaticControl)
   protected
      function f_staticstyle (): DWORD; override;
   end;

   TxlStaticLink = class (TxlStaticText)
   private
   	FHCursor: HCursor;
   protected
      procedure OnCreateControl (); override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
   public
      procedure SetLinkFont (const s_name: widestring; i_size: integer);
   end;

   TxlStaticIcon = class (TxlStaticControl)
   protected
      function f_staticstyle (): DWORD; override;
   public
   	procedure SetIcon (i_id: integer);
   end;

type
   TxlButtonControl = class (TxlControl)
   private
      FonClick, FOnRightClick: TNotifyEvent;
      procedure SetFlat (value: boolean);
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      function f_buttonstyle (): DWORD; virtual; abstract;
      function f_getcheck (): boolean;
      procedure f_setcheck (b_checked: boolean);
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;

//      property Checked: boolean read f_getcheck write f_setcheck;
   public
      function ProcessCommand (dwEvent: word): dword; override;

      property Flat: boolean write SetFlat;
      property OnClick: TNotifyEvent read FonClick write FOnClick;
      property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
   end;

   TxlButton = class (TxlButtonControl)
   private
   	FHIcon: HIcon;
   protected
      function f_buttonstyle (): DWORD; override;
      procedure f_DestroyIcon ();
      procedure SetHIcon (h_icon: HIcon);
      procedure OnDestroyControl (); override;
   public
      procedure SetAsDefault ();
      procedure SetIcon (id_icon: integer); overload;
      procedure SetIcon (const s_iconfile: widestring); overload;
   end;

   TxlCheckBox = class (TxlButtonControl)
   protected
      function f_buttonstyle (): DWORD; override;
   public
      property Checked: boolean read f_getcheck write f_setcheck;
   end;

   TxlRadioButton = class (TxlButtonControl)
   protected
      function f_buttonstyle (): DWORD; override;
   public
      property Checked: boolean read f_getcheck write f_setcheck;
   end;

   TxlGroupBox = class (TxlButtonControl)
   protected
      function f_buttonstyle (): DWORD; override;
   end;

type
	TScrollEvent = procedure (i_pos: integer) of object;

	TxlScrollBar = class (TxlControl)
   private
   	FScrollEvent: TScrollEvent;
      procedure f_SetScrollPos (value: integer);
      function f_GetScrollPos (): integer;
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
      function f_ScrollStyle (): dword; virtual; abstract;
   public
   	procedure SetRange (i_min, i_max: integer);
      property ScrollPos: integer read f_GetScrollPos write f_SetScrollPos;
      property OnScroll: TScrollEvent read FScrollEvent write FScrollEvent;
   end;

   TxlHorzScrollBar = class (TxlScrollBar)
   protected
      function f_ScrollStyle (): dword; override;
	end;

   TxlVertScrollBar = class (TxlScrollBar)
   protected
      function f_ScrollStyle (): dword; override;
	end;

implementation

uses UxlCommDlgs;

function TxlStaticControl.DoCreateControl (HParent: HWND): HWND;
begin
	result := CreateWin32Control (HParent, 'static');
end;

procedure TxlStaticControl.OnCreateControl ();
begin
	SetWndStyle (f_staticstyle or SS_Notify);
end;

function TxlStaticControl.ProcessCommand (dwEvent: word): dword;
begin
	result := 0;
   case dwEvent of
      STN_CLICKED:
         if assigned (FOnClick) then FOnClick (self);
      STN_DBLCLK:
      	if assigned (FOnDblClick) then FOnDblClick (self);
	end;
end;

//------------

function TxlStaticText.f_staticstyle (): DWORD;
begin
	result := 0;
end;

//-----------

procedure TxlStaticLink.OnCreateControl ();
begin
	inherited;
   FHCursor := LoadCursor (0, IDC_HAND);
   SetWndProc (@WindowProc);
   with self.Font do
   begin
      Color := RGB(0, 0, 255);
      Underline := true;
	end;
end;

procedure TxlStaticLink.SetLinkFont (const s_name: widestring; i_size: integer);
begin
	with self.font do
   begin
   	Name := s_name;
      Size := i_size;
   end;
end;

function TxlStaticLink.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
begin
  	case AMessage of
   	WM_MOUSEMOVE:
      	begin
         	SetCursor (FHCursor);
            result := 0;
         end
      else
         result := inherited ProcessMessage (AMessage, wParam, lParam);
  	end;
end;

//-----------------

function TxlStaticIcon.f_staticstyle (): DWORD;
begin
	result := SS_ICON;
end;

procedure TxlStaticIcon.SetIcon (i_id: integer);
begin
   Perform (STM_SETIMAGE, IMAGE_ICON, LoadIconFromResource (i_id));
end;

//---------------------

function TxlButtonControl.DoCreateControl (HParent: HWND): HWND;
begin
	result := CreateWin32Control (HParent, 'button', f_buttonstyle);
end;

procedure TxlButtonControl.OnCreateControl ();
begin
	inherited;
   SetWndProc (@WindowProc);
end;

function TxlButtonControl.ProcessCommand (dwEvent: word): dword;
begin
	result := 0;
   case dwEvent of
      BN_CLICKED:
         if assigned (FOnClick) then FOnClick (self);
	end;
end;

function TxlButtonControl.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
begin
	case AMessage of
   	WM_RBUTTONDOWN:
      	if assigned (FOnRightClick) then
         	FOnRightClick (self);
   end;
   result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

function TxlButtonControl.f_getcheck (): boolean;
begin
   result := (SendMessage (Fhandle, BM_GETCHECK, 0, 0) = BST_CHECKED);
end;

procedure TxlButtonControl.f_setcheck (b_checked: boolean);
begin
	if b_checked then
   	Perform (BM_SETCHECK, BST_CHECKED, 0)
   else
   	Perform (BM_SETCHECK, BST_UNCHECKED, 0);
end;

procedure TxlButtonControl.SetFlat (value: boolean);
begin
	SetWndStyle (BS_FLAT, value);
end;

//-----------------------

function TxlButton.f_buttonstyle (): DWORD;
begin
   result := BS_PUSHBUTTON;
end;

procedure TxlButton.SetAsDefault ();
begin
	Perform (BM_SETSTYLE, BS_DEFPUSHBUTTON, 1);
end;

procedure TxlButton.OnDestroyControl ();
begin
	f_DestroyIcon;
end;

procedure TxlButton.f_DestroyIcon ();
begin
	if FHIcon <> 0 then
   	DestroyIcon (FHIcon);
end;

procedure TxlButton.SetIcon (id_icon: integer);
begin
//	Perform (BCM_GETTEXTMARGIN, 0, rc);
   SetHIcon ( LoadIconFromResource (id_icon, self.Width - 2, self.Height - 2) );
end;

procedure TxlButton.SetIcon (const s_iconfile: widestring);
begin
   SetHIcon ( LoadIconFromFile (s_iconfile) );
end;

procedure TxlButton.SetHIcon (h_icon: HIcon);
begin
	f_DestroyIcon;
	FHIcon := h_icon;
   SetWndStyle (BS_ICON);
   Perform (BM_SETIMAGE, IMAGE_ICON, h_icon);
end;

//-------------------

function TxlCheckBox.f_buttonstyle (): DWORD;
begin
   result := BS_AUTOCHECKBOX;
end;

function TxlRadioButton.f_buttonstyle (): DWORD;
begin
   result := BS_AUTORadioButton;
end;

function TxlGroupBox.f_buttonstyle (): DWORD;
begin
	result := BS_GROUPBOX;
end;

//-----------------------

function TxlScrollBar.DoCreateControl (HParent: HWND): HWND;
begin
	result := CreateWin32Control (HParent, 'SCROLLBAR', f_ScrollStyle);
end;

function TxlHorzScrollBar.f_ScrollStyle (): dword;
begin
	result := SBS_HORZ;
end;

function TxlVertScrollBar.f_ScrollStyle (): dword;
begin
	result := SBS_VERT;
end;

procedure TxlScrollBar.SetRange (i_min, i_max: integer);
begin
	SetScrollRange (Fhandle, SB_CTL, i_Min, i_Max, true) ;
end;

procedure TxlScrollBar.f_SetScrollPos (value: integer);
begin
   SetScrollPos (Fhandle, SB_CTL, value, true);
end;

function TxlScrollBar.f_GetScrollPos (): integer;
begin
	result := GetScrollPos (Fhandle, SB_CTL);
end;

end.

