unit UMainForm;

interface

uses Windows, Messages, UxlWindow, UxlWinClasses, UxlMenu, UxlClasses, UxlExtClasses, UDisplayManager, UQueryManager;

type
	TPopMenu = class (TxlMenu)
   protected
   	procedure OnInitialize (); override;
   end;

	TTrayMenu = class (TxlMenu)
   protected
   	procedure OnInitialize (); override;
	end;

   type TAccelTable = class (TxlAccelTable)
   protected
      procedure OnInitialize (); override;
   end;

   TMainForm = class (TxlWindow, ICommandExecutor, IOptionObserver, IHotkeyOwner)
   private
		FPopMenu: TPopMenu;
      FTrayMenu: TTrayMenu;
      FTrayIcon: TxlTrayIcon;
      FAccelTable: TAccelTable;
      FDisplayMan: TDisplayManager;

      procedure f_OnTrayIconClick (Sender: TObject);
	protected
		function ProcessMessage (AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD; override;
   protected
      procedure OnCreate (); override;
      procedure OnClose (); override;
   	procedure OnHotkey (id: integer);

      procedure OptionChanged ();
      procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
   end;

implementation

uses SysUtils, Resource, UxlCommDlgs, UxlFunctions, UxlMiscCtrls, UDialogs, UxlWinDef, UGlobalObj, UxlDialog;

procedure TPopMenu.OnInitialize ();
begin
	inherited;
   SetTemplate (PopMenu);
end;

procedure TTrayMenu.OnInitialize ();
begin
	inherited;
   SetTemplate (TrayMenu);
end;

procedure TAccelTable.OnInitialize ();
begin
	AddKey (m_copy, 'Ctrl+C');
	AddKey (m_next, 'NEXT');
   AddKey (m_testmode, 'Ctrl+T');
   AddKey (m_newbox, 'Ctrl+NEXT');
   AddKey (m_options, 'Ctrl+O');
   CreateAccelTable;
end;

//--------------------

procedure TMainForm.OnCreate ();
begin
   StayOnTop := true;
   TitleBar := false;
   ToolWindowStyle := true;

   ProgTip := TxlTrackingTip.Create (self);
   ProgTip.TipWidth := 400;
   ProgTip.HideWhenCursorMove := true;

	FPopMenu := TPopMenu.Create (self);
   CommandMan.AddSender (FPopMenu);

	FTrayMenu := TTrayMenu.create (self);
   CommandMan.AddSender (FTrayMenu);

   FTrayIcon := TxlTrayIcon.Create (self, MainIcon, 'ÔÆÓ°µ¥´Ê');
   with FTrayIcon do
   begin
   	Menu := FTrayMenu;
   	OnClick := f_OnTrayIconClick;
   end;
   SetTrayIcon (FTrayIcon);

	FAccelTable := TAccelTable.Create ();
   FAccelTable.AssociateMenu (FPopMenu);
   self.SetAccelTable (FAccelTAble);

   FDisplayMan := TDisplayManager.Create (self);

   OptionMan.AddObserver (self);
   CommandMan.AddExecutor (self);
end;

procedure TMainForm.OnClose ();
begin
	inherited;
   CommandMan.RemoveSender (FPopMenu);
   CommandMan.RemoveSender (FTrayMenu);
   CommandMan.RemoveExecutor (self);
   MemoryMan.Save;
	OptionMan.RemoveObserver (self);
   HotkeyCenter.RemoveOwner (self);
   
	ProgTip.Free;
   SetTrayIcon (nil);
   FAccelTable.Free;
   FTrayIcon.Free;
   FTrayMenu.free;
   FPopMenu.Free;
   FDisplayMan.Free;
end;

function TMainForm.ProcessMessage (AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
begin
	result := 0;
   b_processed := true;
   case AMessage of
      WM_CONTEXTMENU:
      	begin
				FDisplayMan.Pause;
            FPopMenu.PopUp (0, -1, -1, pdLeftTop, true);
            FDisplayMan.Resume;
         end;
      WM_LBUTTONDBLCLK:
      	CommandMan.ExecuteCommand (m_fullscreen);
      else
      	result := inherited ProcessMessage (AMessage, wParam, lParam, b_processed);
   end;
end;

procedure TMainForm.OnHotkey (id: integer);
begin
	CommandMan.ExecuteCommand (id);
end;

procedure TMainForm.f_OnTrayIconClick (Sender: TObject);
begin
	CommandMan.ExecuteCommand (m_start);
end;

procedure TMainForm.ExecuteCommand (opr: word);
   procedure f_OpenLink (const s_link: widestring);
   begin
      try
         ExecuteLink (s_link)
      except
         on E: Exception do ShowMessage (E.Message);
      end;
   end;
begin
	case opr of
   	m_options:
      	begin
         	FDisplayMan.Pause;
				OptionMan.SetOptions (self);
            FDisplayMan.Resume;
         end;
      m_helptopic:
      	f_OpenLink (ProgDir + 'ReadMe.txt');
      m_homepage:
      	f_OpenLink ('http://www.nebulasoft.cn/index.html');
      m_forum:
      	f_OpenLink ('http://www.nebulasoft.cn/bbs');
      m_donate:
        	f_OpenLink ('http://www.nebulasoft.cn/donate.html');
   	m_about:
      	begin
            FDisplayMan.Pause;
            with TAboutBox.create (self, dpScreenCenter) do
            begin
               Execute;
               free;
            end;
            FDisplayMan.Resume;
         end;
   	m_exit: close;
   end;
end;

function TMainForm.CheckCommand (opr: word): boolean;
begin
	result := true;
end;

procedure TMainForm.OptionChanged ();
begin
   Color := OptionMan.Options.WinColor;
   HotkeyCenter.AddHotKey (self, m_start, OptionMan.Options.StartHotkey);
   HotkeyCenter.AddHotKey (self, m_fullscreen, OptionMan.Options.FullScreenHotkey);
   HotkeyCenter.AddHotKey (self, m_trackmouse, OptionMan.Options.TrackMouseHotKey);
end;

end.


