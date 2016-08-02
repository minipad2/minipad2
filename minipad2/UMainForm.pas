unit UMainForm;

interface

uses Windows, Messages, CommCtrl, UxlWindow, UxlWinClasses, UxlFunctions, UPluginManager, UIOHandler,
	UxlList, UxlStrUtils, UxlDialog, UxlFile, UxlClasses, UxlExtClasses, UxlTreeView, UTreeNavigator, UPageFactory, UTypeDef,
   UDialogs, UGlobalObj, Resource, UMenuManager, USpecialMode, UWorkSpace, USearchPage, UFavoritePage, UGroupPage,
   URecentPage, UTagPage, URecycleBin, UContactPage, UCalcDictPage, UMemoPage, ULinkPage, UTemplatePage, UClipPage;

type TMainForm = class (TxlWindow, ICommandExecutor, IMemorizer, IOptionObserver, ISizeObserver, IEventObserver, IHotkeyOwner, IPageObserver)
private
   FWorkSpace: TWorkSpace;
   FMenuMan: TMenuManager;
   FSPModeMan: TSPModeManager;
   FPluginMan: TPluginManager;
   FIOHandler: TIOHandler;
   FBackupHandler: TBackupHandler;

   FEscMinimize: boolean;
   FTransparency: TTransparency;
   FTransparent: boolean;
	FCaptionPageName: boolean;
   FProgramLoaded: boolean;

   procedure f_CheckTransparency ();
	procedure f_RefreshCaption (id: integer);
protected
	procedure OnCreate (); override;
   procedure OnClose (); override;
	procedure OnStatusChanged (); override;
	function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
public
   procedure ExecuteCommand (opr: word);
   function CheckCommand (opr: word): boolean;
   procedure SaveMemory ();
   procedure RestoreMemory ();
   procedure OptionChanged ();
   procedure OnHotkey (id: integer; hk: THotkey);
   procedure EventNotify (event, wparam, lparam: integer);
   procedure AdjustAppearance ();
   procedure OnPageEvent (pct: TPageEvent; id, id2: integer);
end;

implementation

uses SysUtils, UxlCommDlgs, UxlMenu, UxlDragControl, UxlWinControl, UxlWinDef, ShellAPI, UxlMiscCtrls, UOptionManager, ULangManager, UPageStore, UCommonClasses;

const e_CallWindow = 3059;

procedure TMainForm.OnCreate ();
begin
	FProgramLoaded := false;
//   GenerateEscapeMessage := true;
   self.Pos := MemoryMan.WinPos;    // 需放在前面，不然Editor恢复页面的滚动位置可能会有问题
   DragAcceptFiles (Fhandle, true);

   Clipwatcher := TClipwatcher.Create (self);

   FBackupHandler := TBackupHandler.Create;
   FMenuMan := TMenuManager.Create (self);
   FWorkSpace := TWorkSpace.create (self);
   FWorkSpace.align := alClient;
   FSPModeMan := TSPModeManager.Create (self);
   ProgTip := TxlTrackingTip.Create (self);
   ProgTip.TipWidth := 400;
   ProgTip.HideWhenCursorMove := true;

   FPluginMan := TPluginManager.Create (self);  // 需放在前面，以便 LinkHandler 优先处理 WM_DROPFILES 事件
   FIOHandler := TIOHandler.Create (self);

	OptionMan.AddObserver (self);
	CommandMan.AddExecutor (self);
   EventMan.AddObserver(self);
   SizeMan.AddObserver (self);
   MemoryMan.AddObserver (self);
   CommandMan.ItemEnabled [m_save] := false;
   PageCenter.AddObserver (self);

   FProgramLoaded := true;
   CommandMan.CheckCommands;
   ClearMemory;
end;

procedure TMainForm.OnClose ();
begin
//	self.hide;
	ProgTip.Free;
   FIOHandler.free;
   FPluginMan.Free;
   if SaveMan.Active then SaveMan.Save;
   MemoryMan.Save;
   SizeMan.RemoveObserver (self);
   EventMan.RemoveObserver(self);
   MemoryMan.RemoveObserver (self);
   OptionMan.RemoveObserver (self);
   CommandMan.RemoveExecutor (self);
   PageCenter.RemoveObserver(self);
    
   FWorkSpace.free;
	FMenuMan.Free;
   FSPModeMan.Free;
   FBackupHandler.free;
   ClipWatcher.free;
end;

procedure TMainForm.OnStatusChanged ();
var param: dword;
begin
   if (self.status = wsMinimize) then
   begin
		if FProgramLoaded then
      begin
      	SaveMan.Save ();
         MemoryMan.Save;
      end;
      SaveMan.Active := false;
      LogInHandler.NeedReValidate;
      ClearMemory;
   end
   else
   begin
   	SaveMan.Active := true;
      MemoryMan.WinStatus := self.status;
   end;
//   MemoryMan.WinMinimized := (self.status = wsMinimize);

   case self.status of
   	wsMinimize:
         param := p_minimized;
      wsMaximize:
         param := p_maximized;
      else
         param := p_restored;
   end;
   EventMan.EventNotify (e_winstatuschanged, param);
   CommandMan.ItemEnabled [m_specialmode] := (param = p_restored);
end;

function TMainForm.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var b_processed: boolean;
begin
	result := 0;
   b_processed := false;
	case AMessage of
      WM_EDITTREELABEL:
      	CommandMan.ExecuteCommand (m_rename);
      WM_ACTIVATEAPP:
      	if wparam <> 0 then
         	Post (WM_FOCUSEDITOR, 0, 0);
      WM_SIZE:
      	begin
            if (wParam = SIZE_RESTORED) or (wParam = SIZE_MAXIMIZED) then
               SizeMan.WinSized (self.width, self.height);
            SaveMemory;  // 实时记忆窗体尺寸
         end;
      WM_CALLWINDOWDEMAND:  // 用于 CheckSingleton
         begin
            self.BringToTop;
            if self.visible = false then  // restore from tray icon
               ExecuteCommand (m_restore)          // 需要 InvalidateLogIn
            else
               self.Status := MemoryMan.WinStatus;
            b_processed := true;
         end;
		WM_NCLBUTTONDBLCLK:   // 双击标题栏似乎不会自动转换为 WM_SYSCOMMAND 消息
//      	CommandMan.ItemEnabled[m_specialmode] := (self.Status = wsMaximize);
         begin
            if self.Status = wsNormal then
               self.Status := wsMaximize
            else
               self.Status := wsNormal;
//            OnStatusChanged;
            b_processed := true;
         end;
		WM_NCRBUTTONDOWN:
         if self.MainMenu = nil then
         begin
         	EventMan.EventNotify (e_ContextMenuDemand, ProgramContext);
            b_processed := true;
         end;
      WM_ESCAPE:
         if FEscMinimize then
            self.status := wsMinimize
         else
            CommandMan.ExecuteCommand(m_clear);
   end;
	if not b_processed then
      result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

procedure TMainForm.ExecuteCommand (opr: word);
   procedure f_OpenLink (const s_link: widestring);
   begin
      try
         ExecuteLink (s_link)
      except
         on E: Exception do ShowMessage (E.Message, mtInformation, LangMan.GetItem(sr_prompt));
      end;
   end;
   procedure f_ShowTree ();
   var i_oldwidth, i_oldheight, i_newwidth, i_newheight: integer;
   begin
   	CommandMan.SwitchCheck (m_showtree);
      FWorkSpace.GetClientSize (i_oldwidth, i_oldheight);
      FWorkSpace.TreeMode := CommandMan.ItemChecked[m_showtree];

      Update;
      if Visible and (Status <> wsMaximize) then  // 保持 Client 的大小与先前相同
      begin
         FWorkSpace.GetClientSize(i_newwidth, i_newheight);
         Width := Width + i_oldwidth - i_newWidth;  // 由于工具栏的高度可能改变，因此必须先执行一下。
         Height := Height + i_oldheight - i_newHeight;
      end
   end;
   function f_IsChinese (): boolean;
   begin
      result := IsSameStr (LangMan.Language, 'Chinese');
   end;
var s: widestring;
   b: boolean;
begin
   case opr of
   	m_save:
      	begin
	      	SaveMan.Save;
            ProgTip.ShowTip (LangMan.GEtItem (sr_datasaved));
         end;
   	m_showtree:
      	f_ShowTree;
      m_transparent:
      	begin
         	FTransparent := not FTransparent;
            f_CheckTransparency;
            CommandMan.ItemChecked[m_transparent] := FTransparent;
         end;
      m_options:
         begin
      		if (self.Status = wsMinimize) and not LogInHandler.ValidateLogIn then exit;
            OptionMan.SetOptions (self);
         end;
      m_helptopic:
      	begin
         	s := ProgDir + LangMan.Language + '.chm';
      		if FileExists (s) then
            	f_OpenLink (s)
            else if FileExists (ProgDir + 'English.chm') then
            	f_OpenLink (ProgDir + 'English.chm')
            else if FileExists (ProgDir + 'Chinese.chm') then
            	f_OpenLink (ProgDir + 'Chinese.chm')
            else if f_IsChinese then
            	f_OpenLink ('http://www.nebulasoft.cn/minipad2/downloads.html')
            else
            	f_OpenLink ('http://www.nebulasoft.cn/minipad2/downloads_en.html');
         end;
      m_homepage:
      	if f_IsChinese then
         	f_OpenLink ('http://www.nebulasoft.cn/index.html')
         else
         	f_OpenLink ('http://www.nebulasoft.cn/index_en.html');
      m_forum:
      	if f_IsChinese then
         	f_OpenLink ('http://www.nebulasoft.cn/bbs')
         else
         	f_OpenLink ('http://www.nebulasoft.cn/forum');
      m_donate:
      	if f_IsChinese then
         	f_OpenLink ('http://www.nebulasoft.cn/donate.html')
         else
         	f_OpenLink ('http://www.nebulasoft.cn/donate_en.html');
      m_about:
         with TAboutBox.create (self) do
         begin
            Execute;
            free;
         end;
      m_restore:
      	begin
            if self.Status = wsMinimize then
            begin
               b := true;
            	if self.Visible = false then
                  b := LogInHandler.ValidateLogIn;
               if b then
               begin
                  if MemoryMan.WinStatus <> wsMinimize then
               	   self.Status := MemoryMan.WinStatus
                  else
                     self.Status := wsNormal;  //wsRestoreLast;
               end;
            end
            else
               self.Status := wsMinimize;
         end;
      m_exit:
      	self.Close;
   end;
end;

function TMainForm.CheckCommand (opr: word): boolean;
begin
	result := true;
end;

procedure TMainForm.SaveMemory ();
begin
	if not FProgramLoaded then exit;
	MemoryMan.ShowTree := FWorkSpace.TreeMode;
   MemoryMan.Transparent := (self.Transparency <> 0);
   if self.status = wsNormal then
   	MemoryMan.winpos := self.pos;
end;

procedure TMainForm.RestoreMemory ();
   function StartMinimize (): boolean;
   begin
      result := OptionMan.Options.StartMinimize or (IsSameStr(paramstr(1), '-m') and (not MemoryMan.SpecialMode));
   end;
begin
	if MemoryMan.ShowTree then
   	CommandMan.ExecuteCommand (m_showtree)
   else
   	FWorkSpace.TreeMode := false;
   if MemoryMan.Transparent then
   	CommandMan.ExecuteCommand (m_transparent);

	if StartMinimize then  // 需放在后面
   	self.Status := wsMinimize
   else
      self.Status := MemoryMan.WinStatus;
end;

procedure TMainForm.OptionChanged ();
begin
   MinimizeToTray := OptionMan.options.MinimizeToTray;
   CloseMinimize := OptionMan.Options.CloseMinimize;
   FTransparency := OptionMan.options.Transparency;
   f_CheckTransparency;
   FEscMinimize := OptionMan.options.Escminimize;
   FCaptionPageName := OptionMan.Options.CaptionPageName;
   if PageCenter.ActivePage <> nil then
   	f_RefreshCaption (PageCenter.ActivePage.id);

   HotkeyCenter.AddHotKey (self, e_callwindow, OptionMan.Options.CallWinHotkey);
end;

procedure TMainForm.AdjustAppearance ();
begin
	self.Update;
end;

procedure TMainForm.f_CheckTransparency ();
begin
   if FTransparent then
      self.Transparency := FTransparency
   else
      self.Transparency := 0;
end;

procedure TMainForm.EventNotify (event, wparam, lparam: integer);
begin
	case event of
      e_TrayIconClicked:
      	if not FSpModeMan.ShowWindowDemand then
            CommandMan.ExecuteCommand(m_restore);
      e_PageNameChanged:
         f_RefreshCaption (wparam);
   end;
end;

procedure TMainForm.OnHotkey (id: integer; hk: THotkey);
begin
	if id = e_CallWindow then
      if (self.status <> wsMinimize) and (GetForeGroundWindow <> self.handle) then
         SetForeGroundWindow (self.Handle)
      else
         ExecuteCommand (m_restore);
end;

procedure TMainForm.OnPageEvent (pct: TPageEvent; id, id2: integer);
begin
   if (pct = pctSetActive) then
   	f_RefreshCaption (id);
end;

procedure TMainForm.f_RefreshCaption (id: integer);
begin
   if FCaptionPageName then
      self.Text := PageStore[id].Name + ' - minipad2'
   else
   	self.Text := 'minipad2';
end;

end.











