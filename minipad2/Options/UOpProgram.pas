unit UOpProgram;

interface

uses UOptionBox, UxlComboBox, UxlMiscCtrls, UxlFile, UOptionManager;

type
   TOpProgram = class (TOptionPanel)
   private
      FCallWinHotKey: TxlHotKey;
	   FUdRecentNotesCount: TxlUpDown;
      FCmbRecyclerLimit: TxlComboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
		procedure OnCommand (ctrlID: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpEdit = class (TOptionPanel)
   private
   	FTabStops: TxlComboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpNotes = class (TOptionPanel)
   private
      FNewNote_ForeGround_HotKey: TxlHotKey;
      FNewNote_BackGround_Hotkey: TxlHotKey;
      FSnapText_Hotkey: TxlHotKey;
      FNewNoteBG_SnapText_Hotkey: TxlHotKey;
      FSnapTextToFile_Hotkey: TxlHotkey;
      FCloseNote_Hotkey: TxlHotkey;
      FAutoRecord_Hotkey: TxlHotkey;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpImportExport = class (TOptionPanel)
   private
   	FcmbEncode: TxlComboBox;
   	procedure f_CheckEnableImportOptions ();
      procedure f_CheckEnableExportOptions ();
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpBehavior = class (TOptionPanel)
   private
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpLogIn = class (TOptionPanel)
   private
   	FLockTrayTime: TxlComboBox;
		procedure f_CheckEnable ();
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
   	function Validate (): boolean; override;
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpBackUp = class (TOptionPanel)
   private
	   FUdBackupInterval: TxlUpDown;
   	FAutoSaveTime, FBackupIntervalType, FTotalBackup: TxlComboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpSpecialMode = class (TOptionPanel)
   private
      FCmbDirection, FCmbHideDelay, FCmbShowDelay, FCmbAnimationTime, FCmbEdgeWidth: TxlComboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

implementation

uses Windows, ULangManager, UxlCommDlgs, UxlFunctions, UxlMath, UTypeDef, UxlStrUtils, UIOHandler, Resource;

procedure TOpProgram.OnInitialize ();
begin
	SetTemplate (ob_program);
end;

const c_OpProgram: array[0..13] of word = (chk_showtrayicon, chk_autostart, chk_minimizetotray,
	chk_startminimize, chk_closeminimize, chk_autoname, chk_confirmdelete, st_callwindowhotkey,
   chk_externalscroll, chk_rememberpagescroll, chk_alwaysfocuseditor, st_MailClient, st_recentnotescount, st_recyclerlimit);

procedure TOpProgram.OnOpen ();
begin
   RefreshItemText (self, c_OpProgram);

	FUdRecentNotesCount := TxlUpDown.Create (self, ItemHandle[ud_recentnotescount]);
   FUdRecentNotesCount.SetRange (5, 100);

   FCmbRecyclerLimit := TxlComboBox.Create (self, ItemHandle[cmb_recyclerlimit]);
	with FCmbRecyclerLimit do
   begin
   	AllowEdit := true;
   	Items.Add ('No Limit');
      Items.PopulateInt ([0, 5, 10, 15, 20, 30, 50, 80, 100, 150, 200]);
   end;

   FCallWinHotKey := TxlHotKey.create (self, ItemHandle[hk_callwindow]);
end;

procedure TOpProgram.OnClose ();
begin
   FUdRecentNotesCount.free;
   FCmbRecyclerLimit.free;
   FCallWinHotKey.free;
end;

procedure TOpProgram.Load (const op: TOptions);
begin
   ItemChecked[chk_autostart] := op.autostart;
   ItemChecked[chk_showtrayicon] := op.showtrayicon;
   ItemChecked[chk_minimizetotray] := op.minimizetotray;
   ItemChecked[chk_startminimize] := op.startminimize;
   ItemChecked[chk_closeminimize] := op.closeminimize;
   ItemChecked[chk_autoname] := op.autoname;
   ItemChecked[chk_confirmdelete] := op.confirmdelete;
   ItemChecked[chk_externalscroll] := op.ExternalScroll;
   ItemChecked[chk_rememberpagescroll] := op.RememberPageScroll;
   ItemChecked[chk_AlwaysFocusEditor] := op.AlwaysFocusEditor;

   FUdRecentNotesCount.Position := op.RecentNotesCount;

   if op.RecyclerLimit >= 0 then
      FCmbRecyclerLimit.Text := IntToStr(op.RecyclerLimit)
   else
      FCmbRecyclerLimit.Text := 'No Limit';

   FCallWinHotKey.hotkey := op.CallWinHotKey;

   ItemText[sle_mailClient] := op.MailClient;
end;

procedure TOpProgram.Save (var op: TOptions);
begin
   with op do
   begin
      autostart := ItemChecked[chk_autostart];
      showtrayicon := ItemChecked[chk_showtrayicon];
      minimizetotray := ItemChecked[chk_minimizetotray];
      startminimize := ItemChecked[chk_startminimize];
      closeminimize := ItemChecked[chk_closeminimize];
   	autoname := ItemChecked[chk_autoname];
   	confirmdelete := ItemChecked[chk_confirmdelete];
      ExternalScroll := ItemChecked[chk_ExternalScroll];
      RememberPageScroll := ItemChecked[chk_RememberPageScroll];
      AlwaysFocusEditor := ItemChecked[chk_AlwaysFocusEditor];
      RecentNotesCount := ConfineRange (FUdRecentNotesCount.Position, 5, 100);
      RecyclerLimit := StrToIntDef (FCmbRecyclerLimit.Text, -1);
      callwinhotkey := FCallWinHotKey.HotKey;
      MailClient := ItemText [sle_MailClient];
   end;
end;

procedure TOpProgram.OnCommand (ctrlID: integer);
begin
	if ctrlID <> cb_browse then exit;
   with TxlOpenDialog.create do
   begin
      Title := LangMan.GetItem (sr_BrowseMailClient);
      Filter := LangMan.GetItem (sr_exefilter);
      FilterIndex := 1;
      FileName := ItemText[sle_MailClient];
      MultiSelect := false;
      if Execute then
         ItemText[sle_MailClient] := Path + FileName;
      free;
   end
end;

//-----------------------------

procedure TOpEdit.OnInitialize ();
begin
	SetTemplate (ob_edit);
end;

const c_OpEdit: array[0..8] of word = (chk_autoemptyline, chk_autoindent, chk_oneclickopenlink, chk_confirmclear, st_tabstops, st_undolimit, chk_smoothscroll,
	st_ul1, st_ul2);

procedure TOpEdit.OnOpen ();
var i: integer;
begin
	RefreshItemText (self, c_OpEdit);

   FTabStops := TxlComboBox.create (self, ItemHandle[cmb_tabstops]);
   for i := 2 to 8 do
      FTabStops.Items.Add (i);
end;

procedure TOpEdit.OnClose ();
begin
   Ftabstops.free;
end;

procedure TOpEdit.Load (const op: TOptions);
begin
   ItemChecked[chk_autoemptyline] := op.autoemptyline;
   ItemChecked[chk_autoindent] := op.autoindent;
   ItemChecked[chk_smoothscroll] := op.SmoothScroll;
   ItemChecked[chk_oneclickopenlink] := op.OneClickOpenLink;
   ItemChecked[chk_confirmclear] := op.confirmclear;
   ItemText[sle_undolimit] := IntToStr (op.undolimit);
   ItemText[sle_ul1] := op.Ul1;
   ItemText[sle_ul2] := op.UL2;

   FTabStops.text := IntToStr (op.tabstops);
end;

procedure TOpEdit.Save (var op: TOptions);
begin
	with op do
   begin
   	Autoemptyline := ItemChecked[chk_autoemptyline];
   	Autoindent := ItemChecked[chk_autoindent];
      SmoothScroll := ItemChecked[chk_smoothscroll];
      OneClickOpenLink := ItemChecked[chk_oneclickopenlink];
   	Confirmclear := ItemChecked[chk_confirmclear];
   	Tabstops := StrToIntDef (Ftabstops.text, 3);
   	Undolimit := ConfineRange(StrToIntDef (ItemText[sle_undolimit], Undolimit), 1, 32767);
      UL1 := ItemText[sle_ul1];
      UL2 := ItemText[sle_ul2]; 
   end;
end;

//-----------------------------

procedure TOpNotes.OnInitialize ();
begin
	SetTemplate (ob_notes);
end;

const c_OpNotes: array[0..6] of word = (st_newnote_foreground_hotkey, st_newnote_background_hotkey, st_snaptext_hotkey,
   st_newnotebg_snaptext_hotkey, st_snaptexttofile_hotkey, st_closenotehotkey, st_autorecordhotkey);

procedure TOpNotes.OnOpen ();
begin
	RefreshItemText (self, c_OpNotes);

   FNewNote_ForeGround_HotKey := TxlHotKey.create (self, ItemHandle[hk_newnote_foreground]);
   FNewNote_BackGround_Hotkey := TxlHotKey.create (self, ItemHandle[hk_newnote_background]);
   FCloseNote_Hotkey := TxlHotKey.create (self, ItemHandle[hk_closenote]);
   FSnapText_Hotkey := TxlHotKey.create (self, ItemHandle[hk_snaptext]);
   FNewNoteBG_SnapText_Hotkey := TxlHotKey.create (self, ItemHandle[hk_newnotebg_snaptext]);
   FSnapTextToFile_Hotkey := TxlHotKey.create (self, ItemHandle[hk_snaptexttofile]);
   FAutoRecord_Hotkey := TxlHotKey.create (self, ItemHandle[hk_autorecord]);
end;

procedure TOpNotes.OnClose ();
begin
   FNewNote_ForeGround_HotKey.free;
   FNewNote_BackGround_Hotkey.free;
   FCloseNote_Hotkey.free;
   FSnapText_Hotkey.free;
   FNewNoteBG_SnapText_Hotkey.free;
   FSnapTextToFile_Hotkey.free;
   FAutoRecord_Hotkey.free;
end;

procedure TOpNotes.Load (const op: TOptions);
begin
   FNewNote_ForeGround_HotKey.hotkey := op.NewNoteForeGroundHotKey;
   FNewNote_BackGround_Hotkey.hotkey := op.NewNoteBackGroundHotkey;
   FCloseNote_Hotkey.hotkey := op.CloseNoteHotkey;
   FSnapText_Hotkey.hotkey := op.SnapTextHotkey;
   FNewNoteBG_SnapText_Hotkey.hotkey := op.NewNoteBG_SnapTextHotkey;
   FSnapTextToFile_Hotkey.hotkey := op.SnapTextToFileHotkey;
   FAutoRecord_Hotkey.hotkey := op.AutoRecordHotkey;
end;

procedure TOpNotes.Save (var op: TOptions);
begin
	with op do
   begin
   	NewNoteForeGroundHotKey := FNewNote_ForeGround_HotKey.Hotkey;
   	NewNoteBackGroundHotkey := FNewNote_BackGround_Hotkey.Hotkey;
      CloseNoteHotkey := FCloseNote_Hotkey.Hotkey;
   	SnapTextHotkey := FSnapText_Hotkey.Hotkey;
      NewNoteBG_SnapTextHotkey := FNewNoteBG_SnapText_Hotkey.Hotkey;
      SnapTextToFileHotkey := FSnapTextToFile_Hotkey.Hotkey;
      AutoRecordHotkey := FAutoRecord_Hotkey.Hotkey;
   end;
end;

//-----------------------------

procedure TOpImportExport.OnInitialize ();
begin
	SetTemplate (ob_import_export);
end;

const c_OpImportExport: array[0..17] of word = (rb_import_default, rb_import_showdialog, st_importtype, st_exportoption, st_importoption,
	rb_export_showdialog, rb_export_default, st_exporttype, st_separateline, sle_sepline, st_encode, chk_externalsaveWhenDragInFile,
   rb_fromfile, rb_fromfolder, rb_tofile, rb_toclipboard, rb_tofolder, chk_virtualimport);

procedure TOpImportExport.OnOpen ();
begin
	RefreshItemText (self, c_OpImportExport);
   FCmbEncode := TxlComboBox.Create (self, ItemHandle[cmb_Encode]);
end;

procedure TOpImportExport.OnClose ();
begin
   FCmbEncode.free;
end;

procedure TOpImportExport.Load (const op: TOptions);
begin
   ItemChecked[rb_import_showdialog] := op.ShowImportdialog;
   ItemChecked[rb_import_default] := not op.ShowImportdialog;
   f_CheckEnableImportOptions;

   ItemChecked[rb_fromfile] := (op.ImportType = itFromFile);
   ItemChecked[rb_fromfolder] := not ItemChecked[rb_fromfile];
   ItemChecked[chk_virtualimport] := op.VirtualImport;

   ItemChecked[rb_export_showdialog] := op.ShowExportDialog;
   ItemChecked[rb_export_default] := not op.ShowExportDialog;

   case op.ExportType of
      etToClipboard: ItemChecked[rb_toclipboard] := true;
      etToFile: ItemChecked[rb_tofile] := true;
      else ItemChecked[rb_tofolder] := true;
   end;

	f_SetEncode (FCmbEncode, op.ExportEncode);
   f_CheckEnableExportOptions;

   ItemText[sle_sepline] := op.SepLine;
   ItemChecked[chk_externalsaveWhenDragInFile] := op.ExternalSaveWhenDragInFile;
end;

procedure TOpImportExport.Save (var op: TOptions);
begin
	with op do
   begin
      ShowImportdialog := ItemChecked[rb_import_showdialog];
      if Itemchecked[rb_fromfile] then
         ImportType := itFromFile
      else
         ImportType := itFromFolder;
      VirtualImport := ItemChecked[chk_virtualimport];

      ShowExportDialog := ItemChecked[rb_export_showdialog];
      if Itemchecked[rb_tofile] then
         ExportType := etToFile
      else if ItemChecked[rb_toclipboard] then
         ExportType := etToClipboard
      else
         ExportType := etToFolder;
      ExportEncode := f_GetEncode (FCmbEncode);

      SepLine := ItemText[sle_sepline];
      ExternalSaveWhenDragInFile := ItemChecked[chk_externalsaveWhenDragInFile];
   end;
end;

procedure TOpImportExport.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	rb_import_showdialog, rb_import_default:
      	f_CheckEnableImportOptions;
      rb_export_showdialog, rb_export_default, rb_toclipboard, rb_tofile, rb_tofolder:
      	f_CheckEnableExportOptions;
   end;
end;

procedure TOpImportExport.f_CheckEnableImportOptions ();
var b: boolean;
begin
	b := ItemChecked[rb_import_default];
	ItemEnabled[rb_fromfile] := b;
   ItemEnabled[rb_fromfolder] := b;
   ItemEnabled[chk_virtualimport] := b;
end;

procedure TOpImportExport.f_CheckEnableExportOptions ();
var b: boolean;
begin
	b := ItemChecked[rb_export_default];
   ItemEnabled[rb_tofile] := b;
   ItemEnabled[rb_tofolder] := b;
   ItemEnabled[rb_toclipboard] := b;
   ItemEnabled[cmb_encode] := b and (not ItemChecked[rb_toclipboard]);
end;

//-----------------------------

procedure TOpBehavior.OnInitialize ();
begin
	SetTemplate (ob_behavior);
end;

procedure TOpBehavior.OnOpen ();
const c_OpBehavior: array[0..13] of word = (st_esckey, rb_escminimize, rb_escclear, st_pagedblclick, rb_rename1, rb_switchproperty1, rb_delete1,
	st_mbuttonclick, rb_rename2, rb_switchproperty2, rb_delete2, st_groupdblclick, rb_newpage, rb_levelup);
begin
	RefreshItemText (self, c_OpBehavior);
end;

procedure TOpBehavior.Load (const op: TOptions);
begin
   ItemChecked[rb_escminimize] := op.escminimize;
   ItemChecked[rb_escclear] := not op.escminimize;
	ItemChecked[rb_rename1] := (op.PageDblClick = pbRename);
   ItemChecked[rb_switchproperty1] := (op.PageDblClick = pbSwitchProperty);
   ItemChecked[rb_delete1] := (op.PageDblClick = pbDelete);
	ItemChecked[rb_rename2] := (op.PageMButtonClick = pbRename);
   ItemChecked[rb_switchproperty2] := (op.PageMButtonClick = pbSwitchProperty);
   ItemChecked[rb_delete2] := (op.PageMButtonClick = pbDelete);
   ItemChecked[rb_newpage] := (op.GroupDblClick = lbNewPage);
   Itemchecked[rb_levelup] := (op.GroupDblClick = lbLevelUp);
end;

procedure TOpBehavior.Save (var op: TOptions);
begin
	with op do
   begin
      escminimize := ItemChecked[rb_escminimize];
   	if Itemchecked[rb_rename1] then
      	PageDblClick := pbRename
      else if ItemChecked[rb_switchproperty1] then
      	PageDblClick := pbSwitchProperty
      else
      	PageDblClick := pbDelete;
      if ItemChecked[rb_rename2] then
      	PageMButtonClick := pbRename
      else if ItemChecked[rb_switchproperty2] then
      	PageMButtonClick := pbSwitchProperty
      else
      	PageMButtonClick := pbDelete;
      if ItemChecked[rb_newpage] then
      	GroupDblClick := lbNewPage
      else
      	GroupDblClick := lbLevelUp;
   end;
end;

//--------------------------------

procedure TOpLogIn.OnInitialize ();
begin
	SetTemplate (ob_login);
end;

const c_OpLogIn: array[0..4] of word = (chk_needlogin, st_password, st_confirmpassword, chk_locktrayicon, chk_encryptdatafile);

procedure TOpLogIn.OnOpen ();
begin
	RefreshItemText (self, c_OpLogIn);
   FLockTrayTime := TxlComboBox.Create (self, ItemHandle[cmb_locktraytime]);
   FLockTrayTime.AllowEdit := true;
   FLockTrayTime.Items.PopulateInt ([0, 1, 2, 3, 5, 8, 10, 15, 20, 30]);
end;

procedure TOpLogIn.OnClose ();
begin
   FLockTrayTime.free;
end;

procedure TOpLogIn.Load (const op: TOptions);
begin
   ItemChecked[chk_needlogin] := op.NeedLogIn;
   ItemText[sle_password] := op.Password;
   ItemText[sle_confirmpassword] := op.Password;
   ItemChecked[chk_encryptdatafile] := op.EncryptDataFile;

   ItemChecked[chk_locktrayicon] := op.LockTrayTime >= 0;
   if op.LockTrayTime >= 0 then
      FLockTrayTime.text := IntToStr(op.LockTrayTime)
   else
      FLockTrayTime.text := '5';

   f_CheckEnable ();
end;

procedure TOpLogIn.Save (var op: TOptions);
begin
   op.NeedLogIn := Itemchecked[chk_needlogin];
   op.Password := ItemText[sle_password];
   if ItemChecked[chk_locktrayicon] then
   	op.LockTrayTime := ConfineRange (StrToIntDef(FLockTrayTime.Text, op.LockTrayTime), 0, 120)
   else
   	op.LockTrayTime := -1;
   op.EncryptDataFile := Itemchecked[chk_needlogin] and ItemChecked[chk_encryptdatafile];
end;

procedure TOpLogIn.f_CheckEnable ();
var b: boolean;
begin
   b := ItemChecked[chk_needlogin];
   ItemEnabled[sle_password] := b;
   ItemEnabled[sle_confirmpassword] := b;
   ItemEnabled[chk_locktrayicon] := b;
   ItemEnabled[cmb_locktraytime] := b and ItemChecked[chk_locktrayicon];
   ItemEnabled[chk_encryptdatafile] := b;
end;

procedure TOpLogIn.OnCommand (ctrlID: integer);
begin
	case ctrlID of
      chk_needlogin:
      	begin
         	f_CheckEnable;
            if ItemChecked[chk_needlogin] then FocusControl (sle_password);
         end;
      chk_locktrayicon:
      	ItemEnabled[cmb_locktraytime] := ItemChecked[chk_locktrayicon];
   end;
end;

function TOpLogIn.Validate (): boolean;
begin
	if ItemText[sle_password] <> ItemText[sle_confirmpassword] then
   begin
   	ShowMessage (LangMan.GetItem(sr_passwordnotconfirmed, 'ÃÜÂë²»Æ¥Åä! Çë¼ì²é!'), mtInformation, LangMan.GetItem(sr_prompt));
      FocusControl (sle_password);
		result := false;
   end
   else
   	result := true;
end;

//-----------------------------

procedure TOpBackup.OnInitialize ();
begin
	SetTemplate (ob_backup);
end;

procedure TOpBackup.OnOpen ();
const c_OpBackup: array[0..3] of word = (chk_autosave, chk_autobackup, st_totalbackup, st_backuppath);
begin
	RefreshItemTExt (self, c_OpBackup);

   FAutoSaveTime := TxlComboBox.Create (self, ItemHandle[cmb_autosavetime]);
   FAutoSaveTime.AllowEdit := true;
   FAutoSaveTime.Items.PopulateInt ([1, 3, 5, 10, 20, 30, 60]);

	FUdBackupInterval := TxlUpDown.Create (self, ItemHandle[ud_backupinterval]);
   FUdBackupInterval.SetRange (1, 200);

   FBackupIntervalType := TxlComboBox.Create (self, ItemHandle[cmb_backupintervaltype]);
   FBackupIntervalType.AllowEdit := false;
   FBackupIntervalType.Items.Populate ([LangMan.GetItem(sr_saves), LangMan.GetItem(sr_minutes), LangMan.GetItem(sr_hours), LangMan.GetItem(sr_days)]);

   FTotalBackup := TxlComboBox.Create (self, ItemHandle[cmb_totalbackup]);
   FTotalBackup.AllowEdit := true;
   FTotalBackup.Items.PopulateInt([2, 3, 5, 8, 10, 15, 20, 30, 50]);
end;

procedure TOpBackup.OnClose ();
begin
   FAutoSaveTime.free;
   FUdBackupInterval.free;
   FBackupIntervalType.free;
   FTotalBackup.free;
end;

procedure TOpBackup.Load (const op: TOptions);
begin
   ItemChecked[chk_autosave] := (op.autosavetime > 0);
   if op.autosavetime > 0 then
      FAutoSaveTime.text := IntToStr(op.autosavetime)
   else
      FAutoSaveTime.text := '10';
   FAutoSaveTime.Enabled := ItemChecked[chk_autosave];

   FUdBackupInterval.Position := op.BackupInterval;
   FBackupIntervalType.Items.SelIndex := Ord(op.BackupIntervalType);
   FTotalBackup.Text := IntToStr(op.TotalBackup);

   ItemText[sle_backuppath] := op.backuppath;

   ItemChecked[chk_autobackup] := (op.BackupInterval > 0);
   OnCommand (chk_autobackup);
end;

procedure TOpBackup.Save (var op: TOptions);
begin
   if ItemChecked[chk_autosave] then
      op.autosavetime := ConfineRange (StrToIntDef(FAutoSaveTime.Text, op.autosavetime), 1, 120)
   else
      op.autosavetime := 0;

   if ItemChecked[chk_autobackup] then
      op.BackupInterval := ConfineRange (StrToIntDef (ItemTExt[sle_backupinterval], 10), 1, 200)
   else
      op.BackupInterval := 0;

   op.BackupIntervalType := TBackupIntervalType (FBackupIntervalType.Items.SelIndex);
   op.TotalBackup := ConfineRange (StrToIntDef(FTotalBackup.Text, op.TotalBackup), 1, 10000);

   if ItemText[sle_backuppath] <> '' then
   	op.backuppath := ItemText[sle_backuppath]
	else
      op.backuppath := FullToRelPath (ProgDir + 'backup\', ProgDir);
end;

procedure TOpBackup.OnCommand (ctrlID: integer);
var b: boolean;
begin
	case ctrlID of
      chk_autosave:
         ItemEnabled[cmb_autosavetime] := ItemChecked[chk_autosave];
      chk_autobackup:
         begin
            b := ItemChecked[chk_autobackup];
            ItemEnabled[sle_backupinterval] := b;
            ItemEnabled[ud_backupinterval] := b;
            ItemEnabled[cmb_backupintervaltype] := b;
            ItemEnabled[cmb_totalbackup] := b;
            ItemEnabled[sle_backuppath] := b;
            ItemEnabled[cb_browse] := b;
         end;
      cb_browse:
         with TxlPathDialog.Create do
         begin
            Title := LangMan.GetItem (sr_selectbackupfolder);
            Path := ItemText[sle_backuppath];
            if Execute then
               ItemText[sle_backuppath] := FullToRelPath (Path, ProgDir);
            free;
         end;
   end;
end;

//-----------------------------

procedure TOpSpecialMode.OnInitialize ();
begin
	SetTemplate (ob_specialmode);
end;

procedure TOpSpecialMode.OnOpen ();
const c_OpSpecialMode: array[0..6] of word = (st_direction, st_hidedelay, st_showdelay, st_animationtime, st_edgewidth, IDOK, IDCANCEL);
begin
	RefreshItemText (self, c_OpSpecialMode);

   FCmbDirection := TxlComboBox.Create (self, ItemHandle[cmb_direction]);
   with FCmbDirection.Items do
   begin
      Add (LangMan.GetItem (sr_free));
      Add (LangMan.GetItem (sr_left));
      Add (LangMan.GetItem (sr_top));
      Add (LangMan.GetItem (sr_right));
      Add (LangMan.GetItem (sr_bottom));
   end;

   FCmbHideDelay := TxlComboBox.Create (self, ItemHandle[cmb_hidedelay]);
   FCmbHideDelay.AllowEdit := true;
   FCmbHideDelay.Items.PopulateInt ([0, 500, 1000, 1500, 2000, 3000, 5000, 10000, 30000]);

   FCmbShowDelay := TxlComboBox.Create (self, ItemHandle[cmb_showdelay]);
   FCmbShowDelay.AllowEdit := true;
   FCmbShowDelay.Items.PopulateInt ([0, 200, 300, 500, 1000, 1500, 2000, 3000, 5000]);

   FCmbAnimationTime := TxlComboBox.Create (self, ItemHandle[cmb_Animationtime]);
   FCmbAnimationTime.AllowEdit := true;
   FCmbAnimationTime.Items.PopulateInt ([0, 50, 100, 200, 300, 500, 1000]);

   FCmbEdgeWidth := TxlComboBox.create(self, ItemHandle[cmb_edgewidth]);
   FCmbEdgeWidth.AllowEdit := true;
   FCmbEdgeWidth.Items.PopulateInt([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
end;

procedure TOpSpecialMode.OnClose ();
begin
   FCmbDirection.free;
   FCmbHideDelay.free;
   FCmbShowDelay.free;
   FCmbAnimationTime.free;
   FCmbEdgeWidth.free;
end;

procedure TOpSpecialMode.Load (const op: TOptions);
begin
   FCmbDirection.Items.Select (Ord(op.SMOptions.Direction));
   FCmbhidedelay.Text := IntToStr(op.SMOptions.HideDelay);
   FCmbshowdelay.Text := IntToStr(op.SMOptions.ShowDelay);
   Fcmbanimationtime.Text := IntToStr(op.SMOptions.AnimationTime);
   FCmbedgewidth.Text := IntToStr(op.SMOptions.EdgeWidth);
end;

procedure TOpSpecialMode.Save (var op: TOptions);
begin
   with op.SMOptions do
   begin
      Direction := TSMDirection(FCmbDirection.Items.SelIndex);
      HideDelay := ConfineRange(StrToIntDef(FCmbhidedelay.Text, 1500), 0, 300000);
      ShowDelay := ConfineRange(StrtoIntDef(FCmbshowdelay.Text, 300), 0, 10000);
      AnimationTime := Confinerange(StrtoIntDef(FCmbAnimationtime.Text, 200), 0, 2000);
      EdgeWidth := ConfineRange(StrToIntDef (FCmbedgewidth.TExt, 5), 1, 20);
   end;
end;

end.
