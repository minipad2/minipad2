unit UOptionManager;

interface

uses Windows, UxlClasses, UxlWinClasses, UxlExtClasses, UxlFunctions, UxlList, UxlIniFile, UxlWinControl, UxlWindow, UxlStrUtils,
	UxlFile, UxlMiscCtrls, UxlDialog, UTypeDef, USearchPage, UxlRichEdit, UCalculator, UDictionary, UMemoPage, ULinkPage;

type
	TOptions = record
      AutoStart: boolean;
      ShowTrayIcon: boolean;
      MinimizeToTray: boolean;
      StartMinimize: boolean;
      CloseMinimize: boolean;
      AutoName: boolean;
      ConfirmDelete: boolean;
      ExternalScroll: boolean;
      RememberPageScroll: boolean;
      AlwaysFocusEditor: boolean;
      RecentNotesCount: integer;
      RecyclerLimit: integer;
      CallWinHotkey: THotKey;
      MailClient: widestring;
      StartPage: integer;

      AutoEmptyLine: boolean;
      AutoIndent: boolean;
      SmoothScroll: boolean;
      OneClickOpenLink: boolean;
      ConfirmClear: boolean;
      TabStops: integer;
      UndoLimit: integer;
      UL1: widestring;
      UL2: widestring;
      BmkRule: integer;

      NewNoteForeGroundHotKey: THotkey;
      NewNoteBackGroundHotkey: THotkey;
      CloseNoteHotkey: THotkey;
      SnapTextHotkey: THotkey;
      NewNoteBG_SnapTextHotKey: THotKey;
      SnapTextToFileHotkey: THotkey;
      AutoRecordHotkey: THotkey;
      
      ShowImportDialog: boolean;
      ImportType: TImportType;
      VirtualImport: boolean;
      ShowExportDialog: boolean;
      ExportType: TExportType;
      ExportEncode: TEncode;
      SepLine: widestring;
      ExternalSaveWhenDragInFile: boolean;

      EscMinimize: boolean;
      PageDblClick: TPageBehavior;
      PageMButtonClick: TPageBehavior;
      GroupDblClick: TGroupBehavior;

      NeedLogIn: boolean;
      Password: widestring;
      LockTrayTime: integer;
		EncryptDataFile: boolean;
      
      AutoSaveTime: integer;
		BackupInterval: integer;
      BackupIntervalType: TBackupIntervalType;
      TotalBackup: integer;
      BackupPath: widestring;

      SMOptions: TSMOptions;

      Language: widestring;
      ToolWindowStyle: boolean;
      CaptionPageName: boolean;
      ShowToolbar: boolean;
		ShowMenubar: boolean;
      Transparency: TTransparency;
      AutoAdjust: boolean;
      AutoAdjustLevel: integer;
      SplitterWidth: integer;

      TreeFont: TxlFont;
      TreeColor: TColor;
      TreeHorzScroll: boolean;
      NodeButtons: boolean;
      NodeLines: boolean;
      LinesAtRoot: boolean;
      ShowNodeImages: boolean;
      TreeOnRight: boolean;
      
      TabFont: TxlFont;
      TabColor: TColor;
      TabOnBottom: boolean;
      MultiLineTab: boolean;
      TabWidth: integer;
      ShowTabImages: boolean;
      HighlightCurrentTab: boolean;

      ListFont: TxlFont;
      ListColor: TColor;
      BlogFont: TxlFont;
      BlogColor: TColor;
      BlogTitleFont: TxlFont;
      BlogTitleColor: TColor;
      BlogSelTitleFont: TxlFont;
      BlogSelTitleColor: TColor;
      MininoteFont: TxlFont;
      MininoteColor: TColor;

      EditorFont: TxlFont;
      EditorColor: TColor;
      Margins: integer;
      ShowLineNumber: boolean;
      LineNumberColor: TColor;
      HighLightSelLine: boolean;
      SelLineColor: TColor;

      HLColor: array [0..4] of TColor;
      HLUnderline: array [0..4] of boolean;

      EnableCalcpage: boolean;
      EnableDictpage: boolean;
      EnableMemopage: boolean;
      EnableLinkpage: boolean;
      EnableContactpage: boolean;
      EnableTemplate: boolean;
      EnableClipboard: boolean;
      EnableAdveditor: boolean;

      CalcOptions: TCalcOptions;
      UserFuncs: TxlStrList;

      DefRemindTime: integer;

      DictOptions: TDictOptions;
      TipQueryHotKey: THotKey;
      TipQueryColor: TColor;

      LinkOptions: TLinkOptions;
   	FastLinkHotkey: THotKey;
      EnableFastLinkItemHotkey: boolean;

      ClipOptions: TClipOptions;
      clipboardhotkey: THotKey;
      EnableClipItemHotkey: boolean;

      TemplateHotkey: THotKey;
      EnableTemplItemHotkey: boolean;
      CaptureHotKey: THotKey;
      PopupMenuNoFocus: boolean;
   end;

   TOptionManager = class (TxlOption)
   private
   	FOptions: TOptions;
      procedure ChangeOptions (const value: TOptions);
      procedure f_OnApplyOption (Sender: TObject);
   protected
   	procedure OnCreate (); override;
      procedure OnDestroy (); override;
   	procedure DoLoad (); override;
      procedure DoSave (); override;
      function DoSetOptions (WndParent: TxlWinControl): boolean; override;
   public
      function HaveExtPage (): boolean;
      property Options: TOptions read FOptions write ChangeOptions;
   end;

   TMemoryManager = class (TxlMemory)
   protected
   	procedure OnCreate (); override;
      procedure OnDestroy (); override;
   	procedure DoLoad (); override;
      procedure DoSave (); override;
   public
      WinPos: TPos;
		WinStatus: TWindowStatus;
      NotePos: TPos;

      ShowTree: boolean;
      StayOnTop: boolean;
      Transparent: boolean;
      SpecialMode: boolean;
      WordWrap: boolean;
      WatchClipboard: boolean;
      OptionPage: integer;

      TBList: widestring;
      TreeWidth: integer;
      TreeHide: boolean;
      PageId: integer;
      SaveCount: integer;
      LastBackupTime: Int64;
      LastBackupSaveCount: integer;

      SearchCrit: TSearchCrit;
      FindText: widestring;
      ReplaceText: widestring;
      AllowReplace: boolean;
      DirUp: boolean;
      MatchCase: boolean;
      WholeWord: boolean;
      RollBack: boolean;
      FindHistory: TxlStrList;
      ReplaceHistory: TxlStrList;
      ExitAfterFirstMatch: boolean;
      HighlightMatch: boolean;

      MemoTimeMode: TTimeMode;
      MemoAction: TAction;
      MemoUseSound: boolean;
      MemoSoundFile: widestring;
      LinkType: TLinkType;
      InsertLinkType: TLinkType;

      IconFilePath: widestring;
	end;

function OptionMan(): TOptionManager;
function MemoryMan(): TMemoryManager;

implementation

uses CommCtrl, Resource, UOptionBox, UxlMath, UGlobalObj, ULangManager, UCommonClasses;

var FOptionMan: TOptionManager;
	FMemoryMan: TMemoryManager;

function OptionMan(): TOptionManager;
begin
	if FOptionMan = nil then
   	FOptionMan := TOptionManager.Create;
   result := FOptionMan;
end;

function MemoryMan(): TMemoryManager;
begin
	if FMemoryMan = nil then
   	FMemoryMan := TMemoryManager.Create;
   result := FMemoryMan;
end;

//---------------------

procedure TOptionManager.OnCreate ();
begin
   with FOptions do
   begin
		UserFuncs := TxlStrList.Create;
      tabfont := TxlFont.Create;
      treefont := TxlFont.Create;
      editorfont := TxlFont.Create;
      listfont := TxlFont.Create;
      blogfont := TxlFont.Create;
      blogtitlefont := TxlFont.Create;
      blogseltitlefont := TxlFont.Create;
      mininotefont := TxlFont.create;
   end;
end;

procedure TOptionManager.OnDestroy ();
begin
	with FOptions do
   begin
   	UserFuncs.Free;
		tabfont.free;
      treefont.free;
      editorfont.free;
      listfont.free;
      blogfont.free;
      blogtitlefont.free;
      blogseltitlefont.free;
      mininotefont.free;
   end;
end;

procedure TOptionManager.ChangeOptions (const value: TOptions);
begin
	FOptions := value;
   DoSave;
end;

procedure TOptionManager.DoLoad ();
   procedure f_ReadFont (o_inifile: TxlIniFile; const s_seg: widestring; var o_font: TxlFont; var bgcolor: TColor; b_editor: boolean = false);
   begin
   	o_inifile.Section := s_seg;
      with o_font do
      begin
			Name := o_inifile.ReadString ('FontName', IfThen (b_editor, 'Tahoma', 'Arial'));
         color := o_inifile.ReadInteger('FontColor', RGB(0,0,0));
         Size := o_inifile.ReadInteger('FontSize', IfThen (b_editor, 10, 9));
         Bold := o_inifile.ReadBool ('FontBold', false);
         Italic := o_inifile.readbool ('FontItalic', false);
         Underline := o_inifile.readbool ('FontUnderline', false);
         StrikeOut := o_inifile.ReadBool ('FontStrikeOut', false);
      end;
      bgcolor := o_inifile.readinteger ('BGColor', RGB(255, 255, 255));
   end;
var o_inifile: TIniFileEx;
	b_upgraded: boolean;
   i: integer;
begin
	o_inifile := TIniFileEx.Create (ProgIni);
   with FOptions do
   begin
   	o_inifile.Section := 'Program';
      StartPage := o_inifile.ReadInteger ('StartPage', -1);
      minimizetotray := o_inifile.readbool ('MinimizeToTray', true);
      showtrayicon := o_inifile.ReadBool('ShowTrayIcon', true);
      startminimize := o_inifile.readBool ('StartMinimize', false);
      closeminimize := o_inifile.readbool ('CloseMinimize', false);
      callwinhotkey := o_inifile.readinteger ('CallWinHotKey', 0);  //MakeWord(Ord('E'), HOTKEYF_CONTROL));
      autoname := o_inifile.readbool ('AutoName', false);
      confirmdelete := o_inifile.readbool ('ConfirmDelete', true);
      ExternalScroll := o_inifile.ReadBool('ExternalScroll', true);
      RecentNotesCount := o_inifile.ReadInteger ('RecentNotesCount', 20);
      RecyclerLimit := o_inifile.ReadInteger ('RecyclerLimit', 100);
      RememberPageScroll := o_inifile.ReadBool ('RememberPageScroll', true);
      AlwaysFocusEditor := o_inifile.ReadBool('AlwaysFocusEditor', false);
      MailClient := o_inifile.ReadString ('MailClient', '');
      b_upgraded := o_inifile.ReadString ('Version', '') = 'Upgraded';

      o_inifile.Section := 'Edit';
      autoemptyline := o_inifile.ReadBool('AutoEmptyLine', false);
      autoindent := o_inifile.readbool ('AutoIndent', true);
      SmoothScroll := o_inifile.ReadBool ('SmoothScroll', false);
      OneClickOpenLink := o_inifile.ReadBool ('OneClickOpenLink', false);
      confirmclear := o_inifile.readbool ('ConfirmClear', true);
      tabstops := o_inifile.readinteger ('TabStops', 4);
      undolimit := o_inifile.readinteger ('UndoLimit', 100);
      UL1 := o_inifile.ReadString ('UL1', '* ');
      UL2 := o_inifile.ReadString ('UL2', '> ');
      BmkRule := o_inifile.ReadInteger ('BmkRule', 0);

      o_inifile.Section := 'Notes';
      NewNoteForeGroundHotKey := o_inifile.ReadInteger ('NewNoteForeGroundHotKey', 0);
      NewNoteBackGroundHotkey := o_inifile.ReadInteger ('NewNoteBackGroundHotkey', 0);
      CloseNoteHotkey := o_inifile.ReadInteger ('CloseNoteHotkey', 0);
      SnapTextHotkey := o_inifile.ReadInteger ('SnapTextHotkey', 0);
      NewNoteBG_SnapTextHotKey := o_inifile.ReadInteger ('NewNoteBG_SnapTextHotKey', 0);
      SnapTextToFileHotkey := o_inifile.ReadInteger ('SnapTextToFileHotkey', 0);
      AutoRecordHotkey := o_inifile.ReadInteger ('AutoRecordHotkey', 0);

      o_inifile.Section := 'ImportExport';
      ShowImportDialog := o_inifile.ReadBool ('ShowImportDialog', true);
      ImportType := TImportType (o_inifile.ReadInteger ('ImportType', Ord(itFromFile)));
      VirtualImport := o_inifile.ReadBool ('VirtualImport', false);
      ShowExportDialog := o_inifile.ReadBool ('ShowExportDialog', true);
      ExportType := TExportType (o_inifile.ReadInteger ('ExportType', Ord(etToFolder)));
      ExportEncode := TEncode (o_inifile.ReadInteger ('ExportEncode', Ord(enUTF16LE)));
      SepLine := o_inifile.readstring ('SepLine', '%n%s(12)%n');
      ExternalSaveWhenDragInFile := o_inifile.ReadBool ('ExternalSaveWhenDragInFile', false);

      o_inifile.Section := 'Behavior';
      EscMinimize := o_inifile.readbool ('EscMinimize', true);
     	PageDblClick := TPageBehavior(o_inifile.readInteger ('PageDblClick', Ord(pbDelete)));
      PageMButtonClick := TPageBehavior(o_inifile.readInteger ('PageMButtonClick', Ord(pbSwitchProperty)));
      GroupDblClick := TGroupBehavior(o_inifile.readInteger ('GroupDblClick', Ord(lbNewPage)));

		o_inifile.Section := 'LogIn';
      NeedLogIn := o_inifile.ReadBool('NeedLogIn', false);
      Password := DecryptString(o_inifile.ReadString('Password', ''));
      LockTrayTime := o_inifile.ReadInteger ('LockTrayTime', -1);
		EncryptDataFile := o_inifile.ReadBool ('EncryptDataFile', false);

      o_inifile.Section := 'Backup';
      AutoSaveTime := o_inifile.readinteger ('AutoSaveTime', 10);
      BackupInterval := o_inifile.ReadInteger ('BackupInterval', 10);
      BackupIntervalType := TBackupIntervalType (o_inifile.ReadInteger ('BackupIntervalType', Ord(btSaveCount)));
      TotalBackup := o_inifile.ReadInteger ('TotalBackup', 5);
      BackupPath := o_inifile.ReadString('BackupPath', FullToRelPath (ProgDir + 'backup\', ProgDir));

      o_inifile.Section := 'AutoHide';
      smoptions.direction := TSMDirection (o_inifile.readinteger ('Direction', Ord(hdFree)));
      smoptions.hidedelay := o_inifile.readinteger ('HideDelay', 1500);
      smoptions.showdelay := o_inifile.readinteger ('ShowDelay', 300);
      smoptions.animationtime := o_inifile.readinteger ('AnimationTime', 200);
      smoptions.EdgeWidth := o_inifile.ReadInteger ('EdgeWidth', 5);

      o_inifile.Section := 'Appearance';
      language := o_inifile.readstring ('Language', '');
      showtoolbar := o_inifile.readbool ('ShowToolBar', true);
      showmenubar := o_inifile.readbool ('ShowMenuBar', true);
      ToolWindowStyle := o_inifile.readbool ('ToolWindowStyle', false);
      captionpagename := o_inifile.readbool ('CaptionPageName', true);
      Transparency := o_inifile.readinteger ('Transparency', 5);
      AutoAdjust := o_inifile.readbool ('AutoAdjust', true);
      AutoAdjustLevel := o_inifile.readinteger ('AutoAdjustLevel', 3);
      SplitterWidth := ConfineRange (o_inifile.ReadInteger ('SplitterWidth', 4), 1, 10);

      o_inifile.Section := 'TreeView';
      TreeHorzScroll := o_inifile.ReadBool('TreeHorzScroll', true);
      LinesAtRoot := o_inifile.ReadBool('LinesAtRoot', true);
      NodeButtons := o_inifile.ReadBool ('NodeButtons', true);
      NodeLines := o_inifile.ReadBool ('NodeLines', true);
      ShowNodeImages := o_inifile.ReadBool('ShowNodeImages', true);
      TreeOnRight := o_inifile.ReadBool ('TreeOnRight', false);
      
      f_ReadFont (o_inifile, 'TreeFontColor', treefont, treecolor);

      o_inifile.Section := 'TabControl';
      multilinetab := o_inifile.readbool ('MultiLineTab', true);
      tabwidth := o_inifile.readinteger ('TabWidth', 0);
      TabOnBottom := o_inifile.readbool ('TabOnBottom', false);
      ShowTabImages := o_inifile.ReadBool('ShowTabImages', false);
      HighlightCurrentTab := o_inifile.ReadBool ('HighlightCurrentTab', false);

      f_ReadFont (o_inifile, 'TabFontColor', tabfont, tabcolor);

      f_ReadFont (o_inifile, 'ListFontColor', listfont, listcolor);

   	o_inifile.Section := 'AbstractFontColor';
      with blogfont do
      begin
			Name := o_inifile.ReadString ('FontName', 'Tahoma');
         color := o_inifile.ReadInteger('FontColor', RGB(0,0,0));
         Size := o_inifile.ReadInteger('FontSize', 10);
         Bold := o_inifile.ReadBool ('FontBold', false);
         Italic := o_inifile.readbool ('FontItalic', false);
         Underline := o_inifile.readbool ('FontUnderline', false);
         StrikeOut := o_inifile.ReadBool ('FontStrikeOut', false);
      end;
      blogcolor := o_inifile.readinteger ('BGColor', RGB(255, 255, 255));

   	o_inifile.Section := 'AbstractTitleFontColor';
      with blogtitlefont do
      begin
			Name := o_inifile.ReadString ('FontName', 'Tahoma');
         color := o_inifile.ReadInteger('FontColor', RGB(0,0,0));
         Size := o_inifile.ReadInteger('FontSize', 10);
         Bold := o_inifile.ReadBool ('FontBold', true);
         Italic := o_inifile.readbool ('FontItalic', false);
         Underline := o_inifile.readbool ('FontUnderline', false);
         StrikeOut := o_inifile.ReadBool ('FontStrikeOut', false);
      end;
      blogtitlecolor := o_inifile.readinteger ('BGColor', RGB (165,219,181));

      o_inifile.Section := 'AbstractSelTitleFontColor';
      with blogseltitlefont do
      begin
			Name := o_inifile.ReadString ('FontName', 'Tahoma');
         color := o_inifile.ReadInteger('FontColor', RGB(0,0,0));
         Size := o_inifile.ReadInteger('FontSize', 10);
         Bold := o_inifile.ReadBool ('FontBold', true);
         Italic := o_inifile.readbool ('FontItalic', false);
         Underline := o_inifile.readbool ('FontUnderline', false);
         StrikeOut := o_inifile.ReadBool ('FontStrikeOut', false);
      end;
      blogseltitlecolor := o_inifile.readinteger ('BGColor', RGB (255, 190, 173));

      o_inifile.Section := 'Editor';
      margins := o_inifile.readinteger ('Margins', 2);
      showlinenumber := o_inifile.readbool ('ShowLineNumber', false);
      linenumbercolor := o_inifile.readinteger ('LineNumberColor', RGB(198,195,198));
      HighLightSelLine := o_inifile.ReadBool('HighlightSelLine', false);
      SelLineColor := o_inifile.ReadInteger('SelLineColor', 8454143);  //13172690);

      HLColor[0] := o_inifile.ReadInteger ('HLColor0', RGB (100, 200, 100));
      HLColor[1] := o_inifile.ReadInteger ('HLColor1', RGB(255,50,50));
      HLColor[2] := o_inifile.ReadInteger ('HLColor2', RGB(80, 60, 255));
      HLColor[3] := o_inifile.ReadInteger ('HLColor3', RGB(255, 128, 255));
      HLColor[4] := o_inifile.ReadInteger ('HLColor4', RGB(255, 255, 20));
      for i := 0 to 4 do
      	HLUnderline[i] := o_inifile.ReadBool ('HLUnderline' + IntToStr(i), (i in [1, 2]));
      f_ReadFont (o_inifile, 'EditorFontColor', editorfont, editorcolor, true);

      o_inifile.Section := 'OtherControls';
      f_ReadFont (o_inifile, 'MininoteFontColor', mininotefont, mininotecolor, true);

      o_inifile.Section := 'ExtFuncs';
      Enablecalcpage := o_inifile.readbool ('EnableCalcPage', true);
      Enablememopage := o_inifile.readbool ('EnableMemoPage', true);
      Enabledictpage := o_inifile.readbool ('EnableDictPage', true);
      Enablelinkpage := o_inifile.readbool ('EnableLinkPage', true);
//      EnableFastLink := o_inifile.Readbool ('EnableFastLink', true);
      Enablecontactpage := o_inifile.readbool ('EnableContactPage', true);
      Enabletemplate := o_inifile.readbool ('EnableTemplate', true);
      Enableclipboard := o_inifile.readbool ('EnableClipboard', true);
      Enableadveditor := o_inifile.readbool ('EnableAdvEditor', false);

      o_inifile.Section := 'CalcPage';
      CalcOptions.decimal := o_inifile.readinteger ('Decimal', 6);
      CalcOptions.tri_degree := o_inifile.readbool ('Tri_Degree', false);

      o_inifile.Section := 'MemoPage';
      DefRemindTime := o_inifile.ReadInteger ('DefRemindTime', 10);

      o_inifile.Section := 'DictPage';
      DictOptions.SelDictList := o_inifile.ReadString ('SelDictList', '');
      DictOptions.AllDictList := o_inifile.ReadString ('AllDictList', '');
      DictOptions.MultiDict := o_inifile.ReadBool ('MultiDict', true);
      DictOptions.DebugMode := o_inifile.ReadBool ('DebugMode', false);
      TipQueryHotKey := o_inifile.ReadInteger ('TipQueryHotKey', 0);
      TipQueryColor := o_inifile.ReadInteger ('TipQueryColor', RGB(255,255,231));

      o_inifile.Section := 'LinkPage';
      LinkOptions.disableIconRead := o_inifile.ReadBool('DisableIconRead', false);
      LinkOptions.AutoMinimizeAfterOpenLink := o_inifile.ReadBool ('AutoMinimizeAfterOpenLink', false);
      fastlinkhotkey := o_inifile.readinteger('FastLinkHotKey', 0);  //MakeWord(Ord('D'), HOTKEYF_SHIFT or HOTKEYF_CONTROL));
      enablefastlinkitemhotkey := o_inifile.readbool ('EnableFastLinkItemHotKey', false);

      o_inifile.Section := 'Clipboard';
      with clipoptions do
      begin
         maxclipnum := o_inifile.readinteger ('MaxClipNum', 30);
         maxitembyte := o_inifile.readinteger ('MaxItemByte', 100000);
         MenuWidth := o_inifile.ReadInteger('MenuWidth', 30);
         FilterType := TClipFilterType(o_inifile.readInteger ('FilterType', Ord(cftFilterNeighboring)));
         NewPasteToTop := o_inifile.ReadBool('NewPasteToTop', false);
      end;
      clipboardhotkey := o_inifile.readinteger ('ClipboardHotKey', 0);  //MakeWord(Ord('D'), HOTKEYF_ALT));
      enableclipitemhotkey := o_inifile.readbool ('EnableClipItemHotKey', false);

      o_inifile.Section := 'Template';
      templatehotkey := o_inifile.readinteger ('TemplateHotKey', 0);  //makeWord(Ord('D'), HOTKEYF_CONTROL));
      enabletemplitemhotkey := o_inifile.readbool ('EnableTemplItemHotKey', false);
      CaptureHotKey := o_inifile.ReadInteger ('CaptureHotKey', 0);
      PopupMenuNoFocus := o_inifile.ReadBool('PopupMenuNoFocus', false);
   end;

   o_inifile.ReadItemList ('UserFunctions', FOptions.UserFuncs);
	o_inifile.free;

   if b_upgraded then     // 清空从旧版本带来的冗余键值
   begin
   	DeleteFile (ProgIni);
      DoSave;
   end;
end;

procedure TOptionManager.DoSave ();
   procedure f_WriteFontColor (o_inifile: TxlIniFile; const s_seg: widestring; o_font: TxlFont; bgcolor: TColor);
   begin
      with o_inifile do
      begin
      	Section := s_seg;
         writestring ('FontName', o_font.name);
         writeinteger ('FontColor', o_font.color);
         writeinteger ('FontSize', o_font.size);
         writebool ('FontBold', o_font.bold);
         writebool ('FontItalic', o_font.italic);
         writebool ('FontUnderline', o_font.Underline );
         writebool ('FontStrikeOut', o_font.strikeout);
         writeinteger ('BGColor', bgcolor);
      end;
   end;
var o_inifile: TIniFileEx;
	i: integer;
begin
	o_inifile := TIniFileEx.Create(ProgIni);
   with o_inifile do
   begin
   	Section := 'Program';
      WriteString ('Version', Version);
      writebool ('ShowTrayIcon', FOptions.showtrayicon );
      writebool ('MinimizeToTray', FOptions.minimizetotray);
      writebool ('StartMinimize', FOptions.startminimize);
      writebool ('CloseMinimize', FOptions.closeminimize);
      writebool ('AutoName', FOptions.autoname);
      writebool ('ConfirmDelete', FOptions.confirmdelete);
      WriteBool ('ExternalScroll', FOptions.ExternalScroll);
      WriteBool ('RememberPageScroll', FOptions.RememberPageScroll);
      WriteBool ('AlwaysFocusEditor', FOptions.AlwaysFocusEditor);
      WriteInteger ('RecentNotesCount', FOptions.RecentNotesCount);
      WriteInteger ('RecyclerLimit', FOptions.RecyclerLimit);
      writeinteger ('CallWinHotKey', FOptions.callwinhotkey);
      WriteString ('MailClient', FOptions.MailClient);
      WriteInteger ('StartPage', FOptions.StartPage);

      Section := 'Edit';
      writebool ('AutoEmptyLine', FOptions.autoemptyline);
      writebool ('AutoIndent', FOptions.autoindent);
      WriteBool ('SmoothScroll', FOptions.SmoothScroll);
      WriteBool ('OneClickOpenLink', FOptions.OneClickOpenLink);
      writebool ('ConfirmClear', FOptions.confirmclear);
      writeinteger ('TabStops', FOptions.tabstops);
      writeinteger ('UndoLimit', FOptions.undolimit);
      WriteString ('UL1', FOptions.UL1);
      WriteString ('UL2', FOptions.UL2);

      Section := 'Notes';
      WriteInteger ('NewNoteForeGroundHotKey', FOptions.NewNoteForeGroundHotKey);
      WriteInteger ('NewNoteBackGroundHotkey', FOptions.NewNoteBackGroundHotkey);
      WriteInteger ('CloseNoteHotkey', FOptions.CloseNoteHotkey);
      WriteInteger ('SnapTextHotkey', FOptions.SnapTextHotkey);
      WriteInteger ('NewNoteBG_SnapTextHotKey', FOptions.NewNoteBG_SnapTextHotKey);
      WriteInteger ('SnapTextToFileHotKey', FOptions.SnapTextToFileHotKey);
      WriteInteger ('AutoRecordHotkey', FOptions.AutoRecordHotkey);

      Section := 'ImportExport';
      WriteBool ('ShowImportDialog', FOptions.ShowImportDialog);
      WriteInteger ('ImportType', Ord(FOptions.ImportType));
      WriteBool ('VirtualImport', FOptions.VirtualImport);
      WriteBool ('ShowExportDialog', FOptions.ShowExportDialog);
      WriteInteger ('ExportType', Ord(FOptions.ExportType));
      WriteInteger ('ExportEncode', Ord(FOptions.ExportEncode));
      WriteString ('SepLine', FOptions.SepLine);
      WriteBool ('ExternalSaveWhenDragInFile', FOptions.ExternalSaveWhenDragInFile);
      
      Section := 'Behavior';
      writebool ('EscMinimize', FOptions.EscMinimize);
     	WriteInteger ('PageDblClick', Ord(FOptions.PageDblClick));
      WriteInteger ('PageMButtonClick', Ord(FOptions.PageMButtonClick));
      WriteInteger ('GroupDblClick', Ord(FOptions.GroupDblClick));

		Section := 'LogIn';
      WriteBool ('NeedLogIn', FOptions.NeedLogIn);
      WriteString ('PassWord', EncryptString(FOptions.Password));
      WriteInteger ('LockTrayTime', FOptions.LockTrayTime);
		WriteBool ('EncryptDataFile', FOptions.EncryptDataFile);

      Section := 'Backup';
      writeinteger ('AutoSaveTime', FOptions.autosavetime);
      writeinteger ('BackupInterval', FOptions.BackupInterval);
      WriteInteger ('BackupIntervalType', Ord(FOptions.BackupIntervalType));
      WriteInteger ('TotalBackup', FOptions.TotalBackup);
      writestring ('BackupPath', FOptions.backuppath);

      Section := 'AutoHide';
      writeinteger ('Direction', Ord(FOptions.smoptions.direction));
      writeinteger ('HideDelay', FOptions.smoptions.hidedelay);
      writeinteger ('ShowDelay', FOptions.smoptions.showdelay);
      writeinteger ('AnimationTime', FOptions.smoptions.animationtime);
      WriteInteger ('EdgeWidth', FOptions.SMOptions.EdgeWidth);

      Section := 'Appearance';
      writestring ('Language', FOptions.language);
      writebool ('ToolWindowStyle', FOptions.ToolWindowStyle);
      Writebool ('CaptionPageName', FOptions.captionpagename);
      writebool ('ShowMenuBar', FOptions.showmenubar );
      writebool ('ShowToolBar', FOptions.showtoolbar);
      writeinteger ('Transparency', FOptions.transparency);
      WriteBool ('AutoAdjust', FOptions.AutoAdjust);
      WriteInteger ('AutoAdjustLevel', FOptions.AutoAdjustLevel);
      WriteInteger ('SplitterWidth', FOptions.SplitterWidth);
      
      Section := 'TreeView';
      WriteBool ('TreeHorzScroll', FOptions.TreeHorzScroll);
      WriteBool ('LinesAtRoot', FOptions.LinesAtRoot);
      WriteBool ('NodeButtons', FOptions.NodeButtons );
      WriteBool ('NodeLines', FOptions.NodeLines);
      WriteBool ('ShowNodeImages', FOptions.ShowNodeImages);
      WriteBool ('TreeOnRight', FOptions.TreeOnRight);
      f_WriteFontColor (o_inifile, 'TreeFontColor', FOptions.treefont, FOptions.treecolor);

      Section := 'TabControl';
      writebool ('MultiLineTab', FOptions.multilinetab);
      writeinteger ('TabWidth', FOptions.tabwidth);
      WriteBool ('TabOnBottom', FOptions.TabOnBottom);
      WriteBool ('ShowTabImages', FOptions.ShowTabImages);
      WriteBool ('HighlightCurrentTab', FOptions.HighlightCurrentTab);
      f_WriteFontColor (o_inifile, 'TabFontColor', FOptions.tabfont, FOptions.tabcolor);

      f_WriteFontColor (o_inifile, 'ListFontColor', FOptions.listfont, FOptions.listcolor);
      f_WriteFontColor (o_inifile, 'AbstractFontColor', FOptions.blogfont, FOptions.blogcolor);
      f_WriteFontColor (o_inifile, 'AbstractTitleFontColor', FOptions.blogtitlefont, FOptions.blogtitlecolor);
      f_WriteFontColor (o_inifile, 'AbstractSelTitleFontColor', FOptions.blogseltitlefont, FOptions.blogseltitlecolor);

      Section := 'Editor';
      writeinteger ('Margins', FOptions.margins);
      writebool ('ShowLineNumber', FOptions.showlinenumber);
      writeinteger ('LineNumberColor', FOptions.linenumbercolor);
      WriteBool ('HighLightSelLine', FOptions.HighLightSelLine);
      WriteInteger ('SelLineColor', FOptions.SelLineColor);
      for i := 0 to 4 do
      	WriteInteger ('HLColor' + IntToStr(i), FOptions.HLColor[i]);
      for i := 0 to 4 do
      	WriteBool ('HLUnderline' + IntToStr(i), FOptions.HLUnderline[i]);
      f_WriteFontColor (o_inifile, 'EditorFontColor', FOptions.editorfont, FOptions.editorcolor);

      Section := 'OtherControls';
      f_WriteFontColor (o_inifile, 'MininoteFontColor', FOptions.mininotefont, FOptions.mininotecolor);

      Section := 'ExtFuncs';
      writebool ('EnableCalcPage', FOptions.enablecalcpage);
      writebool ('EnableMemoPage', FOptions.enablememopage);
      writebool ('EnableDictPage', FOptions.enabledictpage);
      writebool ('EnableLinkPage', FOptions.enablelinkpage);
//      WriteBool ('EnableFastLink', FOptions.EnableFastLink);
      writebool ('EnableContactPage', FOptions.enablecontactpage);
      writebool ('EnableTemplate', FOptions.enabletemplate);
      writebool ('EnableClipboard', FOptions.enableclipboard);
      writebool ('EnableAdvEditor', FOptions.enableadveditor);

      Section := 'CalcPage';
      writeinteger ('Decimal', FOptions.CalcOptions.decimal);
      writebool ('Tri_Degree', FOptions.CalcOptions.tri_degree);

      Section := 'MemoPage';
      WriteInteger ('DefRemindTime', FOptions.DefRemindTime);

      Section := 'DictPage';
      WriteString ('SelDictList', FOptions.DictOptions.SelDictList);
      WriteString ('AllDictList', FOptions.DictOptions.AllDictList);
      WriteBool ('MultiDict', FOptions.DictOptions.MultiDict);
      WriteBool ('DebugMode', FOptions.DictOptions.DebugMode);
      WriteInteger ('TipQueryHotKey', FOptions.TipQueryHotKey);
      WriteInteger ('TipQueryColor', FOptions.TipQueryColor);

      Section := 'LinkPage';
      WriteBool ('DisableIconRead', FOptions.LinkOptions.disableIconRead);
      WriteBool ('AutoMinimizeAfterOpenLink', FOptions.LinkOptions.AutoMinimizeAfterOpenLink);
      writeinteger ('FastLinkHotKey', FOptions.fastlinkhotkey);
      writebool ('EnableFastLinkItemHotKey', FOptions.enablefastlinkitemhotkey);

      Section := 'Clipboard';
      writeinteger ('MaxClipNum', FOptions.clipoptions.maxclipnum);
      writeinteger ('MaxItemByte', FOptions.clipoptions.maxitembyte);
      WriteInteger ('MenuWidth', FOptions.ClipOptions.MenuWidth);
      WriteInteger ('FilterType', Ord(FOptions.clipoptions.FilterType));
      WriteBool ('NewPasteToTop', FOptions.ClipOptions.NewPasteToTop);
      writeinteger ('ClipboardHotKey', FOptions.clipboardhotkey);
      writebool ('EnableClipItemHotKey', FOptions.enableclipitemhotkey);

      Section := 'Template';
      writeinteger ('TemplateHotKey', FOptions.templatehotkey);
      writebool ('EnableTemplItemHotKey', FOptions.enabletemplitemhotkey);
      WriteInteger ('CaptureHotKey', FOptions.CaptureHotkey);
      writebool ('PopupMenuNoFocus', FOptions.PopupMenuNoFocus);

      SaveItemList ('UserFunctions', FOptions.UserFuncs);
      Free;
   end;
end;

function TOptionManager.DoSetOptions (WndParent: TxlWinControl): boolean;
var o_box: TOptionBox;
	o_reg: TxlRegistry;
   s: widestring;
begin
   o_reg := Txlregistry.create (HKEY_LOCAL_MACHINE);
   if o_reg.openkey ('Software\Microsoft\Windows\CurrentVersion\Run', false) then
   begin
      s := o_reg.readstring ('minipad2');
   	o_reg.closekey;
      FOptions.AutoStart := IsSameStr (ProgExe + ' -m', s);
   end;
   o_reg.free;
   FOptions.language := LangMan.Language;

   o_box := TOptionBox.Create (WndParent);
   o_box.Options := FOptions;
   o_box.OnApplyOption := f_OnApplyOption;
   o_box.Execute();
   result := false;
   o_box.Free;
end;

procedure TOptionManager.f_OnApplyOption (Sender: TObject);
var o_box: TOptionBox;
	o_reg: TxlRegistry;
begin
   o_box := Sender as TOptionBox;
   if o_box.Options.autostart <> FOptions.AutoStart then
   begin
      o_reg := Txlregistry.create (HKEY_LOCAL_MACHINE);
      if o_reg.openkey ('Software\Microsoft\Windows\CurrentVersion\Run', false) then
      begin
         if o_box.Options.autostart then
            o_reg.WriteString ('minipad2', ProgExe + ' -m')
         else
            o_reg.deletevalue ('minipad2');
         o_reg.closekey;
      end;
      o_reg.free;
   end;

   FOptions := o_box.options;
   DoSave;
   NotifyObservers;
//   MainWindow.Update;
end;

function TOptionManager.HaveExtPage (): boolean;
begin
	with FOptions do
		result := EnableCalcPage or EnableMemoPage or EnableDictPage or EnableLinkPage or EnableContactPage;
end;

//---------------

procedure TMemoryManager.OnCreate ();
begin
	FindHistory := TxlStrList.Create;
   ReplaceHistory := TxlStrList.Create;
end;

procedure TMemoryManager.OnDestroy ();
begin
	FindHistory.Free;
   ReplaceHistory.Free;
end;

procedure TMemoryManager.DoLoad ();
var o_inifile: TIniFileEx;
begin
	o_inifile := TIniFileEx.Create (ProgIni);

   o_inifile.Section := 'Window';
   winpos.x := o_inifile.readinteger ('WinPosX', 300);
   winpos.y := o_inifile.readinteger ('WinPosY', 200);
   winpos.width := o_inifile.readinteger ('WinPosWidth', 450);
   winpos.height := o_inifile.readinteger ('WinPosHeight', 380);
   ValidateWindowPos (winpos, true);
   WinStatus := TWindowStatus(o_inifile.readinteger ('WinStatus', Ord(wsNormal)));

   o_inifile.Section := 'NotePos';
   notepos.x := o_inifile.readinteger ('NotePosX', 300);
   notepos.y := o_inifile.readinteger ('NotePosY', 200);
   notepos.width := o_inifile.readinteger ('NotePosWidth', 240);
   notepos.height := o_inifile.readinteger ('NotePosHeight', 200);
   ValidateWindowPos (notepos, true);

   o_inifile.Section := 'Option';
   ShowTree := o_inifile.readbool ('ShowTree', false);
   stayontop := o_inifile.readbool ('StayOnTop', false);
   Transparent := o_inifile.readbool ('Transparent', false);
   specialmode := o_inifile.readbool ('SpecialMode', false);
   wordwrap := o_inifile.readbool ('WordWrap', true);
   watchclipboard := o_inifile.readbool ('WatchClipboard', false);
   tblist := o_inifile.readstring ('ToolButtons', '');

   o_inifile.Section := 'Memory';
   TreeWidth := o_inifile.readinteger ('TreeWidth', 140);  //ConfineRange ( 20, winpos.width - 20);
   TreeHide := o_inifile.ReadBool ('TreeHide', false);
   PageId := o_inifile.readinteger ('PageId', -10);
   SaveCount := StrToIntDef (o_inifile.readstring('SaveCount', '0'));
   LastBackupTime := o_inifile.ReadInteger ('LastBackupTime', 0);
   LastBackupSaveCount := o_inifile.ReadInteger ('LastBackupSaveCount', 0);
   OptionPage := o_inifile.ReadInteger('OptionPage', ob_program);
   IconFilePath := o_inifile.ReadString ('IconFilePath', '');

   MemoTimeMode := TTimemode (o_inifile.ReadInteger ('MemoTimeMode', Ord(tmThisDay)));
   MemoAction := TAction (o_inifile.ReadInteger ('MemoAction', Ord(mmReminder)));
   MemoUseSound := o_inifile.ReadBool ('MemoUseSound', false);
   MemoSoundFile := o_inifile.ReadString ('MemoSoundFile', '');
   LinkType := TLinkType (o_inifile.ReadInteger('LinkType', Ord(ltFile)));
   InsertLinkType := TLinkType (o_inifile.ReadInteger('InsertLinkType', Ord(ltFile)));

   o_inifile.ReadItemList ('FindHistory', FindHistory);
   o_inifile.ReadItemList ('ReplaceHistory', ReplaceHistory);

   o_inifile.Section := 'FindCrit';
   findtext := o_inifile.ReadString ('FindText', '');
   replacetext := o_inifile.ReadString ('ReplaceText', '');
   allowreplace := o_inifile.ReadBool ('AllowReplace', false);
   dirup := o_inifile.ReadBool ('DirUp', false);
   matchcase := o_inifile.ReadBool ('MatchCase', false);
   wholeword := o_inifile.ReadBool ('WholeWord', false);
   rollback := o_inifile.ReadBool ('RollBack', false);
   HighlightMatch := o_inifile.ReadBool ('HighlightMatch', true);
	ExitAfterFirstMatch := o_inifile.ReadBool ('ExitAfterFirstMatch', false);

   o_inifile.Section := 'SearchCrit';
   with SearchCrit do
   begin
   	PageType := TPageType(o_inifile.ReadInteger('PageType', Ord(ptNote)));
   	Col := o_inifile.ReadInteger ('Col', sr_Text);
      Opr := TOpr(o_inifile.ReadInteger ('Opr', Ord(oInclude)));
      Value := o_inifile.ReadString('Value', '');
	end;

   o_inifile.Free;
end;

procedure TMemoryManager.DoSave ();
begin
   with TIniFileEx.Create (ProgIni) do
   begin
   	Section := 'Window';
   	writeinteger ('WinPosX', Winpos.x);
   	writeinteger ('WinPosY', winpos.y);
   	writeinteger ('WinPosWidth', winpos.width);
   	writeinteger ('WinPosHeight', winpos.height);
      writeinteger ('WinStatus', Ord(winstatus));
      
   	Section := 'NotePos';
   	writeinteger ('NotePosX', notepos.x);
   	writeinteger ('NotePosY', notepos.y);
   	writeinteger ('NotePosWidth', notepos.width);
   	writeinteger ('NotePosHeight', notepos.height);

      Section := 'Option';
      writebool ('ShowTree', ShowTree);
   	writebool ('StayOnTop', stayontop);
      Writebool ('Transparent', Transparent);
   	writebool ('SpecialMode', specialmode);
      WriteBool ('WordWrap', wordwrap);
      writebool ('WatchClipboard', watchclipboard);
   	writestring ('ToolButtons', tblist);

      section := 'Memory';
      writeinteger ('TreeWidth', TreeWidth);
      WriteBool ('TreeHide', TreeHide);
      writeinteger ('PageId', PageId);
      writestring ('SaveCount', IntToStr(SaveCount));
      WriteInteger ('LastBackupTime', LastBackupTime);
      WriteInteger ('LastBackupSaveCount', LastBackupSaveCount);
      WriteInteger ('OptionPage', OptionPage);
      WriteString ('IconFilePath', IconFilePath);
      
      WriteInteger ('MemoTimeMode', Ord(MemoTimeMode));
      WriteInteger ('MemoAction', Ord(MemoAction));
      WriteBool ('MemoUseSound', MemoUseSound);
      WriteString ('MemoSoundFile', MemoSoundFile);
      WriteInteger ('LinkType', Ord(LinkType));
      WriteInteger ('InsertLinkType', Ord(InsertLinkType));
      
      SaveItemList ('FindHistory', FindHistory);
      SaveItemList ('ReplaceHistory', ReplaceHistory);

      section := 'FindCrit';
      WriteString ('FindText', FindText);
      WriteString ('ReplaceText', ReplaceText);
      WriteBool ('AllowReplace', AllowReplace);
      WriteBool ('DirUp', DirUp);
      WriteBool ('MatchCase', MatchCase);
      WriteBool ('WholeWord', WholeWord);
      WriteBool ('RollBack', RollBack);
      WriteBool ('HighlightMatch', HighlightMatch);
      WriteBool ('ExitAfterFirstMatch', ExitAfterFirstMatch);

      section := 'SearchCrit';
      WriteInteger ('PageType', Ord(SearchCrit.PageType));
      WriteInteger ('Col', SearchCrit.Col);
      WriteInteger ('Opr', Ord(SearchCrit.Opr));
      WriteString ('Value', SearchCrit.Value);

      Free;
   end;
end;

initialization

finalization
  	FreeAndNil (FOptionMan);
  	FreeAndNil (FMemoryMan);
      
end.
