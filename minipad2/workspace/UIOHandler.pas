unit UIOHandler;

interface

uses Windows, UxlClasses, UxlExtClasses, UPageSuper, UClientSuper, UTypeDef, UxlFile, UxlDialog, UxlComboBox, UxlWindow, UxlWinControl;

type
	TIOHandler = class (TxlInterfacedObject, ICommandExecutor, IOptionObserver, IMessageObserver)
   private
      FMailClient: widestring;
      FExternalSaveWhenDragInFile: boolean;
      Fowner: TxlWindow;

		procedure ImportPage ();
      procedure ExportPage ();
      procedure SendMail ();
      
      procedure ImportFiles (o_parent: TPageSuper);
      procedure ImportFolder (o_parent: TPageSuper);
      function f_SelectFolder (const s_deffolder: widestring = ''): widestring;
		function ImportFrom (o_parent: TPageSuper; const s_pathfile: widestring; b_virtual: boolean; b_ignorenontxtfiles: boolean): boolean;  // 按ESC终止
      procedure ImportMepFile (o_parent: TPageSuper; const s_file: widestring);
      procedure ImportTextFile (o_parent: TPageSuper; const s_file: widestring; b_virtual: boolean);

      procedure ExportGroupToFolder (p: TPageSuper; const s_folder: widestring);
      function f_SelectExportFile (p: TPageSuper): widestring;
      procedure ExportPageToFile (p: TPageSuper; const s_file: widestring);
      procedure ExportPageToClipboard (p: TPageSuper);
      function f_GetParentPage (var o_parent: TPageSuper): boolean;
      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
      procedure f_SelectLastChild (o_parent: TPageSuper);
   public
   	constructor Create (AOwner: TxlWindow);
      destructor Destroy (); override;

   	procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
      procedure OptionChanged ();
   end;

   TImportBox = class (TxlDialog)
   private
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
   	procedure OnCommand (ctrlID: integer); override;
	public
   end;

   TExportBox = class (TxlDialog)
   private
      FGroupExport: boolean;
      FCmbEncode: TxlComboBox;
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   	procedure OnCommand (ctrlID: integer); override;
	public
      property GroupExport: boolean read FGroupExport write FGroupExport;
   end;

// 与 UOpProgram 中的 TOpImportExport 共享代码
procedure f_SetEncode (cmb: TxlComboBox; enc: TEncode);
function f_GetEncode (cmb: TxlComboBox): TEncode;

implementation

uses Messages, UxlCommDlgs, UGlobalObj, UOptionManager, ULangManager, UPageFactory, UDialogs, UxlList, UxlStrUtils, UxlFunctions,
	ShellAPI, UPageStore, UxlMiscCtrls, UxlWinClasses, UxlWinDef, Resource;

function MepVersion (): widestring;
begin
	result := '3.2b3';
end;

constructor TIOHandler.Create (AOwner: TxlWindow);
begin
	CommandMan.AddExecutor (self);
   OptionMan.AddObserver (self);
   FOwner := AOwner;
   FOwner.AddMessageObserver (self);
end;

destructor TIOHandler.Destroy ();
begin
	FOwner.RemoveMessageObserver (self);
	OptionMan.RemoveObserver (self);
	CommandMan.RemoveExecutor (self);
   inherited;
end;

procedure TIOHandler.OptionChanged ();
begin
   FMailClient := OptionMan.Options.MailClient;
   FExternalSaveWhenDragInFile := OptionMan.Options.ExternalSaveWhenDragInFile;
end;

procedure TIOHandler.ExecuteCommand (opr: word);
begin
	case opr of
   	m_import:
      	ImportPage;
      m_export:
     		ExportPage;
      m_sendmail:
     		SendMail;
   end;
end;

function TIOHandler.CheckCommand (opr: word): boolean;
var o_parent: TPageSuper;
begin
	if opr = m_import then
   	result := f_GetParentPage (o_parent)
   else
		result := true;
end;

procedure TIOHandler.SendMail ();
var s_text, s_par: widestring;
	p: TPageSuper;
begin
	p := PageCenter.ActivePage;
	PageCenter.EventNotify (pctSave, p.id);
	s_text := ReplaceStrings (p.ExportText (false), ['%', '?', '=', '&'], ['%25', '%3F', '%3D', '&amp;']);
	s_text := ReplaceStrings (s_text, [#13#10, #9, #32], ['%0D%0A', '%09', '%20']);
   s_par := 'mailto:?subject=' + p.Name + '&body=' + s_text;
   if PathFileExists(FMailClient) then
   	ShellExecuteW (0, 'open', pwidechar(FMailClient), pwidechar(s_par), nil, sw_shownormal)
   else
   	ShellExecuteW (0, 'open', pwidechar(s_par), nil, nil, sw_shownormal);
end;

//--------------------

function TIOHandler.f_SelectFolder (const s_deffolder: widestring = ''): widestring;
begin
   with TxlPathDialog.Create () do
   begin
      Title := LangMan.GetItem (sr_selectfolder);
      Path := s_deffolder;
      if Execute then
         result := Path
      else
         result := '';
      free;
   end;
end;

function TIOHandler.f_GetParentPage (var o_parent: TPageSuper): boolean;
begin
	result := false;
	o_parent := PageCenter.ActivePage;
   if o_parent = nil then exit;
   if not (o_parent.PageType in [ptGroup, ptGroupRoot]) then
   	o_parent := o_parent.Owner;
   if o_parent.PageType in [ptGroup, ptGroupRoot] then
   	result := true;
end;

procedure TIOHandler.ImportPage ();
var b: boolean;
	o_box: TImportBox;
   o_parent: TPageSuper;
   n: integer;
begin
	if not f_GetParentPage (o_parent) then exit;
   n := o_parent.Childs.Count;

   if OptionMan.Options.ShowImportDialog then
   begin
   	o_box := TImportBox.Create ();
      b := o_box.Execute;
      o_box.Free;
      if not b then exit;
   end;

   if OptionMan.Options.ImportType = itFromFile then
   	ImportFiles (o_parent)
   else
   	ImportFolder (o_parent);

   // 如果导入成功，则选中最后一项导入项
   if o_parent.Childs.Count > n then
		f_SelectLastChild (o_parent);
end;

procedure TIOHandler.ImportFiles (o_parent: TPagesuper);
var s_path, s_files: widestring;
   i: integer;
begin
   s_files := '';
   with TxlOpenDialog.create () do
   begin
      Filter := LangMan.GetItem(sr_ImportFilter);
      FileName := '';
      Path := '';
      MultiSelect := true;
      Title := LangMan.GetItem (sr_SelectImportFile);
      DefaultExt := 'txt';
      if Execute then
      begin
         s_path := Path;
         s_files := FileName;
      end;
      free;
   end;
   if s_files = '' then exit;

   with TxlStrList.Create () do
   begin
      Separator := #9;
      Text := s_files;
      for i := Low to High do
         if not ImportFrom (o_parent, s_path + Items[i], OptionMan.Options.VirtualImport, false) then
            break;
      free;
   end;
end;

procedure TIOHandler.ImportFolder (o_parent: TPagesuper);
var s_folder: widestring;
begin
   s_folder := f_SelectFolder;
   if s_folder <> '' then
      ImportFrom (o_parent, s_folder, OptionMan.Options.VirtualImport, true);
end;

function TIOHandler.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
	procedure f_ProcessDropFiles (h_drop: HDrop);
	var i, i_count, n: integer;
   	buffer: array[0..2000] of widechar;
		o_parent: TPageSuper;
      b_virtual: boolean;
   begin
		if not f_GetParentPage (o_parent) then exit;
      n := o_parent.childs.count;
   	i_count := DragQueryFileW (h_drop, $FFFFFFFF, nil, 0);

      if KeyPressed(VK_SHIFT) then
      	b_virtual := true
      else if KeyPressed(VK_CONTROL) then
      	b_virtual := false
      else
      	b_virtual := FExternalSaveWhenDragInFile;
         
      for i := 0 to i_count - 1 do
      begin
      	DragQueryFileW (h_drop, i, @buffer, sizeof(buffer));
     		if not ImportFrom (o_parent, buffer, b_virtual, false) then break;
      end;
      if o_parent.Childs.Count > n then
	      f_SelectLastChild (o_parent);
   end;
begin
	result := 0;
   if AMessage = WM_DROPFILES then
   begin
   	f_ProcessDropFiles (wparam);
      b_processed := true;
   end
   else
   	b_processed := false;
end;

//-------------------

function TIOHandler.ImportFrom (o_parent: TPageSuper; const s_pathfile: widestring; b_virtual: boolean; b_ignorenontxtfiles: boolean): boolean;
var o_list: TxlStrList;
   id, i: integer;
begin
   if not pathfileexists (s_pathfile) then exit;
   if IsDirectory (s_pathfile) then
   begin
      id := PageStore.NewPage (o_parent.id, ptGroup);
      PageStore[id].Name := extractfilename (s_pathfile, false);
      o_parent.Childs.AddChild (id);

      o_list := TxlStrList.Create;
      FindFiles (s_pathfile + '\*.*', o_list);
      for i := o_list.Low to o_list.High do
      	if not ImportFrom (PageStore[id], s_pathfile + '\' + o_list[i], b_virtual, true) then break;
      o_list.Free;
   end
   else
   begin
      PauseEscapeMessage := true;
      ProgTip.ShowTip (s_pathfile, LangMan.GetItem(sr_importingprompt), tiInfo);
      if IsSameStr (extractfileext(s_pathfile), 'mep') then
         ImportMepFile (o_parent, s_pathfile)
      else if not (b_ignorenontxtfiles and (LowerCase(ExtractFileExt(s_pathfile)) <> 'txt')) then
         ImportTextFile (o_parent, s_pathfile, b_virtual);
      ProgTip.HideTip;
   end;
   ProcessMessages;     // 必需！
   result := not KeyPressed (VK_ESCAPE);
   if not result then
      ProgTip.ShowTip (LangMan.GetItem(sr_userabortimport));
end;

procedure TIOHandler.ImportMepFile (o_parent: TPageSuper; const s_file: widestring);
var o_file: TxlTextFile;
	s_text, s, s_mepversion: widestring;
   pt: TPageType;
   id: integer;
begin
	try
      o_file := TxlTextFile.create (s_file, fmRead, enUnknown);

      o_file.ReadLn (s_mepversion);
      if s_mepversion <> MepVersion then
      begin
      	ShowMessage (LangMan.GetItem (sr_mepversionnotmatch));
         o_file.free;
         exit;
      end;

      o_file.ReadLn (s);
      pt := TPageType (StrToIntDef(s));
		o_file.ReadText (s_text);
      o_file.free;

      id := PageStore.NewPage (o_parent.id, pt);
      with PageStore[id] do
      begin
         Name := extractfilename (s_file, false);
         PageProperty.ExportFile := s_file;
         ImportText (s_text);
      end;
      if not PageStore[id].SingleInstance then
         o_parent.Childs.AddChild (id);
   except
   end;
end;

procedure TIOHandler.ImportTextFile (o_parent: TPageSuper; const s_file: widestring; b_virtual: boolean);
var s_text: widestring;
	id: integer;
begin
   id := PageStore.NewPage (o_parent.id, ptNote);
   with PageStore[id] do
   begin
      Name := extractfilename (s_file, false);
      PageProperty.ExportFile := s_file;
      PageProperty.ExternalSave := b_virtual;
      o_parent.Childs.AddChild (id);
      SaveMan.Save; // 防止导入大文件时费时太长，用户终止程序，出现数据库体积膨胀而无相应节点的问题
      if not b_virtual then
      begin
         with TxlTextFile.create (s_file, fmRead, enUnknown) do
         begin
            ReadText (s_text);
            free;
         end;
         Text := s_text;
      end;
   end;
end;

procedure TIOHandler.f_SelectLastChild (o_parent: TPageSuper);
var n, id: integer;
begin
   n := o_parent.childs.Count;
   id := o_parent.Childs.ChildId (n - 1);
   PageCenter.ActivePage := PageStore[id];
end;

//--------------------------

procedure TIOHandler.ExportPage ();
var s_file, s_folder: widestring;
   p: TpageSuper;
   o_box: TExportBox;
   b, b_group: boolean;
begin
   p := PageCenter.ActivePage;
	b_group := p.CanAddChild (ptNote);

   if OptionMan.Options.ShowExportDialog then
   begin
   	o_box := TExportBox.Create ();
	   o_box.GroupExport := b_group;
   	b := o_box.Execute;
   	o_box.Free;
   	if not b then exit;
   end;

	PageCenter.EventNotify (pctSave, p.id);
   if OptionMan.Options.ExportType = etToClipboard then
      ExportPageToClipboard (p)
   else if b_group and (OptionMan.Options.ExportType = etToFolder) then
   begin
   	s_folder := f_SelectFolder (p.PageProperty.ExportFile);
      ExportGroupToFolder (p, s_folder);
   end
   else
   begin
      s_file := f_SelectExportFile (p);
      ExportPageToFile (p, s_file);
   end;
end;

procedure TIOHandler.ExportGroupToFolder (p: TPageSuper; const s_folder: widestring);
var o_list: TxlIntList;
   i: integer;
   psub: TPageSuper;
   s: widestring;
   b_abort: boolean;
begin
   if s_folder = '' then exit;
   p.PageProperty.ExportFile := s_folder;
   if not PathFileExists (s_folder) then
      CreateDir (s_folder);

   o_list := TxlIntList.Create;
   p.GetChildList (o_list);
   for i := o_list.Low to o_list.High do
   begin
      PauseEscapeMessage := true;

      psub := PageStore [o_list[i]];
      s := s_folder + FilterFileName(psub.Name, true);
      ProgTip.ShowTip (psub.Name, LangMan.GetItem(sr_exportingprompt), tiInfo);

      if psub.CanAddChild (ptNote) then   // 导出为文件夹
         ExportGroupToFolder (psub, s + '\')
      else if psub.IsChildItemContainer then   // 对于通讯录等列表页，导出为 mep 文件
         ExportPageToFile (psub, s + '.mep')
      else
         ExportPageToFile (psub, s + '.txt');

      ProgTip.HideTip;
      ProcessMessages;     // 必需！
      b_abort := KeyPressed (VK_ESCAPE);
      if b_abort then
      begin
         ProgTip.ShowTip (LangMan.GetItem(sr_userabortexport));
         break;
      end;
   end;
   o_list.Free;
   if not b_abort then
      ProgTip.ShowTip (s_folder, LangMan.GetItem (sr_ExportedToFolder), tiInfo);
end;

function TIOHandler.f_SelectExportFile (p: TPageSuper): widestring;
var s_filename: widestring;
begin
   if (ExtractFileExt(p.PageProperty.ExportFile) <> '') then
      s_FileName := p.PageProperty.ExportFile
   else if p.IsChildItemContainer then
      s_FileName := FilterFileName(p.Name, true) + '.mep'
   else
      s_filename := FilterFileName(p.Name, true) + '.txt';

   with TxlSaveDialog.Create () do
   begin
      FileName := s_filename;
      DefaultExt := ExtractFileExt (FileName);
		if DefaultExt = 'txt' then
         Filter := LangMan.GetItem(sr_ExportFilter2)
      else
         Filter := LangMan.GetItem(sr_ExportFilter3);

      Path := extractfilepath (FileName);
      Title := LangMan.GetItem (sr_NameExportFile);
      if Execute then
         result := Path + FileName
      else
         result := '';
      free;
   end;
end;

procedure TIOHandler.ExportPageToFile (p: TPageSuper; const s_file: widestring);
var o_file: TxlTextFile;
	b_mep: boolean;
begin
   if s_file = '' then exit;
   b_mep := IsSameStr(extractfileext(s_file), 'mep');

   o_file := TxlTextFile.create (s_file, fmWrite, OptionMan.Options.ExportEncode);
   if b_mep then
   	o_file.WriteLn (MepVersion);
   o_file.WriteText (p.ExportText (b_mep));
   o_file.Free;

   if not p.PageProperty.ExternalSAve then
   	p.PageProperty.ExportFile := s_file;
   if (p.Childs <> nil) and (not p.IsChildItemContainer) then
	   ProgTip.ShowTip (s_file, LangMan.GetItem (sr_GroupExportedToFile), tiInfo)
   else
	   ProgTip.ShowTip (s_file, LangMan.GetItem (sr_PageExportedToFile), tiInfo);
end;

procedure TIOHandler.ExportPageToClipboard (p: TPageSuper);
begin
   Clipboard.Text := p.ExportText (false);
   ProgTip.ShowTip (LangMan.GetItem (sr_PageExportedToClipboard));
end;

//----------------------

procedure TImportBox.OnInitialize ();
begin
	inherited;
	SetTemplate (Import_Box, m_import);
end;

const c_importbox: array [0..5] of word = (st_importtype, rb_fromfile, rb_fromfolder, chk_virtualimport, IDOK, IDCANCEL);

procedure TImportBox.OnOpen ();
begin
	inherited;
	RefreshItemText (self, c_importbox, Import_Box);
   ItemChecked[rb_fromfile] := (OptionMan.Options.ImportType = itFromFile);
   ItemChecked[rb_fromfolder] := not ItemChecked[rb_fromfile];
   ItemChecked[chk_virtualimport] := OptionMan.Options.VirtualImport;
end;

procedure TImportBox.OnCommand (ctrlID: integer);
var o_option: TOptions;
begin
	if ctrlID =	IDOK then
   begin
   	o_option := OptionMan.Options;
      if Itemchecked[rb_fromfile] then
         o_option.ImportType := itFromFile
      else
         o_option.ImportType := itFromFolder;
      o_option.VirtualImport := ItemChecked[chk_virtualimport];
      OptionMan.Options := o_option;
   end;

   inherited OnCommand (ctrlID);
end;

//------------------------------

procedure TExportBox.OnInitialize ();
begin
	inherited;
	SetTemplate (Export_Box, m_Export);
end;

procedure TExportBox.OnOpen ();
const c_Exportbox: array [0..6] of word = (st_Exporttype, rb_tofile, rb_tofolder, rb_toclipboard, st_Encode, IDOK, IDCANCEL);
begin
	inherited;
	RefreshItemText (self, c_Exportbox, Export_Box);
   if OptionMan.Options.ExportType = etToClipboard then
      ItemChecked[rb_toclipboard] := true
   else if GroupExport and (OptionMan.Options.ExportType = etToFolder) then
      ItemChecked[rb_tofolder] := true
   else
      ItemChecked[rb_tofile] := true;
   ItemEnabled[rb_tofolder] := GroupExport;
   ItemEnabled[cmb_encode] := not ItemChecked[rb_toclipboard];

   FCmbEncode := TxlComboBox.Create (self, ItemHandle[cmb_Encode]);
	f_SetEncode (FCmbEncode, OptionMan.Options.ExportEncode);
end;

procedure TExportBox.Onclose ();
begin
	FCmbEncode.free;
   inherited;
end;

procedure TExportBox.OnCommand (ctrlID: integer);
var o_option: TOptions;
begin
	case ctrlID of
      IDOK:
         begin
            o_option := OptionMan.Options;
            if Itemchecked[rb_tofile] then
               o_option.ExportType := etToFile
            else if ItemChecked[rb_toclipboard] then
               o_option.ExportType := etToClipboard
            else
               o_option.ExportType := etToFolder;
            o_option.ExportEncode := f_GetEncode (FCmbEncode);
            OptionMan.Options := o_option;
         end;
      rb_tofile, rb_tofolder, rb_toclipboard:
         ItemEnabled[cmb_encode] := not ItemChecked[rb_toclipboard];
   end;
   
   inherited OnCommand (ctrlID);
end;

procedure f_SetEncode (cmb: TxlComboBox; enc: TEncode);
const c_Encodes: array [0..3] of widestring = ('ANSI', 'Unicode', 'Unicode big endian', 'UTF-8');
var i: integer;
begin
   for i := Low(c_Encodes) to High(c_Encodes) do
   	cmb.Items.Add (c_Encodes[i]);
   case enc of
   	enAnsi: i := 0;
      enUTF16LE: i := 1;
      enUTF16BE: i := 2;
      else i := 3;
   end;
   cmb.Items.SelIndex := i;
end;

function f_GetEncode (cmb: TxlComboBox): TEncode;
begin
   case cmb.Items.SelIndex of
      0: result := enAnsi;
      1: result := enUTF16LE;
      2: result := enUTF16BE;
      else result := enUTF8_BOM;
   end;
end;

end.



