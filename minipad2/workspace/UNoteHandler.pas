unit UNoteHandler;

interface

uses UxlClasses, UxlWinclasses, UxlExtClasses, UxlDialog, UCommonClasses, UxlList, UxlRichEdit;

type
	TNoteHandler = class (TxlInterfacedObject, IHotkeyOwner, IOptionObserver, ICommandExecutor, IClipObserver)
   private
      FNoteBoxList: TxlObjList;
      FClipText: widestring;
		procedure f_NewForeGroundNote (const s_title: widestring = '');
		procedure f_NewBackgroundNote (var s_name: widestring);
      procedure f_NewNoteBG_SnapText (const s_text: widestring);
      procedure f_SnapText (const s_text: widestring);
      procedure f_OnNoteBoxClosed (Sender: TObject);

      procedure f_DoSnapText (const s_text: widestring);
      function f_GetClipText (): widestring;
      function f_GetAbstract (const s: widestring): widestring;
   public
   	constructor Create ();
      destructor Destroy (); override;

      procedure OnHotkey (id: integer; hk: THotkey);
      procedure OptionChanged ();
      procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
      procedure ClipNotify (const value: widestring);
   end;

   TNoteBox = class (TxlDialogML)
   private
   	FNoteTitle: widestring;
   	FNoteText: widestring;
      FNoteEditor: TxlRichEdit;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;

      property NoteTitle: widestring read FNoteTitle write FNoteTitle;
      property NoteText: widestring read FNoteText;
   end;

implementation

uses Windows, UOptionManager, UGlobalObj, ULangManager, UPageFactory, UxlFunctions, UxlWindow, UxlMiscCtrls, UxlStrUtils,
	UTypeDef, UxlFile, UxlCommDlgs, Messages, Resource;

const hk_NewNoteForeGround = 1; hk_NewNoteBackGround = 2; hk_SnapText = 3; hk_NewNoteBG_SnapText = 4;

constructor TNoteHandler.Create ();
begin
   FNoteBoxList := TxlObjList.Create;
   FClipText := '';
   OptionMan.AddObserver (self);
   CommandMan.AddExecutor (self);
end;

destructor TNoteHandler.Destroy ();
var i: integer;
begin
   for i := FNoteBoxList.High downto FNoteBoxList.Low do
      TNoteBox(FNoteBoxList[i]).Close;
	CommandMan.RemoveExecutor (self);
	OptionMan.RemoveObserver (self);
   HotkeyCenter.RemoveOwner (self);
   FNoteBoxList.free;
   inherited;
end;

procedure TNoteHandler.OptionChanged ();
begin
	with HotkeyCenter do
   begin
		AddHotKey (self, hk_NewNoteForeGround, OptionMan.Options.NewNoteForeGroundHotKey);
      AddHotkey (self, hk_NewNoteBackGround, OptionMan.Options.NewNoteBackGroundHotKey);
      AddHotkey (self, hk_CloseNote, OptionMan.Options.CloseNoteHotkey);
      AddHotkey (self, hk_SnapText, OptionMan.Options.SnapTextHotKey);
		AddHotKey (self, hk_NewNoteBG_SnapText, OptionMan.Options.NewNoteBG_SnapTextHotKey);
      AddHotkey (self, hk_SnapTextToFile, OptionMan.Options.SnapTextToFileHotkey);
      AddHotkey (self, hk_AutoRecord, OptionMan.Options.AutoRecordHotkey);
   end;
end;

procedure TNoteHandler.OnHotkey (id: integer; hk: THotkey);
var s, s_file: widestring;
	o_file: TxlTextFile;
   i: integer;
begin
   ReleaseHotKey (hk);
	case id of
      hk_NewNoteForeGround:
      	begin
            s := LeftStr (f_GetClipText, 200);
         	f_newForeGroundNote (s);
         end;
      hk_NewNoteBackGround:
         begin
            s := LeftStr (f_GetClipText, 200);
            f_newBackgroundNote (s);
            SaveMan.Save;
            ProgTip.ShowTip (s, LangMan.GetItem(sr_newnotecreated), tiInfo);
         end;
      hk_SnapText:
      	ClipNotify (f_GetClipText);
      hk_NewNoteBG_SnapText:
         f_NewNoteBG_SnapText (f_GetClipTExt);
      hk_SnapTextToFile:
      	begin
            s := f_GetClipTExt;
            if s = '' then exit;
            with TxlSaveDialog.Create () do
            begin
               FileName := '';
               DefaultExt := 'txt';
               Filter := LangMan.GetItem(sr_ExportFilter1);
               Title := LangMan.GetItem (sr_SaveSnapText);
               if Execute then
               begin
                  s_file := Path + FileName;
                  o_file := TxlTextFile.Create (s_file, fmWrite, OptionMan.Options.ExportEncode);
                  o_file.WriteText (s);
                  o_file.free;
                  ProgTip.ShowTip (f_GetAbstract(s), LangMan.GetItem(sr_SnapTextSavedToFile) + ' ' + s_file, tiInfo);
               end;
               free;
            end;
         end;
      hk_closeNote:
         begin
            i := FNoteBoxList.High;
            if i >= 0 then
               TNoteBox (FNoteBoxList[i]).Close;
         end;
      hk_autorecord:
         CommandMan.ExecuteCommand (m_autorecord);
   end;
end;

//---------------------

function TNoteHandler.f_GetAbstract (const s: widestring): widestring;
begin
   if Length(s) > 500 then
      result := LeftStr(s, 500) + #13#10 + '...'
   else
      result := s;
end;

function TNoteHandler.f_GetClipText (): widestring;
var hfor: HWND;
   o_clip: TxlClipboard;
   s: widestring;
begin
   hfor := GetforegroundWindow;
   o_clip := TxlClipboard.Create (hfor);
   s := o_clip.Text;
   o_clip.Text := '';
   ClipWatcher.PassNext;
   PressCombineKey ('C', VK_CONTROL);
   result := Trim(o_clip.Text);
   if result = '' then
      o_clip.TExt := s;
   o_clip.free;
end;

procedure TNoteHandler.f_DoSnapText (const s_text: widestring);
begin
   EventMan.Message := s_text;
   EventMan.EventNOtify (e_AppendEditorText);
   SaveMan.Save;
end;

procedure TNoteHandler.f_NewForeGroundNote (const s_title: widestring = '');
var o_box: TNoteBox;
begin
   o_box := TNoteBox.Create;
   with o_box do
   begin
      NoteTitle := MultiLineToSingleLine(s_title, true);
      OnDialogClosed := f_OnNoteBoxClosed;
      Open;
		BringToTop;
   	StayOnTop := true;
      Update;
   end;
   FNoteBoxList.Add(o_box);
end;

procedure TNoteHandler.f_OnNoteBoxClosed (Sender: TObject);
var o_box: TNoteBox;
   s: widestring;
begin
	o_box := Sender as TNoteBox;
   if (not ((o_box.NoteTitle = '') and (o_box.NoteText = ''))) then
   begin
      s := o_box.NoteTitle;
      f_NewBackgroundNote (s);
      f_DoSnapText (o_box.NoteText);
   end;

   FNoteBoxList.Remove (o_box);
   o_box.Free;
end;

procedure TNoteHandler.f_NewBackgroundNote (var s_name: widestring);
var b: boolean;
	op: TOptions;
begin
	op := OptionMan.Options;
	b := op.AutoName;
   op.AutoName := true;
   OptionMan.Options := op;

   CommandMan.ExecuteCommand (m_newnote);
   if s_name <> '' then
   	PageCenter.ActivePage.Name := s_name;
  	s_name := PageCenter.ActivePage.Name;

   op.AutoName := b;
   OptionMan.Options := op;
end;

//-----------------

procedure TNoteHandler.ClipNotify (const value: widestring);
begin
   SendKeyPress (VK_CONTROL, false);
   if value <> FClipText then
   begin
      if PageCenter.ActivePage.PageType <> ptNote then
         f_NewNoteBG_SnapText (value)
      else
         f_SnapText (value);
      FClipText := value;
   end;
end;

procedure TNoteHandler.f_NewNoteBG_SnapText (const s_text: widestring);
var s_name: widestring;
begin
   if s_text = '' then exit;

   s_name := '';
   f_NewBackgroundNote (s_name);
   f_DoSnapText (s_text);
   ProgTip.ShowTip (f_GetAbstract(s_text), LangMan.GetItem(sr_newnotebgandsnaptextsuccess), tiInfo);
end;

procedure TNoteHandler.f_SnapText (const s_text: widestring);
begin
   if s_text = '' then exit;
   f_DoSnapText (s_text);
   ProgTip.ShowTip (f_GetAbstract(s_text), LangMan.GetItem(sr_snaptextsuccess), tiInfo);
end;

//------------------------

procedure TNoteHandler.ExecuteCommand (opr: word);
var b: boolean;
begin
	case opr of
   	m_newnoteforeground:
			f_NewForeGroundNote;
      m_autorecord:
         begin
            b := not CommandMan.ItemChecked [m_autorecord];
            CommandMan.ItemChecked [m_autorecord] := b;
            if b then
               ClipWatcher.AddObserver (self)
            else
               ClipWatcher.RemoveObserver (self);
         end;
   end;
end;

function TNoteHandler.CheckCommand (opr: word): boolean;
begin
	if opr = m_newnoteforeground then
		result := CommandMan.CheckCommand (m_newnote)
   else
		result := true;
end;

//-------------------------

procedure TNoteBox.OnInitialize ();
begin
	SetTemplate (Note_Box, m_newnote);
end;

procedure TNoteBox.OnOpen ();
var i: integer;
begin
   inherited;
   RefreshItemText (self, [], Note_Box);
   self.Pos := MemoryMan.NotePos;

   FNoteEditor := TxlRichEdit.Create (self);
   with FNoteEditor do
   begin
      Left := 0;
      Top := 0;
      Align := alClient;
      Font := OptionMan.Options.MiniNoteFont;
      Color := OptionMan.Options.MiniNoteColor;
      WordWrap := true;
      UndoLimit := OptionMan.Options.undolimit;
      TabStops := OptionMan.Options.tabstops;
      AutoIndent := OptionMan.Options.autoindent;
      AutoEmptyLine := OptionMan.Options.autoemptyline;

      SetFocus;
      if FNoteTitle <> '' then
      begin
         Text := '@' + FNoteTitle + #13#10;
         i := Length(Text);
         Post (EM_SETSEL, i, i);
      end;
   end;
end;

procedure TNoteBox.OnClose ();
var i: integer;
   s: widestring;
begin
   FNoteText := FNoteEditor.Text;
   i := FirstPos (#13#10, FNoteText);
   s := Trim(LeftStr (FNoteText, i - 1));
   if LeftStr(s, 1) = '@' then
   begin
     	FNoteTitle := MidStr(s, 2);
      FNoteText := MidStr (FNoteText, i + 2);
   end;

   MemoryMan.NotePos := self.Pos;
   FNoteEditor.free;
	inherited;
end;

end.
