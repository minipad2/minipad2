unit UFindHandler;

interface

uses UxlClasses, UxlExtClasses, UxlRichEdit, UxlRichEditClasses, UxlList, UxlDialog, UxlStdCtrls, UxlComboBox;

type
   TFindBox = class (TxlDialogML)
   private
      FindHistory: TxlStrList;
      ReplaceHistory: TxlStrList;
      FFindHandler: TxlRichEditFindHandler;
      FCmbFindHistory, FCmbReplaceHistory: TxlComboBox;

      procedure f_CheckReplace ();
      procedure f_RefreshCrit ();
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
   	property FindHandler: TxlRichEditFindHandler read FFindHandler write FFindHandler;
   end;

   TFindHandler = class (TxlInterfacedObject, IMemorizer, IOptionObserver, IEventObserver)
   private
      FEditor: TxlRichEdit;
      FFindBox: TFindBox;
		FFindHandler: TxlRichEditFindHandler;

      procedure Find ();
      procedure f_OnFindBoxClosed (Sender: TObject);
   public
      constructor Create (AEditor: TxlRichEdit);
      destructor Destroy (); override;

      function CheckCommand (opr: word): boolean;
      procedure ExecuteCommand (opr: word);
      procedure EventNotify (event, wparam, lparam: integer);
      procedure RestoreMemory ();
      procedure SaveMemory ();
      procedure OptionChanged ();
   end;

implementation

uses Windows, Messages, UOptionManager, UGlobalObj, UxlStrUtils, UxlCommDlgs, ULangManager, Resource, UxlFunctions;

constructor TFindHandler.Create (AEditor: TxlRichEdit);
begin
	FEditor := AEditor;
   FFindHandler := TxlRichEditFindHandler.Create (AEditor);
   OptionMan.AddObserver (self);
   MemoryMan.AddObserver (self);
   EventMan.AddObserver (self);
end;

destructor TFindHandler.Destroy ();
begin
	EventMan.RemoveObserver (self);
	MemoryMan.RemoveObserver (self);
   OptionMan.RemoveObserver (self);
   FFindHandler.free;
   inherited;
end;

procedure TFindHandler.OptionChanged ();
begin
   FFindHandler.HighlightColor := OptionMan.Options.HLColor[0];
   FFindHandler.HighlightThickness := IfThen (OptionMan.Options.HLUnderline[0], 2, 0);
end;

procedure TFindHandler.RestoreMemory ();
begin
	with FFindHandler do
   begin
   	FindText := MemoryMan.FindText;
      DirUp := MemoryMan.DirUp;
      MatchCase := MemoryMan.MatchCase;
      WholeWord := MemoryMan.WholeWord;
      RollBack := MemoryMan.RollBack;
      AllowReplace := MemoryMan.AllowReplace;
      ReplaceText := MemoryMan.ReplaceText;
      HighlightMatch := false; //MemoryMan.HighlightMatch;
   end;
end;

procedure TFindHandler.SaveMemory ();
begin
	with MemoryMan do
   begin
   	FindText := FFindHandler.FindText;
      DirUp := FFindHandler.DirUp;
      MatchCase := FFindHandler.MatchCase;
      WholeWord := FFindHandler.WholeWord;
      RollBack := FFindHandler.RollBack;
      AllowReplace := FFindHandler.AllowReplace;
      ReplaceText := FFindHandler.ReplaceText;
      HighlightMatch := FFindHandler.HighlightMatch;
   end;
end;

procedure TFindHandler.Find ();
begin
	FFindBox := TFindBox.create (FEditor);
   FFindBox.FindHandler := FFindHandler;
   FFindBox.OnDialogClosed := f_OnFindBoxClosed;
	FFindBox.Open;
end;

procedure TFindHandler.f_OnFindBoxClosed (Sender: TObject);
begin
	FreeAndNil (FFindBox);
end;

function TFindHandler.CheckCommand (opr: word): boolean;
begin
	case opr of
   	m_find:
   		result := FFindBox = nil;
      m_subsequentfind, m_highlightmatch, m_findnext, m_findprevious:
         result := FFindHandler.CanFind;
      m_replace, m_replace_p, m_replaceall:
         result := FFindHandler.CanReplace;
      else
         result := true;
   end;
end;

procedure TFindHandler.ExecuteCommand (opr: word);
   procedure f_ReplaceAll ();
   var s, s_msg: widestring;
   begin
		if FFindHandler.rollback then
         s := ''
      else if FFindHandler.dirup then
         s := LangMan.GetItem (sr_upward, '向上的')
      else
         s := LangMan.GetItem (sr_downward, '向下的');
      s_msg := FormatStr (LangMan.GetItem(sr_replaceAllPrompt, '真的要用 "%0" 替换%1每一处 "%2" 吗？'), [FFindHandler.replacetext, s, FFindHandler.findtext]);
      if ShowMessage (s_msg, mtQuestion, LangMan.GetItem(sr_prompt)) = mrCancel then exit;

      if FFindHandler.ReplaceAll () then
         showmessage (LangMan.GetItem (sr_allreplaced, '所有目标文字已替换！'), mtInformation, LangMan.GetItem(sr_info))
      else
         showmessage (LangMan.GetItem (sr_noreplacefound, '未找到待替换文字！'), mtInformation, LangMan.GetItem(sr_info));
   end;
begin
	if not CheckCommand (opr) then exit;
	case opr of
      m_find:
			Find;
      m_findnext, m_findprevious:
      	begin
            if not FFindHandler.Find (opr = m_findprevious) then
               showmessage (LangMan.GetItem (sr_notargetfound, '未找到所要查找的文字！'), mtInformation, LangMan.GetItem(sr_info));
         end;
      m_replace, m_replace_p:
      	begin
      		FFindHandler.Replace ();
            if not FFindHandler.Find (opr = m_replace_p) then
            	showmessage (LangMan.GetItem (sr_noreplacefound, '未找到待替换文字！'), mtInformation, LangMan.GetItem(sr_info));
         end;
      m_replaceall:
			f_ReplaceAll;
      m_highlightmatch:
      	begin
         	CommandMan.SwitchCheck (m_highlightmatch);
         	FFindHandler.HighlightMatch := CommandMan.ItemChecked[m_highlightmatch];
         end;
   end;
end;

procedure TFindHandler.EventNotify (event, wparam, lparam: integer);
begin
	case event of
		e_HighlightSearchResult:
      	begin
         	if GSearchText <> '' then
               with FFindHandler do
               begin
                  findtext := GSearchText;
                  replacetext := '';
                  allowreplace := false;
                  matchcase := GMatchCase;
                  wholeword := false;
                  rollback := false;
                  dirup := false;
                  withinselrange := false;
                  highlightmatch := true;
               end
            else
            	FFindhandler.highlightmatch := false;
            CommandMan.ItemChecked[m_highlightmatch] := FFindhandler.highlightmatch;
         end;
   end;
end;

//-----------------------------

procedure TFindBox.OnInitialize ();
begin
	SetTemplate (Find_Box, m_find);
end;

const c_findbox: array[0..13] of word = (chk_allowreplace, chk_matchcase, chk_wholeword, rb_up, rb_down, chk_rollback, st_targettext, m_findnext, m_replace, m_replaceall, cb_exit, chk_withinselrange, chk_exitafterfirstmatch, chk_highlightmatch);

procedure TFindBox.OnOpen ();
var i: integer;
begin
	inherited;
   RefreshItemText (self, c_findbox, Find_Box);
   FindHistory := MemoryMan.FindHistory;
   ReplaceHistory := MemoryMan.ReplaceHistory;
   ItemChecked[chk_exitafterfirstmatch] := MemoryMan.ExitAfterFirstMatch;
   ItemChecked[chk_highlightmatch] := MemoryMan.HighlightMatch;

   FCmbReplaceHistory := TxlComboBox.create(self, ItemHandle[cmb_replacetext]);
   FCmbReplaceHistory.AllowEdit := true;
   for i := ReplaceHistory.Low to ReplaceHistory.High do
      FCmbReplaceHistory.Items.Add (ReplaceHistory[i]);
  	FCmbReplaceHistory.Text := FFindHandler.replacetext;

   ItemChecked[chk_allowreplace] := FFindHandler.allowreplace;
   ItemChecked[chk_matchcase] := FFindHandler.matchcase;
   ItemChecked[chk_wholeword] := FFindHandler.wholeword;
   ItemChecked[chk_rollback] := FFindHandler.rollback;
  	ItemChecked[rb_up] := FFindHandler.dirup;
  	ItemChecked[rb_down] := not FFindHandler.dirup;
   ItemChecked[chk_withinselrange] := FFindHandler.WithinSelRange;

   FCmbFindHistory := TxlComboBox.create(self, itemHandle[cmb_findtext]);
   FCmbFindHistory.AllowEdit := true;
   for i := FindHistory.Low to FindHistory.High do
      FCmbFindHistory.Items.Add (FindHistory[i]);
   FCmbFindHistory.Text := FFindHandler.findtext;

   f_CheckReplace;
   SendMessageW (ItemHandle[cmb_findtext], BM_SETSTYLE, BS_DEFPUSHBUTTON, 1);
   FocusControl (cmb_findtext);
end;

procedure TFindBox.OnClose ();
begin
   FCmbFindHistory.Free;
   FCmbReplaceHistory.Free;
   MemoryMan.ExitAfterFirstMatch := ItemChecked[chk_exitafterfirstmatch];
   MemoryMan.HighlightMatch := ItemChecked[chk_highlightmatch];
   inherited;
end;

procedure TFindBox.OnCommand (ctrlID: integer);
begin
	case CtrlID of
   	chk_allowreplace:
         begin
            f_CheckReplace;
            if ItemEnabled[cmb_replacetext] then
            	FocusControl (cmb_replacetext);
         end;
   	m_findnext:
      	begin
         	if ItemText[cmb_findtext] = '' then
            begin
            	showmessage (LangMan.GetItem (sr_findprompt, '请输入待查找文字！'), mtInformation, LangMan.GetItem(sr_info));
               FocusControl (cmb_findtext);
            end
            else
            begin
               f_RefreshCrit;
               if FFindHandler.Find (KeyPressed (VK_SHIFT)) then
               begin
                  if ItemChecked[chk_exitafterfirstmatch] then
                     close
                  else if FFindHandler.CanReplace then
                     ItemEnabled[m_replace] := true;
               end
               else
	               showmessage (LangMan.GetItem (sr_notargetfound, '未找到所要查找的文字！'), mtInformation, LangMan.GetItem(sr_info));
            end;
         end;
      m_replace:
      	begin
         	f_RefreshCrit;
            if KeyPressed (VK_SHIFT) then
               CommandMan.ExecuteCommand (m_replace_p)
            else
            	CommandMan.ExecuteCommand (m_replace);
         end;
      m_replaceall:
      	begin
         	f_RefreshCrit;
            CommandMan.ExecuteCommand (m_replaceall);
            if ItemChecked[chk_exitafterfirstmatch] then
               close;
         end;
      cb_exit:
			close;
   end;
end;

procedure TFindBox.f_CheckReplace ();
var b: boolean;
begin
	b := ItemChecked[chk_allowreplace];
   ItemEnabled[cmb_replacetext] := b;
  	ItemEnabled[m_replace] := b;
   ItemEnabled[m_replaceall] := b;
end;

procedure TFindBox.f_RefreshCrit ();
var s: widestring;
begin
	with FFindHandler do
   begin
      findtext := ItemText[cmb_findtext];
      replacetext := ItemText[cmb_replacetext];
      allowreplace := ItemChecked[chk_allowreplace];
      matchcase := ItemChecked[chk_matchcase];
      wholeword := ItemChecked[chk_wholeword];
      rollback := ItemChecked[chk_rollback];
      dirup := ItemChecked[rb_up];
      withinselrange := ItemChecked[chk_withinselrange];
      highlightmatch := ItemChecked[chk_highlightmatch];
   end;
   CommandMan.ItemChecked[m_highlightmatch] := ItemChecked[chk_highlightmatch];

   s := ItemText[cmb_findtext];
   FindHistory.DeleteByValue (s);
   FindHistory.InsertFirst(s);
   FCmbFindHistory.Items.Add (s);

   s := ItemText[cmb_replacetext];
   if (s <> '') then
   begin
      ReplaceHistory.DeleteByValue (s);
      ReplaceHistory.InsertFirst(s);
   end;
   FCmbReplaceHistory.Items.Add (s);
end;

end.


