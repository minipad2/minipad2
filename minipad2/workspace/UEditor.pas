unit UEditor;

interface

uses Windows, Messages, UxlRichEdit, Resource, richedit, UCalcHandler, UxlStrUtils, UPageSuper, UxlRichEditClasses,
	UDialogs, UxlFunctions, UClientSuper, UxlWinControl, UxlClasses, UxlExtClasses, UxlList, UTypeDef, UFindHandler;

type
	TSelMemo = class
   private
   	FBookMarks: TxlStrList;
      FTempList: TxlIntList;
   public
   	constructor Create ();
      destructor Destroy (); override;

   	procedure SetSel (id: integer; i_start, i_len, i_line: integer);
      procedure GetSel (id: integer; var i_start, i_len, i_line: integer);
   end;

   THLTextHandler = class (TxlInterfacedObject, IOptionObserver, ICommandExecutor)
   private
   	FEditor: TxlRichEdit;
      FULText: TxlRichEditHighlightTextBlock;
      FUL1, FUL2: widestring;
   public
   	constructor Create (AEditor: TxlRichEdit);
      destructor Destroy (); override;
      function CheckCommand (opr: word): boolean;
      procedure ExecuteCommand (opr: word);
      procedure OptionChanged ();
   end;

   TEditorClient = class (TClientSuper, IOptionObserver, IMemorizer, IEventObserver, IMessageObserver)
   private
      FWndParent: TxlWinControl;
      FEditor: TxlRichEdit;
      FSelMemo: TSelMemo;
      FFindHandler: TFindHandler;
      FCalcHandler: TCalcHandler;
      FLineNumber: TxlRichEditLineNumber;
      FHLSelLine: TxlRichEditHighlightSelLine;
      FHLTextHandler: THLTextHandler;

      procedure f_OnChange (Sender: TObject);
      procedure SetStatus ();
      function f_OnContextmenu (Sender: TObject): boolean;
		procedure f_RestoreScrollPos ();
      procedure f_SaveScrollPos ();
		procedure f_OnLink (const s_link: widestring);
   protected
		procedure Load (value: TPageSuper); override;
      procedure UnLoad (); override;
   public
      constructor Create (WndParent: TxlWinContainer);
      destructor Destroy (); override;
      function Control (): TxlControl; override;
      procedure Save (); override;

		procedure OnPageEvent (pct: TPageEvent; id, id2: integer); override;
      function CheckCommand (opr: word): boolean; override;
      procedure ExecuteCommand (opr: word); override;
      procedure OptionChanged ();
      procedure RestoreMemory ();
      procedure SaveMemory ();
      procedure EventNotify (event, wparam, lparam: integer);
      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
   end;

implementation

uses CommDlg, UxlCommDlgs, UxlMath, UxlWinClasses, UxlWinDef, UxlWindow, ShellAPI, UGlobalObj, UOptionManager, ULangManager, UExtFuncs,
	UxlListSuper, UPageStore, UPageFactory;

constructor TSelMemo.Create ();
begin
	FBookMarks := TxlStrList.Create (500);
   FBookMarks.IndexDeli := '=';
   FBookMarks.LoadFromFile (DataDir + 'minipad2.bmk');
   FBookMarks.SortType := stByIndexNoDup;

   FTempList := TxlIntList.Create (3);
   FTempList.Separator := ',';
end;

destructor TSelMemo.Destroy ();
begin
   FBookMarks.Free;
   FTempList.Free;
	inherited;
end;

procedure TSelMemo.SetSel (id: integer; i_start, i_len, i_line: integer);    // 不要试图记录 FEditor.ScrollPos!!!
var s: widestring;
begin
   s := IntToStr(i_start) + ',' + IntToStr(i_len) + ',' + IntToStr (i_line);
   if s <> FBookMarks.ItemsByIndex[id] then
   begin
      FBookMarks.ItemsByIndex[id] := s;
      FBookMarks.SaveToFile (DataDir + 'minipad2.bmk');
   end;
end;

procedure TSelMemo.GetSel (id: integer; var i_start, i_len, i_line: integer);
begin
	FTempList.Text := FBookMarks.ItemsByIndex[id];
	i_start := FTempList[0];
   i_len := FTempList[1];
   i_line := FTempList[2];
end;

//----------------------

constructor THLTextHandler.Create (AEditor: TxlRichEdit);
begin
	FEditor := AEditor;
   FULText := TxlRichEditHighlightTextBlock.Create (FEditor);
  	FULText.Enabled := true;
   CommandMan.AddExecutor (self);
   OptionMan.AddObserver (self);
end;

destructor THLTextHandler.Destroy ();
begin
	OptionMan.RemoveObserver (self);
   CommandMan.RemoveExecutor (self);
   FULText.free;
	inherited;
end;

procedure THLTextHandler.ExecuteCommand (opr: word);
	function f_IsRightChar (c2: widechar): boolean;
   var i: integer;
   begin
   	result := false;
		for i := 0 to 3 do
      	if c2 = FULText.Schemes[i].RightChar then
         begin
         	result := true;
            exit;
         end;
   end;
	procedure f_RemoveUnderline ();
	var i_start, i_len, i, i_start1, i_count: integer;
   	s1, s: widestring;
      i_pos1, i_pos2: integer;
   begin
   	FEditor.GetSel (i_start, i_len);
      s := FEditor.SelText;
      i_pos1 := -1;
      i_pos2 := -1;

      // 寻找最近的一个 LeftChar
      if (i_start > 0) and (FEditor.GetTextBlock (i_start - 1, 1) = FULText.LeftChar) then
         i_pos1 := i_start - 1
      else
      begin
         s1 := FEditor.GetTextBlock (i_start, i_len);
         for i := 1 to i_len do
         begin
            if (s1[i] = FULText.LeftChar) then
            begin
               i_pos1 := i + i_start - 1;
               break;
            end;
         end;
      end;
      if i_pos1 < 0 then
      begin
      	i_start1 := max(i_start - 2000, 0);
      	s1 := FEditor.GetTExtBlock (i_start1, i_start - i_start1);
      	for i := length(s1) downto 1 do
      		if (s1[i] = FULText.LeftChar) then
            begin
            	i_pos1 := i + i_start1 - 1;
               break;
            end;
         end;
      if i_pos1 < 0 then exit;

      // 寻找匹配的 RightChar
      i_count := 1;
      s1 := FEditor.GetTextBlock (i_pos1 + 1, max(i_len + i_start - i_pos1, 2000));
      for i := 1 to length(s1) do
      begin
      	if s1[i] = FULText.LeftChar then
         	inc (i_count)
         else if f_IsRightChar(s1[i]) then
         begin
         	dec (i_count);
            if i_count = 0 then
            begin
               i_pos2 := i + i_pos1;
               if (i_pos1 < i_start) and (i_pos2 < i_start) then exit;
               break;
            end;
         end;
      end;
      if i_pos2 < 0 then exit;

      FEditor.SetSel (i_pos1, i_pos2 - i_pos1 + 1);
      s := FEditor.SelText;
      s := MidStr (s, 2, length(s) - 2);
      FEditor.SelTExt := s;
      FEditor.SetSel (i_pos1, i_pos2 - i_pos1 - 1);
      FEditor.Redraw;
	end;
   
   procedure f_RemoveList ();
   begin
      FEditor.Indent (false, FUL1);
      FEditor.Indent (false, FUL2);
      FEditor.Indent (false);
	end;

   procedure f_addcr (var pr: pwidechar);
   begin
      pr^ := #13;
      inc (pr);
      pr^ := #10;
      inc (pr);
   end;

   procedure f_FilterEmptyLine (var s: widestring; b_oneemptyline: boolean);
   var i, j, n: integer;
      p, p2: pwidechar;
      c: widechar;
      b_hasbreak: boolean;
      s2: widestring;
   begin
      n := Length(s);
      if n = 0 then exit;

//      s2 := '';
      getmem (p, n*4+2);
      p2 := p;
      b_hasbreak := false;
      for i := 1 to n do
      begin
         c := s[i];
         if (c = #13) then
         begin
            b_hasbreak := true;
            s2 := '';
         end
         else if (c = #10) then
         begin
         end
         else if ((c = #9) or (c = #32)) and b_hasbreak then
         begin
            s2 := s2 + c;
         end
         else
         begin
            if b_hasbreak then
            begin
               f_addcr (p2);
               if b_oneemptyline then
                  f_addcr (p2);
               if s2 <> '' then
                  for j := 1 to length(s2) do
                  begin
                     p2^ := s2[j];
                     inc (p2);
                  end;
               b_hasbreak := false;
            end;
            p2^ := c;
            inc (p2);
         end;
      end;
      if b_oneemptyline then
         f_addcr (p2);
      p2^ := #0;

      s := p;
      freemem (p);

//      for i := n downto 1 do
//         if (s[i] = #13) and ((i = n) or (s[i+1] <> #10)) then
//            Insert (s, i + 1, #10);
//
//      n := Length(s);
//      for i := n - 3 downto 1 do
//         if (MidStr(s, i, 2) = #13#10) and (MidStr(s, i+2, 2) = #13#10) then
//            Delete (s, i+2, 2);
//      if LeftStr(s, 2) = #13#10 then
//         Delete (s, 1, 2);
//
//      if b_oneemptyline then
//      begin
//         n := Length(s);
//         for i := n - 2 downto 1 do
//            if s[i] = #13 then
//               Insert (s, i+2, #13#10);
//      end;
   end;
var i, i_start, i_len: integer;
	s: widestring;
begin
	if not CheckCommand (opr) then exit;
	case opr of
      m_highlight1, m_highlight2, m_highlight3, m_highlight4:
      	begin
            case opr of
            	m_highlight1: i := 0;
               m_highlight2: i := 1;
               m_highlight3: i := 2;
               m_highlight4: i := 3;
            end;
            FEditor.GetSel (i_start, i_len);
            s := FEditor.SelText;
            if RightStr (s, 1) = #13 then
            begin
            	dec (i_len, 1);
               FEditor.SetSel (i_start, i_len);
            end;
      		FEditor.SelTExt := FULText.LeftChar + FEditor.SelTExt + FULText.Schemes[i].RightChar;
            FEditor.SetSel (i_start + 1, i_len);
         end;
      m_removehighlight:
			f_RemoveUnderline;
      m_ul1:
      	begin
         	f_RemoveList;
	      	FEditor.Indent (true, FUL1);
         end;
      m_ul2:
      	begin
         	f_RemoveList;
	      	FEditor.Indent (true, FUL2);
         end;
      m_ol:
      	begin
         	f_RemoveList;
	      	FEditor.Indent (true);
         end;
      m_removelist:
      	f_RemoveList;
      m_noemptyline, m_oneemptyline:
         begin
            FEditor.GetSel (i_start, i_len);
            if i_len = 0 then
               FEditor.SelectAll;
            s := FEditor.SelText;
            f_FilterEmptyLine (s, opr = m_oneemptyline);
            FEditor.SelText := s;
         end;
   end;
end;

function THLTextHandler.CheckCommand (opr: word): boolean;
begin
	case opr of
   	m_highlight1, m_highlight2, m_highlight3, m_highlight4:
      	result := FEditor.CanCut;
      m_ul1, m_ul2, m_ol, m_removelist:
      	result := FEditor.CanIndent;
//      m_noemptyline, m_oneemptyline:
//         result := FEditor.TextCount > 0;
      else
      	result := true;
   end;
end;

procedure THLTextHandler.OptionChanged ();
var sc: TULScheme;
	i: integer;
begin
   for i := 1 to 4 do     // 共四套方案
   begin
      if i = 4 then
         sc.RightChar := #127
      else
         sc.RightChar := widechar (i + 28);
      sc.Color := OptionMan.Options.HLColor[i];
      if OptionMan.Options.HLUnderline[i] then
      	sc.Thickness := 2
      else
      	sc.Thickness := 0;
   	FULText.Schemes[i - 1] := sc;
   end;
   FUL1 := OptionMan.Options.UL1;
   FUL2 := OptionMan.Options.UL2;
end;

//----------------------

constructor TEditorClient.Create (WndParent: TxlWinContainer);
begin
   FWndParent := WndParent;

   FEditor := TxlRichEdit.Create (WndParent);
	FEditor.OnChange := f_OnChange;
   FEditor.OnContextMenu := f_OnContextMenu;
   FEditor.OnLink := f_OnLink;

   FLineNumber := TxlRichEditLineNumber.Create (FEditor);
   FLineNumber.Font := FEditor.Font;

   FHLSelLine := TxlRichEditHighlightSelLine.Create (FEditor);
   FHLtextHandler := THLTextHandler.Create (FEditor);

   FFindHandler := TFindHandler.Create (FEditor);
   FCalcHandler := TCalcHandler.Create (FEditor);
   FSelMemo := TSelMemo.Create;

   OptionMan.AddObserver(self);
   MemoryMan.AddObserver (self);
   EventMan.AddObserver (self);
   MainWindow.AddMessageObserver (self);
end;

destructor TEditorClient.Destroy ();
begin
	MainWindow.RemoveMessageObserver (self);
	EventMan.RemoveObserver(self);
	MemoryMan.RemoveObserver (self);
	OptionMan.RemoveObserver(self);

	FHLtextHandler.Free;
   FHLSelLine.free;
   FLineNumber.free;
   FSelMemo.Free;
   FCalcHandler.Free;
   FFindHandler.Free;
   FEditor.Free;
   inherited;
end;

function TEditorClient.Control (): TxlControl;
begin
	result := FEditor;
end;

procedure TEditorClient.f_OnChange (Sender: TObject);
begin
	CommandMan.CheckCommands;
end;

//----------------------

procedure TEditorClient.Load (value: TPageSuper);
begin
	if value = nil then exit;
   inherited Load (value);
	FEditor.Text := value.Text;
   SetStatus ();
	FCalcHandler.PageType := value.PageType;

	MainWindow.Post (WM_RESTORESCROLLPOS, 0, 0);
   if OptionMan.Options.AlwaysFocusEditor then
   	MainWindow.Post (WM_FOCUSEDITOR, 0, 0);
end;

procedure TEditorClient.UnLoad ();
begin
	FEditor.Text := '';
end;

procedure TEditorClient.f_RestoreScrollPos ();
var i_selstart, i_sellength, i_line: integer;
  i_start, i_end: cardinal;
 sc: TPoint;
begin
   if not OptionMan.Options.RememberPageScroll then exit;
   FSelMemo.GetSel (FPage.id, i_selstart, i_sellength, i_line);
//   if OptionMan.Options.BmkRule = 0 then   // 适合于绝大部分情况。尝试恢复 FEditor.ScrollPos 会导致大文本下无法恢复位置bug!!!
      FEditor.FirstVisibleLine := i_line;
//   else
//   begin      // 3.1.6 的算法。可能适合于繁体中文版
//      sc.x := 0;
//      sc.y := i_line;
//      FEditor.ScrollPos := sc;
//   end;
//         	FEditor.ScrollPos := FEditor.ScrollPos;

   FEditor.GetVisibleTextRange (i_start, i_end);
   if InRange (i_selstart, i_start, i_end) and (i_selstart + i_sellength <= i_end) then
      FEditor.SetSel(i_SelStart, i_SelLength)
   else
   	FEditor.SetSel (i_start, 0);
end;

procedure TEditorClient.f_SaveScrollPos ();
var i_selstart, i_sellength, i_line: integer;
begin
   if not OptionMan.Options.RememberPageScroll then exit;
  	FEditor.GetSel (i_selstart, i_sellength);
   if OptionMan.Options.BmkRule = 0 then
      i_line := FEditor.FirstVisibleLine
   else
      i_line := FEditor.ScrollPos.y;
   FSelMemo.SetSel (FPage.id, i_selstart, i_sellength, i_line);
end;

procedure TEditorClient.Save ();
begin
  	FPage.Text := FEditor.Text;
   f_SaveScrollPos;
   inherited;
end;

procedure TEditorClient.SetStatus ();
begin
	FEditor.Protected := (FPage.Status = psProtected);
   FEditor.ReadOnly := (FPage.Status = psReadOnly);
end;

procedure TEditorClient.OptionChanged ();
begin
   with FEditor do
   begin
      UndoLimit := OptionMan.Options.undolimit;
      TabStops := OptionMan.Options.tabstops;
      AutoIndent := OptionMan.Options.autoindent;
      AutoEmptyLine := OptionMan.Options.autoemptyline;
      Font := OptionMan.Options.editorfont;
      Color := OptionMan.Options.editorcolor;
      LeftMargin := OptionMan.Options.Margins * 4 + 2;
      RightMargin := OptionMan.Options.Margins * 4 + 2;
      SmoothScroll := OptionMan.Options.SmoothScroll;
      OneClickOpenLink := OptionMan.Options.OneClickOpenLink;
   end;

   FLineNumber.Color := OptionMan.Options.LineNumberColor;
   FLineNumber.Enabled := OptionMan.Options.showlinenumber;
   FHLSelLine.Color := Optionman.Options.SelLineColor;
   FHLSelLine.Enabled := OptionMan.Options.HighLightSelLine;
end;

procedure TEditorClient.RestoreMemory ();
begin
  	FEditor.WordWrap := MemoryMan.WordWrap;
   CommandMan.ItemChecked [m_wordwrap] := FEditor.WordWrap;
end;

procedure TEditorClient.SaveMemory ();
begin
	MemoryMan.WordWrap := FEditor.WordWrap;
end;

//---------------------

procedure TEditorClient.OnPageEvent (pct: TPageEvent; id, id2: integer);
begin
	case pct of
		pctSwitchStatus:
      	if id = FPage.id then
      		SetStatus ();
      else
         inherited OnPageEvent (pct, id, id2);
   end;
end;

function TEditorClient.CheckCommand (opr: word): boolean;
begin
   with FEditor do       // 不要进行任何牵涉到 IsEmpty 的判断，包括 CanClear！否则严重降低大文本下的执行效率！
   begin
      case opr of
         m_undo:
            result := CanUndo ();
         m_redo:
            result := CanRedo ();
         m_cut, m_delete:
            result := CanCut ();
         m_copy:
            result := CanCopy ();
         m_paste:
            result := CanPaste ();
         m_wordwrap:
            result := true;
         m_find, m_subsequentfind, m_findnext, m_findprevious, m_replace, m_replace_p, m_replaceall, m_highlightmatch:
            result := FFindHandler.CheckCommand (opr);
         else
         	result := true;
      end;
   end;
end;

procedure TEditorClient.ExecuteCommand (opr: word);
var s: widestring;
   o_box: TInsertLinkBox;
begin
	if not CheckCommand (opr) then exit;
   case opr of
      m_wordwrap:
      	begin
         	f_SaveScrollPos;
         	FEditor.WordWrap := not FEditor.WordWrap;
            CommandMan.ItemChecked [m_wordwrap] := FEditor.WordWrap;
            f_RestoreScrollPos;
         end;
      m_clear:
      	begin
         	if OptionMan.Options.ConfirmClear and (ShowMessage (LangMan.GetItem(sr_clearprompt), mtQuestion,
               	LangMan.GetItem(sr_prompt)) = mrCancel) then exit;
      		FEditor.clear ();
         end;
      m_delete: FEditor.DeleteSel;
      m_undo: FEditor.undo ();
      m_redo: FEditor.redo ();
      m_selectall: FEditor.selectall ();
      m_cut: FEditor.cut();
      m_copy: FEditor.copy ();
      m_paste: FEditor.paste ();
      m_find, m_subsequentfind, m_findnext, m_findprevious, m_replace, m_replace_p, m_replaceall, m_highlightmatch:
      	FFindHandler.ExecuteCommand (opr);
      m_insertlink:
      	begin
      		o_box := TInsertLinkBox.Create;
         	if o_box.Execute then
            begin
            	FEditor.SetFocus;
            	FEditor.SelText := o_box.LinkText;
            end;
            o_box.free;
         end;
      hk_selastitle:
         begin
         	s := FEditor.SelText;
            if s = '' then exit;
            FPage.Name := MultiLineToSingleLine (LeftStr(s, 200));
			end;
      else
        	exit;
   end;
   f_onchange (self);
end;

function TEditorClient.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
var pt: TPoint;
begin
	if not FEditor.visible then
   	b_processed := false
   else
   begin
   	b_processed := true;
		case AMessage of
         WM_FOCUSEDITOR:
            begin
               FEditor.SetFocus;
               if FEditor.CursorInWindow and KeyPressed(VK_LBUTTON) then   // 不可省略！否则不能立即选中文字！
               begin
                  pt := FEditor.CursorPos;
                  FEditor.Post (WM_LBUTTONDOWN, 0, Makelparam(pt.x, pt.y));
               end;
            end;
         WM_RESTORESCROLLPOS:
            f_RestoreScrollPos;
         else
         	b_processed := false;
      end;
   end;
end;

procedure TEditorClient.EventNotify (event, wparam, lparam: integer);
var s: widestring;
	n: integer;
{$J+}const cs_settings: widestring = ''; {$J-}
begin
	if not FEditor.Visible then exit;
   case event of
//      e_WinStatusChanged:
//      	if wparam in [p_maximized] then
//         	FEditor.FirstVisibleLine := FEditor.FirstVisibleLine;  // 防止出现最大化后滚动超过页面底部的现象。
      e_GetEditorSelText:
         EventMan.Message := IfThen (FEditor.Visible, FEditor.SelText, '');
   	e_AppendEditorText:
         begin
         	s := EventMan.Message;
            n := FEditor.TextCount;
            if n > 0 then
            	s := #13#10 + DecodeTemplate(OptionMan.Options.SepLine) + #13#10 + s;
            FEditor.SetSel (n, 0);
            FEditor.SelText := s;
				FEditor.SetSel (FEditor.TextCount, 0);
         end;
   end;
end;

function TEditorClient.f_OnContextmenu (Sender: TObject): boolean;
var i_start, i_len: integer;
begin
   result := false;
	FEditor.GetSel (i_start, i_len);
	if i_len > 0 then
		EventMan.EventNOtify (e_ContextMenuDemand, EditorContext)
   else
		EventMan.EventNOtify (e_ContextMenuDemand, EditorNoSelContext);
end;

procedure TEditorClient.f_OnLink (const s_link: widestring);
   procedure f_OnUserLink (s_link: widestring);
   var p: TPageSuper;
      idroot: integer;
      pt: TPoint;
   begin
      if LeftStr (s_link, 1) = '\' then
      begin
         PageStore.GetFirstPageId (ptGroupRoot, idroot);
         s_link := PageStore[idroot].Name + s_link;
      end
      else if LeftStr (s_link, 3) = '..\' then
         s_link := PageStore.GetPagePath (FPage.Owner) + MidStr (s_link, 3)
      else if LeftStr (s_link, 1) = '@' then
         s_link := PageStore.GetPagePath (FPage.Owner) + '\' + MidStr (s_link, 2);

      if PageStore.FindPageByPath (s_link, p) then
      begin
         // 防止双击造成文字选中的结果
         pt.x := -1;
         pt.y := -1;
         ClientToScreen (FEditor.handle, pt);
         SetCursorPos (pt.x, pt.y);

         PageCenter.ActivePage := p;
      end
      else
         ProgTip.ShowTip (LangMan.GetItem(sr_invalidnodelink) + ': ' + s_link);
   end;
var s: widestring;
begin
   s := Trim(s_link);
   if IsSameStr(LeftStr(s, 8), 'outlook:') then
   begin
      s := MidStr (s, 9);
      while (length(s) > 0) and (s[1] = '/') do
         s := MidStr(s, 2);
      s := ReplaceStr (s, '/', '\');
      f_OnUserLink (s);
   end
   else
   begin
      if IsSameStr (LeftStr(s, 5), 'file:') then
         s := RelToFullPath (s, ProgDir);
      try
         ExecuteLink (s);
      except
      end;
   end;
end;

end.

