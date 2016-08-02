unit UOpAppearance;

interface

uses UOptionBox, UCommPanels, UxlComboBox, UOptionManager;

type
   TOpAppearance = class (TOptionPanel)
   private
      FCmbLanguage, FCmbAdjustLevel, FCmbTransparency, FCmbSplitterWidth: TxlcomboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpTreeView = class (TOptionPanel)
   private
   	FTreeFontDemo: TFontColorPanel;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpTabControl = class (TOptionPanel)
   private
   	FTabFontDemo: TFontColorPanel;
 		FTabWidth: TxlComboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
		procedure OnCommand (ctrlID: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpListView = class (TOptionPanel)
   private
   	FListFontDemo, FBlogFontDemo, FBlogTitleFontDemo, FBlogSelTitleFontDemo: TFontColorPanel;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpEditor = class (TOptionPanel)
   private
      FEditorFontDemo, FLineNumberColorDemo, FSelLineColorDemo, FHLColorDemo: TFontColorPanel;
      FHLList: TxlListBox;
      FCmbMargins: TxlComboBox;
      FOptions: TOptions;
      procedure f_OnHLListSelChange (Sender: TObject);
		procedure f_OnHLColorChange (Sender: TObject);
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
		procedure OnCommand (ctrlID: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpOtherControls = class (TOptionPanel)
   private
   	FMininoteFontDemo: TFontColorPanel;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

implementation

uses Windows, ULangManager, UxlCommDlgs, UxlFunctions, UxlMath, UTypeDef, UxlStrUtils, UxlList, UGlobalObj, Resource;

procedure TOpAppearance.OnInitialize ();
begin
	SetTemplate (ob_appearance);
end;

const c_OpAppearance: array[0..8] of word = (st_language, chk_toolwindowstyle, chk_captionpagename, chk_showmenubar, chk_showtoolbar,
   st_transparency, chk_autoadjust, st_sensitivity, st_splitterwidth);

procedure TOpAppearance.OnOpen ();
var i: integer;
	o_list: TxlStrList;
begin
	RefreshItemText (self, c_OpAppearance);

   o_list := TxlStrList.Create;
   FindFiles (LangDir + '*.lng', o_list, false);
   FCmbLanguage := TxlComboBox.create(self, ItemHandle[cmb_language]);
   with FCmbLanguage.Items do
   begin
   	Add ('Chinese');
      Add ('English');
      for i := o_list.Low to o_list.High do
         if (not IsSameStr(o_list[i], 'Chinese')) and (not IsSameStr(o_list[i], 'English')) then
            Add (o_list[i]);
   end;
   o_list.free;

   FCmbTransparency := TxlComboBox.create(self, ItemHandle[cmb_transparency]);
   for i := 1 to 10 do
      FCmbTransparency.Items.Add (i);

   FCmbAdjustLevel := TxlComboBox.create(self, ItemHandle[cmb_adjustlevel]);
   for i := 1 to 5 do
      FCmbAdjustLevel.Items.Add (i);

   FCmbSplitterWidth := TxlComboBox.Create (self, ItemHandle[cmb_splitterwidth]);
   for i := 1 to 10 do
      FCmbSplitterWidth.Items.Add (i);
end;

procedure TOpAppearance.OnClose ();
begin
   FCmbTransparency.free;
   FCmbLanguage.free;
   FCmbAdjustLevel.free;
   FCmbSplitterWidth.free;
end;

procedure TOpAppearance.Load (const op: TOptions);
begin
   FCmbLanguage.Text := op.Language;
   ItemChecked[chk_toolwindowstyle] := op.ToolWindowStyle;
   ItemChecked[chk_captionpagename] := op.captionpagename;
   ItemChecked[chk_showmenubar] := op.showmenubar;
   ItemChecked[chk_showtoolbar] := op.ShowToolBar;
   FCmbtransparency.Items.SelIndex := op.Transparency - 1;
   ItemChecked[chk_autoadjust] := op.AutoAdjust;
   FCmbadjustlevel.Items.SelIndex := op.AutoAdjustLevel - 1;
   ItemEnabled[cmb_adjustlevel] := ItemChecked[chk_autoadjust];
   FCmbSplitterWidth.Items.SelIndex := op.SplitterWidth - 1;
end;

procedure TOpAppearance.Save (var op: TOptions);
begin
	with op do
   begin
      language := FCmblanguage.Text;
      ToolWindowStyle := ItemChecked[chk_toolwindowstyle];
      Captionpagename := ItemChecked[chk_captionpagename];
      ShowMenuBar := ItemChecked[chk_showmenubar];
   	ShowToolBar := ItemChecked[chk_showtoolbar];

      AutoAdjust := ItemChecked[chk_autoadjust];
      AutoAdjustLevel := FCmbadjustlevel.Items.SelIndex + 1;
   	Transparency := FCmbtransparency.Items.SelIndex + 1;
      SplitterWidth := FCmbSplitterWidth.Items.SelIndex + 1;
   end;
end;

procedure TOpAppearance.OnCommand (ctrlID: integer);
begin
	case ctrlID of
      chk_autoadjust:
         ItemEnabled[cmb_adjustlevel] := ItemChecked[chk_autoadjust];
	end;
end;

//-----------------------------

const c_OpTreeView: array[0..8] of word = (st_tree, cb_settreefont, cb_settreecolor, chk_treehorzscroll, chk_linesatroot,
	chk_nodebuttons, chk_nodelines, chk_shownodeimages, chk_treeonright);

procedure TOpTreeView.OnInitialize ();
begin
	SetTemplate (ob_treeview);
end;

procedure TOpTreeView.OnOpen ();
begin
	RefreshItemText (self, c_OpTreeView);
   FTreeFontDemo := TFontColorPanel.create (self, ItemHandle[st_treefontdemo], ItemHandle[cb_settreefont], ItemHandle[cb_settreecolor]);
end;

procedure TOpTreeView.OnClose ();
begin
   FTreeFontDemo.free;
end;

procedure TOpTreeView.Load (const op: TOptions);
begin
   FTreeFontDemo.Font := op.TreeFont;
   FTreeFontDemo.Color := op.TreeColor;

   ItemChecked[chk_treehorzscroll] := op.TreeHorzScroll;
   ItemChecked[chk_nodebuttons] := op.NodeButtons;
   ItemChecked[chk_nodelines] := op.NodeLines;
   ItemChecked[chk_linesatroot] := op.LinesAtRoot;
   ItemChecked[chk_shownodeimages] := op.ShowNodeImages;
   ItemChecked[chk_treeonright] := op.TreeOnRight;
end;

procedure TOpTreeView.Save (var op: TOptions);
begin
	with op do
   begin
      TreeHorzScroll := Itemchecked[chk_treehorzscroll];
      NodeButtons := ItemChecked[chk_nodebuttons];
      NodeLines := ItemChecked[chk_nodelines];
      LinesAtRoot := ItemChecked[chk_linesatroot];
      ShowNodeImages := ItemChecked[chk_shownodeimages];
      TreeOnRight := ItemChecked[chk_treeonright];

      TreeFont.assign (FTreeFontDemo.Font);
      TreeColor := FTreeFontDemo.Color;
   end;
end;

//------------------------

procedure TOpTabControl.OnInitialize ();
begin
	SetTemplate (ob_tabcontrol);
end;

procedure TOpTabControl.OnOpen ();
const c_OpTabControl: array[0..6] of word = (st_tabpage, cb_settabfont, chk_showtabimages, chk_tabonbottom, chk_multilinetab,
	chk_fixedtabwidth, chk_highlightcurrenttab);
var i: integer;
begin
  	RefreshItemText (self, c_OpTabControl);

   FTabWidth := TxlComboBox.create(self, ItemHandle[cmb_tabwidth]);
   for i := 1 to 10 do
      FTabWidth.Items.add (i);

   FTabFontDemo := TFontColorPanel.Create (self, ItemHandle[st_tabfontdemo], ItemHandle[cb_settabfont], 0);
end;

procedure TOpTabControl.OnClose ();
begin
   FTabWidth.free;
	FTabFontDemo.free;
end;

procedure TOpTabControl.Load (const op: TOptions);
begin
   ItemChecked[chk_showtabimages] := op.ShowTabImages;
   ItemChecked[chk_tabonbottom] := op.TabOnBottom;
   ItemChecked[chk_multilinetab] := op.multilinetab;
   ItemChecked[chk_fixedtabwidth] := (op.tabwidth <> 0);
   ItemChecked[chk_highlightcurrenttab] := op.HighlightCurrentTab;

   FTabWidth.enabled := ItemChecked[chk_fixedtabwidth];
   if op.tabwidth <> 0 then
      FTabWidth.Text := IntToStr(op.tabwidth)
   else
      FTabWidth.text := '5';

   FTabFontDemo.Font := op.TabFont;
end;

procedure TOpTabControl.Save (var op: TOptions);
begin
	with op do
   begin
      ShowTabImages := ItemChecked[chk_showtabimages];
      TabOnBottom := ItemChecked[chk_tabonbottom];
      multilinetab := ItemChecked[chk_multilinetab];
      HighlightCurrentTab := ItemChecked[chk_highlightcurrenttab];
   end;

   if not ItemChecked[chk_fixedtabwidth] then
      op.tabwidth := 0
   else
      op.tabwidth := StrToInt(Ftabwidth.text);

   op.TabFont.assign (FTabFontDemo.Font);
end;

procedure TOpTabControl.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	chk_fixedtabwidth:
      	ItemEnabled[cmb_tabwidth] := ItemChecked[chk_fixedtabwidth];
	end;
end;

//---------------------------------

const c_OpListView: array[0..11] of word = (st_list, cb_setlistfont, cb_setlistcolor, st_blog, cb_setblogfont, cb_setblogcolor,
   st_blogtitle, cb_setblogtitlefont, cb_setblogtitlecolor, st_blogseltitle, cb_setblogseltitlefont, cb_setblogseltitlecolor);

procedure TOpListView.OnInitialize ();
begin
   SetTemplate (ob_listview);
end;

procedure TOpListView.OnOpen ();
begin
	RefreshItemText (self, c_OpListView);

   FListFontDemo := TFontColorPanel.create (self, ItemHandle[st_listfontdemo], ItemHandle[cb_setlistfont], ItemHandle[cb_setlistcolor]);
   FBlogFontDemo := TFontColorPanel.create (self, ItemHandle[st_blogfontdemo], ItemHandle[cb_setblogfont], ItemHandle[cb_setblogcolor]);
   FBlogTitleFontDemo := TFontColorPanel.create (self, ItemHandle[st_blogtitlefontdemo], ItemHandle[cb_setblogtitlefont], ItemHandle[cb_setblogtitlecolor]);
   FBlogSelTitleFontDemo := TFontColorPanel.create (self, ItemHandle[st_blogseltitlefontdemo], ItemHandle[cb_setblogseltitlefont], ItemHandle[cb_setblogseltitlecolor]);
end;

procedure TOpListView.OnClose ();
begin
   FListFontDemo.free;
   FBlogFontDemo.free;
   FBlogTitleFontDemo.free;
   FBlogSelTitleFontDemo.free;
end;

procedure TOpListView.Load (const op: TOptions);
begin
   FListFontDemo.Font := op.ListFont;
   FListFontDemo.Color := op.ListColor;

   FBlogFontDemo.Font := op.BlogFont;
   FBlogFontDemo.Color := op.BlogColor;

   FBlogTitleFontDemo.Font := op.BlogTitleFont;
   FBlogTitleFontDemo.Color := op.BlogTitleColor;

   FBlogSelTitleFontDemo.Font := op.BlogSelTitleFont;
   FBlogSelTitleFontDemo.Color := op.BlogSelTitleColor;
end;

procedure TOpListView.Save (var op: TOptions);
begin
   op.ListFont.assign (FListFontDemo.Font);
   op.ListColor := FListFontDemo.Color;

   op.BlogFont.assign (FBlogFontDemo.Font);
   op.BlogColor := FBlogFontDemo.Color;

   op.BlogTitleFont.assign (FBlogTitleFontDemo.Font);
   op.BlogTitleColor := FBlogTitleFontDemo.Color;

   op.BlogSelTitleFont.assign (FBlogSelTitleFontDemo.Font);
   op.BlogSelTitleColor := FBlogSelTitleFontDemo.Color;
end;

//-------------------------------

const c_OpEditor: array[0..10] of word = (chk_showlinenumber, cb_setlinenumbercolor, chk_highlightselline, cb_setsellinecolor,
	st_margins, st_editor, cb_seteditorfont, cb_seteditorcolor, chk_useunderline, st_HLText, cb_HLColor);

procedure TOpEditor.OnInitialize ();
begin
	SetTemplate (ob_editor);
end;

procedure TOpEditor.OnOpen ();
var i: integer;
begin
	RefreshItemText (self, c_OpEditor);

   FEditorFontDemo := TFontColorPanel.create (self, ItemHandle[st_editorfontdemo], ItemHandle[cb_seteditorfont], ItemHandle[cb_seteditorcolor]);

   FCmbMargins := TxlComboBox.Create (self, ItemHandle[cmb_margins]);
   for i := 0 to 10 do
      FCmbMargins.Items.Add (i);

   FLineNumberColorDemo := TFontColorPanel.Create (self, ItemHandle[st_linenumberdemo], 0, ItemHandle[cb_setlinenumbercolor]);
   FSelLineColorDemo := TFontColorPanel.Create (self, ItemHandle[st_sellinedemo], 0, ItemHandle[cb_setsellinecolor]);

   FHLColorDemo := TFontColorPanel.Create (self, ItemHandle[st_HLdemo], 0, ItemHandle[cb_HLColor]);
   FHLColorDemo.OnColorChange := f_OnHLColorChange;
   
   FHLList := TxlListBox.Create (self, ItemHandle[lst_hltext]);
   with FHLList.Items do
   begin
   	Add (LangMan.GetItem (m_highlightmatch));
      Add (LangMan.GetItem (m_highlight1));
      Add (LangMan.GEtItem (m_highlight2));
      Add (LangMan.GetItem (m_highlight3));
      Add (LangMan.GetItem (m_highlight4));
      OnSelChange := f_OnHLListSelChange;
      SelIndex := 0;
   end;
end;

procedure TOpEditor.OnClose ();
begin
   FEditorFontDemo.free;
   FLineNumberColorDemo.free;
   FCmbMargins.free;
   FSelLineColorDemo.free;
   FHLColorDemo.free;
   FHLList.free;
end;

procedure TOpEditor.Load (const op: TOptions);
begin
   FEditorFontDemo.Font := op.EditorFont;
   FEditorFontDemo.Color := op.EditorColor;

   FCmbmargins.Items.SelIndex := op.margins;
   ItemChecked[chk_showlinenumber] := op.showlinenumber;
   FLineNumberColorDemo.Color := op.LineNumberColor;
   ItemChecked[chk_highlightselline] := op.HighLightSelLine;
   FSelLineColorDemo.Color := op.SelLineColor;

   FOptions := op;
   f_OnHLListSelChange (self);
end;

procedure TOpEditor.Save (var op: TOptions);
begin
	with FOptions do
   begin
   	Margins := Fcmbmargins.Items.SelIndex;

   	Showlinenumber := ItemChecked[chk_showlinenumber];
   	HighLightSelLine := ItemChecked[chk_highlightselline];

      LineNumberColor := FLineNumberColorDemo.Color;
      SelLineColor := FSelLineColorDemo.Color;

      EditorFont.assign (FEditorFontDemo.Font);
      EditorColor := FEditorFontDemo.Color;
	end;
   op := FOptions;
end;

procedure TOpEditor.OnCommand (ctrlID: integer);
begin
	if ctrlID = chk_useunderline then
   	FOptions.HLUnderline[FHLList.Items.SelIndex] := ItemChecked[chk_useunderline];
end;

procedure TOpEditor.f_OnHLListSelChange (Sender: TObject);
var i: integer;
begin
	i := FHLList.Items.SelIndex;
	FHLColorDemo.Color := FOptions.HLColor[i];
   ItemChecked[chk_useunderline] := FOptions.HlUnderline[i];
end;

procedure TOpEditor.f_OnHLColorChange (Sender: TObject);
var i: integer;
begin
	i := FHLList.Items.SelIndex;
	FOptions.HLColor[i] := FHLColorDemo.Color;
end;

//--------------------------------------

const c_OpOtherControls: array[0..2] of word = (st_mininote, cb_setmininotefont, cb_setmininotecolor);

procedure TOpOtherControls.OnInitialize ();
begin
   SetTemplate (ob_othercontrols);
end;

procedure TOpOtherControls.OnOpen ();
begin
	RefreshItemText (self, c_OpOtherControls);

   FMininoteFontDemo := TFontColorPanel.create (self, ItemHandle[st_mininotefontdemo], ItemHandle[cb_setmininotefont], ItemHandle[cb_setmininotecolor]);
end;

procedure TOpOtherControls.OnClose ();
begin
   FMininoteFontDemo.free;
end;

procedure TOpOtherControls.Load (const op: TOptions);
begin
   FMininoteFontDemo.Font := op.MininoteFont;
   FMininoteFontDemo.Color := op.MininoteColor;
end;

procedure TOpOtherControls.Save (var op: TOptions);
begin
   op.MiniNoteFont.assign (FMiniNoteFontDemo.Font);
   op.MiniNoteColor := FMininoteFontDemo.Color;
end;

end.
