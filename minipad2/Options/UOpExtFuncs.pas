unit UOpExtFuncs;

interface

uses UOptionBox, UxlComboBox, UCommPanels, UxlList, UxlMiscCtrls, UOptionManager;

type
   TOpExtFuncs = class (TOptionPanel)
   private
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpCalcPage = class (TOptionPanel)
   private
   	FDecimal: TxlComboBox;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpDictPage = class (TOptionPanel)
   private
      FSelectPanel: TSelectPanel;
      FDictFiles: TxlStrList;
      FCaptureHotkey: TxlHotKey;
      FColorDemo: TFontColorPanel;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpLinkPage = class (TOptionPanel)
   private
   	FFastLinkHotKey: TxlHotKey;
		procedure f_OnHotKeyChange (Sender: TObject);
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpTemplate = class (TOptionPanel)
   private
   	FTemplateHotKey: TxlHotKey;
      FCaptureHotKey: TxlHotKey;
		procedure f_OnHotKeyChange (Sender: TObject);
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;

   TOpClipboard = class (TOptionPanel)
   private
   	FClipboardHotKey: TxlHotKey;
   	FMaxClipNum, FMaxItemByte, FMenuWidth: TxlComboBox;
		procedure f_OnHotKeyChange (Sender: TObject);
      procedure f_CheckEnableFilter ();
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlId: integer); override;
   public
      procedure Load (const op: TOptions); override;
      procedure Save (var op: TOptions); override;
   end;
   
implementation

uses Windows, ULangManager, UClipHandler, UxlCommDlgs, UxlFunctions, UxlMath, UTypeDef, UxlStrUtils, UxlFile, UGlobalObj, Resource;

const c_OpExtFuncs: array[0..7] of word = (chk_calcpage, chk_memopage, chk_dictpage, chk_linkpage, chk_contactpage, chk_template, chk_clipboard, st_extfuncsprompt);

procedure TOpExtFuncs.OnInitialize ();
begin
	SetTemplate (ob_extfuncs);
end;

procedure TOpExtFuncs.OnOpen ();
begin
	RefreshItemText (self, c_OpExtFuncs);
end;

procedure TOpExtFuncs.Load (const op: TOptions);
begin
	ItemChecked[chk_calcpage] := op.enablecalcpage;
   ItemChecked[chk_memopage] := op.enablememopage;
   ItemChecked[chk_dictpage] := op.enabledictpage;
   ItemChecked[chk_linkpage] := op.enablelinkpage;
   ItemChecked[chk_contactpage] := op.enablecontactpage;
   ItemChecked[chk_template] := op.enabletemplate;
   ItemChecked[chk_clipboard] := op.enableclipboard;
end;

procedure TOpExtFuncs.Save (var op: TOptions);
begin
	with op do
   begin
   	enablecalcpage := Itemchecked[chk_calcpage];
      enablememopage := ItemChecked[chk_memopage];
      enabledictpage := Itemchecked[chk_dictpage];
      enablelinkpage := Itemchecked[chk_linkpage];
      enablecontactpage := Itemchecked[chk_contactpage];
      enabletemplate := Itemchecked[chk_template];
      enableclipboard := Itemchecked[chk_clipboard];
   end;
end;

//-----------------------------

procedure TOpCalcPage.OnInitialize ();
begin
	SetTemplate (ob_calcpage);
end;

const c_OpCalcPage: array[0..4] of word = (st_decimal, st_radianordegree, rb_radian, rb_degree, st_userfunctions);

procedure TOpCalcPage.OnOpen ();
var i: integer;
begin
	RefreshItemText (self, c_OpCalcPage);

   FDecimal := TxlComboBox.create (self, ItemHandle[cmb_decimal]);
   for i := 0 to 12 do
      FDecimal.Items.add (i);
   FDecimal.Items.add ('Float');
end;

procedure TOpCalcPage.OnClose ();
begin
   FDecimal.free;
end;

procedure TOpCalcPage.Load (const op: TOptions);
begin
   if op.CalcOptions.decimal in [0..12] then
      FDecimal.text := IntToStr(op.CalcOptions.decimal)
   else
      FDecimal.text := 'Float';

   if op.CalcOptions.Tri_Degree then
      ItemChecked[rb_degree] := true
   else
      ItemChecked[rb_radian] := true;

   ItemText[mle_userfunctions] := op.UserFuncs.Text;
end;

procedure TOpCalcPage.Save (var op: TOptions);
var i: integer;
begin
   if FDecimal.text = 'Float' then
      op.CalcOptions.decimal := -1
   else
      op.CalcOptions.decimal := StrToInt (FDecimal.text);

   op.CalcOptions.Tri_Degree := ItemChecked[rb_degree];

   with op.UserFuncs do
   begin
   	Text := ItemText[mle_userfunctions];
      for i := High downto Low do
      begin
         Items[i] := Trim(Items[i]);
         if Items[i] = '' then
         	Delete (i)
         else if Items[i][1] = '#' then
         	Items[i] := MidStr(Items[i], 2);
      end;
   end;
end;

//-----------------------------

procedure TOpDictPage.OnInitialize ();
begin
	SetTemplate (ob_dictpage);
end;

const c_OpDictPage: array[0..6] of word = (st_dictlist, chk_selectall, chk_multidict, st_textcapture, cb_up, cb_down, cb_setcolor);

procedure TOpDictPage.OnOpen ();
begin
	RefreshItemText (self, c_OpDictPage);

   FDictFiles := TxlStrList.Create ();
   FDictFiles.Separator := ',';

   FSelectPanel := TSelectPanel.Create(self, ItemHandle[lv_dictlist], ItemHandle[cb_up], ItemHandle[cb_down], ItemHandle[chk_selectall], LangMan.GetItem(sr_optionalitems));
   FCaptureHotKey := TxlHotKey.create (self, ItemHandle[hk_textcapture]);
   FColorDemo := TFontColorPanel.Create (self, ItemHandle[st_colordemo], 0, ItemHandle[cb_setcolor]);
end;

procedure TOpDictPage.OnClose ();
begin
   FDictFiles.Free;
   FSelectPanel.Free;

   FCaptureHotkey.Free;
   FColorDemo.Free;
end;

procedure TOpDictPage.Load (const op: TOptions);
var o_alldict, o_SeldictFiles: TxlStrList;
	o_SelDict: TxlIntList;
   i, n: integer;
   o_file: TxlTextFile;
   s_text, s_dictname: widestring;
   o_availfiles: TxlStrList;
begin
   o_AllDict := TxlStrList.Create;
   o_SelDict := TxlIntList.Create;

   o_SelDictFiles := TxlStrList.Create ();
   o_SelDictFiles.Separator := ',';
   o_SelDictFiles.Text := op.DictOptions.SelDictList;

   FDictFiles.Text := op.DictOptions.AllDictList;
   o_availfiles := TxlStrList.Create;
   FindFiles (DictDir + '*.dic', o_AvailFiles);
   for i := o_AvailFiles.Low to o_AvailFiles.High do
      if FDictFiles.Find (o_AvailFiles[i]) < 0 then
         FDictFiles.Add (o_AvailFiles[i]);
   o_availfiles.free;

   with FDictFiles do
      for i := Low to High do
      begin
         if not PathFileExists (DictDir + Items[i]) then continue;

         o_file := TxlTextFile.Create (DictDir + Items[i], fmRead, enUnknown);
         if o_file.Encode <> enUTF16LE then
         begin
            ProgTip.ShowTip (LangMan.GetItem (sr_codetransfer));
            o_file.ReadTExt (s_text);
            o_file.Reset (fmWrite, enUTF16LE);
            o_file.WriteTExt (s_text);
            o_file.Reset (fmRead, enUTF16LE);
         end;

         o_file.ReadLn (s_dictname);
         n := firstpos ('//', s_dictname);
         if n > 0 then s_dictname := leftstr (s_dictname, n - 1);
         o_AllDict.AddByIndex (i, MidStr (s_dictname, 2));
         if o_SelDictFiles.Find (Items[i]) >= 0 then
            o_SelDict.Add (i);
         o_file.Free;
      end;
   o_seldictfiles.free;
	ProgTip.HideTip;

   FSelectPanel.Initialize(o_SelDict, o_AllDict);
   o_alldict.free;
   o_seldict.free;

   ItemChecked[chk_multidict] := op.DictOptions.MultiDict;
   FCaptureHotKey.hotkey := op.TipQueryHotKey;
   FColorDemo.Color := op.TipQueryColor;
end;

procedure TOpDictPage.Save (var op: TOptions);
var o_sel, o_all: TxlIntList;
	o_seldict, o_alldict: TxlStrList;
   i: integer;
begin
   o_sel := TxlIntList.Create;
   o_all := TxlIntList.Create;
   o_seldict := TxlStrList.Create ();
   o_seldict.Separator := ',';
   o_alldict := TxlStrList.Create ();
   o_alldict.Separator := ',';

   FSelectPanel.Retrieve(o_Sel, o_All);
   for i := o_Sel.Low to o_Sel.High do
      o_seldict.Add (FDictFiles[o_Sel[i]]);
   for i := o_all.Low to o_all.High do
      o_alldict.Add (FDictFiles[o_All[i]]);

   op.DictOptions.SelDictList := o_seldict.Text;
   op.DictOptions.AllDictList := o_alldict.Text;
   o_SelDict.free;
   o_alldict.free;
   o_sel.free;
   o_all.free;

   op.DictOptions.MultiDict := ItemChecked[chk_multidict];
   op.TipQueryHotKey := FCaptureHotkey.Hotkey;
   op.TipQueryColor := FColorDemo.Color;
end;

//-----------------------------

procedure TOpLinkPage.OnInitialize ();
begin
	SetTemplate (ob_linkpage);
end;

const c_OpLinkPage: array[0..3] of word = (st_fastlinkhotkey, chk_enableitemhotkey, chk_disableiconread, chk_autominimizeafteropenlink);

procedure TOpLinkPage.OnOpen ();
begin
	RefreshItemText (self, c_OpLinkPage);

   FFastLinkHotKey := TxlHotKey.create (self, ItemHandle[hk_fastlink]);
   FFastLinkHotKey.OnChange := f_OnHotkeyChange;
end;

procedure TOpLinkPage.OnClose ();
begin
   FFastLinkHotKey.free;
end;

procedure TOpLinkPage.f_OnHotKeyChange (Sender: TObject);
begin
	ItemEnabled[chk_enableitemhotkey] := HiByte (FFastLinkHotKey.HotKey) <> 0;
end;

procedure TOpLinkPage.Load (const op: TOptions);
begin
   ItemChecked[chk_disableiconread] := op.LinkOptions.DisableIconRead;
   ItemChecked[chk_AutoMinimizeAfterOpenLink] := op.LinkOptions.AutoMinimizeAfterOpenLink;
   FFastLinkHotKey.hotkey := op.fastlinkhotkey;
   ItemChecked[chk_enableitemhotkey] := op.enablefastlinkitemhotkey;
end;

procedure TOpLinkPage.Save (var op: TOptions);
begin
   op.LinkOptions.DisableIconRead := ItemChecked[chk_disableiconread];
   op.LinkOptions.AutoMinimizeAfterOpenLink := ItemChecked[chk_AutoMinimizeAfterOpenLink];
   op.fastlinkhotkey := FFastLinkHotKey.HotKey;
   op.enablefastlinkitemhotkey := ItemEnabled[chk_enableitemhotkey] and ItemChecked[chk_enableitemhotkey];
end;

//-----------------------------

procedure TOpTemplate.OnInitialize ();
begin
	SetTemplate (ob_template);
end;

const c_OpTemplate: array[0..3] of word = (st_templatehotkey, chk_enableitemhotkey, st_capturehotkey, chk_popupmenunofocus);

procedure TOpTemplate.OnOpen ();
begin
	RefreshItemText (self, c_OpTemplate);
   FTemplateHotKey := TxlHotKey.create (self, ItemHandle[hk_template]);
   FTemplateHotKey.OnChange := f_OnHotkeyChange;
   FCaptureHotKey := TxlHotKey.Create (self, ItemHandle[hk_capture]);
end;

procedure TOpTemplate.OnClose ();
begin
   FTemplateHotKey.free;
   FCaptureHotKey.Free;
end;

procedure TOpTemplate.f_OnHotKeyChange (Sender: TObject);
begin
	ItemEnabled[chk_enableitemhotkey] := HiByte (FTemplateHotKey.HotKey) <> 0;
end;

procedure TOpTemplate.Load (const op: TOptions);
begin
   FTemplateHotKey.hotkey := op.templatehotkey;
   FCaptureHotKey.Hotkey := op.CaptureHotKey;
   ItemChecked[chk_enableitemhotkey] := op.enabletemplitemhotkey;
   ItemChecked[chk_popupmenunofocus] := op.PopupMenuNoFocus;
end;

procedure TOpTemplate.Save (var op: TOptions);
begin
   op.templatehotkey := FTemplateHotKey.HotKey;
   op.enabletemplitemhotkey := ItemEnabled[chk_enableitemhotkey] and ItemChecked[chk_enableitemhotkey];
   op.CaptureHotKey := FCaptureHotKey.HotKey;
   op.PopupMenuNoFocus := ItemChecked[chk_popupmenunofocus];
end;

//-----------------------------

procedure TOpClipboard.OnInitialize ();
begin
	SetTemplate (ob_clipboard);
end;

const c_OpClipboard: array[0..8] of word = (chk_newpastetotop, st_maxclipnum, st_maxitembyte, st_menuwidth, st_cliphotkey,
   st_separateline, chk_enableitemhotkey, chk_filtersameitems, chk_onlyfilterneighboring);

procedure TOpClipboard.OnOpen ();
begin
	RefreshItemText (self, c_OpClipboard);

   FMaxClipNum := TxlComboBox.create (self, ItemHandle[cmb_maxclipnum]);
   FMaxClipNum.AllowEdit := true;
   FMaxClipNum.Items.PopulateInt ([5, 10, 15, 20, 25, 30, 50, 80]);

   FMaxItemByte := TxlComboBox.create (self, ItemHandle[cmb_maxitembyte]);
   FMaxItemByte.Items.Populate (['10k', '20k', '50k', '100k', '200k', '500k']);

   FMenuWidth := TxlComboBox.create(self, ItemHandle[cmb_menuWidth]);
   FMenuWidth.AllowEdit := true;
   FMenuWidth.Items.PopulateInt ([20, 25, 30, 35, 40, 45, 50, 60, 80]);

   FClipboardHotKey := TxlHotKey.create (self, ItemHandle[hk_clipboard]);
   FClipboardHotKey.OnChange := f_OnHotkeyChange;
end;

procedure TOpClipboard.OnClose ();
begin
   FMaxClipNum.free;
   FMaxItemByte.free;
   FMenuWidth.Free;
   FClipboardHotKey.free;
end;

procedure TOpClipboard.Load (const op: TOptions);
begin
   FMaxClipNum.text := IntToStr(op.clipoptions.maxclipnum);
   FMaxItemByte.text := IntToStr(op.clipoptions.maxitembyte div 1000) + 'k';
   FMenuWidth.Text := IntToStr(op.ClipOptions.MenuWidth);
   ItemChecked[chk_filtersameitems] := op.ClipOptions.FilterType <> cftNoFilter;
   f_CheckEnableFilter;
   ItemChecked[chk_onlyfilterneighboring] := op.ClipOptions.FilterType = cftFilterNeighboring;

   ItemChecked[chk_newpastetotop] := op.ClipOptions.NewPasteToTop;
   FClipboardHotKey.HotKey := op.clipboardhotkey;

   ItemChecked[chk_enableitemhotkey] := op.enableclipitemhotkey;
end;

procedure TOpClipboard.Save (var op: TOptions);
var s: widestring;
begin
   with op.ClipOptions do
   begin
      Maxclipnum := ConfineRange (StrToIntDef (FMaxClipNum.text, MaxClipNum), 3, 100);
      s := LeftStr(FMaxItemByte.text, length(FMaxItemByte.Text) - 1);
      Maxitembyte := StrToIntDef (s, 100) * 1000;
      MenuWidth := ConfineRange (StrToIntDef (FMenuWidth.Text, MenuWidth), 10, 100);
      NewPasteToTop := ItemChecked[chk_NewPasteToTop];

      if not ItemChecked[chk_filtersameitems] then
         FilterType := cftNoFilter
      else if ItemChecked[chk_onlyfilterneighboring] then
         FilterType := cftFilterNeighboring
      else
         FilterType := cftFilterAll;
   end;

   op.clipboardhotkey := FClipboardHotKey.HotKey;
   op.enableclipitemhotkey := ItemEnabled[chk_enableitemhotkey] and ItemChecked[chk_enableitemhotkey];
end;

procedure TOpClipboard.OnCommand (ctrlId: integer);
begin
   if ctrlId = chk_filtersameitems then
      f_CheckEnableFilter;
end;

procedure TOpClipboard.f_OnHotKeyChange (Sender: TObject);
begin
	ItemEnabled[chk_enableitemhotkey] := HiByte (FClipboardHotKey.HotKey) <> 0;
end;

procedure TOpClipboard.f_CheckEnableFilter ();
begin
   ItemEnabled[chk_onlyfilterneighboring] := ItemChecked[chk_filtersameitems];
end;

end.
 
 
 
