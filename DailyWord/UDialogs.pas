unit UDialogs;

interface

uses Windows, Messages, UxlDialog, UxlStdCtrls, UxlTabControl, UxlPanel, UxlMiscCtrls, UxlComboBox, UCommPanels, UGlobalObj;

type
   TAboutBox = class (TxlDialog)
   private
      Fstprogram: TxlStaticText;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   end;

   TOptionFrame = class (TxlFrame)
   private
   	FOptions: TOptions;
   protected
      procedure BeforeOpenTemplate (); override;
      procedure AfterCloseTemplate (); override;
   public
   	property Options: TOptions read FOptions write FOptions;
   end;

   TOptionBox = class (TxlDialog)
   private
   	FOptions: TOptions;
   	FTabCtrl: TxlTabControl;
      FFrame: TOptionFrame;
		procedure f_OnTabSelChanged (index: integer);
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   	procedure OnMove (pt: TPoint); override;
   public
   	property Options: TOptions read FOptions write FOptions;
   end;

   TOptionSuper = class (TxlDialogML)
   protected
   	FOptions: TOptions;
   public
   	property Options: TOptions read FOptions write FOptions;
	end;

   TOpProgram = class (TOptionSuper)
	private
      FStartHotKey: TxlHotKey;
      FFullScreenHotKey: TxlHotKey;
      FTrackMouseHotKey: TxlHotKey;
      FDictFiles: TxlComboBox;
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
		procedure OnCommand (ctrlID: integer); override;
   end;

   TOpAppearance = class (TOptionSuper)
   private
   	FSetFontColor: TFontColorPanel;
		procedure f_CheckMode ();
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
		procedure OnCommand (ctrlID: integer); override;
   end;

   TOpWordBox = class (TOptionSuper)
   private
   	FudSwitchCycle: TxlUpDown;
      FCmbTestModeType: TxlComboBox;
      FudTestCycle: TxlUpdown;
		procedure f_CheckAutoSwitch ();
      procedure f_CheckAutoTest ();
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
		procedure OnCommand (ctrlID: integer); override;
   end;

procedure MessDictFile (const s_file: widestring); forward;

implementation

uses CommCtrl, UxlClasses, UxlCommDlgs, UxlFunctions, UxlMath, UxlList, UDisplay, UDisplayManager, Resource;

procedure TAboutBox.OnInitialize ();
begin
	SetTemplate (About_Box, MainIcon);
end;

procedure TAboutBox.OnOpen ();
begin
	inherited;

	Fstprogram := TxlStaticText.create (self, ItemHandle[st_program]);
	with Fstprogram.Font do
   begin
   	name := 'Arial';
      Color := rgb(0,0,0);
      Size := 14;
      Bold := true;
      Italic := true;
      Underline := false;
      StrikeOut := false;
   end;

   Fstprogram.Color := GetSysColor(COLOR_BTNFACE);

   AddLink (st_homepagelink);
   AddLink (st_forumlink);
   AddLink (st_emaillink, 'mailto:' + ItemText[st_emaillink]);
  	AddLink (st_donate, 'http://www.nebulasoft.cn/donate.html');

   with TxlStaticIcon.create(self, ItemHandle[ProgramIcon]) do
   begin
   	SetIcon (MainIcon);
      Free;
   end;
end;

procedure TAboutBox.OnClose ();
begin
	FstProgram.free;
end;

//--------------------------

procedure TOptionBox.OnInitialize ();
begin
   SetTemplate (Option_Box, m_options);
end;

procedure TOptionBox.OnOpen ();
var o_style: TTabStyle;
begin
	inherited;
   with o_style do
   begin
      MultiLine := true;
      TabWidth := 0;
   end;
	FTabCtrl := TxlTabControl.create(self, ItemHandle[tab_Options]);
   with FTabCtrl do
   begin
      SetStyle (o_style);
      TabPosition := tpTop;

      Items.Add ('程序设置');
      Items.Add ('界面设置');
      Items.Add ('词盒设置');
      Items.OnSelChanged := f_OnTabSelChanged;
   end;
   FFrame := TOptionFrame.Create (self, ItemHandle[st_placeholder]);
   FFrame.Options := self.Options;
   FFrame.BringToTop;
   f_OnTabSelChanged (0);
end;

procedure TOptionBox.OnClose ();
begin
	FFrame.CloseTemplate;
   self.Options := FFrame.Options;
	FFrame.Free;
   FTabCtrl.Free;
   inherited;
end;

procedure TOptionBox.OnMove (pt: TPoint);
begin
  	if FFrame <> nil then
   	FFrame.AdjustFramePos;
end;

procedure TOptionBox.f_OnTabSelChanged (index: integer);
begin
	case index of
   	0:
      	FFrame.SetTemplate (TOpProgram);
      1:
      	FFrame.SetTemplate (TOpAppearance);
   	2:
      	FFrame.SetTemplate (TOpWordBox);
   end;
   self.SetFocus;
end;

//----------------------

procedure TOptionFrame.BeforeOpenTemplate ();
begin
	TOptionSuper (FFrame).Options := self.Options;
end;

procedure TOptionFrame.AfterCloseTemplate ();
begin
	self.Options := TOptionSuper (FFrame).Options;
end;

//---------------------

procedure TOpProgram.OnInitialize ();
begin
	SetTemplate (Op_Program);
end;

procedure TOpProgram.OnOpen ();
var i: integer;
	o_list: TxlStrList;
begin
   FDictFiles := TxlComboBox.Create (self, itemHandle[cmb_dictfile]);
   o_list := TxlStrList.Create;
   FindFiles (ProgDir + '*.dwd', o_list);
   for i := o_list.Low to o_list.High do
   	FDictFiles.Items.Add (o_list[i]);
   o_list.free;
   FDictFiles.TExt := FOptions.DictFile;

   FStartHotKey := TxlHotKey.create (self, ItemHandle[hk_start]);
   FStartHotKey.hotkey := FOptions.StartHotKey;
   FFullScreenHotKey := TxlHotKey.create (self, ItemHandle[hk_fullscreen]);
   FFullScreenHotKey.hotkey := FOptions.FullScreenHotKey;
   FTrackMouseHotKey := TxlHotKey.Create (self, ItemHandle[hk_trackmouse]);
   FTrackMouseHotKey.hotkey := FOptions.TrackMouseHotKey;

   ItemChecked[chk_AutoStart] := FOptions.AutoRun;
end;

procedure TOpProgram.OnClose ();
begin
	with FOptions do
   begin
   	DictFile := ItemText[cmb_dictfile];
      StartHotKey := FStartHotKey.HotKey;
      FullScreenHotKey := FFullScreenHotKey.HotKey;
      TrackMouseHotKey := FTrackMouseHotKey.HotKey;
      AutoRun := ItemChecked[chk_autostart];
   end;
   FStartHotKey.free;
   FFullScreenHotKey.Free;
  	FTrackMouseHotKey.Free;
   FDictFiles.Free;
end;

procedure TOpProgram.OnCommand (ctrlID: integer);
var s_filename, s: widestring;
	b_unmessed: boolean;
begin
	case ctrlID of
   	cb_browse:
         with TxlOpenDialog.create do
         begin
            Title := '选择词库';
            Filter := 'DailyWord 词库文件(*.dwd)|*.dwd|minipad2 词典文件(*.dic)|*.dic';
            FilterIndex := 1;
            Path := ProgDir;
            FileName := ItemText[cmb_dictfile];
            MultiSelect := false;
            if Execute then
            begin
               s_filename := FileName;
               b_unmessed := ExtractFileExt (s_filename) <> 'dwd';
               if (Path <> ProgDir) or b_unmessed then
               begin
               	s := Path + s_FileName;
                  s_filename := ExtractFileName(FileName, false) + '.dwd';
                  CopyFile (s, ProgDir + s_filename, true);
                  if b_unmessed then
                  	MessDictFile (ProgDir + s_filename);
               end;
               ItemText[cmb_dictfile] := s_filename;
               if not FDictFiles.Items.ItemExists (s_filename) then
                  FDictFiles.Items.Add (s_filename);
            end;
            free;
         end;
   	else
			inherited OnCommand (ctrlID);
   end;
   ClearMemory;
end;

//------------------------

procedure TOpAppearance.OnInitialize ();
begin
	SetTemplate (Op_Appearance);
end;

procedure TOpAppearance.OnOpen ();
var i: integer;
begin
	FSetFontColor := TFontColorPanel.Create (self, ItemHandle[st_colordemo], ItemHandle[cb_setfont], ItemHandle[cb_setcolor]);
   FSetFontColor.Font := FOptions.WinFont;
   FSetFontColor.Color := FOptions.WinColor;

   with TxlComboBox.Create (self, ItemHandle[cmb_transparency]) do
   begin
   	for i := 0 to 10 do
   		Items.Add (i);
      Items.Select (FOptions.Transparency);
   	Free;
   end;

   ItemText[sle_xspace] := IntToStr(FOptions.TrackXSpace);
   ItemText[sle_yspace] := IntToStr(FOptions.TrackYSpace);
   ItemChecked[rb_normal] := FOptions.DisplayMode = dmFixed;
   ItemChecked[rb_embed] := FOptions.DisplayMode = dmEmbed;
   ItemChecked[chk_twolines] := FOptions.TwoLines;
   f_CheckMode ();
end;

procedure TOpAppearance.OnClose ();
begin
	with FOptions do
   begin
   	if ItemChecked[rb_normal] then
      	DisplayMode := dmFixed
      else
      	DisplayMode := dmEmbed;
      TwoLines := ItemChecked[chk_twolines];
		WinColor := FSetFontColor.Color;
      WinFont.assign (FSetFontColor.Font);
      Transparency := StrToInt (ItemText[cmb_transparency]);
      TrackXSpace := ConfineRange (StrToIntDef (ItemText[sle_xspace]), -250, 100);
      TrackYSpace := ConfineRange (StrToIntDef (ItemText[sle_yspace]), -150, 100);
      if (TrackXSpace <= 0) and (TrackYSpace <= 0) then // 可能导致鼠标到单词窗口上
   		TrackXSpace := Max(ABS (TrackXSpace), 1);
   end;
	FSetFontColor.Free;
end;

procedure TOpAppearance.OnCommand (ctrlID: integer);
begin
	case ctrlID of
      rb_normal, rb_embed:
      	f_CheckMode;
   	else
			inherited OnCommand (ctrlID);
   end;
end;

procedure TOpAppearance.f_CheckMode ();
var b: boolean;
begin
	b := ItemChecked[rb_normal];
   ItemEnabled[chk_twolines] := b;
   ItemEnabled[cb_setfont] := b;
   Itemenabled[cb_setcolor] := b;
   ItemEnabled[cmb_transparency] := b;
   ItemEnabled[sle_xspace] := b;
   ItemEnabled[sle_yspace] := b;
end;

//------------------------

procedure TOpWordBox.OnInitialize ();
begin
	SetTemplate (Op_WordBox);
end;

procedure TOpWordBox.OnOpen ();
begin
//	inherited;
   with TxlComboBox.Create (self, ItemHandle[cmb_boxsize]) do
   begin
   	AllowEdit := true;
   	Items.PopulateInt ([20, 30, 50, 80, 100, 150, 200, 300, 500]);
      Text := IntToStr(FOptions.BoxSize);
      Free;
   end;

   ItemChecked[chk_autoswitch] := FOptions.AutoNewBox;
   FUdSwitchCycle := TxlUpDown.Create (self, ItemHandle[ud_switchcycle]);  // Auto Buddy in RC file
   with FUdSwitchCycle do
   begin
   	SetRange (1, 20);
      Position := FOptions.NewBoxCycle;
   end;
   f_CheckAutoSwitch;

   ItemChecked[chk_autotest] := FOptions.AutoTestMode;
	FUdTestCycle := TxlUpDown.Create (self, ItemHandle[ud_testcycle]);
   with FUdTestCycle do
   begin
		SetRange (1, 19);
      Position := FOptions.TestModeCycle;
   end;
   f_CheckAutoTest;

   ItemChecked[chk_randombox] := FOptions.RandomNewBox;
   ItemChecked[chk_randomword] := FOptions.RandomSwitchWord;

   with TxlComboBox.Create (self, ItemHandle[cmb_interval]) do
   begin
   	AllowEdit := true;
   	Items.PopulateInt ([2, 3, 5, 10, 15, 20, 30]);
      Text := IntToStr(FOptions.SwitchWordInterval);
      Free;
   end;

   with TxlComboBox.Create (self, ItemHandle[cmb_delay]) do
   begin
   	AllowEdit := true;
   	Items.PopulateInt ([1, 2, 3, 5, 8]);
      Text := IntToStr(FOptions.TestModeDelay);
      Free;
   end;

   FCmbTestModeType := TxlComboBox.Create (self, ItemHandle[cmb_testmodetype]);
   with FCmbTestModeType do
   begin
   	AllowEdit := false;
   	Items.Populate (['释义缓出', '单词缓出', '随机']);
      Items.SelIndex := Ord (FOptions.TestModeType);
   end;
end;

procedure TOpWordBox.OnClose ();
begin
	with FOptions do
   begin
      BoxSize := ConfineRange (StrToIntDef (ItemText[cmb_boxsize], BoxSize), 3, 10000);

      AutoNewBox := ItemChecked[chk_autoswitch];
      NewBoxCycle := ConfineRange (StrToIntDef(ItemText[sle_SwitchCycle.Text], 3), 1, 20);

      AutoTestMode := ItemChecked[chk_autotest];
      TestModeCycle := ConfineRange (StrToIntDef(ItemText[sle_TestCycle.Text], 2), 1, NewBoxCycle - 1);

      RandomNewBox := ItemChecked[chk_randombox];
      RandomSwitchWord := ItemChecked[chk_randomword];

      SwitchWordInterval := ConfineRange (StrToIntDef (ItemText[cmb_interval], SwitchWordInterval), 1, 120);
      TestModeDelay := ConfineRange (StrToIntDef (ItemText[cmb_delay], TestModeDelay), 0, SwitchWordInterval);
      TestModeType := TTestModeType (FCmbTestModeType.Items.SelIndex);
   end;
	FUdSwitchCycle.Free;
   FUdTestCycle.Free;
   FCmbTestModeType.free;
end;

procedure TOpWordBox.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	chk_autoswitch:
      	f_CheckAutoSwitch;
      chk_autotest:
      	f_CheckAutoTest;
   	else
			inherited OnCommand (ctrlID);
   end;
end;

procedure TOpWordBox.f_CheckAutoSwitch ();
var b: boolean;
begin
	b := ItemChecked[chk_autoswitch];
   ItemEnabled[sle_switchcycle] := b;
   ItemEnabled[ud_switchcycle] := b;
end;

procedure TOpWordBox.f_CheckAutoTest ();
var b: boolean;
begin
	b := ItemChecked[chk_autotest];
   ItemEnabled[sle_testcycle] := b;
   ItemEnabled[ud_testcycle] := b;
end;

//-----------------------

procedure MessDictFile (const s_file: widestring);
var o_list: TxlStrList;
	i, j, k, n: integer;
begin
   o_list := TxlStrList.Create (500000);
   o_list.LoadFromFile (s_file);
   o_list[0] := IntToStr (o_list.Count);
   n := o_list.Count * 2;
   for i := 0 to n do
   begin
   	j := RandomInt (1, o_list.High);
      k := RandomInt (1, o_list.High);
   	o_list.SwapPos (j, k);
   end;
   o_list.SaveToFile (s_file);
   o_list.Free;
end;

end.



