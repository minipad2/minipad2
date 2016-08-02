unit UPropertyBox;

interface

uses UxlDialog, UxlTabControl, UxlPanel, Windows, UTypeDef, UxlStdCtrls, UCommPanels, UxlList, UPageSuper, UPageProperty, UxlComboBox;

type
	TPropertyPanelSuper = class (TxlDialogML)
   protected
      FPage: TPageSuper;
   public
   	procedure Load (APage: TPageSuper); virtual; abstract;
   	procedure Save (); virtual; abstract;
	end;
   TPropertyPanelClass = class of TPropertyPanelSuper;

   TGeneralPanel = class (TPropertyPanelSuper)
   private
   	FCmbStatus: TxlComboBox;
      FCbIcon: TxlButton;
      FIconFile: widestring;
      procedure f_DetermineIcon ();
		procedure f_OnCbIconRightClicked (Sender: TObject);
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
   	procedure Load (APage: TPageSuper); override;
   	procedure Save (); override;
   end;

   TEditPanel = class (TPropertyPanelSuper)
   private
      FPage: TPageSuper;
		procedure f_CheckExternalSave ();
	protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
   	procedure Load (APage: TPageSuper); override;
   	procedure Save (); override;
   end;

   TListPanel = class (TPropertyPanelSuper)
   private
   	FPage: TPageSuper;
      FSelectPanel: TSelectPanel;
      FCmbPageStyle: TxlComboBox;
      FAllCols: TxlStrList;
      FAllWidths: TxlIntList;

      procedure f_LoadSettings (lp: TListProperty);
      procedure f_SaveSettings (lp: TListProperty);
      procedure f_MemorizeWidths (lp: TListProperty);
      procedure f_RestoreWidths (lp: TListProperty);
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
   	procedure Load (APage: TPageSuper); override;
   	procedure Save (); override;
   end;

	TPropertyBox = class (TxlDialog)
   private
   	FPage: TPageSuper;
      FPageCtrl: TxlPageControl;
      FGeneralPanel, FSecondPanel: TxlFrame;
   public
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   	procedure OnMove (pt: TPoint); override;
      procedure OnCommand (ctrlID: integer); override;

   	property Page: TPageSuper write FPage;
   end;
   
implementation

uses Messages, Resource, UGlobalObj, UOptionManager, UxlClasses, Uxlfunctions, UxlListView, ULangManager, UPageFactory, UPageStore, UxlCommDlgs;

const c_PropertyBox: array[0..1] of word = (IDOK, IDCancel);

procedure TPropertyBox.OnInitialize ();
begin
	SetTemplate (Property_Box, m_property);
end;

procedure TPropertyBox.OnOpen ();
	procedure f_CreateSecondPanel (o_class: TPropertyPanelClass; id_caption: integer);
   begin
      FSecondPanel := TxlFrame.Create (self);
      FSecondPanel.SetTemplate (o_class);
      FPageCtrl.AddPage (LangMan.GetItem(id_caption), FSecondPanel);
   	TPropertyPanelSuper(FSecondPanel.Frame).Load (FPage);
	end;
var o_style: TTabStyle;
begin
	inherited;
   RefreshItemText (self, c_PropertyBox, Property_Box);

   with o_style do
   begin
      MultiLine := true;
      TabWidth := 0;
   end;
	FPageCtrl := TxlPageControl.create(self, ItemHandle[tab_switch]);
   FPageCtrl.SetStyle (o_style);

   FGeneralPanel := TxlFrame.Create (self);
   FGeneralPanel.SetTemplate (TGeneralPanel);
   FPageCtrl.AddPage (LangMan.GetItem(Property_General), FGeneralPanel);
   TGeneralPanel(FGeneralPanel.Frame).Load (FPage);

   if FPage.ListProperty <> nil then
   	f_CreateSecondPanel (TListPanel, Property_List)
   else
   	f_CreateSecondPanel (TEditPanel, Property_Edit);

   FPageCtrl.SelectPage (0);
end;

procedure TPropertyBox.OnClose ();
begin
	FGeneralPanel.Free;
   FSecondPanel.free;
   FPageCtrl.Free;
	inherited;
end;

procedure TPropertyBox.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	IDOK:
      	begin
            TGeneralPanel (FGeneralPanel.Frame).Save;
           	TPropertyPanelSuper (FSecondPanel.Frame).Save;
            close (true);
         end;
      IDCancel:
         close (false);
   end;
end;

procedure TPropertyBox.OnMove (pt: TPoint);
begin
	if FGeneralPanel <> nil then
  		FGeneralPanel.AdjustFramePos;
   if FSecondPanel <> nil then
   	FSecondPanel.AdjustFramePos;
end;

//------------------

procedure TGeneralPanel.OnInitialize ();
begin
	SetTemplate (Property_General);
end;

procedure TGeneralPanel.OnOpen ();
const c_generalbox: array[0..6] of word = (st_title, st_status, st_createtime, st_modifytime, st_visittime, st_path, st_remark);
begin
	inherited;
	RefreshItemText (self, c_generalbox, Property_General);

   FCmbStatus := TxlComboBox.Create (self, ItemHandle[cmb_status]);
   with FCmbStatus.Items do
   begin
   	Add (LangMan.GEtItem (sr_Normal));
      Add (LangMan.GetItem (sr_Locked));
      Add (LangMan.GetItem (sr_Protected));
      Add (LangMan.GetItem (sr_ReadOnly));
   end;

   FCbIcon := TxlButton.Create (self, ItemHandle[cb_icon]);
   FCbIcon.Flat := true;
   FCbIcon.OnRightClick := f_OnCbIconRightClicked;
end;

procedure TGeneralPanel.OnClose ();
begin
	FCbIcon.free;
   FCmbStatus.Free;
	inherited;
end;

procedure TGeneralPanel.OnCommand (ctrlID: integer);
begin
	case ctrlID of
      cb_icon:
         with TxlOpenDialog.create do
         begin
            Title := LangMan.GetItem (sr_SelectIcon);
            Filter := LangMan.GetItem (sr_IconFilter);
            FilterIndex := 1;
            Path := MemoryMan.IconFilePath;
            FileName := FIconFile;
            MultiSelect := false;
            if Execute then
            begin
               FIconFile := Path + FileName;
               MemoryMan.IconFilePath := Path;
               f_DetermineIcon;
            end;
            free;
         end;
   end;
end;

procedure TGeneralPanel.Load (APage: TPageSuper);
var i: integer;
const c_readonlys: array[0..3] of integer = (sle_createtime, sle_modifytime, sle_visittime, sle_path);
begin
	FPage := APage;
   ItemText [sle_title] := FPage.Name;
   FCmbStatus.Items.SelIndex := Ord(FPage.Status);
   ItemText [sle_createtime] := FPage.GetColText (sr_CreateTime);
   ItemText [sle_modifytime] := FPage.GEtColText (sr_ModifyTime);
   ItemText [sle_visittime] := FPage.GetColText (sr_VisitTime);
   ItemText [sle_path] := PageStore.GetPagePath (FPage);
   ItemText [mle_remark] := FPage.PageProperty.Remark;

	for i := Low(c_readonlys) to High(c_readonlys) do
   	SendMessageW (ItemHandle[c_readonlys[i]], EM_SETREADONLY, 1, 0);

   FIconFile := RelToFullPath(FPage.Icon, ProgDir);
   f_DetermineIcon;
   FocusControl (sle_title);
end;

procedure TGeneralPanel.Save ();
begin
	with FPage do
   begin
   	Name := ItemText[sle_title];
      Status := TPageStatus (FCmbStatus.Items.SelIndex);
      Icon := FullToRelPath(Ficonfile, ProgDir);
      PageProperty.Remark := ItemText[mle_remark];
   end;
end;

procedure TGeneralPanel.f_OnCbIconRightClicked (Sender: TObject);
begin
	FIconFile := '';
   f_DetermineIcon;
end;

procedure TGeneralPanel.f_DetermineIcon ();
begin
	if FIconFile <> '' then
   	FCbIcon.SetIcon (FIconFile)
   else
   	FCbIcon.SetIcon (PageImageList.DefaultIconId (FPage.PageType));
end;

//------------------

procedure TEditPanel.OnInitialize ();
begin
	SetTemplate (Property_Edit);
end;

procedure TEditPanel.OnOpen ();
const c_editbox: array[0..0] of word = (chk_externalsave);
begin
	inherited;
	RefreshItemText (self, c_editbox, Property_Edit);
end;

procedure TEditPanel.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	chk_externalsave:
      	begin
         	f_CheckExternalSave;
            if ItemChecked[chk_externalsave] then
            	FocusControl (sle_exportfile);
         end;
      cb_browse:
         with TxlSaveDialog.create do
         begin
            Title := LangMan.GetItem (sr_NameExportFile);
            Filter := LangMan.GetItem (sr_Exportfilter1);
            FilterIndex := 1;
            FileName := ItemText[sle_ExportFile];
//            MultiSelect := false;
            if Execute then
               ItemText[sle_ExportFile] := FullToRelPath(Path + FileName, ProgDir);
            free;
         end;
   end;
end;

procedure TEditPanel.f_CheckExternalSave ();
begin
   ItemEnabled[sle_exportfile] := ItemChecked[chk_externalsave];
   ItemEnabled[cb_browse] := ItemChecked[chk_externalsave];
end;

procedure TEditPanel.Load (APage: TPageSuper);
var s: widestring;
begin
	FPage := APage;

   s := FPage.PageProperty.ExportFile;
   if s = '' then
      s := GetCurrentDir  + '\' + APage.Name + '.txt';
   ItemText [sle_exportfile] := FullToRelPath (s, ProgDir);

   ItemChecked [chk_externalsave] := FPage.PageProperty.ExternalSave;
   f_CheckExternalSave;
   if ItemEnabled[sle_exportfile] then
   	FocusControl (sle_exportfile);
end;

procedure TEditPanel.Save ();
var b_externalsave: boolean;
begin
	b_externalsave := (ItemText[sle_exportfile] <> '') and ItemChecked[chk_externalsave];
	FPage.PageProperty.ResetSave (b_externalsave, ItemText[sle_exportfile]);
end;

//------------------

procedure TListPanel.OnInitialize ();
begin
	SetTemplate (Property_List);
end;

procedure TListPanel.OnOpen ();
const c_definelistbox: array[0..12] of word = (chk_selectall, chk_fullrowselect, chk_gridlines, cb_saveasdefault, cb_loaddefault,
	cb_applytoall, st_liststyle, sr_optionalitems, chk_checkboxes, IDOK, IDCANCEL, cb_up, cb_down);
begin
	inherited;
	RefreshItemText (self, c_definelistbox, Property_List);

   FCmbPageStyle := TxlComboBox.Create (self, ItemHandle[cmb_style]);
   with FCmbPageStyle.Items do
   begin
   	Add (LangMan.GEtItem (rb_icon));
      Add (LangMan.GetItem (rb_smallicon));
      Add (LangMan.GetItem (rb_list));
      Add (LangMan.GetItem (rb_report));
      Add (LangMan.GetItem (rb_blog));
   end;
   FSelectPanel := TSelectPanel.Create (self, ItemHandle[lv_columns], ItemHandle[cb_up], ItemHandle[cb_down], ItemHandle[chk_selectall], LangMan.GetItem(sr_optionalitems));
   FAllWidths := TxlIntList.Create;
   FAllCols := TxlStrList.Create;
end;

procedure TListPanel.OnClose ();
begin
	FSelectPanel.free;
   FCmbPageStyle.Free;
   FAllWidths.Free;
   FAllCols.Free;
   inherited;
end;

procedure TListPanel.OnCommand (ctrlID: integer);
var lp: TListProperty;
	o_list: TxlIntList;
   i: integer;
begin
	case CtrlID of
      cb_saveasdefault:
      	begin
         	lp := TListProperty.Create (-1);
         	f_SaveSettings (lp);
      		PageDefSettingsMan.SaveDefSettings (FPage.PageType, lp);
            lp.free;
         end;
      cb_loaddefault:
      	begin
         	lp := TListProperty.Create (-1);
         	PageDefSettingsMan.LoadDefSettings (FPage.PageType, lp);
      		f_LoadSettings (lp);
            lp.free;
         end;
      cb_applytoall:
      	begin
            if showmessage (LangMan.GetItem (sr_applytoallprompt, '真的要将当前列表设定应用到所有同类型页面吗？'), mtQuestion,
                  LangMan.GetItem(sr_prompt)) = mrCancel then exit;
            o_list := TxlIntList.Create;
            PageStore.GetPageList (FPage.PageType, o_list);
            for i := o_list.Low to o_list.High do
            begin
            	lp := PageStore[o_list[i]].ListProperty;
            	f_MemorizeWidths (lp);
            	f_SaveSettings (lp);
               f_RestoreWidths (lp);
            end;
            o_list.free;
         end;
   end;
end;

procedure TListPanel.Load (APage: TPageSuper);
var o_list: TxlIntList;
	i: integer;
begin
	FPage := APage;
   o_list := TxlIntList.Create;
	TListPageSuper(FPage).GetListShowCols(o_list);
   FAllCols.Clear;
   for i := o_list.Low to o_list.High do
   	FAllCols.AddByIndex (o_list[i], LangMan.GetItem (o_list[i]));
   o_list.free;

   f_MemorizeWidths (APage.ListProperty);
   f_LoadSettings (APage.ListProperty);
end;

procedure TListPanel.Save ();
begin
	f_SaveSettings (FPage.ListProperty);
   f_RestoreWidths (FPage.ListProperty);
end;

procedure TListPanel.f_LoadSettings (lp: TListProperty);
begin
   FSelectPanel.Initialize (lp.ColList, FAllCols);

  	FCmbPageStyle.Items.SelIndex := Ord(lp.View);
   ItemChecked[chk_fullrowselect] := lp.FullRowSelect;
   ItemChecked[chk_checkboxes] := lp.CheckBoxes;
   ItemChecked[chk_gridlines] := lp.GridLines;
end;

procedure TListPanel.f_SaveSettings (lp: TListProperty);
begin
   FSelectPanel.Retrieve (lp.ColList);

   lp.View := TListPageView(FCmbPageStyle.Items.SelIndex);
   lp.FullRowSelect := ItemChecked[chk_fullrowselect];
   lp.CheckBoxes := ItemChecked[chk_checkboxes];
   lp.GridLines := ItemChecked[chk_gridlines];
end;

procedure TListPanel.f_MemorizeWidths (lp: TListProperty);     // 保存原先各列的宽度值
var i, j: integer;
begin
   FAllWidths.Clear;
   for i := FAllCols.Low to FAllCols.High do
   begin
   	j := lp.ColList.Find (FAllCols.Indexes[i]);
      if lp.WidthList.PosValid (j) then
      	FAllWidths.Add (lp.WidthList[j])
      else
      	FAllWidths.Add (100);
   end;
end;

procedure TListPanel.f_RestoreWidths (lp: TListProperty);
var i, j: integer;
begin
   lp.WidthList.Clear;
   for i := lp.ColList.Low to lp.ColList.High do
   begin
      j := FAllCols.FindByIndex (lp.ColList[i]);
      lp.WidthList.Add (FAllWidths[j]);
   end;
end;

end.


