unit UOptionBox;

interface

uses windows, UxlDialog, Resource, UxlTreeView, CommCtrl, UxlFunctions, UxlStrUtils, UxlStdCtrls, UxlClasses, UGlobalObj,
	UxlCommDlgs, UxlMiscCtrls, UDialogs, UxlList, UxlPanel, UCommPanels, UOptionManager, UxlComboBox;

type
	TOptionFrame = class (TxlFrame)
   private
   	FOptions: TOptions;
   protected
      procedure AfterOpenTemplate (); override;
      procedure BeforeCloseTemplate (); override;
   public
   	function Validate (): boolean;
      procedure SaveOptions ();
   	property Options: TOptions read FOptions write FOptions;
   end;

	TOptionPanel = class (TxlDialogML)
   protected
      FOptions: TOptions;
   public
   	function Validate (): boolean; virtual;
      procedure Load (const op: TOptions); virtual; abstract;
      procedure Save (var op: TOptions); virtual; abstract;
      property Options: TOptions read FOptions write FOptions;
   end;
   TOptionPanelClass = class of TOptionPanel;

   TOptionBox = class (TxlDialog)
   private
      FTree: TxlTreeView;
      FFrame: TOptionFrame;
      FOptions: TOptions;
      FOnApplyOption: TNotifyEvent;

      function f_OnTreeSelChanging (oldhandle, newhandle: HTreeItem): integer;
      function f_OnTreeSelChanged (oldhandle, newhandle: HTreeItem): integer;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnMove (pt: TPoint); override;
      procedure OnCommand (ctrlID: integer); override;
   public
      property Options: TOptions read FOptions write FOptions;
      property OnApplyOption: TNotifyEvent read FOnApplyOption write FOnApplyOption;
   end;

implementation

uses UxlFile, UxlMath, UTypeDef, ULangManager, UOpProgram, UOpAppearance, UOpExtFuncs;

procedure TOptionBox.OnInitialize ();
begin
   SetTemplate (Option_Box, m_options);
end;

const dlg_extfuncs: array[0..4] of integer = (ob_calcpage, ob_dictpage, ob_linkpage, ob_template, ob_clipboard);
      pic_extfuncs: array[0..4] of integer = (m_newcalc, m_newdict, m_newlink, m_template, m_clipboard);
      dlg_progoptions: array[0..6] of integer = (ob_edit, ob_notes, ob_import_export, ob_behavior, ob_login, ob_backup, ob_specialmode);
      pic_progoptions: array[0..6] of integer = (ic_edit, ic_autorecord, m_import, ic_behavior, ic_login, ic_backup, m_specialmode);
      dlg_approptions: array[0..4] of integer = (ob_treeview, ob_tabcontrol, ob_listview, ob_editor, ob_othercontrols);
      pic_approptions: array[0..4] of integer = (m_showtree, ic_tabcontrol, ic_listview, ic_editor, ic_othercontrols);

const c_OptionBox: array[0..2] of word = (IDOK, IDCANCEL, cb_apply);

procedure TOptionBox.OnOpen ();
   function f_GetTreeItem (id_dialog: integer; i_pic: integer; b_children: boolean): TTreeViewItem;
   begin
      with result do
      begin
         text := LangMan.GetItem (id_dialog);
         data := id_dialog;
         image := i_pic;
         selectedimage := image;
         state := 0;
         children := b_children;
      end;
   end;
var h, h2: HTreeItem;
	i_pic, i: integer;
begin
   inherited;
 	RefreshItemTExt (self, c_OptionBox, Option_Box);

	FFrame := TOptionFrame.Create (self, Itemhandle[st_placeholder]);
   FFrame.Options := FOptions;

   FTree := TxlTreeView.create (self, ItemHandle[tv_options]);
   with FTree do
   begin
   	ShowImages := true;
      Items.OnSelChanging := f_OnTreeSelChanging;
   	Items.OnSelChanged := f_OnTreeSelChanged;
  		i_pic := Images.AddIcon (m_options);

      h := Items.Add (nil, f_GetTreeItem(ob_program, i_pic, true));
      for i := Low (dlg_progoptions) to High(dlg_progoptions) do
      	Items.AddChild (h, f_GetTreeItem(dlg_progoptions[i], Images.AddIcon(pic_progoptions[i]), false));
      Items.Expand (h);

      h2 := Items.Add (nil, f_GetTreeItem(ob_appearance, i_pic, true));
      for i := Low(dlg_approptions) to High(dlg_approptions) do
      	Items.AddChild (h2, f_GetTreeItem(dlg_approptions[i], Images.AddIcon(pic_approptions[i]), false));
      Items.Expand (h2);

      h2 := Items.Add (nil, f_GetTreeItem(ob_extfuncs, i_pic, true));
      for i := Low(dlg_extfuncs) to High(dlg_extfuncs) do
      	Items.AddChild (h2, f_GetTreeItem(dlg_extfuncs[i], Images.AddIcon(pic_extfuncs[i]), false));
      Items.Expand (h2);

      h2 := Items.FindByData (MemoryMan.OptionPage);
      if h2 = nil then h2 := h;
      Items.Select (h2, true);
   end;
end;

procedure TOptionBox.OnClose ();
begin
	FFrame.CloseTemplate;
//	Options := FFrame.Options;
   FFrame.Free;
   FTree.Free;
end;

procedure TOptionBox.OnCommand (ctrlID: integer);
begin
   if (ctrlID = IDOK) or (ctrlID = cb_apply) then
   begin
      if not FFrame.Validate then exit;
      FFrame.SaveOptions;
      Options := FFrame.Options;
      if assigned (FOnApplyOption) then
         FOnApplyOption (self);
   end;
   inherited OnCommand (ctrlID);
end;

procedure TOptionBox.OnMove (pt: TPoint);
begin
  	if FFrame <> nil then
   	FFrame.AdjustFramePos;
end;

function TOptionBox.f_OnTreeSelChanging (oldhandle, newhandle: HTreeItem): integer;
begin
	result := IfThen (FFrame.Validate, 0, 1);
end;

function TOptionBox.f_OnTreeSelChanged (oldhandle, newhandle: HTreeItem): integer;
var o_class: TOptionPanelClass;
begin
	result := 0;
	case FTree.Items[newhandle].data of
      ob_program:
			   o_class := TOpProgram;

         ob_edit:
            o_class := TOpEdit;
         ob_notes:
         	o_class := TOpNotes;
         ob_import_export:
         	o_class := TOpImportExport;
         ob_behavior:
            o_class := TOpBehavior;
         ob_login:
            o_class := TOpLogIn;
         ob_backup:
         	o_class := TOpBackUp;
         ob_specialmode:
            o_class := TOpSpecialMode;

      ob_appearance:
      	   o_class := TOpAppearance;

         ob_treeview:
         	o_class := TOpTreeView;
         ob_tabcontrol:
         	o_class := TOpTabControl;
         ob_listview:
         	o_class := TOpListView;
         ob_editor:
         	o_class := TOpEditor;
         ob_othercontrols:
            o_class := TOpOtherControls;

      ob_extfuncs:
      	   o_class := TOpExtFuncs;

         ob_calcpage:
            o_class := TOpCalcPage;
         ob_dictpage:
            o_class := TOpDictPage;
         ob_linkpage:
            o_class := TOpLinkPage;
         ob_template:
            o_class := TOpTemplate;
         ob_clipboard:
            o_class := TOpClipboard;
      else
      	exit;
   end;

	MemoryMan.OptionPage := FTree.Items[newhandle].data;
   FFrame.SetTemplate(o_class);
end;

//-----------------------------

procedure TOptionFrame.AfterOpenTemplate ();
begin
//	TOptionPanel (FFrame).Options := FOptions;
   TOptionPanel (FFrame).Load (FOptions);
end;

procedure TOptionFrame.BeforeCloseTemplate ();
begin
   SaveOptions;
end;

procedure TOptionFrame.SaveOptions ();
begin
   TOptionPanel(FFrame).Save (FOptions);
//	Options := TOptionPanel(FFrame).Options;
end;

function TOptionFrame.Validate (): boolean;
begin
	if assigned(FFrame) then
   	result := TOptionPanel(FFrame).Validate
   else
   	result := true;
end;

//------------------

function TOptionPanel.Validate (): boolean;
begin
	result := true;
end;

end.



