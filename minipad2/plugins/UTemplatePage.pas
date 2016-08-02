unit UTemplatePage;

interface

uses UPageSuper, UPageProperty, UxlClasses, UxlList, UxlMiscCtrls, UTypeDef, UEditBox, UxlComboBox;

type
	TTemplatePage = class (TChildItemContainer)
   private
   public
   	class function PageType (): TPageType; override;
      class function DefChildType (): TPageType; override;
      class procedure GetListShowCols (o_list: TxlIntList); override;
		class procedure InitialListProperty (lp: TListProperty); override;
      class function SingleInstance (): boolean; override;
   end;

   TTemplateProperty = class (TClonableProperty)
   private
   public
   	Template: widestring;
      HotKey: THotKey;
      Abbrev: widestring;
      Remark: widestring;

   	procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      class procedure GetShowCols (o_list: TxlIntList); 
      function GetColText (id_col: integer; var s_result: widestring): boolean; override;
   end;

   TTemplateItem = class (TChildItemSuper)
   private
   	FTemplate: TTemplateProperty;
   protected
      function GetImageIndex (): integer; override;
	public
      constructor Create (i_id: integer); override;
      destructor Destroy (); override;

   	class function PageType (): TPageType; override;
//      function GetColText (id_col: integer): widestring; override;
      procedure Delete (); override;
		class procedure GetSearchCols (o_list: TxlIntList); override;
      property Template: TTemplateProperty read FTemplate;
   end;

   TTemplateBox = class (TEditBoxSuper)
   private
   	FHotKey: TxlHotKey;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;

      procedure LoadItem (value: TPageSuper); override;
      procedure ClearAndNew (); override;
      function SaveItem (value: TPageSuper): integer; override;
   public
   	class function PageType (): TPageType; override;
   end;

implementation

uses Windows, UxlFunctions, UxlListView, UxlStrUtils, UPageFactory, UPageStore, ULangManager, UGlobalObj, Resource;

class	function TTemplatePage.PageType (): TPageType;
begin
	result := ptTemplate;
end;

class function TTemplatePage.DefChildType (): TPageType;
begin
	result := ptTemplateItem;
end;

class function TTemplatePage.SingleInstance (): boolean;
begin
	result := true;
end;

class procedure TTemplatePage.InitialListProperty (lp: TListProperty);
const c_cols: array[0..2] of integer = (sr_Title, sr_Text, sr_Hotkey);
	c_widths: array[0..2] of integer = (100, 200, 60);
begin
	with lp do
   begin
		ColList.Populate (c_cols);
		WidthList.Populate (c_widths);
      CheckBoxes := false;
		View := lpvReport;
   	FullrowSelect := true;
   	GridLines := false;
   end;
end;

class procedure TTemplatePage.GetListShowCols (o_list: TxlIntList);
begin
	TTemplateProperty.GetShowCols (o_list);
end;

//-----------------------

constructor TTemplateItem.Create (i_id: integer);
begin
	inherited Create (i_id);
	FTemplate := TTemplateProperty.Create (i_id);
   FItemProperty := FTemplate;
   AddProperty (FTemplate);
end;

destructor TTemplateItem.Destroy ();
begin
	FTemplate.free;
   inherited;
end;

class function TTemplateItem.PageType (): TPageType;
begin
	result := ptTemplateItem;
end;

function TTemplateItem.GetImageIndex (): integer;
begin
	result := PageImageList.IndexOf (PageType);
end;

class procedure TTemplateItem.GetSearchCols (o_list: TxlIntList);
begin
   TTemplateProperty.GetShowCols (o_list);
end;

procedure TTemplateItem.Delete ();
begin
	EventMan.EventNotify (e_TemplateItemDeleted, id);
	inherited Delete;
end;

//----------------

procedure TTemplateProperty.Load (o_list: TxlStrList);
begin
	Template := SingleLineToMultiLine(o_list[0]);
   HotKey := StrToIntDef(o_list[1]);
   Abbrev := o_list[2];
   Remark := SingleLineToMultiLine(o_list[3]);
   o_list.Delete (0, 4);
end;

procedure TTemplateProperty.Save (o_list: TxlStrList);
begin
	with o_list do
   begin
   	Add (MultiLineToSingleLine(Template));
      Add (IntToStr(HotKey));
      Add (Abbrev);
      Add (MultiLineToSingleLine(Remark));
   end;
end;

class procedure TTemplateProperty.GetShowCols (o_list: TxlIntList);
const c_cols: array[0..4] of integer = (sr_Title, sr_Text, sr_Hotkey, sr_abbrev, sr_Remark);
var i: integer;
begin
   for i := Low(c_cols) to High(c_cols) do
		o_list.Add (c_cols[i]);
end;

function TTemplateProperty.GetColText (id_col: integer; var s_result: widestring): boolean;
begin
	result := true;
	case id_col of
   	sr_Title:
      	s_result := PageStore[FPageId].Name;
   	sr_Text:
      	s_result := Template;
      sr_Hotkey:
      	s_result := HotKeyToString(HotKey);
      sr_Abbrev:
         s_result := Abbrev;
      sr_Remark:
      	s_result := Remark;
      else
      	result := false;
   end;
end;

//--------------------

procedure TTemplateBox.OnInitialize ();
begin
	SetTemplate (Template_Box, m_Template);
end;

const c_TemplateBox: array[0..6] of word = (st_title, st_text, st_hotkey, st_abbrev, cb_new, IDOK, IDCANCEL);

procedure TTemplateBox.OnOpen ();
begin
	FHotKey := TxlHotKey.Create (self, ItemHandle[hk_templatehotkey]);
   inherited;
   RefreshItemText (self, c_TemplateBox);
end;

procedure TTemplateBox.OnClose ();
begin
	FHotKey.Free;
	inherited;
end;

class function TTemplateBox.PageType (): TPageType;
begin
	result := ptTemplateItem;
end;

procedure TTemplateBox.LoadItem (value: TPageSuper);
var p: TTemplateProperty;
begin
	self.Text := LangMan.GetItem(sr_EditTemplate);

   ItemText[sle_title] := value.name;
   p := TTemplateItem(value).Template;
   ItemText[mle_text] := p.Template;
   FHotKey.HotKey := p.HotKey;
   ItemText[sle_abbrev] := p.Abbrev;
   ItemText[mle_remark] := p.remark;

   FocusControl (sle_title);
end;

procedure TTemplateBox.ClearAndNew ();
begin
	self.Text := LangMan.GetItem(sr_NewTemplate);

   ItemText[sle_title] := '';
   ItemText[mle_text] := '';
   FHotkey.Hotkey := 0;
   ItemText[sle_abbrev] := '';
   ItemText[mle_remark] := '';

   FocusControl (sle_title);
end;

function TTemplateBox.SaveItem (value: TPageSuper): integer;
begin
   value.name := ItemText[sle_title];
   with TTemplateItem (value).Template do
   begin
      if ItemText[mle_text] <> '' then
         Template := itemText[mle_text]
      else
         Template := ItemText[sle_title];
      if HotKey <> FHotkey.HotKey then
      begin
      	Hotkey := FHotKey.HotKey;
         EventMan.EventNotify (e_TemplateHotkeyChanged, value.id, Hotkey);
      end;
      Abbrev := ItemText[sle_abbrev];
      remark := ItemText[mle_remark];
   end;
end;

//--------------------

initialization
	PageFactory.RegisterClass (TTemplatePage);
   PageImageList.AddImageWithOverlay (ptTemplate, m_Template);
//	PageNameMan.RegisterDefName (ptTemplate, sr_defTemplatename);

	PageFactory.RegisterClass (TTemplateItem);
   PageImageList.AddImages (ptTemplateItem, [m_inserttemplate]);
//	PageNameMan.RegisterDefName (ptTemplateItem, sr_defTemplateItemname);
   EditBoxFactory.RegisterClass (TTemplateBox);

finalization

end.
