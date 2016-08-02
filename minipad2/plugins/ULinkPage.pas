unit ULinkPage;

interface

uses UxlClasses, UxlMiscCtrls, UPageSuper, UPageProperty, UxlList, UTypeDef, UEditBox, UxlComboBox;

type
	TLinkPage = class (TChildItemContainer)
   private
   public
   	class function PageType (): TPageType; override;
      class function DefChildType (): TPageType; override;
      class procedure GetListShowCols (o_list: TxlIntList); override;
		class procedure InitialListProperty (lp: TListProperty); override;
   end;

   TFastLinkPage = class (TLinkPage)
   public
   	class function PageType (): TPageType; override;
		class function SingleInstance (): boolean; override;
	end;

   TLinkProperty = class (TClonableProperty)
   private
   public
      LinkType: TLinkType;
      Link: widestring;
      HotKey: THotKey;
      Abbrev: widestring;
      Remark: widestring;

   	procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      class procedure GetShowCols (o_list: TxlIntList); 
      function GetColText (id_col: integer; var s_result: widestring): boolean; override;
   end;

   TLinkItem = class (TChildItemSuper)
   private
   	FLink: TLinkProperty;
   protected
      function GetImageIndex (): integer; override;
   public
      constructor Create (i_id: integer); override;
      destructor Destroy (); override;

   	class function PageType (): TPageType; override;
//      function GetColText (id_col: integer): widestring; override;
		class procedure GetSearchCols (o_list: TxlIntList); override;
      procedure Delete (); override;
      property Link: TLinkProperty read FLink;
   end;

   TLinkBox = class (TEditBoxSuper)
   private
   	FHotKey: TxlHotKey;
      procedure f_CheckEnable ();
   protected
      function f_DefaultIcon (): integer; override;
      function AllowUserDefinedIcon (): boolean; override;

      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;

      procedure LoadItem (value: TPageSuper); override;
      procedure ClearAndNew (); override;
      function SaveItem (value: TPageSuper): integer; override;
   public
   	class function PageType (): TPageType; override;
   end;

procedure DecryptLink (const s_full: widestring; var s_link, s_dir, s_param: widestring);

implementation

uses Windows, UxlFunctions, UxlListView, UxlCommDlgs, UxlStrUtils, UPageFactory, UOptionManager, UPageStore, ULangManager, UGlobalObj, Resource;

class	function TLinkPage.PageType (): TPageType;
begin
	result := ptLink;
end;

class function TLinkPage.DefChildType (): TPageType;
begin
	result := ptLinkItem;
end;

class procedure TLinkPage.InitialListProperty (lp: TListProperty);
const c_cols: array[0..1] of integer = (sr_Title, sr_Link);
	c_widths: array[0..1] of integer = (100, 200);
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

class procedure TLinkPage.GetListShowCols (o_list: TxlIntList);
begin
	TLinkProperty.GetShowCols (o_list);
end;

//------------------------

class function TFastLinkPage.PageType (): TPageType;
begin
	result := ptFastLink;
end;

class function TFastLinkPage.SingleInstance (): boolean;
begin
	result := true;
end;

//-----------------------

constructor TLinkItem.Create (i_id: integer);
begin
	inherited Create (i_id);
	FLink := TLinkProperty.Create (i_id);
   FItemProperty := FLink;
   AddProperty (FLink);
end;

destructor TLinkItem.Destroy ();
begin
	FLink.free;
   inherited;
end;

class function TLinkItem.PageType (): TPageType;
begin
	result := ptLinkItem;
end;

function TLinkItem.GetImageIndex (): integer;
var s_link, s_dir, s_param: widestring;
begin
   if (FLink.LinkType in [ltFile, ltFolder]) and (not OptionMan.Options.LinkOptions.DisableIconRead) then
   begin
      DecryptLink (FLink.Link, s_link, s_dir, s_param);
      if s_link = '' then
         result := -1
      else
         result := PageImageList.ImageFromFile (RelToFullPath (s_link, ProgDir));
   end
   else
      result := PageImageList.IndexOf (PageType) + Ord (FLink.LinkType);
end;

class procedure TLinkItem.GetSearchCols (o_list: TxlIntList);
begin
   TLinkProperty.GetShowCols (o_list);
end;

procedure TLinkItem.Delete ();
begin
	EventMan.EventNotify (e_LinkItemDeleted, id);
	inherited Delete;
end;

//----------------

procedure TLinkProperty.Load (o_list: TxlStrList);
begin
   LinkType := TLinkType(StrToIntDef(o_list[0]));
   Link := SingleLineToMultiLine (o_list[1]);
   HotKey := StrToIntDef(o_list[2]);
   Abbrev := o_list[3];
   Remark := SingleLineToMultiLine(o_list[4]);
   o_list.Delete (0, 5);
end;

procedure TLinkProperty.Save (o_list: TxlStrList);
begin
	with o_list do
   begin
      Add (IntToStr(Ord(LinkType)));
      Add (MultiLineToSingleLine(Link));
      Add (IntToStr(HotKey));
      Add (Abbrev);
      Add (MultiLineToSingleLine(Remark));
   end;
end;

class procedure TLinkProperty.GetShowCols (o_list: TxlIntList);
const c_cols: array[0..5] of integer = (sr_Title, sr_Link, sr_LinkType, sr_HotKey, sr_Abbrev, sr_Remark);
var i: integer;
begin
   for i := Low(c_cols) to High(c_cols) do
		o_list.Add (c_cols[i]);
end;

function TLinkProperty.GetColText (id_col: integer; var s_result: widestring): boolean;
begin
	result := true;
	case id_col of
   	sr_Title:
      	s_result := PageStore[FPageId].Name;
      sr_LinkType:
	      s_result := LangMan.GetItem (sr_LinkTypes + Ord(LinkType));
      sr_Link:
      	s_result := Link;
      sr_HotKey:
      	s_result := HotKeyToString(HotKey);
      sr_Abbrev:
         s_result := Abbrev;
      sr_Remark:
      	s_result := Remark;
      else
      	result := false;
   end;
end;

procedure DecryptLink (const s_full: widestring; var s_link, s_dir, s_param: widestring);
var i: integer;
begin
   s_link := trim(s_full);
   s_dir := '';
   s_param := '';
   if s_link = '' then exit;
   if s_link[1] = '"' then
   begin
      i := firstpos('"', s_link, 2);
      s_param := trimleft(midstr (s_link, i + 1));
      s_link := midstr (s_link, 2, i - 2);
   end;
   if PathFileExists (s_link) then
      s_dir := ExtractFilePath (s_link);
end;

//--------------------

procedure TLinkBox.OnInitialize ();
begin
   SetTemplate (Link_Box, m_newlink);
end;

const c_LinkBox: array[0..16] of word = (st_title, st_category1, rb_filelink, rb_folderlink, rb_pagelink, rb_emaillink, st_link1,
   st_category, rb_batchlink, cb_add, st_link, st_hotkey, st_abbrev, cb_new, st_remark, IDOK, IDCANCEL);

procedure TLinkBox.OnOpen ();
begin
	FHotKey := TxlHotKey.Create (self, ItemHandle[hk_linkhotkey]);
   inherited;
   RefreshItemText (self, c_LinkBox);
end;

procedure TLinkBox.OnClose ();
begin
   FHotkey.free;
	inherited;
end;

class function TLinkBox.PageType (): TPageType;
begin
	result := ptLinkItem;
end;

procedure TLinkBox.f_CheckEnable ();
begin
  	ItemEnabled[cb_browse] := ItemChecked[rb_filelink] or ItemChecked[rb_folderlink];
   ItemEnabled[cb_add] := ItemChecked[rb_batchlink];
   ItemEnabled[mle_linktext] := ItemChecked[rb_batchlink];
   ItemEnabled[sle_linktext] := not ItemEnabled[mle_linktext];
end;

procedure TLinkBox.LoadItem (value: TPageSuper);
var p: TLinkProperty;
begin
	self.Text := LangMan.GetItem(sr_EditLink);

   ItemText[sle_title] := value.name;
   p := TLinkItem(value).Link;

   ItemChecked[rb_filelink] := (p.LinkType = ltFile);
   ItemChecked[rb_folderlink] := (p.LinkType = ltFolder);
   ItemChecked[rb_pagelink] := (p.Linktype = ltPage);
   ItemChecked[rb_emaillink] := (p.Linktype = ltEmail);
   ItemChecked[rb_batchlink] := (p.LinkType = ltBatch);
  	if p.LinkType = ltBatch then
   begin
      ItemText[mle_linktext] := p.link;
      ItemText[sle_linktext] := '';
   end
   else
   begin
      ItemText[sle_linktext] := p.link;
      ItemText[mle_linktext] := '';
   end;
   f_checkenable ();
   ItemText[sle_abbrev] := p.Abbrev;
//   ItemEnabled[sle_abbrev] := value.owner.pagetype = ptFastLink;
   ItemText[mle_remark] := p.Remark;

   FHotKey.HotKey := p.hotkey;
   FocusControl (sle_title);
   inherited LoadItem (value);
end;

procedure TLinkBox.ClearAndNew ();
begin
	self.Text := LangMan.GetItem(sr_NewLink);

   ItemText[sle_title] := '';
   ItemChecked[rb_filelink] := true;
   ItemChecked[rb_folderlink] := false;
   ItemChecked[rb_pagelink] := false;
   ItemChecked[rb_emaillink] := false;
   ItemChecked[rb_batchlink] := false;
   ItemText[mle_linktext] := '';
   ItemText[sle_linktext] := '';
   f_checkenable ();
   ItemText[sle_abbrev] := '';
   ItemText[mle_remark] := '';

   FHotKey.HotKey := 0;
   FocusControl (sle_title);
   inherited ClearAndNew;
end;

function TLinkBox.SaveItem (value: TPageSuper): integer;
begin
   value.name := ItemText[sle_title];
   with TLinkItem (value).Link do
   begin
      if ItemChecked[rb_filelink] then
         LinkType := ltFile
      else if ItemChecked[rb_folderlink] then
         LinkType := ltFolder
      else if ItemChecked[rb_pagelink] then
         LinkType := ltpage
      else if ItemChecked[rb_emaillink] then
         LinkType := ltEmail
      else
         LinkType := ltBatch;
      if LinkType = ltBatch then
         Link := ItemText[mle_linktext]
      else
         Link := ItemText[sle_linktext];
      if HotKey <> FHotkey.HotKey then
      begin
      	Hotkey := FHotKey.HotKey;
         EventMan.EventNotify (e_LinkHotkeyChanged, value.id, Hotkey);
      end;
      Abbrev := ItemText[sle_abbrev];
      Remark := ItemText[mle_remark];
   end;
   inherited SaveItem (value);
end;

function TLinkBox.f_DefaultIcon (): integer;
begin
	if ItemChecked[rb_filelink] then
   	result := ic_filelink
   else if ItemChecked[rb_folderlink] then
   	result := ic_folderlink
   else if ItemChecked[rb_pagelink] then
   	result := ic_pagelink
   else if ItemChecked[rb_emaillink] then
   	result := ic_emaillink
   else
   	result := ic_batchlink;
end;

function TLinkBox.AllowUserDefinedIcon (): boolean; 
begin
	result := true;
end;

procedure TLinkBox.OnCommand (ctrlID: integer);
	procedure f_SetLink (const s_linktext, s: widestring);
   begin
     	ItemText[sle_linktext] := FulltoRelPath (s_linktext, ProgDir);
      if ItemText[sle_Title] = '' then ItemText[sle_Title] := ExtractFileName (s, false);
   end;
var s_files: widestring;
begin
	case ctrlID of
      cb_add:
         begin
            s_files := ItemText[mle_linktext];
            if AddFiles (s_files) then
            begin
               ItemTExt[mle_linktext] := s_files;
               MoveCursorLast (mle_linktext);
            end;
         end;
      cb_browse:
         begin
            if ItemChecked[rb_filelink] then
               with TxlOpenDialog.create do
               begin
                  Title := LangMan.GetItem (sr_selectfile);
                  Filter := LangMan.GetItem (sr_filefilter);
                  FilterIndex := 1;
                  FileName := ItemText[sle_linktext];
                  MultiSelect := false;
                  if Execute then
                     f_setLink (Path + FileName, Path + FileName);
                  free;
               end
            else if ItemChecked[rb_folderlink] then
               with TxlPathDialog.Create do
               begin
                  Title := LangMan.GetItem (sr_selectfolder);
                  Path := ItemText[sle_linktext];
                  if Execute then
                     f_setLink (Path, LeftStr(Path, Length(Path) - 1));
                  free;
               end;
            MoveCursorLast (sle_linktext);
         end;
      rb_filelink, rb_folderlink, rb_pagelink, rb_emaillink, rb_batchlink:
         begin
      	   f_CheckEnable;
            f_DetermineIcon;
            if ItemEnabled [sle_linktext] then
               MoveCursorLast (sle_linktext)
            else
               MoveCursorLast (mle_linktext);
         end;
      else
         inherited;
   end;
end;

//--------------------

initialization
	PageFactory.RegisterClass (TLinkPage);
   PageImageList.AddImageWithOverlay (ptLink, m_newLink);
	PageNameMan.RegisterDefName (ptLink, sr_defLinkname);

	PageFactory.RegisterClass (TFastLinkPage);
   PageImageList.AddImageWithOverlay (ptFastLink, m_FastLink);
//	PageNameMan.RegisterDefName (ptLink, sr_defLinkname);

	PageFactory.RegisterClass (TLinkItem);
   PageImageList.AddImages (ptLinkItem, [ic_filelink, ic_folderlink, ic_pagelink, ic_emaillink, ic_batchlink]);
//	PageNameMan.RegisterDefName (ptLinkItem, sr_defLinkItemname);
   EditBoxFactory.RegisterClass (TLinkBox);

finalization

end.
