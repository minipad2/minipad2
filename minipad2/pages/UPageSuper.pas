unit UPageSuper;

interface

uses Windows, UxlClasses, UxlList, UTypeDef, UPageProperty, Resource;

type
   TPageSuper = class
   private
   	Fid: integer;
      FOwnerId: integer;
      FName: widestring;
   	FStatus: TPageStatus;
      FIcon: widestring;
      FChecked: boolean;
      FProperties: array of TPropertySuper;

      procedure SetOwner (value: TPageSuper);
      function GetOwner (): TPageSuper;
      procedure SetName (const value: widestring);
      procedure SetIcon (const value: widestring);
      function GetText (): widestring;
      procedure TriggerChange (pct: TPageEvent);
	protected
   	procedure AddProperty (value: TPropertySuper);
      procedure SetText (const value: widestring); virtual;
      function f_GetText (i_maxchar: integer = -1): widestring; virtual;
      procedure SetStatus (value: TPageStatus); virtual;
   public
      property Id: integer read FId;
      property OwnerId: integer read FOwnerID write FOwnerId;
      property Name: widestring read FName write SetName;
      property Status: TPageStatus read FStatus write SetStatus;
      property Icon: widestring read FIcon write SetIcon;
      property Checked: boolean read FChecked write FChecked;
      property Text: widestring read GetText write SetText;

      constructor Create (i_id: integer); virtual;
      procedure Initialize (); virtual;
   	class function PageType(): TPageType; virtual; abstract;
      function PageControl (): TPageControl; virtual; abstract;
      function ImageIndex (): integer; virtual;
      class function SingleInstance (): boolean; virtual;

      procedure Load (o_list: TxlStrList); virtual;
      procedure Save (o_list: TxlStrList); virtual;
		procedure Clone (p: TPageSuper); virtual;

      function GetColText (id_col: integer): widestring; virtual;

      function ExportText (b_mep: boolean): widestring; virtual;
//      function CanExportMep (): boolean; virtual;
      function AbstractText (): widestring; virtual;
      procedure ImportText (const value: widestring); virtual;

      class function CanSearch (): boolean; virtual;
      class procedure GetSearchCols (o_list: TxlIntList); virtual;

      property Owner: TPageSuper read GetOwner write SetOwner;
   	function Nearest (): TPageSuper;
      procedure GetChildList (o_list: TxlIntList); virtual;

      function CanAddChild (ptChild: TPageType): boolean; virtual;
      function CanOwnChild (ptChild: TPageType): boolean; virtual;
      class function DefChildType (): TPageType; virtual;
		function ChildShowInTree (ptChild: TPageType): boolean; virtual;
		function IsChildItemContainer (): boolean; virtual;
      function IsVirtualContainer (): boolean; virtual;
      
      function Childs (): TPageChilds; virtual;
      function ListProperty (): TListProperty; virtual;
      function PageProperty (): TTruePageProperty; virtual;
		class procedure InitialListProperty (lp: TListProperty); virtual;

   	function CanDelete (): boolean; virtual;
      procedure Delete (); virtual;
   end;
   TPageClass = class of TPageSuper;

   TTruePageSuper = class (TPageSuper)
   private
   	FPageProperty: TTruePageProperty;
   protected
      procedure SetText (const value: widestring); override;
      function f_GetText (i_maxchar: integer = -1): widestring; override;
   public
      constructor Create (i_id: integer); override;
      destructor Destroy (); override;
      function PageProperty (): TTruePageProperty; override;

      class procedure GetSearchCols (o_list: TxlIntList); override;
      function GetColText (id_col: integer): widestring; override;
   end;

   TListPageSuper = class (TTruePageSuper)
   private
   	FListProperty: TListProperty;
   protected
	public
   	constructor Create (i_id: integer); override;
      destructor Destroy (); override;
      procedure Initialize (); override;
      function PageControl (): TPageControl; override;
      function ListProperty (): TListProperty; override;
      class procedure GetListShowCols (o_list: TxlIntList); virtual;
		class procedure InitialListProperty (lp: TListProperty); override;
      function ExportText (b_mep: boolean): widestring; override;
      function AbstractText (): widestring; override;
		function GetColText (id_col: integer): widestring; override;
	end;

	TPageContainer = class (TListPageSuper)
   private
   	FChilds: TPageChilds;
   protected
	public
   	constructor Create (i_id: integer); override;
      destructor Destroy (); override;
      function Childs (): TPageChilds; override;
      procedure Clone (p: TPageSuper); override;
      function CanDelete (): boolean; override;
      procedure Delete (); override;
      procedure SetStatus (value: TPageStatus); override;
      procedure GetChildList (o_list: TxlIntList); override;
	end;

   TChildItemContainer = class (TPageContainer)
   public
   	function IsChildItemContainer (): boolean; override;
      function ExportText (b_mep: boolean): widestring; override;
      procedure ImportText (const value: widestring); override;
      class function CanSearch (): boolean; override;
   end;

   TEditPageSuper = class (TTruePageSuper)
   private
   protected
   public
      function PageControl (): TPageControl; override;
      function GetColText (id_col: integer): widestring; override;
      class function CanSearch (): boolean; override;
	end;

   TChildItemSuper = class (TPageSuper)
   private
   	function IsSeparatorLine (): boolean;
   protected
   	FItemProperty: TPropertySuper;
   	function GetImageIndex (): integer; virtual; abstract;
   public
      function PageControl (): TPageControl; override;
      function GetColText (id_col: integer): widestring; override;
      function ImageIndex (): integer; override;
   end;

implementation

uses UPageStore, UGlobalObj, ULangManager, UOptionManager, UxlFunctions, UxlMath, UxlListSuper, UxlDateTimeUtils, UxlFile,
	UxlListView, UPageFactory, UxlStrUtils, UExtFuncs;

const cAbstractLength = 200;

constructor TPageSuper.Create (i_id: integer);
begin
	FId := i_id;
end;

procedure TPageSuper.Initialize ();
var i: integer;
begin
//   FOwnerid := i_ownerid;
   if Owner <> nil then
      FName := Owner.Childs.GetNewChildName (PageType);
   FStatus := psNormal;
   FIcon := '';
//   FColor := -1;
   FChecked := false;
   for i := Low(FProperties) to High(FProperties) do
   	FProperties[i].Initialize ();
end;

procedure TPageSuper.Load (o_list: TxlStrList);
var i: integer;
begin
//	FOwnerId := StrToInt (o_list[0]);
   FName := o_list[0];
	Fstatus := TPageStatus(StrToIntDef (o_list[1]));
   FIcon := o_list[2];
//   FColor := TColor(StrToIntDef(o_list[6], -1));
   FChecked := StrToBool (o_list[3]);
	o_list.Delete (0, 4);
   for i := Low(FProperties) to High(FProperties) do
   	FProperties[i].Load (o_list);
end;

procedure TPageSuper.Save (o_list: TxlStrList);
var i: integer;
begin
	with o_list do
   begin
   	Clear;
//      Add (IntToStr(OwnerId));
      Add (Name);
      Add (IntToStr(Ord(FStatus)));
      Add (FIcon);
//      Add (IntToStr(Ord(FColor)));
      Add (BoolToStr(FChecked));
   end;
   for i := Low(FProperties) to High(FProperties) do
   	FProperties[i].Save (o_list);
end;

procedure TPageSuper.Clone (p: TPageSuper);
var i: integer;
begin
   Fname := p.name;
   Fstatus := p.status;
   FIcon := p.Icon;
//   FColor := p.Color;
   FChecked := p.Checked;
   for i := Low(FProperties) to High(FProperties) do
   	FProperties[i].Clone (p.FProperties[i]);
   Text := p.Text;
end;

function TPageSuper.ImageIndex (): integer;
begin
   if (Icon <> '') then
   	result := PageImageList.ImageFromFile (RelToFullPath(Icon, ProgDir));
   if (Icon = '') or (result < 0) then
		result := PageImageList.IndexOf (PageType) + Ord(Status);
end;

class function TPageSuper.SingleInstance (): boolean;
begin
	result := false;
end;

procedure TPageSuper.AddProperty (value: TPropertySuper);
var n: integer;
begin
	n := Length(FProperties);
	SetLength (FProperties, n + 1);
   FProperties[n] := value;
end;

procedure TPageSuper.SetName (const value: widestring);
begin
	if value <> FName then
   begin
      FName := LeftStr(MultiLineToSingleLine(value, true), 200);
      TriggerChange (pctFieldModified);
   end;
end;

procedure TPageSuper.SetStatus (value: TPageStatus);
begin
	if value <> FStatus then
   begin
      FStatus := value;
      TriggerChange (pctSwitchStatus);
   end;
end;

procedure TPageSuper.SetIcon (const value: widestring);
begin
	if value <> FIcon then
   begin
		FIcon := value;
      TriggerChange (pctIcon);
   end;
end;

class function TPageSuper.DefChildType (): TPageType;
begin
	result := ptNOne;
end;

procedure TPageSuper.TriggerChange (pct: TPageEvent);
begin
	PageCenter.EventNotify (pct, self.id);
end;

function TPageSuper.Childs (): TPageChilds;
begin
	result := nil;
end;

function TPageSuper.ListProperty (): TListProperty;
begin
	result := nil;
end;

function TPageSuper.PageProperty (): TTruePageProperty;
begin
	result := nil;
end;

class procedure TPageSuper.InitialListProperty (lp: TListProperty); 
begin
end;

function TPageSuper.IsChildItemContainer (): boolean;
begin
	result := false;
end;

function TPageSuper.IsVirtualContainer (): boolean;
begin
	result := false;
end;

//-----------------------

function TPageSuper.f_GetText (i_maxchar: integer = -1): widestring;
begin
	result := PageStore.RetrieveText (self.id, i_maxchar);
end;

function TPageSuper.GetText (): widestring;
begin
	result := f_GetText;
end;

procedure TPageSuper.SetText (const value: widestring);
begin
   if PageStore.SaveText (self.id, value) and (PageProperty <> nil) then
     	PageProperty.ModifyTime := Now;
end;

function TPageSuper.ExportText (b_mep: boolean): widestring;
begin
	result := IfThen (b_mep, IntToStr(Ord(PageType)) + #13#10 + Text, Text);
end;

//function TPageSuper.CanExportMep (): boolean;
//begin
//   result := false;
//end;

function TPageSuper.AbstractText (): widestring;
begin
   result := f_GetText (cAbstractLength);
end;

procedure TPageSuper.ImportText (const value: widestring);
begin
	self.TExt := value;
end;

//-------------------------

class procedure TPageSuper.GetSearchCols (o_list: TxlIntList);
const c_cols: array[0..0] of integer = (sr_Title);
begin
	o_list.Populate (c_cols);
end;

class function TPageSuper.CanSearch (): boolean;
begin
	result := false;
end;

function TPageSuper.GetColText (id_col: integer): widestring;
begin
	case id_col of
      sr_title:
      	result := Name;
      sr_Abstract, sr_SearchResult:
      	result := f_GetText (cAbstractLength);
      sr_Text:
      	result := Text;
      sr_Nodepath:
         result := PageStore.GetPagePath (self);
      else
      	result := '';
   end;
end;

//----------------------

function TPageSuper.GetOwner (): TPageSuper;
begin
	result := PageStore.Pages [self.Ownerid];
end;

procedure TPageSuper.SetOwner (value: TPageSuper);
begin
	if value <> nil then
		self.OwnerId := value.id
   else
   	self.OwnerId := -10;
end;

function TPageSuper.Nearest (): TPageSuper;
var o_list: TxlIntList;
	i: integer;
begin
	result := TPageSuper(Owner);
   if result = nil then exit;
   o_list := TxlIntList.Create;
	Owner.Childs.GetChildList (o_list);
   for i := o_list.Low to o_list.High do
   	if o_list[i] = self.id then
      begin
      	if i < o_list.High then
         	result := PageStore.Pages[o_list[i + 1]]
         else if i > o_list.Low then
         	result := PageStore.Pages[o_list[i - 1]];
         break;
      end;
   o_list.free;
end;

function TPageSuper.CanAddChild (ptChild: TPageType): boolean;
begin
	result := CanOwnChild (ptChild);
end;

function TPageSuper.CanOwnChild (ptChild: TPageType): boolean;
begin
	result := ptChild = DefChildType;
end;

function TPageSuper.ChildShowInTree (ptChild: TPageType): boolean;
begin
	result := false;
end;

procedure TPageSuper.GetChildList (o_list: TxlIntList);
begin
end;

function TPageSuper.CanDelete (): boolean;
begin
  	result := (Status = psNormal);
end;

procedure TPageSuper.Delete ();
begin
	PageStore.DeletePage (self.id);
end;

//-----------------------------

constructor TTruePageSuper.Create (i_id: integer);
begin
	inherited Create (i_id);
	FPageProperty := TTruePageProperty.Create (i_id);
   AddProperty (FPageProperty);
end;

destructor TTruePageSuper.Destroy ();
begin
	FPageProperty.free;
   inherited;
end;

function TTruePageSuper.PageProperty (): TTruePageProperty;
begin
	result := FPageProperty;
end;

class procedure TTruePageSuper.GetSearchCols (o_list: TxlIntList);
begin
	inherited GetSearchCols (o_list);
   o_list.Add (sr_Text);
   TTruePageProperty.GetShowCols (o_list);
end;

function TTruePageSuper.GetColText (id_col: integer): widestring;
begin
	if not FPageProperty.GetColText (id_col, result) then
   	result := inherited GetColText (id_col);
end;

function TTruePageSuper.f_GetText (i_maxchar: integer = -1): widestring;
var o_file: TxlTextFile;
begin
	if FPageProperty.ExternalSave and (FPageProperty.ExportFile <> '') then
   begin
   	try
         o_file := TxlTextFile.Create (FPageProperty.ExportFile, fmRead, enUnknown);
         o_file.ReadText (result, i_maxchar);
         if not o_file.eof then
            result := result + ' ...';
         o_file.Free;
      except
      	result := inherited f_GetText (i_maxchar);
      end;
   end
   else
		result := inherited f_GetText (i_maxchar);
end;

procedure TTruePageSuper.SetText (const value: widestring);
var o_file: TxlTextFile;
   s_text, s_file: widestring;
begin
   if (FPageProperty.ExternalSave) then
   begin
   	try
      	s_file := FPageProperty.ExportFile;
      	if PathFileExists (s_file) then
         begin
            o_file := TxlTextFile.Create (s_file, fmRead, enUnknown);
            o_file.ReadText (s_text);
            if (length(s_Text) = length(value)) and (s_Text = value) then
            begin
               o_file.free;
					exit;
            end
            else
	            o_file.Reset (fmWrite, o_file.Encode);
         end
         else
         	o_file := TxlTextFile.Create (s_file, fmWrite, OptionMan.Options.ExportEncode);

         o_file.WriteText (value);
         o_file.Free;
         PageProperty.ModifyTime := Now;
      except
      	inherited SetText (value);
      end;
   end
   else
		inherited SetText (value);
end;

//------------------------

constructor TListPageSuper.Create (i_id: integer);
begin
	inherited Create (i_id);
   FListProperty := TListProperty.Create (i_id);
   AddProperty (FListProperty);
end;

destructor TListPageSuper.Destroy ();
begin
	FListProperty.free;
   inherited;
end;

function TListPageSuper.PageControl (): TPageControl;
begin
	result := FListProperty.PageControl;
end;

procedure TListPageSuper.Initialize ();
begin
	inherited Initialize ();
   PageDefSettingsMan.LoadDefSettings (PageType, FListProperty);
end;

function TListPageSuper.ListProperty (): TListProperty;
begin
	result := FListProperty;
end;

class procedure TListPageSuper.GetListShowCols (o_list: TxlIntList);
const c_cols: array[0..5] of integer = (sr_Title, sr_CreateTime, sr_ModifyTime, sr_VisitTime, sr_Abstract, sr_NodePath);
var i: integer;
begin
	for i := Low(c_cols) to High(c_cols) do
      if not o_list.ItemExists (c_cols[i]) then
		   o_list.Add (c_cols[i]);
   TTruePageProperty.GetShowCols (o_list);
end;

function TListPageSuper.GetColText (id_col: integer): widestring;
begin
	case id_col of
      sr_Text:
      	result := ExportText (false);
      sr_Abstract, sr_SearchResult:
			result := AbstractText ;
      else
      	result := inherited GetColText (id_col);
   end;
end;

class procedure TListPageSuper.InitialListProperty (lp: TListProperty);
const c_cols: array[0..2] of integer = (sr_Title, sr_Abstract, sr_ModifyTime);
	c_widths: array[0..2] of integer = (120, 240, 100);
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

function TListPageSuper.ExportText (b_mep: boolean): widestring;
var o_list: TxlIntList;
	o_slist, o_slist2: TxlStrList;
	i: integer;
begin
	if b_mep then
   	result := inherited ExportText (b_mep)
   else
   begin
      o_slist := TxlStrList.Create;
      o_slist.Separator := #13#10 + DecodeTemplate(OptionMan.Options.SepLine) + #13#10;
      o_slist2 := TxlStrList.Create;
      o_list := TxlIntList.Create;
      GetChildList(o_list);
      for i := o_list.Low to o_list.High do
         o_slist2.Add (PageStore[o_list[i]].Name);
      o_slist.Add (o_slist2.TExt);
      for i := o_list.Low to o_list.High do
         o_slist.Add (PageStore[o_list[i]].ExportText (false));
      result := o_slist.Text;
      o_list.Free;
      o_slist2.free;
      o_slist.free;
   end;
end;

function TListPageSuper.AbstractText (): widestring;
var o_slist: TxlStrList;
   o_list: TxlIntList;
   i: integer;
begin
   o_list := TxlIntList.Create;
   GetChildList(o_list);
   o_slist := TxlStrList.Create;
   for i := o_list.Low to o_list.High do
      o_slist.Add (PageStore[o_list[i]].Name);
   result := o_slist.text;
   o_slist.free;
   o_list.free;
end;

//------------------------------

constructor TPageContainer.Create (i_id: integer);
begin
	inherited Create (i_id);
   FChilds := TPageChilds.Create (i_id);
   AddProperty (FChilds);
end;

destructor TPageContainer.Destroy ();
begin
	FChilds.free;
   inherited;
end;

function TPageContainer.Childs (): TPageChilds;
begin
	result := FChilds;
end;

function TPageContainer.CanDelete (): boolean;
begin
   result := (inherited CanDelete) and FChilds.CanClear;
end;

procedure TPageContainer.Delete ();
begin
   FChilds.Clear;
   inherited Delete;
end;

procedure TPageContainer.SetStatus (value: TPageStatus);
var i: integer;
	o_list: TxlIntList;
   p: TPageSuper;
begin
	inherited SetStatus (value);
   o_list := TxlIntList.Create;
   GetChildList (o_list);
   for i := o_list.Low to o_list.High do
   begin
   	p := PageStore[o_list[i]];
   	if p.Owner = self then
      	p.Status := self.Status;
   end;
   o_list.free;
end;

procedure TPageContainer.GetChildList (o_list: TxlIntList);
begin
	FChilds.GetChildList (o_list);
end;

procedure TPageContainer.Clone (p: TPageSuper);
var i, id, newid: integer;
	p2: TPageSuper;
begin
   for i := 0 to p.childs.count - 1 do
   begin
   	id := p.childs.childid (i);
   	if not PageStore.PageValid (id) then continue;
   	p2 := PageStore[id];
      if p2.owner = p then
      begin
         newid := PageStore.NewPage (self.id, p2.PageType);
         PageStore[newid].Clone (p2);
         Childs.AddChild (newid);
      end
      else
         Childs.AddChild (p2.id);
   end;
end;

//------------------------

function TEditPageSuper.PageControl (): TPageControl;
begin
  	result := pcEdit;
end;

class function TEditPageSuper.CanSearch (): boolean;
begin
	result := true;
end;

function TEditPageSuper.GetColText (id_col: integer): widestring;
var s: widestring;
	i: integer;
begin
	if (id_col = sr_SearchResult) and (GSearchText <> '') then
   begin
   	s := f_GetText;
      i := FirstPos (GSearchText, s);
      if i > 0 then
      begin
      	i := Max (i - 10, 1);
   		result := MultiLineToSingleLine (MidStr (s, i, 200), true);
      end
      else
         result := inherited GetColText (id_col);
   end
   else
      result := inherited GetColText (id_col);
end;

//--------------------------

function TChildItemSuper.IsSeparatorLine (): boolean;
begin
	result := (Name = '') or (LeftStr(Name, 3) = '---');
end;

function TChildItemSuper.PageControl (): TPageControl;
begin
	result := pcNone;
end;

function TChildItemSuper.GetColText (id_col: integer): widestring;
var o_list: TxlStrList;
   o_collist: TxlIntList;
   i: integer;
begin
	if IsSeparatorLine then
   	result := Name
	else if id_col = sr_Abstract then
   begin
      o_collist := Owner.ListProperty.ColList;
      o_list := TxlStrList.Create;
      for i := o_collist.Low to o_collist.High do
         if (o_collist[i] <> sr_Title) and (o_collist[i] <> sr_Name) then
            o_list.Add (LangMan.GetItem(o_collist[i]) + ': ' + GetColText(o_collist[i]));
      result := o_list.Text;
      o_list.free;
   end
   else if (FItemProperty = nil) or (not FItemProperty.GetColText (id_col, result)) then
     	result := inherited GetColText (id_col);
end;

function TChildItemSuper.ImageIndex (): integer;
begin
	if IsSeparatorLine then
   	result := -1
	else
   begin
   	if Icon <> '' then
			result := PageImageList.ImageFromFile (Icon);
		if (Icon = '') or (result < 0) then
   		result := GetImageIndex;
   end;
end;

//---------------------

function TChildItemContainer.IsChildItemContainer (): boolean;
begin
	result := true;
end;

function TChildItemContainer.ExportText (b_mep: boolean): widestring;
var i, j: integer;
	o_list: TxlIntList;
   o_slist, o_linelist: TxlStrList;
begin
   o_list := TxlIntList.Create;
   o_slist := TxlStrList.Create;
   o_slist.Separator := #9;
   o_linelist := TxlStrList.Create;
   Childs.GetChildList (o_list);

	if b_mep then
   begin
   	o_linelist.Add (IntToStr(Ord(PageType)));
      for i := o_list.Low to o_list.High do
      begin
         PageStore[o_list[i]].Save (o_slist);
         o_linelist.Add (o_slist.Text);
      end;
   end
   else
   begin
   	for i := ListProperty.ColList.Low to ListProperty.ColList.High do
      	o_slist.Add (LangMan.GetItem(ListProperty.ColList[i]));
      o_linelist.Add (o_slist.Text);

      for i := o_list.Low to o_list.High do
      begin
      	o_slist.Clear;
         for j := ListProperty.ColList.Low to ListProperty.ColList.High do
         	o_slist.Add (PageStore[o_list[i]].GetColText(ListProperty.ColList[j]));
         o_linelist.Add (o_slist.Text);
      end;
   end;

   result := o_linelist.Text;
   o_linelist.free;
   o_slist.free;
   o_list.free;
end;

procedure TChildItemContainer.ImportText (const value: widestring);
var i, i_id: integer;
	o_list, o_list2: TxlStrList;
begin
	o_list := TxlStrList.Create;
   o_list.Text := value;
   o_list2 := TxlStrList.Create;
   o_list2.Separator := #9;
   for i := o_list.Low to o_list.High do
   begin
   	i_id := PageStore.NewPage (self.id, DefChildType);
      o_list2.Text := o_list[i];
      PageStore[i_id].Load (o_list2);
      Childs.AddChild (i_id);
   end;
   o_list.free;
   o_list2.free;
end;

class function TChildItemContainer.CanSearch (): boolean; 
begin
	result := not SingleInstance;
end;

end.
















