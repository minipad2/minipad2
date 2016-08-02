unit UPageProperty;

interface

uses Windows, UTypeDef, UxlList;

type
	TPropertySuper = class
   protected
   	FPageId: integer;
   public
   	constructor Create (APageId: integer); virtual;

   	procedure Load (o_list: TxlStrList); virtual; abstract;
      procedure Save (o_list: TxlStrList); virtual; abstract;
      procedure Clone (p: TPropertySuper); virtual; abstract;
      procedure Initialize (); virtual;
      function GetColText (id_col: integer; var s_result: widestring): boolean; virtual;
   end;

   TClonableProperty = class (TPropertySuper)
	public
      procedure Clone (p: TPropertySuper); override;
	end;

	TTruePageProperty = class (TPropertySuper)
	private
      FCreateTime: TSystemTime;
      FModifyTime: TSystemTime;
      FVisitTime: TSystemTime;
      FExternalSave: boolean;
      FExportFile: widestring;
      FRemark: widestring;
      function GetExportFile (): widestring;
      procedure SetExportFile (const value: widestring);
   public
   	constructor Create (APageId: integer); override;
   	procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      procedure Clone (p: TPropertySuper); override;
      procedure Initialize (); override;

      class procedure GetShowCols (o_list: TxlIntList);
      function GetColText (id_col: integer; var s_result: widestring): boolean;
      procedure ResetSave (b_externalsave: boolean; const s_exportfile: widestring);  // 针对 TPropertyBox

      property CreateTime: TSystemTime read FCreateTime;
      property ModifyTime: TSystemTime read FModifyTime write FModifyTime;
      property VisitTime: TSystemTime read FVisitTime write FVisitTime;
      property ExternalSave: boolean read FExternalSave write FExternalSave;
      property ExportFile: widestring read GetExportFile write SetExportFile;
      property Remark: widestring read FRemark write FRemark;
	end;

   TPageChilds = class (TPropertySuper)
   private
   	FChilds: TxlIntList;
   public
   	constructor Create (APageId: integer); override;
      destructor Destroy (); override;

   	procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      procedure Clone (p: TPropertySuper); override;
      procedure GetChildList (o_list: TxlIntList);

   	procedure AddChild (id: integer; sid: integer = -1);
      procedure RemoveChild (id: integer);
      function CanRemoveChild (id: integer): boolean;
      procedure AddChildByPos (id: integer; i_pos: integer = -1);
      procedure RemoveChildByPos (i_pos: integer);
      procedure Clear ();
   	function CanClear (): boolean;

      function IsEmpty (): boolean;
      function FindChild (id: integer): integer;
      function Count (): integer;
		function HasChildInTree (): boolean;
		function GetNewChildName (pt: TPageType): widestring;
      function ChildId (index: integer): integer;
   end;

	TListPageView = (lpvIcon, lpvSmallIcon, lpvList, lpvReport, lpvBlog);

   TListProperty = class (TClonableProperty)
   protected
		FView: TListPageView;
      FFullrowSelect, FGridLines, FCheckBoxes: boolean;
      FColList: TxlIntList;
      FWidthList: TxlIntList;
//      procedure f_checkChange (oldpc: TPageControl);
//      procedure SetView (value: TListPageView);
   public
   	constructor Create (APageId: integer); override;
      destructor Destroy (); override;
      function PageControl (): TPageControl;

   	procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      
		property View: TListPageView read FView write FView;
      property FullRowSelect: boolean read FFullRowSelect write FFullRowSelect;
      property GridLines: boolean read FGridLines write FGridLines;
      property CheckBoxes: boolean read FCheckBoxes write FCheckBoxes;
      property ColList: TxlIntList read FColList;
      property WidthList: TxlIntList read FWidthList;
   end;

implementation

uses UPageStore, UPageFactory, UPageSuper, UxlFunctions, UxlStrUtils, UxlDateTimeUtils, Resource;

constructor TPropertySuper.Create (APageId: integer);
begin
	FPageId := APageId;
end;

procedure TPropertySuper.Initialize ();
begin
end;

function TPropertySuper.GetColText (id_col: integer; var s_result: widestring): boolean;
begin
	result := false;
end;

//----------------------

procedure TClonableProperty.Clone (p: TPropertySuper);
var o_list: TxlStrList;
begin
   o_list := TxlStrList.Create;
   p.Save (o_list);
   Load (o_list);
   o_list.free;
end;

//----------------------

constructor TTruePageProperty.Create (APageId: integer);
begin
	inherited Create (APageId);
   FExternalSave := false;
   FExportFile := '';
   FRemark := '';
end;

procedure TTruePageProperty.Load (o_list: TxlStrList);
begin
   FCreateTime := StringToSystemTime (o_list[0]);
   FModifyTime := StringToSystemTime (o_list[1]);
   FVisitTime := StringToSystemTime (o_list[2]);
	FExternalSave := StrToBool (o_list[3]);
   FExportFile := o_list[4];
   FRemark := SingleLineToMultiLine (o_list[5]);
   o_list.Delete (0, 6);
end;

procedure TTruePageProperty.Save (o_list: TxlStrList);
begin
	with o_list do
   begin
      Add (SystemTimeToString(CreateTime, dtmDateTimeWithSecond));
      Add (SystemTimeToString(ModifyTime, dtmDateTimeWithSecond));
      Add (SystemTimeToString(VisitTime, dtmDateTimeWithSecond));
   	Add (BoolToStr(FExternalSave));
      Add (FExportFile);
      Add (MultiLineToSingleLine (FRemark));
   end;
end;

procedure TTruePageProperty.Initialize ();
begin
   FCreateTime := Now;
   FModifyTime := Now;
   FVisitTime := Now;
end;

procedure TTruePageProperty.Clone (p: TPropertySuper);
begin
   Remark := TTruePageProperty(p).Remark;
end;

class procedure TTruePageProperty.GetShowCols (o_list: TxlIntList);
const c_cols: array[0..5] of integer = (sr_CreateTime, sr_ModifyTime, sr_VisitTime, sr_ExternalSave, sr_ExportFile, sr_Remark);
var i: integer;
begin
   for i := Low(c_cols) to High(c_cols) do
      if not o_list.ItemExists (c_cols[i]) then
		   o_list.Add (c_cols[i]);
end;

function TTruePageProperty.GetColText (id_col: integer; var s_result: widestring): boolean;
begin
	result := true;
	case id_col of
      sr_createtime:
      	s_result := SystemTimeToLocaleStr (CreateTime);
      sr_modifytime:
      	s_result := SystemTimeToLocaleStr (ModifyTime);
      sr_visittime:
      	s_result := SystemTimeToLocaleStr (VisitTime);
      sr_ExternalSave:
      	s_result := BoolToCheckMark (ExternalSave);
      sr_ExportFile:
      	s_result := FExportFile;
      sr_Remark:
      	s_result := Remark;
      sr_DateTime:
      	if SystemTimeToString (CreateTime, dtmDate) <> SystemTimeToString(ModifyTime, dtmDate) then
         	s_result := GetDatePeriod (CreateTime, ModifyTime)
         else
         	s_result := SystemTimeToLocaleStr (ModifyTime);
      else
      	result := false;
   end;
end;

function TTruePageProperty.GetExportFile (): widestring;
begin
	result := RelToFullPath (FExportFile, ProgDir);
end;

procedure TTruePageProperty.SetExportFile (const value: widestring);
begin
	FExportFile := FullToRelPath (value, ProgDir);
end;

procedure TTruePageProperty.ResetSave (b_externalsave: boolean; const s_exportfile: widestring);
var s_text: widestring;
   b_resave: boolean;
begin
	b_resave := (b_externalsave <> FExternalSave) or (Fexternalsave and (s_exportfile <> FExportFile));
	if b_resave then  // 重新存储
   begin
   	s_text := PageStore[FPageId].Text;
      if not FExternalSave then   // 由内部存储转向外部存储，清空数据库中内容。
      	PageStore[FPageId].Text := '';
   end;
   FExternalSave := b_ExternalSave;
   ExportFile := s_exportfile;
   if b_resave then
   	PageStore[FPageId].Text := s_text;
   PageCenter.EventNotify (pctFieldModified, FPageId);
end;

//--------------------------

constructor TPageChilds.Create(APageId: integer);
begin
	inherited Create (APageId);
   FChilds := TxlIntList.Create;
   FChilds.Separator := ',';
end;

destructor TPageChilds.Destroy ();
begin
	FChilds.free;
	inherited;
end;

procedure TPageChilds.Load (o_list: TxlStrList);
var i: integer;
begin
  	FChilds.Text := o_list[0];
   for i := FChilds.High downto FChilds.Low do
   	if not PageStore.PageValid (FChilds[i]) then
      	FChilds.Delete (i);
   o_list.Delete (0);
end;

procedure TPageChilds.Save (o_list: TxlStrList);
begin
	o_list.Add (FChilds.Text);
end;

procedure TPageChilds.Clone (p: TPropertySuper);
begin
end;

procedure TPageChilds.AddChild (id: integer; sid: integer = -1);
begin
	if FChilds.ItemExists (id) then exit;
   if sid < 0 then
   begin
   	FChilds.Add (id);
      PageCenter.EventNotify (pctAddChild, FPageId, id);
   end
   else
      AddChildByPos (id, FChilds.Find (sid));
end;

procedure TPageChilds.AddChildByPos (id: integer; i_pos: integer = -1);
begin
	if FChilds.ItemExists (id) then exit;
   if FChilds.PosValid (i_pos) then
   	FChilds.Insert (i_pos, id)
   else
   	FChilds.Add (id);
   PageCenter.EventNotify (pctAddChild, FPageId, id);
end;

procedure TPageChilds.RemoveChild (id: integer);
begin
	if FChilds.ItemExists (id) then
   begin
      PageCenter.EventNotify (pctRemoveChild, FPageId, id);
		FChilds.DeleteByValue (id);
   end;
end;

procedure TPageChilds.RemoveChildByPos (i_pos: integer);
var id: integer;
begin
	if FChilds.PosValid (i_pos) then
   begin
      PageCenter.EventNotify (pctRemoveChild, FPageId, FChilds[i_pos]);
		FChilds.Delete (i_pos);
   end;
end;

function TPageChilds.CanRemoveChild (id: integer): boolean;
begin
	if PageStore.PageValid (id) and (PageStore[id].ownerId = FPageId) then
   	result := PageStore[id].CanDelete
   else
		result := true;
end;

procedure TPageChilds.Clear ();
var i, id: integer;
begin
	for i := FChilds.High downto FChilds.Low do
   begin
   	id := FChilds[i];
   	RemoveChild (id);
      if (PageStore[id].OwnerId = FPageId) and (not PageStore[id].SingleInstance) then
      	PageStore[id].Delete;
   end;
end;

function TPageChilds.CanClear (): boolean;
var i: integer;
begin
	result := false;
	for i := FChilds.Low to FChilds.High do
   	if not CanRemoveChild (FChilds[i]) then exit;
   result := true;
end;

function TPageChilds.FindChild (id: integer): integer;
begin
	result := FChilds.Find(id);
end;

function TPageChilds.IsEmpty (): boolean;
begin
	result := FChilds.IsEmpty;
end;

function TPageChilds.Count (): integer;
begin
	result := FChilds.Count;
end;

function TPageChilds.GetNewChildName (pt: TPageType): widestring;
   function f_PageNameExists (const s_name: widestring): boolean;
   var i: integer;
   begin
      result := false;
      for i := FChilds.Low to FChilds.High do
         if PageStore.PageValid(FChilds[i]) and (PageStore[FChilds[i]].Name = s_name) then
         begin
            result := true;
            break;
         end;
   end;
var i: integer;
   s_defname: widestring;
begin
	s_defname := PageNameMan.GetDefName (pt);
   i := 0;
   repeat
      inc (i);
      result := s_defname + IntToStr(i);
   until not f_Pagenameexists (result);
end;

procedure TPageChilds.GetChildList (o_list: TxlIntList);
var i: integer;
begin
	o_list.Clear;
   i := FChilds.Low;
   while i <= FChilds.High do
   begin
   	if (FChilds[i] <> FPageId) and PageStore.PageValid (FChilds[i]) then
      begin
   		o_list.Add (FChilds[i]);
         inc (i);
      end
      else
      	FChilds.Delete(i);
   end;
end;

function TPageChilds.ChildId (index: integer): integer;
begin
	result := FChilds[index];
end;

function TPageChilds.HasChildInTree (): boolean;
var p: TPageSuper;
	i: integer;
begin
	result := false;
   p := PageStore[FPageId];
   for i := FChilds.Low to FChilds.High do
   	if PageStore.PageValid (FChilds[i]) and p.ChildShowInTree (PageStore[FChilds[i]].PageType) then
      begin
      	result := true;
         exit;
      end;
end;

//---------------------

constructor TListProperty.Create (APageId: integer);
begin
   inherited Create (APageId);
	FColList := TxlIntList.Create;
   FColList.Separator := ',';
   FWidthList := TxlIntList.Create;
   FWidthList.Separator := ',';
end;

destructor TListProperty.Destroy ();
begin
	FColList.Free;
   FWidthList.Free;
   inherited;
end;

procedure TListProperty.Load (o_list: TxlStrList);
var i, n: integer;
begin
//   pc := PageControl;

   FColList.Text := o_list[0];
   FWidthList.Text := o_list[1];
   n := FWidthList.High;
   for i := n + 1 to FColList.High do     // 防止万一 widthlist 的长度小于 collist
      FWidthList.Add (100);
   CheckBoxes := StrToBool (o_list[2]);
   FView := TListPageView(StrToIntDef (o_list[3]));
   FullRowSelect := StrToBool (o_list[4]);
   GridLines :=  StrToBool (o_list[5]);
   o_list.Delete (0, 6);

//   f_CheckChange (pc);
end;

procedure TListProperty.Save (o_list: TxlStrList);
begin
	with o_list do
   begin
      Add (FColList.Text);
      Add (FWidthList.Text);
      Add (BoolToStr(FCheckBoxes));
      Add (IntToStr(Ord(FView)));
      Add (BoolToStr(FFullRowSelect));
      Add (BoolToStr(FGridLines));
   end;
end;

//------------------

function TListProperty.PageControl (): TPageControl;
begin
   if View = lpvBlog then
      result := pcBlog
   else
      result := pcListView;
end;

//procedure TListProperty.SetView (value: TListPageView);
//var pc: TPageControl;
//begin
//	if value = FView then exit;
//   pc := PageControl;
//   FView := value;
//   f_CheckChange (pc);
//end;

end.




