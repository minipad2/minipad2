unit UPageFactory;

interface

uses UxlImageList, UPageSuper, UPageProperty, UTypeDef, UxlList;

type
   TPageFactory = class     // single instance
   private
   	FClasses: array of TPageClass;
   public
		procedure RegisterClass (o_class: TPageClass);
      function NewPage (pt: TPageType; i_id, i_ownerid: integer): TPageSuper;
      procedure GetPageTypeList (o_list: TxlIntList);
		function GetClass (pt: TPageType): TPageClass;
   end;

   TPageImageList = class   // single instance
   private
		FImageIndex: TxlIntList;  // PageType -- Offset
      FFileIndex: TxlIntList;  // File -- Index
      FImages: TxlImageList;
      FLargeImages, FSmallImages: TxlImageList;    // TreeView 与 TabControl 可共享一个 ImageList, ListView 不行，因会自动在图标上混入背景色
   public
   	constructor Create ();
		destructor Destroy (); override;
		procedure AddImageWithOverlay (pt: TPageType; i_icon: integer);
      procedure AddImages (pt: TPageType; arr: array of integer; b_withgrayed: boolean = false);

      function IndexOf (pt: TPageType): integer;
      function DefaultIconId (pt: TPageType): integer;
      function ImageFromFile (s_file: widestring): integer;
      property Images: TxlImageList read FImages;
      property LargeImages: TxlImageList read FLargeImages;
      property SmallImages: TxlImageList read FSmallImages;
   end;

   TPageNameManager = class   // single instance
   private
   	FTypes: array of TPageType;
      FIdNames: array of integer;
   public
   	procedure RegisterDefName (pt: TPageType; id_name: integer);
      function GetDefName (pt: TPageType): widestring;
   end;

   TListPageDefSettingsManager = class  // single instance, 缓存记忆设置，防止反复读取 Ini 文件。
   private
      FDefSettings: TxlStrList;
   public
   	constructor Create ();
      destructor Destroy (); override;
   	procedure LoadDefSettings (pt: TPageType; lp: TListProperty);
      procedure SaveDefSettings (pt: TPageType; lp: TListProperty);
   end;

  	IPageObserver = interface
   	procedure OnPageEvent (pct: TPageEvent; id, id2: integer);
   end;

   TPageCenter = class
   private
      FObservers: TxlInterfaceList;
      FActivePage: TPageSuper;
      procedure SetActivePage (p: TPageSuper);
   public
   	constructor Create ();
      destructor Destroy (); override;

      procedure AddObserver (value: IPageObserver);
      procedure RemoveObserver (value: IPageObserver);
      procedure EventNotify (pct: TPageEvent; id: integer; id2: integer = -1);

      property ActivePage: TPageSuper read FActivePage write SetActivePage;
	end;

function PageFactory (): TPageFactory;
function PageImageList (): TPageImageList;
function PageNameMan (): TPageNameManager;
function PageDefSettingsMan (): TListPageDefSettingsManager;
function PageCenter (): TPageCenter;

implementation

uses Windows, ShellAPI, UxlWinDef, Resource, ULangManager, UPageStore, UxlStrUtils, UxlIniFile, UxlFunctions,
	UxlDateTimeUtils, UxlCommDlgs;

var FPageFactory: TPageFactory;
	FPageImageList: TPageImageList;
   FPageNameMan: TPageNameManager;
   FPageDefSettingsMan: TListPageDefSettingsManager;
   FPageCenter: TPageCenter;

function PageFactory (): TPageFactory;
begin
	if not assigned (FPageFactory) then
   	FPageFactory := TPageFactory.Create;
   result := FPageFactory;
end;

function PageImageList (): TPageImageList;
begin
	if not assigned (FPageImageList) then
   	FPageImageList := TPageImageList.Create;
   result := FPageImageList;
end;

function PageNameMan (): TPageNameManager;
begin
	if not assigned (FPageNameMan) then
   	FPageNameMan := TPageNameManager.Create;
   result := FPageNameMan;
end;

function PageDefSettingsMan (): TListPageDefSettingsManager;
begin
	if not assigned (FPageDefSettingsMan) then
   	FPageDefSettingsMan := TListPageDefSettingsManager.Create;
   result := FPageDefSettingsMan;
end;

function PageCenter (): TPageCenter;
begin
	if not assigned (FPageCenter) then
   	FPageCenter := TPageCenter.Create;
   result := FPageCenter;
end;

//------------------

procedure TPageFactory.RegisterClass (o_class: TPageClass);
var n: integer;
begin
	n := Length (FClasses);
   SetLength (FClasses, n + 1);
   FClasses[n] := o_class;
end;

function TPageFactory.NewPage (pt: TPageType; i_id, i_ownerid: integer): TPageSuper;
begin
	result := GetClass (pt).Create (i_id);
   result.ownerid := i_ownerid;
end;

function TPageFactory.GetClass (pt: TPageType): TPageClass;
var i: integer;
begin
	for i := Low (FClasses) to High (FClasses) do
   	if FClasses[i].PageType = pt then
      begin
      	result := FClasses[i];
         break;
      end;
end;

procedure TPageFactory.GetPageTypeList (o_list: TxlIntList);
var i, pt: integer;
begin
	o_list.clear;
   for i := Low(FClasses) to High(FClasses) do
   begin
   	pt := Ord(FClasses[i].PageType);
   	o_list.AddByIndex (pt, pt);
   end;
   o_list.SortByINdex;
end;

//----------------------

const c_overlay: array[0..2] of integer = (ic_lockedmask, ic_protectedmask, ic_readonlymask);

constructor TPageImageList.Create ();
	function f_CreateImageList (i_width: integer): TxlImageList;
   var i: integer;
   begin
      result := TxlImageList.Create (i_width, i_width);
      for i := 0 to Length(c_overlay) - 1 do
      	result.AddOverlay (c_overlay[i]);
	end;
begin
	FImages := f_CreateImageList (GetSystemMetrics (SM_CXSMICON));
	FLargeImages := f_CreateImageList (GetSystemMetrics (SM_CXICON));
   FSmallImages := f_CreateImageList (GetSystemMetrics (SM_CXSMICON));
   FImageIndex := TxlIntList.Create;
   FFileIndex := TxlIntList.Create;
end;

destructor TPageImageList.Destroy ();
begin
	FFileIndex.free;
   FImageIndex.free;
	FImages.free;
   FLargeImages.free;
   FSmallImages.free;
	inherited;
end;

procedure TPageImageList.AddImageWithOverlay (pt: TPageType; i_icon: integer);
	function f_AddImageWithOverlay (o_image: TxlImageList; i_icon: integer): integer;
   var i: integer;
   begin
   	result := o_image.AddIcon (i_icon);
      for i := 1 to Length(c_overlay) do
         o_image.CreateOverlayedIcon (result, i);
   end;
var n: integer;
begin
   n := f_AddImageWithOverlay (FImages, i_icon);
   f_AddImageWithOverlay (FLargeImages, i_icon);
   f_AddImageWithOverlay (FSmallImages, i_icon);
   FImageIndex.AddByIndex (Ord(pt), n);
end;

procedure TPageImageList.AddImages (pt: TPageType; arr: array of integer; b_withgrayed: boolean = false);
var i, n: integer;
begin
	for i := 0 to Length(arr) - 1 do
   begin
   	n := FImages.AddIcon (arr[i]);
      FLargeImages.AddIcon (arr[i]);
      FSmallImages.AddIcon (arr[i]);
      if b_withgrayed then
      begin
      	FImages.AddIconGrayed (arr[i]);
         FLargeImages.AddIconGrayed (arr[i]);
         FSmallImages.AddIconGrayed (arr[i]);
      end;
      if i = 0 then FImageIndex.AddByIndex (Ord(pt), n);
   end;
end;

function TPageImageList.IndexOf (pt: TPageType): integer;
begin
	result := FImageIndex.ItemsByIndex [Ord(pt)];
end;

function TPageImageList.DefaultIconId (pt: TPageType): integer;
var n: integer;
begin
	n := IndexOf (pt);
	result := FImages.Icons[n];
end;

function TPageImageList.ImageFromFile (s_file: widestring): integer;
var h_icon: HIcon;
begin
	result := -1;
	s_file := LowerCase(s_file);
   if FFileIndex.KeyValid (s_file) then
		result := FFileIndex.ItemsByKey [s_file]
   else
   begin
   	if leftstr(lowercase(s_file), 5) = 'file:' then
      	s_file := MidStr(s_file, 6);
      if leftstr(s_file, 2) = '//' then
      	s_file := MidStr(s_file, 3);
		h_icon := LoadIconFromFile (s_file);
      if h_icon > 0 then
      begin
         result := FImages.AddHIcon (h_icon);
         FLargeImages.AddHIcon (h_icon);
         FSmallImages.AddHIcon (h_icon);
         DestroyIcon (h_icon);
         FFileIndex.AddByKey (s_file, result);
   	end;
   end;
end;

//----------------------

procedure TPageNameManager.RegisterDefName (pt: TPageType; id_name: integer);
var n: integer;
begin
	n := Length (FTypes);
   SetLength (FTypes, n + 1);
   FTypes[n] := pt;
   SetLength (FIdNames, n + 1);
   FIdNames[n] := id_name
end;

function TPageNameManager.GetDefName (pt: TPageType): widestring;
var i: integer;
begin
	for i := Low(FTypes) to High(FTypes) do
   	if FTypes[i] = pt then
      begin
      	result := LangMan.GetItem (FIdNames[i]);
         break;
      end;
end;

//----------------------

constructor TListPageDefSettingsManager.Create ();
begin
	FDefSettings := TxlStrList.Create;
end;

destructor TListPageDefSettingsManager.Destroy ();
begin
	FDefSettings.free;
   inherited;
end;

procedure TListPageDefSettingsManager.LoadDefSettings (pt: TPageType; lp: TListProperty);
var o_list: TxlStrList;
   o_class: TPageClass;
   o_inifile: TxlIniFile;
begin
	o_list := TxlStrList.Create;
   o_list.Separator := ';';
   if FDefSettings.IndexValid (Ord(pt)) then
   begin
  		o_list.Text := FDefSettings.ItemsByIndex[Ord(pt)];
      lp.Load (o_list);
   end
   else
   begin
      o_inifile := TxlIniFile.Create (ProgIni);
      o_inifile.Section := 'PageDefSettings';
      o_list.Text := o_inifile.ReadString (IntToStr(Ord(pt)), '');
      o_inifile.Free;
      if o_list.Count = 0 then
      begin
         o_class := PageFactory.GetClass (pt);
         o_class.InitialListProperty (lp);
         lp.Save (o_list);
      end
      else
      	lp.Load (o_list);
      FDefSettings.AddByIndex (Ord(pt), o_list.Text);
   end;
   o_list.free;
end;

procedure TListPageDefSettingsManager.SaveDefSettings (pt: TPageType; lp: TListProperty);
var o_list: TxlStrList;
begin
	o_list := TxlStrList.Create;
   lp.Save (o_list);
	o_list.Separator := ';';
	FDefSettings.ItemsByIndex[Ord(pt)] := o_list.Text;
   with TxlIniFile.Create (ProgIni) do
   begin
      Section := 'PageDefSettings';
      WriteString (IntToStr(Ord(pt)), o_list.Text);
      free;
   end;
   o_list.free;
end;

//-----------------------------

constructor TPageCenter.Create ();
begin
   FObservers := TxlInterfaceList.create;
//   FExecutors := TxlInterfaceList.Create;
end;

destructor TPageCenter.Destroy ();
begin
//	FExecutors.Free;
   FObservers.Free;
	inherited;
end;

procedure TPageCenter.AddObserver (value: IPageObserver);
begin
	FObservers.Add (value);
end;

procedure TPageCenter.RemoveObserver (value: IPageObserver);
begin
	FObservers.Remove (value);
end;

procedure TPageCenter.EventNotify (pct: TPageEvent; id, id2: integer);
var i: integer;
begin
   for i := FObservers.Low to FObservers.High do
   	IPageObserver(FObservers[i]).OnPageEvent (pct, id, id2);
end;

procedure TPageCenter.SetActivePage (p: TPageSuper);
var pid, id: integer;
begin
	if (p = nil) then    // 创建第一个页
   begin
   	PageStore.GetFirstPageId (ptGroupRoot, pid);
      id := PageStore.NewPage (pid, ptNote);
      PageStore[pid].Childs.AddChild (id);
      p := PageStore[id];
   end;
   if (p = Root) then
   begin
   	PageStore.GetFirstPageId (ptGroupRoot, id);
      p := PageStore[id];
   end;
   if  (p.Owner.Childs.FindChild(p.id) < 0) then
   begin
   	PageStore.GetFirstPageId (ptGroupRoot, id);
      p := PageStore[id];
   end;

//   if p.Owner = RecycleBin then exit;
	if FActivePage <> nil then
   	EventNotify (pctSave, FActivePage.id);
	if (FActivePage = nil) or (p.PageControl <> FActivePage.PageControl) then
   	EventNotify (pctControlChange, p.id);
	FActivePage := p;
   p.PageProperty.VisitTime := Now;
	EventNotify (pctSetActive, p.id);
end;

//-------------------------

initialization

finalization
	FPageFactory.Free;
   FPageDefSettingsMan.Free;
   FPageImageList.Free;
   FPageNameMan.Free;
   FPageCenter.Free;
   
end.



