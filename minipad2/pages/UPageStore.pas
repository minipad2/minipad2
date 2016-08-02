unit UPageStore;

interface

uses UTypeDef, UPageSuper, UxlList, UxlClasses, UxlStack;

type
	TPageRecord = record
      id: integer;
      ownerid: integer;
      pagetype: TPageType;
      length: integer;
      value: widestring;
      Page: TPageSuper;
   end;
   PPageRecord = ^TPageRecord;

   TPageStore = class       // single instance
   private
   	FArray: TxlObjList;
		FTemprList: TxlStrList;
      FIdxFile, FDataFile: widestring;
      FIdRecycler: TxlIntQueue;

		function f_GetPageByINdex (index: integer): TPageSuper;
		function f_DoCreateNewPage (i_id, i_ownerid: integer; pt: TPageType): TPageSuper;
      function f_GetOffset (n: integer): integer;
		function f_GetOwnerId (p: PPageRecord): integer; // ownerid 可能在运行过程中改变，故不能简单用record.ownerid代替
		function f_InRecycler (p: PPageRecord): boolean;
      function GetPageById (id: integer): TPageSuper;
		procedure LoadIndex ();
   public
   	constructor Create (const s_workspace: widestring);
      destructor Destroy (); override;
      procedure SaveIndex ();

   	function NewPage (ownerid: integer; pt: TPageType): integer;  // return id
      procedure DeletePage (id: integer);

		function RetrieveText (id: integer; i_maxchar: integer = -1): widestring;
      function SaveText (id: integer; const value: widestring): boolean;  // return saved or not
      function TextLength (id: integer): integer;

      property Pages [id: integer]: TPageSuper read GetPageById; default;
      function PageValid (id: integer): boolean;
      procedure GetPageTypeList (o_list: TxlIntList);
      procedure GetPageList (pt: TPageType; o_list: TxlIntList);  // 若 pt = ptNone，则获取所有 CanSearch 的页面
      function GetPageCount (pt: TPageType): integer;
      function GetFirstPageId (pt: TPageType; var id: integer): boolean;

      function GetPagePath (p: TPageSuper): widestring;
      function FindPageByPath (const s_path: widestring; var p: TPageSuper): boolean;
   end;

function PageStore (): TPageStore;
function Root (): TPageSuper;

const RootId = -1; RootParent = -2;    // 为了便于导入 3.1 版的数据，RootId 须为 -1，以便 GroupRoot 为 0

implementation

uses Windows, UxlFunctions, UxlStrUtils, UxlIniFile, UxlFile, UxlMath, Resource, UGlobalObj, ULangManager, UPageFactory,
	UxlDateTimeUtils, UVersionManager, UxlListSuper, UOptionManager;

var FPageStore: TPageStore;

procedure f_SplitStr (const s_source: widestring; var i: integer; var s: widestring);
var n: integer;
begin
   n := FirstPos (#9, s_source);
   i := StrToIntDef (LeftStr(s_source, n - 1));
   s := MidStr(s_source, n + 1);
end;

function PageStore (): TPageStore;
begin
	if FPageStore = nil then
   	FPageStore := TPageStore.Create ('minipad2');
   result := FPageStore;
end;

function Root (): TPageSuper;
begin
	result := PageStore[RootId];
end;

//----------------

constructor TPageStore.Create (const s_workspace: widestring);
begin
	FArray := TxlObjList.Create (1000);
   FArray.SortType := stByIndexNoDup;
	FTemprlist := TxlStrList.create (25);
   FTemprList.Separator := #9;
   FIdRecycler := TxlIntQueue.Create;
   FIdRecycler.Separator := ',';
   
	FIdxFile := DataDir + s_workspace + '.idx';
   FDataFile := DataDir + s_workspace + '.dat';
//   CheckVersion (FIdxFile, FDataFile);
   LoadIndex;
end;

destructor TPageStore.Destroy ();
var i: integer;
	p: PPageRecord;
begin
   FTemprList.Free;
   FIdRecycler.free;

   for i := FArray.Low to FArray.High do
   begin
   	p := PPageRecord (FArray[i]);
      p.Page.Free;
      dispose (p);
   end;
   FArray.free;
   inherited;
end;

procedure TPageStore.LoadIndex ();
var o_file: TxlMemoryTextFile;
   s_line, s_version, s, s2, s3: widestring;
   pt: integer;
   p: PPageRecord;
begin
   o_file := TxlMemoryTextFile.create (FIdxFile, fmRead, enUTF16LE);
   if not o_file.EOF then o_file.ReadLn (s_version);
   if not o_file.EOF then
   begin
   	o_file.ReadLn (s);
      FIdRecycler.Text := s;
   end;

   while not o_file.EOF do
   begin
   	o_file.ReadLn (s_line);
      new (p);
      f_SplitStr (s_line, p^.id, s);
      f_SplitStr (s, p^.ownerid, s2);
      f_SplitStr (s2, pt, s3);
      p^.PageType := TPageType(pt);
      f_SplitStr (s3, p^.length, p^.value);
      p^.page := nil;
      FArray.AddByIndex (p^.id, p);
   end;
   o_file.free;
end;

procedure TPageStore.SaveIndex ();
var o_list: TxlStrList;
	i: integer;
   s_line: widestring;
   p: PPageRecord;
   o_file: TxlTextFile;
begin
	o_list := TxlStrList.Create (FArray.Count + 2);
   o_list.Add (Version);
   o_list.Add (FIdRecycler.Text);
   for i := FArray.Low to FArray.High do
   begin
   	p := PPageRecord (FArray[i]);
		s_line := IntToStr(p^.id) + #9 + IntToStr(f_GetOwnerId(p)) + #9 + IntToStr(Ord(p^.PageType)) + #9 + IntToStr(p^.Length) + #9;
      if p^.Page <> nil then
      begin
         p^.Page.Save (FTemprList);
         s_line := s_line + FTemprList.Text;
      end
      else
         s_line := s_line + p^.Value;
      o_list.Add (s_line);
   end;

   o_file := TxlTextFile.Create (FIdxFile, fmWrite, enUTF16LE, OptionMan.Options.EncryptDataFile);
   o_file.WriteText (o_list.Text);
   o_file.free;
//   o_list.SaveToFile (FIdxFile);       // 使用 TxlStrList 一次性 WriteText，比早期版本的逐行 WriteLn 要安全一些，尽管速度稍慢。这是为了防止由于某些因素文件只写了一半就意外中断。
   o_list.free;
end;

function TPageStore.PageValid (id: integer): boolean;
begin
   result := FArray.IndexValid (id);
end;

function TPageStore.GetPageById (id: integer): TPageSuper;
var i: integer;
begin
   i := FArray.FindByIndex (id);
   if i >= 0 then
   	result := f_GetPageByIndex (i)
   else if id = RootId then
      result := f_DoCreateNewPage (id, RootParent, ptRoot)
   else
		result := nil;
end;

function TPageStore.f_GetPageByINdex (index: integer): TPageSuper;
var p: PPageRecord;
begin
   p := PPageRecord (FArray[index]);
   if not assigned (p^.Page) then
   begin
      p^.Page := PageFactory.NewPage (p^.PageType, p^.id, p^.OwnerId);
      FTemprList.Text := p^.value;
      p^.Page.Load (FTemprList);
      p^.value := '';
   end;
   result := p^.Page;
end;

function TPageStore.f_GetOwnerId (p: PPageRecord): integer;
begin
	if p^.page <> nil then
   	result := p^.page.ownerid
   else
   	result := p^.ownerid;
end;

procedure TPageStore.GetPageTypeList (o_list: TxlIntList);
var i, pt: integer;
begin
	o_list.Clear;
   for i := FArray.Low to FArray.High do
   begin
   	pt := Ord (PPageRecord(FArray[i]).PageType);
   	if not o_list.itemExists (pt) then
      	o_list.AddByIndex (pt, pt);
	end;
   o_list.SortByIndex;
end;

function TPageStore.f_InRecycler (p: PPageRecord): boolean;
begin
   result := false;
   while p^.id <> RootId do
   begin
      p := PPageRecord (FArray.ItemsByINdex[f_GetOwnerId(p)]);
      if p^.PageType = ptRecycleBin then
      begin
         result := true;
         exit;
      end;
   end;
end;

procedure TPageStore.GetPageList (pt: TPageType; o_list: TxlIntList);
var i: integer;
	p: PPageRecord;
   b: boolean;
begin
	o_list.Clear;
	for i := FArray.Low to FArray.High do
   begin
      p := PPageRecord (FArray[i]);
      b := (p^.PageType = pt) or ((pt = ptNone) and PageFactory.GetClass (p^.PageType).CanSearch);
   	if b and (not f_InRecycler(p)) then
      	o_list.Add (p^.id);
   end;
end;

function TPageStore.GetPageCount (pt: TPageType): integer;
var i: integer;
	p: PPageRecord;
begin
	result := 0;
	for i := FArray.Low to FArray.High do
   begin
      p := PPageRecord (FArray[i]);
   	if (p^.PageType = pt) and (not f_InRecycler(p)) then
      	inc (result);
   end;
end;

function TPageStore.GetFirstPageId (pt: TPageType; var id: integer): boolean;
var i: integer;
	p: PPageRecord;
begin
	result := false;
	for i := FArray.Low to FArray.High do
   begin
   	p := PPageRecord (FArray[i]);
   	if p^.PageType = pt then
      begin
      	id := p^.id;
         result := true;
         exit;
      end;
   end;
end;

function TPageStore.GetPagePath (p: TPageSuper): widestring;
var o_list: TxlStrList;
begin
	o_list := TxlStrList.Create;
   o_list.Separator := '\';

   while p <> Root do
   begin
      o_list.InsertFirst (p.name);
      p := p.Owner;
   end;
	result := o_list.Text;
   o_list.free;
end;

function TPageStore.FindPageByPath (const s_path: widestring; var p: TPageSuper): boolean;
var i: integer;
begin
	result := false;
	for i := FArray.Low to FArray.High do
   begin
      p := f_GetPageByIndex (i);
   	if IsSameStr (GetPagePath(p), s_path) then
      begin
         result := true;
			break;
      end;
   end;
end;

function TPageStore.NewPage (ownerid: integer; pt: TPageType): integer;    // return id
begin
   if PageFactory.GetClass (pt).SingleInstance and GetFirstPageId (pt, result) then exit;

   if not FIdRecycler.Pop (result) then
      result := FArray.GetNewIndex;
   f_DoCreateNewPage (result, ownerid, pt);
end;

function TPageStore.f_DoCreateNewPage (i_id, i_ownerid: integer; pt: TPageType): TPageSuper;
var p: PPageRecord;
begin
	new (p);
   with p^ do
   begin
   	id := i_id;
      OwnerId := i_ownerid;
      PageType := pt;
   	length := 0;
		page := PageFactory.NewPage (pt, i_id, i_ownerid);
		page.Initialize ();
   end;
   FArray.AddByIndex (i_id, p);
   result := p^.page;
end;

procedure TPageStore.DeletePage (id: integer);
var i: integer;
	p: PPageRecord;
begin
	i := FArray.FindByIndex (id);
   if i >= 0 then
   begin
   	p := PPageRecord (FArray[i]);
      SaveText (p^.id, '');
      FIdRecycler.Push (p^.id);
      FArray.Delete (i);
      p^.page.free;
      dispose (p);
   end;
end;

//------------------

function TPageStore.f_GetOffset (n: integer): integer;
var i: integer;
begin
   result := 0;
   for i := 0 to n - 1 do
   	inc (result, PPageRecord(FArray[i])^.length);
end;

function TPageStore.RetrieveText (id: integer; i_maxchar: integer = -1): widestring;
var i_offset, n, i_len: integer;
   o_file: TxlTextFile;
begin
	result := '';
   if (i_maxchar = 0) then exit;
	n := FArray.FindByIndex (id);
   if (n < 0) then exit;
   i_len := PPageRecord(FArray[n])^.Length;
   if i_len = 0 then exit;

   i_offset := f_GetOffset (n);
   try
      o_file := TxlTextFile.create(FDataFile, fmRead, enUTF16LE);
      if o_file.TextCount >= i_offset then
      begin
         o_file.CharIndex := i_Offset;
         if InRange (i_maxchar, 1, i_len - 1) then
         begin
            o_file.ReadText (result, i_maxchar);
            result := result + ' ...';
         end
         else
            o_file.ReadText (result, i_len);
      end;
   finally
	   o_file.free;
   end;
end;

function TPageStore.SaveText (id: integer; const value: widestring): boolean;
var o_file: TxlTextFile;
   s1, s2: widestring;
   i_offset, i_count, len1, pos2, len2, n: integer;
   p: PPageRecord;
begin
	result := false;
	n := FArray.FindByIndex (id);
   if n < 0 then exit;
   p := PPageRecord (FArray[n]);
   if (p^.Length = length(value)) and (RetrieveText (id) = value) then exit;

   i_offset := f_GetOffset (n);
   o_file := TxlTextFile.create (FDataFile, fmRead, enUTF16LE);
   i_count := o_file.TextCount;
   len1 := Min (i_offset, i_count);
   pos2 := Min(i_offset + p^.Length, i_count);
   len2 := i_count - pos2;

   with o_file do
   begin
      readText (s1, len1);
      CharIndex := pos2;
      readText (s2, len2);

      reset (fmWrite, enUTF16LE, OptionMan.Options.EncryptDataFile);
      WriteText (s1);
      writeText (value);
      writeText (s2);
   end;
   o_file.free;

   p^.length := length(value);
   result := true;
   SaveIndex;
end;

function TPageStore.TextLength (id: integer): integer;
var n: integer;
begin
	result := 0;
   n := FArray.FindByIndex (id);
   if n >= 0 then
   	result := PPageRecord(FArray[n])^.Length;
end;

//------------------

initialization

finalization
	FPageStore.free;
   
end.





