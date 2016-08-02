unit UxlListSuper;

interface

uses UxlClasses;

type
	TListItem = record
      index: integer;
      key: WideString;
      strval: WideString;
      intval: integer;
      objval: pointer;
      interfval: IInterface;
   end;
	PListItem = ^TListItem;

	TListFindMode = (fmByIndex, fmByKey, fmByStrVal, fmByIntVal, fmByObjVal, fmByInterfVal);
	TListDirection = (dFirst, dPrior, dCurrent, dNext, dLast);
   TListSortType = (stNone, stByIndex, stByKey, stByIndexNoDup, stByKeyNoDup);

   TxlListSuper = class (TxlCollection)
   private
      procedure SetMinSize (value: cardinal);
      procedure f_SetIndexList (const s_text: widestring);
      function f_GetIndexList (): widestring;

      function f_GetIndex (i_pos: integer): integer;
      procedure f_SetIndex (i_pos: integer; i_index: integer);
      function f_GetKey (i_pos: integer): widestring;
      procedure f_SetKey (i_pos: integer; const s_key: widestring);
      function f_FindPosBySortedIndex (index: integer; i_start, i_end: integer; b_match: boolean): integer;
      function f_FindPosBySortedKey (const s_key: widestring; i_start, i_end: integer; b_match: boolean): integer;

      procedure f_DeletePointer (i_pos: integer);
      procedure f_SetSortType (value: TListSortType);
   protected
      FPList: array of PListItem;
      FCount: cardinal;
      FMinSize: cardinal;     // to increase performance
      FPos: integer;
      Fseparator: WideString;
      FSortType: TListSortType;

      procedure f_OnCreate (); virtual;
      procedure f_Shift (i_startpos, i_endpos, i_offset: integer);
      function f_newitem (): PListItem;
      function f_insertitem (i_pos: integer; q: PListItem): integer;
      function f_find (const o_item: TListItem; fmMode: TListFindMode): integer;
      function f_GetPointerByDir (dDir: TListDirection; var p: PListItem): boolean;
   public
      constructor Create (i_Minsize: cardinal = 100);
      destructor Destroy (); override;
      function Count (): integer; override;
      procedure Clear ();
   //   procedure Clone (value: TxlListSuper);

      function Delete (i_pos: integer; i_count: integer = 1): boolean;
      function DeleteByIndex (i_index: integer): boolean;
      function DeleteByKey (const s_key: widestring): boolean;

      function FindByIndex (i_index: integer): integer;
      function FindByKey (const s_key: WideString): integer;
      procedure SortByIndex (b_noduplicate: boolean = false);
      procedure SortByKey (b_noduplicate: boolean = false);
      function GetNewIndex (): cardinal;     // >= 1

      function SwapPos (i_pos1, i_pos2: integer): boolean;
      function MovePos (i_pos1, i_pos2: integer; b_insertbefore: boolean = true): integer;
      function ShiftPos (i_pos, i_dir: integer): integer;
      function IndexValid (idx: integer): boolean;
      function KeyValid (const s_key: widestring): boolean;

      property MinSize: cardinal read FMinSize write SetMinSize;
      property Position: integer read FPos write FPos;    //start from 0
      property Separator: widestring read FSeparator write FSeparator;
      property SortType: TListSortType read FSortType write f_SetSortType;
      property Indexes[i_pos: integer]: integer read f_GetIndex write f_SetIndex;
      property IndexList: widestring read f_GetIndexList write f_SetIndexList;
      property Keys[i_pos: integer]: widestring read f_GetKey write f_SetKey;
   end;

   TxlStreamList = class (TxlListSuper)
   private
   protected
      function GetText (): widestring; virtual; abstract;
      procedure SetText (const s_text: widestring); virtual; abstract;
   public
      procedure LoadFromFile (const s_file: widestring);
      procedure SaveToFile (const s_file: widestring);

      property Text: widestring read GetText write SetText;
   end;
   
implementation

uses UxlFunctions, UxlStrUtils, UxlMath, UxlFile;

constructor TxlListSuper.Create (i_minsize: cardinal = 100);
begin
	MinSize := i_minsize;
	setlength (FPList, i_minsize);
   Separator := #13#10;
   FCount := 0;
   SortType := stNone;
end;

procedure TxlListSuper.f_OnCreate ();
begin
end;

destructor TxlListSuper.destroy ();
begin
	clear();
   inherited;
end;

function TxlListSuper.Count (): integer;
begin
	result := FCount;
end;

procedure TxlListSuper.Clear ();
var i: integer;
begin
	for i := Low to High do
   	if FPList[i]^.interfval = nil then dispose (FPList[i]);
   if Count <> FMinSize then
	   setlength (FPList, FMinSize);
   FCount := 0;
   FPos := -1;
end;

procedure TxlListSuper.SetMinSize (value: cardinal);
begin
	FMinSize := value;
   if Count < FMinSize then
   	SetLength (FPList, value);
end;

function TxlListSuper.IndexValid (idx: integer): boolean;
begin
	result := PosValid (FindByIndex(idx));
end;

function TxlListSuper.KeyValid (const s_key: widestring): boolean;
begin
	result := PosValid (FindByKey (s_key));
end;

function TxlListSuper.f_insertitem (i_pos: integer; q: PListItem): integer;
begin
	if ((SortType = stByIndexNoDup) and INdexValid (q^.index))
   	or ((SortType = stByKeyNoDup) and KeyValid (q^.key)) then
   begin
   	dispose (q);
      exit;
   end;

   if SortType in [stByIndex, stByIndexNoDup] then
      i_pos := f_FindPosBySortedIndex (q^.index, Low, High, false)
   else if SortType in [stByKey, stByKeyNoDup] then
   	i_pos := f_FindPosBySortedKey (q^.key, Low, High, false);
	result := ConfineRange (i_pos, 0, Count);

   inc (FCount);
   if Count > length(FPList) then
   	setlength (FPList, Count);
   f_Shift (result, High - 1, 1);
	FPList[result] := q;
//   f_OnChange (cctNew, result);
end;

function TxlListSuper.f_newitem (): PListItem;
begin
	new (result);
   with result^ do
   begin
   	index := 0;
   	key := '';
      strval := '';
      intval := 0;
      objval := nil;
      interfval := nil;
   end;
end;

//-------------------

procedure TxlListSuper.f_Shift (i_startpos, i_endpos, i_offset: integer);
var i: integer;
begin
	if i_startpos > i_endpos then exit;
	if i_offset > 0 then
   begin
		for i := i_endpos + i_offset downto i_startpos + i_offset do
      	FPList[i] := FPList[i - 1];
   end
   else if i_offset < 0 then
   begin
   	for i := i_startpos + i_offset to i_endpos + i_offset do
      	FPList[i] := FPList[i + 1];
   end;
end;

procedure TxlListSuper.f_DeletePointer (i_pos: integer);
begin
//   f_Onchange (cctDelete, i_pos);
	f_Shift (i_pos + 1, High, -1);
   Dec (FCount);
   if Count >= FMinSize then
   	setlength (FPList, Count);
end;

function TxlListSuper.Delete (i_pos: integer; i_count: integer = 1): boolean;
var i: integer;
begin
	result := false;
	for i := 1 to i_count do
   begin
      if not PosValid (i_pos) then exit;
      if FPList[i_pos]^.interfval = nil then
      	dispose (FPList[i_pos]);
      f_DeletePointer (i_pos);
      result := true;
   end;
end;

function TxlListSuper.DeleteByIndex (i_index: integer): boolean;
begin
	result := Delete (FindByIndex(i_index));
end;

function TxlListSuper.DeleteByKey (const s_key: widestring): boolean;
begin
   result := Delete (FindByKey(s_key));
end;

function TxlListSuper.FindByIndex (i_index: integer): integer;
var o_item: TListitem;
begin
	o_item.index := i_index;
	result := f_find (o_item, fmByIndex);
end;

function TxlListSuper.FindByKey (const s_key: WideString): integer;
var o_item: TListitem;
begin
	o_item.key := s_key;
	result := f_find (o_item, fmByKey);
end;

function TxlListSuper.f_find (const o_item: TListItem; fmMode: TListFindMode): integer;
	function f_compare (const a, b: TListItem; fmMode: TListFindMode): boolean;
   begin
   	case fmMode of
      	fmByIndex: result := (a.index = b.index);
         fmByKey: result := IsSameStr (a.key, b.key);
         fmByStrVal: result := IsSameStr (a.strval, b.strval);
         fmByIntVal: result := (a.intval = b.intval);
         fmByObjVal: result := (a.objval = b.objval);
         else result := (a.interfval = b.interfval);
      end;
   end;
var i: integer;
begin
	if (SortType in [stByIndex, stByIndexNoDup]) and (fmMode = fmByIndex) then
   	result := f_FindPosBySortedIndex (o_item.index, Low, HIgh, true)
	else if (SortType in [stByKey, stByKeyNoDup]) and (fmMode = fmByKey) then
   	result := f_FindPosBySortedKey (o_item.key, Low, High, true)
   else
   begin
      result := -1;
      for i := Low to High do
         if f_compare (FPList[i]^, o_item, fmMode) then
         begin
            result := i;
            exit;
         end;
   end;
end;

function TxlListSuper.GetNewIndex (): cardinal;
var i: integer;
begin
   result := 0;
	for i := Low to High do
   	if FPList[i]^.index > result then
      	result := FPList[i]^.index;
   inc (result);
end;

//-----------------------

procedure TxlListSuper.f_SetIndex (i_pos: integer; i_index: integer);
begin
   if PosValid (i_pos) then
   	FPList[i_pos]^.index := i_index;
end;

function TxlListSuper.f_GetIndex (i_pos: integer): integer;
begin
	if PosValid (i_pos) then
		result := FPList[i_pos]^.index
   else
   	result := 0;
end;

procedure TxlListSuper.f_SetKey (i_pos: integer; const s_key: widestring);
begin
   if PosValid (i_pos) then
   	FPList[i_pos]^.key := s_key;
end;

function TxlListSuper.f_GetKey (i_pos: integer): widestring;
begin
	if PosValid (i_pos) then
		result := FPList[i_pos]^.key
   else
   	result := '';
end;

//-------------------

function TxlListSuper.f_GetPointerByDir (dDir: TListDirection; var p: PListItem): boolean;
begin
   case dDir of
      dFirst: FPos := Low;
      dPrior: Dec (FPos);
      dNext: Inc (FPos);
      dLast: FPos := High;
 	end;

   result := PosValid (FPos);
   if result then
      p := FPList[FPos];
end;

function TxlListSuper.SwapPos (i_pos1, i_pos2: integer): boolean;
var p: plistitem;
begin
	result := PosValid (i_pos1) and PosValid (i_pos2);
   if result then
   begin
   	p := FPList[i_pos1];
      FPList[i_pos1] := FPList[i_pos2];
      FPList[i_pos2] := p;
   end;
end;

function TxlListSuper.MovePos (i_pos1, i_pos2: integer; b_insertbefore: boolean = true): integer;
var p: PListItem;
begin
   if (i_pos1 = i_pos2) or ((i_pos1 = i_pos2 - 1) and b_insertbefore) or ((i_pos1 = i_pos2 + 1) and (not b_insertbefore)) then
   begin
   	result := i_pos1;
      exit;
   end;

   if (not PosValid(i_pos1)) or (not PosValid(i_pos2)) then
   begin
   	result := -1;
      exit;
   end;

   p := FPList[i_pos1];

   if i_pos1 < i_pos2 then
	  	f_Shift (i_pos1 + 1, i_pos2, -1)
   else
   	f_Shift (i_pos2, i_pos1 - 1, 1);

   FPList[i_pos2] := p;
end;

function TxlListSuper.ShiftPos (i_pos, i_dir: integer): integer;
begin
	result := MovePos (i_pos, i_pos + i_dir, (i_dir < 0));
end;

//-------------------

procedure TxlListSuper.f_SetIndexList (const s_text: widestring);
	procedure f_addindex (const s: widestring);
   var p: PListItem;
   begin
      p := f_newitem;
      p^.index := StrToIntDef(s);
   end;
var ls, i_pos, i_pos1: integer;
begin
	Clear;
   if s_text = '' then exit;
	ls := length(FSeparator);
	i_pos1 := 1;
	i_pos := FirstPos (FSeparator, s_text, i_pos1);
   while i_pos > 0 do
   begin
      f_addindex (MidStr(s_text, i_pos1, i_pos - i_pos1));
      i_pos1 := i_pos + ls;
      i_pos := FirstPos (FSeparator, s_text, i_pos1);
   end;
   f_addindex (MidStr(s_text, i_pos1));
end;

function TxlListSuper.f_GetIndexList (): widestring;
var i: integer;
begin
	if count = 0 then
   	result := ''
   else
   begin
      result := IntToStr(FPList[0]^.index);
      for i := 1 to Count - 1 do
         result := result + Fseparator + IntToStr(FPList[i]^.index);
   end;
end;

//---------------------

procedure TxlListSuper.f_SetSortType (value: TListSortType);
begin
	FSortType := value;
   case value of
   	stByIndex: SortByIndex (false);
      stByKey: SortByKey (false);
      stByIndexNoDup: SortByINdex (true);
      stByKeyNoDup: SortByKey (true);
   end;
end;

function TxlListSuper.f_FindPosBySortedIndex (index: integer; i_start, i_end: integer; b_match: boolean): integer;
var i: integer;
begin
	i := 0;
   while i_start <= i_end do
   begin
      i := i_start + (i_end - i_start) div 2;
      if FPList[i]^.index < index then
         i_start := i + 1
      else if FPList[i]^.index > index then
      	i_end := i - 1
      else
      begin
         result := i;
         exit;
      end;
   end;
   if b_match or (not PosValid (i)) then
   	result := -1
   else
   	result := IfThen (FPList[i]^.index > index, i, i + 1);
end;

procedure TxlListSuper.SortByIndex (b_noduplicate: boolean = false);     // 选择法排序
var i, i_pos: integer;
	p: PListItem;
begin
// 插入法排序
	for i := Low + 1 to High do
   begin
      i_pos := f_FindPosBySortedIndex (FPList[i]^.index, Low, i - 1, false);
      if i_pos < i then
      begin
      	p := FPList[i];
      	f_Shift (i_pos, i - 1, 1);
         FPList[i_pos] := p;
      end;
   end;

   if b_noduplicate then     // Delete or Combine items with same key or index.
      for i := High - 1 downto Low do
      begin
         if FPList[i]^.index = FPList[i + 1]^.index then
            Delete (i + 1);
   	end;
end;

function TxlListSuper.f_FindPosBySortedKey (const s_key: widestring; i_start, i_end: integer; b_match: boolean): integer;
var i: integer;
begin
   repeat
      i := i_start + (i_end - i_start) div 2;
      case CompareStr (FPList[i]^.key, s_key) of
         crSmaller:
            i_start := i + 1;
         crLarger:
            i_end := i - 1;
         else
         begin
            result := i;
            exit;
         end;
      end;
   until (i_start > i_end);
   if b_match then
   	result := -1
   else
   	result := Ifthen ( CompareStr(FPList[i]^.key, s_key) = crLarger, i, i + 1);
end;

procedure TxlListSuper.SortByKey (b_noduplicate: boolean = false);
// 目前速度方面的主要瓶颈在于 CompareStringW 执行很慢
var i: integer;
	p: PListItem;
   i_pos: integer;
begin
// 插入法排序
	for i := Low + 1 to High do
   begin
      i_pos := f_FindPosBySortedKey (FPList[i]^.key, Low, i - 1, false);
      if i_pos < i then
      begin
      	p := FPList[i];
      	f_Shift (i_pos, i - 1, 1);
         FPList[i_pos] := p;
      end;
   end;

   if b_noduplicate then   // Delete or Combine items with same key or index.
      for i := High - 1 downto Low do
      begin
         if FPList[i]^.key = FPList[i + 1]^.key then
				Delete (i + 1);
      end;
end;

//---------------------------

procedure TxlStreamList.LoadFromFile (const s_file: widestring);
var s_text: widestring;
begin
   with TxlTextFile.create (s_file, fmRead, enUnknown) do
   begin
   	ReadText (s_text);
      Free;
   end;
	SetText (s_text);
end;

procedure TxlStreamList.SaveToFile (const s_file: widestring);
begin
	with TxlTextFile.create(s_file, fmWrite, enUTF16LE) do
   begin
   	WriteText (GetText);
      Free;
   end;
end;

end.

// 选择法排序
//   for i := Low to High - 1 do
//   begin
//      k := i;
//   	for j := i + 1 to High do
////      	if CompareStringW (LOCALE_USER_DEFAULT, 0, pwidechar(FPList[j]^.key), length(FPList[j]^.key), pwidechar(FPList[k]^.key), length(FPList[k]^.key)) = 1 then
////      	if FPList[j]^.key < FPList[k]^.key then
//         if CompareStr (FPList[j]^.key, FPList[k]^.key) = crSmaller then
//				k := j;
//      if k <> i then
//      begin
//      	p := FPList[i];
//      	FPList[i] := FPList[k];
//      	FPList[k] := p;
//      end;
//   end;

//      for i := Low to High - 1 do
//   begin
//      k := i;
//   	for j := i + 1 to High do
//         if FPList[j]^.index < FPList[k]^.index then
//				k := j;
//      if k <> i then
//      	SwapPos (i, k);
//   end;


