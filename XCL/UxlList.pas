unit UxlList;

interface

uses UxlListSuper;

type TxlStrList = class (TxlStreamList)
private
   FKeyDeli: widestring;
   FIndexDeli: widestring;

   procedure f_SetValue (i_pos: integer; const value: widestring);
   procedure f_SetValueByKey (const s_key, value: WideString);
   procedure f_SetValueByIndex (i_index: integer; const value: widestring);

   function f_GetValue (i_pos: integer): widestring;
   function f_GetValueByKey (const s_key: widestring): widestring;
   function f_GetValueByIndex (i_index: integer): widestring;

   function f_GetValueByDir (dDir: TListDirection; var value: widestring): boolean;
protected
	procedure f_OnCreate (); override;
   function GetText (): widestring; override;
   procedure SetText (const s_text: widestring); override;
public
   function Add (const s_line: WideString): cardinal;
   procedure AddByKey (const s_key, s_value: WideString);
   procedure AddByIndex (i_index: integer; const s_value: widestring);
   function Insert (i_pos: integer; const s_line: Widestring): cardinal;
   function InsertFirst (const value: widestring): cardinal;
   function DeleteByValue (const s_value: widestring): boolean;
   function Find (const s_value: widestring): integer;
   function ItemExists (const s_value: widestring): boolean;

   function GetFirst (var s_value: widestring): boolean; 
   function GetPrior (var s_value: widestring): boolean;
   function GetCurrent (var s_value: widestring): boolean; 
   function GetNext (var s_value: widestring): boolean; 
   function GetLast (var s_value: widestring): boolean; 

   procedure Populate (const arr: array of widestring);
   function TextCount (): integer;

   property KeyDeli: widestring read FKeyDeli write FKeyDeli;
   property IndexDeli: widestring read FIndexDeli write FIndexDeli;
   property Items[i_pos: integer]: widestring read f_GetValue write f_SetValue; default;
   property ItemsByKey[const s_key: widestring]: widestring read f_GetValueByKey write f_SetValueByKey;
   property ItemsByIndex[i_index: integer]: widestring read f_GetValueByIndex write f_SetValueByIndex;
end;

type TxlIntList = class (TxlStreamList)
private
   procedure f_SetValue (i_pos: integer; value: integer);
   procedure f_SetValueByIndex (i_index: integer; value: integer);
   procedure f_SetValueByKey (const s_key: widestring; value: integer);

   function f_GetValue (i_pos: integer): integer;
   function f_GetValueByIndex (i_index: integer): integer;
   function f_GetValueByKey (const s_key: widestring): integer;

   function f_GetValueByDir (dDir: TListdirection; var i_number: integer): boolean;
protected
   function GetText (): widestring; override;
   procedure SetText (const s_text: widestring); override;
public
   function Add (value: integer): cardinal;
   procedure AddByIndex (i_index: integer; value: integer);
   procedure AddByKey (const s_key: widestring; value: integer);
   function Insert (i_pos: integer; value: integer): cardinal;
   function InsertFirst (value: integer): cardinal;
   function Find (value: integer): integer;
   function ItemExists (value: integer): boolean;
   function DeleteByValue (value: integer): boolean;

   function GetFirst (var value: integer): boolean;
   function GetPrior (var value: integer): boolean;
   function GetCurrent (var value: integer): boolean;
   function GetNext (var value: integer): boolean;
   function GetLast (var value: integer): boolean;

   procedure Populate (const arr: array of integer);
   
   property Items[i_pos: integer]: integer read f_GetValue write f_SetValue; default;
   property ItemsByKey[const s_key: widestring]: integer read f_GetValueByKey write f_SetValueByKey;
   property ItemsByIndex[i_index: integer]: integer read f_GetValueByIndex write f_SetValueByIndex;
end;

type TxlObjList = class (TxlListSuper)
private
   procedure f_SetValue (i_pos: integer; value: pointer);
   procedure f_SetValueByIndex (i_index: integer; value: pointer);
   procedure f_SetValueByKey (const s_key: widestring; value: pointer);

   function f_GetValue (i_pos: integer): pointer;
   function f_GetValueByIndex (i_index: integer): pointer;
   function f_GetValueByKey (const s_key: widestring): pointer;

   function f_GetValueByDir (dDir: TListdirection; var value: pointer): boolean;
public
   function Add (value: pointer): cardinal;
   procedure AddByIndex (i_index: integer; value: pointer);
   procedure AddByKey (const s_key: widestring; value: pointer);
   function Insert (i_pos: integer; value: pointer): cardinal;
   function InsertFirst (value: pointer): cardinal;
   function Find (value: pointer): integer;
   procedure Remove (value: pointer);
   function ItemExists (value: pointer): boolean;

   function GetFirst (var value: pointer): boolean;
   function GetPrior (var value: pointer): boolean;
   function GetCurrent (var value: pointer): boolean;
   function GetNext (var value: pointer): boolean;
   function GetLast (var value: pointer): boolean;
	function DeleteAndGetNext (var value: pointer): boolean;
   procedure ClearAndFreeAll ();

   property Items[i_pos: integer]: pointer read f_GetValue write f_SetValue; default;
   property ItemsByIndex[i_index: integer]: pointer read f_GetValueByIndex write f_SetValueByIndex;
   property ItemsByKey[const s_key: widestring]: pointer read f_GetValueByKey write f_SetValueByKey;
end;

type TxlInterfaceList = class (TxlListSuper)
private
   procedure f_SetValue (i_pos: integer; value: IInterface);
   procedure f_SetValueByIndex (i_index: integer; value: IInterface);

   function f_GetValue (i_pos: integer): IInterface;
   function f_GetValueByIndex (i_index: integer): IInterface;

   function f_GetValueByDir (dDir: TListdirection; var value: IInterface): boolean;
public
   function Add (value: IInterface): cardinal;
   procedure AddByIndex (i_index: integer; value: IInterface);
   function Insert (i_pos: integer; value: IInterface): cardinal;
   function Find (value: IInterface): integer;
   function ItemExists (value: IInterface): boolean;
   procedure Remove (value: IInterface);
   
   function GetFirst (var value: IInterface): boolean;
   function GetPrior (var value: IInterface): boolean;
   function GetCurrent (var value: IInterface): boolean;
   function GetNext (var value: IInterface): boolean;
   function GetLast (var value: IInterface): boolean;

   property Items[i_pos: integer]: IInterface read f_GetValue write f_SetValue; default;
   property ItemsByIndex[i_index: integer]: IInterface read f_GetValueByIndex write f_SetValueByIndex;
end;

implementation

uses UxlStrUtils, UxlFunctions, Windows;

procedure TxlStrList.f_OnCreate ();
begin
	KeyDeli := '';
   IndexDeli := '';
end;

function TxlStrList.Add (const s_line: WideString): cardinal;
begin
	result := Insert (Count, s_line);
end;

function TxlStrList.InsertFirst (const value: widestring): cardinal;
begin
	result := Insert (Low, value);
end;

function TxlStrList.Insert (i_pos: integer; const s_line: Widestring): cardinal;
var n: integer;
	p: PListItem;
   s: widestring;
begin
	p := f_newitem();
	if (KeyDeli <> '') or (IndexDeli <> '') then
   begin
   	s := trim(s_line);
   	if leftstr(s, 2) = '//' then exit;  // 对于带key的忽略注释行
      if KeyDeli <> '' then
      begin
         n := FirstPos (KeyDeli, s);
        	p^.key := IfThen (n > 0, LeftStr (s, n - 1), '');
      end
      else
      begin
         n := FirstPos (IndexDeli, s);
        	p^.index := IfThen (n > 0, StrToIntDef (LeftStr (s, n - 1)), 0);
      end;
      p^.strval := MidStr (s, n + 1);
   end
   else
   	p^.strval := s_line;

   result := f_insertitem (i_pos, p);
end;

function TxlStrList.Find (const s_value: widestring): integer;
var o_item: TListItem;
begin
	o_item.strval := s_value;
   result := f_find (o_item, fmByStrVal);
end;

function TxlStrList.ItemExists (const s_value: widestring): boolean;
begin
	result := PosValid (Find (s_value));
end;

procedure TxlStrList.AddByKey (const s_key, s_value: WideString);
var p: PListItem;
begin
   p := f_newitem();
   p^.key := s_key;
   p^.strval := s_value;
   f_insertitem (Count, p);
end;

procedure TxlStrList.AddByIndex (i_index: integer; const s_value: WideString);
var p: PListItem;
begin
   p := f_newitem();
   p^.index := i_index;
   p^.strval := s_value;
   f_insertitem (Count, p);
end;

function TxlStrList.DeleteByValue (const s_value: widestring): boolean;
var i: integer;
begin
	result := false;
	for i := High downto Low do
   	if IsSameStr(Items[i], s_value) then
			result := Delete (i);
end;

procedure TxlStrList.f_SetValue (i_pos: integer; const value: widestring);
begin
	if PosValid(i_pos) then
		FPList[i_pos]^.strval := value;
end;

procedure TxlStrList.f_SetValueByKey (const s_key, value: WideString); //if the item with the specific key does not exist, then create a new one with the key.
var i_pos: integer;
begin
   i_pos := FindByKey (s_key);
   if PosValid(i_pos) then
      FPList[i_pos]^.strval := value
   else
		AddByKey (s_key, value);
end;

procedure TxlStrList.f_SetValueByIndex (i_index: integer; const value: WideString); //if the item with the specific key does not exist, then create a new one with the key.
var i_pos: integer;
begin
   i_pos := FindByIndex (i_index);
   if PosValid(i_pos) then
      FPList[i_pos]^.strval := value
   else
		AddByIndex (i_index, value);
end;

function TxlStrList.f_GetValue (i_pos: integer): widestring;
begin
	if PosValid (i_pos) then
   	result := FPList[i_pos]^.strval
   else
   	result := '';
end;

function TxlStrList.f_GetValueByKey (const s_key: widestring): widestring;
begin
   result := f_GetValue (FindByKey (s_key));
end;

function TxlStrList.f_GetValueByIndex (i_index: integer): widestring;
begin
   result := f_GetValue (FindByIndex (i_index));
end;

function TxlStrList.GetFirst (var s_value: widestring): boolean;
begin
	result := f_GetValueByDir (dFirst, s_value);
end;

function TxlStrList.GetPrior (var s_value: widestring): boolean;
begin
	result := f_GetValueByDir (dPrior, s_value);
end;

function TxlStrList.GetCurrent (var s_value: widestring): boolean;
begin
	result := f_GetValueByDir (dCurrent, s_value);
end;

function TxlStrList.GetNext (var s_value: widestring): boolean;
begin
	result := f_GetValueByDir (dNext, s_value);
end;

function TxlStrList.GetLast (var s_value: widestring): boolean;
begin
	result := f_GetValueByDir (dLast, s_value);
end;

function TxlStrList.f_GetValueByDir (dDir: TListDirection; var value: widestring): boolean;
var p: PListItem;
begin
	result := f_GetPointerByDir (dDir, p);
   if result then value := p^.strval;
end;

procedure TxlStrList.SetText (const s_text: widestring);
var ls, i_pos, i_pos1: integer;
begin
	clear;
   if s_text = '' then exit;
	ls := length(FSeparator);
	i_pos1 := 1;
	i_pos := FirstPos (FSeparator, s_text, i_pos1);
   while i_pos > 0 do
   begin
		Add (copy(s_text, i_pos1, i_pos - i_pos1));
      i_pos1 := i_pos + ls;
      i_pos := FirstPos (FSeparator, s_text, i_pos1);
   end;
   Add (copy(s_text, i_pos1, length(s_text) - i_pos1 + 1));
end;

function TxlStrList.TextCount (): integer;
var i, n2, n3, n4: integer;
begin
   result := 0;
   n2 := length(Fseparator);
   n3 := length(KeyDeli);
   n4 := length(IndexDeli);
   for i := Low to High do
   begin
      if (n3 > 0) and (FPList[i]^.key <> '') then
         result := result + n3 + length(FPList[i]^.key);
      if n4 > 0 then
         result := result + n4 + length(IntToStr(FPList[i]^.index));
      inc (result, length(FPList[i]^.strval));
      if i > Low then
         inc (result, n2);
   end;
end;

function TxlStrList.GetText (): widestring;   // 优化算法，避免频繁的调整 result 的字符串空间
var p: PListItem;
   q: pword;
   s: widestring;
	i, n, n2, n3, n4: integer;
begin
	if Count = 0 then
   	result := ''
   else
   begin
      n2 := length(Fseparator);
      n3 := length(KeyDeli);
      n4 := length(IndexDeli);
		setlength (result, TextCount + 1);

      q := @result[1];
		for i := Low to High do
      begin
         if i > Low then
         begin
         	copymemory (q, pwidechar(Fseparator), n2 * 2);
            inc (q, n2);
         end;
         p := FPList[i];
         if (n3 > 0) and (p^.key <> '') then
         begin
         	n := length(p^.key);
         	copymemory (q, pwidechar(p^.key), n * 2);
            inc (q, n);
            copymemory (q, pwidechar(FKeyDeli), n3 * 2);
            inc (q, n3);
         end;
         if (n4 > 0) then
         begin
         	s := IntToStr(p^.index);
         	n := length(s);
         	copymemory (q, pwidechar(s), n * 2);
            inc (q, n);
            copymemory (q, pwidechar(FIndexDeli), n4 * 2);
            inc (q, n4);
         end;
         n := Length(p^.strval);
         copymemory (q, pwidechar(p^.strval), n * 2);
         inc (q, n);
      end;
      q^ := 0;
      result := pwidechar(result);
   end;
end;

procedure TxlStrList.Populate (const arr: array of widestring);
var i: integer;
begin
	Clear;
   for i := 0 to Length(arr) - 1 do
   	Add (arr[i]);
end;

//-----------------------

//class function TxlIntList.GetInstance (): TxlIntList;
//begin
//	if not assigned (IntList) then
//   	IntList := TxlIntList.Create();
//   IntList.Clear;
//   result := IntList;
//end;

function TxlIntList.Add (value: integer): cardinal;
begin
	result := Insert (Count, value);
end;

function TxlIntList.Insert (i_pos: integer; value: integer): cardinal;
var p: PListItem;
begin
   p := f_newitem();
   p^.intval := value;
   result := f_insertitem (i_pos, p);
end;

function TxlIntList.InsertFirst (value: integer): cardinal;
begin
	result := Insert (0, value);
end;

function TxlIntList.Find (value: integer): integer;
var o_item: TListitem;
begin
	o_item.intval := value;
   result := f_find (o_item, fmByIntVal);
end;

function TxlIntList.ItemExists (value: integer): boolean;
begin
	result := PosValid (Find (value));
end;

function TxlIntList.DeleteByValue (value: integer): boolean;
var i: integer;
begin
	result := false;
	for i := High downto Low do
   	if Items[i] = value then
			result := Delete (i);
end;

procedure TxlIntList.AddByIndex (i_index: integer; value: integer);
var p: PListItem;
begin
   p := f_newitem();
   p^.index := i_index;
   p^.intval := value;
   f_insertitem (Count, p);
end;

procedure TxlIntList.AddByKey (const s_key: WideString; value: integer);
var p: PListItem;
begin
   p := f_newitem();
   p^.key := s_key;
   p^.intval := value;
   f_insertitem (Count, p);
end;

procedure TxlIntList.f_SetValue (i_pos: integer; value: integer);
begin
   if PosValid (i_pos) then
      FPList[i_pos]^.intval := value;
end;

procedure TxlIntList.f_SetValueByIndex (i_index: integer; value: integer);
var i_pos: integer;
begin
   i_pos := FindByIndex (i_index);
   if PosValid(i_pos) then
      FPList[i_pos]^.intval := value
   else
		AddByIndex (i_index, value);
end;

procedure TxlIntList.f_SetValueByKey (const s_key: widestring; value: integer);
var i_pos: integer;
begin
   i_pos := FindByKey (s_key);
   if PosValid(i_pos) then
      FPList[i_pos]^.intval := value
   else
		AddByKey (s_key, value);
end;

function TxlIntList.f_GetValue (i_pos: integer): integer;
begin
	if PosValid (i_pos) then
   	result := FPList[i_pos]^.intval
   else
   	result := 0;
end;

function TxlIntList.f_GetValueByIndex (i_index: integer): integer;
begin
   result := f_GetValue (FindByIndex (i_index));
end;

function TxlIntList.f_GetValueByKey (const s_key: widestring): integer;
begin
   result := f_GetValue (FindByKey (s_key));
end;

function TxlIntList.GetFirst (var value: integer): boolean;
begin
	result := f_GetValueByDir (dFirst, value);
end;

function TxlIntList.GetPrior (var value: integer): boolean;
begin
   result := f_GetValueByDir (dPrior, value);
end;

function TxlIntList.GetCurrent (var value: integer): boolean;
begin
	result := f_GetValueByDir (dCurrent, value);
end;

function TxlIntList.GetNext (var value: integer): boolean;
begin
	result := f_GetValueByDir (dNext, value);
end;

function TxlIntList.GetLast (var value: integer): boolean;
begin
	result := f_GetValueByDir (dLast, value);
end;

function TxlIntList.f_GetValueByDir (dDir: TListdirection; var i_number: integer): boolean;
var p: PListItem;
begin
	result := f_GetPointerByDir (dDir, p);
   if result then i_number := p^.intval;
end;

procedure TxlIntList.SetText (const s_text: widestring);
var ls, i_pos, i_pos1: integer;
	s: widestring;
begin
	Clear;
   if s_text = '' then exit;
	ls := length(FSeparator);
	i_pos1 := 1;
	i_pos := FirstPos (FSeparator, s_text, i_pos1);
   while i_pos > 0 do
   begin
   	s := copy(s_text, i_pos1, i_pos - i_pos1);
   	Add (StrToIntDef(s));
      i_pos1 := i_pos + ls;
      i_pos := FirstPos (FSeparator, s_text, i_pos1);
   end;
   Add (StrToIntDef(MidStr(s_text, i_pos1)));
end;

function TxlIntList.GetText (): widestring;
var i: integer;
begin
   result := '';
   for i := Low to High do
   begin
      if i > Low then result := result + Fseparator;
      result := result + IntToStr(FPList[i]^.intval);
   end;
end;

procedure TxlIntList.Populate (const arr: array of integer);
var i: integer;
begin
	Clear;
   for i := 0 to Length(arr) - 1 do
   	Add (arr[i]);
end;

//-----------------------

procedure TxlObjList.ClearAndFreeAll ();
var i: integer;
begin
   for i := Low to High do
      FreeAndNil (FPList[i]^.objval);
   Clear ();
end;

function TxlObjList.Add (value: pointer): cardinal;
begin
	result := Find (value);
	if not PosValid(result) then
		result := Insert (Count, value);
end;

procedure TxlObjList.AddByKey (const s_key: WideString; value: pointer);
var p: PListItem;
begin
   p := f_newitem();
   p^.key := s_key;
   p^.objval := value;
   f_insertitem (Count, p);
end;

function TxlObjList.Insert (i_pos: integer; value: pointer): cardinal;
var p: PListitem;
begin
	p := f_newitem();
   p^.objval := value;
	result := f_insertitem (i_pos, p);
end;

function TxlObjList.InsertFirst (value: pointer): cardinal;
begin
	result := Insert (0, value);
end;

function TxlObjList.Find (value: pointer): integer;
var o_item: TListitem;
begin
	o_item.objval := value;
	result := f_find (o_item, fmByObjVal);
end;

procedure TxlObjList.Remove (value: pointer);
var i: integer;
begin
	for i := High downto Low do
   	if Items[i] = value then
      	Delete (i);
end;

function TxlObjList.ItemExists (value: pointer): boolean;
begin
	result := PosValid(Find (value))
end;

procedure TxlObjList.AddByIndex (i_index: integer; value: pointer);
var p: PListItem;
begin
   p := f_newitem();
   p^.index := i_index;
   p^.objval := value;
   f_insertitem (Count, p);
end;

procedure TxlObjList.f_SetValue (i_pos: integer; value: pointer);
begin
   if PosValid (i_pos) then
   	FPList[i_pos]^.objval := value;
end;

procedure TxlObjList.f_SetValueByIndex (i_index: integer; value: pointer);  //if the item with the specific key does not exist, then create a new one with the key.
var i_pos: integer;
begin
   i_pos := FindByIndex (i_index);
   if PosValid(i_pos) then
      FPList[i_pos]^.objval := value
   else
		AddByIndex (i_index, value);
end;

procedure TxlObjList.f_SetValueByKey (const s_key: widestring; value: pointer);
var i_pos: integer;
begin
   i_pos := FindByKey (s_key);
   if PosValid(i_pos) then
      FPList[i_pos]^.objval := value
   else
		AddByKey (s_key, value);
end;

function TxlObjList.f_GetValue (i_pos: integer): pointer;
begin
	if PosValid (i_pos) then
   	result := FPList[i_pos]^.objval
   else
   	result := nil;
end;

function TxlObjList.f_GetValueByIndex (i_index: integer): pointer;
begin
	result := f_GetValue (FindByIndex(i_index));
end;

function TxlObjList.f_GetValueByKey (const s_key: widestring): pointer;
begin
   result := f_GetValue (FindByKey (s_key));
end;

function TxlObjList.GetFirst (var value: pointer): boolean;
begin
	result := f_GetValueByDir (dFirst, value);
end;

function TxlObjList.GetPrior (var value: pointer): boolean;
begin
   result := f_GetValueByDir (dPrior, value);
end;

function TxlObjList.GetCurrent (var value: pointer): boolean;
begin
	result := f_GetValueByDir (dCurrent, value);
end;

function TxlObjList.GetNext (var value: pointer): boolean;
begin
	result := f_GetValueByDir (dNext, value);
end;

function TxlObjList.GetLast (var value: pointer): boolean;
begin
	result := f_GetValueByDir (dLast, value);
end;

function TxlObjList.DeleteAndGetNext(var value: pointer): boolean;
begin
	result := false;
	if PosValid(FPos) then
   begin
      result := GetNext (value);
      Delete (FPos);
   end;
end;

function TxlObjList.f_GetValueByDir (dDir: TListdirection; var value: pointer): boolean;
var p: PListItem;
begin
	result := f_GetPointerByDir (dDir, p);
   if result then value := p^.objval;
end;

//-------------------------

function TxlInterfaceList.Add (value: IInterface): cardinal;
begin
	result := Find (value);
   if not PosValid (result) then
	   result := Insert (Count, value);
end;

procedure TxlInterfaceList.AddByIndex (i_index: integer; value: IInterface);
var p: PListItem;
begin
   p := f_newitem();
   p^.index := i_index;
   p^.interfval := value;
   f_insertitem (Count, p);
end;

function TxlInterfaceList.Insert (i_pos: integer; value: IInterface): cardinal;
var p: PListitem;
begin
	p := f_newitem();
   p^.interfval := value;
	result := f_insertitem (i_pos, p);
end;

function TxlInterfaceList.Find (value: IInterface): integer;
var o_item: TListitem;
begin
	o_item.interfval := value;
	result := f_find (o_item, fmByInterfVal);
end;

function TxlInterfaceList.ItemExists (value: IInterface): boolean;
begin
	result := PosValid (Find(value));
end;

procedure TxlInterfaceList.Remove (value: IInterface);
var i: integer;
begin
	for i := High downto Low do
   	if Items[i] = value then
      	Delete (i);
end;

procedure TxlInterfaceList.f_SetValue (i_pos: integer; value: IInterface);
begin
   if PosValid (i_pos) then
   	FPList[i_pos]^.interfval := value;
end;

procedure TxlInterfaceList.f_SetValueByIndex (i_index: integer; value: IInterface);
var i_pos: integer;
begin
   i_pos := FindByIndex (i_index);
   if PosValid(i_pos) then
      FPList[i_pos]^.interfval := value
   else
		AddByIndex (i_index, value);
end;

function TxlInterfaceList.f_GetValue (i_pos: integer): IInterface;
begin
	if PosValid (i_pos) then
   	result := FPList[i_pos]^.interfval
   else
   	result := nil;
end;

function TxlInterfaceList.f_GetValueByIndex (i_index: integer): IInterface;
begin
	result := f_GetValue (FindByIndex(i_index));
end;

function TxlInterfaceList.f_GetValueByDir (dDir: TListdirection; var value: IInterface): boolean;
var p: PListItem;
begin
	result := f_GetPointerByDir (dDir, p);
   if result then value := p^.interfval;
end;

function TxlInterfaceList.GetFirst (var value: IInterface): boolean;
begin
	result := f_GetValueByDir (dFirst, value);
end;

function TxlInterfaceList.GetPrior (var value: IInterface): boolean;
begin
   result := f_GetValueByDir (dPrior, value);
end;

function TxlInterfaceList.GetCurrent (var value: IInterface): boolean;
begin
   result := f_GetValueByDir (dCurrent, value);
end;

function TxlInterfaceList.GetNext (var value: IInterface): boolean;
begin
   result := f_GetValueByDir (dNext, value);
end;

function TxlInterfaceList.GetLast (var value: IInterface): boolean;
begin
   result := f_GetValueByDir (dLast, value);
end;

//initialization
//
//finalization
//	StrList.Free;
//   IntList.Free;

end.






