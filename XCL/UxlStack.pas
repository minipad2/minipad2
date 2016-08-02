unit UxlStack;

interface

uses UxlClasses;

type
   TxlIntStackQueueSuper = class
   private
      FStack: array of integer;
      FSeparator: widestring;
      procedure SetText (const value: widestring);
      function GetText (): widestring;
   protected
      function IsStack (): boolean; virtual; abstract;  // first in last out
	public
      procedure Push (i: integer);
      function Peek (var i: integer): boolean;
		function Pop (var i: integer): boolean;

      procedure Clear ();
      function Count(): integer;
      function IsEmpty (): boolean;

      property Separator: widestring read FSeparator write FSeparator;
      property Text: widestring read GetTExt write SetText;
   end;

   TxlIntStack = class (TxlIntStackQueueSuper)
   protected
      function IsStack (): boolean; override;
   end;

   TxlIntQueue = class (TxlIntStackQueueSuper)
   protected
      function IsStack (): boolean; override;
   end;

type
   TxlStrStackQueueSuper = class
   private
      FStack: array of widestring;
   protected
      function IsStack (): boolean; virtual; abstract;  // first in last out
	public
      procedure Push (const s: widestring);
      function Peek (var s: widestring): boolean;
		function Pop (var s: widestring): boolean;

      procedure Clear ();
      function Count(): integer;
      function IsEmpty (): boolean;
   end;

   TxlStrStack = class (TxlStrStackQueueSuper)
   protected
      function IsStack (): boolean; override;
   end;

   TxlStrQueue = class (TxlStrStackQueueSuper)
   protected
      function IsStack (): boolean; override;
   end;

type
   TxlObjListLite = class (TxlCollection)
   private
      FItems: array of pointer;
      function f_GetItem (index: integer): pointer;
      procedure f_SetItem (index: integer; value: pointer);
   public
      function Add (value: pointer): integer;
      function Find (value: pointer): integer;
      procedure Delete (index: integer);
      function Assigned (index: integer): boolean;
      procedure Clear ();
      function Count (): integer; override;

      property Items[index: integer]: pointer read f_getitem write f_setitem; default;
   end;

   TxlIntListLite = class (TxlCollection)
   private
      FItems: array of integer;
      function f_GetItem (index: integer): integer;
      procedure f_SetItem (index: integer; value: integer);
   public
      function Add (value: integer): integer;
      function Find (value: integer): integer;
      procedure Delete (index: integer);
      procedure Clear ();
      function Count (): integer; override;

      property Items[index: integer]: integer read f_getitem write f_setitem; default;
   end;

   TxlBoolList = class (TxlCollection)
   private
   	FValue: integer;
      FCount: integer;
      function f_GetItem (index: integer): boolean;
      procedure f_SetItem (index: integer; value: boolean);
   public
   	function Add (value: boolean): integer;
      procedure Clear ();
      function Count (): integer; override;
      property Items[index: integer]: boolean read f_GetItem write f_SetItem; default;
      property BitVal: integer read FValue write FValue;
   end;

implementation

uses UxlMath, UxlStrUtils, UxlFunctions;

procedure TxlIntStackQueueSuper.Push (i: integer);
var n: integer;
begin
	n := Count;
   SetLength (FStack, n + 1);
   FStack[n] := i;
end;

function TxlIntStackQueueSuper.Pop (var i: integer): boolean;
var j: integer;
begin
	result := Peek (i);
   if result then
   begin
   	if not IsStack then
      	for j := 0 to Count - 2 do
            FStack[j] := FStack[j+1];
   	SetLength (FStack, Count - 1);
   end;
end;

function TxlIntStackQueueSuper.Peek (var i: integer): boolean;
begin
	result := Count > 0;
   if result then
   begin
   	if IsStack then
   		i := FStack[Count - 1]
      else
      	i := FStack[0];
	end;
end;

procedure TxlIntStackQueueSuper.Clear ();
begin
	SetLength (FStack, 0);
end;

function TxlIntStackQueueSuper.Count (): integer;
begin
   result := length(FStack);
end;

function TxlIntStackQueueSuper.IsEmpty (): boolean;
begin
	result := Count = 0;
end;

procedure TxlIntStackQueueSuper.SetText (const value: widestring);
var ls, i_pos, i_pos1: integer;
	s: widestring;
begin
	Clear;
   if value = '' then exit;
	ls := length(FSeparator);
	i_pos1 := 1;
	i_pos := FirstPos (FSeparator, value, i_pos1);
   while i_pos > 0 do
   begin
   	s := copy(value, i_pos1, i_pos - i_pos1);
   	Push (StrToIntDef(s));
      i_pos1 := i_pos + ls;
      i_pos := FirstPos (FSeparator, value, i_pos1);
   end;
   Push (StrToIntDef(MidStr(value, i_pos1)));
end;

function TxlIntStackQueueSuper.GetText (): widestring;
var i: integer;
begin
  	result := '';
   for i := 0 to Count - 1 do
   begin
      if i > 0 then result := result + Fseparator;
      result := result + IntToStr(FStack[i]);
   end;
end;

//---------------------------

function TxlIntStack.IsStack (): boolean;
begin
   result := true;
end;

function TxlIntQueue.IsStack (): boolean;
begin
   result := false;
end;

//---------------------------

procedure TxlStrStackQueueSuper.Push (const s: widestring);
var n: integer;
begin
	n := Count;
   SetLength (FStack, n + 1);
   FStack[n] := s;
end;

function TxlStrStackQueueSuper.Pop (var s: widestring): boolean;
var j: integer;
begin
	result := Peek (s);
   if result then
   begin
   	if not IsStack then
      	for j := 0 to Count - 2 do
            FStack[j] := FStack[j+1];
   	SetLength (FStack, Count - 1);
   end;
end;

function TxlStrStackQueueSuper.Peek (var s: widestring): boolean;
begin
	result := Count > 0;
   if result then
   begin
   	if IsStack then
   		s := FStack[Count - 1]
      else
      	s := FStack[0];
	end;
end;

procedure TxlStrStackQueueSuper.Clear ();
begin
	SetLength (FStack, 0);
end;

function TxlStrStackQueueSuper.Count (): integer;
begin
   result := length(FStack);
end;

function TxlStrStackQueueSuper.IsEmpty (): boolean;
begin
	result := Count = 0;
end;

//------------------------

function TxlStrStack.IsStack (): boolean;
begin
   result := true;
end;

function TxlStrQueue.IsStack (): boolean;
begin
   result := false;
end;

//----------------------------

function TxlObjListLite.Count (): integer;
begin
   result := length(FItems);
end;

function TxlObjListLite.f_GetItem (index: integer): pointer;
begin
	result := nil;
	if InRange (index, Low, High) then
		result := FItems[index];
end;

procedure TxlObjListLite.f_SetItem (index: integer; value: pointer);
begin
	if InRange (index, Low, High) then
		FItems[index] := value;
end;

function TxlObjListLite.Add (value: pointer): integer;
begin
	result := Count;
   setlength (FItems, result + 1);
   FItems[result] := value;
end;

procedure TxlObjListLite.Delete (index: integer);
var i: integer;
begin
	if not InRange (index, Low, High) then exit;
	for i := index to High - 1 do
   	FItems[i] := FItems[i+1];
   SetLength (FItems, Count - 1);
end;

function TxlObjListLite.Find(value: pointer): integer;
var i: integer;
begin
	result := -1;
	for i := Low to High do
   	if FItems[i] = value then
      begin
      	result := i;
         break;
      end;
end;

function TxlObjListLite.Assigned (index: integer): boolean;
begin
	if InRange (index, Low, High) then
		result := (FItems[index] <> nil)
   else
   	result := false;
end;

procedure TxlObjListLite.Clear ();
begin
	setlength (FItems, 0);
end;

//----------------------

function TxlIntListLite.Count (): integer;
begin
   result := length(FItems);
end;

function TxlIntListLite.f_GetItem (index: integer): integer;
begin
	result := 0;
   if InRange (index, Low, High) then
   	result := FItems[index];
end;

procedure TxlIntListLite.f_SetItem (index: integer; value: integer);
begin
	if InRange (index, Low, High) then
		FItems[index] := value;
end;

function TxlIntListLite.Add (value: integer): integer;
begin
	result := Count;
   setlength (FItems, result + 1);
   FItems[result] := value;
end;

procedure TxlIntListLite.Delete (index: integer);
var i: integer;
begin
	if not InRange (index, Low, High) then exit;
	for i := index to High - 1 do
   	FItems[i] := FItems[i+1];
   SetLength (FItems, Count - 1);
end;

function TxlIntListLite.Find(value: integer): integer;
var i: integer;
begin
	result := -1;
	for i := Low to High do
   	if FItems[i] = value then
      begin
      	result := i;
         break;
      end;
end;

procedure TxlIntListLite.Clear ();
begin
	setlength (FItems, 0);
end;

//----------------------------

function TxlBoolList.f_GetItem (index: integer): boolean;
begin
	result := (FValue and (1 shl index)) <> 0;
end;

procedure TxlBoolList.f_SetItem (index: integer; value: boolean);
begin
	if value then
   	FValue := FValue or (1 shl index)
   else
   	FValue := FValue and (not (1 shl index));
end;

function TxlBoolList.Add (value: boolean): integer;
begin
	if value then
   	FValue := FValue + (1 shl FCount);
	inc (FCount);
end;

procedure TxlBoolList.Clear ();
begin
	FValue := 0;
   FCount := 0;
end;

function TxlBoolList.Count (): integer;
begin
	result := FCount;
end;

end.

