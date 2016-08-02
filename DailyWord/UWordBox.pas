unit UWordBox;

interface

uses
	UxlClasses, UxlExtClasses, UxlList;

type
	TWord = record
      word: widestring;
      exp: widestring;
      level: integer;
   end;

   TWordBox = class
   private
		FBoxSize: integer;
   	FNewDict: widestring;
      FAllWordCount: integer;
      FCurrentDict: widestring;

      FWordList: TxlStrList;
      FDeleteList: TxlStrList;
      FTempr: TxlStrList;
      
      procedure Save ();
		function GetWord (index: integer): TWord;
      procedure SetWord (index: integer; const value: TWord);
      procedure SetDictFile (const value: widestring);
	public
   	constructor Create ();
      destructor Destroy (); override;

      procedure NewBox (index: integer);
      function BoxCount (): integer;
      procedure DeleteWord (index: integer);
      procedure Export (o_list: TxlStrList);

      property BoxSize: integer read FBoxSize write FBoxSize;
      property DictFile: widestring read FNewDict write SetDictFile;
      property Word [index: integer]: TWord read GetWord write SetWord; default;
      function WordCount (): integer;
   end;

implementation

uses SysUtils, UxlMath, UGlobalObj, UxlFunctions, UxlFile, UxlStrUtils;

constructor TWordBox.Create ();
begin
	FWordList := TxlStrList.Create;
   FDeleteList := TxlStrList.Create;
   FTempr := TxlStrList.Create;
   FTempr.Separator := #9;
end;

destructor TWordBox.Destroy ();
begin
	Save;
	FWordList.Free;
   FDeleteList.Free;
   FTempr.Free;
   inherited;
end;

procedure TWordBox.SetDictFile (const value: widestring);
var s: widestring;
	n: integer;
begin
	FNewDict := value;
   if not PathFileExists (FNewDict) then exit;

   with TxlTextFile.Create (FNewDict, fmRead) do
   begin
      ReadLn (s);
      Free;
   end;
   n := firstpos ('//', s);
   if n > 0 then s := leftstr (s, n - 1);
   FAllWordCount := StrToIntDef (s);
end;

procedure TWordBox.NewBox (index: integer);
var i: integer;
   s: widestring;
   o_file: TxlTextFile;
begin
	Save;
   FWordList.Clear;
   if not PathFileExists (FNewDict) then exit;

   FWordList.MinSize := FBoxSize;
   o_file := TxlTextFile.Create (FNewDict, fmRead);
   try
      o_file.ReadLn (s);
      for i := 0 to BoxSize * index - 1 do
         o_file.ReadLn (s);
      while FWordList.Count < BoxSize do
      begin
         if o_file.EOF then    // 回到开头
         begin
            o_file.Locate (0);
            o_file.ReadLn (s);
         end;
         o_file.ReadLn (s);
         s := Trim(s);
         if (s = '') or (LeftStr(s, 2) = '//') then continue;
         FWordList.Add (s);
      end;
   finally
   	o_file.Free;
   end;
   FCurrentdict := FNewDict;
   ClearMemory;
end;

function TWordBox.BoxCount (): integer;
begin
	result := Ceil (FAllWordCount / BoxSize);
end;

procedure TWordBox.Save ();
begin
   if not PathFileExists (FCurrentDict) then exit;
end;

function TWordBox.GetWord (index: integer): TWord;
	function f_GetString (const s: widestring): widestring;
   begin
      result := ReplaceStr (s, '\n', #13#10);
      result := ReplaceStr (result, '\t', #9);
	end;
begin
   FTempr.Text := FWordList[index];
   with result do
   begin
      Word := f_GetString (FTempr[0]);
      Exp := f_GetString (FTempr[1]);
      Level := StrToIntDef (FTempr[2]);
   end;
end;

procedure TWordBox.SetWord (index: integer; const value: TWord);
var s_exp: widestring;
begin
	s_exp := ReplaceStr(value.exp, #13#10, '\n');
   s_exp := ReplaceStr (value.exp, #9, '\t');
	FWordList[index] := value.word + #9 + s_exp + #9 + IntToStr(value.Level);
end;

function TWordBox.WordCount (): integer;
begin
	result := FWordList.Count;
end;

procedure TWordBox.DeleteWord (index: integer);
begin
end;

procedure TWordBox.Export (o_list: TxlStrList);
var i: integer;
begin
	o_list.Clear;
   for i := FWordList.Low to FWordList.High do
   	o_list.Add (FWordList[i]);
end;

end.


