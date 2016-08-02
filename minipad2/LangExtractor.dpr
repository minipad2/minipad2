program LangExtractor;

uses UxlList, UxlStrUtils, UxlFunctions;

var ConstList: TxlStrList;
	ResList: TxlStrList;
   ResultList: TxlStrList;
   i: integer;
   s_key: widestring;

begin
	ConstList := TxlStrList.Create;
   ConstList.KeyDeli := '=';
   ConstList.LoadFromFile (ProgDir + 'Resource.pas');
   for i := ConstList.Low to ConstList.High do
   begin
   	ConstList.Keys[i] := Trim (ConstList.Keys[i]);
      ConstList.Items[i] := Trim (ReplaceStr(ConstList.Items[i], ';', ''));
   end;

   ResList := TxlStrList.Create;
   ResList.KeyDeli := ',';
   ResList.LoadFromFile (ProgDir + 'Strings.rc');
   for i := ResList.Low to ResList.High do
   begin
   	ResList.Keys[i] := Trim(ResList.Keys[i]);
      ResList.Items[i] := Trim(ReplaceStr(ResList.Items[i], '"', ''));
   end;

   ResultList := TxlStrList.Create;
   ResultList.KeyDeli := '=';
   for i := ResList.Low to ResList.High do
   	if ResList.Keys[i] <> '' then
      begin
      	s_key := ConstList.ItemsByKey[ResList.Keys[i]];
         ResultList.AddByKey (Trim(s_key), ResList[i]);
      end;
   ResultList.SaveToFile (ProgDir + 'lang\English.lng');

   ConstList.Free;
   ResList.Free;
   ResultList.Free;
end.
