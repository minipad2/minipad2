unit UBrowseHistory;

interface

uses UxlList;

type
	THistory = class
   private
      FIDList: TxlIntList;
      FPos: integer;
   public
   	constructor Create ();
      destructor Destroy (); override;

   	function GetPrior (): integer;
      function GetNext (): integer;
      procedure Add (id: integer);
      procedure Delete (id: integer);
      function HasPrior (): boolean;
      function HasNext (): boolean;
   end;

implementation

uses UxlCommDlgs, UxlFunctions;

constructor THistory.Create ();
begin
	FIDList := TxlIntList.Create;
   FPOs := -1;
end;

destructor THistory.Destroy ();
begin
	FIDList.Free;
   inherited;
end;

function THistory.GetPrior (): integer;
begin
	if FPos > FIDList.Low then dec (FPos);
   result := FIDList[FPos];
end;

function THistory.GetNext (): integer;
begin
	if FPos < FIDList.High then inc (FPos);
   result := FIDList[FPos];
end;

procedure THistory.Add (id: integer);
var i, n: integer;
begin
	if (not FIDList.PosValid(FPos)) or (id <> FIDList[FPos]) then
   begin
      n := FIDList.High;
      for i := n downto FPOs + 1 do
         FIDList.Delete (i);
      FIDList.Add (id);
      FPos := FIDList.High;
   end;
end;

procedure THistory.Delete (id: integer);
var i: integer;
begin
	i := FIDList.Find (id);
   FIDList.Delete (i);
end;

function THistory.HasPrior (): boolean;
begin
	result := FPos > FIDList.Low;
end;

function THistory.HasNext (): boolean;
begin
	result := FPos < FIDList.High;
end;

end.
