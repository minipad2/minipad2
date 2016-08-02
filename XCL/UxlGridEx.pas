unit UxlGridEx;

interface

uses UxlGrid;

type
	TxlGridEx = class (TxlGrid)
   private
      function GetSettings (): widestring;
      procedure SetSettings (const value: widestring);
   protected
   	function GetText (): widestring; override;
      procedure SetText (const s_text: widestring); override;
   public
      property Settings: widestring read GetSettings write SetSettings;
   end;

implementation

uses UxlList, UxlFunctions;

function TxlGridEx.GetText (): widestring;
var o_rows, o_cols: TxlStrList;
	i, j, m, n: integer;
begin
	SaveGrid;
   o_rows := TxlStrList.Create();
   o_cols := TxlStrList.Create();
   o_cols.Separator := #9;
   m := IfThen (FixedRow = frcAuto, 1, 0);
   n := IfThen (FixedCol = frcAuto, 1, 0);
   for i := m to RowCount - 1 do
   begin
   	o_cols.Clear;
   	for j := n to ColCount - 1 do
         o_cols.Add(Cells[i, j]);
      o_rows.Add(o_cols.Text);
   end;
   result := o_rows.Text;
   o_rows.Free;
   o_cols.Free;
end;

procedure TxlGridEx.SetText (const s_text: widestring);
var o_rows, o_cols: TxlStrList;
	i, j, m, n: integer;
begin
	o_rows := TxlStrList.Create();
   o_cols := TxlStrList.Create();
   o_cols.Separator := #9;
   o_rows.Text := s_text;
   for i := o_rows.Low to o_rows.High do
   begin
   	o_cols.Text := o_rows[i];
      for j := o_cols.Low to o_cols.High do
      begin
      	m := IfThen (FixedRow = frcAuto, i + 1, i);
         n := IfThen (FixedCol = frcAuto, j + 1, j);
      	Cells[m, n] := o_cols[j];
      end;
   end;
   o_rows.Free;
   o_cols.free;
   Redraw;
end;

function TxlGridEx.GetSettings (): widestring;
var o_list1: TxlStrList;
	o_list2: TxlIntList;
	i: integer;
begin
	o_list1 := TxlStrList.Create();
   o_list1.Separator := ';';
   o_list2 := TxlIntList.Create();
   o_list2.Separator := ',';

   for i := 0 to RowCount - 1 do
   	o_list2.Add(RowHeight[i]);
   o_list1.Add(o_list2.Text);

   o_list2.Clear;
   for i := 0 to ColCount - 1 do
   	o_list2.Add(ColWidth[i]);
   o_list1.Add(o_list2.Text);

   o_list1.Add(IntToStr(Ord(FixedRow)));
   o_list1.Add(IntToStr(Ord(FixedCol)));

   result := o_list1.Text;
   o_list1.free;
   o_list2.free;
end;

procedure TxlGridEx.SetSettings (const value: widestring);
var o_list1: TxlStrList;
	o_list2: TxlIntList;
	i: integer;
begin
	o_list1 := TxlStrList.Create();
   o_list1.Separator := ';';
   o_list2 := TxlIntList.Create();
   o_list2.Separator := ',';
   o_list1.Text := value;

   o_list2.Text := o_list1[0];
   RowCount := o_list2.Count;
   for i := o_list2.Low to o_list2.High do
   	RowHeight[i] := o_list2[i];

   o_list2.Text := o_list1[1];
   ColCount := o_list2.Count;
   for i := o_list2.Low to o_list2.High do
   	ColWidth[i] := o_list2[i];

   FixedRow := TFixedRowCol(StrToIntDef (o_list1[2]));
   FixedCol := TFixedRowCol(StrToIntDef (o_list1[3]));

   o_list1.free;
   o_list2.free;
end;

end.
