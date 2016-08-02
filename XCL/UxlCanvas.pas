unit UxlCanvas;

interface

uses Windows, UxlWinControl, UxlClasses;

type TxlCanvas = class
private
	FOwner: TxlWinControl;
   FForeColor: TColor;
   function OwnerHandle (): HWND;
   procedure SetBackColor (value: Tcolor);
   function GetBackColor (): TColor;
public
	constructor Create (AOwner: TxlWinControl);
   destructor Destroy (); override;

   function Width(): cardinal;
   function Height(): cardinal;
   function XLow (): cardinal;
   function XHigh (): cardinal;
   function YLow (): cardinal;
   function YHigh (): cardinal;

   procedure Circle (o_center: TPoint; i_radius: cardinal);
   procedure Line (o_start, o_end: TPoint);

   property ForeColor: TColor read FForeColor write FForeColor;
   property BackColor: TColor read GetBackColor write SetBackColor;
end;

implementation

uses UxlCommDlgs;

constructor TxlCanvas.Create (AOwner: TxlWinControl);
begin
	FOwner := AOwner;
end;

destructor TxlCanvas.Destroy ();
begin
	inherited;
end;

function TxlCanvas.Width(): cardinal;
var rc: TRect;
begin
	GetClientRect (FOwner.Handle, rc);
	result := RectToPos(rc).width;
end;

function TxlCanvas.Height(): cardinal;
var rc: TRect;
begin
	GetClientRect (FOwner.Handle, rc);
	result := RectToPos(rc).height;
end;

function TxlCanvas.XLow (): cardinal;
begin
	result := 0;
end;

function TxlCanvas.XHigh (): cardinal;
begin
	result := Width - 1;
end;

function TxlCanvas.YLow (): cardinal;
begin
	result := 0;
end;

function TxlCanvas.YHigh (): cardinal;
begin
	result := Height - 1;
end;

//----------------------

function TxlCanvas.OwnerHandle (): HWND;
begin
	if FOwner <> nil then
   	result := FOwner.handle
   else
   	result := 0;
end;

procedure TxlCanvas.SetBackColor (value: Tcolor);
begin
	FOwner.Color := value;
end;

function TxlCanvas.GetBackColor (): TColor;
begin
  	result := FOwner.Color
end;

//----------------

procedure TxlCanvas.Line (o_start, o_end: TPoint);
var h_dc: HDC;
begin
   h_dc := GetDC (Ownerhandle);
   MoveToEx (h_dc, o_start.x, o_start.y, nil);
   LineTo (h_dc, o_end.X, o_end.Y);
   ReleaseDC (Ownerhandle, h_dc);
end;

procedure TxlCanvas.Circle (o_center: TPoint; i_radius: cardinal);
var h_dc: HDC;
	h_pen: HBrush;
	o_rect: TRect;
   o_start: TPoint;
begin
	with o_rect do
   begin
   	Left := o_center.x - i_radius;
      Top := o_center.Y - i_radius;
      Right := o_center.x + i_radius;
      Bottom := o_center.Y + i_radius;
   end;
   o_start.x := o_center.x;
   o_start.y := o_rect.Top;

   h_dc := GetDC (Ownerhandle);
   h_pen := CreatePen (PS_Solid, 1, FForeColor);
   SelectObject (h_dc, h_pen);
   Arc (h_dc, o_rect.Left, o_rect.Top, o_rect.right, o_rect.bottom, o_start.x, o_start.Y, o_start.x, o_start.y);
   ReleaseDC (Ownerhandle, h_dc);
   DeleteObject (h_pen);
end;

end.

