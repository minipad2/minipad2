unit UBoard;

interface

uses Windows, UxlWinControl;

type
	TShape = (esSquare, esRound, esOval);

	TElement = record
   	Text: widestring;
      Shape: TShape;
      Position: TPoint;
   end;

	TBoard = class (TxlControl)
   private
   	FElements: array of TElement;
      procedure f_Draw ();
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
   	procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
      function IsUserClass (): boolean; override;
   public
   	procedure AddElement (const value: TElement);
   end;

implementation

uses Messages;

function TBoard.DoCreateControl (HParent: HWND): HWND;
begin
	RegisterControlClass ('TBoard', GetStockObject(WHITE_BRUSH));
  	result := CreateWin32Control (HParent, 'TBoard');
end;

procedure TBoard.OnCreateControl ();
begin
	inherited;
end;

procedure TBoard.OnDestroyControl ();
begin
   inherited;
end;

function TBoard.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
begin
	result := 0;
	case AMessage of
   	WM_PAINT:
        	f_Draw;
      else
         result := inherited ProcessMessage (AMessage, wParam, lParam);
   end;
end;

function TBoard.IsUserClass (): boolean;
begin
	result := true;
end;

procedure TBoard.AddElement (const value: TElement);
var n: integer;
begin
	n := Length(FElements);
	SetLength (FElements, n + 1);
   FElements[n] := value;
end;

procedure TBoard.f_Draw ();
	procedure f_DrawElement (h_dc: HDC; const e: TElement);
   begin
   	TextOutW (h_dc, e.Position.x, e.Position.y, pwidechar(e.Text), Length(e.Text));
   end;
var h_dc: HDC;
   ps: PaintStruct;
   i: integer;
begin
   h_dc := Beginpaint ( Fhandle, ps );
   for i := Low (FElements) to High (FElements) do
   	f_DrawElement (h_dc, FElements[i]);
   EndPaint ( Fhandle, ps );
end;

end.
