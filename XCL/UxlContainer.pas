unit UxlContainer;

interface

type

   TxlContainter = class (TxlInterfacedObject)
   private
   protected
   	procedure SuperOnCreate (); virtual;
      procedure SuperOnDestroy (); virtual;
   public

   end;

implementation






function TxlContainter.GetVisible (): boolean;
begin
	result := FVisible;
end;

procedure TxlContainter.SetVisible (value: boolean);
begin
end;





end.
 