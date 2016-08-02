unit UxlPanel;

interface

uses Windows, UxlWinControl, UxlTabControl, UxlList, UxlClasses, UxlDialog;

type
	TxlCustomControl = class (TxlControl)
   private
	public
   	function IsUserClass (): boolean; override;
	end;

   TxlPanel = class (TxlCustomControl)
   protected
   	function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;

      procedure OnCreate (); virtual;
      procedure OnDestroy (); virtual;
   public
   end;

   TxlFrame = class (TxlPanel)     // adapter for TxlDialogML and TxlControl
   private
   protected
   	FFrame: TxlDialogML;
      procedure OnCreate (); override;
      procedure OnDestroy (); override;
   	procedure OnMove (pt: TPoint); override;
      procedure OnSize (clpos: TPos); override;
      procedure SetVisible (value: boolean); override;
      procedure SetEnabled (value: boolean); override;

      procedure BeforeOpenTemplate (); virtual;
      procedure AfterOpenTemplate (); virtual;
      procedure BeforeCloseTemplate (); virtual;
      procedure AfterCloseTemplate (); virtual;
   public
   	procedure SetTemplate (ATemplate: TDialogMLClass);
		procedure CloseTemplate ();
      procedure AdjustFramePos ();
      property Frame: TxlDialogML read FFrame;
   end;

implementation

uses Messages, SysUtils, UxlFunctions;

function TxlCustomControl.IsUserClass (): boolean;
begin
	result := true;
end;

//--------------

function TxlPanel.DoCreateControl (HParent: HWND): HWND;
begin
	RegisterControlClass ('TxlPanel', COLOR_BACKGROUND);
  	result := CreateWin32Control (HParent, 'TxlPanel');         // , WS_BORDER
end;

procedure TxlPanel.OnCreateControl ();
begin
	inherited;
   OnCreate;
end;

procedure TxlPanel.OnDestroyControl ();
begin
   OnDestroy;
   inherited;
end;

procedure TxlPanel.OnCreate ();
begin
end;

procedure TxlPanel.OnDestroy ();
begin
end;

//---------------------------

procedure TxlFrame.OnCreate ();
begin
	inherited;
	SetWndStyle (WS_DLGFRAME, true);
	FFrame := nil;
end;

procedure TxlFrame.OnDestroy ();
begin
	CloseTemplate;
   inherited;
end;

procedure TxlFrame.CloseTemplate ();
begin
	if FFrame <> nil then
   begin
      BeforeCloseTemplate;
   	FFrame.Close();
   	AfterCloseTemplate;
      FreeAndNil (FFrame);
   end;
end;

procedure TxlFrame.AdjustFramePos ();
var o_rect: TRect;
begin
	GetWindowRect (self.handle, o_rect);
   zoomrect (o_rect, -2);
   dec (o_rect.Top, 1);
   FFrame.Pos := RectToPos (o_rect);
end;

procedure TxlFrame.SetTemplate (ATemplate: TDialogMLClass);
begin
   CloseTemplate;
   FFrame := ATemplate.Create(self);
   BeforeOpenTemplate ();
   FFrame.Open(false);
   AdjustFramePos;
   AfterOpenTemplate ();
   FFrame.Show;
end;

procedure TxlFrame.BeforeCloseTemplate ();
begin
end;

procedure TxlFrame.AfterCloseTemplate ();
begin
end;

procedure TxlFrame.BeforeOpenTemplate ();
begin
end;

procedure TxlFrame.AfterOpenTemplate ();
begin
end;

procedure TxlFrame.OnSize (clpos: TPos);
begin
	inherited;
   AdjustFramePos;
end;

procedure TxlFrame.OnMove (pt: TPoint);
begin
	inherited;
	AdjustFramePos;
end;

procedure TxlFrame.SetVisible (value: boolean);
begin
	inherited;
   FFrame.Visible := value;
end;

procedure TxlFrame.SetEnabled (value: boolean);
begin
	inherited;
   FFrame.Enabled := value;
end;

end.
