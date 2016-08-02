unit UxlDragControl;

interface

uses windows, messages, UxlWindow, UxlWinControl, UxlClasses, UxlStrUtils, UxlFunctions, UxlDialog, UxlList, UxlImageList;

const c_nilhidx = -1;

type
   IDragSource = interface
   	function Handle (): HWND;
      function HitTestItem (): integer;
   	procedure GetDragData (o_list: TxlIntList);
      procedure DeleteDragItems ();
   end;

   IDropTarget = interface
   	function Handle (): HWND;
      function HitTestItem (): integer;
		procedure SetDropHighlight (hidx: integer; b_highlight: boolean);
      procedure DropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
   end;

   TDropEvent = procedure (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean) of object;

   TxlDragControl = class (TxlControl, IDragSource, IDropTarget)
   private
   	FCanDrag: boolean;
      FCanDrop: boolean;
      FOnDropEvent: TDropEvent;
      procedure CheckDragWithin ();
      procedure CheckDragDrop ();
   protected
   	procedure SetDropHighlight (hidx: integer; b_highlight: boolean); virtual; abstract;
      function HitTestItem (): integer; virtual; abstract;
      procedure GetDragData (o_list: TxlIntList); virtual; abstract;
      procedure DeleteDragItems (); virtual; abstract;
      procedure DropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);

      procedure CheckBeginDrag ();
      function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;
   public
      property OnDropEvent: TDropEvent read FOnDropEvent write FOnDropEvent;
      property CanDrag: boolean write FCanDrag;
      property CanDrop: boolean write FCanDrop;
   end;

   TxlControlWithImages = class (TxlDragControl)
   private
      FImages: TxlImageList;
      FImagesCreated: boolean;
      FShowImages: boolean;

      procedure SetImages (value: TxlImageList);
      function GetImages (): TxlImageList;
   protected
   	procedure OnDestroyControl (); override;
		procedure SetShowImages (value: boolean); virtual;
   public
      property ShowImages: boolean read FShowImages write SetShowImages;
      property Images: TxlImageList read GetImages write SetImages;
   end;

implementation

uses commctrl, UxlConst;

type
   TDragHandler = class
   private
      FDragSource: IDragSource;
      FDropTarget: IDropTarget;
      FImageList: TxlImageList;
      FDragging: boolean;
      FCopyMode: boolean;
      FHidxSource, FHidxTarget: integer;
   public
      constructor Create ();
		destructor Destroy (); override;

      procedure BeginDrag (o_Source: IDragSource; CurPos: TPoint);
		procedure CheckDragMode (CurPos: TPoint);
      procedure DragWithin (o_Target: IDropTarget; CurPos: TPoint);
      procedure DragCancel ();
      procedure DragDrop (o_Target: IDropTarget);
      property Dragging: boolean read FDragging;
   end;

var DragHandler: TDragHandler;   // 为所有 DragControl 共享

procedure TxlDragControl.DropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
begin
	if assigned (FOnDropEvent) then
   	FOnDropEvent (o_dragsource, hidxTarget, b_copy);
end;

procedure TxlDragControl.CheckBeginDrag ();
begin
   if FCanDrag then
      DragHandler.BeginDrag (self, CursorPos);
end;

procedure TxlDragControl.CheckDragWithin ();
begin
   if DragHandler.Dragging and FCanDrop then
  		DragHandler.DragWithin (self, CursorPos);
end;

procedure TxlDragControl.CheckDragDrop ();
begin
   if DragHandler.Dragging then
   begin
      if FCanDrop then
         DragHandler.DragDrop (self)
      else
      	Draghandler.DragCancel;
   end;
end;

function TxlDragControl.ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD;
var b_processed: boolean;
begin
	result := 0;
   b_processed := false;
	case AMessage of
   	WM_KEYDOWN, WM_KEYUP:
      	if (wparam = VK_CONTROL) and DragHandler.Dragging then
         	DragHandler.CheckDragMode (CursorPos)
         else if (wparam = VK_ESCAPE) and DragHandler.Dragging then
         begin
         	Draghandler.DragCancel;
            b_processed := true;
         end;
      WM_MOUSEMOVE:
         CheckDragWithin;
      WM_LBUTTONUP:
         CheckDragDrop;
      WM_RBUTTONDOWN:
      	if DragHandler.Dragging then
      		Draghandler.DragCancel;
   end;
   if not b_processed then
      result := inherited ProcessMessage (AMessage, wParam, lParam);
end;

//---------------------------

constructor TDragHandler.Create ();
begin
   FDragging := false;
   FCopyMode := false;
	FImageList := TxlImageList.Create (32, 32);
   FImageList.AddIcon(ic_move);
   FImageList.AddIcon(ic_copy);
end;

destructor TDragHandler.Destroy ();
begin
	FImageList.Free;
	inherited;
end;

procedure TDragHandler.CheckDragMode (CurPos: TPoint);
var i_pic: integer;
   l_handle: HWND;
begin
	if FCopyMode and KeyPressed (VK_CONTROL) then exit;   // 避免频繁刷新
	FCopyMode := KeyPressed (VK_CONTROL);
   ImageList_EndDrag ();
   if FCopyMode then
   	i_pic := 1
   else
   	i_pic := 0;
   ImageList_BeginDrag (FImageList.handle, i_pic, 0, 0);
   if FDropTarget <> nil then
   	l_handle := FDropTarget.handle
   else
   	l_handle := FDragSource.handle;
   IMageList_DragEnter (l_handle, CurPos.x, CurPos.y);
end;

procedure TDragHandler.BeginDrag (o_Source: IDragSource; CurPos: TPoint);
var hidxSource: integer;
begin
	hidxSource := o_source.HitTestItem;
   if hidxSource = c_nilhidx then exit;

	FDragSource := o_source;
   FDropTarget := nil;
//   ShowCursor (FALSE);
   CheckDragMode (CurPos);

   FHidxSource := hidxSource;
   FHidxTarget := c_nilhidx;
   FDragging := TRUE;
end;

procedure f_SetHighlight (ctrl: IDropTarget; hidx: integer; b_highlight: boolean);
begin
   ctrl.SetDropHighLight (hidx, b_highlight);
   InvalidateRect (ctrl.Handle, nil, true);
   UpdateWindow (ctrl.handle);
end;

procedure TDragHandler.DragWithin (o_Target: IDropTarget; CurPos: TPoint);
var hidxTarget: integer;
begin
   if not FDragging then exit;

   hidxTarget := o_target.HitTestItem ();
   if (FDropTarget = nil) or (FDropTarget.handle <> o_target.handle) or (hidxTarGet <> FHidxTarget) then
   begin
   	if FDropTarget <> nil then
      begin
         ImageList_DragLeave (FDropTarget.handle);
         f_SetHighlight (FDropTarget, FHidxTarget, false);
		end;
      f_SetHighlight (o_target, hidxTarget, true);
      ImageList_DragEnter (o_target.handle, CurPos.x, CurPos.y);

      FDropTarget := o_Target;
      FHidxTarget := hidxTarget;
   end
   else
      ImageList_DragMove(CurPos.x, CurPos.y);
end;

procedure TDragHandler.DragCancel ();
begin
	if not FDragging then exit;
   if FDropTarget <> nil then
   begin
      ImageList_DragLeave (FDropTarget.handle);
      f_SetHighlight (FDropTarget, FHidxTarget, false);
   end;
   ImageList_EndDrag ();
//   ShowCursor (TRUE);
   FDragging := FALSE;
end;

procedure TDragHandler.DragDrop (o_Target: IDropTarget);
var hidxTarget: integer;
begin
	if not FDragging then exit;
	DragCancel;

   hidxTarget := o_Target.HitTestItem ();
   if (FDropTarget <> nil) and (hidxTarget <> c_nilhidx) then
      FDropTarget.SetDropHighLight (hidxTarget, false);
   if (o_target.handle = FDragSource.handle) and (hidxTarget = FHidxSource) then exit;

   o_target.DropEvent (FDragSource, hidxtarget, FCopyMode);
end;

//--------------------

procedure TxlControlWithImages.OnDestroyControl ();
begin
	Images := nil;
   inherited;
end;

procedure TxlControlWithImages.SetImages (value: TxlImageList);
begin
	if FImagesCreated then
   begin
   	FImages.free;
      FImagesCreated := false;
   end;
   FImages := value;
end;

function TxlControlWithImages.GetImages (): TxlImageList;
var i_width: integer;
begin
	if FImages = nil then
   begin
      i_width := GetSystemMetrics (SM_CXSMICON);
      FImages := TxlImageList.create (i_width, i_width);
      FImagesCreated := true;
   end;
   result := FImages;
end;

procedure TxlControlWithImages.SetShowImages (value: boolean);
begin
	FShowImages := value;
end;

//----------------

initialization
	DragHandler := TDragHandler.Create;

end.


