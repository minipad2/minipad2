unit UxlAppbar;

interface

uses Windows, UxlClasses, UxlWindow, UxlWinControl, ShellAPI;

type
   TAnchorEdge = (aeLeft, aeTop, aeRight, aeBottom);

   TxlAppbar = class (TxlInterfacedObject, IMessageObserver)
   private
      FWin: TxlWindow;
      FEnabled: boolean;
      FAutoHide: boolean;
      g_uSide: UInt;

      procedure SetEnable (value: boolean);
      procedure SetAutoHide (value: boolean);
      procedure f_AppBarCallback (hwndAccessBar: HWND; uNotifyMsg, LPARAM: UINT);
      procedure AppBarPosChanged (pabd: TAPPBARDATA);
      procedure AppBarQuerySetPos (uEdge: UINT; lprc: TRECT; pabd: TAPPBARDATA);
      function f_GetEdge (ae: TAnchorEdge): UInt;
   public
      constructor Create (AWindow: TxlWindow);
      destructor Destroy (); override;
      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;

      procedure Anchor (ae: TAnchorEdge; rc: TRect);
      property Enabled: boolean read FEnabled write SetEnable;
      property AutoHide: boolean read FAutoHide write SetAutoHide;
   end;

implementation

uses UxlWinDef, UxlFunctions;

constructor TxlAppbar.Create (AWindow: TxlWindow);
begin
   FWin := AWindow;
   FWin.AddMessageObserver (self);
end;

destructor TxlAppbar.Destroy ();
begin
   FWin.RemoveMessageObserver(self);
   inherited;
end;

procedure TxlAppbar.SetEnable (value: boolean);
var pabd: TAppBarData;
begin
   if value = FEnabled then exit;
   with pabd do
   begin
      cbsize := sizeof (pabd);
      hwnd := FWin.handle;
   end;
   if value then
   begin
      pabd.uCallBackMessage := APPBAR_CALLBACK;
      FEnabled := IntToBool (SHAppBarMessage (ABM_NEW, pabd));
      g_uSide := ABE_TOP;
   end
   else
   begin
      SHAppBarMessage(ABM_REMOVE, pabd);
      FEnabled := false;
   end;
end;

function TxlAppbar.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
begin
   case AMessage of
      APPBAR_CALLBACK:
         begin
            f_AppbarCallback (ASender.Handle, wparam, lparam);
            b_processed := true;
         end;
      else
         b_processed := false;
   end;
end;

procedure TxlAppbar.SetAutoHide (value: boolean);
begin
end;

//---------------------------

function TxlAppbar.f_GetEdge (ae: TAnchorEdge): UInt;
begin
   case ae of
      aeLeft: result := ABE_LEFT;
      aeTop: result := ABE_TOP;
      aeRight: result := ABE_RIGHT;
      else result := ABE_BOTTOM;
   end;
end;

procedure TxlAppbar.Anchor (ae: TAnchorEdge; rc: TRect);
var pabd: TAppBarData;
begin
   with pabd do
   begin
      cbsize := sizeof (pabd);
      hwnd := FWin.handle;
   end;
   AppBarQuerySetPos (f_GetEdge (ae), rc, pabd);
end;

procedure TxlAppbar.AppBarQuerySetPos (uEdge: UINT; lprc: TRECT; pabd: TAPPBARDATA);
var iHeight, iWidth: integer;
begin
   pabd.rc := lprc;
   pabd.uEdge := uEdge;

   // Copy the screen coordinates of the appbar's bounding rectangle into the APPBARDATA structure.
   if uEdge in [ABE_LEFT, ABE_RIGHT] then
   begin
      iWidth := pabd.rc.right - pabd.rc.left;
      pabd.rc.top := 0;
      pabd.rc.bottom := GetSystemMetrics(SM_CYSCREEN);
   end
   else
   begin
      iHeight := pabd.rc.bottom - pabd.rc.top;
      pabd.rc.left := 0;
      pabd.rc.right := GetSystemMetrics(SM_CXSCREEN);
   end;

   // Query the system for an approved size and position.
   SHAppBarMessage (ABM_QUERYPOS, pabd);

   // Adjust the rectangle, depending on the edge to which the appbar is anchored.
   case uEdge of
      ABE_LEFT:
         pabd.rc.right := pabd.rc.left + iWidth;
      ABE_RIGHT:
         pabd.rc.left := pabd.rc.right - iWidth;
      ABE_TOP:
         pabd.rc.bottom := pabd.rc.top + iHeight;
      ABE_BOTTOM:
         pabd.rc.top := pabd.rc.bottom - iHeight;
   end;

   // Pass the final bounding rectangle to the system. 
   SHAppBarMessage(ABM_SETPOS, pabd);

   // Move and size the appbar so that it conforms to the bounding rectangle passed to the system.
   MoveWindow (pabd.hWnd, pabd.rc.left, pabd.rc.top, pabd.rc.right - pabd.rc.left, pabd.rc.bottom - pabd.rc.top, TRUE);
end;

procedure TxlAppbar.f_AppBarCallback (hwndAccessBar: HWND; uNotifyMsg, LPARAM: UINT);
var abd: TAPPBARDATA;
    uState, us: UINT;
begin
   abd.cbSize := sizeof(abd);
   abd.hWnd := hwndAccessBar;

   case uNotifyMsg of
      ABN_STATECHANGE:
         begin
            // Check to see if the taskbar's always-on-top state has changed and, if it has, change the appbar's state accordingly.
            uState := SHAppBarMessage(ABM_GETSTATE, abd);
            If (ABS_ALWAYSONTOP and uState) > 0 then
               us := HWND_TOPMOST
            else
               us := HWND_BOTTOM;
            SetWindowPos (hwndAccessBar, us,
                0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
         end;
      ABN_FULLSCREENAPP:
         begin
            // A full-screen application has started, or the last full-screen application has closed. Set the appbar's z-order appropriately.
            if lParam > 0 then
            begin
               if (ABS_ALWAYSONTOP and uState) > 0 then
                  us := HWND_TOPMOST
               else
                  us := HWND_BOTTOM;
               SetWindowPos ( hwndAccessBar, us,
                    0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE)
            end
            else
            begin
                uState := SHAppBarMessage(ABM_GETSTATE, abd);
                if (uState and ABS_ALWAYSONTOP) > 0 then
                   SetWindowPos (hwndAccessBar, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
            end;
         end;
      ABN_POSCHANGED:
         // The taskbar or another appbar has changed its size or position.
         AppBarPosChanged (abd);
   end;
end;

// AppBarPosChanged - adjusts the appbar's size and position. 
// pabd - address of an APPBARDATA structure that contains information used to adjust the size and position
procedure TxlAppbar.AppBarPosChanged (pabd: TAPPBARDATA);
var rc, rcWindow: TRECT;
    iHeight, iWidth: integer;
begin
   with rc do
   begin
      top := 0;
      left := 0;
      right := GetSystemMetrics (SM_CXSCREEN);
      bottom := GetSystemMetrics (SM_CYSCREEN);
   end;

   GetWindowRect (pabd.hWnd, rcWindow);
   iHeight := rcWindow.bottom - rcWindow.top;
   iWidth := rcWindow.right - rcWindow.left;

   case g_uSide of
      ABE_TOP:
         rc.bottom := rc.top + iHeight;
      ABE_BOTTOM:
         rc.top := rc.bottom - iHeight;
      ABE_LEFT:
         rc.right := rc.left + iWidth;
      ABE_RIGHT:
         rc.left := rc.right - iWidth;
   end;
   AppBarQuerySetPos (g_uSide, rc, pabd);
end;

end.
