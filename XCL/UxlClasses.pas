unit UxlClasses;

interface

uses Windows;

type
   TCollectionChangeType = (cctNew, cctDelete, cctModify, cctClear);
   TCollectionChangeEvent = procedure (cct: TCollectionChangeType; index: integer) of object;
   TxlCollection = class
   private
//   	FOnChange: TCollectionChangeEvent;
//      procedure f_OnChange (cct: TCollectionChangeType; index: integer);
   public
   	function Count (): integer; virtual; abstract;
      function Low (): integer;
      function High (): integer;
		function PosValid (index: integer): boolean;
      function IsEmpty (): boolean;
//      property OnChange: TCollectionChangeEvent read FOnChange write FOnchange;
   end;

	TNotifyEvent = procedure (Sender: Tobject) of object;
   TCommandEvent = procedure (ID: WORD) of object;
   TSelChangeEvent = function (oldindex, newindex: integer): integer of object;  // return 1 to prevent from change
	TSelItemEvent = procedure (index: integer; sel: boolean) of object;
   TLabelEditEvent = function (index: integer; const newtext: widestring): integer of object;  // return 1 to accept edit
   TItemEvent = procedure (index: integer) of object;
   TPointEvent = procedure (x, y: integer) of object;
   TMessagehandler = function (AMessage, WParam, LParam: DWORD): DWORD of Object;
   TDemandEvent = function (Sender: TObject): boolean of object;
   TIntEvent = procedure (value: integer) of object;
   TStringEvent = procedure (const s: widestring) of object;
   TProgressEvent = procedure (i_current, i_total: integer) of object;
   
   TPos = record
      x: integer;
      y: integer;
      width: integer;
      height: integer;
   end;

	TAlign = (alNone, alLeft, alCenter, alRight, alTop, alBottom, alClient, alNoChange);
	TColor = -1 .. High(COLORREF);
	TTransparency = 0..10;
   THotKey = Word;
	TDirection = (drUp = -1, drDown = 1);
	TWindowStatus = (wsNormal, wsMinimize, wsMaximize); //, wsRestoreLast);

function PosToRect (const o_pos: TPos): TRect;
function RectToPos (const o_rect: TRect): TPos;
function RectToPoint (const o_rect: TRect): TPoint;
function PointInRect (const o_point: TPoint; const o_rect: TRect): boolean;
function RectInRect (const rect1, rect2: TRect): boolean;
procedure ShiftRect (var o_rect: TRect; xoffset, yoffset: integer);
procedure ZoomRect (var o_rect: TRect; i_inc: integer);
function ScreenToClientRect (h: HWND; const screct: TRect): TRect;
function ClientToScreenRect (h: HWND; const clrect: TRect): TRect;

function HotKeyToString (hk_hotkey: THotKey): widestring;

type
	TxlFont = class
   private
      FName: widestring;
      FColor: TColor;
      FHeight: integer;
      FBold: boolean;
		FItalic: boolean;
      FUnderline: boolean;
      FStrikeOut: boolean;
      FHandle: HFont;
      FOnChange: TNotifyEvent;

      procedure SetName (const value: widestring);
      procedure SetHeight (value: integer);
      procedure SetBold (value: boolean);
      procedure SetItalic (value: boolean);
      procedure SetUnderline (value: boolean);
      procedure SetStrikeOut (value: boolean);

      procedure SetColor (value: TColor);
      procedure SetSize (value: integer);
      function GetSize (): integer;
      procedure SetLogFont (const o_lfont: LogFontW);
      function GetLogFont (): LogFontW;
      procedure f_OnChange ();
		procedure f_DeleteHandle ();
   public
   	constructor Create ();
      destructor Destroy (); override;
		function Handle (): HFont;

		function Equal (o_f2: TxlFont): boolean;
      procedure Assign (o_f2: TxlFont);

      property Name: widestring read FName write SetName;
      property Color: TColor read FColor write SetColor;
      property Size: integer read GetSize write SetSize;
      property Bold: boolean read FBold write SetBold;
      property Italic: boolean read FItalic write SetItalic;
      property Underline: boolean read FUnderline write SetUnderline;
      property StrikeOut: boolean read FStrikeOut write SetStrikeOut;
      property Height: integer read FHeight write SetHeight;    // 一般不需设定
      property LogFont: LogFontW read GetLogFont write SetLogFont;

      property OnChange: TNotifyEvent read FOnChange write FOnChange;
   end;

   TxlBrush = class
   private
   	FColor: TColor;
      FHandle: HBrush;
      procedure SetColor (value: TColor);
      procedure SetHandle (value: HBrush);
      function GetHandle (): HBrush;
      procedure f_DeleteHandle ();
   public
      destructor Destroy (); override;
      property Color: TColor read FColor write SetColor;
      property Handle: HBrush read GetHandle write SetHandle;
   end;

type
	TxlInterfacedObject = class(TObject, IInterface)
  	protected
      FRefCount: Integer;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
   public
      property RefCount: Integer read FRefCount;
   end;

   TxlThread = class
   private
      FHandle: THandle;
   protected
      FStop: boolean;
      procedure DoExecute (); virtual; abstract;
   public
      constructor Create ();
      destructor Destroy (); override;
      procedure Execute ();
      procedure SusPend ();
      procedure Resume ();
      procedure Terminate ();
   end;

const clBlack: TColor = $000000;
   clWhite: TColor = $FFFFFF;

implementation

uses UxlFunctions, UxlStrUtils, UxlMath, UxlList;

function TxlCollection.Low (): integer;
begin
	result := 0;
end;

function TxlCollection.High (): integer;
begin
	result := Count - 1;
end;

function TxlCollection.PosValid (index: integer): boolean;
begin
	result := InRange (index, Low, High);
end;

function TxlCollection.IsEmpty (): boolean;
begin
	result := Count = 0;
end;

//procedure TxlCollection.f_OnChange (cct: TCollectionChangeType; index: integer);
//begin
//   if assigned (FOnChange) then
//      FOnChange (cct, index);
//end;

//------------------

function TxlInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TxlInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TxlInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
//  if Result = 0 then
//    Destroy;
end;

//-------------------

constructor TxlFont.Create ();
begin
   Name := '';
   Size := 9;
   Color := clBlack;
   Bold := false;
   Italic := false;
   Underline := false;
   StrikeOut := false;
end;

destructor TxlFont.Destroy ();
begin
	f_DeleteHandle;
   inherited;
end;

procedure TxlFont.f_DeleteHandle ();
begin
   if FHandle <> 0 then
   	DeleteObject (FHandle);
	FHandle := 0;
end;

function TxlFont.Handle (): HFont;
begin
	if FHandle = 0 then
   	FHandle := CreateFontIndirectW (LogFont);
   result := FHandle;
end;

procedure TxlFont.f_OnChange ();
begin
	f_DeleteHandle;
   FHandle := CreateFontIndirectW (LogFont);
   if assigned (FOnChange) then
   	FOnChange (self);
end;

procedure TxlFont.SetName (const value: widestring);
begin
	if value <> FName then
   begin
      FName := value;
      f_OnChange;
   end;
end;

procedure TxlFont.SetHeight (value: integer);
begin
	if value <> FHeight then
   begin
   	FHeight := value;
      f_OnChange;
   end;
end;

procedure TxlFont.SetBold (value: boolean);
begin
	if value <> FBold then
   begin
   	FBold := value;
      f_OnChange;
   end;
end;

procedure TxlFont.SetItalic (value: boolean);
begin
	if value <> FItalic then
   begin
   	FItalic := value;
      f_OnChange;
   end;
end;

procedure TxlFont.SetUnderline (value: boolean);
begin
	if value <> FUnderline then
   begin
   	FUnderline := value;
      f_OnChange;
   end;
end;

procedure TxlFont.SetStrikeOut (value: boolean);
begin
	if value <> FStrikeOut then
   begin
   	FStrikeOut := value;
      f_OnChange;
   end;
end;

procedure TxlFont.SetColor (value: TColor);
begin
	if value = FColor then exit;
   FColor := value;
   if assigned (FOnChange) then
   	FOnChange (self);
end;

procedure TxlFont.SetSize (value: integer);
var h_dc: HDC;
begin
	h_dc := GetDC (0);
	Height := -MulDiv (value, GetDeviceCaps (h_DC, LOGPIXELSY), 72);
   ReleaseDC (0, h_dc);
end;

function TxlFont.GetSize (): integer;
var h_dc: HDC;
begin
	h_dc := GetDC (0);
	result := -MulDiv (Height, 72, GetDeviceCaps (h_DC, LOGPIXELSY));
   ReleaseDC (0, h_dc);
end;

function TxlFont.GetLogFont (): LogFontW;
var n: integer;
	s_name: widestring;
begin
   if Name <> '' then
   	s_name := Name
//   else if IsLocaleChinese then
//         s_name := '宋体'       // 宋体的间距太小
   else
		Fname := 'Arial';
   with result do
   begin
      lfHeight := Height;
      lfWidth := 0;
      if Bold then
         lfWeight := FW_BOLD       // 700
      else
         lfWeight := FW_NORMAL;    // 400
      lfItalic := booltoint(Italic);
      lfUnderline := booltoint(Underline);
      lfStrikeOut := booltoint(Strikeout);
      lfCharSet := Default_CHARSET ;
      lfEscapement := 0;
   end;
   n := length(Name) + 1;
   copymemory (@result.lfFaceName, pwidechar(s_Name), n * 2);
end;

procedure TxlFont.SetLogFont (const o_lfont: LogFontW);
begin
   FName := o_lfont.lfFaceName;
   FHeight := o_lfont.lfHeight;
   FBold := (o_lfont.lfWeight = FW_BOLD);
   FItalic := IntToBool (o_lfont.lfItalic);
   FUnderline := IntToBool (o_lfont.lfUnderline);
   FStrikeOut := IntToBool (o_lfont.lfStrikeOut);
   f_OnChange;
end;

function TxlFont.Equal (o_f2: TxlFont): boolean;
begin
   result := false;
   if Name <> o_f2.name then exit;
   if size <> o_f2.Size then exit;
   if color <> o_f2.color then exit;
   if Bold <> o_f2.Bold then exit;
   if Italic <> o_f2.Italic then exit;
   if Underline <> o_f2.Underline then exit;
   if StrikeOut <> o_f2.StrikeOut then exit;
   result := true;
end;

procedure TxlFont.Assign (o_f2: TxlFont);
begin
	FName := o_f2.Name;
   FHeight := o_f2.Height;
   FBold := o_f2.Bold;
   FItalic := o_f2.Italic;
   FUnderline := o_f2.Underline;
   FStrikeOut := o_f2.StrikeOut;
   FColor := o_f2.Color;
   f_OnChange;
end;

//----------------

destructor TxlBrush.Destroy ();
begin
	f_DeleteHandle;
	inherited;
end;

procedure TxlBrush.f_DeleteHandle ();
begin
	if FHandle <> 0 then
   begin
   	DeleteObject (FHandle);
   	FHandle := 0;
   end;
end;

procedure TxlBrush.SetColor (value: TColor);
begin
	f_DeleteHandle;
   FColor := value;
end;

procedure TxlBrush.SetHandle(value: HBrush);
begin
	f_DeleteHandle;
   FHandle := value;
end;

function TxlBrush.GetHandle (): HBrush;
begin
	if FHandle = 0 then
   	FHandle := CreateSolidBrush (FColor);
   result := FHandle;
end;

//--------------

function PosToRect (const o_pos: TPos): TRect;
begin
	SetRect (result, o_pos.x, o_pos.y, o_pos.x + o_pos.width, o_pos.y + o_pos.height);
end;

function RectToPos (const o_rect: TRect): TPos;
begin
	with result do
   begin
   	x := o_rect.left;
      y := o_rect.top;
      width := o_rect.right - o_rect.left;
      height := o_rect.bottom - o_rect.top;
   end;
end;

function RectToPoint (const o_rect: TRect): TPoint;
begin
   result.x := o_rect.left;
   result.y := o_rect.Top;
end;

function PointInRect (const o_point: TPoint; const o_rect: TRect): boolean;
begin
	if (o_point.x >= o_rect.left) and (o_point.x < o_rect.right) and (o_point.y >= o_rect.top) and (o_point.y < o_rect.bottom) then
   	result := true
   else
   	result := false;
end;

function RectInRect (const rect1, rect2: TRect): boolean;
begin
	result := (rect1.Left >= rect2.Left) and (rect1.Top >= Rect2.Top) and (rect1.Right <= rect2.right) and (rect1.Bottom <= rect2.bottom);
end;

procedure ShiftRect (var o_rect: TRect; xoffset, yoffset: integer);
begin
	inc (o_rect.Left, xoffset);
   inc (o_rect.right, xoffset);
   inc (o_rect.Top, yoffset);
   inc (o_rect.bottom, yoffset);
end;

procedure ZoomRect (var o_rect: TRect; i_inc: integer);
begin
	dec(o_rect.Left, i_inc);
   dec(o_rect.Top, i_inc);
   inc (o_rect.Right, i_inc);
   inc (o_rect.Bottom, i_inc);
end;

function ScreenToClientRect (h: HWND; const screct: TRect): TRect;
var rt: TRect;
begin
   GetWindowRect (h, rt);
   with result do
   begin
   	Left := screct.Left - rt.Left;
      Top := screct.Top - rt.Top;
      Right := screct.Right - rt.Left;
      Bottom := screct.Bottom - rt.Top;
   end;
end;

function ClientToScreenRect (h: HWND; const clrect: TRect): TRect;
var rt: TRect;
begin
	GetWindowRect (h, rt);
   with result do
   begin
   	Left := clrect.Left + rt.Left;
      Top := clrect.Top + rt.Top;
      Right := clrect.Right + rt.Left;
      Bottom := clrect.Bottom + rt.Top;
   end;
end;

//---------------

function HotKeyToString (hk_hotkey: THotKey): widestring;
var o_list: TxlStrList;
	hkm, vk: Byte;
   buffer: array[0..12] of widechar;
begin
	if hk_hotkey = 0 then
   	result := ''
   else
   begin
      o_list := TxlStrList.Create();
      o_list.Separator := '+';
      hkm := HiByte (hk_hotkey);
      if (hkm and 1) <> 0 then o_list.Add ('Shift');
      if (hkm and 2) <> 0 then o_list.Add ('Ctrl');
      if (hkm and 4) <> 0 then o_list.Add ('Alt');
      vk := LoByte(hk_hotkey);
      GetKeyNameTextW (MapVirtualKey (vk, 0) shl 16, buffer, 13);
      o_list.Add (pwidechar(@buffer));
      result := o_list.Text;
      o_list.free;
   end;
end;

//---------------

constructor TxlThread.Create ();
begin
	Fhandle := 0;
end;

destructor TxlThread.Destroy ();
begin
	Terminate;
   inherited;
end;

procedure TxlThread.Execute ();
var id: cardinal;
	FThreadFunc: procedure () of object;
begin
	FStop := false;
   FThreadFunc := DoExecute;
	Fhandle := CreateThread (nil, 0, @FThreadFunc, nil, 0, id);
   ExitThread (0);
   Fhandle := 0;
end;

procedure TxlThread.SusPend ();
begin
	if Fhandle <> 0 then
   	SuspendThread (Fhandle);
end;

procedure TxlThread.Resume ();
begin
	if Fhandle <> 0 then
   	ResumeThread (Fhandle);
end;

procedure TxlThread.Terminate ();
begin
	FStop := true;
end;

end.


//   Exception = class
//   private
//      FMessage: widestring;
//   protected
//   public
//      constructor Create(const Msg: widestring);
//      property Message: Widestring read FMessage write FMessage;
//   end;

//constructor TxlException.Create(const Msg: widestring);
//begin
//	FMessage := Msg;
//end;




