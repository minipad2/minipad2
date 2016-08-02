unit UCommonClasses;

interface

uses Windows, UxlIniFile, UxlList, UxlClasses, UxlWinControl, UxlWindow, UxlWinClasses;

type
   TIniFileEx = class (TxlIniFile)
   public
   	procedure ReadItemList (const s_section: widestring; o_list: TxlStrList);
		procedure SaveItemList (const s_section: widestring; o_list: TxlStrList);
	end;

   TBrowseHistory = class
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

type
   IClipObserver = interface
      procedure ClipNotify (const s_text: widestring);
   end;

   TClipWatcher = class (TxlInterfacedObject, IMessageObserver)
   private
   	FWin: TxlWindow;
   	FHNext: HWND;
      FPassNext: boolean;
      FLastTime: integer;
      FTimer: TxlTimer;
      FObservers: TxlInterfaceList;

      procedure DrawClipboard (wParam, lParam: DWORD);
      procedure ChangeCBChain (wParam, lParam: DWORD);
		procedure SetStart (b_start: boolean);
		procedure ReLinkChain (Sender: TObject);
      procedure NotifyObservers (const s: widestring);
   public
      constructor Create (AWindow: TxlWindow);
      destructor Destroy (); override;

      procedure AddObserver (value: IClipObserver);
      procedure RemoveObserver (value: IClipObserver);
      procedure PassNext ();

      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
   end;

implementation

uses Messages, UxlFunctions;

procedure TIniFileEx.ReadItemList (const s_section: widestring; o_list: TxlStrList);
var i, n: integer;
begin
	Cache.ReadSection (s_section);
   n := Cache.GetInteger ('ItemCount', 0);
   o_list.Clear;
   for i := 0 to n - 1 do
   	o_list.Add (Cache.GetString ('Item' + IntToStr(i), ''));
end;

procedure TIniFileEx.SaveItemList (const s_section: widestring; o_list: TxlStrList);
var i: integer;
begin
	Section := s_section;
   WriteInteger('ItemCount', o_list.Count);
   for i := o_list.Low to o_list.High do
      WriteString('Item' + IntToStr(i), o_list[i]);
end;

//--------------------------

constructor TBrowseHistory.Create ();
begin
	FIDList := TxlIntList.Create;
   FPOs := -1;
end;

destructor TBrowseHistory.Destroy ();
begin
	FIDList.Free;
   inherited;
end;

function TBrowseHistory.GetPrior (): integer;
begin
	if FPos > FIDList.Low then dec (FPos);
   result := FIDList[FPos];
end;

function TBrowseHistory.GetNext (): integer;
begin
	if FPos < FIDList.High then inc (FPos);
   result := FIDList[FPos];
end;

procedure TBrowseHistory.Add (id: integer);
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

procedure TBrowseHistory.Delete (id: integer);
var i: integer;
begin
	i := FIDList.Find (id);
   FIDList.Delete (i);
end;

function TBrowseHistory.HasPrior (): boolean;
begin
	result := FPos > FIDList.Low;
end;

function TBrowseHistory.HasNext (): boolean;
begin
	result := FPos < FIDList.High;
end;

//------------------------

constructor TClipWatcher.Create (AWindow: TxlWindow);
begin
   FWin := AWindow;
   FObservers := TxlInterfaceList.Create;

   FTimer := TimerCenter.NewTimer;
   FTimer.Interval := 1200000;
   FTimer.OnTimer := RelinkChain;

   FWin.AddMessageObserver (self);
end;

destructor TClipWatcher.Destroy ();
begin
   SetStart (false);
	TimerCenter.ReleaseTimer (FTimer);
   FWin.RemoveMessageObserver (self);
   FObservers.free;
   inherited;
end;

procedure TClipWatcher.AddObserver (value: IClipObserver);
begin
   if FObservers.Count = 0 then
      SetStart (true);
	FObservers.Add (value);
end;

procedure TClipWatcher.RemoveObserver (value: IClipObserver);
begin
	FObservers.Remove (value);
   if FObservers.Count = 0 then
      SetStart (false);
end;

procedure TClipWatcher.NotifyObservers (const s: widestring);
var i: integer;
begin
	i :=  FObservers.Low;
   while i <= FObservers.High do
   begin
		if FObservers[i] <> nil then
   		IClipObserver(FObservers[i]).ClipNotify (s);
   	inc (i);
   end;
end;

procedure TClipWatcher.PassNext ();
begin
   FPassNext := true;
end;

procedure TClipWatcher.SetStart (b_start: boolean);
begin
   if b_start then
      FHNext := SetClipboardViewer (FWin.handle)
   else
   	ChangeClipboardChain (FWin.handle, FHNext);
   FTimer.Enabled := b_start;
   FPassNext := false;
   FLastTime := 0;
end;

procedure TClipWatcher.ReLinkChain (Sender: TObject);
begin
   ChangeClipboardChain (FWin.handle, FHNext);
   FHNext := SetClipboardViewer (FWin.handle);
end;

//------------------------

function TClipWatcher.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
begin
	b_processed := false;
   case AMessage of
   	WM_DRAWCLIPBOARD:
      	DrawClipboard (wParam, lParam);
      WM_CHANGECBCHAIN:
      	ChangeCBChain (wParam, lParam);
   end;
end;

procedure TClipWatcher.DrawClipboard (wParam, lParam: DWORD);
var i_time: integer;
   s: widestring;
begin
	i_time := GetTickCount;
	if i_time - Flasttime <= 100 then   // 防止由于某些未知因素引起的死循环
   	exit
   else
   	Flasttime := i_time;

   s := Clipboard.Text;
   if (s <> '') and (not FPassNext) then
      NotifyObservers (s);
   FPassNext := false;

   if IsWindow(FHNext) and (FHNext <> FWin.handle) then
   	PostMessageW (FHNext, WM_DRAWCLIPBOARD, wParam, lParam);
end;

procedure TClipWatcher.ChangeCBChain (wParam, lParam: DWORD);
begin
	if wParam = FHNext then
   	FHNext := lParam
   else if IsWindow(FHNext) and (FHNext <> FWin.handle) then
   	PostMessageW (FHNext, WM_CHANGECBCHAIN, wParam, lParam);
end;

end.
