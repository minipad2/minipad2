unit UDisplay;

interface

uses Windows, Messages, UxlClasses, UxlWindow, UWordBox, UxlExtClasses, UxlWinClasses;

type
	TDisplayMode = (dmFixed, dmTrack, dmEmbed, dmFullScreen, dmNone);

	TDisplaySuper = class (TxlInterfacedObject)
   private
   protected
   public
      procedure ShowWord (o_word: TWord); virtual; abstract;
      procedure ShowString (const value: widestring);
      function ExportWord (): widestring; virtual;
   end;

   TNormalDisplaySuper = class (TDisplaySuper, IOptionObserver)
   private
      FWord: TWord;
      FFont: TxlFont;
      FTwoLines: boolean;
      procedure DoShowWord ();
   protected
   	FMainWin: TxlWindow;
		function f_ProcessMessages (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWORD; virtual;
   public
   	constructor Create (AWin: TxlWindow); virtual;
      destructor Destroy (); override;

      procedure ShowWord (o_word: TWord); override;
      function ExportWord (): widestring; override;
      procedure OptionChanged (); virtual;
   end;

   TFixedDisplay = class (TNormalDisplaySuper, IMemorizer)
   protected
		function f_ProcessMessages (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWORD; override;
   public
   	constructor Create (AWin: TxlWindow); override;
      destructor Destroy (); override;
      procedure RestoreMemory ();
      procedure SaveMemory ();
	end;

   TTrackDisplay = class (TNormalDisplaySuper)
   private
      FTrackTimer: TxlTimer;
      FXSpace, FYSpace: integer;
      procedure f_OnTrackTimer (Sender: TObject);
   public
   	constructor Create (AWin: TxlWindow); override;
      destructor Destroy (); override;
      procedure OptionChanged (); override;
	end;

   TFullScreenDisplay = class (TDisplaySuper, IOptionObserver)
   private
   	FMainWin: TxlWindow;
      FWord: TWord;
      FWordFont: TxlFont;
      FExpFont: TxlFont;
//      FTextColor: TColor;
		function f_ProcessMessages (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWORD;
//		procedure f_DeleteHFonts ();
      procedure DoShowWord ();
   public
   	constructor Create (AWin: TxlWindow);
      destructor Destroy (); override;
      procedure ShowWord (o_word: TWord); override;
      function ExportWord (): widestring; override;
      procedure OptionChanged ();
   end;

   TEmbedDisplay = class (TDisplaySuper)
   private
      FHForeWin: HWND;
      FTitle, FSentence: widestring;
   	procedure f_TryRestore ();
   public
   	constructor Create (AWin: TxlWindow);
		destructor Destroy (); override;

      procedure ShowWord (o_word: TWord); override;
   end;

   TNoneDisplay = class (TDisplaySuper)
   public
   	constructor Create (AWin: TxlWindow);
      procedure ShowWord (o_word: TWord); override;
   end;

   TDisplayFactory = class
   private
   	FMainWin: TxlWindow;
   	FDisplay: TDisplaySuper;
      FDisplayMode: integer;
   public
		constructor Create (AWin: TxlWindow);
   	destructor Destroy (); override;
      function GetDisplay (dm: TDisplayMode): TDisplaySuper;
   end;

implementation

uses UGlobalObj, UxlFunctions, UxlWinDef, UxlStrUtils, UxlMath, UxlCommDlgs, Resource;

const cColors: array[0..4] of TColor = ($FFFFFF, $FFFFFF, $FFFFFF, $FFFFFF, $FFFFFF);

procedure TDisplaySuper.ShowString (const value: widestring);
var o_word: TWord;
begin
	o_word.word := '';
   o_word.exp := value;
	ShowWord (o_word);
end;

function TDisplaySuper.ExportWord (): widestring;
begin
	result := '';
end;

//--------------------

constructor TNormalDisplaySuper.Create (AWin: TxlWindow);
begin
	FFont := TxlFont.Create;
	FMainWin := AWin;
   AWin.MessageHandler := f_ProcessMessages;
   OptionMan.AddObserver (self);
end;

destructor TNormalDisplaySuper.Destroy ();
begin
	OptionMan.RemoveObserver (self);
   FMainWin.MessageHandler := nil;
   FFont.free;
	inherited;
end;

procedure TNormalDisplaySuper.OptionChanged ();
begin
   FmainWin.Transparency := OptionMan.Options.Transparency;
   FFont.assign (OptionMan.Options.WinFont);
   FTwoLines := OptionMan.Options.TwoLines;
   FMainWin.Redraw;
end;

procedure TNormalDisplaySuper.ShowWord (o_word: TWord);
begin
	FWord := o_word;
   FMainWin.Redraw;
end;

function TNormalDisplaySuper.f_ProcessMessages (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWORD;
begin
   b_processed := false;
   case AMessage of
   	WM_PAINT:
			DoShowWord;
   	WM_ESCAPE:
      	begin
      		CommandMan.ExecuteCommand(m_start);
            result := 0;
            b_processed := true;
         end;
   end;
end;

function TNormalDisplaySuper.ExportWord (): widestring;
var s2: widestring;
begin
   result := FWord.Word;
   s2 := FWord.Exp;
   if s2 = '' then exit;
   if result <> '' then
   begin
      if FTwoLines or IsSubStr (#13#10, s2) then
      begin
         if FirstPos (#13#10, s2) <> 1 then
            result := result + #13#10;
      end
      else
         result := result + ': ';
   end;
   result := result + s2;
end;

procedure TNormalDisplaySuper.DoShowWord ();
var h_DC: HDC;
   ps: PaintStruct;
   s: widestring;
   hf: HFont;
   cl: COLORREF;
   rt, scrt: TRect;
begin
   GetScreenRect (scrt);
   h_DC := BeginPaint (FMainWin.handle, ps);
   SetBkMode (h_DC, TRANSPARENT);
   hf := SelectObject (h_DC, FFont.handle);
   cl := SetTextColor (h_DC, Ffont.Color);

   s := ExportWord;
   with rt do
   begin
   	Left := 10;
      Top := 4;
      Right := 200 - 10;
      Bottom := 15;
   end;

   DrawTextW (h_dc, pwidechar(s), length(s), rt, DT_CALCRECT or DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX);
   FMainWin.Width := ConfineRange (rt.Right + 10, 150, scrt.Right);
   FMainWin.Height := ConfineRange (rt.Bottom + 5, 15, scrt.Bottom);
   DrawTextW (h_DC, pwidechar(s), length(s), rt, DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX);

   SetTextColor (h_dc, cl);
   SelectObject (h_dc, hf);
   EndPaint (FMainWin.handle, ps);
end;

//------------------

constructor TFixedDisplay.Create (AWin: TxlWindow);
begin
	inherited Create (AWin);
   MemoryMan.AddObserver(self);
   AWin.Show;
end;

destructor TFixedDisplay.Destroy ();
begin
	SaveMemory;
   MemoryMan.RemoveObserver (self);
   inherited;
end;

procedure TFixedDisplay.RestoreMemory ();
begin
	FMainWin.Left := MemoryMan.WinPos.X;
   FMainWin.Top := MemoryMan.WinPos.Y;
end;

procedure TFixedDisplay.SaveMemory ();
begin
   MemoryMan.WinPos.X := FMainWin.Left;
   MemoryMan.WinPos.Y := FMainWin.Top;
end;

function TFixedDisplay.f_ProcessMessages (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWord;
	procedure f_SetCursor (pc: pAnsiChar);
   begin
   	SetCursor (LoadCursor (0, pc));
	end;
var pt: TPoint;
{$J+}
	const dx: integer = 0;
	const dy: integer = 0;
{$J-}
begin
	b_processed := true;
   result := 0;
   case AMessage of
   	WM_LBUTTONDOWN:
      	begin
      		f_SetCursor (IDC_HAND);
            GetCursorPos (pt);
            dx := pt.x - FMainWin.Left;
            dy := pt.y - FMainWin.Top;
         end;
      WM_MOUSEMOVE:
         if KeyPressed (VK_LBUTTON) then
         begin
            f_SetCursor (IDC_HAND);
            GetCursorPos (pt);
            FMainWin.Left := pt.x - dx;
            FMainWin.Top := pt.y - dy;
         end;
      WM_LBUTTONUP:
     		f_SetCursor (IDC_ARROW);
      else
      	result := inherited f_ProcessMessages (AMessage, wParam, lParam, b_processed);
   end;
end;

//--------------------

constructor TTrackDisplay.Create (AWin: TxlWindow);
begin
	inherited Create (AWin);
   FTrackTimer := TimerCenter.NewTimer;
   FTrackTimer.Interval := 20;
   FTrackTimer.OnTimer := f_OnTrackTimer;
   FTrackTimer.Start;
   f_OnTrackTimer (self);
   AWin.Show;
end;

destructor TTrackDisplay.Destroy ();
begin
   TimerCenter.ReleaseTimer (FTrackTimer);
   inherited;
end;

procedure TTrackDisplay.f_OnTrackTimer (Sender: TObject);
var pt, wt: TPoint;
	rc: TRect;
begin
//	if KeyPressed (VK_LBUTTON) or KeyPressed (VK_RBUTTON) then exit;
	GetCursorPos (pt);
//   if not PointInRect (pt, FMainWin.Rect) then
	GetScreenRect (rc);
	wt.x := pt.x + FXSpace;
   if not InRange (wt.x, 0, rc.Right - FMainWin.Width) then
   	wt.x := pt.x - FMainWin.Width - FXSpace;
   wt.y := pt.y + FYSpace;
   if not InRange (wt.y, 0, rc.Bottom - FMainWin.Height) then
   	wt.y := pt.Y - FMainWin.Height - FYSpace;
	FMainWin.Move (wt.x, wt.y);
end;

procedure TTrackDisplay.OptionChanged ();
begin
	inherited;
   FXSpace := OptionMan.Options.TrackXSpace;
   FYSpace := OptionMan.Options.TrackYSpace;
end;

//----------------

constructor TFullScreenDisplay.Create (AWin: TxlWindow);
var rt: TRect;
begin
	FWordFont := TxlFont.Create;
   FExpFont := TxlFont.Create;

	FMainWin := AWin;
   FMainWin.Transparency := 0;
   FMainWin.Show;
   FMainWin.MessageHandler := f_ProcessMessages;
   GetScreenRect (rt);
   FMainWin.Rect := rt;
   SetForeGroundWindow (FMainWin.Handle);
   OptionMan.AddObserver (self);
end;

destructor TFullScreenDisplay.Destroy ();
begin
	OptionMan.RemoveObserver (self);
   FMainWin.MessageHandler := nil;
   FWordFont.free;
   FExpFont.free;
   inherited;
end;

procedure TFullScreenDisplay.OptionChanged ();
begin
	FWordFont.assign (OptionMan.Options.WinFont);
   FWordFont.Size := 30;

   FExpFont.Assign (OptionMan.Options.WinFont);
   FExpFont.Size := 25;
end;

procedure TFullScreenDisplay.ShowWord (o_word: TWord);
begin
   FWord := o_word;
   if FirstPos (#13#10, FWord.Exp) = 1 then
   	FWord.Exp := MidStr (FWord.Exp, 3);
   FMainWin.Redraw;
end;

function TFullScreenDisplay.ExportWord (): widestring;
begin
   result := FWord.Word + #13#10 + FWord.Exp;
end;

function TFullScreenDisplay.f_ProcessMessages (AMessage, WParam, LParam: DWORD; var b_processed: boolean): DWORD;
begin
	b_processed := false;
   case AMessage of
   	WM_PAINT:
			DoShowWord;
   	WM_ESCAPE:
      	begin
      		CommandMan.ExecuteCommand(m_fullscreen);
            result := 0;
            b_processed := true;
         end;
   end;
end;

procedure TFullScreenDisplay.DoShowWord ();
var h_DC: HDC;
   ps: PaintStruct;
   hf: HFont;
   cl: COLORREF;
   rt, rt2: TRect;
   s: widestring;
   i_height, i_height2, i_wordheight: integer;
   i_flag: integer;
begin
   h_DC := BeginPaint (FMainWin.handle, ps);
   SetBkMode (h_DC, TRANSPARENT);
   cl := SetTextColor (h_DC, FExpFont.Color);
   i_flag := DT_CENTER or DT_TOP or DT_EXPANDTABS or DT_NOPREFIX;
   i_wordheight := ABS(FWordFont.Height) + 25;

   hf := SelectObject (h_DC, FExpFont.handle);
   s := FWord.exp;

 // 计算位置，使释义位于屏幕中心
   rt := FMainWin.Rect;
   i_height := rt.Bottom - rt.Top;
	rt2 := rt;
	DrawTextW (h_dc, pwidechar(s), length(s), rt2, DT_CALCRECT or i_flag);
	i_height2 := rt2.Bottom - rt2.Top;
   rt.Top := Max ((i_height - i_height2) div 2, i_wordheight + 20);
   DrawTextW (h_dc, pwidechar(s), length(s), rt, i_flag);

	dec (rt.Top, i_wordheight);
   SelectObject (h_dc, FWordFont.handle);
   DrawTextW (h_dc, pwidechar(FWord.word), length(FWord.word), rt, i_flag);

   SelectObject (h_DC, hf);
   SetTextColor (h_DC, cl);
   EndPaint (FMainWin.handle, ps);
end;

//----------------

procedure f_SetForeText (hw: HWND; const s: widestring);
begin
   SendMessageW (hw, WM_SETTEXT, 0, lparam(pwidechar(s)));
end;

function f_GetForeText (hw: HWND): widestring;
var i_length: integer;
begin
   result := '';
   i_length := SendMessageW (hw, WM_GETTEXTLENGTH, 0, 0);
   if i_length <= 0 then exit;
   setlength (result, i_length);
   SendMessageW (hw, WM_GETTEXT, i_length + 1, lparam(pwidechar(result)));
end;

constructor TEmbedDisplay.Create (AWin: TxlWindow);
begin
	AWin.Hide;
end;

destructor TEmbedDisplay.Destroy ();
begin
	f_TryRestore;
	inherited;
end;

procedure TEmbedDisplay.ShowWord (o_word: TWord);
var hw: HWND;
	s: widestring;
begin
   hw := GetForeGroundWindow;
   if hw <> FHForeWin then
   begin
      f_TryRestore;
      FHForeWin := hw;
   end;

   s := f_GetForeText (hw);
   if s <> FSentence then    // 如果当前窗口标题不等于前一次查词结果，则可能窗口自己更改了文字，记忆该文字
      FTitle := s;

   FSentence := o_word.Word + ': ' + o_word.Exp;
   f_SetForeText (hw, FSentence);
end;

procedure TEmbedDisplay.f_TryRestore ();
begin
   if f_GetForeText(FHForeWin) = FSentence then   // 如果该窗口的当前内容等于前一次的查词结果，则替换回原标题。
      f_SetForeText (FHForeWin, FTitle);
   FSentence := '';
   FTitle := '';
end;

//-----------------------

constructor TNoneDisplay.Create (AWin: TxlWindow);
begin
	AWin.Hide;
end;

procedure TNoneDisplay.ShowWord (o_word: TWord);
begin
end;

//-----------------------

constructor TDisplayFactory.Create (AWin: TxlWindow);
begin
	FMainWin := AWin;
   FDisplayMode := -1;
end;

destructor TDisplayFactory.Destroy ();
begin
   FreeAndNil (FDisplay);
   inherited;
end;

function TDisplayFactory.GetDisplay (dm: TDisplayMode): TDisplaySuper;
begin
	if Ord(dm) <> FDisplayMode then
   begin
      FDisplay.Free;
      case dm of
         dmFixed:
            FDisplay := TFixedDisplay.Create (FMainWin);
         dmTrack:
         	FDisplay := TTrackDisplay.Create (FMainWin);
         dmEmbed:
            FDisplay := TEmbedDisplay.create (FMainWin);
         dmFullScreen:
            FDisplay := TFullScreenDisplay.Create (FMainWin)
         else
            FDisplay := TNoneDisplay.Create (FMainWin);
      end;
      FDisplayMode := Ord(dm);
   end;
   result := FDisplay;
end;

end.

