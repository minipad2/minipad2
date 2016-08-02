unit UxlCommDlgs;

interface

uses Windows, CommDlg, UxlClasses, UxlWinControl, UxlFunctions, ShlObj, UxlStrUtils;

type TxlCommDlgSuper = class
protected
	FOwnerHandle: HWND;
   procedure OnInitialize (); virtual;
public
	constructor Create (AOwner: TxlWinControl = nil); overload;
   constructor Create (HOwner: HWND); overload;
   function Execute(): boolean; virtual; abstract;
end;

type TOpenSaveType = (ostOpen, ostSave);

type TxlOpenSaveDialog = class (TxlCommDlgSuper)
protected
   FType: TOpenSaveType;
   FMultiSelect: boolean;
   FOverwritePrompt: boolean;
	FFilter: WideString;
   FTitle: WideString;
   FDefaultExt: WideString;
   FFilterIndex: integer;
   FFileName: WideString;
   FPath: WideString;

   procedure OnInitialize (); override;
public
   property Title: widestring write FTitle;
   property Path: widestring read Fpath write FPath;  // return value ends with '\'
   property FileName: widestring read FFileName write FFileName;    // return value contains no path
   property Filter: widestring write FFilter;
   property FilterIndex: integer read FFilterIndex write FFilterIndex;
   property DefaultExt: widestring read FDefaultExt write FDefaultExt;

   function Execute(): boolean; override;
end;

type TxlOpenDialog = class (TxlOpenSaveDialog)
protected
   procedure OnInitialize (); override;
public
   property MultiSelect: boolean write FMultiSelect;
end;

type TxlSaveDialog = class (TxlOpenSaveDialog)
protected
   procedure OnInitialize (); override;
public
   property OverWritePrompt: boolean write FOverwritePrompt;
end;

type TxlPathDialog = class (TxlCommDlgSuper)
public
   Path: widestring;
   Title: widestring;
   function Execute(): boolean; override;
end;

type TxlFontDialog = class (TxlCommDlgSuper)
private
   FFont: TxlFont;
   procedure SetFont (value: TxlFont);
public
	constructor Create (AOwner: TxlWinControl = nil);
   destructor Destroy (); override;
   function Execute(): boolean; override;
   property Font: TxlFont read FFont write SetFont;
end;

type TxlColorDialog = class (TxlCommDlgSuper)
private
	FColor: TColor;
   FCustColors: array [0..15] of TColor;
public
   function Execute(): boolean; override;
   property Color: TColor read FColor write FColor;
end;

type TxlPrintDialog = class (TxlCommDlgSuper)
private
	FCopies: integer;
   FPrinter: integer;
public
	function Execute(): boolean; override;
   property Copies: integer read FCopies write FCopies;
   property Printer: integer read FPrinter write FPrinter;
end;

type TMessageType = (mtWarning, mtExclamation, mtInformation, mtQuestion, mtQuestionYesNo, mtQuestion3B, mtError);
   TMessageResult = (mrOK, mrCancel, mrYes, mrNo);

//function BrowseCallbackProc (Wnd: HWND; uMsg: UINT; l_Param, lpData: LPARAM): integer; stdcall;
function ShowMessage (const s_msg: widestring; fmMessageType: TMessageType = mtInformation; s_caption: widestring = ''): TMessageResult; overload;
procedure ShowMessage (const i_msg: Int64); overload;

implementation

uses UxlDialog, UxlWindow, UxlWinDef;

constructor TxlCommDlgSuper.Create (AOwner: TxlWinControl = nil);
begin
	if AOwner <> nil then
      FOwnerHandle := AOwner.handle
   else
      FOwnerHandle := MainWinHandle;
   OnInitialize ();
end;

constructor TxlCommDlgSuper.Create (HOwner: HWND);
begin
   FOwnerHandle := HOwner;
   OnInitialize ();
end;

procedure TxlCommDlgSuper.OnInitialize ();
begin
end;

//----------------

function TxlOpenSaveDialog.Execute(): boolean;
var ofn: OpenFileNameW;
   strFile, strFilter: pwidechar;
   i: integer;
   b: boolean;
const i_maxchar = 1024;
begin
   strFile := AllocMem (i_maxchar * 2);
   CopyMemory (StrFile, pwidechar(FFileName), length(FFileName) * 2);

   strFilter := AllocMem (i_maxchar * 2);
   CopyMemory (StrFilter, pwidechar(FFilter), length(FFilter) * 2);
   for i := 0 to length(FFilter) - 1 do
   	if strFilter[i] = '|' then strFilter[i] := #0;

   with ofn do
   begin
      lStructSize := sizeof(ofn);
      hWndOwner := FOwnerHandle;
      hInstance := system.Maininstance;
      lpstrFilter := strFilter;
      lpstrCustomFilter := nil;
      nMaxCustFilter := 0;
      nFilterIndex := FFilterIndex;
      lpstrFile := strFile;
      nMaxFile := i_maxchar;
      lpstrFileTitle := nil;
      nMaxFileTitle:= 0;
      lpstrInitialDir := pwidechar(FPath);
      lpstrTitle := PWideChar(FTitle);
      Flags := OFN_EXPLORER or OFN_NOCHANGEDIR;
      if FType = ostOpen then
      begin
         Flags := Flags or OFN_CREATEPROMPT;
         if FMultiSelect then Flags := Flags or OFN_ALLOWMULTISELECT;
      end
      else
         if FOverWritePrompt then Flags := FLags or OFN_OVERWRITEPROMPT;
      nFileOffset:=0;
      nFileExtension:=0;
      lpstrDefExt:=pWideChar(FDefaultExt);
      lCustData:=0;
      lpfnHook:=nil;
      lpTemplateName:='';
      pvReserved:=nil;
      dwReserved:=0;
      FlagsEx:=0;
   end;
   if FType = ostOpen then
	   b := GetOpenFileNameW(ofn)
   else
   	b := GetSaveFileNameW(ofn);
   if b then
   begin
   	if FMultiSelect then
      begin
         for i := 1 to i_maxchar - 2 do
            if (strFile[i] = #0) and (strFile[i - 1] <> #0) and (strFile[i + 1] <> #0) then
               strFile[i] := #9;
         FFileName := MidStr(strFile, ofn.nFileOffset + 1);
         FPath := LeftStr (StrFile, ofn.nFileOffset - 1) + '\';
      end
      else
      begin
         FFileName := ExtractFileName (strFile);
         FPath := ExtractFilePath (strFile);
      end;
      result := true;
   end
   else
   	result := false;

   FreeMem (strFile, i_maxchar * 2);
   FreeMem (strFilter, i_maxchar * 2);
end;

procedure TxlOpenSaveDialog.OnInitialize ();
begin
	FFilter := '';
   FFilterIndex := 0;
   FTitle := '';
   FDefaultExt := '';
   FFileName := '';
   FPath := '';
end;

procedure TxlOpenDialog.OnInitialize ();
begin
	FType := ostOpen;
   FMultiSelect := false;
   inherited;
end;

procedure TxlSaveDialog.OnInitialize ();
begin
	FType := ostSave;
   FOverWritePrompt := true;
   inherited;
end;

//---------------------

function TxlPathDialog.Execute(): boolean;
var o_binfo: TBROWSEINFOW;
   p: pwidechar;
   p2: pointer;
begin
	p := AllocMem (2000);
//	copymemory (p, pwidechar(Path), length(Path) * 2);
   with o_binfo do
   begin
      hwndOwner := FOwnerHandle;
      pidlRoot := nil;
      pszDisplayName := p;
      lpszTitle := pwidechar(Title);
      ulFlags := BIF_NEWDIALOGSTYLE; // BIF_VALIDATE;
      lpfn := nil;  //BrowseCallBackProc;
      lParam := 0;
   end;
	p2 := SHBrowseForFolderW (o_binfo);
   result := (p2 <> nil);
   if result then
   begin
	   SHGetPathFromIDListW (p2, p);
   	Path := p;
      if rightstr(path, 1) <> '\' then path := path + '\';
   end;
   FreeMem (p, 2000);
end;

//---------------------

function TxlColorDialog.Execute(): boolean;
var o_color: TChooseColor;
begin
	with o_color do
   begin
   	lStructSize := sizeof (o_color);
		hWndOwner := FOwnerHandle;
      hInstance := system.Maininstance;
      rgbResult := FColor;
  		lpCustColors := @FCustColors;
  		Flags := CC_RGBINIT or CC_FULLOPEN;
 		lCustData := 0;
  		lpfnHook := nil;
  		lpTemplateName := nil;
   end;
   result := ChooseColor (o_color);
   if result then
   	FColor := o_color.rgbResult;
end;

//-----------------------

constructor TxlFontDialog.Create (AOwner: TxlWinControl = nil);
begin
	inherited Create (AOwner);
   FFont := TxlFont.Create;
end;

destructor TxlFontDialog.Destroy ();
begin
	FFont.Free;
   inherited;
end;

procedure TxlFontDialog.SetFont (value: TxlFont);
begin
	FFont.Assign (value);
end;

function TxlFontDialog.Execute(): boolean;
var o_cfont: TChooseFontW;
	o_lfont: LogFontW;
begin
	o_lfont := FFont.LogFont;
	with o_cfont do
   begin
   	lStructSize := sizeof (o_cfont);
		hWndOwner := FOwnerHandle;
      hDC := 0;
      lpLogFont := @o_lfont;
      iPointSize := 0;
      Flags := CF_EFFECTS or CF_INITTOLOGFONTSTRUCT or CF_SCREENFONTS or CF_NOSCRIPTSEL;  // or CF_USESTYLE
      rgbColors := FFont.Color;
      lCustData := 0;
  		lpfnHook := nil;
  		lpTemplateName := nil;
      hInstance := 0;
      lpszStyle := nil;
      nFontType := 0;
      nSizeMin := 0;
      nSizeMax := 0;
   end;
   result := ChooseFontW (o_cfont);
   
   if result then
   begin
   	FFont.LogFont := o_cfont.lpLogFont^;
      FFont.Color := o_cfont.rgbColors;
   end;
end;

//-------------------------

function TxlPrintDialog.Execute(): boolean;
//var o_prdlg: LPPrintDlgEx;
//	hr: HResult;
begin
//	result := false;
//   hr := PrintDlgEx (o_prdlg);
//   if not hr = S_OK then exit;
//
//   if o_prdlg.dwResultAction = PD_RESULT_PRINT then
//   begin
   	result := true;
//   end;
end;

//-------------------------

function ShowMessage (const s_msg: widestring; fmMessageType: TMessageType = mtInformation; s_caption: widestring = ''): TMessageResult;
var i_icon: cardinal;
	hParent: HWND;
begin
	case fmMessageType of
   	mtExclamation, mtWarning:
      	begin
         	i_icon := MB_OK or MB_ICONExclamation;
            if s_caption = '' then s_caption := '警告!';
         end;
      mtError:
      	begin
         	i_icon := MB_OK or MB_ICONERROR;
            if s_caption = '' then s_caption := '错误!';
         end;
      mtQuestion, mtQuestion3B, mtQuestionYesNo:
      	begin
         	if fmMessageType = mtQuestion then
         		i_icon := MB_OKCANCEL
            else if fmMessageType = mtQuestionYesNo then
            	i_icon := MB_YESNO
            else
            	i_icon := MB_YESNOCANCEL;
            i_icon := i_icon or MB_ICONQUESTION;
            if s_caption = '' then s_caption := '注意!';
         end;
      else
      	begin
         	i_icon := MB_OK or MB_ICONInformation;
            if s_caption = '' then s_caption := '提示!';
         end;
   end;
   hParent := GetWindow (MainWinHandle, GW_ENABLEDPOPUP);
   if hParent = 0 then
      hParent := MainWinHandle;
   SendMessageW (hParent, WM_DIALOGOPENED, 0, 0);
	case MessageBoxW (hParent, pwidechar(s_msg), pwidechar(s_caption), i_icon) of
   	IDOK:	result := mrOK;
   	IDCancel: result := mrCancel;
      IDYes: result := mrYes;
      else result := mrNo;  // IDNo
   end;
   SendMessageW (hParent, WM_DIALOGCLOSED, 0, 0);
end;

procedure ShowMessage (const i_msg: Int64);
begin
   ShowMessage (IntToStr(i_msg));
end;

end.





