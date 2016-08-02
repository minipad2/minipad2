unit UxlIniFile;

interface

uses windows, UxlFunctions, UxlList;

type
   TIniCache = class
   private
   	FIniFile: widestring;
   	FList: TxlStrList;
      FBuffer: pwidechar;
   public
   	constructor Create (const s_inifile: widestring);
      destructor Destroy (); override;
      procedure Clear ();

   	procedure ReadSection (const s_section: widestring);
      function GetString (const s_key, s_default: widestring): widestring;
      function GetInteger (const s_key: widestring; i_default: integer): integer;
      function GetBool (const s_key: widestring; b_default: boolean): boolean;
      function GetFloat (const s_key: widestring; d_default: double): double;

      procedure AddString (const s_key, s_value: widestring);
      procedure AddInteger (const s_key: widestring; i_value: integer);
      procedure AddBool (const s_key: widestring; b_value: boolean);
      procedure AddFloat (const s_key: widestring; d_value: double);
      procedure WriteSection (const s_section: widestring);
   end;

   TxlIniFile = class
   private
      Finifile: widestring;
      FSection: widestring;
      FCache: TIniCache;
   public
      constructor Create (const s_inifile: widestring);
      destructor Destroy (); override;
      function Cache(): TIniCache;
      property Section: widestring read FSection write FSection;

      function ReadString (const s_key, s_default: widestring): widestring;
      function ReadInteger (const s_key: widestring; i_default: integer): integer;
      function ReadBool (const s_key: widestring; b_default: boolean): boolean;
      function ReadFloat (const s_key: widestring; d_default: double): double;

      procedure WriteString (const s_key, s_value: widestring);
      procedure WriteInteger (const s_key: widestring; i_value: integer);
      procedure WriteBool (const s_key: widestring; b_value: boolean);
      procedure WriteFloat (const s_key: widestring; d_value: double);
   end;

type
   TxlRegistry = class
   private
      FRootKey: HKey;
      FCurrentKey: HKey;
      FLazyWrite: boolean;

      function f_GetBaseKey (): HKey;
   public
      constructor Create (const rootkey: hkey = 0);
      destructor Destroy (); override;

      function OpenKey(const s_Key: WideString; CanCreate: Boolean = true): Boolean;
      procedure CloseKey ();
      function CreateKey (const s_key: Widestring): boolean;   // Does not change CurrentKey
      function DeleteKey (const s_key: widestring): boolean;

      function ReadString (const s_name: widestring): widestring;
      function ReadInteger (const s_name: widestring): integer;
      function ReadBool (const s_name: widestring): boolean;
      function WriteString (const s_name, s_value: widestring): boolean;
      function WriteInteger (const s_name: widestring; i_value: integer): boolean;
      function WriteBool (const s_name: widestring; b_value: boolean): boolean;
      function DeleteValue (const s_name: widestring): boolean;

      property RootKey: HKey read FRootKey write FRootKey;
      property LazyWrite: boolean read FLazyWrite write FLazyWrite;
   end;

procedure SetFileAssociate (s_ext: widestring; b_associate: boolean; iconidx: integer = 0);
function CheckFileAssociate (s_ext: widestring): boolean;

implementation

uses UxlStrUtils;

const c_buffersize = 10000;

constructor TIniCache.Create (const s_inifile: widestring);
begin
	FIniFile := s_inifile;
   FList := TxlStrList.Create;
   FList.KeyDeli := '=';
   GetMem (FBuffer, c_buffersize * 2);
end;

destructor TIniCache.Destroy ();
begin
	FreeMem (FBuffer, c_buffersize * 2);
   FList.free;
   inherited;
end;

procedure TIniCache.Clear ();
begin
	FList.Clear;
end;

procedure TIniCache.ReadSection (const s_section: widestring);
var p: pwidechar;
	s: widestring;
begin
   GetPrivateProfileSectionW(pwidechar(s_section), FBuffer, c_buffersize, pwidechar(FIniFile));
   p := FBuffer;
   Clear;
   while p^ <> #0 do
   begin
   	s := pwidechar (p);
      FList.Add (s);
      inc (p, Length(s) + 1);
   end;
end;

function TIniCache.GetString (const s_key, s_default: widestring): widestring;
begin
	result := FList.ItemsByKey[s_key];
   if result = '' then result := s_default;
end;

function TIniCache.GetInteger (const s_key: widestring; i_default: integer): integer;
begin
	result := StrToIntDef (GetString(s_key, IntToStr(i_default)), i_default);
end;

function TIniCache.GetBool (const s_key: widestring; b_default: boolean): boolean;
begin
	result := StrToBool (GetString(s_key, BoolToStr(b_default)));
end;

function TIniCache.GetFloat (const s_key: widestring; d_default: double): double;
begin
	result := StrToFloatDef (GetString(s_key, FloatToStr(d_default)), d_default);
end;

procedure TIniCache.AddString (const s_key, s_value: widestring);
begin
	FList.AddByKey (s_key, s_value);
end;

procedure TIniCache.AddInteger (const s_key: widestring; i_value: integer);
begin
	AddString (s_key, IntToStr(i_value));
end;

procedure TIniCache.AddBool (const s_key: widestring; b_value: boolean);
begin
	AddString (s_key, BoolToStr(b_value));
end;

procedure TIniCache.AddFloat (const s_key: widestring; d_value: double);
begin
	AddString (s_key, FloatToStr(d_value));
end;

procedure TIniCache.WriteSection (const s_section: widestring);
var i, n: integer;
	p: pwidechar;
   s: widestring;
begin
	p := FBuffer;
	for i := FList.Low to FList.High do
   begin
   	s := FList.Keys[i] + FList.KeyDeli + FList[i];
//      if i = FList.High then
//      	s := s + #13#10;
      n := Length(s)+1;
      copymemory (p, pwidechar(s), n*2);
      inc (p, n);
   end;
   p^ := #0;
	WritePrivateProfileSectionW (pwidechar(s_section), FBuffer, pwidechar(FIniFile));
   Clear;
end;

//-----------------------

constructor TxlIniFile.Create (const s_inifile: widestring);
begin
	Finifile := s_inifile;
end;

destructor TxlIniFile.Destroy ();
begin
	FCache.free;
   inherited;
end;

function TxlIniFile.Cache(): TIniCache;
begin
	if not assigned (FCache) then
   	FCache := TIniCache.Create (FIniFile);
   result := FCache;
end;

function TxlIniFile.ReadString (const s_key, s_default: widestring): widestring;
var p: array [0..1001] of widechar;
	n: integer;
begin
   n := GetPrivateProfileStringW (pwidechar(Fsection), pwidechar(s_key), pwidechar(s_default), p, 500, pwidechar(Finifile));
   p[n] := #0;
   result := p;
end;

function TxlIniFile.ReadInteger (const s_key: widestring; i_default: integer): integer;
begin
	result := GetPrivateProfileIntW (pwidechar(Fsection), pwidechar(s_key), i_default, pwidechar(Finifile));
end;

function TxlInifile.ReadFloat (const s_key: widestring; d_default: double): double;
begin
	result := StrToFloatDef (readstring (s_key, FloatToStr(d_default)), d_default);
end;

function TxlIniFile.ReadBool (const s_key: widestring; b_default: boolean): boolean;
begin
   result := IntToBool (ReadInteger(s_key, booltoint(b_default)));
end;

procedure TxlIniFile.WriteString (const s_key, s_value: widestring);
begin
	WritePrivateProfileStringW (pwidechar(Fsection), pwidechar(s_key), pwidechar(s_value), pwidechar(Finifile));
end;

procedure TxlIniFile.WriteInteger (const s_key: widestring; i_value: integer);
begin
	WriteString (s_key, IntToStr(i_value));
end;

procedure TxlIniFile.WriteBool (const s_key: widestring; b_value: boolean);
begin
	WriteInteger (s_key, BoolToInt(b_value));
end;

procedure TxlInifile.WriteFloat (const s_key: widestring; d_value: double);
begin
	WriteString (s_key, FloatToStr(d_value));
end;

//------------------------

constructor TxlRegistry.Create (const rootkey: hkey = 0);
begin
   if rootkey <> 0 then
   	FRootKey := rootkey
   else
   	FRootKey := HKEY_CURRENT_USER;
   FLazyWrite := true;
   FCurrentKey := 0;
end;

destructor TxlRegistry.Destroy ();
begin
	CloseKey;
   inherited;
end;

function TxlRegistry.OpenKey(const s_key: WideString; CanCreate: Boolean = true): Boolean;
var i: integer;
begin
	CloseKey;
   if CanCreate then
   	i := RegCreateKeyExW (f_GetBaseKey, PwideChar(S_key), 0, nil, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, FCurrentKey, nil)
   else
		i := RegOpenKeyExW (f_GetBaseKey, pwidechar(s_key), 0, KEY_ALL_ACCESS, FCurrentKey);
   result := i = ERROR_SUCCESS;
end;

procedure TxlRegistry.CloseKey ();
begin
	if FCurrentKey = 0 then exit;
	if FLazyWrite then
		RegCloseKey (FCurrentKey)
   else
   	RegFlushKey (FCurrentKey);
   FCurrentKey := 0;
end;

function TxlRegistry.CreateKey (const s_key: Widestring): boolean;
var TempKey: HKey;
begin
	CloseKey;
   result := RegCreateKeyExW (f_GetBaseKey, PwideChar(S_key), 0, nil, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, TempKey, nil) = ERROR_SUCCESS;
   if result then RegCloseKey (TempKey);
end;

function TxlRegistry.DeleteKey (const s_key: widestring): boolean;
begin
   result := RegDeleteKeyW (f_GetBaseKey, pwidechar(s_key)) = ERROR_SUCCESS;
end;

function TxlRegistry.ReadString (const s_name: widestring): widestring;
var p: pointer;
	n: integer;
begin
	result := '';
	if FCurrentKey = 0 then exit;
   if RegQueryValueExW (FCurrentKey, pwidechar(s_name), nil, nil, nil, @n) = ERROR_SUCCESS then
   begin
      GetMem (p, n);
      if RegQueryValueExW (FCurrentKey, pwidechar(s_name), nil, nil, p, @n) = ERROR_SUCCESS then
         result := pwidechar(p);
      FreeMem (p, n);
   end;
end;

function TxlRegistry.ReadInteger (const s_name: widestring): integer;
var n: integer;
begin
	result := 0;
   n := sizeof(result);
	if FCurrentKey <> 0 then
   	RegQueryValueExW (FCurrentKey, pwidechar(s_name), nil, nil, @result, @n);
end;

function TxlRegistry.ReadBool (const s_name: widestring): boolean;
begin
   result := IntToBool (ReadInteger(s_name));
end;

function TxlRegistry.WriteString (const s_name, s_value: widestring): boolean;
begin
	if FCurrentKey = 0 then
   	result := false
   else
		result := RegSetValueExW (FCurrentKey, pwidechar(s_name), 0, REG_SZ, pwidechar(s_value), length(s_value) * 2) = ERROR_SUCCESS;
end;

function TxlRegistry.WriteInteger (const s_name: widestring; i_value: integer): boolean;
begin
	if FCurrentKey = 0 then
   	result := false
   else
		result := RegSetValueExW (FCurrentKey, pwidechar(s_name), 0, REG_DWORD, @i_value, sizeof(i_value)) = ERROR_SUCCESS;
end;

function TxlRegistry.WriteBool (const s_name: widestring; b_value: boolean): boolean;
begin
	result := WriteInteger (s_name, Ord(b_value));
end;

function TxlRegistry.DeleteValue (const s_name: widestring): boolean;
begin
	if FCurrentKey = 0 then
   	result := false
   else
		result := RegDeleteValueW (FCurrentKey, pwidechar(s_name)) = ERROR_SUCCESS;
end;

function TxlRegistry.f_GetBaseKey (): HKey;
begin
	if FCurrentKey = 0 then
   	result := FRootKey
   else
   	result := FCurrentKey;
end;

//--------------------

procedure SHChangeNotify(wEventId: Longint; uFlags: UINT; dwItem1, dwItem2: Pointer); stdcall; external 'Shell32.dll' name 'SHChangeNotify';
const SHCNE_ASSOCCHANGED = $08000000;
	SHCNF_FLUSHNOWAIT = $2000;

procedure SetFileAssociate (s_ext: widestring; b_associate: boolean; iconidx: integer = 0);
var s_key: widestring;
   s_filetype: widestring;
begin
   if s_ext[1] <> '.' then
   begin
      s_filetype := s_ext;
   	s_ext := '.' + s_ext;
   end
   else
   	s_filetype := MidStr (s_ext, 2);
   with TxlRegistry.Create (HKey_Classes_Root) do
   begin
   	if b_associate then
      begin
         OpenKey (s_ext, true);
         s_key := ProgName + s_ext;
         WriteString ('', s_key);
         CloseKey;

         OpenKey (s_key, true);
         WriteString ('', ProgName + ' ' + UpperCase(s_filetype) + ' File');
         CloseKey;
         
         OpenKey (s_key + '\DefaultIcon', true);
         WriteString ('', ProgExe + ',' + IntToStr(iconidx));
         CloseKey;

         OpenKey (s_key + '\shell\open\command', true);
         WriteString ('', '"' + ProgExe + '" "%1"');
         CloseKey;

         SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_FLUSHNOWAIT,nil,nil); // 立即刷新关联图标
      end
      else
      begin
      	OpenKey (s_ext, true);
         WriteString ('', '');
         CloseKey;
      end;
      Free;
   end;
end;

function CheckFileAssociate (s_ext: widestring): boolean;
var o_reg: TxlRegistry;
	s_key, s_value: widestring;
begin
	result := false;
	o_reg := TxlRegistry.Create (HKey_Classes_Root);

   if s_ext[1] <> '.' then s_ext := '.' + s_ext;
   if not o_reg.OpenKey (s_ext, false) then exit;
   s_key := o_reg.ReadString ('');
   o_reg.CloseKey;
   if s_key = '' then exit;
   
   if not o_reg.OpenKey (s_key + '\shell\open\command', false) then exit;
   s_value := o_reg.ReadString ('');
   result := FirstPos (lowerCase(progExe), lowerCase(s_value)) > 0;
   
   o_reg.CloseKey;
   o_reg.Free;
end;

end.
 