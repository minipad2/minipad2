unit UxlFile;

interface

uses Windows, SysUtils, UxlClasses, UxlFunctions, UxlStrUtils;

type TFileMode = (fmRead, fmWrite);
	TEncode = (enUnknown, enAnsi, enUTF8, enUTF8_BOM, enUTF16LE, enUTF16BE);

type
   TxlFile = class
   private
      function f_GetPosition (): cardinal;
      procedure f_SetPosition (value: cardinal);
   protected
      FFileName: widestring;
      Fhandle: HFILE;
   public
      constructor create (const s_file: Widestring; fmMode: TFileMode);
      destructor destroy (); override;
      procedure Reset (fmMode: TFileMode); 

      function Read (buffer: pointer; i_count: cardinal): cardinal;    // i_count 为字节数；返回实际读取的字节数
      function Write (buffer: pointer; i_count: cardinal): cardinal;    // i_count 为字节数；返回实际写入的字节数
      procedure Move (i_offset: integer);

      function EOF(): boolean; virtual;
      function Size(): cardinal;
      property Position: cardinal read f_GetPosition write f_SetPosition;
   end;

   TxlTextFile = class (TxlFile)
   private
      FEncode: TEncode;
      FEncrypted: boolean;
      FHeadOffset: integer;   // 除去 encrypt 与 BOM 部分
      FPBuffer: pointer;
      FBufferSize: integer;

      function f_IsUnicode (): boolean;
		function f_DeterminEncode (): TEncode;
		function f_ReadText (i_bytecount: integer): widestring;
   private
      procedure SetCharIndex (value: integer);
      function GetCharIndex (): integer;
   protected
		procedure Initialize (fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean); virtual;
   public
      constructor create (const s_file: Widestring; fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean = false);
      destructor Destroy (); override;
      function TextCount (): cardinal;  // 如编码为 Unicode，则返回字符数；否则返回字节数

      procedure Reset (fmMode: TFileMode; enEncode: TEncode); overload;
      procedure Reset (fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean); overload;

      procedure ReadLn (var s_line: widestring); virtual;  // 如果仅读取一行，使用此函数比使用 TxlMemoryTextFile 效率高。
      procedure WriteLn (const s_line: widestring = '');
      procedure ReadText (var s_text: widestring; i_count: integer = -1);  // 如编码为 Unicode，则 i_count 为字符数；否则为字节数
      procedure WriteText (const s_text: widestring);

      property Encode: TEncode read FEncode;
      property Encrypted: boolean read FEncrypted;
      property CharIndex: integer read GetCharIndex write SetCharIndex;
	end;

   TxlMemoryTextFile = class (TxlTextFile)   // 提高性能。对于整个文件的读取与逐行解析，比逐行 TxlTextFile.ReadLn 效率高出甚多。
   private
   	FCharIndex: integer;
      FText: widestring;
   protected
		procedure Initialize (fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean); override;
   public
      function EOF(): boolean; override;
      procedure ReadLn (var s_line: widestring); override;
   end;

function FilterFileName (const s_file: widestring; b_noslash: boolean = false): widestring;

implementation

constructor TxlFile.create (const s_file: Widestring; fmMode: TFileMode);
var s_path: widestring;
begin
	FFileName := s_file;
   s_path := ExtractFilePath (s_file);
   if not PathFileExists (s_path) then CreateDir (s_path);
	if fmMode = fmRead then
   	Fhandle := CreateFileW (pWideChar(s_file), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0)
   else
   	Fhandle := CreateFileW (PWideChar(s_file), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if Fhandle = INVALID_HANDLE_VALUE then
   	raise Exception.create ('File Open Error!');
end;

destructor TxlFile.destroy ();
begin
   CloseHandle (Fhandle);
   inherited;
end;

procedure TxlFile.Reset(fmMode: TFileMode);
begin
	CloseHandle (Fhandle);
   Create (FFileName, fmMode);
end;

function TxlFile.Read (buffer: pointer; i_count: cardinal): cardinal;
begin
	if not ReadFile (Fhandle, buffer^, i_count, result, nil) then
      raise Exception.create ('File Read Error!');
end;

function TxlFile.Write (buffer: pointer; i_count: cardinal): cardinal;
begin
	if not WriteFile (Fhandle, buffer^, i_count, result, nil) then
      raise Exception.create ('File Write Error!');
end;

//-------------------

function TxlFile.EOF (): boolean;
begin
   result := Position >= Size;
end;

function TxlFile.Size (): cardinal;
begin
	result := GetFileSize (Fhandle, nil);
   if result = INVALID_FILE_SIZE then
   	raise Exception.create ('Cannot Get File Size!');
end;

procedure TxlFile.Move (i_offset: integer);
begin
   SetFilePointer (Fhandle, i_offset, nil, FILE_CURRENT);
end;

procedure TxlFile.f_SetPosition (value: cardinal);
begin
	Move (value - Position);
end;

function TxlFile.f_GetPosition (): cardinal;
begin
	result := SetFilePointer (Fhandle, 0, nil, FILE_CURRENT);
end;

//------------------

constructor TxlTextFile.create (const s_file: Widestring; fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean = false);
begin
	inherited Create (s_file, fmMode);
   FBufferSize := 0;
   FPBuffer := nil;
	Initialize (fmMode, enEncode, b_encrypt);
end;

destructor TxlTextFile.Destroy ();
begin
	if FPBuffer <> nil then
	   FreeMem (FPBuffer, FBufferSize);
   inherited;
end;

procedure TxlTextFile.Reset (fmMode: TFileMode; enEncode: TEncode);
begin
	Reset (fmMode, enEncode, FEncrypted);
end;

procedure TxlTextFile.Reset (fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean);
begin
   inherited Reset (fmMode);
   Initialize (fmMode, enEncode, b_encrypt);
end;

procedure TxlTextFile.Initialize (fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean);
   procedure ReadBOM (enEncode: TEncode);     // skip the BOM part
   begin
      case enEncode of
         enUTF16LE, enUTF16BE:
         	if Size - Position >= 2 then
           		Move (2);
         enUTF8_BOM:
         	if Size - Position >= 3 then
           		Move (3);
      end;
   end;

   procedure WriteBOM (enEncode: TEncode);
   var w: word;
      bt: byte;
   begin
      if FEncode in [enUTF16LE, enUTF16BE, enUTF8_BOM] then    // write BOM
      begin
         case FEncode of
            enUTF16LE: w := $FEFF;
            enUTF16BE: w := $FFFE;
            enUTF8_BOM: w := $BBEF;
         end;
         self.Write(@w, 2);
         if FEncode = enUTF8_BOM then
         begin
            bt := $BF;
            self.Write(@bt, 1);
         end;
      end;
   end;

var bt: byte;
begin
   // 加密部分
   if (fmMode = fmRead) and (Size >= 1) then
   begin
      Read (@bt, 1);
      b_encrypt := bt = $FD;
      if not b_encrypt then
         Position := 0;
   end
   else if b_encrypt and (fmMode = fmWrite) then
   begin
      bt := $FD;
      Write (@bt, 1);
   end;
   FEncrypted := b_encrypt;

   // BOM 部分
   if enEncode = enUnknown then
   begin
   	if fmMode = fmWrite then
       	enEncode := enUTF16LE
		else
         enEncode := f_DeterminEncode;
   end;
   FEncode := enEncode;
   if fmMode = fmRead then
   	ReadBOM (enEncode)
   else
   	WriteBOM (enEncode);

   FHeadOffset := Position;
end;

function TxlTextFile.f_DeterminEncode (): TEncode;
   function f_IsTextUTF8 (const lpstrInputStream: pansichar; iLen: integer): boolean;  //distinguish between ANSI and UTF8_without_BOM
   var i: integer;
      cOctets: DWORD;  // octets to go in this UTF-8 encoded character
      chr: byte;
      bAllAscii: boolean;
   begin
      result := false;
      bAllAscii := true;
      cOctets := 0;

      for i := 0 to iLen - 1 do
      begin
         chr := byte(lpstrInputStream[i]);
         if ((chr and $80) <> $00) then bAllAscii := false;
         if( cOctets = 0 ) then begin //7 bit ascii after 7 bit ascii is just fine.  Handle start of encoding case.
            if ( chr >= $80 ) then // count of the leading 1 bits is the number of characters encoded
            begin
               repeat
                  chr := chr shl 1;
                  inc (cOctets) ;
               until (chr and $80) = 0;
               dec (cOctets);  // count includes this character
               if ( cOctets = 0 ) then exit;  // must start with 11xxxxxx
            end
         end else begin
            if (chr and $C0) <> $80 then exit;  // non-leading bytes must start as 10xxxxxx
            dec (cOctets);  // processed another octet in encoding
         end;
      end;

      if ( cOctets > 0 ) then exit;   // End of text. Check for consistency. anything left over at the end is an error
      if( bAllAscii ) then exit;   // Not utf-8 if all ascii.  Forces caller to use code pages for conversion

      result := true;
   end;

var w: word;
  bt: byte;
  i_size: integer;
  p: pointer;
begin
   if Size <= 1 then
      result := enAnsi
	else
      try
         result := enUnknown;
         Read (@w, 2);
         if w = $FEFF then
            result := enUTF16LE
         else if w = $FFFE then
            result := enUTF16BE
         else if (w = $BBEF) and (not EOF) then
         begin
           Read (@bt, 1);
           if bt = $BF then
             result := enUTF8_BOM;
         end;

         if result = enUnknown then   //disinguish between Ansi and UTF8_no_BOM
         begin
            i_size := Size;
            GetMem (p, i_size);
            Position := 0;
            Read (p, i_size);
            if f_IsTextUTF8 (p, i_size) then
               result := enUTF8
            else
               result := enAnsi;
            FreeMem (p, i_size);
         end;
      finally
         if result = enUnknown then
            result := enAnsi;
         Position := IfThen (Encrypted, 1, 0);
      end;
end;

//------------------------

function TxlTextFile.f_IsUnicode (): boolean;
begin
	result := FEncode in [enUTF16LE, enUTF16BE];
end;

function TxlTextFile.TextCount (): cardinal;
begin
	result := Size - FHeadOffset;
	if f_IsUnicode then result := result div 2;
end;

procedure TxlTextFile.SetCharIndex (value: integer);
begin
   if f_IsUnicode then
   	Position := FHeadOffset + value * 2
   else
   	Position := FHeadOffset + value;
end;

function TxlTextFile.GetCharIndex (): integer;
begin
	result := Position - FHeadOffset;
	if f_IsUnicode then
   	result := result div 2;
end;

//--------------------------

procedure TxlTextFile.ReadLn (var s_line: widestring);
var bt: byte;
	wd: word;
   b: boolean;
   i_pos1, i_pos2: integer;
begin
	i_pos1 := Position;
   i_pos2 := i_pos1;
   if f_IsUnicode then
      while not EOF do
      begin
         Read (@wd, 2);
         if Fencode = enUTF16BE then
             wd := MakeWord (Hi(wd), Lo(wd));

         inc (i_pos2, 2);
         if (wd = 13) then
            b := true
         else if b and (wd = 10) then
         	break
         else
         	b := false;
      end
	else
   	while not EOF do
      begin
         Read(@bt, 1);
         inc (i_pos2);
         if (bt = 13) then
            b := true
         else if b and (bt = 10) then
         	break
         else
            b := false;
      end;

   Position := i_pos1;
   s_line := f_ReadText (i_pos2 - i_pos1);
   if b then //对于以回车换行结尾的，删去字符串末尾的回车换行符。
   	s_line := LeftStr (s_line, Length(s_line) - 2);
end;

procedure TxlTextFile.WriteLn (const s_line: widestring = '');
begin
	WriteText (s_line + #13#10);
end;

procedure TxlTextFile.ReadText (var s_text: widestring; i_count: integer = -1);
begin
	if f_IsUnicode then i_count := i_count * 2;
	s_text := f_REadTExt (i_count);
end;

function TxlTextFile.f_ReadText (i_bytecount: integer): widestring;
   procedure f_GetWidestringFromBuffer (buffer: pointer; enCode: TEncode; var s_text: widestring);
   var pw: pword;
   begin
      case encode of
         enUTF16LE:
            s_text := pwidechar(buffer);
         enUTF16BE:
            begin
               pw := buffer;
               while pw^ <> 0 do
               begin
                  pw^ := MakeWord (Hi(pw^), Lo(pw^));
                  inc (pw);
               end;
               s_text := pwidechar(buffer);
            end;
         enUTF8, enUTF8_BOM:
            s_text := UTF8Decode (pansichar(buffer));
         else
            s_text := AnsiToUnicode (pansichar(buffer));
      end;
   end;

var p: pbyte;
begin
	if (i_bytecount < 0) or (i_bytecount > size - Position) then
   	i_bytecount := size - Position;
   if FBufferSize < i_bytecount + 2 then
   begin
   	ReAllocMem (FPBuffer, i_bytecount + 2);
      FBufferSize := i_bytecount + 2;
   end;
   Read (FPBuffer, i_bytecount);

   p := FPBuffer;
   inc (p, i_bytecount);
   p^ := 0;
   inc (p);
   p^ := 0;

	f_GetWideStringFromBuffer (FPBuffer, FEncode, result);
end;

procedure TxlTextFile.WriteText (const s_text: widestring);
var s: pansichar;
	w: WORD;
   i, n: integer;
begin
   if s_text = '' then exit;

	case FEncode of
   	enUTF16LE:
        	Write (pwidechar(s_text), length(s_text) * 2);
      enUTF16BE:
      	begin
         	n := length (s_text);
            for i := 1 to n do
            begin
               w := word (s_text[i]);
               w := MakeWord (HiByte(w), LoByte(w));
               Write(@w, 2);
            end;
         end;
      enUTF8, enUTF8_BOM:
      	begin
         	s := pansichar(UTF8Encode (s_text));
            self.Write(s, length(s));
         end;
      else
      	begin
         	s := pansichar(WideCharToString (pwidechar(s_text)));
            self.Write(s, length(s));
         end;
   end;
end;

//---------------------------

procedure TxlMemoryTextFile.Initialize (fmMode: TFileMode; enEncode: TEncode; b_encrypt: boolean);
begin
	inherited Initialize (fmMode, enEncode, b_encrypt);
	if fmMode = fmRead then
   begin
   	ReadText (FText);
   	FCharIndex := 1;
   end;
end;

procedure TxlMemoryTextFile.ReadLn (var s_line: widestring);
var i, j, n: integer;
begin
	i := FCharIndex;
   n := Length (FText);
   for j := i to n do
   	if (FText[j] = #13) and (j < n) and (FText[j+1] = #10) then
      	break;
   if j < n then dec (j);
   s_line := SubStr (FText, i, j);
   FCharIndex := j + 3;
end;

function TxlMemoryTextFile.EOF (): boolean;
begin
	result := FCharIndex > Length(FText);
end;

//--------------------------

function FilterFileName (const s_file: widestring; b_noslash: boolean = false): widestring;
const s_chars: array[0..6] of widestring = ('<', '>', '|', ':', '"', '*', '?');
var i: integer;
begin
   result := s_file;
   for i := Low(s_chars) to High(s_chars) do
      result := ReplaceStr (result, s_chars[i], ' ');
   if b_noslash then
   begin
      result := ReplaceStr (result, '/', ' ');
      result := ReplaceStr (result, '\', ' ');
   end;
end;

end.







