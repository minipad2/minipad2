unit U30to31;

interface

procedure UpdateFrom30To31 (const s_idxfile, s_datafile: widestring);

implementation

uses SysUtils, UxlList, UxlFile, UxlIniFile, UxlFunctions, UGlobalObj, UTypeDef, UVersionManager;

procedure UpdateFrom30To31 (const s_idxfile, s_datafile: widestring);
var s_line, s_text, s, s_version: widestring;
	o_line, o_newindex: TxlStrList;
   o_idxfile: TxlMemoryTextFile;
   i_pid, id, i_groupsortno, i_pagesortno: integer;
begin
   with Txlinifile.create (ProgIni) do
   begin
   	Section := 'Program';
      s_version := readstring ('Version', '');
      if s_version = '3.0a1' then
      	WriteString ('Version', '3.1a');
      free;
   end;
   if s_version <> '3.0a1' then exit;

   o_line := TxlStrList.Create;
   o_line.Separator := #9;
   id := 0;
   i_groupsortno := 0;
   s_text := '';
   o_newindex := TxlStrList.Create;
   o_idxfile := TxlMemoryTextFile.Create (s_idxfile, fmRead, enUTF16LE);
	while not o_idxfile.eof do
   begin
   	o_idxfile.ReadLn (s_line);
      if s_line = '' then continue;
      inc (id);
      o_line.Text := s_line;
      if o_line[0] <> '' then  // group
      begin
      	with TxlTextFile.Create (DataDir + o_line[1], fmRead, enUTF16LE) do
         begin
         	ReadText (s);
            s_text := s_text + s;
            Free;
         end;
         BackupAndDeleteFile (o_line[1]);

      	inc (i_groupsortno);
         i_pid := id;
      	with o_line do
         begin
         	Delete (1);
            InsertFirst (IntToStr(Ord(ptGroup)));  // pagetype
         	InsertFirst (IntToStr(i_groupsortno)); // sortno
            InsertFirst ('0');  // pid
            InsertFirst (IntToStr(id));  // id
            Add (IntToStr(Ord(psNormal)));  // status
            Add ('0');  // length
            Add ('');  // settings;
            Add ('');  // export file
         end;
         i_pagesortno := 0;
      end
      else  // page
      begin
         inc (i_pagesortno);
         with o_line do
         begin
         	Delete (0);
         	InsertFirst (IntToStr(i_pagesortno)); // sortno
            InsertFirst (IntToStr(i_pid));  // pid
            InsertFirst (IntToStr(id));  // id
         end;
      end;
      o_newindex.Add (o_line.text);
   end;
   o_idxfile.Reset ( fmWrite, enUTF16LE);
   o_idxfile.WriteText (o_newindex.Text);
   o_idxfile.Free;
   
   with TxlTextFile.Create (s_datafile, fmWrite, enUTF16LE) do
   begin
   	WriteText (s_Text);
      Free;
   end;
end;

end.


