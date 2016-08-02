unit U23to30;

interface

implementation

procedure UpgradeFrom23to30 ();
var o_oldfile, o_idxfile, o_datafile: TxlFile;
	s_oldfile, s_line, s_index, s_name: widestring;
   m, i_id, i_sortno, i_status: integer;
   i_pagetype: integer;
   o_text: TxlStrList;
const c_sep = '!@#$%^';
	procedure f_WritePage ();
   begin
      o_idxfile.writeln (s_index + #9 + IntToStr(o_text.TextCount));
      o_datafile.writetext (o_text.Text);
   end;
begin
	result := true;
   DeleteFile (ProgIni);

   s_oldfile := ProgDir + 'minipad2.txt';
   if not pathfileexists (s_oldfile) then exit;

	o_oldfile := TxlFile.create (s_oldfile, fmReadText, enAnsi);
   o_idxfile := TxlFile.create (ProgDir + 'data\minipad2.idx', fmWriteText, enUTF16LE);
   o_datafile := TxlFile.create (ProgDir + 'data\minipad2.dat', fmWriteText, enUTF16LE);
   o_text := TxlStrList.Create;

   s_index := '';
   i_id := 0;
   while not o_oldfile.eof() do
   begin
   	o_oldfile.readln (s_line);
      if firstpos(c_sep, s_line) = 1 then    // 索引行
      begin
         if s_index <> '' then
            f_WritePage;

         inc (i_id);
         i_sortno := i_id;
         m := length(c_sep) + 1;
         i_status := StrToIntDef(s_line[m+1]);
         s_name := midstr(s_line, m + 2);
         s_name := leftstr (s_name, firstpos(c_sep, s_name) - 1);    // 去除导出文件名部分
         i_pagetype := StrToIntDef(s_line[m]);
         case i_pagetype of
            0, 2: i_pagetype := Ord(ptNote);   // 备忘录转为普通页
            1: i_pagetype := Ord(ptCalc);  // 计算页仍为计算页
            3: i_pagetype := Ord(ptDict);  // 词典页仍为词典页
         end;
         s_index := IntToStr(i_id) + #9 + '0' + #9 + IntToStr(i_sortno) + #9 + IntToStr(i_pagetype) + #9 + s_name + #9 + IntToStr(i_status);

         o_text.Clear;
      end
      else
			o_text.Add (s_line);
   end;
	f_WritePage;

   o_oldfile.free;
   o_idxfile.free;
   o_datafile.free;
   o_text.free;
end;

end.
