unit U31to32;

// 3.2 structure: id, ownerid, pagetype, length, name, status, icon, checked, createtime, modifytime, visittime, externalsave, exportfile, remark
// for groups: collist, widthlist, checkboxes, view, fullrowselect, gridlines, childlist

interface

procedure UpdateFrom31to32a (const s_idxfile, s_datafile: widestring);
procedure UpdateFrom32ato320 (const s_idxfile, s_datafile: widestring);

implementation

uses SysUtils, UxlIniFile, UxlFunctions, UTypeDef, UxlList, UPageProperty, UGlobalObj, UGroupPage, UxlDateTimeUtils, UPageSuper, UPageFactory,
	UPageStore, UMemoPage, UContactPage, ULInkPage, UTemplatePage, UVersionManager, UxlStrUtils, UxlFile;

type TIds = record
	id: integer;
   pid: integer;
   sortno: integer;
   pagetype: TPageType;
end;

const GroupRootId = 0;

function f_GetChildList (pid: integer; a_ids: array of TIds): widestring;
var j: integer;
	o_childlist: TxlIntList;
begin
	o_childlist := TxlIntList.Create;
   o_childlist.Separator := ',';
   for j := Low(a_ids) to High(a_ids) do
      if (a_ids[j].pid = pid) and (a_ids[j].id <> pid) then
         o_childlist.AddByIndex (a_ids[j].sortno, a_ids[j].id);
   o_childlist.SortByIndex;
   result := o_childlist.Text;
   o_childlist.free;
end;

procedure f_AddDefItems (o_list4: TxlStrList);    // icon, checked, createtime, modifytime, visittime
var i: integer;
begin
   with o_list4 do
   begin
      Add ('');            // icon
      Add ('0');           // checked
      for i := 0 to 2 do   // createtime, modifytime, visittime
         Add (SystemTimeToString (Now, dtmDateTimeWithSecond));
      Add ('0');    // externalsave
      Add ('');     // exportfile
      Add ('');     // remark
   end;
end;

procedure f_CreateListPage (o_class: TPageClass; i_id, i_pid: integer; const s_name: widestring; o_list2: TxlStrList);
var lp: TListProperty;
begin
   with o_list2 do
   begin
   	Clear;
      Add (IntToStr(i_id));      // id
      Add (IntToStr(i_pid));  // ownerid
      Add (IntToStr(Ord(o_class.PageType)));    // pagetype
      Add ('0');                 // length
      Add (s_name);                      // name
      Add (IntToStr(Ord(psNormal)));    // status
   end;
   f_AddDefItems (o_list2);

   lp := TListProperty.Create (-1);
   o_class.InitialListProperty (lp);
   lp.Save (o_list2);
   lp.free;
end;

//------------------------

function f_ImportExtPages (o_class: TPageClass; const s_file: widestring; var i_id: integer; o_finallist: TxlStrList; b_addremark: boolean): integer;
var i, n: integer;
	o_childlist: TxlIntList;
   o_list, o_list2: TxlStrList;
begin
	result := 0;
	if not pathfileexists (DataDir + s_file) then exit;

   o_list := TxlStrList.Create;
   o_list.LoadFromFile (DataDir + s_file);
	BackupAndDeleteFile (s_file);

	if o_list.Count > 1 then
   begin
      o_list.Delete (0); // 设置行

      inc (i_id);
      o_list2 := TxlStrList.Create;
      o_list2.Separator := #9;
      f_CreateListPage (o_class, i_id, IfThen(o_class.SingleInstance, RootId, GroupRootId), ExtractFileName (s_file, false), o_list2);
      n := o_finallist.Add (o_list2.Text);
      result := i_id;

      o_childlist := TxlIntList.Create;
      o_childlist.Separator := ',';
      for i := o_list.Low to o_list.High do
      begin
         if o_list[i] = '' then continue;
         inc (i_id);
         with o_list2 do
         begin
            Text := o_list[i];    // id, Title, ...
            Items[0] := IntToStr(i_id);  // new id
            Insert (1, IntToStr(result)); // ownerid
            Insert (2, IntToStr(Ord(o_class.DefChildType))); // pagetype
            Insert (3, '0');  // length
            Insert (5, IntToStr(Ord(psNormal)));  // status
            Insert (6, ''); // icon
            Insert (7, '0'); // checked
            if b_addremark then
               Add ('');     // Remark
         end;
         o_finallist.Add (o_list2.Text);
         o_childlist.Add (i_id);
      end;
      o_finallist[n] := o_finallist[n] + #9 + o_childlist.Text;
      o_childlist.free;
      o_list2.free;
   end;
   o_list.free;
end;

//------------------------

procedure UpdateFrom31to32a (const s_idxfile, s_datafile: widestring);
var s_version: widestring;
	o_list, o_list2, o_finallist: TxlStrList;
   i, i_id, id2, i_grouprootline: integer;
   a_ids: array of TIds;
   lp: TListProperty;
   o_class: TPageClass;

   procedure f_addtogrouproot (id: integer);
   begin
      if id > 0 then
      	o_finallist[i_grouprootline] := o_finallist[i_grouprootline] + ',' + IntToStr(id);
   end;
begin
	with Txlinifile.create (ProgIni) do
   begin
   	Section := 'Program';
      s_version := readstring ('Version', '');
      if s_version = '3.1a' then
      begin
      	WriteString ('Version', 'Upgraded');   // 与optionmanager配合，待启动后删除ini，以清除无用的旧键
         Section := 'Option';
         WriteString ('ToolButtons', '');     // 清除旧版的工具栏项目。因菜单项的id已变。
      end;
      free;
   end;
   if s_version <> '3.1a' then exit;
//   	raise Exception.Create ('Database upgrade failed!');     // 尽量不要调用 LangMan 等其他对象

   o_list := TxlStrList.Create;
   o_list.LoadFromFile (s_idxfile);
   o_list2 := TxlStrList.Create;
   o_list2.Separator := #9;
	SetLength (a_ids, o_list.count);

   lp := TListProperty.Create (-1);

   for i := o_list.Low to o_list.High do
   begin
   	o_list2.Text := o_list[i];        // id, pid, sortno, pagetype, name, status, length, settings, exportfile
      if o_list2.count < 4 then continue;

      with a_ids[i] do
      begin
      	id := StrToInt(o_list2[0]);
         pid := StrToInt(o_list2[1]);
         sortno := StrToInt(o_list2[2]);
         pagetype := TPageType(StrToInt(o_list2[3]));
      end;
   end;

   o_finallist := TxlStrList.Create;
   with o_finallist do
   begin
      Add ('3.2a');
      Add ('');

      f_CreateListPage (TRoot, RootId, RootParent, '', o_list2);
      o_list2.Add (IntToStr(GroupRootId));
   	Add (o_list2.Text);

      f_CreateListPage (TGroupRoot, GroupRootId, RootId, '', o_list2);
      o_list2.Add ( f_GetChildList(GroupRootId, a_ids) );
      i_grouprootline := o_finallist.Add (o_list2.Text);
   end;

   for i := o_list.Low to o_list.High do
   begin
   	o_list2.Text := o_list[i];        // id, pid, sortno, pagetype, name, status, length, settings, exportfile
      if o_list2.count < 4 then continue;  // ignore empty line

      with o_list2 do
      begin
      	Swappos (2, 3);				//  id, ownerid, pagetype, sortno, name, status, length, settings, exportfile
         SwapPos (3, 6);				//  id, ownerid, pagetype, length, name, status, sortno, settings, exportfile
         Delete (6, 3);                //  id, ownerid, pagetype, length, name, status
      end;
      f_AddDefItems (o_list2);   // id, ownerid, pagetype, length, name, status, icon, checked, createtime, modifytime, visittime, externalsave, exportfile, remark

      o_class := PageFactory.GetClass (a_ids[i].pagetype);
      o_class.InitialListProperty (lp);
      lp.Save (o_list2);
      if a_ids[i].pagetype = ptGroup then
         o_list2.Add (f_GetChildList(a_ids[i].id, a_ids));
      if o_class.SingleInstance then
      	o_list2[2] := IntToStr(Ord(ptNote));

      o_finallist.Add (o_list2.Text);
   end;

   i_id := GroupRootId;
   for i := Low(a_ids) to High(a_ids) do
   	if a_ids[i].id > i_id then
      	i_id := a_ids[i].id;

// id, ownerid, pagetype, length, name, status, icon, checked, TimeMode, Action, Description, UseSound, SoundFile, AgainTime, Reminded, Remark
   id2 :=  f_ImportExtPages (TMemoPage, 'Memo.txt', i_id, o_finallist, true);
   f_addtogrouproot (id2);

// id, ownerid, pagetype, length, name, status, icon, checked, Sex, Mobile, Email, IM1, IM2, Company, Department, Address, Zipcode, Tel, Fax, Others, Remark
   id2 := f_ImportExtPages (TContactPage, 'Contact.txt', i_id, o_finallist, false);
   f_addtogrouproot (id2);

// id, ownerid, pagetype, length, name, status, icon, checked, LinkType, Link, HotKey, Remark
   id2 := f_ImportExtPages (TLinkPage, 'Link.txt', i_id, o_finallist, false);
   f_addtogrouproot (id2);

// id, ownerid, pagetype, length, name, status, icon, checked, Text, HotKey, Remark
   f_ImportExtPages (TTemplatePage, 'Template.txt', i_id, o_finallist, true);

	o_finallist.SaveToFile (s_idxfile);
	BackupAndDeleteFile ('clipboard.txt');

   o_finallist.free;
   o_list.free;
   o_list2.free;
   lp.free;
end;

//------------------------------------

procedure UpdateFrom32ato320 (const s_idxfile, s_datafile: widestring);
var o_list, o_list2: TxlStrList;
   s_text: widestring;
   o_file: TxlTextFile;
   i, k: integer;
begin
   o_list := TxlStrList.Create;
   o_list2 := TxlStrList.Create;
   o_list2.Separator := #9;

   o_list.LoadFromFile (s_idxfile);
   if (o_list.Count > 0) and (o_list[0] = '3.2a') then
   begin
      for i := 2 to o_list.High do
      begin
         o_list2.Text := o_list[i];
         if o_list2.count < 3 then continue;
         case TPageType(StrToIntDef(o_list2[2])) of
            ptLinkItem, ptTemplateItem: o_list2.Insert (o_list2.High, '');     // 对于 LinkItem，在 Remark 之前插入缩写字符项；
            else
               Continue;
         end;
         o_list[i] := o_list2.Text;
      end;
      o_list[0] := '3.2.0';
      o_list.SaveToFile (s_idxfile);

      // 调整下划线高亮的特征字符
      o_file := TxlTextFile.Create (s_datafile, fmRead, enUTF16LE);
      o_file.ReadText (s_text);

      s_text := ReplaceStr (s_text, #127, #1);
      s_text := ReplaceStr (s_text, #31, #127);
      s_text := ReplaceStr (s_text, #30, #31);
      s_text := ReplaceStr (s_text, #29, #30);
      s_text := ReplaceStr (s_text, #28, #29);
      s_text := ReplaceStr (s_text, #1, #28);

      o_file.Reset (fmWrite, enUTF16LE);
      o_file.WriteText (s_text);
      o_file.free;
   end;
   
   o_list.free;
   o_list2.free;
end;

end.

