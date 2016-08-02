unit UVersionManager;

interface

function CheckVersion (): boolean;  // 未来多数据库版本则应为：(const s_idxfile, s_datafile: widestring);
procedure BackupAndDeleteFile (s_file: widestring);

implementation

uses SysUtils, UxlIniFile, UxlFunctions, UxlStrUtils, UxlList, UxlCommDlgs, UxlFile, UGlobalObj, U30to31, U31to32;

function OldDataDir (): widestring;
begin
	result := ProgDir + 'olddata\';
end;

procedure BackupAndDeleteFile (s_file: widestring);
begin
   CopyFile (DataDir + s_file, OldDataDir + s_file, false);
   DeleteFile (DataDir + s_file);
end;

function CheckVersion (): boolean;
var s_version: widestring;
   o_file: TxlTextFile;
   s_idxfile, s_datafile: widestring;
begin
	try
      s_idxfile := DataDir + 'minipad2.idx';
      s_datafile := DataDir + 'minipad2.dat';
		s_version := Version;
      o_file := TxlTextFile.Create (s_idxfile, fmRead, enUTF16LE);
      if not o_file.eof then
         o_file.ReadLn (s_version);
      o_file.free;

      if s_version <> Version then
      begin
         // 旧数据做备份
         if not PathFileExists (OldDataDir) then
         	CreateDir (OldDataDir);
//         CopyFiles (DataDir, OldDataDir, false);
         CopyFile (s_idxfile, replacestr(s_idxfile, DataDir, OldDataDir), false);   // 考虑到未来的多数据库
         CopyFile (s_datafile, replacestr(s_datafile, DataDir, OldDataDir), false);

         UpdateFrom30to31 (s_idxfile, s_datafile);
         UpdateFrom31to32a (s_idxfile, s_datafile);
         UpdateFrom32ato320 (s_idxfile, s_datafile);
      end;
      result := true;
   except
   	on E: Exception do
      begin
      	ShowMessage (E.Message);
	      result := false;
      end;
   end;
end;

//------------------


//type
//	TUpdateSuper = class      // 装饰者模式
//   private
//   	FInner: TUpdateSuper;
//   protected
//      procedure DoUpdate (const s_idxfile, s_datafile: widestring); virtual; abstract;
//   public
//   	constructor Create (AInner: TUpdateSuper);
//      procedure Update (const s_idxfile, s_datafile: widestring);
//   end;

//         // 以装饰者模式进行数据升级
//         o_30to31 := T30to31.Create (nil);
//         o_31to32 := T31to32.Create (o_30to31);
//         o_31to32.Update (s_idxfile, s_datafile);
//         o_30to31.free;
//         o_31to32.free;
//
//constructor TUpdateSuper.Create (AInner: TUpdateSuper);
//begin
//	FInner := AInner;
//end;
//
//procedure TUpdateSuper.Update (const s_idxfile, s_datafile: widestring);
//begin
//   if FInner <> nil then
//   	FInner.Update (s_idxfile, s_datafile);
//   DoUpdate (s_idxfile, s_datafile);
//end;

//uses UVersionManager;

//type
//	T30to31 = class (TUpdateSuper)
//   protected
//      procedure DoUpdate (const s_idxfile, s_datafile: widestring); override;
//	end;


end.

