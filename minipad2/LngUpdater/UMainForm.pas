unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    sle_filename: TEdit;
    cb_convert: TButton;
    cb_exit: TButton;
    cb_browse: TButton;
    st_prompt: TLabel;
    procedure cb_exitClick(Sender: TObject);
    procedure cb_browseClick(Sender: TObject);
    procedure cb_convertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UxlCommDlgs, UxlList, UxlStrUtils, UxlFile;

procedure TForm1.cb_exitClick(Sender: TObject);
begin
	close;
end;

procedure TForm1.cb_browseClick(Sender: TObject);
begin
   with TxlOpenDialog.create do
   begin
      Title := '选择语言文件';
      Filter := 'Language Files (*.lng)|*.lng';
      FilterIndex := 1;
      MultiSelect := false;
      if Execute then
      begin
         sle_filename.Text := FileName;
         st_prompt.Caption := '等待转换...';
      end;
      free;
   end
end;

procedure TForm1.cb_convertClick(Sender: TObject);
var o_oldeng, o_otherlng, o_neweng: TxlStrList;
	i, j, k, i_pos: integer;
   s, s_lngfile, s_item, s_value: widestring;
   o_file: TxlTextFile;
begin
	s_lngfile := sle_filename.Text;

	o_neweng := TxlStrList.Create;
   o_file := TxlTextFile.Create ('English.lng', fmRead, enUTF16LE);
   while not o_file.EOF do
   begin
   	o_file.ReadLn (s);
      o_neweng.Add(s);
   end;
   o_file.free;

	o_oldeng := TxlStrList.Create;
   o_oldeng.IndexDeli := '=';
   o_oldeng.LoadFromFile ('English-316.lng');

   o_otherlng := TxlStrList.Create;
   o_otherlng.IndexDeli := '=';
	o_otherlng.LoadFromFile (s_lngfile);

   // 以新的语言文件为纲，每一项目若在旧的语言文件中有对应翻译则抓过来，否则保留英语
   for i := o_neweng.Low to o_neweng.High do
   begin
   	s_item := o_neweng[i];
   	if (s_item = '') or (LeftStr(s_item, 2) = '//') or (not IsSubStr ('=', s_item)) then continue;
      i_pos := FirstPos ('=', s_item);
      s_value := MidStr (s_item, i_pos + 1);

      j := o_oldeng.Find (s_value);
      if j < 0 then continue;
      k := o_otherlng.FindByINdex (o_oldeng.INdexes[j]);
      if k < 0 then continue;
      o_neweng[i] := LeftStr (s_item, i_pos) + o_otherlng[k];
   end;

   o_neweng.SaveToFile (s_lngfile);
	o_neweng.free;
   o_oldeng.free;
   o_otherlng.free;

//   ShowMessage ('已升级一下语言文件：' + #13#10 + s_lngfile);
   st_prompt.Caption := '转换完成';
end;

end.
