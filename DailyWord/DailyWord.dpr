program DailyWord;

{$R 'DailyWord.res' 'DailyWord.RC'}
{$R 'DIALOGS.res' 'DIALOGS.RC'}

uses
  UxlWindow,
  UMainForm in 'UMainForm.pas',
  Resource in 'RESOURCE.PAS',
  UDialogs in 'UDialogs.pas',
  UWordBox in 'UWordBox.pas',
  UGlobalObj in 'UGlobalObj.pas',
  UQueryManager in 'UQueryManager.pas',
  UDisplayManager in 'UDisplayManager.pas',
  UDisplay in 'UDisplay.pas';

var WndStrc: TWindowStruct;

begin
	if not CheckSingleton then exit;
   
   with WndStrc do
   begin
      Caption := 'DailyWord';
      Icon := MainIcon;
      x := 0;
      y := 0;
      clwidth := 0;
      clheight := 0;
      MinBox := false;
      MaxBox := false;
      Sizable := false;
   end;
   with TMainForm.create (WndStrc) do
   	Run();
end.
