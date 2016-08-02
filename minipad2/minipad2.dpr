program minipad2;

{$R 'MINIPAD2.res' 'MINIPAD2.RC'}
{$R '..\XCL\XCLRes.res' '..\XCL\XCLRes.Rc'}
{$R 'DIALOGS.res' 'DIALOGS.RC'}
{$R 'Options.res' 'Options.RC'}
{$R 'plugins.res' 'plugins.rc'}
{$R 'strings.res' 'strings.rc'}

uses
  Windows,
  UxlWindow,
  UxlCommDlgs,
  Resource in 'RESOURCE.PAS',
  UGlobalObj in 'UGlobalObj.pas',
  UMainForm in 'UMainForm.pas',
  UxlStrUtils,
  UxlFunctions,
  UxlFile,
  UxlIniFile,
  UMenuManager in 'UMenuManager.pas',
  UDialogs in 'UDialogs.pas',
  USpecialMode in 'USpecialMode.pas',
  UTypeDef in 'UTypeDef.pas',
  ULangManager in 'ULangManager.pas',
  UVersionManager;

var WndStrc: TWindowStruct;

begin
	if not CheckSingleton then exit;
   if not CheckVersion then exit;
   if not LogInHandler.ValidateLogIn then exit;

   with WndStrc do
   begin
      Caption := 'minipad2';
      Icon := MainIcon;
      x := 100;
      y := 100;
      clwidth := 440;
      clheight := 340;
      MinBox := true;
      MaxBox := true;
      Sizable := true;
   end;
   with TMainForm.create (WndStrc) do
      Run();
end.



