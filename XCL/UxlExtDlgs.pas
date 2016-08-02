unit UxlExtDlgs;

interface

uses UxlWinControl, UxlDialog;

function InputBox (const s_caption, s_prompt: widestring; var s_text: widestring; i_icon: cardinal = 0; const s_ok: widestring = ''; const s_cancel: widestring = ''): boolean;

implementation

uses Windows, UxlConst;

//const
//   Input_Box = 1017;
//      sle_inputtext = 1020;
//      st_prompt = 1021;

type TInputBox = class (TxlDialog)
private
	FCaption, FPrompt, FInputText, FOK, FCancel: widestring;
   FIcon: cardinal;
protected
	procedure OnInitialize (); override;
   procedure OnOpen (); override;
   procedure OnCommand (ctrlID: integer); override;
public
	property Caption: widestring write Fcaption;
   property Prompt: widestring write FPrompt;
   property OK: widestring write FOK;
   property Cancel: widestring write FCancel;
   property Icon: cardinal write FIcon;
   property InputText: widestring read FInputText write FInputText;
end;

procedure TInputBox.OnInitialize ();
begin
	SetTemplate (Input_Box);
   FIcon := 0;
end;

procedure TInputBox.OnOpen ();
begin
	inherited;
   if FIcon > 0 then SetIcon (FIcon);
	self.text := FCaption;
   ItemText[st_prompt] := FPrompt;
	ItemText[sle_inputtext] := FInputText;
   if FOK <> '' then ItemText[IDOK] := FOK;
   if FCancel <> '' then ItemText[IDCANCEL] := FCancel;
   FocusControl (sle_inputtext);
end;

procedure TInputBox.OnCommand (ctrlID: integer);
begin
	case CtrlID of
   	IDOK:
      	begin
            FInputText := ItemText[sle_inputtext];
            close (true);
         end;
      IDCancel:
      	close (false);
   end;
end;

function InputBox (const s_caption, s_prompt: widestring; var s_text: widestring; i_icon: cardinal = 0; const s_ok: widestring = ''; const s_cancel: widestring = ''): boolean;
begin
	with TInputBox.create () do
   begin
   	Caption := s_caption;
      Prompt := s_prompt;
      InputText := s_text;
      OK := s_ok;
      Cancel := s_cancel;
      Icon := i_icon;
      result := Execute ();
      if result then s_text := InputText;
      free;
   end;
end;

end.
