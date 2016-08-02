unit UDialogs;

interface

uses Windows, UxlDialog, UxlRichEdit, Resource, UxlFunctions, UxlListView, UxlList, UxlWinControl, UxlStrUtils,
	CommCtrl, UxlStdCtrls, UxlClasses, UGlobalObj, UxlMiscCtrls, UxlCommDlgs, UCommPanels, UTypeDef, UxlFile, UxlComboBox;

type
   TAboutBox = class (TxlDialog)
   private
      Fstprogram: TxlStaticText;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   end;

	TLogInBox = class (TxlDialog)
   private
   	FPassword: widestring;
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
   	procedure OnCommand (ctrlID: integer); override;
   public
   	property Password: widestring write FPassword;
   end;

   TInsertLinkBox = class (TxlDialog)
   private
   	FLinkText: widestring;
      FCmbLinkType: TxlComboBox;
		procedure f_OnLinkTypeChange (Sender: TObject);
		function f_LinkType (): TLinkType;
   protected
   	procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   	procedure OnCommand (ctrlID: integer); override;
   public
   	property LinkText: widestring read FLinkText;
   end;

implementation

uses UxlMenu, ULangManager, UOptionManager;

const c_aboutbox: array[0..3] of word = (st_translator, IDOK, st_freeware, st_pleasedonate);

procedure TAboutBox.OnInitialize ();
begin
	SetTemplate (About_Box, MainIcon);
end;

procedure TAboutBox.OnOpen ();
begin
	inherited;
	if not IsSameStr (LangMan.Language, 'Chinese') then
   begin
//      ItemText[st_freeware] := 'This program is a freeware!';
      ItemText[st_copyright] := 'Copyright:';
      ItemText[st_forum] := 'Forum:';
      ItemText[st_homepage] := 'Homepage:';
      ItemText[st_forumlink] := 'http://www.nebulasoft.cn/forum';
      ItemText[st_authorname] := 'Xu Liang';
      ItemText[st_author] := 'Author:';
      ItemText[st_donate] := 'Donate';
      ItemText[st_releasedate] := 'Release Date:';
//      ItemText[st_pleasedonate] := 'If you wish to support, please click:';
   end;
   RefreshItemText (self, c_aboutbox, About_Box);

	Fstprogram := TxlStaticText.create (self, ItemHandle[st_program]);
	with Fstprogram.Font do
   begin
   	name := 'Arial';
      Color := rgb(0,0,0);
      Size := 14;
      Bold := true;
      Italic := true;
      Underline := false;
      StrikeOut := false;
   end;
   Fstprogram.Color := GetSysColor(COLOR_BTNFACE);

   AddLink (st_homepagelink);
   AddLink (st_forumlink);
   AddLink (st_emaillink, 'mailto:' + ItemText[st_emaillink]);
   if IsSameStr (LangMan.Language, 'Chinese') then
   	AddLink (st_donate, 'http://www.nebulasoft.cn/donate.html')
   else
      AddLink (st_donate, 'http://www.nebulasoft.cn/donate_en.html');

   with TxlStaticIcon.create(self, ItemHandle[ProgramIcon]) do
   begin
   	SetIcon (MainIcon);
      Free;
   end;
end;

procedure TAboutBox.OnClose ();
begin
	FstProgram.free;
   inherited;
end;

//--------------------

procedure TLogInBox.OnInitialize ();
begin
	inherited;
	SetTemplate (LogIn_Box, MainIcon);
end;

const c_loginbox: array [0..2] of word = (st_inputpassword, IDOK, IDCANCEL);

procedure TLogInBox.OnOpen ();
begin
	inherited;
	RefreshItemText (self, c_loginbox, LogIn_Box);
   FocusControl (sle_password);
end;

procedure TLogInBox.OnCommand (ctrlID: integer);
begin
	if (ctrlID = IDOK) and (ItemText[sle_password] <> FPassWord) then
   begin
		ShowMessage (LangMan.GetItem (sr_wrongpassword, 'ÃÜÂë²»ÕýÈ·£¡'), mtInformation, LangMan.GetItem(sr_prompt));
      FocusControl (sle_password);
   end
   else
   	inherited OnCommand (ctrlID);
end;

//------------------------------

procedure TInsertLinkBox.OnInitialize ();
begin
	SetTemplate (InsertLink_Box);
end;

const c_insertlinkbox: array [0..3] of word = (st_category, st_link, IDOK, IDCANCEL);

procedure TInsertLinkBox.OnOpen ();
var i: integer;
//const c_linktypes: array[0..5] of integer = (sr_filelink, sr_folderlink, sr_pagelink, sr_emaillink, sr_nodelink, sr_otherlink);
begin
	inherited;
	RefreshItemText (self, c_insertlinkbox, InsertLink_Box);

   FCmbLinkType := TxlComboBox.Create (self, ItemHandle[cmb_linktype]);
   FCmbLinkType.Items.OnSelChange := f_OnLinkTypeChange;
   for i := 0 to 5 do
   	FCmbLinkType.Items.Add (LangMan.GetItem (sr_LinkTypes + i));
   FCmbLinkType.Items.SelIndex := Ord(MemoryMan.InsertLinktype);
   f_OnLinkTypeChange(self);
end;

procedure TInsertLinkBox.OnClose ();
begin
	MemoryMan.InsertLinkType := f_LinkType;
	FCmbLinkType.free;
   inherited;
end;

procedure TInsertLinkBox.OnCommand (ctrlID: integer);
begin
	case ctrlID of
   	IDOK:
        	FLinkText := '<' + GetFullLink(ItemText[sle_linktext], f_LinkType, true, false) + '>';
      cb_browse:
      	if f_LinkType = ltFile then
            with TxlOpenDialog.create do
            begin
               Title := LangMan.GetItem (sr_selectfile);
               Filter := LangMan.GetItem (sr_filefilter);
               FilterIndex := 1;
               FileName := ItemText[sle_linktext];
               MultiSelect := false;
               if Execute then
                  ItemText[sle_linktext] := FulltoRelPath (Path + FileName, ProgDir);
               free;
            end
         else
            with TxlPathDialog.Create do
            begin
               Title := LangMan.GetItem (sr_selectfolder);
               Path := ItemText[sle_linktext];
               if Execute then
                  ItemText[sle_linktext] := FullToRelPath (Path, ProgDir);
               free;
            end;
   end;
   inherited OnCommand (ctrlID);
end;

function TInsertLinkBox.f_LinkType (): TLinkType;
begin
	result := TLinkType(FCmbLinkType.Items.SelIndex);
end;

procedure TInsertLinkBox.f_OnLinkTypeChange (Sender: TObject);
begin
	ItemEnabled[cb_browse] := FCmbLinkType.Items.SelIndex in [0, 1];
end;

end.

