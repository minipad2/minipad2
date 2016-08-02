unit UEditBox;

interface

uses UxlDialog, UxlStdCtrls, UPageSuper, UTypeDef;

type
	TEditBoxSuper = class (TxlDialog)
   private
      FItemID: integer;
      FInsertBeforeId: integer;
      FParentPage: TPageSuper;

      procedure f_LoadRec ();
		procedure f_OnCbIconRightClicked (Sender: TObject);
   protected
      FCbIcon: TxlButton;
      FIconFile: widestring;
      procedure f_DetermineIcon ();
      function f_DefaultIcon (): integer; virtual;
      function AllowUserDefinedIcon (): boolean; virtual;

      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;

      procedure LoadItem (value: TPageSuper); virtual;
      procedure ClearAndNew (); virtual;
      function SaveItem (value: TPageSuper): integer; virtual;
   public
      class function PageType (): TPageType; virtual; abstract;
      property InsertBeforeId: integer write FInsertBeforeId;
      property ItemID: integer read FItemID write FItemID;
      property ParentPage: TPageSuper write FParentPage;     // 对象指针传入
   end;
   TEditBoxClass = class of TEditBoxSuper;

   TEditBoxFactory = class
   private
   	FClasses: array of TEditBoxClass;
   public
		procedure RegisterClass (o_class: TEditBoxClass);
		function NewEditBox (pt: TPageType): TEditBoxSuper;
   end;

function EditBoxFactory(): TEditBoxFactory;
function AddFiles (var s_text: widestring): boolean;

implementation

uses Windows, UxlList, UxlStrUtils, UxlCommDlgs, UxlFunctions, ULangManager, UPageStore, UPageFactory, Resource;

var FEditBoxFactory: TEditBoxFactory;

function EditBoxFactory (): TEditBoxFactory;
begin
	if not assigned (FEditBoxFactory) then
   	FEditBoxFactory := TEditBoxFactory.Create;
   result := FEditBoxFactory;
end;

procedure TEditBoxFactory.RegisterClass (o_class: TEditBoxClass);
var n: integer;
begin
	n := Length (FClasses);
   SetLength (FClasses, n + 1);
   FClasses[n] := o_class;
end;

function TEditBoxFactory.NewEditBox (pt: TPageType): TEditBoxSuper;
var i: integer;
begin
	result := nil;
	for i := Low (FClasses) to High (FClasses) do
   	if FClasses[i].PageType = pt then
      begin
      	result := FClasses[i].Create;
         break;
      end;
end;

//----------------

procedure TEditBoxSuper.OnOpen ();
begin
	if AllowUserDefinedIcon then
   begin
      FIconFile := '';
      FCbIcon := TxlButton.Create (self, ItemHandle[cb_icon]);
      FCbIcon.Flat := true;
      FCbIcon.OnRightClick := f_OnCbIconRightClicked;
   end;

   f_LoadRec ();
end;

procedure TEditBoxSuper.OnClose ();
begin
	FCbIcon.free;
	inherited;
end;

function TEditBoxSuper.AllowUserDefinedIcon (): boolean;
begin
	result := false;
end;

procedure TEditBoxSuper.f_DetermineIcon ();
begin
	if (FCbIcon = nil) then exit;
	if FIconFile <> '' then
   	FCbIcon.SetIcon (FIconFile)
	else
   	FCbIcon.SetIcon (f_DefaultIcon);
end;

function TEditBoxSuper.f_DefaultIcon (): integer;
begin
	result := 0;
end;

procedure TEditBoxSuper.f_OnCbIconRightClicked (Sender: TObject);
begin
	FIconFile := '';
   f_DetermineIcon;
end;

procedure TEditBoxSuper.f_LoadRec ();
var i: integer;
	o_list: TxlIntList;
begin
   o_list := TxlIntList.Create;
   FParentPage.GetchildList (o_list);
   if FItemid > 0 then
   begin
   	LoadItem (PageStore[FItemId]);
   	i := o_list.Find (FItemid);
   end
   else
   begin
   	ClearAndNew;
   	i := o_list.Find (FInsertBeforeId);
   end;
   ItemEnabled[cb_previous] := (i > o_list.Low);
   ItemEnabled[cb_next] := (i >= o_list.Low) and (i < o_list.High);
   o_list.free;
end;

procedure TEditBoxSuper.LoadItem (value: TPageSuper);
begin
	FIconFile := RelToFullPath (value.Icon, ProgDir);
   f_DetermineIcon;
end;

procedure TEditBoxSuper.ClearAndNew ();
begin
	FIconFile := '';
   f_DetermineIcon;
end;

function TEditBoxSuper.SaveItem (value: TPageSuper): integer;
begin
	value.Icon := FullToRelPath (FIconFile, ProgDir);
end;

procedure TEditBoxSuper.OnCommand (ctrlID: integer);
  procedure f_SaveRec ();
   begin
   	if FItemId < 0 then
      begin
      	FItemId := PageStore.NewPage (FparentPage.id, self.PageType);
         FparentPage.Childs.AddChild (FItemid, FInsertBeforeId);
      end;
      SaveItem (PageStore[FItemId]);
      PageCenter.EventNotify (pctFieldModified, FItemId);
   end;

   procedure f_Go (i_dir: integer);
   var i: integer;
   	o_list: TxlIntList;
   begin
   	f_SaveRec;
      i := FparentPage.Childs.FindChild (FItemid) + i_dir;
      o_list := TxlIntList.Create;
      FParentPage.GetChildList (o_list);
      if o_list.PosValid (i) then  // 装载下一记录
      begin
         FItemID := o_list[i];
         f_LoadRec ();
      end;
      o_list.free;
   end;

begin
   case CtrlID of
      cb_previous: f_Go (-1);
      cb_next: f_Go (1);
      cb_new:
      	begin
            f_SaveRec;
            FInsertBeforeId := -1;
            FitemID := -1;
            f_LoadRec ();
         end;
      cb_icon:
      	with TxlOpenDialog.create do
         begin
            Title := LangMan.GetItem (sr_SelectIcon);
            Filter := LangMan.GetItem (sr_IconFilter);
            FilterIndex := 1;
            FileName := FIconFile;
            MultiSelect := false;
            if Execute then
            begin
               FIconFile := Path + FileName;
               f_DetermineIcon;
            end;
            free;
         end;
      IDOK:
         begin
            f_SaveRec;
            close (true);
         end;
      IDCANCEL:
         close (false);
   end;
end;

//------------------

function AddFiles (var s_text: widestring): boolean;
var s_files, s_path: widestring;
   o_list: TxlStrList;
   i: integer;
begin
   result := false;
   with TxlOpenDialog.Create do
   begin
      Title := LangMan.GetItem (sr_selectfile);
      Filter := LangMan.GetItem (sr_filefilter);
      FilterIndex := 1;
      MultiSelect := true;
      if Execute then
      begin
         s_path := Path;
         s_files := FileName;
         result := true;
      end;
      free;
   end;
   if result then
   begin
      o_list := TxlStrList.Create;
      o_list.text := Trim(s_text);
      with TxlStrList.Create () do
      begin
      	Separator := #9;
         Text := s_files;
         for i := Low to High do
            o_list.Add (s_path + Items[i]);
         Free;
      end;
      s_text := o_list.Text;
      o_list.free;
   end;
end;

//-----------------

initialization

finalization
	FEditBoxFactory.free;

end.
