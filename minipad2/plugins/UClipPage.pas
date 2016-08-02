unit UClipPage;

interface

uses UPageSuper, UPageProperty, UxlClasses, UxlList, UxlMiscCtrls, UTypeDef, UEditBox, UxlComboBox;

type
	TClipPage = class (TChildItemContainer)
   private
   public
   	class function PageType (): TPageType; override;
      class function DefChildType (): TPageType; override;
      class procedure GetListShowCols (o_list: TxlIntList); override;
		class procedure InitialListProperty (lp: TListProperty); override;
      class function SingleInstance (): boolean; override;

      procedure Delete (index: integer);
      procedure Clear ();
      function IsFixed (index: integer): boolean;
   end;

   TClipItem = class (TPageSuper)
   private
      FFixed: boolean;
   public
   	class function PageType (): TPageType; override;
      function ImageIndex (): integer; override;
      function PageControl (): TPageControl; override;
      procedure Load (o_list: TxlStrList); override;
      procedure Save (o_list: TxlStrList); override;
      property Fixed: boolean read FFixed write FFixed;
   end;

   TClipBox = class (TEditBoxSuper)
   private
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;

      procedure LoadItem (value: TPageSuper); override;
      procedure ClearAndNew (); override;
      function SaveItem (value: TPageSuper): integer; override;
   public
   	class function PageType (): TPageType; override;
   end;

implementation

uses Windows, UxlFunctions, UxlListView, UxlStrUtils, UPageFactory, UPageStore, ULangManager, Resource;

class	function TClipPage.PageType (): TPageType;
begin
	result := ptClip;
end;

class function TClipPage.DefChildType (): TPageType;
begin
	result := ptClipItem;
end;

class function TClipPage.SingleInstance (): boolean;
begin
	result := true;
end;

class procedure TClipPage.InitialListProperty (lp: TListProperty);
const c_cols: array[0..1] of integer = (sr_Title, sr_Abstract);
	c_widths: array[0..1] of integer = (100, 200);
begin
	with lp do
   begin
		ColList.Populate (c_cols);
		WidthList.Populate (c_widths);
      CheckBoxes := false;
		View := lpvReport;
   	FullrowSelect := true;
   	GridLines := false;
   end;
end;

class procedure TClipPage.GetListShowCols (o_list: TxlIntList);
const c_cols: array[0..2] of integer = (sr_Title, sr_CreateTime, sr_Abstract);
begin
	o_list.Populate (c_cols);
end;

procedure TClipPage.Delete (index: integer);
var id: integer;
begin
   id := Childs.ChildId(index);
   Childs.RemoveChildByPos (index);
   PageStore[id].Delete;
end;

procedure TClipPage.Clear ();
var i: integer;
begin
   for i := Childs.Count - 1 downto 0 do
      if not IsFixed(i) then
         Delete (i);
end;

function TClipPage.IsFixed (index: integer): boolean;
begin
   result := TClipItem(PageStore[Childs.ChildId(index)]).Fixed;
end;

//-----------------------

class function TClipItem.PageType (): TPageType;
begin
	result := ptClipItem;
end;

function TClipItem.ImageIndex (): integer;
begin
	result := PageImageList.IndexOf (PageType) + Ord(Fixed);
end;

function TClipItem.PageControl (): TPageControl;
begin
	result := pcNone;
end;

procedure TClipItem.Load (o_list: TxlStrList);
begin
   inherited Load (o_list);
   FFixed := StrToBool(o_list[0]);
end;

procedure TClipItem.Save (o_list: TxlStrList);
begin
   inherited Save (o_list);
   o_list.Add (BoolToStr(FFixed));
end;

//--------------------

procedure TClipBox.OnInitialize ();
begin
	SetTemplate (Clip_Box, m_Clipboard);
end;

const c_ClipBox: array[0..4] of word = (st_title, st_Text, cb_new, IDOK, IDCANCEL);

procedure TClipBox.OnOpen ();
begin
   inherited;
   RefreshItemText (self, c_ClipBox);
end;

class function TClipBox.PageType (): TPageType;
begin
	result := ptClipItem;
end;

procedure TClipBox.LoadItem (value: TPageSuper);
begin
	self.Text := LangMan.GetItem(sr_EditClipItem);

   ItemText[sle_title] := value.name;
   ItemText[mle_text] := value.Text;
   ItemChecked[chk_fixed] := TClipItem(value).Fixed;

   FocusControl (mle_text);
end;

procedure TClipBox.ClearAndNew ();
begin
	self.Text := LangMan.GetItem(sr_NewClipItem);

   ItemText[sle_title] := '';
   ItemText[mle_text] := '';
   ItemChecked[chk_fixed] := false;

   FocusControl (sle_title);
end;

function TClipBox.SaveItem (value: TPageSuper): integer;
begin
   value.name := ItemText[sle_title];
   if ItemText[mle_text] <> '' then
      value.Text := itemText[mle_text]
   else
      value.Text := ItemText[sle_title];
   TClipItem(value).Fixed := ItemChecked[chk_fixed];
end;

//-----------------------

initialization
	PageFactory.RegisterClass (TClipPage);
   PageImageList.AddImageWithOverlay (ptClip, m_Clipboard);

	PageFactory.RegisterClass (TClipItem);
   PageImageList.AddImages (ptClipItem, [m_insertClipText, ic_fixedclipitem]);
   EditBoxFactory.RegisterClass (TClipBox);

finalization

end.
