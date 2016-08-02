unit UCommPanels;

interface

uses Windows, UxlClasses, UxlList, UxlDialog, UxlStdCtrls, UxlListView, UxlDragControl;

type
   TSelectPanel = class
   private
      FListView: TxlListView;
      FCb_Up, FCb_Down: TxlButton;
      FChk_SelectAll: TxlCheckBox;
      procedure f_OnItemSelEvent (index: integer; sel: boolean);
      procedure f_MoveItem (Sender: TObject);
      procedure f_SelectAll (Sender: TObject);
      procedure f_OnDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
   public
      constructor Create (AOwner: TxlDialogSuper; h_listview, h_button_up, h_button_down, h_checkbox_selall: HWND; const s_coltitle: widestring = '');
      Destructor Destroy (); override;

      procedure Initialize (o_sellist: TxlIntList; o_alllist: TxlStrList);
      procedure Retrieve (o_sellist: TxlIntList; o_alllist: TxlIntList = nil);
   end;

	TFontColorPanel = class
   private
   	FOwner: TxlDialogSuper;
      FDemo: TxlStaticText;
      FCbSetFont, FCbSetColor: TxlButton;
      FOnFontChange: TNotifyEvent;
      FOnColorChange: TNotifyEvent;
		procedure SetFont (value: TxlFont);
      procedure SetColor (value: TColor);
      function GetFont (): TxlFont;
      function GetColor (): TColor;
      procedure f_OnSetFont (Sender: TObject);
      procedure f_OnSetColor (Sender: TObject);
   public
   	constructor Create (AOwner: TxlDialogSuper; h_demo, h_setfont, h_setcolor: HWND);
      Destructor Destroy (); override;

      property Font: TxlFont read GetFont write SetFont;
      property Color: TColor read GetColor write SetColor;
      property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;
      property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
   end;
   
implementation

uses CommCtrl, UxlWinControl, UxlFunctions, UxlCommDlgs;

constructor TSelectPanel.Create (AOwner: TxlDialogSuper; h_listview, h_button_up, h_button_down, h_checkbox_selall: HWND; const s_coltitle: widestring = '');
var lvs: TListViewStyle;
begin
   InitCommonControl (ICC_LISTVIEW_CLASSES);

   with lvs do
   begin
      ViewStyle := lvsReport;
      MultiSelect := false;
      FullRowSelect := false;
      GridLines := false;
      EditLabels := false;
      ShowHeader := true;
      HeaderDragDrop := false;
      CheckBoxes := true;
   end;
   FListView := TxlListView.Create (AOwner, h_listview);
   with FListView do
   begin
      CanDrag := true;
      CanDrop := true;
//		SetWndStyle (LVS_SHOWSELALWAYS, true);
      SetStyle (lvs);
      OnDropEvent := f_OnDropEvent;
   	Items.OnSelect := f_OnItemSelEvent;
      Cols.Add (IfThen(s_coltitle <> '', s_coltitle, '可选项目'), Pos.Width - 20, alLeft);
	end;

   FCb_Up := TxlButton.Create (AOwner, h_button_up);
   FCb_Up.OnClick := f_MoveItem;

   FCb_Down := TxlButton.Create (AOwner, h_button_down);
   FCb_Down.OnClick := f_MoveItem;

   FChk_SelectAll := TxlCheckBox.Create (AOwner, h_checkbox_selall);
   FChk_SelectAll.OnClick := f_SelectAll;
end;

Destructor TSelectPanel.Destroy ();
begin
//   FListView.free;    // 会引起再次进入对话框时列表拖放无响应。原因不明。
   FCb_Up.free;
   FCb_Down.free;
   FChk_SelectAll.free;
   inherited;
end;

procedure TSelectPanel.Initialize (o_sellist: TxlIntList; o_alllist: TxlStrList);
var o_item: TListViewItem;
   b_allsel: boolean;
   i: integer;
begin
	o_item.image := 0;
   o_item.state := 0;
	FListView.Items.Clear;
   for i := o_sellist.Low to o_sellist.High do
   begin
   	o_item.data := o_sellist[i];
      o_item.text := o_alllist.ItemsByIndex[o_sellist[i]];
      o_item.checked := true;
      FListView.Items.Add (o_item);
      b_allsel := true;
   end;
   for i := o_alllist.Low to o_alllist.High do
   begin
   	if o_sellist.ItemExists(o_alllist.Indexes[i]) then continue;
      o_item.data := o_alllist.Indexes[i];
      o_item.text := o_alllist[i];
      o_item.checked := false;
   	FListView.Items.Add (o_item);
      b_allsel := false;
   end;
   Fchk_selectall.Checked := b_allsel;
end;

procedure TSelectPanel.Retrieve (o_sellist: TxlIntList; o_alllist: TxlIntList = nil);
var i: integer;
begin
   o_sellist.Clear;
   if o_alllist <> nil then o_alllist.Clear;
   with FListView.Items do
      for i := Low to High do
      begin
         if o_alllist <> nil then
         	o_alllist.Add (Items[i].data);
         if Items[i].checked then
            o_sellist.Add (Items[i].data);
      end;
end;

procedure TSelectPanel.f_OnItemSelEvent (index: integer; sel: boolean);
begin
	FCb_up.Enabled := (index > FListView.Items.Low);
   Fcb_down.Enabled := (index < FListView.Items.High);
end;

procedure TSelectPanel.f_MoveItem (Sender: TObject);
var i, j, i_dir: integer;
begin
   if Sender = FCb_Up then
      i_dir := -1
   else
      i_dir := 1;

   with FListView.Items do
   begin
      i := SelIndex;
      j := i + i_dir;
      if Move (i, j) then
         Select (j);
   end;
end;

procedure TSelectPanel.f_SelectAll (Sender: TObject);
begin
   FListView.Items.CheckAll (FChk_SelectAll.Checked);
end;

procedure TSelectPanel.f_OnDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
begin
   with FListView.Items do
   begin
      if Move (SelIndex, hidxTarget) then
         Select (hidxTarget);
   end;
end;

//----------------------

constructor TFontColorPanel.Create (AOwner: TxlDialogSuper; h_demo, h_setfont, h_setcolor: HWND);
begin
	FOwner := AOwner;
   FDemo := TxlStaticText.Create (AOwner, h_demo);
   if h_setfont <> 0 then
   begin
      FCbSetFont := TxlButton.Create (AOwner, h_setfont);
      FCbSetFont.OnClick := f_OnsetFont;
   end;
   if h_setcolor <> 0 then
   begin
      FCbSetColor := TxlButton.Create (AOwner, h_setcolor);
      FCbSetColor.OnClick := f_OnSetColor;
   end;
end;

Destructor TFontColorPanel.Destroy ();
begin
	FDemo.Free;
   FCbSetFont.Free;
   FCbSetColor.Free;
	inherited;
end;

procedure TFontColorPanel.SetFont (value: TxlFont);
begin
   FDemo.Text := value.name + ', ' + IntToStr (value.size);
   FDemo.Font.assign (value);
end;

procedure TFontColorPanel.SetColor (value: TColor);
begin
   FDemo.Color := value;
end;

function TFontColorPanel.GetFont (): TxlFont;
begin
	result := FDemo.Font;
end;

function TFontColorPanel.GetColor (): TColor;
begin
	result := FDemo.Color;
end;

procedure TFontColorPanel.f_OnSetFont (Sender: TObject);
var o_fontbox: TxlFontDialog;
begin
	o_fontbox := TxlFontDialog.Create (FOwner);
   o_fontbox.Font := Font;
   if o_fontbox.Execute() then
   begin
		Font := o_fontbox.Font;
      if assigned (FOnFontChange) then
      	FOnFontChange (self);
   end;
   o_fontbox.free;
end;

procedure TFontColorPanel.f_OnSetColor (Sender: TObject);
var o_box: TxlColorDialog;
begin
   o_box := TxlColorDialog.create (FOwner);
   o_box.Color := Color;
   if o_box.Execute () then
   begin
      Color := o_box.Color;
      if assigned (FOnColorChange) then
      	FOnColorChange (self);
   end;
   o_box.Free;
end;

end.


//Tip_Box DIALOG 6, 15, 120, 12
//STYLE DS_SETFONT | WS_VISIBLE | WS_BORDER | WS_POPUP
//FONT 9, "Arial"
//BEGIN
//    CONTROL         "", st_tip, WC_STATIC, NOT WS_GROUP | SS_LEFTNOWORDWRAP, 0, 0, 120, 12
//END

//type
//   _TTipBox = class (TxlDialogML)
//   protected
//      procedure OnInitialize (); override;
//      procedure SetTip (const s_text: widestring; i_width: cardinal = 0);
//   end;
//
//   TTipBox = class
//   private
//      FTipBox: _TTipBox;
//   public
//      constructor Create (AOwner: TxlWinControl; const s_text: widestring; i_width: cardinal = 0);
//      destructor Destroy (); override;
//   end;

//----------------------

//procedure _TTipBox.OnInitialize ();
//begin
//   SetTemplate (Tip_Box);
//end;
//
//procedure _TTipBox.SetTip (const s_text: widestring; i_width: cardinal = 0);
//begin
//   if i_width <= 0 then i_width := length(s_text) * 14;
//
//   Move (-1, -1, i_Width, -1);
//
//   with TxlStaticText.create(self, ItemHandle[st_tip]) do
//   begin
//      Text := s_text;
//      Move (0, 0, i_Width, self.Pos.Height);
//      Color := rgb(255, 255, 128);
//      free;
//   end;
//end;
//
//constructor TTipBox.Create (AOwner: TxlWinControl; const s_text: widestring; i_width: cardinal = 0);
//begin
//   FTipBox := _TTipBox.Create (AOwner);
//   with FTipBox do
//   begin
//      Open (false);
//      SetTip (s_text, i_width);
//      Show;
//   end;
//end;
//
//destructor TTipBox.Destroy ();
//begin
//   FTipBox.Close;
//   FTipBox.Free;
//end;

