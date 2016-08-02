unit UDefineToolbarBox;

interface

uses UxlDialog, UxlListView, UxlStdCtrls, UxlList, UxlImageList, UxlDragControl;

type
	TLVToLV = class
   private
      FLVFrom: TxlListview;
      FLVTo: TxlListView;
      FCbMove: TxlButton;
      FKeepSep: boolean;
      procedure f_OnCbMoveClick (Sender: TObject);
      procedure f_OnItemDblClick (index: integer);
      procedure f_OnDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
      procedure f_LVToLVMove ();
   public
   	constructor Create (ALVFrom, ALVTo: TxlListView; ACbMove: TxlButton);
      procedure MoveInsideTo (i_dir: integer);
      property KeepSep: boolean read FKeepSep write FKeepSep;
   end;
   
   TDefineToolBarBox = class (TxlDialog)
   private
      FTBCurrent: TxlIntList;
      FTBAll: TxlIntList;
      FTBDefault: TxlIntList;

      FLVAvailable: TxlListView;
      FLVCurrent: TxlListView;
      FCbAdd: TxlButton;
      FCbRemove: TxlButton;
      FImages: TxlImageList;

      FAvailToCurrent: TLVToLV;
      FCurrentToAvail: TLVToLV;

      function f_NewItem (id: integer): TListViewItem;

      procedure f_AvailableItemSelEvent (index: integer; sel: boolean);
      procedure f_CurrentItemSelEvent (index: integer; sel: boolean);

      procedure f_Reset (o_tbcurrent: TxlIntList);
      procedure f_Save ();
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
      procedure Initialize (o_currentlist, o_alllist, o_default: TxlIntList);
   end;
   
implementation

uses Windows, Resource, UGlobalObj, UxlClasses, UxlMenu, UxlCommDlgs, ULangManager;

constructor TLVToLV.Create (ALVFrom, ALVTo: TxlListView; ACbMove: TxlButton);
begin
	FLVFrom := ALVFrom;
   with FLVFrom do
   begin
   	CanDrag := true;
      OnItemDblClick := f_OnItemDblClick;
   end;

   FLVTo := ALVTo;
   with FLVTo do
   begin
      CanDrop := true;
      OnDropEvent := f_OnDropEvent;
   end;

   FCbMove := ACbMove;
   FCbMove.OnClick := f_OnCbMoveClick;
end;

procedure TLVToLV.f_OnItemDblClick(index: integer);
begin
	f_LVToLVMove;
end;

procedure TLVToLV.f_OnCbMoveClick (Sender: TObject);
begin
	f_LVToLVMove;
end;

procedure TLVToLV.f_LVToLVMove ();
   procedure f_Move (id: integer);
   var i, index: integer;
   	o_item: TListViewItem;
   begin
   	if id < 0 then
      	index := -1 * id
      else
	   	index := FLVFrom.Items.FindByData (id);
      o_item := FLVFrom.Items[index];
      o_item.Selected := false;

      // delete from LVFrom
      if (o_item.data > 0) or (not KeepSep) then
      begin
         FLVFrom.Items.Delete (index);
         if not FLVFrom.Items.PosValid (index) then dec (index);
         if FLVFrom.Items.PosValid (index) then
         	FLVFrom.Items.Select (index);
      end;

      // insert into LVTo
      if (o_item.data > 0) or KeepSep then
      begin
         i := FLVTo.Items.SelIndex;
         if i < 0 then
            FLVTo.Items.Add (o_item)
         else
         begin
            FLVTo.Items.Insert (i, o_item);
            FLVTo.Items.Select (i + 1);
         end;
      end;
   end;
var i, id: integer;
	o_list: TxlIntList;
begin
	o_list := TxlIntList.Create;
   for i := FLVFrom.Items.Low to FLVFrom.Items.High do
   	if FLVFrom.Items[i].Selected then
      begin
         id := FLVFrom.Items[i].data;
         if id = 0 then
         	id := -1 * i;    // 因id=0的分隔符可能存在多个，单纯使用id无法区分
			o_list.Add (id);
      end;
   for i := o_list.Low to o_list.High do
   	f_Move (o_list[i]);
   o_list.free;
end;

procedure TLVToLV.f_OnDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
begin
	if o_dragsource = IDragSource (FLVFrom) then
   begin
   	FLVTo.Items.SelectAll (false);
   	FLVTo.Items.SelIndex := hidxTarget;
      f_LVToLVMove;
   end
   else if o_dragsource = IDragSource (FLVTo) then
   	MoveInsideTo (hidxTarget - FLVTo.Items.SelIndex);
end;

procedure TLVToLV.MoveInsideTo (i_dir: integer);
	procedure f_CheckMove (index, i_dir: integer);
   begin
   	if FLVTo.Items[index].Selected and FLVTo.Items.Move (index, index + i_dir) then
         FLVTo.Items.Select (index + i_dir);
   end;
var i: integer;
begin
	if i_dir > 0 then
   	for i := FLVTo.Items.High downto FLVTo.Items.Low do
      	f_CheckMove (i, i_dir)
   else if i_dir < 0 then
   	for i := FLVTo.Items.Low to FLVTo.Items.High do
      	f_CheckMove (i, i_dir);
end;

//----------------

procedure TDefineToolBarBox.OnInitialize ();
begin
	SetTemplate (DefineToolBar_Box, m_definetoolbar);
end;

procedure TDefineToolBarBox.Initialize(o_currentlist, o_alllist, o_default: TxlIntList);
begin
	FTBCurrent := o_currentlist;
   FTBAll := o_alllist;
   FTBDefault := o_default;
end;

const c_definetoolbarbox: array[0..4] of word = (cb_reset, IDOK, IDCANCEL, cb_up, cb_down);

procedure TDefineToolBarBox.OnOpen ();
var lvs: TListviewStyle;
	i: integer;
begin
	inherited;
	RefreshItemText (self, c_definetoolbarbox, DefineToolBar_Box);

   FImages := TxlImageList.Create ();
   for i := FTBAll.Low to FTBAll.High do
      FImages.AddIcon (FTBAll[i]);

   FLVAvailable := TxlListView.create (self, ItemHandle[lv_available]);
   FLVCurrent := TxlListView.create (self, ItemHandle[lv_current]);

   with lvs do
   begin
      ViewStyle := lvsReport;
      MultiSelect := true;
      FullRowSelect := false;
      GridLines := false;
      EditLabels := false;
      ShowHeader := true;
      HeaderDragDrop := false;
      CheckBoxes := false;
   end;
   with FLVAvailable do
   begin
   	SetStyle (lvs);
      SmallImages := FImages;
      ShowImages := true;
      Items.OnSelect := f_AvailableItemSelEvent;
      Cols.Add (LangMan.GetItem(sr_availablebuttons, '可用工具栏按钮'), 195, alLeft);
   end;
   with FLVCurrent do
   begin
   	SetStyle (lvs);
      SmallImages := FImages;
      ShowImages := true;
      Items.OnSelect := f_CurrentItemSelEvent;
      Cols.Add (LangMan.GetItem(sr_currentbuttons, '当前工具栏按钮'), 195, alLeft);
   end;

   f_reset (FTBCurrent);

   FCbAdd := TxlButton.Create (self, ItemHandle[cb_add]);
   FCbRemove := TxlButton.Create (self, ItemHandle[cb_remove]);
   FAvailToCurrent := TLVToLV.Create (FLVAvailable, FLVCurrent, FCbAdd);
   FAvailToCurrent.KeepSep := true;
   FCurrentToAvail := TLVToLV.Create (FLVCurrent, FLVAvailable, FCbRemove);
   FCurrentToAvail.KeepSep := false;
end;

procedure TDefineToolBarBox.OnClose ();
begin
   FAvailToCurrent.Free;
   FCurrentToAvail.Free;
//   FLVCurrent.free;     // 会引起再次进入对话框时列表拖放无响应。原因不明。
//   FLVAvailable.Free;
   FCbAdd.Free;
	FCbRemove.Free;
   FImages.Free;
   inherited;
end;

function TDefineToolBarBox.f_NewItem (id: integer): TListViewItem;
begin
	with result do
   begin
   	if id > 0 then
   		text := GetSimpleText (LangMan.GetItem (id))
      else
      	text := LangMan.GetItem(sr_separator, '分隔符');
   	data := id;
   	image := FImages.FindIcon(id);
   end;
end;

procedure TDefineToolBarBox.OnCommand (ctrlID: integer);
begin
	case CtrlID of
   	IDOK:
      	begin
            f_save ();
         	close (true);
         end;
      IDCANCEL:
         close (false);
      cb_up:
      	FAvailToCurrent.MoveInsideTo (-1);
      cb_down:
      	FAvailToCurrent.MoveInsideTo (1);
      cb_reset:
      	if showmessage (LangMan.GetItem(sr_resettoolbarprompt, '是否恢复默认的工具栏设置？'), mtQuestion, LangMan.GetItem(sr_prompt)) = mrOK then
         	f_reset (FTBDefault);
   end;
end;

procedure TDefineToolBarBox.f_Save ();
var i: integer;
begin
   FTBCurrent.clear;
   for i := FLVCurrent.Items.Low to FLVCurrent.Items.High do
      FTBCurrent.add (FLVCurrent.Items[i].data);
end;

procedure TDefineToolBarBox.f_reset (o_tbcurrent: TxlIntList);
var i, i_menu: integer;
begin
   FLVAvailable.Items.Clear ();
   for i := FTBAll.Low to FTBAll.High do
   begin
      i_menu := FTBAll[i];
      if not o_tbcurrent.ItemExists (i_menu) then
          FLVAvailable.Items.Add (f_NewItem(i_menu));
   end;
   FLVAvailable.Items.Add (f_NewItem(0));

   FLVCurrent.Items.Clear ();
   for i := FTBCurrent.Low to FTBCurrent.High do
      FLVCurrent.Items.Add (f_NewItem(o_tbcurrent[i]));

   ItemEnabled[cb_up] := false;
   ItemEnabled[cb_down] := false;
   ItemEnabled[cb_add] := false;
   ItemEnabled[cb_remove] := false;
end;

procedure TDefineToolBarBox.f_AvailableItemSelEvent (index: integer; sel: boolean);
begin
   ItemEnabled[cb_add] := FLVAvailable.Items.SelCount > 0;
end;

procedure TDefineToolBarBox.f_CurrentItemSelEvent (index: integer; sel: boolean);
begin
	ItemEnabled[cb_remove] := FLVCurrent.Items.SelCount > 0;
	if sel then
   begin
      ItemEnabled[cb_up] := (index > FLVCurrent.Items.Low);
      ItemEnabled[cb_down] := (index < FLVCurrent.Items.High);
   end;
end;

end.
