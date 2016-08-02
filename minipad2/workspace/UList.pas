unit UList;

interface

uses Windows, UxlClasses, UxlWinControl, UxlDragControl, UxlListView, UClientSuper, UxlExtClasses, UTypeDef,
	UPageSuper, UxlList, UPageProperty;

type
   TListClient = class (TListClientSuper, IOptionObserver, ILangObserver)
   private
      FWndParent: TxlWincontrol;
      FListView: TxlListView;
      FSortCol: integer;
      FTemprList: TxlStrList;
      FSettings: widestring;
      FPList: TListProperty;

      procedure f_OnContextMenu (index: integer);
      procedure f_OnDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
      function f_OnEndLabelEdit (index: integer; const newtext: widestring): integer;
      procedure f_OnColumnClick (index: integer);
      procedure f_OnToolTipDemand (index: integer; const rt: TRect);
		function f_GetListItem (o_page: TPageSuper): TListViewItem;
      procedure f_OnDeleteItemDemand (Sender: TObject);
   protected
      procedure Load (value: TPageSuper); override;
      procedure UnLoad (); override;
      function Items (index: integer): TPageSuper; override;
   public
      constructor Create (WndParent: TxlWinContainer);
      destructor Destroy (); override;
      function Control (): TxlControl; override;
		procedure OnPageEvent (pct: TPageEvent; id, id2: integer); override;
      procedure Save (); override;

      function CheckCommand (opr: word): boolean; override;
      procedure ExecuteCommand (opr: word); override;
      procedure OptionChanged ();
      procedure LanguageChanged ();
   end;

implementation

uses Messages, UxlWinClasses, UxlStrUtils, UxlCommDlgs, UxlFunctions, Resource, UGlobalObj, UPageStore, UOptionManager,
	ULangManager, UPageFactory, UxlMath;

constructor TListClient.Create (WndParent: TxlWinContainer);
begin
   FWndParent := WndParent;
   FSortCol := -1;

   FListView := TxlListview.Create (WndParent);
   with FListView do
   begin
      LargeImages := PageImageList.LargeImages;
      SmallImages := PageImageList.SmallImages;
      ShowImages := true;
      CanDrag := true;
      CanDrop := true;
      OnDropEvent := f_OnDropEvent;
   	OnItemDblClick := f_OnItemDblClick;
      OnItemReturn := f_OnItemDblClick;
      Items.OnSelect := f_OnSelectItem;
      OnContextMenu := f_OnContextMenu;
      Items.OnEndLabelEdit := f_OnEndLabelEdit;
      Cols.OnClick := f_OnColumnClick;
      OnToolTipDemand := f_OnToolTipDemand;
      OnDeleteItemDemand := f_OnDeleteItemDemand;
   end;

   FTemprList := TxlStrList.Create;
   FTemprList.Separator := #9;

   OptionMan.AddObserver(self);
   LangMan.AddObserver (self);
end;

destructor TListClient.Destroy ();
begin
	LangMan.RemoveObserver (self);
	OptionMan.RemoveObserver(self);

   FListView.free;
   FTemprList.Free;
   inherited;
end;

function TListClient.Control (): TxlControl;
begin
	result := FListView;
end;

procedure TListClient.OptionChanged ();
begin
   FListView.Color := OptionMan.Options.ListColor;
	FListView.Font := OptionMan.Options.ListFont;    // 需放在setcolor之后，原因不明
end;

procedure TListClient.LanguageChanged ();
begin
	FSettings := '';
   Refresh;
end;

function TListClient.Items (index: integer): TPageSuper;
begin
	result := PageStore[FListView.Items[index].data];
end;

//-----------------------

procedure TListClient.Load (value: TPageSuper);
   function f_ListStyle (value: TListProperty): TListViewStyle;
   begin
      with result do
      begin
         ViewStyle := TViewType(Ord(value.View));
         MultiSelect := true;
         FullRowSelect := value.FullRowSelect;
         GridLines := value.GridLines;
         CheckBoxes := value.CheckBoxes;
         EditLabels := ViewStyle <> lvsReport;
         ShowHeader := true;
         HeaderDragDrop := true;
      end;
   end;
var o_list: TxlIntList;
	o_list2: TxlStrList;
	i: integer;
   o_item: TListViewItem;
begin
	if value = nil then exit;
   inherited Load (value);

	// initialize and add columns, 并缓存记忆
   FPList := value.ListProperty;
   o_list2 := TxlStrList.Create;
   FPList.Save (o_list2);
   if (o_list2.Text <> FSettings) then
   begin
      FListView.Clear;
      FListView.Style := f_ListStyle(FPList);
      for i := FPList.ColList.Low to FPList.ColList.High do
         FListView.Cols.Add (LangMan.GetItem(FPList.ColList[i]), FPList.widthlist[i]);
      FSettings := o_list2.Text;
   end;
   o_list2.free;

   // add items
   FListView.items.Clear;
	o_list := TxlIntList.Create;
	value.GetChildList (o_list);
   for i := o_list.Low to o_list.High do
   begin
   	o_item := f_GetListItem (PageStore[o_list[i]]);
      FListView.Items.Add (o_item);
   end;
   o_list.Free;
   CommandMan.CheckCommands;
end;

procedure TListClient.UnLoad ();
begin
	FListView.Items.Clear;
end;

procedure TListClient.Save ();
var i, i_col: integer;
   o_list, o_list2: TxlIntList;
begin
   o_list := TxlIntList.Create;
   o_list2 := TxlIntLIst.Create();

   FListView.Cols.GetHeaderOrder (o_list);     // 记忆列头拖放的效果
   for i := FPList.ColList.Low to FPList.ColList.High do
   	o_list2.Add (FPList.ColList[i]);
   for i := FPList.ColList.Low to FPList.ColList.High do
   begin
   	i_col := o_list[i];
   	FPList.ColList[i] := o_list2[i_col];
   	FPList.WidthList[i] := FListView.Cols.Width[i_col];
   end;

   for i := FListView.Items.Low to FListView.Items.High do
   	Items(i).Checked := FListView.Items.Checked[i];

   o_list.Free;
   o_list2.Free;
end;

function TListClient.f_GetListItem (o_page: TPageSuper): TListViewItem;
var j: integer;
begin
   FTemprList.Clear;
   for j := FPList.collist.Low to FPList.collist.High do
      FTemprList.Add (MultiLineToSingleLine(o_page.GetColText(FPList.collist[j]), true));
   with result do
   begin
      text := FTemprList.Text;
      data := o_page.id;
      image := o_page.ImageIndex;
      state := 0;
      checked := o_page.Checked;
      selected := false;
   end;
end;

procedure TListClient.OnPageEvent (pct: TPageEvent; id, id2: integer);
var o_item: TListViewItem;
   i, j: integer;
begin
   case pct of
      pctFieldModified, pctIcon, pctColor, pctSwitchStatus:
      	begin
         	i := FListView.Items.FindByData (id);
            if i >= 0 then
					FListView.Items[i] := f_GetListItem (PageStore[id]);
         end;
      pctAddChild:
         if id = FPage.id then
      	begin
            o_item := f_GetListItem (PageStore[id2]);
            i := FPage.Childs.FindChild (id2);
				if i <= FListView.Items.High then
            begin
            	if FListView.Style.ViewStyle in [lvsList, lvsReport] then
               	FListView.Items.Insert (i, o_item)
               else            // 对于icon和small icon风格，使用insert不能插入到正确位置，而总是在最后
               	Refresh;
            end
            else
               FListView.Items.Add (o_item);
            for j := FListView.Items.Low to FListView.Items.High do
               FListView.Items.Select (j, j = i);
         end;
      pctRemoveChild:
      	begin
         	if not (id = FPage.id) then exit;
         	i := FListView.Items.FindByData (id2);
            if i >= 0 then
					FListView.Items.Delete (i);
         end;
      pctListProperty:
      	Refresh;
   else
      inherited OnPageEvent (pct, id, id2);
   end
end;

//---------------

function TListClient.CheckCommand (opr: word): boolean;
begin
	case opr of
      m_clear, m_selectall:
         result := FListView.Items.Count > 0;
      m_undo, m_redo, m_cut, m_copy, m_paste:
      	result := false;
      m_delete:
         if FPage = nil then
            result := false
         else if FPage.IsVirtualContainer then
            result := CommandMan.CheckCommand (m_removeitem)
         else
      	   result := CommandMan.CheckCommand (m_deleteitem);
      m_wordwrap, m_find, m_subsequentfind, m_highlightmatch, m_texttools, m_insertlink, m_insertcliptext, m_inserttemplate:
      	result := false;
      else
         result := true;
   end;
end;

procedure TListClient.ExecuteCommand (opr: word);
begin
	if not CheckCommand (opr) then exit;
	case opr of
		m_clear:
      	begin
//         	if OptionMan.Options.ConfirmClear then
//            	if (ShowMessage (LangMan.GetItem(sr_clearprompt), mtQuestion,
//               	LangMan.GetItem(sr_prompt)) = mrCancel) then exit;
         	FListView.Items.SelectAll;
				CommandMan.ExecuteCommand (m_deleteitem);  // 此过程本身会出现提示
         end;
      m_selectall:
      	FListView.Items.SelectAll;
      m_delete:
         if FPage.IsVirtualContainer then
            CommandMan.ExecuteCommand (m_removeitem)
         else
            CommandMan.ExecuteCommand (m_deleteitem);
//      else
//      	inherited ExecuteCommand (opr);
   end;
end;

//--------------------------

procedure TListClient.f_OnDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
var i_targetid: integer;
begin
   if hidxTarget < 0 then
      hidxTarget := FListView.Items.High;
   if (hidxTarget >= 0) and assigned (OnDropItems) then
   begin
   	i_targetid := FListView.Items[hidxTarget].data;
   	OnDropItems (o_dragsource, i_targetid, FPage.id, b_copy);
   end;
end;

function TListClient.f_OnEndLabelEdit (index: integer; const newtext: widestring): integer;
begin
   if newtext <> '' then
   begin
      result := 1;
      Items(index).name := newtext;
   end
   else
   	result := 0;
end;

procedure TListClient.f_OnContextMenu (index: integer);
var i_demand: cardinal;
begin
	if index >= 0 then
//   begin
//   	if FPage.PageType = ptFavorite then
//      	i_demand := FavoriteListContext
//      else
	     	i_demand := ListContext
//   end
   else
   	i_demand := ListNoSelContext;
   EventMan.EventNotify (e_ContextMenuDemand, i_demand);
end;

procedure TListClient.f_OnColumnClick (index: integer);
var b_asc: boolean;
begin
	if index = FSortCol then
   begin
   	b_asc := false;
      FSortCol := -1;
   end
   else
   begin
   	b_asc := true;
      FSortCol := index;
   end;
	FListView.Sort (index, b_asc);
end;

procedure TListClient.f_OnToolTipDemand (index: integer; const rt: TRect);
begin
end;

procedure TListClient.f_OnDeleteItemDemand (Sender: TObject);
begin
   CommandMan.ExecuteCommand (m_delete);
end;

end.













