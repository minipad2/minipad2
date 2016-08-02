unit UxlTreeView;

interface

uses Windows, Messages, CommCtrl, UxlDragControl, UxlWinControl, UxlClasses, UxlImageList, UxlFunctions, UxlList;

type
   TTreeViewStyle = record
      EditLabels: boolean;
      NoHScroll: boolean;
      HasButtons: boolean;
      HasLines: boolean;
      LinesAtRoot: boolean;
   end;

	TTreeItemPointEvent = procedure (handle: HTreeItem; const pt: TPoint) of object;

   TTreeViewItem = record
      text: widestring;
      data: ulong;
      image: integer;
      selectedimage: integer;
      state: integer;
      children: boolean;
      level: integer;
   end;

	TFindMode = (fmCurrentItem, fmParentItem, fmChildItem, fmNextItem, fmPreviousItem, fmRootItem);
	TTreeItemEvent = procedure (handle: HTreeItem) of object;
	TTreeItemSelChangeEvent = function (oldhandle, newhandle: HTreeItem): integer of object;  // return 1 to prevent from change
	TTreeLabelEditEvent = function (handle: HTreeItem; const newtext: widestring): integer of object;  // return 1 to accept edit

	TTreeViewItems = class
   private
   	FOwner: TxlWinControl;
      FSelChangeEvent: boolean;
      FOnSelChanging: TTreeItemSelChangeEvent;
      FOnSelChanged: TTreeItemSelChangeEvent;
      FOnBeginLabelEdit: TTreeLabelEditEvent;
      FOnEndLabelEdit: TTreeLabelEditEvent;
      FOnExpanding: TTreeItemEvent;

      procedure f_testHItem (var h_Item: HTreeItem);
      function f_InsertItem (h_parent, h_insertafter: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
      procedure SetItem (h_Item: HTREEITEM; const o_item: TTreeViewItem);
      procedure SetItemText (h_Item: HTREEITEM; const s_text: widestring);
      function GetItem (h_Item: HTREEITEM): TTreeViewItem;
      function GetItemText (h_Item: HTREEITEM): widestring;
//      procedure SetItemState (h_Item: HTreeItem; i_state: integer);
//      function GetItemState (h_Item: HTreeItem): integer;
   public
   	constructor Create (AOwner: TxlWinControl);
      procedure Clear ();

      function Add (h_sibling: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
      function AddChild (h_parent: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
      function InsertBefore (h_sibling: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
      function InsertAfter (h_sibling: HTreeItem; const o_item: TTreeViewItem): HTreeItem;
      function Find (fmmode: TFindMode; h_Item: HTREEITEM = nil): HTREEITEM;
      function FindByData (i_data: integer; h_parent: HTreeItem = nil): HTreeItem;
      function Select (h_Item: HTREEITEM; b_selchangeevent: boolean = true): boolean;
      function Delete (h_Item: HTREEITEM = nil; b_selchangeevent: boolean = true): boolean;
      procedure EditLabel (h_item: HTREEITEM = nil);
      procedure Expand (h_Item: HTreeItem = nil; b_expand: boolean = true);

      function ProcessNotify (code: integer; lParam: dword): dword;
      property Items[h_Item: HTREEITEM]: TTreeViewItem read GetItem write SetItem; default;
      property ItemText[h_Item: HTreeItem]: widestring read GetItemText write SetItemText;
//      property ItemState[h_Item: HTreeItem]: integer read GetItemState write SetItemState;

      property OnSelChanging: TTreeItemSelChangeEvent read FOnSelChanging write FOnSelChanging;
      property OnSelChanged: TTreeItemSelChangeEvent read FOnSelChanged write FOnSelChanged;
      property OnBeginLabelEdit: TTreeLabelEditEvent read FOnBeginLabelEdit write FOnBeginLabelEdit;
      property OnEndLabelEdit: TTreeLabelEditEvent read FOnEndlabelEdit write FOnEndLabelEdit;
      property OnFirstExpanding: TTreeItemEvent read FOnExpanding write FOnExpanding;
   end;

   TxlTreeView = class (TxlControlWithImages)
   private
      FOnRightClick: TTreeItemPointEvent;
      FOnContextMenu: TTreeItemEvent;

      FStyle: TTreeViewStyle;
      FItems: TTreeViewItems;

      procedure SetStyle (const value: TTreeViewStyle);
   protected
		function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;

      procedure OnFontChange (Sender: TObject); override;
      procedure SetColor (i_color: TColor); override;
      procedure SetShowImages (value: boolean); override;

      procedure SetDropHighlight (hidx: integer; b_highlight: boolean); override;
      function HitTestItem (): integer; override;
      procedure GetDragData (o_list: TxlIntList); override;
      procedure DeleteDragItems (); override;
   public
      function ProcessNotify (code: integer; lParam: dword): dword; override;
      property OnRightClick: TTreeItemPointEvent read FOnRightClick write FOnRightClick;
      property OnContextMenu: TTreeItemEvent read FOnContextMenu write FOnContextMenu;

      property Style: TTreeViewStyle read FStyle write SetStyle;
      property Items: TTreeViewItems read FItems write FItems;
   end;

implementation

uses UxlWinDef;

constructor TTreeViewItems.Create(AOwner: TxlWinControl);
begin
	FOwner := AOwner;
      FSelChangeEvent := true;

end;

procedure TTreeViewItems.Clear ();
begin
	TreeView_DeleteAllItems (FOwner.handle);
end;

function TTreeViewItems.Add (h_sibling: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
begin
	if (h_sibling = TVI_ROOT) or (h_sibling = nil) then
   	result := f_insertitem (h_sibling, TVI_LAST, o_item)
   else
   	result := f_insertitem (Find(fmParentItem, h_sibling), TVI_LAST, o_item);
end;

function TTreeViewItems.AddChild (h_parent: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
begin
	result := f_insertitem (h_parent, TVI_LAST, o_item);
end;

function TTreeViewItems.InsertBefore (h_sibling: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
var h_parent, h_insertafter: HTreeItem;
begin
	h_parent := Find (fmParentItem, h_sibling);
   h_insertafter := Find (fmPreviousItem, h_sibling);
   if h_insertafter = nil then h_insertafter := TVI_FIRST;
	result := f_insertitem (h_parent, h_insertafter, o_item);
end;

function TTreeViewItems.InsertAfter (h_sibling: HTreeItem; const o_item: TTreeViewItem): HTreeItem;
begin
	result := f_insertitem (Find(fmParentItem, h_sibling), h_sibling, o_item);
end;

function TTreeViewItems.f_InsertItem (h_parent, h_insertafter: HTREEITEM; const o_item: TTreeViewItem): HTREEITEM;
var lpis: TV_INSERTSTRUCTW;
begin
	lpis.hParent := h_Parent;
  	lpis.hInsertAfter := h_insertafter;
   with lpis.itemex do
   begin
   	mask := TVIF_ITEMINFO;
      pszText := pwidechar (o_item.text);
      lParam := o_item.data;
      iImage := o_item.image;
      iSelectedImage := o_item.selectedimage;
      State := o_item.state shl 8;
      StateMask := TVIS_OVERLAYMASK;
      cChildren := BoolToInt(o_item.children);
   end;
	result := HTREEITEM(FOwner.Perform (TVM_INSERTITEMW, 0, DWORD(@lpis)));
end;

function TTreeViewItems.GetItem (h_Item: HTREEITEM): TTreeViewItem;
var pitem: TTVITEMEXW;
   h: HTREEITEM;
begin
   f_testHItem (h_item);
   if h_Item = nil then exit;
	with pitem do
   begin
   	mask := TVIF_HANDLE or TVIF_ITEMINFO or TVIF_CHILDREN;
      pszText := sharedbuffer;
      cchTextMax := sharedbuffersize;
      hItem := h_Item;
      StateMask := TVIS_OVERLAYMASK;
   end;
	FOwner.Perform (TVM_GETITEMW, 0, DWORD(@pitem));
   with result do
   begin
   	text := sharedbuffer;
      data := pitem.lparam;
      image := pitem.iImage;
      selectedimage := pitem.iSelectedImage;
      children := (pitem.cChildren > 0);
      state := pitem.State shr 8;
   end;
   result.level := 1;
   h := Find (fmParentItem, h_Item);
   while h <> nil do
   begin
   	inc (result.level);
      h := Find (fmParentItem, h);
   end;
end;

procedure TTreeViewItems.SetItem (h_Item: HTREEITEM; const o_item: TTreeViewItem);
var pitem: TTVITEMEXW;
begin
   f_testHItem (h_item);
   if h_Item = nil then exit;
	with pitem do
   begin
   	mask := TVIF_HANDLE or TVIF_ITEMINFO or TVIF_CHILDREN;
      hItem := h_Item;
      pszText := pwidechar(o_item.text);
      lParam := o_item.data;
      iImage := o_item.image;
      iSelectedimage := o_item.Selectedimage;
      State := o_item.state shl 8;
      if o_item.children then
      	cchildren := 1
      else
      	cchildren := 0;
      StateMask := TVIS_OVERLAYMASK;
   end;
   FOwner.Perform (TVM_SETITEMW, 0, DWORD(@pitem));
end;

function TTreeViewItems.GetItemText (h_Item: HTreeItem): widestring;
begin
   f_testHItem (h_item);
	if h_item = nil then exit;
	result := Items[h_Item].text;
end;

procedure TTreeViewItems.SetItemText (h_Item: HTreeItem; const s_text: widestring);
var pitem: TTVITEMEXW;
begin
   f_testHItem (h_item);
   if h_Item = nil then exit;
	with pitem do
   begin
   	mask := TVIF_HANDLE or TVIF_TEXT;
      hItem := h_Item;
      pszText := pwidechar(s_text);
   end;
   FOwner.Perform (TVM_SETITEMW, 0, DWORD(@pitem));
end;

//因暂时不用而屏蔽，勿删
//procedure TTreeViewItems.SetItemState (h_Item: HTreeItem; i_state: integer);
//var pitem: TTVITEMEXW;
//begin
//   f_testHItem (h_item);
//   if h_Item = nil then exit;
//	with pitem do
//   begin
//   	mask := TVIF_HANDLE or TVIF_STATE;
//      hItem := h_Item;
//      State := INDEXTOOVERLAYMASK(i_state);
//      StateMask := TVIS_OVERLAYMASK;
//   end;
//   FOwner.Perform (TVM_SETITEMW, 0, DWORD(@pitem));
//end;
//
//function TTreeViewItems.GetItemState (h_Item: HTreeItem): integer;
//begin
//   f_testHItem (h_item);
//   if h_Item = nil then
//   	result := 0
//   else
//   	result := Items[h_Item].state;
//end;

function TTreeViewItems.Find (fmmode: TFindMode; h_Item: HTREEITEM = nil): HTREEITEM;
var flag: word;
begin
   case fmmode of
   	fmCurrentItem: flag := TVGN_CARET;
      fmParentItem: flag := TVGN_PARENT;
      fmChildItem: flag := TVGN_CHILD;
      fmNextItem: flag := TVGN_NEXT;
      fmPreviousItem: flag := TVGN_PREVIOUS;
      else flag := TVGN_ROOT;    // fmRootItem:
   end;
   result := TreeView_GetNextItem (FOwner.handle, h_Item, flag);
end;

function TTreeViewItems.FindByData (i_data: integer; h_parent: HTreeItem = nil): HTreeItem;
var h, h2: HTreeItem;
begin
   result := nil;
   if h_parent = nil then
   	h := Find (fmRootItem)
   else
   	h := Find (fmChildItem, h_parent);

   while h <> nil do
   begin
      if Items[h].data = i_data then
      begin
         result := h;
         exit;
      end
      else
      begin
         h2 := FindByData (i_data, h);
         if h2 <> nil then
         begin
            result := h2;
            break;
         end
         else
            h := Find (fmNextItem, h);
      end;
   end;
end;

function TTreeViewItems.Select (h_Item: HTREEITEM; b_selchangeevent: boolean = true): boolean;
begin
	FSelChangeEvent := b_selchangeevent;
   result := IntToBool (FOwner.Perform (TVM_SelectItem, TVGN_CARET, DWord(h_Item)));
   FSelChangeEvent := true;
end;

function TTreeViewItems.Delete (h_Item: HTREEITEM = nil; b_selchangeevent: boolean = true): boolean;
begin
   f_testHItem (h_item);
   if h_item = nil then
      result := false
   else
   begin
      FSelChangeEvent := b_selchangeevent;
	   result := TreeView_DeleteItem (FOwner.handle, h_item);
      FSelChangeEvent := true;
   end;
end;

procedure TTreeViewItems.EditLabel (h_item: HTREEITEM = nil);
begin
   f_testHItem (h_item);
	if h_item <> nil then TreeView_EditLabel (FOwner.handle, h_item);
end;

procedure TTreeViewItems.Expand (h_Item: HTreeItem = nil; b_expand: boolean = true);
begin
   f_testHItem (h_item);
   if h_item = nil then exit;
	if b_expand then
   	FOwner.Perform (TVM_EXPAND, TVE_EXPAND, lparam(h_Item))
   else
   	FOwner.Perform (TVM_EXPAND, TVE_COLLAPSE, lparam(h_Item));
end;

procedure TTreeViewItems.f_testHItem (var h_Item: HTreeItem);
begin
   if h_Item = nil then
      h_item := Find (fmCurrentItem, nil);
end;

function TTreeViewItems.ProcessNotify (code: integer; lParam: dword): dword;
var nmtdi: TNMTVDISPINFOW;
   nmtv: TNMTREEVIEW;
   i: integer;
begin
	result := 0;
   case code of
   	TVN_BEGINLABELEDITW:
      	if assigned (FOnBeginLabelEdit) then
         begin
         	nmtdi := PNMTVDISPINFOW (lParam)^;
         	result := FOnBeginLabelEdit (nmtdi.item.hItem, nmtdi.item.pszText);
         end;
   	TVN_ENDLABELEDITW:
      	if assigned (FOnEndLabelEdit) then
         begin
         	nmtdi := PNMTVDISPINFOW (lParam)^;
         	result := FOnEndLabelEdit (nmtdi.item.hItem, nmtdi.item.pszText);
         end;
   	TVN_SelChangingW:
      	if FSelChangeEvent and assigned (FOnSelChanging) then
         begin
            nmtv := PNMTREEVIEW (lParam)^;
            result := FOnSelChanging (nmtv.itemOld.hItem, nmtv.itemNew.hItem);
         end;
      TVN_SelChangedW:
      	if FSelChangeEvent and assigned (FOnSelChanged) then
         begin
            nmtv := PNMTREEVIEW (lParam)^;
            result := FOnSelChanged (nmtv.itemOld.hItem, nmtv.itemNew.hItem);
         end;
      TVN_ITEMEXPANDINGW:
      	if assigned (FOnExpanding) then
         begin
            nmtv := PNMTREEVIEW (lParam)^;
            i := nmtv.itemNew.state and TVIS_EXPANDEDONCE;
            if i = 0 then
					FOnExpanding (nmtv.itemNew.hItem);
         end;
   end;
end;

//------------------------------ TxlTreeView -----------------------------------

function TxlTreeView.DoCreateControl (HParent: HWND): HWND;
var i_style: DWord;
begin
   i_Style := TVS_HASBUTTONS or TVS_HASLINES or TVS_LINESATROOT or TVS_SHOWSELALWAYS;
   i_style := i_style and not TVS_EDITLABELS;
   InitCommonControl (ICC_TREEVIEW_CLASSES);
   result := CreateWin32Control (HParent, WC_TREEVIEW, i_style, WS_EX_CLIENTEDGE);
end;

procedure TxlTreeView.OnCreateControl ();
begin
   SetWndProc (@WindowProc);
   self.Color := clWhite;
   FItems := TTreeViewItems.Create (self);
end;

procedure TxlTreeView.OnDestroyControl ();
begin
	FItems.Free;
   DeInitCommonControl();
   inherited;
end;

procedure TxlTreeView.SetStyle (const value: TTreeViewStyle);
begin
	SetWndStyle (TVS_EDITLABELS, value.EditLabels);
   SetWndStyle (TVS_HASBUTTONS, value.HasButtons);
   SetWndStyle (TVS_HASLINES, value.HasLines);
   SetWndStyle (TVS_LINESATROOT, value.LinesAtRoot);
   SetWndStyle (TVS_NOHSCROLL, value.NoHScroll);
   FStyle := value;
end;

procedure TxlTreeView.SetShowImages (value: boolean);
begin
   if value then
      TreeView_SetImageList(Fhandle, Images.handle, TVSIL_NORMAL)
   else
      TreeView_SetImageList(Fhandle, 0, TVSIL_NORMAL);
	inherited SetShowImages (value);
end;

procedure TxlTreeView.OnFontChange (Sender: TObject);
begin
   inherited OnfontChange (Sender);
   Perform (TVM_SETTEXTCOLOR, 0, lParam(Font.color));
end;

procedure TxlTreeView.SetColor (i_color: TColor);
begin
   Perform (TVM_SETBKCOLOR, 0, lparam(i_color));
end;

//-------------------------

function TxlTreeView.ProcessNotify (code: integer; lParam: dword): dword;
var nmtv: TNMTREEVIEW;
begin
	result := 0;
   case code of
      NM_RCLICK:
         if assigned (FOnContextMenu) then
             FOnContextMenu (HTreeItem(HitTestItem));
      TVN_BEGINDRAGW:
      	begin
            nmtv := PNMTREEVIEW (lParam)^;
				Items.Select (nmtv.itemNew.hItem);
      		CheckBeginDrag;
         end;
		else
      	result := FItems.ProcessNotify (code, lParam);
   end;
end;

//---------------

function TxlTreeView.HitTestItem (): integer;
var tvhit: TTVHITTESTINFO;
	h: HTreeItem;
begin
	tvhit.pt := CursorPos;
   tvhit.flags := TVHT_ONITEM;
	h := TreeView_HitTest (Fhandle, tvhit);
   if h <> nil then
   	result := Integer(h)
   else
   	result := c_nilhidx;
end;

procedure TxlTreeView.SetDropHighlight (hidx: integer; b_highlight: boolean);
var pitem: TTVITEMEXW;
begin
	if hidx = c_nilhidx then exit;
   with pitem do
   begin
      mask := TVIF_Handle or TVIF_STATE;
      hItem := HTreeItem(hidx);
      StateMask := TVIS_DROPHILITED;
      if b_highlight then
         State := TVIS_DROPHILITED
      else
         State := 0;
   end;
   Perform (TVM_SETITEMW, 0, DWORD(@pitem));
end;

procedure TxlTreeView.GetDragData (o_list: TxlIntList);
var h: HTreeItem;
begin
	h := Items.Find (fmCurrentItem);
   if h = nil then exit;
	o_list.clear;
  	o_list.Add (Items[h].data);
end;

procedure TxlTreeView.DeleteDragItems ();
var h: HTreeItem;
begin
	h := Items.Find (fmCurrentItem);
   if h = nil then exit;
   Items.Delete (h);
end;

end.


