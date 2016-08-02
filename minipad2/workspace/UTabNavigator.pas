unit UTabNavigator;

interface

uses Windows, UxlWinControl, UxlClasses, UxlDragControl, UNavigatorSuper, UxlTabControl, UxlExtClasses, UGlobalObj, 
	UPageSuper,	UTypeDef;

type TTabNavigator = class (TNavigatorSuper, IOptionObserver, ISizeObserver)
private
   FTab: TxlTabControl;
   FWndParent: TxlWinControl;
   FPage: TPageSuper;

   procedure f_OnTabSelChanged (newindex: integer);
   procedure f_OnTabDblClick (index: integer);
   procedure f_OnTabContextMenu (index: integer);
	procedure f_OnTabDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
   function f_FindItem (value: TPageSuper): integer;
	function f_GetTabItem (value: TPageSuper): TTabItem;
   function f_ProcessMessage (AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
	procedure f_ExecutePageOperation(pb: TPageBehavior);
   function Page (index: integer): TPageSuper;
protected
   procedure LoadIndex (); override;
   procedure SelectPage (value: TPageSuper); override;
	procedure AddPage (value: TPageSuper); override;
   procedure ResetPage (value: TPageSuper); override;
   procedure DeletePage (value: TPageSuper); override;
   procedure RenamePage (value: TPageSuper); override;
public
	constructor Create (WndParent: TxlWinContainer);
   destructor Destroy (); override;
   function Control (): TxlDragControl; override;

   procedure OptionChanged ();
   procedure AdjustAppearance ();
end;

implementation

uses MESSAGES, UxlExtDlgs, UxlList, Resource, UxlCommDlgs, UxlFunctions, UxlMath, UxlStrUtils, UPageStore, UOptionManager,
	UPageFactory, ULangManager;

constructor TTabNavigator.Create (WndParent: TxlWinContainer);
begin
   FWndParent := WndParent;

	FTab := TxlTabControl.create (WndParent);
   with FTab do
   begin
      Images := PageImageList.Images;
      Items.OnSelChanged := f_OnTabSelChanged;
      OnDblClick := f_OnTabDblClick;
      OnContextMenu := f_OnTabContextMenu;
      OnDropEvent := f_OnTabDropEvent;
      MessageHandler := f_ProcessMessage;
	end;
   OptionMan.AddObserver (self);
   SizeMan.AddObserver (self);
end;

destructor TTabNavigator.Destroy ();
begin
	OptionMan.RemoveObserver (self);
   SizeMan.RemoveObserver (self);
	FTab.free;
   inherited;
end;

function TTabNavigator.Control (): TxlDragControl;
begin
	result := FTab;
end;

function TTabNavigator.Page (index: integer): TPageSuper;
begin
	result := PageStore[FTab.Items[index].data];
end;

procedure TTabNavigator.OptionChanged ();
begin
   with FTab do
   begin
      CanDrag := true;
      CanDrop := true;
   	Font := OptionMan.Options.tabfont;
   end;
   if FTab.ShowImages <> OptionMan.Options.ShowTabImages then
   begin
  	 	FTab.ShowImages := OptionMan.Options.ShowTabImages;
      if Active then
      begin
      	LoadIndex ();
         SelectPage (FPage);
      end;
   end;
   AdjustAppearance;
   if Active and (FTab.Items.SelIndex >= 0) then
  		FTab.Items.ItemHighlight [FTab.Items.SelIndex] := OptionMan.Options.HighlightCurrentTab;
end;

procedure TTabNavigator.AdjustAppearance ();
var tcs: TTabStyle;
begin
	tcs := FTab.Style;
   with tcs do
   begin
   	multiline := OptionMan.Options.MultiLineTab and SizeMan.MultiLineTab;
      if OptionMan.Options.tabwidth <= 0 then
      	tabwidth := 0
      else
      	tabwidth := 4 + 2 * OptionMan.Options.tabwidth;
   end;
   FTab.Style := tcs;
end;

//----------------------

procedure TTabNavigator.LoadIndex ();
begin
   FTab.Items.Clear;
end;

procedure TTabNavigator.SelectPage (value: TPageSuper);
var o_list: TxlIntList;
	i: integer;
   o_item: TTabItem;
begin
	if value = nil then exit;
	if (FTab.Items.Count = 0) or (value.Owner <> FPage.Owner) then
   begin
   	o_list := TxlIntList.Create;
      if value.Owner <> nil then
   		value.Owner.GetChildList (o_list)
      else
      	o_list.Add (value.id);
      FTab.Items.Clear;
      for i := o_list.Low to o_list.High do
		begin
      	o_item := f_GetTabItem (PageStore[o_list[i]]);
   		FTab.Items.Add (o_item);
      end;
      o_list.free;
   	AdjustAppearance;
   end;
   FPage := value;
   for i := FTab.Items.Low to FTab.Items.High do
   	if FTab.Items[i].data = value.id then
      begin
      	FTab.Items.SelIndex := i;
  			FTab.Items.ItemHighlight [i] := OptionMan.Options.HighlightCurrentTab;
      end
      else
         FTab.Items.ItemHighlight [i] := false;
end;

procedure TTabNavigator.AddPage (value: TPageSuper);
var o_item: TTabItem;
	i: integer;
begin
	if value.Owner <> FPage.Owner then exit;
	o_item := f_GetTabItem (value);

   i := FPage.Owner.Childs.FindChild (value.id);
   if i <= FTab.Items.High then
      FTab.Items.Insert (i, o_item)
   else
      FTab.Items.Add (o_item);
   FTab.Update;
end;

function TTabNavigator.f_GetTabItem (value: TPageSuper): TTabItem;
begin
   with result do
   begin
   	if FTab.ShowImages then
      	text := value.name
      else
      	text := f_NameToLabel (value);
      data := value.id;
      image := value.ImageIndex;
      highlight := false;
   end;
end;

procedure TTabNavigator.ResetPage (value: TPageSuper);
var i: integer;
begin
	i := f_FindItem (value);
   if i >= 0 then
      FTab.Items[i] := f_GetTabItem (value);
end;

procedure TTabNavigator.RenamePage (value: TPageSuper);
var s_name: widestring;
begin
	s_name := value.name;
	if InputBox (LangMan.GetItem(sr_rename, '重命名'), LangMan.GetItem(sr_inputpagename, '请输入标签页名称：'),
      s_name, m_rename, LangMan.GetItem(IDOK), LangMan.GetItem(IDCancel)) then
   begin
      value.Name := MultiLineToSingleLine (s_name, true);
      Eventman.EventNotify (e_PageNameChanged, value.id);
   end;
end;

procedure TTabNavigator.DeletePage (value: TPageSuper);
var i_index: integer;
begin
	i_index := f_FindItem (value);
   if i_index >= 0 then
     	FTab.Items.Delete (i_index, false);
end;

function TTabNavigator.f_FindItem (value: TPageSuper): integer;
var i: integer;
begin
	result := -1;
	for i := FTab.Items.Low to FTab.Items.High do
   	if FTab.Items[i].data = value.id then
      begin
      	result := i;
         exit;
      end;
end;

//---------------------

procedure TTabNavigator.f_OnTabSelChanged (newindex: integer);
begin
	PageCenter.ActivePage := Page(newindex);
end;

procedure TTabNavigator.f_ExecutePageOperation(pb: TPageBehavior);
var m: integer;
begin
	case pb of
   	pbRename:
      	m := m_rename;
      pbSwitchProperty:
      	m := m_switchLock;
      else
      	m := m_deletepage;
   end;
   CommandMan.ExecuteCommand (m);
end;

procedure TTabNavigator.f_OnTabContextMenu (index: integer);
var p: TPageSuper;
begin
   if index < 0 then exit;
	p := Page(index);
   if p <> PageCenter.ActivePage then
		PageCenter.ActivePage := p;
   EventMan.EventNotify (e_ContextMenuDemand, PageContext);
end;

procedure TTabNavigator.f_OnTabDropEvent (o_dragsource: IDragSource; hidxTarget: integer; b_copy: boolean);
var o_page: TPageSuper;
begin
   if (hidxTarget >= 0) and assigned (OnDropItems) then
   begin
   	o_page := Page(hidxTarget);
   	OnDropItems (o_dragsource, o_page.id, o_Page.Owner.id, b_copy);
//      FTab.Update;
   end;
end;

procedure TTabNavigator.f_OnTabDblClick (index: integer);
begin
	f_ExecutePageOperation (OptionMan.Options.PageDblClick);
end;

function TTabNavigator.f_ProcessMessage (AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
var i_newpage: integer;
	w: smallint;
begin
	result := 0;
	b_processed := true;
	case AMessage of
      WM_MBUTTONDOWN:
			f_ExecutePageOperation (OptionMan.Options.PageMButtonClick);
      WM_MOUSEWHEEL:
      	begin
            w := wParam shr 16;
            i_newpage := ConfineRange (FTab.Items.SelIndex + -1 * sign(w), FTab.Items.Low, FTab.Items.High);
            if i_newpage <> FTab.Items.SelIndex then
					f_OnTabSelChanged (i_newpage);
      	end;
      else
      	b_processed := false;
   end;
end;

end.





