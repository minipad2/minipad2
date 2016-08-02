unit UMenuManager;

interface

uses Windows, UxlWindow, UxlMenu, UxlClasses, UxlExtClasses, UxlList, UxlWinClasses, UGlobalObj, UxlMiscCtrls, UxlWinControl, UPageFactory, UTypeDef;

type TMenuSuper = class (TxlMenu, ILangObserver, IMessageObserver)
private
   FMenuTip: TxlTrackingTip;
protected
	procedure OnCreate (); override;
   procedure OnDestroy (); override;
   procedure f_UpdateViewMenu ();
   procedure f_InitNewPagesMenu (h_popup: HMenu);
   procedure f_InitPopupMenu (h_popup: HMenu; c_items: array of integer);
   procedure f_InitViewMenu (h: HMenu);
   procedure f_InitTextToolsMenu (h: HMenu);
   procedure f_InitSubsequentFindMenu (h: HMenu);
	procedure f_DetermineDeleteOrClosePage ();
	procedure f_DetermineDeleteOrRemoveItem ();
   function f_DetermineAddOrRemoveFavorite (): integer;
public
	procedure LanguageChanged(); virtual;
	procedure UpdateListMenu (h_pmenu: HMENU; pid: integer);
   function ProcessMessage (ASender: TxlWinControl; AMessage, wparam, lparam: DWORD; var b_processed: boolean): DWORD;
end;

type TMainMenu = class (TMenuSuper)
private
   Fpopup, Fhinsertcliptext, Fhinserttemplate, Fhfavorite, Fhview, FhTExtTools, FhSubsequentFind, FhWorkSpace, FhEdit: HMenu;
   FhrecentRoot, FhRecentCreate, FhRecentModify, FhRecentVisit: HMenu;
	procedure Initialize (b_asmainmenu: boolean);
public
	procedure LanguageChanged(); override;
   procedure InitMenuPopup (h_menu: HMenu); override;
end;

type TTrayMenu = class (TMenuSuper, IEventObserver)
private
	Fhtraypopup, Fhopenfastlink, Fhinsertcliptext, Fhinserttemplate: HMENU;
   FWinStatus: word;
	procedure f_SetRestoreItem ();
protected
	procedure OnCreate (); override;
   procedure OnDestroy (); override;
public
	procedure LanguageChanged(); override;
	procedure InitMenuPopup (h_menu: HMenu); override;
   procedure EventNotify (event, wparam, lparam: integer);
end;

type TPopupMenu = class (TMenuSuper, IEventObserver)
private
   FhPopup: HMenu;
protected
	procedure OnCreate (); override;
	procedure OnDestroy (); override;
public
   procedure EventNotify (event, wparam, lparam: integer);
//   procedure InitMenuPopup (h_menu: HMenu); override;
end;

type TAccelTable = class (TxlAccelTable)
protected
	procedure OnInitialize (); override;
end;

type TMenuManager = class (TxlInterfacedObject, IOptionObserver, IMemorizer, ICommandExecutor, IEventObserver, ISizeObserver)
private
	FWndParent: TxlWindow;
   FMainMenu: TMainMenu;
   FSysMenu: TMainMenu;
   FAccelTable: TxlAccelTable;
   FPopupMenu: TPopupMenu;
   FToolBar: TxlToolBar;
   FTrayMenu: TTrayMenu;
   FTrayIcon: TxlTrayIcon;
   procedure f_OnTrayIconClick (sender: TObject);
   procedure f_OnToolBarContextMenu (sender: TObject);
public
	constructor Create (WndParent: TxlWindow);
   destructor Destroy (); override;
	procedure OptionChanged ();
   procedure SaveMemory ();
   procedure RestoreMemory ();
   procedure ExecuteCommand (opr: word);
   function CheckCommand (opr: word): boolean;
   procedure EventNotify (event, wparam, lparam: integer);
   procedure AdjustAppearance ();
   property TrayMenu: TTrayMenu read FTrayMenu;
end;

implementation

uses Messages, Resource, UDialogs, UDefineToolbarBox, UxlFunctions, UxlStrUtils, UxlMath, UOptionManager, ULangManager, UPageSuper,
	UxlListView, UPageProperty, UPageStore, UxlCommDlgs;

const c_tooltipwidth = 400;

procedure TMenuSuper.OnCreate ();
var o_style: TToolTipStyle;
begin
   with o_style do
   begin
   	TipWidth := c_tooltipwidth;
      InitialTime := 500;
      AutoPopTime := 0;
   end;
   FMenuTip := TxlTrackingTip.Create (FWndParent);
	FMenuTip.Style := o_style;

	LangMan.AddObserver (self);
   CommandMan.AddSender (self);
   FWndParent.AddMessageObserver (self);
   inherited;
end;

procedure TMenuSuper.OnDestroy ();
begin
	FWndParent.RemoveMessageObserver (self);
	LangMan.RemoveObserver (self);
   CommandMan.RemoveSender (self);
   FMenuTip.Free;
   inherited;
end;

function TMenuSuper.ProcessMessage (ASender: TxlWinControl; AMessage, wparam, lparam: DWORD; var b_processed: boolean): DWORD;
var id, i_index: integer;
	rt: TRect;
   s_tip: widestring;
   pt: TPoint;
begin
	b_processed := false;
	if AMessage = WM_MENUSELECT then
   begin
   	id := LoWord(wparam);
      if ListCenter.GetSubItem (id, i_index, s_tip, 200) then
		begin
         // calculate suitable place
         GetMenuItemRect (0, lparam, i_index, rt);
         if FWndParent.StayOnTop and (FWndParent.Status <> wsMinimize) then   // 此种情况下不能将 Tooltip StayOnTop，否则菜单条会转到后台。因此 ToolTip 只能显示在菜单右侧以防被遮没
         begin
            pt.x := rt.Right + 5;
            pt.y := rt.Top;
            FMenuTip.ShowTip (pt, s_tip);
         end
         else
         begin
            GetCursorPos (pt);
            if PointInRect (pt, rt) then
               FMenuTip.ShowTip (s_tip)    // 不规定位置，待Tip浮现时实时取位置
            else
            begin    // 针对不用鼠标，而用键盘触发 wm_menuselect 的场合，将 Tip 显示在菜单条下侧
               pt.x := (rt.Left + rt.Right) div 2;
               pt.y := rt.Bottom;
               FMenuTip.ShowTip (pt, s_tip);
            end;
         end;
         b_processed := true;
      end
      else
      	FMenuTip.HideTip;
   end;
end;

procedure TMenuSuper.LanguageChanged ();
var i: integer;
	o_list: TxlIntList;
begin
	o_list := TxlIntList.Create;
   GetItemList (Fhandle, o_list);
   for i := o_List.Low to o_List.High do
      ItemText[o_list[i]] := LangMan.GetItem(o_list[i]);
   o_list.free;
end;

procedure TMenuSuper.UpdateListMenu (h_pmenu: HMENU; pid: integer);
var i, m, n, id: integer;
	o_list: TxlStrList;
begin
   o_list := TxlStrList.Create;
   ListCenter.FillList (pid, o_list);

   m := o_list.count;
   n := GetItemCount (h_pmenu);
   for i := 0 to m - 1 do
   begin
      id := pid + i + 1;
      if i < n then
         ItemText[id] := o_list[i]
      else
         AddItem(h_pmenu, id, o_list[i]);
   end;
   for i := m to n - 1 do
      Deleteitem (pid + i + 1);

   o_list.free;
end;

procedure TMenuSuper.f_UpdateViewMenu ();
var v: TListPageView;
	b: boolean;
begin
   b := PageCenter.ActivePage.ListProperty <> nil;
   if b then
      v := PageCenter.ActivePage.ListProperty.View;
   ItemChecked[rb_icon] := b and (v = lpvIcon);
   ItemChecked[rb_smallicon] := b and (v = lpvSmallIcon);
   ItemChecked[rb_list] := b and (v = lpvList);
   ItemChecked[rb_report] := b and (v = lpvReport);
   ItemChecked[rb_blog] := b and (v = lpvBlog);
end;

procedure TMenuSuper.f_InitNewPagesMenu (h_popup: HMenu);
begin
   ClearPopup (h_popup);
   with OptionMan.Options do
   begin
      if EnableCalcPage then AddItem (h_popup, m_newcalc, CommandMan.ItemText[m_newcalc]);
      if EnableMemoPage then AddItem (h_popup, m_newmemo, CommandMan.ItemText[m_newmemo]);
      if EnableDictPage then AddItem (h_popup, m_newdict, CommandMan.ItemText[m_newdict]);
      if EnableLinkPage then AddItem (h_popup, m_newlink, CommandMan.ItemText[m_newlink]);
      if EnableContactPage then AddItem (h_popup, m_newcontact, CommandMan.ItemText[m_newcontact]);
   end;
end;

procedure TMenuSuper.f_InitPopupMenu (h_popup: HMenu; c_items: array of integer);
var i: integer;
begin
   ClearPopup (h_popup);
   for i := Low(c_items) to High(c_items) do
   	if c_items[i] = 0 then
      	AddSeparator (h_popup)
      else
      	AddItem (h_popup, c_items[i], CommandMan.ItemText[c_items[i]]);
end;

procedure TMenuSuper.f_InitViewMenu (h: HMenu);
begin
	f_InitPopupMenu (h, [rb_icon, rb_smallicon, rb_list, rb_report, rb_blog]);
end;

procedure TMenuSuper.f_InitTextToolsMenu (h: HMenu);
begin
	f_InitPopupMenu (h, [m_highlight1, m_highlight2, m_highlight3, m_highlight4, m_removehighlight, 0, m_ol, m_ul1, m_ul2, m_removelist, 0, m_noemptyline, m_oneemptyline]);
end;

procedure TMenuSuper.f_InitSubsequentFindMenu (h: HMenu);
begin
	f_InitPopupMenu (h, [m_findnext, m_findprevious, m_replace, m_replace_p, m_replaceall]);
end;

procedure TMenuSuper.f_DetermineDeleteOrClosePage ();
begin
	if PageCenter.ActivePage = nil then exit;
   if PageCenter.ActivePage.SingleInstance then
      ItemText[m_deletepage] := LangMan.GetItem (m_closepage)
   else
      ItemText[m_deletepage] := LangMan.GetItem (m_deletepage);
end;

procedure TMenuSuper.f_DetermineDeleteOrRemoveItem ();
var p: TPageSuper;
begin
	p := PageCenter.ActivePage;
	if p = nil then exit;
   if p.PageControl = pcEdit then
   	ItemTExt[m_delete] := LangMan.GetItem (m_delete)
	else if p.IsVirtualContainer then
      ItemText[m_delete] := LangMan.GetItem (m_removeitem)
   else
      ItemText[m_delete] := LangMan.GetItem (m_deleteitem);
end;

function TMenuSuper.f_DetermineAddOrRemoveFavorite (): integer;
var id: integer;
begin
	if PageCenter.ActivePage = nil then exit;
   PageStore.GetFirstPageId(ptFavorite, id);
	if PageStore[id].Childs.FindChild(PageCenter.ActivePage.id) >= 0 then
   	result := m_removefavorite
   else
   	result := m_addfavorite;
end;

//-------------------

procedure TMainMenu.Initialize (b_asmainmenu: boolean);
var hm, h, hParent: HMenu;
begin
	if b_asMainMenu then
   	hparent := self.Handle
   else
   begin
   	FPopup := AddPopup (self.handle, m_popup);
      hParent := FPopup;
   end;

   hm := AddPopup (hParent, mc_workspace);
   FhWorkSpace := hm;
      AddItem (hm, m_newnote);
      if OptionMan.HaveExtPage then
      begin
         h := AddPopup (hm, m_newpage);
            f_InitNewPagesMenu (h);
      end;
      AddItem (hm, m_newgroup);

      AddSeparator (hm);
      AddItem (hm, m_rename);
      AddItem (hm, m_switchlock);
      AddItem (hm, m_save);
      AddItem (hm, m_deletepage);

      AddSeparator (hm);
      AddItem (hm, m_property);
      Fhview := AddPopup (hm, m_view);
      f_InitViewMenu (Fhview);

      AddSeparator (hm);
      AddItem (hm, m_import);
      AddItem (hm, m_export);
      AddItem (hm, m_sendmail);

      AddSeparator (hm);
      AddItem (hm, m_exit);

   hm := AddPopup (hParent, mc_edit);
   FhEdit := hm;
      AddItem (hm, m_clear);
      AddItem (hm, m_undo);
      AddItem (hm, m_redo);
      AddItem (hm, m_selectall);

      AddSeparator (hm);
      AddItem (hm, m_cut);
      AddItem (hm, m_copy);
      AddItem (hm, m_paste);
      AddItem (hm, m_delete);

      AddSeparator (hm);
      AddItem (hm, m_newitem);
      AddItem (hm, m_insertitem);
      AddItem (hm, m_edititem);

      AddSeparator (hm);
      AddItem (hm, m_wordwrap);
      FhTextTools := AddPopup (hm, m_texttools);
      f_InitTextToolsMenu (FhTextTools);

      AddSeparator (hm);
      AddItem (hm, m_find);
      FhSubsequentFind := AddPopup (hm, m_subsequentfind);
      f_InitSubsequentFindMenu (FhSubsequentFind);
      AddItem (hm, m_highlightmatch);

      AddSeparator (hm);
      AddItem (hm, m_insertlink);
      with OptionMan.Options do
      begin
         if EnableTemplate then
         	Fhinserttemplate := AddPopup (hm, m_inserttemplate);
         if EnableClipboard then Fhinsertcliptext := AddPopup (hm, m_insertcliptext);
      end;

   hm := AddPopup (hParent, mc_navigation);
      AddItem (hm, m_prior);
      AddItem (hm, m_next);
      AddItem (hm, m_levelup);
      AddSeparator (hm);

      h := AddPopup (hm, m_recentroot);
         FhRecentCreate := AddPopup (h, m_recentcreate);
         FhRecentModify := AddPopup (h, m_recentModify);
         FhRecentVisit := AddPopup (h, m_recentVisit);
         AddSeparator (h);
         AddItem (h, m_managerecent);
      Fhrecentroot := h;

   	Fhfavorite := AddPopup (hm, m_favorite);
      AddSeparator (hm);

      AddItem (hm, m_search);
//      AddItem (hm, m_tag);
      AddItem (hm, m_recyclebin);

   hm := AddPopup (hParent, mc_tools);
      AddItem (hm, m_showtree);
      AddItem (hm, m_stayontop);
      AddItem (hm, m_transparent);
      AddItem (hm, m_specialmode);
      with OptionMan.Options do
      begin
         if EnableTemplate or EnableLinkPage then AddSeparator (hm);
         if EnableTemplate then AddItem (hm, m_template);
         if EnableLinkPage then AddItem (hm, m_fastlink);
         if EnableClipboard then
         begin
            AddSeparator (hm);
            AddItem (hm, m_watchclipboard);
            AddItem (hm, m_clearclipboard);
            AddItem (hm, m_clipboard);
         end;
      end;
      AddSeparator (hm);
      AddItem (hm, m_statistics);
      AddSeparator (hm);
      AddItem (hm, m_definetoolbar);
      AddItem (hm, m_options);

   hm := AddPopup (hParent, mc_help);
      AddItem (hm, m_helptopic);
      AddSeparator (hm);
      AddItem (hm, m_homepage);
      AddItem (hm, m_forum);
      AddSeparator (hm);
      AddItem (hm, m_donate);
      AddItem (hm, m_about);
   LanguageChanged;
end;

procedure TMainMenu.LanguageChanged ();
begin
	inherited;
   if FPopup = 0 then  // as main menu
   	FWndParent.Redraw;
end;

procedure TMainMenu.InitMenuPopup (h_menu: HMenu);
var m: integer;
begin
   if (h_menu = FPopup) then
   begin
   	InitMenuPopup (Fhinsertcliptext);
      InitMenuPopup (Fhinserttemplate);
   end
	else if (h_menu = Fhinsertcliptext) then
   	UpdateListMenu (h_menu, m_insertcliptext)
   else if h_menu = Fhinserttemplate then
   	UpdateListMenu (h_menu, m_inserttemplate)
//   else if h_menu = FhrecentRoot then
//   begin
//      UpdateListMenu (FhrecentCreate, m_recentcreate);
//   	UpdateListMenu (FhrecentModify, m_recentmodify);
//   	UpdateListMenu (FhrecentVisit, m_recentvisit);

   else if h_menu = FhrecentCreate then
      UpdateListMenu (h_menu, m_recentcreate)
   else if h_menu = FhrecentModify then
   	UpdateListMenu (h_menu, m_recentmodify)
   else if h_menu = FhrecentVisit then
   	UpdateListMenu (h_menu, m_recentvisit)
   else if h_menu = Fhfavorite then
   begin
   	ClearPopup (h_menu);
   	UpdateListMenu (h_menu, m_favorite);
      AddSeparator (h_menu);
      m := f_DetermineAddOrRemoveFavorite;
      AddItem (h_menu, m, #9 + LangMan.GetItem (m));
      AddItem (h_menu, m_managefavorite, #9 + LangMan.GetItem (m_managefavorite));
   end
   else if h_menu = Fhview then
   	f_UpdateViewMenu
   else if h_menu = FhWorkSpace then
   	f_DetermineDeleteOrClosePage
   else if h_menu = FhEdit then
   	f_DetermineDeleteOrRemoveItem;
end;

//----------------------

procedure TTrayMenu.OnCreate ();
var hm: HMenu;
begin
	hm := AddPopup (Fhandle, m_tray);
      with OptionMan.Options do
      begin
         if EnableLinkPage then
         begin
            Fhopenfastlink := AddPopup (hm, m_openfastlink);
            AddSeparator (hm);
         end;
         if EnableTemplate then Fhinserttemplate := AddPopup (hm, m_inserttemplate);
         if EnableClipboard then
         begin
            Fhinsertcliptext := AddPopup (hm, m_insertcliptext);
            AddItem (hm, m_watchclipboard);
            AddItem (hm, m_clearclipboard);
         end;
         if EnableTemplate or EnableClipboard then AddSeparator (hm);
      end;
      AddItem (hm, m_newnoteforeground);
      AddItem (hm, m_autorecord);
      AddItem (hm, m_options);
      AddSeparator (hm);
      AddItem (hm, m_restore);
      AddItem (hm, m_exit);
   Fhtraypopup := hm;
   EventMan.AddObserver (self);
   inherited;
end;

procedure TTrayMenu.OnDestroy ();
begin
	EventMan.RemoveObserver (self);
   inherited;
end;

procedure TTrayMenu.InitMenuPopup (h_menu: HMenu);
begin
	if h_menu = Fhtraypopup then
   begin
   	InitMenuPopup (Fhinsertcliptext);
      InitMenuPopup (Fhinserttemplate);
      InitMenuPopup (Fhopenfastlink);
      ItemEnabled[m_inserttemplate] := true;
      ItemEnabled[m_insertcliptext] := true;
   end
   else if h_menu = Fhinsertcliptext then
   	UpdateListMenu (h_menu, m_insertcliptext)
   else if h_menu = Fhinserttemplate then
   	UpdateListMenu (h_menu, m_inserttemplate)
   else if h_menu = Fhopenfastlink then
   	UpdateListMenu (h_menu, m_openfastlink);
end;

procedure TTrayMenu.f_SetRestoreItem ();
begin
   if FWinStatus = p_Minimized then
      ItemText[m_restore] := LangMan.GetItem (m_restore)
   else
      ItemText[m_restore] := LangMan.GetItem (m_minimize);
end;

procedure TTrayMenu.LanguageChanged ();
begin
	inherited;
   f_SetRestoreItem ();
end;

procedure TTrayMenu.EventNotify (event, wparam, lparam: integer);
begin
	if event = e_WinStatusChanged then
   begin
      case wparam of
         p_minimized, p_maximized, p_restored:
            begin
               FWinStatus := wparam;
               f_SetRestoreItem ();
            end;
      end;
   end;
end;

//--------------------------------

procedure TPopupMenu.OnCreate ();
begin
	inherited;
   FhPopup := AddPopup(FHandle, m_popup);
   EventMan.AddObserver(self);
end;

procedure TPopupMenu.OnDestroy ();
begin
	EventMan.RemoveObserver (self);
   inherited;
end;

const
   TreeMenus: array [0..8] of integer = (m_newnote, m_newgroup, 0, m_rename, m_switchlock, m_property, 0, m_export, m_deletepage);
   EditorMenus: array[0..7] of integer = (m_cut, m_copy, m_paste, m_delete, 0, m_find, 0, m_insertlink);
   EditorNoSelMenus: array[0..11] of integer = (m_undo, m_redo, m_selectall, 0, m_wordwrap, m_removehighlight, 0, m_find, m_highlightmatch, 0, m_paste, m_insertlink);
   ListMenus: array[0..3] of integer = (m_newitem, m_insertitem, m_edititem, m_delete);
   ListNoSelMenus: array[0..2] of integer = (m_newitem, 0, m_property);
   ToolbarMenus: array[0..0] of integer = (m_definetoolbar);
//   FavoriteListMenus: array[0..1] of integer = (m_edititem, m_removeitem);
   SearchMenus: array[0..4] of integer = (m_search, 0, m_property, 0, m_deletepage);
   RecycleBinMenus: array[0..4] of integer = (m_clear, 0, m_property, 0, m_deletepage);
	SysPageMenus: array[0..2] of integer = (m_property, 0, m_deletepage);
   RecentPagesMenus: array[0..0] of integer = (m_property);

procedure TPopupMenu.EventNotify (event, wparam, lparam: integer);
	procedure f_PopulatePopupMenu (const i_menuitems: array of integer);
	var i, m: integer;
   begin
		ClearPopup (FhPopup);
   	for i := Low(i_menuitems) to High(i_menuitems) do
      begin
      	m := i_menuitems[i];
         if m = 0 then
            AddSeparator (FhPopup)
         else
         begin
            AddItem(FhPopup, m, LangMan.GetItem(m));  //.GetItem(m));
            ItemEnabled[m] := CommandMan.ItemEnabled[m];
         end;
      end;
   end;
   procedure f_InsertPopup (i_pos: integer; m_menu: integer);
	var h: HMenu;
   begin
   	h := InsertPopup (FhPopup, i_pos, m_menu, CommandMan.ItemText[m_menu]);
      case m_menu of
      	m_view:
         	begin
            	f_InitViewMenu (h);
               f_UpdateViewMenu;
            end;
         m_texttools: f_InitTextToolsMenu (h);
         m_subsequentfind: f_InitSubsequentFindMenu (h);
         m_inserttemplate, m_insertcliptext:
         	UpdateListMenu (h, m_menu);
      end;
   end;
var h: HMenu;
	m: integer;
begin
	if event <> e_ContextMenuDemand then exit;

//   FHasViewMenu := wparam in [PageContext, SearchContext, RecycleBinContext, SysPageContext, RecentPagesContext, ListNoSelContext];
	case wparam of
   	PageContext:
      	begin
	      	f_PopulatePopupMenu (TreeMenus);
            if PageCenter.ActivePage.PageControl <> pcEdit then
            	f_InsertPopup (6, m_view);
            if OptionMan.HaveExtPage then
            begin
               h := InsertPopup (FhPopup, 1, m_newpage, CommandMan.ItemText[m_newpage]);
               f_InitNewPagesMenu (h);
            end;
            m := f_DetermineAddOrRemoveFavorite;
            InsertItem (FhPopup, 9, m, LangMan.GetItem(m));
         end;
      SearchContext:
      	begin
	      	f_PopulatePopupMenu (SearchMenus);
            f_InsertPopup (3, m_view);
         end;
      RecycleBinContext:
      	begin
      		f_PopulatePopupMenu (RecycleBinMenus);
            f_InsertPopup (3, m_view);
         end;
      SysPageContext:
      	begin
	      	f_PopulatePopupMenu (SysPageMenus);
            f_InsertPopup (1, m_view);
         end;
      RecentPagesContext:
      	begin
      		f_PopulatePopupMenu (RecentPagesMenus);
            f_InsertPopup (1, m_view);
         end;
      EditorContext:
      	begin
				f_PopulatePopupMenu (EditorMenus);
            f_InsertPopup (5, m_texttools);
            if OptionMan.Options.EnableClipboard then
            	f_InsertPopup (9, m_insertcliptext);
            if OptionMan.Options.EnableTemplate then
            	f_InsertPopup (9, m_inserttemplate);
         end;
      EditorNoSelContext:
      	begin
	      	f_PopulatePopupMenu (EditorNoSelMenus);
            f_InsertPopup (8, m_subsequentfind);
            ItemChecked[m_wordwrap] := CommandMan.ItemChecked[m_wordwrap];
            if OptionMan.Options.EnableClipboard then
            	f_InsertPopup (13, m_insertcliptext);
            if OptionMan.Options.EnableTemplate then
            	f_InsertPopup (13, m_inserttemplate);
         end;
      ListContext:
			f_PopulatePopupMenu (ListMenus);
      ListnoselContext:
      	begin
				f_PopulatePopupMenu (ListNoSelMenus);
            f_InsertPopup (3, m_view);
         end;
//      FavoriteListContext:
//        	f_PopulatePopupMenu (FavoriteListMenus);
      ToolbarContext:
	     	f_PopulatePopupMenu (ToolbarMenus);
      else
      	exit;
   end;

	f_DetermineDeleteOrClosePage;
   f_DetermineDeleteOrRemoveItem;

   EventMan.EventNotify (WM_ENTERMENULOOP);
   Popup (m_popup);
   EventMan.EventNotify (WM_EXITMENULOOP);
end;

//--------------------------

procedure TAccelTable.OnInitialize ();
var s_file: widestring;
begin
   AddKey (m_newnote, 'Ctrl+N');
   AddKey (m_newgroup, 'Ctrl+G');
   AddKey (m_rename, 'F2');
   AddKey (m_switchlock, 'F6');
   AddKey (m_save, 'Ctrl+S');
   AddKey (m_deletepage, 'Shift+Del');
   AddKey (m_moveup, 'Alt+Up');
   AddKey (m_movedown, 'Alt+Down');
   
   AddKey (m_undo, 'Ctrl+Z', false);
   AddKey (m_redo, 'Ctrl+Y', false);
   AddKey (m_selectall, 'Ctrl+A', false);
   AddKey (m_cut, 'Ctrl+X', false);
   AddKey (m_copy, 'Ctrl+C', false);
   AddKey (m_paste, 'Ctrl+V', false);
   AddKey (m_clear, 'Ctrl+L');

   AddKey (m_find, 'Ctrl+F');
   AddKey (m_findnext, 'F3');
   AddKey (m_findprevious, 'Shift+F3');
   AddKey (m_replace, 'F4');
   AddKey (m_replace_p, 'Shift+F4');
   AddKey (m_replaceall, 'Ctrl+F4');

   AddKey (m_newitem, 'Ctrl+I');
   AddKey (m_insertlink, 'Ctrl+K');
   AddKey (m_delete, 'Del', false);

   AddKey (m_prior, 'Alt+Left');
   AddKey (m_next, 'Alt+Right');
   AddKey (m_levelup, 'Ctrl+U');
   AddKey (m_search, 'Ctrl+Shift+F');

   AddKey (m_showtree, 'Ctrl+T');
   AddKey (m_stayontop, 'Ctrl+P');
   AddKey (m_transparent, 'Ctrl+R');
   AddKey (m_specialmode, 'Ctrl+H');

   AddKey (m_helptopic, 'F1');
   AddKey (hk_switchfocus, 'Ctrl+Tab');
   AddKey (hk_selastitle, 'F10');

   AddKey (hk_treeleft, 'Ctrl+Alt+Left');
   AddKey (hk_treeright, 'Ctrl+Alt+Right');

   LoadFromFile (ProgDir + 'accel.txt');

   CreateAccelTable;
end;

//-----------------------------

const
	defbuttons: array [0..20] of integer = (m_newnote, m_newpage, m_newgroup, 0, m_switchlock, m_property, m_view, 0,
   	m_undo, m_texttools, m_find, m_newitem, 0, m_prior, m_next, m_levelup, m_favorite, 0, m_showtree, m_stayontop, m_specialmode);

	allbuttons: array [0..59] of integer = (m_newnote, m_newpage, m_newcalc, m_newmemo, m_newdict, m_newlink, m_newcontact, m_newgroup,
   	m_rename, m_switchlock, m_save, m_deletepage, m_property, m_view, m_import, m_export, m_sendmail, m_exit,
      m_clear, m_undo, m_redo, m_selectall, m_cut, m_copy, m_paste, m_delete, m_newitem, m_insertitem, m_edititem, m_wordwrap,
      m_texttools, m_find, m_subsequentfind, m_insertlink, m_inserttemplate, m_insertcliptext,
      m_prior, m_next, m_levelup, m_recentcreate, m_recentmodify, m_recentvisit, m_favorite, m_search, m_recyclebin,
      m_showtree, m_stayontop, m_transparent, m_specialmode, m_template, m_fastlink, m_watchclipboard, m_clearclipboard, m_clipboard,
      m_statistics, m_definetoolbar, m_options, m_helptopic, m_homepage, m_forum); 

constructor TMenuManager.Create (WndParent: TxlWindow);
begin
   FWndParent := WndParent;
	FAccelTable := TAccelTable.Create ();

   FMainMenu := TMainMenu.create (WndParent);
   FAccelTable.AssociateMenu (FMainMenu);
   FMainMenu.Initialize(true);

   FSysMenu := TMainMenu.Create(FWndParent);
   FSysMenu.Initialize(false);
   FSysMenu.AccelTable := FAccelTable;

   FToolBar := TxlToolBar.create (WndParent);
   with FToolBar do
   begin
   	AssociateMenu (FMainMenu);
      OnContextMenu := f_OnToolBarContextMenu;
   end;

   FPopupMenu := TPopupMenu.create (WndParent);

   FTrayMenu := TTrayMenu.create (WndParent);
   FTrayIcon := TxlTrayIcon.Create (WndParent, MainIcon, 'minipad2');
   with FTrayIcon do
   begin
   	Menu := FTrayMenu;
   	OnClick := f_OnTrayIconClick;
   end;

   with WndParent do
   begin
   	SetAccelTable(FAccelTable);
   	SetToolbar (FToolbar);
   end;

   OptionMan.AddObserver (self);
	MemoryMan.AddObserver (self);
   CommandMan.AddExecutor(self);
   EventMan.AddObserver (self);
   SizeMan.AddObserver (self);
   CommandMan.AddSender (FSysMenu);
end;

destructor TMenuManager.Destroy ();
begin
	SizeMan.RemoveObserver (self);
	EventMan.RemoveObserver (self);
	OptionMan.RemoveObserver (self);
	MemoryMan.RemoveObserver(self);
   CommandMan.RemoveExecutor(self);

   with FWndParent do
   begin
      SetAccelTable(nil);
   	SetToolbar (nil);
		SetMainMenu(nil);
      SetTrayIcon (nil);
	end;

	FToolBar.free;
  	FPopupMenu.free;
  	FSysMenu.Free;
   FMainMenu.free;
   FTrayIcon.Free;
   FTrayMenu.free;
   FAccelTable.Free;
   inherited;
end;

procedure TMenuManager.AdjustAppearance ();
var b: boolean;
begin
	if (OptionMan.options.showmenubar) and SizeMan.ShowMenu then
   	FWndParent.SetMainMenu(FMainMenu)
   else
   	FWndParent.SetMainMenu(nil);

   b := OptionMan.options.showtoolbar and SizeMan.ShowToolbar;
   FToolBar.visible := b;
end;

procedure TMenuManager.EventNotify (event, wparam, lparam: integer);
begin
	case event of
   	e_ContextMenuDemand:
      	if (wparam = ProgramContext) then FSysMenu.Popup;
   end;
end;

procedure TMenuManager.OptionChanged ();
begin
	AdjustAppearance;
	if OptionMan.Options.ShowTrayIcon then
		FWndParent.SetTrayIcon (FTrayIcon)
   else
   	FWndParent.SetTrayIcon (nil);
end;

procedure TMenuManager.SaveMemory ();
begin
end;

procedure TMenuManager.RestoreMemory ();
var i: integer;
	o_list: TxlIntList;
begin
	o_list := TxlIntList.Create ();
   o_list.Separator := ',';
	o_list.Text := MemoryMan.TBList;
   if o_list.count = 0 then
   	for i := low(defbuttons) to high(defbuttons) do
         o_list.add (defbuttons[i]);
	for i := o_list.High downto o_list.Low do
   	if not FMainMenu.ItemExists(o_list[i]) then o_list.Delete(i);
   for i := o_list.High - 1 downto o_list.Low do
   	if o_list[i] = o_list[i + 1] then o_list.Delete(i + 1);
   FToolBar.SetButtons (o_list);
   o_list.free;
end;

procedure TMenuManager.ExecuteCommand (opr: word);
	procedure f_DefineToolBar ();
   var o_currentlist, o_alllist, o_defaultlist: TxlIntList;
      i: integer;
   begin
      o_currentlist := TxlIntList.Create ();
      o_currentlist.Separator := ',';
      FToolBar.GetButtons (o_currentlist);

      o_alllist := TxlIntList.create;
      for i := Low(allbuttons) to high(allbuttons) do
         if FMainMenu.ItemExists(allbuttons[i]) then o_alllist.add (allbuttons[i]);

      o_defaultlist := TxlIntList.Create;
      for i := Low(defbuttons) to high(defbuttons) do
      	o_defaultlist.Add(defbuttons[i]);

      with TDefineToolBarbox.create (FWndParent) do
      begin
         Initialize (o_currentlist, o_alllist, o_defaultlist);      // 对象指针传入
         if Execute () then
         begin
            MemoryMan.TBList := o_currentlist.text;
            RestoreMemory;
            FWndParent.Update;
         end;
         Free;
      end;
      o_currentlist.Free;
      o_alllist.free;
   end;
begin
	case opr of
   	m_definetoolbar: f_DefineToolBar;
//      m_showtree, m_stayontop, m_transparent, m_specialmode, m_wordwrap:
//      	CommandMan.SwitchCheck(opr);
   end;
end;

function TMenuManager.CheckCommand (opr: word): boolean;
begin
	result := true;
end;

procedure TMenuManager.f_OnTrayIconClick (sender: TObject);
begin
  	EventMan.EventNotify (e_TrayIconClicked);
end;

procedure TMenuManager.f_OnToolBarContextMenu (sender: TObject);
begin
	EventMan.EventNotify(e_ContextMenuDemand, ToolbarContext);
end;

end.










