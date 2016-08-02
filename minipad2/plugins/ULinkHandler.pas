unit ULinkHandler;

interface

uses Windows, UxlClasses, UxlExtClasses, UxlWindow, UxlList, UPageSuper, ULinkPage, USysPageHandler, UTypeDef, UxlWinClasses, UxlWinControl;

type
   TLinkHandler = class (TxlInterfacedObject, IEventObserver, IOptionObserver, IHotkeyOwner, IMessageObserver)
   private
   	FParent: TxlWindow;
		FAutoMinimizeAfterOpenLink: boolean;
      procedure Execute (id: integer);
		function f_GetLink (id: integer): TLinkProperty;
   public
      constructor Create (AWindow: TxlWindow);
      destructor Destroy (); override;

      procedure EventNotify (event, wparam, lparam: integer);
      procedure OptionChanged ();
      procedure OnHotkey (id: integer; hk: THotkey);
      function ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
   end;

   TFastLinkHandler = class (THandlerWithSubHotKey)
   private
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
      procedure ExecuteItem (id: integer); override;
   public
      procedure OptionChanged (); override;
   end;

procedure ExecBatchLink (const lnk: widestring);

implementation

uses Messages, SysUtils, ShellAPI, UxlFunctions, UxlCommDlgs, UxlStrUtils, UPageStore, UGlobalObj, UOptionManager,
	UPageFactory, Resource;

procedure ExecLink (const lnk: widestring);
var s_link, s_dir, s_param: widestring;
begin
   if lnk = '' then exit;
   DecryptLink (Lnk, s_link, s_dir, s_param);
   try
      ExecuteLink (s_link, s_dir, s_param);
   except
      on E: Exception do
         ShowMessage ('链接 ' + Lnk + ' 执行失败！' + #13#10 + '原因：' + E.Message);
   end;
end;

procedure ExecBatchLink (const lnk: widestring);
var i: integer;
	o_list: TxlStrList;
begin
   o_list := TxlStrList.Create;
   o_list.Text := Lnk;
   for i := o_list.Low to o_list.High do
   begin
      ExecLink (o_list[i]);
      sleep (20);
   end;
   o_list.Free;
end;

//-------------------------

constructor TLinkHandler.Create (AWindow: TxlWindow);
var i: integer;
	o_list: TxlIntList;
begin
   o_list := TxlIntList.Create;
   PageStore.GetPageList (ptLinkItem, o_list);

   for i := o_list.Low to o_list.High do
      if f_GetLink (o_list[i]).HotKey <> 0 then
      	HotkeyCenter.AddHotKey (self, o_list[i], f_GetLink (o_list[i]).HotKey);
   o_list.free;

   FParent := AWindow;
   EventMan.AddObserver (self);
   OptionMan.AddObserver (self);
   AWindow.AddMessageObserver (self);
end;

destructor TLinkHandler.Destroy ();
begin
	FParent.RemoveMessageObserver (self);
	OptionMan.RemoveObserver (self);
	EventMan.RemoveObserver (self);
   HotkeyCenter.RemoveOwner (self);
   inherited;
end;

function TLinkHandler.f_GetLink (id: integer): TLinkProperty;
begin
	result := TLinkItem(PageStore[id]).Link;
end;

procedure TLinkHandler.OptionChanged ();
begin
	FAutoMinimizeAfterOpenLink := OptionMan.Options.LinkOptions.AutoMinimizeAfterOpenLink;
end;

procedure TLinkHandler.OnHotkey (id: integer; hk: THotkey);
begin
   if PageStore.PageValid (id) and (PageStore[id].PageType = ptLinkItem) then
   	Execute (id);
end;

procedure TLinkHandler.EventNotify (event, wparam, lparam: integer);
begin
	case event of
   	e_ExecuteLink:
			Execute (wparam);
   	e_LinkHotKeyChanged:
         HotkeyCenter.AddHotkey (self, wparam, lparam);
      e_LinkItemDeleted:
        	HotkeyCenter.RemoveHotkey (self, wparam);
   end;
end;

procedure TLinkHandler.Execute (id: integer);
var s_link: widestring;
begin
   with f_GetLink(id) do
      if linkType = ltBatch then
         ExecBatchLink (Link)
      else
         ExecLink (GetFullLink(Link, LinkType, false, true));
   if FAutoMinimizeAfterOpenLink and (Fparent.Status <> wsMinimize) then
      FParent.Status := wsMinimize;
end;

function TLinkHandler.ProcessMessage (ASender: TxlWinControl; AMessage, wParam, lParam: DWORD; var b_processed: boolean): DWORD;
	procedure f_ProcessDropFiles (o_parent: TPageSuper; h_drop: HDrop);
	var i, i_count, id: integer;
   	buffer: array[0..2000] of widechar;
		s_link, s_pathfile, s_desc: widestring;
      o_link: TLinkItem;
   begin
   	i_count := DragQueryFileW (h_drop, $FFFFFFFF, nil, 0);
      for i := 0 to i_count - 1 do
      begin
      	DragQueryFileW (h_drop, i, @buffer, sizeof(buffer));

         s_link := buffer;
			s_pathfile := s_link;
         if ExtractFileExt (s_link) = 'lnk' then  // resolve shortcut
            ResolveShortCut (MainWinHandle, s_link, s_pathfile, s_desc);

         id := PageStore.NewPage (o_parent.id, ptLinkItem);
         o_link := TLinkItem (PageStore[id]);
         o_link.Name := ExtractFileName (s_link, false);
         with o_Link.Link do
         begin
            if IsDirectory (s_pathfile) then
               LinkType := ltFolder
            else
               LinkType := ltFile;
            Link := FullToRelPath (s_pathfile, ProgDir);
            HotKey := 0;
            Remark := '';
         end;
         o_parent.Childs.AddChild (id);
      end;
   end;
begin
	result := 0;
   if (AMessage = WM_DROPFILES) and (PageCenter.ActivePage.PageType in [ptLink, ptFastLink]) then
   begin
   	f_ProcessDropFiles (PageCenter.ActivePage, wparam);
      b_processed := true;
   end
   else
   	b_processed := false;
end;

//-------------------

function TFastLinkHandler.PageType (): TPageType;
begin
	result := ptFastLink;
end;

function TFastLinkHandler.NameResource (): integer;
begin
	result := sr_FastLink;
end;

function TFastLinkHandler.MenuItem_Manage (): integer;
begin
	result := m_fastlink;
end;

function TFastLinkHandler.MenuItem_List (): integer;
begin
	result := m_openfastlink;
end;

procedure TFastLinkHandler.ExecuteItem (id: integer);
begin
   EventMan.EventNotify (e_ExecuteLink, id);
end;

procedure TFastLinkHandler.OptionChanged ();
begin
	with OptionMan.Options do
   	AddHotkeyAndSubKey (FastLinkhotkey, EnableFastLinkItemHotkey);
end;

end.
