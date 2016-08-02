unit UWorkSpace;

interface

uses Windows, UxlPanel, UxlClasses, UxlWinControl, UxlMiscCtrls, UxlExtClasses, UxlList, UGlobalObj, UxlEdit, UNavigatorSuper,
	UClientSuper, UTypeDef, UPageSuper, USpace, UxlDragControl, UPageFactory, USysPageManager, UNoteHandler, UStatisticsHandler;

type
   TScrollHandler = class (TxlInterfacedObject, IMessageObserver)
   public
   	function ProcessMessage (ASender: TxlWinControl; AMessage, wparam, lparam: DWORD; var b_processed: boolean): DWORD;
   end;

   TTreeWidthHandler = class
   private
      FPanel: TxlPanel;
      FSplitter: TxlVertSplitter;
      FTree: TxlControl;

      FTreeOnRight: boolean;
      FTreeWidth: integer;

      function GetTreeWidth (): integer;
      procedure SetTreeWidth (value: integer);
      function GetTreeHide (): boolean;
      procedure SetTreeHide (value: boolean);
   public
      constructor Create (APanel: TxlPanel;  ATree: TxlControl; ASplitter: TxlVertSplitter);

      property TreeWidth: integer read GetTreeWidth write SetTreeWidth;
      property TreeHide: boolean read GetTreeHide write SetTreeHide;
      property TreeOnRight: boolean read FTreeOnRight write FTreeOnRight;

      procedure ShowTree ();
      procedure HideTree ();
      procedure ShowTree_Expand ();
      procedure HideTree_Collapse ();
   end;

   TWorkSpace = class (TxlPanel, IMemorizer, IOptionObserver, ISizeObserver, IPageObserver, ICommandExecutor)
   private
      FSplitter: TxlVertSplitter;
      FTree, FTab, FNavigator: TNavigatorSuper;
      FEditor, FList, FBlog, FClient: TClientSuper;
      FSysPageManager: TSysPageManager;
      FNoteHandler: TNoteHandler;
      FSpace: TSpace;
      FScrollHandler: TScrollHandler;
      FStatisticsHandler: TStatisticsHandler;

      FTreeMode: boolean;
      FTreeHandler: TTreeWidthHandler;

      procedure ShowTree (value: boolean);
      procedure f_SetTreeNav ();
      procedure f_OnSplitterButtonclick (Sender: TObject);
      procedure f_OnSplitterButtonRightclick (Sender: TObject);
		procedure f_OnDropItems (o_dragsource: IDragSource; i_targetid, i_pid: integer; b_copy: boolean);
   public
      procedure OnCreate (); override;
      procedure OnDestroy (); override;
		function ProcessMessage (AMessage, wParam, lParam: DWORD): DWORD; override;

      procedure SaveMemory ();
      procedure RestoreMemory ();
      procedure OptionChanged ();
      function CheckCommand (opr: word): boolean;
      procedure ExecuteCommand (opr: word);
      procedure OnPageEvent (pct: TPageEvent; id, id2: integer);
      procedure AdjustAppearance ();
      procedure GetClientSize (var i_width, i_height: integer);
      property TreeMode: boolean read FTreeMode write ShowTree;
   end;

implementation

uses Messages, ShellAPI, UEditor, UList, UBlog, UPageStore, UTabNavigator, UTreeNavigator, Resource, UxlCommDlgs, UxlWindow, UxlTabControl,
	UxlRichEdit, UxlFunctions, UOptionManager;

function TScrollHandler.ProcessMessage (ASender: TxlWinControl; AMessage, wparam, lparam: DWORD; var b_processed: boolean): DWORD;
var o_point: TPoint;
	o_rect: TRect;
   hwin: HWND;
begin
	result := 0;
   b_processed := false;
   if (not OptionMan.Options.ExternalScroll) or (AMessage <> WM_MOUSEWHEEL) then exit;

   GetCursorPos (o_point);
   GetWindowRect (MainWinhandle, o_rect);
   if PointInRect(o_point, o_rect) then exit;

   hwin := WindowFromPoint (o_point);
   if hwin <> 0 then
   begin
      SendMessage (hwin, AMessage, wParam, lParam);
      b_processed := true;
   end;
end;

//-------------------

procedure TWorkSpace.OnCreate ();
begin
	inherited;

   FSpace := TSpace.Create;

   FTree := TTreeNavigator.create (self);
   FTree.OnDropItems := f_OnDropItems;

   FTab := TTabNavigator.create (self);
   FTab.OnDropItems := f_OnDropItems;

   FEditor := TEditorClient.Create (self);

   FList := TListClient.Create (self);
   FList.OnDropItems := f_OnDropItems;

   FBlog := TBlogClient.Create (self);

	FSplitter := TxlVertSplitter.Create (self);
   FSplitter.ShowButton := true;
   FSplitter.OnButtonclick := f_OnSplitterButtonClick;
   FSplitter.OnButtonRightclick := f_OnSplitterButtonRightClick;

	FSysPageManager := TSysPageManager.Create;

   FScrollHandler := TScrollHandler.Create;
   FTree.Control.AddMessageObserver (FScrollHandler);
   FEditor.Control.AddMessageObserver (FScrollHandler);
   FList.Control.AddMessageObserver (FScrollHandler);
   FBlog.Control.AddMessageObserver (FScrollHandler);

   FStatisticsHandler := TStatisticsHandler.Create ();
   FNoteHandler := TNoteHandler.Create;
   FTreeHandler := TTreeWidthHandler.Create (self, FTree.control, FSplitter);

   SetWndStyle (CS_DBLCLKS);
   PageCenter.AddObserver (self);
   OptionMan.AddObserver(self);
   MemoryMan.AddObserver (self);
   SizeMan.AddObserver (self);
   CommandMan.AddExecutor (self);
end;

procedure TWorkSpace.OnDestroy ();
begin
	CommandMan.RemoveExecutor (self);
	SizeMan.RemoveObserver (self);
	OptionMan.RemoveObserver(self);
   MemoryMan.RemoveObserver(self);
   PageCenter.RemoveObserver (self);

   FNoteHandler.free;
   FTreeHandler.free;

   FBlog.Control.RemoveMessageObserver (FScrollHandler);
   FTree.Control.RemoveMessageObserver (FScrollHandler);
   FList.Control.RemoveMessageObserver (FScrollHandler);
   FEditor.Control.RemoveMessageObserver (FScrollHandler);
   FScrollHandler.free;

	FSysPageManager.free;
   FStatisticsHandler.free;
   
   FBlog.free;
	FList.free;
   FEditor.free;
   FTree.free;
   FTab.free;
   FSplitter.Free;
   FSpace.free;
   
   inherited;
end;

function TWorkSpace.ProcessMessage (AMessage, wparam, lparam: DWORD): DWORD;
begin
	result := 0;
	case AMessage of
   	WM_LBUTTONDBLCLK:
      	begin
            if OptionMan.Options.GroupDblClick = lbLevelUp then
               CommandMan.ExecuteCommand(m_levelup)
            else
               CommandMan.ExecuteCommand(m_newsiblingnote);
         end
      else
         result := inherited ProcessMessage (AMessage, wParam, lParam);
   end;
end;

function TWorkSpace.CheckCommand (opr: word): boolean;
begin
	result := true;
end;

procedure TWorkSpace.ExecuteCommand (opr: word);
begin
	case opr of
      hk_switchfocus:
         begin
            if GetFocus = FClient.Control.handle then
               FNavigator.Control.SetFocus
            else
               FClient.Control.SetFocus;
         end;
      hk_treeleft:
         begin
            if not FTreeMode then exit;

            if FTreeHandler.TreeOnRight then
            begin
               if FTreeHandler.TreeHide then
                  FTreeHandler.ShowTree
               else
                  FTreeHandler.HideTree_Collapse;
            end
            else
            begin
               if FTreeHandler.TreeHide then
                  FTreeHandler.ShowTree_Expand
               else
                  FTreeHandler.HideTree;
            end;
         end;
      hk_treeright:
         begin
            if not FTreeMode then exit;

            if FTreeHandler.TreeOnRight then
            begin
               if FTreeHandler.TreeHide then
                  FTreeHandler.ShowTree_Expand
               else
                  FTreeHandler.HideTree;
            end
            else
            begin
               if FTreeHandler.TreeHide then
                  FTreeHandler.ShowTree
               else
                  FTreeHandler.HideTree_Collapse;
            end;
         end;
      m_moveup, m_movedown:
         if FTree.Control.HasFocus or FTab.Control.HasFocus then
            FSpace.StepMove (false, opr = m_moveup)
         else if FList.Control.HasFocus or FBlog.Control.HasFocus then
            FSpace.StepMove (true, opr = m_moveup);
   end;
end;

//-----------------------

procedure TWorkSpace.GetClientSize (var i_width, i_height: integer);
begin
	i_width := FEditor.Control.Width;
   i_height := FEditor.Control.Height;
end;

//------------------------

procedure TWorkSpace.SaveMemory ();
begin
  	MemoryMan.TreeWidth := FTreeHandler.TreeWidth;
   MemoryMan.TreeHide := FTreeHandler.TreeHide;

   if PageCenter.ActivePage <> nil then
   	MemoryMan.PageID := PageCenter.ActivePage.Id;
end;

procedure TWorkSpace.RestoreMemory ();
var id: integer;
begin
   FTreeHandler.TreeWidth := MemoryMan.TreeWidth;
   FTreeHandler.TreeHide := MemoryMan.TreeHide;

   // 此段不可省
   if OptionMan.Options.StartPage >= 0 then
      id := OptionMan.Options.StartPage
   else
      id := MemoryMan.PageID;
  	PageCenter.ActivePage := PageStore[id];
end;

procedure TWorkSpace.OptionChanged ();
var tp: TTabPosition;
begin
   if OptionMan.Options.TabOnBottom then
   	tp := tpBottom
   else
   	tp := tpTop;
   if tp <> TxlTabControl(FTab.Control).TabPosition then
   begin
      TxlTabControl(FTab.Control).TabPosition := tp;
      if not FTreeMode then
      	ShowTree (FTreeMode)
   end;
   FSplitter.Width := OptionMan.Options.SplitterWidth;
   FTreeHandler.TreeOnRight := OptionMan.Options.TreeOnRight;
   if FTreeMode then //OptionMan.Options.TreeOnRight <> FTreeOnRight then
   begin
//      FTreeOnRight := OptionMan.Options.TreeOnRight;
      f_SetTreeNav;
   end;
end;

procedure TWorkSpace.OnPageEvent (pct: TPageEvent; id, id2: integer);
var b: boolean;
	p: TPageControl;
begin
	if pct = pctControlChange then
   begin
   	p := PageStore[id].PageControl;

   	b := p = pcEdit;
      FEditor.Active := b;
      FEditor.Control.Visible := b;

      b := p = pcListView;
      FList.Active := b;
      FList.Control.Visible := b;

      b := p = pcBlog;
      FBlog.Active := b;
      FBlog.Control.Visible := b;

      case p of
      	pcEdit: FClient := FEditor;
         pcListView: FClient := FList;
         else FClient := FBlog;
      end;
   end;
end;

procedure TWorkSpace.ShowTree (value: boolean);
begin
	FTreeMode := value;
   self.ClearChildList;
   FTab.Control.ClearChildList;
   FTree.Control.Visible := value;
   FSplitter.Visible := value;
   FTab.Control.Visible := not value;

	if value then
   begin
      f_SetTreeNav
   end
   else
   begin
      AddChild (FTab.Control, alClient);
		FSplitter.ClearBuddy;
      FTab.Control.AddChild (FEditor.Control, alClient);
      FTab.Control.AddChild (FList.Control, alClient);
      FTab.Control.AddChild (FBlog.Control, alClient);
   end;

   FTree.Active := value;
   FTab.Active := not value;
	if FTreeMode then
   	FNavigator := FTree
   else
   	FNavigator := FTab;
   if FEditor.Active then
   	FEditor.Control.SetFocus;
end;

procedure TWorkSpace.f_SetTreeNav ();
begin
   ClearChildList;
   if OptionMan.Options.TreeOnRight then
   begin
      AddChild (FTree.Control, alRight);
      AddChild (FSplitter, alRight);
      FSplitter.SetBuddy ([FList.Control, FEditor.Control, FBlog.Control], [FTree.Control]);
   end
   else
   begin
      AddChild (FTree.Control, alLeft);
      AddChild (FSplitter, alLeft);
      FSplitter.SetBuddy ([FTree.Control], [FList.Control, FEditor.Control, FBlog.Control]);
   end;

   AddChild (FEditor.Control, alClient);
   AddChild (FList.Control, alClient);
   AddChild (FBlog.Control, alClient);
//   AdjustAppearance;
   self.Width := self.Width - 1;
   self.Width := self.Width + 1;
end;

procedure TWorkSpace.AdjustAppearance ();
begin
   if FTreeMode then
   begin
   	FTree.Control.Visible := SizeMan.ShowTree;
      FSplitter.Visible := FTree.Control.Visible;
   end
   else
   begin
      if SizeMan.ShowTab then
      begin
      	if FEditor.Control.Parent = FTab.Control then exit;
      	FEditor.Control.Parent := FTab.Control;
         FList.Control.Parent := FTab.Control;
         FBlog.Control.Parent := FTab.Control;
         ShowWindow (FTab.Control.handle, SW_Show);    // 不能用 FTab.Control.Visible！因 TxlWinContainer 的 visible 改变会影响 child 的 visible
      end
      else
      begin
      	if FEditor.Control.Parent = self then exit;
      	FEditor.Control.Parent := self;
         FList.Control.Parent := self;
         FBlog.Control.Parent := self;
         ShowWindow (FTab.Control.handle, SW_Hide);
      end;
   end;
end;

procedure TWorkSpace.f_OnSplitterButtonclick (Sender: TObject);
begin
   if FTreeHandler.TreeHide then
      FTreeHandler.ShowTree
   else
      FTreeHandler.HideTree;
end;

procedure TWorkSpace.f_OnSplitterButtonRightclick (Sender: TObject);
begin
   if FTreeHandler.TreeHide then
      FTreeHandler.ShowTree_Expand
   else
      FTreeHandler.HideTree_Collapse;
end;

procedure TWorkSpace.f_OnDropItems (o_dragsource: IDragSource; i_targetid, i_pid: integer; b_copy: boolean);
var i, id, i_sourceid: integer;
	o_sourceid: TxlIntList;
begin
	o_sourceid := TxlIntList.Create();
	o_dragsource.GetDragData(o_sourceid);

	for i := o_sourceid.Low to o_sourceid.High do
   begin
   	i_sourceid := o_sourceid[i];
   	id := FSpace.ProcessMoveCopy (i_sourceid, i_targetid, i_pid, b_copy, KeyPressed(VK_SHIFT));
   end;

   o_sourceid.free;
   if o_dragsource = IDragSource(TxlDragControl(FTree.Control)) then
   	PageCenter.ActivePage := PageStore[id];
end;

//-------------------------

constructor TTreeWidthHandler.Create (APanel: TxlPanel; ATree: TxlControl; ASplitter: TxlVertSplitter);
begin
   FPanel := APanel;
   FSplitter := ASplitter;
   FTree := ATree;
end;

function TTreeWidthHandler.GetTreeWidth (): integer;
begin
   if FTree.Width > 0 then
      result := FTree.Width
   else
      result := FTreeWidth;
end;

procedure TTreeWidthHandler.SetTreeWidth (value: integer);
begin
   value := ABS (value);
//   if (not TreeHide) then
      FTree.Width := value;
   if value > 0 then
      FTreeWidth := value;
end;

function TTreeWidthHandler.GetTreeHide (): boolean;
begin
   result := FTree.Width = 0;
end;

procedure TTreeWidthHandler.SetTreeHide (value: boolean);
begin
   if value then
      FTree.Width := 0
   else
      FTree.Width := FTreeWidth;
//   FTree.Visible := value;
end;

procedure TTreeWidthHandler.ShowTree_Expand ();
var w: integer;
   o_pos: TPos;
begin
   o_pos := MainWindow.Pos;
   w := TreeWidth;
   FTreeWidth := w;

   if FTreeOnRight then
   begin
      inc (o_pos.width, w);
      TreeWidth := w;
   end
   else
   begin
      dec (o_pos.x, w);
      inc (o_pos.width, w);
      FSplitter.SlidePos := w;
   end;
   MainWindow.Pos := o_pos;
end;

procedure TTreeWidthHandler.HideTree_Collapse ();
var w: integer;
   o_pos: TPos;
begin
   o_pos := MainWindow.Pos;
   w := TreeWidth;
   FTreeWidth := w;

   if FTreeOnRight then
   begin
      TreeWidth := 0;
      dec (o_pos.Width, w);
   end
   else
   begin
      inc (o_pos.x, w);
      dec (o_pos.width, w);
      FSplitter.SlidePos := 0;
   end;
   MainWindow.Pos := o_pos;
end;

procedure TTreeWidthHandler.ShowTree ();
var w: integer;
begin
   w := TreeWidth;
   FTreeWidth := w;

   if FTreeOnRight then
      FSplitter.SlidePos := FPanel.Width - w - FSplitter.Width
   else
      FSplitter.SlidePos := w;
end;

procedure TTreeWidthHandler.HideTree ();
begin
   FTreeWidth := TreeWidth;
   if FTreeOnRight then
      FSplitter.SlidePos := FPanel.Width - FSplitter.Width
   else
      FSplitter.SlidePos := 0;
end;

end.











