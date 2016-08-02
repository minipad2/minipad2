unit USpace;

interface

uses UxlClasses, UxlWinControl, UxlList, UxlExtClasses, UPageSuper, UPageFactory, UTypeDef, UxlRichEdit, Resource, UBrowseHistory;

type
   TSpace = class (TxlInterfacedObject, ICommandExecutor, ISaver, IPageObserver)  // single instance
   private
      FHistory: THistory;
      FSelItems: TxlIntList;

      procedure f_TriggerChange (pct: TPageEvent; p: TPageSuper);
		procedure CanCreateNewPage (pt: TPageType; b_alwayssibling: boolean; var o_parent: TPageSuper; var sid: integer);

      procedure SetActivePage (value: TPageSuper);
      function GetActivePage (): TPageSuper;
      property ActivePage: TPageSuper read GetActivePage write SetActivePage;
		function CheckMove (p1, pp1, p2, pp2: TPageSuper; b_AlwaysAsSibling: boolean; var pparent, pinsbefore: TPageSuper): boolean;
   public
   	constructor Create;
      destructor Destroy (); override;

      function CheckCommand (opr: word): boolean;
      procedure ExecuteCommand (opr: word);
   	procedure OnPageEvent (pct: TPageEvent; id, id2: integer);
      procedure Save ();

		function ProcessMoveCopy (sid, tid, pid: integer; b_copy, b_alwaysassibling: boolean): integer;
      procedure StepMove (b_item: boolean; b_up: boolean);
   end;

implementation

uses Windows, UxlFile, UxlFunctions, UGlobalObj, UxlMath, UxlStrUtils, UPageStore, UxlCommDlgs, UDialogs, UOptionManager,
	ULangManager, UPageProperty, UEditBox, UPropertyBox, UDeleteHandler, UxlExtDlgs, UxlWindow, UxlWinDef, UxlMiscCtrls;

constructor TSpace.Create ();
begin
   FHistory := THistory.Create;
   FSelItems := TxlIntList.Create;
   CommandMan.AddExecutor (self);
   SaveMan.AddObserver (self);
   PageCenter.AddObserver (self);
end;

destructor TSpace.Destroy ();
begin                         
	PageCenter.RemoveObserver (self);
	SaveMan.RemoveObserver (self);
	CommandMan.RemoveExecutor (self);
	FSelItems.free;
   FHistory.Free;
	inherited;
end;

procedure TSpace.SetActivePage (value: TPageSuper);
begin
	PageCenter.ActivePage := value;
end;

function TSpace.GetActivePage (): TPageSuper;
begin
	result := PageCenter.ActivePage;
end;

procedure TSpace.Save ();
begin
	f_TriggerChange (pctSave, ActivePage);
   PageStore.SaveIndex;
end;

//----------------------

procedure TSpace.OnPageEvent (pct: TPageEvent; id, id2: integer);
begin
   case pct of
   	pctSetActive:
      	begin
      		FHistory.Add (id);
   			FSelItems.Clear;
            CommandMan.CheckCommands;
			end;
      pctSelect:
      	begin
				FSelItems.Add (id);
            CommandMan.CheckCommands;
         end;
      pctDeSelect:
      	begin
				FSelItems.DeleteByValue (id);
            CommandMan.CheckCommands;
			end;
      pctSwitchStatus:
      	CommandMan.CheckCommands;
   end;
end;

procedure TSpace.f_TriggerChange (pct: TPageEvent; p: TPageSuper);
begin
   if p <> nil then
	   PageCenter.EventNotify (pct, p.id);
end;

//-------------------------

function TSpace.CheckMove (p1, pp1, p2, pp2: TPageSuper; b_AlwaysAsSibling: boolean; var pparent, pinsbefore: TPageSuper): boolean;
var p: TPageSuper;
   i, j: integer;
begin
   result := false;

   // 根级节点相互间只能平级移动, 非根级节点不允许移为根级节点。
	if (pp2 = Root) then
   begin
   	if (pp1 = Root) then
   		b_AlwaysAsSibling := true
      else
      	b_AlwaysAsSibling := false;
   end;

   // 不允许从虚拟容器拖入 Owner 容器。如果执行此操作，则视为从虚拟容器中移除该页的链接。
   if (p1.Owner <> pp1) and pp2.CanOwnChild (p1.Pagetype) then
   begin
      pp1.childs.RemoveChild (p1.id);
   	exit;
   end;

   if (p2.CanAddChild (p1.PageType)) and (not b_AlwaysAsSibling) then    // move as child
   begin
//   	if p2.Childs.FindChild (p1.id) >= 0 then exit;  // 已存在该子节点
      pparent := p2;
      pinsbefore := nil;
   end
   else if pp2.CanAddChild (p1.PageType) then   // move as sibling
   begin
      pparent := pp2;
      pinsbefore := p2;
      if pp1 = pp2 then  // 同一父节点下的子节点，区分是上移还是下移。
      begin
      	i := pp2.Childs.FindChild (p1.id);
      	j := pp2.Childs.findChild (p2.id);
         if i < j then      // 从上往下拖动，insert after p2
         begin
            If (j < pp2.Childs.Count - 1) then
            	pinsbefore := PageStore[pp2.Childs.ChildId(j+1)]
            else
         		pinsbefore := nil;
         end;
      end;
   end
   else
   	exit;

   // 不允许自己移给自己，也不允许子节点移到父节点之上
   p := pparent;
   while p <> Root do
   begin
      if p = p1 then exit;
      p := p.Owner;
   end;
   result := true;
end;

procedure TSpace.StepMove (b_item: boolean; b_up: boolean);
   function f_StepMove (psource, pparent: TPageSuper; b_up: boolean): boolean;
   var i, j: integer;
   begin
      result := false;
      i := pparent.Childs.FindChild (psource.id);
      if b_up then
      begin
         if i = 0 then exit;
         j := i - 1;
      end
      else
      begin
         if i >= pparent.Childs.Count - 1 then exit;
         j := i + 1;
      end;
      pparent.Childs.RemoveChildByPos (i);
      pparent.Childs.AddChildByPos (psource.id, j);
      result := true;
   end;
var psource, ptarget, pparent: TPageSuper;
   i, j: integer;
begin
   if ActivePage = nil then exit;
   if b_item then
   begin
      if FSelItems.Count <> 1 then exit;
      psource := PageStore[FSelItems[0]];
      pparent := ActivePage;
      f_StepMove (psource, pparent, b_up);
   end
   else
   begin
      psource := ActivePage;
      pparent := psource.owner;
      if f_StepMove (psource, pparent, b_up) then
         ActivePage := psource;
   end;
end;

function TSpace.ProcessMoveCopy (sid, tid, pid: integer; b_copy, b_alwaysassibling: boolean): integer;
var psource, pinsbefore, pparent, pcurrentparent: TPageSuper;
begin
   psource := PageStore[sid];
   if (ActivePage.Childs <> nil) and (ActivePage.Childs.FindChild (sid) >= 0) then
   	pcurrentparent := ActivePage
   else
   	pcurrentparent := psource.Owner;
   if not CheckMove (psource, pcurrentparent, PageStore[tid], PageStore[pid], b_alwaysassibling, pparent, pinsbefore) then exit;

   result := psource.id;
   if pparent.CanOwnChild (psource.pagetype) then     // 拖入owner类容器。必定从owner-->owner，不允许从virtual-->owner
   begin
      if b_copy and (not psource.SingleInstance) then
      begin
         result := PageStore.NewPage (pparent.id, psource.PageType);
         PageStore[result].Clone (psource);
      end
      else
      begin
         pcurrentparent.Childs.RemoveChild (result);
         psource.owner := pparent;
      end;
   end
   else if psource.owner = pcurrentparent then     // owner --> virtual（tag、favorite等虚拟容器），只能为copy
   begin
   	if pparent.Childs.FindChild (psource.id) >= 0 then exit;  // 已存在该链接
   end
   else        // virtual --> virtual
   begin
   	if not b_copy then
      	pcurrentparent.Childs.RemoveChild (result);
   end;

   if pinsbefore = nil then
      pparent.Childs.AddChild (result)
   else
      pparent.Childs.AddChild (result, pinsbefore.id);
end;

//------------------------

function TSpace.CheckCommand (opr: word): boolean;
//	function f_Check (pt: TPageType): boolean;
//	var o_parent: TPageSuper;
//   	sid: integer;
//   begin
//   	result := CanCreateNewPage (pt, false, o_parent, sid);
//   end;
var b, b_delete: boolean;
	i: integer;
begin
	b := (ActivePage <> nil);
	case opr of
//   	m_newpage, m_newnote, m_newgroup, m_newcontact, m_newmemo, m_newlink, m_newcalc, m_newdict:
//      	result := true; //b and f_Check (ptNote);
		m_prior:
      	result := FHistory.HasPrior;
      m_next:
      	result := FHistory.HasNext;
      m_levelup, m_rename:
      	result := b and (ActivePage.Owner <> Root);
      m_deletepage:
      	result := b and DeleteHandler.CanRemove (ActivePage, ActivePage.owner, b_delete);
   	m_property:
      	result := b;
      m_view:
      	result := b and (ActivePage.ListProperty <> nil);
      m_newitem:
      	result := b and (ActivePage.DefChildType <> ptNone);
      m_insertitem:
      	result := b and (ActivePage.DefChildType <> ptNone) and (not FSelItems.IsEmpty);
      m_removeitem:
      	result := b and (not FSelItems.IsEmpty);
      m_deleteitem:
      	begin
	      	result := b and (not FSelItems.IsEmpty);
            if result then
            begin
            	result := false;
            	for i := FSelItems.Low to FSelItems.High do
               	if PageStore[FSelItems[i]].CanDelete then
                  begin
                  	result := true;
                     exit;
                  end;
            end;
         end;
      m_edititem:
      	result := b and (not FSelItems.IsEmpty) and (not (ActivePage.PageType = ptRecycleBin));
      else
         result := true;
	end;
end;

procedure TSpace.CanCreateNewPage (pt: TPageType; b_alwayssibling: boolean; var o_parent: TPageSuper; var sid: integer);
   function GroupRoot (): TPageSuper;
   var id: integer;
   begin
      PageStore.GetFirstPageId (ptGroupRoot, id);
      result := PageStore[id];
   end;
var b_insertbefore: boolean;
begin
   if ActivePage = nil then
      o_parent := GroupRoot
   else
   begin
      b_alwayssibling := b_alwayssibling or ModKeyPressed(VK_SHIFT);
      b_insertbefore := ModKeyPressed(VK_CONTROL);
      if ActivePage.CanOwnChild (pt) and (not (b_alwayssibling or b_insertbefore)) then   // new child
         o_parent := ActivePage
      else   // new sibling
         o_parent := ActivePage.Owner;
      if (o_parent = Root) or (not o_parent.CanOwnChild (pt)) or (o_parent.PageType = ptRecycleBin) then
         o_parent := GroupRoot
      else if b_insertbefore then
         sid := ActivePage.id;
   end;
end;

procedure TSpace.ExecuteCommand (opr: word);
   procedure f_CheckListChange (oldpc: TPageControl);
   begin
      if ActivePage.PageControl <> oldpc then
      begin
         f_TriggerChange (pctControlChange, ActivePage);
         f_TriggerChange (pctListProperty, ActivePage);
         f_TriggerChange (pctSetActive, ActivePage);
      end
      else
         f_TriggerChange (pctListProperty, ActivePage);
   end;
   procedure f_ChangeListStyle (opr: word);
   var p: TListProperty;
   	oldpc: TPageControl;
   begin
		p := ActivePage.ListProperty;
      oldpc := p.PageControl;
      case opr of
         rb_icon: p.View := lpvIcon;
         rb_smallicon: p.View := lpvSmallIcon;
         rb_list: p.View := lpvList;
         rb_report: p.View := lpvReport;
      	else p.View := lpvBlog;
      end;
      f_CheckListChange (oldpc);
   end;
   procedure NewPage (pt: TPageType; b_alwaysassibling: boolean = false; sid: integer = -1);
   var o_parent: TPageSuper;
      id: integer;
      s_name: widestring;
   begin
		CanCreateNewPage (pt, b_alwaysassibling, o_parent, sid);
      s_name := o_parent.Childs.GetNewChildName (pt);
      if not (CommandMan.ItemChecked[m_showtree] or OptionMan.Options.AutoName) then
      begin
         if not InputBox (LangMan.GetItem(sr_newpage, '新建标签页'), LangMan.GetItem(sr_inputpagename, '请输入标签页名称：'),
            s_name, 0, LangMan.GetItem(IDOK), LangMan.GetItem(IDCancel)) then exit;
      end;
      id := PageStore.NewPage (o_parent.id, pt);
      PageStore[id].Name := s_name;
      o_parent.Childs.AddChild (id, sid);
      ActivePage := PageStore[id];
      if CommandMan.ItemChecked[m_showtree] and (not OptionMan.Options.AutoName) then
         PostMessageW (MainWinHandle, WM_EDITTREELABEL, 0, 0)
      else
         MainWindow.Post (WM_FOCUSEDITOR, 0, 0);
   end;
   procedure NewItem (sid: integer = -1);
   var o_box: TEditBoxSuper;
      pt: TPageType;
   begin
      pt := ActivePage.DefChildType;
      o_box := EditBoxFactory.NewEditBox (pt);
      if o_box <> nil then
      begin
         o_box.ParentPage := ActivePage;
         o_box.ItemId := -1;
         o_box.InsertBeforeId := sid;
         o_box.Execute;
         o_box.free;
      end
      else
         NewPage (pt, false, sid);
   end;
   procedure EditPage (id: integer);
   var o_box: TEditBoxSuper;
   begin
   	o_box := EditBoxFactory.NewEditBox (PageStore[id].PageType);
      if o_box <> nil then
      begin
      	o_box.ParentPage := ActivePage;
         o_box.ItemId := id;
         o_box.Execute;
         o_box.free;
      end
      else
      	ActivePage := PageStore[id];
   end;
   procedure DeletePage ();
   var b_delete: boolean;
   	p: TPageSuper;
   begin
      p := ActivePage;
      if not DeleteHandler.CanRemove (p, p.owner, b_delete) then exit;

      if b_delete then
      begin
         if p.PageType = ptGroup then
         begin
            if (not p.Childs.IsEmpty) and (ShowMessage (LangMan.GetItem (sr_deletegroupprompt, '当前组包含子节点。真的要全部删除吗？'), mtQuestion,
               LangMan.GetItem(sr_prompt)) = mrCancel) then exit;
         end
         else if OptionMan.Options.ConfirmDelete then
         begin
            SaveMan.Save;
            if ((PageStore.TextLength(p.id) > 0) or ((p.Childs <> nil) and (not p.Childs.IsEmpty))) then
               if (ShowMessage (LangMan.GetItem (sr_deleteprompt, '真的要删除当前页吗？'), mtQuestion,
                  LangMan.GetItem(sr_prompt)) = mrCancel) then exit;
         end;
      end;

      FHistory.Delete (p.id);
      ActivePage := p.Nearest;
      DeleteHandler.Remove (p, p.owner);
   end;
   procedure DeleteItems ();
   var i: integer;
	   o_list: TxlIntList;
		p: TPageSuper;
   begin
   	o_list := TxlIntList.Create;
      for i := FSelItems.Low to FSelItems.High do   // FselItems 中的条目会受到删除操作本身的影响而改变
         o_list.Add (FSelItems[i]);
      for i := o_list.Low to o_list.High do
      begin
         p := PageStore[o_list[i]];
         PauseEscapeMessage := true;
         ProgTip.ShowTip (p.name, LangMan.GetItem(sr_deletingprompt), tiInfo);
         DeleteHandler.Remove (p, ActivePage);
         ProgTip.HideTip;
         ProcessMessages;     // 必需！
         if KeyPressed (VK_ESCAPE) then
         begin
            ProgTip.ShowTip (LangMan.GetItem(sr_userabortdelete));
            break;
         end;
      end;
//      FSelItems.Clear;
      o_list.free;
   end;

var id: integer;
   oldpc: TPageControl;
begin
	if ActivePage = nil then exit;
   if not CheckCommand (opr) then exit;
   case opr of
   	m_newnote: NewPage (ptNote);
      m_newsiblingnote: NewPage (ptNote, true);
      m_newcalc: NewPage (ptCalc);
      m_newmemo: NewPage (ptMemo);
      m_newdict: NewPage (ptDict);
      m_newlink: NewPage (ptLink);
      m_newcontact: NewPage (ptContact);
      m_newgroup: NewPage (ptGroup);
		m_prior:
      	if FHistory.HasPrior then
         	ActivePage := PageStore[FHistory.GetPrior];
      m_next:
      	if FHistory.HasNext then
         	ActivePage := PageStore[FHistory.GetNext];
      m_levelup:
      	if ActivePage.Owner <> nil then
         	ActivePage := ActivePage.Owner;
   	m_rename:
     		f_TriggerChange (pctRenameDemand, ActivePage);
      m_switchlock:
         case ActivePage.Status of
         	psNormal: ActivePage.Status := psLocked;
         	psLocked, psProtected: ActivePage.Status := psReadOnly;
            else
               ActivePage.Status := psNormal;
         end;
   	m_property:
      	begin
         	f_TriggerChange (pctSave, ActivePage);
            oldpc := ActivePage.PageControl;
            with TPropertyBox.Create () do
            begin
               Page := ActivePage;
               if Execute then
               	if (ActivePage.ListProperty <> nil) then
                  	f_CheckListChange (oldpc);
               Free;
            end;
         end;
		m_deletepage:
      	DeletePage;
      m_newitem:
      	NewItem ();
      m_insertitem:
      	begin
         	FSelItems.GetLast (id);
         	NewItem (id);
//            Pages[id].Parent.Childs.InsertChild (p.id, Pages[id]);
         end;
      m_edititem:
      	begin
            FSelItems.GetLast (id);
            EditPage (id);
         end;
      m_deleteitem:
         begin
         	if ShowMessage (LangMan.GetItem(sr_DeleteItemsPrompt), mtQuestion, LangMan.GetItem(sr_Prompt)) = mrCancel then exit;
            DeleteItems;
         end;
      m_removeitem:
      	begin
         	if ShowMessage (LangMan.GetItem(sr_RemoveItemsPrompt), mtQuestion, LangMan.GetItem(sr_Prompt)) = mrCancel then exit;
            DeleteItems;
         end;
      rb_icon, rb_smallicon, rb_list, rb_report, rb_blog:
			f_ChangeListStyle (opr);
   end;
end;

end.








