unit UBlog;

interface

uses UClientSuper, UPageSuper, UBlogView, UTypeDef, UxlWinControl, UxlExtClasses;

type
   TBlogClient = class (TListClientSuper, ILangObserver, IOptionObserver)
   private
   	FBlog: TBlogView;
		procedure f_GetBlogItem (var o_item: TBlogItem; value: TPageSuper; b_virtual: boolean = false);
      procedure f_OnPopulateBlogItem (index: integer; var o_item: TBlogItem);
      procedure f_OnContextMenu (index: integer);
      procedure f_OnDeleteItemDemand (Sender: TObject);
   protected
   	function Items (index: integer): TPageSuper; override;
		procedure Load (value: TPageSuper); override;
      procedure UnLoad (); override;
   public
      constructor Create (WndParent: TxlWinContainer);
      destructor Destroy (); override;
   	function Control (): TxlControl; override;
      procedure Save (); override;

   	procedure OnPageEvent (pct: TPageEvent; id, id2: integer); override;
      function CheckCommand (opr: word): boolean; override;
      procedure ExecuteCommand (opr: word); override;
      procedure LanguageChanged ();
      procedure OptionChanged ();
   end;

implementation

uses UxlDateTimeUtils, UxlList, UPageStore, UPageFactory, UGlobalObj, UxlcommDlgs, UxlStrUtils, ULangManager, UOptionManager, Resource;

constructor TBlogClient.Create (WndParent: TxlWinContainer);
begin
	FBlog := TBlogView.Create (WndParent);
   with FBlog do
   begin
   	Items.OnPopulateItem := f_OnPopulateBlogItem;
   	Items.OnSelect := f_OnSelectItem;
   	MultiSelect := true;
   	OnItemDblClick := f_OnItemDblClick;
      OnItemReturn := f_OnItemDblClick;
      OnDeleteItemDemand := f_OnDeleteItemDemand;
      OnContextMenu := f_OnContextMenu;
   end;
   LangMan.AddObserver (self);
   OptionMan.AddObserver(self);
end;

destructor TBlogClient.Destroy ();
begin
	LangMan.RemoveObserver (self);
	OptionMan.RemoveObserver(self);
	FBlog.free;
   inherited;
end;

function TBlogClient.Control (): TxlControl;
begin
	result := FBlog;
end;

function TBlogClient.Items (index: integer): TPageSuper;
begin
	result := PageStore[FBlog.Items[index].data];
end;

procedure TBlogClient.f_GetBlogItem (var o_item: TBlogItem; value: TPageSuper; b_virtual: boolean = false);
begin
	o_item.Data := value.id;
   o_item.IsVirtual := b_virtual;
   if not b_virtual then
      with o_item do
      begin
         Title := value.GetColText (sr_Title);
         DateTime := value.GetColText (sr_DateTime);
         Text := SingleLineToMultiLine(value.GetColText (sr_Abstract));
      end;
end;

procedure TBlogClient.f_OnPopulateBlogItem (index: integer; var o_item: TBlogItem);
begin
	f_GetBlogItem (o_item, PageStore[o_item.data], false);
end;

procedure TBlogClient.f_OnContextMenu (index: integer);
var i_demand: cardinal;
begin
	if index >= 0 then
     	i_demand := ListContext
   else
   	i_demand := ListNoSelContext;
   EventMan.EventNotify (e_ContextMenuDemand, i_demand);
end;

procedure TBlogClient.f_OnDeleteItemDemand (Sender: TObject);
begin
   CommandMan.ExecuteCommand (m_delete);
end;

procedure TBlogClient.Load (value: TPageSuper);
var i: integer;
	o_list: TxlIntList;
   o_item: TBlogItem;
begin
	if value = nil then exit;
	inherited Load (value);
   FBlog.Items.Clear;
	o_list := TxlIntList.Create;
	value.GetChildList (o_list);
   for i := o_list.Low to o_list.High do
   begin
   	f_GetBlogItem (o_item, PageStore[o_list[i]], true);
      o_item.Selected := false;
      FBlog.Items.Add (o_item);
   end;
   o_list.Free;
   FBlog.Update;
end;

procedure TBlogClient.UnLoad ();
begin
	FBlog.Items.Clear;
end;

procedure TBlogClient.Save ();
begin
end;

procedure TBlogClient.LanguageChanged ();
begin
	Refresh;
end;

procedure TBlogClient.OptionChanged ();
begin
	FBlog.Font := OptionMan.Options.BlogFont;
   FBlog.Color := OptionMan.Options.BlogColor;

   FBlog.TitleFont := OptionMan.Options.BlogTitleFont;
   FBlog.TitleColor := OptionMan.Options.BlogTitleColor;

   FBlog.TitleSelFont := OptionMan.Options.BlogSelTitleFont;
   FBlog.TitleSelColor := OptionMan.Options.BlogSelTitleColor;

   FBlog.Update;
end;

procedure TBlogClient.OnPageEvent (pct: TPageEvent; id, id2: integer);
var o_item: TBlogItem;
   i: integer;
begin
   case pct of
      pctFieldModified:
      	begin
         	i := FBlog.Items.FindByData (id);
            if i >= 0 then
            begin
               o_item := FBlog.Items[i];
               f_GetBlogItem (o_item, PageStore[id], false);
               FBlog.Items[i] := o_item;
               FBlog.Redraw;
            end;
         end;
      pctAddChild:
         if id = FPage.id then
         begin
         	f_GetBlogItem (o_item, PageStore[id2], true);
            i := FPage.Childs.FindChild (id2);
         	if i <= FBlog.Items.High then
               FBlog.Items.Insert (i, o_item)
            else
            	FBlog.Items.Add (o_item);
            FBlog.Items.SelIndex := i;
            Fblog.Redraw;
            FBlog.SetFocus;
         end;
      pctRemoveChild:
      	if id = FPage.id then
      	begin
         	i := FBlog.Items.FindByData (id2);
            if i >= 0 then
            begin
					FBlog.Items.Delete (i);
               FBlog.Redraw;
            end;
         end;
      pctListProperty:
      	Refresh;
   else
      inherited OnPageEvent (pct, id, id2);
   end
end;

function TBlogClient.CheckCommand (opr: word): boolean;
begin
   case opr of
      m_clear, m_selectall:
         result := false; //FBlog.Items.Count > 0;
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

procedure TBlogClient.ExecuteCommand (opr: word);
begin
	case opr of
      m_delete, m_removeitem: CommandMan.ExecuteCommand (m_deleteitem);
   end;
end;

end.

