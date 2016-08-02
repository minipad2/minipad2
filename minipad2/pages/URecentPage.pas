unit URecentPage;

interface

uses UTypeDef, UPageSuper, UPageProperty, USysPageHandler, UxlClasses, UxlExtClasses, UxlList;

type
   TRecentPage_Root = class (TPageContainer)
   public
     	class function PageType (): TPageType; override;
		function ChildShowInTree (ptChild: TPageType): boolean; override;
      function CanOwnChild (ptChild: TPageType): boolean; override;
      class function SingleInstance (): boolean; override;
	end;

   TRecentPageSuper = class (TListPageSuper)
   private
   public
		procedure GetChildList (o_list: TxlIntList); override;
		class procedure InitialListProperty (lp: TListProperty); override;
      class function SingleInstance (): boolean; override;
	end;

   TRecentPage_Create = class (TRecentPageSuper)
   public
     	class function PageType (): TPageType; override;
		class procedure InitialListProperty (lp: TListProperty); override;
	end;

   TRecentPage_Modify = class (TRecentPageSuper)
   public
     	class function PageType (): TPageType; override;
		class procedure InitialListProperty (lp: TListProperty); override;
	end;

   TRecentPage_Visit = class (TRecentPageSuper)
   public
     	class function PageType (): TPageType; override;
		class procedure InitialListProperty (lp: TListProperty); override;
	end;

type
   TRecentCreateHandler = class (TSysPageContainerHandler)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
   end;

   TRecentModifyHandler = class (TSysPageContainerHandler)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
   end;

   TRecentVisitHandler = class (TSysPageContainerHandler)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
   end;

   TRecentRootHandler = class (TSysPageHandlerSuper)
   private
      FCreateHandler: TRecentCreateHandler;
      FModifyHandler: TRecentModifyHandler;
      FVisitHandler: TRecentVisitHandler;
      procedure AddChilds ();
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   public
   	constructor Create (AParent: TPageSuper); override;
      destructor Destroy (); override;
      procedure ExecuteCommand (opr: word); override;
   end;

implementation

uses Windows, UPageStore, UxlStrUtils, UGlobalObj, UOptionManager, ULangManager, UPageFactory,
	UxlDateTimeUtils, Resource;

class function TRecentPage_Root.PageType (): TPageType;
begin
	result := ptRecentRoot;
end;

function TRecentPage_Root.ChildShowInTree (ptChild: TPageType): boolean;
begin
	result := CanOwnChild (ptChild);
end;

function TRecentPage_Root.CanOwnChild (ptChild: TPageType): boolean;
begin
	result := ptChild in [ptRecentCreate, ptRecentModify, ptRecentVisit];
end;

class function TRecentPage_Root.SingleInstance (): boolean;
begin
	result := true;
end;

//--------------------

class function TRecentPageSuper.SingleInstance (): boolean;
begin
	result := true;
end;

procedure TRecentPageSuper.GetChildList (o_list: TxlIntList);
var i: integer;
   o_list2: TxlIntList;
begin
   o_list2 := TxlIntList.Create;

   PageStore.GetPageList(ptNone, o_list2);
   for i := o_list2.Low to o_list2.High do
      case PageType of
         ptRecentCreate:
            o_list2.Keys[i] := SystemTimeToString (PageStore[o_list2[i]].PageProperty.CreateTime, dtmDateTimeWithSecond);
         ptRecentModify:
            o_list2.Keys[i] := SystemTimeToString (PageStore[o_list2[i]].PageProperty.ModifyTime, dtmDateTimeWithSecond);
         else
            o_list2.Keys[i] := SystemTimeToString (PageStore[o_list2[i]].PageProperty.VisitTime, dtmDateTimeWithSecond);
      end;
   o_list2.SortByKey;
   for i := o_list2.High downto o_list2.Low do
   begin
      o_list.Add (o_list2[i]);
      if o_list.Count = OptionMan.Options.RecentNotesCount then exit;
   end;
   o_list2.free;
end;

class procedure TRecentPageSuper.InitialListProperty (lp: TListProperty);
begin
	with lp do
   begin
   	CheckBoxes := false;
		View := lpvReport;
   	FullrowSelect := true;
   	GridLines := false;
   end;
end;

//----------------------

class function TRecentPage_Create.PageType (): TPageType;
begin
	result := ptRecentCreate;
end;

class procedure TRecentPage_Create.InitialListProperty (lp: TListProperty);
const c_cols: array[0..3] of integer = (sr_Title, sr_Abstract, sr_CreateTime, sr_NodePath);
	c_widths: array[0..3] of integer = (100, 150, 80, 100);
begin
	inherited InitialListProperty (lp);
	lp.ColList.Populate (c_cols);
   lp.WidthList.Populate (c_widths);
end;

//-------------------

class function TRecentPage_Modify.PageType (): TPageType;
begin
	result := ptRecentModify;
end;

class procedure TRecentPage_Modify.InitialListProperty (lp: TListProperty);
const c_cols: array[0..3] of integer = (sr_Title, sr_Abstract, sr_ModifyTime, sr_NodePath);
	c_widths: array[0..3] of integer = (100, 150, 80, 100);
begin
	inherited InitialListProperty (lp);
	lp.ColList.Populate (c_cols);
   lp.WidthList.Populate (c_widths);
end;

//----------------------

class function TRecentPage_Visit.PageType (): TPageType;
begin
	result := ptRecentVisit;
end;

class procedure TRecentPage_Visit.InitialListProperty (lp: TListProperty);
const c_cols: array[0..3] of integer = (sr_Title, sr_Abstract, sr_VisitTime, sr_NodePath);
	c_widths: array[0..3] of integer = (100, 150, 80, 100);
begin
	inherited InitialListProperty (lp);
	lp.ColList.Populate (c_cols);
   lp.WidthList.Populate (c_widths);
end;

//----------------------

constructor TRecentRootHandler.Create (AParent: TPageSuper);
begin
	inherited Create (AParent);
   FCreateHandler := TRecentCreateHandler.Create (FPage);
   FModifyHandler := TRecentModifyHandler.Create (FPage);
   FVisitHandler := TRecentVisitHandler.Create (FPage);
	AddChilds;
end;

destructor TRecentRootHandler.Destroy ();
begin
	FCreateHandler.free;
   FModifyHandler.Free;
   FVisitHandler.free;
   inherited;
end;

function TRecentRootHandler.PageType (): TPageType;
begin
	result := ptRecentRoot;
end;

function TRecentRootHandler.NameResource (): integer;
begin
	result := sr_RecentRoot;
end;

function TRecentRootHandler.MenuItem_Manage (): integer;
begin
	result := m_ManageRecent;
end;

procedure TRecentRootHandler.ExecuteCommand (opr: word);
begin
	if opr = m_managerecent then
   begin
   	inherited ExecuteCommand (opr);
      AddChilds;
   end;
end;

procedure TRecentRootHandler.AddChilds ();
begin
   with FPage.Childs do
   begin
   	AddChild (FCreateHandler.Page.id);
      AddChild (FModifyHandler.Page.id);
      AddChild (FVisitHandler.Page.id);
   end;
end;

//--------------------------

function TRecentCreateHandler.PageType (): TPageType;
begin
	result := ptRecentCreate;
end;

function TRecentCreateHandler.NameResource (): integer;
begin
	result := sr_RecentCreate;
end;

function TRecentCreateHandler.MenuItem_Manage (): integer;
begin
	result := -1;
end;

function TRecentCreateHandler.MenuItem_List (): integer;
begin
	result := m_RecentCreate;
end;

//--------------------------

function TRecentModifyHandler.PageType (): TPageType;
begin
	result := ptRecentModify;
end;

function TRecentModifyHandler.NameResource (): integer;
begin
	result := sr_RecentModify;
end;

function TRecentModifyHandler.MenuItem_Manage (): integer;
begin
	result := -1;
end;

function TRecentModifyHandler.MenuItem_List (): integer;
begin
	result := m_RecentModify;
end;

//------------------------------

function TRecentVisitHandler.PageType (): TPageType;
begin
	result := ptRecentVisit;
end;

function TRecentVisitHandler.NameResource (): integer;
begin
	result := sr_RecentVisit;
end;

function TRecentVisitHandler.MenuItem_Manage (): integer;
begin
	result := -1;
end;

function TRecentVisitHandler.MenuItem_List (): integer;
begin
	result := m_RecentVisit;
end;

//--------------------------

initialization
	PageFactory.RegisterClass (TRecentPage_Root);
   PageImageList.AddImageWithOverlay (ptRecentRoot, m_recentroot);

	PageFactory.RegisterClass (TRecentPage_Create);
   PageImageList.AddImageWithOverlay (ptRecentCreate, m_recentcreate);

	PageFactory.RegisterClass (TRecentPage_Modify);
   PageImageList.AddImageWithOverlay (ptRecentModify, m_recentmodify);

	PageFactory.RegisterClass (TRecentPage_Visit);
   PageImageList.AddImageWithOverlay (ptRecentVisit, m_recentvisit);

finalization

end.


