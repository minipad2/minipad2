unit UGroupPage;

interface

uses UPageSuper, USysPageHandler, UTypeDef;

type
   TRoot = class (TPageContainer)
   public
   	class function PageType (): TPageType; override;
		function ChildShowInTree (ptChild: TPageType): boolean; override;
      function CanOwnChild (ptChild: TPageType): boolean; override;
      class function SingleInstance (): boolean; override;
	end;

	TGroupSuper = class (TPageContainer)
   public
      function CanOwnChild (ptChild: TPageType): boolean; override;
	end;

   TGroupPage = class (TGroupSuper)
   protected
   public
   	class function PageType (): TPageType; override;
		function ChildShowInTree (ptChild: TPageType): boolean; override;
      class function DefChildType (): TPageType; override;
   end;

   TGroupRoot = class (TGroupPage)
   public
   	class function PageType (): TPageType; override;
      class function SingleInstance (): boolean; override;
      function CanDelete (): boolean; override;
	end;

   TGroupRootHandler = class (TSysPageHandlerSuper)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   end;

implementation

uses UxlFunctions, UPageFactory, Resource;

class function TRoot.PageType (): TPageType;
begin
	result := ptRoot;
end;

function TRoot.ChildShowInTree (ptChild: TPageType): boolean;
begin
	result := true;
end;

function TRoot.CanOwnChild (ptChild: TPageType): boolean;
begin
	result := ptChild in [ptRecentRoot, ptFAvorite, ptTagRoot, ptSearch, ptGroupRoot, ptRecycleBin, ptTemplate, ptFastLink, ptClip];
end;

class function TRoot.SingleInstance (): boolean;
begin
	result := true;
end;

//--------------------

function TGroupSuper.CanOwnChild (ptChild: TPageType): boolean;
begin
	result := ptChild in [ptGroup, ptNote, ptCalc, ptMemo, ptDict, ptLink, ptContact, ptTemplate, ptSheet];
end;

//------------------

class	function TGroupPage.PageType (): TPageType;
begin
	result := ptGroup;
end;

function TGroupPage.ChildShowInTree (ptChild: TPageType): boolean;
begin
	result := true;
end;

class function TGroupPage.DefChildType (): TPageType;
begin
	result := ptNote;
end;

//------------------

class function TGroupRoot.PageType (): TPageType;
begin
	result := ptGroupRoot;
end;

class function TGroupRoot.SingleInstance (): boolean;
begin
	result := true;
end;

function TGroupRoot.CanDelete (): boolean;
begin
	result := false;
end;

//------------------

function TGroupRootHandler.PageType (): TPageType;
begin
	result := ptGroupRoot;
end;

function TGroupRootHandler.NameResource (): integer;
begin
	result := sr_GroupRoot;
end;

function TGroupRootHandler.MenuItem_Manage (): integer;
begin
	result := -1;
end;

//-------------------

initialization
	PageFactory.RegisterClass (TGroupPage);
   PageImageList.AddImageWithOverlay (ptGroup, m_newgroup);
	PageNameMan.RegisterDefName (ptGroup, sr_defgroupname);
//   PageDefSettingsMan.RegisterType (ptGroup, TListPageSuper.ListInitialsettings);

	PageFactory.RegisterClass (TGroupRoot);
   PageImageList.AddImageWithOverlay (ptGroupRoot, m_newgroup);

	PageFactory.RegisterClass (TRoot);

finalization

end.




