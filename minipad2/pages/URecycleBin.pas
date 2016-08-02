unit URecycleBin;

interface

uses UPageSuper, USysPageHandler, UGroupPage, UPageFactory, UTypeDef, UxlClasses, UxlExtClasses;

type
   TRecycleBin = class (TGroupSuper)
   public
   	class function PageType (): TPageType; override;
		function ChildShowInTree (ptChild: TPageType): boolean; override;
      class function SingleInstance (): boolean; override;
	end;

   TRecycleHandler = class (TSysPageHandlerSuper)
   private
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   public
   	constructor Create (AParent: TPageSuper); override;
   end;

implementation

uses UGlobalObj, ULangManager, UPageStore, UDeleteHandler, Resource;

class	function TRecycleBin.PageType (): TPageType;
begin
	result := ptRecycleBin;
end;

function TRecycleBin.ChildShowInTree (ptChild: TPageType): boolean;
begin
	result := false;
end;

class function TRecycleBin.SingleInstance (): boolean;
begin
	result := true;
end;

//----------------------

constructor TRecycleHandler.Create (AParent: TPageSuper);
begin
	inherited Create (AParent);
   DeleteHandler.SetRecycleBin (FPage);
end;

function TRecycleHandler.PageType (): TPageType;
begin
	result := ptRecycleBin;
end;

function TRecycleHandler.NameResource (): integer;
begin
	result := sr_RecycleBin;
end;

function TRecycleHandler.MenuItem_Manage (): integer;
begin
	result := m_recyclebin;
end;

//---------------------

initialization
	PageFactory.RegisterClass (TRecycleBin);
   PageImageList.AddImageWithOverlay (ptRecycleBin, m_recyclebin);

finalization

end.

