unit UFavoritePage;

interface

uses UPageSuper, USysPageHandler, UTypeDef, UxlClasses, UxlExtClasses, UxlList, UPageProperty;

type
   TFavoritePage = class (TPageContainer)
   protected
   public
   	function IsVirtualContainer (): boolean; override;
   	class function PageType (): TPageType; override;
      function CanAddChild (ptChild: TPageType): boolean; override;
		class procedure InitialListProperty (lp: TListProperty); override;
      class function SingleInstance (): boolean; override;
	end;

   TFavoriteHandler = class (TSysPageContainerHandler)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
   public
      procedure ExecuteCommand (opr: word); override;
   end;

implementation

uses UGlobalObj, UPageStore, UPageFactory, ULangManager, Resource;

class function TFavoritePage.PageType (): TPageType;
begin
	result := ptFavorite;
end;

function TFavoritePage.CanAddChild (ptChild: TPageType): boolean;
begin
	result := true;
end;

class function TFavoritePage.SingleInstance (): boolean;
begin
	result := true;
end;

function TFavoritePage.IsVirtualContainer (): boolean;
begin
	result := true;
end;

class procedure TFavoritePage.InitialListProperty (lp: TListProperty);
const c_cols: array[0..3] of integer = (sr_Title, sr_Abstract, sr_CreateTime, sr_NodePath);
	c_widths: array[0..3] of integer = (100, 150, 80, 100);
begin
	inherited InitialListProperty (lp);
	lp.ColList.Populate (c_cols);
   lp.WidthList.Populate (c_widths);
end;

//-------------------------

function TFavoriteHandler.PageType (): TPageType;
begin
	result := ptFavorite;
end;

function TFavoriteHandler.NameResource (): integer;
begin
	result := sr_FavoritePage;
end;

function TFavoriteHandler.MenuItem_Manage (): integer;
begin
	result := m_managefavorite;
end;

function TFavoriteHandler.MenuItem_List (): integer;
begin
	result := m_favorite;
end;

procedure TFavoriteHandler.ExecuteCommand (opr: word);
begin
	case opr of
      m_addfavorite:
         FPage.Childs.AddChild (PageCenter.ActivePage.Id);
      m_removefavorite:
	      FPage.Childs.RemoveChild (PageCenter.ActivePage.Id);
      else
      	inherited ExecuteCommand (opr);
   end;
end;

initialization
	PageFactory.RegisterClass (TFavoritePage);
   PageImageList.AddImageWithOverlay (ptFavorite, m_favorite);

finalization

end.
