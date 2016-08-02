unit USysPageManager;

interface

uses URecentPage, UFavoritePage, USearchPage, UGroupPage, URecycleBin;

type
	TSysPageManager = class
   private
      FRecentHandler: TRecentRootHandler;
      FFavoriteHandler: TFavoriteHandler;
      FSearchHandler: TSearchHandler;
      FGroupRootHandler: TGroupRootHandler;
      FRecycleHandler: TRecycleHandler;
   public
   	constructor Create ();
      destructor Destroy (); override;
   end;

//function GroupRoot (): TPageSuper;
//   function FirstNote (): TPageSuper;

implementation

uses UPageStore;

//function GroupRoot (): TPageSuper;
//begin
//	result := PageStore[GroupRootId];
//end;

constructor TSysPageManager.Create ();
begin
   FRecentHandler := TRecentRootHandler.Create (Root);
   FFavoriteHandler := TFavoriteHandler.Create (Root);
	FSearchHandler := TSearchHandler.Create (Root);
   FGroupRootHandler := TGroupRootHandler.Create (Root);
   FRecycleHandler := TRecycleHandler.Create (Root);

   if Root.Childs.IsEmpty then
   	with Root.Childs do
      begin
         AddChild (FRecentHandler.Page.id);
         AddChild (FFavoriteHandler.Page.id);
         AddChild (FSearchHandler.Page.id);
         AddChild (FGroupRootHandler.Page.id);
         AddChild (FRecycleHandler.Page.id);
      end;
end;

destructor TSysPageManager.Destroy (); 
begin
   FRecycleHandler.Free;
   FGroupRootHandler.Free;
   FSearchHandler.free;
   FFavoriteHandler.Free;
   FRecentHandler.free;
	inherited;
end;

end.
