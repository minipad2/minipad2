unit URootPageHandler;

interface

type
   TRootPageHandlerSuper = class (TxlInterfacedObject, ICommandExecutor, ILangObserver)
   private
   	FPage: TPageSuper;
		function Page (): TPageSuper;
   protected
   	function PageType (): TPageType; virtual; abstract;
      function NameResource (): integer; virtual; abstract;
      function MenuItem_Manage (): integer; virtual; abstract;
   public
   	constructor Create (); virtual;
      destructor Destroy (); override;

      function CheckCommand (opr: word): boolean; virtual;
      procedure ExecuteCommand (opr: word); virtual;
      procedure LanguageChanged ();
   end;

   TRootListPageHandler = class (TRootPageHandlerSuper, IListProvider)
   private
   protected
   	function MenuItem_List (): integer; virtual; abstract;
   public
   	constructor Create (); override;
      destructor Destroy (); override;

      function CheckCommand (opr: word): boolean; override;
      procedure ExecuteCommand (opr: word); override;
      procedure FillList (id: integer; o_list: TxlStrList);
	end;

   T
implementation

constructor TRootPageHandlerSuper.Create ();
begin
   CommandMan.AddExecutor (self);
   LangMan.AddObserver (self);
end;

destructor TRootPageHandlerSuper.Destroy ();
begin
	LangMan.RemoveObserver (self);
   CommandMan.RemoveExecutor (self);
   inherited;
end;

procedure TRootPageHandlerSuper.LanguageChanged ();
begin
   Page.Name := LangMan.GetItem(NameResource);
end;

function TRootPageHandlerSuper.Page (): TPageSuper;
var id: integer;
begin
	if FPage = nil then
   begin
		if not PageStore.GetFirstPageId (PageType, id) then
      	id := PageStore.NewPage (Root.id, PageType);
      FPage := PageStore[id];
      FPage.name := LangMan.GetItem (NameResource);
   end;
   result := FPage;
end;

function TRootPageHandlerSuper.CheckCommand (opr: word): boolean;
begin
   result := true;
end;

procedure TRootPageHandlerSuper.ExecuteCommand (opr: word);
begin
	case opr of
      MenuItem_Manage:
      	begin
            Root.Childs.AddChild (FPage.id);
            PageCenter.ActivePage := FPage;
         end;
   end;
end;

//-----------------------

constructor TRootListPageHandler.Create ();
begin
	inherited;
   ListCenter.AddProvider (self);
end;

destructor TRootListPageHandler.Destroy ();
begin
	ListCenter.RemoveProvider (self);
	inherited;
end;

procedure TRootListPageHandler.ExecuteCommand (opr: word);
var o_list: TxlIntList;
begin
	case opr of
      MenuItem_List + 1 .. MenuItem_List + 999:
         begin
            o_list := TxlIntList.Create;
            Page.Childs.GetChildList (o_list);
            PageCenter.ActivePage := PageStore[o_list[opr - MenuItem_List - 1]];
            o_list.free;
         end;
      else
      	inherited ExecuteCommand (opr);
   end;
end;

procedure TRootListPageHandler.FillList (id: integer; o_list: TxlStrList);
var i: integer;
	o_list2: TxlIntList;
begin
	if id <> MenuItem_List then exit;
   o_list2 := TxlIntList.Create;
   Page.GetChildList (o_list2);
   o_list.clear;
   for i := o_list2.Low to o_list2.High do
		o_list.Add (PageStore[o_list2[i]].Name);
   o_list2.free;
end;

end.
