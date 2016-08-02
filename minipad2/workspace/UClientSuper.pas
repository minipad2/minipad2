unit UClientSuper;

interface

uses UxlClasses, UPageSuper, UPageFactory, UxlDragControl, UTypeDef, UxlWinControl, UxlExtClasses;

type
   TClientSuper = class (TxlInterfacedObject, IPageObserver, ICommandExecutor)
	private
   	FActive: boolean;
   	FOnDropItems: TDropPageEvent;
   	procedure SetActive (value: boolean);
   protected
      FPage: TPageSuper;
		procedure Load (value: TPageSuper); virtual;
      procedure Refresh ();
      procedure UnLoad (); virtual; abstract;
   public
   	function Control (): TxlControl; virtual; abstract;
   	procedure OnPageEvent (pct: TPageEvent; id, id2: integer); virtual;
      procedure Save (); virtual; abstract;

      property OnDropItems: TDropPageEvent read FOnDropItems write FOnDropItems;
   	property Active: boolean read FActive write SetActive;
      function CheckCommand (opr: word): boolean; virtual; abstract;
      procedure ExecuteCommand (opr: word); virtual; abstract;
      property Page: TPageSuper read FPage;
   end;

   TListClientSuper = class (TClientSuper)
   protected
   	function Items (index: integer): TPageSuper; virtual; abstract;
   	procedure f_OnItemDblClick (idx: integer);
      procedure f_OnSelectItem (index: integer; sel: boolean);
	end;

implementation

uses Windows, UGlobalObj, UOptionManager, Resource, UPageStore, ULangManager;

procedure TClientSuper.Load (value: TPageSuper);
begin
	if value <> nil then
		FPage := value;
end;

procedure TClientSuper.Refresh ();
begin
	if FPage <> nil then
   	Load (FPage);
end;

procedure TClientSuper.SetActive (value: boolean);
begin
	FActive := value;
   if value then
   begin
   	CommandMan.AddExecutor (self);
      PageCenter.AddObserver (self);
   end
   else
   begin
      CommandMan.RemoveExecutor (self);
   	PageCenter.RemoveObserver (self);
      UnLoad;
   end;
end;

procedure TClientSuper.OnPageEvent (pct: TPageEvent; id, id2: integer);
begin
	case pct of
      pctSave:
      	if (FPage <> nil) and (id = FPage.id) then
     			Save ();
      pctSetActive:
      	Load (PageStore[id]);
   end;
end;

//-----------------------

procedure TListClientSuper.f_OnItemDblClick (idx: integer);
begin
	if idx < 0 then
   begin
   	if OptionMan.Options.GroupDblClick = lbLevelUp then
   		CommandMan.ExecuteCommand(m_levelup)
      else
      	CommandMan.ExecuteCommand (m_newitem);
   end
   else
   begin
   	if Items(idx).PageType = ptLinkItem then
      	EventMan.EventNotify (e_ExecuteLink, Items(idx).id)
      else
      	CommandMan.ExecuteCommand (m_edititem);
   end;
end;

procedure TListClientSuper.f_OnSelectItem (index: integer; sel: boolean);
begin
   if sel then
   	PageCenter.EventNotify (pctSelect, Items(index).id)
   else
   	PageCenter.EventNotify (pctDeSelect, Items(index).id);
end;

end.
