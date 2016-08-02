unit UNavigatorSuper;

interface

uses UxlClasses, UPageSuper, UxlExtClasses, UxlDragControl, UPageFactory, UTypeDef;

type
   TNavigatorSuper = class (TxlInterfacedObject, IPageObserver)
   private
   	FOnDropItems: TDropPageEvent;
   	FActive: boolean;
   	procedure SetActive (value: boolean);
   protected
   	function f_NameToLabel (o_page: TPageSuper): widestring;

      procedure LoadIndex (); virtual; abstract;
      procedure SelectPage (value: TPageSuper); virtual; abstract;
		procedure AddPage (value: TPageSuper); virtual; abstract;
      procedure ResetPage (value: TPageSuper); virtual; abstract;
      procedure DeletePage (value: TPageSuper); virtual; abstract;
      procedure RenamePage (value: TPageSuper); virtual; abstract;
   public
   	function Control (): TxlDragControl; virtual; abstract;
   	procedure OnPageEvent (pct: TPageEvent; id, id2: integer);

      property OnDropItems: TDropPageEvent read FOnDropItems write FOnDropItems;
   	property Active: boolean read FActive write SetActive;
   end;

implementation

uses UGlobalObj, UPageStore, UxlCommDlgs, Resource;

function TNavigatorSuper.f_NameToLabel (o_page: TPageSuper): widestring;
begin
	result := o_page.name;
   case o_page.Status of
      psLocked: result := '+ ' + result;
      psProtected: result := '* ' + result;
      psReadOnly: result := '# ' + result;
   end;
end;

procedure TNavigatorSuper.SetActive (value: boolean);
begin
	FActive := value;
   if value then
   begin
   	LoadIndex;
      PageCenter.AddObserver (self);
      if PageCenter.ActivePage <> nil then
      	SelectPage (PageCenter.ActivePage);
   end
   else
   	PageCenter.RemoveObserver (self);
end;

procedure TNavigatorSuper.OnPageEvent (pct: TPageEvent; id, id2: integer);
begin
	case pct of
   	pctRenameDemand:
      	RenamePage (PageStore[id]);
   	pctFieldModified, pctSwitchStatus, pctIcon:
      	ResetPage (PageStore[id]);
      pctAddChild:
      	if PageStore[id].ChildShowInTree (PageStore[id2].PageType) then
         	AddPage (PageStore[id2]);
      pctRemoveChild:
     		if PageStore[id].ChildShowInTree (PageStore[id2].PageType) then
         	DeletePage (PageStore[id2]);
      pctSetActive:
      	SelectPage (PageStore[id]);
   end;
end;

end.



