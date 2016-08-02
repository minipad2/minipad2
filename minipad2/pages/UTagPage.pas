unit UTagPage;

interface

uses UPageSuper, UTypeDef;

type
   TTagPage = class (TPageContainer)
   public
   	class function PageType (): TPageType; override;
		function ChildShowInTree (ptChild: TPageType): boolean; override;
      function CanAddChild (ptChild: TPageType): boolean; override;
      function CanOwnChild (ptChild: TPageType): boolean; override;
      class function DefChildType (): TPageType; override;
	end;

implementation

uses UxlFunctions, UPageFactory, Resource;

class function TTagPage.PageType (): TPageType;
begin
	result := ptTag;
end;

function TTagPage.CanAddChild (ptChild: TPageType): boolean;
begin
	result := ptChild in [ptGroup, ptNote, ptCalc, ptMemo, ptDict, ptLink, ptContact, ptTemplate, ptFastLink, ptSheet, ptTag];
end;

function TTagPage.CanOwnChild (ptChild: TPageType): boolean;
begin
	result := ptChild = ptTag;
end;

class function TTagPage.DefChildType (): TPageType;
begin
	result := ptTag;
end;

function TTagPage.ChildShowInTree (ptChild: TPageType): boolean;
begin
	result := ptChild = ptTag;
end;

//-------------------------

initialization
	PageFactory.RegisterClass (TTagPage);
   PageImageList.AddImageWithOverlay (ptTag, m_tag);
	PageNameMan.RegisterDefName (ptTag, sr_deftagname);

finalization

end.
