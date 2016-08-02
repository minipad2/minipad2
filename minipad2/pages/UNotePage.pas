unit UNotePage;

interface

uses UPageSuper, UTypeDef;

type
   TNotePage = class (TEditPageSuper)
   public
   	class function PageType (): TPageType; override;
      function ImageIndex (): integer; override;
   end;

implementation

uses UPageFactory, UxlFunctions, Resource;

class	function TNotePage.PageType (): TPageType;
begin
	result := ptNote;
end;

function TNotePage.ImageIndex (): integer;
begin
   if PageProperty.ExternalSave and (not PathFileExists(Icon)) then
      result := PageImageList.IndexOf (ptExternalNote) + Ord(Status)
   else
      result := inherited ImageIndex;
end;

//------------------

initialization
   PageFactory.RegisterClass (TNotePage);
   PageImageList.AddImageWithOverlay (ptNote, m_newnote);
   PageImageList.AddImageWithOverlay (ptExternalNote, ic_externalnote);
	PageNameMan.RegisterDefName (ptNote, sr_defnotename);

finalization

end.

