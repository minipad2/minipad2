unit UCalcDictPage;

interface

uses UPageSuper, UTypeDef;

type
   TCalcPage = class (TEditPageSuper)
   protected
   public
   	class function PageType (): TPageType; override;
	end;

   TDictPage = class (TEditPageSuper)
   public
   	class function PageType (): TPageType; override;
	end;

implementation

uses UPageFactory, Resource;

class	function TCalcPage.PageType (): TPageType;
begin
	result := ptCalc;
end;

//------------------

class	function TDictPage.PageType (): TPageType;
begin
	result := ptDict;
end;

//------------------

initialization
   PageFactory.RegisterClass (TCalcPage);
   PageImageList.AddImageWithOverlay (ptCalc, m_newcalc);
	PageNameMan.RegisterDefName (ptCalc, sr_defcalcname);

   PageFactory.RegisterClass (TDictPage);
   PageImageList.AddImageWithOverlay (ptDict, m_newdict);
	PageNameMan.RegisterDefName (ptDict, sr_defdictname);

finalization

end.
