unit UPluginManager;

interface

uses UTemplateHandler, ULinkHandler, UClipHandler, UMemoHandler, UPasteHandler, UDictionary, UxlWindow;

type
	TPluginManager = class
   private
	   FTemplateMan: TTemplateHandler;
      FLinkMan: TLinkHandler;
      FFastLinkMan: TFastLinkHandler;
      FClipMan: TClipHandler;
      FMemoMan: TMemoHandler;
      FPopupMan: TPopupHandler;
      FPasteMan: TPasteHandler;
   	FTipQuery: TTipQuery;
   public
   	constructor Create (AOwner: TxlWindow);
      destructor Destroy (); override;
   end;

implementation

uses UOptionManager, UPageStore, UGlobalObj;

constructor TPluginManager.Create (AOwner: TxlWindow);
begin
   with OptionMan.Options do
   begin
      if EnableTemplate or EnableClipboard then
      	FPasteMan := TPasteHandler.Create (AOwner);

      if EnableTemplate or EnableClipboard or EnableLinkPage then
         FPopupMan := TPopupHandler.Create (AOwner);

      if EnableMemoPage then
         FMemoMan := TMemoHandler.Create;

      if EnableLinkPage then
      begin
         FLinkMan := TLinkHandler.Create (AOwner);
         FFastLinkMan := TFastLinkHandler.Create (Root);
         FFastLinkMan.PopupHandler := FPopupMan;
      end;

      if EnableTemplate then
      begin
      	FTemplateMan := TTemplateHandler.Create (Root);
         FTemplateMan.PopupHandler := FPopupMan;
         FTemplateMan.PasteHandler := FPasteMan;
      end;

      if EnableClipboard then
      begin
      	FClipMan := TClipHandler.Create (Root, AOwner);
         FClipMan.PopupHandler := FPopupMan;
         FClipMan.PasteHandler := FPasteMan;
      end;

      if EnableDictPage then
      	FTipQuery := TTipQuery.Create (AOwner);
   end;
end;

destructor TPluginManager.Destroy ();
begin
	FTipQuery.Free;
   FMemoMan.Free;
	FFastLinkMan.Free;
   FLinkMan.Free;
	FTemplateMan.Free;
//   FClipMan.Free;  // 在复制操作后，free时造成软件死掉，原因未详
   FPasteMan.Free;
   FPopupMan.Free;

	inherited;
end;

end.

//   TPluginCenter = class
//   private
//   public
//   	procedure RegisterPlugIn (value: TPlugin);
//   end;
