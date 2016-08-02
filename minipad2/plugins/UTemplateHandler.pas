unit UTemplateHandler;

interface

uses UPageSuper, USysPageHandler, UTypeDef, UxlClasses, UxlExtClasses, UxlList, UPasteHandler;

type
   TTemplateHandler = class (THandlerWithPaste, IEventObserver)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   	function MenuItem_List (): integer; override;
      procedure ExecuteItem (id: integer); override;
   public
   	constructor Create (AParent: TPageSuper); override;
      destructor Destroy (); override;

      procedure EventNotify (event, wparam, lparam: integer);
      procedure OptionChanged (); override;
      procedure OnHotkey (id: integer; hk: THotkey); override;
   end;

implementation

uses UGlobalObj, UOptionManager, UPageStore, ULangManager, UPageFactory, UxlWinClasses, UExtFuncs, UTemplatePage, UxlFunctions, Resource;

constructor TTemplateHandler.Create (AParent: TPageSuper);
var i, id: integer;
	o_list: TxlIntList;
   hk: THotkey;
begin
	inherited Create (AParent);
   o_list := TxlIntList.Create;
   PageStore.GetPageList (ptTemplateItem, o_list);

   for i := o_list.Low to o_list.High do
   begin
   	id := o_list[i];
      hk := TTemplateItem(PageStore[id]).Template.HotKey;
      if hk <> 0 then
      	HotkeyCenter.AddHotKey (self, id, hk);
   end;
   o_list.free;
   EventMan.AddObserver (self);
end;

destructor TTemplateHandler.Destroy ();
begin
	EventMan.RemoveObserver (self);
	inherited;
end;

function TTemplateHandler.PageType (): TPageType;
begin
	result := ptTemplate;
end;

function TTemplateHandler.NameResource (): integer;
begin
	result := sr_TemplatePage;
end;

function TTemplateHandler.MenuItem_Manage (): integer;
begin
	result := m_Template;
end;

function TTemplateHandler.MenuItem_List (): integer;
begin
	result := m_InsertTemplate;
end;

procedure TTemplateHandler.ExecuteItem (id: integer);
begin
   FPasteHandler.Paste (DecodeTemplate(PageStore[id].GetColText (sr_Text)), true);
end;

procedure TTemplateHandler.OptionChanged ();
begin
	with OptionMan.Options do
   	AddHotkeyAndSubKey (Templatehotkey, EnableTemplItemHotkey);
end;

procedure TTemplateHandler.OnHotkey (id: integer; hk: THotkey); 
begin
   if PageStore.PageValid (id) and (PageStore[id].PageType = ptTemplateItem) then
   begin
   	ReleaseHotkey (hk);
   	ExecuteItem (id);
   end
   else
		inherited OnHotKey (id, hk);
end;

procedure TTemplateHandler.EventNotify (event, wparam, lparam: integer);
begin
	case event of
   	e_TemplateHotKeyChanged:
         HotkeyCenter.AddHotkey (self, wparam, lparam);
      e_TemplateItemDeleted:
        	HotkeyCenter.RemoveHotkey (self, wparam);
   end;
end;

end.
