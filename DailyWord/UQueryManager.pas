unit UQueryManager;

interface

uses SysUtils, UxlClasses, UxlExtClasses, UxlWindow, UWordBox;

type
	TWordChangeEvent = procedure (o_word: TWord) of object;

   TQueryManager = class (TxlInterfacedObject, IOptionObserver, IMemorizer, ICommandExecutor)
   private
      FWordBox: TWordBox;
      FOnBoxChange: TNotifyEvent;

      FRandomBox: boolean;
      FRandomWord: boolean;
      FNewBoxCycle: integer;

      FSwitchCount: integer;
      FBoxIndex: integer;
      FWordIndex: integer;

		procedure f_NewBox (b_changeindex: boolean);
      procedure NewBox ();
   public
   	constructor Create (WndParent: TxlWindow);
      destructor Destroy (); override;
      function GetWord (): TWord;
      function CycleCount (): integer;
      
      procedure ExecuteCommand (opr: word);
      function CheckCommand (opr: word): boolean;
		procedure OptionChanged ();
      procedure RestoreMemory ();
      procedure SaveMemory ();

      property OnBoxChange: TNotifyEvent read FOnBoxChange write FOnBoxChange;
   end;

implementation

uses UGlobalObj, UxlList, UxlMath, UxlCommDlgs, UxlWinClasses, UxlStrUtils, UxlFunctions, Resource;

constructor TQueryManager.Create (WndParent: TxlWindow);
begin
   FWordBox := TWordBox.Create;

   MemoryMan.AddObserver (self);
	OptionMan.AddObserver (self);
   CommandMan.AddExecutor (self);
end;

destructor TQueryManager.Destroy ();
begin
	CommandMan.RemoveExecutor(self);
	OptionMan.RemoveObserver (self);
   MemoryMan.RemoveObserver (self);
   FWordBox.Free;
	inherited;
end;

//---------------

procedure TQueryManager.ExecuteCommand (opr: word);
   procedure DoExportBox ();
   var o_list: TxlStrList;
   begin
      o_list := TxlStrList.Create;
      FWordBox.Export (o_list);
      Clipboard.Text := o_list.Text;
      o_list.Free;
      ProgTip.ShowTip ('当前词盒内容已复制到剪贴板!');
   end;
begin
	case opr of
      m_newBox: NewBox;
      m_exportBox: DoExportBox;
      m_IncLevel: ;
      m_DecLevel: ;
      m_Delete: ;
      m_Prior: ;
      m_Next: ;
   end;
end;

function TQueryManager.CheckCommand (opr: word): boolean;
begin
  	result := true;
end;

procedure TQueryManager.NewBox ();
begin
	f_NewBox (true);
   if assigned (FOnBoxChange) then
      FOnBoxChange (self);
end;

procedure TqueryManager.f_NewBox (b_changeindex: boolean);
var i: integer;
begin
   if FWordBox.BoxCount = 0 then
      raise Exception.Create ('词库为空或尚未定义!');

   if b_changeindex then
   begin
      if FRandomBox then
      begin
         repeat
            i := RandomInt (0, FWordBox.BoxCount - 1);
         until (i <> FBoxIndex) or (FWordBox.BoxCount = 1);
         FBoxIndex := i;
      end
      else
         inc (FBoxIndex);
   	FWordIndex := -1;
   end;
   FBoxIndex := FBoxIndex mod FWordBox.BoxCount;

   FWordBox.NewBox (FBoxIndex);
end;

//-------------------

procedure TQueryManager.OptionChanged ();
begin
   FRandomBox := OptionMan.Options.RandomNewBox;
   FRandomWord := OptionMan.Options.RandomSwitchWord;
   if OptionMan.Options.AutoNewBox then
   	FNewBoxCycle := OptionMan.Options.NewBoxCycle
   else
   	FNewBoxCycle := 0;
	if (FWordBox.DictFile <> ProgDir + OptionMan.Options.DictFile) or (FWordBox.BoxSize <> OptionMan.Options.BoxSize) then
   begin
      FWordBox.BoxSize := OptionMan.Options.BoxSize;
      FWordBox.DictFile := ProgDir + OptionMan.Options.DictFile;
      try
      	f_NewBox (false);
      except
      end;
   end;
end;

procedure TQueryManager.RestoreMemory ();
begin
   FSwitchCount := MemoryMan.SwitchCount;
   FBoxIndex := MemoryMan.BoxIndex;
   FWordIndex := MemoryMan.WordIndex;
end;

procedure TQueryManager.SaveMemory ();
begin
	MemoryMan.SwitchCount := FSwitchCount;
   MemoryMan.BoxIndex := FBoxIndex;
   MemoryMan.WordIndex := FWordIndex;
end;

function TQueryManager.GetWord (): TWord;
var i: integer;
begin
   if FWordBox.WordCount = 0 then  // 程序启动后第一次载入词库
      f_NewBox (false)
   else if (FNewBoxCycle > 0) and (CycleCount >= FNewBoxCycle) then
   begin
   	f_NewBox (true);
      FSwitchCount := 0;
   end;

   if FWordBox.WordCount = 0 then
   	raise Exception.Create ('词盒为空!');

   if FRandomWord then
   begin
   	repeat
      	i := RandomInt (0, FWordBox.WordCount - 1);
      until (i <> FWordIndex) or (FWordBox.WordCount = 1);
      FWordIndex := i;
   end
   else
      FWordIndex := (FWordIndex + 1 ) mod FWordBox.WordCount;
   result := FWordBox.Word[FWordIndex];

   inc (FSwitchCount);
end;

function TQueryManager.CycleCount (): integer;
begin
	result := FSwitchCount div FWordBox.BoxSize;
end;

end.

