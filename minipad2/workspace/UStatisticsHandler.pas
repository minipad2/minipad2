unit UStatisticsHandler;

interface

uses UxlClasses, UxlExtClasses, UxlDialog, UxlTabControl, UxlList, UPageSuper;

type
	TStatisticsHandler = class (TxlInterfacedObject, ICommandExecutor)
   private
   public
   	constructor Create ();
      destructor Destroy (); override;

      function CheckCommand (opr: word): boolean;
      procedure ExecuteCommand (opr: word);
   end;

   TStatisticsBox = class (TxlDialog)
   private
      FTabCtrl: TxlTabControl;
      
		procedure f_OnPageSwitched (index: integer);
      procedure f_GetCurrentPageInfo (o_itemlist: TxlStrList; o_countlist: TxlStrList);
      procedure f_GetDatabaseInfo (o_itemlist: TxlStrList; o_countlist: TxlStrList);
      procedure f_GetEditPageInfo (pg: TPageSuper; o_itemlist: TxlStrList; o_countlist: TxlStrList);
      procedure f_GetListPageInfo (pg: TPageSuper; o_itemlist: TxlStrList; o_countlist: TxlStrList);
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
   public
   end;

implementation

uses UGlobalObj, ULangManager, UxlFunctions, UxlStrUtils, UPageFactory, UPageStore, UTypeDef, Resource;

constructor TStatisticsHandler.Create ();
begin
   CommandMan.AddExecutor (self);
end;

destructor TStatisticsHandler.Destroy ();
begin
	CommandMan.RemoveExecutor (self);
   inherited;
end;

function TStatisticsHandler.CheckCommand (opr: word): boolean;
begin
   result := true;
end;

procedure TStatisticsHandler.ExecuteCommand (opr: word);
var o_box: TStatisticsBox;
begin
	if opr = m_statistics then
   begin
   	SaveMan.Save;
   	o_box := TStatisticsBox.Create();
      o_box.Execute;
      o_box.free;
   end;
end;

//---------------------------

procedure TStatisticsBox.OnInitialize ();
begin
	SetTemplate (Statistics_Box, m_statistics);
end;

procedure TStatisticsBox.OnOpen ();
begin
	inherited;
	Text := LangMan.GetItem(Statistics_Box, Text);

   FTabCtrl := TxlTabControl.create(self, ItemHandle[tab_switch]);
   FTabCtrl.Items.Add (LangMan.GetItem(tp_CurrentPage));
   FTabCtrl.Items.Add (LangMan.GetItem(tp_Database));
   FTabCtrl.Items.OnSelChanged := f_OnPageSwitched;
   f_OnPageSwitched (0);
end;

procedure TStatisticsBox.OnClose ();
begin
	FTabCtrl.free;
   inherited;
end;

procedure TStatisticsBox.f_OnPageSwitched (index: integer);
var o_itemlist: TxlStrList;
	o_countlist: TxlStrList;
begin
	o_itemlist := TxlStrList.Create;
   o_countlist := TxlStrList.Create;
   if index = 0 then
   	f_GetCurrentPageInfo (o_itemlist, o_countlist)
   else
   	f_GetDatabaseInfo (o_itemlist, o_countlist);
	ItemText[st_count_left] := o_itemlist.Text;
   ItemText[st_count_right] := o_countlist.Text;
   o_itemlist.free;
   o_countlist.free;
end;

//--------------------------

procedure TStatisticsBox.f_GetCurrentPageInfo (o_itemlist: TxlStrList; o_countlist: TxlStrList);
var pg: TPageSuper;
begin
	pg := PageCenter.ActivePage;
   if pg = nil then exit;

   if pg.PageControl = pcEdit then
   	f_GetEditPageInfo (pg, o_itemlist, o_countlist)
   else
   	f_GetListPageInfo (pg, o_itemlist, o_countlist);
end;

procedure TStatisticsBox.f_GetEditPageInfo (pg: TPageSuper; o_itemlist: TxlStrList; o_countlist: TxlStrList);
var s_text: widestring;
   i, i_crcount, i_wordcount, i_charcount, i_truecharcount, i_paracount: cardinal;
   p, q: pword;
   b_lastiscr: boolean;
begin
	s_text := pg.Text;
   p := pword(pwidechar(s_Text));
   q := nil;
   i_crcount := 0;
   i_wordcount := 0;
   i_paracount := 0;
   b_lastiscr := true;

   while p^ <> 0 do
   begin
      if (p^ = 13) then
      begin
         inc(i_crcount);
         if not b_lastIsCr then
         	inc (i_paracount);
         b_lastiscr := true;
      end
      else if p^ <> 10 then
      	b_lastIsCr := false;
      if ((q = nil) or IsWordBreak (q^)) and (not IsWordBreak(p^)) then
         inc(i_wordcount);
      q := p;
      inc (p);
   end;
   if not b_lastIsCr then
   	inc (i_paracount);
   i_charcount := Length(s_text);
   i_truecharcount := i_charcount;
   for i := 1 to i_charcount do
      if (s_text[i] <= #32) or (s_text[i] = #127) then
         dec (i_truecharcount);

   with o_itemlist do
   begin
   	Add (LangMan.GetItem(sr_CharCount, '总字符数：'));
      Add (LangMan.GetItem(sr_ExCrCharCount, '非回车字符数：'));
      Add (LangMan.GetItem(sr_ExBlCharCount, '非空字符数：'));
      Add (LangMan.GetItem(sr_WordCount, '单词数：'));
      Add (LangMan.GetItem(sr_LineCount, '行数：'));
      Add (LangMan.GetItem(sr_ParaCount, '段落数：'));
   end;
   with o_countlist do
   begin
      Add (IntToStr(i_charcount));
      Add (IntToStr(i_charcount - i_crcount * 2));
      Add (IntToStr(i_truecharcount));
      Add (IntToStr(i_wordcount));
      Add (IntToStr(i_crcount));
      Add (IntToStr(i_paracount));
   end;
end;

procedure TStatisticsBox.f_GetListPageInfo (pg: TPageSuper; o_itemlist: TxlStrList; o_countlist: TxlStrList);
var o_list: TxlIntList;
begin
	o_itemlist.Add (LangMan.GEtItem(sr_ItemCount, '项目数：'));
   o_list := TxlIntList.Create;
   pg.GetChildList (o_list);
   o_countlist.Add (IntToStr(o_list.Count));
   o_list.free;
end;

//------------------------------

procedure TStatisticsBox.f_GetDatabaseInfo (o_itemlist: TxlStrList; o_countlist: TxlStrList);
var o_list: TxlIntList;
	i: integer;
   o_class: TPageClass;
   pt: TPageType;
begin
	o_list := TxlIntList.Create;
	PageStore.GetPageTypeList (o_list);
   for i := o_list.Low to o_list.High do
   begin
   	pt := TPageType(o_list[i]);
   	o_class := PageFactory.GetClass (pt);
   	if o_Class.SingleInstance then continue;
      if (pt in [ptNote, ptMemoItem]) and (i > 0) then
      begin
      	o_itemlist.Add ('');
         o_countlist.Add ('');
      end;
		o_itemlist.Add (LangMan.GetItem(sr_GroupPage + o_list[i]));
   	o_countlist.Add (IntToStr(PageStore.GetPageCount(pt)));
   end;
   o_list.free;
end;

end.
