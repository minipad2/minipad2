unit USearchPage;

interface

uses UTypeDef, UPageSuper, USysPageHandler, UxlClasses, UxlExtClasses, UxlList, UxlDialog, UxlStdCtrls, UxlWinControl, UxlComboBox,
	UPageProperty;

type
   TOpr = (oEqual, oNotEqual, oInclude, oNotInclude);

   TSearchCrit = record
   	PageType: TPageType;
   	Col: integer;
      Opr: TOpr;
   	Value: widestring;
      MatchCase: boolean;
   end;

   TSearchPage = class (TListPageSuper)
   private
   	FSearchCrit: TSearchCrit;
   public
   	class function PageType (): TPageType; override;
      class function SingleInstance (): boolean; override;
		procedure GetChildList (o_list: TxlIntList); override;
      class procedure GetListShowCols (o_list: TxlIntList); override;
		class procedure InitialListProperty (lp: TListProperty); override;
      property SearchCrit: TSearchCrit read FSearchCrit write FSearchCrit;
	end;

   TSearchBox = class (TxlDialog)
   private
   	FSearchCrit: TSearchCrit;
      FCmbPageType: TxlComboBox;
      FCmbCol: TxlComboBox;
      FCmbOpr: TxlComboBox;
      FCols: TxlIntList;
      FPageTypes: TxlIntList;
		procedure f_OnPageTypeChange (Sender: TObject);
      function f_GetCol (): integer;
//      function PageType (): TPageType;
   protected
      procedure OnInitialize (); override;
      procedure OnOpen (); override;
      procedure OnClose (); override;
      procedure OnCommand (ctrlID: integer); override;
   public
      property SearchCrit: TSearchCrit read FSearchCrit write FSearchCrit;
   end;

   TSearchHandler = class (TSysPageHandlerSuper)
   protected
   	function PageType (): TPageType; override;
      function NameResource (): integer; override;
      function MenuItem_Manage (): integer; override;
   public
      procedure ExecuteCommand (opr: word); override;
   end;

implementation

uses Windows, UPageStore, UxlStrUtils, UGlobalObj, UOptionManager, ULangManager, UPageFactory, UNotePage,
	UxlCommDlgs, Resource;

class function TSearchPage.PageType (): TPageType;
begin
	result := ptSearch;
end;

class function TSearchPage.SingleInstance (): boolean;
begin
	result := true;
end;

procedure TSearchPage.GetChildList (o_list: TxlIntList);
var i: integer;
	b: boolean;
   s1, s2: widestring;
   o_list2: TxlIntList;
begin
   o_list.Clear;
   s1 := FSearchCrit.Value;
   if s1 = '' then exit;
   if not FSearchCrit.MatchCase then
      s1 := lowercase (s1);

   o_list2 := TxlIntList.Create;
   PageStore.GetPageList (FSearchCrit.PageType, o_list2);
   for i := o_list2.Low to o_list2.High do
   begin
      s2 := PageStore[o_list2[i]].GetColTExt (FSearchCrit.Col);
      if not FSearchCrit.MatchCase then
         s2 := lowercase (s2);
      case FSearchCrit.Opr of
         oEqual: b := (s1 = s2);
         oNotEqual: b := (s1 <> s2);
         oInclude: b := IsSubStr (s1, s2);
         oNotInclude: b := not IsSubStr (s1, s2);
      end;
      if b then
         o_list.Add (o_list2[i]);
   end;
   o_list2.free;
end;

class procedure TSearchPage.GetListShowCols (o_list: TxlIntList);
begin
	inherited GetListShowCols (o_list);
   o_list.Add (sr_searchresult);
end;

class procedure TSearchPage.InitialListProperty (lp: TListProperty);
const c_cols: array[0..3] of integer = (sr_Title, sr_SearchResult, sr_ModifyTime, sr_NodePath);
	c_widths: array[0..3] of integer = (100, 150, 80, 100);
begin
	inherited InitialListProperty (lp);
	lp.ColList.Populate (c_cols);
   lp.WidthList.Populate (c_widths);
end;

//----------------------

const c_searchbox: array[0..4] of word = (st_pagetype, st_searchcrit, chk_matchcase, IDOK, IDCancel);
   c_opr: array[0..3] of integer = (sr_Equal, sr_NotEqual, sr_Include, sr_NotInclude);

procedure TSearchBox.OnInitialize ();
begin
	SetTEmplate (Search_Box, m_search);
end;

procedure TSearchBox.OnOpen ();
var i, n, id: integer;
	pt: TPageType;
   o_list: TxlIntList;
begin
	inherited;
   RefreshItemText (self, c_searchbox, Search_Box);

   FCmbOpr := TxlComboBox.Create (self, ItemHandle[cmb_opr]);
   for i := Low(c_opr) to High(c_opr) do
   	FCmbOpr.Items.Add (LangMan.GetItem(c_opr[i]));
   FCmbOpr.Items.SelIndex := Ord(FSearchCrit.Opr);

   FCmbCol := TxlComboBox.Create (self, ItemHandle[cmb_col]);
   FCols := TxlIntList.Create;

   FCmbPageType := TxlComboBox.Create (self, ItemHandle[cmb_pagetype]);
   FCmbPageType.Items.OnSelChange := f_OnPageTypeChange;

   FPageTypes := TxlIntList.Create;
   o_list := TxlIntList.Create;
   PageFactory.GetPageTypeList (o_list);
   n := -1;
   for i := o_list.Low to o_list.High do
   begin
   	pt := TPageType(o_list[i]);
   	if not PageFactory.GetClass (pt).CanSearch then continue;
      if not PageStore.GetFirstPageId (pt, id) then continue;

      inc (n);
   	FCmbPageType.Items.Add (LangMan.GetItem(sr_GroupPage + o_list[i]));
      if pt = FSearchCrit.PageType then
      begin
      	FCmbPageType.Items.SelIndex := n;
         f_OnPageTypeChange (self);
      end;
      FPageTypes.Add (o_list[i]);
   end;
   o_list.free;

   ItemTExt[sle_Value] := FSearchCrit.Value;
   ItemChecked[chk_matchcase] := FSearchCrit.MatchCase;
   FocusControl (sle_value);
end;

procedure TSearchBox.OnClose ();
begin
	FCmbPageType.free;
   FCols.free;
   FCmbCol.free;
   FPageTypes.free;
   inherited;
end;

function TSearchBox.f_GetCol (): integer;
begin
   result := FCols [FCmbCol.Items.SelIndex];
end;

procedure TSearchBox.f_OnPageTypeChange (Sender: TObject);
var i, col: integer;
	o_class: TPageClass;
begin
	if FCmbCol.Items.Count > 0 then
   	col := f_GetCol
   else
   	col := FSearchCrit.Col;
	i := FCmbPageType.Items.SelIndex;

   o_class := PageFactory.GetClass (TPageType(FPageTypes[i]));
   o_class.GetSearchCols (FCols);
   FCmbCol.Items.Clear;
   for i := FCols.Low to FCols.High do
   begin
      FCmbCol.Items.Add (LangMan.GetItem(FCols[i]));
      if FCols[i] = col then
         FCmbCol.Items.SelIndex := i;
   end;
end;

procedure TSearchBox.OnCommand (ctrlID: integer);
begin
	if ctrlID = IDOK then
   	with FSearchCrit do
      begin
         PageType := TPageType(FPageTypes[FCmbPageType.Items.SelIndex]);
         Col := f_GetCol;
         Opr := TOpr(FCmbOpr.Items.SelIndex);
         Value := ItemText[sle_value];
         MatchCase := ItemChecked[chk_matchcase];
      end;
	inherited OnCommand (ctrlID);
end;

//----------------------

function TSearchHandler.PageType (): TPageType;
begin
	result := ptSearch;
end;

function TSearchHandler.NameResource (): integer;
begin
	result := sr_SearchPage;
end;

function TSearchHandler.MenuItem_Manage (): integer;
begin
	result := m_search;
end;

procedure TSearchHandler.ExecuteCommand (opr: word);
var o_box: TSearchBox;
	sc: TSearchCrit;
begin
	if opr = m_search then
   begin
      o_box := TSearchBox.Create ();
      o_box.SearchCrit := MemoryMan.SearchCrit;
      if o_box.Execute then
      begin
      	sc := o_box.SearchCrit;
      	MemoryMan.SearchCrit := sc;
         TSearchPage(FPage).SearchCrit := sc;
         if (sc.PageType in [ptNote, ptCalc, ptDict]) then
         begin
            if (sc.Col = sr_Text) and (sc.Opr = oInclude) then
            begin
               GSearchText := sc.Value;
               GMatchCase := sc.MatchCase;
            end
            else
               GSearchText := '';
            EventMan.EventNotify (e_HighlightSearchResult);
         end;
			inherited ExecuteCommand (opr);
      end;
      o_box.free;
   end;
end;

initialization
	PageFactory.RegisterClass (TSearchPage);
   PageImageList.AddImageWithOverlay (ptSearch, m_search);
//   PageDefSettingsMan.RegisterType (ptSearch, TListPageSuper.ListInitialsettings);

finalization

end.


