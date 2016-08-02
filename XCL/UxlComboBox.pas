unit UxlComboBox;

interface

uses Windows, UxlClasses, UxlWinControl, UxlList;

type
	TListItems = class (TxlCollection)
   private
   	FOwner: TxlWinControl;
      FOnSelChange: TNotifyEvent;
      function f_getcursel (): integer;
   public
   	constructor Create (AOwner: TxlWinControl);
      function Count (): integer; override;
      function Add (const s: widestring): integer; overload;
      procedure Select (index: integer);
      function ProcessCommand (dwEvent: word): dword;

      property SelIndex: integer read f_getcursel write Select;
      property OnSelChange: TNotifyEvent read FOnSelChange write FOnSelChange;
	end;

   TxlListBox = class (TxlControl)
   private
      FItems: TListItems;
   protected
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
   	function DoCreateControl (HParent: HWND): HWND; override;
   public
      function ProcessCommand (dwEvent: word): dword; override;
      property Items: TListItems read FItems;
   end;

type
	TComboItems = class (TxlCollection)
   private
   	FOwner: TxlWinControl;
      FOnSelChange: TNotifyEvent;
      function f_getcursel (): integer;
      function f_getString (i_index: integer): widestring;
      function f_GetNumber (i_index: integer): integer;
   public
   	constructor Create (AOwner: TxlWinControl);
      function Count (): integer; override;
      procedure Clear ();
      function ProcessCommand (dwEvent: word): dword;

      function Add (const s: widestring): integer; overload;
      function Find (const s: widestring): integer; overload;
      function ItemExists (const s: widestring): boolean; overload;
      procedure Populate (const strArray: array of widestring); overload;
      procedure Populate (o_list: TxlStrList); overload;

      function Add (i: integer): integer; overload;
      function Find (i: integer): integer; overload;
      function ItemExists (i: integer): boolean; overload;
      procedure PopulateInt (const IntArray: array of Integer);
      procedure Populate (o_list: TxlIntList); overload;

      property Items[i_index: integer]: widestring read f_GetString; default;
      property IntItems[i_index: integer]: integer read f_GetNumber;

      procedure Select (index: integer);
      property SelIndex: integer read f_getcursel write Select;
      property OnSelChange: TNotifyEvent read FOnSelChange write FOnSelChange;
   end;

   TxlComboBox = class (TxlControl)
   private
      FAllowEdit: boolean;
      FItems: TComboItems;
      procedure f_SetAllowEdit (value: boolean);
   protected
      function DoCreateControl (HParent: HWND): HWND; override;
      procedure OnCreateControl (); override;
      procedure OnDestroyControl (); override;
      procedure SetText (const s_text: widestring); override;
   public
      function ProcessCommand (dwEvent: word): dword; override;
      property AllowEdit: boolean read FAllowEdit write f_SetAllowEdit;
      property Items: TComboItems read FItems;
   end;

implementation

uses Messages, CommCtrl, UxlFunctions;

constructor TListItems.Create(AOwner: TxlWinControl);
begin
	FOwner := AOwner;
end;

function TListItems.Count (): integer;
begin
	result := FOwner.Perform (LB_GETCOUNT, 0, 0);
end;

function TListItems.Add (const s: widestring): integer;
begin
	result := FOwner.Perform (LB_ADDSTRING, 0, lparam(pwidechar(s)));
end;

procedure TListItems.Select (index: integer);
begin
	FOwner.Perform (LB_SETCURSEL, index, 0);
end;

function TListItems.f_getcursel (): integer;
begin
	result := FOwner.Perform (LB_GETCURSEL, 0, 0);
   if result = CB_ERR then result := -1;
end;

function TListItems.ProcessCommand (dwEvent: word): dword;
begin
	result := 0;
   case dwEvent of
      LBN_SELCHANGE:
         if assigned (FOnSelChange) then
            FOnSelChange (self);
   end;
end;

//-------------------

procedure TxlListBox.OnCreateControl ();
begin
	inherited;
   FItems := TListItems.Create (self);
end;

procedure TxlListBox.OnDestroyControl ();
begin
	FItems.Free;
   inherited;
end;

function TxlListBox.DoCreateControl (HParent: HWND): HWND;
begin
	result := CreateWin32Control (HParent, 'ListBox', WS_VISIBLE or LBS_NOINTEGRALHEIGHT or LBS_HASSTRINGS);
end;

function TxlListBox.ProcessCommand (dwEvent: word): dword;
begin
	result := FItems.ProcessCommand (dwEvent);
end;

//--------------------

constructor TComboItems.Create(AOwner: TxlWinControl);
begin
	FOwner := AOwner;
end;

procedure TComboItems.Select (index: integer);
begin
	FOwner.Perform (CB_SETCURSEL, index, 0);
end;

function TComboItems.f_getcursel (): integer;
begin
	result := FOwner.Perform (CB_GETCURSEL, 0, 0);
   if result = CB_ERR then result := -1;
end;

function TComboItems.f_getString (i_index: integer): widestring;
var i: integer;
begin
	i := FOwner.Perform (CB_GETLBTEXTLEN, i_index, 0);
   setlength (result, i);
	FOwner.Perform (CB_GETLBTEXT, i_index, lparam(pwidechar(result)));
end;

function TComboItems.f_GetNumber (i_index: integer): integer;
begin
	result := StrToInt (Items[i_index]);
end;

procedure TComboItems.Clear ();
begin
	FOwner.Perform (CB_RESETCONTENT, 0, 0);
end;

function TComboItems.Count (): integer;
begin
	result := FOwner.Perform (CB_GETCOUNT, 0, 0);
end;

function TComboItems.Add (const s: widestring): integer;
begin
	result := FOwner.Perform (CB_ADDSTRING, 0, lparam(pwidechar(s)));
end;

function TComboItems.Add (i: integer): integer;
begin
	result := Add (IntToStr(i));
end;

function TComboItems.Find (const s: widestring): integer;
begin
	result := FOwner.Perform (CB_FINDSTRING, dword(-1), lparam(pwidechar(s)));
end;

function TComboItems.Find (i: integer): integer;
begin
	result := Find (IntToStr(i));
end;

function TComboItems.ItemExists (const s: widestring): boolean;
begin
	result := Find (s) >= 0;
end;

function TComboItems.ItemExists (i: integer): boolean;
begin
	result := Find (i) >= 0;
end;

procedure TComboItems.PopulateInt (const IntArray: array of Integer);
var i: integer;
begin
	for i := 0 to Length(IntArray) - 1 do
   	Add (IntArray[i]);
end;

procedure TComboItems.Populate(o_list: TxlIntList);
var i: integer;
begin
   for i := o_list.Low to o_list.High do
   	Add (o_list[i]);
end;

procedure TComboItems.Populate (const strArray: array of widestring);
var i: integer;
begin
	for i := 0 to Length(strArray) - 1 do
   	Add (strArray[i]);
end;

procedure TComboItems.Populate(o_list: TxlStrList);
var i: integer;
begin
   for i := o_list.Low to o_list.High do
   	Add (o_list[i]);
end;

function TComboItems.ProcessCommand (dwEvent: word): dword;
begin
	result := 0;
   case dwEvent of
      CBN_SELCHANGE:
         if assigned (FOnSelChange) then
            FOnSelChange (self);
   end;
end;

//-----------------------------

function TxlComboBox.DoCreateControl (HParent: HWND): HWND;
var i_style: dword;
begin
   i_Style := CBS_AUTOHSCROLL or WS_VSCROLL;
   if FAllowEdit then
   	i_Style := i_style or CBS_DROPDOWN
   else
   	i_Style := i_style or CBS_DROPDOWNLIST;
   result := CreateWin32Control (HParent, 'combobox', i_style);
end;

procedure TxlComboBox.OnCreateControl ();
begin
	inherited;
   FItems := TComboItems.Create (self);
end;

procedure TxlComboBox.OnDestroyControl ();
begin
	FItems.Free;
   inherited;
end;

procedure TxlComboBox.f_SetAllowEdit (value: boolean);
begin
	FAllowEdit := value;
   ReCreate;
end;

procedure TxlComboBox.SetText (const s_text: widestring);
begin
	if AllowEdit then
   	inherited
   else
   	Perform (CB_SELECTSTRING, dword(-1), lparam(pwidechar(s_text)));
end;

function TxlComboBox.ProcessCommand (dwEvent: word): dword;
begin
	result := FItems.ProcessCommand (dwEvent);
end;

end.
