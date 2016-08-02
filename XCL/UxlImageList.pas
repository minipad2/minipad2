unit UxlImageList;

interface

uses windows, commctrl, UxlClasses, UxlList;

type TxlImageList = class
private
	Fhandle: HImageList;
   FIconList: TxlIntList;
   FOverlayNum: integer;
   function GetIconByIndex (index: integer): integer;
public
	constructor Create (width: integer = 0; height: integer = 0; imgnum: integer = 0);
   destructor Destroy (); override;
   procedure Clear ();
   function Count (): integer;

   function AddIcon (id_icon: integer): integer;   // returns index
   function AddIconGrayed (id_icon: integer; id_target: integer = 0): integer;
	function CreateOverlayedIcon (i_icon, i_overlay: integer; id_target: integer = 0): integer;
   procedure AddIcons (IconArray: array of cardinal);
   function AddHIcon (h_icon: HIcon): integer;
   function FindIcon (id_icon: integer): integer;
   function DeleteIcon (id_icon: integer): boolean;
   function AddOverlay (id_icon: integer): integer;

   property handle: HImageList read Fhandle;
   property Icons[index: integer]: integer read GetIconByIndex; // return resource id
end;

implementation

uses UxlWindow, UxlFunctions;

function GetGrayedIcon (h_icon: HIcon): HIcon; forward;

constructor TxlImageList.Create (width: integer = 0; height: integer = 0; imgnum: integer = 0);
var i_style: DWord;
begin
	if width <= 0 then
   	width := GetSystemMetrics (SM_CXSMICON);
	if height <= 0 then
   	height := GetSystemMetrics (SM_CXSMICON);
	InitCommonControls ();
   i_style := ILC_COLOR32 or ILC_MASK;    // 必须加上 ILC_MASK，否则透明背景显示黑色
   if imgnum <= 0 then
   	Fhandle := ImageList_Create (width, height, i_style, 0, 200)
   else
   	Fhandle := ImageList_Create (width, height, i_style, imgnum, 0);
   FIconList := TxlIntList.create;
   FOverlayNum := 0;
end;

destructor TxlImageList.Destroy ();
begin
	Clear;
	FIconList.free;
   ImageList_Destroy (Fhandle);
   inherited;
end;

procedure TxlImageList.Clear ();
var i: integer;
begin
	ImageList_RemoveAll (Fhandle);
   for i := FIconList.Low to FIconList.High do
   	if FIconList[i] > 0 then
      	DestroyIcon (FIconList[i]);
   FIconList.clear;
   FOverlaynum := 0;
end;

function TxlImageList.Count (): integer;
begin
	result := ImageList_GetImageCount (Fhandle);
end;

//----------------------

function TxlImageList.AddIcon(id_icon: integer): integer;
var h_Icon: HIcon;
begin
//	result := FIconList.FindByIndex(id_icon);
//   if result >= 0 then exit;
   h_icon := LoadIconFromResource (id_icon);
	if h_icon = 0 then
   	result := -1
   else
   begin
      result := AddHIcon (h_icon);
      FIconList.addByIndex (id_icon, 0);
   end;
end;

function TxlImageList.AddIconGrayed (id_icon: integer; id_target: integer = 0): integer;
var h_Icon: HIcon;
begin
	if id_target = 0 then
   	id_target := FIconList.GetNewIndex;
//	result := FIconList.FindByIndex(id_target);
//   if result >= 0 then exit;
   h_icon := LoadIconFromResource (id_icon);
	if h_icon = 0 then
   	result := -1
   else
   begin
      h_icon := GetGrayedIcon (h_icon);
      result := AddHIcon (h_icon);
      FIconList.AddByIndex(id_target, h_icon);
   end;
end;

function TxlImageList.CreateOverlayedIcon (i_icon, i_overlay: integer; id_target: integer = 0): integer;
var h_icon: HIcon;
begin
	if id_target = 0 then
   	id_target := FIconList.GetNewIndex;
//	result := FIconList.FindByIndex(id_target);
//   if result >= 0 then exit;
	h_icon := ImageList_GetIcon (Fhandle, i_icon, INDEXTOOVERLAYMASK (i_overlay) or ILD_TRANSPARENT);
	result := AddHIcon (h_icon);
   FIconList.AddByIndex (id_target, h_icon);
end;

procedure TxlImageList.AddIcons (IconArray: array of cardinal);
var i: integer;
begin
	for i := Low(IconArray) to High(IconArray) do
   	AddIcon (IconArray[i]);
end;

function TxlImageList.AddHIcon (h_icon: HIcon): integer;
begin
   ImageList_AddIcon (Fhandle, h_icon);
   result := Count() - 1;
end;

function TxlImageList.AddOverlay (id_icon: integer): integer;
var i: integer;
begin
	i := AddIcon(id_icon);
   if i >= 0 then
   begin
      inc (Foverlaynum);
      ImageList_SetOverlayImage (Fhandle, i, Foverlaynum);
      result := Foverlaynum;
   end;
end;

function TxlImageList.FindIcon (id_icon: integer): integer;
begin
	result := FIconList.FindByIndex (id_icon);
end;

function TxlImageList.DeleteIcon (id_icon: integer): boolean;
var i_pos: integer;
begin
	i_pos := FindIcon(id_icon);
   if i_pos < 0 then exit;
	result := ImageList_Remove (Fhandle, i_pos);
   if result then
   begin
   	if FIconList[i_pos] > 0 then DestroyIcon (FIconList[i_pos]);
      FIconList.delete (i_pos);
   end;
end;

function TxlImageList.GetIconByIndex (index: integer): integer;
begin
	result := FIconList.Indexes[index];
end;

//-------------

function GetGrayedIcon (h_icon: HIcon): HIcon;
var iconinfo: TIconInfo;
   h_dc, hdcmem: HDC;
   i, j: integer;
   h_bitmap: HBitMap;
   cl: TColor;
   lt: byte;
   d: double;
   h_handle: HWND;
begin
	h_handle := MainWinHandle;
   GetIconInfo (h_icon, iconinfo);
   h_bitmap := iconinfo.hbmColor;
   h_dc := GetDC (h_handle);
   hdcmem := CreateCompatibleDC (h_dc);
   SelectObject (hdcmem, h_bitmap);
   for i := 0 to 31 do     // 不能是 i_width - 1 !
      for j := 0 to 31 do
      begin
         cl := GetPixel (hdcmem, i, j);
         d := GetRValue(cl) * 0.299 + GetGValue(cl) * 0.587 + GetBValue(cl) * 0.114;
         lt := Round(d);
         SetPixel (hdcmem, i, j, rgb(lt,lt,lt));
      end;
   DeleteDC (hdcmem);
   ReleaseDC (h_handle, h_dc);

   result := CreateIconIndirect (iconinfo);
end;

var ComCtl32Dll: HMODULE;

initialization
	Comctl32Dll := LoadLibraryW ('comctl32.dll');

finalization
   FreeLibrary (Comctl32dll);

end.

