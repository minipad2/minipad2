unit UDeleteHandler;

interface

uses UPageSuper, UxlClasses;

type
	TDeleteHandler = class
   private
      FRecycleBin: TPageSuper;
   public
      procedure SetRecycleBin (value: TPageSuper);

      procedure Remove (value, parent: TPageSuper);
      function CanRemove (value, parent: TPageSuper; var b_delete: boolean): boolean;   // b_delete 用于决定菜单条目是“删除”还是“移除”
   end;

function DeleteHandler(): TDeleteHandler;

implementation

uses UGlobalObj, UOptionManager, UPageStore, UPageFactory, UTypeDef;

var FDeleteHandler: TDeleteHandler;

function DeleteHandler(): TDeleteHandler;
begin
	if FDeleteHandler = nil then
   	FDeleteHandler := TDeleteHandler.Create;
   result := FDeleteHandler;
end;

procedure TDeleteHandler.SetRecycleBin (value: TPageSuper);
begin
	FRecycleBin := value;
end;

procedure TDeleteHandler.Remove (value, parent: TPageSuper);
var b_delete: boolean;
	i, n, id: integer;
begin
	if not CanRemove (value, parent, b_delete) then exit;

   if parent.Childs <> nil then
   	parent.Childs.RemoveChild (value.id)
   else
   	PageCenter.EventNotify (pctRemoveChild, parent.id, value.id);
   if b_delete then
   begin
      if (FRecycleBin = nil) or (value.owner = FRecycleBin) or (not FRecycleBin.CanAddChild (value.pagetype)) then
      	value.Delete
      else
      begin
      	if parent <> value.owner then   // 针对searchpage等无真正意义上子条目的页面
         	value.owner.childs.RemoveChild (value.id);
         PageStore.GetFirstPageId (ptFavorite, id);
         PageStore[id].Childs.RemoveChild (value.id);
         value.Owner := FRecycleBin;
         FRecycleBin.Childs.AddChild (value.id);

         n := FRecycleBin.Childs.Count;
         if (OptionMan.Options.RecyclerLimit >= 0) and (n > OptionMan.Options.RecyclerLimit) then
         	for i := n - OptionMan.Options.RecyclerLimit -1 downto 0 do
            begin
            	id := FRecycleBin.Childs.ChildId (i);
            	FRecycleBin.Childs.RemoveChild (id);
            	PageStore[id].delete;
            end;
      end;
   end;
end;

function TDeleteHandler.CanRemove (value, parent: TPageSuper; var b_delete: boolean): boolean;
begin
   b_delete := false;
	if parent.isvirtualcontainer then   // 收藏夹中的虚拟链接任何情况下都可删除
   	result := true
   else
   begin
      result := false;
      if value = nil then exit;
      if not value.CanDelete then exit;

      result := true;
      b_delete := not value.SingleInstance;
   end;
end;

//------------

initialization

finalization
	FDeleteHandler.free;

end.

//   	FRemoveOnly: array of TPageSuper;
//      FNoRemovables: array of TPageSuper;

//      procedure AddRemoveOnly (value: TPageSuper);     // 只移除不删除
//      procedure AddNoRemovable (value: TPageSuper);   // 不能移除

//procedure TDeleteHandler.AddRemoveOnly (value: TPageSuper);
//var n: integer;
//begin
//   n := Length (FRemoveOnly);
//   SetLength (FRemoveOnly, n + 1);
//   FRemoveOnly[n] := value;
//end;
//
//procedure TDeleteHandler.AddNoRemovable (value: TPageSuper);
//var n: integer;
//begin
//   n := Length (FNoRemovables);
//   SetLength (FNoRemovables, n + 1);
//   FNoRemovables[n] := value;
//end;


