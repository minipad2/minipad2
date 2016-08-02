unit UTypeDef;

interface

uses Windows, Messages, UxlClasses, UxlDragControl;

// UExtFuncs
type
   TPageBehavior = (pbRename, pbSwitchProperty, pbDelete);
   TGroupBehavior = (lbNewPage, lbLevelUp);
   TImportType = (itFromFile, itFromFolder);
   TExportType = (etToFile, etToFolder, etToClipboard);
   TBackupIntervalType = (btSaveCount, btMinute, btHour, btDay);

 	TLinkType = (ltFile, ltFolder, ltPage, ltEmail, ltNode, ltOthers, ltBatch);
   TLinkOptions = record
      DisableIconRead: boolean;
      AutoMinimizeAfterOpenLink: boolean;
   end;

   TClipFilterType = (cftNoFilter, cftFilterNeighboring, cftFilterAll);
   TClipOptions = record
      MaxClipNum: integer;
      MaxItemByte: integer;
      MenuWidth: integer;
      FilterType: TClipFilterType;
      NewPasteToTop: boolean;
   end;

// USpecialMode
type
   TSMDirection = (hdFree, hdLeft, hdTop, hdRight, hdBottom);
   
   TSMOptions = record
   	Direction: TSMDirection;
      HideDelay: cardinal;
      ShowDelay: cardinal;
      AnimationTime: cardinal;
      EdgeWidth: cardinal;
   end;

// UListObjects

// UPageSuper
type
	TPageType = (ptNone = -1, ptGroup, ptNote, ptCalc, ptMemo, ptDict, ptLink, ptContact, ptTemplate, ptFastLink, ptSheet, ptFavorite,
      ptSearch, ptTag, ptRecycleBin, ptRecentRoot, ptMemoItem, ptLinkItem, ptContactItem, ptTemplateItem, ptRecentCreate, ptRecentModify,
      ptRecentVisit, ptGroupRoot, ptRoot, ptTagRoot, ptClip, ptClipItem, ptExternalNote);
	TPageStatus = (psNormal, psLocked, psProtected, psReadOnly);
   TPageControl = (pcNone, pcEdit, pcListView, pcBlog);

   TPageEvent =
   (
   	pctReNameDemand,   // id: target page
   	pctFieldModified,   //
      pctSwitchStatus,  //
      pctListProperty,
      pctIcon,   //
    	pctColor,   //
//      pctChecked,  //
   	pctControlChange,
      pctSetActive,  //
      pctSave,  //
      pctAddChild,  // pid, id
      pctRemoveChild,   // pid, id
      pctClear,  // id
      pctSelect,  // id
      pctDeSelect  // id
   );

   TPageCommand = (pcSetActive, pcSelectItem, pcDeSelectItem);
   
   TDropPageEvent = procedure (o_dragsource: IDragSource; targetid, pid: integer; b_copy: boolean) of object;

implementation

end.

