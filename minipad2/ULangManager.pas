unit ULangManager;

interface

uses UxlExtClasses, UxlList, UxlDialog;

type
   TLangManager = class (TxlLanguage, IOptionObserver)
   private
   protected
      procedure FillItems (var lang: widestring; itemlist: TxlStrList); override;
   public
   	procedure OptionChanged ();
   end;

function LangMan(): TLangManager;
procedure RefreshItemText (DlgParent: TxlDialogSuper; arritems: array of word; captionid: word = 0);

implementation

uses Windows, UOptionManager, UGlobalObj, UxlStrUtils, UxlFunctions, UTypeDef, Resource;

var FLangMan: TLangManager;

function LangMan (): TLangManager;
begin
	if not assigned (FLangMan) then
   begin
      FLangMan := TLangManager.Create;
   	FLangMan.SetInnerLanguage('Chinese');
   	OptionMan.AddObserver(FLangMan);
   end;
   result := FLangMan;
end;

procedure RefreshItemText (DlgParent: TxlDialogSuper; arritems: array of word; captionid: word = 0);
var i: integer;
begin
	with DlgParent do
   begin
   	if captionid > 0 then
      	Text := LangMan.GetItem (captionid, Text);
      for i := Low(arritems) to High(arritems) do
         ItemText[arritems[i]] := LangMan.GetItem (arritems[i], ItemText[arritems[i]]);
   end;
end;

//-------------------

procedure TLangManager.FillItems (var lang: widestring; itemlist: TxlStrList);
   procedure f_loadchinese (itemlist: TxlStrList);
   begin
   	with itemlist do
      begin
	      AddByIndex (mc_workspace, '文件(&F)');
            AddByIndex (m_newnote, '新建笔记页(&N)');
//            AddByIndex (m_newsiblingnote, '新建笔记页(&N)');
            AddByIndex (m_newpage, '新建功能页(&T)');
               AddByIndex (m_newcalc, '新建计算页(&C)');
               AddByIndex (m_newmemo, '新建备忘页(&M)');
               AddByIndex (m_newdict, '新建词典页(&D)');
               AddByIndex (m_newlink, '新建链接页(&L)');
               AddByIndex (m_newcontact, '新建通讯页(&T)');
            AddByIndex (m_newgroup, '新建组(&G)');

            AddByIndex (m_rename, '重命名(&A)');
            AddByIndex (m_switchlock, '加锁切换(&K)');
            AddByIndex (m_save, '保存(&S)');
            AddByIndex (m_deletepage, '删除(&D)');
            AddByIndex (m_closepage, '关闭(&C)');

            AddByIndex (m_property, '属性(&P)...');
            AddByIndex (m_view, '视图(&V)');

            AddByIndex (m_import, '导入(&I)...');
            AddByIndex (m_export, '导出(&E)...');
            AddByIndex (m_sendmail, '发送邮件(&M)...');

            AddByIndex (m_exit, '退出(&X)');

         AddByIndex (mc_edit, '编辑(&E)');
            AddByIndex (m_clear, '清空(&L)');
            AddByIndex (m_undo, '撤销(&U)');
            AddByIndex (m_redo, '重做(&R)');
            AddByIndex (m_selectall, '全选(&A)');

            AddByIndex (m_cut, '剪切(&T)');
            AddByIndex (m_copy, '复制(&C)');
            AddByIndex (m_paste, '粘贴(&P)');
            AddByIndex (m_delete, '删除(&D)');

            AddByIndex (m_newitem, '新建项目(&N)');
            AddByIndex (m_insertitem, '插入项目(&I)');
            AddByIndex (m_edititem, '编辑项目(&E)');
            AddByIndex (m_deleteitem, '删除项目(&D)');
            AddByIndex (m_removeitem, '移除项目(&R)');

            AddByINdex (m_wordwrap, '自动换行(&W)');
            AddByIndex (m_texttools, '文字工具(&T)');
               AddByIndex (m_highlight1, '高亮 1');
               AddByIndex (m_highlight2, '高亮 2');
               AddByIndex (m_highlight3, '高亮 3');
               AddByIndex (m_highlight4, '高亮 4');
               AddByIndex (m_removehighlight, '移除高亮效果');
               AddByIndex (m_ul1, '无序列表 1');
               AddByIndex (m_ul2, '无序列表 2');
               AddByIndex (m_ol, '有序列表');
               AddByIndex (m_removelist, '移除列表');
               AddByIndex (m_noemptyline, '删除空行');
               AddByIndex (m_oneemptyline, '只空一行');
               
            AddByIndex (m_find, '查找／替换(&F)...');
            AddByIndex (m_subsequentfind, '后续查找');
               AddByIndex (m_findnext, '查找下一个');
               AddByIndex (m_findprevious, '查找上一个');
               AddByIndex (m_replace, '替换并查找下一个');
               AddByIndex (m_replace_p, '替换并查找上一个');
               AddByIndex (m_replaceall, '全部替换');
            AddByINdex (m_highlightmatch, '高亮查找结果');
            
            AddByINdex (m_insertlink, '插入链接(&L)...');
            AddByIndex (m_inserttemplate, '插入模板文字(&M)');
            AddByIndex (m_insertcliptext, '插入剪贴文字(&X)');

         AddByIndex (mc_navigation, '导航(&N)');
            AddByIndex (m_prior, '后退(&P)');
            AddByIndex (m_next, '前进(&N)');
            AddByIndex (m_levelup, '上一层(&U)');

            AddByINdex (m_recentroot, '最近的笔记(&R)');
            	AddByIndex (m_recentcreate, '最近创建(&C)');
               AddByIndex (m_recentmodify, '最近修改(&M)');
               AddByIndex (m_recentvisit, '最近访问(&V)');
            	AddByINdex (m_managerecent, '管理最近的笔记(&M)');
         	AddByIndex (m_favorite, '收藏夹(&F)');
            	AddByIndex (m_addfavorite, '添加至收藏夹(&A)');
               AddByIndex (m_removefavorite, '从收藏夹移除(&R)');
               AddByINdex (m_managefavorite, '收藏夹管理(&M)');
            AddByIndex (m_search, '搜索(&S)...');
            AddByINdex (m_tag, '分类检索(&T)');
            AddByINdex (m_recyclebin, '回收站管理(&R)');

         AddByIndex (mc_tools, '工具(&T)');
            AddByIndex (m_ShowTree, '显示目录树(&T)');
            AddByIndex (m_stayontop, '总在最前(&S)');
            AddByIndex (m_transparent, '透明效果(&R)');
            AddByIndex (m_specialmode, '自动隐藏(&H)');

            AddByIndex (m_template, '文字模板管理(&P)');
            AddByIndex (m_fastlink, '快速链接管理(&L)');

            AddByIndex (m_watchclipboard, '剪贴板监视(&W)');
            AddByIndex (m_clearclipboard, '清空剪贴条目(&C)');
            AddByIndex (m_clipboard, '剪贴板管理(&M)');

            AddByIndex (m_statistics, '统计信息(&A)...');
            AddByIndex (m_definetoolbar, '自定义工具栏(&D)...');
            AddByIndex (m_options, '选项(&O)...');

         AddByIndex (mc_help, '帮助(&H)');
            AddByIndex (m_helptopic, '帮助主题(&H)');
            AddByIndex (m_homepage, '主页(&P)');
            AddByIndex (m_forum, '论坛(&F)');
            AddByIndex (m_donate, '捐助(&D)');
            AddByIndex (m_about, '关于(&A)...');

         AddByIndex (ob_program, '程序设置');
            AddByIndex (ob_edit, '编辑设置');
            AddByIndex (ob_notes, '笔记摘录');
            AddByIndex (ob_import_export, '导入导出');
            AddByIndex (ob_behavior, '操作习惯');
            AddByIndex (ob_login, '登录设置');
            AddByIndex (ob_backup, '保存与备份');
            AddByIndex (ob_specialmode, '自动隐藏');

         AddByIndex (ob_appearance, '界面设置');
            AddByIndex (ob_treeview, '树窗口');
            AddByIndex (ob_tabcontrol, '标签栏');
            AddByIndex (ob_listview, '列表窗口');
            AddByIndex (ob_editor, '编辑器');
            AddByIndex (ob_othercontrols, '其它');

         AddByIndex (ob_extfuncs, '扩展功能');
            AddByIndex (ob_calcpage, '计算页');
            AddByIndex (ob_dictpage, '词典页');
            AddByIndex (ob_linkpage, '链接页');
            AddByIndex (ob_clipboard, '多重剪贴板');
            AddByINdex (ob_template, '文字模板');

         AddByIndex (m_openfastlink, '快速链接');
         AddByIndex (m_restore, '还原');
         AddByIndex (m_minimize, '最小化');
         AddByIndex (m_newnoteforeground, '新建笔记');
         AddByIndex (m_autorecord, '自动记录');
         
         AddByIndex (sr_filefilter, '所有文件（*.*）|*.*|可执行文件(*.exe)|*.exe|DOC 文件(*.doc)|*.doc|XLS 文件(*.xls)|*.xls|PPT 文件(*.ppt)|*.ppt|PDF 文件(*.pdf)|*.pdf|文本文件(*.txt)|*.txt');
         AddByIndex (sr_selectfile, '选择文件');
         AddByIndex (sr_selectfolder, '选择文件夹');
         AddByINdex (sr_selectbackupfolder, '选择备份文件夹');
         AddByIndex (sr_ExeFilter, '可执行文件(*.exe)|*.exe');
         AddByINdex (sr_selectsoundfile, '选择声音文件');
         AddByIndex (sr_soundfilefilter, '声音文件(*.wav)|*.wav');

         AddByIndex (sr_free, '自由');
         AddByIndex (sr_left, '左');
         AddByINdex (sr_top, '上');
         AddByIndex (sr_right, '右');
         AddByIndex (sr_bottom, '下');

         AddByIndex (sr_NewReminder, '新建备忘条目');
         AddByIndex (sr_EditReminder, '编辑备忘条目');
         AddByINdex (sr_NewLink, '新建链接');
         AddByIndex (sr_EditLink, '编辑链接');
         AddByIndex (sr_NewTemplate, '新建文字模板');
         AddByIndex (sr_EditTemplate, '编辑文字模板');
         AddByINdex (sr_EditContact, '编辑联系人');
         AddByINdex (sr_NewContact, '新建联系人');

         AddByINdex (sr_MemoAction, '定时提醒');
         AddByIndex (sr_MemoAction + 1, '执行任务');
         AddByIndex (sr_MemoAction + 2, '无操作');

         AddByINdex (sr_Time, '时间');
         AddByIndex (sr_Action, '操作');
         AddByIndex (sr_Description, '描述');
         AddByIndex (sr_UseSound, '提示音');
         AddByIndex (sr_SoundFile, '声音文件');

         AddByIndex (sr_today, '当天');
         AddByIndex (sr_daily, '每天');
         AddByIndex (sr_weekly, '每周');
         AddByIndex (sr_monthly, '每月');
         AddByIndex (sr_yearly, '每年');
         AddByIndex (sr_timespan, '日程');
         AddByIndex (sr_notime, '无时间');

         AddByIndex (sr_LinkTypes, '程序／文件');
         AddByIndex (sr_LinkTypes + 1, '文件夹');
         AddByIndex (sr_LinkTypes + 2, '网址');
         AddByIndex (sr_LinkTypes + 3, '收件地址');
         AddByIndex (sr_LinkTypes + 4, '节点');
         AddByIndex (sr_LinkTypes + 5, '其他');
         AddByIndex (sr_LinkTypes + 6, '批处理');

         AddByIndex (sr_LinkType, '类别');
         AddByIndex (sr_Link, '链接');
         AddByIndex (sr_Hotkey, '热键');
         AddByIndex (sr_Abbrev, '缩写');

         AddByIndex (sr_boy, '男');
         AddByIndex (sr_girl, '女');

         AddByIndex (sr_Name, '姓名');
         AddByIndex (sr_Sex, '性别');
         AddByIndex (sr_Mobile, '手机');
         AddByIndex (sr_Email, 'Email');
         AddByIndex (sr_IM1, 'QQ');
         AddByIndex (sr_IM2, 'MSN');
         AddByIndex (sr_Company, '单位');
         AddByIndex (sr_Department, '部门／职务');
         AddByIndex (sr_Address, '地址');
         AddByIndex (sr_Zipcode, '邮编');
         AddByIndex (sr_Tel, '电话');
         AddByIndex (sr_Fax, '传真');
         AddByIndex (sr_Others, '其他');

         AddByIndex (sr_NewClipItem, '新建剪贴条目');
         AddByIndex (sr_EditClipItem, '编辑剪贴条目');

         AddByIndex (IDOK, '确    定');
         AddByIndex (IDCancel, '取    消');
         AddByIndex (sr_prompt, '注意!');
         AddByINdex (sr_info, '提示!');

         AddByIndex (sr_optionalitems, '可选项目');
         AddByIndex (sr_SelectImportFile, '选择导入文件');
         AddByIndex (sr_ImportFilter, '文本文件（*.txt）|*.txt|minipad2 导出文件(*.mep)|*.mep|所有文件（*.*）|*.*');
         AddByIndex (sr_NameExportFile, '设定导出文件名');
         AddByIndex (sr_ExportFilter1, '文本文件（*.txt）|*.txt|所有文件（*.*）|*.*');
         AddByIndex (sr_ExportFilter2, '文本文件（*.txt）|*.txt|minipad2 导出文件(*.mep)|*.mep|所有文件（*.*）|*.*');
         AddByIndex (sr_ExportFilter3, 'minipad2 导出文件(*.mep)|*.mep|文本文件（*.txt）|*.txt|所有文件（*.*）|*.*');
         AddByIndex (sr_ListCopied, '列表内容已复制到剪贴板！');
         AddByIndex (sr_BrowseMailClient, '选择邮件客户端可执行文件');
         AddByIndex (sr_TemplateCaptured, '所选文字已被添加到 minipad2 的文字模板列表。');
         AddByIndex (sr_TemplateExists, '模板列表中相同内容的条目（第 %0 项）已存在！');
         AddByIndex (sr_TableOfContents, '目录');
         AddByIndex (sr_codetransfer, '词典字符编码转换中, 请等待...');
         AddByIndex (sr_SelectIcon, '选择图标文件');
         AddByIndex (sr_IconFilter, '图标文件（*.ico）|*.ico|所有文件（*.*）|*.*');

         AddByIndex (sr_RemoveFastLinkItemsPrompt, '是否将所选项目从快速链接列表中移除?');
         AddByIndex (sr_DeleteItemsPrompt, '是否删除所选项目?');
         AddByIndex (sr_RemoveItemsPrompt, '是否移除所选项目?');
         AddByIndex (sr_DeletePagesPrompt, '是否删除所选标签页?');
         AddByIndex (sr_deletegroupprompt, '当前组包含子节点。真的要全部删除吗？');
         AddByINdex (sr_clearprompt, '真的要清空当前标签页的内容吗？');
         AddByINdex (sr_deleteprompt, '真的要删除当前页吗？');
         AddByIndex (sr_clearclipboardprompt, '是否清空所有剪贴板条目？');
         AddByIndex (sr_UnsupportedOperation, '操作不支持！');
         AddByIndex (sr_NewNoteCreated, '已创建新笔记，标题为：');
         AddByIndex (sr_SnapTextSuccess, '已抓取如下文字：');
         AddByIndex (sr_newnotebgandsnaptextsuccess, '已创建新笔记并抓取如下文字：');
         AddByIndex (sr_DataSaved, '数据已保存.');
         AddByIndex (sr_SaveSnapText, '保存捕捉文字');
         AddByIndex (sr_SnapTextSavedToFile, '以下文字已保存到文件');
			AddByIndex (sr_ExportedToFolder, '当前节点（及其子节点）已导出到文件夹：');
         AddByIndex (sr_PageExportedToFile, '当前页内容已导出到文件：');
         AddByIndex (sr_PageExportedToClipboard, '当前页内容已导出到剪贴板');
         AddByIndex (sr_GroupExportedToFile, '当前节点（及其子节点）内容已导出到文件：');
         AddByINdex (sr_mepversionnotmatch, 'mep 文件版本不匹配！');
         AddByINdex (sr_invalidnodelink, '节点链接无效！');
         AddByIndex (sr_importingPrompt, '文件导入中，按住 ESC 可终止...');
         AddByIndex (sr_userabortimport, '用户终止导入');
         AddByIndex (sr_exportingprompt, '页面导出中，按住 ESC 可终止...');
         AddByIndex (sr_userabortexport, '用户终止导出');
         AddByIndex (sr_deletingprompt, '项目删除中，按住 ESC 可终止...');
         AddByIndex (sr_userabortdelete, '用户终止删除');

         AddByIndex (sr_TemplatePage, '文字模板');
         AddByIndex (sr_FastLink, '快速链接');
         AddByIndex (sr_Clipboard, '剪贴文字');

         AddByIndex (sr_RecentRoot, '最近的笔记');
            AddByIndex (sr_RecentCreate, '最近创建');
            AddByIndex (sr_RecentModify, '最近修改');
            AddByIndex (sr_RecentVisit, '最近访问');
         AddByINdex (sr_FavoritePage, '收藏夹');
         AddByIndex (sr_SearchPage, '搜索');
         AddByIndex (sr_TagRoot, '分类');
         AddByIndex (sr_GroupRoot, '全部');
         AddByIndex (sr_RecycleBin, '回收站');

         AddByINdex (sr_Normal, '常规');
         AddByIndex (sr_Locked, '加锁');
         AddByIndex (sr_Protected, '追加');
         AddByIndex (sr_ReadOnly, '只读');

         AddByIndex (sr_saves, '次保存');
         AddByIndex (sr_minutes, '分钟');
         AddByIndex (sr_hours, '小时');
         AddByIndex (sr_days, '天');
         
         AddByIndex (rb_icon, '大图标');
         AddByIndex (rb_smallicon, '小图标');
         AddByIndex (rb_list, '列表');
         AddByINdex (rb_report, '详细信息');
         AddByIndex (rb_blog, '摘要');

         AddByIndex (Property_General, '常规');
         AddByINdex (Property_Edit, '保存');
         AddByIndex (Property_List, '列表');

         AddByIndex (sr_Title, '标题');
         AddByIndex (sr_CreateTime, '创建时间');
         AddByIndex (sr_ModifyTime, '修改时间');
         AddByIndex (sr_VisitTime, '访问时间');
         AddByIndex (sr_ExportFile, '文件');
         AddByIndex (sr_ExternalSave, '外部存储');
         AddByIndex (sr_Abstract, '内容');
         AddByIndex (sr_SearchResult, '检索摘要');
         AddByIndex (sr_Remark, '备注');
         AddByIndex (sr_Text, '内容');
         AddByIndex (sr_NodePath, '节点路径');
         
         AddByINdex (sr_GroupPage, '组');
         AddByINdex (sr_GroupPage + Ord(ptNote), '笔记页');
         AddByIndex (sr_GroupPage + Ord(ptCalc), '计算页');
         AddByIndex (sr_GroupPage + Ord(ptMemo), '备忘页');
         AddByIndex (sr_GroupPage + Ord(ptDict), '词典页');
         AddByIndex (sr_GroupPage + Ord(ptLink), '链接页');
         AddByIndex (sr_GroupPage + Ord(ptContact), '通讯页');
         AddByIndex (sr_GroupPage + Ord(ptMemoItem), '备忘项');
         AddByIndex (sr_GroupPage + Ord(ptLinkItem), '链接项');
         AddByIndex (sr_GroupPage + Ord(ptContactItem), '联系人');
         AddByIndex (sr_GroupPage + Ord(ptTemplateItem), '文字模板');
         AddByIndex (sr_GroupPage + Ord(ptClipItem), '剪贴项目');

         AddByIndex (sr_Include, '包含');
         AddByINdex (sr_NotInclude, '不包含');

         AddByIndex (tp_CurrentPage, '当前页');
         AddByIndex (tp_Database, '数据库');
      end;
   end;
var s_file: widestring;
begin
	itemlist.clear;
   s_file := LangDir + lang + '.lng';
	if IsSameStr (lang, 'Chinese') then
   	f_loadchinese (itemlist)
   else if pathfileexists (s_file) then
   begin
   	itemlist.IndexDeli := '=';
      itemlist.loadfromfile (s_file);
   end;
end;

procedure TLangManager.OptionChanged ();
begin
   SetLanguage (OptionMan.options.Language);
end;

initialization

finalization
	FreeAndNil (FLangMan);

end.






