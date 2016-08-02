unit Resource;

interface

const
	MainIcon = 1;
	MainMenu = 98;
	m_tray = 99;
	MainAccelTable = 100;
   
	hk_switchfocus = 354;
	hk_selastitle = 355;
   hk_treeleft = 356;
   hk_treeright = 357;

   cb_previous = 400;
   cb_next = 401;
	cb_add	=	402;
	cb_remove	=	403;
	cb_up	=	404;
	cb_down	=	405;
   cb_new = 406;
	cb_browse	=	407;
	cb_reset	=	408;
	cb_exit	=	409;
	cb_icon	=	412;
	cb_apply	=	413;

	ic_reminder = 550;
	ic_boy = 551;
	ic_girl = 552;
	ic_fontcolor = 553;
	ic_behavior = 554;
	ic_pagelink = 555;
	ic_emaillink = 556;
	ic_batchlink = 557;
	ic_batchlink_d = 558;
	ic_filelink = 559;
	ic_folderlink = 561;
	ic_noaction = 587;
	ic_backup = 563;
	ic_listview = 564;
	ic_edit = 565;
	ic_tabcontrol = 566;
	ic_editor = 567;
	ic_login = 568;
	ic_autorecord = 569;

	ic_lockedmask = 570;
	ic_protectedmask = 571;
	ic_readonlymask = 572;

	ic_externalnote = 573;
   ic_othercontrols = 574;
   ic_fixedclipitem = 575;
   
	mc_workspace = 1000;
		m_newnote = 1010;
		m_newsiblingnote = 1015;
		m_newpage = 1020;
		m_newcalc = 1030;
		m_newmemo = 1040;
		m_newdict = 1050;
		m_newlink = 1060;
		m_newcontact = 1070;
		m_newgroup = 1110;

		m_rename = 1120;
		m_switchlock = 1130;
		m_deletepage = 1140;
		m_closepage = 1145;
		m_save = 1150;

      m_moveup = 1220;
      m_movedown = 1230;
      
		m_property = 1160;
		m_view = 1170;

		m_import = 1180;
		m_export = 1190;
		m_sendmail = 1200;
		m_exit = 1210;

	mc_edit = 1300;
		m_clear = 1310;
		m_undo = 1320;
		m_redo = 1330;
		m_selectall = 1340;

		m_cut = 1350;
		m_copy = 1360;
		m_paste = 1370;
		m_delete = 1400;
		m_deleteitem = 1405;
		m_removeitem = 1406;
      
		m_newitem = 1410;
		m_insertitem = 1420;
		m_edititem = 1430;

		m_wordwrap = 1440;
		m_texttools = 1380;
			m_highlight1 = 1381;
			m_highlight2 = 1382;
			m_highlight3 = 1383;
			m_highlight4 = 1384;
			m_removehighlight = 1385;
			m_ul1 = 1386;
			m_ul2 = 1387;
			m_ol = 1388;
			m_removelist = 1389;
         m_noemptyline = 1390;
         m_oneemptyline = 1391;

	m_find = 1450;
   m_subsequentfind = 1460;
			m_findnext = 1462;
			m_findprevious = 1464;
			m_replace = 1466;
			m_replace_p = 1468;
			m_replaceall = 1470;
			m_highlightmatch = 1480;

	m_insertlink = 1490;

	mc_navigation = 1550;
		m_prior = 1560;
		m_next = 1570;
		m_levelup = 1580;
		m_recentroot = 1590;
		m_managerecent = 1600;
		m_addfavorite = 1610;
      m_removefavorite = 1615;
		m_managefavorite = 1620;
		m_search = 1630;
      m_tag = 1640;
		m_recyclebin = 1650;

	mc_tools = 1700;
		m_showtree = 1710;
		m_stayontop = 1720;
		m_transparent = 1730;
		m_specialmode = 1740;

		m_template = 1750;
		m_fastlink = 1760;

		m_watchclipboard = 1770;
		m_clearclipboard = 1780;
		m_clipboard = 1790;

		m_statistics = 1800;
		m_definetoolbar = 1810;
		m_options = 1820;

	mc_help = 1900;
		m_helptopic = 1910;
		m_homepage = 1920;
		m_forum = 1930;
		m_about = 1940;
		m_donate = 1950;

	m_restore = 1960;
	m_minimize = 1970;
	m_newnoteforeground = 1975;
   m_autorecord = 1980;

	m_popup = 1990;

	e_WinStatusChanged = 3050;
		p_maximized = 1;
		p_minimized = 2;
		p_restored = 3;

	e_ContextMenuDemand = 3051;
		ToolbarContext = 1;
		PageContext = 2;
		ListContext = 3;
		ListnoselContext = 4;
		EditorContext = 5;
      EditorNoSelContext = 6;
		ProgramContext = 7;
		SearchContext = 8;
		RecycleBinContext = 9;
		SysPageContext = 10;
		RecentPagesContext = 11;

	e_TrayIconClicked = 3060;
	e_ExecuteLink = 3061;
	e_LinkHotKeyChanged = 3067;
	e_TemplateHotKeyChanged = 3068;
	e_LinkItemDeleted = 3074;
	e_TemplateItemDeleted = 3075;
	e_ShowFromSPMode = 3077;
	e_AppendEditorText = 3078;
	e_GetEditorSelText = 3086;
	e_HighlightSearchResult = 3087;
   e_PageNameChanged = 3088;
   
	m_insertcliptext = 4000;
	m_inserttemplate = 5000;
	m_openfastlink = 6000;
	m_favorite = 6500;
	m_recentCreate = 7000;
	m_recentModify = 7500;
	m_recentVisit = 8000;



	About_Box = 10000;
		st_translator = 10010;
		st_freeware = 10020;
		st_copyrightinfo = 10030;
		st_copyright = 10040;
		st_forum = 10050;
		st_forumlink = 10060;
		st_homepagelink = 10070;
		st_homepage = 10080;
		st_email = 10090;
		st_emaillink = 10100;
		st_authorname = 10110;
		st_author = 10120;
		st_program = 10130;
		ProgramIcon = 10140;
		st_donate	=	10150;
		st_releasedate = 10170;
		st_pleasedonate = 10180;

	Find_Box = 10200;
		cmb_findtext	=	10210;
		cmb_replacetext	=	10220;
		chk_allowreplace	=	10230;
		st_targettext = 10240;
		sr_findprompt = 10250;
		sr_replaceallprompt = 10260;
		sr_upward = 10270;
		sr_downward = 10280;
		sr_allreplaced = 10290;
		sr_notargetfound = 10300;
		sr_noreplacefound = 10310;
		rb_up	=	10330;
		rb_down	=	10340;
		chk_matchcase	=	10350;
		chk_wholeword	=	10360;
		chk_rollback	=	10370;
		chk_withinselrange	= 10390;
		chk_exitafterfirstmatch	=	10400;
		chk_highlightmatch	=	10410;

	DefineToolbar_Box = 10450;
		lv_available =	10460;
		lv_current = 10470;
		sr_availablebuttons = 10480;
		sr_currentbuttons = 10490;
		sr_separator = 10500;
		sr_ResetToolbarPrompt = 10510;

	Property_Box = 10600;
		tab_switch = 10610;

	Property_General = 10650;
		st_status	=	10660;
		cmb_status	=	10670;
		st_createtime	=	10680;
		st_modifytime	=	10690;
		st_visittime	=	10760;
      st_path	=	10770;
      sle_createtime	=	10780;
      sle_modifytime	=	10790;
      sle_visittime	=	10800;
      sle_path	=	10810;

	Property_Edit = 10850;
		sle_exportfile	=	10860;
		chk_externalsave	=	10870;

	Property_List = 11000;
		lv_columns = 11010;
		cb_loaddefault = 11020;
		cb_saveasdefault = 11030;
		chk_selectall	=	11040;
		rb_Icon	=	11050;
		rb_SmallIcon = 11060;
		rb_list	=	11070;
		rb_report	=	11080;
		rb_blog = 11090;
		chk_gridlines	=	11100;
		st_liststyle = 11110;
		chk_fullrowselect = 11120;
		sr_optionalitems = 11130;
		cmb_style	=  11140;
		chk_checkboxes	=	11150;
		cb_applytoall = 11160;

	Statistics_Box = 11250;
		st_count_left	=	11260;
		st_count_right	=  11270;
		tp_CurrentPage = 11280;
		tp_Database = 11290;
		sr_CharCount = 11300;
		sr_WordCount = 11310;
		sr_ParaCount = 11320;
		sr_ItemCount = 11330;
      sr_ExCrCharCount = 11340;
      sr_ExBlCharCount = 11350;
      sr_LineCount = 11360;

	LogIn_Box = 11400;
		st_inputpassword = 11410;
		sle_password = 11420;
		sr_wrongpassword = 11430;

	Import_Box = 11500;
		rb_fromfile	=	11510;
		rb_fromfolder	=	11520;
		chk_virtualimport = 11530;

	Export_Box = 11600;
		rb_tofile = 11610;
		rb_tofolder = 11620;
	   rb_toclipboard	=	11625;
		st_encode = 11630;
		cmb_encode = 11640;

	Search_Box = 11700;
		cmb_pagetype	=	11710;
		st_pagetype	=	11720;
		st_searchcrit	=	11730;
		cmb_col	=	11740;
		cmb_opr	=	11750;
		sle_value	=	11760;

	Note_Box = 11850;
   Clip_Box = 11855;
		sr_newClipItem = 11860;
		sr_editClipitem = 11870;
	   chk_fixed	=	11880;
      mle_text = 11890;
      
	InsertLink_Box = 11950;
		cmb_linktype	=	11960;

	Option_Box = 15000;
		tv_options = 15010;
		st_placeholder = 15020;

	ob_program = 15050;
		chk_autostart	=	15060;
		chk_showtrayicon	=	15070;
		chk_minimizetotray = 15080;
		chk_startminimize = 15090;
		chk_closeminimize	=	15100;
		chk_autoname	=	15110;
		chk_confirmdelete	=	15120;
		chk_externalscroll = 15130;
		chk_AlwaysFocusEditor	=	15140;
		st_callwindowhotkey = 15150;
		hk_callwindow = 15160;
		st_mailclient	=	15170;
		sle_mailclient	=	15180;
		sle_recentnotescount	=	15185;
		st_recentnotescount	=	15190;
		ud_recentnotescount	=	15195;
		cmb_recyclerlimit	=	15055;
		st_recyclerlimit	=	15065;
	   chk_rememberpagescroll =	15066;

	ob_edit = 15200;
		chk_autoindent	=	15210;
		chk_confirmclear	=	15220;
		st_tabstops = 15230;
		cmb_tabstops	=	15240;
		st_undolimit = 15250;
		sle_undolimit	=	15260;
		chk_autoemptyline = 15270;
		chk_smoothscroll	=	15295;
		sle_ul1	=	15205;
		st_ul1	=	15215;
		st_ul2	=	15225;
		sle_ul2	=	15235;
		chk_oneclickopenlink	=	15245;

	ob_notes = 15300;
		hk_newnote_foreground	=	15305;
		hk_newnote_background	=	15310;
		hk_snaptext = 15315;
		st_newnote_foreground_hotkey	=	15320;
		st_newnote_background_hotkey = 15325;
		st_snaptext_hotkey	=	15330;
		st_snaptexttofile_hotkey	=	15345;
      st_newnotebg_snaptext_hotkey	=	15335;
      hk_newnotebg_snaptext	=	15340;
      hk_snaptexttofile	=	15346;
      st_closenotehotkey	=	15341;
      st_autorecordhotkey	=	15342;
      hk_closenote	=	15306;
      hk_autorecord	=	15307;

	ob_import_export = 15350;
		rb_import_default	=	15360;
		rb_import_showdialog	=	15370;
		st_importtype	=	15380;
		st_exportoption	=	15390;
		st_importoption	=	15400;
		rb_export_showdialog	=	15410;
		rb_export_default	=	15420;
		st_exporttype	=	15430;
		st_separateline	=	15440;
		sle_sepline	=	15450;
		chk_externalsaveWhenDragInFile	=	15460;

	ob_behavior = 15500;
		st_esckey	=	15530;
		rb_escminimize	=	15510;
		rb_escclear	=	15520;
		st_pagedblclick	=	15540;
		rb_switchproperty1	=	15550;
		rb_rename1	=	15560;
		rb_delete1	=	15570;
		rb_newpage	=	15580;
		rb_rename2	=	15590;
		rb_switchproperty2	=	15600;
		rb_delete2	=	15610;
		rb_levelup	=	15620;
		st_mbuttonclick	=	15630;
		st_groupdblclick	=	15640;
      
	ob_login = 15700;
		chk_needlogin	=	15710;
		sle_confirmpassword	=	15720;
		st_password	=	15730;
		st_confirmpassword	=	15740;
		cmb_locktraytime	=	15750;
		chk_locktrayicon	=	15760;
		chk_encryptdatafile	=	15770;

	ob_backup = 15800;
		chk_autosave	=	15810;
		cmb_autosavetime	=	15820;
		chk_autobackup	=	15830;
		sle_backuppath	=	15840;
		st_backuppath = 15850;
		sr_selectbackupfolder = 15860;
		cmb_backupintervaltype	=	15870;
		ud_backupinterval = 15880;
		st_totalbackup	=	15890;
		cmb_totalbackup	=	15805;
		sle_backupinterval	=	15815;
		sr_saves = 15825;
		sr_minutes = 15835;
		sr_hours = 15845;
		sr_days = 15855;

	ob_specialmode = 15900;
		cmb_direction	=	15910;
		cmb_hidedelay	=	15920;
		cmb_showdelay	=	15930;
		cmb_Animationtime	=	15940;
		cmb_edgewidth	=	15950;
		st_direction = 15960;
		st_hidedelay = 15970;
		st_showdelay = 15980;
		st_animationtime = 15990;
		st_edgewidth = 16000;
		sr_free = 16010;
		sr_left = 16020;
		sr_top = 16030;
		sr_right = 16040;
		sr_bottom = 16050;



	ob_appearance = 16500;
		chk_autoadjust = 16510;
		cmb_adjustlevel = 16520;
		chk_toolwindowstyle	=	16530;
		st_sensitivity = 16540;
		chk_showmenubar	=	16550;
		chk_showtoolbar	=	16560;
		cmb_transparency	=	16570;
		st_transparency = 16580;
		chk_captionpagename	=	16590;
		cmb_language	=	16600;
		st_language	=	16610;
      st_splitterwidth	=	16620;
      cmb_splitterwidth	=	16630;

	ob_treeview = 16700;
		chk_treehorzscroll	=	16710;
		chk_nodebuttons	=	16720;
		chk_nodelines	=	16730;
		chk_linesatroot	=	16740;
		chk_shownodeimages	=	16750;
		st_treefontdemo = 16760;
		cb_settreefont = 16770;
		cb_settreecolor = 16780;
		st_tree = 16790;
	   chk_treeonright =	16800;

	ob_tabcontrol = 16850;
		chk_multilinetab	=	16860;
		chk_fixedtabwidth	=	16870;
		cmb_tabwidth = 16880;
		chk_tabonbottom	=	16890;
		chk_showtabimages	=	16900;
		st_tabfontdemo = 16910;
		cb_settabfont = 16920;
		st_tabpage = 16930;
		chk_highlightcurrenttab	=	16940;

	ob_listview = 17000;
		st_listfontdemo = 17010;
		cb_setlistfont = 17020;
		cb_setlistcolor = 17030;
		st_list = 17040;
      st_blog	=	17045;
      st_blogfontdemo	=	17050;
      cb_setblogfont	=	17055;
      cb_setblogcolor	=	17060;
      st_blogtitle	=	17065;
      st_blogtitlefontdemo	=	17070;
      cb_setblogtitlefont	=	17075;
      cb_setblogtitlecolor	=	17080;
      st_blogseltitle	=	17085;
      st_blogseltitlefontdemo	=	17088;
      cb_setblogseltitlefont	=	17090;
      cb_setblogseltitlecolor	=	17095;

	ob_editor = 17100;
		st_editorfontdemo = 17110;
		cb_seteditorfont = 17120;
		cb_seteditorcolor = 17130;
		st_editor = 17140;
		chk_showlinenumber	=	17150;
		st_linenumberdemo	=	17160;
		cb_setlinenumbercolor	=	17170;
		chk_highlightselline	=	17180;
		st_sellinedemo	=	17190;
		cb_setsellinecolor = 17200;
		cb_HLColor	=	17210;
		st_HLDemo	=	17220;
		st_HLText	=	17230;
		chk_useunderline	=	17240;
		lst_HLText	=	17250;
		st_margins = 15280;
		cmb_margins	=	15290;

	ob_othercontrols = 17400;
      st_mininote	=	17410;
      st_mininotefontdemo	=	17420;
      cb_setmininotefont	=	17430;
      cb_setmininotecolor	=	17440;

	ob_extfuncs = 17600;
		chk_calcpage	=	17610;
		chk_memopage	=	17620;
		chk_dictpage	=	17630;
		chk_linkpage	=	17640;
		chk_contactpage	=	17660;
		chk_template	=	17670;
		chk_clipboard	=	17680;
		st_extfuncsprompt = 17690;

	ob_calcpage = 17800;
		cmb_decimal	=	17810;
		rb_radian = 17820;
		rb_degree = 17830;
		st_decimal = 17840;
		st_radianordegree = 17850;
		st_userfunctions = 17860;
		mle_userfunctions = 17870;

	ob_dictpage = 17950;
		chk_multidict	=	17960;
		lv_dictlist	=	17970;
		st_dictlist = 17980;
		sr_codetransfer = 17990;
		st_textcapture	=	18000;
		hk_textcapture = 18010;
		cb_setcolor	=	18020;
		st_colordemo	=	18030;

	ob_linkpage = 18100;
		chk_disableiconread	=	18110;
		chk_AutoMinimizeAfterOpenLink	=	18120;
		st_fastlinkhotkey = 18130;
		chk_EnablefastLinkHotKey = 18140;
		hk_fastlink = 18150;

	ob_template = 18200;
		chk_enableitemhotkey = 18210;
		st_templatehotkey = 18220;
		hk_template = 18230;
		chk_popupmenunofocus	=	18240;
		st_capturehotkey	=	18250;
		hk_capture	=	18260;

	ob_clipboard = 18300;
		cmb_maxclipnum	=	18310;
		cmb_maxitembyte	=	18320;
      chk_filtersameitems	=	18340;
      chk_onlyfilterneighboring	=	18345;
		st_maxclipnum = 18350;
		st_maxitembyte = 18360;
		st_cliphotkey = 18370;
		hk_clipboard = 18380;
		st_menuwidth	=	18390;
		cmb_menuwidth	=	18400;
		chk_newpastetotop	=	18410;

	Link_Box = 20000;
		st_title	=	20010;
		sle_title	=	20020;
      rb_filelink = 20030;
		rb_folderlink	=	20040;
		rb_pagelink	=	20050;
		rb_emaillink	=	20060;
		sle_linktext	=	20070;
		rb_batchlink	=	20080;
		mle_linktext	=	20090;
		hk_linkhotkey	=	20100;
		st_link = 20120;
		st_category = 20130;
		st_hotkey = 20140;
		st_link1 = 20150;
		st_category1 = 20160;
		sr_NewLink = 20170;
		sr_EditLink = 20180;

	Memo_Box = 20300;
		cmb_timemode	=	20310;
		st_time	=	20320;
		sle_days	=	20330;
		dtp_date = 20340;
		dtp_time = 20350;
		dtp_date2 = 20360;
		mle_description	=	20370;
		rb_reminder	=	20380;
		rb_executelink	=	20390;
		rb_noaction	=	20400;
		sle_soundfile	=	20410;
		st_to	=	20420;
		st_description = 20430;
		st_action = 20440;
		sr_today = 20450;
		sr_daily = 20460;
		sr_weekly = 20470;
		sr_monthly = 20480;
      sr_yearly = 20485;
		sr_timespan = 20490;
		sr_newReminder = 20500;
		sr_EditReminder = 20510;
		st_sound	=	20520;
		rb_soundfile	= 20530;
		rb_nosound	=	20540;
		rb_beep	=	20550;
		sr_notime = 20560;

	Template_Box = 20700;
      st_text = 20720;
		hk_templatehotkey	=	20730;
		sr_newTemplate = 20740;
		sr_EditTemplate = 20750;
      st_abbrev	=	20760;
      sle_abbrev	=	20770;

	ReminderPopup_Box = 20900;
		st_remindertext	=	20910;
		chk_popupagain	=	20920;
		cmb_time	=	20930;
		st_minuteslater = 20940;
		chk_noremind	=	20950;
		chk_deleteafterclose	=	20960;

	Contact_Box = 21000;
		sle_name = 21010;
		cmb_sex	=	21020;
		sle_mobile	=	21030;
		sle_email	=	21040;
		sle_im1	=	21050;
		sle_im2	=	21060;
		sle_company	=	21070;
		sle_department	=	21080;
		sle_address	=	21090;
		sle_zipcode	=	21100;
		sle_tel	=	21110;
		sle_fax	=	21120;
		sle_others	=	21130;
		mle_remark	=	21140;
		st_name = 21150;
		st_company = 21160;
		st_mobile = 21170;
		st_im1 = 21180;
		st_im2 = 21190;
		st_sex = 21200;
		st_address = 21210;
		st_zipcode = 21220;
		st_tel = 21230;
		st_fax = 21240;
		st_others = 21250;
		st_remark = 21260;
		st_department = 21270;
		sr_NewContact = 21280;
		sr_EditContact = 21290;



	sr_newpage = 25000;
	sr_inputpagename = 25010;
	sr_prompt = 25020;
	sr_deleteprompt = 25030;
	sr_rename = 25040;
	sr_clearprompt = 25050;

	sr_boy = 25060;
	sr_girl = 25070;

	sr_selectfile = 25080;
	sr_filefilter = 25090;
	sr_selectsoundfile = 25100;
	sr_soundfilefilter = 25110;
	sr_selectfolder = 25120;
	
	sr_memoAction = 25130;
	
	sr_Time = 25140;
	sr_Action = 25150;
	sr_UseSound = 25160;
	sr_SoundFile = 25170;
	sr_Description = 25180;
   
	sr_LinkType = 25210;
	sr_LinkTypes = 25190;
	sr_Link = 25200;
	sr_Hotkey = 25220;
   sr_Abbrev = 25225;

	sr_Name = 25230;
	sr_Sex = 25240;
	sr_Mobile = 25250;
	sr_Email = 25260;
	sr_IM1 = 25270;
	sr_IM2 = 25280;
	sr_Company = 25290;
	sr_Department = 25300;
	sr_Address = 25310;
	sr_Zipcode = 25320;
	sr_Tel = 25330;
	sr_Fax = 25340;
	sr_Others = 25350;

	sr_GroupPage = 25360;
	sr_TemplatePage = 25370;
	sr_FastLink = 25380;
	sr_Clipboard = 25390;

	sr_clearclipboardprompt = 25400;
	sr_SelectImportFile = 25410;
	sr_ImportFilter = 25420;
	sr_NameExportFile = 25430;
	sr_ExportFilter1 = 25440;
	sr_ExportFilter2 = 25450;
	sr_ExportFilter3 = 25460;
	sr_ListCopied = 25470;
	sr_RemoveFastLinkItemsPrompt = 25480;
	sr_DeleteItemsPrompt = 25490;
	sr_RemoveItemsPrompt = 25495;
	sr_DeletePagesPrompt = 25500;
	sr_Root = 25510;
	sr_DeleteGroupFailed = 25520;
	sr_DeleteGroupPrompt = 25530;
	sr_passwordnotconfirmed = 25540;
	sr_BrowseMailClient = 25550;
	sr_ExeFilter = 25560;
	sr_TemplateCaptured = 25570;
	sr_TemplateExists = 25580;
	sr_TableOfContents = 25590;
	sr_info = 25600;

	sr_SelectIcon = 25610;
	sr_IconFilter = 25620;
	sr_UnsupportedOperation = 25625;
	sr_NewNoteCreated = 25626;
	sr_SnapTextSuccess = 25627;
	sr_DataSaved = 25628;
   sr_newnotebgandsnaptextsuccess = 25630;

	sr_defgroupname = 25670;
	sr_deftagname = 25680;
	sr_defnotename = 25690;
	sr_defcalcname = 25700;
	sr_defmemoname = 25710;
	sr_defdictname = 25720;
	sr_deflinkname = 25730;
	sr_defcontactname = 25740;

	sr_normal = 25750;
	sr_locked = 25760;
	sr_protected = 25770;
	sr_readonly = 25780;

	sr_createtime = 25790;
	sr_modifytime = 25800;
	sr_visittime = 25810;
	sr_datetime = 25815;
	sr_exportfile = 25820;
	sr_remark = 25830;
	sr_title = 25840;
	sr_abstract = 25850;
	sr_searchresult = 25855;
	sr_externalsave = 25860;
   sr_nodepath = 25865;
   
	sr_favoritepage = 25870;
	sr_searchpage = 25880;
	sr_tagroot = 25890;
	sr_grouproot = 25900;
	sr_recyclebin = 25910;
	sr_Text = 25920;
	sr_RecentRoot = 25930;
	sr_RecentCreate = 25940;
	sr_RecentModify = 25950;
	sr_RecentVisit = 25960;

	sr_Equal = 25970;
	sr_NotEqual = 25980;
	sr_Include = 25990;
	sr_NotInclude = 26000;

	sr_SaveSnapText = 26010;
   sr_SnapTextSavedToFile = 26020;
   sr_ExportedToFolder = 26030;
   sr_PageExportedToFile = 26040;
   sr_PageExportedToClipboard = 26045;
   sr_GroupExportedToFile = 26050;

   sr_MepVersionNotMatch = 26060;
   sr_InvalidNodeLink = 26070;
   sr_applytoallprompt = 26080;

   sr_ImportingPrompt = 26090;
   sr_UserAbortImport = 26095;
   sr_ExportingPrompt = 26100;
   sr_UserAbortExport = 26105;
   sr_DeletingPrompt = 26110;
   sr_UserAbortDelete = 26115;

implementation
end.