/*****************************************************************************

		Copyright (c) My Company

 Project:  LAB5
 FileName: LAB5.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "lab5.inc"
include "lab5.con"
include "hlptopic.con"

%BEGIN_WIN Task Window
/***************************************************************************
		Event handling for Task Window
***************************************************************************/

predicates

  task_win_eh : EHANDLER

constants

%BEGIN Task Window, CreateParms, 18:57:29-22.4.2019, Code automatically updated!
  task_win_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings]
  task_win_Menu  = res_menu(idr_task_menu)
  task_win_Title = "Lab5"
  task_win_Help  = idh_contents
%END Task Window, CreateParms

clauses

%BEGIN Task Window, e_Create
  task_win_eh(_Win,e_Create(_),0):-!,
  dlg_main_menu_Create(_Win), win_Destroy(_Win),
%BEGIN Task Window, InitControls, 18:57:29-22.4.2019, Code automatically updated!
%END Task Window, InitControls
%BEGIN Task Window, ToolbarCreate, 18:57:29-22.4.2019, Code automatically updated!
	tb_project_toolbar_Create(_Win),
	tb_help_line_Create(_Win),
%END Task Window, ToolbarCreate
ifdef use_message
	msg_Create(100),
enddef
	!.
%END Task Window, e_Create

%MARK Task Window, new events

%BEGIN Task Window, id_help_contents
  task_win_eh(_Win,e_Menu(id_help_contents,_ShiftCtlAlt),0):-!,
  	vpi_ShowHelp("lab5.hlp"),
	!.
%END Task Window, id_help_contents

%BEGIN Task Window, id_file_exit
  task_win_eh(Win,e_Menu(id_file_exit,_ShiftCtlAlt),0):-!,
  	win_Destroy(Win),
	!.
%END Task Window, id_file_exit

%BEGIN Task Window, e_Size
  task_win_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
ifdef use_message
	msg_Resize(_Win),
enddef
	!.
%END Task Window, e_Size

%END_WIN Task Window

/***************************************************************************
		Invoking on-line Help
***************************************************************************/

  project_ShowHelpContext(HelpTopic):-
  	vpi_ShowHelpContext("lab5.hlp",HelpTopic).

/***************************************************************************
			Main Goal
***************************************************************************/

goal

ifdef use_mdi
  vpi_SetAttrVal(attr_win_mdi,b_true),
enddef
ifdef ws_win
  ifdef use_3dctrl
    vpi_SetAttrVal(attr_win_3dcontrols,b_true),
  enddef
enddef  
  vpi_Init(task_win_Flags,task_win_eh,task_win_Menu,"lab5",task_win_Title).

%BEGIN_TLB Project toolbar, 18:57:29-22.4.2019, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Project toolbar
**************************************************************************/

clauses

  tb_project_toolbar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_top,0xC0C0C0,_Parent,
		[tb_ctrl(id_file_new,pushb,idb_new_up,idb_new_dn,idb_new_up,"New;New file",1,1),
		 tb_ctrl(id_file_open,pushb,idb_open_up,idb_open_dn,idb_open_up,"Open;Open file",1,1),
		 tb_ctrl(id_file_save,pushb,idb_save_up,idb_save_dn,idb_save_up,"Save;File save",1,1),
		 separator,
		 tb_ctrl(id_edit_undo,pushb,idb_undo_up,idb_undo_dn,idb_undo_up,"Undo;Undo",1,1),
		 tb_ctrl(id_edit_redo,pushb,idb_redo_up,idb_redo_dn,idb_redo_up,"Redo;Redo",1,1),
		 separator,
		 tb_ctrl(id_edit_cut,pushb,idb_cut_up,idb_cut_dn,idb_cut_up,"Cut;Cut to clipboard",1,1),
		 tb_ctrl(id_edit_copy,pushb,idb_copy_up,idb_copy_dn,idb_copy_up,"Copy;Copy to clipboard",1,1),
		 tb_ctrl(id_edit_paste,pushb,idb_paste_up,idb_paste_dn,idb_paste_up,"Paste;Paste from clipboard",1,1),
		 separator,
		 separator,
		 tb_ctrl(id_help_contents,pushb,idb_help_up,idb_help_down,idb_help_up,"Help;Help",1,1)]),
enddef
	true.
%END_TLB Project toolbar

%BEGIN_TLB Help line, 18:57:29-22.4.2019, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Help line
**************************************************************************/

clauses

  tb_help_line_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_text(idt_help_line,tb_context,452,0,4,10,0x0,"")]),
enddef
	true.
%END_TLB Help line

%BEGIN_DLG Entry
/**************************************************************************
	Creation and event handling for dialog: Entry
**************************************************************************/

constants

%BEGIN Entry, CreateParms, 19:04:56-9.5.2019, Code automatically updated!
  dlg_entry_ResID = idd_entry
  dlg_entry_DlgType = wd_Modal
  dlg_entry_Help = idh_contents
%END Entry, CreateParms

predicates

  dlg_entry_eh : EHANDLER
  dlg_entry_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_entry_update(DIALOG_VAL_LIST)

clauses

  dlg_entry_Create(Parent):-

%MARK Entry, new variables

	dialog_CreateModal(Parent,dlg_entry_ResID,"",
  		[
%BEGIN Entry, ControlList, 19:04:56-9.5.2019, Code automatically updated!
%END Entry, ControlList
		],
		dlg_entry_eh,0,VALLIST,ANSWER),
	dlg_entry_handle_answer(ANSWER,VALLIST).

  dlg_entry_handle_answer(idc_ok,VALLIST):-!,
	dlg_entry_update(VALLIST).
  dlg_entry_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_entry_handle_answer(_,_):-
	errorexit().

  dlg_entry_update(_VALLIST):-
%BEGIN Entry, Update controls, 19:04:56-9.5.2019, Code automatically updated!
%END Entry, Update controls
	true.

%MARK Entry, new events



%BEGIN Entry, idc_push_button_drama _CtlInfo
  dlg_entry_eh(_Win,e_Control(idc_push_button_drama,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  win_Destroy(_Win), dlg_find_yo_Create(_Win,"1"),
	!.
%END Entry, idc_push_button_drama _CtlInfo

%BEGIN Entry, idc_push_button_actionmovie _CtlInfo
  dlg_entry_eh(_Win,e_Control(idc_push_button_actionmovie,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  win_Destroy(_Win), dlg_find_yo_Create(_Win,"2"),
	!.
%END Entry, idc_push_button_actionmovie _CtlInfo

%BEGIN Entry, idc_push_button_anime _CtlInfo
  dlg_entry_eh(_Win,e_Control(idc_push_button_anime,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  win_Destroy(_Win), dlg_find_yo_Create(_Win,"0"),
	!.
%END Entry, idc_push_button_anime _CtlInfo

  dlg_entry_eh(_,_,_):-!,fail.

%END_DLG Entry

%BEGIN_DLG study
/**************************************************************************
	Creation and event handling for dialog: study
**************************************************************************/

constants

%BEGIN study, CreateParms, 18:22:23-9.5.2019, Code automatically updated!
  dlg_study_ResID = idd_study
  dlg_study_DlgType = wd_Modal
  dlg_study_Help = idh_contents
%END study, CreateParms

predicates

  dlg_study_eh : EHANDLER
  dlg_study_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_study_update(DIALOG_VAL_LIST)
  
  addMovie(integer,integer,integer,string)
  writeToFile(string,string)
  checkOfExistence(string,string)
  checkOfExistenceToFile(file,string)
clauses

  checkOfExistence(Path,TitleMovie):- openread(fileselector,Path), readdevice(fileselector),checkOfExistenceToFile(fileselector,TitleMovie), closefile(fileselector),!.
  checkOfExistence(_,_):- closefile(fileselector), fail,!.
  
  checkOfExistenceToFile(File,TitleMovie):- 
  not(eof(File)),
  readln(Readln), 
  Readln <> TitleMovie,
  checkOfExistenceToFile(File,TitleMovie),
  	!.
  checkOfExistenceToFile(File,_):- eof(File).

  addMovie(Genre,YO,Long,Name):- 
  str_int(GenreStr,Genre),str_int(YOStr,YO),str_int(LongStr,Long),
  concat(GenreStr,YOStr,GenreYO), concat(GenreYO,LongStr,GenreYOLong), concat("Database\\",GenreYOLong,DirGenreYOLong),
  checkOfExistence(DirGenreYOLong,Name), 
  writeToFile(DirGenreYOLong,Name),!. 
  
  writeToFile(Path,String):- openappend(fileselector,Path), writedevice(fileselector), write(String), nl, closefile(fileselector),!.
  
  dlg_study_Create(Parent):-

%MARK study, new variables

	dialog_CreateModal(Parent,dlg_study_ResID,"",
  		[
%BEGIN study, ControlList, 18:22:23-9.5.2019, Code automatically updated!
		df(idc_genre,listbox(["Аниме","Драма","Боевик"],[0]),nopr),
		df(idc_yo,listbox(["6+","12+","17+"],[0]),nopr),
		df(idc_long,listbox(["Меньше 90 минут","Больше 90 минут"],[0]),nopr),
		df(idc_name,editstr("",[]),nopr)
%END study, ControlList
		],
		dlg_study_eh,0,VALLIST,ANSWER),
	dlg_study_handle_answer(ANSWER,VALLIST).

  dlg_study_handle_answer(idc_ok,VALLIST):-!,
	dlg_study_update(VALLIST).
  dlg_study_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_study_handle_answer(_,_):-
	errorexit().

  dlg_study_update(_VALLIST):-
%BEGIN study, Update controls, 18:22:23-9.5.2019, Code automatically updated!
	dialog_VLGetListBox(idc_genre,_VALLIST,_IDC_GENRE_ITEMLIST,_IDC_GENRE_SELECT),
	dialog_VLGetListBox(idc_yo,_VALLIST,_IDC_YO_ITEMLIST,_IDC_YO_SELECT),
	dialog_VLGetListBox(idc_long,_VALLIST,_IDC_LONG_ITEMLIST,_IDC_LONG_SELECT),
	_IDC_NAME_VALUE = dialog_VLGetstr(idc_name,_VALLIST),
%END study, Update controls
	true.

%MARK study, new events

%BEGIN study, idc_ok _CtlInfo
  dlg_study_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
  MovieTitle_Handle = win_GetCtlHandle(_Win,idc_name),     
  MovieTitle_String = win_GetText(MovieTitle_Handle), 
  MovieTitle_String <> "",
  
  Genre_Handle = win_GetCtlHandle(_Win,idc_genre),
  YO_Handle = win_GetCtlHandle(_Win,idc_yo),
  Long_Handle = win_GetCtlHandle(_Win,idc_long),
  
  Genre_SelectedIndex = lbox_GetSelIndex(Genre_Handle),
  YO_SelectedIndex = lbox_GetSelIndex(YO_Handle),
  Long_SelectedIndex = lbox_GetSelIndex(Long_Handle),
    
  addMovie(Genre_SelectedIndex,YO_SelectedIndex,Long_SelectedIndex,MovieTitle_String),
  dlg_Note("Фильм успешно добавлен"),
  win_SetText(MovieTitle_Handle,""), 
	!.
	
  dlg_study_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
  MovieTitle_Handle = win_GetCtlHandle(_Win,idc_name),     
  MovieTitle_String = win_GetText(MovieTitle_Handle), 
  MovieTitle_String = "",
  dlg_Error("Ошибка", "Введите название фильма!"),
	!.
	
  dlg_study_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
  dlg_Error("Ошибка", "Такой фильм уже есть в базе!"),
	!.
	
%END study, idc_ok _CtlInfo

%BEGIN study, idc_cancel _CtlInfo
  dlg_study_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  win_Destroy(_Win), dlg_main_menu_Create(_Win),
	!.
%END study, idc_cancel _CtlInfo

  dlg_study_eh(_,_,_):-!,fail.

%END_DLG study

%BEGIN_DLG main_menu
/**************************************************************************
	Creation and event handling for dialog: main_menu
**************************************************************************/

constants

%BEGIN main_menu, CreateParms, 17:13:31-9.5.2019, Code automatically updated!
  dlg_main_menu_ResID = idd_main_menu
  dlg_main_menu_DlgType = wd_Modal
  dlg_main_menu_Help = idh_contents
%END main_menu, CreateParms

predicates

  dlg_main_menu_eh : EHANDLER
  dlg_main_menu_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_main_menu_update(DIALOG_VAL_LIST)

clauses

  dlg_main_menu_Create(Parent):-

%MARK main_menu, new variables

	dialog_CreateModal(Parent,dlg_main_menu_ResID,"",
  		[
%BEGIN main_menu, ControlList, 17:13:31-9.5.2019, Code automatically updated!
%END main_menu, ControlList
		],
		dlg_main_menu_eh,0,VALLIST,ANSWER),
	dlg_main_menu_handle_answer(ANSWER,VALLIST).

  dlg_main_menu_handle_answer(idc_ok,VALLIST):-!,
	dlg_main_menu_update(VALLIST).
  dlg_main_menu_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_main_menu_handle_answer(_,_):-
	errorexit().

  dlg_main_menu_update(_VALLIST):-
%BEGIN main_menu, Update controls, 17:13:31-9.5.2019, Code automatically updated!
%END main_menu, Update controls
	true.

%MARK main_menu, new events

%BEGIN main_menu, e_Create
  dlg_main_menu_eh(_Win,e_Create(_CreationData),0):-!,
	!.
%END main_menu, e_Create


%BEGIN main_menu, idc_find_film _CtlInfo
  dlg_main_menu_eh(_Win,e_Control(idc_find_film,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  win_Destroy(_Win), dlg_entry_Create(_Win),
	!.
%END main_menu, idc_find_film _CtlInfo

%BEGIN main_menu, idc_add_film _CtlInfo
  dlg_main_menu_eh(_Win,e_Control(idc_add_film,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  win_Destroy(_Win), dlg_study_Create(_Win),
	!.
%END main_menu, idc_add_film _CtlInfo

  dlg_main_menu_eh(_,_,_):-!,fail.

%END_DLG main_menu

%BEGIN_DLG find_yo
/**************************************************************************
	Creation and event handling for dialog: find_yo
**************************************************************************/

constants

%BEGIN find_yo, CreateParms, 20:55:38-9.5.2019, Code automatically updated!
  dlg_find_yo_ResID = idd_find_yo
  dlg_find_yo_DlgType = wd_Modal
  dlg_find_yo_Help = contents
%END find_yo, CreateParms


facts-find_yo
choices_find_yo(string)

predicates

  dlg_find_yo_eh : EHANDLER
  dlg_find_yo_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_find_yo_update(DIALOG_VAL_LIST)

clauses

  dlg_find_yo_Create(Parent,String):-
	assert (choices_find_yo(String)),
%MARK find_yo, new variables

	dialog_CreateModal(Parent,dlg_find_yo_ResID,"",
  		[
%BEGIN find_yo, ControlList, 20:55:38-9.5.2019, Code automatically updated!
%END find_yo, ControlList
		],
		dlg_find_yo_eh,0,VALLIST,ANSWER),
	dlg_find_yo_handle_answer(ANSWER,VALLIST).

  dlg_find_yo_handle_answer(idc_ok,VALLIST):-!,
	dlg_find_yo_update(VALLIST).
  dlg_find_yo_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_find_yo_handle_answer(_,_):-
	errorexit().

  dlg_find_yo_update(_VALLIST):-
%BEGIN find_yo, Update controls, 20:55:38-9.5.2019, Code automatically updated!
%END find_yo, Update controls
	true.

%MARK find_yo, new events


%BEGIN find_yo, idc_17 _CtlInfo
  dlg_find_yo_eh(_Win,e_Control(idc_17,_CtrlType,_CtrlWin,_CtlInfo),0):-
  choices_find_yo(Choices), retract(choices_find_yo(Choices)), concat(Choices,"2",ChoicesResult),
  win_Destroy(_Win), dlg_find_long_Create(_Win,ChoicesResult),
	!.
%END find_yo, idc_17 _CtlInfo

%BEGIN find_yo, idc_12 _CtlInfo
  dlg_find_yo_eh(_Win,e_Control(idc_12,_CtrlType,_CtrlWin,_CtlInfo),0):-
  choices_find_yo(Choices), retract(choices_find_yo(Choices)), concat(Choices,"1",ChoicesResult),
  win_Destroy(_Win), dlg_find_long_Create(_Win,ChoicesResult),
	!.
%END find_yo, idc_12 _CtlInfo

%BEGIN find_yo, idc_6 _CtlInfo
  dlg_find_yo_eh(_Win,e_Control(idc_6,_CtrlType,_CtrlWin,_CtlInfo),0):-
  choices_find_yo(Choices), retract(choices_find_yo(Choices)), concat(Choices,"0",ChoicesResult),
  win_Destroy(_Win), dlg_find_long_Create(_Win,ChoicesResult),
	!.
%END find_yo, idc_6 _CtlInfo

  dlg_find_yo_eh(_,_,_):-!,fail.

%END_DLG find_yo

%BEGIN_DLG find_long
/**************************************************************************
	Creation and event handling for dialog: find_long
**************************************************************************/

constants

%BEGIN find_long, CreateParms, 16:05:48-11.5.2019, Code automatically updated!
  dlg_find_long_ResID = idd_find_long
  dlg_find_long_DlgType = wd_Modal
  dlg_find_long_Help = contents
%END find_long, CreateParms

facts-find_long
choices_find_long(string)

predicates

  dlg_find_long_eh : EHANDLER
  dlg_find_long_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_find_long_update(DIALOG_VAL_LIST)
  
clauses

  dlg_find_long_Create(Parent,String):-
	assert (choices_find_long(String)),
%MARK find_long, new variables

	dialog_CreateModal(Parent,dlg_find_long_ResID,"",
  		[
%BEGIN find_long, ControlList, 16:05:48-11.5.2019, Code automatically updated!
%END find_long, ControlList
		],
		dlg_find_long_eh,0,VALLIST,ANSWER),
	dlg_find_long_handle_answer(ANSWER,VALLIST).

  dlg_find_long_handle_answer(idc_ok,VALLIST):-!,
	dlg_find_long_update(VALLIST).
  dlg_find_long_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_find_long_handle_answer(_,_):-
	errorexit().

  dlg_find_long_update(_VALLIST):-
%BEGIN find_long, Update controls, 16:05:48-11.5.2019, Code automatically updated!
%END find_long, Update controls
	true.

%MARK find_long, new events


%BEGIN find_long, idc_short _CtlInfo
  dlg_find_long_eh(_Win,e_Control(idc_short,_CtrlType,_CtrlWin,_CtlInfo),0):-
  choices_find_long(Choices), retract(choices_find_long(Choices)), concat(Choices,"0",ChoicesResult),
  win_Destroy(_Win), dlg_find_result_Create(_Win,ChoicesResult),
  	!.
%END find_long, idc_short _CtlInfo

%BEGIN find_long, idc_long _CtlInfo
  dlg_find_long_eh(_Win,e_Control(idc_long,_CtrlType,_CtrlWin,_CtlInfo),0):-
  choices_find_long(Choices), retract(choices_find_long(Choices)), concat(Choices,"1",ChoicesResult),
  win_Destroy(_Win), dlg_find_result_Create(_Win,ChoicesResult),
	!.
%END find_long, idc_long _CtlInfo

  dlg_find_long_eh(_,_,_):-!,fail.

%END_DLG find_long

%BEGIN_DLG find_result
/**************************************************************************
	Creation and event handling for dialog: find_result
**************************************************************************/

constants

%BEGIN find_result, CreateParms, 16:05:04-11.5.2019, Code automatically updated!
  dlg_find_result_ResID = idd_find_result
  dlg_find_result_DlgType = wd_Modal
  dlg_find_result_Help = contents
%END find_result, CreateParms

facts-find_result
choices_find_result(string)

predicates

  dlg_find_result_eh : EHANDLER
  dlg_find_result_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_find_result_update(DIALOG_VAL_LIST)
  
  getAnswer(string,string)
  readFromFile(file,string,string)
  
  attributeIndex0ToName(char,string)
  attributeIndex1ToName(char,string)
  attributeIndex2ToName(string,string)
clauses
  
  getAnswer(Path,Answer):- 
  concat("Database\\",Path,DirPath), 
  openread(fileselector,DirPath), 
  readdevice(fileselector), 
  readFromFile(fileselector,"",Answer), 
  closefile(fileselector),
  	!.
  readFromFile(File,AnswerTemp,Answer):-
  AnswerTemp="",
  not(eof(File)),
  readln(Readln),
  concat(Readln,AnswerTemp,AnswerTemp2), 
  readFromFile(File,AnswerTemp2,Answer),
  	!.
  readFromFile(File,AnswerTemp,Answer):- 
  not(eof(File)),
  readln(Readln),
  concat(Readln,", ",ReadlnTemp),concat(ReadlnTemp,AnswerTemp,AnswerTemp2), 
  readFromFile(File,AnswerTemp2,Answer),
  	!.
  readFromFile(File,AnswerTemp,AnswerTemp):- eof(File).

  attributeIndex0ToName('0',"Аниме").
  attributeIndex0ToName('1',"Драма").
  attributeIndex0ToName('2',"Боевик").
  
  attributeIndex1ToName('0',"6+").
  attributeIndex1ToName('1',"12+").
  attributeIndex1ToName('2',"17+"). 
  
  attributeIndex2ToName("0","Меньше 90 минут").
  attributeIndex2ToName("1","Больше 90 минут").
   
  dlg_find_result_Create(Parent,String):-
	assert (choices_find_result(String)),
%MARK find_result, new variables

	dialog_CreateModal(Parent,dlg_find_result_ResID,"",
  		[
%BEGIN find_result, ControlList, 16:05:04-11.5.2019, Code automatically updated!
		df(idc_genre,editstr("",[]),nopr),
		df(idc_yo,editstr("",[]),nopr),
		df(idc_long,editstr("",[]),nopr),
		df(idc_edit,editstr("",[]),nopr)
%END find_result, ControlList
		],
		dlg_find_result_eh,0,VALLIST,ANSWER),
	dlg_find_result_handle_answer(ANSWER,VALLIST).

  dlg_find_result_handle_answer(idc_ok,VALLIST):-!,
	dlg_find_result_update(VALLIST).
  dlg_find_result_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_find_result_handle_answer(_,_):-
	errorexit().

  dlg_find_result_update(_VALLIST):-
%BEGIN find_result, Update controls, 16:05:04-11.5.2019, Code automatically updated!
	_IDC_GENRE_VALUE = dialog_VLGetstr(idc_genre,_VALLIST),
	_IDC_YO_VALUE = dialog_VLGetstr(idc_yo,_VALLIST),
	_IDC_LONG_VALUE = dialog_VLGetstr(idc_long,_VALLIST),
	_IDC_EDIT_VALUE = dialog_VLGetstr(idc_edit,_VALLIST),
%END find_result, Update controls
	true.

%MARK find_result, new events


%BEGIN find_result, idc_rerun _CtlInfo
  dlg_find_result_eh(_Win,e_Control(idc_rerun,_CtrlType,_CtrlWin,_CtlInfo),0):-
  choices_find_result(Choices),
  getAnswer(Choices,Answer),
  Answer <> "",
  Answer_TextBox_Handle = win_GetCtlHandle(_Win,idc_edit),     
  win_SetText(Answer_TextBox_Handle,Answer),
	!.
%END find_result, idc_rerun _CtlInfo

%BEGIN find_result, e_Create
  dlg_find_result_eh(_Win,e_Create(_CreationData),0):-
  Answer_TextBox_Handle = win_GetCtlHandle(_Win,idc_edit),
  Аttribute0_Handle = win_GetCtlHandle(_Win,idc_genre),
  Аttribute1_Handle = win_GetCtlHandle(_Win,idc_yo),
  Аttribute2_Handle = win_GetCtlHandle(_Win,idc_long),
  
  choices_find_result(Choices),

  frontChar(Choices,Choices0, ChoicesM1),
  frontChar(ChoicesM1,Choices1, Choices2),
  attributeIndex0ToName(Choices0,Choices0Name),
  attributeIndex1ToName(Choices1,Choices1Name),
  attributeIndex2ToName(Choices2,Choices2Name),
  
  win_SetText(Аttribute0_Handle,Choices0Name),
  win_SetText(Аttribute1_Handle,Choices1Name),
  win_SetText(Аttribute2_Handle,Choices2Name),
  
  getAnswer(Choices,Answer),
  Answer <> "",     
  win_SetText(Answer_TextBox_Handle,Answer),
	!.
  dlg_find_result_eh(_Win,e_Create(_CreationData),0):-
  Answer_TextBox_Handle = win_GetCtlHandle(_Win,idc_edit),     
  win_SetText(Answer_TextBox_Handle,"Empty"),
  dlg_Note("Результат поиска","По заданным критериям поиска фильмы не найдены. Вы можете добавить фильмы сами. Для этого перейдите в \"Главное меню->Добавить фильм\"."),
	!.
%END find_result, e_Create


%BEGIN find_result, idc_ok _CtlInfo
  dlg_find_result_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):- choices_find_result(Choices), retract(choices_find_result(Choices)), win_Destroy(_Win), dlg_main_menu_Create(_Win),!.
%END find_result, idc_ok _CtlInfo

  dlg_find_result_eh(_,_,_):-!,fail.

%END_DLG find_result

