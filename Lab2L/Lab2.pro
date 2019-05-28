GSTACKSIZE=10000000
/*****************************************************************************

		Copyright (c) My Company

 Project:  LAB2
 FileName: LAB2.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "lab2.inc"
include "lab2.con"
include "hlptopic.con"

%BEGIN_WIN Task Window
/***************************************************************************
		Event handling for Task Window
***************************************************************************/

predicates

  task_win_eh : EHANDLER

constants

%BEGIN Task Window, CreateParms, 16:10:48-23.2.2019, Code automatically updated!
  task_win_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings]
  task_win_Menu  = res_menu(idr_task_menu)
  task_win_Title = "Lab2"
  task_win_Help  = idh_contents
%END Task Window, CreateParms

clauses

%BEGIN Task Window, e_Create
task_win_eh(_Win,e_Create(_),0):- dlg_main_window_Create(_Win), win_Destroy(_Win),!, /* при открытиии стандартного окна его удаляем и создаём пользовательское окно*/
%BEGIN Task Window, InitControls, 16:10:48-23.2.2019, Code automatically updated!
%END Task Window, InitControls
%BEGIN Task Window, ToolbarCreate, 16:10:48-23.2.2019, Code automatically updated!
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
  	vpi_ShowHelp("lab2.hlp"),
	!.
%END Task Window, id_help_contents

%BEGIN Task Window, id_help_about
  task_win_eh(Win,e_Menu(id_help_about,_ShiftCtlAlt),0):-!,
	dlg_about_dialog_Create(Win),
	!.
%END Task Window, id_help_about

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
  	vpi_ShowHelpContext("lab2.hlp",HelpTopic).

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
  vpi_Init(task_win_Flags,task_win_eh,task_win_Menu,"lab2",task_win_Title).

%BEGIN_TLB Project toolbar, 16:10:48-23.2.2019, Code automatically updated!
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

%BEGIN_TLB Help line, 16:10:48-23.2.2019, Code automatically updated!
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

%BEGIN_DLG About dialog
/**************************************************************************
	Creation and event handling for dialog: About dialog
**************************************************************************/

constants

%BEGIN About dialog, CreateParms, 16:10:48-23.2.2019, Code automatically updated!
  dlg_about_dialog_ResID = idd_dlg_about
  dlg_about_dialog_DlgType = wd_Modal
  dlg_about_dialog_Help = idh_contents
%END About dialog, CreateParms

predicates

  dlg_about_dialog_eh : EHANDLER

clauses

  dlg_about_dialog_Create(Parent):-
	win_CreateResDialog(Parent,dlg_about_dialog_DlgType,dlg_about_dialog_ResID,dlg_about_dialog_eh,0).

%BEGIN About dialog, idc_ok _CtlInfo
  dlg_about_dialog_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END About dialog, idc_ok _CtlInfo
%MARK About dialog, new events

  dlg_about_dialog_eh(_,_,_):-!,fail.

%END_DLG About dialog

%BEGIN_DLG main_window
/**************************************************************************
	Creation and event handling for dialog: main_window
**************************************************************************/

constants

%BEGIN main_window, CreateParms, 20:53:38-26.2.2019, Code automatically updated!
  dlg_main_window_ResID = idd_main_window
  dlg_main_window_DlgType = wd_Modal
  dlg_main_window_Help = idh_contents
%END main_window, CreateParms

domains
  startNumber,endNumber = integer

predicates

  dlg_main_window_eh : EHANDLER
  dlg_main_window_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_main_window_update(DIALOG_VAL_LIST)
  prime_numbers(startNumber,endNumber, window)
  check_number(integer)
  
clauses

  dlg_main_window_Create(Parent):-

%MARK main_window, new variables

	dialog_CreateModal(Parent,dlg_main_window_ResID,"",
  		[
%BEGIN main_window, ControlList, 20:53:38-26.2.2019, Code automatically updated!
		df(idc_start,editstr("",[]),nopr),
		df(idc_end,editstr("",[]),nopr),
		df(idc_edit,listbox([],[0]),nopr)
%END main_window, ControlList
		],
		dlg_main_window_eh,0,VALLIST,ANSWER),
	dlg_main_window_handle_answer(ANSWER,VALLIST).

  dlg_main_window_handle_answer(idc_ok,VALLIST):-!,
	dlg_main_window_update(VALLIST).
  dlg_main_window_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_main_window_handle_answer(_,_):-
	errorexit().

  dlg_main_window_update(_VALLIST):-
%BEGIN main_window, Update controls, 20:53:38-26.2.2019, Code automatically updated!
	_IDC_START_VALUE = dialog_VLGetstr(idc_start,_VALLIST),
	_IDC_END_VALUE = dialog_VLGetstr(idc_end,_VALLIST),
	dialog_VLGetListBox(idc_edit,_VALLIST,_IDC_EDIT_ITEMLIST,_IDC_EDIT_SELECT),
%END main_window, Update controls
	true.

%MARK main_window, new events


%BEGIN main_window, idc_ok _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):- /**/
  StartHandle = win_GetCtlHandle(_Win,idc_start),
  EndHandle = win_GetCtlHandle(_Win,idc_end),
  StartText = win_GetText(StartHandle),
  EndText = win_GetText(EndHandle),
  str_int(StartText,StartNumber),
  str_int(EndText,EndNumber),
  StartNumber>0,
  StartNumber<EndNumber,
  1291>EndNumber,
  prime_numbers(StartNumber,EndNumber,_Win),
  	!.
  dlg_main_window_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
  StartHandle = win_GetCtlHandle(_Win,idc_start),
  EndHandle = win_GetCtlHandle(_Win,idc_end),
  StartText = win_GetText(StartHandle),
  EndText = win_GetText(EndHandle),
  str_int(StartText,StartNumber),
  str_int(EndText,EndNumber),
  1291<EndNumber,
  dlg_Error("Ошибка валидации","Слишком большое число. Выход за пределы диапазона! Максимальный диапазон 1291"),
  	!.
  dlg_main_window_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
  dlg_Error("Ошибка валидации","Неправильный ввод данных"),
  	!.
%END main_window, idc_ok _CtlInfo

%BEGIN main_window, idc_help _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):- dlg_Note("Задание","Написать программу для построения кубов четных чисел. Пределы ряда задаются пользователем, вывод результата осуществить в отдельное окно. Реализовать систему сообщений об ошибках "),
  	!.
%END main_window, idc_help _CtlInfo

%BEGIN main_window, idc_cancel _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):- win_Destroy(_Win),
	!.
%END main_window, idc_cancel _CtlInfo

  dlg_main_window_eh(_,_,_):-!,fail.

%END_DLG main_window

%BEGIN main_window, getPrimeNumbers

prime_numbers(StartNumber,EndNumber,_Win):- 
StartNumber>EndNumber,
dlg_Note("Сообщение","Построение ряда кубов четных чисел закончен"),
	!.

prime_numbers(StartNumber,EndNumber,_Win):-
check_number(StartNumber),
StartNumberX3 = StartNumber * StartNumber * StartNumber,
str_int(StartString, StartNumberX3),
EditHandle = win_GetCtlHandle(_Win,idc_edit),
lbox_Add(EditHandle,StartString),
StartNumberPlusTwo=StartNumber+2,
prime_numbers(StartNumberPlusTwo,EndNumber,_Win),
	!.

prime_numbers(StartNumber,EndNumber,_Win):-
StartNumberPlusOne=StartNumber+1,
prime_numbers(StartNumberPlusOne,EndNumber,_Win),
	!. 
%END main_window, primeNumbers

%BEGIN main_window, check_prime_number
check_number(Number):-
Number mod 2 = 0,
	!.
%END main_window, check_prime_number
