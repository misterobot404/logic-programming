/*****************************************************************************

		Copyright (c) My Company

 Project:  LAB3
 FileName: LAB3.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "lab3.inc"
include "lab3.con"
include "hlptopic.con"

%BEGIN_WIN Task Window
/***************************************************************************
		Event handling for Task Window
***************************************************************************/

predicates

  task_win_eh : EHANDLER

constants

%BEGIN Task Window, CreateParms, 21:39:04-5.4.2019, Code automatically updated!
  task_win_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings]
  task_win_Menu  = res_menu(idr_task_menu)
  task_win_Title = "Lab3"
  task_win_Help  = idh_contents
%END Task Window, CreateParms

clauses


%MARK Task Window, new events

%BEGIN Task Window, e_Create
  task_win_eh(_Win,e_Create(_CreationData),0):-!,
  dlg_main_window_Create(_Win), win_Destroy(_Win),!.
%END Task Window, e_Create

%BEGIN Task Window, id_help_contents
  task_win_eh(_Win,e_Menu(id_help_contents,_ShiftCtlAlt),0):-!,
  	vpi_ShowHelp("lab3.hlp"),
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
  	vpi_ShowHelpContext("lab3.hlp",HelpTopic).

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
  vpi_Init(task_win_Flags,task_win_eh,task_win_Menu,"lab3",task_win_Title).

%BEGIN_TLB Project toolbar, 21:39:04-5.4.2019, Code automatically updated!
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

%BEGIN_TLB Help line, 21:39:04-5.4.2019, Code automatically updated!
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

%BEGIN About dialog, CreateParms, 21:39:04-5.4.2019, Code automatically updated!
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

%BEGIN main_window, CreateParms, 11:47:08-11.4.2019, Code automatically updated!
  dlg_main_window_ResID = idd_main_window
  dlg_main_window_DlgType = wd_Modal
  dlg_main_window_Help = idh_contents
%END main_window, CreateParms

domains

  intArray = integer*
  symArray = symbol*

predicates

  dlg_main_window_eh : EHANDLER
  dlg_main_window_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_main_window_update(DIALOG_VAL_LIST)
  
  %num_list_to_string(intArray, string)
  sym_list_to_string(symArray, string)
    
  string_to_list(string, symArray)
  
  count_symbol_before_separator(string, symbol, integer, integer)
  
clauses
  /*
  num_list_to_string([], "").  
  num_list_to_string([H | T], S) :- num_list_to_string(T, S1), str_int(C, H), concat(C," ", C_with_space), concat(C_with_space, S1, S).
  */
  
  sym_list_to_string([], "").
  sym_list_to_string([H | T], S) :- T = [], sym_list_to_string(T, S1), !, concat(H, S1, S).
  sym_list_to_string([H | T], S) :- sym_list_to_string(T, S1), !, concat(H, ", ", H_with_space), concat(H_with_space, S1, S).
  
  string_to_list("",[]):- !.
  string_to_list(String,[Symbol|Tail]):- count_symbol_before_separator(String, ",", CountSymbol, TextEnd), TextEnd = 0, !, frontstr(CountSymbol, String, Symbol, StringTailTemp), frontstr(1, StringTailTemp, _, StringTail), string_to_list(StringTail, Tail).
  string_to_list(String,[Symbol|Tail]):- count_symbol_before_separator(String, ",", CountSymbol, TextEnd), TextEnd = 1, frontstr(CountSymbol, String, Symbol, _), Tail = [].
  
  count_symbol_before_separator("", _, Count, TextEnd):- Count = 0, TextEnd = 1, !.
  count_symbol_before_separator(String, Separator, Count, TextEnd):- frontstr(1, String,Symbol,StringTail), Symbol <> Separator, count_symbol_before_separator(StringTail, Separator, CountPlusOne, TextEnd), Count = CountPlusOne + 1, !.
  count_symbol_before_separator(_, _, Count, TextEnd):- Count = 0, TextEnd = 0.
      
  dlg_main_window_Create(Parent):-

%MARK main_window, new variables

	dialog_CreateModal(Parent,dlg_main_window_ResID,"",
  		[
%BEGIN main_window, ControlList, 11:47:08-11.4.2019, Code automatically updated!
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
%BEGIN main_window, Update controls, 11:47:08-11.4.2019, Code automatically updated!
%END main_window, Update controls
	true.

%MARK main_window, new events

%BEGIN main_window, idc_sort _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_sort,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_сортировка_по_убыванию_методом_вставок_Create(_Win),
	!.
%END main_window, idc_sort _CtlInfo

%BEGIN main_window, idc_append _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_append,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_Create(_Win),
	!.
%END main_window, idc_append _CtlInfo


%BEGIN main_window, idc_reverse _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_reverse,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_revers_Create(_Win),
	!.
%END main_window, idc_reverse _CtlInfo

%BEGIN main_window, idc_insert_el _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_insert_el,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_insert_el_Create(_Win),
	!.
%END main_window, idc_insert_el _CtlInfo

%BEGIN main_window, idc_help _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_Note("Помощь","В данной програме список представляет собой последовательность элементов разделенных запятыми. В конце списка точку ставить не нужно."),
	!.
%END main_window, idc_help _CtlInfo

%BEGIN main_window, idc_cancel _CtlInfo
  dlg_main_window_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!, win_Destroy(_Win),
	!.
%END main_window, idc_cancel _CtlInfo

  dlg_main_window_eh(_,_,_):-!,fail.

%END_DLG main_window

%BEGIN_DLG insert_el
/**************************************************************************
	Creation and event handling for dialog: insert_el
**************************************************************************/
constants

%BEGIN insert_el, CreateParms, 16:20:47-13.4.2019, Code automatically updated!
  dlg_insert_el_ResID = idd_insert_el
  dlg_insert_el_DlgType = wd_Modal
  dlg_insert_el_Help = idh_contents
%END insert_el, CreateParms

predicates

  %insert(integer, integer, intArray, intArray)
  insert(symbol, symbol, symArray, symArray)
  find_el(symbol, symArray, integer)

  dlg_insert_el_eh : EHANDLER
  dlg_insert_el_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_insert_el_update(DIALOG_VAL_LIST)

clauses 
     
  insert(_,_,[],[]).
  insert(X,Head,[Head|Z],[Head,X|T]):- insert(X,Head,Z,T),!.
  insert(X,Y,[Head|Z],[Head|N]):- insert(X,Y,Z,N).
  
  find_el(_, [], Result):- Result = 0, !.
  find_el(Symbol, [Head|Tail], Result):- Head <> Symbol, find_el(Symbol, Tail, Result), !.
  find_el(_, _, Result):- Result = 1.
   
  dlg_insert_el_Create(Parent):-

%MARK insert_el, new variables

	dialog_CreateModal(Parent,dlg_insert_el_ResID,"",
  		[
%BEGIN insert_el, ControlList, 16:20:47-13.4.2019, Code automatically updated!
		df(idc_sym_list_res,listbox([],[0]),nopr),
		df(idc_edit_insert_sym,editstr("",[]),nopr),
		df(idc_edit_after_sym,editstr("",[]),nopr),
		df(idc_edit,editstr("",[]),nopr)
%END insert_el, ControlList
		],
		dlg_insert_el_eh,0,VALLIST,ANSWER),
	dlg_insert_el_handle_answer(ANSWER,VALLIST).

  dlg_insert_el_handle_answer(idc_ok,VALLIST):-!,
	dlg_insert_el_update(VALLIST).
  dlg_insert_el_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_insert_el_handle_answer(_,_):-
	errorexit().

  dlg_insert_el_update(_VALLIST):-
%BEGIN insert_el, Update controls, 16:20:47-13.4.2019, Code automatically updated!
	dialog_VLGetListBox(idc_sym_list_res,_VALLIST,_IDC_SYM_LIST_RES_ITEMLIST,_IDC_SYM_LIST_RES_SELECT),
	_IDC_EDIT_INSERT_SYM_VALUE = dialog_VLGetstr(idc_edit_insert_sym,_VALLIST),
	_IDC_EDIT_AFTER_SYM_VALUE = dialog_VLGetstr(idc_edit_after_sym,_VALLIST),
	_IDC_EDIT_VALUE = dialog_VLGetstr(idc_edit,_VALLIST),
%END insert_el, Update controls
	true.

%MARK insert_el, new events

%BEGIN insert_el, idc_ok_sym _CtlInfo
  dlg_insert_el_eh(_Win,e_Control(idc_ok_sym,_CtrlType,_CtrlWin,_CtlInfo),0):- 
  LBox_Handle = win_GetCtlHandle(_Win,idc_sym_list_res),
  
  Array_Handle = win_GetCtlHandle(_Win,idc_edit),
  Array_Text = win_GetText(Array_Handle),
  Array_Text <> "",  
  string_to_list(Array_Text, SymArray),
    
  EditInsert_Handle = win_GetCtlHandle(_Win,idc_edit_insert_sym),
  EditInsert_Text =win_GetText(EditInsert_Handle),
  EditInsert_Text <> "",
  
  EditAfter_Handle = win_GetCtlHandle(_Win,idc_edit_after_sym),
  EditAfter_Text =win_GetText(EditAfter_Handle),
  EditAfter_Text <> "",
  find_el(EditAfter_Text, SymArray, FindResult),
  FindResult <> 0,
  
  insert(EditInsert_Text, EditAfter_Text, SymArray, SymArrayRes),
  
  sym_list_to_string(SymArrayRes,StringArray),
  lbox_Clear (LBox_Handle),
  lbox_Add(LBox_Handle, StringArray),
	!.
	
  dlg_insert_el_eh(_Win,e_Control(idc_ok_sym,_CtrlType,_CtrlWin,_CtlInfo),0):- 
  dlg_Error("Ошибка", "Ошибка валидации данных. Проверьте, что бы символ, после которого нужно производить вставку, существовал"),	
  	!.	
%END insert_el, idc_ok_sym _CtlInfo

/*
%BEGIN insert_el, idc_ok _CtlInfo
  dlg_insert_el_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  LBox_Handle = win_GetCtlHandle(_Win,idc_num_list_res),
  
  EditAfter_Handle = win_GetCtlHandle(_Win,idc_edit_after),
  EditAfter_Text =win_GetText(EditAfter_Handle),
  str_int(EditAfter_Text, After_number),
  
  EditInsert_Handle = win_GetCtlHandle(_Win,idc_edit_insert),
  EditInsert_Text =win_GetText(EditInsert_Handle),
  str_int(EditInsert_Text, Insert_number),
  
  NumArray = [-5,4,0,-15,4,0,1,-5],   
  insert(Insert_number, After_number, NumArray, NumArrayRes),
  
  num_list_to_string(NumArrayRes,StringArray),
  lbox_Clear (LBox_Handle),
  lbox_Add(LBox_Handle, StringArray),
	!.
%END insert_el, idc_ok _CtlInfo
*/

  dlg_insert_el_eh(_,_,_):-!,fail.

%END_DLG insert_el

%BEGIN_DLG revers
/**************************************************************************
	Creation and event handling for dialog: revers
**************************************************************************/

constants

%BEGIN revers, CreateParms, 16:10:25-13.4.2019, Code automatically updated!
  dlg_revers_ResID = idd_revers
  dlg_revers_DlgType = wd_Modal
  dlg_revers_Help = idh_contents
%END revers, CreateParms

predicates

  dlg_revers_eh : EHANDLER
  dlg_revers_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_revers_update(DIALOG_VAL_LIST)
  
  revers(symArray, symArray)
  revers(symArray, symArray, symArray)

clauses

  revers(X,Y):- revers([],X,Y).  
  revers(Y,[],Y).  
  revers(X1,[Z|X2],Y):- revers([Z|X1],X2,Y).
  
  dlg_revers_Create(Parent):-

%MARK revers, new variables

	dialog_CreateModal(Parent,dlg_revers_ResID,"",
  		[
%BEGIN revers, ControlList, 16:10:25-13.4.2019, Code automatically updated!
		df(id_edit,listbox([],[0]),nopr),
		df(idc_edit,editstr("",[]),nopr)
%END revers, ControlList
		],
		dlg_revers_eh,0,VALLIST,ANSWER),
	dlg_revers_handle_answer(ANSWER,VALLIST).

  dlg_revers_handle_answer(idc_ok,VALLIST):-!,
	dlg_revers_update(VALLIST).
  dlg_revers_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_revers_handle_answer(_,_):-
	errorexit().

  dlg_revers_update(_VALLIST):-
%BEGIN revers, Update controls, 16:10:25-13.4.2019, Code automatically updated!
	dialog_VLGetListBox(id_edit,_VALLIST,_ID_EDIT_ITEMLIST,_ID_EDIT_SELECT),
	_IDC_EDIT_VALUE = dialog_VLGetstr(idc_edit,_VALLIST),
%END revers, Update controls
	true.

%MARK revers, new events

%BEGIN revers, idc_ok_sym _CtlInfo
  dlg_revers_eh(_Win,e_Control(idc_ok_sym,_CtrlType,_CtrlWin,_CtlInfo),0):-
  LBox_Handle = win_GetCtlHandle(_Win,id_edit),
  
  Array1_Handle = win_GetCtlHandle(_Win,idc_edit),
  Array1_Text = win_GetText(Array1_Handle),
  Array1_Text <> "",
  
  string_to_list(Array1_Text, SymArray),
  
  revers(SymArray,SymArrayRes),
  sym_list_to_string(SymArrayRes,StringRes),
  
  lbox_Clear(LBox_Handle),
  lbox_Add(LBox_Handle, StringRes),
	!.
%END revers, idc_ok_sym _CtlInfo
  dlg_revers_eh(_Win,e_Control(idc_ok_sym,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Ошибка", "Ничего не введено!"),	
  	!.

  dlg_revers_eh(_,_,_):-!,fail.

%END_DLG revers

%BEGIN_DLG ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ
/**************************************************************************
	Creation and event handling for dialog: ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ
**************************************************************************/

constants

%BEGIN ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, CreateParms, 15:51:46-13.4.2019, Code automatically updated!
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_ResID = idd_append
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_DlgType = wd_Modal
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_Help = idh_contents
%END ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, CreateParms

predicates

  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_eh : EHANDLER
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_update(DIALOG_VAL_LIST)
  
  %append(integer, integer, intArray, intArray, intArray, intArray)
  %append(intArray,intArray,intArray)
    
  append(integer, integer, symArray, symArray, symArray, symArray)
  append(symArray,symArray,symArray)

clauses

  append([], L, L).
  append([N|L1], L2, [N|L3]) :- append(L1,L2,L3).
  
  append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=1, Second=2, !, append(L1, L2, Res), append(Res, L3, L4).
  append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=1, Second=3, !, append(L1, L3, Res), append(Res, L2, L4).
  append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=2, Second=1, !, append(L2, L1, Res), append(Res, L3, L4).
  append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=2, Second=3, !, append(L2, L3, Res), append(Res, L1, L4).
  append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=3, Second=1, !, append(L3, L1, Res), append(Res, L2, L4).
  append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=3, Second=2, !, append(L3, L2, Res), append(Res, L1, L4).
  
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_Create(Parent):-

%MARK ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, new variables

	dialog_CreateModal(Parent,dlg_объединение_трёх_списков_в_один_в_указанном_порядке_ResID,"",
  		[
%BEGIN ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, ControlList, 15:51:46-13.4.2019, Code automatically updated!
		df(idc_lbox_res,listbox([],[0]),nopr),
		df(idc_edit_sym_second,editstr("",[]),nopr),
		df(idc_edit_sym_first,editstr("",[]),nopr),
		df(idc_edit1,editstr("",[]),nopr),
		df(idc_edit2,editstr("",[]),nopr),
		df(idc_edit3,editstr("",[]),nopr)
%END ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, ControlList
		],
		dlg_объединение_трёх_списков_в_один_в_указанном_порядке_eh,0,VALLIST,ANSWER),
	dlg_объединение_трёх_списков_в_один_в_указанном_порядке_handle_answer(ANSWER,VALLIST).

  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_handle_answer(idc_ok,VALLIST):-!,
	dlg_объединение_трёх_списков_в_один_в_указанном_порядке_update(VALLIST).
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_handle_answer(_,_):-
	errorexit().

  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_update(_VALLIST):-
%BEGIN ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, Update controls, 15:51:46-13.4.2019, Code automatically updated!
	dialog_VLGetListBox(idc_lbox_res,_VALLIST,_IDC_LBOX_RES_ITEMLIST,_IDC_LBOX_RES_SELECT),
	_IDC_EDIT_SYM_SECOND_VALUE = dialog_VLGetstr(idc_edit_sym_second,_VALLIST),
	_IDC_EDIT_SYM_FIRST_VALUE = dialog_VLGetstr(idc_edit_sym_first,_VALLIST),
	_IDC_EDIT1_VALUE = dialog_VLGetstr(idc_edit1,_VALLIST),
	_IDC_EDIT2_VALUE = dialog_VLGetstr(idc_edit2,_VALLIST),
	_IDC_EDIT3_VALUE = dialog_VLGetstr(idc_edit3,_VALLIST),
%END ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, Update controls
	true.

%MARK ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, new events

%BEGIN ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, idc_ok_sym _CtlInfo
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_eh(_Win,e_Control(idc_ok_sym,_CtrlType,_CtrlWin,_CtlInfo),0):-
  LBox_Handle = win_GetCtlHandle(_Win,idc_lbox_res),
  
  Array1_Handle = win_GetCtlHandle(_Win,idc_edit1),
  Array1_Text = win_GetText(Array1_Handle), 
  string_to_list(Array1_Text, Array1),
  
  Array2_Handle = win_GetCtlHandle(_Win,idc_edit2),
  Array2_Text = win_GetText(Array2_Handle), 
  string_to_list(Array2_Text, Array2), 
  
  Array3_Handle = win_GetCtlHandle(_Win,idc_edit3),
  Array3_Text = win_GetText(Array3_Handle), 
  string_to_list(Array3_Text, Array3),
  
  Edit1_Handle = win_GetCtlHandle(_Win,idc_edit_sym_first),
  Edit1_Text = win_GetText(Edit1_Handle),
  str_int(Edit1_Text, First),
  First>=1,
  First<=3,
  
  Edit2_Handle = win_GetCtlHandle(_Win,idc_edit_sym_second),
  Edit2_Text =win_GetText(Edit2_Handle),
  str_int(Edit2_Text, Second),
  Second>=1,
  Second<=3,
 
  append(First, Second, Array1, Array2, Array3, SymArray3Res),
  sym_list_to_string(SymArray3Res, SymArray3ResString),
  lbox_Clear(LBox_Handle),
  lbox_Add(LBox_Handle, SymArray3ResString),
	!.
	
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_eh(_Win,e_Control(idc_ok_sym,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Ошибка", "Ошибка валидации порядка списков"),	
  	!.
	
%END ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, idc_ok_sym _CtlInfo

/*
%BEGIN ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, idc_ok_num _CtlInfo
  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_eh(_Win,e_Control(idc_ok_num,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  LBox_Handle = win_GetCtlHandle(_Win,idc_lbox_res),
  
  Edit1_Handle = win_GetCtlHandle(_Win,idc_edit_num_first),
  Edit1_Text = win_GetText(Edit1_Handle),
  str_int(Edit1_Text, First),
  
  Edit2_Handle = win_GetCtlHandle(_Win,idc_edit_num_second),
  Edit2_Text =win_GetText(Edit2_Handle),
  str_int(Edit2_Text, Second),
  
  NumArray1 = [-5,4,0,-15,4],
  NumArray2 = [0,1,2],
  NumArray3 = [-1,-2,-3],
  
  append(First, Second, NumArray1, NumArray2, NumArray3, NumArray3Res),
  num_list_to_string(NumArray3Res, NumArray3ResString),
  lbox_Clear(LBox_Handle),
  lbox_Add(LBox_Handle, NumArray3ResString),
	!.
%END ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ, idc_ok_num _CtlInfo
*/

  dlg_объединение_трёх_списков_в_один_в_указанном_порядке_eh(_,_,_):-!,fail.

%END_DLG ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ

%BEGIN_DLG СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК
/**************************************************************************
	Creation and event handling for dialog: СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК
**************************************************************************/

constants

%BEGIN СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, CreateParms, 15:52:02-13.4.2019, Code automatically updated!
  dlg_сортировка_по_убыванию_методом_вставок_ResID = idd_sort
  dlg_сортировка_по_убыванию_методом_вставок_DlgType = wd_Modal
  dlg_сортировка_по_убыванию_методом_вставок_Help = idh_contents
%END СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, CreateParms

predicates

  dlg_сортировка_по_убыванию_методом_вставок_eh : EHANDLER
  dlg_сортировка_по_убыванию_методом_вставок_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_сортировка_по_убыванию_методом_вставок_update(DIALOG_VAL_LIST)
  
  insert_sort(symArray,symArray)
  insertEl(symbol,symArray,symArray) 

clauses

  insert_sort([],[]).
  insert_sort([X|Tail],Sorted_list) :- insert_sort(Tail,Sorted_Tail), insertEl(X,Sorted_Tail,Sorted_list).
 
  insertEl(X,[Y|Sorted_list],[Y|Sorted_list1]) :-  str_int(X,X_int), str_int(Y,Y_int), X_int < Y_int, !, insertEl(X,Sorted_list,Sorted_list1).
  insertEl(X, [Y|Sorted_list],[X,Y|Sorted_list]) :-  str_int(X,X_int), str_int(Y,Y_int), X_int > Y_int, !.
  
  insertEl(X,[Y|Sorted_list],[Y|Sorted_list1]) :-  X<Y, !, insertEl(X,Sorted_list,Sorted_list1).  
  insertEl(X,Sorted_list,[X|Sorted_list]).
  
  dlg_сортировка_по_убыванию_методом_вставок_Create(Parent):-

%MARK СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, new variables

	dialog_CreateModal(Parent,dlg_сортировка_по_убыванию_методом_вставок_ResID,"",
  		[
%BEGIN СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, ControlList, 15:52:02-13.4.2019, Code automatically updated!
		df(id_edit,listbox([],[0]),nopr),
		df(idc_array,editstr("",[]),nopr)
%END СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, ControlList
		],
		dlg_сортировка_по_убыванию_методом_вставок_eh,0,VALLIST,ANSWER),
	dlg_сортировка_по_убыванию_методом_вставок_handle_answer(ANSWER,VALLIST).

  dlg_сортировка_по_убыванию_методом_вставок_handle_answer(idc_ok,VALLIST):-!,
	dlg_сортировка_по_убыванию_методом_вставок_update(VALLIST).
  dlg_сортировка_по_убыванию_методом_вставок_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_сортировка_по_убыванию_методом_вставок_handle_answer(_,_):-
	errorexit().

  dlg_сортировка_по_убыванию_методом_вставок_update(_VALLIST):-
%BEGIN СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, Update controls, 15:52:02-13.4.2019, Code automatically updated!
	dialog_VLGetListBox(id_edit,_VALLIST,_ID_EDIT_ITEMLIST,_ID_EDIT_SELECT),
	_IDC_ARRAY_VALUE = dialog_VLGetstr(idc_array,_VALLIST),
%END СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, Update controls
	true.

%MARK СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, new events

%BEGIN СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, idc_ok _CtlInfo
  dlg_сортировка_по_убыванию_методом_вставок_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
    
  Array_Handle = win_GetCtlHandle(_Win,idc_array),
  Array_Text = win_GetText(Array_Handle), 
  string_to_list(Array_Text, Array),
  
  insert_sort(Array,SymArrayRes),
  sym_list_to_string(SymArrayRes,StringRes),
  
  LBox_Handle = win_GetCtlHandle(_Win,id_edit),
  lbox_Clear(LBox_Handle),
  lbox_Add(LBox_Handle, StringRes),
	!.
%END СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК, idc_ok _CtlInfo

  dlg_сортировка_по_убыванию_методом_вставок_eh(_,_,_):-!,fail.

%END_DLG СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК

