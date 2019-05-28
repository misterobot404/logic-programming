/*****************************************************************************

		Copyright (c) My Company

 Project:  KR
 FileName: KR.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

include "kr.inc"
include "kr.con"
include "hlptopic.con"

%BEGIN_WIN Task Window
/***************************************************************************
		Event handling for Task Window
***************************************************************************/

predicates

  task_win_eh : EHANDLER

constants

%BEGIN Task Window, CreateParms, 15:24:59-23.4.2019, Code automatically updated!
  task_win_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings]
  task_win_Menu  = res_menu(idr_task_menu)
  task_win_Title = "KR"
  task_win_Help  = idh_contents
%END Task Window, CreateParms

clauses

%BEGIN Task Window, e_Create
  task_win_eh(_Win,e_Create(_),0):-!,
  %dlg_rules_Create(_Win), 
  dlg_rules_Create(_Win), dlg_game_Create(_Win), win_Destroy(_Win),
%BEGIN Task Window, InitControls, 15:24:59-23.4.2019, Code automatically updated!
%END Task Window, InitControls
%BEGIN Task Window, ToolbarCreate, 15:24:59-23.4.2019, Code automatically updated!
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
  	vpi_ShowHelp("kr.hlp"),
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
  	vpi_ShowHelpContext("kr.hlp",HelpTopic).

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
  vpi_Init(task_win_Flags,task_win_eh,task_win_Menu,"kr",task_win_Title).

%BEGIN_TLB Project toolbar, 15:24:59-23.4.2019, Code automatically updated!
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

%BEGIN_TLB Help line, 15:24:59-23.4.2019, Code automatically updated!
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


%BEGIN_DLG game
/**************************************************************************
	Creation and event handling for dialog: game
**************************************************************************/
domains
strList = string*

constants

%BEGIN game, CreateParms, 19:35:22-29.4.2019, Code automatically updated!
  dlg_game_ResID = idd_game
  dlg_game_DlgType = wd_Modal
  dlg_game_Help = idh_contents
%END game, CreateParms

predicates

  dlg_game_eh : EHANDLER
  dlg_game_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_game_update(DIALOG_VAL_LIST)
  
  ñheckWin(strList)
  ñheckWin(strList,integer)
  
  strList_to_string(strList, string)
  
  get_strList_and_checking_for_error(Window, strList)
  setStateTableFromStrList(Window, strList)
  
  add_to_strList(strList,string,strList)
  
  getVoidNumber(strList,integer)
  getValueOfNumber(strList,integer,string)
  getNumberOfValue(strList,integer,string)
  setValueByNumber(strList,integer,string,strList)
  
  getCurrentNotSortedÑell(strList,integer)
  getCurrentNotSortedÑell(strList,integer,integer)
  
  getSortingLine(strList,integer,integer)
  
  trueMove(strList,strList,integer,integer,integer,Window,integer) 
  moveLeft(strList,strList,strList)
  moveRight(strList,strList,strList)
  moveUp(strList,strList)
  moveDown(strList,strList)
  
  checkValueToCell(strList,integer, string)
  checkVertical(integer, integer)
  checkHorizontal(integer, integer)
  
clauses
  checkValueToCell([Head|_],Cell,Head):- Cell = 1,!.
  checkValueToCell([_|Tail],Cell,Value):- Cell > 1, CellTemp = Cell - 1, checkValueToCell(Tail,CellTemp,Value), !.

  ñheckWin(StrList):-ñheckWin(StrList,1),!.
  ñheckWin([Head|[]],Number):-Number=16, Head = "", dlg_Note("Ïîçäğàâëÿåì","Âû ïîáåäèëè!"),!.
  ñheckWin([Head|Tail],Number):-str_int(NumberStr,Number),Head = NumberStr, NumberTemp = Number + 1, ñheckWin(Tail,NumberTemp), !.
  ñheckWin(_,_).
  
  getVoidNumber([Head|Tail], Result):- Head <> "", getVoidNumber(Tail, TempResult), Result = TempResult + 1, !.
  getVoidNumber(_, TempResult):- TempResult = 1.

  getValueOfNumber([Head|_],Index,Head):- Index = 1,!.
  getValueOfNumber([_|Tail],Index,Result):- IndexTemp = Index - 1, getValueOfNumber(Tail,IndexTemp,Result),!.
  
  getNumberOfValue([Head|_],Index,Head):- Index = 1,!.
  getNumberOfValue([_|Tail],Index,Value):- getNumberOfValue(Tail,IndexTemp,Value),Index = IndexTemp + 1,!.
  getNumberOfValue([Head|_],Index, _):- Head = "", Index = 1,!.
  
  getCurrentNotSortedÑell(StrList,Result):-getCurrentNotSortedÑell(StrList,1,Result),!.
  getCurrentNotSortedÑell([Head|_],Index,Index):- str_int(StrIndex,Index), StrIndex <> Head,!.
  getCurrentNotSortedÑell([_|Tail],Index,Result):- IndexTemp = Index + 1, getCurrentNotSortedÑell(Tail,IndexTemp,Result),!.
  
  getSortingLine([Head|Tail],Index,Result):- IndexTemp = Index + 1, str_int(StrIndex,IndexTemp), StrIndex = Head, getSortingLine(Tail,IndexTemp,Result),!.
  getSortingLine(_,Index,Res):- Res = Index div 4 + 1,!.
  
  setValueByNumber([Head|Z], Counter, Str,[Head|Y]):- Counter <> 1, CounterTemp = Counter - 1, setValueByNumber(Z,CounterTemp,Str,Y),!.
  setValueByNumber([_|Z], _, Str,[Str|Z]).
  
  moveLeft([Head,X|Z],[X,Head|Z], StrList):- X = "", getNumberOfValue(StrList,Number,Head), Number <> 4, Number <> 8, Number <> 12, !.
  moveLeft([Head|Z],[Head|N],StrList):- moveLeft(Z,N,StrList), !.
  moveLeft(StrList,StrList,_).
  
  moveRight([Head,X|Z],[X,Head|Z], StrList):- Head = "", getNumberOfValue(StrList,Number,X), Number <> 5, Number <> 9, Number <> 13, !.
  moveRight([Head|Z],[Head|N], StrList):- moveRight(Z,N,StrList), !.
  moveRight(StrList,StrList, _).
  
  moveUp(StrList,StrList_Res):- 
  getVoidNumber(StrList, VoidNumber), 
  ValueNumberTemp = VoidNumber - 4, getValueOfNumber(StrList, ValueNumberTemp, Value), 
  setValueByNumber(StrList,VoidNumber,Value,StrList_Res1),
  setValueByNumber(StrList_Res1,ValueNumberTemp,"",StrList_Res),!.
  moveUp(StrList,StrList).
  
  moveDown(StrList,StrList_Res):- 
  getVoidNumber(StrList, VoidNumber), 
  ValueNumberTemp = VoidNumber + 4, getValueOfNumber(StrList, ValueNumberTemp, Value), 
  setValueByNumber(StrList,VoidNumber,Value,StrList_Res1),
  setValueByNumber(StrList_Res1,ValueNumberTemp,"",StrList_Res),!.
  moveDown(StrList,StrList).
  
  strList_to_string([], "").
  strList_to_string([H | T], S) :- T = [], strList_to_string(T, S1), !, concat(H, S1, S).
  strList_to_string([H | T], S) :-  H = "", strList_to_string(T, S1), !, concat("_", ", ", H_with_space), concat(H_with_space, S1, S).
  strList_to_string([H | T], S) :- strList_to_string(T, S1), !, concat(H, ", ", H_with_space), concat(H_with_space, S1, S).

  add_to_strList([],V,[V]):- str_int(V, V_int), !, V_int < 16, V_int > 0.
  add_to_strList([],V,[V]):-!, V = "".
  add_to_strList([H|T],V,[H|T1]):- H <> V, add_to_strList(T,V,T1).
  
  setStateTableFromStrList(Win, StrList):-
  Value1_Handle = win_GetCtlHandle(Win,idc_value1), getValueOfNumber(StrList, 1, Value1), win_SetText(Value1_Handle, Value1),
  Value2_Handle = win_GetCtlHandle(Win,idc_value2), getValueOfNumber(StrList, 2, Value2), win_SetText(Value2_Handle, Value2),
  Value3_Handle = win_GetCtlHandle(Win,idc_value3), getValueOfNumber(StrList, 3, Value3), win_SetText(Value3_Handle, Value3),
  Value4_Handle = win_GetCtlHandle(Win,idc_value4), getValueOfNumber(StrList, 4, Value4), win_SetText(Value4_Handle, Value4),
  Value5_Handle = win_GetCtlHandle(Win,idc_value5), getValueOfNumber(StrList, 5, Value5), win_SetText(Value5_Handle, Value5),
  Value6_Handle = win_GetCtlHandle(Win,idc_value6), getValueOfNumber(StrList, 6, Value6), win_SetText(Value6_Handle, Value6),
  Value7_Handle = win_GetCtlHandle(Win,idc_value7), getValueOfNumber(StrList, 7, Value7), win_SetText(Value7_Handle, Value7),
  Value8_Handle = win_GetCtlHandle(Win,idc_value8), getValueOfNumber(StrList, 8, Value8), win_SetText(Value8_Handle, Value8),
  Value9_Handle = win_GetCtlHandle(Win,idc_value9), getValueOfNumber(StrList, 9, Value9), win_SetText(Value9_Handle, Value9),
  Value10_Handle = win_GetCtlHandle(Win,idc_value10), getValueOfNumber(StrList, 10, Value10), win_SetText(Value10_Handle, Value10),
  Value11_Handle = win_GetCtlHandle(Win,idc_value11), getValueOfNumber(StrList, 11, Value11), win_SetText(Value11_Handle, Value11), 
  Value12_Handle = win_GetCtlHandle(Win,idc_value12), getValueOfNumber(StrList, 12, Value12), win_SetText(Value12_Handle, Value12),
  Value13_Handle = win_GetCtlHandle(Win,idc_value13), getValueOfNumber(StrList, 13, Value13), win_SetText(Value13_Handle, Value13),
  Value14_Handle = win_GetCtlHandle(Win,idc_value14), getValueOfNumber(StrList, 14, Value14), win_SetText(Value14_Handle, Value14),
  Value15_Handle = win_GetCtlHandle(Win,idc_value15), getValueOfNumber(StrList, 15, Value15), win_SetText(Value15_Handle, Value15),
  Value16_Handle = win_GetCtlHandle(Win,idc_value16), getValueOfNumber(StrList, 16, Value16), win_SetText(Value16_Handle, Value16).
  
  get_strList_and_checking_for_error(Win, StrList_Res):-
  Value1_Handle = win_GetCtlHandle(Win,idc_value1), Value1 = win_GetText(Value1_Handle), add_to_strList(_, Value1, StrList_Res1),
  Value2_Handle = win_GetCtlHandle(Win,idc_value2), Value2 = win_GetText(Value2_Handle), add_to_strList(StrList_Res1, Value2, StrList_Res2),
  Value3_Handle = win_GetCtlHandle(Win,idc_value3), Value3 = win_GetText(Value3_Handle), add_to_strList(StrList_Res2, Value3, StrList_Res3),
  Value4_Handle = win_GetCtlHandle(Win,idc_value4), Value4 = win_GetText(Value4_Handle), add_to_strList(StrList_Res3, Value4, StrList_Res4),
  Value5_Handle = win_GetCtlHandle(Win,idc_value5), Value5 = win_GetText(Value5_Handle), add_to_strList(StrList_Res4, Value5, StrList_Res5),
  Value6_Handle = win_GetCtlHandle(Win,idc_value6), Value6 = win_GetText(Value6_Handle), add_to_strList(StrList_Res5, Value6, StrList_Res6),
  Value7_Handle = win_GetCtlHandle(Win,idc_value7), Value7 = win_GetText(Value7_Handle), add_to_strList(StrList_Res6, Value7, StrList_Res7),
  Value8_Handle = win_GetCtlHandle(Win,idc_value8), Value8 = win_GetText(Value8_Handle), add_to_strList(StrList_Res7, Value8, StrList_Res8),
  Value9_Handle = win_GetCtlHandle(Win,idc_value9), Value9 = win_GetText(Value9_Handle), add_to_strList(StrList_Res8, Value9, StrList_Res9),
  Value10_Handle = win_GetCtlHandle(Win,idc_value10), Value10 = win_GetText(Value10_Handle), add_to_strList(StrList_Res9, Value10, StrList_Res10),
  Value11_Handle = win_GetCtlHandle(Win,idc_value11), Value11 = win_GetText(Value11_Handle), add_to_strList(StrList_Res10, Value11, StrList_Res11),
  Value12_Handle = win_GetCtlHandle(Win,idc_value12), Value12 = win_GetText(Value12_Handle), add_to_strList(StrList_Res11, Value12, StrList_Res12), 
  Value13_Handle = win_GetCtlHandle(Win,idc_value13), Value13 = win_GetText(Value13_Handle), add_to_strList(StrList_Res12, Value13, StrList_Res13),
  Value14_Handle = win_GetCtlHandle(Win,idc_value14), Value14 = win_GetText(Value14_Handle), add_to_strList(StrList_Res13, Value14, StrList_Res14), 
  Value15_Handle = win_GetCtlHandle(Win,idc_value15), Value15 = win_GetText(Value15_Handle), add_to_strList(StrList_Res14, Value15, StrList_Res15), 
  Value16_Handle = win_GetCtlHandle(Win,idc_value16), Value16 = win_GetText(Value16_Handle), add_to_strList(StrList_Res15, Value16, StrList_Res).
     
  dlg_game_Create(Parent):-

%MARK game, new variables

	dialog_CreateModal(Parent,dlg_game_ResID,"",
  		[
%BEGIN game, ControlList, 19:35:22-29.4.2019, Code automatically updated!
	
		df(idc_value1,editstr("",[]),nopr),
		df(idc_value2,editstr("",[]),nopr),
		df(idc_value3,editstr("",[]),nopr),
		df(idc_value4,editstr("",[]),nopr),
		df(idc_value5,editstr("",[]),nopr),
		df(idc_value6,editstr("",[]),nopr),
		df(idc_value7,editstr("",[]),nopr),
		df(idc_value8,editstr("",[]),nopr),
		df(idc_value9,editstr("",[]),nopr),
		df(idc_value10,editstr("",[]),nopr),
		df(idc_value11,editstr("",[]),nopr),
		df(idc_value12,editstr("",[]),nopr),
		df(idc_value13,editstr("",[]),nopr),
		df(idc_value14,editstr("",[]),nopr),
		df(idc_value15,editstr("",[]),nopr),
		df(idc_value16,editstr("",[]),nopr),	

		df(idc_debug,listbox([],[0]),nopr)
%END game, ControlList
		],
		dlg_game_eh,0,VALLIST,ANSWER),
	dlg_game_handle_answer(ANSWER,VALLIST).

  dlg_game_handle_answer(idc_ok,VALLIST):-!,
	dlg_game_update(VALLIST).
  dlg_game_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_game_handle_answer(_,_):-
	errorexit().

  dlg_game_update(_VALLIST):-
%BEGIN game, Update controls, 19:35:22-29.4.2019, Code automatically updated!
	_IDC_VALUE1_VALUE = dialog_VLGetstr(idc_value1,_VALLIST),
	_IDC_VALUE2_VALUE = dialog_VLGetstr(idc_value2,_VALLIST),
	_IDC_VALUE3_VALUE = dialog_VLGetstr(idc_value3,_VALLIST),
	_IDC_VALUE4_VALUE = dialog_VLGetstr(idc_value4,_VALLIST),
	_IDC_VALUE8_VALUE = dialog_VLGetstr(idc_value8,_VALLIST),
	_IDC_VALUE7_VALUE = dialog_VLGetstr(idc_value7,_VALLIST),
	_IDC_VALUE6_VALUE = dialog_VLGetstr(idc_value6,_VALLIST),
	_IDC_VALUE5_VALUE = dialog_VLGetstr(idc_value5,_VALLIST),
	_IDC_VALUE13_VALUE = dialog_VLGetstr(idc_value12,_VALLIST),
	_IDC_VALUE12_VALUE = dialog_VLGetstr(idc_value11,_VALLIST),
	_IDC_VALUE10_VALUE = dialog_VLGetstr(idc_value10,_VALLIST),
	_IDC_VALUE9_VALUE = dialog_VLGetstr(idc_value9,_VALLIST),
	_IDC_VALUE18_VALUE = dialog_VLGetstr(idc_value16,_VALLIST),
	_IDC_VALUE16_VALUE = dialog_VLGetstr(idc_value15,_VALLIST),
	_IDC_VALUE15_VALUE = dialog_VLGetstr(idc_value14,_VALLIST),
	_IDC_VALUE14_VALUE = dialog_VLGetstr(idc_value13,_VALLIST),
	dialog_VLGetListBox(idc_debug,_VALLIST,_IDC_DEBUG_ITEMLIST,_IDC_DEBUG_SELECT),
%END game, Update controls
	true.

%MARK game, new events

checkVertical(Cell_1, Cell_2):- Cell_1 mod 4 = Cell_2 mod 4.
checkHorizontal(Cell_1, Cell_2):- (Cell_1 - 1) div 4 = (Cell_2 - 1) div 4.

%BEGIN game, trueMove
 %Circle
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"14"),  checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"15"), checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Circle1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 12,
 checkValueToCell(StrList,10,"14"),  checkValueToCell(StrList,11,"10"),
 checkValueToCell(StrList,14,"15"), checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Circle2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 16,
 checkValueToCell(StrList,10,"14"),  checkValueToCell(StrList,11,"10"), checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"), checkValueToCell(StrList,15,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Circle3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 15,
 checkValueToCell(StrList,10,"14"),  checkValueToCell(StrList,11,"10"), checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"), checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Circle4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 14,
 checkValueToCell(StrList,10,"14"),  checkValueToCell(StrList,11,"10"), checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,15,"15"), checkValueToCell(StrList,16,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Circle5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 10,
 checkValueToCell(StrList,11,"10"), checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Circle6"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"10"), checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Circle7"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 12,
 checkValueToCell(StrList,10,"10"), checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Circle8"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 10,
 checkValueToCell(StrList,11,"11"),  checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"10"), checkValueToCell(StrList,15,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"11"),  checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"10"), checkValueToCell(StrList,15,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 15,
 checkValueToCell(StrList,10,"11"),  checkValueToCell(StrList,11,"12"),
 checkValueToCell(StrList,14,"10"), checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå13"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 16,
 checkValueToCell(StrList,10,"11"),  checkValueToCell(StrList,11,"12"),
 checkValueToCell(StrList,14,"10"), checkValueToCell(StrList,15,"14"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå14"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 not(checkVertical(VoidCell, 10)), 
 checkValueToCell(StrList,14,"10"), checkValueToCell(StrList,15,"14"), checkValueToCell(StrList,16,"15"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 10,
 checkValueToCell(StrList,11,"10"),  checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"12"), checkValueToCell(StrList,15,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"10"),  checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"12"), checkValueToCell(StrList,15,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 15,
 checkValueToCell(StrList,10,"10"),  checkValueToCell(StrList,11,"11"),
 checkValueToCell(StrList,14,"12"), checkValueToCell(StrList,16,"14"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíè23"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 14,
 checkValueToCell(StrList,10,"10"),  checkValueToCell(StrList,11,"11"),
 checkValueToCell(StrList,15,"12"), checkValueToCell(StrList,16,"14"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå24"),
 	!.
  %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 10,
 checkValueToCell(StrList,11,"12"),  checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"14"), checkValueToCell(StrList,15,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå31"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"12"),  checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"14"), checkValueToCell(StrList,15,"10"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå32"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 15,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"14"), 
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíè33"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 16,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"14"), 
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå34"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 12,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå35"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå36"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 10,
 checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå37"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 14,
 VoidCell = 14,
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå38"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 15,
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"12"), checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,16,"11"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå39"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 VoidCell = 10,
 checkValueToCell(StrList,11,"15"),  checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"), checkValueToCell(StrList,15,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå41"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 11,
 checkValueToCell(StrList,10,"15"),  checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"), checkValueToCell(StrList,15,"14"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå42"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 15,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"), checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå43"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 VoidCell = 14,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,15,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå44"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 VoidCell = 10, 
 checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå45"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå51"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"10"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå52"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå53"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,""),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå54"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå55"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå56"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå57"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå58"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"11"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå59"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå510"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå511"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå512"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå513"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå514"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå515"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå516"),
 	!.			
  %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå71"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"10"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå72"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"10"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå73"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"10"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå74"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 VoidCell = 10, 
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå75"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 VoidCell = 10, 
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå76"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå81"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå82"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå91"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"14"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå92"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå93"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå94"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå95"),
 	!.
 
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå96"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå97"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå98"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå99"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå910"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå911"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå912"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"10"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå913"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"10"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå914"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"10"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå915"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå916"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"10"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"10"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,""),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"14"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"14"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_6"),
 	!.	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"14"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_7"),
 	!.	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_8"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_9"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_10"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_11"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå10_12"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"11"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_6"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_7"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_8"),
 	!.		
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"14"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_9"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"14"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_10"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå11_-"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"14"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_6"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_7"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_8"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_9"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_9.2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_10"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_11"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_12"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"15"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå12_13"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå13_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå13_1"),
 	!.	
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"15"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå14_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"15"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå14_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"15"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå14_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"15"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå14_4"),
 	!.
  %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"14"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"14"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_6"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_7"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_8"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"11"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_9"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_10"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_11"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"11"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_12"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå15_13"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå16_1"),
 	!.	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå16_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"11"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå16_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå16_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå16_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå16_6"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"14"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå17_1"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"15"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå18_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"15"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå18_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå18_3"),
 	!.	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå18_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå18_5"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"15"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå19_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"15"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå19_2"),
 	!.	
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå20_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"12"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå20_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå20_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå20_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå20_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,""),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå20_6"),
 	!.
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"15"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"14"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå21_6"),
 	!.
 	
 %2x3
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"12"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22_2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"14"),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22_4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,""),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"14"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"14"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå22_6"),
 	!.

 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"11"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå61"),
 	!.
 	
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"12"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"14"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå61"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"14"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"15"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"12"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"11"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"14"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"10"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"12"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"15"),checkValueToCell(StrList,15,"10"),checkValueToCell(StrList,16,"12"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"15"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"15"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"15"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,""),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,"15"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"14"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,""),checkValueToCell(StrList,11,"14"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,"12"),checkValueToCell(StrList,16,"15"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"12"),checkValueToCell(StrList,15,"14"),checkValueToCell(StrList,16,"15"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,""),
 dlg_Note("Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,""),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"11"),checkValueToCell(StrList,16,"15"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"11"),checkValueToCell(StrList,11,"10"),checkValueToCell(StrList,12,""),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,"12"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"12"),checkValueToCell(StrList,11,"11"),checkValueToCell(StrList,12,"10"),
 checkValueToCell(StrList,14,"14"),checkValueToCell(StrList,15,"15"),checkValueToCell(StrList,16,""),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.		
 trueMove(StrList,StrList,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine > 2,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,13,"13"),
 checkValueToCell(StrList,10,"10"),checkValueToCell(StrList,11,"15"),checkValueToCell(StrList,12,"14"),
 checkValueToCell(StrList,14,"11"),checkValueToCell(StrList,15,""),checkValueToCell(StrList,16,"12"),
 dlg_Note("Óïñ","Ïğè òåêóùåé ñèòóàöèè ñòîëà ïîáåäíûõ õîäîâ íåò!"),
 lbox_Add(Lbox_Handle,"2x3 çàâåğøåíèå62"),
 	!.
 								
 %9 è 15
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 9,
 CellSortingValue = 10,
 VoidCell = 13,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà ôèíàë1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 13,
 CellSortingValue = 10,
 VoidCell = 9,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà ôèíàë2"),
 	!.
 
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = VoidCell+ 1,
 CellSortingValue = VoidCell + 2,
 VoidCell mod 4<>0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = VoidCell - 1,
 CellSortingValue = VoidCell + 1,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = VoidCell - 2,
 CellSortingValue = VoidCell - 1,
 VoidCell < 13, 
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 checkValueToCell(StrList,12,"9"),
 checkValueToCell(StrList,11,"13"), 
 VoidCell = 14,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà ââåğõ"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 checkValueToCell(StrList,12,"9"),
 checkValueToCell(StrList,11,"13"), 
 VoidCell mod 4 = 1,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà ïğàâî2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 CurrentNotSortedÑell=VoidCell - 4,
 VoidCell > 12,
 checkValueToCell(StrList,9,"9"),
 not(checkValueToCell(StrList,13,"13")),  
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà4 ëåâ"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = CellSortingValue - 1,
 VoidCell div 4 + 1 > 3,
 VoidCell - 1 <> 12,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 = 10,
 CellSortingValue = 11,
 VoidCell - 1 = 12,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 âñòàâêà4"),
 	!.
 	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3, 
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 CellSortingValue = 12,
 Ñell13 = VoidCell - 4,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñïğàâà?1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 CellSortingValue = 12,
 Ñell13 <> 13,
 Ñell13 = VoidCell - 5,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñïğàâà?2"),
 	!.
 	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 Ñell13 <> 11,
 CellSortingValue = 12,
 Ñell13 = VoidCell +1, C = Ñell13 + 1, checkHorizontal(C,VoidCell),
 VoidCell + 4 < 17,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà_ñïğàâà?2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 CellSortingValue = 12,
 Ñell13 = VoidCell - 3,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3, 
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 CellSortingValue = 12,
 Ñell13 = VoidCell - 4,
 VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 CellSortingValue = 12,
 Ñell13 <> 13,
 Ñell13 = VoidCell - 5,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà?4"),
 	!.
 	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 Ñell13 <> 10,
 CellSortingValue=12,
 not(checkHorizontal(VoidCell,Ñell13)),
 VoidCell mod 4 <> CellSortingValue mod 4,
 CellSortingValue + 1 > Ñell13,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 up"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 CellSortingValue=12,
 checkHorizontal(VoidCell,CellSortingValue),
 CellSortingValue + 1 > Ñell13,
 VoidCell mod 4 <> 1,
 moveLeft(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"9 and 13 left"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 VoidCell = 12,
 checkValueToCell(StrList,11,"9"),
 checkHorizontal(VoidCell,CellSortingValue),
 VoidCell mod 4 <> 1,
 moveLeft(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"9 and 13 left2"),
 	!.
 		
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 getNumberOfValue(StrList,ÑellNine,"9"),
 checkHorizontal(VoidCell,ÑellNine),
 not(checkVertical(Ñell13,ÑellNine)),
 ÑellNine < VoidCell,
 moveLeft(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"9 and 13_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,9,"13"),
 checkValueToCell(StrList,13,"9"),
 VoidCell = 14,
 moveLeft(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"9 and 13_1_+"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,12,"9"),
 checkValueToCell(StrList,15,"13"),
 VoidCell = 16,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13_3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,16,"9"),
 checkValueToCell(StrList,15,"13"),
 VoidCell = 12,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13_4"),
 	!.
 	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,16,"9"),
 checkValueToCell(StrList,15,"13"),
 VoidCell = 11,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13_5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,16,"9"),
 checkValueToCell(StrList,11,"13"),
 VoidCell = 15,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13_6"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,16,"9"),
 checkValueToCell(StrList,11,"13"),
 VoidCell = 14,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13_7"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,16,"9"),
 checkValueToCell(StrList,11,"13"),
 VoidCell = 10,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13_8"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 getNumberOfValue(StrList,ÑellNine,"9"),
 ÑellNine > Ñell13,
 checkHorizontal(Ñell13,ÑellNine),
 not(checkVertical(VoidCell,Ñell13)),
 not(checkVertical(VoidCell,ÑellNine)),
 VoidCell div 4 + 1 > 3,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13_11"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,12,"9"),
 checkValueToCell(StrList,10,"13"),
 VoidCell div 4 + 1 > 3,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13_22"),
 	!.
 		
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 getNumberOfValue(StrList,Ñell13,"13"),
 Ñell13 <> 13,
 getNumberOfValue(StrList,ÑellNine,"9"),
 9<>ÑellNine,
 checkHorizontal(VoidCell,ÑellNine),
 ÑellNine mod 4 <> 0,
 ÑellNine > VoidCell + 1,
 moveRight(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"9 and 13"),
 	!.	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 CellSortingValue = VoidCell +1, C = CellSortingValue+1, checkHorizontal(C,VoidCell),
 CellSortingValue div 4 + 1 < 4,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà1"), 
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 CellSortingValue=VoidCell - 3,
 VoidCell mod 4 <> 3,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà2"), 
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 VoidCell = 13,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,10,"13"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà3.2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3, 
 CellSortingValue=VoidCell - 4,
 VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 CellSortingValue=VoidCell - 5,
 VoidCell div 4 + 1  > 3, 
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 VoidCell = 14,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,10,"13"),
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâà4.2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 3,
 checkValueToCell(StrList,9,"9"),
 checkValueToCell(StrList,10,""),
 not(checkValueToCell(StrList,13,"13")),
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñïğàâàL"),
 	!.
 	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 VoidCell = 11,
 checkValueToCell(StrList,12,"9"),
 checkValueToCell(StrList,16,"13"),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïîäíÿòèå 13_1_|_v"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 VoidCell = 15,
 checkValueToCell(StrList,12,"9"),
 checkValueToCell(StrList,16,"13"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïîäíÿòèå 13_1_n"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 checkValueToCell(StrList,12,"9"),
 getNumberOfValue(StrList,Res13,"13"),
 not(checkVertical(VoidCell,Res13)),
 VoidCell - 1 <> Res13,
 VoidCell mod 4 > Res13 mod 4,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïîäíÿòèå 13_1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 3,
 checkValueToCell(StrList,12,"9"),
 getNumberOfValue(StrList,Res13,"13"),
 checkVertical(VoidCell,Res13),
 VoidCell div 4 + 1 = 3,
 VoidCell - 1 <> Res13,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïîäíÿòèå 13_2"),
 	!.
  % Óñòàíîâêà ïóñòîòû ñâåğõó ïğè ïîëå 2x4	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkValueToCell(StrList,9,"9")), not(checkValueToCell(StrList,13,"13")),
 VoidCell div 4 + 1 > 3,
 VoidCell <> 12,
 SortLine = 3,  C = VoidCell + 1, checkValueToCell(StrList,C,"9"), 
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñëåâà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkValueToCell(StrList,9,"9")), not(checkValueToCell(StrList,13,"13")),
 VoidCell div 4 + 1 > 3,  
 SortLine = 3,  C = VoidCell + 1, checkValueToCell(StrList,C,"13"), 
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñëåâà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 not(checkValueToCell(StrList,9,"9")), not(checkValueToCell(StrList,13,"13")),
 SortLine = 3,  C = VoidCell + 5, checkValueToCell(StrList,C,"9"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñëåâà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 not(checkValueToCell(StrList,9,"9")), not(checkValueToCell(StrList,13,"13")),
 CellSortingValue <> VoidCell + 4,
 SortLine = 3,  C = VoidCell + 5, checkValueToCell(StrList,C,"13"),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñëåâà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkValueToCell(StrList,9,"9")),
 VoidCell div 4 + 1 > 3, 
 SortLine = 3,  C = VoidCell - 1, checkValueToCell(StrList,C,"9"),
 VoidCell > 12, 
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñïğàâà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkValueToCell(StrList,9,"9")),
 VoidCell div 4 + 1 > 3,  
 VoidCell > 12, 
 SortLine = 3,  C = VoidCell - 1, checkValueToCell(StrList,C,"13"), 
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñïğàâà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-  
 not(checkValueToCell(StrList,9,"9")), not(checkValueToCell(StrList,13,"13")), 
 SortLine = 3,  C = VoidCell + 3, checkValueToCell(StrList,C,"9"),
 VoidCell mod 4 <> 1,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñïğàâà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkValueToCell(StrList,9,"9")), not(checkValueToCell(StrList,13,"13")),
 SortLine = 3,  C = VoidCell + 3, checkValueToCell(StrList,C,"13"),
 VoidCell + 4 <> CellSortingValue,
 VoidCell mod 4 <> 1,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"9 and 13 ïóñòîòà ñâåğõó_ñïğàâà2"),
 	!.
 	
 %final move
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CurrentNotSortedÑell = VoidCell, CellSortingValue=VoidCell + 1, VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"final move"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CurrentNotSortedÑell = VoidCell, CellSortingValue=VoidCell - 1, 
 moveLeft(StrList,StrList_Res, StrList),
 lbox_Add(Lbox_Handle,"final move"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CurrentNotSortedÑell = VoidCell, CellSortingValue= VoidCell + 4, CurrentNotSortedÑellOne = CurrentNotSortedÑell + 1, CurrentNotSortedÑellTwo = CurrentNotSortedÑell + 2, 
 getValueOfNumber(StrList,CurrentNotSortedÑellOne,StrResOne), str_int(StrResOne,ResOne), CurrentNotSortedÑellOne <> ResOne,
 getValueOfNumber(StrList,CurrentNotSortedÑellTwo,StrResTwo), str_int(StrResTwo,ResTwo), CurrentNotSortedÑellTwo <> ResTwo,  
 moveDown(StrList,StrList_Res), 
 lbox_Add(Lbox_Handle,"final move"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CurrentNotSortedÑell = VoidCell, CellSortingValue=VoidCell - 4,
 moveUp(StrList,StrList_Res), 
 lbox_Add(Lbox_Handle,"final move"),
 	!.
 %line completion
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell, VoidCell < 4,
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,2,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 3,
 getValueOfNumber(StrList,4,StrRes7), str_int(StrRes7,Res7), Res7 = 4,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 VoidCell < 4,
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,3,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 3,
 getValueOfNumber(StrList,4,StrRes7), str_int(StrRes7,Res7), Res7 = 4,
 getValueOfNumber(StrList,5,StrRes5), str_int(StrRes5,Res5), Res5 = 1,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell, VoidCell = 1,
 getValueOfNumber(StrList,2,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,3,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 3,
 getValueOfNumber(StrList,4,StrRes7), str_int(StrRes7,Res7), Res7 = 4,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî3"),
 	!.
 
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell,
 getValueOfNumber(StrList,2,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,3,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 3,
 getValueOfNumber(StrList,8,StrRes7), str_int(StrRes7,Res7), Res7 = 4,
 VoidCell < 4,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âïğàâî1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,3,StrRes3), str_int(StrRes3,Res3), Res3 = 3,
 getValueOfNumber(StrList,5,StrRes5), str_int(StrRes5,Res5), Res5 = 1,
 VoidCell < 4,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âïğàâî2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkVertical(VoidCell,8)), 
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,2,StrRes3), str_int(StrRes3,Res3), Res3 = 3,
 getValueOfNumber(StrList,5,StrRes5), str_int(StrRes5,Res5), Res5 = 1,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 4,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âïğàâî3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 checkVertical(VoidCell,8),
 getValueOfNumber(StrList,5,StrRes5), str_int(StrRes5,Res5), Res5 = 1,
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,2,StrRes3), str_int(StrRes3,Res3), Res3 = 3,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 4,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Óñòàíîâêà ïîñëåäíåãî ıëåìåíòà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 checkVertical(VoidCell,8),
 getValueOfNumber(StrList,5,StrRes5), str_int(StrRes5,Res5), Res5 = 1,
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,2,StrRes3), str_int(StrRes3,Res3), Res3 = 3,
 getValueOfNumber(StrList,4,StrRes8), str_int(StrRes8,Res8), Res8 = 4,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Óñòàíîâêà ïîñëåäíåãî ıëåìåíòà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 getValueOfNumber(StrList,5,StrRes5), str_int(StrRes5,Res5), Res5 = 1,
 getValueOfNumber(StrList,1,StrResOne), str_int(StrResOne,ResOne), ResOne = 2,
 getValueOfNumber(StrList,2,StrRes3), str_int(StrRes3,Res3), Res3 = 3,
 getValueOfNumber(StrList,4,StrRes8), str_int(StrRes8,Res8), VoidCell - Res8 = 3,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Óñòàíîâêà ïîñëåäíåãî ıëåìåíòà3"),
 	!.
 %line completion 2
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine <> 1,
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 5,
 getValueOfNumber(StrList,6,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 6,
 getValueOfNumber(StrList,7,StrRes7), str_int(StrRes7,Res7), Res7 = 7,
 VoidCell - 7 = CurrentNotSortedÑell,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"çàâåğøåíèå ñòğîêè1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 2, 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 5,
 getValueOfNumber(StrList,6,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 6,
 getValueOfNumber(StrList,7,StrRes7), str_int(StrRes7,Res7), Res7 = 7,
 getValueOfNumber(StrList,12,StrRes12), str_int(StrRes12,Res12), Res12 = 8,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"çàâåğøåíèå ñòğîêè2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine = 2, 
 CurrentNotSortedÑell = CellSortingValue - 4,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 5,
 getValueOfNumber(StrList,6,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 6,
 getValueOfNumber(StrList,7,StrRes7), str_int(StrRes7,Res7), Res7 = 7,
 getValueOfNumber(StrList,12,StrRes12), str_int(StrRes12,Res12), Res12 = 8,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"çàâåğøåíèå ñòğîêè3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine = 2,
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell,
 getValueOfNumber(StrList,6,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,7,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 7,
 getValueOfNumber(StrList,12,StrRes7), str_int(StrRes7,Res7), Res7 = 8,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âïğàâî1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
  SortLine = 2,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,7,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 7,
 getValueOfNumber(StrList,12,StrRes7), str_int(StrRes7,Res7), Res7 = 8,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âïğàâî2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkVertical(VoidCell,12)),
 SortLine = 2,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,6,StrResTwo), str_int(StrResTwo,ResTwo), ResTwo = 7,
 getValueOfNumber(StrList,12,StrRes7), str_int(StrRes7,Res7), Res7 = 8,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âïğàâî3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 checkVertical(VoidCell,12),
 getValueOfNumber(StrList,9,StrRes5), str_int(StrRes5,Res5), Res5 = 5,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,6,StrRes3), str_int(StrRes3,Res3), Res3 = 7,
 getValueOfNumber(StrList,12,StrRes8), str_int(StrRes8,Res8), Res8 = 8,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Óñòàíîâêà ïîñëåäíåãî ıëåìåíòà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 checkVertical(VoidCell,12),
 getValueOfNumber(StrList,9,StrRes5), str_int(StrRes5,Res5), Res5 = 5,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,6,StrRes3), str_int(StrRes3,Res3), Res3 = 7,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 8,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Óñòàíîâêà ïîñëåäíåãî ıëåìåíòà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 not(checkHorizontal(VoidCell,CurrentNotSortedÑell)),
 getValueOfNumber(StrList,9,StrRes5), str_int(StrRes5,Res5), Res5 = 5,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,6,StrRes3), str_int(StrRes3,Res3), Res3 = 7,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 8,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Óñòàíîâêà ïîñëåäíåãî ıëåìåíòà3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,6,StrRes3), str_int(StrRes3,Res3), Res3 = 7,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 8,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 getValueOfNumber(StrList,9,StrResOne), str_int(StrResOne,ResOne), ResOne = 5,
 getValueOfNumber(StrList,5,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,7,StrRes3), str_int(StrRes3,Res3), Res3 = 7,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 8,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell, VoidCell = 5,
 getValueOfNumber(StrList,9,StrResOne), str_int(StrResOne,ResOne), ResOne = 5,
 getValueOfNumber(StrList,6,StrResOne), str_int(StrResOne,ResOne), ResOne = 6,
 getValueOfNumber(StrList,7,StrRes3), str_int(StrRes3,Res3), Res3 = 7,
 getValueOfNumber(StrList,8,StrRes8), str_int(StrRes8,Res8), Res8 = 8,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 SortLine < 3,
 VoidCell = CurrentNotSortedÑell,
 VoidCell mod 4 <> 0,
 not(checkHorizontal(VoidCell,CellSortingValue)),
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âíèç00"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 SortLine < 3,
 VoidCell = CurrentNotSortedÑell + 4,
 CellSortingValue = CurrentNotSortedÑell + 3,
 VoidCell mod 4 <> 1,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"Ñìåùåíèå âëåâî00"),
 	!.
 %bubble CellSortingValue move
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell,Lbox_Handle, SortLine):-
 checkVertical(VoidCell,CellSortingValue), VoidCell < CellSortingValue,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"bubble move1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 checkHorizontal(VoidCell,CellSortingValue), VoidCell < CellSortingValue, checkHorizontal(CellSortingValue,CurrentNotSortedÑell),
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move2"),
 	!.
 %move VoidCell on void CellSortingValue
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell=VoidCell, CellSortingValue - 3 = CurrentNotSortedÑell, SortLine = 1, 
 VoidCell + 4 < 17,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü çíà÷åíèå íà ìåñòî ïóñòîòû1"),
 	!.
 %move VoidCell on top CellSortingValue
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CellSortingValue = VoidCell +1, C = CellSortingValue+1, checkHorizontal(C,VoidCell),
 VoidCell + 4 < 17,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñâåğõó1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CellSortingValue=VoidCell - 3, CellSortingValue div 4 + 1 <> SortLine, VoidCell mod 4 + 1 <> 4,
 VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñâåğõó2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CellSortingValue=VoidCell - 4, CellSortingValue div 4 + 1 <> SortLine, VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñâåğõó3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CellSortingValue=VoidCell - 5, CellSortingValue div 4 + 1 <> SortLine,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñâåğõó4"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CellSortingValue mod 4 <> 0,
 CellSortingValue - 4 > CurrentNotSortedÑell,
 CellSortingValue=VoidCell - 1, CellSortingValue div 4 + 1 > SortLine, CellSortingValue - 3 <> CurrentNotSortedÑell, VoidCell - 4 <> CurrentNotSortedÑell,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñâåğõó5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CellSortingValue=VoidCell + 3, CurrentNotSortedÑell <> VoidCell, CurrentNotSortedÑell mod 4 <> 0, CellSortingValue div 4 + 1 > SortLine,
 VoidCell mod 4 <> 1,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñâåğõó6"),
 	!.
 %line completion
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4, VoidCell-1<>CurrentNotSortedÑell,
 checkValueToCell(StrList, 8, "4"),
 VoidCell div 4 < 2,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"çàâåğøåíèå ñòğîêè1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CurrentNotSortedÑell = CellSortingValue - 4,
 not(checkHorizontal(VoidCell,CurrentNotSortedÑell)),
 VoidCell div 4 < 2,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"çàâåğøåíèå ñòğîêè2"),
 	!.
 %move VoidCell on left CellSortingValue
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CellSortingValue = VoidCell - 1, C = CellSortingValue - 1, checkHorizontal(C,VoidCell), CurrentNotSortedÑell mod 4 <  CellSortingValue mod 4,  
 CurrentNotSortedÑell mod 4 <> 0,
 SortLine<>4,
 SortLine < 3,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñëåâà1"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CellSortingValue=VoidCell - 5,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñëåâà2"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):- 
 CellSortingValue=VoidCell - 4,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñëåâà3"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CellSortingValue=VoidCell - 3,
 VoidCell mod 4 <> 0,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"ïîñòàâèòü ïóñòîòó ñëåâà4"),
 	!.
 %bubble CellSortingValue move
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 = 0,
 VoidCell = CurrentNotSortedÑell,
 SortLine < 3,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"bubble move âñòàâêà âíèç"),!.
 
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 CellSortingValue mod 4 = 0,
 VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move3_r"),!.
 
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 = 0,
 CellSortingValue mod 4 = 1,
 VoidCell - CellSortingValue = 3,
 CurrentNotSortedÑell + 1 < VoidCell,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move3_l+_"),
 	!.

 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 <> CellSortingValue mod 4,
 VoidCell div 4 > CellSortingValue div 4,
 VoidCell div 4 <> SortLine,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"bubble move3_d"),
 	!.	
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 > CellSortingValue mod 4,
 CurrentNotSortedÑell + 1 < VoidCell,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move3_l"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 = 0, 
 CellSortingValue  = VoidCell - 1,
 CurrentNotSortedÑell + 1 < VoidCell,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move3_ll"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 > CellSortingValue mod 4,
 CurrentNotSortedÑell + 1 = VoidCell,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"bubble move3_d"),
 	!.
 %bubble CellSortingValue move
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 < CellSortingValue mod 4,
 VoidCell mod 4 <> 0,
 moveRight(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move4"),
 	!.
 %bubble CellSortingValue move
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 > CellSortingValue mod 4,
 CurrentNotSortedÑell = VoidCell,
 moveDown(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"bubble move5"),
 	!.
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell > CurrentNotSortedÑell,
 VoidCell mod 4 = 0, 
 CellSortingValue mod 4 <> 0,
 VoidCell div 4 <= CellSortingValue div 4 + 1,
 moveLeft(StrList,StrList_Res,StrList),
 lbox_Add(Lbox_Handle,"bubble move3_lll"),
 	!.		
 trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue, VoidCell, Lbox_Handle, SortLine):-
 VoidCell mod 4 = CellSortingValue mod 4,
 VoidCell div 4 > CellSortingValue div 4,
 VoidCell - 4 <> CellSortingValue,
 moveUp(StrList,StrList_Res),
 lbox_Add(Lbox_Handle,"bubble move3_u"),
 	!.	
 trueMove(StrList,StrList,_,_,_,_,_).
 
%END game, trueMove

%BEGIN game, idc_nextmove _CtlInfo
  dlg_game_eh(_Win,e_Control(idc_nextmove,_CtrlType,_CtrlWin,_CtlInfo),0):-
  Lbox_Handle = win_GetCtlHandle(_Win,idc_debug),
  lbox_Clear(Lbox_Handle),
  get_strList_and_checking_for_error(_Win, StrList),
  getCurrentNotSortedÑell(StrList,CurrentNotSortedÑell),
  str_int(StrCurrentNotSortedÑell,CurrentNotSortedÑell),
  getNumberOfValue(StrList,CellSortingValue,StrCurrentNotSortedÑell),
  getVoidNumber(StrList, VoidCell),
  getSortingLine(StrList,0,SortLine),
  trueMove(StrList,StrList_Res,CurrentNotSortedÑell,CellSortingValue,VoidCell,Lbox_Handle,SortLine),
  setStateTableFromStrList(_Win, StrList_Res),
  ñheckWin(StrList_Res),
  
  getCurrentNotSortedÑell(StrList_Res,CurrentNotSortedÑell2),
  str_int(StrCurrentNotSortedÑell2,CurrentNotSortedÑell2),
  concat("Òåêóùàÿ íåîòñîğòèğîâàííàÿ ÿ÷åéêà: ", StrCurrentNotSortedÑell2, Con1),
  lbox_Add(Lbox_Handle,Con1),
  getSortingLine(StrList,0,SortLine),
  str_int(StrSortLine,SortLine),
  concat("Òåêóùàÿ ñîğòèğóåìàÿ ñòğîêà: ", StrSortLine, Con4),
  lbox_Add(Lbox_Handle,Con4),
  getNumberOfValue(StrList_Res,CellSortingValue2,StrCurrentNotSortedÑell2),
  str_int(StrCellSortingValue2,CellSortingValue2),
  concat("Íóæíûé ıëåìåíò â ÿ÷åéêå: ", StrCellSortingValue2, FindCon),
  lbox_Add(Lbox_Handle,FindCon),
  getVoidNumber(StrList_Res, VoidCell2),
  str_int(StrVoidCell2,VoidCell2),
  concat("Ïóñòàÿ ÿ÷åéêà: ", StrVoidCell2, VoidCellCon),
  lbox_Add(Lbox_Handle,VoidCellCon),
	!.
  dlg_game_eh(_Win,e_Control(idc_nextmove,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Îøèáêà", "Ïğîâåğüòå ñîñòîÿíèå ñòîëà!"),
	!.
%END game, idc_nextmove _CtlInfo

%BEGIN game, idc_table_clean _CtlInfo
  dlg_game_eh(_Win,e_Control(idc_table_clean,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  Value1_Handle = win_GetCtlHandle(_Win,idc_value1), win_SetText(Value1_Handle, ""),
  Value2_Handle = win_GetCtlHandle(_Win,idc_value2), win_SetText(Value2_Handle, ""),
  Value3_Handle = win_GetCtlHandle(_Win,idc_value3), win_SetText(Value3_Handle, ""),
  Value4_Handle = win_GetCtlHandle(_Win,idc_value4), win_SetText(Value4_Handle, ""),
  Value5_Handle = win_GetCtlHandle(_Win,idc_value5), win_SetText(Value5_Handle, ""),
  Value6_Handle = win_GetCtlHandle(_Win,idc_value6), win_SetText(Value6_Handle, ""),
  Value7_Handle = win_GetCtlHandle(_Win,idc_value7), win_SetText(Value7_Handle, ""),
  Value8_Handle = win_GetCtlHandle(_Win,idc_value8), win_SetText(Value8_Handle, ""),
  Value9_Handle = win_GetCtlHandle(_Win,idc_value9), win_SetText(Value9_Handle, ""),
  Value10_Handle = win_GetCtlHandle(_Win,idc_value10), win_SetText(Value10_Handle, ""),
  Value11_Handle = win_GetCtlHandle(_Win,idc_value11), win_SetText(Value11_Handle, ""), 
  Value12_Handle = win_GetCtlHandle(_Win,idc_value12), win_SetText(Value12_Handle, ""),
  Value13_Handle = win_GetCtlHandle(_Win,idc_value13), win_SetText(Value13_Handle, ""),
  Value14_Handle = win_GetCtlHandle(_Win,idc_value14), win_SetText(Value14_Handle, ""),
  Value15_Handle = win_GetCtlHandle(_Win,idc_value15), win_SetText(Value15_Handle, ""),
  Value16_Handle = win_GetCtlHandle(_Win,idc_value16), win_SetText(Value16_Handle, ""),
	!.
%END game, idc_table_clean _CtlInfo

%BEGIN game, idc_button_right _CtlInfo
  dlg_game_eh(_Win,e_Control(idc_button_right,_CtrlType,_CtrlWin,_CtlInfo),0):-
  get_strList_and_checking_for_error(_Win, StrList),
  moveRight(StrList, StrList_Res, StrList),
  setStateTableFromStrList(_Win, StrList_Res),  
  ñheckWin(StrList_Res),
	!.
  dlg_game_eh(_Win,e_Control(idc_button_right,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Îøèáêà", "Ïğîâåğüòå ñîñòîÿíèå ñòîëà!"),
	!.
%END game, idc_button_right _CtlInfo

%BEGIN game, idc_button_left _CtlInfo
  dlg_game_eh(_Win,e_Control(idc_button_left,_CtrlType,_CtrlWin,_CtlInfo),0):-
  get_strList_and_checking_for_error(_Win, StrList),
  moveLeft(StrList, StrList_Res, StrList),
  setStateTableFromStrList(_Win, StrList_Res),   
  ñheckWin(StrList_Res),
	!.
  dlg_game_eh(_Win,e_Control(idc_button_left,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Îøèáêà", "Ïğîâåğüòå ñîñòîÿíèå ñòîëà!"),
	!.
%END game, idc_button_left _CtlInfo

%BEGIN game, idc_button_down _CtlInfo
  dlg_game_eh(_Win,e_Control(idc_button_down,_CtrlType,_CtrlWin,_CtlInfo),0):-
  get_strList_and_checking_for_error(_Win, StrList),
  moveDown(StrList, StrList_Res),
  setStateTableFromStrList(_Win, StrList_Res),  
  ñheckWin(StrList_Res),
	!.
  dlg_game_eh(_Win,e_Control(idc_button_down,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Îøèáêà", "Ïğîâåğüòå ñîñòîÿíèå ñòîëà!"),
	!.
%END game, idc_button_down _CtlInfo

%BEGIN game, idc_button_up _CtlInfo
dlg_game_eh(_Win,e_Control(idc_button_up,_CtrlType,_CtrlWin,_CtlInfo),0):-
  get_strList_and_checking_for_error(_Win, StrList),
  moveUp(StrList, StrList_Res),
  setStateTableFromStrList(_Win, StrList_Res),  
  ñheckWin(StrList_Res),
	!.
  dlg_game_eh(_Win,e_Control(idc_button_up,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  dlg_Error("Îøèáêà", "Ïğîâåğüòå ñîñòîÿíèå ñòîëà!"),
	!.
%END game, idc_button_up _CtlInfo

%BEGIN game, idc_help _CtlInfo
  dlg_game_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_rules_Create(_Win),
	!.
%END game, idc_help _CtlInfo

  dlg_game_eh(_,_,_):-!,fail.

%END_DLG game

%BEGIN_DLG rules
/**************************************************************************
	Creation and event handling for dialog: rules
**************************************************************************/

constants

%BEGIN rules, CreateParms, 15:24:18-3.5.2019, Code automatically updated!
  dlg_rules_ResID = idd_rules
  dlg_rules_DlgType = wd_Modal
  dlg_rules_Help = idh_contents
%END rules, CreateParms

predicates

  dlg_rules_eh : EHANDLER
  dlg_rules_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_rules_update(DIALOG_VAL_LIST)

clauses

  dlg_rules_Create(Parent):-

%MARK rules, new variables

	dialog_CreateModal(Parent,dlg_rules_ResID,"",
  		[
%BEGIN rules, ControlList, 15:24:18-3.5.2019, Code automatically updated!
%END rules, ControlList
		],
		dlg_rules_eh,0,VALLIST,ANSWER),
	dlg_rules_handle_answer(ANSWER,VALLIST).

  dlg_rules_handle_answer(idc_ok,VALLIST):-!,
	dlg_rules_update(VALLIST).
  dlg_rules_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_rules_handle_answer(_,_):-
	errorexit().

  dlg_rules_update(_VALLIST):-
%BEGIN rules, Update controls, 15:24:18-3.5.2019, Code automatically updated!
%END rules, Update controls
	true.

%MARK rules, new events

  dlg_rules_eh(_,_,_):-!,fail.

%END_DLG rules

