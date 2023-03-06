*&---------------------------------------------------------------------*
*& Report ZRMM3010
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM3010
*& T-CODE              : ZRMM3010
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Report
*& Description         : 구매요청 진행 현황 조회 Report
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZRMM3010 MESSAGE-ID ZMM01.

include ZRMM3010TOP. "Top Include
include ZRMM3010CLS. "ALV/TREE Class
include ZRMM3010SCR. "Selection Screen
include ZRMM3010O01. "Process Before Output
include ZRMM3010I01. "Process After Input
include ZRMM3010F01. "Business Logic Routine
include ZRMM3010F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.

  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF SY-BATCH IS NOT INITIAL. EXIT. ENDIF.

*  PERFORM CREATE_OBJ.

  PERFORM CHECK_AUTHORITY.  "권한체크

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

  IF GT_DISP IS INITIAL.
    MESSAGE S005(ZMM01) DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN '0100'.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
