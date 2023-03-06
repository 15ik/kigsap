*&---------------------------------------------------------------------*
*& Report ZRMM3020
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM3020
*& T-CODE              : ZRMM3020
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Report
*& Description         : 구매 실적 조회 Report
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& SEQ   SR번호      변경자     변경일자      변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZRMM3020 MESSAGE-ID ZMM01.

include ZRMM3020TOP. "Top Include
include ZRMM3020CLS. "ALV/TREE Class
include ZRMM3020SCR. "Selection Screen
include ZRMM3020O01. "Process Before Output
include ZRMM3020I01. "Process After Input
include ZRMM3020F01. "Business Logic Routine
include ZRMM3020F02. "ALV /Tree Logic Routine

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
