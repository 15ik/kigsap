*&---------------------------------------------------------------------*
*& Report ZRMM3050
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM3050
*& T-CODE              : ZRMM3050
*& Referenced Program  : N/A
*& Created by          :
*& Created On          :
*& Type                : Report
*& Description         : 국내/해외 발주 현황 조회 Report
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZRMM3050 MESSAGE-ID ZMM01.

include ZRMM3050TOP. "Top Include
include ZRMM3050CLS. "ALV/TREE Class
include ZRMM3050SCR. "Selection Screen
include ZRMM3050O01. "Process Before Output
include ZRMM3050I01. "Process After Input
include ZRMM3050F01. "Business Logic Routine
include ZRMM3050F02. "ALV /Tree Logic Routine

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

  PERFORM CHECK_AUTHORITY.  "권한체크

  PERFORM GET_DATA.

  IF ( P_RA = 'X' AND GT_DISP_RA IS INITIAL ) OR
     ( P_RB = 'X' AND GT_DISP_RB IS INITIAL ).
    MESSAGE S005(ZMM01) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL SCREEN '0100'.
*
*  IF P_MAIL IS NOT INITIAL.
*    PERFORM SEND_MAIL.
*  ELSE.

*  ENDIF.


*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
