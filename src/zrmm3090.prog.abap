*&---------------------------------------------------------------------*
*& Report ZRMM3090
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM3090
*& T-CODE              : ZRMM3090
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : REP
*& Description         : 납품일별 입고 현황
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
REPORT ZRMM3090 MESSAGE-ID ZMM01.

include ZRMM3090TOP. "Top Include
include ZRMM3090CLS. "ALV/TREE Class
include ZRMM3090SCR. "Selection Screen
include ZRMM3090O01. "Process Before Output
include ZRMM3090I01. "Process After Input
include ZRMM3090F01. "Business Logic Routine
include ZRMM3090F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF NOT SY-BATCH IS INITIAL. EXIT. ENDIF.

*  PERFORM CHECK_AUTHORITY.  "U1 - 권한체크 추가

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

  IF GT_DISP[] IS NOT INITIAL.
    DESCRIBE TABLE GT_DISP LINES DATA(GV_TCNT). MESSAGE S011(ZMM01) WITH GV_TCNT.
    CALL SCREEN '0100'.
  ELSE.
    MESSAGE S005(ZMM01).
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
