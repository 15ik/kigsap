*&---------------------------------------------------------------------*
*& Report ZRMM2130
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM2130
*& T-CODE              : ZRMM2130
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Report
*& Description         : 제조공장별 예상재고 조회
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
REPORT ZRMM2130 MESSAGE-ID ZCN00.

include ZRMM2130TOP. "Top Include
include ZRMM2130CLS. "ALV/TREE Class
include ZRMM2130SCR. "Selection Screen
include ZRMM2130O01. "Process Before Output
include ZRMM2130I01. "Process After Input
include ZRMM2130F01. "Business Logic Routine
include ZRMM2130F02. "ALV /Tree Logic Routine

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

  PERFORM CREATE_OBJ.

  PERFORM CHECK_AUTHORITY.  "권한체크가 필요한 경우

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

  IF GT_DISP[] IS NOT INITIAL.
    DESCRIBE TABLE GT_DISP LINES GV_TCNT.
    IF P_RD1C = 'X'.
       GV_TCNT = GV_TCNT / 9.
    ENDIF.
    MESSAGE S011(ZMM01) WITH GV_TCNT.
    CALL SCREEN '0100'.
  ELSE.
    MESSAGE S005(ZMM01).
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
