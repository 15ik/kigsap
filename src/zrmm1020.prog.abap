*&---------------------------------------------------------------------*
*& Report ZRMM1020
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM1020
*& T-CODE              : ZRMM1020
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Report
*& Description         : 외주 임가공 기준정보 점검
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
REPORT ZRMM1020 MESSAGE-ID ZCN00.

include ZRMM1020TOP. "Top Include
include ZRMM1020CLS. "ALV/TREE Class
include ZRMM1020SCR. "Selection Screen
include ZRMM1020O01. "Process Before Output
include ZRMM1020I01. "Process After Input
include ZRMM1020F01. "Business Logic Routine
include ZRMM1020F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  PERFORM CREATE_OBJ.
*
  PERFORM CHECK_AUTHORITY.  "권한체크가 필요한 경우

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

*-
  IF P_CHK2 IS INITIAL. "백그라운드처리 가 아닌경우

    IF GT_DISP[] IS NOT INITIAL.
      DESCRIBE TABLE GT_DISP LINES GV_TCNT. MESSAGE S011(ZMM01) WITH GV_TCNT.
      CALL SCREEN '0100'.
    ELSE.
      MESSAGE S005(ZMM01).
    ENDIF.

  ELSE.
    PERFORM BJOB_VER_INIT.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF NOT SY-BATCH IS INITIAL. EXIT. ENDIF.
