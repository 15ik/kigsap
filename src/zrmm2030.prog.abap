*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report zrmm2030
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM2020
*& T-CODE              : ZRMM2020
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : REPORT
*& Description         : LTP 리스트
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
REPORT ZRMM2030 MESSAGE-ID ZCN00.

INCLUDE ZRMM2030TOP.
INCLUDE ZRMM2030CLS.
INCLUDE ZRMM2030SCR.
INCLUDE ZRMM2030O01.
INCLUDE ZRMM2030I01.
INCLUDE ZRMM2030F01.
INCLUDE ZRMM2030F02.

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
    DESCRIBE TABLE GT_DISP LINES DATA(GV_TCNT).
    GV_TCNT = GV_TCNT / 4.
    MESSAGE S011(ZMM01) WITH GV_TCNT. CALL SCREEN '0100'.
  ELSE.
    MESSAGE S005(ZMM01).
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
