*&---------------------------------------------------------------------*
*& Report ZRMM5010
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
REPORT ZRMM5010 MESSAGE-ID ZMM01.

include ZRMM5010TOP. "Top Include
include ZRMM5010CLS. "ALV/TREE Class
include ZRMM5010SCR. "Selection Screen
include ZRMM5010O01. "Process Before Output
include ZRMM5010I01. "Process After Input
include ZRMM5010F01. "Business Logic Routine
include ZRMM5010F02. "ALV /Tree Logic Routine

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

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

  PERFORM GET_GR_DATA.

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
