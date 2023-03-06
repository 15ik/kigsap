*&---------------------------------------------------------------------*
*& Report ZRMM3030
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRMM3030 MESSAGE-ID ZMM01.

include ZRMM3030TOP. "Top Include
include ZRMM3030CLS. "ALV/TREE Class
include ZRMM3030SCR. "Selection Screen
include ZRMM3030O01. "Process Before Output
include ZRMM3030I01. "Process After Input
include ZRMM3030F01. "Business Logic Routine
include ZRMM3030F02. "ALV /Tree Logic Routine

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

*  PERFORM CHECK_AUTHORITY.  "권한체크
*
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
