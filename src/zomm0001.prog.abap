*&---------------------------------------------------------------------*
*& Report ZOMM0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZOMM0001 MESSAGE-ID zmm00.

include zomm0001top. "Top Include
include zomm0001cls. "ALV/TREE Class
include zomm0001scr. "Selection Screen
include zomm0001o01. "Process Before Output
include zomm0001i01. "Process After Input
include zomm0001f01. "Business Logic Routine
include zomm0001f02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF NOT sy-batch IS INITIAL. EXIT. ENDIF.

  PERFORM check_authority.  "권한체크가 필요한 경우

  CALL SCREEN '0100'.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
