*&---------------------------------------------------------------------*
*& Report ZOMM3992
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM3992
*& T-CODE              : ZOMM3992
*& Referenced Program  : N/A
*& Created by          : T0210128
*& Created On          : 2021.08.24
*& Type                : Online
*& Description         : PR 일괄 생성 프로그램
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N       T0210128      2021.08.24          최초생성
*&    U1      T0210053      2022.08.29          Tier2 번역및 수정 반영
*&---------------------------------------------------------------------*
REPORT ZOMM3992 MESSAGE-ID ZMM01.

INCLUDE ZOMM3992TOP.        "Top Include
INCLUDE ZOMM3992CLS.        "ALV/TREE Class
INCLUDE ZOMM3992SCR.        "Selection Screen
INCLUDE ZOMM3992O01.        "Process Before Output
INCLUDE ZOMM3992I01.        "Process After Input
INCLUDE ZOMM3992F01.        "Business Logic Routine
INCLUDE ZOMM3992F02.        "ALV /Tree Logic Routine

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

  PERFORM CREATE_OBJ.

  PERFORM CHECK_AUTHORITY.  "권한체크

  PERFORM UPLOAD_DATA.

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
