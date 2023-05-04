*&---------------------------------------------------------------------*
*& Report ZRMM3040
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM3040
*& T-CODE              : ZRMM3040
*& Referenced Program  : N/A
*& Created by          : T0210128
*& Created On          : 2021.06.17
*& Type                : Report
*& Description         : 단가 계약 현황(구매정보레코드) 조회 Report
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N       T0210128      2021.06.17          최초생성
*&    U1
*&---------------------------------------------------------------------*
REPORT ZRMM3040 MESSAGE-ID ZMM01.

INCLUDE ZRMM3040TOP.        "Top Include
INCLUDE ZRMM3040CLS.        "ALV/TREE Class
INCLUDE ZRMM3040SCR.        "Selection Screen
INCLUDE ZRMM3040O01.        "Process Before Output
INCLUDE ZRMM3040I01.        "Process After Input
INCLUDE ZRMM3040F01.        "Business Logic Routine
INCLUDE ZRMM3040F02.        "ALV /Tree Logic Routine

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
    RETURN.
  ENDIF.

  CALL SCREEN '0100'.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
