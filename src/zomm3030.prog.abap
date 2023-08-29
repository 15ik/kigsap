*&---------------------------------------------------------------------*
*& Report ZOMM3030
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM3030
*& T-CODE              : ZOMM3030
*& Referenced Program  : N/A
*& Created by          : T0210128
*& Created On          : 2021.04.05
*& Type                : Online
*& Description         : 구매(계약)요청에 대한 결재 상신 및 현황 조회
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N       T0210128      2021.04.05          최초생성
*&    U1(T2)  T0210053      2022.09.26          결재상태 번역
*&---------------------------------------------------------------------*
REPORT ZOMM3030 MESSAGE-ID ZMM01.

INCLUDE ZOMM3030TOP.        "Top Include
INCLUDE ZOMM3030CLS.        "ALV/TREE Class
INCLUDE ZOMM3030SCR.        "Selection Screen
INCLUDE ZOMM3030O01.        "Process Before Output
INCLUDE ZOMM3030I01.        "Process After Input
INCLUDE ZOMM3030F01.        "Business Logic Routine
INCLUDE ZOMM3030F02.        "ALV /Tree Logic Routine

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
  ELSE.
    IF P_APPR = 'X'.
      GT_DISP_APV = GT_DISP.
      PERFORM SUBMIT_DRAFT.
    ELSE.
      CALL SCREEN '0100'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
