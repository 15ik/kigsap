*&---------------------------------------------------------------------*
*& Report ZOMM9910
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM9910
*& T-CODE              : ZOMM9910
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : ONLR
*& Description         : 배치 및 기초 재고 마이그레이션용 임시 프로그램
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZOMM9910 MESSAGE-ID ZCN00 LINE-SIZE 80.

include ZOMM9910TOP. "Top Include
include ZOMM9910CLS. "ALV/TREE Class
include ZOMM9910SCR. "Selection Screen
include ZOMM9910O01. "Process Before Output
include ZOMM9910I01. "Process After Input
include ZOMM9910F01. "Business Logic Routine
include ZOMM9910F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM CREATE_OBJ.
*
*  PERFORM CHECK_AUTHORITY.  "권한체크가 필요한 경우
*
  PERFORM GET_DATA.

*-
  CASE 'X'.
    WHEN P_RD1A.
      IF GT_BATCH[] IS NOT INITIAL.
        DESCRIBE TABLE GT_BATCH LINES GV_TCNT. MESSAGE S011(ZMM01) WITH GV_TCNT.
      ELSE.
        MESSAGE S005(ZMM01).  " &1 데이터가 존재하지 않습니다.
      ENDIF.
      CALL SCREEN '0100'.
    WHEN OTHERS.
      IF GT_DISP[] IS NOT INITIAL.
        DESCRIBE TABLE GT_DISP LINES GV_TCNT. MESSAGE S011(ZMM01) WITH GV_TCNT.
      ELSE.
        MESSAGE S005(ZMM01).
      ENDIF.
      CALL SCREEN '0200'.

  ENDCASE.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
