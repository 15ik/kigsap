*&---------------------------------------------------------------------*
*& Report ZOMM4990
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM4990
*& T-CODE              : ZOMM4990
*& Referenced Program  : N/A
*& Created by          : T0210051
*& Created On          : 2021.07.23
*& Type                : ONLR
*& Description         : 배치 및 기초 재고 마이그레이션용 임시 프로그램
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N       T0210051      2021.07.23          최초생성
*&    U1                                        변경내용 기재1
*&    U2                                        변경내용 기재2
*&    U3      T0220140      2022.10.18          변경내용 기재2
*&---------------------------------------------------------------------*
REPORT ZOMM4990 MESSAGE-ID ZCN00 LINE-SIZE 80.

INCLUDE ZOMM4990TOP.        "Top Include
INCLUDE ZOMM4990CLS.        "ALV/TREE Class
INCLUDE ZOMM4990SCR.        "Selection Screen
INCLUDE ZOMM4990O01.        "Process Before Output
INCLUDE ZOMM4990I01.        "Process After Input
INCLUDE ZOMM4990F01.        "Business Logic Routine
INCLUDE ZOMM4990F02.        "ALV /Tree Logic Routine

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

  PERFORM CHECK_AUTHORITY.  "권한체크가 필요한 경우

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
