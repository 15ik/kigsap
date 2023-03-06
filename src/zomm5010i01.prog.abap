*&---------------------------------------------------------------------*
*& Include          ZOMM5010I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.

  CASE OK_CODE.
    WHEN 'DYN'.
      PERFORM SWITCH_NAV_CONT.

    WHEN 'MEV0020BUTTON'.
      IF GS_EXPAND-H = ABAP_TRUE.
        GS_EXPAND-H  = ABAP_FALSE.
      ELSE.
        GS_EXPAND-H  = ABAP_TRUE.
      ENDIF.

    WHEN 'MEV0021BUTTON'.
      IF GS_EXPAND-I = ABAP_TRUE.
        GS_EXPAND-I  = ABAP_FALSE.
      ELSE.
        GS_EXPAND-I  = ABAP_TRUE.
      ENDIF.

    WHEN 'MEV0022BUTTON'.
      IF P_RP4 EQ ABAP_TRUE.
        IF GS_EXPAND-D = ABAP_TRUE.
          GS_EXPAND-D  = ABAP_FALSE.
        ELSE.
          GS_EXPAND-D  = ABAP_TRUE.
        ENDIF.
      ENDIF.

    WHEN 'FTAX'.
      PERFORM CALCULATE_TAX USING SPACE.
      PERFORM CONVERT_TOTAL_AMOUNT_TO_KRW USING SPACE. "KRW 환산금액

    WHEN 'FTAX2'.  "세금계산 체크박스
      CHECK GS_SCR100-XMWST EQ ABAP_TRUE.
      PERFORM CALCULATE_TAX USING ABAP_TRUE.
      PERFORM CONVERT_TOTAL_AMOUNT_TO_KRW USING SPACE. "KRW 환산금액

    WHEN 'FZLSCH'.  "지급방법
      PERFORM SET_NETDT_WITH_ZLSCH USING GS_SCR100-ZFBDT.

    WHEN 'INVOICE'.  "송장처리
      PERFORM CREATE_INCOMINGINVOICE.

    WHEN 'MESSAGE'.      "송장처리 메시지
      PERFORM DISPLAY_LOG.

    WHEN 'CALCAMT'.
      PERFORM CALCULATE_AMOUNT.

    WHEN 'REFRESH'.  "데이터 Refresh
      PERFORM REFRESH_ALL.

    WHEN 'REFRESH_HEAD'.
      PERFORM CLEAR_SCREEN_AMOUNT.

    WHEN 'ISSUE'.    "정발행 전표 Mapping
      CALL TRANSACTION 'ZDTV3_AP_P01'.

*>확인 필: 역발행 전표 Mapping 을위한 T-CODE(ZSBDTIAP01) 미존재
    WHEN 'ISSUE_R'.  "역발행 전표 Mapping
*    CALL TRANSACTION 'ZSBDTIAP01'.
      MESSAGE I000 WITH '적용 예정'(Z15).

    WHEN 'FRATE'.    "환율적용
      IF GS_SCR100-WWERT IS INITIAL.
        "환율 적용일자를 입력하세요.
        MESSAGE S017 WITH TEXT-M25.
        EXIT.
      ENDIF.

      PERFORM CALCULATE_EXCHANGE_RATE.
    WHEN 'CHANGE'.      "입고금액변경
      CLEAR GS_SCR100.
      GS_SCR100 = GS_SCR100_BACK.

    WHEN 'CLEAR'.       "상단 금액초기화
      PERFORM CLEAR_SCREEN_AMOUNT.

    WHEN 'CHG_KALSK'.   "공급업체 정발행/역발행 변경
      PERFORM CHANGE_LIFNR_LIPRE.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.

*> 확인 필 : 임시 로직 - 전자세금계산서 생성.
    WHEN 'CRT_EACC'.
*      PERFORM CREATE_EACC_DATA.
    WHEN 'DWPAY_LOG'.
      PERFORM DISPLAY_DOWNPAYMENT_LIST.
    WHEN OTHERS.
  ENDCASE.

  CLEAR OK_CODE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  SET_NETDT  INPUT
*&---------------------------------------------------------------------*
*       기산일 계산, 지급방법 체크
*----------------------------------------------------------------------*
MODULE SET_NETDT INPUT.

  PERFORM SET_ADJUST_NETDT.

ENDMODULE. " SET_NETDT INPUT
*&---------------------------------------------------------------------*
*&      MODULE  SET_NETDT2  INPUT
*&---------------------------------------------------------------------*
*       만기일 체크
*----------------------------------------------------------------------*
MODULE SET_NETDT2 INPUT.

  CASE OK_CODE.
    WHEN 'FTAX2' OR 'FRATE'.
      EXIT.
    WHEN OTHERS.
  ENDCASE.

  PERFORM SET_NETDT2.

ENDMODULE. " SET_NETDT2 INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_BUPLA  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_BUPLA INPUT.

*  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES GT_BUPLA[]
*                                       USING  GC_BUPLA.

  PERFORM HELP_F4_BUPLA.

ENDMODULE. " HLP_BUPLA INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_MWSKZ  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_MWSKZ INPUT.

  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES GT_MWSKZ[]
                                       USING  GC_MWSKZ.

ENDMODULE. " HLP_MWSKZ INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_DWTAX  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_DWTAX INPUT.

  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES GT_MWSKZ[]
                                       USING  GC_MWSKZ.

ENDMODULE. " HLP_MWSKZ INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_ZLSCH  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_ZLSCH INPUT.

  IF GT_ZLSCH[] IS INITIAL.
    SELECT ZLSCH TEXT2
           INTO TABLE GT_ZLSCH
           FROM T042ZT
          WHERE SPRAS = SY-LANGU
            AND LAND1 = GC_KR.
  ENDIF.

  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES GT_ZLSCH[]
                                       USING  GC_ZLSCH.

ENDMODULE. " HLP_ZLSCH INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_ZTERM  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_ZTERM INPUT.

  PERFORM SET_F4_ZTERM.

ENDMODULE. " HLP_ZTERM INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_BVTYP  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_BVTYP INPUT.

  PERFORM HELP_F4_BVTYP.

ENDMODULE. " HLP_BVTYP INPUT
*&---------------------------------------------------------------------*
*&      MODULE  HLP_HBKID  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE HLP_HBKID INPUT.

  PERFORM HELP_F4_HBKID.

ENDMODULE. " HLP_HBKID INPUT
*&---------------------------------------------------------------------*
*&      MODULE  EXIT  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE. " EXIT INPUT


*&---------------------------------------------------------------------*
*&      MODULE  CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE CHECK_DATE INPUT.

  IF GS_SCR100-BLDAT > SY-DATUM.
    SET CURSOR FIELD 'GS_SCR100-BLDAT'.
    MESSAGE E108 WITH '작성일자'(M13).
  ENDIF.

  "기산일 및 만기일 계산
  IF GS_SCR100-BLDAT IS NOT INITIAL AND
      GS_SCR100-ZTERM IS NOT INITIAL AND
      GS_SCR100-ZFBDT IS INITIAL.
    PERFORM SET_ADJUST_NETDT.
  ENDIF.

ENDMODULE. " CHECK_DATE INPUT
*&---------------------------------------------------------------------*
*&      MODULE  CHECK_DOUBLE_CLICK  INPUT
*&---------------------------------------------------------------------*
*       송장번호 클릭시 송장조회화면으로 이동한다.
*----------------------------------------------------------------------*
MODULE CHECK_DOUBLE_CLICK INPUT.
  PERFORM DOUBLE_CLICK_START.

ENDMODULE. " CHECK_DOUBLE_CLICK INPUT
*&---------------------------------------------------------------------*
*&      MODULE  GET_EXCHANGE_RATE  INPUT
*&---------------------------------------------------------------------*
*       환율 구하기
*----------------------------------------------------------------------*
MODULE GET_EXCHANGE_RATE INPUT.

  PERFORM GET_EXCHANGE_RATE.

ENDMODULE. " GET_EXCHANGE_RATE INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0071  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0071 INPUT.

  CASE OK_CODE_POPUP.
    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0061  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0061 INPUT.

  GET CURSOR FIELD GV_LAST_CURSOR_61.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HLP_EMPFK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HLP_EMPFK INPUT.
  PERFORM HELP_F4_EMPFK.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_EMPFK_NM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_EMPFK_NM INPUT.
  PERFORM SET_EMPFK_NM.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_DIFF_LIFNR_NM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DIFF_LIFNR_NM INPUT.

  PERFORM SET_DIFF_LIFNR_NM.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HLP_DIFF_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HLP_DIFF_LIFNR INPUT.
  PERFORM HELP_F4_DIFF_LIFNR.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_BUPLA_NM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_BUPLA_NM INPUT.

  PERFORM SET_BUPLA_NM.

ENDMODULE.
