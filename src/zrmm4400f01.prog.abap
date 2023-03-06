*&---------------------------------------------------------------------*
*& Include          ZRMM4400F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

*  SELECT SINGLE COMPANY, COMPANY_NAME
*      INTO ( @P_BUKRS, @GV_BUTXT )
*    FROM ZSVMM_USER_INFO
*  WHERE USER_ID = @SY-UNAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECTION_SCREEN_OUTPUT .

  LOOP AT SCREEN.

    CASE SCREEN-NAME.
      WHEN 'P_BUKRS'.
        SCREEN-INPUT = 1.
    ENDCASE.

    CASE 'X'.

      WHEN P_RD1B.
        IF SCREEN-GROUP1 EQ 'SC1'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-GROUP1 EQ 'SC2'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.

      WHEN P_RD1C.
        IF SCREEN-GROUP1 EQ 'SC1'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-GROUP1 EQ 'SC2'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY .

*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      IV_USER = SY-UNAME.
**     IV_OBJECT                   = 'ZMM_COMMON'
**     IV_BUKRS                    =
**     IV_EKORG                    =
**     IV_LGORT                    =
**     IV_WERKS                    =
**     IV_ZEXSPA                   =
**     IV_ZPODEP                   =
**     IV_ZPRDEP                   =
** TABLES
**     IT_BUKRS                    =
**     IT_EKORG                    =
**     IT_LGORT                    =
**     IT_WERKS                    =
**     IT_ZEXSPA                   =
**     IT_ZPODEP                   =
**     IT_ZPRDEP                   =
** EXCEPTIONS
**     NO_ID_DATA_FOUND            = 1
**     AUTHORIZATION_FAILURE       = 2
**     NO_INPUT_AUTH_VALUE         = 3
**     NO_DATA_FOUND               = 4
**     MANDATORYFIELDISMISS        = 5
**     OTHERS  = 6
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_COMMON_CONFIG
*&---------------------------------------------------------------------*
*& U7 - TIER2 대상 법인 조회 COMMON CONGIF 추가
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_COMMON_CONFIG.

  IF P_BUKRS IN GR_APPLIED_BUKRS.
    GV_APPLIED_BUKRS = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .



*& [U10 변경 시작 2022.12.08 - 운송중 재고 관련 로직 보완 - GV_OPT 로 VARIABLE 변경]


  GV_OPT = COND #( WHEN P_RD1A = 'X' THEN 'A'
                   WHEN P_RD1B = 'X' THEN 'B'
                   WHEN P_RD1C = 'X' THEN 'C'
                   WHEN P_RD1D = 'X' THEN 'D'
                   WHEN P_RD1E = 'X' THEN 'E' ).

*& [U10 변경 종료 2022.12.08 - 운송중 재고 관련 로직 보완]

  PERFORM GET_COMMON_CONFIG.

  PERFORM MAIN_SELECT.

  PERFORM PROCESSING_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN_SELECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_OPT
*&---------------------------------------------------------------------*
FORM MAIN_SELECT.

  _G_INIT : GT_DATA.


*-
  CASE GV_OPT.
    WHEN 'A'. " Total 재고
      IF P_MHDRZ IS NOT INITIAL. "잔여 유효기간 입력 값 O

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
               TYPE, BATCHSOL AS BATCH, BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
               PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
          FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B     "U11
*           ON A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY   "U11
*           AND A~BWTAR = B~BWTAR   "U11
          WHERE COMPANY = @P_BUKRS
            AND PLANT IN @S_WERKS
            AND SLOCATION IN @S_LGORT
            AND BP IN @S_LIFNR
            AND MATERIAL IN @S_MATNR
            AND BATCH IN @S_CHARG
            AND EXPIREDDATE <> @GC_00000000
            AND REMAINDATE LE @P_MHDRZ
            AND EKGRP IN @S_EKGRP
            AND DISPO IN @S_DISPO
            AND MATKL IN @S_MATKL
*            AND BKLAS IN @S_BKLAS  "U11
          INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ELSE. "잔여 유효기간 입력 값 X

*        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
*              TYPE, BATCHSOL AS BATCH,BWTAR,  MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
*              PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL
*         FROM ZSVCMM_REALSTOCK
*         WHERE COMPANY = @P_BUKRS
*           AND PLANT IN @S_WERKS
*           AND SLOCATION IN @S_LGORT
*           AND BP IN @S_LIFNR
*           AND MATERIAL IN @S_MATNR
*           AND BATCH IN @S_CHARG
*           AND EKGRP IN @S_EKGRP
*           AND DISPO IN @S_DISPO
*            AND MATKL IN @S_MATKL
*         INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
              TYPE, BATCHSOL AS BATCH, A~BWTAR,  MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
              PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
         FROM ZSVCMM_REALSTOCK AS A "U11
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
         WHERE COMPANY = @P_BUKRS
           AND PLANT IN @S_WERKS
           AND SLOCATION IN @S_LGORT
           AND BP IN @S_LIFNR
           AND MATERIAL IN @S_MATNR
           AND BATCH IN @S_CHARG
           AND EKGRP IN @S_EKGRP
           AND DISPO IN @S_DISPO
           AND MATKL IN @S_MATKL
*           AND BKLAS IN @S_BKLAS     "U11
         INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.


      ENDIF.

    WHEN 'B'. " 저장위치 재고
      IF P_MHDRZ IS NOT INITIAL. "잔여 유효기간 입력 값 O

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
              TYPE, BATCHSOL AS BATCH, A~BWTAR,  MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
              PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL" BKLAS
         FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
         WHERE COMPANY = @P_BUKRS
           AND PLANT IN @S_WERKS
           AND SLOCATION IN @S_LGORT
           AND MATERIAL IN @S_MATNR
           AND BATCH IN @S_CHARG
           AND EXPIREDDATE <> @GC_00000000
           AND REMAINDATE LE @P_MHDRZ
           AND TYPE = @GC_COND_SL
           AND EKGRP IN @S_EKGRP
           AND DISPO IN @S_DISPO
           AND MATKL IN @S_MATKL
*           AND BKLAS IN @S_BKLAS  "U11
         INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ELSE.

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
              TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
              PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL" BKLAS
         FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
         WHERE COMPANY = @P_BUKRS
           AND PLANT IN @S_WERKS
           AND SLOCATION IN @S_LGORT
           AND MATERIAL IN @S_MATNR
           AND BATCH IN @S_CHARG
           AND TYPE = @GC_COND_SL
           AND EKGRP IN @S_EKGRP
           AND DISPO IN @S_DISPO
           AND MATKL IN @S_MATKL
*           AND BKLAS IN @S_BKLAS  "U11
         INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ENDIF.

    WHEN 'C'. " 업체 SC 재고
      IF P_MHDRZ IS NOT INITIAL. "잔여 유효기간 입력 값 O

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
             TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
             PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
        FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
        WHERE COMPANY = @P_BUKRS
          AND PLANT IN @S_WERKS
          AND BP IN @S_LIFNR
          AND MATERIAL IN @S_MATNR
          AND BATCH IN @S_CHARG
          AND EXPIREDDATE <> @GC_00000000
          AND REMAINDATE LE @P_MHDRZ
          AND TYPE = @GC_COND_SC
          AND EKGRP IN @S_EKGRP
          AND DISPO IN @S_DISPO
          AND MATKL IN @S_MATKL
*          AND BKLAS IN @S_BKLAS  "U11
        INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ELSE.

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
               TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
               PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
            FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
            WHERE COMPANY = @P_BUKRS
              AND PLANT IN @S_WERKS
              AND BP IN @S_LIFNR
              AND MATERIAL IN @S_MATNR
              AND BATCH IN @S_CHARG
              AND TYPE = @GC_COND_SC
              AND EKGRP IN @S_EKGRP
              AND DISPO IN @S_DISPO
              AND MATKL IN @S_MATKL
*              AND BKLAS IN @S_BKLAS  "U11
           INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ENDIF.

    WHEN 'D'. " 업체 위탁 재고
      IF P_MHDRZ IS NOT INITIAL. "잔여 유효기간 입력 값 O

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
             TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
             PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
        FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
        WHERE COMPANY = @P_BUKRS
          AND PLANT IN @S_WERKS
          AND SLOCATION IN @S_LGORT
          AND BP IN @S_LIFNR
          AND MATERIAL IN @S_MATNR
          AND BATCH IN @S_CHARG
          AND EXPIREDDATE <> @GC_00000000
          AND REMAINDATE LE @P_MHDRZ
          AND TYPE = @GC_COND_CS
          AND EKGRP IN @S_EKGRP
          AND DISPO IN @S_DISPO
          AND MATKL IN @S_MATKL
*          AND BKLAS IN @S_BKLAS  "U11
        INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ELSE.

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
               TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
               PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
          FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
          WHERE COMPANY = @P_BUKRS
            AND PLANT IN @S_WERKS
            AND BP IN @S_LIFNR
            AND MATERIAL IN @S_MATNR
            AND BATCH IN @S_CHARG
            AND TYPE = @GC_COND_CS
            AND EKGRP IN @S_EKGRP
            AND DISPO IN @S_DISPO
            AND MATKL IN @S_MATKL
*            AND BKLAS IN @S_BKLAS  "U11
          INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ENDIF.

    WHEN 'E'. " 고객 판매 재고
      IF P_MHDRZ IS NOT INITIAL. "잔여 유효기간 입력 값 O

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
             TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
             PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
        FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
        WHERE COMPANY = @P_BUKRS
          AND PLANT IN @S_WERKS
          AND SLOCATION IN @S_LGORT
          AND BP IN @S_LIFNR
          AND MATERIAL IN @S_MATNR
          AND BATCH IN @S_CHARG
          AND EXPIREDDATE <> @GC_00000000
          AND REMAINDATE LE @P_MHDRZ
          AND TYPE = @GC_COND_SO
          AND EKGRP IN @S_EKGRP
          AND DISPO IN @S_DISPO
          AND MATKL IN @S_MATKL
*          AND BKLAS IN @S_BKLAS  "U11
        INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ELSE.

        SELECT PLANT, PLANTNAME, SLOCATION, SLNAME, BP, BPNAME, MATERIAL, MATNAME,
               TYPE, BATCHSOL AS BATCH, A~BWTAR, MEINS, AVAILABLESTOCK, QISTOCK, BLOCKSTOCK, CHAR_BP,
               PRDUCTIONDATE, EXPIREDDATE, PRDDUCTLOT, MAKER, REMAINDATE, EKGRP, EKNAM, DISPO, DSNAM, MATKL "BKLAS
          FROM ZSVCMM_REALSTOCK AS A
*          INNER JOIN MBEW AS B      "U11
*           ON  A~MATERIAL = B~MATNR "U11
*           AND A~PLANT = B~BWKEY    "U11
*           AND A~BWTAR = B~BWTAR    "U11
          WHERE COMPANY = @P_BUKRS
            AND PLANT IN @S_WERKS
            AND BP IN @S_LIFNR
            AND MATERIAL IN @S_MATNR
            AND BATCH IN @S_CHARG
            AND TYPE = @GC_COND_SO
            AND EKGRP IN @S_EKGRP
            AND DISPO IN @S_DISPO
            AND MATKL IN @S_MATKL
*            AND BKLAS IN @S_BKLAS  "U11
          INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

      ENDIF.
  ENDCASE.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID .
  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD,
       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
*  LS_TOOLBTN-BTN_MADD   = 'X'.       "Multi Add Row
*  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row
*  LS_TOOLBTN-BTN_DEL    = 'X'.       "Delete Row
*  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
*  LS_TOOLBTN-BTN_INFO  = 'X'.       "Batch Search
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM10010'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
**  SELECT SINGLE BUTXT,WAERS
**    INTO @DATA(LS_DATA)
**    FROM T001
**   WHERE BUKRS = @P_BUKRS.
**
**  _L_ADD_FIELD: 'KOKRS' P_KOKRS,
**                'BUKRS' P_BUKRS,
**                'BUTXT' LS_DATA-BUTXT,
**                'WAERS' LS_DATA-WAERS,
**                'VERSN' P_VERSN.

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

*--------------------------------------------------
* Set Lock Name..
*--------------------------------------------------
  DATA: LV_TIMESTAMP_OUT TYPE TIMESTAMP.
  GET TIME STAMP FIELD LV_TIMESTAMP_OUT.
*  DATA(LV_LOCK_NM) = S_LAEDA-LOW. "LV_TIMESTAMP_OUT.

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY
      IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD
*     iv_scr_wr  = '20:10:70'
      IT_HEADER  = LT_HEADER.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT .
*  DATA:LV_ANS.

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------
*  PERFORM CHECK_CHANGED_DATA USING 'E' CHANGING LV_ANS.

*  CHECK LV_ANS IS INITIAL.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_ZFDLV_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_ZFDLV_INIT .

  DATA : LT_SELIDX TYPE LVC_T_ROW.
*         LV_TITLE  TYPE STRING,
*         LV_TEXT2  TYPE STRING.


*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

  IF LV_TSELLINES IS INITIAL.
    MESSAGE S006(ZMM01) DISPLAY LIKE 'I'. EXIT. "선택된 데이타가 없습니다.
  ELSEIF LV_TSELLINES > 1.
    MESSAGE S007(ZMM01) DISPLAY LIKE 'E'. EXIT. "1 건의 데이타만 선택 가능 합니다.
  ENDIF.

  LOOP AT LT_SELIDX INTO DATA(LV_SELIDX).
*    DATA(LV_TABIX) = SY-TABIX.
    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.
      IF <LS_DISP>-BATCH IS INITIAL.
        MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M02. . "배치 관리 재고가 아닙니다.
      ELSE.
        SET PARAMETER ID 'MAT' FIELD <LS_DISP>-MATERIAL.
        SET PARAMETER ID 'CHA' FIELD <LS_DISP>-BATCH.
        CALL TRANSACTION 'MSC2N' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.


*-
*    GRF_GRID->REFRESH_GRID_DISPLAY( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_PRINT_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM BTN_ON_PRINT_INIT .
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_110
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA_110 .

  PERFORM GET_DATA.
  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUKRS_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_BUKRS
*&---------------------------------------------------------------------*
FORM BUKRS_CHECK USING IV_BUKRS.

  IF IV_BUKRS IS NOT INITIAL.
    SELECT SINGLE BUTXT FROM T001 INTO @GV_BUTXT
    WHERE BUKRS = @IV_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E005 WITH TEXT-F02 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form werks_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_WERKS_LOW
*&---------------------------------------------------------------------*
FORM WERKS_CHECK USING IV_WERKS.

  IF P_BUKRS IS INITIAL AND IV_WERKS IS NOT INITIAL.
    CLEAR : S_WERKS.
    MESSAGE S017 WITH TEXT-F02.
    LEAVE SCREEN.
  ENDIF.

  IF IV_WERKS IS NOT INITIAL.
    SELECT SINGLE B~NAME1
       FROM T001K AS A INNER JOIN T001W AS B
                                       ON A~BWKEY = B~BWKEY
       INTO @GV_NAME1
    WHERE A~BUKRS = @P_BUKRS
         AND B~WERKS = @IV_WERKS.
    IF SY-SUBRC NE 0.
      MESSAGE E005 WITH TEXT-F03 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form lgort_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_LGORT_LOW
*&---------------------------------------------------------------------*
FORM LGORT_CHECK USING IV_LGORT.

  IF S_WERKS IS INITIAL AND IV_LGORT IS NOT INITIAL.
    MESSAGE S017 WITH TEXT-F03.
    LEAVE SCREEN.
  ENDIF.

  IF IV_LGORT IS NOT INITIAL.

    SELECT SINGLE A~LGORT, A~LGOBE
       INTO @DATA(LS_T001L)
      FROM T001L AS A
     WHERE A~WERKS IN @S_WERKS
         AND A~LGORT = @IV_LGORT.
    IF SY-SUBRC NE 0.
      MESSAGE E005 WITH TEXT-F04 DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

  GV_LGOBE = LS_T001L-LGOBE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VARIANT_F4_1000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM VARIANT_F4_1000 USING IV_FNAME IV_TEXT IV_OPTION.

  TYPES: BEGIN OF LTS_VALUE_WERKS,
           KYFLD TYPE T001W-WERKS,
           DESCR TYPE T001W-NAME1,
         END OF LTS_VALUE_WERKS.

  TYPES: BEGIN OF LTS_VALUE_LGORT,
           KYFLD TYPE T001L-LGORT,
           DESCR TYPE T001L-LGOBE,
         END OF LTS_VALUE_LGORT.

  TYPES: BEGIN OF LTS_VALUE_DISPO,
           KYFLD1 TYPE T024D-WERKS,
           KYFLD  TYPE T024D-DISPO,
           DESCR  TYPE T024D-DSNAM,
         END OF LTS_VALUE_DISPO.

  DATA: LT_FIELD    TYPE APB_LPD_T_DFIES, "TABLE OF DFIES,
        LT_RETURN   TYPE TABLE OF DDSHRETVAL,
        LT_DYUPDATE TYPE TABLE OF  DYNPREAD.

  DATA: LV_REFLD     TYPE DFIES-FIELDNAME,
        LV_DYFLD     TYPE HELP_INFO-DYNPROFLD,
        LV_DESC      TYPE HELP_INFO-DYNPROFLD,
        LV_TITLE(50).

  DATA: LRF_DATA TYPE REF TO DATA.

  FIELD-SYMBOLS: <LT_DATA> TYPE STANDARD TABLE.

  CONSTANTS: LC_KYFLD   TYPE FIELDNAME VALUE 'KYFLD',
             LC_KYFLD1  TYPE FIELDNAME VALUE 'KYFLD1',
             LC_DESCR   TYPE FIELDNAME VALUE 'DESCR',
             LC_1000(4) VALUE '1000'.

  DEFINE _L_SET_VAL.

    lt_field = VALUE #( BASE lt_field
               ( fieldname = &1 inttype = &2
               leng = &3 intlen = &3 outputlen = &3
                 reptext = &4 offset = &5 ) ).

  END-OF-DEFINITION.


  IF P_BUKRS IS INITIAL.
    PERFORM SCREEN_VALUE_FIND USING GC_BUKRS
                              CHANGING P_BUKRS.
  ENDIF.

  IF S_WERKS[] IS INITIAL.

    DATA : LV_FNAME TYPE FIELDNAME.

    LV_FNAME = GC_WERKS.

    DATA(LT_PARAMS) = ZCL_MM_COMMON=>GET_DYNP_PARAM(
                          IV_FNAME = LV_FNAME
                          IV_DYNNR = GC_1000  ).
    DATA(LS_PARAMS) = LT_PARAMS[ SELNAME = GC_WERKS ].

    IF LS_PARAMS-LOW IS NOT INITIAL.
      S_WERKS[] = VALUE #(
                          ( SIGN = 'I'  OPTION = 'EQ'
                            LOW = LS_PARAMS-LOW HIGH = LS_PARAMS-HIGH ) ).
    ENDIF.

  ENDIF.


  LV_DYFLD = IV_FNAME.
  LV_DESC = IV_TEXT.


  CASE IV_FNAME.
    WHEN 'WERKS'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_WERKS.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD' 'C' '8'  TEXT-C07        '0',
                   'DESCR' 'C' '30' TEXT-C08     '8'.

    WHEN 'LGORT'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_LGORT.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD' 'C' '8'  TEXT-C13       '0',
                   'DESCR' 'C' '30' TEXT-C14     '8'.

    WHEN 'DISPO'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_DISPO.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD1' 'C' '8'  TEXT-C07      '0',
                   'KYFLD'  'C' '6'  TEXT-C11     '8',
                   'DESCR'  'C' '30' TEXT-C12  '14'.
  ENDCASE.

*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
  PERFORM EVT_GRID_F4_1000 USING  IV_FNAME
                                    CHANGING <LT_DATA>
                                             LV_TITLE.

  DATA(LT_MAP) = VALUE ICL_DSELC_T(
              ( FLDNAME = LC_KYFLD DYFLDNAME = LV_DYFLD )
              ( FLDNAME = LC_DESCR DYFLDNAME = LV_DESC ) ).

  "-Call F4 Search Help
  CHECK LV_TITLE IS NOT INITIAL.
  LV_REFLD = IV_FNAME.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = LV_REFLD
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = LV_DYFLD
      WINDOW_TITLE    = LV_TITLE
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = <LT_DATA>
      FIELD_TAB       = LT_FIELD
      DYNPFLD_MAPPING = LT_MAP
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  CHECK LT_RETURN IS NOT INITIAL.
  SORT LT_RETURN BY FIELDNAME.
  READ TABLE LT_RETURN INTO DATA(LS_RETURN)
          WITH KEY FIELDNAME = LC_KYFLD BINARY SEARCH.

  IF LV_DESC IS NOT INITIAL.
    READ TABLE LT_RETURN INTO DATA(LS_RETURN_T)
            WITH KEY FIELDNAME = LC_DESCR BINARY SEARCH.
  ENDIF.

  CASE IV_FNAME.
    WHEN 'WERKS'.

      IF IV_OPTION = 'LOW'.
        S_WERKS-LOW = LS_RETURN-FIELDVAL.
        GV_NAME1 = LS_RETURN_T-FIELDVAL.
      ELSE.
        S_WERKS-HIGH = LS_RETURN-FIELDVAL.
        GV_NAME1 = LS_RETURN_T-FIELDVAL.
      ENDIF.

    WHEN 'LGORT'.

      IF IV_OPTION = 'LOW'.
        S_LGORT-LOW = LS_RETURN-FIELDVAL.
        GV_LGOBE = LS_RETURN_T-FIELDVAL.
      ELSE.
        S_LGORT-HIGH = LS_RETURN-FIELDVAL.
        GV_LGOBE = LS_RETURN_T-FIELDVAL.
      ENDIF.

    WHEN 'DISPO'.

      IF IV_OPTION = 'LOW'.
        S_DISPO-LOW = LS_RETURN-FIELDVAL.
        GV_DISPO = LS_RETURN_T-FIELDVAL.
      ELSE.
        S_DISPO-HIGH = LS_RETURN-FIELDVAL.
        GV_DISPO = LS_RETURN_T-FIELDVAL.
      ENDIF.
  ENDCASE.

  CHECK LS_RETURN-FIELDVAL IS NOT INITIAL AND
            LV_DESC IS NOT INITIAL.

  CLEAR LT_DYUPDATE[].
  LT_DYUPDATE = VALUE #(
             ( FIELDNAME  = IV_TEXT FIELDVALUE = LS_RETURN_T-FIELDVAL ) ).

  "PBO가 없는 화면필드내용 변경
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME               = SY-REPID
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = LT_DYUPDATE[]
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      UNDEFIND_ERROR       = 7
      OTHERS               = 8.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCREEN_VALUE_FIND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_BUKRS
*&      <-- P_BUKRS
*&---------------------------------------------------------------------*
FORM SCREEN_VALUE_FIND USING IV_PARA
                        CHANGING CV_RETURN.

  DATA : LV_FNAME TYPE FIELDNAME.

  LV_FNAME = IV_PARA.

  DATA(LT_PARAMS) = ZCL_MM_COMMON=>GET_DYNP_PARAM(
                        IV_FNAME = LV_FNAME
                        IV_DYNNR = GC_1000  ).
  DATA(LS_PARAMS) = LT_PARAMS[ SELNAME = IV_PARA ].
  CV_RETURN = LS_PARAMS-LOW.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_f4_1000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FNAME
*&      <-- <LT_DATA>
*&      <-- LV_TITLE
*&---------------------------------------------------------------------*
FORM EVT_GRID_F4_1000 USING IV_FIELDNAME
                  CHANGING CT_F4_LIST  TYPE TABLE
                           CV_TITLE.

  CASE IV_FIELDNAME.
    WHEN 'WERKS'.

      IF P_BUKRS IS INITIAL.
        MESSAGE S017 WITH TEXT-F02.
        EXIT.
      ENDIF.

      SELECT B~WERKS AS KYFLD , B~NAME1 AS DESCR
        INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
        FROM T001K AS A INNER JOIN T001W AS B
                                       ON A~BWKEY = B~BWKEY
       WHERE A~BUKRS = @P_BUKRS
       ORDER BY B~WERKS.
      IF SY-SUBRC NE 0.
        MESSAGE S005 WITH TEXT-F03 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      _G_SET_VALUE : CV_TITLE          TEXT-C07.

    WHEN 'LGORT'.
      IF S_WERKS[] IS INITIAL.
        MESSAGE S017 WITH TEXT-F03.
        EXIT.
      ENDIF.

      SELECT LGORT AS KYFLD , LGOBE AS DESCR
        INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
        FROM T001L
       WHERE WERKS IN @S_WERKS
      ORDER BY LGORT.
      IF SY-SUBRC NE 0.
        MESSAGE S005 WITH TEXT-F04 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      _G_SET_VALUE : CV_TITLE          TEXT-C13.

    WHEN 'DISPO'.
      IF S_WERKS[] IS NOT INITIAL.

        SELECT WERKS AS KYFLD1, DISPO AS KYFLD , DSNAM AS DESCR
          INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
          FROM T024D
         WHERE WERKS IN @S_WERKS.

        IF SY-SUBRC NE 0.
          MESSAGE S005 WITH TEXT-F05 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*        DELETE ADJACENT DUPLICATES FROM CT_F4_LIST COMPARING ALL FIELDS.
        _G_SET_VALUE : CV_TITLE          TEXT-C11.

      ELSE.
        SELECT WERKS AS KYFLD1, DISPO AS KYFLD , DSNAM AS DESCR
        INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
        FROM T024D
        ORDER BY DISPO.

        IF SY-SUBRC NE 0.
          MESSAGE S005 WITH TEXT-F05 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        DELETE ADJACENT DUPLICATES FROM CT_F4_LIST COMPARING ALL FIELDS.
        _G_SET_VALUE : CV_TITLE           TEXT-C11.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VARIANT_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIANT
*&      <-- P_VAR
*&---------------------------------------------------------------------*
FORM VARIANT_F4 USING IS_VARIANT TYPE ANY
                 CHANGING  CV_VARIANT_SCREEN TYPE ANY.

  DATA: LS_VARIANT TYPE DISVARIANT,
        LV_EXIT(1) TYPE C.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = IS_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      E_EXIT        = LV_EXIT
      ES_VARIANT    = LS_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE I031(ZPCA01).  "'No found layout available for F4.'
  ENDIF.

  IF LV_EXIT IS INITIAL.
    CV_VARIANT_SCREEN = LS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA .


*& [U7 변경 시작 2022.11.03 - CODE INSPECTION COMPLEXITY 오류 수정]


  DATA : LT_VBBE       LIKE TABLE OF GS_VBBE,
         LT_RESB       LIKE TABLE OF GS_RESB,
         LT_MDUB       LIKE TABLE OF GS_MDUB,
         LT_MDBS       LIKE TABLE OF GS_MDBS,
         LT_TEMP       LIKE TABLE OF GS_MDBS,
         LT_GRINFO     LIKE TABLE OF GS_GRINFO,
         LT_VBBE_SUM   LIKE TABLE OF GS_VBBE,
         LT_RESB_SUM   LIKE TABLE OF GS_RESB,
         LT_MDUB_SUM   LIKE TABLE OF GS_MDUB_SUM,
         LT_GRINFO_SUM LIKE TABLE OF GS_MDUB,
         LS_VBBE       LIKE LINE OF LT_VBBE,
         LS_RESB       LIKE LINE OF LT_RESB,
         LS_MDUB       LIKE LINE OF LT_MDUB,
         LS_MDUB_SUM   LIKE LINE OF LT_MDUB_SUM,
         LS_GRINFO     LIKE LINE OF LT_GRINFO.
*         LR_VBELN      TYPE RANGE OF MATDOC-WEMPF,
*         LR_POSNR      TYPE RANGE OF MATDOC-LGPLA.





*& [U7 변경 종료 2022.11.03 - CODE INSPECTION COMPLEXITY 오류 수정]
*-
  DATA: LS_DISP  TYPE TS_DISP.
*-
  _G_INIT : GT_DISP.
*-

  "자재그룹 내역 추가

  DATA(LT_DATA) = GT_DATA[].
  SORT LT_DATA BY MATKL.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING MATKL.

  IF LT_DATA[] IS NOT INITIAL.

    SELECT FROM T023T
      FIELDS MATKL, WGBEZ, WGBEZ60
      FOR ALL ENTRIES IN @LT_DATA
      WHERE MATKL = @LT_DATA-MATKL
      AND SPRAS = @SY-LANGU
      INTO TABLE @DATA(LT_T023T).
    SORT LT_T023T BY MATKL.

  ENDIF.

  FREE LT_DATA.

  "공급처(CHAR_BP)명 추가
  LT_DATA[] = GT_DATA[].
  SORT LT_DATA BY CHAR_BP.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING CHAR_BP.

  IF LT_DATA[] IS NOT INITIAL.

    SELECT FROM MCH1 AS A INNER JOIN LFA1 AS B ON A~LIFNR = B~LIFNR
      FIELDS A~LIFNR, B~NAME1
      FOR ALL ENTRIES IN @LT_DATA
      WHERE A~LIFNR = @LT_DATA-CHAR_BP
      INTO TABLE @DATA(LT_LFA1).
    SORT  LT_LFA1  BY LIFNR.
    FREE LT_DATA.

  ENDIF.

*  납품예정 수량 정보를 추가 발췌
*  데이터 조회시 조건 추가 및 검색 조건 추가
  LT_DATA[] = GT_DATA[].

  SORT LT_DATA BY PLANT SLOCATION MATERIAL.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING PLANT SLOCATION MATERIAL.
*운송중 재고 관련 로직
  PERFORM GET_STOCKINFO USING LT_DATA.

  LT_VBBE[] = GT_VBBE[].
  LT_RESB[] = GT_RESB[].
  LT_MDUB[] = GT_MDUB[].
  LT_MDBS[] = GT_MDBS[].


  CLEAR : LT_VBBE_SUM, GS_VBBE.
  LOOP AT LT_VBBE INTO LS_VBBE.
    COLLECT LS_VBBE INTO LT_VBBE_SUM.
  ENDLOOP.
  SORT LT_VBBE_SUM BY WERKS LGORT MATNR CHARG.

*운송중 재고, 법인간 재고 로직 추가]
  LOOP AT LT_MDUB INTO LS_MDUB.
    MOVE-CORRESPONDING LS_MDUB TO LS_MDUB_SUM.
    COLLECT LS_MDUB_SUM INTO LT_MDUB_SUM.
  ENDLOOP.
  SORT LT_MDUB_SUM BY WERKS LGORT MATNR CHARG.



  LT_TEMP[] = LT_MDBS[].

  LOOP AT LT_TEMP INTO DATA(LS_TEMP).
    LS_TEMP-WEMPF = LS_TEMP-VBELN.
    LS_TEMP-LGPLA = LS_TEMP-POSNR.
    MODIFY LT_TEMP FROM LS_TEMP TRANSPORTING WEMPF LGPLA.

    CLEAR LS_TEMP.
  ENDLOOP.

  SORT LT_TEMP BY EBELN WEMPF LGPLA.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING EBELN WEMPF LGPLA.

  IF NOT LT_TEMP[] IS INITIAL.
    SELECT EBELN, MATNR, CHARG, MENGE, WEMPF, LGPLA
      FROM MATDOC
      FOR ALL ENTRIES IN @LT_TEMP
      WHERE EBELN = @LT_TEMP-EBELN
        AND WEMPF = @LT_TEMP-WEMPF
        AND LGPLA = @LT_TEMP-LGPLA
        AND CANCELLED = ' '
        AND CANCELLATION_TYPE = ''
       INTO TABLE @DATA(LT_MATDOC).
  ENDIF.

  SORT LT_MATDOC BY EBELN WEMPF LGPLA.

  LOOP AT LT_MDBS INTO DATA(LS_MDBS).
    READ TABLE LT_MATDOC INTO DATA(LS_MATDOC) WITH KEY EBELN = LS_MDBS-EBELN
                                                       WEMPF = LS_MDBS-VBELN
                                                       LGPLA = LS_MDBS-POSNR
                                                       BINARY SEARCH.
    IF SY-SUBRC NE 0.    "입고 처리 정보가 없을 시
      LS_GRINFO-WERKS = LS_MDBS-WERKS.
      LS_GRINFO-LGORT = LS_MDBS-LGORT.
      LS_GRINFO-MATNR = LS_MDBS-MATNR.
      LS_GRINFO-CHARG = LS_MDBS-CHARG.
      LS_GRINFO-WEMNG = LS_MDBS-LFIMG.
      COLLECT LS_GRINFO INTO LT_GRINFO_SUM.
      CLEAR LS_GRINFO.
    ENDIF.

  ENDLOOP.

  SORT LT_GRINFO_SUM BY WERKS LGORT MATNR CHARG.

*-
  SORT GT_DATA BY  PLANT SLOCATION MATERIAL BWTAR.

*운송중 재고 관련 로직 보완]
  PERFORM SET_MDUB_MDBS_INFO USING LT_MDUB_SUM
                                   LT_GRINFO_SUM.

* 예약 수량 정보 추가

  CLEAR : LT_RESB_SUM, LS_RESB.

  LOOP AT LT_RESB INTO LS_RESB.
    COLLECT LS_RESB INTO LT_RESB_SUM.
  ENDLOOP.

  SORT LT_RESB_SUM BY WERKS LGORT MATNR CHARG.

  DATA(LT_RESB_SUM_B) = LT_RESB_SUM[].
  SORT LT_RESB_SUM_B BY WERKS LGORT MATNR BWTAR.

  FREE LT_DATA.

  DATA : BEGIN OF LS_11030,
           MATNR LIKE MARA-MATNR,
           WERKS LIKE T001W-WERKS,
           LGORT LIKE T001L-LGORT,
           LGPBE LIKE MARD-LGPBE,
         END OF LS_11030,
         LT_11030 LIKE TABLE OF LS_11030,

         BEGIN OF LS_11010T,
           MATNR  LIKE MARA-MATNR,
           MAKTX LIKE MAKT-MAKTX,
         END OF LS_11010T,
         LT_11010T LIKE TABLE OF LS_11010T.

  PERFORM GET_CBO_TABLE_DATA TABLES LT_11030
                                    LT_11010T.

  SORT LT_11030 BY MATNR WERKS LGORT.
  SORT LT_11010T BY MATNR.

  LOOP AT GT_DATA INTO DATA(LS_DATA).

    MOVE-CORRESPONDING LS_DATA TO LS_DISP.

*  저장수명/사용기간 관리 타입 추가
*    READ TABLE LT_STO INTO DATA(LS_STO_S) WITH KEY WERKS = LS_DATA-PLANT MATNR = LS_DATA-MATERIAL
*                                               BINARY SEARCH .
*    IF SY-SUBRC = 0.
*      LS_DISP-ZSTO = 'Y'.
*    ELSE.
*      LS_DISP-ZSTO = 'N'.
*    ENDIF.

    READ TABLE LT_T023T INTO DATA(LS_T023T) WITH KEY MATKL = LS_DISP-MATKL
                                     BINARY SEARCH .
    IF SY-SUBRC = 0.
      LS_DISP-WGBEZ = LS_T023T-WGBEZ.
    ENDIF.

    IF LS_DATA-CHAR_BP IS NOT INITIAL.
      READ TABLE LT_LFA1 INTO DATA(LS_LFA1) WITH KEY LIFNR = LS_DATA-CHAR_BP
                                          BINARY SEARCH .
      IF SY-SUBRC = 0.
        LS_DISP-NAME1 = LS_LFA1-NAME1.
      ENDIF.
    ENDIF.

    LS_DISP-REMAINDATE_2 = LS_DATA-REMAINDATE.

    PERFORM SETTING_TYPE_KOR USING LS_DISP-TYPE
                                                   CHANGING LS_DISP-TYPE_KOR.



** 회사코드가 DI , D1400, D1405 기준으로 존재시에만 검색되게 조건 추가

    IF P_BUKRS IN GR_APPLIED_BUKRS.

**  CODE INSPECTION COMPLEXITY 초과로 인한 로직 이동
      PERFORM SET_STOCKINFO USING LS_DATA
                                  LT_VBBE_SUM
                                  LT_RESB_SUM
                                  LT_RESB_SUM_B
                                  LT_MDUB_SUM
                                  LT_GRINFO_SUM
                         CHANGING LS_DISP.

    ENDIF.

    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING
        VALUE = LS_DISP-REMAINDATE_2.


    APPEND LS_DISP TO GT_DISP. CLEAR LS_DISP.


  ENDLOOP.

  SORT GT_DISP BY PLANT SLOCATION MATERIAL BATCH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_STOCKINFO
*&---------------------------------------------------------------------*
FORM GET_STOCKINFO USING IT_DATA LIKE GT_DATA.

  DATA: LT_DATA  TYPE TABLE OF TY_DATA,
        LV_EBELP TYPE EKPO-EBELP.

  LT_DATA[] = IT_DATA[].

  SORT LT_DATA BY PLANT SLOCATION MATERIAL.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING PLANT SLOCATION MATERIAL.

*운송중 재고, 입고예정 발췌 로직 추가, 납품예정 발췌로직 수정]

  IF LT_DATA[] IS NOT INITIAL.

    SELECT A~WERKS, A~LGORT, A~MATNR, A~CHARG, A~LFIMG
      FROM LIPS AS A INNER JOIN T001K AS B
                             ON B~BWKEY = A~WERKS
      FOR ALL ENTRIES IN @LT_DATA
      WHERE A~WERKS EQ @LT_DATA-PLANT
        AND A~LGORT EQ @LT_DATA-SLOCATION
        AND A~MATNR EQ @LT_DATA-MATERIAL
        AND LFIMG > 0
        AND WBSTA NE 'C'                   "처리 완료건 제외
        AND B~BUKRS IN @GR_APPLIED_BUKRS   "조회조건의 COMPANY
        INTO TABLE @GT_VBBE.

*- 미처리 예약 수량 정보 추가 발췌.

*    SELECT A~WERKS, A~LGORT, A~MATNR, A~CHARG, A~BWTAR, A~BDMNG
*      FROM ZSVBMMRESBQTY AS A INNER JOIN T001K AS B
*                                      ON B~BWKEY = A~WERKS
*      FOR ALL ENTRIES IN @LT_DATA
*      WHERE A~WERKS   EQ @LT_DATA-PLANT
*        AND A~LGORT   EQ @LT_DATA-SLOCATION
*        AND A~MATNR   EQ @LT_DATA-MATERIAL
*        AND B~BUKRS IN @GR_APPLIED_BUKRS
*      INTO TABLE @GT_RESB.

  ENDIF.


  IF GV_OPT EQ 'A' OR GV_OPT EQ 'B'.


*- 운송중 재고
    SELECT A~WERKS, C~LGORT, J~LGOBE, D~MATNR, D~CHARG, D~LFIMG, A~MEINS, A~EBELN, A~EBELP, D~VGPOS, D~VBELN, D~POSNR,
           E~HSDAT AS PRDUCTIONDATE,
           E~VFDAT AS EXPIREDDATE,
           E~LICHN AS PRDDUCTLOT,
           F~EKGRP,
           F~DISPO,
           G~MATKL,
           H~NAME1,
           I~MAKTX
      FROM MDUB AS A INNER JOIN T001K AS B
                             ON B~BWKEY = A~WERKS
                     INNER JOIN EKPO  AS C
                             ON C~EBELN = A~EBELN AND C~EBELP = A~EBELP
                     INNER JOIN LIPS  AS D
                             ON D~VGBEL = A~EBELN
*                            AND D~VGPOS = A~EBELP
                     LEFT OUTER JOIN ZCCMM_BATCHFIND AS E
                             ON D~MATNR = E~MATNR
                            AND D~CHARG = E~BATCH
                     INNER JOIN MARC AS F
                             ON D~MATNR = F~MATNR
                            AND A~WERKS = F~WERKS
                     INNER JOIN MARA AS G
                             ON D~MATNR = G~MATNR
                     INNER JOIN T001W AS H
                             ON A~WERKS = H~WERKS
                     INNER JOIN MAKT AS I
                             ON D~MATNR = I~MATNR
                            AND I~SPRAS = @SY-LANGU
                     INNER JOIN T001L AS J
                             ON A~WERKS = J~WERKS
                            AND C~LGORT = J~LGORT
*    FOR ALL ENTRIES IN @LT_DATA
      WHERE A~WERKS IN @S_WERKS
*    A~WERKS IN @S_WERKS
      AND A~LGORT IN @S_LGORT
      AND A~MATNR IN @S_MATNR
      AND B~BUKRS IN @GR_APPLIED_BUKRS
      AND B~BUKRS EQ @P_BUKRS
      AND D~CHARG IN @S_CHARG
      AND F~EKGRP IN @S_EKGRP
      AND F~DISPO IN @S_DISPO
      AND G~MATKL IN @S_MATKL
      AND A~MENGE > 0
      AND D~LFIMG > 0
      AND A~WAMNG > 0
      AND A~LOEKZ = @SPACE
      AND A~ELIKZ = @SPACE
      INTO TABLE @GT_MDUB.

    IF GT_MDUB[] IS NOT INITIAL.

      LOOP AT GT_MDUB INTO DATA(LS_MDUB).
        LV_EBELP = LS_MDUB-VGPOS+1(5).
        IF LS_MDUB-EBELP NE LV_EBELP.
          DELETE TABLE GT_MDUB FROM LS_MDUB.
        ENDIF.

        CLEAR LV_EBELP.
      ENDLOOP.

    ENDIF.


*- 입고예정 발췌 로직을 추가 함.

    SELECT A~EBELN, A~EBELP, A~WERKS, A~LGORT,J~LGOBE, D~VBELN, D~POSNR, D~MATNR, D~CHARG, D~LFIMG, A~MEINS,
           E~HSDAT AS PRDUCTIONDATE,
           E~VFDAT AS EXPIREDDATE,
           E~LICHN AS PRDDUCTLOT,
           F~EKGRP,
           F~DISPO,
           G~MATKL,
           H~NAME1,
           I~MAKTX,
           K~POSEX
      FROM MDBS AS A INNER JOIN EKKO AS B ON B~EBELN = A~EBELN
                     INNER JOIN VBAK AS C ON C~BSTNK = A~EBELN
                                         AND C~VBTYP = 'C'
                     INNER JOIN VBAP AS K ON K~VBELN = C~VBELN
                     INNER JOIN LIPS AS D ON D~VGBEL = C~VBELN
                                         AND D~VGPOS = K~POSNR
                                         AND A~MATNR = D~MATNR
                     LEFT OUTER JOIN ZCCMM_BATCHFIND AS E
                           ON D~MATNR = E~MATNR
                          AND D~CHARG = E~BATCH
                     INNER JOIN MARC AS F
                           ON D~MATNR = F~MATNR
                          AND A~WERKS = F~WERKS
                   INNER JOIN MARA AS G
                           ON D~MATNR = G~MATNR
                   INNER JOIN T001W AS H
                           ON A~WERKS = H~WERKS
                   INNER JOIN MAKT AS I
                           ON D~MATNR = I~MATNR
                          AND I~SPRAS = @SY-LANGU
                   INNER JOIN T001L AS J
                           ON A~WERKS = J~WERKS
                          AND A~LGORT = J~LGORT
*     FOR ALL ENTRIES IN @LT_DATA
      WHERE  A~WERKS IN @S_WERKS
        AND  A~LGORT IN @S_LGORT
        AND  A~MATNR IN @S_MATNR
        AND  B~BUKRS EQ @P_BUKRS
        AND  B~LIFNR IN @GR_VENDOR_INFO
        AND  D~CHARG IN @S_CHARG
        AND  F~EKGRP IN @S_EKGRP
        AND  F~DISPO IN @S_DISPO
        AND  G~MATKL IN @S_MATKL
        AND  B~BSAKZ = @SPACE
        AND  A~LOEKZ = @SPACE
        AND  A~ELIKZ = @SPACE
        AND  D~LFIMG > 0
      INTO TABLE @GT_MDBS.

    IF GT_MDBS[] IS NOT INITIAL.

      LOOP AT GT_MDBS INTO DATA(LS_MDBS).
        LV_EBELP = LS_MDBS-POSEX+1(5).
        IF LS_MDBS-EBELP NE LV_EBELP.
          DELETE TABLE GT_MDBS FROM LS_MDBS.
        ENDIF.

        CLEAR LV_EBELP.
      ENDLOOP.

      SELECT VBELN_IM, VBELP_IM, MATNR, CHARG, ERFMG, ERFME
        FROM MATDOC
        FOR ALL ENTRIES IN @GT_MDBS
       WHERE VBELN_IM = @GT_MDBS-VBELN  " DO NO
         AND VBELP_IM = @GT_MDBS-POSNR  " DO ITEM
         AND MATNR    = @GT_MDBS-MATNR
         AND CHARG    = @GT_MDBS-CHARG
         AND CANCELLED = @SPACE
         AND CANCELLATION_TYPE = @SPACE
        INTO TABLE @DATA(LT_GI_DATA).

      SORT LT_GI_DATA BY VBELN_IM VBELP_IM MATNR CHARG.
      CLEAR LS_MDBS.
      LOOP AT GT_MDBS INTO LS_MDBS.
        READ TABLE LT_GI_DATA INTO DATA(LS_GI_DATA) WITH KEY VBELN_IM = LS_MDBS-VBELN
                                                             VBELP_IM = LS_MDBS-POSNR
                                                             MATNR    = LS_MDBS-MATNR
                                                             CHARG    = LS_MDBS-CHARG  BINARY SEARCH.
        IF SY-SUBRC NE 0.
          DELETE GT_MDBS.
        ENDIF.
        CLEAR LS_MDBS.
      ENDLOOP.

    ENDIF.

    FREE: LT_DATA.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STOCKINFO
*&---------------------------------------------------------------------*
FORM SET_STOCKINFO USING IS_DATA LIKE LINE OF GT_DATA
                          IT_VBBE_SUM   LIKE GT_VBBE "TABLE OF GS_VBBE
                          IT_RESB_SUM   LIKE GT_RESB "TABLE OF GS_RESB
                          IT_RESB_SUM_B LIKE GT_RESB "TABLE OF GS_RESB
                          IT_MDUB_SUM   LIKE GT_MDUB_SUM "TABLE OF GS_MDUB
                          IT_GRINFO_SUM   LIKE GT_GRINFO "TABLE OF GS_GRINFO
                 CHANGING CS_DISP LIKE LINE OF GT_DISP.

* 납품예정 및 미처리 예약 수량 정보를 READ 후 예약재고 및 실 가용재고 정보를 구성 한다..

  IF IS_DATA-BATCH IS INITIAL AND IS_DATA-BWTAR IS NOT INITIAL.
    READ TABLE IT_VBBE_SUM INTO DATA(LS_VBBE) WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 CHARG = IS_DATA-BWTAR
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      DATA(LV_OMENG) = LS_VBBE-OMENG.
    ENDIF.

    READ TABLE IT_RESB_SUM_B INTO DATA(LS_RESB) WITH KEY WERKS = IS_DATA-PLANT
                                                   LGORT = IS_DATA-SLOCATION
                                                   MATNR = IS_DATA-MATERIAL
                                                   BWTAR = IS_DATA-BWTAR
                                                   BINARY SEARCH.
    IF SY-SUBRC = 0.
      DATA(LV_BDMNG) = LS_RESB-BDMNG.
    ENDIF.

** 운송중 재고 로직 추가
    READ TABLE IT_MDUB_SUM INTO DATA(LS_MDUB) WITH KEY   WERKS = IS_DATA-PLANT
                                                   LGORT = IS_DATA-SLOCATION
                                                   MATNR = IS_DATA-MATERIAL
                                                   CHARG = IS_DATA-BWTAR
                                                   BINARY SEARCH.
    IF SY-SUBRC = 0.
      DATA(LV_GLMNG) = LS_MDUB-LFIMG.   "운송중재고
    ENDIF.

    READ TABLE IT_GRINFO_SUM INTO DATA(LS_GRINFO) WITH KEY WERKS = IS_DATA-PLANT
                                                       LGORT = IS_DATA-SLOCATION
                                                       MATNR = IS_DATA-MATERIAL
                                                       CHARG = IS_DATA-BWTAR
                                                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      DATA(LV_WEMNG) = LS_GRINFO-WEMNG.   "입고예정
    ENDIF.



  ELSEIF IS_DATA-BATCH IS NOT INITIAL AND IS_DATA-BWTAR IS INITIAL.
    READ TABLE IT_VBBE_SUM INTO LS_VBBE WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 CHARG = IS_DATA-BATCH
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_OMENG = LS_VBBE-OMENG.
    ENDIF.

    READ TABLE IT_RESB_SUM INTO LS_RESB WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 CHARG = IS_DATA-BATCH
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_BDMNG = LS_RESB-BDMNG.
    ENDIF.


    READ TABLE IT_MDUB_SUM INTO LS_MDUB WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 CHARG = IS_DATA-BATCH
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_GLMNG = LS_MDUB-LFIMG.    "운송중재고
    ENDIF.

    READ TABLE IT_GRINFO_SUM INTO LS_GRINFO WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 CHARG = IS_DATA-BATCH
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_WEMNG = LS_GRINFO-WEMNG.  "입고예정
    ENDIF.

  ELSE.
    READ TABLE IT_VBBE_SUM INTO LS_VBBE WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_OMENG = LS_VBBE-OMENG.
    ENDIF.

    READ TABLE IT_RESB_SUM INTO LS_RESB WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_BDMNG = LS_RESB-BDMNG.
    ENDIF.


    READ TABLE IT_MDUB_SUM INTO LS_MDUB WITH KEY WERKS = IS_DATA-PLANT
                                                 LGORT = IS_DATA-SLOCATION
                                                 MATNR = IS_DATA-MATERIAL
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_GLMNG = LS_MDUB-LFIMG.    "운송중재고
    ENDIF.

    READ TABLE IT_GRINFO_SUM INTO LS_GRINFO WITH KEY WERKS = IS_DATA-PLANT
                                                     LGORT = IS_DATA-SLOCATION
                                                     MATNR = IS_DATA-MATERIAL
                                                     BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_WEMNG = LS_GRINFO-WEMNG.  "입고예정
    ENDIF.


  ENDIF.

  CS_DISP-WEMNG = LV_WEMNG.
  CS_DISP-GLMNG = LV_GLMNG.
  CS_DISP-BDMNG = LV_BDMNG + LV_OMENG.
  CS_DISP-AVE_STOCK = CS_DISP-AVAILABLESTOCK - CS_DISP-BDMNG.
  WRITE : CS_DISP-AVE_STOCK TO CS_DISP-AVE_STOCK_C UNIT CS_DISP-MEINS.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = CS_DISP-AVE_STOCK_C.


  CLEAR : LV_BDMNG, LV_OMENG,  LV_GLMNG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHANGE_STOCK_QI_AV.

  DATA : LS_GOODSMVT_HEADER  LIKE BAPI2017_GM_HEAD_01,
         LS_GOODSMVT_CODE    LIKE BAPI2017_GM_CODE,
         LV_MATERIALDOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
         LV_MATDOCUMENTYEAR  TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
         LS_GOODSMVT_ITEM    TYPE BAPI2017_GM_ITEM_CREATE,
         LT_GOODSMVT_ITEM    TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
         LT_RETURN           TYPE TABLE OF BAPIRET2.

  IF GS_CHANGE-AVAILABLESTOCK IS INITIAL.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M03. EXIT. "변경할 수량을 입력하세요.

  ELSE.
    IF GS_CHANGE-QISTOCK - GS_CHANGE-AVAILABLESTOCK < 0.
      MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M04.  EXIT.  "재고보다 변경수량이 많습니다.
    ENDIF.
  ENDIF.

  CLEAR : LS_GOODSMVT_HEADER, LS_GOODSMVT_CODE, LS_GOODSMVT_ITEM,
          LV_MATERIALDOCUMENT, LV_MATDOCUMENTYEAR.

  _G_INIT : LT_GOODSMVT_ITEM, LT_RETURN.

  IF GS_DISP IS NOT INITIAL.

*- 공통
    IF GS_CHANGE-ZDATE IS NOT INITIAL.
      LS_GOODSMVT_HEADER-PSTNG_DATE = GS_CHANGE-ZDATE.
    ELSE.
      LS_GOODSMVT_HEADER-PSTNG_DATE = SY-DATLO.
    ENDIF.

    LS_GOODSMVT_HEADER-HEADER_TXT = TEXT-T11 && SY-UNAME && ')'.   "품질-->가용 상태 변경(사용자 : SY-UNAME)

    LS_GOODSMVT_CODE-GM_CODE    = '04'.

    LS_GOODSMVT_ITEM-MATERIAL_LONG    = GS_DISP-MATERIAL.
    LS_GOODSMVT_ITEM-PLANT            = GS_DISP-PLANT.
    LS_GOODSMVT_ITEM-BATCH            = GS_DISP-BATCH.
    LS_GOODSMVT_ITEM-MOVE_TYPE        = '321'.
    LS_GOODSMVT_ITEM-SPEC_STOCK        = 'O'.
    LS_GOODSMVT_ITEM-VENDOR         = GS_DISP-BP.
    LS_GOODSMVT_ITEM-ENTRY_UOM        = GS_DISP-MEINS.

    LS_GOODSMVT_ITEM-ENTRY_QNT        = GS_CHANGE-AVAILABLESTOCK.

    APPEND LS_GOODSMVT_ITEM TO LT_GOODSMVT_ITEM. CLEAR : LS_GOODSMVT_ITEM.


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        GOODSMVT_HEADER  = LS_GOODSMVT_HEADER
        GOODSMVT_CODE    = LS_GOODSMVT_CODE
      IMPORTING
        MATERIALDOCUMENT = LV_MATERIALDOCUMENT
*       MATDOCUMENTYEAR  = LV_MATDOCUMENTYEAR
      TABLES
        GOODSMVT_ITEM    = LT_GOODSMVT_ITEM
        RETURN           = LT_RETURN.

    IF LV_MATERIALDOCUMENT IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      IF SY-SUBRC = 0.
        MESSAGE S000 DISPLAY LIKE 'S' WITH TEXT-M06.  "재고 상태 변경을 완료했습니다.
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      IF SY-SUBRC = 0.
        MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M05.  "재고 상태 변경 중 오류가 발생했습니다.
      ENDIF.

    ENDIF.

*-----------------------------
* Refresh
*-----------------------------
    PERFORM GET_DATA. "Refresh

    GRF_GRID->REFRESH_GRID_DISPLAY( ).

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_STOCK_AV_QI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHANGE_STOCK_AV_QI .


  DATA : LS_GOODSMVT_HEADER  LIKE BAPI2017_GM_HEAD_01,
         LS_GOODSMVT_CODE    LIKE BAPI2017_GM_CODE,
         LV_MATERIALDOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
         LV_MATDOCUMENTYEAR  TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
         LS_GOODSMVT_ITEM    TYPE BAPI2017_GM_ITEM_CREATE,
         LT_GOODSMVT_ITEM    TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
         LT_RETURN           TYPE TABLE OF BAPIRET2.

  IF GS_CHANGE-QISTOCK IS INITIAL.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M03. EXIT. "변경할 수량을 입력하세요.

  ELSE.
    IF GS_CHANGE-AVAILABLESTOCK - GS_CHANGE-QISTOCK < 0.
      MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M04.  EXIT.  "재고보다 변경수량이 많습니다.
    ENDIF.
  ENDIF.

  CLEAR : LS_GOODSMVT_HEADER, LS_GOODSMVT_CODE, LS_GOODSMVT_ITEM,
          LV_MATERIALDOCUMENT, LV_MATDOCUMENTYEAR.

  _G_INIT : LT_GOODSMVT_ITEM, LT_RETURN.

  IF GS_DISP IS NOT INITIAL.

*- 공통
    IF GS_CHANGE-ZDATE IS NOT INITIAL.
      LS_GOODSMVT_HEADER-PSTNG_DATE = GS_CHANGE-ZDATE.
    ELSE.
      LS_GOODSMVT_HEADER-PSTNG_DATE = SY-DATLO.
    ENDIF.

    LS_GOODSMVT_HEADER-HEADER_TXT = TEXT-T12 && SY-UNAME && ')'.   "가용-->품질 상태 변경(사용자 : SY-UNAME)

    LS_GOODSMVT_CODE-GM_CODE    = '04'.

    LS_GOODSMVT_ITEM-MATERIAL_LONG    = GS_DISP-MATERIAL.
    LS_GOODSMVT_ITEM-PLANT            = GS_DISP-PLANT.
    LS_GOODSMVT_ITEM-BATCH            = GS_DISP-BATCH.
    LS_GOODSMVT_ITEM-MOVE_TYPE        = '322'.
    LS_GOODSMVT_ITEM-SPEC_STOCK        = 'O'.
    LS_GOODSMVT_ITEM-VENDOR         = GS_DISP-BP.
    LS_GOODSMVT_ITEM-ENTRY_UOM        = GS_DISP-MEINS.

    LS_GOODSMVT_ITEM-ENTRY_QNT        = GS_CHANGE-QISTOCK.

    APPEND LS_GOODSMVT_ITEM TO LT_GOODSMVT_ITEM. CLEAR : LS_GOODSMVT_ITEM.


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        GOODSMVT_HEADER  = LS_GOODSMVT_HEADER
        GOODSMVT_CODE    = LS_GOODSMVT_CODE
      IMPORTING
        MATERIALDOCUMENT = LV_MATERIALDOCUMENT
*       MATDOCUMENTYEAR  = LV_MATDOCUMENTYEAR
      TABLES
        GOODSMVT_ITEM    = LT_GOODSMVT_ITEM
        RETURN           = LT_RETURN.

    IF LV_MATERIALDOCUMENT IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      IF SY-SUBRC = 0.
        MESSAGE S000 DISPLAY LIKE 'S' WITH TEXT-M06.  "재고 상태 변경을 완료했습니다.
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      IF SY-SUBRC = 0.
        MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M05.  "재고 상태 변경 중 오류가 발생했습니다.
      ENDIF.

    ENDIF.

*-----------------------------
* Refresh
*-----------------------------
    PERFORM GET_DATA. "Refresh

    GRF_GRID->REFRESH_GRID_DISPLAY( ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETTING_TYPE_KOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_TYPE
*&      <-- LS_DISP_TYPE_KOR
*&---------------------------------------------------------------------*
FORM SETTING_TYPE_KOR USING IV_TYPE
                       CHANGING CV_TYPE_KOR.

  IF IV_TYPE = GC_COND_SL.
    CV_TYPE_KOR = TEXT-C36.

  ELSEIF IV_TYPE = GC_COND_SC.
    CV_TYPE_KOR = TEXT-C37.

  ELSEIF IV_TYPE = GC_COND_CS.
    CV_TYPE_KOR = TEXT-C38.

  ELSEIF IV_TYPE = GC_COND_SO.
    CV_TYPE_KOR = TEXT-C39.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CBO_TABLE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_11030
*&      --> LT_11010T
*&---------------------------------------------------------------------*
FORM GET_CBO_TABLE_DATA TABLES PT_11030
                                  PT_11010T.

  DATA(LT_DATA) = GT_DATA[].
  SORT LT_DATA BY MATERIAL PLANT SLOCATION.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING MATERIAL PLANT SLOCATION.

*  IF LT_DATA[] IS NOT INITIAL.
*
*    SELECT MATNR, WERKS, LGORT, LGPBE
*      FROM ZTMD11030
*       FOR ALL ENTRIES IN @LT_DATA
*     WHERE MATNR = @LT_DATA-MATERIAL
*       AND WERKS = @LT_DATA-PLANT
*       AND LGORT = @LT_DATA-SLOCATION
*      INTO TABLE @PT_11030.
*
*    FREE LT_DATA.
*
*  ENDIF.
*
*  LT_DATA[] = GT_DATA[].
*  SORT LT_DATA BY MATERIAL.
*  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING MATERIAL.
*
*  IF LT_DATA[] IS NOT INITIAL.
*
*    SELECT MATNR, ZZNAME
*      FROM ZTMD11010T
*       FOR ALL ENTRIES IN @LT_DATA
*     WHERE MATNR = @LT_DATA-MATERIAL
*       AND SPRAS = @SY-LANGU
*      INTO TABLE @PT_11010T.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_MDUB_MDBS_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_MDUB_MDBS_INFO USING IT_MDUB_SUM LIKE GT_MDUB_SUM "TABLE OF GS_MDUB
                             IT_GRINFO_SUM   LIKE GT_GRINFO. "TABLE OF GS_GRINFO


  DATA : LS_TEMP  LIKE LINE OF GT_DATA,
         LV_DATLO TYPE SY-DATLO.

  LV_DATLO = SY-DATLO.

  SORT GT_MDUB BY WERKS LGORT MATNR CHARG.
  LOOP AT IT_MDUB_SUM INTO DATA(LS_MDUB_SUM).
    READ TABLE GT_DATA INTO DATA(LS_DATA1) WITH KEY PLANT     = LS_MDUB_SUM-WERKS
                                                    SLOCATION = LS_MDUB_SUM-LGORT
                                                    MATERIAL  = LS_MDUB_SUM-MATNR
                                                    BATCH     = LS_MDUB_SUM-CHARG.
    IF SY-SUBRC NE 0.
      LS_TEMP-PLANT         = LS_MDUB_SUM-WERKS.
      LS_TEMP-SLOCATION     = LS_MDUB_SUM-LGORT.
      LS_TEMP-MATERIAL      = LS_MDUB_SUM-MATNR.
      LS_TEMP-BATCH         = LS_MDUB_SUM-CHARG.
      LS_TEMP-GLMNG         = LS_MDUB_SUM-LFIMG.

      READ TABLE GT_MDUB INTO DATA(LS_MDUB) WITH KEY WERKS = LS_MDUB_SUM-WERKS
                                                     LGORT = LS_MDUB_SUM-LGORT
                                                     MATNR = LS_MDUB_SUM-MATNR
                                                     CHARG = LS_MDUB_SUM-CHARG.
      IF SY-SUBRC EQ 0.
        LS_TEMP-PRDDUCTLOT    = LS_MDUB-PRDDUCTLOT.
        LS_TEMP-PRDUCTIONDATE = LS_MDUB-PRODUCTIONDATE.
        LS_TEMP-EXPIREDDATE   = LS_MDUB-EXPIREDDATE.
        LS_TEMP-EKGRP         = LS_MDUB-EKGRP.
        LS_TEMP-DISPO         = LS_MDUB-DISPO.
        LS_TEMP-MEINS         = LS_MDUB-MEINS.
        LS_TEMP-MATKL         = LS_MDUB-MATKL.
        LS_TEMP-MATNAME       = LS_MDUB-MAKTX.
        LS_TEMP-PLANTNAME     = LS_MDUB-NAME1.
        LS_TEMP-SLNAME        = LS_MDUB-LGOBE.
        LS_TEMP-TYPE_KOR      = TEXT-C36.

        IF LS_TEMP-EXPIREDDATE IS NOT INITIAL OR LS_TEMP-EXPIREDDATE NE GC_00000000.
          LS_TEMP-REMAINDATE = LS_TEMP-EXPIREDDATE - LV_DATLO.
        ENDIF.


      ENDIF.

      APPEND LS_TEMP TO GT_DATA.
    ENDIF.

  ENDLOOP.

  CLEAR: LS_DATA1, LS_TEMP.

  SORT GT_MDBS BY WERKS LGORT MATNR CHARG.

  LOOP AT  IT_GRINFO_SUM INTO DATA(LS_GRINFO_SUM).
    READ TABLE GT_DATA INTO LS_DATA1 WITH KEY   PLANT     = LS_GRINFO_SUM-WERKS
                                                SLOCATION = LS_GRINFO_SUM-LGORT
                                                MATERIAL  = LS_GRINFO_SUM-MATNR
                                                BATCH     = LS_GRINFO_SUM-CHARG.
    IF SY-SUBRC NE 0.
      LS_TEMP-PLANT         = LS_GRINFO_SUM-WERKS.
      LS_TEMP-SLOCATION     = LS_GRINFO_SUM-LGORT.
      LS_TEMP-MATERIAL      = LS_GRINFO_SUM-MATNR.
      LS_TEMP-BATCH         = LS_GRINFO_SUM-CHARG.
      LS_TEMP-WEMNG         = LS_GRINFO_SUM-WEMNG.

      READ TABLE GT_MDBS INTO DATA(LS_MDBS) WITH KEY  WERKS = LS_GRINFO_SUM-WERKS
                                                      LGORT = LS_GRINFO_SUM-LGORT
                                                      MATNR = LS_GRINFO_SUM-MATNR
                                                      CHARG = LS_GRINFO_SUM-CHARG.
      IF SY-SUBRC EQ 0.
        LS_TEMP-PRDDUCTLOT    = LS_MDBS-PRDDUCTLOT.
        LS_TEMP-PRDUCTIONDATE = LS_MDBS-PRODUCTIONDATE.
        LS_TEMP-EXPIREDDATE   = LS_MDBS-EXPIREDDATE.
        LS_TEMP-EKGRP         = LS_MDBS-EKGRP.
        LS_TEMP-DISPO         = LS_MDBS-DISPO.
        LS_TEMP-MEINS         = LS_MDBS-MEINS.
        LS_TEMP-MATKL         = LS_MDBS-MATKL.
        LS_TEMP-MATNAME       = LS_MDBS-MAKTX.
        LS_TEMP-PLANTNAME     = LS_MDBS-NAME1.
        LS_TEMP-SLNAME        = LS_MDBS-LGOBE.
        LS_TEMP-TYPE_KOR      = TEXT-C36.


        IF LS_TEMP-EXPIREDDATE IS NOT INITIAL OR LS_TEMP-EXPIREDDATE NE GC_00000000.
          LS_TEMP-REMAINDATE = LS_TEMP-EXPIREDDATE - LV_DATLO.
        ENDIF.

      ENDIF.

      APPEND LS_TEMP TO GT_DATA.
    ENDIF.

  ENDLOOP.


  CLEAR: LS_DATA1, LS_TEMP.



ENDFORM.
