FUNCTION ZFMM_INFORECORD_MAINTAIN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EV_INFNR) TYPE  EINE-INFNR
*"  TABLES
*"      IT_EINA STRUCTURE  ZSMM_INFOR_MAINT_EINA
*"      IT_EINAX STRUCTURE  ZSMM_INFOR_MAINT_EINAX
*"      IT_EINE STRUCTURE  ZSMM_INFOR_MAINT_EINE OPTIONAL
*"      IT_EINEX STRUCTURE  ZSMM_INFOR_MAINT_EINEX OPTIONAL
*"      IT_TXT_LINES STRUCTURE  ZSMM_INFOR_MAINT_TXT_LINES OPTIONAL
*"      IT_COND_VALIDITY STRUCTURE  ZSMM_INFOR_MAINT_COND_VALIDITY
*"       OPTIONAL
*"      IT_CONDITION STRUCTURE  ZSMM_INFOR_MAINT_CONDITION OPTIONAL
*"      ET_RETURN TYPE  FS4MIG_T_BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------


  DATA: LT_TMP      TYPE TABLE OF EINA,
        LT_TMP_EINE TYPE TABLE OF EINE.

  CLEAR: GT_EINA_OLD, GT_EINE_OLD, GS_MODE, GT_MODE.

  LT_TMP[] = CORRESPONDING #( IT_EINA[] ).
  LT_TMP_EINE[] = CORRESPONDING #( IT_EINE[] ).

  SORT LT_TMP BY INFNR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING INFNR.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT INFNR, LIFNR, MATNR
      FROM EINA
      FOR ALL ENTRIES IN @LT_TMP
     WHERE INFNR = @LT_TMP-INFNR
      INTO CORRESPONDING FIELDS OF TABLE @GT_EINA_OLD.
    FREE LT_TMP.
    SORT GT_EINA_OLD BY INFNR.
  ENDIF.

  SORT LT_TMP_EINE BY INFNR EKORG ESOKZ WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EINE COMPARING INFNR EKORG ESOKZ WERKS.
  IF NOT LT_TMP_EINE[] IS INITIAL.
    SELECT INFNR, EKORG, ESOKZ, WERKS, EKGRP
      FROM EINE
      FOR ALL ENTRIES IN @LT_TMP_EINE
     WHERE INFNR = @LT_TMP_EINE-INFNR
       AND EKORG = @LT_TMP_EINE-EKORG
       AND ESOKZ = @LT_TMP_EINE-ESOKZ
       AND WERKS = @LT_TMP_EINE-WERKS
      INTO CORRESPONDING FIELDS OF TABLE @GT_EINE_OLD.
    FREE LT_TMP_EINE.
    SORT GT_EINE_OLD BY INFNR EKORG ESOKZ WERKS.
  ENDIF.

**********************************************************************
*> General Data SET
**********************************************************************
  DATA: LT_BAPI_T_EINA  TYPE MEWIEINA_MIG_T,
        LT_BAPI_T_EINAX TYPE MEWIEINAX_T.

  PERFORM SET_EINA_DATA TABLES IT_EINA
                               LT_BAPI_T_EINA.

  PERFORM SET_EINAX_DATA TABLES IT_EINAX
                               LT_BAPI_T_EINAX.

**********************************************************************
*> Purch. Info SET
**********************************************************************
  DATA: LT_BAPI_T_EINE  TYPE MEWIEINE_T,
        LT_BAPI_T_EINEX TYPE MEWIEINEX_T.

  PERFORM SET_EINE_DATA TABLES IT_EINE
                               LT_BAPI_T_EINE.

  PERFORM SET_EINEX_DATA TABLES IT_EINEX
                               LT_BAPI_T_EINEX.

**********************************************************************
*> Text Lines
**********************************************************************
  DATA: LT_BAPI_TXT_LINES  TYPE MEWIPIRTEXT_TT.

  PERFORM SET_TXT_LINES TABLES IT_TXT_LINES
                               LT_BAPI_TXT_LINES.

**********************************************************************
*> COND VALIDITY
**********************************************************************
  DATA: LT_BAPI_COND_VALIDITY  TYPE MEWIVALIDITY_TT.

  PERFORM SET_COND_VALIDITY TABLES IT_COND_VALIDITY
                               LT_BAPI_COND_VALIDITY.

**********************************************************************
*> CONDITION
**********************************************************************
  DATA: LT_BAPI_CONDITION  TYPE MEWICONDITION_TT.

  PERFORM SET_CONDITION TABLES IT_CONDITION
                               LT_BAPI_CONDITION.

**********************************************************************
*> BAPI 실행.
**********************************************************************
  DATA: LT_RST_EINA TYPE MEWIEINA_MIG_T,
        LT_RST_EINE TYPE MEWIEINE_T.

  CALL FUNCTION 'ME_INFORECORD_MAINTAIN_MULTI'
    EXPORTING
      TESTRUN       = SPACE
    IMPORTING
      ET_EINA       = LT_RST_EINA
      ET_EINE       = LT_RST_EINE
    TABLES
      T_EINA        = LT_BAPI_T_EINA
      T_EINAX       = LT_BAPI_T_EINAX
      T_EINE        = LT_BAPI_T_EINE
      T_EINEX       = LT_BAPI_T_EINEX
      TXT_LINES     = LT_BAPI_TXT_LINES
      COND_VALIDITY = LT_BAPI_COND_VALIDITY
      CONDITION     = LT_BAPI_CONDITION
      RETURN        = ET_RETURN.

  SORT ET_RETURN BY TYPE.
  READ TABLE ET_RETURN WITH KEY TYPE = 'E'
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    DATA: LS_RETURN_COMMIT TYPE BAPIRET2.

    IF NOT LT_RST_EINA[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        IMPORTING
          RETURN = LS_RETURN_COMMIT.

      READ TABLE LT_RST_EINA INTO DATA(LS_RST_EINA) INDEX 1.
      EV_INFNR = LS_RST_EINA-INFO_REC.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFUNCTION.
