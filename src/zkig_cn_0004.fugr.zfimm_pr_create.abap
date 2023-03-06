FUNCTION ZFIMM_PR_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_PRHEADER) TYPE  ZSMM_PRHEADER OPTIONAL
*"     VALUE(IS_PRHEADERX) TYPE  ZSMM_PRHEADERX OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_BANFN) TYPE  BANFN
*"  TABLES
*"      IT_PRHEADERTEXT STRUCTURE  ZSMM_PRHEADERTEXT OPTIONAL
*"      IT_PRITEM STRUCTURE  ZSMM_PRITEM
*"      IT_PRITEMX STRUCTURE  ZSMM_PRITEMX OPTIONAL
*"      IT_PRACCOUNT STRUCTURE  ZSMM_PRACCOUNT OPTIONAL
*"      IT_PRACCOUNTX STRUCTURE  ZSMM_PRACCOUNTX OPTIONAL
*"      IT_PRADDRDELIVERY STRUCTURE  ZSMM_PRADDRDELIVERY OPTIONAL
*"      IT_PRCOMPONENTS STRUCTURE  ZSMM_PRCOMPONENTS OPTIONAL
*"      IT_PRCOMPONENTSX STRUCTURE  ZSMM_PRCOMPONENTSX OPTIONAL
*"      IT_PRITEMTEXT STRUCTURE  BAPIMEREQITEMTEXT OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  CLEAR GT_RETURN.

**********************************************************************
*> HEADER SET
**********************************************************************
  DATA: LS_PR_BAPI_HEADER  TYPE BAPIMEREQHEADER,
        LS_PR_BAPI_HEADERX TYPE BAPIMEREQHEADERX.

  PERFORM SET_HEADER_DATA USING IS_PRHEADER
                                IS_PRHEADERX
                          CHANGING LS_PR_BAPI_HEADER
                                   LS_PR_BAPI_HEADERX.

**********************************************************************
*> HEADER TEXT
**********************************************************************
  DATA: LT_PR_BAPI_HEADTEXT TYPE TABLE OF BAPIMEREQHEADTEXT.

  PERFORM SET_HEADER_TEXT TABLES IT_PRHEADERTEXT
                                 LT_PR_BAPI_HEADTEXT.

**********************************************************************
*> ITEM SET
**********************************************************************
  DATA: LT_PR_BAPI_ITEM  TYPE TABLE OF BAPIMEREQITEMIMP,
        LT_PR_BAPI_ITEMX TYPE TABLE OF BAPIMEREQITEMX.

  PERFORM SET_ITEM_DATA TABLES IT_PRITEM
                               LT_PR_BAPI_ITEM.

  PERFORM SET_ITEMX_DATA TABLES IT_PRITEMX
                               LT_PR_BAPI_ITEMX.

**********************************************************************
*> ACCOUNT SET
**********************************************************************
  DATA: LT_PR_BAPI_ACC  LIKE TABLE OF BAPIMEREQACCOUNT,
        LT_PR_BAPI_ACCX LIKE TABLE OF BAPIMEREQACCOUNTX.

  PERFORM SET_ACCOUNT_DATA TABLES IT_PRACCOUNT
                                  LT_PR_BAPI_ACC.

  PERFORM SET_ACCOUNTX_DATA TABLES IT_PRACCOUNTX
                                  LT_PR_BAPI_ACCX.

**********************************************************************
*> DELIVERY ADDRESS
**********************************************************************
  DATA: LT_PR_BAPI_DEL_ADDR LIKE TABLE OF BAPIMERQADDRDELIVERY.

  PERFORM SET_DELIVERY_ADDR TABLES IT_PRADDRDELIVERY
                                   LT_PR_BAPI_DEL_ADDR.

**********************************************************************
*> EXTENSION (ITEM 추가 필드) -> 미정..
**********************************************************************
  DATA: LT_PR_BAPI_EXTENSIONIN TYPE STANDARD TABLE OF BAPIPAREX.

  PERFORM SET_EXTENSION TABLES IT_PRITEM
                               IT_PRITEMX
                               LT_PR_BAPI_EXTENSIONIN.

**********************************************************************
*> COMPONENTS
**********************************************************************
  DATA: LT_PR_BAPI_COMPONENT  LIKE TABLE OF BAPIMEREQCOMPONENT,
        LT_PR_BAPI_COMPONENTX LIKE TABLE OF BAPIMEREQCOMPONENTX.

  PERFORM SET_COMPONENTS_DATA TABLES IT_PRCOMPONENTS
                                     LT_PR_BAPI_COMPONENT.

  PERFORM SET_COMPONENTSX_DATA TABLES IT_PRCOMPONENTSX
                                     LT_PR_BAPI_COMPONENTX.

**********************************************************************
*> BAPI 실행.
**********************************************************************
*> BAPI 수행 전 이미 Error 가 존재하는 경우 BAPI 수행하지 않음.
  IF NOT GT_RETURN[] IS INITIAL.
    ET_RETURN[] = GT_RETURN[].
    EXIT.
  ENDIF.

  DATA LV_PR_NO TYPE BAPIMEREQHEADER-PREQ_NO.

  DATA LT_RETURN TYPE TABLE OF BAPIRET2.

  CALL FUNCTION 'BAPI_PR_CREATE'
    EXPORTING
      PRHEADER       = LS_PR_BAPI_HEADER
      PRHEADERX      = LS_PR_BAPI_HEADERX
* TESTRUN =
    IMPORTING
      NUMBER         = LV_PR_NO
* PRHEADEREXP =
    TABLES
      RETURN         = LT_RETURN
      PRITEM         = LT_PR_BAPI_ITEM
      PRITEMX        = LT_PR_BAPI_ITEMX
* PRITEMEXP =
* PRITEMSOURCE =
      PRACCOUNT      = LT_PR_BAPI_ACC
* PRACCOUNTPROITSEGMENT =
      PRACCOUNTX     = LT_PR_BAPI_ACCX
      PRADDRDELIVERY = LT_PR_BAPI_DEL_ADDR
      PRITEMTEXT     = IT_PRITEMTEXT          "BAPI 와 동일한 구조로 적용
      PRHEADERTEXT   = LT_PR_BAPI_HEADTEXT
      EXTENSIONIN    = LT_PR_BAPI_EXTENSIONIN
* EXTENSIONOUT =
* PRVERSION =
* PRVERSIONX =
* ALLVERSIONS =
      PRCOMPONENTS   = LT_PR_BAPI_COMPONENT
      PRCOMPONENTSX  = LT_PR_BAPI_COMPONENTX
* SERVICEOUTLINE =
* SERVICEOUTLINEX =
* SERVICELINES =
* SERVICELINESX =
* SERVICELIMIT =
* SERVICELIMITX =
* SERVICECONTRACTLIMITS =
* SERVICECONTRACTLIMITSX =
* SERVICEACCOUNT =
* SERVICEACCOUNTX =
* SERVICELONGTEXTS =
* SERIALNUMBER =
* SERIALNUMBERX =
    .

  SORT LT_RETURN BY TYPE.
  READ TABLE LT_RETURN WITH KEY TYPE = 'E'
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
  IF NOT LV_PR_NO IS INITIAL AND SY-SUBRC NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING WAIT = 'X'.

**********************************************************************
*> CBO 저장 - ZTMM30010
**********************************************************************
    DATA: LV_SUBRC TYPE SY-SUBRC.

    PERFORM SAVE_CBO_DATA USING LV_PR_NO
                                IS_PRHEADER
                                IS_PRHEADERX
                          CHANGING LV_SUBRC.

    IF LV_SUBRC EQ 0.

      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

    EV_BANFN = LV_PR_NO.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ET_RETURN[] = LT_RETURN[].
  ENDIF.

ENDFUNCTION.
