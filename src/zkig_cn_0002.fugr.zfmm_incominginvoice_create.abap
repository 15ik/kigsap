FUNCTION ZFMM_INCOMINGINVOICE_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_HEADERDATA) TYPE  ZSMM_INCINV_CREATE_HEADER
*"  EXPORTING
*"     VALUE(ES_INVOICEDOCNUMBER) TYPE  ZSMM_INCINV_CREATE_RESULT
*"  TABLES
*"      IT_TAXDATA STRUCTURE  ZSMM_INCINV_CREATE_TAX OPTIONAL
*"      IT_VENDORITEMSPLITDATA STRUCTURE
*"        ZSMM_INCINV_CREATE_VENDORSPLIT OPTIONAL
*"      IT_ITEMDATA STRUCTURE  ZSMM_INCINV_CREATE_ITEM OPTIONAL
*"      IT_GLACCOUNTDATA STRUCTURE  ZSMM_INCINV_CREATE_GL_ACCOUNT
*"       OPTIONAL
*"      IT_MATERIALDATA STRUCTURE  ZSMM_INCINV_CREATE_MATERIAL OPTIONAL
*"      IT_ACCOUNTINGDATA STRUCTURE  ZSMM_INCINV_CREATE_ACCOUNT
*"       OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------*
*& FUNCTION ZFMM_INCOMINGINVOICE_CREATE.
*&--------------------------------------------------------------------*
*&
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& Module : MM
*& Program ID : ZFMM_INCOMINGINVOICE_CREATE
*& T-CODE : N/A
*& Referenced Program : N/A
*& Created by : N/A
*& Created On : N/A
*& Type : NFC
*& Description : 송장생성 FC
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ 변경자 변경일자 변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*

**********************************************************************
*> SET HEADER DATA
**********************************************************************
* Global data declarations

  DATA: LS_BAPI_HEADERDATA LIKE BAPI_INCINV_CREATE_HEADER.

  CLEAR LS_BAPI_HEADERDATA.
  MOVE-CORRESPONDING IS_HEADERDATA TO LS_BAPI_HEADERDATA.

  CASE IS_HEADERDATA-INVOICE_TYPE.
    WHEN 'IV'.    "Invoce
      LS_BAPI_HEADERDATA-INVOICE_IND = 'X'.
      LS_BAPI_HEADERDATA-DE_CRE_IND = SPACE.
    WHEN 'CM'.    "Credit Memo
      LS_BAPI_HEADERDATA-INVOICE_IND = SPACE.
      LS_BAPI_HEADERDATA-DE_CRE_IND = SPACE.
    WHEN 'S_IV'.  "차후차변
      LS_BAPI_HEADERDATA-INVOICE_IND = 'X'.
      LS_BAPI_HEADERDATA-DE_CRE_IND = 'X'.
    WHEN 'S_CM'. "차후대변
      LS_BAPI_HEADERDATA-INVOICE_IND = SPACE.
      LS_BAPI_HEADERDATA-DE_CRE_IND = 'X'.
    WHEN OTHERS. "Nomal..
  ENDCASE.

  "외화이고 회사코드가 한국이면 지급방법 'F' (FI 요청사항)
  IF LS_BAPI_HEADERDATA-CURRENCY NE GC_WAERS_KRW AND
     ( LS_BAPI_HEADERDATA-COMP_CODE = GC_BUKRS_1101 OR
     LS_BAPI_HEADERDATA-COMP_CODE = GC_BUKRS_2101 OR
     LS_BAPI_HEADERDATA-COMP_CODE = GC_BUKRS_3101 ).

    LS_BAPI_HEADERDATA-PYMT_METH = 'F'.

  ENDIF.

**********************************************************************
*> SET ITEM DATA
**********************************************************************
  DATA: LT_BAPI_ITEMDATA LIKE TABLE OF BAPI_INCINV_CREATE_ITEM,
        LS_BAPI_ITEMDATA LIKE BAPI_INCINV_CREATE_ITEM.

  CLEAR LT_BAPI_ITEMDATA.
  LOOP AT IT_ITEMDATA INTO DATA(LS_ITEMDATA).
    CLEAR LS_BAPI_ITEMDATA.
    MOVE-CORRESPONDING LS_ITEMDATA TO LS_BAPI_ITEMDATA.
    APPEND LS_BAPI_ITEMDATA TO LT_BAPI_ITEMDATA.
  ENDLOOP.

*(+)U1 start
**********************************************************************
*> SET ACCOUNTING DATA
**********************************************************************
  DATA : LT_BAPI_ACCOUNTINGDATA LIKE TABLE OF BAPI_INCINV_CREATE_ACCOUNT,
         LS_BAPI_ACCOUNTINGDATA LIKE  BAPI_INCINV_CREATE_ACCOUNT.

  CLEAR : LT_BAPI_ACCOUNTINGDATA.
  LOOP AT IT_ACCOUNTINGDATA  INTO DATA(LS_ACCOUNTINGDATA).
    CLEAR LS_BAPI_ACCOUNTINGDATA.
    MOVE-CORRESPONDING LS_ACCOUNTINGDATA TO LS_BAPI_ACCOUNTINGDATA.
    APPEND LS_BAPI_ACCOUNTINGDATA TO LT_BAPI_ACCOUNTINGDATA.
  ENDLOOP.

*(+)U1 end

**********************************************************************
*> SET GL ACCOUNT DATA
**********************************************************************
  DATA: LT_BAPI_GLACCOUNTDATA LIKE TABLE OF BAPI_INCINV_CREATE_GL_ACCOUNT,
        LS_BAPI_GLACCOUNTDATA LIKE BAPI_INCINV_CREATE_GL_ACCOUNT.
  CLEAR LT_BAPI_GLACCOUNTDATA.
  LOOP AT IT_GLACCOUNTDATA INTO DATA(LS_GLACCOUNTDATA).
    CLEAR LS_BAPI_GLACCOUNTDATA.
    MOVE-CORRESPONDING LS_GLACCOUNTDATA TO LS_BAPI_GLACCOUNTDATA.

    _G_CONV_DATA_EAI_TO_SAP LS_GLACCOUNTDATA-GL_ACCOUNT '' LS_BAPI_GLACCOUNTDATA-GL_ACCOUNT.

    APPEND LS_BAPI_GLACCOUNTDATA TO LT_BAPI_GLACCOUNTDATA.
  ENDLOOP.

**********************************************************************
*> SET MATERIAL DATA
**********************************************************************
  DATA: LT_BAPI_MATERIALDATA LIKE TABLE OF BAPI_INCINV_CREATE_MATERIAL,
        LS_BAPI_MATERIALDATA LIKE BAPI_INCINV_CREATE_MATERIAL.

  CLEAR LT_BAPI_MATERIALDATA.
  LOOP AT IT_MATERIALDATA INTO DATA(LS_MATERIALDATA).
    CLEAR LS_BAPI_MATERIALDATA.
    MOVE-CORRESPONDING LS_MATERIALDATA TO LS_BAPI_MATERIALDATA.
    APPEND LS_BAPI_MATERIALDATA TO LT_BAPI_MATERIALDATA.
  ENDLOOP.

**********************************************************************
*> SET TAX DATA
**********************************************************************
  DATA: LT_BAPI_TAXDATA LIKE TABLE OF BAPI_INCINV_CREATE_TAX,
        LS_BAPI_TAXDATA LIKE BAPI_INCINV_CREATE_TAX.

  CLEAR LT_BAPI_TAXDATA.
  LOOP AT IT_TAXDATA INTO DATA(LS_TAXDATA).
    CLEAR LS_BAPI_TAXDATA.
    MOVE-CORRESPONDING LS_TAXDATA TO LS_BAPI_TAXDATA.
    APPEND LS_BAPI_TAXDATA TO LT_BAPI_TAXDATA.
  ENDLOOP.

**********************************************************************
*> SET VENDOR SPLIT DATA
**********************************************************************
  DATA: LT_BAPI_VENDORITEMSPLITDATA LIKE TABLE OF BAPI_INCINV_CREATE_VENDORSPLIT,
        LS_BAPI_VENDORITEMSPLITDATA LIKE BAPI_INCINV_CREATE_VENDORSPLIT,
        LV_SPLIT_KEY                TYPE BAPI_INCINV_CREATE_VENDORSPLIT-SPLIT_KEY.

  CLEAR LT_BAPI_VENDORITEMSPLITDATA.
  LOOP AT IT_VENDORITEMSPLITDATA INTO DATA(LS_VENDORITEMSPLITDATA).
    CLEAR LS_BAPI_VENDORITEMSPLITDATA.
    ADD 1 TO LV_SPLIT_KEY.
    MOVE-CORRESPONDING LS_VENDORITEMSPLITDATA TO LS_BAPI_VENDORITEMSPLITDATA.
    LS_BAPI_VENDORITEMSPLITDATA-SPLIT_KEY = LV_SPLIT_KEY.

    "외화이고 회사코드가 한국이면 지급방법 'F' (FI 요청사항)
    IF LS_BAPI_HEADERDATA-CURRENCY NE GC_WAERS_KRW AND
       ( LS_BAPI_HEADERDATA-COMP_CODE = GC_BUKRS_1101 OR
       LS_BAPI_HEADERDATA-COMP_CODE = GC_BUKRS_2101 OR
       LS_BAPI_HEADERDATA-COMP_CODE = GC_BUKRS_3101 ).

      LS_BAPI_VENDORITEMSPLITDATA-PYMT_METH = 'F'.
    ENDIF.

    APPEND LS_BAPI_VENDORITEMSPLITDATA TO LT_BAPI_VENDORITEMSPLITDATA.
  ENDLOOP.

**********************************************************************
*> BAPI 실행.
**********************************************************************
  DATA: LV_INVOICEDOCNUMBER TYPE BAPI_INCINV_FLD-INV_DOC_NO,
        LV_FISCALYEAR       TYPE BAPI_INCINV_FLD-FISC_YEAR.

  DATA: LT_BAPI_RETURN   TYPE TABLE OF BAPIRET2,
        LS_COMMIT_RETURN TYPE BAPIRET2.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
    EXPORTING
      HEADERDATA          = LS_BAPI_HEADERDATA
* ADDRESSDATA =
* INVOICESTATUS = '5'
    IMPORTING
      INVOICEDOCNUMBER    = LV_INVOICEDOCNUMBER
      FISCALYEAR          = LV_FISCALYEAR
    TABLES
      ITEMDATA            = LT_BAPI_ITEMDATA
      ACCOUNTINGDATA      = LT_BAPI_ACCOUNTINGDATA   "(+)U1
      GLACCOUNTDATA       = LT_BAPI_GLACCOUNTDATA
      MATERIALDATA        = LT_BAPI_MATERIALDATA
      TAXDATA             = LT_BAPI_TAXDATA
* WITHTAXDATA =
      VENDORITEMSPLITDATA = LT_BAPI_VENDORITEMSPLITDATA
      RETURN              = LT_BAPI_RETURN
* EXTENSIONIN =
* EXTENSIONOUT =
* TM_ITEMDATA =
* ASSETDATA =
    .


  SORT LT_BAPI_RETURN BY TYPE.
  READ TABLE LT_BAPI_RETURN WITH KEY TYPE = 'E'
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
  IF NOT LV_INVOICEDOCNUMBER IS INITIAL AND SY-SUBRC NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT   = 'X'
      IMPORTING
        RETURN = LS_COMMIT_RETURN.

**********************************************************************
*> CBO 저장 - ZTMM30010
**********************************************************************
    IF LS_COMMIT_RETURN-TYPE NE 'E'.
      COMMIT WORK.
    ELSE.
      APPEND LS_COMMIT_RETURN TO LT_BAPI_RETURN.
      ROLLBACK WORK.
    ENDIF.

    ES_INVOICEDOCNUMBER-INV_DOC_NO = LV_INVOICEDOCNUMBER.
    ES_INVOICEDOCNUMBER-FISC_YEAR = LV_FISCALYEAR.
    ES_INVOICEDOCNUMBER-PSTNG_DATE = IS_HEADERDATA-PSTNG_DATE.
  ELSE.
    ET_RETURN[] = LT_BAPI_RETURN[].
  ENDIF.

ENDFUNCTION.