*----------------------------------------------------------------------*
***INCLUDE LZKIG_CN_0004F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form transfer_to_extensionin
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM TRANSFER_TO_EXTENSIONIN USING IS_BAPI_EXTENSIONIN TYPE ANY
                             CHANGING CS_BAPIPAREX TYPE BAPIPAREX.

  DATA LV_DISTANCE_CHARACTERS TYPE I.

  FIELD-SYMBOLS <LV_ANY> TYPE ANY.

  DESCRIBE DISTANCE BETWEEN CS_BAPIPAREX-STRUCTURE AND CS_BAPIPAREX-VALUEPART1
           INTO LV_DISTANCE_CHARACTERS IN CHARACTER MODE.

  ASSIGN CS_BAPIPAREX+LV_DISTANCE_CHARACTERS(*) TO <LV_ANY>

  CASTING LIKE IS_BAPI_EXTENSIONIN.

  <LV_ANY> = IS_BAPI_EXTENSIONIN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_return_msg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_RETURN_MSG USING IV_MSGTY.

  DATA LS_RETURN LIKE LINE OF GT_RETURN.

  CLEAR LS_RETURN.

  LS_RETURN-TYPE = IV_MSGTY.
  LS_RETURN-ID = SY-MSGID.
  LS_RETURN-NUMBER = SY-MSGNO.
  LS_RETURN-MESSAGE_V1 = SY-MSGV1.
  LS_RETURN-MESSAGE_V2 = SY-MSGV2.
  LS_RETURN-MESSAGE_V3 = SY-MSGV3.
  LS_RETURN-MESSAGE_V4 = SY-MSGV4.

  APPEND LS_RETURN TO GT_RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_header_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_HEADER_DATA USING IS_PRHEADER TYPE ZSMM_PRHEADER
                               IS_PRHEADERX TYPE ZSMM_PRHEADERX
                      CHANGING ES_PR_BAPI_HEADER TYPE BAPIMEREQHEADER
                               ES_PR_BAPI_HEADERX TYPE BAPIMEREQHEADERX.

*>?????? ?????? ??????
  ES_PR_BAPI_HEADER-PREQ_NO = IS_PRHEADER-BANFN.
  ES_PR_BAPI_HEADERX-PREQ_NO = IS_PRHEADERX-BANFN.

*>?????? ??????(??????)
  IF NOT IS_PRHEADER-BSART IS INITIAL.
    ES_PR_BAPI_HEADER-PR_TYPE = IS_PRHEADER-BSART.
    ES_PR_BAPI_HEADERX-PR_TYPE = IS_PRHEADERX-BSART.
  ELSE.
    MESSAGE S017(ZMM01) WITH GC_BSART_TXT.
    PERFORM SET_RETURN_MSG USING 'E'.
  ENDIF.

*> ???????????????(????????????/??????????????????)
  ES_PR_BAPI_HEADER-CREATE_IND = IS_PRHEADER-ESTKZ.
  ES_PR_BAPI_HEADERX-CREATE_IND = IS_PRHEADERX-ESTKZ.

*> ?????? ????????? ??????
  ES_PR_BAPI_HEADER-AUTO_SOURCE = IS_PRHEADER-KZZUO.
  ES_PR_BAPI_HEADERX-AUTO_SOURCE = IS_PRHEADER-KZZUO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_header_text
*&---------------------------------------------------------------------*
*& text

*&---------------------------------------------------------------------*
FORM SET_HEADER_TEXT TABLES IT_PRHEADERTEXT STRUCTURE ZSMM_PRHEADERTEXT
                               ET_PR_BAPI_HEADTEXT STRUCTURE BAPIMEREQHEADTEXT.

  DATA: LS_PR_BAPI_HEADTEXT TYPE BAPIMEREQHEADTEXT.

  TYPES: BEGIN OF LTY_TEXTTABLE,
           LINE(4096),
         END OF LTY_TEXTTABLE.

  DATA: LT_TEXT_CONV_RST LIKE TABLE OF TLINE,
        LT_TEXT_CONV     TYPE TABLE OF LTY_TEXTTABLE,
        LS_TEXT_CONV     TYPE LTY_TEXTTABLE.

  CLEAR: ET_PR_BAPI_HEADTEXT.

  SORT IT_PRHEADERTEXT BY TDID.

  DATA(LT_TMP_HEADTEXT) = IT_PRHEADERTEXT[].
  SORT LT_TMP_HEADTEXT BY TDID.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_HEADTEXT COMPARING TDID.

  LOOP AT LT_TMP_HEADTEXT INTO DATA(LS_TMP_HEADTEXT).
    READ TABLE IT_PRHEADERTEXT WITH KEY TDID = LS_TMP_HEADTEXT-TDID
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
    CLEAR LT_TEXT_CONV.
    LOOP AT IT_PRHEADERTEXT INTO DATA(LS_PRHEADERTEXT) FROM SY-TABIX.
      IF LS_PRHEADERTEXT-TDID NE LS_TMP_HEADTEXT-TDID.
        EXIT.
      ENDIF.

      CLEAR LS_TEXT_CONV.
      LS_TEXT_CONV-LINE = LS_PRHEADERTEXT-TEXT_LINE.
      APPEND LS_TEXT_CONV TO LT_TEXT_CONV.
    ENDLOOP.

    IF NOT LT_TEXT_CONV[] IS INITIAL.
      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
        EXPORTING
          LANGUAGE    = SY-LANGU
        TABLES
          TEXT_STREAM = LT_TEXT_CONV
          ITF_TEXT    = LT_TEXT_CONV_RST.
    ENDIF.

    LOOP AT LT_TEXT_CONV_RST INTO DATA(LS_TEXT_CONV_RST).
      CLEAR LS_PR_BAPI_HEADTEXT.
      LS_PR_BAPI_HEADTEXT-TEXT_ID = LS_TMP_HEADTEXT-TDID.
      LS_PR_BAPI_HEADTEXT-TEXT_FORM = LS_TEXT_CONV_RST-TDFORMAT.
      LS_PR_BAPI_HEADTEXT-TEXT_LINE = LS_TEXT_CONV_RST-TDLINE.
      APPEND LS_PR_BAPI_HEADTEXT TO ET_PR_BAPI_HEADTEXT.
    ENDLOOP.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_item_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_ITEM_DATA TABLES IT_PRITEM STRUCTURE ZSMM_PRITEM
                             ET_PR_BAPI_ITEM STRUCTURE BAPIMEREQITEMIMP.

  DATA: LS_PR_BAPI_ITEM TYPE BAPIMEREQITEMIMP,
        LV_ITEM_NO      TYPE BNFPO.

  CLEAR ET_PR_BAPI_ITEM.

  LOOP AT IT_PRITEM INTO DATA(LS_PRITEM).
    CLEAR: LS_PR_BAPI_ITEM.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRITEM-BNFPO IS INITIAL.
      LS_PR_BAPI_ITEM-PREQ_ITEM = LS_PRITEM-BNFPO.
      LV_ITEM_NO = LS_PRITEM-BNFPO.
    ELSE.
      MESSAGE S017(ZMM01) WITH GC_BNFPO_TXT.
      PERFORM SET_RETURN_MSG USING 'E'.
      CONTINUE.
    ENDIF.

*> ????????????
    IF NOT LS_PRITEM-EKGRP IS INITIAL.
      LS_PR_BAPI_ITEM-PUR_GROUP = LS_PRITEM-EKGRP.
    ELSE.
      MESSAGE S016(ZMM01) WITH GC_EKGRP_TXT LV_ITEM_NO.
      PERFORM SET_RETURN_MSG USING 'E'.
    ENDIF.

*> ????????? ??????
    LS_PR_BAPI_ITEM-PREQ_NAME = LS_PRITEM-AFNAM.

*> ??????
    LS_PR_BAPI_ITEM-SHORT_TEXT = LS_PRITEM-TXZ01.

*> ????????????
    IF NOT LS_PRITEM-MATNR IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = LS_PRITEM-MATNR
        IMPORTING
          OUTPUT       = LS_PR_BAPI_ITEM-MATERIAL_LONG
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.

      IF SY-SUBRC NE 0.
        PERFORM SET_RETURN_MSG USING 'E'.
      ENDIF.
    ENDIF.

*> ?????????
    IF NOT LS_PRITEM-WERKS IS INITIAL.
      LS_PR_BAPI_ITEM-PLANT = LS_PRITEM-WERKS.
    ELSE.
      MESSAGE S016(ZMM01) WITH GC_WERKS_TXT LV_ITEM_NO.
      PERFORM SET_RETURN_MSG USING 'E'.
    ENDIF.

*> ????????????
    LS_PR_BAPI_ITEM-STORE_LOC = LS_PRITEM-LGORT.

*> ?????? ?????? ??????
    LS_PR_BAPI_ITEM-TRACKINGNO = LS_PRITEM-BEDNR.

*> ?????? ??????
    LS_PR_BAPI_ITEM-MATL_GROUP = LS_PRITEM-MATKL.

*> ?????? ?????? ????????? ?????? ??????(??????)?????????
    LS_PR_BAPI_ITEM-SUPPL_PLNT = LS_PRITEM-RESWK.

*> ???????????? ??????
    LS_PR_BAPI_ITEM-QUANTITY = LS_PRITEM-BAMNG.

*> ???????????? ??????
    _G_CONV_DATA_EAI_TO_SAP LS_PRITEM-BAMEI '' LS_PR_BAPI_ITEM-UNIT.

    "ISO ????????? ?????? ????????? ????????????...
    IF NOT LS_PR_BAPI_ITEM-UNIT IS INITIAL.
      LS_PR_BAPI_ITEM-PREQ_UNIT_ISO = LS_PR_BAPI_ITEM-UNIT.
    ENDIF.

*> ?????????
    LS_PR_BAPI_ITEM-PREQ_DATE = LS_PRITEM-BADAT.

*> ????????? ??????
    LS_PR_BAPI_ITEM-DEL_DATCAT_EXT = LS_PRITEM-LPEIN.

*> ???????????????
    LS_PR_BAPI_ITEM-DELIV_DATE = LS_PRITEM-EINDT.

*> ?????? ?????? ????????????
    LS_PR_BAPI_ITEM-REL_DATE = LS_PRITEM-FRGDT.

*> ?????????????????? (???)
    LS_PR_BAPI_ITEM-GR_PR_TIME = LS_PRITEM-WEBAZ.

*> ?????? ??????
    LS_PR_BAPI_ITEM-PREQ_PRICE = LS_PRITEM-PREIS.

*> ????????????
    LS_PR_BAPI_ITEM-PRICE_UNIT = LS_PRITEM-EPEIN.

*> ?????? ????????? ?????? ??????
    LS_PR_BAPI_ITEM-ITEM_CAT = LS_PRITEM-PSTYP.

*> ?????? ????????? ?????? ??????
    LS_PR_BAPI_ITEM-ACCTASSCAT = LS_PRITEM-KNTTP.

*> ?????? ?????? ????????? ?????? ?????? ?????????
    LS_PR_BAPI_ITEM-DISTRIB = LS_PRITEM-VRTKZ.

*> ?????? ?????? ?????????
    LS_PR_BAPI_ITEM-PART_INV = LS_PRITEM-TWRKZ.

*> ?????? ?????????
    LS_PR_BAPI_ITEM-GR_IND = LS_PRITEM-WEPOS.

*> ????????? ??????
    LS_PR_BAPI_ITEM-GR_NON_VAL = LS_PRITEM-WEUNB.

*> ?????? ?????? ?????????
    LS_PR_BAPI_ITEM-IR_IND = LS_PRITEM-REPOS.

*> ?????? ????????????
    IF NOT LS_PRITEM-WLIEF IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PRITEM-WLIEF
        IMPORTING
          OUTPUT = LS_PR_BAPI_ITEM-DES_VENDOR.
    ENDIF.

*> ?????? ????????????
    IF NOT LS_PRITEM-FLIEF IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PRITEM-FLIEF
        IMPORTING
          OUTPUT = LS_PR_BAPI_ITEM-FIXED_VEND.
    ENDIF.

*> ?????? ??????
    LS_PR_BAPI_ITEM-PURCH_ORG = LS_PRITEM-EKORG.

*> ?????? ??????
    LS_PR_BAPI_ITEM-VAL_TYPE = LS_PRITEM-BWTAR.

*> ???????????? ??????
    LS_PR_BAPI_ITEM-CLOSED = LS_PRITEM-EBAKZ.

*> ????????? ?????? ??????
    LS_PR_BAPI_ITEM-FIXED = LS_PRITEM-BAFIX.

*> ?????? ?????? ??????
    LS_PR_BAPI_ITEM-PO_UNIT = LS_PRITEM-BSTME.
    IF NOT LS_PR_BAPI_ITEM-PO_UNIT IS INITIAL.
      LS_PR_BAPI_ITEM-PO_UNIT_ISO = LS_PR_BAPI_ITEM-PO_UNIT.
    ENDIF.

*> ?????? ??????
    LS_PR_BAPI_ITEM-CMMT_ITEM = LS_PRITEM-FIPOS.

*> ?????? ?????? ??????
    LS_PR_BAPI_ITEM-FUNDS_CTR = LS_PRITEM-FISTL.

*> ??????
    IF NOT LS_PRITEM-KUNNR IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PRITEM-KUNNR
        IMPORTING
          OUTPUT = LS_PR_BAPI_ITEM-CUSTOMER.
    ENDIF.

*> ?????? ????????????
    IF NOT LS_PRITEM-EMLIF IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PRITEM-EMLIF
        IMPORTING
          OUTPUT = LS_PR_BAPI_ITEM-SUPP_VENDOR.
    ENDIF.

*> SC ??????
    IF NOT LS_PRITEM-LBLKZ IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PRITEM-LBLKZ
        IMPORTING
          OUTPUT = LS_PR_BAPI_ITEM-SC_VENDOR.
    ENDIF.

*> ??????????????????
    LS_PR_BAPI_ITEM-VALUATION_SPEC_STOCK = LS_PRITEM-KZBWS.

*> ?????????
    LS_PR_BAPI_ITEM-CURRENCY = LS_PRITEM-WAERS.
    IF NOT LS_PR_BAPI_ITEM-CURRENCY IS INITIAL.
      LS_PR_BAPI_ITEM-CURRENCY_ISO = LS_PR_BAPI_ITEM-CURRENCY.
    ENDIF.

*> ?????? ?????? ?????? ??????(???)
    LS_PR_BAPI_ITEM-PLND_DELRY = LS_PRITEM-PLIFZ.

    APPEND LS_PR_BAPI_ITEM TO ET_PR_BAPI_ITEM.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_itemx_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_PRITEMX
*&      --> LT_PR_BAPI_ITEMX
*&---------------------------------------------------------------------*
FORM SET_ITEMX_DATA TABLES IT_PRITEMX STRUCTURE ZSMM_PRITEMX
                              ET_PR_BAPI_ITEMX STRUCTURE BAPIMEREQITEMX.

  DATA: LS_PR_BAPI_ITEMX TYPE BAPIMEREQITEMX,
        LV_ITEM_NO       TYPE BNFPO.

  CLEAR ET_PR_BAPI_ITEMX.
  LOOP AT IT_PRITEMX INTO DATA(LS_PRITEMX).
    CLEAR: LS_PR_BAPI_ITEMX.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRITEMX-BNFPO IS INITIAL.
      LS_PR_BAPI_ITEMX-PREQ_ITEM = LS_PRITEMX-BNFPO.
      LV_ITEM_NO = LS_PRITEMX-BNFPO.
    ELSE.
      MESSAGE S017(ZMM01) WITH GC_BNFPO_TXT.
      PERFORM SET_RETURN_MSG USING 'E'.
      CONTINUE.
    ENDIF.

*> ????????????
    LS_PR_BAPI_ITEMX-PUR_GROUP = LS_PRITEMX-EKGRP.

*> ????????? ??????
    LS_PR_BAPI_ITEMX-PREQ_NAME = LS_PRITEMX-AFNAM.

*> ??????
    LS_PR_BAPI_ITEMX-SHORT_TEXT = LS_PRITEMX-TXZ01.

*> ????????????
    LS_PR_BAPI_ITEMX-MATERIAL_LONG = LS_PRITEMX-MATNR.

*> ?????????
    LS_PR_BAPI_ITEMX-PLANT = LS_PRITEMX-WERKS.

*> ????????????
    LS_PR_BAPI_ITEMX-STORE_LOC = LS_PRITEMX-LGORT.

*> ?????? ?????? ??????
    LS_PR_BAPI_ITEMX-TRACKINGNO = LS_PRITEMX-BEDNR.

*> ?????? ??????
    LS_PR_BAPI_ITEMX-MATL_GROUP = LS_PRITEMX-MATKL.

*> ?????? ?????? ????????? ?????? ??????(??????)?????????
    LS_PR_BAPI_ITEMX-SUPPL_PLNT = LS_PRITEMX-RESWK.

*> ???????????? ??????
    LS_PR_BAPI_ITEMX-QUANTITY = LS_PRITEMX-BAMNG.

*> ???????????? ??????
    LS_PR_BAPI_ITEMX-UNIT = LS_PRITEMX-BAMEI.

    "ISO ????????? ?????? ????????? ????????????...
    IF NOT LS_PR_BAPI_ITEMX-UNIT IS INITIAL.
      LS_PR_BAPI_ITEMX-PREQ_UNIT_ISO = LS_PR_BAPI_ITEMX-UNIT.
    ENDIF.

*> ?????????
    LS_PR_BAPI_ITEMX-PREQ_DATE = LS_PRITEMX-BADAT.

*> ????????? ??????
    LS_PR_BAPI_ITEMX-DEL_DATCAT_EXT = LS_PRITEMX-LPEIN.

*> ???????????????
    LS_PR_BAPI_ITEMX-DELIV_DATE = LS_PRITEMX-EINDT.

*> ?????? ?????? ????????????
    LS_PR_BAPI_ITEMX-REL_DATE = LS_PRITEMX-FRGDT.

*> ?????????????????? (???)
    LS_PR_BAPI_ITEMX-GR_PR_TIME = LS_PRITEMX-WEBAZ.

*> ?????? ??????
    LS_PR_BAPI_ITEMX-PREQ_PRICE = LS_PRITEMX-PREIS.

*> ????????????
    LS_PR_BAPI_ITEMX-PRICE_UNIT = LS_PRITEMX-EPEIN.

*> ?????? ????????? ?????? ??????
    LS_PR_BAPI_ITEMX-ITEM_CAT = LS_PRITEMX-PSTYP.

*> ?????? ????????? ?????? ??????
    LS_PR_BAPI_ITEMX-ACCTASSCAT = LS_PRITEMX-KNTTP.

*> ?????? ?????? ????????? ?????? ?????? ?????????
    LS_PR_BAPI_ITEMX-DISTRIB = LS_PRITEMX-VRTKZ.

*> ?????? ?????? ?????????
    LS_PR_BAPI_ITEMX-PART_INV = LS_PRITEMX-TWRKZ.

*> ?????? ?????????
    LS_PR_BAPI_ITEMX-GR_IND = LS_PRITEMX-WEPOS.

*> ????????? ??????
    LS_PR_BAPI_ITEMX-GR_NON_VAL = LS_PRITEMX-WEUNB.

*> ?????? ?????? ?????????
    LS_PR_BAPI_ITEMX-IR_IND = LS_PRITEMX-REPOS.

*> ?????? ????????????
    LS_PR_BAPI_ITEMX-DES_VENDOR = LS_PRITEMX-WLIEF.

*> ?????? ????????????
    LS_PR_BAPI_ITEMX-FIXED_VEND = LS_PRITEMX-FLIEF.

*> ?????? ??????
    LS_PR_BAPI_ITEMX-PURCH_ORG = LS_PRITEMX-EKORG.

*> ?????? ??????
    LS_PR_BAPI_ITEMX-VAL_TYPE = LS_PRITEMX-BWTAR.

*> ???????????? ??????
    LS_PR_BAPI_ITEMX-CLOSED = LS_PRITEMX-EBAKZ.

*> ????????? ?????? ??????
    LS_PR_BAPI_ITEMX-FIXED = LS_PRITEMX-BAFIX.

*> ?????? ?????? ??????
    LS_PR_BAPI_ITEMX-PO_UNIT = LS_PRITEMX-BSTME.
    IF NOT LS_PR_BAPI_ITEMX-PO_UNIT IS INITIAL.
      LS_PR_BAPI_ITEMX-PO_UNIT_ISO = LS_PR_BAPI_ITEMX-PO_UNIT.
    ENDIF.

*> ?????? ??????
    LS_PR_BAPI_ITEMX-CMMT_ITEM = LS_PRITEMX-FIPOS.

*> ?????? ?????? ??????
    LS_PR_BAPI_ITEMX-FUNDS_CTR = LS_PRITEMX-FISTL.

*> ??????
    LS_PR_BAPI_ITEMX-CUSTOMER = LS_PRITEMX-KUNNR.

*> ?????? ????????????
    LS_PR_BAPI_ITEMX-SUPP_VENDOR = LS_PRITEMX-EMLIF.

*> SC ??????
    LS_PR_BAPI_ITEMX-SC_VENDOR = LS_PRITEMX-LBLKZ.

*> ??????????????????
    LS_PR_BAPI_ITEMX-VALUATION_SPEC_STOCK = LS_PRITEMX-KZBWS.

*> ?????????
    LS_PR_BAPI_ITEMX-CURRENCY = LS_PRITEMX-WAERS.
    IF NOT LS_PR_BAPI_ITEMX-CURRENCY IS INITIAL.
      LS_PR_BAPI_ITEMX-CURRENCY_ISO = LS_PR_BAPI_ITEMX-CURRENCY.
    ENDIF.

*> ?????? ?????? ?????? ??????(???)
    LS_PR_BAPI_ITEMX-PLND_DELRY = LS_PRITEMX-PLIFZ.

    APPEND LS_PR_BAPI_ITEMX TO ET_PR_BAPI_ITEMX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_account_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_ACCOUNT_DATA TABLES IT_PRACCOUNT STRUCTURE ZSMM_PRACCOUNT
                                ET_PR_BAPI_ACC STRUCTURE BAPIMEREQACCOUNT.

  DATA: LS_PR_BAPI_ACC TYPE BAPIMEREQACCOUNT,
        LV_ITEM_NO     TYPE BNFPO.

  CLEAR ET_PR_BAPI_ACC.
  LOOP AT IT_PRACCOUNT INTO DATA(LS_PRACCOUNT).
    CLEAR LS_PR_BAPI_ACC.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRACCOUNT-BNFPO IS INITIAL.
      LS_PR_BAPI_ACC-PREQ_ITEM = LS_PRACCOUNT-BNFPO.
      LV_ITEM_NO = LS_PR_BAPI_ACC-PREQ_ITEM.
    ELSE.
      LV_ITEM_NO = SY-TABIX.
    ENDIF.


*> ?????? ?????? ??????
    LS_PR_BAPI_ACC-SERIAL_NO = LS_PRACCOUNT-ZEBKN.

*> ??????
    LS_PR_BAPI_ACC-QUANTITY = LS_PRACCOUNT-MENGE.

*> ?????? ?????? ????????? ?????? ?????? ?????????
    LS_PR_BAPI_ACC-DISTR_PERC = LS_PRACCOUNT-VPROZ.

*> BAPI??? ?????? ????????????
    LS_PR_BAPI_ACC-NET_VALUE = LS_PRACCOUNT-BAPICUREXT.

*> G/L ?????? ??????
    IF NOT LS_PRACCOUNT-SAKNR IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_PRACCOUNT-SAKNR
        IMPORTING
          OUTPUT = LS_PR_BAPI_ACC-GL_ACCOUNT.
    ENDIF.

*> ?????? ??????
    LS_PR_BAPI_ACC-BUS_AREA = LS_PRACCOUNT-GSBER.

*> ???????????????
    LS_PR_BAPI_ACC-COSTCENTER = LS_PRACCOUNT-KOSTL.

*> ?????? ?????? ??????
    LS_PR_BAPI_ACC-ASSET_NO = LS_PRACCOUNT-ANLN1.

*> ?????? ?????? ??????
    LS_PR_BAPI_ACC-SUB_NUMBER = LS_PRACCOUNT-ANLN2.

*> ?????? ??????
    LS_PR_BAPI_ACC-ORDERID = LS_PRACCOUNT-AUFNR.

*> ?????? ??????
    LS_PR_BAPI_ACC-PROFIT_CTR = LS_PRACCOUNT-PRCTR.

*> ?????? ?????? ????????????(WBS ??????)
    IF NOT LS_PRACCOUNT-PS_POSID IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
        EXPORTING
          INPUT  = LS_PRACCOUNT-PS_POSID
        IMPORTING
          OUTPUT = LS_PR_BAPI_ACC-WBS_ELEMENT.
    ENDIF.

*> ???????????? ??????????????????
    LS_PR_BAPI_ACC-NETWORK = LS_PRACCOUNT-NPLNR.

*> ?????? ??????
    LS_PR_BAPI_ACC-CMMT_ITEM = LS_PRACCOUNT-FIPOS.

*> ?????? ?????? ??????
    LS_PR_BAPI_ACC-FUNDS_CTR = LS_PRACCOUNT-FISTL.

    APPEND LS_PR_BAPI_ACC TO ET_PR_BAPI_ACC.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_accountX_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_ACCOUNTX_DATA TABLES IT_PRACCOUNTX STRUCTURE ZSMM_PRACCOUNTX
                                 ET_PR_BAPI_ACCX STRUCTURE BAPIMEREQACCOUNTX.

  DATA: LS_PR_BAPI_ACCX TYPE BAPIMEREQACCOUNTX,
        LV_ITEM_NO      TYPE BNFPO.

  CLEAR ET_PR_BAPI_ACCX.
  LOOP AT IT_PRACCOUNTX INTO DATA(LS_PRACCOUNTX).
    CLEAR LS_PR_BAPI_ACCX.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRACCOUNTX-BNFPO IS INITIAL.
      LS_PR_BAPI_ACCX-PREQ_ITEM = LS_PRACCOUNTX-BNFPO.
      LV_ITEM_NO = LS_PR_BAPI_ACCX-PREQ_ITEM.
    ELSE.
      LV_ITEM_NO = SY-TABIX.
    ENDIF.


*> ?????? ?????? ??????
    LS_PR_BAPI_ACCX-SERIAL_NO = LS_PRACCOUNTX-ZEBKN.

*> ??????
    LS_PR_BAPI_ACCX-QUANTITY = LS_PRACCOUNTX-MENGE.

*> ?????? ?????? ????????? ?????? ?????? ?????????
    LS_PR_BAPI_ACCX-DISTR_PERC = LS_PRACCOUNTX-VPROZ.

*> BAPI??? ?????? ????????????
    LS_PR_BAPI_ACCX-NET_VALUE = LS_PRACCOUNTX-BAPICUREXT.

*> G/L ?????? ??????
    LS_PR_BAPI_ACCX-GL_ACCOUNT = LS_PRACCOUNTX-SAKNR.

*> ?????? ??????
    LS_PR_BAPI_ACCX-BUS_AREA = LS_PRACCOUNTX-GSBER.

*> ???????????????
    LS_PR_BAPI_ACCX-COSTCENTER = LS_PRACCOUNTX-KOSTL.

*> ?????? ?????? ??????
    LS_PR_BAPI_ACCX-ASSET_NO = LS_PRACCOUNTX-ANLN1.

*> ?????? ?????? ??????
    LS_PR_BAPI_ACCX-SUB_NUMBER = LS_PRACCOUNTX-ANLN2.

*> ?????? ??????
    LS_PR_BAPI_ACCX-ORDERID = LS_PRACCOUNTX-AUFNR.

*> ?????? ??????
    LS_PR_BAPI_ACCX-PROFIT_CTR = LS_PRACCOUNTX-PRCTR.

*> ?????? ?????? ????????????(WBS ??????)
    LS_PR_BAPI_ACCX-WBS_ELEMENT = LS_PRACCOUNTX-PS_POSID.

*> ???????????? ??????????????????
    LS_PR_BAPI_ACCX-NETWORK = LS_PRACCOUNTX-NPLNR.

*> ?????? ??????
    LS_PR_BAPI_ACCX-CMMT_ITEM = LS_PRACCOUNTX-FIPOS.

*> ?????? ?????? ??????
    LS_PR_BAPI_ACCX-FUNDS_CTR = LS_PRACCOUNTX-FISTL.

    APPEND LS_PR_BAPI_ACCX TO ET_PR_BAPI_ACCX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_delivery_addr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_DELIVERY_ADDR TABLES IT_PRADDRDELIVERY STRUCTURE ZSMM_PRADDRDELIVERY
                                 ET_PR_BAPI_DEL_ADDR STRUCTURE BAPIMERQADDRDELIVERY.

  DATA: LS_PR_BAPI_DEL_ADDR TYPE BAPIMERQADDRDELIVERY,
        LV_ITEM_NO          TYPE BNFPO.

  CLEAR ET_PR_BAPI_DEL_ADDR.
  LOOP AT IT_PRADDRDELIVERY INTO DATA(LS_PRADDRDELIVERY).
    CLEAR LS_PR_BAPI_DEL_ADDR.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRADDRDELIVERY-BNFPO IS INITIAL.
      LS_PR_BAPI_DEL_ADDR-PREQ_ITEM = LS_PRADDRDELIVERY-BNFPO.
      LV_ITEM_NO = LS_PR_BAPI_DEL_ADDR-PREQ_ITEM.
    ELSE.
      LV_ITEM_NO = SY-TABIX.
    ENDIF.


*> ?????? ?????? ??????
    LS_PR_BAPI_DEL_ADDR-PREQ_NO = LS_PRADDRDELIVERY-BANFN.

*> ?????? ?????? ?????? ??????
    LS_PR_BAPI_DEL_ADDR-PREQ_ITEM = LS_PRADDRDELIVERY-BNFPO.

*> ??????1
    LS_PR_BAPI_DEL_ADDR-NAME = LS_PRADDRDELIVERY-NAME1.

*> ??????2
    LS_PR_BAPI_DEL_ADDR-NAME_2 = LS_PRADDRDELIVERY-NAME2.

*> ????????????
    LS_PR_BAPI_DEL_ADDR-STREET = LS_PRADDRDELIVERY-STREET.

*> ??????
    LS_PR_BAPI_DEL_ADDR-HOUSE_NO = LS_PRADDRDELIVERY-HOUSE_NUM1.

*> ??????
    LS_PR_BAPI_DEL_ADDR-CITY = LS_PRADDRDELIVERY-CITY1.

*> ??????
    LS_PR_BAPI_DEL_ADDR-COUNTRY = LS_PRADDRDELIVERY-LAND1.

*> ????????????
    LS_PR_BAPI_DEL_ADDR-POSTL_COD1 = LS_PRADDRDELIVERY-POST_CD.

    APPEND LS_PR_BAPI_DEL_ADDR TO ET_PR_BAPI_DEL_ADDR.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_extension
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_EXTENSION TABLES IT_PRITEM STRUCTURE ZSMM_PRITEM
                             IT_PRITEMX STRUCTURE ZSMM_PRITEMX
                             ET_PR_BAPI_EXTENSIONIN STRUCTURE BAPIPAREX.

  DATA: LS_CI_EBANDB           TYPE BAPI_TE_MEREQITEM,
        LS_CI_EBANDBX          TYPE BAPI_TE_MEREQITEMX,
        LS_PR_BAPI_EXTENSIONIN TYPE BAPIPAREX.

  CONSTANTS: LC_EXT_STR_NAME  TYPE BAPIPAREX-STRUCTURE VALUE 'BAPI_TE_MEREQITEM',
             LC_EXTX_STR_NAME TYPE BAPIPAREX-STRUCTURE VALUE 'BAPI_TE_MEREQITEMX'.

  CLEAR ET_PR_BAPI_EXTENSIONIN.

  LOOP AT IT_PRITEM INTO DATA(LS_PRITEM).
    CLEAR: LS_CI_EBANDB, LS_PR_BAPI_EXTENSIONIN.
    LS_CI_EBANDB-PREQ_ITEM = LS_PRITEM-BNFPO.
*    LS_CI_EBANDB-ZORDER_PERSON = LS_PRITEM-ZORDER_PERSON.
*    LS_CI_EBANDB-ZORDER_DEPARTMENT = LS_PRITEM-ZORDER_DEPARTMENT.
*    LS_CI_EBANDB-ZMRO_CATEGORY = LS_PRITEM-ZMRO_CATEGORY.
*    LS_CI_EBANDB-ZMARKET_ID = LS_PRITEM-ZMARKET_ID.
*    LS_CI_EBANDB-ZSTARTDATE = LS_PRITEM-ZSTARTDATE.
*    LS_CI_EBANDB-ZENDDATE = LS_PRITEM-ZENDDATE.
    LS_PR_BAPI_EXTENSIONIN-STRUCTURE = LC_EXT_STR_NAME.

    PERFORM TRANSFER_TO_EXTENSIONIN USING LS_CI_EBANDB CHANGING LS_PR_BAPI_EXTENSIONIN.
    APPEND LS_PR_BAPI_EXTENSIONIN TO ET_PR_BAPI_EXTENSIONIN.
  ENDLOOP.

*--------------------------------------------------------------------*

  LOOP AT IT_PRITEMX INTO DATA(LS_PRITEMX).
    CLEAR: LS_CI_EBANDBX, LS_PR_BAPI_EXTENSIONIN.
    LS_CI_EBANDBX-PREQ_ITEM = LS_PRITEMX-BNFPO.
*    LS_CI_EBANDBX-ZORDER_PERSON = LS_PRITEMX-ZORDER_PERSON.
*    LS_CI_EBANDBX-ZORDER_DEPARTMENT = LS_PRITEMX-ZORDER_DEPARTMENT.
*    LS_CI_EBANDBX-ZMRO_CATEGORY = LS_PRITEMX-ZMRO_CATEGORY.
*    LS_CI_EBANDBX-ZMARKET_ID = LS_PRITEMX-ZMARKET_ID.
*    LS_CI_EBANDBX-ZSTARTDATE = LS_PRITEMX-ZSTARTDATE.
*    LS_CI_EBANDBX-ZENDDATE = LS_PRITEMX-ZENDDATE.
    LS_PR_BAPI_EXTENSIONIN-STRUCTURE = LC_EXTX_STR_NAME.
    PERFORM TRANSFER_TO_EXTENSIONIN USING LS_CI_EBANDBX CHANGING LS_PR_BAPI_EXTENSIONIN.
    APPEND LS_PR_BAPI_EXTENSIONIN TO ET_PR_BAPI_EXTENSIONIN.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_components_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_COMPONENTS_DATA TABLES IT_PRCOMPONENTS STRUCTURE ZSMM_PRCOMPONENTS
                                   ET_PR_BAPI_COMPONENT STRUCTURE BAPIMEREQCOMPONENT.

  DATA: LS_PR_BAPI_COMPONENT TYPE BAPIMEREQCOMPONENT,
        LV_ITEM_NO           TYPE BNFPO.

  CLEAR ET_PR_BAPI_COMPONENT.
  LOOP AT IT_PRCOMPONENTS INTO DATA(LS_PRCOMPONENTS).
    CLEAR LS_PR_BAPI_COMPONENT.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRCOMPONENTS-BNFPO IS INITIAL.
      LS_PR_BAPI_COMPONENT-PREQ_ITEM = LS_PRCOMPONENTS-BNFPO.
      LV_ITEM_NO = LS_PR_BAPI_COMPONENT-PREQ_ITEM.
    ELSE.
      LV_ITEM_NO = SY-TABIX.
    ENDIF.


*> ???????????? ????????????
    LS_PR_BAPI_COMPONENT-PREQ_ITEM = LS_PRCOMPONENTS-BNFPO.

*> ??????/?????? ???????????? ?????? ?????? ??????
    LS_PR_BAPI_COMPONENT-ITEM_NO = LS_PRCOMPONENTS-RSPOS.

*> ????????????(18???)
    IF NOT LS_PRCOMPONENTS-MATNR IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = LS_PRCOMPONENTS-MATNR
        IMPORTING
          OUTPUT       = LS_PR_BAPI_COMPONENT-MATERIAL
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.

      IF SY-SUBRC NE 0.
        PERFORM SET_RETURN_MSG USING 'E'.
      ENDIF.
    ENDIF.

*> ???????????? ?????????
    LS_PR_BAPI_COMPONENT-ENTRY_QUANTITY = LS_PRCOMPONENTS-CO_MENGE.

*> ????????????
    LS_PR_BAPI_COMPONENT-ENTRY_UOM = LS_PRCOMPONENTS-ERFME.

*> ????????????
    LS_PR_BAPI_COMPONENT-FIXED_QUAN = LS_PRCOMPONENTS-FMENG.

*> ?????????
    LS_PR_BAPI_COMPONENT-PLANT = LS_PRCOMPONENTS-WERKS.

*> ???????????? ?????????
    LS_PR_BAPI_COMPONENT-REQ_DATE = LS_PRCOMPONENTS-BDTER.

*> ?????? ??????
    LS_PR_BAPI_COMPONENT-BATCH = LS_PRCOMPONENTS-CHARG.

*> ?????? ?????? ??????
    LS_PR_BAPI_COMPONENT-ISS_ST_LOC = LS_PRCOMPONENTS-LGPRO.

*> ?????? ?????????
    LS_PR_BAPI_COMPONENT-CHANGE_ID = LS_PRCOMPONENTS-CHANGE_ID.

    APPEND LS_PR_BAPI_COMPONENT TO ET_PR_BAPI_COMPONENT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_componentsx_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_COMPONENTSX_DATA TABLES IT_PRCOMPONENTSX STRUCTURE ZSMM_PRCOMPONENTSX
                                    ET_PR_BAPI_COMPONENTX STRUCTURE BAPIMEREQCOMPONENTX.

  DATA: LS_PR_BAPI_COMPONENTX TYPE BAPIMEREQCOMPONENTX,
        LV_ITEM_NO            TYPE BNFPO.

  CLEAR ET_PR_BAPI_COMPONENTX.
  LOOP AT IT_PRCOMPONENTSX INTO DATA(LS_PRCOMPONENTSX).
    CLEAR LS_PR_BAPI_COMPONENTX.

*> ?????? ?????? ?????? ??????
    IF NOT LS_PRCOMPONENTSX-BNFPO IS INITIAL.
      LS_PR_BAPI_COMPONENTX-PREQ_ITEM = LS_PRCOMPONENTSX-BNFPO.
      LV_ITEM_NO = LS_PR_BAPI_COMPONENTX-PREQ_ITEM.
    ELSE.
      LV_ITEM_NO = SY-TABIX.
    ENDIF.


*> ???????????? ????????????
    LS_PR_BAPI_COMPONENTX-PREQ_ITEM = LS_PRCOMPONENTSX-BNFPO.

*> ??????/?????? ???????????? ?????? ?????? ??????
    LS_PR_BAPI_COMPONENTX-ITEM_NO = LS_PRCOMPONENTSX-RSPOS.

*> ????????????(18???)
    LS_PR_BAPI_COMPONENTX-MATERIAL = LS_PRCOMPONENTSX-MATNR.

*> ???????????? ?????????
    LS_PR_BAPI_COMPONENTX-ENTRY_QUANTITY = LS_PRCOMPONENTSX-CO_MENGE.

*> ????????????
    LS_PR_BAPI_COMPONENTX-ENTRY_UOM = LS_PRCOMPONENTSX-ERFME.

*> ????????????
    LS_PR_BAPI_COMPONENTX-FIXED_QUAN = LS_PRCOMPONENTSX-FMENG.

*> ?????????
    LS_PR_BAPI_COMPONENTX-PLANT = LS_PRCOMPONENTSX-WERKS.

*> ???????????? ?????????
    LS_PR_BAPI_COMPONENTX-REQ_DATE = LS_PRCOMPONENTSX-BDTER.

*> ?????? ??????
    LS_PR_BAPI_COMPONENTX-BATCH = LS_PRCOMPONENTSX-CHARG.

*> ?????? ?????? ??????
    LS_PR_BAPI_COMPONENTX-ISS_ST_LOC = LS_PRCOMPONENTSX-LGPRO.

*> ????????????
    LS_PR_BAPI_COMPONENTX-CHANGE_ID = LS_PRCOMPONENTSX-CHANGE_ID.

    APPEND LS_PR_BAPI_COMPONENTX TO ET_PR_BAPI_COMPONENTX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_cbo_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IS_PRHEADER
*&---------------------------------------------------------------------*
FORM SAVE_CBO_DATA USING IV_BANFN
                             IS_PRHEADER TYPE ZSMM_PRHEADER
                             IS_PRHEADERX TYPE ZSMM_PRHEADERX
                    CHANGING EV_SUBRC.

  DATA: LS_ZTMM30010 LIKE ZTMM30010.

  LS_ZTMM30010-BANFN = IV_BANFN.

  IF IS_PRHEADERX-ZPRTITLE EQ 'X'.
    LS_ZTMM30010-ZPRTITLE = IS_PRHEADER-ZPRTITLE.
  ENDIF.

  IF IS_PRHEADERX-ZPEQ_DEPARTMENT EQ 'X'.
    LS_ZTMM30010-ZPEQ_DEPARTMENT = IS_PRHEADER-ZPEQ_DEPARTMENT.
  ENDIF.

  IF IS_PRHEADERX-ZREQUESTER EQ 'X'.
    LS_ZTMM30010-ZREQUESTER = IS_PRHEADER-ZREQUESTER.
  ENDIF.

  IF IS_PRHEADERX-ZNOPRICE EQ 'X'.
    LS_ZTMM30010-ZNOPRICE = IS_PRHEADER-ZNOPRICE.
  ENDIF.

  IF IS_PRHEADERX-ZURGENT EQ 'X'.
    LS_ZTMM30010-ZURGENT = IS_PRHEADER-ZURGENT.
  ENDIF.

  IF IS_PRHEADERX-ZPRE_INPUT EQ 'X'.
    LS_ZTMM30010-ZPRE_INPUT = IS_PRHEADER-ZPRE_INPUT.
  ENDIF.

  IF IS_PRHEADERX-ZURGENT_REASON EQ 'X'.
    LS_ZTMM30010-ZURGENT_REASON = IS_PRHEADER-ZURGENT_REASON.
  ENDIF.

  IF IS_PRHEADERX-ZPRE_INPUT_REASON EQ 'X'.
    LS_ZTMM30010-ZPRE_INPUT_REASON = IS_PRHEADER-ZPRE_INPUT_REASON.
  ENDIF.

  IF IS_PRHEADERX-ZRECEIPT EQ 'X'.
    LS_ZTMM30010-ZRECEIPT = IS_PRHEADER-ZRECEIPT.
  ENDIF.

  IF IS_PRHEADERX-ZSOURCE_SYS EQ 'X'.
    LS_ZTMM30010-ZSOURCE_SYS = IS_PRHEADER-ZSOURCE_SYS.
  ENDIF.

  LS_ZTMM30010-ERDAT = SY-DATUM.
  LS_ZTMM30010-ERZET = SY-UZEIT.
  LS_ZTMM30010-ERNAM = SY-UNAME.

  MODIFY ZTMM30010 FROM LS_ZTMM30010.

  EV_SUBRC = SY-SUBRC.

ENDFORM.
