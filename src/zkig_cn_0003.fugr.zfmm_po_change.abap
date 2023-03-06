FUNCTION ZFMM_PO_CHANGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EBELN
*"     VALUE(IS_POHEADER) TYPE  ZSMM_POHEADER OPTIONAL
*"     VALUE(IS_POHEADERX) TYPE  ZSMM_POHEADERX OPTIONAL
*"     VALUE(IV_AUTH) TYPE  BAPIFLAG OPTIONAL
*"     VALUE(IV_PRICE_FROM_PO) TYPE  BAPIFLAG OPTIONAL
*"     VALUE(IV_TESTRUN) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(EV_PURCHASEORDER) LIKE  BAPIMEPOHEADER-PO_NUMBER
*"     VALUE(EV_HEADER) LIKE  BAPIMEPOHEADER STRUCTURE  BAPIMEPOHEADER
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2
*"      IT_POITEM STRUCTURE  ZSMM_POITEM
*"      IT_POITEMX STRUCTURE  ZSMM_POITEMX
*"      IT_POADDRDELIVERY STRUCTURE  ZSMM_POADDRDELIVERY
*"      IT_POSCHEDULE STRUCTURE  ZSMM_POSCHEDULE
*"      IT_POSCHEDULEX STRUCTURE  ZSMM_POSCHEDULEX
*"      IT_POACCOUNT STRUCTURE  ZSMM_POACCOUNT
*"      IT_POACCOUNTX STRUCTURE  ZSMM_POACCOUNTX
*"      IT_POCOND STRUCTURE  ZSMM_POCOND
*"      IT_POCONDX STRUCTURE  ZSMM_POCONDX
*"      IT_POTEXTHEADER STRUCTURE  ZSMM_POTEXTHEADER
*"      IT_POPARTNER STRUCTURE  ZSMM_POPARTNER
*"      IT_POCOMPONENTS STRUCTURE  ZSMM_POCOMPONENTS
*"      IT_POCOMPONENTSX STRUCTURE  ZSMM_POCOMPONENTSX
*"      IT_POSHIPPING STRUCTURE  ZSMM_POSHIPPING
*"      IT_POSHIPPINGX STRUCTURE  ZSMM_POSHIPPINGX
*"      IT_POACCOUNTPROFITSEGMENT STRUCTURE
*"        ZSMM_POACCOUNTPROFITSEGMENT
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Module : MM
*& Program ID : zfmm_po_change
*& T-CODE : N/A
*& Referenced Program : N/A
*& Created by : N/A
*& Created On : N/A
*& Type : Online
*& Description : 구매오더 변경 공통 FC
*& 1. 구매오더 생성을 위한 공통 함수
* 표준 BAPI인 BAPI_PO_CHANGE 을 참조하여 공통 함수로 개발
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ 변경자 변경일자 변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

* Global data declarations

  DATA : lv_no_mess LIKE  bapiflag-bapiflag.

  CLEAR : gs_poheader,  gs_poheaderx,
          gt_poitem[],  gt_poitemx[], gt_poaddrdelivery[],
          gt_poschedule[], gt_poschedulex[],
          gt_poaccount[], gt_poaccountx[],
          gt_poaccountprofitsegment[],"수익성 세그먼트(삭제 가능)
          gt_pocond[], gt_pocondx[],
          gt_extensionin[],
          gt_potextheader[], gt_popartner[],
          gt_pocomponents[], gt_pocomponentsx[],
          gt_poshipping[],  gt_poshippingx[],
          gv_header, gt_return[],
          lv_no_mess.

  gv_ebeln = iv_ebeln.
  gv_authority = iv_auth.
  gv_price_from_po = iv_price_from_po.

  DATA : lc_she_check(1) TYPE c.

*Header Data
  IF is_poheader IS NOT INITIAL.
    PERFORM header_info TABLES et_return
                         USING  is_poheader is_poheaderx.
    CHECK et_return[] IS INITIAL.
  ENDIF.


*Other Data
*  IF is_poheaderother IS NOT INITIAL.
*    PERFORM headerother_info USING iv_ebeln  is_poheaderother is_poheaderotherx.
*  ENDIF.


*Item Data

  IF it_poitem[] IS NOT INITIAL.
    PERFORM item_info TABLES it_poitem it_poitemx.
  ENDIF.


*Item Other Data
  IF it_poitem[] IS NOT INITIAL.
    PERFORM itemother_info TABLES it_poitem it_poitemx.
  ENDIF.


*Addresses for Inward Delivery (Item)
  IF it_poaddrdelivery[] IS NOT INITIAL.
    PERFORM addrdelivery_info TABLES it_poaddrdelivery.
  ENDIF.


*Delivery Schedule
  IF it_poschedule[] IS NOT INITIAL.
    PERFORM schedule_info TABLES it_poschedule it_poschedulex.
  ENDIF.


*Account Assignment Fields
  IF it_poaccount[] IS NOT INITIAL.
    PERFORM account_info TABLES it_poaccount it_poaccountx.
  ENDIF.

  "[U2 변경시작 2022.07.14].
*Reservation Profitability Segment: BAPI_PROFITABILITY_SEGMENT
  IF it_poaccountprofitsegment[] IS NOT INITIAL.
    PERFORM account_profit TABLES it_poaccountprofitsegment.
  ENDIF.
  "[U2 변경종료 2022.07.14].

*Conditions
  IF it_pocond[] IS NOT INITIAL.
    PERFORM condition_info TABLES it_pocond it_pocondx.
  ENDIF.


*Header Texts
  IF it_potextheader[] IS NOT INITIAL.
    PERFORM textheader_info TABLES it_potextheader.
  ENDIF.


*Partner
  IF it_popartner[] IS NOT INITIAL.
    PERFORM partner_info TABLES it_popartner.
  ENDIF.


*BAPI Structure for Components
  IF it_pocomponents[] IS NOT INITIAL.
    PERFORM components_info TABLES it_pocomponents it_pocomponentsx.
  ENDIF.


*Shipping Data for Stock Transport Orders
  IF it_poshipping[] IS NOT INITIAL.
    PERFORM shipping_info TABLES it_poshipping it_poshippingx.
  ENDIF.

*intercompany po check.
  PERFORM intercompany_po_check CHANGING lv_no_mess.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder          = gv_ebeln
      poheader               = gs_poheader
      poheaderx              = gs_poheaderx
      no_messaging           = lv_no_mess
      no_message_req         = abap_true
      no_authority           = gv_authority
      no_price_from_po       = gv_price_from_po
      testrun                = iv_testrun
    IMPORTING
      expheader              = gv_header
    TABLES
      return                 = gt_return
      poitem                 = gt_poitem
      poitemx                = gt_poitemx
      poaddrdelivery         = gt_poaddrdelivery
      poschedule             = gt_poschedule
      poschedulex            = gt_poschedulex
      poaccount              = gt_poaccount
      poaccountx             = gt_poaccountx
      poaccountprofitsegment = gt_poaccountprofitsegment
      pocond                 = gt_pocond
      pocondx                = gt_pocondx
      extensionin            = gt_extensionin
*EXTENSIONOUT = gt_extensionout
      potextheader           = gt_potextheader
      popartner              = gt_popartner
      pocomponents           = gt_pocomponents
      pocomponentsx          = gt_pocomponentsx
      poshipping             = gt_poshipping
      poshippingx            = gt_poshippingx.

*결과값.
  ev_header = gv_header.
  et_return[] = gt_return[].


  CHECK iv_testrun IS INITIAL.

  SORT gt_return BY type.
  READ TABLE gt_return WITH KEY type = 'S' BINARY SEARCH
                                   TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.



ENDFUNCTION.
