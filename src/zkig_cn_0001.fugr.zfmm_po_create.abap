FUNCTION ZFMM_PO_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_POHEADER) TYPE  ZSMM_POHEADER
*"     VALUE(IS_POHEADERX) TYPE  ZSMM_POHEADERX OPTIONAL
*"     VALUE(IV_AUTH) TYPE  BAPIFLAG OPTIONAL
*"     VALUE(IV_PRICE_FROM_PO) TYPE  BAPIFLAG OPTIONAL
*"     VALUE(IV_TESTRUN) TYPE  CHAR1 OPTIONAL
*"     VALUE(IV_COMMIT) TYPE  CHAR1 DEFAULT 'X'
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
*"*"Local interface:
*" IMPORTING
*" VALUE(IS_POHEADER) TYPE ZSMM_POHEADER
*" VALUE(IS_POHEADERX) TYPE ZSMM_POHEADERX OPTIONAL
*" VALUE(IS_POHEADEROTHER) TYPE ZSMM_POHEADEROTHER OPTIONAL
*" VALUE(IS_POHEADEROTHERX) TYPE ZSMM_POHEADEROTHERX OPTIONAL
*" VALUE(IV_AUTH) TYPE BAPIFLAG OPTIONAL
*" VALUE(IV_PRICE_FROM_PO) TYPE BAPIFLAG OPTIONAL
*" VALUE(IV_TESTRUN) TYPE CHAR1 OPTIONAL
*" VALUE(IV_COMMIT) TYPE CHAR1 DEFAULT 'X'
*" EXPORTING
*" VALUE(EV_PURCHASEORDER) LIKE BAPIMEPOHEADER-PO_NUMBER
*" VALUE(EV_HEADER) LIKE BAPIMEPOHEADER STRUCTURE BAPIMEPOHEADER
*" TABLES
*" ET_RETURN STRUCTURE BAPIRET2 OPTIONAL
*" IT_POITEM STRUCTURE ZSMM_POITEM OPTIONAL
*" IT_POITEMX STRUCTURE ZSMM_POITEMX OPTIONAL
*" IT_POADDRDELIVERY STRUCTURE ZSMM_POADDRDELIVERY OPTIONAL
*" IT_POSCHEDULE STRUCTURE ZSMM_POSCHEDULE OPTIONAL
*" IT_POSCHEDULEX STRUCTURE ZSMM_POSCHEDULEX OPTIONAL
*" IT_POACCOUNT STRUCTURE ZSMM_POACCOUNT OPTIONAL
*" IT_POACCOUNTX STRUCTURE ZSMM_POACCOUNTX OPTIONAL
*" IT_POCOND STRUCTURE ZSMM_POCOND OPTIONAL
*" IT_POCONDX STRUCTURE ZSMM_POCONDX OPTIONAL
*" IT_POTEXTHEADER STRUCTURE ZSMM_POTEXTHEADER OPTIONAL
*" IT_POPARTNER STRUCTURE ZSMM_POPARTNER OPTIONAL
*" IT_POCOMPONENTS STRUCTURE ZSMM_POCOMPONENTS OPTIONAL
*" IT_POCOMPONENTSX STRUCTURE ZSMM_POCOMPONENTSX OPTIONAL
*" IT_POSHIPPING STRUCTURE ZSMM_POSHIPPING OPTIONAL
*" IT_POSHIPPINGX STRUCTURE ZSMM_POSHIPPINGX OPTIONAL
*" IT_POACCOUNTPROFITSEGMENT STRUCTURE
*" ZSMM_POACCOUNTPROFITSEGMENT OPTIONAL

*&---------------------------------------------------------------------*
*& Module : MM
*& 생성자 : T0210054
*& 생성일 : 2021.02.23
*& Description : 구매오더 생성 공통 FC
*& 1. 구매오더 생성을 위한 공통 함수
* 표준 BAPI인 BAPI_PO_CREATE1 을 참조하여 공통 함수로 개발
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ 변경자 변경일자 변경내용
*&---------------------------------------------------------------------*
*& U1 t0220213 2022.07.11 메세지 내역 변경
*& U2 t0220213 2022.07.14 impoert 테이블 추가
*& U3 t0220213 2022.09.01 삭제 지시자 추가
*&---------------------------------------------------------------------*

* Global data declarations

  CLEAR : gs_poheader,  gs_poheaderx,
  gt_poitem[],  gt_poitemx[], gt_poaddrdelivery[],
  gt_poschedule[], gt_poschedulex[],
  gt_poaccount[], gt_poaccountx[],
  "[U2 변경시작 2022.07.14].
  gt_poaccountprofitsegment[],
  "[U2 변경종료 2022.07.14].
  gt_pocond[], gt_pocondx[],
  gt_extensionin[],
  gt_potextheader[], gt_popartner[],
  gt_pocomponents[], gt_pocomponentsx[],
  gt_poshipping[],  gt_poshippingx[],
  gv_purchaseorder, gv_header, gt_return[], gs_return_commit.


  gv_authority = iv_auth.
  gv_price_from_po = iv_price_from_po.

  DATA : lc_she_check(1) TYPE c.

* DATA : gt_she_input LIKE TABLE OF zsmm_she_certi_input,
* gt_she-output LIKE TABLE OF zsmm_she_certi_output.
*
* DATA : lS_she_input LIKE zsmm_she_certi_input,
* lt_header TYPE zscn_if_header.
* CONSTANTS : lc_ZMAIN_CAT TYPE ztmm00002-zmain_cat VALUE 'A1',
* lc_ZMIDD_CAT TYPE ztmm00002-zmidd_cat VALUE 'AG001',
* lc_ZSMAL_CAT TYPE ztmm00002-zsmal_cat VALUE 'AG102'.
*
*
* SELECT zmain_cat,zmidd_cat,zsmal_cat,field1,field2
* FROM ztmm00002
* WHERE zmain_cat = @lc_ZMAIN_CAT
* AND zmidd_cat = @lc_ZMIDD_CAT AND zsmal_cat = @lc_ZSMAL_CAT
* INTO TABLE @DATA(lt_matkl).



*Header Data
  IF is_poheader IS NOT INITIAL.
    PERFORM header_info TABLES et_return
                                      USING is_poheader is_poheaderx.
    CHECK et_return[] IS INITIAL.
  ENDIF.


*Other Data
*  IF is_poheaderother IS NOT INITIAL.
*    PERFORM headerother_info USING is_poheaderother is_poheaderotherx.
*  ENDIF.

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


  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      testrun                = iv_testrun
      poheader               = gs_poheader
      poheaderx              = gs_poheaderx
      no_messaging           = abap_true
      no_message_req         = abap_true
      no_authority           = gv_authority
      no_price_from_po       = gv_price_from_po
    IMPORTING
      exppurchaseorder       = gv_purchaseorder
      expheader              = gv_header
*exppoexpimpheader =
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

  WAIT UP TO 1 SECONDS.

*결과값.
  ev_header = gv_header.
  et_return[] = gt_return[].


* 테스트가 아니면 COMMIT 수행.
  CHECK iv_testrun IS INITIAL.

  SORT gt_return BY type.
  READ TABLE gt_return WITH KEY type = 'E'
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    SORT gt_return BY type id number.
    READ TABLE gt_return WITH KEY type = 'S'
                                  id = '06'
                                  number = '017'
                         BINARY SEARCH
                         TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      "Commit 이 ''X' 이면 수행.('X' 아니더라도 결과 RETURN)
      IF iv_commit = 'X'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = gs_return_commit.

        IF gs_return_commit-type = 'E'.
          APPEND gs_return_commit TO et_return.
        ELSE.
          ev_purchaseorder  = gv_purchaseorder.
        ENDIF.
      ELSE.
        ev_purchaseorder  = gv_purchaseorder.
      ENDIF.
    ENDIF.
  ELSE.
    "Commit 이 ''X' 이면 수행.
    IF iv_commit = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
