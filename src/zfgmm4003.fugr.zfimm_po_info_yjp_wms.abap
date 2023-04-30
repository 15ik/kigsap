*&---------------------------------------------------------------------*
*& Module      : MM
*& 생성자      : T0210051
*& 생성일      : 2021.03.08
*& Description : PO 정보 송신 (SAP -> YJP WMS)
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FUNCTION ZFIMM_PO_INFO_YJP_WMS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_HEADER) TYPE  ZSCN_IF_HEADER
*"  EXPORTING
*"     VALUE(ES_HEADER) TYPE  ZSCN_IF_HEADER
*"  TABLES
*"      IT_POHEADER STRUCTURE  ZSMM_PO_WMSHEADER
*"      IT_POITEM STRUCTURE  ZSMM_PO_WMSITEM_YJP
*"----------------------------------------------------------------------

  IF IS_HEADER-IF_ID IS INITIAL.
    EXIT.
  ENDIF.

ENDFUNCTION.
