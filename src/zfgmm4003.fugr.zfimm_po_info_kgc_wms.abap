*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZFIMM_ID_INFO_KGC_3PL
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0210051
*& Created On          : 2021.03.08
*& Type                : RFNC
*& Description         : PO 정보 송신 (SAP -> KGC WMS)
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N       T0210051      2021.03.08          최초생성
*&    U1                                        변경내용 기재1
*&    U2                                        변경내용 기재2
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FUNCTION ZFIMM_PO_INFO_KGC_WMS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_HEADER) TYPE  ZSCN_IF_HEADER
*"  EXPORTING
*"     VALUE(ES_HEADER) TYPE  ZSCN_IF_HEADER
*"  TABLES
*"      IT_POHEADER STRUCTURE  ZSMM_PO_WMSHEADER_KGC
*"      IT_POITEM STRUCTURE  ZSMM_PO_WMSITEM_KGC
*"----------------------------------------------------------------------

  IF IS_HEADER-IF_ID IS INITIAL.
    EXIT.
  ENDIF.

ENDFUNCTION.
