FUNCTION-POOL ZKIG_CN_0004.                 "MESSAGE-ID ..

CONSTANTS: gc_bsart_txt(20) TYPE c VALUE '오더 유형',
           gc_ekgrp_txt(20) TYPE c VALUE '구매 그룹',
           gc_werks_txt(20) TYPE c VALUE '플랜트',
           gc_bnfpo_txt(20) TYPE c VALUE '구매요청 품목번호'.

DATA: gt_return TYPE TABLE OF bapiret2.


**> 값 단위 별 변환 (EAI TO SAP)
DEFINE _G_CONV_DATA_EAI_TO_SAP.

  ZCL_MM_COMMON=>CONV_DATA_EAI_TO_SAP( EXPORTING IV_VALUE  = &1
                                          IV_UNIT = &2
                                IMPORTING EV_VALUE   = &3 ).

END-OF-DEFINITION.
* INCLUDE LZKIG_CN_0004D...                  " Local class definition
