FUNCTION-POOL ZFGMM4003.                    "MESSAGE-ID ..

**> 값 단위 별 변환
DEFINE _G_CONV_DATA_SAP_TO_EAI.

  ZCL_MM_COMMON=>CONV_DATA_SAP_TO_EAI( EXPORTING IV_VALUE  = &1
                                          IV_UNIT = &2
                                IMPORTING EV_VALUE   = &3 ).

END-OF-DEFINITION.
**> 구조체 별 변환
DEFINE _G_CONV_STRC_SAP_TO_EAI.

  ZCL_MM_COMMON=>CONV_STRUCTURE_SAP_TO_EAI( EXPORTING IS_STRUCTURE  = &1
                                IMPORTING ES_STRUCTURE   = &2 ).

END-OF-DEFINITION.

* INCLUDE LZFGMM4003D...                     " Local class definition
