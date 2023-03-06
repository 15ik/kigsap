FUNCTION ZFMM_BOM_EXPL_MAT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IV_DATUV) TYPE  STKO-DATUV DEFAULT SY-DATUM
*"     VALUE(IV_MEHRS) TYPE  CHAR1 OPTIONAL
*"     VALUE(IV_ZFDLV) TYPE  CHAR1 OPTIONAL
*"     VALUE(IV_MDMPS) TYPE  CSDATA-XFELD DEFAULT ''
*"     VALUE(IV_STPST) TYPE  STPOX-STUFE OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      IT_MATNR STRUCTURE  ZSMM00001
*"      IT_STB STRUCTURE  ZSMM_STPOX OPTIONAL
*"      IT_MATCAT STRUCTURE  CSCMAT OPTIONAL
*"      IT_TOPMAT STRUCTURE  CSCMAT OPTIONAL
*"----------------------------------------------------------------------

  DATA : LS_TOPMAT    TYPE CSTMAT,
         LT_STB       TYPE TABLE OF STPOX,
         LT_MATCAT    TYPE TABLE OF CSCMAT.

  IF IT_MATNR[] IS INITIAL.
    EV_RETURN-TYPE = 'E'.
    EV_RETURN-MESSAGE = TEXT-M04. "Check Required entry
    EXIT.
  ENDIF.

  LOOP AT IT_MATNR ASSIGNING FIELD-SYMBOL(<LS_MATNR>).

    CLEAR : LS_TOPMAT, LT_STB[], LT_MATCAT[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        AUSKZ                 = 'X'            "Take Scrap into Accounr
        CAPID                 = 'PP01'         "고정값
        DATUV                 = IV_DATUV
        EMENG                 = <LS_MATNR>-EMENG
        MBWLS                 = 'X'            "Read Material Valuation
        MKTLS                 = 'X'            "자재 내역 조회
        MEHRS                 = IV_MEHRS
        MDMPS                 = IV_MDMPS        "21.03.04 추가
* MMORY = '1'
* RNDKZ = '1' "Round off(''- always, '1' - never)
        MTNRV                 = <LS_MATNR>-MATNR
        WERKS                 = <LS_MATNR>-WERKS
        VERID                 = <LS_MATNR>-VERID "Production version
        STPST                 = IV_STPST
      IMPORTING
        TOPMAT                = LS_TOPMAT
      TABLES
        STB                   = LT_STB
        MATCAT                = LT_MATCAT
      EXCEPTIONS
        ALT_NOT_FOUND         = 1
        CALL_INVALID          = 2
        MATERIAL_NOT_FOUND    = 3
        MISSING_AUTHORIZATION = 4
        NO_BOM_FOUND          = 5
        NO_PLANT_DATA         = 6
        NO_SUITABLE_BOM_FOUND = 7
        CONVERSION_ERROR      = 8
        OTHERS                = 9.

* CALL FUNCTION 'CS_BOM_EXPL_MAT_V2_HANA'
* EXPORTING
* AUSKZ = 'X' "Take Scrap into Accounr
* CAPID = 'PP01' "고정값
* DATUV = IV_DATUV
* EMENG = <LS_MATNR>-EMENG
* MBWLS = 'X' "Read Material Valuation
* MKTLS = 'X' "자재 내역 조회
* MEHRS = IV_MEHRS
* MDMPS = IV_MDMPS "21.03.04 추가
** MMORY = '1'
* RNDKZ = '1' "Round off(''- always, '1' - never)
* MTNRV = <LS_MATNR>-MATNR
* WERKS = <LS_MATNR>-WERKS
* VERID = <LS_MATNR>-VERID "Production version
* IMPORTING
* TOPMAT = LS_TOPMAT
* CHANGING
* CT_STB = LT_STB
* CT_MATCAT = LT_MATCAT
* EXCEPTIONS
* ALT_NOT_FOUND = 1
* CALL_INVALID = 2
* MATERIAL_NOT_FOUND = 3
* MISSING_AUTHORIZATION = 4
* NO_BOM_FOUND = 5
* NO_PLANT_DATA = 6
* NO_SUITABLE_BOM_FOUND = 7
* CONVERSION_ERROR = 8
* NOT_SUPPORTED = 9
* DB_NOT_SUPPORTED = 10
* NO_VALID_TO = 11
* OTHERS = 12.

    IF SY-SUBRC = 0.

      IF LT_STB[] IS NOT INITIAL.

        LOOP AT LT_STB INTO DATA(LS_STB).

          MOVE-CORRESPONDING LS_STB TO IT_STB.
          IT_STB-MATNR = <LS_MATNR>-MATNR.

          APPEND IT_STB. CLEAR IT_STB.

        ENDLOOP.

      ENDIF.

      MOVE-CORRESPONDING LS_TOPMAT TO IT_TOPMAT.
      APPEND IT_TOPMAT. CLEAR IT_TOPMAT.

    ENDIF.
  ENDLOOP.

  IF IT_STB[] IS NOT INITIAL.
    EV_RETURN-TYPE = 'S'.
    EV_RETURN-MESSAGE = TEXT-M05.     "SUCCESS

  ELSE.

    EV_RETURN-TYPE = 'E'.
    EV_RETURN-MESSAGE = TEXT-M06.     "No Data Found

  ENDIF.

ENDFUNCTION.
