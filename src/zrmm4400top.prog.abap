*&---------------------------------------------------------------------*
*& Include          ZRMM4400TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES :T001, T001W, T001L, LFA1, MARA, MARC, T024D, MCH1, MBEW.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
*>TYPE

CONSTANTS: GC_COND_SL TYPE ZSVCMM_REALSTOCK-TYPE VALUE 'SL_STOCK',
           GC_COND_SC         TYPE ZSVCMM_REALSTOCK-TYPE VALUE 'SC_STOCK',
           GC_COND_CS         TYPE ZSVCMM_REALSTOCK-TYPE VALUE 'CS_STOCK',
           GC_COND_SO         TYPE ZSVCMM_REALSTOCK-TYPE VALUE 'SO_STOCK',

*           GC_KOR_SL(10)      TYPE C VALUE '창고재고',              "U5 - TEXT 심볼로 대체
*           GC_KOR_SC(10)      TYPE C VALUE 'SC재고',
*           GC_KOR_CS(10)      TYPE C VALUE '위탁재고',
*           GC_KOR_SO(10)      TYPE C VALUE '고객재고',

           GC_C110(10)        VALUE 'C110',
           GC_C200(10)        VALUE 'C200',
           GC_C201(10)        VALUE 'C201',
           GC_C210(10)        VALUE 'C210',
           GC_C300(10)        VALUE 'C300',
           GC_C310(10)        VALUE 'C310',
           GC_C400(10)        VALUE 'C400',
           GC_C500(10)        VALUE 'C500',
           GC_C510(10)        VALUE 'C510',
           GC_C600(10)        VALUE 'C600',
           GC_C610(10)        VALUE 'C610',
           GC_C700(10)        VALUE 'C700',
           GC_C710(10)        VALUE 'C710',

           GC_1000(4)         VALUE '1000',
           GC_3101(4)         VALUE '3101',
           GC_BUKRS(7)        VALUE 'P_BUKRS',
           GC_WERKS(7)        VALUE 'S_WERKS',
           GC_LGORT(7)        VALUE 'S_LGORT',
           GC_DISPO(7)        VALUE 'S_DISPO',

           GC_MATNAME(10)     TYPE C VALUE 'MATNAME',
           GC_ZRMM4400_01(20) TYPE C VALUE 'ZRMM4400_01',

           GC_RA(2)           TYPE C VALUE 'RA',

           GC_00000000        TYPE VFDAT VALUE '00000000'.
*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATA,
         PLANT           TYPE ZSVCMM_REALSTOCK-PLANT,
         PLANTNAME       TYPE ZSVCMM_REALSTOCK-PLANTNAME,
         SLOCATION       TYPE ZSVCMM_REALSTOCK-SLOCATION,
         SLNAME          TYPE ZSVCMM_REALSTOCK-SLNAME,
         BP              TYPE ZSVCMM_REALSTOCK-BP,
         BPNAME          TYPE ZSVCMM_REALSTOCK-BPNAME,
         MATERIAL        TYPE ZSVCMM_REALSTOCK-MATERIAL,
         MATNAME         TYPE ZSVCMM_REALSTOCK-MATNAME,
         MATKL           TYPE ZSVCMM_REALSTOCK-MATKL,
         WGBEZ           TYPE T023T-WGBEZ,
         TYPE            TYPE ZSVCMM_REALSTOCK-TYPE,
         BATCH           TYPE ZSVCMM_REALSTOCK-BATCH,
         BWTAR           TYPE ZSVCMM_REALSTOCK-BWTAR,
         MEINS           TYPE ZSVCMM_REALSTOCK-MEINS,
         AVAILABLESTOCK  TYPE ZSVCMM_REALSTOCK-AVAILABLESTOCK,
         QISTOCK         TYPE ZSVCMM_REALSTOCK-QISTOCK,
         BLOCKSTOCK      TYPE ZSVCMM_REALSTOCK-BLOCKSTOCK,
         BDMNG           TYPE RESB-BDMNG,
         GLMNG           TYPE LIPS-LFIMG,
         WEMNG           TYPE LIPS-LFIMG,
         AVE_STOCK       TYPE DMENG,
         AVE_STOCK_C(20) TYPE C,
         CHAR_BP         TYPE ZSVCMM_REALSTOCK-CHAR_BP,
         NAME1           TYPE LFA1-NAME1,
         MAKER           TYPE ZSVCMM_REALSTOCK-MAKER,
         PRDDUCTLOT      TYPE ZSVCMM_REALSTOCK-PRDDUCTLOT,
         PRDUCTIONDATE   TYPE ZSVCMM_REALSTOCK-PRDUCTIONDATE,
         EXPIREDDATE     TYPE ZSVCMM_REALSTOCK-EXPIREDDATE,
         REMAINDATE      TYPE ZSVCMM_REALSTOCK-REMAINDATE,
         REMAINDATE_2(4) TYPE C,
         TYPE_KOR(20)    TYPE C,
         EKGRP           TYPE ZSVCMM_REALSTOCK-EKGRP,
         EKNAM           TYPE ZSVCMM_REALSTOCK-EKNAM,
         DISPO           TYPE ZSVCMM_REALSTOCK-DISPO,
         DSNAM           TYPE ZSVCMM_REALSTOCK-DSNAM,
         ZMEINS          TYPE ZSVCMM_REALSTOCK-MEINS,
         "[U4 변경시작 2022.07.04].
         LGPBE           TYPE LGPBE,
         ZSTO(1),            "TYPE ZTMD11020-ZZSHELFTYPE,
         "[U4 변경종료 2022.07.04].
         BKLAS          TYPE MBEW-BKLAS,"U11
       END OF TY_DATA.

DATA : BEGIN OF GS_VBBE,
         WERKS TYPE VBBE-WERKS,
         LGORT TYPE VBBE-LGORT,
         MATNR TYPE VBBE-MATNR,
         CHARG TYPE VBBE-CHARG,
         OMENG TYPE VBBE-OMENG,
       END OF GS_VBBE.

DATA : BEGIN OF GS_RESB,
         WERKS TYPE RESB-WERKS,
         LGORT TYPE RESB-LGORT,
         MATNR TYPE RESB-MATNR,
         CHARG TYPE RESB-CHARG,
         BWTAR TYPE BWTAR,
         BDMNG TYPE RESB-BDMNG,
       END OF GS_RESB.

DATA : BEGIN OF GS_MDUB,
         WERKS          TYPE MDUB-WERKS,
         LGORT          TYPE EKPO-LGORT,
         LGOBE          TYPE T001L-LGOBE,
         MATNR          TYPE LIPS-MATNR,
         CHARG          TYPE LIPS-CHARG,
         LFIMG          TYPE LIPS-LFIMG,
         MEINS          TYPE MDUB-MEINS,
         EBELN          TYPE MDUB-EBELN,
         EBELP          TYPE MDUB-EBELP,
         VGPOS          TYPE LIPS-VGPOS,
         VBELN          TYPE LIPS-VBELN,
         POSNR          TYPE LIPS-POSNR,
         PRODUCTIONDATE TYPE ZCCMM_BATCHFIND-HSDAT,
         EXPIREDDATE    TYPE ZCCMM_BATCHFIND-VFDAT,
         PRDDUCTLOT     TYPE ZCCMM_BATCHFIND-LICHN,
         EKGRP          TYPE MARC-EKGRP,
         DISPO          TYPE MARC-DISPO,
         MATKL          TYPE MARA-MATKL,
         NAME1          TYPE T001W-NAME1,
         MAKTX          TYPE MAKT-MAKTX,
       END OF GS_MDUB.

DATA : BEGIN OF GS_MDUB_SUM,
         WERKS TYPE MDUB-WERKS,
         LGORT TYPE EKPO-LGORT,
         MATNR TYPE LIPS-MATNR,
         CHARG TYPE LIPS-CHARG,
         LFIMG TYPE LIPS-LFIMG,
       END OF GS_MDUB_SUM.

DATA : BEGIN OF GS_GRINFO,
         WERKS          TYPE MDUB-WERKS,
         LGORT          TYPE EKPO-LGORT,
         LGOBE          TYPE T001L-LGOBE,
         MATNR          TYPE LIPS-MATNR,
         CHARG          TYPE LIPS-CHARG,
         WEMNG          TYPE LIPS-LFIMG,
         MEINS          TYPE MDUB-MEINS,
         EBELN          TYPE MDUB-EBELN,
         EBELP          TYPE MDUB-EBELP,
         VGPOS          TYPE LIPS-VGPOS,
         VBELN          TYPE LIPS-VBELN,
         POSNR          TYPE LIPS-POSNR,
         PRODUCTIONDATE TYPE ZCCMM_BATCHFIND-HSDAT,
         EXPIREDDATE    TYPE ZCCMM_BATCHFIND-VFDAT,
         PRDDUCTLOT     TYPE ZCCMM_BATCHFIND-LICHN,
         EKGRP          TYPE MARC-EKGRP,
         DISPO          TYPE MARC-DISPO,
         MATKL          TYPE MARA-MATKL,
         NAME1          TYPE T001W-NAME1,
         MAKTX          TYPE MAKT-MAKTX,
       END OF GS_GRINFO.



DATA : BEGIN OF GS_MDBS,
         EBELN          TYPE MDUB-EBELN,
         EBELP          TYPE MDUB-EBELP,
         WERKS          TYPE MDUB-WERKS,
         LGORT          TYPE MDUB-LGORT,
         LGOBE          TYPE T001L-LGOBE,
         VBELN          TYPE LIPS-VBELN,
         POSNR          TYPE LIPS-POSNR,
         MATNR          TYPE LIPS-MATNR,
         CHARG          TYPE LIPS-CHARG,
         LFIMG          TYPE LIPS-LFIMG,
         MEINS          TYPE MDUB-MEINS,
         PRODUCTIONDATE TYPE ZCCMM_BATCHFIND-HSDAT,
         EXPIREDDATE    TYPE ZCCMM_BATCHFIND-VFDAT,
         PRDDUCTLOT     TYPE ZCCMM_BATCHFIND-LICHN,
         EKGRP          TYPE MARC-EKGRP,
         DISPO          TYPE MARC-DISPO,
         MATKL          TYPE MARA-MATKL,
         NAME1          TYPE T001W-NAME1,
         MAKTX          TYPE MAKT-MAKTX,
         POSEX          TYPE VBAP-POSEX,
         WEMPF          TYPE MATDOC-WEMPF,
         LGPLA          TYPE MATDOC-LGPLA,
       END OF GS_MDBS.




"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
         NEW,                    "신규데이타
       END OF  TS_DISP.

TYPES: BEGIN OF TY_CHANGE,
         ZDATE          TYPE ZSVCMM_REALSTOCK-PRDUCTIONDATE,
         AVAILABLESTOCK TYPE ZSVCMM_REALSTOCK-AVAILABLESTOCK,
         QISTOCK        TYPE ZSVCMM_REALSTOCK-QISTOCK,
         MEINS          TYPE ZSVCMM_REALSTOCK-MEINS,
         ZMEINS         TYPE ZSVCMM_REALSTOCK-MEINS,
       END OF TY_CHANGE.
*----------------------------------------------------------------------*
* ALV Object
*----------------------------------------------------------------------*
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID. " ALV Gridv

DATA : GRF_DE_CON TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GRF_DE_GRID  TYPE REF TO CL_GUI_ALV_GRID,
       GT_DE_FIELD  TYPE LVC_T_FCAT, "Fieldcatalog
       GS_DE_LAYOUT TYPE LVC_S_LAYO.
*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DATA TYPE TABLE OF TY_DATA.
DATA: GT_DISP TYPE TABLE OF TS_DISP.

DATA : GT_VBBE LIKE TABLE OF GS_VBBE,
       GT_RESB     LIKE TABLE OF GS_RESB,
       GT_MDUB     LIKE TABLE OF GS_MDUB,
       GT_MDUB_SUM LIKE TABLE OF GS_MDUB_SUM,
       GT_MDBS     LIKE TABLE OF GS_MDBS,
       GT_GRINFO   LIKE TABLE OF GS_GRINFO.







DATA: GS_CHANGE TYPE TY_CHANGE,
      GS_DISP   TYPE TS_DISP.





DATA: OK_CODE TYPE SY-UCOMM,
      GV_MODE           TYPE C VALUE '',                          "Display :D  Edit:E (화면편집)
      GV_OK_CODE        TYPE SY-UCOMM,
      OK_CODE_DETAIL    TYPE SY-UCOMM,
      GV_OK_CODE_DETAIL TYPE SY-UCOMM,
      GV_MODE_DETAIL,
      GV_APPLIED_BUKRS  TYPE C,
      GV_OPT            TYPE C.

DATA: GV_TCNT TYPE SY-TABIX.

DATA : GS_VARIANT TYPE DISVARIANT. " Variant
DATA : GV_NAME1 TYPE T001W-NAME1,
       GV_LGOBE TYPE T001L-LGOBE,
       GV_DISPO TYPE T024D-DISPO.

DATA: GR_APPLIED_BUKRS TYPE RANGE OF T001-BUKRS.
DATA: GR_VENDOR_INFO TYPE RANGE OF EKKO-LIFNR.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
************************************************************************
DEFINE _G_APPEND_VALUE.
  &1-OPTION = &2.
  &1-SIGN   = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  APPEND &1.
END-OF-DEFINITION.

DEFINE _G_INIT.

  CLEAR:&1, &1[].

END-OF-DEFINITION .

DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .
