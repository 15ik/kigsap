*&---------------------------------------------------------------------*
*& Include          ZRMM3010TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: ZSVCMMPR_RELEASE.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: GC_DYNNR_1000 TYPE SY-DYNNR VALUE '1000',
           GC_ALL        TYPE CHAR05   VALUE 'ALL'.

*U3> 외부적인 표시는 언어별로/내부적으로는 한글로 - START
CONSTANTS: GC_WFSTS_1 TYPE CHAR50 VALUE '결재대기',
           GC_WFSTS_2 TYPE CHAR50 VALUE '결재반려',
           GC_WFSTS_3 TYPE CHAR50 VALUE '결재중',
           GC_WFSTS_4 TYPE CHAR50 VALUE '반려',
           GC_WFSTS_5 TYPE CHAR50 VALUE '상신중',
           GC_WFSTS_6 TYPE CHAR50 VALUE '생성중',
           GC_WFSTS_7 TYPE CHAR50 VALUE '승인완료',
           GC_WFSTS_8 TYPE CHAR50 VALUE '접수대기',
           GC_WFSTS_9 TYPE CHAR50 VALUE '접수반려',
           GC_WFSTS_10 TYPE CHAR50 VALUE '접수완료'.
*U3> 외부적인 표시는 언어별로/내부적으로는 한글로 - END

* ALV 관련 상수
CONSTANTS: GC_EMPHSZ_C300 TYPE LVC_EMPHSZ VALUE 'C300'. "노란색

* GOS 관련 상수
CONSTANTS: GC_GOS_TYPEID TYPE SWO_OBJTYP VALUE 'BUS2105', "SWO1 등록한 OBJECT TYPE
           GC_GOS_CATID  TYPE SIBFCATID  VALUE 'BO'.      "Category of Objects "Instances of BOR Object Types

* Condition Type
CONSTANTS: GC_COND_TYPE TYPE ZSVBMMINFOPRICE-CONDITIONTYPE VALUE 'PB00',
           GC_CONFIRM   TYPE EINE-EXPRF VALUE 'CP'.


*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATA,
         APV_STAT          TYPE ICON-ID,
         STATUS            TYPE ICON-ID,
         WF_STAUS          TYPE ZSVCMMPR_RELEASE-WF_STAUS,                  "PR결재상태
         BUKRS             TYPE BUKRS,
         BSART_PR          TYPE ZSVCMMPR_RELEASE-BSART,                     "PR유형
         BANFN             TYPE ZSVCMMPR_RELEASE-BANFN,                     "구매요청
         BNFPO             TYPE ZSVCMMPR_RELEASE-BNFPO,                     "품목
         MATNR             TYPE ZSVCMMPR_RELEASE-MATNR,                     "자재
         TXZ01             TYPE ZSVCMMPR_RELEASE-TXZ01,                     "내역
         MENGE_PR          TYPE ZSVCMMPR_RELEASE-MENGE,                     "요청수량
         MEINS_PR          TYPE ZSVCMMPR_RELEASE-MEINS,                     "단위
         PSTYP             TYPE ZSVCMMPR_RELEASE-PSTYP,                     "임가공
         TOLL_MANUF        TYPE ICON-ID,
         BSMNG             TYPE ZSVCMMPR_RELEASE-BSMNG,                     "총PO수량
         GR_QTY            TYPE ZSVCMMPR_RELEASE-GR_QTY,                    "총입고
         VALID_COND,                                                        "유효단가 유무
         WERKS             TYPE ZSVCMMPR_RELEASE-WERKS,                     "플랜트
         ZDUPL             TYPE C,

         WF_STATUS         TYPE ZSVCMM_POSCHEDU-WF_STATUS,                  "PO결재상태
         BSTYP_PO          TYPE ZSVCMM_POSCHEDU-BSTYP,
         BSART_PO          TYPE ZSVCMM_POSCHEDU-BSART,                      "PO유형
         EBELN             TYPE ZSVCMM_POSCHEDU-EBELN,                      "구매오더
         EBELP             TYPE ZSVCMM_POSCHEDU-EBELP,                      "품목
         MENGE_PO          TYPE ZSVCMM_POSCHEDU-MENGE,                      "PO수량
         MEINS_PO          TYPE ZSVCMM_POSCHEDU-MEINS,                      "PO단위
         NETPR             TYPE ZSVCMM_POSCHEDU-NETPR,                      "구매단가
         WAERS             TYPE ZSVCMM_POSCHEDU-WAERS,                      "통화
         SUM_GRQTY         TYPE ZSVCMM_POSCHEDU-SUM_GRQTY,                  "PO별 입고

         BADAT             TYPE ZSVCMMPR_RELEASE-BADAT,                     "요청일
*         ZPEQ_DEPARTMENT   TYPE ZSVCMMPR_RELEASE-ZPEQ_DEPARTMENT,           "요청부서
*         DEPART_NAME_PR    TYPE ZSVMM_USER_INFO-DEPART_NAME,                "요청부서명
*         ZREQUESTER        TYPE ZSVCMMPR_RELEASE-ZREQUESTER,                "요청자
*         EMPLOY_NAME_PR    TYPE ZSVMM_USER_INFO-EMPLOY_NAME,                "요청자명
*         ZPRTITLE          TYPE ZSVCMMPR_RELEASE-ZPRTITLE,                  "요청명
         BATXT             TYPE ZSVCMMPR_RELEASE-BATXT,                     "문서유형내역
         PUR_CURR          TYPE ZSVCMMPR_RELEASE-PURREQNITEMCURRENCY,       "통화
         PREIS             TYPE ZSVCMMPR_RELEASE-PREIS,                     "추정단가
         PEINH             TYPE ZSVCMMPR_RELEASE-PEINH,                     "가격단위
         BAPRE             TYPE BSEG-DMBTR,                                 "추정금액(요청수량*추정단가)
         LFDAT             TYPE ZSVCMMPR_RELEASE-LFDAT,                     "납품일
         PLANTNAME         TYPE ZSVCMMPR_RELEASE-PLANTNAME,                 "플랜트명
         LGORT             TYPE ZSVCMMPR_RELEASE-LGORT,                     "저장위치
         LGOBE             TYPE ZSVCMMPR_RELEASE-LGOBE,                     "저장위치명
         LIFNR             TYPE EBAN-LIFNR,                                 "희망공급업체
         LIFNR_TEXT        TYPE LFA1-NAME1,
         FLIEF             TYPE EBAN-FLIEF,                                 "고정공급업체
         FLIEF_TEXT        TYPE LFA1-NAME1,
         EKGRP             TYPE ZSVCMMPR_RELEASE-EKGRP,                     "구매그룹
         EKNAM             TYPE ZSVCMMPR_RELEASE-EKNAM,                     "구매그룹명

*         ZORDER_PERSON     TYPE ZSVCMMPR_RELEASE-ZORDER_PERSON,             "계약담당자
*         EMPLOY_NAME_PO    TYPE ZSVMM_USER_INFO-EMPLOY_NAME,                "계약담당자명
*         ZORDER_DEPARTMENT TYPE ZSVCMMPR_RELEASE-ZORDER_DEPARTMENT,         "계약부서
*         DEPART_NAME_PO    TYPE ZSVMM_USER_INFO-DEPART_NAME,                "계약부서명
         MATKL             TYPE ZSVCMMPR_RELEASE-MATKL,                     "자재그룹
         MATKL_NAME        TYPE ZSVCMMPR_RELEASE-MATERIALGROUPNAME,         "자재그룹명
*         ZNOPRICE          TYPE ZSVCMMPR_RELEASE-ZNOPRICE,                  "미단가계약
*         ZPRE_INPUT        TYPE ZSVCMMPR_RELEASE-ZPRE_INPUT,                "선투입여부
*         ZURGENT           TYPE ZSVCMMPR_RELEASE-ZURGENT,                   "긴급여부

         SALESORDER        TYPE CPRACCTASSGMT-SALESORDER,                   "판매오더
         SALESORDERITEM    TYPE CPRACCTASSGMT-SALESORDERITEM,               "항목
         GLACCOUNT         TYPE CPRACCTASSGMT-GLACCOUNT,                    "계정
         COSTCENTER        TYPE CPRACCTASSGMT-COSTCENTER,                   "코스트센터
         FIXEDASSET        TYPE CPRACCTASSGMT-MASTERFIXEDASSET,             "고정자산
         ORDERID           TYPE CPRACCTASSGMT-ORDERID,                      "ORDER
         WBSELEMENT        TYPE CPRACCTASSGMT-WBSELEMENT,                   "WBS
         ZRECEIPT          TYPE ICON-ID,                                    "SRM/타시스템 전송 여부

*         APVIFKEY          TYPE ZTMM30031-APVIFKEY,
*         APVSTATUS         TYPE ZTMM30031-APVSTATUS,
*         ZMROP             TYPE ZTMM10020-PREIS,
*         ZMRO_CATALOG      TYPE ZTMM10020-ZMRO_CATALOG,
*         ZMRO_CATALOG_DE   TYPE ZTMM10020-ZMRO_CATALOG_DE,
*         MRO_WAERS         TYPE ZTMM10020-WAERS,
         ICON              TYPE ICON_D,                                     "MRO 단가(2개이상)
         WF_STAUS_TEXT    TYPE CHAR50,                                     "결재상태 내역
       END OF TY_DATA.

"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP.

* 첨부파일 정보
TYPES: BEGIN OF TY_ATTA,
         INSTID_A TYPE SIBFBORIID,
       END OF TY_ATTA.

* 단가 이력 조회
TYPES: BEGIN OF TY_COND_HIST,
         INFNR      TYPE ZSVBMMINFOPRICE-INFNR,                        "구매정보레코드
         ESOKZ      TYPE ZSVBMMINFOPRICE-PURCHASINGINFORECORDCATEGORY, "구매정보레코드 범주
         ESOKZ_TEXT TYPE VAL_TEXT,
         LIFNR      TYPE ZSVBMMINFOPRICE-LIFNR,                        "공급업체
         NAME1      TYPE ZSVBMMINFOPRICE-NAME1,                        "업체명
         MATNR      TYPE ZSVBMMINFOPRICE-MATNR,                        "자재
         MAKTX      TYPE ZSVBMMINFOPRICE-MATERIALDESCRIPTION,          "자재내역
         EKORG      TYPE ZSVBMMINFOPRICE-PURCHASINGORGANIZATION,       "구매조직
         WERKS      TYPE ZSVBMMINFOPRICE-PLANT,                        "플랜트
         NETPR      TYPE ZSVBMMINFOPRICE-CONDITIONRATEVALUE,           "금액
         PEINH      TYPE ZSVBMMINFOPRICE-CONDITIONQUANTITY,            "가격단위
         WAERS      TYPE ZSVBMMINFOPRICE-CONDITIONRATEVALUEUNIT,       "통화
         DATAB      TYPE ZSVBMMINFOPRICE-CONDITIONVALIDITYSTARTDATE,   "유효 시작일
         DATBI      TYPE ZSVBMMINFOPRICE-CONDITIONVALIDITYENDDATE,     "유효 종료일
       END OF TY_COND_HIST.

* 임가공 BOM 조회
TYPES: BEGIN OF TY_TOLL_MANUF,
         MATNR TYPE IPPSUBCONTRCOMP-MATERIAL,                  "자재
         MAKTX TYPE MAKT-MAKTX,                                "내역
         BDMNG TYPE IPPSUBCONTRCOMP-REQUIREDQUANTITY,          "소요량
         ERFME TYPE IPPSUBCONTRCOMP-ENTRYUNIT,                 "단위
         WERKS TYPE IPPSUBCONTRCOMP-PLANT,                     "플랜트
         LGORT TYPE IPPSUBCONTRCOMP-STORAGELOCATION,           "저장위치
         BDTER TYPE IPPSUBCONTRCOMP-REQUIREMENTDATE,           "소요일
         POSNR TYPE IPPSUBCONTRCOMP-BILLOFMATERIALITEMNUMBER,  "항목
         CHARG TYPE IPPSUBCONTRCOMP-BATCH,                     "배치
         AUSCH TYPE IPPSUBCONTRCOMP-COMPONENTSCRAPINPERCENT,   "구성품 스크랩
         BANFN TYPE IPPSUBCONTRCOMP-PURCHASEREQUISITION,
         BNFPO TYPE IPPSUBCONTRCOMP-PURCHASEREQUISITIONITEM,
       END OF TY_TOLL_MANUF.

* 결재 진행 정보
*TYPES: BEGIN OF TY_APV,
*         APVIFKEY  TYPE ZTMM30031-APVIFKEY,
*         FIID      TYPE ZTMM30031-FIID,
*         WFOBJECT  TYPE ZTMM30032-WFOBJECT,
*         APVSTATUS TYPE ZTMM30031-APVSTATUS,
*       END OF TY_APV.
*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: GRF_HEAD TYPE REF TO CL_GUI_CONTAINER, "Header Information Object
      GRF_BODY TYPE REF TO CL_GUI_CONTAINER.    "ALV Object

DATA: GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID. " ALV Grid

* 단가 이력 조회 Popup
DATA: GRF_CONT_HIST TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRF_GRID_HIST TYPE REF TO LCL_CUST_ALV_GRID.        " ALV Grid

* MRO 이력 조회 Popup
DATA: GRF_CONT_MRO TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRF_GRID_MRO TYPE REF TO LCL_CUST_ALV_GRID.        " ALV Grid

* BOM 조회 Popup
DATA: GRF_CONT_BOM TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRF_GRID_BOM TYPE REF TO LCL_CUST_ALV_GRID.         " ALV Grid

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DISP TYPE TABLE OF TY_DISP.

*DATA: GT_MRO TYPE TABLE OF ZTMM10020.

DATA: GT_COND TYPE TABLE OF TY_COND_HIST. "유효단가 유무 체크

*DATA: GT_USER_INFO TYPE TABLE OF ZSVMM_USER_INFO,
*      GT_DEPT_INFO TYPE TABLE OF ZTCN00001,
*      GT_APV       TYPE TABLE OF TY_APV.   "결재 진행 정보
DATA: GT_ATTA      TYPE HASHED TABLE OF TY_ATTA WITH UNIQUE KEY INSTID_A.

DATA: GT_COND_HIST TYPE TABLE OF TY_COND_HIST,
      GV_YEAR       TYPE T5A4A-DLYYR,            "구매단가 이력 조회 기간
      GT_TOLL_MANUF TYPE TABLE OF TY_TOLL_MANUF.

*DATA: GT_COND_MRO TYPE TABLE OF ZTMM10020.

DATA: OK_CODE TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      GV_MROW    TYPE I VALUE 10.    "Multi Row

* Selected Rows
DATA: GT_ROWS TYPE LVC_T_ROW.

DATA: GV_EKORG TYPE EKKO-EKORG.

DATA: GV_EXC_USER TYPE C.

DATA: GV_ORG1 TYPE CHAR10.

*외부적인 표시는 언어별로/내부적으로는 한글로
DATA: BEGIN OF GS_WF_STATUS,
      CODE TYPE CHAR50,
      NAME TYPE CHAR100,
      END OF GS_WF_STATUS,
      GT_WF_STATUS LIKE TABLE OF GS_WF_STATUS.
*U3> T2 용 적용 - END

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION.

DEFINE _G_SET_VALUES.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = &1
      values = &2.

END-OF-DEFINITION.
