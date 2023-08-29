*&---------------------------------------------------------------------*
*& Include          ZOMM3030TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: EBAN, SSCRFIELDS.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: GC_DYNNR_1000 TYPE SY-DYNNR VALUE '1000'.

* 결재 상태
CONSTANTS: GC_APV_DRAFT    TYPE ZAPVSTATUS VALUE 'DRAFT',    "기안    <- 상신완료
           GC_APV_APPROVAL TYPE ZAPVSTATUS VALUE 'APPROVAL', "결재    <- 결재진행중
           GC_APV_COMPLETE TYPE ZAPVSTATUS VALUE 'COMPLETE', "완료    <- 결재완료
           GC_APV_REJECT   TYPE ZAPVSTATUS VALUE 'REJECT',   "반려    <- 결재반려
           GC_APV_WITHDRAW TYPE ZAPVSTATUS VALUE 'WITHDRAW'. "회수    <- 결재회수

CONSTANTS: GC_APV_TYPE TYPE ZTCN00004-APVPSTYPE VALUE 'MM_PR_RELEASE'.

* ALV 관련 상수
CONSTANTS: GC_EMPHSZ_C300 TYPE LVC_EMPHSZ VALUE 'C300'. "노란색

* GOS 관련 상수
CONSTANTS: GC_GOS_TYPEID TYPE SWO_OBJTYP VALUE 'BUS2105', "SWO1 등록한 OBJECT TYPE
           GC_GOS_CATID  TYPE SIBFCATID  VALUE 'BO'.      "Category of Objects "Instances of BOR Object Types

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATA,
         APV_STAT          TYPE ICON-ID,
         BUKRS             TYPE T024E-BUKRS,                          "회사코드
         FRGKZ             TYPE ZSVCMMPR_RELEASE-FRGKZ,               "결재상태
*U1> 결재번역 (상태 자리수 증가)
*         WF_STATUS         TYPE ZSVCMMPR_RELEASE-WF_STAUS,            "결재상태 TEXT
         WF_STATUS         TYPE CHAR30,                               "결재상태 TEXT
         BANFN             TYPE ZSVCMMPR_RELEASE-BANFN,               "구매요청번호
         PR_LOCK           TYPE ICON-ID,
         BSART             TYPE ZSVCMMPR_RELEASE-BSART,               "문서유형
         BATXT             TYPE ZSVCMMPR_RELEASE-BATXT,               "문서유형내역
         TITLE             TYPE ZSVCMMPR_RELEASE-ZPRTITLE,            "구매요청명
         BACNT             TYPE I,                                    "품목수

         BNFPO             TYPE ZSVCMMPR_RELEASE-BNFPO,               "품목
         PSTYP             TYPE ZSVCMMPR_RELEASE-PSTYP,               "품목범주
         PSTYP_IND,                                                   "임가공여부
         MATNR             TYPE ZSVCMMPR_RELEASE-MATNR,               "자재
         TXZ01             TYPE ZSVCMMPR_RELEASE-TXZ01,               "자재내역
         MENGE             TYPE ZSVCMMPR_RELEASE-MENGE,               "요청수량
         MEINS             TYPE ZSVCMMPR_RELEASE-MEINS,               "단위
         BADAT             TYPE ZSVCMMPR_RELEASE-BADAT,               "요청일
         LFDAT             TYPE ZSVCMMPR_RELEASE-LFDAT,               "납품일
         WERKS             TYPE ZSVCMMPR_RELEASE-WERKS,               "플랜트
         PLANTNAME         TYPE ZSVCMMPR_RELEASE-PLANTNAME,

*         PREIS             TYPE EBAN-PREIS,
*         PEINH             TYPE EBAN-PEINH,
*         MENGE             TYPE EBAN-MENGE,
         BAPRE             TYPE EBAN-RLWRT,                           "정가(품목별 가격*수량의 합)
         WAERS             TYPE ZSVCMMPR_RELEASE-PURREQNITEMCURRENCY, "통화
         ZURGENT_REASON    TYPE ZSVCMMPR_RELEASE-ZURGENT_REASON,      "긴급사유
         ZPRE_INPUT_REASON TYPE ZSVCMMPR_RELEASE-ZPRE_INPUT_REASON,   "선투입사유

         APVIFKEY          TYPE ZTMM30031-APVIFKEY,
         APVSTATUS         TYPE ZTMM30031-APVSTATUS,
       END OF TY_DATA.

"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP.

* 첨부파일 정보
TYPES: BEGIN OF TY_ATTA,
         INSTID_A TYPE SIBFBORIID,
       END OF TY_ATTA.

* 결재 진행 정보
TYPES: BEGIN OF TY_APV,
         APVIFKEY  TYPE ZTMM30031-APVIFKEY,
         FIID      TYPE ZTMM30031-FIID,
         WFOBJECT  TYPE ZTMM30032-WFOBJECT,
         APVSTATUS TYPE ZTMM30031-APVSTATUS,
       END OF TY_APV.

TYPES: BEGIN OF TY_APV_RET,
         RESULT    TYPE BAPI_MTYPE,
         MESSAGE   TYPE BAPI_MSG,
         APVIFKEY  TYPE ZAPVIFKEY,
         FIID      TYPE ZAPVFIID,
         APVPSTYPE TYPE ZAPVPSTYPE,
       END OF TY_APV_RET.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,    "Header Information Object
      GRF_BODY TYPE REF TO CL_GUI_CONTAINER.    "ALV Object

DATA: GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID.   " ALV Grid

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DISP TYPE TABLE OF TY_DISP,
      GT_ATTA TYPE HASHED TABLE OF TY_ATTA WITH UNIQUE KEY INSTID_A,  "첨부파일 정보
      GT_APV  TYPE TABLE OF TY_APV.   "결재 진행 정보

DATA: GT_ITEM TYPE TABLE OF TY_DISP.  "품목 상세보기

* 전자결재 대상 PR
DATA: GT_DISP_APV TYPE TABLE OF TY_DISP,
      GT_APV_PR   TYPE TABLE OF ZSMM_BANFN.

* 전자결재 상신 결과
DATA: GS_APV_RET TYPE TY_APV_RET.

DATA: OK_CODE    TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      GV_MROW    TYPE I VALUE 10.    "Multi Row

* Selected Rows
DATA: GT_ROWS TYPE LVC_T_ROW.

DATA: GV_EXC_USER TYPE C.

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
      ID     = &1
      VALUES = &2.

END-OF-DEFINITION.
