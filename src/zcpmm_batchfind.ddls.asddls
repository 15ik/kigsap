@AbapCatalog.sqlViewName: 'ZSVPMM_BATFIND'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]배치속성 조회 COMPOSITE VIEW'
define view ZCPMM_BATCHFIND as select from ZCBMM_BATCHFIND
{

    key MATNR,
    key CHARG,
    class,
    posnr,
    adzhl,
    atinn,
    atnam,
    atfor,
    atbez,
    aterf,
    ATWRT,

    case when HSDAT is null then cast('00000000' as abap.dats) else HSDAT end as HSDAT,
    case when VFDAT is null then cast('00000000' as abap.dats) else VFDAT end as VFDAT,
    case when LICHN is null then '' else LICHN end as LICHN,
    case when ZMAKER is null then '' else ZMAKER end as ZMAKER,
    case when waterratio is null then 0 else waterratio end as waterratio,
    case when potency is null then 0 else potency end as potency,
    case when net is null then 0 else net end as net,
    case when custlot is null then '' else custlot end as custlot
 }
