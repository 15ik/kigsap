@AbapCatalog.sqlViewName: 'ZSVCMMSTOCKTIMES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]기간 유형 별 재고 조회(CC)'
define view ZCCMMSTOCKTIMES
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
 p_startdate : vdm_v_start_date,
 p_enddate : vdm_v_end_date,
 p_periodtype : nsdm_period_type
as select from ZCBMMDAYSTOCK (p_startdate: $parameters.p_startdate,
                                         p_enddate : $parameters.p_enddate,
                                         p_periodtype :$parameters.p_periodtype) as A
               left outer join ZCCMM_BATCHFIND as B on A.matnr = B.matnr and A.Batch = B.BATCH
{
  key A.BUKRS,
  key A.CompanyCodeName,
  key A.matnr,
  key A.MaterialName,
  key A.werks,
  key A.PlantName,
  key case when A.stocktype = '06' then 'SIT' else A.LGORT end as LGORT,
  key A.StorageLocationName,
  key A.Supplier as LIFNR,
  key A._Supplier.SupplierName as NAME1,
  key A.InventorySpecialStockType,
  key A.Batch,
  key A.PeriodType,
  key A.YearPeriod,
  key A.zdate,
  case when A.stocktype = '02' then '품질검사중재고'
       when A.stocktype = '07' then '보류재고'
       else '가용재고'
       end as TYPENAME,
  case when A.stocktype = '02' then '02'
       when A.stocktype = '07' then '03'
       else '01'
       end as TYPECODE,

  A.stockqty,
  A.MEINS,
  case when B.LICHN is null then '' else B.LICHN end as LICHN,
  case when B.hsdat is null then '' else B.hsdat end as HSDAT,
  case when B.VFdat is null then '' else B.VFdat end as VFDAT,
  A.ekgrp,
  A.eknam,
  A.dispo,
  A.dsnam,
  A.matkl
} where stockqty > 0 and stocktype <> '03'
