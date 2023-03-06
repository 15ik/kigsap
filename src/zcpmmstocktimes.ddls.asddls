@AbapCatalog.sqlViewName: 'ZSVPMMSTOCKTIMES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]기간 유형 별 재고 조회(REPORT용)'
define view ZCPMMSTOCKTIMES
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
 p_startdate : vdm_v_start_date,
 p_enddate : vdm_v_end_date,
 p_periodtype : nsdm_period_type
as select from ZCCMMSTOCKTIMES (p_startdate: $parameters.p_startdate,
                                         p_enddate : $parameters.p_enddate,
                                         p_periodtype :$parameters.p_periodtype)
{
  key BUKRS,
  key CompanyCodeName,
  key matnr,
  key MaterialName,
  key werks,
  key PlantName,
  key LGORT,
  key StorageLocationName,
  key LIFNR,
  key NAME1,
  key Batch,
  key InventorySpecialStockType,
  key PeriodType,
  key YearPeriod,
  key zdate,
  key TYPENAME,
  key TYPECODE,
  sum(stockqty) as STOCKQTY,
  MEINS,
  LICHN,
  HSDAT,
  VFDAT,
  ekgrp,
  eknam,
  dispo,
  dsnam,
  matkl

}
group by BUKRS, CompanyCodeName, matnr, MaterialName, werks, PlantName, LGORT, StorageLocationName, LIFNR , NAME1 , Batch,
         InventorySpecialStockType, PeriodType, YearPeriod, zdate, TYPECODE,TYPENAME, MEINS, LICHN, HSDAT, VFDAT, ekgrp, eknam, dispo, dsnam,
         matkl
