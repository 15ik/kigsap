@AbapCatalog.sqlViewName: 'ZSVBMMDAYSTOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]일자별 재고 현황(Timeseries)'

define view ZCBMMDAYSTOCK
with parameters 
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
    p_startdate : vdm_v_start_date,
    p_enddate : vdm_v_end_date,
    p_periodtype : nsdm_period_type
// Split Valuation 자재
as 
select distinct  from mara as A inner join I_MaterialStockTimeSeries(P_StartDate: $parameters.p_startdate,  //  KTGA-3910 distinct 추가  2023.01.11 
                                                           P_EndDate : $parameters.p_enddate,
                                                           P_PeriodType :$parameters.p_periodtype) as B on A.matnr = B.Material
                      inner join marc as c on B.Material = c.matnr and B.Plant = c.werks
                      inner join mbew as d on B.Material = d.matnr and B.Plant = d.bwkey  //  and B.Batch = d.bwtar   KTGA-3910 주석처리 2023.01.11 
                      association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp     
                      association [1]   to t024d as t024d  on $projection.werks = t024d.werks
                                                            and $projection.dispo = t024d.dispo        
{
  key B.CompanyCode as BUKRS,
  key B._CompanyCode.CompanyCodeName,
  key B.PeriodType,
  key B.YearPeriod,
  key B.EndDate as zdate,
  key B.Plant as werks,
  key B._Plant.PlantName,
  key B.StorageLocation as LGORT,
  key B._StorageLocation.StorageLocationName,
  key B.Material as matnr,
  key _Material._Text[1: Language= $session.system_language].MaterialName,
  key B.Batch,
  key B.Supplier,
  key B.Customer,
  key B.InventoryStockType as stocktype,
  key B.InventorySpecialStockType,
  key B.FiscalYearVariant,   
  @Semantics.unitOfMeasure  
  key B.MaterialBaseUnit as MEINS,
  @Semantics.quantity.unitOfMeasure: 'MEINS'
      cast( B.MatlWrhsStkQtyInMatlBaseUnit as abap.quan( 13, 3 ))  as stockqty,
      d.bklas,
      _UnitOfMeasure,
      _Material,
      _CompanyCode,
      _Plant,
      _StorageLocation,
      _Supplier,
      _Customer,
      _InventoryStockType,
      _InventorySpecialStockType,
      c.ekgrp,
      t024.eknam,
      c.dispo,
      t024d.dsnam,
      A.matkl      
} where c.bwtty <> ''

union all

// Non Split Valuation 자재
select from mara as A inner join I_MaterialStockTimeSeries(P_StartDate: $parameters.p_startdate,
                                                           P_EndDate : $parameters.p_enddate,
                                                           P_PeriodType :$parameters.p_periodtype) as B on A.matnr = B.Material
                      inner join marc as c on B.Material = c.matnr and B.Plant = c.werks
                      left outer join mbew as d on B.Material = d.matnr and B.Plant = d.bwkey 
                      association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp     
                      association [1]   to t024d as t024d  on $projection.werks = t024d.werks and $projection.dispo = t024d.dispo      
{
  key B.CompanyCode as BUKRS,
  key B._CompanyCode.CompanyCodeName,
  key B.PeriodType,
  key B.YearPeriod,
  key B.EndDate as zdate,
  key B.Plant as werks,
  key B._Plant.PlantName,
  key B.StorageLocation as LGORT,
  key B._StorageLocation.StorageLocationName,
  key B.Material as matnr,
  key _Material._Text[1: Language= $session.system_language].MaterialName,
  key B.Batch,
  key B.Supplier,
  key B.Customer,
  key B.InventoryStockType as stocktype,
  key B.InventorySpecialStockType,
  key B.FiscalYearVariant,   
  @Semantics.unitOfMeasure  
  key B.MaterialBaseUnit as MEINS,
  @Semantics.quantity.unitOfMeasure: 'MEINS'
      cast( B.MatlWrhsStkQtyInMatlBaseUnit as abap.quan( 13, 3 ))  as stockqty,
      case when d.bklas is null then cast('' as abap.char( 4 ))
           else d.bklas end as BKLAS,
      _UnitOfMeasure,
      _Material,
      _CompanyCode,
      _Plant,
      _StorageLocation,
      _Supplier,
      _Customer,
      _InventoryStockType,
      _InventorySpecialStockType,
      c.ekgrp,
      t024.eknam,
      c.dispo,
      t024d.dsnam,
      A.matkl        
} where c.bwtty = ''
