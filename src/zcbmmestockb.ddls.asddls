@AbapCatalog.sqlViewName: 'ZSVBMMSTOCKB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '[재고수불부]-MATDOC 기초 데이터'
@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.dataClass:#MIXED
@ClientHandling.algorithm: #SESSION_VARIABLE

define view ZCBMMESTOCKB
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
    P_EndDate:    sydate
as
select from matdoc
association [1..1] to I_CompanyCode               as _CompanyCode               on $projection.CompanyCode = _CompanyCode.CompanyCode
association [1..1] to I_Plant                     as _Plant                     on $projection.Plant = _Plant.Plant
association [0..1] to I_StorageLocation           as _StorageLocation           on $projection.Plant = _StorageLocation.Plant and $projection.StorageLocation = _StorageLocation.StorageLocation
association [0..1] to I_Supplier                  as _Supplier                  on $projection.Supplier = _Supplier.Supplier
association [0..1] to I_Customer                  as _Customer                  on $projection.Customer = _Customer.Customer
association [1..1] to I_InventoryStockType        as _InventoryStockType        on $projection.InventoryStockType = _InventoryStockType.InventoryStockType
association [1..1] to I_InventorySpecialStockType as _InventorySpecialStockType on $projection.InventorySpecialStockType = _InventorySpecialStockType.InventorySpecialStockType
association [1..1] to I_UnitOfMeasure             as _UnitOfMeasure             on $projection.MaterialBaseUnit = _UnitOfMeasure.UnitOfMeasure
association [1..1] to V_MaterialStockValueByKeyDate as _MaterialStockValueByKeyDate on  $projection.CostEstimate =  _MaterialStockValueByKeyDate.CostEstimate
                                                                                    and matdoc.kalnr             <> '000000000000'
association [1..1] to V_MaterialLedgerPrice         as _MaterialLedgerPrice         on  $projection.CostEstimate =  _MaterialLedgerPrice.CostEstimate
                                                                                    and matdoc.kalnr             <> '000000000000'
association [0..1] to I_CurrentMatlPriceByCostEst as _CurrentInvtryPrice        on  $projection.CostEstimate = _CurrentInvtryPrice.CostEstimate
                                                                                    and matdoc.kalnr             <> '000000000000'
{
  key matbf                          as Material,
  key werks                          as Plant,
  key cast(lgort_sid as lgort_d)     as StorageLocation,
  key cast(charg_sid as charg_d)     as Batch,
  key cast(lifnr_sid as md_supplier) as Supplier,
  key mat_kdauf                      as SDDocument,
  key mat_kdpos                      as SDDocumentItem,
  key mat_pspnr                      as WBSElementInternalID,
  key cast(kunnr_sid as kunnr)       as Customer,
  key lbbsa_sid                      as InventoryStockType,
  key sobkz                          as InventorySpecialStockType,
  key bukrs                          as CompanyCode,
  key kalnr                          as CostEstimate,
  key _Plant.ValuationArea as ValuationArea,
  key bwtar as InventoryValuationType,
  key xobew as IsSupplierStockValuation,
  key kzbws as InventorySpecialStockValnType,
  @Semantics.currencyCode: true
  key _CompanyCode.Currency as Currency,
  @Semantics.unitOfMeasure
  key meins                        as MaterialBaseUnit,
  @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
  @DefaultAggregation : #SUM
     sum(stock_qty)                     as MatlWrhsStkQtyInMatlBaseUnit,
     _UnitOfMeasure,
     _CompanyCode,
     _Plant,
     _StorageLocation,
     _Supplier,
     _Customer,
     _InventoryStockType,
     _InventorySpecialStockType,
     _MaterialStockValueByKeyDate,
     _MaterialLedgerPrice,
     _CurrentInvtryPrice
} where meins <> '' and budat < $parameters.P_EndDate
  group by kalnr, matbf, werks, lgort_sid, charg_sid,
           lifnr_sid, mat_kdauf, mat_kdpos, mat_pspnr, kunnr_sid, lbbsa_sid, sobkz,
           bukrs, _Plant.ValuationArea, bwtar, xobew, kzbws,
           meins, _CompanyCode.Currency
