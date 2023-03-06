@AbapCatalog.sqlViewName: 'ZSVCMMEDAYSTOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-일자별 기초재고'
define view ZCCMMEDAYSTOCK
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_LANGUAGE
P_Language:   sylangu,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:  sydate,
//BUKRS ADD
P_Bukrs: bukrs

as select from ZCBMMETARGETR(P_Bukrs: $parameters.P_Bukrs) as A inner join ZCPMMESTOCKAG2(P_Language: $parameters.P_Language,
//as A left outer join ZCPMMESTOCKAG2(P_Language: $parameters.P_Language,
                                             P_EndDate : $parameters.P_EndDate) as B
                                             on A.bukrs = B.CompanyCode
                                             and A.werks = B.Plant
                                             and A.lgort = B.StorageLocation
                                             and A.charg = B.batch
                                             and A.LIFNR = B.supplier
                                             and A.matnr = B.Material
                                             and A.bwtar = B.InventoryValuationType

{

// Stock Identifier
key A.matnr,
// Further Stock Groups
key A.bukrs,
key A.werks,
key A.lgort,
key A.charg,

//key case when supplier is null then cast('' as abap.char(10))
//                           else supplier end as LIFNR,
key A.LIFNR,
key A.bwtar,
/*
SDDocument,
SDDocumentItem,
*/
WBSElementInternalID,
Customer,
//InventoryStockType,
InventorySpecialStockType,

// Quantity and Unit
A.meins as MaterialBaseUnit,
//MatlWrhsStkQtyInMatlBaseUnit,
case when Currency is null then cast('KRW' as abap.cuky( 5 )) else Currency end as CURRENCY,


//Quantity of Stock
sum(CurrentStock) as CurrentStock,
@Semantics.amount.currencyCode: 'Currency'
sum(CurrentStockValue) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
sum(CurrentStockCurVal) as CurrentStockCurVal ,
//cast(cast(round(CurrentStock * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as abap.dec(23,2)) as mmim_currentstockcurval ) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(QualityInspectionStockCurVal) as QIStockCurVal,
// InventoryStockType = '03', not valuated
@Semantics.amount.currencyCode: 'Currency'
sum(ReturnsBlockedStockCurVal) as ReturnsBlockedStockCurVal ,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockStorageLocCurVal) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockPlantCurVal) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(StockInTransitCurVal) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(BlockedStockCurVal) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(RestrictedStockCurVal) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(TiedEmptiesStockCurVal) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(GoodsReceiptBlockedStockCurVal) as GoodsReceiptBlockedStockCurVal,

sum(QualityInspectionStockQuantity) as QIStockQty,
@Semantics.amount.currencyCode: 'Currency'
sum(QualityInspectionStockValue) as QIStockVqlue,
sum(ReturnsBlockedStockQuantity) as ReturnsBlockedStockQuantity,
@Semantics.amount.currencyCode: 'Currency'
sum(ReturnsBlockedStockValue) as ReturnsBlockedStockValue,
sum(TransferStockStorageLocQty) as TRStockSLQty,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockStorageLocValue) as TRStockSLValue,
sum(TransferStockPlantQuantity) as TRStockPLQty,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockPlantValue) as TRStockPLValue,
sum(StockInTransitQuantity) as SITQty,
@Semantics.amount.currencyCode: 'Currency'
sum(StockInTransitValue) as SITValue,
sum(BlockedStockQuantity) as BLStockQTY,
@Semantics.amount.currencyCode: 'Currency'
sum(BlockedStockValue) as BLStockValue,
sum(RestrictedStockQuantity) as RESTStockQty,
@Semantics.amount.currencyCode: 'Currency'
sum(RestrictedStockValue) as RESTStockValue,
sum(TiedEmptiesStockQuantity) as TiedEmptiesStockQuantity,
@Semantics.amount.currencyCode: 'Currency'
sum(TiedEmptiesStockValue) as TiedEmptiesStockValue,
sum(GoodsReceiptBlockedStockQty) as GRBlockedStockQty,
@Semantics.amount.currencyCode: 'Currency'
sum(GoodsReceiptBlockedStockValue) as GRBlockedStockValue,
A.matkl
//Values of Stock Quantity


//Current Values of Stock Quantity, Consideration of new fields calulation with price
//cast(CurrentStockCurVal             as mmim_currentstockcurval preserving type)        as CurrentStockCurVal,


} where A.bukrs = $parameters.P_Bukrs
//where Material = '000000001000000084'
//  and Batch ='0000000034'
//  and CurrentStock > 0
group by
A.matnr,
// Further Stock Groups
A.bukrs,
A.werks,
A.lgort,
A.charg,
A.LIFNR,

//[KTGL-35854 변경시작 2023.02.24] - 주석처리
//SDDocument,
//SDDocumentItem,
//[KTGL-35854 변경종료 2023.02.24] - 주석처리

WBSElementInternalID,
Customer,
//InventoryStockType,
InventorySpecialStockType,

// Quantity and Unit
A.meins,
//MaterialBaseUnit,
//MatlWrhsStkQtyInMatlBaseUnit,
Currency,
A.bwtar,
A.matkl

union

select from  ZCPMMESTOCKAG2(P_Language: $parameters.P_Language,
                            P_EndDate : $parameters.P_EndDate) as A inner join mara as B
                                            on A.Material = B.matnr
{

// Stock Identifier
key Material as matnr,
// Further Stock Groups
key CompanyCode as bukrs,
key Plant as werks,
key cast('' as abap.char(4)) as lgort,
key batch as charg,

//key case when supplier is null then cast('' as abap.char(10))
//                           else supplier end as LIFNR,
key supplier as LIFNR,
key InventoryValuationType as bwtar,
//SDDocument,
//SDDocumentItem,
WBSElementInternalID,
Customer,
//InventoryStockType,
InventorySpecialStockType,

// Quantity and Unit
B.meins as MaterialBaseUnit,
//MatlWrhsStkQtyInMatlBaseUnit,
case when Currency is null then cast('KRW' as abap.cuky( 5 )) else Currency end as CURRENCY,


//Quantity of Stock
sum(CurrentStock) as CurrentStock,
@Semantics.amount.currencyCode: 'Currency'
sum(CurrentStockValue) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
sum(CurrentStockCurVal) as CurrentStockCurVal ,
//cast(cast(round(CurrentStock * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as abap.dec(23,2)) as mmim_currentstockcurval ) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(QualityInspectionStockCurVal) as QIStockCurVal,
// InventoryStockType = '03', not valuated
@Semantics.amount.currencyCode: 'Currency'
sum(ReturnsBlockedStockCurVal) as ReturnsBlockedStockCurVal ,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockStorageLocCurVal) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockPlantCurVal) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(StockInTransitCurVal) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(BlockedStockCurVal) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(RestrictedStockCurVal) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(TiedEmptiesStockCurVal) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
sum(GoodsReceiptBlockedStockCurVal) as GoodsReceiptBlockedStockCurVal,

sum(QualityInspectionStockQuantity) as QIStockQty,
@Semantics.amount.currencyCode: 'Currency'
sum(QualityInspectionStockValue) as QIStockVqlue,
sum(ReturnsBlockedStockQuantity) as ReturnsBlockedStockQuantity,
@Semantics.amount.currencyCode: 'Currency'
sum(ReturnsBlockedStockValue) as ReturnsBlockedStockValue,
sum(TransferStockStorageLocQty) as TRStockSLQty,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockStorageLocValue) as TRStockSLValue,
sum(TransferStockPlantQuantity) as TRStockPLQty,
@Semantics.amount.currencyCode: 'Currency'
sum(TransferStockPlantValue) as TRStockPLValue,
sum(StockInTransitQuantity) as SITQty,
@Semantics.amount.currencyCode: 'Currency'
sum(StockInTransitValue) as SITValue,
sum(BlockedStockQuantity) as BLStockQTY,
@Semantics.amount.currencyCode: 'Currency'
sum(BlockedStockValue) as BLStockValue,
sum(RestrictedStockQuantity) as RESTStockQty,
@Semantics.amount.currencyCode: 'Currency'
sum(RestrictedStockValue) as RESTStockValue,
sum(TiedEmptiesStockQuantity) as TiedEmptiesStockQuantity,
@Semantics.amount.currencyCode: 'Currency'
sum(TiedEmptiesStockValue) as TiedEmptiesStockValue,
sum(GoodsReceiptBlockedStockQty) as GRBlockedStockQty,
@Semantics.amount.currencyCode: 'Currency'
sum(GoodsReceiptBlockedStockValue) as GRBlockedStockValue,
B.matkl
//Values of Stock Quantity


//Current Values of Stock Quantity, Consideration of new fields calulation with price
//cast(CurrentStockCurVal             as mmim_currentstockcurval preserving type)        as CurrentStockCurVal,


} where CompanyCode = $parameters.P_Bukrs
    and StorageLocation = ''
//where Material = '000000001000000084'
//  and Batch ='0000000034'
//  and CurrentStock > 0
group by
A.Material,
// Further Stock Groups
A.CompanyCode,
A.Plant,
A.batch,
A.supplier,

//[KTGL-35854 변경시작 2023.02.24] - 주석처리
//SDDocument,
//SDDocumentItem,
//[KTGL-35854 변경종료 2023.02.24] - 주석처리

WBSElementInternalID,
Customer,
//InventoryStockType,
InventorySpecialStockType,

// Quantity and Unit
B.meins,
//MaterialBaseUnit,
//MatlWrhsStkQtyInMatlBaseUnit,
Currency,
A.InventoryValuationType,
B.matkl
