@AbapCatalog.sqlViewName: 'ZSVPMMESTOCKAG2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '[재고수불부]-기말재고 Aggregation2'
@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.dataClass:#MIXED
@ClientHandling.algorithm: #SESSION_VARIABLE
define view ZCPMMESTOCKAG2
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_LANGUAGE
P_Language:   sylangu,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:    sydate
as
select from ZCPMMESTOCKAG1(P_Language: $parameters.P_Language,
                                          P_EndDate : $parameters.P_EndDate) as a
         inner join marc as B on a.Material = B.matnr and a.Plant = B.werks

{


// Stock Identifier
Material,
Plant,
StorageLocation,
case when B.xchpf is null or B.xchpf = '' then ''
 else
    case when Batch is null then '' else Batch end
end as batch,
case when Supplier is null then '' else Supplier end as supplier,
SDDocument,
SDDocumentItem,
WBSElementInternalID,
Customer,
//InventoryStockType,
InventorySpecialStockType,

// Further Stock Groups
CompanyCode,

// Quantity and Unit
MaterialBaseUnit,
//MatlWrhsStkQtyInMatlBaseUnit,
Currency,
InventoryValuationType,

//Quantity of Stock
CurrentStock,
QualityInspectionStockQuantity,
ReturnsBlockedStockQuantity,
TransferStockStorageLocQty,
TransferStockPlantQuantity,
StockInTransitQuantity,
BlockedStockQuantity,
RestrictedStockQuantity,
TiedEmptiesStockQuantity,
GoodsReceiptBlockedStockQty,

//Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockValue as mmim_currentstockval) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockValue as mmim_qualityinspectionstval) as QualityInspectionStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockValue as mmim_returnsblockedstockval) as ReturnsBlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocValue as mmim_transferststorlocval) as TransferStockStorageLocValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantValue as mmim_transferstockplantval) as TransferStockPlantValue,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitValue as mmim_stockintransitval) as StockInTransitValue,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockValue as mmim_blockedstockval) as BlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockValue as mmim_restrictedstockval) as RestrictedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockValue as mmim_tiedemptiesstockval) as TiedEmptiesStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockValue as mmim_goodsreceiptblockstval) as GoodsReceiptBlockedStockValue,

//Current Values of Stock Quantity, Consideration of new fields calulation with price
//cast(CurrentStockCurVal             as mmim_currentstockcurval preserving type)        as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(CurrentStock * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_currentstockcurval ) as CurrentStockCurVal,
//cast(cast(round(CurrentStock * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as abap.dec(23,2)) as mmim_currentstockcurval ) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(QualityInspectionStockQuantity * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_qualityinspectionstcurval ) as QualityInspectionStockCurVal,
// InventoryStockType = '03', not valuated
@Semantics.amount.currencyCode: 'Currency'
cast(0 as mmim_returnsblockedstockcurval ) as ReturnsBlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(TransferStockStorageLocQty * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_transferststorloccurval ) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(TransferStockPlantQuantity * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_transferstockplantcurval ) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(StockInTransitQuantity * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_stockintransitcurval ) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(BlockedStockQuantity * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_blockedstockcurval ) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(RestrictedStockQuantity * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_restrictedstockcurval ) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(TiedEmptiesStockQuantity * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_tiedemptiesstockcurval ) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(round(GoodsReceiptBlockedStockQty * division(_CurrentInvtryPrice.InventoryPrice, _CurrentInvtryPrice.MaterialPriceUnitQty, 5 ), 2) as mmim_goodsreceiptblockstcurval ) as GoodsReceiptBlockedStockCurVal

}
//where MatlDocLatestPostgDate <= $parameters.P_EndDate
//group by Material, Plant, StorageLocation, Batch,
//         Supplier, SDDocument, SDDocumentItem, WBSElementInternalID, Customer, //InventoryStockType,
//         InventorySpecialStockType, CompanyCode, InventoryValuationType, MaterialBaseUnit, Currency

union all

select from F_Mmim_Marc_Dval

{
  Material,
  Plant,
  StorageLocation,
  Batch,
  Supplier,
  SDDocument,
  SDDocumentItem,
  WBSElementInternalID,
  Customer,
  InventorySpecialStockType,
  CompanyCode,
  MaterialBaseUnit,
  Currency,
  InventoryValuationType,

//Quantity of Stock
CurrentStock,
QualityInspectionStockQuantity,
ReturnsBlockedStockQuantity,
TransferStockStorageLocQty,
TransferStockPlantQuantity,
StockInTransitQuantity,
BlockedStockQuantity,
RestrictedStockQuantity,
TiedEmptiesStockQuantity,
GoodsReceiptBlockedStockQty,

//Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockValue as mmim_currentstockval) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockValue as mmim_qualityinspectionstval) as QualityInspectionStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockValue as mmim_returnsblockedstockval) as ReturnsBlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocValue as mmim_transferststorlocval) as TransferStockStorageLocValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantValue as mmim_transferstockplantval) as TransferStockPlantValue,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitValue as mmim_stockintransitval) as StockInTransitValue,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockValue as mmim_blockedstockval) as BlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockValue as mmim_restrictedstockval) as RestrictedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockValue as mmim_tiedemptiesstockval) as TiedEmptiesStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockValue as mmim_goodsreceiptblockstval) as GoodsReceiptBlockedStockValue,

//Current Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockCurVal as mmim_currentstockcurval) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockCurVal as mmim_qualityinspectionstcurval) as QualityInspectionStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockCurVal as mmim_returnsblockedstockcurval) as ReturnsBlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocCurVal as mmim_transferststorloccurval) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantCurVal as mmim_transferstockplantcurval) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitCurVal as mmim_stockintransitcurval) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockCurVal as mmim_blockedstockcurval) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockCurVal as mmim_restrictedstockcurval) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockCurVal as mmim_tiedemptiesstockcurval) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockCurVal as mmim_goodsreceiptblockstcurval) as GoodsReceiptBlockedStockCurVal

}
union all

select from F_Mmim_Mard_Dval

{
  Material,
  Plant,
  StorageLocation,
  Batch,
  Supplier,
  SDDocument,
  SDDocumentItem,
  WBSElementInternalID,
  Customer,
  InventorySpecialStockType,
  CompanyCode,
  MaterialBaseUnit,
  Currency,
  InventoryValuationType,

//Quantity of Stock
CurrentStock,
QualityInspectionStockQuantity,
ReturnsBlockedStockQuantity,
TransferStockStorageLocQty,
TransferStockPlantQuantity,
StockInTransitQuantity,
BlockedStockQuantity,
RestrictedStockQuantity,
TiedEmptiesStockQuantity,
GoodsReceiptBlockedStockQty,

//Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockValue as mmim_currentstockval) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockValue as mmim_qualityinspectionstval) as QualityInspectionStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockValue as mmim_returnsblockedstockval) as ReturnsBlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocValue as mmim_transferststorlocval) as TransferStockStorageLocValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantValue as mmim_transferstockplantval) as TransferStockPlantValue,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitValue as mmim_stockintransitval) as StockInTransitValue,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockValue as mmim_blockedstockval) as BlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockValue as mmim_restrictedstockval) as RestrictedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockValue as mmim_tiedemptiesstockval) as TiedEmptiesStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockValue as mmim_goodsreceiptblockstval) as GoodsReceiptBlockedStockValue,

//Current Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockCurVal as mmim_currentstockcurval) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockCurVal as mmim_qualityinspectionstcurval) as QualityInspectionStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockCurVal as mmim_returnsblockedstockcurval) as ReturnsBlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocCurVal as mmim_transferststorloccurval) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantCurVal as mmim_transferstockplantcurval) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitCurVal as mmim_stockintransitcurval) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockCurVal as mmim_blockedstockcurval) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockCurVal as mmim_restrictedstockcurval) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockCurVal as mmim_tiedemptiesstockcurval) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockCurVal as mmim_goodsreceiptblockstcurval) as GoodsReceiptBlockedStockCurVal

}

union all

select from F_Mmim_Mchb_Dval

{
  Material,
  Plant,
  StorageLocation,
  Batch,
  Supplier,
  SDDocument,
  SDDocumentItem,
  WBSElementInternalID,
  Customer,
  InventorySpecialStockType,
  CompanyCode,
  MaterialBaseUnit,
  Currency,
  InventoryValuationType,

//Quantity of Stock
CurrentStock,
QualityInspectionStockQuantity,
ReturnsBlockedStockQuantity,
TransferStockStorageLocQty,
TransferStockPlantQuantity,
StockInTransitQuantity,
BlockedStockQuantity,
RestrictedStockQuantity,
TiedEmptiesStockQuantity,
GoodsReceiptBlockedStockQty,

//Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockValue as mmim_currentstockval) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockValue as mmim_qualityinspectionstval) as QualityInspectionStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockValue as mmim_returnsblockedstockval) as ReturnsBlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocValue as mmim_transferststorlocval) as TransferStockStorageLocValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantValue as mmim_transferstockplantval) as TransferStockPlantValue,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitValue as mmim_stockintransitval) as StockInTransitValue,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockValue as mmim_blockedstockval) as BlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockValue as mmim_restrictedstockval) as RestrictedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockValue as mmim_tiedemptiesstockval) as TiedEmptiesStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockValue as mmim_goodsreceiptblockstval) as GoodsReceiptBlockedStockValue,

//Current Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockCurVal as mmim_currentstockcurval) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockCurVal as mmim_qualityinspectionstcurval) as QualityInspectionStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockCurVal as mmim_returnsblockedstockcurval) as ReturnsBlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocCurVal as mmim_transferststorloccurval) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantCurVal as mmim_transferstockplantcurval) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitCurVal as mmim_stockintransitcurval) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockCurVal as mmim_blockedstockcurval) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockCurVal as mmim_restrictedstockcurval) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockCurVal as mmim_tiedemptiesstockcurval) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockCurVal as mmim_goodsreceiptblockstcurval) as GoodsReceiptBlockedStockCurVal

}

union all

select from F_Mmim_Mcha_Dval

{
  Material,
  Plant,
  StorageLocation,
  Batch,
  Supplier,
  SDDocument,
  SDDocumentItem,
  WBSElementInternalID,
  Customer,
  InventorySpecialStockType,
  CompanyCode,
  MaterialBaseUnit,
  Currency,
  InventoryValuationType,

//Quantity of Stock
CurrentStock,
QualityInspectionStockQuantity,
ReturnsBlockedStockQuantity,
TransferStockStorageLocQty,
TransferStockPlantQuantity,
StockInTransitQuantity,
BlockedStockQuantity,
RestrictedStockQuantity,
TiedEmptiesStockQuantity,
GoodsReceiptBlockedStockQty,

//Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockValue as mmim_currentstockval) as CurrentStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockValue as mmim_qualityinspectionstval) as QualityInspectionStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockValue as mmim_returnsblockedstockval) as ReturnsBlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocValue as mmim_transferststorlocval) as TransferStockStorageLocValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantValue as mmim_transferstockplantval) as TransferStockPlantValue,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitValue as mmim_stockintransitval) as StockInTransitValue,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockValue as mmim_blockedstockval) as BlockedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockValue as mmim_restrictedstockval) as RestrictedStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockValue as mmim_tiedemptiesstockval) as TiedEmptiesStockValue,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockValue as mmim_goodsreceiptblockstval) as GoodsReceiptBlockedStockValue,

//Current Values of Stock Quantity
@Semantics.amount.currencyCode: 'Currency'
cast(CurrentStockCurVal as mmim_currentstockcurval) as CurrentStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(QualityInspectionStockCurVal as mmim_qualityinspectionstcurval) as QualityInspectionStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(ReturnsBlockedStockCurVal as mmim_returnsblockedstockcurval) as ReturnsBlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockStorageLocCurVal as mmim_transferststorloccurval) as TransferStockStorageLocCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TransferStockPlantCurVal as mmim_transferstockplantcurval) as TransferStockPlantCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(StockInTransitCurVal as mmim_stockintransitcurval) as StockInTransitCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(BlockedStockCurVal as mmim_blockedstockcurval) as BlockedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(RestrictedStockCurVal as mmim_restrictedstockcurval) as RestrictedStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(TiedEmptiesStockCurVal as mmim_tiedemptiesstockcurval) as TiedEmptiesStockCurVal,
@Semantics.amount.currencyCode: 'Currency'
cast(GoodsReceiptBlockedStockCurVal as mmim_goodsreceiptblockstcurval) as GoodsReceiptBlockedStockCurVal

}
