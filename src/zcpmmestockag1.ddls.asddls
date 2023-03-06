@AbapCatalog.sqlViewName: 'ZSVPMMESTOCKAG1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '[재고수불부]-기말재고 Aggregation1'
@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.dataClass:#MIXED
@ClientHandling.algorithm: #SESSION_VARIABLE
define view ZCPMMESTOCKAG1
  with parameters
    @Consumption.hidden: true
    @Environment.systemField: #SYSTEM_LANGUAGE
    P_Language : sylangu,
    @Environment.systemField: #SYSTEM_DATE
    P_EndDate  : sydate
  as select from ZCBMMESTOCKB(P_EndDate : $parameters.P_EndDate)

{
  // Stock Identifier
  Material,
  Plant,
  StorageLocation,
  Batch,
  Supplier,
  SDDocument,
  SDDocumentItem,
  WBSElementInternalID,
  Customer,
  InventoryStockType,
  InventorySpecialStockType,

  // Further Stock Groups
  CompanyCode,
  //MatlDocLatestPostgDate,

  // Quantity and Unit
  MaterialBaseUnit,
  MatlWrhsStkQtyInMatlBaseUnit,

  //Fields for derivation of values
  ValuationArea,
  InventoryValuationType,
  CostEstimate,

  //_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as AmountInCompanyCode,
  Currency,

  case when InventoryStockType = '01'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as CurrentStock,

  case when InventoryStockType = '02'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as QualityInspectionStockQuantity,

  case when InventoryStockType = '03'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as ReturnsBlockedStockQuantity,

  case when InventoryStockType = '04'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as TransferStockStorageLocQty,

  case when InventoryStockType = '05'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as TransferStockPlantQuantity,

  case when InventoryStockType = '06'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as StockInTransitQuantity,

  case when InventoryStockType = '07'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as BlockedStockQuantity,

  case when InventoryStockType = '08'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as RestrictedStockQuantity,

  case when InventoryStockType = '09'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as TiedEmptiesStockQuantity,

  case when InventoryStockType = '10'
     then cast( MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) )
     else cast( 0 as abap.dec(21,3 ) )
  end as GoodsReceiptBlockedStockQty,


  // InventoryStockType = '01'
  case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
               or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
               or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
         and  ( InventoryStockType = '01' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
       then
       cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
       as abap.dec(23,2) )
       when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
         or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '01'
       then cast( 0 as abap.dec(23,2 ) )
       when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
               or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
               or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
         and  ( InventoryStockType = '01' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
         and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
        then
        cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
        as abap.dec(23,2) )
        when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
               or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
               or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
         and  ( InventoryStockType = '01' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
         and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
        then
        cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
        as abap.dec(23,2) )
        else cast( 0 as abap.dec(23,2 ) )
  end as CurrentStockValue,

  // InventoryStockType = '02'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '02' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '02'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '02' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '02' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as QualityInspectionStockValue,

  // InventoryStockType = '03', not valuated
    case when InventoryStockType = '03'
       then cast( 0 as abap.dec(23,2 ) )
       else cast( 0 as abap.dec(23,2 ) )
    end as ReturnsBlockedStockValue,

  // please never delete this, maybe coding will be used as proposal
  //case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
  //             or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
  //             or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
  //       and  ( InventoryStockType = '03' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
  //     then cast( round( ( division( ( cast( _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(15,4) ) ), cast( _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(18, 3) ) , 5 ) * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(18,3) ) ) , 3 ) as abap.dec(18,3) )
  //     when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
  //       or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '03'
  //     then cast( 0 as abap.dec(18,3 ) )
  //     when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
  //             or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
  //             or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
  //       and  ( InventoryStockType = '03' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
  //       and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
  //      then cast( round( ( division( ( cast( _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy as abap.dec(15,4) ) ), cast( _MaterialLedgerPrice( P_EndDate:$parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency as abap.dec(18, 3) ) , 5 ) * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(18,3) ) ) , 3 ) as abap.dec(18,3) )
  //      when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
  //             or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
  //             or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
  //       and  ( InventoryStockType = '03' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
  //       and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
  //      then cast( round( ( division( ( cast( _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy as abap.dec(15,4) ) ), cast( _MaterialLedgerPrice(P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency as abap.dec(18, 3) ) , 5 ) * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(18,3) ) ) , 3 ) as abap.dec(18,3) )
  //      else cast( 0 as abap.dec(18,3 ) )
  //end as ReturnsBlockedStockValue,

  // InventoryStockType = '04'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '04' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '04'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '04' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '04' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as TransferStockStorageLocValue,

  // InventoryStockType = '05'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '05' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '05'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '05' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '05' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as TransferStockPlantValue,

  // InventoryStockType = '06'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '06' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '06'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '06' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '06' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as StockInTransitValue,


  // InventoryStockType = '07'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '07' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '07'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '07' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '07' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as BlockedStockValue,

  // InventoryStockType = '08'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '08' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '08'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '08' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '08' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as RestrictedStockValue,

  // InventoryStockType = '09'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '09' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '09'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '09' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '09' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as TiedEmptiesStockValue,

  // InventoryStockType = '10'
    case when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '10' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty <> 0 )
         then
         cast(
         round(
           cast(
            division(
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).AmountInCompanyCodeCurrency as abap.dec(23,2) ),
              cast(_MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate ).InventoryQty as abap.dec(23,3) ) , 7 ) as abap.dec(16,7) )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
         as abap.dec(23,2) )
         when ( InventorySpecialStockType = 'K' or InventorySpecialStockType = 'M' or InventorySpecialStockType = 'B' or InventorySpecialStockType = 'I' or InventorySpecialStockType = 'C'
           or InventorySpecialStockType = 'J' or InventorySpecialStockType = 'P' or InventorySpecialStockType = 'Y' ) and  InventoryStockType = '10'
         then cast( 0 as abap.dec(23,2 ) )
         when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '10' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'S' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).StandardPriceInCoCodeCrcy,
               _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
               * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          when (  InventorySpecialStockType ='O' or InventorySpecialStockType = 'V' or InventorySpecialStockType = 'W' or InventorySpecialStockType = ' '
                 or ( ( InventorySpecialStockType = 'E' or InventorySpecialStockType = 'Q' or InventorySpecialStockType = 'T' ) and InventorySpecialStockValnType <> '' )
                 or ( ( InventorySpecialStockType = 'F' or InventorySpecialStockType = 'R' ) and ( InventorySpecialStockValnType <> '' or IsSupplierStockValuation = 'X' ) ) )
           and  ( InventoryStockType = '10' and MatlWrhsStkQtyInMatlBaseUnit <> 0 and _MaterialStockValueByKeyDate( P_Language: $parameters.P_Language , P_EndDate: $parameters.P_EndDate).InventoryQty = 0 )
           and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MaterialPriceControl = 'V' and _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency <> 0
          then
          cast(
          round(
             division(
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MovingAveragePriceInCoCodeCrcy,
                _MaterialLedgerPrice( P_EndDate: $parameters.P_EndDate ).MatPriceUnitInCoCodeCurrency, 7 )
                * cast(MatlWrhsStkQtyInMatlBaseUnit as abap.dec(21,3) ), 2 )
          as abap.dec(23,2) )
          else cast( 0 as abap.dec(23,2 ) )
    end as GoodsReceiptBlockedStockValue,

  // Names and descriptions
  //_Material._Text[1: Language=$parameters.P_Language].MaterialName,
  //_CompanyCode.CompanyCodeName,
  //_Plant.PlantName,
  //_StorageLocation.StorageLocationName,
  //_Supplier.SupplierName,
  //_Customer.CustomerName,
  //_InventoryStockType._Text[1: Language=$parameters.P_Language].InventoryStockTypeName,
  //_InventorySpecialStockType._Text[1: Language=$parameters.P_Language].InventorySpecialStockTypeName

  _CurrentInvtryPrice

}
