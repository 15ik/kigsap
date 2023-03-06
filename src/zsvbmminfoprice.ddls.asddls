@AbapCatalog.sqlViewName: 'ZSVBMMINFOPRICE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@Metadata.allowExtensions: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '구매 단가 이력'
define view ZCBMMINFOPRICE as select from I_PurInfCndPeriod
     inner join eina
    on I_PurInfCndPeriod.PurchasingInfoRecord = eina.infnr
    association [1]   to I_MaterialDescription as matdec on $projection.matnr = matdec.Material
    association [1]   to lfa1 as lfa1  on $projection.lifnr = lfa1.lifnr
    association [1]   to eine as eine  on $projection.infnr = eine.infnr
                                      and $projection.PurchasingInfoRecordCategory = eine.esokz
                                      and $projection.PurchasingOrganization = eine.ekorg
                                      and $projection.Plant = eine.werks



   {
    key infnr,
    matnr,
    matdec.MaterialDescription,
    matkl,
    lifnr,
    lfa1.name1,
    PurchasingOrganization,
    PurchasingInfoRecordCategory,
    Plant,
    eine.exprf as confirm_price,
    ConditionValidityEndDate,
    ConditionValidityStartDate,
    ConditionRecord,
    ConditionSequentialNumber,
    ConditionType,
    ConditionRateValue,
    ConditionRateValueUnit,
    ConditionQuantity,
    ConditionQuantityUnit,
    ConditionCalculationType,
    ConditionIsDeleted,
    ConditionToBaseQtyNmrtr,
    ConditionToBaseQtyDnmntr,
    currentdate,
    BaseUnit,
    eine.verid,
    eina.urzla,
    eina.loekz as eina_loekz,
    eine.ekgrp,
    eine.bstae,
    eine.mwskz,
    eine.uebto,
    eine.webre,
    eine.loekz as eine_loekz,
    eine.inco1,
       ( case when ConditionValidityStartDate <= currentdate and currentdate <= ConditionValidityEndDate then 'Y'
          else 'N' end ) as validprice


} where ConditionRateValue is not initial
