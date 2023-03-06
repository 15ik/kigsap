@AbapCatalog.sqlViewName: 'ZSVCMM_ORG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MM 조직 관계'
define view zccmm_org as select from t026z
left outer join t001 on t026z.eksgb = t001.bukrs
association [1] to t024e as t024e on $projection.ekorg = t024e.ekorg
association [1] to t024 as t024 on $projection.ekgrp = t024.ekgrp
{
    key ekgrp,
    t024.eknam,
    ekorg,
    t024e.ekotx,
    t024e.bukrs,
    butxt    
}
