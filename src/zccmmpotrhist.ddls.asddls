@AbapCatalog.sqlViewName: 'ZSVCMMPOTRHIST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PO 전송 이력 관리[WMS 전송]'
define view ZCCMMPOTRHIST 
with parameters 
P_BUKRS : abap.char(4)

as select from ZCBMMPOTRHIST(P_BUKRS: $parameters.P_BUKRS)
{
    
    key EBELN,
    bukrs,
    butxt,
    werks,
    lifnr,
    name1,
    bsart,
    ZAEDAT,
    max(changenr) as changenr,    
    max(UDATE) as UDATE,
    max(UTIME) as UTIME,
    max(tcode) as tcode,
    max(change_ind) as change_ind,
//    max(ZSTATUS) as ZSTATUS,
//    max(zmsg) as zmsg,
    max(ZVBELN) as ZVBELN
//    max(erdat) as ERDAT,
//    max(erzet) as ERZET,
//    max(ernam) as ERNAM
    
} where bsart <> 'ZUB1' and bsart <> 'ZUB2' and bsart <> 'ZUB3' and bsart <> 'ZUB4' and bsart <> 'ZUB5' and bsart <> 'ZZG1' and bsart <> 'ZZP1' and bsart <> 'ZZT1'
        and bsart <> 'PSIN' and  bsart <> 'PSIM' and ( knttp = '' or knttp = 'M' or knttp = 'Y' ) //KTGA-5067:Y(증지무상발주) 추가-2023.03.12
        and bstae <> '0004'
        //and ZWMS = 'X'
group by bukrs,butxt,werks,lifnr,name1,bsart,ZAEDAT,objectclas, EBELN
