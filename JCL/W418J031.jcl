//W418J031 JOB (670W4180100W418J031,W100),'RTN W418D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W418    EXEC W418P031                                                         
//*                                                                             
//* OM W418AI INNEHÅLLER DATA SKA W418D3-RUTINEN VÄNTA PÅ BILL-IT.              
//* DETTA ORDNAS GENOM ATT AKTIVERA WBAT2KRE VILKEN SEDAN END:AS                
//* AV BILL-IT NÄR TRANSAKTIONERNA ÄR FÄRDIGBEHANDLADE.                         
//* OM DET ÄR ÅRSSLUT ("0101") SKA MAN INTE VÄNTA PÅ BILL-IT,                   
//* EFTERSOM TRANSARNA TILLHÖR DET NYA ÅRET, INTE DENNA KÖRNING.                
//*                                                                             
//EMPTYTST EXEC WEMPTST,DSIN=W418.W418D2.W418AI(+0)                             
//IFDATA IF (EMPTYTST.T.RC = 0) THEN                                            
//      EXEC WSOP                                                               
  IF-CALENDAR AFTERWEEK                                                         
    IF-CALENDAR NOT-0101                                                        
      ACTIVATE WBAT2KRE                                                         
    ENDIF                                                                       
  ENDIF                                                                         
//*                                                                             
//IFDATA ENDIF                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J031                                         
