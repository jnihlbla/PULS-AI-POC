//W512J063 JOB (640W5120100W512J063,W100),'RTN W512M2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W512    EXEC W512P063                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W512.W512M2.W51263(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W512.W512M2.W51263(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512J063                                         
