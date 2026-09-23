//W412J063 JOB (640W4120100W412J063,W100),'RTN W412D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W412    EXEC W412P063                                                         
//*                                                                             
//TOMTEST  EXEC WEMPTST,DSIN=W412.W412D5.W41265(+1)                             
//*                                                                             
//WMEMOSND EXEC WMEMOSND,CONDS='(0,LT,TOMTEST.T)',                              
//             REQS=W412D5M1                                                    
//SEND DD *                                                                     
                                                                                
=====================================                                           
STATUS S TRANSACTIONS IN THE DISPATCH                                           
=====================================                                           
                                                                                
The transactions in this memo will NOT be treated in the central                
system. Actions must be taken to correct them.                                  
                                                                                
Node     Datum  Tid      User     Err Status                                    
                                                                                
//     DD DSN=W412.W412D5.W41265(+1),DISP=(OLD,KEEP)                            
//     DD *                                                                     
//*                                                                             
//TOMTEST2 EXEC WEMPTST,DSIN=W412.W412D5.W41263(+1)                             
//*                                                                             
//WMEMOSND EXEC WMEMOSND,CONDS='(0,LT,TOMTEST2.T)',                             
//             REQS=W412D5M2,INDRTE=W.XDEV                                      
//SEND DD *                                                                     
                                                                                
===========================                                                     
TACDIS ORDERS DELETE FAILED                                                     
===========================                                                     
                                                                                
Node       Datum      Tid       Err        Dist/Kund/Order                      
                                                                                
//     DD DSN=W412.W412D5.W41263(+1),DISP=(OLD,KEEP)                            
//     DD *                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J063                                         
