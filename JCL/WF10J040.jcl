//WF10J040 JOB (640WF100100WF10J040,W100),'RTN WF10D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF10    EXEC WF10P040                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J040                                         
