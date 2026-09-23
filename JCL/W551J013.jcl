//W551J013 JOB (640W5510100W551J013,W100),'RTN W551B7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W551    EXEC W551P013                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J013                                         
