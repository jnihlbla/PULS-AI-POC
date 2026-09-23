//W551J083 JOB (640W5510100W551J083,W100),'RTN W551B8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W551    EXEC W551P083                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J083                                         
