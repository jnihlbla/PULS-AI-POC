//W512J029 JOB (670W5120100W512J029,W100),'RTN W512M3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W512    EXEC W512P029                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512J029                                         
