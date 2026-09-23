//W440J086 JOB (640W4400100W440J086,W100),'RTN W440V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W440    EXEC W440P086                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J086                                         
