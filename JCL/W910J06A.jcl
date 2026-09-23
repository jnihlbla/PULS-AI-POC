//W910J06A JOB (640W9100100W910J06A,W100),'RTN W910V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W910    EXEC W910P06A                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J06A                                         
