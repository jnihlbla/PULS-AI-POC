//W910J088 JOB (640W9100100W910J088,W100),'RTN W910P2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W910    EXEC W910P088                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J088                                         
