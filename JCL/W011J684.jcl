//W011J684 JOB (640W0110100W011J684,W100),'RTN W011S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P084                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J684                                         
