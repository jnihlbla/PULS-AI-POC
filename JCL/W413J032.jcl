//W413J032 JOB (640W4130100W413J032,W100),'RTN W413D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W413    EXEC W413P032                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W413J032                                         
