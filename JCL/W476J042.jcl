//W476J042 JOB (640W4760100W476J042,W100),'RTN W476V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W476    EXEC W476P042                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J042                                         
