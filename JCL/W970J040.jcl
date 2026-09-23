//W970J040 JOB (640W0000100W970J040,W100),'RTN W970V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W970    EXEC W970P040                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J040                                         
