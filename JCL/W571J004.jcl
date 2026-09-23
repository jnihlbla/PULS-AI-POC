//W571J004 JOB (640W5710100W571J004,W100),'RTN W571B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W571    EXEC W571P004                                                         
//*                                                                             
&IDDC                                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W571J004                                         
