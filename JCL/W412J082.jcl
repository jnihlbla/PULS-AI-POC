//W412J082 JOB (640W4120100W412J082,W100),'RTN W412S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W412    EXEC W412P082                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J082                                         
