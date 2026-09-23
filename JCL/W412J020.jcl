//W412J020 JOB (640W4120100W412J020,W100),'RTN W412D8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W412    EXEC W412P020                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J020                                         
