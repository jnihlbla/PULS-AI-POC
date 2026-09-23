//W483J024 JOB (640W4830100W483J024,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE    XEQ LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*                                                                             
//W483    EXEC W483P024                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W483J024                                         
