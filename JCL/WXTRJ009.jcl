//WXTRJ009 JOB (640W0001000WXTRJ009,W100),'RTN WXTRV3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WXTR    EXEC WXTRP009                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ009                                         
