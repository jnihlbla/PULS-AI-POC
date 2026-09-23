//W432J033 JOB (650W4320100W432J033,W100),'RTN W432D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W432    EXEC W432P033                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W432J033                                         
