//W432J018 JOB (640W4320100W432J018,W100),'RTN W432V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W432    EXEC W432P018                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W432J018                                         
