//W426J042 JOB (640W4260100W426J042,W100),'RTN W426V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P042                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J042                                         
