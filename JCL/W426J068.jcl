//W426J068 JOB (640W4260100W426J068,W100),'RTN W426V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W426    EXEC W426P068                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J068                                         
