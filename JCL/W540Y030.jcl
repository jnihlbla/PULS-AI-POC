//W540Y030 JOB (670W5400100W540Y030,W100),'RTN WYR001',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W540    EXEC W540P030,INDIN=W540.WYR001,INDUT=W540.WYR001                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W540Y030                                         
