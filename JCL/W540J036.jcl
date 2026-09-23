//W540J036 JOB (640W5400100W540J036,W100),'RTN WYR001',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W540    EXEC W540P036                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W540J036                                         
