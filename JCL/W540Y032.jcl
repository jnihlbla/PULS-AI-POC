//W540Y032 JOB (670W5400100W540Y032,W100),'RTN WYR001',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W540    EXEC W540Y032                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W540Y032                                         
