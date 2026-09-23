//W461J0E9 JOB (640W4610100W461J0E9,W100),'RTN W461D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*461    EXEC W461P0E9                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461J0E9                                         
