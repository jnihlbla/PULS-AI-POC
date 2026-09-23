//W461J02A JOB (640W4610100W461J02A,W100),'RTN W461D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P02A                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461J02A                                         
