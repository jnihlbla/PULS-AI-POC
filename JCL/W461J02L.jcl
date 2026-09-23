//W461J02L JOB (640W4610100W461J02L,W100),'RTN W461D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P02L                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461J02L                                         
