//W476J053 JOB (670W4760100W476J053,W100),'RTN W476D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W476    EXEC W476P053                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J053                                         
