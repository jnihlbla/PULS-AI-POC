//W510J092 JOB (640W5100100W510J092,W100),'RTN W510D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P092                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510J092                                         
