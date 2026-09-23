//W553J024 JOB (640W5530100W553J024,W100),'RTN W553V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W553    EXEC W553P024                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W553J024                                         
