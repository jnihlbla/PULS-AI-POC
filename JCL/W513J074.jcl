//W513J074 JOB (640W5130100W513J074,W100),'RTN W513D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W513    EXEC W513P074                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J074                                         
