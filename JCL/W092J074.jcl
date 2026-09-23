//W092J074 JOB (640W0920100W092J074,W100),'RTN W092D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W092    EXEC W092P074                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W092J074                                         
