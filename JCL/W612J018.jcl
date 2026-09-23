//W612J018 JOB (640W6120100W612J018,W100),'RTN W612V6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P018                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J018                                         
