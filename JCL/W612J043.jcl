//W612J043 JOB (640W6120100W612J043,W100),'RTN W612S4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P043,                                                        
//             DEST1=&PRT.                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J043                                         
