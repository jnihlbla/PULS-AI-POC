//W612J308 JOB (640W6120100W612J308,W100),'RTN W612DD',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P008,                                                        
//        INDIN=&W612..W612DD,                                                  
//        INDUT=&W612..W612DD                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J308                                         
