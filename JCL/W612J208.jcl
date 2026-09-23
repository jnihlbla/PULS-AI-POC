//W612J208 JOB (640W6120100W612J208,W100),'RTN W612R4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P208                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J208                                         
