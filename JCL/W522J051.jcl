//W522J051 JOB (640W5220100W522J051,W100),'RTN W522V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W522    EXEC W522P051                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W522J051                                         
