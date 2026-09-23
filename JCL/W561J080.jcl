//W561J080 JOB (650W5100100W561J080,W100),'RTN W561D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W561    EXEC W561P080                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J080                                         
