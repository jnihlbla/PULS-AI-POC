//W615J036 JOB (640W6150100W615J036,W100),'RTN W615V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W615    EXEC W615P036                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W615J036                                         
