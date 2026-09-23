//W335J088 JOB (640W3350100W335J088,W100),'RTN W335D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W335    EXEC W335P088                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J088                                         
