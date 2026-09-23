//W335J09A JOB (640W3350100W335J09A,W100),'RTN W335V6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W335    EXEC W335P09A,DB2GRP=PROD                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J09A                                         
