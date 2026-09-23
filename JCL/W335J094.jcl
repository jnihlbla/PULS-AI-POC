//W335J094 JOB (670W3350100W335J094,W100),'RTN W335S6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND VCC1                                                               
//W335    EXEC W335P094                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J094                                         
