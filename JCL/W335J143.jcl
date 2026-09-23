//W335J143 JOB (640W3350100W335J143,W100),'RTN W335B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W335    EXEC W335P043,INDIN1=W335.W335B2,INDUT=W335.W335B2                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J143                                         
