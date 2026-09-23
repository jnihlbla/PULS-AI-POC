//W335J144 JOB (670W3350100W335J144,W100),'RTN W335B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*ANVÄNDER SAMMA PROCEDUR SOM J044 I RUTIN W335D5                              
//W335     EXEC W335P044,                                                       
//             INDIN=W335.W335B2,                                               
//             INDUT=W335.W335B2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J144                                         
