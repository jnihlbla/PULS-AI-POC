//W335J038 JOB (670W3350100W335J038,W100),'RTN W335D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W335     EXEC W335P038,                                                       
//             INDIN=W335.W335D5,                                               
//             INDIN3=W335.W335D5,                                              
//             INDUT=W335.W335D5                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J038                                         
