//W335J240 JOB (670W3350100W335J240,W100),'RTN W335D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W335     EXEC W335P240,                                                       
//             INDIN2=W335.W335D7,                                              
//             INDUT=W335.W335D7                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J240                                         
