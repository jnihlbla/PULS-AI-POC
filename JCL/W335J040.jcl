//W335J040 JOB (670W3350100W335J040,W100),'RTN W335D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
/*JOBPARM FORMS=1800,LINECT=0                                                   
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W335     EXEC W335P040,                                                       
//             INDIN2=W335.W335D5,                                              
//             INDUT=W335.W335D5                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J040                                         
