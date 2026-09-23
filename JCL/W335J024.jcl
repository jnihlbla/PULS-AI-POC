//W335J024 JOB (640W3350100W335J024,W100),'RTN W335S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=9,CARDS=0,FORMS=1800                                            
//*+JBS BIND D2G0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//W335     EXEC W335P024                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J024                                         
