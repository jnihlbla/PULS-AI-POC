//W236J036 JOB (640W2360100W236J036,W100),'RTN W236V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W236    EXEC W236P036                                                         
// EXEC WZ14PDAP,DSIN=W236.W236V2.W23637(+1)                                    
//SYSIN           DD *                                                          
W23636-001                                                                      
W23636                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J036                                         
