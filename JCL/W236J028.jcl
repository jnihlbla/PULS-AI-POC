//W236J028 JOB (640W2360100W236J028,W100),'RTN W236P2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W236    EXEC W236P028                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W236.W236P2.W23627(+1)                                    
//SYSIN           DD *                                                          
W23628-001                                                                      
W23628                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J028                                         
