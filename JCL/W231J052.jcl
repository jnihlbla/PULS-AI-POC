//W231J052 JOB (640W2310100W231J052,W100),'RTN W231V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W231    EXEC W231P052                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W231.W231V1.W23152(+1)                                    
//SYSIN           DD *                                                          
W23152-001                                                                      
W23152                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J052                                         
