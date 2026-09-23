//W231J099 JOB (640W2310100W231J099,W100),'RTN W231V3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W231    EXEC W231P099                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W231.W231V3.W2319C(+1)                                    
//SYSIN           DD *                                                          
W23199-001                                                                      
W23199                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J099                                         
