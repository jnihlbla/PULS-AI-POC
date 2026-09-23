//W231J097 JOB (650W2310100W231J097,W100),'RTN W231V3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W231    EXEC W231P097                                                         
// EXEC WZ14PDAP,DSIN=W231.W231V3.W2319B(+1)                                    
//SYSIN           DD *                                                          
W23197-001                                                                      
W23197                                                                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J097                                         
