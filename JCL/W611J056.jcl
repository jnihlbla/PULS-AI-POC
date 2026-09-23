//W611J056 JOB (640W6110100W611J056,W100),'RTN W611D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P056                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W611.W611D1.W61153(+1)                                    
//SYSIN           DD *                                                          
W61156-001                                                                      
W61156                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J056                                         
