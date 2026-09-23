//W611J05E JOB (640W6110100W611J05E,W100),'RTN W611V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W611    EXEC W611P05E                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W611.W611V1.W6115E(+1)                                    
//SYSIN           DD *                                                          
W6115E-001                                                                      
W6115E                                                                          
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J05E                                         
