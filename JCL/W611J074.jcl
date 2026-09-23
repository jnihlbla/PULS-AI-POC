//W611J074 JOB (640W6110100W611J074,W100),'RTN W611D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P074                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W611.W611D1.W61174(+1)                                    
//SYSIN           DD *                                                          
W61174-001                                                                      
W61174                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J074                                         
