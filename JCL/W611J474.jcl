//W611J474 JOB (640W6110100W611J474,W100),'RTN W611D4',                         
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
//*+JBS BIND IMG0                                                               
//W611    EXEC W611P474                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W611.W611D4.W61174B(+1)                                   
//SYSIN           DD *                                                          
W61174-005                                                                      
W61174                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J474                                         
