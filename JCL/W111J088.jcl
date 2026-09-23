//W111J088 JOB (640W1110100W111J088,W100),'RTN W111V4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W111    EXEC W111P088                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W111.W111V4.W11188(+1)                                    
//SYSIN           DD *                                                          
W11188-001                                                                      
W1118800                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W111J088                                         
