//W371J048 JOB (640W3710100W371J048,W100),'RTN W371D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P048                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W371.W371D3.W37148(+1)                                    
//SYSIN           DD *                                                          
W37148-091                                                                      
W3714800                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W371J048                                         
