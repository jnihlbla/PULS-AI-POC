//W371J04L JOB (640W3710100W371J04L,W100),'RTN W371D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P04L                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W371.W371D3.W3714P(+1)                                    
//SYSIN           DD *                                                          
W3714L-091                                                                      
W3714L00                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W371J04L                                         
