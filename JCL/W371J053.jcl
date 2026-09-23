//W371J053 JOB (640W3710100W371J053,W100),'RTN W371D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P053                                                         
//*                                                                             
//W37153.W37153D1 DD  DSN=W371.W371D3.W3714C(+0),DISP=SHR                       
//*                                                                             
// EXEC WZ14PDAP,DSIN=W371.W371D3.W37153(+1)                                    
//SYSIN           DD *                                                          
W37153-091                                                                      
W3715300                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W371J053                                         
