//W371J077 JOB (640W3710100W371J077,W100),'RTN W371V7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P077                                                         
//*                                                                             
//*                                                                             
//W371M  EXEC WMEMOSND                                                          
//M.APIFILE DD DSN=W371.W371V7.W37177(+1),DISP=SHR                              
//M.SEND    DD DUMMY                                                            
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J077                                         
