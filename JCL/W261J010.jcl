//W261J010 JOB (650W2610100W261J010,W100),'RTN W261P1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ LOCAL                                                              
/*ROUTE PRINT LOCAL                                                             
//W261     EXEC W261P010                                                        
//VRCABE   EXEC VRCABEND,COND=(8,GE)                                            
//SOP     EXEC WSOPEND,PROCESS=W261J010                                         
/*                                                                              
