//W461J075 JOB (650W4610100W461J075,W100),'RTN W461D2',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P075                                                         
//W46175.W46175D1 DD                                                            
// DD                                                                           
// DD DUMMY                                                                     
//SOP     EXEC WSOPEND,PROCESS=W461J075                                         
