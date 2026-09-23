//W461J078 JOB (650W4610100W461J078,W100),'RTN W461D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P078                                                         
//W46178.W46178D1 DD                                                            
//                DD                                                            
//                DD DUMMY                                                      
//SOP     EXEC WSOPEND,PROCESS=W461J078                                         
