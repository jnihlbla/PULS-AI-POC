//W330J020 JOB (650W3300100W330J020,W100),'RTN W330R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(10,0)                                              
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P020 EXEC W330P020                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J020                                            
