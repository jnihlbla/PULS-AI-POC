//W330J112 JOB (650W3300100W330J112,W100),'RTN W330D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(10,0)                                              
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P112 EXEC W330P112                                                        
//*                                                                             
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J112                                            
