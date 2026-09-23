//W330J012 JOB (650W3300100W330J012,W100),'RTN W330V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M,TIME=(25,0)                                              
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P012 EXEC W330P012                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J012                                            
