//W330J042 JOB (650W3300100W330J042,W100),'RTN W330B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(15,0)                                              
/*JOBPARM LINES=9,CARDS=0,FORMS=1800                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P042 EXEC W330P042                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J042                                            
