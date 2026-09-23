//W330J082 JOB (650W3300100W330J082,W100),'RTN W330B2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(15,0)                                              
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P082 EXEC W330P082                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J082                                            
