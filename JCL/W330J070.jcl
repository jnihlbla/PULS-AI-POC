//W330J070 JOB (650W3300100W330J070,W100),'RTN W330B2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M,TIME=(69,0)                                              
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P070 EXEC W330P070                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J070                                            
