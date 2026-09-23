//W330J040 JOB (650W3300100W330J040,W100),'RTN W330B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(180,0)                                             
/*JOBPARM LINES=999,CARDS=0,FORMS=1800                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P040 EXEC W330P040                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J040                                            
