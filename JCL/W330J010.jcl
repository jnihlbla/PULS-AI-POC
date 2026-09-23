//W330J010 JOB (650W3300100W330J010,W100),'RTN W330V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(10,0)                                              
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P010 EXEC W330P010                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J010                                            
