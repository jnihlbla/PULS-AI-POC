//W330J019 JOB (650W3300100W330J019,W100),'RTN W330V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M,TIME=(10,0)                                              
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P019 EXEC W330P019                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J019                                            
