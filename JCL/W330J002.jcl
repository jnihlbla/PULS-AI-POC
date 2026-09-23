//W330J002 JOB (650W3300100W330J002,W100),'RTN W330V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(5,0)                                               
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P002 EXEC W330P002                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J002                                            
