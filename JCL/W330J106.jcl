//W330J106 JOB (650W3300100W330J106,W100),'RTN W330D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(5,0)                                               
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P006 EXEC W330P006,                                                       
//             INDIN2=W418.W330D5,                                              
//             INDUT=W330.W330D5                                                
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J106                                            
