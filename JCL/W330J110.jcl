//W330J110 JOB (650W3300100W330J110,W100),'RTN W330D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(10,0)                                              
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P010 EXEC W330P010,                                                       
//             INDIN=W330.W330D5,                                               
//             INDUT=W330.W330D5                                                
//*                                                                             
//W33010.W33010D2 DD DUMMY                                                      
//W33010.W33010D3 DD DUMMY                                                      
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J110                                            
