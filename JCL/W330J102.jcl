//W330J102 JOB (650W3300100W330J102,W100),'RTN W330D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(5,0)                                               
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P002 EXEC W330P002,                                                       
//             INDIN3=W371.W330D5,                                              
//             INDIN4=W476.W330D5,                                              
//             INDUT=W330.W330D5                                                
//*                                                                             
//W33002.SYSUDUMP DD SYSOUT=*                                                   
//W33002.CEEDUMP  DD SYSOUT=*                                                   
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J102                                            
