//W510J195 JOB (640W5100100W510J195,W100),'RTN W500V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P095,                                                        
//             INDIN1=W510.W500V1,                                              
//             INDIN2=W510.W500V1                                               
//W51095.W51095D1 DD DSN=W510.W500V1.W51053(-1),DISP=SHR                        
//                DD DSN=W510.W500V1.W51054(-1),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510J195                                         
