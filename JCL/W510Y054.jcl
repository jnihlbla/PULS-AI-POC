//W510Y054 JOB (640W5100100W510Y054,W100),'RTN WYR001',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W510    EXEC W510P054,INDUT=W510.W500V1                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510Y054                                         
