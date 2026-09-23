//W428J037 JOB (640W4280100W428J037,W100),'RTN W428D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W428    EXEC W428P037                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J037                                         
