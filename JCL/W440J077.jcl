//W440J077 JOB (640W4400100W440J077,W100),'RTN W440V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P077                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J077                                         
