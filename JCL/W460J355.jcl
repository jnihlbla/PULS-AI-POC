//W460J355 JOB (640W4600100W460J355,W100),'RTN W460S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W460    EXEC W460P355                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W460J355                                         
