//W980ACPT JOB (640W0090100W980ACPT,W100),'RTN W980D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SUB     EXEC WFSUBMIT,JCLLIB=W.ACPT.JCL,SUBMIT=W980ACPT                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980ACPT                                         
