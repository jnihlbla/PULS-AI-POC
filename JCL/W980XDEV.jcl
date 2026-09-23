//W980XDEV JOB (640W0090100W980XDEV,W100),'RTN W980D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SUB     EXEC WFSUBMIT,JCLLIB=W.XDEV.JCL,SUBMIT=W980XDEV                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980XDEV                                         
