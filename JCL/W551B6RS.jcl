//W551B6RS JOB (640W5510100W551B6RS,W100),'RTN W551B6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W551B6                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B6RS                                         
