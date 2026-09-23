//W440D6RS JOB (640W4400100W440D6RS,W100),'RTN W440D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W440D6                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440D6RS                                         
