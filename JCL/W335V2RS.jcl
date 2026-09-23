//W335V2RS JOB (640W3350100W335V2RS,W100),'RTN W335V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W335V2                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335V2RS                                         
