//W335V1RS JOB (640W3350100W335V1RS,W100),'RTN W335V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W335V1                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335V1RS                                         
