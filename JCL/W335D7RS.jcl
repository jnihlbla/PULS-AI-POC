//W335D7RS JOB (640W3350100W335D7RS,W100),'RTN W335D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W335D7                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335D7RS                                         
