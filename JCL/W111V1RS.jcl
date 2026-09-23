//W111V1RS JOB (640W1110100W111V1RS,W100),'RTN W111V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W111V1                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111V1RS                                         
