//W611S3RS JOB (640W6110100W611S3RS,W100),'RTN W611S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W611S3                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611S3RS                                         
