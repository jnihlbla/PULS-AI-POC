//W560J064 JOB (640W5600100W560J064,W100),'RTN W560Y1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W560    EXEC W560P064                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560J064                                         
