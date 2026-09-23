//W611J020 JOB (640W6110100W611J020,W100),'RTN W611V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P020                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J020                                         
