//W611J08H JOB (640W6110100W611J08H,W100),'RTN W611V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W611    EXEC W611P08H                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J08H                                         
