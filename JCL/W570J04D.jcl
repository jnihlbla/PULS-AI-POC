//W570J04D JOB (640W5700100W570J04D,W100),'RTN W570M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W570    EXEC W570P04D                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J04D                                         
