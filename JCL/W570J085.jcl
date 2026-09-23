//W570J085 JOB (640W5700100W570J085,W100),'RTN W570M3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W570    EXEC W570P085                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J085                                         
