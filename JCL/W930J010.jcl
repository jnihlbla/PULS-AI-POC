//W930J010 JOB (640W9300100W930J010,W100),'RTN W930S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W930    EXEC W930P010                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W930J010                                         
