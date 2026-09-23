//W513J012 JOB (640W5130100W513J012,W100),'RTN W513V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W513    EXEC W513P012                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J012                                         
