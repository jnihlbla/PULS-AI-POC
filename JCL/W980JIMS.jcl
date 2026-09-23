//W980JIMS JOB (640W0090100W980JIMS,W100),'RTN W980D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*                                                                             
//W980    EXEC W980PIMS                                                         
//SYSTSIN DD *                                                                  
%W980JIMS &DATETIME W.QASE W.QASE W.QASE.CONSTANT W980HOUR                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980JIMS                                         
//*                                                                             
//BKUP    EXEC WSOPCOPY,BLOCKS=1000,                                            
//             SOPREG2=W.DUMP.QASE.SOP(+1)                                      
