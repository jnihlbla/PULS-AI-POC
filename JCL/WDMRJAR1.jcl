//WDMRJAR1  JOB (650W0030200WDMRJAR1,W100),'RTN WDMRV9',                        
//            USER=?,PASSWORD=?,                                                
//            CLASS=T                                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ   NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//DMR    EXEC WDMRARCH,USER=W,DICT=PROD                                         
LOG ARCHIVE.                                                                    
LOG STATUS.                                                                     
//*                                                                             
//DELA   EXEC PGM=IEFBR14,COND=(8,GT,DMR.ARCHIVE)                               
//DDA      DD DSN=W.PROD.ARCHLOG(+1),DISP=(OLD,DELETE,DELETE)                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJAR1                                         
