//WDMRJUNL  JOB (650W0030200WDMRJUNL,W100),'RTN WDMRV9',                        
//            USER=?,PASSWORD=?,                                                
//            CLASS=T                                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ   NJESD                                                             
/*ROUTE PRINT NJOVC                                                             
//WDMR   EXEC WDMRUNLO,USER=W,DICT=PROD                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJUNL                                         
