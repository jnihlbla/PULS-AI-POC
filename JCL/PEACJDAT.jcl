//PEACJDAT JOB (540W0030200PEACJDAT,0000),'RTN PEACW1',                         
//             MSGCLASS=A,CLASS=K,USER=?,PASSWORD=?                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE   PRINT NJOVC                                                           
//PROC JCLLIB ORDER=(WEAC.PUBLIC.PROCLIB,W.QASE.PROCLIB)                        
//*                                                                             
//ENV  INCLUDE MEMBER=ENVPUBL                                                   
//ORDER   EXEC EACREQST                                                         
//REQUEST.SYSIN DD *                                                            
 ORDER EACDAT SYMBOLS                                                           
//*                                                                             
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//SOPEND  EXEC WSOPEND,PROCESS=PEACJDAT                                         
