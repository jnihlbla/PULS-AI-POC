//PEACJSPW JOB (540W0030200PEACJSPW,0000),'RTN PEACW1',                         
//             MSGCLASS=A,CLASS=K,USER=?,PASSWORD=?                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE   PRINT NJOVC                                                           
//PROC JCLLIB ORDER=(WEAC.PUBLIC.PROCLIB,W.QASE.PROCLIB)                        
//*                                                                             
//ENV  INCLUDE MEMBER=ENVPUBL                                                   
//ORDER   EXEC EACREQST                                                         
//REQUEST.SYSIN DD *                                                            
 ORDER EACSPW SYMBOLS                                                           
   SYSUSER(PCEAC01)                                                             
//*                                                                             
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//SOPEND  EXEC WSOPEND,PROCESS=PEACJSPW                                         
