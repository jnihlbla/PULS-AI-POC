//WEACJSUS JOB (540W0030200WEACJSUS,W100),'RTN WEACS1',                         
//             MSGCLASS=A,CLASS=K,USER=?,PASSWORD=?                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJESD                                                             
/*ROUTE   PRINT LOCAL                                                           
//PROC JCLLIB ORDER=(WEAC.PUBLIC.PROCLIB,W.QASE.PROCLIB)                        
//*                                                                             
//ENV  INCLUDE MEMBER=ENVPUBL                                                   
//ORDER   EXEC EACREQST                                                         
//REQUEST.SYSIN DD *                                                            
 ORDER EACSUS SYMBOLS                                                           
   SYSTEM(PULS) ACCOUNT(W00302)                                                 
   EMERIDS(PCQ00) JOBNAME(PCQ00XX)                                              
   GRACETIME(6)                                                                 
//*                                                                             
//ORDER   EXEC EACREQST                                                         
//REQUEST.SYSIN DD *                                                            
 ORDER EACSUS SYMBOLS                                                           
   SYSTEM(PULS) ACCOUNT(W00302)                                                 
   EMERIDS(PCQ02) JOBNAME(PCQ02XX)                                              
   GRACETIME(72)                                                                
//*                                                                             
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//SOPEND  EXEC WSOPEND,PROCESS=WEACJSUS                                         
