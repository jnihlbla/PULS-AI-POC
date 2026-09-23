//WEACJSUD JOB (540W0030200WEACJSUD,W100),'RTN WEACS1',                         
//             MSGCLASS=A,CLASS=K,USER=?,PASSWORD=?                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//PROC JCLLIB ORDER=(WEAC.PUBLIC.PROCLIB,W.QASE.PROCLIB)                        
//*                                                                             
//ENV  INCLUDE MEMBER=ENVPUBL                                                   
//ORDER   EXEC EACREQST                                                         
//REQUEST.SYSIN DD *                                                            
 ORDER EACSUS SYMBOLS                                                           
   SYSTEM(PULSDBA) ACCOUNT(W00302)                                              
   EMERIDS(W) JOBNAME(WDBASUD)                                                  
   GRACETIME(6)                                                                 
//*                                                                             
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//SOPEND  EXEC WSOPEND,PROCESS=WEACJSUD                                         
