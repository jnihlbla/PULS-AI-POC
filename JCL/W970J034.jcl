//W970J034 JOB (640W0000100W970J034,W100),'RTN W970B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ  &ROUTE                                                           
/*ROUTE   PRINT NJOVC                                                           
//*+JBS BIND &BIND                                                              
//*                                                                             
//UXLIST  EXEC WOPNMVS,OPTION=NOLOGINPUT                                        
//SYSIN    DD  DSN=W.QASE.CONSTANT(W970&UXGRP),DISP=SHR                         
//SYSPRINT DD  DSN=&&W97034,DISP=(NEW,PASS,DELETE),                             
//             RECFM=VB,LRECL=136,                                              
//             DATACLAS=PSEN                                                    
//*                                                                             
//ACFLST  EXEC W001ISPF                                                         
//SYSTSIN  DD *                                                                 
 ISPSTART CMD(%ACFLSTDR)                                                        
//TSO.SYSIN  DD DSN=W.QASE.CONSTANT(W970&RULES),DISP=SHR                        
//TSO.ACFLIB DD DSN=W970.W970B3.TGR.ACF2RES,DISP=SHR                            
//TSO.REPORT DD DSN=&&RPTIN,DISP=(NEW,PASS,DELETE),                             
//             DATACLAS=PSEN,RECFM=VB,LRECL=254                                 
//*                                                                             
//ACLRPT  EXEC W970P034                                                         
//*                                                                             
//COPY    EXEC W001ISPF                                                         
//SYSTSIN  DD  *                                                                
 ISPSTART CMD(%DSCOPY FROMDD(IN) TODD(OUT) )                                    
 ISPSTART CMD(%DSCOPY FROMDD(IN2) TODD(OUT) )                                   
//TSO.IN   DD  DSN=&&W97034,DISP=(OLD,DELETE)                                   
//TSO.IN2  DD  DSN=&&RPTOUT,DISP=(OLD,DELETE)                                   
//TSO.OUT  DD  DSN=W970.W970B3.W97030(+0),                                      
//             DISP=(MOD,KEEP,KEEP),                                            
//             RECFM=VBA,LRECL=137,BLKSIZE=27966                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J034                                         
