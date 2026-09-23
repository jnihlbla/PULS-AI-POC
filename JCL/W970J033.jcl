//W970J033 JOB (640W0000100W970J033,W100),'RTN W970B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   NJEVC                                                            
/*ROUTE  PRINT NJOVC                                                            
//*                                                                             
//DSNTEP2 EXEC FLOGON                                                           
//SYSPRINT DD  DSN=&&W97033,DISP=(NEW,PASS,DELETE),                             
//             RECFM=FBA,LRECL=133,                                             
//             DATACLAS=PSEN                                                    
//SYSTSIN  DD  DSN=W.&DB2ENV..CONSTANT(DSNTEP2),DISP=SHR                        
//SYSIN    DD  DSN=W.QASE.CONSTANT(W970DB2G),DISP=SHR                           
//*                                                                             
//COPY    EXEC  W001ISPF                                                        
//SYSTSIN  DD *                                                                 
 ISPSTART CMD(%DSCOPY FROMDD(IN) TODD(OUT) )                                    
//TSO.IN   DD  DSN=&&W97033,DISP=(OLD,DELETE)                                   
//TSO.OUT  DD  DSN=W970.W970B3.W97030(+0),                                      
//             DISP=(MOD,KEEP,KEEP),                                            
//             RECFM=VBA,LRECL=137,BLKSIZE=27966                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J033                                         
