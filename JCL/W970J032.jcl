//W970J032 JOB (640W0000100W970J032,W100),'RTN W970B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   NJEVC                                                            
/*ROUTE  PRINT NJOVC                                                            
//*                                                                             
//PRINT   EXEC  W001ISPF                                                        
//TSO.IN   DD  DSN=W.SECUREND.ACF2RES,DISP=SHR                                  
//TSO.OUT  DD  DSN=W970.W970B3.W97030(+0),                                      
//             DISP=(MOD,KEEP,KEEP),                                            
//             RECFM=VBA,LRECL=137,BLKSIZE=27966                                
//SYSTSIN  DD *                                                                 
 PROFILE PREFIX(VOLVO)  /* HIX FOR TEMP ISPF LIST DATASET */                    
 ISPSTART CMD(%DSPRINT FROMDD(IN) FROMMEM(W*) TODD(OUT) )                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J032                                         
