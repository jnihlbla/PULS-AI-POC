//W970J030 JOB (640W0000100W970J030,W100),'RTN W970B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*                                                                             
//WCOPY    EXEC  W001ISPF                                                       
//SYSTSIN  DD *                                                                 
 ISPSTART CMD(%DSCOPY FROMDS(W.SECUR.ACF2RULE) FROMMEM(*) +                     
               TODS(W970.W970B3.ACF2RULE) REPLACE(Y) )                          
 ACF                                                                            
 SET RULE                                                                       
 DECOMP LIKE(W-) INTO('W970.W970B3.ACF2RULE')                                   
 END                                                                            
                                                                                
 ISPSTART CMD(%DSCOPY FROMDS(W.SECURTGR.ACF2RES) FROMMEM(*) +                   
               TODS(W970.W970B3.TGR.ACF2RES) REPLACE(Y) )                       
 ACF                                                                            
 SET RESOURCE(TGR)                                                              
 DECOMP LIKE(W-) INTO('W970.W970B3.TGR.ACF2RES')                                
 END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J030                                         
