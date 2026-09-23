//W980JOMS JOB (650W0090100W980JOMS,W100),'RTN W980D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MAIL SKICKAS TILL BEREDSKAPARE OM WBMPS004 INTE ÄR WAITING                  
//* KL 02:00 MÅNDAG-LÖRDAG (DÅ HAR INTE WBMP KOMMIT IGÅNG ORDENTLIGT)           
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE WBMP EJ NYSTARTAD                                                        
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
 WBMP verkar inte ha startat korrekt efter datumbytet                           
)END                                                                            
//*                                                                             
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  *                                                                
 000000 02:00 WBMP inte startat korrekt efter datumbytet                        
//*                                                                             
//SOPEND EXEC WSOPEND,PROCESS=W980JOMS                                          
