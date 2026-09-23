//W980JBMS JOB (650W0090100W980JBMS,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MAIL SKICKAS TILL BEREDSKAPARE OM WBATCH ÄR AKTIV 04.00.                    
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE WBATCH EJ KLAR                                                           
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
 WBATCH är inte klar än                                                         
)END                                                                            
//*                                                                             
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  *                                                                
 000000 04:00 WBATCH är inte klar än                                            
//*                                                                             
//SOPEND EXEC WSOPEND,PROCESS=W980JBMS                                          
