//W980JNMS JOB (650W0090100W980JNMS,W100),'RTN W980D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MAIL SKICKAS TILL BEREDSKAPARE OM WBATCHNA ÄR AKTIV 07:00                   
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE WBATCHNA SEN                                                             
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
 WBATCHNA är ej klar                                                            
)END                                                                            
//*                                                                             
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  *                                                                
 000000 07:00 WBATCHNA är ej klar                                               
//*                                                                             
//SOPEND EXEC WSOPEND,PROCESS=W980JNMS                                          
