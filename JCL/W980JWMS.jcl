//W980JWMS JOB (650W0090100W980JWMS,W100),'RTN W980D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MAIL SKICKAS TILL BEREDSKAPARE OM WBATCHW1 INTE STARTAT                     
//* KL 06:00 PÅ SÖNDAGAR.                                                       
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE WEEK-BATCH EJ STARTAD                                                    
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
 WBATCHW1 är fortfarande waiting                                                
)END                                                                            
//*                                                                             
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  *                                                                
 000000 06:00 WBATCHW1 är fortfarande waiting                                   
//*                                                                             
//SOPEND EXEC WSOPEND,PROCESS=W980JWMS                                          
