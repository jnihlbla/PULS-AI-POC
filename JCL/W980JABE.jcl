//W980JABE JOB (650W0090100W980JABE,W100),'RTN W980D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 ETITLE &ABENDMSG                                                               
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
                                                                                
 &ABENDMSG                                                                      
                                                                                
)END                                                                            
//*                                                                             
//*        -- UPPDATERA VECKO-LOGGEN                                            
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  *                                                                
&ABENDMSG                                                                       
//*                                                                             
//SOPEND  EXEC WSOP,COND=EVEN                                                   
END W980JABE                                                                    
