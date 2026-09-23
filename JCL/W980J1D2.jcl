//W980J1D2 JOB (650W0090100W980J1D2,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* MAIL to on call phone if W011D2 still Activ 22.00 on 1st January            
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE W011D2 not ready. Read instructions!!                                    
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
 W011D2 är inte klar än                                                         
)END                                                                            
//*                                                                             
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  *                                                                
 000000 22:00 W011D2 är inte klar än                                            
//*                                                                             
//SOPEND EXEC WSOPEND,PROCESS=W980J1D2                                          
