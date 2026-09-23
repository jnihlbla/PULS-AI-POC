//W980JLST JOB (650W0090100W980JLST,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//*                                                                             
//* MAIL IS SENT TO ALL IN THE ON-CALL POOL IF ANYTHING ON SOP:S                
//* LIST STARTED MENU SEEMS WRONG                                               
//*                                                                             
//W980    EXEC W980P029                                                         
//W98029.SYSTSIN DD *                                                           
ISPSTART PGM(W98029)                                                            
//*                                                                             
//W98030T EXEC WEMPTST,DSIN=W980.W980D1.W98030(+1)                              
//*                                                                             
//EJTOM IF (W98030T.T.RC EQ 0) THEN                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE LS                                                                       
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
//         DD  DSN=W980.W980D1.W98030(+1),DISP=SHR                              
//         DD  *                                                                
)END                                                                            
//*                                                                             
//*        -- UPPDATERA VECKO-LOGGEN                                            
//JOURLOG EXEC W980PMSG                                                         
//INDATA   DD  DSN=W980.W980D1.W98030(+1),DISP=SHR                              
//*                                                                             
//EJTOM ENDIF   -- END W98030T.T.RC = 0                                         
//*                                                                             
//SOPEND EXEC WSOP,COND=EVEN                                                    
END W980JLST                                                                    
