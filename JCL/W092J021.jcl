//W092J021 JOB (650W0920100W092J021,W100),'RTN W092D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*                                     //*---  WMEMOSND,EXC                    
//W092    EXEC W092P021                                                         
//*                                                                             
//  EXEC WMEMOSND,CONDS='(8,GT,W092.W09206)'                                    
)SEND                                                                           
TITLE W092D2-FEL                                                                
DEST WSYST@VOLVOCARS.COM                                                        
OPTION FORCE                                                                    
MEMO                                                                            
                                                                                
 FELAKTIGA TRANSAKTIONS-TYPER!                                                  
 VISSA TRANSAKTIONER TILL IN-TRATTEN W092J021 SAKNAR KOPPLING                   
 NÅGON BEHANDLANDE MODUL, OCH HAR SKIPPATS.                                     
                                                                                
 SE FIL W092.W092D2.W09226 FÖR DETALJER.                                        
                                                                                
)END                                                                            
//SOP     EXEC WSOPEND,PROCESS=W092J021                                         
