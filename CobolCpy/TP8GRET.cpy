000100* GENERATION OF COBOL HOST STRUCTURE FROM TP8GRET-TAB                     
000200  01 TP8GRET.                                                             
000300*              TP8GRET                                                    
000400   03 IDDISTR            PIC S9(5) COMP-3.                                
000500*              DISTRIKTNUMMER                                             
000600   03 IDKUNDNR           PIC S9(7) COMP-3.                                
000700*              KUNDNUMMER                                                 
000800   03 KDANMORS           PIC X(2).                                        
000900*              ORSAK TILL LEVERANSANMÄRKNING                              
001000   03 FLINVLDC           PIC X(1).                                        
001100*              INVOICE FLAG LDC                                           
001200   03 SUARTBTO-MIN       PIC S9(7)V9(2) COMP-3.                           
001300*              SUMMA FÖRSÄLJNINGSVÄRDE MIN                                
001400   03 SUARTBTO-LDC-MIN   PIC S9(7)V9(2) COMP-3.                           
001500*              SUMMA FÖRSÄLJNINGSVÄRDE LDC                                
001600   03 IDPARTNR           PIC X(9).                                        
001700*              FINANCIELL KUND                                            
001800   03 IDUSER             PIC X(8).                                        
001900*              ANVÄNDARENS SÄKERHETS ID                                   
002000   03 DAREGDAT           PIC X(8).                                        
002100*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
002200   03 DAUPPDAT           PIC X(8).                                        
002300*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
002400   03 IDFTG              PIC X(2).                                        
002500*              FÖRETAGSID EKONOM REDOVISNING                              
002600*                                                                         
002700*** END OF VILMAII-COPY LENGTH= 55 OLD LENGTH=                            
