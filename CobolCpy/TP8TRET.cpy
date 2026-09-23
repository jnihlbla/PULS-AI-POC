000100* GENERATION OF COBOL HOST STRUCTURE FROM TP8TRET-TAB                     
000200  01 TP8TRET.                                                             
000300*              TP8TRET                                                    
000400   03 IDDISTR        PIC S9(5) COMP-3.                                    
000500*              DISTRIKTNUMMER                                             
000600   03 IDKUNDNR       PIC S9(7) COMP-3.                                    
000700*              KUNDNUMMER                                                 
000800   03 KDANMORS       PIC X(2).                                            
000900*              ORSAK TILL LEVERANSANMÄRKNING                              
001000   03 PRARTNTO       PIC S9(7)V9(2) COMP-3.                               
001100*              ARTIKELPRIS NETTO                                          
001200   03 PRARTNTO-LDC   PIC S9(7)V9(2) COMP-3.                               
001300*              ARTIKELPRIS NETTO LDC                                      
001400   03 FLINVFEE       PIC X(1).                                            
001500*              INVOICE FLAG                                               
001600   03 FLINVLDC       PIC X(1).                                            
001700*              INVOICE FLAG LDC                                           
001800   03 IDPARTNR       PIC X(9).                                            
001900*              FINANCIELL KUND                                            
002000   03 IDUSER         PIC X(8).                                            
002100*              ANVÄNDARENS SÄKERHETS ID                                   
002200   03 DAREGDAT       PIC X(8).                                            
002300*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
002400   03 DAUPPDAT       PIC X(8).                                            
002500*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
002600   03 IDFTG          PIC X(2).                                            
002700*              FÖRETAGSID EKONOM REDOVISNING                              
002800*                                                                         
002900*** END OF VILMAII-COPY LENGTH= 56 OLD LENGTH=                            
