000100* GENERATION OF COBOL HOST STRUCTURE FROM TP8FRET-TAB                     
000200  01 TP8FRET.                                                             
000300*              TP8FRET                                                    
000400   03 IDPARTNR           PIC X(9).                                        
000500*              PARTNERNUMMER                                              
000600   03 KDANMORS           PIC X(2).                                        
000700*              ORSAK TILL LEVERANSANMÄRKNING                              
000800   03 SUARTBTO-MIN       PIC S9(7)V9(2) COMP-3.                           
000900*              SUMMA FÖRSÄLJNINGSVÄRDE MIN                                
001000   03 FLINVLDC           PIC X(1).                                        
001100*              INVOICE FLAG LDC                                           
001200   03 SUARTBTO-LDC-MIN   PIC S9(7)V9(2) COMP-3.                           
001300*              SUMMA FÖRSÄLJNINGSVÄRDE LDC                                
001400   03 IDUSER             PIC X(8).                                        
001500*              ANVÄNDARENS SÄKERHETS ID                                   
001600   03 DAREGDAT           PIC X(8).                                        
001700*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001800   03 DAUPPDAT           PIC X(8).                                        
001900*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
002000   03 IDFTG              PIC X(2).                                        
002100*              FÖRETAGSID EKONOM REDOVISNING                              
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 48 OLD LENGTH=                            
