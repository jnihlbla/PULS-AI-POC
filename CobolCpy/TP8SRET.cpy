000100* GENERATION OF COBOL HOST STRUCTURE FROM TP8SRET-TAB                     
000200  01 TP8SRET.                                                             
000300*              TP8SRET                                                    
000400   03 IDPARTNR       PIC X(9).                                            
000500*              PARTNERNUMMER                                              
000600   03 KDANMORS       PIC X(2).                                            
000700*              ORSAK TILL LEVERANSANMÄRKNING                              
000800   03 FLINVFEE       PIC X(1).                                            
000900*              INVOICE FLAG                                               
001000   03 PRARTNTO       PIC S9(7)V9(2) COMP-3.                               
001100*              ARTIKELPRIS NETTO                                          
001200   03 PRARTNTO-LDC   PIC S9(7)V9(2) COMP-3.                               
001300*              ARTIKELPRIS NETTO LDC                                      
001400   03 FLINVLDC       PIC X(1).                                            
001500*              INVOICE FLAG LDC                                           
001600   03 IDUSER         PIC X(8).                                            
001700*              ANVÄNDARENS SÄKERHETS ID                                   
001800   03 DAREGDAT       PIC X(8).                                            
001900*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
002000   03 DAUPPDAT       PIC X(8).                                            
002100*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
002200   03 IDFTG          PIC X(2).                                            
002300*              FÖRETAGSID EKONOM REDOVISNING                              
002400*                                                                         
002500*** END OF VILMAII-COPY LENGTH= 49 OLD LENGTH=                            
