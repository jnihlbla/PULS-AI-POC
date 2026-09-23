000100* GENERATION OF COBOL HOST STRUCTURE FROM TP0WLOG-TAB                     
000200  01 TP0WLOG.                                                             
000300*              TP0WLOG                                                    
000400   03 IDDC       PIC X(2).                                                
000500*              IDENTIFIERARE LAGER                                        
000600   03 TIREGDAT   PIC S9(7) COMP-3.                                        
000700*              REGISTRERINGSDATUM (ÅÅMMDD)                                
000800   03 TIREGTID   PIC S9(7) COMP-3.                                        
000900*              REGISTRERINGSTID                                           
001000   03 IDUSER     PIC X(8).                                                
001100*              ANVÄNDARENS SÄKERHETS ID                                   
001200   03 IDLOPNR    PIC S9(3) COMP-3.                                        
001300*              LÖPNUMMER                                                  
001400   03 KVMILSEC   PIC S9(7) COMP-3.                                        
001500*              ANTAL MILLISEK FÖR WEBSVAR                                 
001600   03 BEWEBSCR   PIC X(50).                                               
001700*              VALD BILD I PULS WEBBEN                                    
001800   03 BEWEBURL   PIC X(50).                                               
001900*              LÄNK PULS WEB SIDA                                         
002000*                                                                         
002100*** END OF VILMAII-COPY LENGTH= 124 OLD LENGTH=                           
