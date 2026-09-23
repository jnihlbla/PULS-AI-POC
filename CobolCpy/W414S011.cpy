000100 01  ANNVOR-W414S011.                                                     
000200*                                 SKAPAS FÖR ANNULLATION                  
000300*                                 AV VORKÖ-RADER                          
000400*                                 ANVÄNDS VID TRANSAKTION-                
000500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000600     03 ANNVOR-IDARTNR-S     PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 ANNVOR-IDARTNR-TILLK-S                                            
000900                             PIC S9(9)           COMP-3.                  
001000*                                 TILLKOMMANDE ARTIKELNUMMER              
001100     03 ANNVOR-W414011.                                                   
001200*                                 SKAPAS FÖR ANNULLATION                  
001300*                                 AV VORKÖRAD                             
001400*                                 SÄNDS TILL SRS.                         
001500        05 ANNVOR-IDPTYP     PIC X(3).                                    
001600*                                 POSTTYP                                 
001700        05 ANNVOR-IDARTNR    PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900        05 ANNVOR-IDDC       PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100        05 ANNVOR-IDDISTR    PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 ANNVOR-IDKUNDNR   PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 ANNVOR-KDPRODSL   PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700        05 ANNVOR-FLDIRLEV   PIC X.                                       
002800*                                 DIREKTLEVERANS ?                        
002900        05 ANNVOR-KVANNANT   PIC S9(7)           COMP-3.                  
003000*                                 ANNULLERAT ANTAL ARTIKLAR               
003100        05 ANNVOR-KVAVBART   PIC S9(7)           COMP-3.                  
003200*                                 AVBOKAT ANTAL ARTIKLAR                  
003300        05 ANNVOR-KVBEART-Q  PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003500*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
