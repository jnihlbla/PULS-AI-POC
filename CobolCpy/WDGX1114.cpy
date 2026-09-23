000100 01  1114-WDGX1114.                                                       
000200*                                 ERSÄTTNING                              
000300*                                 TILLKOMMANDE RADER                      
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                  (KVKORT + LOWVALUE)                    
000600     03 1114-IDKORTNR        PIC S9(3)           COMP-3.                  
000700*                                 KORTNUMMER                              
000800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
000900*                                 SEQUENCE NUMBER FOR EACH RECORD         
001000*                                  IN A SUPERSESSION                      
001100     03 1114-LOWVALUE        PIC X(8).                                    
001200     03 1114-FLTEXT          PIC X.                                       
001300*                                 FINNS TEXTINFORMATION ?                 
001400     03 1114-TYP1.                                                        
001500        05 1114-IDARTNR-TILLK                                             
001600                             PIC S9(9)           COMP-3.                  
001700*                                 TILLKOMMANDE ARTIKELNUMMER              
001800*                                 REPLACEMENT PART NO.                    
001900        05 1114-DIERS-TILLK  PIC S9(4)V9(3)      COMP-3.                  
002000*                                 TILLKOMMANDE ARTIKELANTAL               
002100*                                 NUMBER OF SUPERSEDING                   
002200        05 FILLER            PIC X(11).                                   
002300     03 1114-TYP2 REDEFINES 1114-TYP1.                                    
002400        05 1114-BEERS        PIC X(20).                                   
002500*                                 ERSÄTTNINGSTEXT                         
002600*                                 REPLACEMENT TEXT                        
002700     03 FILLER               PIC X(9).                                    
002800*** END COPY WDGX1114C0  LENGTH=40                                        
