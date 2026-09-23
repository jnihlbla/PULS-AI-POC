000100 01  W42863A.                                                             
000200*                                 LISTHUVUD FÖRPACKNINGSLISTA             
000300*                                                                         
000400     03 SORT-AREA.                                                        
000500        05 SORT-IDPTYP       PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 SORT-IDARTNR      PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900        05 SORT-TIAAPP       PIC S9(5)           COMP-3.                  
001000*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
001100*                                 12 PER ÅR                               
001200        05 SORT-IDDISTR      PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400        05 SORT-IDKUNDNR     PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 DATA-AREA.                                                        
001700        05 BEART             PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900        05 IDDC              PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100        05 PERIOD-AREA       OCCURS 12 TIMES.                             
002200           07 TIAAPP         PIC S9(5)           COMP-3.                  
002300*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
002400*                                 12 PER ÅR                               
002500           07 SULEVANT-PER   PIC S9(9)           COMP-3.                  
002600*                                 ANTAL LEV ART SENASTE PERIOD            
002700*                                 (AF5)                                   
002800*** END OF VILMAII-COPY LENGTH= 141 BYTES                                 
