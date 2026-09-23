000100 01  W42861A.                                                             
000200*                                 LISTHUVUD INVENTERINGSLISTA             
000300*                                                                         
000400     03 SORT-GRP.                                                         
000500        05 SORT-IDPTYP       PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 SORT-KDANMORS     PIC X(2).                                    
000800*                                 ORSAK TILL LEVERANSANMÄRKNING           
000900        05 SORT-TIKNOTA      PIC S9(7)           COMP-3.                  
001000*                                 KREDITNOTADATUM                         
001100        05 SORT-TIRETANK     PIC S9(7)           COMP-3.                  
001200*                                 ANKOMSTDATUM                            
001300        05 SORT-TILEVANM     PIC S9(7)           COMP-3.                  
001400*                                 DATUM LEVERANSANMÄRKNING                
001500     03 DATA-GRP.                                                         
001600        05 IDARTNR           PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800        05 IDDC              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 IDDISTR           PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400        05 TIAAMMDD-FOM      PIC S9(7)           COMP-3.                  
002500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002600        05 TIAAMMDD-TOM      PIC S9(7)           COMP-3.                  
002700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002800        05 BEART             PIC X(25).                                   
002900*                                 ARTIKELBENÄMNING                        
003000*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
