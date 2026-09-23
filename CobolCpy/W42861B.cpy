000100 01  W42861B.                                                             
000200*                                 LISTRAD   INVENTERINGSLISTA             
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
001600        05 FLDIRLEV          PIC X.                                       
001700*                                 DIREKTLEVERANS ?                        
001800        05 IDDISTR           PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000        05 IDFAKT            PIC S9(7)           COMP-3.                  
002100*                                 FAKTURANUMMER                           
002200        05 IDKNOTNR          PIC S9(7)           COMP-3.                  
002300*                                 KREDITNOTANUMMER                        
002400        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 IDRAPPNR          PIC 9(7).                                    
002700*                                 RAPPORT NUMMER                          
002800        05 KVLEVANM          PIC S9(7)           COMP-3.                  
002900*                                 LEVERANSANMÄRKNINGSANTAL                
003000        05 PRARTBTO          PIC S9(7)V9(2)      COMP-3.                  
003100*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003200        05 TIFAKT            PIC S9(7)           COMP-3.                  
003300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003400        05 TIINLINL          PIC S9(7)           COMP-3.                  
003500*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003600*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
