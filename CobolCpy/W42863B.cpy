000100 01  W42863B.                                                             
000200*                                 PERIODPOST FÖRPACKNINGSLISTA            
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
001700        05 KDFRAKT           PIC S9(3)           COMP-3.                  
001800*                                 FRAKTSÄTT DC TILL KUND                  
001900        05 KDEMBLEV          PIC S9              COMP-3.                  
002000*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
002100        05 KVLEVANM          PIC S9(7)           COMP-3.                  
002200*                                 LEVERANSANMÄRKNINGSANTAL                
002300        05 PRARTBTO          PIC S9(7)V9(2)      COMP-3.                  
002400*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
002500        05 IDKOLLI           PIC S9(5)           COMP-3.                  
002600*                                 KOLLINUMMER                             
002700        05 KDFAKTYP          PIC X.                                       
002800*                                 FAKTURATYP                              
002900        05 IDFAKT            PIC S9(7)           COMP-3.                  
003000*                                 FAKTURANUMMER                           
003100        05 TIFAKT            PIC S9(7)           COMP-3.                  
003200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003300        05 IDRAPPNR          PIC 9(7).                                    
003400*                                 RAPPORT NUMMER                          
003500        05 TILEVANM          PIC S9(7)           COMP-3.                  
003600*                                 DATUM LEVERANSANMÄRKNING                
003700*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
