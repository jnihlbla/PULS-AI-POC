000100 01  W41843.                                                              
000200*                                 DL-ARTIKLAR FÖR VIR-RAPPORT TIL         
000300*                                 L LEV.                                  
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDLEVANM.                                                         
000800*                                 LEVERANSANMÄRKNINGSIDENTITET            
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300        05 IDRAPPNR          PIC 9(7).                                    
001400*                                 RAPPORT NUMMER                          
001500     03 IDDC                 PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDLEVNR              PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 KDANMORS             PIC X(2).                                    
002000*                                 ORSAK TILL LEVERANSANMÄRKNING           
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KVLEVANM             PIC S9(7)           COMP-3.                  
002400*                                 LEVERANSANMÄRKNINGSANTAL                
002500     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
002600*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
002700     03 IDORDNR5             PIC 9(5).                                    
002800*                                 ORDERNUMMER                             
002900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
003000*                                 PRODUKTIONSNUMMER                       
003100     03 IDKOLLI              PIC S9(5)           COMP-3.                  
003200*                                 KOLLINUMMER                             
003300     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
003400*                                 KREDITNOTANUMMER                        
003500     03 IDLEVNR-PRIS         PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700     03 KDVALISO             PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900     03 TILLAEGGSKOSTNADER.                                               
004000        05 PRFOERS           PIC S9(7)V9(2)      COMP-3.                  
004100*                                 FÖRSÄKRINGSPREMIE                       
004200        05 PRFRAKT           PIC S9(7)V9(2)      COMP-3.                  
004300*                                 FRAKTKOSTNAD                            
004400        05 PRLEGKST          PIC S9(7)V9(2)      COMP-3.                  
004500*                                 LEGALISERINSKOSTNAD                     
004600        05 PRLANDCO          PIC S9(7)V9(2)      COMP-3.                  
004700*                                 LANDING COST                            
004800        05 PREMBHNT          PIC S9(7)V9(2)      COMP-3.                  
004900*                                 EMBALLAGE O HANTERINGSKOST              
005000        05 PRMOMS            PIC S9(7)V9(2)      COMP-3.                  
005100*                                 MERVÄRDESSKATT                          
005200*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
