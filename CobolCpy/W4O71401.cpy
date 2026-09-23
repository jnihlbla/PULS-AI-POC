000100 01  MOD-W4O71401.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4071400           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDARTNR-IN       PIC X(8).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDRADNR-IN       PIC X(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDARTNR-ENTER    PIC 9(8).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
003000*                                 RADNUMMER                               
003100     03 MOD-IDARTNR-NEXT     PIC 9(8).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
003400*                                 RADNUMMER                               
003500     03 MOD-TILEVANM         PIC 9(6).                                    
003600*                                 DATUM LEVERANSANMÄRKNING                
003700     03 MOD-RELANDCO         PIC Z(2)9.9(2).                              
003800*                                 LANDING COST PROCENT                    
003900     03 MOD-PRFRAKT          PIC Z(6)9.9(2).                              
004000*                                 FRAKTKOSTNAD                            
004100     03 MOD-PRFOERS          PIC Z(6)9.9(2).                              
004200*                                 FÖRSÄKRINGSPREMIE                       
004300     03 MOD-PRLEGKST         PIC Z(6)9.9(2).                              
004400*                                 LEGALISERINSKOSTNAD                     
004500     03 MOD-KDVALISO         PIC X(3).                                    
004600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004700     03 MOD-LEVANM-RAD       OCCURS 13 TIMES.                             
004800*                                 LEVERANSANM.RAD I PGM W4071400          
004900        05 MOD-IDARTNR       PIC Z(7)9.                                   
005000*                                 ARTIKELNUMMER                           
005100        05 FILLER            PIC X.                                       
005200        05 MOD-IDRADNR       PIC Z(3)9.                                   
005300*                                 RADNUMMER                               
005400        05 FILLER            PIC X(2).                                    
005500        05 MOD-KDANMORS      PIC X(2).                                    
005600*                                 ORSAK TILL LEVERANSANMÄRKNING           
005700        05 FILLER            PIC X.                                       
005800        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
005900*                                 ARTIKELPRIS NETTO                       
006000        05 FILLER            PIC X.                                       
006100        05 MOD-PRARTBTO      PIC Z(6)9.9(2).                              
006200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
006300        05 FILLER            PIC X(2).                                    
006400        05 MOD-ADLAGOMR      PIC Z9.                                      
006500*                                 LAGEROMRÅDE                             
006600        05 FILLER            PIC X.                                       
006700        05 MOD-ADGANG        PIC Z9.                                      
006800*                                 GÅNG                                    
006900        05 FILLER            PIC X.                                       
007000        05 MOD-ADPLATS       PIC Z(4)9.                                   
007100*                                 LAGERPLATSNUMMER                        
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 904 BYTES                                 
