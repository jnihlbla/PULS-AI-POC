000100 01  MID-W4I70401.                                                        
000200*                                 MID-COPYTEXT FÖR W4070400               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDRAPPNR-IN      PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MID-IDRAPPNR-UT      PIC X(7).                                    
001400*                                 RAPPORT NUMMER                          
001500     03 MID-RELANDCO         PIC X(6).                                    
001600*                                 LANDING COST PROCENT                    
001700     03 MID-PRFRAKT          PIC X(10).                                   
001800*                                 FRAKTKOSTNAD                            
001900     03 MID-PRFOERS          PIC X(10).                                   
002000*                                 FÖRSÄKRINGSPREMIE                       
002100     03 MID-PRLEGKST         PIC X(10).                                   
002200*                                 LEGALISERINSKOSTNAD                     
002300     03 MID-LEVANM-KLAR      PIC X.                                       
002400     03 MID-INFO-RAD         OCCURS 999 TIMES.                            
002500*                                 RADINFORMATION                          
002600        05 MID-IDORDNR5      PIC X(5).                                    
002700*                                 ORDERNUMMER                             
002800        05 MID-IDKOLLI       PIC X(5).                                    
002900*                                 KOLLINUMMER                             
003000        05 MID-IDARTNR       PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200        05 MID-KVLEVANM      PIC X(6).                                    
003300*                                 LEVERANSANMÄRKNINGSANTAL                
003400        05 MID-KDANMORS      PIC X(2).                                    
003500*                                 ORSAK TILL LEVERANSANMÄRKNING           
003600        05 MID-KDEMBLEV      PIC X.                                       
003700*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
003800        05 MID-PRARTBTO      PIC X(10).                                   
003900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004000        05 MID-KDFAKTYP      PIC X.                                       
004100*                                 FAKTURATYP                              
004200        05 MID-IDFAKT        PIC X(7).                                    
004300*                                 FAKTURANUMMER                           
004400        05 MID-IDDC          PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600*** END OF VILMAII-COPY LENGTH= 48023 BYTES                               
