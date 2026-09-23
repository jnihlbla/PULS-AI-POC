000100 01  MID-W4I71201.                                                        
000200*                                 MID-COPYTEXT FÖR W4071200               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDRAPPNR-IN      PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MID-IDRAPPNR-UT      PIC 9(7).                                    
001400*                                 RAPPORT NUMMER                          
001500     03 MID-IDARTNR-IN       PIC X(8).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MID-IDARTNR-UT       PIC X(8).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-IDRADNR-IN       PIC X(4).                                    
002000*                                 RADNUMMER                               
002100     03 MID-IDRADNR-UT       PIC X(4).                                    
002200*                                 RADNUMMER                               
002300     03 MID-IDARTNR-ENTER    PIC X(8).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MID-IDRADNR-ENTER    PIC 9(5).                                    
002600*                                 RADNUMMER                               
002700     03 MID-IDARTNR-NEXT     PIC X(8).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MID-IDRADNR-NEXT     PIC 9(5).                                    
003000*                                 RADNUMMER                               
003100     03 MID-INFO-RAD         OCCURS 12 TIMES.                             
003200*                                 RADINFORMATION                          
003300        05 MID-IDARTNR       PIC X(8).                                    
003400*                                 ARTIKELNUMMER                           
003500        05 MID-IDRADNR       PIC X(4).                                    
003600*                                 RADNUMMER                               
003700        05 MID-KDANMORS      PIC X(2).                                    
003800*                                 ORSAK TILL LEVERANSANMÄRKNING           
003900        05 MID-KDKREBEH      PIC X(3).                                    
004000*                                 BEHANDLINGSSTATUS                       
004100     03 MID-RAD18-IDARTNR    PIC 9(8).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MID-RAD18-IDRADNR    PIC 9(4).                                    
004400*                                 RADNUMMER                               
004500     03 MID-RAD18-KDANMORS   PIC 9(2).                                    
004600*                                 ORSAK TILL LEVERAN KDANMORS-002         
004700     03 MID-RAD18-KVLEVANM   PIC 9(6).                                    
004800*                                 LEVERANSANMÄRKNINGSANTAL                
004900     03 MID-RAD18-PRARTBTO   PIC X(10).                                   
005000*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005100     03 MID-RAD18-FLDIRLEV   PIC X.                                       
005200*                                 DIREKTLEVERANS ?                        
005300*** END OF VILMAII-COPY LENGTH= 319 BYTES                                 
