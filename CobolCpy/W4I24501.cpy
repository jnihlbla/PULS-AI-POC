000100 01  MID-W4I24501.                                                        
000200*                                 MID-COPYTEXT FÖR W40245                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDKUNDRF-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MID-IDKUNDRF-UT      PIC X(7).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-IDARTNR-IN       PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MID-IDORDER-ENTER    PIC 9(7).                                    
002400*                                 VOLVO PARTS ORDERNUMMER                 
002500     03 MID-IDORDER-NEXT     PIC 9(7).                                    
002600*                                 VOLVO PARTS ORDERNUMMER                 
002700     03 MID-IDDC-ENTER       PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-IDDC-NEXT        PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MID-ADLAGOMR-ENTER   PIC 9(3).                                    
003200*                                 LAGEROMRÅDE                             
003300     03 MID-ADLAGOMR-NEXT    PIC 9(3).                                    
003400*                                 LAGEROMRÅDE                             
003500     03 MID-ADGANG-ENTER     PIC 9(3).                                    
003600*                                 GÅNG                                    
003700     03 MID-ADGANG-NEXT      PIC 9(3).                                    
003800*                                 GÅNG                                    
003900     03 MID-ADPLATS-ENTER    PIC 9(5).                                    
004000*                                 LAGERPLATSNUMMER                        
004100     03 MID-ADPLATS-NEXT     PIC 9(5).                                    
004200*                                 LAGERPLATSNUMMER                        
004300     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
004400*                                 ARTIKELNUMMER                           
004500     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
004600*                                 ARTIKELNUMMER                           
004700     03 MID-IDLOPNR-ENTER    PIC 9(3).                                    
004800*                                 LÖPNUMMER                               
004900     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
005000*                                 LÖPNUMMER                               
005100     03 MID-IDPRODNR-ENTER   PIC 9(7).                                    
005200*                                 PRODUKTIONSNUMMER                       
005300     03 MID-IDPRODNR-NEXT    PIC 9(7).                                    
005400*                                 PRODUKTIONSNUMMER                       
005500     03 MID-IDPLKLST-ENTER   PIC 9(3).                                    
005600*                                 PLOCKLISTNUMMER                         
005700     03 MID-IDPLKLST-NEXT    PIC 9(3).                                    
005800*                                 PLOCKLISTNUMMER                         
005900     03 MID-IDPURAD-ENTER    PIC 9(5).                                    
006000*                                 RADNUMMER                               
006100     03 MID-IDPURAD-NEXT     PIC 9(5).                                    
006200*                                 RADNUMMER                               
006300     03 MID-INPUT.                                                        
006400*                                 INDATA FÖR UPPDATERING                  
006500        05 MID-FLAGGA-UPDATE PIC X.                                       
006600*                                 JA/NEJ-FLAGGA                           
006700        05 MID-CMD-UPDATE    OCCURS 6 TIMES                               
006800                             PIC X.                                       
006900*                                 BEHANDLINGSKOD-X                        
007000        05 MID-KVBEART-Q-UPDATE                                           
007100                             OCCURS 6 TIMES                               
007200                             PIC X(7).                                    
007300*                                 ANTAL ARTNR PER BRYTBEGREPP             
007400     03 MID-IDLOPNR-SPAR     OCCURS 6 TIMES                               
007500                             PIC 9(3).                                    
007600*                                 LÖPNUMMER                               
007700     03 MID-ADLAGOMR-SPAR    OCCURS 6 TIMES                               
007800                             PIC 9(3).                                    
007900*                                 LAGEROMRÅDE                             
008000     03 MID-ADGANG-SPAR      OCCURS 6 TIMES                               
008100                             PIC 9(3).                                    
008200*                                 GÅNG                                    
008300     03 MID-ADPLATS-SPAR     OCCURS 6 TIMES                               
008400                             PIC 9(5).                                    
008500*                                 LAGERPLATSNUMMER                        
008600     03 MID-IDARTNR-SPAR     OCCURS 6 TIMES                               
008700                             PIC 9(9).                                    
008800*                                 ARTIKELNUMMER                           
008900     03 MID-IDPRODNR-SPAR    OCCURS 6 TIMES                               
009000                             PIC 9(7).                                    
009100*                                 PRODUKTIONSNUMMER                       
009200     03 MID-IDPLKLST-SPAR    OCCURS 6 TIMES                               
009300                             PIC 9(3).                                    
009400*                                 PLOCKLISTNUMMER                         
009500     03 MID-IDPURAD-SPAR     OCCURS 6 TIMES                               
009600                             PIC 9(5).                                    
009700*                                 RADNUMMER                               
009800*** END OF VILMAII-COPY LENGTH= 427 BYTES                                 
