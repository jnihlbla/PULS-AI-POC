000100 01  MID-W4I20401.                                                        
000200*                                 MID-COPYTEXT FÖR W40204                 
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
005100     03 MID-INPUT.                                                        
005200*                                 INDATA FÖR UPPDATERING                  
005300        05 MID-FLAGGA-UPDATE PIC X.                                       
005400*                                 JA/NEJ-FLAGGA                           
005500        05 MID-CMD-UPDATE    OCCURS 6 TIMES                               
005600                             PIC X.                                       
005700*                                 BEHANDLINGSKOD-X                        
005800        05 MID-KVBEART-Q-UPDATE                                           
005900                             OCCURS 6 TIMES                               
006000                             PIC X(7).                                    
006100*                                 ANTAL ARTNR PER BRYTBEGREPP             
006200        05 MID-PRARTNTO-UPDATE                                            
006300                             OCCURS 6 TIMES                               
006400                             PIC X(10).                                   
006500*                                 ARTIKELPRIS NETTO                       
006600        05 MID-BERADREF-UPDATE                                            
006700                             OCCURS 6 TIMES                               
006800                             PIC X(10).                                   
006900*                                 KUNDENS RADREFERENS                     
007000     03 MID-IDLOPNR-SPAR     OCCURS 6 TIMES                               
007100                             PIC 9(3).                                    
007200*                                 LÖPNUMMER                               
007300     03 MID-ADLAGOMR-SPAR    OCCURS 6 TIMES                               
007400                             PIC 9(3).                                    
007500*                                 LAGEROMRÅDE                             
007600     03 MID-ADGANG-SPAR      OCCURS 6 TIMES                               
007700                             PIC 9(3).                                    
007800*                                 GÅNG                                    
007900     03 MID-ADPLATS-SPAR     OCCURS 6 TIMES                               
008000                             PIC 9(5).                                    
008100*                                 LAGERPLATSNUMMER                        
008200     03 MID-IDARTNR-SPAR     OCCURS 6 TIMES                               
008300                             PIC 9(9).                                    
008400*                                 ARTIKELNUMMER                           
008500*** END OF VILMAII-COPY LENGTH= 427 BYTES                                 
