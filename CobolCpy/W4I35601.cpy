000100 01  MID-W4I35601.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W4I35601                                
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDDISTR-UT       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MID-IDORDNR7-IN      PIC X(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 MID-IDORDNR7-UT      PIC X(7).                                    
001500*                                 ORDERNUMMER                             
001600     03 MID-ADLAGOMR-IN      PIC X(2).                                    
001700*                                 LAGEROMRÅDE                             
001800     03 MID-ADLAGOMR-UT      PIC X(2).                                    
001900*                                 LAGEROMRÅDE                             
002000     03 MID-IDPRODNR-IN      PIC X(7).                                    
002100*                                 PRODUKTIONSNUMMER                       
002200     03 MID-IDPRODNR-UT      PIC X(7).                                    
002300*                                 PRODUKTIONSNUMMER                       
002400     03 MID-FLEKOD-IN        PIC X.                                       
002500*                                 JA/NEJ-FLAGGA                           
002600     03 MID-FLEKOD-UT        PIC X.                                       
002700*                                 JA/NEJ-FLAGGA                           
002800     03 MID-IDARTNR-IN       PIC X(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 MID-IDARTNR-UT       PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 MID-IDDC-IN          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 MID-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MID-IDARTNR-ENTER    PIC X(9).                                    
003700*                                 ARTIKELNUMMER                           
003800     03 MID-IDLOPNR-ENTER    PIC 9(3).                                    
003900*                                 LÖPNUMMER                               
004000     03 MID-ADLAGOMR-ENTER   PIC X(2).                                    
004100*                                 LAGEROMRÅDE                             
004200     03 MID-IDARTNR-NEXT     PIC X(9).                                    
004300*                                 ARTIKELNUMMER                           
004400     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
004500*                                 LÖPNUMMER                               
004600     03 MID-ADLAGOMR-NEXT    PIC X(2).                                    
004700*                                 LAGEROMRÅDE                             
004800     03 MID-IDORDER-SPAR     PIC X(7).                                    
004900*                                 VOLVO PARTS ORDERNUMMER                 
005000     03 MID-RAD-KOLL         OCCURS 14 TIMES.                             
005100*                                 001-GRP FÖR MID                         
005200*                                 W4I35601                                
005300*                                                                         
005400        05 MID-IDSPECEMB-RAD PIC X(4).                                    
005500*                                 SPECIALEMBALLAGEID                      
005600        05 MID-IDSPECEMB-NY  PIC X(4).                                    
005700*                                 SPECIALEMBALLAGEID                      
005800        05 MID-IDARTNR-RAD   PIC X(9).                                    
005900*                                 ARTIKELNUMMER                           
006000        05 MID-IDLOPNR-RAD   PIC X(3).                                    
006100*                                 LÖPNUMMER                               
006200        05 MID-ADLAGOMR-RAD  PIC X(2).                                    
006300*                                 LAGEROMRÅDE                             
006400        05 MID-ADGANG-RAD    PIC X(2).                                    
006500*                                 GÅNG                                    
006600     03 MID-ADPLATS-Q401     OCCURS 14 TIMES.                             
006700*                                 002-GRP FÖR MID                         
006800*                                 W4I35601                                
006900*                                                                         
007000        05 MID-ADPLATS-Q4    PIC X(5).                                    
007100*                                 LAGERPLATSNUMMER                        
