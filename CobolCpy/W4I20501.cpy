000100 01  MID-W4I20501.                                                        
000200*                                 MID-COPYTEXT FÖR W40205                 
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDTRP-IN.                                                     
000800*                                 TRANSPORTIDENTITET                      
000900        05 MID-IDTRPLOS      PIC X(3).                                    
001000*                                 TRANSPORTLÖSNING                        
001100        05 MID-IDTRPVAR      PIC X(2).                                    
001200*                                 TRANSPORTLÖSNINGSGRUPP                  
001300     03 MID-IDTRP-UT.                                                     
001400*                                 TRANSPORTIDENTITET                      
001500        05 MID-IDTRPLOS      PIC X(3).                                    
001600*                                 TRANSPORTLÖSNING                        
001700        05 MID-IDTRPVAR      PIC X(2).                                    
001800*                                 TRANSPORTLÖSNINGSGRUPP                  
001900     03 MID-TITRPAVG-IN      PIC X(7).                                    
002000*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
002100     03 MID-TITRPAVG-UT      PIC X(7).                                    
002200*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
002300     03 MID-IDDISTR-IN       PIC X(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 MID-IDDISTR-UT       PIC X(4).                                    
002600*                                 DISTRIKTNUMMER                          
002700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
002800*                                 KUNDNUMMER                              
002900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
003000*                                 KUNDNUMMER                              
003100     03 MID-KDFRAKT-IN       PIC X(2).                                    
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300     03 MID-KDFRAKT-UT       PIC X(2).                                    
003400*                                 FRAKTSÄTT DC TILL KUND                  
003500     03 MID-IDORDNR7-IN      PIC X(7).                                    
003600*                                 ORDERNUMMER                             
003700     03 MID-IDORDNR7-UT      PIC X(7).                                    
003800*                                 ORDERNUMMER                             
003900     03 MID-TIAAMMDD-ENTER   PIC 9(6).                                    
004000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004100     03 MID-TIHHMM-ENTER     PIC 9(4).                                    
004200*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004300     03 MID-TIAAMMDD-NEXT    PIC 9(6).                                    
004400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004500     03 MID-TIHHMM-NEXT      PIC 9(4).                                    
004600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004700     03 MID-IDORDER-ENTER    PIC 9(7).                                    
004800*                                 VOLVO PARTS ORDERNUMMER                 
004900     03 MID-IDORDER-NEXT     PIC 9(7).                                    
005000*                                 VOLVO PARTS ORDERNUMMER                 
005100     03 MID-IDKUNDNR-ENTER   PIC 9(7).                                    
005200*                                 KUNDNUMMER                              
005300     03 MID-IDKUNDNR-NEXT    PIC 9(7).                                    
005400*                                 KUNDNUMMER                              
005500     03 MID-IDORDNR7-ENTER   PIC 9(7).                                    
005600*                                 ORDERNUMMER                             
005700     03 MID-IDORDNR7-NEXT    PIC 9(7).                                    
005800*                                 ORDERNUMMER                             
005900     03 MID-RAD              OCCURS 11 TIMES.                             
006000*                                 INDATA FÖR UPPDATERING                  
006100        05 MID-CMD-UPDATE    PIC X.                                       
006200*                                 BEHANDLINGSKOD-X                        
006300        05 MID-IDDISTR-RAD   PIC 9(4).                                    
006400*                                 DISTRIKTNUMMER                          
006500        05 MID-IDKUNDNR-RAD  PIC 9(6).                                    
006600*                                 KUNDNUMMER                              
006700        05 MID-IDORDNR7-RAD  PIC 9(7).                                    
006800*                                 ORDERNUMMER                             
006900        05 MID-KDTRPKAT-RAD  PIC X.                                       
007000*                                 TRANSPORTKATEGORI                       
007100        05 MID-TITRPAVG-RAD  PIC 9(7).                                    
007200*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
007300     03 MID-VLORDNTO-TOT     PIC X(8).                                    
007400*                                 ORDERVOLYM NETTO (M3)                   
007500     03 MID-VKORDNTO-TOT     PIC X(8).                                    
007600*                                 ORDERVIKT NETTO (KG)                    
007700     03 MID-SUORDV-TOT       PIC X(12).                                   
007800*                                 SUMMA ORDERVÄRDE                        
007900     03 MID-TEASTRIX-TOT     PIC X.                                       
008000*                                 ASTERISK                                
008100     03 MID-INPUT.                                                        
008200*                                 INDATA FÖR UPPDATERING                  
008300        05 MID-TITRPAVG-UPDATE                                            
008400                             PIC X(7).                                    
008500*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
008600        05 MID-TIRFS-UPDATE  PIC X(7).                                    
008700*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
008800     03 MID-IDPRODNR-ENTER   PIC 9(7).                                    
008900*                                 PRODUKTIONSNUMMER                       
009000     03 MID-IDPRODNR-NEXT    PIC 9(7).                                    
009100*                                 PRODUKTIONSNUMMER                       
009200     03 MID-IDPLKLST-ENTER   PIC X(3).                                    
009300*                                 PLOCKLISTNUMMER                         
009400     03 MID-IDPLKLST-NEXT    PIC X(3).                                    
009500*                                 PLOCKLISTNUMMER                         
009600*** END OF VILMAII-COPY LENGTH= 477 BYTES                                 
