000100 01  MID-W2I31201-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W2031200               
000300     03 MID-IDKAMPRF-IN      PIC X(7).                                    
000400*                                 KAMPANJREFERENS                         
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-KVKVARFD         PIC 9(7).                                    
001000*                                 KVAR ATT FÖRDELA TILL KUND              
001100     03 MID-W2I31201-001-GRP OCCURS 12 TIMES.                             
001200*                                 INDATA FÖR UPPDATERING                  
001300        05 MID-CMD-UPD       PIC X.                                       
001400        05 MID-IDDISTR-FOM   PIC 9(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600        05 MID-IDDISTR-TOM   PIC 9(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800        05 MID-IDKUNDNR-FOM  PIC 9(6).                                    
001900*                                 KUNDNUMMER                              
002000        05 MID-IDKUNDNR-TOM  PIC 9(6).                                    
002100*                                 KUNDNUMMER                              
002200        05 MID-KVBEART-KAMP  PIC 9(6).                                    
002300*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
002400        05 MID-KVBEART-KUND  PIC 9(6).                                    
002500*                                 AV KUND BESTÄLLT KVANTITET              
002600        05 MID-KVBEART-REM   PIC 9(6).                                    
002700*                                 AV KUND BESTÄLLT KVANTITET              
002800     03 MID-IDDISTR-FOM-NEW  PIC 9(4).                                    
002900*                                 DISTRIKTNUMMER                          
003000     03 MID-IDDISTR-TOM-NEW  PIC 9(4).                                    
003100*                                 DISTRIKTNUMMER                          
003200     03 MID-IDKUNDNR-FOM-NEW PIC 9(6).                                    
003300*                                 KUNDNUMMER                              
003400     03 MID-IDKUNDNR-TOM-NEW PIC 9(6).                                    
003500*                                 KUNDNUMMER                              
003600     03 MID-KVBEART-KAMP-NEW PIC 9(6).                                    
003700*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
003800     03 MID-IDKAMPRF-COPY    PIC X(7).                                    
003900*                                 KAMPANJREFERENS                         
004000     03 MID-IDDC-COPY        PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200     03 MID-IDARTNR-COPY     PIC X(9).                                    
004300*                                 ARTIKELNUMMER                           
004400*** END OF VILMAII-COPY LENGTH= 537 BYTES                                 
