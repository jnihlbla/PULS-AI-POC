000100 01  MID-W2I30301.                                                        
000200*                                 MID-COPYTEXT FÖR W20303                 
000300     03 MID-IDANSK-FOM-IN    PIC X(3).                                    
000400*                                 ANSKAFFARNUMMER                         
000500     03 MID-IDANSK-FOM-UT    PIC X(3).                                    
000600*                                 ANSKAFFARNUMMER                         
000700     03 MID-IDANSK-TOM-IN    PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-IDANSK-TOM-UT    PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MID-FLBYGGB-IN       PIC X.                                       
001200*                                 FLAGGA BYGGBAR SATSORDER                
001300     03 MID-FLBYGGB-UT       PIC X.                                       
001400*                                 FLAGGA BYGGBAR SATSORDER                
001500     03 MID-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-IDORDNSB-IN      PIC X(4).                                    
002000*                                 SATSORDERNUMMER-BAS                     
002100     03 MID-IDORDNSS-IN      PIC X.                                       
002200*                                 SATSORDERNUMMER-SUFFIX                  
002300     03 MID-IDORDNSB-UT      PIC X(4).                                    
002400*                                 SATSORDERNUMMER-BAS                     
002500     03 MID-IDORDNSS-UT      PIC X.                                       
002600*                                 SATSORDERNUMMER-SUFFIX                  
002700     03 MID-ING-IDARTNR-IN   PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MID-ING-IDARTNR-UT   PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MID-KDSATKMB-IN      PIC X.                                       
003200*                                 KOMBINATIONSKOD SATS                    
003300     03 MID-KDSATKMB-UT      PIC X.                                       
003400*                                 KOMBINATIONSKOD SATS                    
003500     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
003800*                                 ARTIKELNUMMER                           
003900     03 MID-ING-IDARTNR-RAD  OCCURS 8 TIMES                               
004000                             PIC X(11).                                   
004100*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
004200     03 MID-INPUT.                                                        
004300*                                 INDATA FÖR UPPDATERING                  
004400        05 MID-ING-IDARTNR1-UPDATE                                        
004500                             PIC X(9).                                    
004600*                                 ARTIKELNUMMER                           
004700        05 MID-REANTPSA1-UPDATE                                           
004800                             PIC X(6).                                    
004900*                                 ANTAL PER SATS                          
005000        05 MID-REBEART1-UPDATE                                            
005100                             PIC X(7).                                    
005200*                                 ANTAL PER ORDERRAD SATS                 
005300        05 MID-KDSATAND1-UPDATE                                           
005400                             PIC X.                                       
005500*                                 ÄNDRINGSKOD SATS                        
005600        05 MID-KDSATKMB-UPDATE                                            
005700                             PIC X.                                       
005800*                                 KOMBINATIONSKOD SATS                    
005900        05 MID-ING-IDARTNR2-UPDATE                                        
006000                             PIC X(9).                                    
006100*                                 ARTIKELNUMMER                           
006200        05 MID-REANTPSA2-UPDATE                                           
006300                             PIC X(6).                                    
006400*                                 ANTAL PER SATS                          
006500        05 MID-REBEART2-UPDATE                                            
006600                             PIC X(7).                                    
006700*                                 ANTAL PER ORDERRAD SATS                 
006800        05 MID-KDSATAND2-UPDATE                                           
006900                             PIC X.                                       
007000*                                 ÄNDRINGSKOD SATS                        
007100*** END COPY W2I30301C0  LENGTH=215                                       
