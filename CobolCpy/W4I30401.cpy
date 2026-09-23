000100 01  MID-W4I30401.                                                        
000200*                                 MID-COPYTEXT FÖR R30401                 
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDARTNR          PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-BEART            PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100     03 MID-INPUT.                                                        
001200*                                 INDATA FÖR UPPDATERING                  
001300        05 MID-IDPURAD1      OCCURS 11 TIMES                              
001400                             PIC X(4).                                    
001500*                                 RADNUMMER PÅ PACKUNDERLAG               
001600        05 MID-REBEART1      OCCURS 11 TIMES                              
001700                             PIC X(7).                                    
001800*                                 ANTAL PER ORDERRAD SATS                 
001900        05 MID-IDPURAD2      OCCURS 11 TIMES                              
002000                             PIC X(4).                                    
002100*                                 RADNUMMER PÅ PACKUNDERLAG               
002200        05 MID-REBEART2      OCCURS 11 TIMES                              
002300                             PIC X(7).                                    
002400*                                 ANTAL PER ORDERRAD SATS                 
002500        05 MID-IDPURAD3      OCCURS 11 TIMES                              
002600                             PIC X(4).                                    
002700*                                 RADNUMMER PÅ PACKUNDERLAG               
002800        05 MID-REBEART3      OCCURS 11 TIMES                              
002900                             PIC X(7).                                    
003000*                                 ANTAL PER ORDERRAD SATS                 
003100*** END OF VILMAII-COPY LENGTH= 411 BYTES                                 
