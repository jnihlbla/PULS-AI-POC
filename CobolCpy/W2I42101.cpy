000100 01  MID-W2I42101.                                                        
000200*                                 MID-COPYTEXT FÖR W2042100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT.                                                        
001200        05 MID-IDAVTAL-IN    PIC 9(12).                                   
001300*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001400*                                 PPP   = INKÖPARNR (PREFIX)              
001500*                                 BBBBB = BESTÄLLARNR                     
001600*                                 SSS   = SUFFIX                          
001700        05 MID-IDLEVNR-AVT-IN                                             
001800                             PIC X(5).                                    
001900*                                 LEVERANTÖR ENLIGT AVTAL                 
002000        05 MID-IDLEVNR-SHIP-IN                                            
002100                             PIC X(5).                                    
002200*                                 SKEPPANDE LEVERANTÖR                    
002300        05 MID-TIAVTAL-IN    PIC 9(6).                                    
002400*                                 AVTALSDATUM  (ÅÅMMDD)                   
002500        05 MID-KDCMD-NC-IN   PIC X.                                       
002600        05 MID-KDCMD-C-IN    PIC X.                                       
002700*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
