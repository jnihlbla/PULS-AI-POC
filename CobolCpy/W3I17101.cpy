000100 01  MID-W3I17101.                                                        
000200*                                 MID-COPYTEXT FÖR W3017100               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDBYTRAP-IN      PIC X(7).                                    
000800*                                 RAPPORTNUMMER  BYTES                    
000900     03 MID-KDBYTSTA-IN      PIC X.                                       
001000*                                 STATUSKOD BYTESOBJEKT                   
001100     03 MID-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MID-INPUT            OCCURS 13 TIMES.                             
001400*                                 RADINFORMATION                          
001500        05 MID-KDSVAR        PIC X.                                       
001600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001700        05 MID-IDDISTR       PIC 9(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900        05 MID-IDKUNDNR      PIC 9(6).                                    
002000*                                 KUNDNUMMER                              
002100        05 MID-IDBYTRAP      PIC 9(7).                                    
002200*                                 RAPPORTNUMMER  BYTES                    
002300        05 MID-KDBYTSTA      PIC X.                                       
002400*                                 STATUSKOD BYTESOBJEKT                   
002500        05 MID-ADBYTANK      PIC X(10).                                   
002600*                                 ANKOMSTADRESS BYTESOBJEKT               
002700*** END OF VILMAII-COPY LENGTH= 397 BYTES                                 
