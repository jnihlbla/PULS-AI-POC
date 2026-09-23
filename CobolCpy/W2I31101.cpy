000100 01  MID-W2I31101-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W2031100               
000300     03 MID-IDKAMPRF-IN      PIC X(7).                                    
000400*                                 KAMPANJREFERENS                         
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-W2I31101-001-GRP.                                             
001000*                                 INDATA FÖR UPPDATERING                  
001100        05 MID-TISTADAT-IN   PIC 9(6).                                    
001200*                                 GENERELLT STARTDATUM                    
001300        05 MID-TISTODAT-IN   PIC 9(6).                                    
001400*                                 GENERELLT STOPPDATUM                    
001500        05 MID-CMD-RAD1      PIC X.                                       
001600*                                 RAD-UPPDATERINGSKOMMANDO                
001700*                                  BLANK  = INGENTING                     
001800*                                  D , B  = DELETE                        
001900*                                  R , Ä  = REPLACE                       
002000*                                  I,N,A  = INSERT                        
002100*                                  S , V  = SELECT                        
002200*                                  P , P  = PRINT                         
002300*                                  C , K  = COPY                          
002400        05 MID-IDARTNR-RAD1  PIC 9(8).                                    
002500*                                 ARTIKELNUMMER                           
002600        05 MID-W2I31101-002-GRP                                           
002700                             OCCURS 9 TIMES.                              
002800*                                 INDATA FÖR UPPDATERING                  
002900           07 MID-CMD-RAD    PIC X.                                       
003000*                                 RAD-UPPDATERINGSKOMMANDO                
003100*                                  BLANK  = INGENTING                     
003200*                                  D , B  = DELETE                        
003300*                                  R , Ä  = REPLACE                       
003400*                                  I,N,A  = INSERT                        
003500*                                  S , V  = SELECT                        
003600*                                  P , P  = PRINT                         
003700*                                  C , K  = COPY                          
003800           07 MID-IDARTNR-RAD                                             
003900                             PIC 9(8).                                    
004000*                                 ARTIKELNUMMER                           
004100        05 MID-IDARTNR       PIC X(9).                                    
004200*                                 ARTIKELNUMMER                           
004300        05 MID-KVBEART-KAMP  PIC 9(6).                                    
004400*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
004500        05 MID-TIRES         PIC 9(6).                                    
004600*                                 RESERVATIONSDATUM                       
004700*** END OF VILMAII-COPY LENGTH= 141 BYTES                                 
