000100 01  MID-W3I21101.                                                        
000200*                                 COPYTEXT FÖR MID W3I21101               
000300*                                                                         
000400     03 MID-IDARTNR-SAMMA    PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDDISTR-SAMMA    PIC S9(4).                                   
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-TIFSGVV-SAMMA    PIC 9(4).                                    
000900*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
001000     03 MID-IDARTNR-NAESTA   PIC 9(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MID-IDDISTR-NAESTA   PIC S9(4).                                   
001300*                                 DISTRIKTNUMMER                          
001400     03 MID-TIFSGVV-NAESTA   PIC 9(4).                                    
001500*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
001600     03 MID-KDSVAR           PIC X.                                       
001700*                                 SVARSKOD FRÅN SUBPROGRAM                
001800     03 MID-IDARTNR          PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MID-IDDISTR          PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MID-TIFSGVV          PIC X(4).                                    
002300*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002400     03 MID-PRARTNTO         PIC X(10).                                   
002500*                                 ARTIKELPRIS NETTO                       
002600     03 MID-KVLEVART         PIC X(7).                                    
002700*                                 LEVERERAT ANTAL ARTIKLAR                
002800     03 MID-PRARTSJK         PIC X(10).                                   
002900*                                 ARTIKELNS SJÄLVKOSTNAD                  
003000*** END COPY W3I21101C0  LENGTH=79                                        
