000100 01  MID-W6I32301.                                                        
000200*                                                                         
000300     03 MID-KDARBTYP-IN      PIC X(8).                                    
000400*                                 TYP AV ARBETE                           
000500     03 MID-KDARBTYP-UT      PIC X(8).                                    
000600*                                 TYP AV ARBETE                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDARTNR-IN       PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-IDARTNR-UT       PIC 9(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MID-FLKLAR           PIC X.                                       
001600*                                 AVSLUTNINGSMARKERING                    
001700     03 MID-INPUT            OCCURS 12 TIMES.                             
001800*                                                                         
001900        05 MID-CMD           PIC X.                                       
002000        05 MID-IDARTNR       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 MID-SUBEL         PIC 9(7).                                    
002300*                                 SUMMABELOPP                             
002400        05 MID-IDDC          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600        05 MID-TIDATUM       PIC X(6).                                    
002700*                                 DATUM ENLIGT KDDATFORM                  
002800*** END OF VILMAII-COPY LENGTH= 339 BYTES                                 
