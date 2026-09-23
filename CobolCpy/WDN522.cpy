000100 01  TEXT-WDN522.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 RUBRIKTEXT-SEGMENT                      
000400*                                 SÖKBEGREPP: KDSEGKEY                    
000500     03 TEXT-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 TEXT-BERUBTEXT.                                                   
000900        05 TEXT-BERUBTEXT-1  PIC X(30).                                   
001000*                                 RUBRIKTEXT                              
001100        05 TEXT-BERUBTEXT-2  PIC X(30).                                   
001200*                                 RUBRIKTEXT                              
001300     03 TEXT-FILLER REDEFINES TEXT-BERUBTEXT.                             
001400        05 TEXT-TEKOL        PIC X(25).                                   
001500*                                 KOLUMNTEXT                              
001600*                                 COLUMN HEADER TEXT                      
001700        05 TEXT-FILLER       PIC X(35).                                   
001800     03 TEXT-FILLER REDEFINES TEXT-BERUBTEXT.                             
001900        05 TEXT-TEKATANM     PIC X(23).                                   
002000*                                 ANMÄRKNINGSTEXT                         
002100*                                 NOTES TEXT                              
002200        05 TEXT-FILLER       PIC X(37).                                   
002300*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
