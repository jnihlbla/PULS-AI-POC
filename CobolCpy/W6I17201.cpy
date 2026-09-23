000100 01  MID-W6I17201.                                                        
000200*                                 MID-COPYTEXT FÖR W6017200               
000300     03 MID-IDARTNR-IN       PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC 9(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-TABINDX-ENTER    PIC 9(9).                                    
000800     03 MID-TABINDX-NEXT     PIC 9(9).                                    
000900     03 MID-INPUT            OCCURS 12 TIMES.                             
001000*                                 INDATA FÖR UPPDATERING                  
001100        05 MID-KDCMDVAL      PIC X(3).                                    
001200*                                 GENERELL KOMMANDOKOD                    
001300        05 MID-ADBUFFOMR     PIC 9(2).                                    
001400*                                 BUFFERTOMRÅDE                           
001500        05 MID-ADBUFFGANG    PIC 9(2).                                    
001600*                                 BUFFERT GÅNG                            
001700        05 MID-ADBUFFPL      PIC 9(5).                                    
001800*                                 BUFFERPLATSNUMMER                       
001900        05 MID-ADBUFPPL      PIC 9(2).                                    
002000*                                 PALLPLATSNUMMER I BUFFERT               
002100        05 MID-KVBUFF-F      PIC 9(7).                                    
002200*                                 FÖRÄDLAT BUFFERSALDO                    
002300*** END COPY W6I17201    LENGTH=288                                       
