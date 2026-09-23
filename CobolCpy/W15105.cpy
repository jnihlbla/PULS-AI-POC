000100 01  RUB-W15105.                                                          
000200*                                 RUBRIKNUMMER MED RUBRIKTEXTER           
000300*                                 PÅ VALDA SPRÅK                          
000400     03 RUB-IDRUBNR          PIC S9(5)           COMP-3.                  
000500*                                 RUBRIKNUMMER                            
000600     03 RUB-FLKOMBINERAS     PIC X.                                       
000700*                                 FÅR KOMBINERAS                          
000800     03 RUB-IDSKYLT-1        PIC X(3).                                    
000900*                                 NATIONALITETSTECKEN                     
001000     03 RUB-BERUBTXT-RAD-1   OCCURS 3 TIMES.                              
001100        05 RUB-IDSEGMNR-1    PIC S9              COMP-3.                  
001200*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
001300        05 RUB-BERUBTXT-1    PIC X(30).                                   
001400*                                 RUBRIKTEXT                              
001500     03 RUB-IDSKYLT-2        PIC X(3).                                    
001600*                                 NATIONALITETSTECKEN                     
001700     03 RUB-BERUBTXT-RAD-2   OCCURS 3 TIMES.                              
001800        05 RUB-IDSEGMNR-2    PIC S9              COMP-3.                  
001900*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
002000        05 RUB-BERUBTXT-2    PIC X(30).                                   
002100*                                 RUBRIKTEXT                              
002200     03 RUB-KDPRTVAL         PIC X.                                       
002300*                                 PRINTER-VAL KOD                         
002400*** END COPY W15105CCC0  LENGTH=197                                       
