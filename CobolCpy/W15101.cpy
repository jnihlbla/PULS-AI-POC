000100 01  FOTNOT-W15101.                                                       
000200*                                 FOTNOTSNUMMER MED FOTNOTTEXT            
000300*                                 PÅ VALT SPRÅK                           
000400     03 FOTNOT-IDFOTNR       PIC S9(5)           COMP-3.                  
000500*                                 FOTNOTSNUMMER                           
000600     03 FOTNOT-KDFORDON      PIC X(2).                                    
000700*                                 FORDONSSLAG                             
000800     03 FOTNOT-FLOVERSATT    PIC X.                                       
000900*                                 KAN TEXT ÖVERSÄTTAS, I KATALOG          
001000*                                 ÄR  TEXT ÖVERSATT,   I BENREG.          
001100     03 FOTNOT-IDSKYLT       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300     03 FOTNOT-BEFOTNOT-RAD  OCCURS 6 TIMES.                              
001400        05 FOTNOT-IDSEGMNR   PIC S9              COMP-3.                  
001500*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
001600        05 FOTNOT-BEFOTNOT   PIC X(55).                                   
001700*                                 FOTNOTSTEXT                             
001800     03 FOTNOT-KDPRTVAL      PIC X.                                       
001900*                                 PRINTER-VAL KOD                         
002000*** END COPY W15101CCC0  LENGTH=346                                       
