000100 01  W3300003.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 ARTIKELRECORD                           
000400*                                 RESTER FRÅN DEN GAMLA                   
000500*                                 FÖRSÄLJNINGSSTATISTIKEN                 
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 KDCLAGER             PIC S9              COMP-3.                  
001100*                                 CENTRALLAGERKOD                         
001200     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001300*                                 FUNKTIONSGRUPP                          
001400     03 KDRBT                PIC S9              COMP-3.                  
001500*                                 PRODUKTTYP                              
001600     03 KDLFTF               PIC S9              COMP-3.                  
001700*                                 ARTIKELTILLHÖRIGHET                     
001800     03 TIAA                 PIC S9(3)           COMP-3.                  
001900*                                 ÅR    (ÅÅ)                              
002000     03 TIP                  PIC S9              COMP-3.                  
002100*                                 PLANERINGSPERIOD (P)                    
002200*                                 8 PER ÅR                                
002300     03 SUSTD-GRP            OCCURS 16 TIMES.                             
002400*                                 ARTIKELRECORD                           
002500        05 SUSTD             PIC S9(9)V9(2)      COMP-3.                  
002600*                                 SUMMA STANDARDPRIS                      
002700        05 SUSTD-PB          PIC S9(9)V9(2)      COMP-3.                  
002800*                                 SUMMA STANDARDPRIS                      
002900*** END COPY W3300003C0  LENGTH=209                                       
