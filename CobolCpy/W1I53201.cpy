000100 01  MID-W1I53201.                                                        
000200*                                 MID-COPYTEXT F÷R BILD 1532              
000300*                                 VADIS / KATALOG S÷KBEGREPP              
000400     03 MID-IDCATNR-IN       PIC X(5).                                    
000500*                                 KATALOG-ID IN                           
000600     03 MID-IDCATNR-UT       PIC X(5).                                    
000700*                                 KATALOG-ID UT                           
000800     03 MID-PARTNER-GRP      OCCURS 6 TIMES.                              
000900        05 MID-IDPARTGRP     PIC X(6).                                    
001000*                                 PARTNER-GRUPP                           
001100     03 MID-FLKATVAD         PIC X.                                       
001200*                                 KATALOG TILL VADIS?                     
001300     03 MID-RAD              OCCURS 8 TIMES.                              
001400        05 MID-IDMODELL      PIC X(3).                                    
001500*                                 BILENS NUMERISKA MODELLBET.             
001600        05 MID-TIMODAAR-STA  PIC X(4).                                    
001700*                                 MODELL≈R (≈≈≈≈) START≈R                 
001800        05 MID-TIMODAAR-STO  PIC X(4).                                    
001900*                                 MODELL≈R (≈≈≈≈) STOPP≈R                 
002000        05 MID-IDVARIANT     PIC X(15).                                   
002100*                                 BILVARIANT                              
002200        05 MID-IDRADNR       PIC 9(3).                                    
002300*                                 RADNUMMER                               
002400*** END OF VILMAII-COPY LENGTH= 279 BYTES                                 
