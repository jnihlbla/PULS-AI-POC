000100 01  W22257.                                                              
000200*                                 POST     F÷R UPPDAT AV                  
000300*                                 KVPB-JUST  P≈ WDK626                    
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 PBJUST               OCCURS 2 TIMES.                              
000700        05 KVPB-JUST         PIC S9(6)V9(1)      COMP-3.                  
000800*                                 PERIODBEHOVSJUSTERING                   
000900        05 TIPBJUST          PIC S9(5)           COMP-3.                  
001000*                                 DATUM F÷R PB-JUSTERING (≈≈VV)           
001100*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
