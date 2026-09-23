000100 01  PROG-WDK727.                                                         
000200*                                 FRAMTIDA PROGNOSER XDC                  
000300*                                 FYSISK NYCKEL KDSEGKEY                  
000400*                                  ALLTID = "1"                           
000500     03 PROG-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 PROG-KVPB-JUST       OCCURS 2 TIMES                               
000900                             PIC S9(6)V9(1)      COMP-3.                  
001000*                                 PERIODBEHOVSJUSTERING                   
001100     03 PROG-TIPBJUST        OCCURS 2 TIMES                               
001200                             PIC S9(5)           COMP-3.                  
001300*                                 DATUM F÷R PB-JUSTERING (≈≈VV)           
001400*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
