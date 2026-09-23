000100 01  JUST-WDK626.                                                         
000200*                                 TREND SÄSONG JUSTERING                  
000300*                                 SÖKBEGREPP TIPBJUST                     
000400     03 JUST-CENTR-PBJUST.                                                
000500        05 JUST-REPBJUST     PIC S9V9(2)         COMP-3.                  
000600*                                 JUSTERINGSFAKTOR-PB                     
000700        05 JUST-TIPBJUST-CENTR                                            
000800                             PIC S9(5)           COMP-3.                  
000900*                                 DATUM CENTRAL PB-JUSTERING ÅÅVV         
001000     03 JUST-PBJUST          OCCURS 2 TIMES.                              
001100        05 JUST-KVPB-JUST    PIC S9(6)V9(1)      COMP-3.                  
001200*                                 PERIODBEHOVSJUSTERING                   
001300        05 JUST-TIPBJUST     PIC S9(5)           COMP-3.                  
001400*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
001500     03 JUST-DAMANSEA        PIC 9(8).                                    
001600*                                 DATUM MANUELL SÄSONG (AAAAMMDD)         
001700*                                 DATE MANUAL SEASON (YYYYMMDD)           
001800     03 JUST-DASPSEA         PIC 9(8).                                    
001900*                                 DATUM MANUELL SÄSONG ÅÅÅÅMMDD           
002000*                                 DATE FOR MANUAL SEASON YYYYMMDD         
002100     03 JUST-RESEASON        OCCURS 12 TIMES                              
002200                             PIC S9V9(2)         COMP-3.                  
002300*                                 SÄSONGSINDEX                            
002400*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
