000100 01  JUST-W01166X.                                                        
000200*                                 A COPY OF W01166 - TO CREATE WX         
000300*                                 TR FILE IN EDITABLE FORMAT              
000400     03 JUST-IDARTNR         PIC Z(7)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 JUST-WDK626.                                                      
000900*                                 TREND SÄSONG JUSTERING                  
001000*                                 SÖKBEGREPP TIPBJUST                     
001100        05 JUST-CENTR-PBJUST.                                             
001200           07 JUST-REPBJUST  PIC 9.9(2)                                   
001300                             VALUE ZEROS.                                 
001400*                                 JUSTERINGSFAKTOR-PB                     
001500           07 JUST-TIPBJUST-CENTR                                         
001600                             PIC 9(4)                                     
001700                             VALUE ZEROS.                                 
001800*                                 DATUM CENTRAL PB-JUSTERING ÅÅVV         
001900        05 JUST-PBJUST       OCCURS 2 TIMES.                              
002000           07 JUST-KVPB-JUST PIC Z(5)9.9                                  
002100                             VALUE ZEROS.                                 
002200*                                 PERIODBEHOVSJUSTERING                   
002300           07 JUST-TIPBJUST  PIC 9(4)                                     
002400                             VALUE ZEROS.                                 
002500*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
002600        05 JUST-DAMANSEA     PIC 9(8)                                     
002700                             VALUE ZEROS.                                 
002800*                                 DATUM MANUELL SÄSONG (AAAAMMDD)         
002900*                                 DATE MANUAL SEASON (YYYYMMDD)           
003000        05 JUST-DASPSEA      PIC 9(8)                                     
003100                             VALUE ZEROS.                                 
003200*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
003300*                                 DATE SEASON BLOCKED TO YYYYMMDD         
003400        05 JUST-RESEASON     OCCURS 12 TIMES                              
003500                             PIC 9.9(2)                                   
003600                             VALUE ZEROS.                                 
003700*                                 SÄSONGSINDEX                            
003800*** END OF VILMAII-COPY LENGTH= 104 BYTES                                 
