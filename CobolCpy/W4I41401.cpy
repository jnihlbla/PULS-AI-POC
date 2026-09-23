000100 01  MID-W4I41401.                                                        
000200*                                                                         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-INPUT.                                                        
001200*                                                                         
001300        05 MID-TABELL-A      OCCURS 8 TIMES.                              
001400*                                                                         
001500           07 MID-IDDC-BULK  PIC X(2).                                    
001600*                                 IDENTIFIERARE BULKORDERLAGER            
001700           07 MID-IDDC-DAY   PIC X(2).                                    
001800*                                 IDENTIFIERARE DAGORDERLAGER             
001900           07 MID-IDDC-VOR   PIC X(2).                                    
002000*                                 IDENTIFIERARE VORORDERLAGER             
002100        05 MID-TABELL-B      OCCURS 7 TIMES.                              
002200*                                                                         
002300           07 MID-IDDC-BULK-B                                             
002400                             PIC X(2).                                    
002500*                                 IDENTIFIERARE BULKORDERLAGER            
002600           07 MID-IDDC-DAY-B PIC X(2).                                    
002700*                                 IDENTIFIERARE DAGORDERLAGER             
002800           07 MID-IDDC-VOR-B PIC X(2).                                    
002900*                                 IDENTIFIERARE VORORDERLAGER             
003000        05 MID-TABELL-DDGS   OCCURS 7 TIMES.                              
003100*                                                                         
003200           07 MID-IDDC-DDGS  PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 MID-KVDAGAR-DOW   PIC 9(2).                                    
003500*                                 ANTAL DAGAR FÖRE DC CLEARING            
003600        05 MID-IDDC-DAY-ALT  PIC X(2).                                    
003700*                                 DAG DC BARA UNDER GIVNA TIDER           
003800        05 MID-TIHHMM-START  PIC 9(4).                                    
003900*                                 KLOCKSLAG (TIMMAR/MIN.) START           
004000        05 MID-TIHHMM-STOP   PIC 9(4).                                    
004100*                                 KLOCKSLAG (TIMMAR/MIN.) STOP            
004200        05 MID-TISTADAT      PIC 9(6).                                    
004300*                                 GENERELLT STARTDATUM                    
004400        05 MID-TISTODAT      PIC 9(6).                                    
004500*                                 GENERELLT STOPPDATUM                    
004600*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
