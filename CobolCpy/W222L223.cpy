000100 01  W222L223.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22222 MOT ARTIKELREG OCH               
000400*                                 ART.REG ORDER ENTRY                     
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAES-ARTIKEL        VALUE +301.                                  
000800      88 LAES-PROGNOS-BEHOV  VALUE +302.                                  
000900      88 LAES-TPO-SALDO      VALUE +303.                                  
001000      88 LAES-ART-GEMINFO    VALUE +304.                                  
001100*                                                                         
001200     03 FLJANEJ-ANROP        PIC X.                                       
001300      88 ANROP-OK            VALUE 'J'.                                   
001400      88 ANROP-FEL           VALUE 'N'.                                   
001500*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 IOAREA.                                                           
001900*                                                                         
002000        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002100*                                 PRODUKTSLAG                             
002200        05 TIFINLV           PIC S9(5)           COMP-3.                  
002300*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002400        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
002500*                                 SEPARAT PERIODBEHOV                     
002600        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
002700*                                 ANTAL I Q0 FÖRPACKNING                  
002800        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
002900*                                 ANTAL I Q1 FÖRPACKNING                  
003000        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
003100*                                 ANTAL I Q2 FÖRPACKNING                  
003200        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
003300*                                 ANTAL I Q3 FÖRPACKNING                  
003400        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
003500*                                 ANTAL I Q4 FÖRPACKNING                  
003600        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
003900*                                 DIREKTLEVERANSANDEL                     
004000        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
004100*                                 TPO-KVANTITET, TOTAL                    
004200        05 FLJANEJ-JUST-PB   PIC X.                                       
004300         88 JUST-PB-FINNS    VALUE 'J'.                                   
004400         88 JUST-PB-SAKNAS   VALUE 'N'.                                   
004500*                                 JA/NEJ-FLAGGA FÖR R2XX                  
004600        05 CENTR-PBJUST.                                                  
004700*                                                                         
004800           07 REPBJUST       PIC S9V9(2)         COMP-3.                  
004900*                                 JUSTERINGSFAKTOR-PB                     
005000           07 TIPBJUST-CENTR PIC S9(5)           COMP-3.                  
005100*                                 DATUM CENTRAL PB-JUSTERING ÅÅVV         
005200        05 PBJUST            OCCURS 2 TIMES.                              
005300*                                                                         
005400           07 KVPB-JUST      PIC S9(6)V9(1)      COMP-3.                  
005500*                                 PERIODBEHOVSJUSTERING                   
005600           07 TIPBJUST       PIC S9(5)           COMP-3.                  
005700*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
005800        05 RESEASON          OCCURS 8 TIMES                               
005900                             PIC S9V9(2)         COMP-3.                  
006000*                                 SÄSONGSINDEX                            
006100        05 TREND.                                                         
006200*                                                                         
006300           07 KVTREND        PIC S9(6)V9(1)      COMP-3.                  
006400*                                 TRENDANTAL                              
006500           07 RVTREND        PIC S9(3)           COMP-3.                  
006600*                                 ÅTERSTÅENDE TRENDPERIODER               
006700           07 TITREND        PIC S9(3)           COMP-3.                  
006800*                                 STARTPERIOD TREND                       
006900        05 KDGK              PIC S9              COMP-3.                  
007000*                                 GODSMOTTAGAREKOD                        
007100        05 PB-PLAN.                                                       
007200*                                                                         
007300           07 DAPBPLAN       PIC 9(8).                                    
007400*                                 DATUM KVPB-PLAN GILTIG TOM              
007500           07 DASEASON       PIC 9(8).                                    
007600*                                 DATUM RESEASON-LEDTID GILTIG TO         
007700*                                 M                                       
007800           07 KVPB-PLAN      PIC S9(6)V9(1)      COMP-3.                  
007900*                                 PLANERAT PERIODBEHOV                    
008000           07 RESEASON-PLAN  OCCURS 8 TIMES                               
008100                             PIC S9V9(2)         COMP-3.                  
008200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
008300*** END OF VILMAII-COPY LENGTH= 124 BYTES                                 
