000100 01  W221L471.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT TPO-ORDER                    
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-DIVERSEORDER    VALUE +471.                                  
000700*                                 ANROPSTYP       KDCALL-W221-002         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                 SUMMERAD INFO OM TPO-ORDER              
001600*                                 PER VECKA                               
001700*                                                                         
001800        05 KVOKS-BULK        OCCURS 2 TIMES                               
001900                             PIC S9(7)           COMP-3.                  
002000*                                 ORDERKÖSALDO, KLASS 2-4                 
002100        05 KVOKS-DAG         OCCURS 2 TIMES                               
002200                             PIC S9(7)           COMP-3.                  
002300*                                 ORDERKÖSALDO, KLASS 1                   
002400        05 KVOKS-VOR         OCCURS 2 TIMES                               
002500                             PIC S9(7)           COMP-3.                  
002600*                                 ORDERKÖSALDO, VOR                       
002700        05 SUTPO-TOT         OCCURS 2 TIMES                               
002800                             PIC S9(7)           COMP-3.                  
002900*                                 TPO-KVANTITET, TOTAL                    
003000        05 TIBEHOV           OCCURS 7 TIMES                               
003100                             PIC S9(5)           COMP-3.                  
003200*                                 BEHOVSVECKA           (ÅÅVV)            
003300        05 SUTPO-TOT-VECKA   OCCURS 7 TIMES                               
003400                             PIC S9(7)           COMP-3.                  
003500*                                 TPO-KVANTITET, TOTAL                    
003600*** END COPY W221L471C0  LENGTH=89                                        
