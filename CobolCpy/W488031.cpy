000100 01  W488031.                                                             
000200*                                 RECORD FRÅN W48832-PGM:ET (FSU)         
000300*                                 SAMT FRÅN W48830-PGM:ET (PDP)           
000400*                                 HÖGLAGERARTIKLAR, SALDOBAS              
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700*                                 RECORD TYPE                             
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 ADBUFFOMR            PIC S9(3)           COMP-3.                  
001500*                                 BUFFERTOMRÅDE                           
001600*                                 BUFFER AREA                             
001700     03 KVBUFF-F             PIC S9(7)           COMP-3.                  
001800*                                 FÖRÄDLAT BUFFERSALDO                    
001900*                                 BUFFER QUANTANTITY PREPARED             
002000     03 KVBUFF-OF            PIC S9(7)           COMP-3.                  
002100*                                 BUFFERSALDO OFÖRÄDLAT GODS              
002200*                                 BUFFER BALANCE UNPREPARED GOODS         
002300     03 KVKOLLI-F            PIC S9(5)           COMP-3.                  
002400*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
002500*                                 Q PREPARED CASES IN BUFFER              
002600     03 KVKOLLI-OF           PIC S9(5)           COMP-3.                  
002700*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
002800*                                 Q UNPREPARED CASES IN BUFFER            
