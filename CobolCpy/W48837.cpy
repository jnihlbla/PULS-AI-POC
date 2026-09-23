000100 01  W48837.                                                              
000200*                                 FELLISTPOSTER AVST. SALDOBAS            
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 KVBUFF-F-LOK         PIC S9(7)           COMP-3.                  
001300*                                 FÖRÄDLAT BUFFERSALDO                    
001400*                                 BUFFER QUANTANTITY PREPARED             
001500     03 KVBUFF-OF-LOK        PIC S9(7)           COMP-3.                  
001600*                                 BUFFERSALDO OFÖRÄDLAT GODS              
001700*                                 BUFFER BALANCE UNPREPARED GOODS         
001800     03 KVBUFF-F-CEN         PIC S9(7)           COMP-3.                  
001900*                                 FÖRÄDLAT BUFFERSALDO                    
002000*                                 BUFFER QUANTANTITY PREPARED             
002100     03 KVBUFF-OF-CEN        PIC S9(7)           COMP-3.                  
002200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
002300*                                 BUFFER BALANCE UNPREPARED GOODS         
002400     03 KVKOLLI-F-LOK        PIC S9(5)           COMP-3.                  
002500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
002600*                                 Q PREPARED CASES IN BUFFER              
002700     03 KVKOLLI-OF-LOK       PIC S9(5)           COMP-3.                  
002800*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
002900*                                 Q UNPREPARED CASES IN BUFFER            
003000     03 KVKOLLI-F-CEN        PIC S9(5)           COMP-3.                  
003100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
003200*                                 Q PREPARED CASES IN BUFFER              
003300     03 KVKOLLI-OF-CEN       PIC S9(5)           COMP-3.                  
003400*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
003500*                                 Q UNPREPARED CASES IN BUFFER            
