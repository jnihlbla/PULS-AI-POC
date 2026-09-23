000100 01  SALDO-WDD811.                                                        
000200*                                 SALDOREGISTER BUFFERTLAGER              
000300*                                 ADRESS OCH SALDO SEGMENT                
000400*                                 FYSISK NYCKEL: WDD811KY                 
000500*                                  (IDDC     + ADBUFFOMR +                
000600*                                   DABUFPAF + ADBUFFGANG                 
000700*                                   ADBUFFPL)                             
000800     03 SALDO-IDDC           PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 SALDO-ADBUFFOMR      PIC S9(3)           COMP-3.                  
001200*                                 BUFFERTOMRÅDE                           
001300*                                 BUFFER AREA                             
001400     03 SALDO-DABUFPAF       PIC 9(8).                                    
001500*                                 BUFFERT PÅFYLLNINGS DATUM               
001600*                                 BUFFERT REFILLING DATE                  
001700     03 SALDO-ADBUFFGANG     PIC S9(3)           COMP-3.                  
001800*                                 BUFFERT GÅNG                            
001900     03 SALDO-ADBUFFPL       PIC S9(5)           COMP-3.                  
002000*                                 BUFFERPLATSNUMMER                       
002100*                                 LOCATION IN BUFFER                      
002200     03 SALDO-KDBRIST        PIC S9              COMP-3.                  
002300*                                 BRIST KOD                               
002400*                                 SHORTNESS CODE                          
002500     03 SALDO-KDPAF          PIC S9              COMP-3.                  
002600*                                 PÅFYLLNADSKOD                           
002700*                                 PAFF CODE                               
002800     03 SALDO-KVBUFF-F       PIC S9(7)           COMP-3.                  
002900*                                 FÖRÄDLAT BUFFERSALDO                    
003000*                                 BUFFER QUANTANTITY PREPARED             
003100     03 SALDO-KVBUFF-OF      PIC S9(7)           COMP-3.                  
003200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
003300*                                 BUFFER BALANCE UNPREPARED GOODS         
003400     03 SALDO-KVKOLLI-F      PIC S9(5)           COMP-3.                  
003500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
003600*                                 Q PREPARED CASES IN BUFFER              
003700     03 SALDO-KVKOLLI-OF     PIC S9(5)           COMP-3.                  
003800*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
003900*                                 Q UNPREPARED CASES IN BUFFER            
004000*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
