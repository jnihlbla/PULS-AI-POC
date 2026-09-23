000100 01  AREA.                                                                
000200*                                 LÄNKAREA 1                              
000300*                                 ANVÄNDS VID UPPDATERA SALDOBAS          
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 ADBUFFOMR            PIC S9(3)           COMP-3.                  
001100*                                 BUFFERTOMRÅDE                           
001200*                                 BUFFER AREA                             
001300     03 ADBUFFGANG           PIC S9(3)           COMP-3.                  
001400*                                 BUFFERT GÅNG                            
001500     03 ADBUFFPL             PIC S9(5)           COMP-3.                  
001600*                                 BUFFERPLATSNUMMER                       
001700*                                 LOCATION IN BUFFER                      
001800     03 KVBUFF-F             PIC S9(7)           COMP-3.                  
001900*                                 FÖRÄDLAT BUFFERSALDO                    
002000*                                 BUFFER QUANTANTITY PREPARED             
002100     03 KVBUFF-OF            PIC S9(7)           COMP-3.                  
002200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
002300*                                 BUFFER BALANCE UNPREPARED GOODS         
002400     03 KVKOLLI-F            PIC S9(5)           COMP-3.                  
002500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
002600*                                 Q PREPARED CASES IN BUFFER              
002700     03 KVKOLLI-OF           PIC S9(5)           COMP-3.                  
002800*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
002900*                                 Q UNPREPARED CASES IN BUFFER            
