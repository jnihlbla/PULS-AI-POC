000100 01  AREA.                                                                
000200*                                 FIL COPYTEXT                            
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 ADBUFFOMR            PIC S9(3)           COMP-3.                  
001000*                                 BUFFERTOMRÅDE                           
001100*                                 BUFFER AREA                             
001200     03 ADBUFFGANG           PIC S9(3)           COMP-3.                  
001300*                                 BUFFERT GÅNG                            
001400     03 ADBUFFPL             PIC S9(5)           COMP-3.                  
001500*                                 BUFFERPLATSNUMMER                       
001600*                                 LOCATION IN BUFFER                      
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
002900*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
