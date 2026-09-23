000100 01  SEQB-WDD8B1.                                                         
000200*                                 BUFFERTREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDD811             
000400*                                 DC/ARTIKEL INGÅNG                       
000500*                                 FYSISK NYCKEL: WDD8B1KY                 
000600*                                  (IDDC, IDARTNR, ADBUFFOMR,             
000700*                                  ADBUFFGANG,ADBUFFPL,DABUFPAF)          
000800*                                 SECONDARY NYCKEL: WDD8BSEQ              
000900*                                  (IDDC)                                 
001000     03 SEQB-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 SEQB-ADBUFFOMR       PIC S9(3)           COMP-3.                  
001700*                                 BUFFERTOMRÅDE                           
001800*                                 BUFFER AREA                             
001900     03 SEQB-ADBUFFGANG      PIC S9(3)           COMP-3.                  
002000*                                 BUFFERT GÅNG                            
002100     03 SEQB-ADBUFFPL        PIC S9(5)           COMP-3.                  
002200*                                 BUFFERPLATSNUMMER                       
002300*                                 LOCATION IN BUFFER                      
002400     03 SEQB-DABUFPAF        PIC 9(8).                                    
002500*                                 BUFFERT PÅFYLLNINGS DATUM               
002600*                                 BUFFERT REFILLING DATE                  
002700     03 SEQB-KVBUFF-F        PIC S9(7)           COMP-3.                  
002800*                                 FÖRÄDLAT BUFFERSALDO                    
002900*                                 BUFFER QUANTANTITY PREPARED             
003000     03 SEQB-KVBUFF-OF       PIC S9(7)           COMP-3.                  
003100*                                 BUFFERSALDO OFÖRÄDLAT GODS              
003200*                                 BUFFER BALANCE UNPREPARED GOODS         
003300     03 SEQB-KVKOLLI-F       PIC S9(5)           COMP-3.                  
003400*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
003500*                                 Q PREPARED CASES IN BUFFER              
003600     03 SEQB-KVKOLLI-OF      PIC S9(5)           COMP-3.                  
003700*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
003800*                                 Q UNPREPARED CASES IN BUFFER            
003900*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
