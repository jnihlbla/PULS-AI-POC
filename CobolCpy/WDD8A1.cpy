000100 01  SEQA-WDD8A1.                                                         
000200*                                 BUFFERTREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDD811             
000400*                                 PLATS      INGÅNG                       
000500*                                 FYSISK NYCKEL: WDD8A1KY                 
000600*                                  (IDDC, ADBUFFOMR, ADBUFFGANG,          
000700*                                   ADBUFFPL, DABUFPAF, IDARTNR)          
000800*                                 SECONDARY NYCKEL: WDD8ASEQ              
000900*                                  (IDDC,ADBUFFOMR,ADBUFFGANG,            
001000*                                   ADBUFFPL)                             
001100     03 SEQA-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQA-ADBUFFOMR       PIC S9(3)           COMP-3.                  
001500*                                 BUFFERTOMRÅDE                           
001600*                                 BUFFER AREA                             
001700     03 SEQA-ADBUFFGANG      PIC S9(3)           COMP-3.                  
001800*                                 BUFFERT GÅNG                            
001900     03 SEQA-ADBUFFPL        PIC S9(5)           COMP-3.                  
002000*                                 BUFFERPLATSNUMMER                       
002100*                                 LOCATION IN BUFFER                      
002200     03 SEQA-DABUFPAF        PIC 9(8).                                    
002300*                                 BUFFERT PÅFYLLNINGS DATUM               
002400*                                 BUFFERT REFILLING DATE                  
002500     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
