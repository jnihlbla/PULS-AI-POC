000100 01  RUBA-WDN2A1.                                                         
000200*                                 KATALOG RUBRIK-LEXIKON                  
000300*                                 SECONDARY-INDEX PÅ IDSKYLT              
000400*                                 FYSISK NYCKEL: WDN2A1KY                 
000500*                                  (IDSKYLT + IDRUBNR + IDSEGMNR)         
000600*                                 SECONDARY NYCKEL: WDN2ASEQ              
000700*                                  (IDSKYLT)                              
000800     03 RUBA-IDSKYLT         PIC X(3).                                    
000900*                                 NATIONALITETSTECKEN                     
001000*                                 SPRÅKIDENTIFIKATION                     
001100*                                 NATIONALITY SIGN                        
001200*                                 LANGUAGE IDENTIFIER                     
001300     03 RUBA-IDRUBNR         PIC S9(5)           COMP-3.                  
001400*                                 RUBRIKNUMMER                            
001500*                                 HEADLINE ID NUMBER                      
001600     03 RUBA-IDSEGMNR        PIC S9              COMP-3.                  
001700*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
001800*                                 SEQUENCE ORDER ID, ON SEGMENT           
001900     03 RUBA-BERUBTXT        PIC X(30).                                   
002000*                                 RUBRIKTEXT                              
002100*                                 HEADER TEXT                             
002200*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
