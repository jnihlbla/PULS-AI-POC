000100 01  FOTA-WDN3A1.                                                         
000200*                                 KATALOG FOTNOTS-LEXIKON                 
000300*                                 SECONDARY-INDEX PÅ IDSKYLT              
000400*                                 FYSISK NYCKEL: WDN3A1KY                 
000500*                                  (IDSKYLT + KDFORDON + IDFOTNR          
000600*                                   + IDSEGMNR)                           
000700*                                 SECONDARY NYCKEL: WDN3ASEQ              
000800*                                  (IDSKYLT)                              
000900     03 FOTA-IDSKYLT         PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN                     
001100*                                 SPRÅKIDENTIFIKATION                     
001200*                                 NATIONALITY SIGN                        
001300*                                 LANGUAGE IDENTIFIER                     
001400     03 FOTA-KDFORDON        PIC X(2).                                    
001500*                                 FORDONSSLAG                             
001600*                                 TYPE OF VEHICLE CODE                    
001700     03 FOTA-IDFOTNR         PIC S9(5)           COMP-3.                  
001800*                                 FOTNOTSNUMMER                           
001900*                                 FOOT NOTE ID NUMBER                     
002000     03 FOTA-IDSEGMNR        PIC S9              COMP-3.                  
002100*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
002200*                                 SEQUENCE ORDER ID, ON SEGMENT           
002300     03 FOTA-BEFOTNOT        PIC X(55).                                   
002400*                                 FOTNOTSTEXT                             
002500*                                 FOOTNOTE TEXT                           
002600*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
