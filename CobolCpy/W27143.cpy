000100 01  W27143.                                                              
000200*                                 UPDATERING AV WDK7                      
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 FLREFBEO             PIC X.                                       
001100*                                 AUTOMATISK REFILL BEORDRING?            
001200*                                 AUTOMATIC REFILL ORDERING?              
001300     03 FLPB-FLYTT           PIC X.                                       
001400*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
001500*                                  REDA PÅ KOPIERING AV PROGNOS           
001600*                                 FLAG                                    
001700     03 KDERS                PIC S9(3)           COMP-3.                  
001800*                                 ERSÄTTNINGSKOD                          
001900*                                 SUPERSESSION CODE                       
002000     03 FLIART               PIC X.                                       
002100*                                 ARTIKELN INGÅR I SATS                   
002200*                                 PART IN KIT                             
002300     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
002400*                                 PERIODBEHOV REFILLING                   
002500*                                 FORECAST REFILLING                      
002600*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
