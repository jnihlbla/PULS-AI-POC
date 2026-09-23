000100 01  TRP-WDM911.                                                          
000200*                                 HISTORIK TRANSPORTER (VTV<->RA)         
000300*                                 TRANSPORT INFO                          
000400*                                 FYSISK NYCKEL: DADATTID-9KOMPL          
000500     03 TRP-DADATTID-9KOMPL  PIC 9(14).                                   
000600*                                 DATUM+KLOCKSLAG 9-KOMPLEMENT            
000700*                                 DATE+TIME 9-COMPLEMENT                  
000800     03 TRP-ADTRDEST         PIC X(3).                                    
000900*                                 TRANSPORTDESTINATION                    
001000*                                 ADDRESS OF TRANSPORT                    
001100     03 TRP-IDTRPTNR         PIC S9(5)           COMP-3.                  
001200*                                 TRANSPORTIDENTITET                      
001300*                                 TRANSPORT IDENTITY                      
001400     03 TRP-KVANTAL          PIC S9(7)           COMP-3.                  
001500*                                 ANTAL                                   
001600*                                 NUMBER                                  
001700     03 TRP-TITRPMOT         PIC S9(7)           COMP-3.                  
001800*                                 MOTTAGNINGSDATUM                        
001900*                                 DATE WHEN A TRANSPORT WAS REPOR         
002000*                                 TED AS RECIEVED                         
002100     03 TRP-IDUSER-TRP       PIC X(8).                                    
002200*                                 ANVÄNDAR-ID SENASTE UPPDATERING         
002300*                                 USER ID LAST UPDATE                     
002400     03 TRP-TETRPMED         PIC X(20).                                   
002500*                                 TEXT VID TRANSPORTBEGÄRAN               
002600*                                 TEXT FOR TRANSPORT REQUEST              
002700     03 TRP-ADINLOMR-LPL     PIC X(4).                                    
002800*                                 LOSSNINGSPLATS                          
002900*                                 UNLOADING AREA                          
003000*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
