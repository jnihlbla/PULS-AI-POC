000100 01  AVG-WDM521.                                                          
000200*                                 TRANSPORTER MELLAN VTV OCH RA           
000300*                                 AVGÅNGAR                                
000400*                                 FYSISK NYCKEL: DADATTID                 
000500     03 AVG-DADATTID         PIC 9(14).                                   
000600*                                 DATUM + TID(ÅÅÅÅMMDDTTMMSS)             
000700*                                 DATE + TIME(YYYYMMDDHHMMSS)             
000800     03 AVG-IDARTNR          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 AVG-KDTRPSTA         PIC X.                                       
001200*                                 TRANSPORTSTATUS                         
001300*                                 TRANSPORT STATUS                        
001400     03 AVG-KVANTAL          PIC S9(7)           COMP-3.                  
001500*                                 ANTAL                                   
001600*                                 NUMBER                                  
001700     03 AVG-TIUPPDAT         PIC S9(7)           COMP-3.                  
001800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001900*                                 UPDATING DATE     (YYMMDD)              
002000     03 AVG-IDUSER-TRP       PIC X(8).                                    
002100*                                 ANVÄNDAR-ID SENASTE UPPDATERING         
002200*                                 USER ID LAST UPDATE                     
002300     03 AVG-TETRPMED         PIC X(20).                                   
002400*                                 TEXT VID TRANSPORTBEGÄRAN               
002500*                                 TEXT FOR TRANSPORT REQUEST              
002600     03 AVG-FLEJBOK          PIC X.                                       
002700*                                 ANGER ATT ANGIVEN KVANTITET EJ          
002800*                                 SKALL UPPDATERA SALDON                  
002900     03 AVG-FILLER           PIC X(13).                                   
003000*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
