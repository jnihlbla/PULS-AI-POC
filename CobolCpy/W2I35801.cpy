000100 01  MID-W2I35801.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 ON ORDER FROM LOCAL VENDORS             
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-INPUT.                                                        
001300        05 MID-RAD           OCCURS 13 TIMES.                             
001400           07 MID-IDDC       PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600           07 MID-TIREGDAT   PIC 9(6).                                    
001700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001800           07 MID-KVBEART-IN PIC X(6).                                    
001900*                                 BESTÄLLT ANTAL STYCKEN                  
002000           07 MID-TIBERANK-IN                                             
002100                             PIC X(6).                                    
002200*                                 BERÄKNAD ANKOMSTDATUM                   
002300           07 MID-TIREGTID   PIC 9(6).                                    
002400*                                 REGISTRERINGSTID                        
002500*** END OF VILMAII-COPY LENGTH= 360 BYTES                                 
