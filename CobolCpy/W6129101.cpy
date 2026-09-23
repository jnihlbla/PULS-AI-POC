000100 01  W6129101.                                                            
000200*                                 WDJ901  + WDJ911 KEYS                   
000300*                                 IDARTNR + WDJ911KY                      
000400*                                 (IDDC + DASTADAT-9KOMPL +               
000500*                                  TISTATID-9KOMPL        +               
000600*                                  ADLAGOMR + ADGANG + ADPLATS)           
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 DASTADAT-9KOMPL      PIC S9(9)           COMP-3.                  
001400*                                 GENERELLT STARTDATUM 9KOMPL             
001500*                                 GENERAL START DATE 9COMPL               
001600     03 TISTATID-9KOMPL      PIC S9(7)           COMP-3.                  
001700*                                 GENERELL STARTTID                       
001800*                                 GENERAL START TIME                      
001900     03 ADLAGOMR             PIC 9(2).                                    
002000*                                 LAGEROMRÅDE                             
002100*                                 AREA                                    
002200     03 ADGANG               PIC 9(2).                                    
002300*                                 GÅNG                                    
002400*                                 AISLE                                   
002500     03 ADPLATS              PIC 9(5).                                    
002600*                                 LAGERPLATSNUMMER                        
002700*                                 LOCATION                                
002800*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
