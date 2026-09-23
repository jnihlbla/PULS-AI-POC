000100 01  HIST-WDJ911.                                                         
000200*                                 PLATS REGISTER                          
000300*                                 FYSISK NYCKEL: WDJ911KY                 
000400*                                 (IDDC + DASTADAT-9KOMPL +               
000500*                                  TISTATID-9KOMPL        +               
000600*                                  ADLAGOMR + ADGANG + ADPLATS)           
000700     03 HIST-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 HIST-DASTADAT-9KOMPL PIC S9(9)           COMP-3.                  
001100*                                 GENERELLT STARTDATUM 9KOMPL             
001200*                                 GENERAL START DATE 9COMPL               
001300     03 HIST-TISTATID-9KOMPL PIC S9(7)           COMP-3.                  
001400*                                 GENERELL STARTTID                       
001500*                                 GENERAL START TIME                      
001600     03 HIST-ADLAGOMR        PIC 9(2).                                    
001700*                                 LAGEROMRÅDE                             
001800*                                 AREA                                    
001900     03 HIST-ADGANG          PIC 9(2).                                    
002000*                                 GÅNG                                    
002100*                                 AISLE                                   
002200     03 HIST-ADPLATS         PIC 9(5).                                    
002300*                                 LAGERPLATSNUMMER                        
002400*                                 LOCATION                                
002500     03 HIST-KDLOC           PIC X.                                       
002600*                                 TYP AV LAGERPLATS                       
002700*                                 TYPE OF LOCATION                        
002800     03 HIST-IDUSER          PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000*                                 USER SECURITY-IDENTITY                  
003100     03 HIST-IDUSER-STO      PIC X(8).                                    
003200*                                 ANVÄNDARENS SÄKERHETS ID                
003300*                                 USER SECURITY-IDENTITY                  
003400     03 HIST-DASTODAT        PIC S9(9)           COMP-3.                  
003500*                                 GENERELLT STOPPDATUM                    
003600*                                 GENERAL STOP DATE                       
003700*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
