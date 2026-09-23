000100 01  1124-WDGX1124.                                                       
000200*                                 BASLAGER                                
000300*                                 PROJEKTSTRUKTURER                       
000400*                                 PROJEKTTIDER                            
000500*                                 FYSISK NYCKEL: KDSEGKEY                 
000600*                                 SKALL VARA  = "1"                       
000700     03 1124-KDSEGKEY        PIC X.                                       
000800*                                 TEKNISK SEGMENT-NYCKEL                  
000900*                                 TECHNICAL SEGMENT KEY                   
001000     03 1124-TIBLREG         PIC S9(7)           COMP-3.                  
001100*                                 REGISTRERINGSTID FÖRSTA ARTIKEL         
001200*                                 REGISTRATION TIME FIRST PART            
001300     03 1124-TIGENORD        PIC S9(7)           COMP-3.                  
001400*                                 GENERELL TID FÖR ORDERSLÄPP             
001500*                                 GENERAL TIME FOR ORDER RELEASE          
001600     03 1124-TIPROJSTO       PIC S9(7)           COMP-3.                  
001700*                                 PROJEKTSTOPP BASLAGER                   
001800*                                 PROJECT STOP BASIC STOCK                
001900     03 1124-TISTAMREG       PIC S9(7)           COMP-3.                  
002000*                                 STARTTID 5 VECKORS REGELN               
002100*                                 START TIME 5 WEEKS RULES                
002200     03 FILLER               PIC X(23).                                   
002300*** END COPY WDGX1124C0  LENGTH=40                                        
