000100 01  4526-WDGX4526.                                                       
000200*                                 AUTOMATISK                              
000300*                                 ORDERNUMMERSERIE                        
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (KDSEGKEY + LOW-VALUE)                  
000600     03 4526-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 4526-LOW-VALUE       PIC X(4).                                    
001000     03 4526-IDORDNR7-MIN    PIC S9(7)           COMP-3.                  
001100*                                 LÄGSTA TILLÅTNA ORDERNUMMER             
001200     03 4526-IDORDNR7-MAX    PIC S9(7)           COMP-3.                  
001300*                                 HÖGSTA TILLÅTNA ORDERNUMMER             
001400     03 4526-IDORDNR7-LEDIG  PIC S9(7)           COMP-3.                  
001500*                                 NÄSTA LEDIGA ORDERNUMMER                
001600     03 4526-IDORDER-LEDIG   PIC S9(7)           COMP-3.                  
001700*                                 NÄSTA LEDIGA PARTS ORDERNUMMER          
001800     03 4526-IDPRODNR-LEDIG  PIC S9(7)           COMP-3.                  
001900*                                 NÄSTA LEDIGA PRODUKTIONSNR              
002000     03 4526-IDORDNSB-LEDIG  PIC S9(5)           COMP-3.                  
002100*                                 SATSORDERNR NÄSTA LEDIGA BASV           
002200*                                 KIT ORDER NO NEXT FREE                  
002300     03 4526-FILLER          PIC X(2).                                    
002400*** END COPY WDGX4526C0  LENGTH=30                                        
