000100 01  KUL-WDGX4747.                                                        
000200*                                 4747 KULLAGEWREGISTER                   
000300*                                 NYCKEL I ROTEN: IDARTNR                 
000400     03 KUL-KDSEGKEY         PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600     03 KUL-BEARTKUL.                                                     
000700*                                 KULLAGERBENÄMNING                       
000800        05 KUL-BEARTKUL-1    PIC X(15).                                   
000900*                                 DEL AV KULLAGERBENÄMNING                
001000        05 KUL-BEARTKUL-2    PIC X(15).                                   
001100*                                 DEL AV KULLAGERBENÄMNING                
001200     03 KUL-BELEVKUL.                                                     
001300*                                 LEVERANSBENÄMNING                       
001400        05 KUL-BELEVKUL-1    PIC X(11).                                   
001500*                                 LEVERANSBENÄMNING DEL 1                 
001600        05 KUL-BELEVKUL-2    PIC X(10).                                   
001700*                                 LEVERANSBENÄMNING DEL 2                 
001800     03 KUL-DIKULLAG-INNER   PIC S9(4)V9(1)      COMP-3.                  
001900*                                 INNERDIAMETER PÅ KULLAGER               
002000     03 KUL-DIKULLAG-YTTER   PIC S9(4)V9(1)      COMP-3.                  
002100*                                 YTTERDIAMETER PÅ KULLAGER               
002200     03 KUL-IDLEVKUL.                                                     
002300*                                 LEVERANSIDENTITET                       
002400        05 KUL-IDLEVKUL-1    PIC X(8).                                    
002500*                                 DEL AV LEVERANSIDENTITET                
002600        05 KUL-IDLEVKUL-2    PIC X(8).                                    
002700*                                 DEL AV LEVERANSIDENTITET                
002800     03 KUL-IDSTAKUL         PIC S9(9)           COMP-3.                  
002900*                                 STATISTISKT NUMMER FRÅN USA             
003000     03 KUL-VKARTNTO         PIC S9(4)V9(3)      COMP-3.                  
003100*                                 ARTIKELVIKT NETTO (KG)                  
003200     03 KUL-KDARTURS         PIC X(2).                                    
003300*                                 ARTIKELURSPRUNGSKOD                     
003400     03 FILLER               PIC X(15).                                   
003500*** END OF VILMAII-COPY LENGTH= 100 BYTES                                 
