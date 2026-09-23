000100 01  MID-W0I81601.                                                        
000200*                                 MIDCOPYTEXT FÖR BILD 0816               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-INPUT.                                                        
000600*                                 MID-INDATA                              
000700        05 MID-KDCMD-BEARTKUL                                             
000800                             PIC X.                                       
000900*                                 RAD-UPPDATERINGSKOMMANDO                
001000*                                  BLANK  = INGENTING                     
001100*                                  D , B  = DELETE                        
001200*                                  R , Ä  = REPLACE                       
001300*                                  I , N  = INSERT                        
001400        05 MID-BEARTKUL.                                                  
001500*                                 KULLAGERBENÄMNING                       
001600           07 MID-BEARTKUL-1 PIC X(15).                                   
001700*                                 DEL AV KULLAGERBENÄMNING                
001800           07 MID-BEARTKUL-2 PIC X(15).                                   
001900*                                 DEL AV KULLAGERBENÄMNING                
002000        05 MID-KDCMD-BELEVKUL                                             
002100                             PIC X.                                       
002200*                                 RAD-UPPDATERINGSKOMMANDO                
002300*                                  BLANK  = INGENTING                     
002400*                                  D , B  = DELETE                        
002500*                                  R , Ä  = REPLACE                       
002600*                                  I , N  = INSERT                        
002700        05 MID-BELEVKUL.                                                  
002800*                                 LEVERANSBENÄMNING                       
002900           07 MID-BELEVKUL-1 PIC X(11).                                   
003000*                                 LEVERANSBENÄMNING DEL 1                 
003100           07 MID-BELEVKUL-2 PIC X(10).                                   
003200*                                 LEVERANSBENÄMNING DEL 2                 
003300        05 MID-KDCMD-DIKULLAG-INNER                                       
003400                             PIC X.                                       
003500*                                 RAD-UPPDATERINGSKOMMANDO                
003600*                                  BLANK  = INGENTING                     
003700*                                  D , B  = DELETE                        
003800*                                  R , Ä  = REPLACE                       
003900*                                  I , N  = INSERT                        
004000        05 MID-DIKULLAG-INNER                                             
004100                             PIC X(6).                                    
004200*                                 INNERDIAMETER PÅ KULLAGER               
004300        05 MID-KDCMD-DIKULLAG-YTTER                                       
004400                             PIC X.                                       
004500*                                 RAD-UPPDATERINGSKOMMANDO                
004600*                                  BLANK  = INGENTING                     
004700*                                  D , B  = DELETE                        
004800*                                  R , Ä  = REPLACE                       
004900*                                  I , N  = INSERT                        
005000        05 MID-DIKULLAG-YTTER                                             
005100                             PIC X(6).                                    
005200*                                 YTTERDIAMETER PÅ KULLAGER               
005300        05 MID-KDCMD-IDLEVKUL                                             
005400                             PIC X.                                       
005500*                                 RAD-UPPDATERINGSKOMMANDO                
005600*                                  BLANK  = INGENTING                     
005700*                                  D , B  = DELETE                        
005800*                                  R , Ä  = REPLACE                       
005900*                                  I , N  = INSERT                        
006000        05 MID-IDLEVKUL.                                                  
006100*                                 LEVERANSIDENTITET                       
006200           07 MID-IDLEVKUL-1 PIC X(8).                                    
006300*                                 DEL AV LEVERANSIDENTITET                
006400           07 MID-IDLEVKUL-2 PIC X(8).                                    
006500*                                 DEL AV LEVERANSIDENTITET                
006600        05 MID-KDCMD-IDSTAKUL                                             
006700                             PIC X.                                       
006800*                                 RAD-UPPDATERINGSKOMMANDO                
006900*                                  BLANK  = INGENTING                     
007000*                                  D , B  = DELETE                        
007100*                                  R , Ä  = REPLACE                       
007200*                                  I , N  = INSERT                        
007300        05 MID-IDSTAKUL      PIC 9(9).                                    
007400*                                 STATISTISKT NUMMER FRÅN USA             
007500        05 MID-KDCMD-VKARTNTO                                             
007600                             PIC X.                                       
007700*                                 RAD-UPPDATERINGSKOMMANDO                
007800*                                  BLANK  = INGENTING                     
007900*                                  D , B  = DELETE                        
008000*                                  R , Ä  = REPLACE                       
008100*                                  I , N  = INSERT                        
008200        05 MID-VKARTNTO      PIC X(8).                                    
008300*                                 ARTIKELVIKT NETTO (KG)                  
008400        05 MID-KDCMD-KDARTURS                                             
008500                             PIC X.                                       
008600*                                 RAD-UPPDATERINGSKOMMANDO                
008700*                                  BLANK  = INGENTING                     
008800*                                  D , B  = DELETE                        
008900*                                  R , Ä  = REPLACE                       
009000*                                  I , N  = INSERT                        
009100        05 MID-KDARTURS      PIC X(2).                                    
009200*                                 ARTIKELURSPRUNGSKOD                     
009300     03 MID-FLANNULL         PIC X.                                       
009400*                                 ANNULLATION                             
009500*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
