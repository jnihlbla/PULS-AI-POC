000100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
000200* AVSER   : NYA BEST PRISER (TILL SM-2)                         *         
000300* URSPRUNG:                                                     *         
000400* INNEHÅLL:                                                     *         
000500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
000600 01  A310T985.                                                            
000700     05  PT                      PIC S9(3)         COMP-3.                
000800     05  GSDB-FORB               PIC X(5).                                
000900     05  ARTNR                   PIC S9(9)         COMP-3.                
001000     05  GSDB-LEV                PIC X(5).                                
001100     05  PRISSKR-BEST            PIC S9(7)V9(2)    COMP-3.                
001200     05  BESTPRIS                PIC S9(9)         COMP-3.                
001300     05  KDENH-BEST              PIC X(1).                                
001400     05  KDSORT                  PIC X(1).                                
001700     05  KDPRIS-TYP              PIC X(1).                                
001800*                            *** 0 = FÖRHANDLAT, 1 = TIPPAT,              
001900*                                2 = GRATIS                               
002000     05  KDVAL-BEST              PIC S9(3)         COMP-3.                
002100     05  DATUM-BESTPRIS          PIC S9(7)         COMP-3.                
002200     05  INKNR                   PIC 9(3).                                
002300     05  BESTTYP                 PIC X(1).                                
002400*                            *** 1 = GÄLLANDE AVTALS-BESTÄLLNING          
002500*                            *** 2 = GÄLLANDE KVANTITETS BEST             
002600*                            *** 3 = BESTÄLLNING ANNULLERAD               
002700*                            *** 4 = SEGMENT SAKNAS                       
002800     05  KDVAL-ISO               PIC X(3).                                
003100     05  FORBNR                  PIC S9(5)         COMP-3.                
003200     05  LEVNUM                  PIC S9(5)         COMP-3.                
003300     05  KDFPKPRI                PIC X.                                   
003400*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
