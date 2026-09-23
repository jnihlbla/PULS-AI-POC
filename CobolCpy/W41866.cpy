000100 01  W41866-CTX.                                                          
000200*                                 SDC22-KREDITNOTOR                       
000300*                                                                         
000400     03 W41866-001-GRP.                                                   
000500        05 IDPTYP            PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 SORTNR            PIC S9              COMP-3.                  
000800*                                 SORTERINGSNR                            
000900*                                 ANV SOM SORTBEGR VID LISTUTSKRI         
001000*                                 FT,FOB-LISTA                            
001100     03 W41866-002-GRP.                                                   
001200        05 IDVAT             PIC X(17).                                   
001300*                                 MOMREGISTRERINGSNUMMER                  
001400        05 IDDISTR           PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800        05 IDKNOTNR          PIC S9(7)           COMP-3.                  
001900*                                 KREDITNOTANUMMER                        
002000        05 TIM-KN            PIC S9(7)           COMP-3.                  
002100*                                 DATUM (ÅÅMMDD)                          
002200        05 KDVALISO          PIC X(3).                                    
002300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002400        05 SUKRENOT          PIC S9(9)V9(2)      COMP-3.                  
002500*                                 KREDITNOTASUMMA                         
002600        05 PRMOMS            PIC S9(7)V9(2)      COMP-3.                  
002700*                                 MERVÄRDESSKATT                          
002800        05 SUKRENOT-FRF      PIC S9(9)V9(2)      COMP-3.                  
002900*                                 KREDITNOTASUMMA                         
003000        05 SUVAT-FRF         PIC S9(11)V9(2)     COMP-3.                  
003100*                                 MOMSVÄRDE PER MOMSKOD                   
003200*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
