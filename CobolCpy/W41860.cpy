000100 01  W418600.                                                             
000200*                                 SDC-KREDITNOTOR                         
000300*                                                                         
000400     03 SORT-DEL.                                                         
000500        05 IDPTYP            PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 SORTNR            PIC S9              COMP-3.                  
000800*                                 SORTERINGSNR                            
000900*                                 ANV SOM SORTBEGR VID LISTUTSKRI         
001000*                                 FT,FOB-LISTA                            
001100     03 FILLER.                                                           
001200        05 IDVAT             PIC X(17).                                   
001300*                                 MOMSREGISTRERINGSNUMMER                 
001400        05 IDPARTNR          PIC X(9).                                    
001500*                                 PARTNERNUMMER                           
001600        05 IDDISTR           PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000        05 IDKNOTNR          PIC S9(7)           COMP-3.                  
002100*                                 KREDITNOTANUMMER                        
002200        05 TIKNOTA           PIC S9(7)           COMP-3.                  
002300*                                 KREDITNOTADATUM                         
002400        05 KDVALISO          PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600        05 SUKRENOT          PIC S9(9)V9(2)      COMP-3.                  
002700*                                 KREDITNOTASUMMA                         
002800        05 PRMOMS            PIC S9(7)V9(2)      COMP-3.                  
002900*                                 MERVÄRDESSKATT                          
003000        05 SUKRENOT-LOC      PIC S9(9)V9(2)      COMP-3.                  
003100*                                 KREDITNOTASUMMA                         
003200        05 SUVAT-LOC         PIC S9(11)V9(2)     COMP-3.                  
003300*                                 MOMSVÄRDE PER MOMSKOD                   
003400*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
