000100 01  MID-W4I61101-CTX.                                                    
000200*                                 COPYTEXT FÖR MID W4I61101               
000300*                                                                         
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDDISTR-UT       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MID-KDFRAKT-IN       PIC X(2).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 MID-KDFRAKT-UT       PIC X(2).                                    
001500*                                 FRAKTSÄTT DC TILL KUND                  
001600     03 MID-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MID-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MID-SPAR-IDDISTR     PIC 9(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MID-SPAR-IDKUNDNR    PIC 9(6).                                    
002300*                                 KUNDNUMMER                              
002400     03 MID-SPAR-KDFRAKT     PIC 9(2).                                    
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 MID-SPAR-IDPRODNR-SAMP                                            
002700                             PIC 9(7).                                    
002800*                                 PRODUKTIONSNUMMER SAMPACKNING           
002900     03 MID-FLSAMTL          PIC X.                                       
003000*                                 J= SAMTL KOLLI VALDA PÅ ORDERN          
003100     03 MID-W4I61101-001-GRP OCCURS 13 TIMES.                             
003200        05 MID-KDUPPTYP      PIC X.                                       
003300*                                 UPPDATERINGSTYP                         
003400        05 MID-IDDISTR       PIC X(4).                                    
003500*                                 DISTRIKTNUMMER                          
003600        05 MID-IDKUNDNR      PIC X(6).                                    
003700*                                 KUNDNUMMER                              
003800        05 MID-KDFRAKT       PIC X(2).                                    
003900*                                 FRAKTSÄTT DC TILL KUND                  
004000        05 MID-IDORDNR       PIC X(5).                                    
004100*                                 ORDERNUMMER UTGÅR PD90                  
004200        05 MID-KDORDKL       PIC X.                                       
004300*                                 ORDERKLASS                              
004400        05 MID-IDPRODNR      PIC X(7).                                    
004500*                                 PRODUKTIONSNUMMER                       
004600        05 MID-RADTEXT       PIC X(4).                                    
004700*** END OF VILMAII-COPY LENGTH= 438 BYTES                                 
