000100 01  MOD-W3O32201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3322              
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDFKNGRP-IN      PIC X(4).                                    
000900*                                 FUNKTIONSGRUPP                          
001000     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
001100*                                 FUNKTIONSGRUPP                          
001200     03 MOD-KDPRODSL-IN      PIC X(2).                                    
001300*                                 PRODUKTSLAG                             
001400     03 MOD-KDPRODSL-UT      PIC X(2).                                    
001500*                                 PRODUKTSLAG                             
001600     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002100*                                 GRUPP MED TABELL RADER                  
002200        05 MOD-IDARTNR       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400        05 MOD-BEART         PIC X(18).                                   
002500        05 MOD-IDLEVNR       PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700        05 MOD-AF2-ST        PIC Z(8)9.                                   
002800*                                 ANTAL LEV ART RULLANDE ÅR               
002900*                                 (AF2)                                   
003000        05 MOD-VKART         PIC Z(6)9.                                   
003100*                                 ARTIKELVIKT (G)                         
003200        05 MOD-KDERS         PIC X(2).                                    
003300*                                 ERSÄTTNINGSKOD                          
003400        05 MOD-PRARTSJK      PIC Z(6)9.9(2).                              
003500*                                 ARTIKELNS SJÄLVKOSTNAD                  
003600        05 MOD-AF2-TG        PIC -Z9.9.                                   
003700        05 MOD-TIURPROD      PIC X(4).                                    
003800*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
003900     03 MOD-TEMFSINF         PIC X(55).                                   
004000*                                 INFORMATIONSMEDDELANDE                  
004100*** END OF VILMAII-COPY LENGTH= 949 BYTES                                 
