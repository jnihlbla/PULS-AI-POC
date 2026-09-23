000100 01  MOD-W1O23101.                                                        
000200*                                 MOD-COPYTEXT FÖR W1023100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-LEV-IN           PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-KDPRODSL-IN      PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100     03 MOD-IDSKYLT-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-LEV-UT           PIC X.                                       
001400     03 MOD-KDPRODSL-UT      PIC X(2).                                    
001500*                                 PRODUKTSLAG                             
001600     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001700*                                 NATIONALITETSTECKEN                     
001800*                                 SPRÅKIDENTIFIKATION                     
001900     03 MOD-IDARTNR-STR-ENTER                                             
002000                             PIC 9(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDARTNR-STR-NEXT PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-OUTPUT           OCCURS 14 TIMES.                             
002500*                                 RADINFORMATION                          
002600        05 MOD-SELECT-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-SELECT        PIC X.                                       
002900        05 MOD-IDARTNR-STR   PIC Z(9).                                    
003000*                                 ARTIKELNUMMER                           
003100        05 MOD-BEART         PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300        05 MOD-TIFINLV       PIC Z(4)9.                                   
003400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
003500        05 MOD-IDANSK        PIC Z(2)9.                                   
003600*                                 ANSKAFFARNUMMER                         
003700        05 MOD-IDLEVNR       PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900        05 MOD-KDPRODSLA     PIC Z9.                                      
004000*                                 PRODUKTSLAG                             
004100        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
004200*                                 FUNKTIONSGRUPP                          
004300        05 MOD-SATSSTATUS    PIC X.                                       
004400     03 MOD-TEMFSINF         PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 927 BYTES                                 
