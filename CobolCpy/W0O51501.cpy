000100 01  MOD-W0O51501.                                                        
000200*                                 MODCOPYTEXT TILL W00515.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-RADER            OCCURS 12 TIMES.                             
001600*                                 RADINFORMATION                          
001700        05 MOD-IDTRANS-ATTR  PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-IDTRANS-HOPP  PIC X(4).                                    
002000*                                 BILDNUMMER                              
002100        05 MOD-IDARTNR       PIC Z(8)9.                                   
002200*                                 ARTIKELNUMMER                           
002300        05 MOD-BEART         PIC X(25).                                   
002400*                                 ARTIKELBENÄMNING                        
002500        05 MOD-KDPRODSL      PIC Z(2)9.                                   
002600*                                 PRODUKTSLAG                             
002700        05 MOD-IDFKNGRP      PIC Z(4)9.                                   
002800*                                 FUNKTIONSGRUPP                          
002900        05 MOD-KDERS         PIC Z9(2).                                   
003000*                                 ERSÄTTNINGSKOD                          
003100        05 MOD-KDSORT        PIC X(2).                                    
003200*                                 SORT-KOD                                
003300        05 MOD-IDLEVNR       PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500     03 MOD-TEMFSINF         PIC X(55).                                   
003600*                                 INFORMATIONSMEDDELANDE                  
003700*** END OF VILMAII-COPY LENGTH= 817 BYTES                                 
