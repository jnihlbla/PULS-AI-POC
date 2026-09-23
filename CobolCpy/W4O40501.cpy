000100 01  MOD-W4O40501.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-RAD              OCCURS 34 TIMES.                             
001200*                                                                         
001300        05 MOD-KDANMORS      PIC X(2).                                    
001400*                                 ORSAK TILL LEVERANSANMÄRKNING           
001500     03 MOD-RAD              OCCURS 16 TIMES.                             
001600*                                                                         
001700        05 MOD-IDFKNGRP      PIC Z(4)9.                                   
001800*                                 FUNKTIONSGRUPP                          
001900     03 MOD-RAD              OCCURS 18 TIMES.                             
002000*                                                                         
002100        05 MOD-IDARTNR       PIC Z(8)9.                                   
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-RAD              OCCURS 21 TIMES.                             
002400*                                                                         
002500        05 MOD-IDDC-EXCP     PIC X(2).                                    
002600*                                 DC FÖR RETUR EJ TILLÅTET                
002700     03 MOD-FLARTDC-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLARTDC-UPD      PIC X.                                       
003000*                                 INDIKERAR OM ART SKA VARA PÅ DC         
003100     03 MOD-KDBEHX-ATTR      PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDBEHX           PIC X.                                       
003400*                                 BEHANDLINGSKOD-X                        
003500     03 MOD-KDANMORS-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDANMORS-UPD     PIC X(2).                                    
003800*                                 ORSAK TILL LEVERANSANMÄRKNING           
003900     03 MOD-IDFKNGRP-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDFKNGRP-UPD     PIC Z(4)9.                                   
004200*                                 FUNKTIONSGRUPP                          
004300     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDARTNR-UPD      PIC Z(9).                                    
004600*                                 ARTIKELNUMMER                           
004700     03 MOD-IDDC-EXCP-ATTR   PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-IDDC-EXCP-UPD    PIC X(2).                                    
005000*                                 DC FÖR RETUR EJ TILLÅTET                
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END OF VILMAII-COPY LENGTH= 487 BYTES                                 
