000100 01  REQU-WL0175I1.                                                       
000200*                                 REQUEST TO PGM WL0175                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-ADLAGOMR-KEY    PIC 9(2).                                    
000600*                                 LAGEROMRÅDE                             
000700     03 REQU-ADGANG-KEY      PIC 9(2).                                    
000800*                                 GÅNG                                    
000900     03 REQU-ADPLATS-KEY     PIC 9(5).                                    
001000*                                 LAGERPLATSNUMMER                        
001100     03 REQU-KDINVPRIO-KEY   PIC 9.                                       
001200*                                 INVENTERING PRIORITET                   
001300     03 REQU-KDVVKL-KEY      PIC 9.                                       
001400*                                 VOLYMVÄRDESKLASS                        
001500     03 REQU-KDINVKAT-KEY    PIC 9(2).                                    
001600*                                 INVENTERINGSKATEGORI                    
001700     03 REQU-FLINVSKR-KEY    PIC X.                                       
001800*                                 INVENTERINGSANMODAN UTSKRIVEN           
001900     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 REQU-KDPRODSL-KEY    PIC 9(2).                                    
002200*                                 PRODUKTSLAG                             
002300     03 REQU-IDFKNGRP-KEY    PIC 9(4).                                    
002400*                                 FUNKTIONSGRUPP                          
002500     03 REQU-DAREGDAT-KEY    PIC X(10).                                   
002600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002700     03 REQU-KVINVSKR-5302   PIC 9(2).                                    
002800*                                 BEGÄRDA INVENTERINGSUNDERLAG            
002900     03 REQU-FLBUFDOC        PIC X.                                       
003000*                                 ALLMÄN FLAGGA                           
003100     03 REQU-FLURVAL-IN      PIC X.                                       
003200*                                 FLAGGA BEHANDLA HELA URVAET             
003300     03 REQU-KVRADER-MAX     PIC 9(5).                                    
003400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003500*                                 EDAN.                                   
003600     03 REQU-INV-ART-GRP     OCCURS 1 TO 500 TIMES                        
003700                             DEPENDING ON REQU-KVRADER-MAX.               
003800*                                 INVENTERINGSARTIKELGRUPP                
003900        05 REQU-IDARTNR-UTSKR                                             
004000                             PIC 9(8).                                    
004100*                                 ARTIKELNUMMER                           
004200        05 REQU-KDINVPRIO-UTSKR                                           
004300                             PIC 9.                                       
004400*                                 INVENTERING PRIORITET                   
004500        05 REQU-KDINVKAT-UTSKR                                            
004600                             PIC 9(2).                                    
004700*                                 INVENTERINGSKATEGORI                    
004800*** END OF VILMAII-COPY LENGTH= 5549 BYTES                                
