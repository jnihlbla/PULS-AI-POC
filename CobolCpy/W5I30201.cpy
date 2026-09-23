000100 01  MID-W5I30201.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 INVENTERING                             
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 MID-ADLAGOMR-IN      PIC X(2).                                    
000900*                                 LAGEROMRÅDE                             
001000     03 MID-ADLAGOMR-UT      PIC X(2).                                    
001100*                                 LAGEROMRÅDE                             
001200     03 MID-ADGANG-IN        PIC X(2).                                    
001300*                                 GÅNG                                    
001400     03 MID-ADGANG-UT        PIC X(2).                                    
001500*                                 GÅNG                                    
001600     03 MID-ADPLATS-IN       PIC X(5).                                    
001700*                                 LAGERPLATSNUMMER                        
001800     03 MID-ADPLATS-UT       PIC X(5).                                    
001900*                                 LAGERPLATSNUMMER                        
002000     03 MID-KDINVPRIO-IN     PIC X.                                       
002100*                                 INVENTERING PRIORITET                   
002200     03 MID-KDINVPRIO-UT     PIC X.                                       
002300*                                 INVENTERING PRIORITET                   
002400     03 MID-KDVVKL-IN        PIC X.                                       
002500*                                 VOLYMVÄRDESKLASS                        
002600     03 MID-KDVVKL-UT        PIC X.                                       
002700*                                 VOLYMVÄRDESKLASS                        
002800     03 MID-KDINVKAT-IN      PIC X(2).                                    
002900*                                 INVENTERINGSKATEGORI                    
003000     03 MID-KDINVKAT-UT      PIC X(2).                                    
003100*                                 INVENTERINGSKATEGORI                    
003200     03 MID-FLINVSKR-IN      PIC X.                                       
003300*                                 INVENTERINGSANMODAN UTSKRIVEN           
003400     03 MID-FLINVSKR-UT      PIC X.                                       
003500*                                 INVENTERINGSANMODAN UTSKRIVEN           
003600     03 MID-KDPRODSL-IN      PIC X(2).                                    
003700*                                 PRODUKTSLAG                             
003800     03 MID-KDPRODSL-UT      PIC X(2).                                    
003900*                                 PRODUKTSLAG                             
004000     03 MID-IDFKNGRP-IN      PIC X(4).                                    
004100*                                 FUNKTIONSGRUPP                          
004200     03 MID-IDFKNGRP-UT      PIC X(4).                                    
004300*                                 FUNKTIONSGRUPP                          
004400     03 MID-TIREGDAT-IN      PIC X(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600     03 MID-TIREGDAT-UT      PIC X(6).                                    
004700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004800     03 MID-IDARTNR-IN       PIC X(9).                                    
004900*                                 ARTIKELNUMMER                           
005000     03 MID-IDLISTNR         PIC X.                                       
005100*                                 ALLMÄN FLAGGA                           
005200     03 MID-KVINVSKR-5302    PIC 9(2).                                    
005300*                                 BEGÄRDA INVENTERINGSUNDERLAG            
005400     03 MID-IDARTNR-UT       PIC X(9).                                    
005500*                                 ARTIKELNUMMER                           
005600     03 MID-IDNODE           PIC X(8).                                    
005700*                                 VTAM NODE-NAMN                          
005800     03 MID-FLURVAL-IN       PIC X.                                       
005900*                                 FLAGGA BEHANDLA HELA URVAET             
006000     03 MID-FLURVAL-UT       PIC X.                                       
006100*                                 FLAGGA BEHANDLA HELA URVAET             
006200     03 MID-INV-ART-GRP      OCCURS 10 TIMES.                             
006300*                                 INVENTERINGSARTIKELGRUPP                
006400        05 MID-IDARTNR-UTSKR PIC 9(9).                                    
006500*                                 ARTIKELNUMMER                           
006600        05 MID-KDINVPRIO-UTSKR                                            
006700                             PIC X.                                       
006800*                                 INVENTERING PRIORITET                   
006900        05 MID-KDINVKAT-UTSKR                                             
007000                             PIC 9(2).                                    
007100*                                 INVENTERINGSKATEGORI                    
007200*** END OF VILMAII-COPY LENGTH= 207 BYTES                                 
