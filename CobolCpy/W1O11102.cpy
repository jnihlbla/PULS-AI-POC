000100 01  MOD-W1O11102.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 FELMEDDELANDEFÄLT                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MOD-IDARTNR-UT       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-KDSORT-UT        PIC X(2).                                    
001100*                                 SORT-KOD                                
001200     03 MOD-IDFKNGRP-UT      PIC Z(3)9.                                   
001300*                                 FUNKTIONSGRUPP                          
001400     03 MOD-KDPRODSL-UT      PIC Z9.                                      
001500*                                 PRODUKTSLAG                             
001600     03 MOD-FLLSRDEL-UT      PIC X.                                       
001700*                                 LEVERERAS SOM RESDEL                    
001800     03 MOD-KDUART-UT        PIC X.                                       
001900*                                 UNDANTAGSARTIKEL                        
002000     03 MOD-KDBPSR-UT        PIC 9.                                       
002100*                                 BASLAGERFÖRSLAGSNIVÅ                    
002200     03 MOD-KDSORT-IN        PIC X(2).                                    
002300*                                 SORT-KOD                                
002400     03 MOD-IDFKNGRP-IN      PIC X(4).                                    
002500*                                 FUNKTIONSGRUPP                          
002600     03 MOD-KDPRODSL-IN      PIC X(2).                                    
002700*                                 PRODUKTSLAG                             
002800     03 MOD-FLLSRDEL-IN      PIC X.                                       
002900*                                 LEVERERAS SOM RESDEL                    
003000     03 MOD-KDUART-IN        PIC X.                                       
003100*                                 UNDANTAGSARTIKEL                        
003200     03 MOD-KDBPSR-IN        PIC X.                                       
003300*                                 BASLAGERFÖRSLAGSNIVÅ                    
003400     03 MOD-TIFINLV-UT       PIC Z(4)9.                                   
003500*                                 PUBLICERINGSVECKA                       
003600     03 MOD-KDFARLIG-UT      PIC 9.                                       
003700*                                 KOD FÖR FARLIGT GODS                    
003800     03 MOD-IDBERED-UT       PIC Z9.                                      
003900*                                 BEREDARENUMMER                          
004000     03 MOD-KDYTBEH-UT       PIC Z9.                                      
004100*                                 YTBEHANDLINGSKOD                        
004200     03 MOD-TIFINLV-IN       PIC X(5).                                    
004300*                                 PUBLICERINGSVECKA                       
004400     03 MOD-KDFARLIG-IN      PIC 9.                                       
004500*                                 KOD FÖR FARLIGT GODS                    
004600     03 MOD-IDBERED-IN       PIC X(2).                                    
004700*                                 BEREDARENUMMER                          
004800     03 MOD-KDYTBEH-IN       PIC X(2).                                    
004900*                                 YTBEHANDLINGSKOD                        
005000     03 MOD-IDKAT-1          PIC X(5).                                    
005100*                                 KATALOGBETECKNING                       
005200     03 MOD-IDKAT-2          PIC X(5).                                    
005300*                                 KATALOGBETECKNING                       
005400     03 MOD-IDKAT-3          PIC X(5).                                    
005500*                                 KATALOGBETECKNING                       
005600     03 MOD-IDPROENH-1       PIC X(8).                                    
005700*                                 PRODUKTIONSENHET                        
005800     03 MOD-IDPROENH-2       PIC X(8).                                    
005900*                                 PRODUKTIONSENHET                        
006000     03 MOD-IDPROENH-3       PIC X(8).                                    
006100*                                 PRODUKTIONSENHET                        
006200     03 MOD-IDAO             PIC X(10).                                   
006300*                                 ÄNDRINGSORDERNUMMER                     
006400     03 MOD-IDPROJ           PIC X(4).                                    
006500*                                 PROJEKTIDENTITET                        
006600     03 MOD-IDPROJUP         PIC X(8).                                    
006700*                                 PROJEKTUPPDRAG                          
006800     03 MOD-IDAETNR          PIC Z(2)9.                                   
006900*                                 ÄNDRINGSTILLFÄLLENUMMER                 
007000     03 MOD-IDRITN           PIC X(10).                                   
007100*                                 RITNINGSNUMMER                          
007200     03 MOD-IDSKYLT-TILLV    PIC X(3).                                    
007300*                                 TILLVERKNINGSLAND                       
007400     03 MOD-BEART            PIC X(25).                                   
007500*                                 ARTIKELBENÄMNING                        
007600     03 MOD-FLRSBEART        PIC X.                                       
007700*                                 RS-UNIK BENÄMNING                       
007800     03 MOD-TEARTNOT         PIC X(40).                                   
007900*                                 NOTERING OM ARTIKELN                    
008000     03 MOD-FLNYRAPP         PIC X.                                       
008100*                                 SKA NYKÖPSRAPPORT SKRIVAS.              
008200     03 MOD-FLGAMART         PIC 9.                                       
008300*                                 ARTIKELÄNDRING                          
008400     03 MOD-IDLEVNR          PIC Z(4)9.                                   
008500*                                 LEVERANTÖRNUMMER                        
008600     03 MOD-BELEV            PIC X(30).                                   
008700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
008800     03 MOD-TEMFSINF         PIC X(61).                                   
008900*                                 INFORMATIONSMEDDELANDE                  
009000*** END COPY W1O11102C0  LENGTH=345                                       
