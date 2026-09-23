000100 01  W11810.                                                              
000200     03 IDARTNR              PIC X(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 IDPRODNR             PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 FLBYTES-MAN          PIC X.                                       
000700*                                 BYTESARTIKEL                            
000800     03 FLSPARE              PIC X.                                       
000900*                                 SÄSONG PÅ ARTIKEL                       
001000     03 KDERS                PIC 9(3).                                    
001100*                                 ERSÄTTNINGSKOD                          
001200     03 IDBERED              PIC X(2).                                    
001300*                                 BEREDARENUMMER                          
001400     03 KDPRODSL             PIC X(2).                                    
001500*                                 PRODUKTSLAG                             
001600     03 KDSORT               PIC X(2).                                    
001700*                                 SORT-KOD                                
001800     03 IDPROENH             PIC X(8).                                    
001900*                                 PRODUKTIONSENHET                        
002000     03 KDYTBEH              PIC X(2).                                    
002100*                                 YTBEHANDLINGSKOD                        
002200     03 IDPROJ               PIC X(4).                                    
002300*                                 PARTS PROJEKTIDENTITET                  
002400     03 KDFARLIG             PIC X.                                       
002500*                                 KOD FÖR FARLIGT GODS                    
002600     03 KDBPSR               PIC X.                                       
002700*                                 BASLAGERFÖRSLAGSNIVÅ                    
002800     03 IDKAT-1              PIC X(5).                                    
002900*                                 KATALOGBETECKNING                       
003000     03 IDKAT-2              PIC X(5).                                    
003100*                                 KATALOGBETECKNING                       
003200     03 IDKAT-3              PIC X(5).                                    
003300*                                 KATALOGBETECKNING                       
003400     03 KDUART               PIC X.                                       
003500*                                 UNDANTAGSARTIKEL                        
003600     03 IDPROJK              PIC X(4).                                    
003700*                                 PROJEKTIDENTITET KONSTRUKTION           
003800     03 FLPISK               PIC X.                                       
003900*                                 PISK ARTIKEL                            
004000     03 IDAO                 PIC X(10).                                   
004100*                                 ÄNDRINGSORDERNUMMER                     
004200     03 TISOP-PRINS          PIC 9(6).                                    
004300*                                 DATUM I SEKEL + ÅR + VECKA              
004400     03 IDSKYLT              PIC X(3).                                    
004500*                                 NATIONALITETSTECKEN                     
004600*                                 SPRÅKIDENTIFIKATION                     
004700     03 FLLSRDEL             PIC X.                                       
004800*                                 LEVERERAS SOM RESDEL                    
004900     03 IDPROJUP             PIC X(8).                                    
005000*                                 PROJEKTUPPDRAG                          
005100     03 BEART                PIC X(25).                                   
005200*                                 ARTIKELBENÄMNING                        
005300     03 FLRSBEART            PIC X.                                       
005400*                                 RS-UNIK BENÄMNING                       
005500     03 IDFKNGRP             PIC X(4).                                    
005600*                                 FUNKTIONSGRUPP                          
005700     03 TEORSAK-1            PIC X(50).                                   
005800*                                 INFO OM SLAG AV ÅTGÄRD                  
005900     03 TEARTNOT-2           PIC X(40).                                   
006000*                                 ARTIKEL NOTERING                        
006100     03 IDRITN               PIC X(10).                                   
006200*                                 RITNINGSNUMMER                          
006300     03 TEARTNOT-7           PIC X(40).                                   
006400*                                 ARTIKEL NOTERING                        
006500     03 TEARTNOT-4           PIC X(40).                                   
006600*                                 ARTIKEL NOTERING                        
006700     03 IDARTNR-MOTSV        PIC X(9).                                    
006800*                                 MOTSVARANDE ARTIKEL                     
006900     03 AVSL-MOT             PIC X(40).                                   
007000*                                 ARTIKEL NOTERING                        
007100     03 IDPSN                PIC X(3).                                    
007200*                                 PROPER SHIPPING NAME                    
007300     03 KDARTHNT             PIC X(6).                                    
007400*                                 HANTERINGSKOD                           
007500     03 KDEMBKOD-TEXT        PIC X(20).                                   
007600     03 VLFG                 PIC X(8).                                    
007700*                                 VOLYM FARLIGT GODS                      
007800     03 KDSORT-VLFG          PIC X(4).                                    
007900*                                 SORT-KOD VOLYM FARLIGT GODS             
008000     03 VKART-NTO            PIC 9(8).                                    
008100*                                 ARTIKELNS NETTOVIKT                     
008200     03 TIPRINS-UTC          PIC X(24).                                   
008300     03 IDCDS                PIC X(8).                                    
008400*                                 ANVÄNDARENS CDS ID                      
008500     03 KDARTSYS             PIC X(2).                                    
008600*                                 KOD FÖR SYST. ÄGARE AV ARTIKEL          
008700     03 IN-2133-GRP.                                                      
008800*                                 PRICE DETAILS FOR 2133 FROM TCP         
008900*                                 LM                                      
009000        05 IDLEVNR           PIC X(5).                                    
009100*                                 LEVERANTÖRNUMMER                        
009200        05 IDPLANGR-AG       PIC 9.                                       
009300*                                 PLANERINGSGRUPP ANSKAFFARE              
009400        05 IDANSK            PIC 9(3).                                    
009500*                                 ANSKAFFARNUMMER                         
009600        05 PRARTSTD          PIC X(10).                                   
009700*                                 ARTIKELSTANDARDPRIS                     
009800        05 KVPB-C1           PIC X(8).                                    
009900     03 FILLER               PIC X(50).                                   
010000*                                                                         
010100     03 SUPERSESSION-GRP.                                                 
010200*                                 SUPERSESSION INFO FROM TCPLM            
010300        05 FLSSCHG           PIC X.                                       
010400*                                 FLAG FOR SUPERSESSION CHANGE            
010500        05 DIERS-ERS         PIC X(7).                                    
010600*                                 KVANTITET I ERSÄTTN.                    
010700        05 TIERSDAT-UTC      PIC X(24).                                   
010800        05 KVRADER           PIC 9(5).                                    
010900*                                 ANTAL RADER                             
011000        05 SS-RAD            OCCURS 99 TIMES.                             
011100           07 IDARTNR-TILLK  PIC X(9).                                    
011200*                                 ARTIKELNUMMER                           
011300           07 DIERS-TILLK    PIC X(7).                                    
011400*                                 KVANTITET I ERSÄTTN.                    
011500           07 BEERS-GRP.                                                  
011600*                                 SUPERSESSION TEXT FOR TCPLM             
011700              09 BEERS       OCCURS 10 TIMES                              
011800                             PIC X(20).                                   
011900*                                 ERSÄTTNINGSTEXT                         
012000*** END OF VILMAII-COPY LENGTH= 21934 BYTES                               
