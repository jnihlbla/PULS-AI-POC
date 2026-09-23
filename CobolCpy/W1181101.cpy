000100 01  W1181101.                                                            
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDARTNR              PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 IDPRODNR             PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 KDERS                PIC 9(2).                                    
000900*                                 ERSÄTTNINGSKOD                          
001000     03 IDBERED              PIC 9(2).                                    
001100*                                 BEREDARENUMMER                          
001200     03 KDPRODSL             PIC 9(2).                                    
001300*                                 PRODUKTSLAG                             
001400     03 KDSORT               PIC X(2).                                    
001500*                                 SORT-KOD                                
001600     03 IDPROENH             PIC X(8).                                    
001700*                                 PRODUKTIONSENHET                        
001800     03 KDYTBEH              PIC 9(2).                                    
001900*                                 YTBEHANDLINGSKOD                        
002000     03 IDPROJ               PIC X(4).                                    
002100*                                 PARTS PROJEKTIDENTITET                  
002200     03 KDFARLIG             PIC 9.                                       
002300*                                 KOD FÖR FARLIGT GODS                    
002400     03 KDBPSR               PIC 9.                                       
002500*                                 BASLAGERFÖRSLAGSNIVÅ                    
002600     03 IDKAT-1              PIC X(5).                                    
002700*                                 KATALOGBETECKNING                       
002800     03 IDKAT-2              PIC X(5).                                    
002900*                                 KATALOGBETECKNING                       
003000     03 IDKAT-3              PIC X(5).                                    
003100*                                 KATALOGBETECKNING                       
003200     03 KDUART               PIC X.                                       
003300*                                 UNDANTAGSARTIKEL                        
003400     03 IDPROJK              PIC X(4).                                    
003500*                                 PROJEKTIDENTITET KONSTRUKTION           
003600     03 FLPISK               PIC X.                                       
003700*                                 PISK ARTIKEL                            
003800     03 IDAO                 PIC X(10).                                   
003900*                                 ÄNDRINGSORDERNUMMER                     
004000     03 TISOP                PIC 9(5).                                    
004100*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
004200     03 IDSKYLT              PIC X(3).                                    
004300*                                 NATIONALITETSTECKEN                     
004400*                                 SPRÅKIDENTIFIKATION                     
004500     03 FLLSRDEL             PIC X.                                       
004600*                                 LEVERERAS SOM RESDEL                    
004700     03 IDPROJUP             PIC X(8).                                    
004800*                                 PROJEKTUPPDRAG                          
004900     03 BEART                PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100     03 FLRSBEART            PIC X.                                       
005200*                                 RS-UNIK BENÄMNING                       
005300     03 IDFKNGRP             PIC 9(4).                                    
005400*                                 FUNKTIONSGRUPP                          
005500     03 TEORSAK-1            PIC X(50).                                   
005600*                                 INFO OM SLAG AV ÅTGÄRD                  
005700     03 TEARTNOT-2           PIC X(40).                                   
005800*                                 ARTIKEL NOTERING                        
005900     03 IDRITN               PIC X(10).                                   
006000*                                 RITNINGSNUMMER                          
006100     03 TEARTNOT-7           PIC X(40).                                   
006200*                                 ARTIKEL NOTERING                        
006300     03 TEARTNOT-4           PIC X(40).                                   
006400*                                 ARTIKEL NOTERING                        
006500     03 IDARTNR-MOTSV        PIC X(9).                                    
006600*                                 MOTSVARANDE ARTIKEL                     
006700     03 AVSL-MOT             PIC X(40).                                   
006800*                                 ARTIKEL NOTERING                        
006900     03 IDPSN                PIC 9(3).                                    
007000*                                 PROPER SHIPPING NAME                    
007100     03 KDARTHNT             PIC 9(6).                                    
007200*                                 HANTERINGSKOD                           
007300     03 KDEMBKOD-2           PIC 9(3).                                    
007400*                                 EMBALLAGEKOD 2                          
007500     03 VLFG                 PIC 9(4)V9(3).                               
007600*                                 VOLYM FARLIGT GODS                      
007700     03 KDSORT-VLFG          PIC X(4).                                    
007800*                                 SORT-KOD VOLYM FARLIGT GODS             
007900     03 VKART-NTO            PIC 9(8).                                    
008000*                                 ARTIKELNS NETTOVIKT                     
008100     03 IDCDS                PIC X(8).                                    
008200*                                 ANVÄNDARENS CDS ID                      
008300     03 KDARTSYS             PIC X(2).                                    
008400*                                 KOD FÖR SYST. ÄGARE AV ARTIKEL          
008500     03 IN-2133-GRP.                                                      
008600*                                 PRICE DETAILS FOR 2133 FROM TCP         
008700*                                 LM                                      
008800        05 IDLEVNR           PIC X(5).                                    
008900*                                 LEVERANTÖRNUMMER                        
009000        05 IDPLANGR-AG       PIC 9.                                       
009100*                                 PLANERINGSGRUPP ANSKAFFARE              
009200        05 IDANSK            PIC 9(3).                                    
009300*                                 ANSKAFFARNUMMER                         
009400        05 PRARTSTD          PIC X(10).                                   
009500*                                 ARTIKELSTANDARDPRIS                     
009600        05 KVPB-C1           PIC X(8).                                    
009700     03 FILLER               PIC X(50).                                   
009800*                                                                         
009900     03 SUPERSESSION-GRP.                                                 
010000*                                 SUPERSESSION INFO FROM TCPLM            
010100        05 DIERS-ERS         PIC X(7).                                    
010200*                                 KVANTITET I ERSÄTTN.                    
010300        05 TIERSDAT-PREL     PIC X(5).                                    
010400*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
010500        05 KVRADER-MAX9      PIC 9(5).                                    
010600*                                 MAX INDEX KOPPLAT TILL OCCURS N         
010700*                                 EDAN.                                   
010800        05 SS-RAD            OCCURS 1 TO 99 TIMES                         
010900                             DEPENDING ON KVRADER-MAX9.                   
011000           07 IDARTNR-TILLK  PIC X(9).                                    
011100*                                 ARTIKELNUMMER                           
011200           07 DIERS-TILLK    PIC X(7).                                    
011300*                                 KVANTITET I ERSÄTTN.                    
011400           07 BEERS-GRP.                                                  
011500*                                 SUPERSESSION TEXT FOR TCPLM             
011600              09 BEERS       OCCURS 10 TIMES                              
011700                             PIC X(20).                                   
011800*                                 ERSÄTTNINGSTEXT                         
011900*** END OF VILMAII-COPY LENGTH= 21871 BYTES                               
