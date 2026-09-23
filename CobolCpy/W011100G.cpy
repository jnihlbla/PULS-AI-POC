000100 01  W011100.                                                             
000200*                                 W011100 = LAGERBAND DAG 9-KANAL         
000300*                                 SPEGLAR ARTIKEL- OCH CDC-INFO           
000400*                                 FRAMSTÄLLS KVÄLL OCH VECKOSLUT          
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 ADGANG               PIC S9(3)           COMP-3.                  
000800*                                 GÅNG                                    
000900     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001000*                                 LAGEROMRÅDE                             
001100     03 ADPLATS              PIC S9(5)           COMP-3.                  
001200*                                 LAGERPLATSNUMMER                        
001300     03 BEART-ENG            PIC X(25).                                   
001400*                                 ENGELSK ARTIKELBENÄMNING                
001500     03 BEART-FRA            PIC X(25).                                   
001600*                                 FRANSK ARTIKELBENÄMNING                 
001700     03 BEART-SPA            PIC X(25).                                   
001800*                                 SPANSK ARTIKELBENÄMNING                 
001900     03 BEART-SVE            PIC X(25).                                   
002000*                                 SVENSK ARTIKELBENÄMNING                 
002100     03 BEART-TYS            PIC X(25).                                   
002200*                                 TYSK ARTIKELBENÄMNING                   
002300     03 BEFT                 PIC S9(3)           COMP-3.                  
002400*                                 FÖRPACKNINGSTYP                         
002500     03 FLAVRART             PIC X.                                       
002600*                                 AVROPSARTIKEL                           
002700     03 FLIART               PIC X.                                       
002800*                                 ARTIKELN INGÅR I SATS                   
002900     03 FLLSRDEL             PIC X.                                       
003000*                                 LEVERERAS SOM RESDEL                    
003100     03 FLLTKSP              PIC X.                                       
003200*                                 SPÄRR UTLEVERANS C2-LAGER               
003300     03 FLMANAT              PIC X.                                       
003400*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
003500     03 FLMANBK              PIC X.                                       
003600*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
003700     03 FLMANKP              PIC X.                                       
003800*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
003900     03 FLMANLT              PIC X.                                       
004000*                                 MANUELLT SATT LEDTID ?                  
004100     03 FLMANPB              PIC X.                                       
004200*                                 MANUELLT REGISTRERAT PB-TPO             
004300     03 FLMANQ               PIC X.                                       
004400*                                 MANUELL HEMTAGNINGSKVANTITET            
004500     03 FLSPECPR             PIC X.                                       
004600*                                 SPECIALPRISFLAGGA                       
004700     03 FLTOPP               PIC X.                                       
004800*                                 TOPP-200-ARTIKEL                        
004900     03 IDANSK               PIC S9(3)           COMP-3.                  
005000*                                 ANSKAFFARNUMMER                         
005100     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
005200*                                 EMBALLAGEARTIKELNR FÖR Q0               
005300     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
005400*                                 EMBALLAGEARTIKELNR FÖR Q1               
005500     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
005600*                                 EMBALLAGEARTIKELNR FÖR Q2               
005700     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
005800*                                 EMBALLAGEARTIKELNR FÖR Q3               
005900     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
006000*                                 EMBALLAGEARTIKELNR FÖR Q4               
006100     03 IDBERED              PIC S9(3)           COMP-3.                  
006200*                                 BEREDARENUMMER                          
006300     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
006400*                                 FUNKTIONSGRUPP                          
006500     03 IDINK                PIC S9(3)           COMP-3.                  
006600*                                 INKÖPARNUMMER                           
006700     03 IDLEVNR              PIC S9(5)           COMP-3.                  
006800*                                 LEVERANTÖRNUMMER                        
006900     03 IDLKTO               PIC S9(7)           COMP-3.                  
007000*                                 LAGERKONTO (FFHHHUU)                    
007100     03 IDPLANGR-AG          PIC S9              COMP-3.                  
007200*                                 PLANERINGSGRUPP ANSKAFFARE              
007300     03 IDPLANGR-LEV         PIC S9              COMP-3.                  
007400*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
007500     03 IDPROJ               PIC X(4).                                    
007600*                                 PARTS PROJEKTIDENTITET                  
007700     03 IDPROJUP             PIC X(8).                                    
007800*                                 PROJEKTUPPDRAG                          
007900     03 KDARTHNT             PIC S9(7)           COMP-3.                  
008000*                                 HANTERINGSKOD                           
008100     03 KDARTURS             PIC X(2).                                    
008200*                                 ARTIKELURSPRUNGSKOD                     
008300     03 KDAVT                PIC S9              COMP-3.                  
008400*                                 AVTALSMÄRKNING                          
008500     03 KDBPSR               PIC S9              COMP-3.                  
008600*                                 BASLAGERFÖRSLAGSNIVÅ                    
008700     03 KDEMBKOD-0           PIC S9(3)           COMP-3.                  
008800*                                 EMBALLAGEKOD 0                          
008900     03 KDEMBKOD-1           PIC S9(3)           COMP-3.                  
009000*                                 EMBALLAGEKOD 1                          
009100     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
009200*                                 EMBALLAGEKOD 2                          
009300     03 KDERS                PIC S9(3)           COMP-3.                  
009400*                                 ERSÄTTNINGSKOD                          
009500     03 KDFARLIG             PIC S9              COMP-3.                  
009600*                                 KOD FÖR FARLIGT GODS                    
009700     03 KDFORP               PIC S9(5)           COMP-3.                  
009800*                                 FÖRPACKNINGSKOD                         
009900     03 KDGK                 PIC S9              COMP-3.                  
010000*                                 GODSMOTTAGAREKOD                        
010100     03 KDHF                 PIC S9              COMP-3.                  
010200*                                 HUVUDFÖRRÅDSMÄRKNING                    
010300     03 KDKG                 PIC S9              COMP-3.                  
010400*                                 KURANSGRUPP                             
010500     03 KDKSP                PIC S9              COMP-3.                  
010600*                                 KÖPSPÄRR                                
010700     03 KDLEVSP              PIC S9(3)           COMP-3.                  
010800*                                 SPÄRRKOD LEVERANS                       
010900     03 KDLTK                PIC S9              COMP-3.                  
011000*                                 LAGERTILLHÖRIGHETSKOD                   
011100     03 KDLPSP               PIC S9              COMP-3.                  
011200*                                 LEVERANSPLANESPÄRR                      
011300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
011400*                                 PRODUKTSLAG                             
011500     03 KDPRTILL             PIC S9              COMP-3.                  
011600*                                 PRISTILLÄMPNINGSKOD                     
011700     03 KDSORT               PIC X(2).                                    
011800*                                 SORT-KOD                                
011900     03 KDSRA                PIC S9(3)           COMP-3.                  
012000*                                 SRA-KOD                                 
012100     03 KDTIPPR              PIC S9              COMP-3.                  
012200*                                 TIPPAT PRIS KOD                         
012300     03 KDUART               PIC X.                                       
012400*                                 UNDANTAGSARTIKEL                        
012500     03 KDVSOP               PIC S9(3)           COMP-3.                  
012600*                                 VSOP-KOD                                
012700     03 KDVTH                PIC S9              COMP-3.                  
012800*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
012900     03 KDVVKL               PIC S9              COMP-3.                  
013000*                                 VOLYMVÄRDESKLASS                        
013100     03 KDYTBEH              PIC S9(3)           COMP-3.                  
013200*                                 YTBEHANDLINGSKOD                        
013300     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
013400*                                 DEL AV AK SOM LIGGER I CDC              
013500     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
013600*                                 DEL AV AK PÅ VÄG                        
013700     03 KVAKS-T              PIC S9(7)           COMP-3.                  
013800*                                 DEL AV AK I EN TERMINAL                 
013900     03 KVBK                 PIC S9(7)           COMP-3.                  
014000*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
014100     03 KVBR-TOT             PIC S9(7)           COMP-3.                  
014200*                                 TOT BEST REST                           
014300     03 KVEFRS               PIC S9(7)           COMP-3.                  
014400*                                 EJ FAKTURERAT ANTAL STYCK               
014500     03 KVKP                 PIC S9(7)           COMP-3.                  
014600*                                 KÖPPUNKT                                
014700     03 KVLAAN               PIC S9(7)           COMP-3.                  
014800*                                 LÅNESALDO                               
014900     03 KVLS                 PIC S9(7)           COMP-3.                  
015000*                                 LAGERSALDO                              
015100     03 KVMP                 PIC S9(7)           COMP-3.                  
015200*                                 MAXPUNKT                                
015300     03 KVOVERF              PIC S9(7)           COMP-3.                  
015400*                                 ÖVERFÖRINGSSALDO                        
015500     03 KVPALL               PIC S9(7)           COMP-3.                  
015600*                                 ANTAL I PALL                            
015700     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
015800*                                 SATS-PERIODBEHOV                        
015900     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
016000*                                 SEPARAT PERIODBEHOV                     
016100     03 KVQ                  PIC S9(7)           COMP-3.                  
016200*                                 EKONOMISK HEMTAGNINGSKVANTITET          
016300     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
016400*                                 ANTAL I Q0 FÖRPACKNING                  
016500     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
016600*                                 ANTAL I Q1 FÖRPACKNING                  
016700     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
016800*                                 ANTAL I Q2 FÖRPACKNING                  
016900     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
017000*                                 ANTAL I Q3 FÖRPACKNING                  
017100     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
017200*                                 ANTAL I Q4 FÖRPACKNING                  
017300     03 KVRESS               PIC S9(7)           COMP-3.                  
017400*                                 RESERVERAT ANTAL ARTIKLAR               
017500     03 KVROS                PIC S9(7)           COMP-3.                  
017600*                                 RESTORDERSALDO                          
017700     03 KVSLAGER             PIC S9(7)           COMP-3.                  
017800*                                 SÄKERHETSLAGER                          
017900     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
018000*                                 SLUTKÖPSSALDO                           
018100     03 KVSPANT              PIC S9(7)           COMP-3.                  
018200*                                 SPÄRRAT ANTAL                           
018300     03 KVUTRS               PIC S9(7)           COMP-3.                  
018400*                                 UTREDNINGSSALDO                         
018500     03 KVVECKOR-AT          PIC S9(3)           COMP-3.                  
018600*                                 ANTAL VECKOR ANSKAFFNINGSTID            
018700     03 KVVECKOR-BT          PIC S9(3)           COMP-3.                  
018800*                                 ANTAL VECKOR BESTÄLLNINGSTID            
018900     03 KVVECKOR-FT          PIC S9(3)           COMP-3.                  
019000*                                 ANTAL VECKOR FRYSNINGSTID               
019100     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
019200*                                 ANTAL VECKOR LEDTID                     
019300     03 PRARTBEL-PR          PIC S9(8)V9(3)      COMP-3.                  
019400*                                 DETTA BESTÄLLNINGSPRIS                  
019500*                                 (I LEVERANTÖRENS VALUTA)                
019600     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
019700*                                 BESTÄLLNINGSPRIS I KRONOR               
019800     03 PRARTBES-PR          PIC S9(7)V9(2)      COMP-3.                  
019900*                                 DETTA BESTÄLLNINGSPRIS (KR)             
020000     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
020100*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
020200     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
020300*                                 ARTIKELNS SJÄLVKOSTNAD                  
020400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
020500*                                 ARTIKELSTANDARDPRIS                     
020600     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
020700*                                 INKÖPSPRIS                              
020800     03 REKSIFFR             PIC S9              COMP-3.                  
020900*                                 KONTROLLSIFFRA                          
021000     03 REPROCFP             PIC S9V9(2)         COMP-3.                  
021100*                                 PROCENT-FÖRPACKNING                     
021200     03 RESLJUST             PIC S9(2)V9(1)      COMP-3.                  
021300*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
021400     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
021500*                                 TPO-KVANTITET, TOTAL                    
021600     03 TIERSDAT             PIC S9(5)           COMP-3.                  
021700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
021800     03 TIFINLV              PIC S9(5)           COMP-3.                  
021900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
022000     03 TIINVDAT             PIC S9(5)           COMP-3.                  
022100*                                 INVENTERINGSDATUM                       
022200     03 TILPSP               PIC S9(5)           COMP-3.                  
022300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
022400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
022500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
022600     03 TIRODAT              PIC S9(5)           COMP-3.                  
022700*                                 RESTORDERDATUM      TIRODAT-002         
022800*                                 (AAVVD)                                 
022900     03 TISLJUST             PIC S9(5)           COMP-3.                  
023000*                                 VECKA DÅ JUSTERING AV SÄKER-            
023100*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
023200     03 TIURPROD             PIC S9(5)           COMP-3.                  
023300*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
023400     03 VKART                PIC S9(7)           COMP-3.                  
023500*                                 ARTIKELVIKT (G)                         
023600     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
023700*                                 ARTIKELVOLYM NETTO (CM3)                
023800*** END OF VILMAII-COPY LENGTH= 435 BYTES                                 
