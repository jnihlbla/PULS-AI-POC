000100 01  W011200.                                                             
000200*                                 W011200  = ART.REG. VECKA               
000300*                                 ANVÄNDS AV PERIODSYSTEM OCH EZT         
000400*                                                                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 BEART-ENG            PIC X(25).                                   
000800*                                 ENGELSK ARTIKELBENÄMNING                
000900     03 BEART-SVE            PIC X(25).                                   
001000*                                 SVENSK ARTIKELBENÄMNING                 
001100     03 BEFT                 PIC S9(3)           COMP-3.                  
001200*                                 FÖRPACKNINGSTYP                         
001300     03 FLAVRART             PIC X.                                       
001400*                                 AVROPSARTIKEL                           
001500     03 FLFSP                PIC X.                                       
001600*                                 FÖRDELNINGSSPÄRR ?                      
001700     03 FLIART               PIC X.                                       
001800*                                 INGÅENDE ARTIKEL                        
001900     03 FLINFART             PIC S9              COMP-3.                  
002000*                                 INFORMATION ARTIKELMÄRKNING             
002100     03 FLLSRDEL             PIC X.                                       
002200*                                 LEVERERAS SOM RESDEL                    
002300     03 FLMANAT              PIC X.                                       
002400*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
002500     03 FLMANBK              PIC X.                                       
002600*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
002700     03 FLMANKP              PIC X.                                       
002800*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
002900     03 FLMANLT              PIC X.                                       
003000*                                 MANUELLT SATT LEDTID ?                  
003100     03 FLMANQ               PIC X.                                       
003200*                                 MANUELL HEMTAGNINGSKVANTITET            
003300     03 FLMPB-C1             PIC X.                                       
003400*                                 MASKINELLT SATT PROGNOS C1              
003500     03 FLMPB-C2             PIC X.                                       
003600*                                 MASKINELLT SATT PROGNOS C2              
003700     03 FLPRRAPP             PIC S9              COMP-3.                  
003800*                                 PRISRAPPORTMÄRKNING                     
003900     03 FLSPECPR             PIC S9              COMP-3.                  
004000*                                 SPECIALPRISFLAGGA                       
004100     03 FLTOPP               PIC X.                                       
004200*                                 TOPP-200-ARTIKEL                        
004300     03 IDAETNR              PIC S9(3)           COMP-3.                  
004400*                                 ÄNDRINGSTILLFÄLLENUMMER                 
004500     03 IDANSK               PIC S9(3)           COMP-3.                  
004600*                                 ANSKAFFARNUMMER                         
004700     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
004800*                                 EMBALLAGEARTIKELNR FÖR Q0               
004900     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
005000*                                 EMBALLAGEARTIKELNR FÖR Q1               
005100     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
005200*                                 EMBALLAGEARTIKELNR FÖR Q2               
005300     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
005400*                                 EMBALLAGEARTIKELNR FÖR Q3               
005500     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
005600*                                 EMBALLAGEARTIKELNR FÖR Q4               
005700     03 IDBERED              PIC S9(3)           COMP-3.                  
005800*                                 BEREDARENUMMER                          
005900     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
006000*                                 FUNKTIONSGRUPP                          
006100     03 IDINK                PIC S9(3)           COMP-3.                  
006200*                                 INKÖPARNUMMER                           
006300     03 IDLEVNR              PIC S9(5)           COMP-3.                  
006400*                                 LEVERANTÖRNUMMER                        
006500     03 IDLKTO               PIC S9(7)           COMP-3.                  
006600*                                 LAGERKONTO                              
006700     03 IDPLANGR-AG          PIC S9              COMP-3.                  
006800*                                 PLANERINGSGRUPP ANSKAFFARE              
006900     03 IDPLANGR-LEV         PIC S9              COMP-3.                  
007000*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
007100     03 IDPROD               PIC S9(3)           COMP-3.                  
007200*                                 PRODUKTKOD                              
007300     03 KDAGENT              PIC S9              COMP-3.                  
007400*                                 AGENTKOD                                
007500     03 KDARTHNT             PIC S9(7)           COMP-3.                  
007600*                                 HANTERINGSKOD                           
007700     03 KDARTURS             PIC S9(3)           COMP-3.                  
007800*                                 ARTIKELURSPRUNGSKOD                     
007900     03 KDAVT                PIC S9              COMP-3.                  
008000*                                 AVTALSMÄRKNING                          
008100     03 KDBPSR               PIC S9              COMP-3.                  
008200*                                 BASLAGERFÖRSLAGSNIVÅ                    
008300     03 KDEMBKOD-0           PIC S9(3)           COMP-3.                  
008400*                                 EMBALLAGEKOD 0                          
008500     03 KDEMBKOD-1           PIC S9(3)           COMP-3.                  
008600*                                 EMBALLAGEKOD 1                          
008700     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
008800*                                 EMBALLAGEKOD 2                          
008900     03 KDERS                PIC S9(3)           COMP-3.                  
009000*                                 ERSÄTTNINGSKOD                          
009100     03 KDFARLIG             PIC S9              COMP-3.                  
009200*                                 KOD FÖR FARLIGT GODS                    
009300     03 KDFORP               PIC S9(5)           COMP-3.                  
009400*                                 FÖRPACKNINGSKOD                         
009500     03 KDGK                 PIC S9              COMP-3.                  
009600*                                 GODSMOTTAGAREKOD                        
009700     03 KDHF                 PIC S9              COMP-3.                  
009800*                                 HUVUDFÖRRÅDSMÄRKNING                    
009900     03 KDKG                 PIC S9              COMP-3.                  
010000*                                 KURANSGRUPP                             
010100     03 KDKONTR              PIC S9(5)           COMP-3.                  
010200*                                 KVALITETSKONTROLLKOD                    
010300     03 KDKSP                PIC S9              COMP-3.                  
010400*                                 KÖPSPÄRR                                
010500     03 KDLEVSP              PIC S9(3)           COMP-3.                  
010600*                                 SPÄRRKOD LEVERANS                       
010700     03 KDLPSP               PIC S9              COMP-3.                  
010800*                                 LEVERANSPLANESPÄRR                      
010900     03 KDLTK                PIC S9              COMP-3.                  
011000*                                 LAGERTILLHÖRIGHETSKOD                   
011100     03 KDPRIO               PIC S9              COMP-3.                  
011200*                                 PRIORITETSKOD                           
011300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
011400*                                 PRODUKTSLAG                             
011500     03 KDPRTILL             PIC S9              COMP-3.                  
011600*                                 PRISTILLÄMPNINGSKOD                     
011700     03 KDRABATT             PIC S9(3)           COMP-3.                  
011800*                                 RABATTKOD                               
011900     03 KDSORT               PIC X(2).                                    
012000*                                 SORT-KOD                                
012100     03 KDSRA                PIC S9(3)           COMP-3.                  
012200*                                 SRA-KOD                                 
012300     03 KDTIPPR              PIC S9              COMP-3.                  
012400*                                 TIPPAT PRIS KOD                         
012500     03 KDUART               PIC X.                                       
012600*                                 UNDANTAGSARTIKEL                        
012700     03 KDVOLART             PIC S9              COMP-3.                  
012800*                                 VOLYMARTIKELKOD                         
012900     03 KDVSOP               PIC S9(3)           COMP-3.                  
013000*                                 VSOP-KOD                                
013100     03 KDVTH                PIC S9              COMP-3.                  
013200*                                 VOLVO TILLHANDAHÅLLER MÄRKNING          
013300     03 KDVVKL               PIC S9              COMP-3.                  
013400*                                 VOLYMVÄRDESKLASS                        
013500     03 KVAKS                PIC S9(7)           COMP-3.                  
013600*                                 ANKOMSTSALDO                            
013700     03 KVAKS-E              PIC S9(7)           COMP-3.                  
013800*                                 DEL AV EFR TILL ANDRA CLAGRET           
013900     03 KVAL                 PIC S9              COMP-3.                  
014000*                                 ANTAL LEVERANTÖRER                      
014100     03 KVBK                 PIC S9(7)           COMP-3.                  
014200*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
014300     03 KVBR-TOT             PIC S9(7)           COMP-3.                  
014400*                                 TOT BEST REST                           
014500     03 KVDOSALD             PIC S9(7)           COMP-3.                  
014600*                                 DIVERSEORDERSALDO                       
014700     03 KVEFRS               PIC S9(7)           COMP-3.                  
014800*                                 EJ-FAKTURERADE-RADER SALDO              
014900     03 KVKP                 PIC S9(7)           COMP-3.                  
015000*                                 KÖPPUNKT                                
015100     03 KVLAAN               PIC S9(7)           COMP-3.                  
015200*                                 LÅNESALDO                               
015300     03 KVLS                 PIC S9(7)           COMP-3.                  
015400*                                 LAGERSALDO                              
015500     03 KVMP                 PIC S9(7)           COMP-3.                  
015600*                                 MAXPUNKT                                
015700     03 KVOVERF              PIC S9(7)           COMP-3.                  
015800*                                 ÖVERFÖRINGSSALDO                        
015900     03 KVPALL               PIC S9(7)           COMP-3.                  
016000*                                 ANTAL PALLAR                            
016100     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
016200*                                 SATS-PERIODBEHOV                        
016300     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
016400*                                 TOTALT PERIODBEHOV                      
016500     03 KVQ                  PIC S9(7)           COMP-3.                  
016600*                                 EKONOMISK HEMTAGNINGSKVANTITET          
016700     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
016800*                                 ANTAL I Q0 FÖRPACKNING                  
016900     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
017000*                                 ANTAL I Q1 FÖRPACKNING                  
017100     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
017200*                                 ANTAL I Q2 FÖRPACKNING                  
017300     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
017400*                                 ANTAL I Q3 FÖRPACKNING                  
017500     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
017600*                                 ANTAL I Q4 FÖRPACKNING                  
017700     03 KVRESS               PIC S9(7)           COMP-3.                  
017800*                                 RESERVERAT ANTAL ARTIKLAR               
017900     03 KVROS                PIC S9(7)           COMP-3.                  
018000*                                 RESTORDERSALDO                          
018100     03 KVSLAGER             PIC S9(7)           COMP-3.                  
018200*                                 SÄKERHETSLAGER                          
018300     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
018400*                                 SLUTKÖPSSALDO                           
018500     03 KVVECKOR-AT          PIC S9(3)           COMP-3.                  
018600*                                 ANTAL VECKOR ANSKAFFNINGSTID            
018700     03 KVVECKOR-BT          PIC S9(3)           COMP-3.                  
018800*                                 ANTAL VECKOR BESTÄLLNINGSTID            
018900     03 KVVECKOR-FT          PIC S9(3)           COMP-3.                  
019000*                                 ANTAL VECKOR FRYSNINGSTID               
019100     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
019200*                                 ANTAL VECKOR LEDTID                     
019300     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
019400*                                 BESTÄLLNINGSPRIS I KRONOR               
019500     03 PRARTBTO-SVE         PIC S9(7)V9(2)      COMP-3.                  
019600*                                 CIRKAPRIS                               
019700     03 PRARTBTO-UTL         PIC S9(7)V9(2)      COMP-3.                  
019800*                                 EXPORTPRIS                              
019900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
020000*                                 ARTIKELNS SJÄLVKOSTNAD                  
020100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
020200*                                 ARTIKELSTANDARDPRIS                     
020300     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
020400*                                 INKÖPSPRIS                              
020500     03 REF                  PIC S9V9(2)         COMP-3.                  
020600*                                 FÖRDELNINGSFAKTOR                       
020700     03 REKSIFFR             PIC S9              COMP-3.                  
020800*                                 KONTROLLSIFFRA                          
020900     03 REOMRTAL-DO          PIC S9(2)V9(3)      COMP-3.                  
021000*                                 OMRÄKNINGSTAL DAGORDER                  
021100     03 REOMRTAL-MO          PIC S9(2)V9(3)      COMP-3.                  
021200*                                 OMRÄKNINGSTAL KVANTORDER                
021300     03 REPROCFP             PIC S9V9(2)         COMP-3.                  
021400*                                 PROCENT-FÖRPACKNING                     
021500     03 RESLJUST-C1          PIC S9(2)V9(1)      COMP-3.                  
021600*                                 SÄKERHETSLAGER JUST                     
021700     03 RESLJUST-C2          PIC S9(2)V9(1)      COMP-3.                  
021800*                                 SÄKERHETSLAGER JUST                     
021900     03 TIAAVVD-REG          PIC S9(5)           COMP-3.                  
022000*                                 REGISTRERINGSDATUM                      
022100     03 TIERSDAT             PIC S9(5)           COMP-3.                  
022200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
022300     03 TIFINLV              PIC S9(5)           COMP-3.                  
022400*                                 PUBLICERINGSVECKA                       
022500     03 TILPSP               PIC S9(5)           COMP-3.                  
022600*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
022700     03 TISLJUST-C1          PIC S9(5)           COMP-3.                  
022800*                                 DAT SÄK-LAG-JUST-FAKT C1                
022900     03 TISLJUST-C2          PIC S9(5)           COMP-3.                  
023000*                                 DAT SÄK-LAG-JUST-FAKT C2                
023100     03 TIURPROD             PIC S9(5)           COMP-3.                  
023200*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
023300     03 VKART                PIC S9(7)           COMP-3.                  
023400*                                 ARTIKELVIKT (G)                         
023500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
023600*                                 ARTIKELVOLYM NETTO (CM3)                
023700     03 PRARTBTO-N           PIC S9(7)V9(2)      COMP-3.                  
023800*                                 KOMMANDE BRUTTOPRIS EXPORT FOB          
023900*                                 (SAMMA SOM PRARTBTO-EXP-N)              
024000     03 PRARTBTO-SVE-N       PIC S9(7)V9(2)      COMP-3.                  
024100*                                 KOMMANDE CIRKAPRIS                      
024200     03 IDPROJ               PIC X(4).                                    
024300*                                 PROJEKTIDENTITET                        
024400     03 IDPROJUP             PIC X(8).                                    
024500*                                 PROJEKTUPPDRAG                          
024600     03 KDYTBEH              PIC S9(3)           COMP-3.                  
024700*                                 YTBEHANDLINGSKOD                        
024800     03 PRARTBES-PR          PIC S9(7)V9(2)      COMP-3.                  
024900*                                 DETTA BESTÄLLNINGSPRIS (KR)             
025000     03 PRARTBEL-PR          PIC S9(8)V9(3)      COMP-3.                  
025100*                                 DETTA BESTÄLLNINGSPRIS                  
025200*                                 (I LEVERANTÖRENS VALUTA)                
025300*** END COPY W011200CC0  LENGTH=378                                       
