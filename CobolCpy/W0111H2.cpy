000100 01  W011100.                                                             
000200*                                 W011100 = LAGERBAND DAG 9-KANAL         
000300*                                 SPEGLAR ARTIKEL- OCH CDC-INFO           
000400*                                 FRAMSTÄLLS KVÄLL OCH VECKOSLUT          
000500*                                 GÄLLER LAGERBAND SOM FRAMS-             
000600*                                 STÄLLTS MELLAN APRIL 95 OCH             
000700*                                 MAJ 97 (KANADAINST)                     
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 ADGANG               PIC S9(3)           COMP-3.                  
001100*                                 GÅNG                                    
001200     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001300*                                 LAGEROMRÅDE                             
001400     03 ADPLATS              PIC S9(5)           COMP-3.                  
001500*                                 LAGERPLATSNUMMER                        
001600     03 BEART-ENG            PIC X(25).                                   
001700*                                 ENGELSK ARTIKELBENÄMNING                
001800     03 BEART-FRA            PIC X(25).                                   
001900*                                 FRANSK ARTIKELBENÄMNING                 
002000     03 BEART-SPA            PIC X(25).                                   
002100*                                 SPANSK ARTIKELBENÄMNING                 
002200     03 BEART-SVE            PIC X(25).                                   
002300*                                 SVENSK ARTIKELBENÄMNING                 
002400     03 BEART-TYS            PIC X(25).                                   
002500*                                 TYSK ARTIKELBENÄMNING                   
002600     03 BEFT                 PIC S9(3)           COMP-3.                  
002700*                                 FÖRPACKNINGSTYP                         
002800     03 FLAVRART             PIC X.                                       
002900*                                 AVROPSARTIKEL                           
003000     03 FLIART               PIC X.                                       
003100*                                 ARTIKELN INGÅR I SATS                   
003200     03 FLLSRDEL             PIC X.                                       
003300*                                 LEVERERAS SOM RESDEL                    
003400     03 FLLTKSP              PIC X.                                       
003500*                                 SPÄRR UTLEVERANS C2-LAGER               
003600     03 FLMANAT              PIC X.                                       
003700*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
003800     03 FLMANBK              PIC X.                                       
003900*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
004000     03 FLMANKP              PIC X.                                       
004100*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
004200     03 FLMANLT              PIC X.                                       
004300*                                 MANUELLT SATT LEDTID ?                  
004400     03 FLMANPB              PIC X.                                       
004500*                                 MANUELLT REGISTRERAT PB-TPO             
004600     03 FLMANQ               PIC X.                                       
004700*                                 MANUELL HEMTAGNINGSKVANTITET            
004800     03 FLSPECPR             PIC X.                                       
004900*                                 SPECIALPRISFLAGGA                       
005000     03 FLTOPP               PIC X.                                       
005100*                                 TOPP-200-ARTIKEL                        
005200     03 IDANSK               PIC S9(3)           COMP-3.                  
005300*                                 ANSKAFFARNUMMER                         
005400     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
005500*                                 EMBALLAGEARTIKELNR FÖR Q0               
005600     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
005700*                                 EMBALLAGEARTIKELNR FÖR Q1               
005800     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
005900*                                 EMBALLAGEARTIKELNR FÖR Q2               
006000     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
006100*                                 EMBALLAGEARTIKELNR FÖR Q3               
006200     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
006300*                                 EMBALLAGEARTIKELNR FÖR Q4               
006400     03 IDBERED              PIC S9(3)           COMP-3.                  
006500*                                 BEREDARENUMMER                          
006600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
006700*                                 FUNKTIONSGRUPP                          
006800     03 IDINK                PIC S9(3)           COMP-3.                  
006900*                                 INKÖPARNUMMER                           
007000     03 IDLEVNR              PIC S9(5)           COMP-3.                  
007100*                                 LEVERANTÖRNUMMER                        
007200     03 IDLKTO               PIC S9(7)           COMP-3.                  
007300*                                 LAGERKONTO (FFHHHUU)                    
007400     03 IDPLANGR-AG          PIC S9              COMP-3.                  
007500*                                 PLANERINGSGRUPP ANSKAFFARE              
007600     03 IDPLANGR-LEV         PIC S9              COMP-3.                  
007700*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
007800     03 IDPROJ               PIC X(4).                                    
007900*                                 PARTS PROJEKTIDENTITET                  
008000     03 IDPROJUP             PIC X(8).                                    
008100*                                 PROJEKTUPPDRAG                          
008200     03 KDARTHNT             PIC S9(7)           COMP-3.                  
008300*                                 HANTERINGSKOD                           
008400     03 KDARTURS             PIC X(2).                                    
008500*                                 ARTIKELURSPRUNGSKOD                     
008600     03 KDAVT                PIC S9              COMP-3.                  
008700*                                 AVTALSMÄRKNING                          
008800     03 KDBPSR               PIC S9              COMP-3.                  
008900*                                 BASLAGERFÖRSLAGSNIVÅ                    
009000     03 KDEMBKOD-0           PIC S9(3)           COMP-3.                  
009100*                                 EMBALLAGEKOD 0                          
009200     03 KDEMBKOD-1           PIC S9(3)           COMP-3.                  
009300*                                 EMBALLAGEKOD 1                          
009400     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
009500*                                 EMBALLAGEKOD 2                          
009600     03 KDERS                PIC S9(3)           COMP-3.                  
009700*                                 ERSÄTTNINGSKOD                          
009800     03 KDFARLIG             PIC S9              COMP-3.                  
009900*                                 KOD FÖR FARLIGT GODS                    
010000     03 KDFORP               PIC S9(5)           COMP-3.                  
010100*                                 FÖRPACKNINGSKOD                         
010200     03 KDGK                 PIC S9              COMP-3.                  
010300*                                 GODSMOTTAGAREKOD                        
010400     03 KDHF                 PIC S9              COMP-3.                  
010500*                                 HUVUDFÖRRÅDSMÄRKNING                    
010600     03 KDKG                 PIC S9              COMP-3.                  
010700*                                 KURANSGRUPP                             
010800     03 KDKSP                PIC S9              COMP-3.                  
010900*                                 KÖPSPÄRR                                
011000     03 KDLEVSP              PIC S9(3)           COMP-3.                  
011100*                                 SPÄRRKOD LEVERANS                       
011200     03 KDLTK                PIC S9              COMP-3.                  
011300*                                 LAGERTILLHÖRIGHETSKOD                   
011400     03 KDLPSP               PIC S9              COMP-3.                  
011500*                                 LEVERANSPLANESPÄRR                      
011600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
011700*                                 PRODUKTSLAG                             
011800     03 KDPRTILL             PIC S9              COMP-3.                  
011900*                                 PRISTILLÄMPNINGSKOD                     
012000     03 KDSORT               PIC X(2).                                    
012100*                                 SORT-KOD                                
012200     03 KDSRA                PIC S9(3)           COMP-3.                  
012300*                                 SRA-KOD                                 
012400     03 KDTIPPR              PIC S9              COMP-3.                  
012500*                                 TIPPAT PRIS KOD                         
012600     03 KDUART               PIC X.                                       
012700*                                 UNDANTAGSARTIKEL                        
012800     03 KDVSOP               PIC S9(3)           COMP-3.                  
012900*                                 VSOP-KOD                                
013000     03 KDVTH                PIC S9              COMP-3.                  
013100*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
013200     03 KDVVKL               PIC S9              COMP-3.                  
013300*                                 VOLYMVÄRDESKLASS                        
013400     03 KDYTBEH              PIC S9(3)           COMP-3.                  
013500*                                 YTBEHANDLINGSKOD                        
013600     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
013700*                                 DEL AV AK SOM LIGGER I CDC              
013800     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
013900*                                 DEL AV AK PÅ VÄG                        
014000     03 KVAKS-T              PIC S9(7)           COMP-3.                  
014100*                                 DEL AV AK I EN TERMINAL                 
014200     03 KVBK                 PIC S9(7)           COMP-3.                  
014300*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
014400     03 KVBR-TOT             PIC S9(7)           COMP-3.                  
014500*                                 TOT BEST REST                           
014600     03 KVEFRS               PIC S9(7)           COMP-3.                  
014700*                                 EJ FAKTURERAT ANTAL STYCK               
014800     03 KVKP                 PIC S9(7)           COMP-3.                  
014900*                                 KÖPPUNKT                                
015000     03 KVLAAN               PIC S9(7)           COMP-3.                  
015100*                                 LÅNESALDO                               
015200     03 KVLS                 PIC S9(7)           COMP-3.                  
015300*                                 LAGERSALDO                              
015400     03 KVMP                 PIC S9(7)           COMP-3.                  
015500*                                 MAXPUNKT                                
015600     03 KVOVERF              PIC S9(7)           COMP-3.                  
015700*                                 ÖVERFÖRINGSSALDO                        
015800     03 KVPALL               PIC S9(7)           COMP-3.                  
015900*                                 ANTAL I PALL                            
016000     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
016100*                                 SATS-PERIODBEHOV                        
016200     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
016300*                                 SEPARAT PERIODBEHOV                     
016400     03 KVQ                  PIC S9(7)           COMP-3.                  
016500*                                 EKONOMISK HEMTAGNINGSKVANTITET          
016600     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
016700*                                 ANTAL I Q0 FÖRPACKNING                  
016800     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
016900*                                 ANTAL I Q1 FÖRPACKNING                  
017000     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
017100*                                 ANTAL I Q2 FÖRPACKNING                  
017200     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
017300*                                 ANTAL I Q3 FÖRPACKNING                  
017400     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
017500*                                 ANTAL I Q4 FÖRPACKNING                  
017600     03 KVRESS               PIC S9(7)           COMP-3.                  
017700*                                 RESERVERAT ANTAL ARTIKLAR               
017800     03 KVROS                PIC S9(7)           COMP-3.                  
017900*                                 RESTORDERSALDO                          
018000     03 KVSLAGER             PIC S9(7)           COMP-3.                  
018100*                                 SÄKERHETSLAGER                          
018200     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
018300*                                 SLUTKÖPSSALDO                           
018400     03 KVSPANT              PIC S9(7)           COMP-3.                  
018500*                                 SPÄRRAT ANTAL                           
018600     03 KVUTRS               PIC S9(7)           COMP-3.                  
018700*                                 UTREDNINGSSALDO                         
018800     03 KVVECKOR-AT          PIC S9(3)           COMP-3.                  
018900*                                 ANTAL VECKOR ANSKAFFNINGSTID            
019000     03 KVVECKOR-BT          PIC S9(3)           COMP-3.                  
019100*                                 ANTAL VECKOR BESTÄLLNINGSTID            
019200     03 KVVECKOR-FT          PIC S9(3)           COMP-3.                  
019300*                                 ANTAL VECKOR FRYSNINGSTID               
019400     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
019500*                                 ANTAL VECKOR LEDTID                     
019600     03 PRARTBEL-PR          PIC S9(8)V9(3)      COMP-3.                  
019700*                                 DETTA BESTÄLLNINGSPRIS                  
019800*                                 (I LEVERANTÖRENS VALUTA)                
019900     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
020000*                                 BESTÄLLNINGSPRIS I KRONOR               
020100     03 PRARTBES-PR          PIC S9(7)V9(2)      COMP-3.                  
020200*                                 DETTA BESTÄLLNINGSPRIS (KR)             
020300     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
020400*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
020500     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
020600*                                 ARTIKELNS SJÄLVKOSTNAD                  
020700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
020800*                                 ARTIKELSTANDARDPRIS                     
020900     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
021000*                                 INKÖPSPRIS                              
021100     03 REKSIFFR             PIC S9              COMP-3.                  
021200*                                 KONTROLLSIFFRA                          
021300     03 REPROCFP             PIC S9V9(2)         COMP-3.                  
021400*                                 PROCENT-FÖRPACKNING                     
021500     03 RESLJUST             PIC S9(2)V9(1)      COMP-3.                  
021600*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
021700     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
021800*                                 TPO-KVANTITET, TOTAL                    
021900     03 TIERSDAT             PIC S9(5)           COMP-3.                  
022000*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
022100     03 TIFINLV              PIC S9(5)           COMP-3.                  
022200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
022300     03 TIINVDAT             PIC S9(5)           COMP-3.                  
022400*                                 INVENTERINGSDATUM                       
022500     03 TILPSP               PIC S9(5)           COMP-3.                  
022600*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
022700     03 TIREGDAT             PIC S9(7)           COMP-3.                  
022800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
022900     03 TIRODAT              PIC S9(5)           COMP-3.                  
023000*                                 RESTORDERDATUM      TIRODAT-002         
023100*                                 (AAVVD)                                 
023200     03 TISLJUST             PIC S9(5)           COMP-3.                  
023300*                                 VECKA DÅ JUSTERING AV SÄKER-            
023400*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
023500     03 TIURPROD             PIC S9(5)           COMP-3.                  
023600*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
023700     03 VKART                PIC S9(7)           COMP-3.                  
023800*                                 ARTIKELVIKT (G)                         
023900     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
024000*                                 ARTIKELVOLYM NETTO (CM3)                
024100*** END OF VILMAII-COPY LENGTH= 435 BYTES                                 
