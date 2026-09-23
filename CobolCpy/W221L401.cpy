000100 01  W221L401.                                                            
000200*                                                                         
000300     03 KDCALL               PIC S9(3)           COMP-3.                  
000400      88 OPPNA               VALUE +1.                                    
000500      88 BEARBETA            VALUE +2.                                    
000600      88 AVSLUTA             VALUE +3.                                    
000700      88 LAES-ARTIKEL-DATA   VALUE +401.                                  
000800      88 LAES-LEVERANTOER    VALUE +402.                                  
000900      88 LAES-EXLEV-OMSPEC   VALUE +403.                                  
001000      88 LAES-ARTIKEL        VALUE +404.                                  
001100      88 UPPDAT-CLAG         VALUE +405.                                  
001200      88 UPPDAT-EXLEV        VALUE +409.                                  
001300      88 BORTTAG-OMSPEC      VALUE +410.                                  
001400      88 BORTTAG-EXLEV       VALUE +411.                                  
001500      88 NYUPPL-EXLEV        VALUE +412.                                  
001600*                                 ANROPSTYP     KDCALL-W221               
001700     03 FLJANEJ-ANROP        PIC X.                                       
001800      88 ANROP-OK            VALUE 'J'.                                   
001900      88 ANROP-FEL           VALUE 'N'.                                   
002000*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002400*                                 PRODUKTSLAG                             
002500     03 KDCLAGER             PIC S9              COMP-3.                  
002600      88 KDCLAGER-C1         VALUE +1.                                    
002700      88 KDCLAGER-C2         VALUE +2.                                    
002800*                                 CENTRALLAGERKOD                         
002900     03 KVAL-BR              PIC S9              COMP-3.                  
003000*                                 ANTAL LEVERANTÖRER                      
003100     03 KVANTAL-CLAGER       PIC S9              COMP-3.                  
003200     03 KVANTAL-LEVPLAN      PIC S9              COMP-3.                  
003300     03 TIAAVV-AKT           PIC S9(5)           COMP-3.                  
003400*                                 ÅR - VECKA  (ÅÅVV)                      
003500     03 TIAAVVD-AKT          PIC S9(5)           COMP-3.                  
003600*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
003700     03 TIAAVV-NEXT-QF       PIC S9(5)           COMP-3.                  
003800*                                 ÅR - VECKA  (ÅÅVV)                      
003900     03 IDINK                PIC X(4).                                    
004000*                                 INKÖPARNUMMER                           
004100     03 IDAVTAL              PIC S9(13)          COMP-3.                  
004200*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
004300*                                 PPP   = INKÖPARNR (PREFIX)              
004400*                                 BBBBB = BESTÄLLARNR                     
004500*                                 SSS   = SUFFIX                          
004600     03 ARBETSFELT.                                                       
004700        05 KDLPORS-GRP.                                                   
004800           07 KDLPORS-TAB    OCCURS 3 TIMES                               
004900                             PIC S9(3)           COMP-3.                  
005000*                                 LEVERANSPLANEORSAK                      
005100        05 KVBEST-PL         PIC S9(7)           COMP-3.                  
005200*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
005300        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
005400*                                 TOT BEST REST                           
005500        05 KVLS-SDC-OVER     PIC S9(7)           COMP-3.                  
005600*                                 LAGERSALDO                              
005700        05 KVOEKORR          PIC S9(7)           COMP-3.                  
005800*                                 ÖVRE KORRIDORGRÄNS                      
005900        05 KVPB-SDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
006000*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
006100*                                 :ER                                     
006200        05 KVPB-SDC-EJ-DIR   PIC S9(6)V9(1)      COMP-3.                  
006300*                                 PB-DC TOTALT FÖR ALLA DC:ER, EJ         
006400*                                  DIRLEV                                 
006500     03 REGISTER-DATA-LAES.                                               
006600        05 BEFT              PIC S9(3)           COMP-3.                  
006700*                                 FÖRPACKNINGSTYP                         
006800        05 FLAVRART          PIC X.                                       
006900*                                 AVROPSARTIKEL                           
007000        05 FLFSP             OCCURS 2 TIMES                               
007100                             PIC X.                                       
007200*                                 FÖRDELNINGSSPÄRR                        
007300        05 FLMANBK           PIC X.                                       
007400*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
007500        05 FLMANKP           PIC X.                                       
007600*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
007700        05 FLMANAT           PIC X.                                       
007800*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
007900        05 FLTPO1            PIC X.                                       
008000*                                 ARTIKELN GODKÄND FÖR TPO1               
008100        05 IDANSK            PIC S9(3)           COMP-3.                  
008200*                                 ANSKAFFARNUMMER                         
008300        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
008400*                                 FUNKTIONSGRUPP                          
008500        05 IDLEVNR           PIC X(5).                                    
008600*                                 LEVERANTÖRNUMMER                        
008700        05 IDLKTO            PIC S9(7)           COMP-3.                  
008800*                                 LAGERKONTO (FFHHHUU)                    
008900        05 IDPROD            PIC S9(3)           COMP-3.                  
009000*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
009100        05 KDERS             OCCURS 2 TIMES                               
009200                             PIC S9(3)           COMP-3.                  
009300*                                 ERSÄTTNINGSKOD                          
009400        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
009500*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
009600        05 KDGK              PIC S9              COMP-3.                  
009700*                                 GODSMOTTAGAREKOD                        
009800        05 KDHF              PIC S9              COMP-3.                  
009900*                                 HUVUDFÖRRÅDSMÄRKNING                    
010000        05 KDUART            PIC X.                                       
010100*                                 UNDANTAGSARTIKEL                        
010200        05 KVAKS             OCCURS 2 TIMES                               
010300                             PIC S9(7)           COMP-3.                  
010400*                                 ANKOMSTSALDO                            
010500        05 KVBR              PIC S9(7)           COMP-3.                  
010600*                                 BESTÄLLNINGSREST                        
010700        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
010800*                                 DAGAR TULL- OCH TRANSPORT-TID           
010900        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
011000*                                 FRYSTID FÖR TPO-ORDER                   
011100        05 KVLAAN            PIC S9(7)           COMP-3.                  
011200*                                 LÅNESALDO                               
011300        05 KVLS              OCCURS 2 TIMES                               
011400                             PIC S9(7)           COMP-3.                  
011500*                                 LAGERSALDO                              
011600        05 KVOKS-BULK        OCCURS 2 TIMES                               
011700                             PIC S9(7)           COMP-3.                  
011800*                                 ORDERKÖSALDO, KLASS 2-4                 
011900        05 KVOKS-DAG         OCCURS 2 TIMES                               
012000                             PIC S9(7)           COMP-3.                  
012100*                                 ORDERKÖSALDO, KLASS 1                   
012200        05 KVOKS-VOR         OCCURS 2 TIMES                               
012300                             PIC S9(7)           COMP-3.                  
012400*                                 ORDERKÖSALDO, VOR                       
012500        05 SUTPO-TOT         OCCURS 2 TIMES                               
012600                             PIC S9(7)           COMP-3.                  
012700*                                 TPO-KVANTITET, TOTAL                    
012800        05 KVPALL            PIC S9(7)           COMP-3.                  
012900*                                 ANTAL I PALL                            
013000        05 KVPB-SATS         OCCURS 2 TIMES                               
013100                             PIC S9(6)V9(1)      COMP-3.                  
013200*                                 SATS-PERIODBEHOV                        
013300        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
013400*                                 ANTAL KVANTITETFÖRPACKNINGAR            
013500        05 KVRESS            OCCURS 2 TIMES                               
013600                             PIC S9(7)           COMP-3.                  
013700*                                 RESERVERAT ANTAL ARTIKLAR               
013800        05 KVROS             OCCURS 2 TIMES                               
013900                             PIC S9(7)           COMP-3.                  
014000*                                 RESTORDERSALDO                          
014100        05 KVRETUR           OCCURS 2 TIMES                               
014200                             PIC S9(5)           COMP-3.                  
014300*                                 ANTAL I RETUR                           
014400        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
014500*                                 ARTIKELSTANDARDPRIS                     
014600        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
014700*                                 BESTÄLLNINGSPRIS I KRONOR               
014800        05 REDIRLEV          OCCURS 2 TIMES                               
014900                             PIC S9V9(2)         COMP-3.                  
015000*                                 DIREKTLEVERANSANDEL                     
015100        05 SUTPO-PB          OCCURS 2 TIMES                               
015200                             PIC S9(7)           COMP-3.                  
015300*                                 TPO-KVANTITET, BEHOVSPÅVERKANDE         
015400        05 TIDISPIN          OCCURS 2 TIMES                               
015500                             PIC S9(7)           COMP-3.                  
015600*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
015700        05 TIFINLV           PIC S9(5)           COMP-3.                  
015800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
015900        05 TIOMSPEC          PIC S9(5)           COMP-3.                  
016000*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
016100        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
016200*                                 LAGEROMRÅDE                             
016300        05 FLNYBER           PIC X.                                       
016400*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
016500        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
016600*                                 ORDERSÄRKOSTNAD                         
016700        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
016800*                                 ARTIKELVOLYM NETTO (CM3)                
016900        05 KVVECKOR-LVAR     PIC S9(2)V9(1)      COMP-3.                  
017000*                                 VARIANS I LEDTIDEN                      
017100     03 REGISTER-DATA-UPPD.                                               
017200        05 FLMANQ            PIC X.                                       
017300*                                 MANUELL HEMTAGNINGSKVANTITET            
017400        05 FLMANPB           OCCURS 2 TIMES                               
017500                             PIC X.                                       
017600*                                 MANUELLT REGISTRERAT PB-TPO             
017700        05 FLMPB             OCCURS 2 TIMES                               
017800                             PIC X.                                       
017900*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
018000        05 KDAVT             PIC S9              COMP-3.                  
018100*                                 AVTALSMÄRKNING                          
018200        05 FILLER            PIC X(3).                                    
018300        05 KDKSP             PIC S9              COMP-3.                  
018400*                                 KÖPSPÄRR                                
018500        05 KDLPSP            PIC S9              COMP-3.                  
018600*                                 LEVERANSPLANESPÄRR                      
018700        05 KDLTK             PIC S9              COMP-3.                  
018800*                                 LAGERTILLHÖRIGHETSKOD                   
018900        05 KDVVKL            PIC S9              COMP-3.                  
019000*                                 VOLYMVÄRDESKLASS                        
019100        05 KDOPPLAN          PIC X.                                       
019200*                                 OPTIMAL PLAN INOM FRYSTID               
019300        05 KVAP              PIC S9(7)           COMP-3.                  
019400*                                 ANNULLATIONSPUNKT                       
019500        05 KVBK              PIC S9(7)           COMP-3.                  
019600*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
019700        05 KVKP              PIC S9(7)           COMP-3.                  
019800*                                 KÖPPUNKT                                
019900        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
020000*                                 INLEVERANSTID     (ANTAL DAGAR)         
020100        05 KVDAGAR-FFH       PIC S9(3)           COMP-3.                  
020200*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
020300        05 KVMAD-SEP         OCCURS 2 TIMES                               
020400                             PIC S9(6)V9(1)      COMP-3.                  
020500*                                 SEPARAT PROGNOSFEL                      
020600        05 KVMAD-TOT         OCCURS 2 TIMES                               
020700                             PIC S9(6)V9(1)      COMP-3.                  
020800*                                 TOTALT PROGNOSFEL                       
020900        05 KVMP              OCCURS 2 TIMES                               
021000                             PIC S9(7)           COMP-3.                  
021100*                                 MAXPUNKT                                
021200        05 KVOVERF           PIC S9(7)           COMP-3.                  
021300*                                 ÖVERFÖRINGSSALDO                        
021400        05 KVPB-SEP          OCCURS 2 TIMES                               
021500                             PIC S9(6)V9(1)      COMP-3.                  
021600*                                 SEPARAT PERIODBEHOV                     
021700        05 KVPB-TPO          OCCURS 2 TIMES                               
021800                             PIC S9(6)V9(1)      COMP-3.                  
021900*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
022000        05 KVPB-VESL         OCCURS 2 TIMES                               
022100                             PIC S9(6)V9(1)      COMP-3.                  
022200*                                 GÄLLANDE PB VID VECKOSLUT               
022300        05 KVQ               PIC S9(7)           COMP-3.                  
022400*                                 EKONOMISK HEMTAGNINGSKVANTITET          
022500        05 KVQ-JUST          PIC S9(7)           COMP-3.                  
022600*                                 NY EKON HEMTAGNINGSKVANTITET            
022700        05 FLJIT             PIC X.                                       
022800*                                 JUST-IN-TIME FLAGGA                     
022900        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
023000*                                 ANTAL VECKOR FRYSNINGSTID               
023100        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
023200*                                 ANTAL VECKOR LEDTID                     
023300        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
023400*                                 ANTAL VECKOR BESTÄLLNINGSTID            
023500        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
023600*                                 ANTAL VECKOR ANSKAFFNINGSTID            
023700        05 KVSLAGER          OCCURS 2 TIMES                               
023800                             PIC S9(7)           COMP-3.                  
023900*                                 SÄKERHETSLAGER                          
024000        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
024100*                                 SLUTKÖPSSALDO                           
024200        05 RESLJUST          OCCURS 2 TIMES                               
024300                             PIC S9(2)V9(1)      COMP-3.                  
024400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
024500        05 TIBESRPT          PIC S9(5)           COMP-3.                  
024600*                                 DATUM FÖR BESTÄLLNINGSRAPPORT           
024700*                                 (ÅÅVV)                                  
024800        05 TIBESRPT-PAAM     PIC S9(5)           COMP-3.                  
024900*                                 PÅMINNELSEDATUM FÖR                     
025000*                                 BESTÄLLNINGSRAPPORT (ÅÅVV)              
025100        05 TILPSP            PIC S9(5)           COMP-3.                  
025200*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
025300        05 TILTK             PIC S9(5)           COMP-3.                  
025400*                                 LTK-ÄNDRINGSDATUM                       
025500        05 TIQJUST           PIC S9(5)           COMP-3.                  
025600*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
025700        05 TISLJUST          OCCURS 2 TIMES                               
025800                             PIC S9(5)           COMP-3.                  
025900*                                 VECKA DÅ JUSTERING AV SÄKER-            
026000*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
026100        05 RVPROFEL          OCCURS 2 TIMES                               
026200                             PIC S9(3)           COMP-3.                  
026300*                                 ANTAL STORA PROGNOSFEL                  
026400        05 RVPROURS          OCCURS 2 TIMES                               
026500                             PIC S9(3)           COMP-3.                  
026600*                                 ANTAL PROGNOSFEL I FÖLJD                
026700        05 TIXLEVSP          PIC S9(5)           COMP-3.                  
026800*                                 SPÄRWDATUM EXTRA-LEVERANS  ÅÅVV         
026900        05 KDLEVPLF          PIC X.                                       
027000*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
027100        05 KDFREKKL          PIC X.                                       
027200*                                 FREKVENSKLASS                           
027300        05 KDPRISKL          PIC X.                                       
027400*                                 PRISKLASS                               
027500        05 DAPBPLAN          PIC 9(8).                                    
027600*                                 DATUM KVPB-PLAN GILTIG TOM              
027700        05 DASEASON          PIC 9(8).                                    
027800*                                 DATUM RESEASON-LEDTID GILTIG TO         
027900*                                 M                                       
028000        05 KVPB-PLAN         PIC S9(6)V9(1)      COMP-3.                  
028100*                                 PLANERAT PERIODBEHOV                    
028200        05 RESEASON-PLAN     OCCURS 12 TIMES                              
028300                             PIC S9V9(2)         COMP-3.                  
028400*                                 SÄSONGSINDEX INKLUSIVE REFILL           
028500        05 KVULOAD           PIC S9(7)           COMP-3.                  
028600*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
028700        05 KVEOQ             PIC S9(7)           COMP-3.                  
028800*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
028900*                                 ET                                      
029000        05 KVSLAGER-OPT      PIC S9(7)           COMP-3.                  
029100*                                 OPTIMALT SÄKERHETSLAGER                 
029200     03 LEVDAGTAB.                                                        
029300        05 TILEVDAG          OCCURS 5 TIMES                               
029400                             PIC S9              COMP-3.                  
029500*                                 AVSÄNDNINGSDAG INOM VECKA               
029600*** END OF VILMAII-COPY LENGTH= 456 BYTES                                 
