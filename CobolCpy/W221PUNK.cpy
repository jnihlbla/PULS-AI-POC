000100 01  W221PUNK.                                                            
000200*                                                                         
000300     03 ARBETSFELT.                                                       
000400        05 KDLPORS-GRP.                                                   
000500           07 KDLPORS-TAB    OCCURS 3 TIMES                               
000600                             PIC S9(3)           COMP-3.                  
000700*                                 LEVERANSPLANEORSAK                      
000800        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
000900*                                 TOT BEST REST                           
001000        05 KVOEKORR          PIC S9(7)           COMP-3.                  
001100*                                 ÖVRE KORRIDORGRÄNS                      
001200     03 INDATA.                                                           
001300        05 IDARTNR           PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500        05 KDPRODSL          PIC S9(3)           COMP-3.                  
001600*                                 PRODUKTSLAG                             
001700        05 KVANTAL-CLAGER    PIC S9              COMP-3.                  
001800        05 TIAAVV-AKT        PIC S9(5)           COMP-3.                  
001900*                                 ÅR - VECKA  (ÅÅVV)                      
002000        05 TIAAVVD-AKT       PIC S9(5)           COMP-3.                  
002100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002200        05 FLAVRART          PIC X.                                       
002300*                                 AVROPSARTIKEL                           
002400        05 FLFSP             OCCURS 2 TIMES                               
002500                             PIC X.                                       
002600*                                 FÖRDELNINGSSPÄRR                        
002700        05 FLMANBK           PIC X.                                       
002800*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
002900        05 FLMANKP           PIC X.                                       
003000*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
003100        05 IDLEVNR           PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300        05 IDPROD            PIC S9(3)           COMP-3.                  
003400*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
003500        05 KDERS             OCCURS 2 TIMES                               
003600                             PIC S9(3)           COMP-3.                  
003700*                                 ERSÄTTNINGSKOD                          
003800        05 KDGK              PIC S9              COMP-3.                  
003900*                                 GODSMOTTAGAREKOD                        
004000        05 KDHF              PIC S9              COMP-3.                  
004100*                                 HUVUDFÖRRÅDSMÄRKNING                    
004200        05 KDUART            PIC X.                                       
004300*                                 UNDANTAGSARTIKEL                        
004400        05 KVAKS             OCCURS 2 TIMES                               
004500                             PIC S9(7)           COMP-3.                  
004600*                                 ANKOMSTSALDO                            
004700        05 KVLS              OCCURS 2 TIMES                               
004800                             PIC S9(7)           COMP-3.                  
004900*                                 LAGERSALDO                              
005000        05 KVLS-SDC-OVER     PIC S9(7)           COMP-3.                  
005100*                                 LAGERSALDO                              
005200        05 KVOKS-BULK        OCCURS 2 TIMES                               
005300                             PIC S9(7)           COMP-3.                  
005400*                                 ORDERKÖSALDO, KLASS 2-4                 
005500        05 KVOKS-DAG         OCCURS 2 TIMES                               
005600                             PIC S9(7)           COMP-3.                  
005700*                                 ORDERKÖSALDO, KLASS 1                   
005800        05 KVOKS-VOR         OCCURS 2 TIMES                               
005900                             PIC S9(7)           COMP-3.                  
006000*                                 ORDERKÖSALDO, VOR                       
006100        05 KVPALL            PIC S9(7)           COMP-3.                  
006200*                                 ANTAL I PALL                            
006300        05 KVPB-SATS         OCCURS 2 TIMES                               
006400                             PIC S9(6)V9(1)      COMP-3.                  
006500*                                 SATS-PERIODBEHOV                        
006600        05 KVPB-SDC-EJ-DIR   PIC S9(6)V9(1)      COMP-3.                  
006700*                                 PB-DC TOTALT FÖR ALLA DC:ER, EJ         
006800*                                  DIRLEV                                 
006900        05 KVPB-SDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
007000*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
007100*                                 :ER                                     
007200        05 KVPB-TPO          OCCURS 2 TIMES                               
007300                             PIC S9(6)V9(1)      COMP-3.                  
007400*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
007500        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
007600*                                 ANTAL KVANTITETFÖRPACKNINGAR            
007700        05 KVPB-VESL         OCCURS 2 TIMES                               
007800                             PIC S9(6)V9(1)      COMP-3.                  
007900*                                 GÄLLANDE PB VID VECKOSLUT               
008000        05 KVRESS            OCCURS 2 TIMES                               
008100                             PIC S9(7)           COMP-3.                  
008200*                                 RESERVERAT ANTAL ARTIKLAR               
008300        05 KVROS             OCCURS 2 TIMES                               
008400                             PIC S9(7)           COMP-3.                  
008500*                                 RESTORDERSALDO                          
008600        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
008700*                                 ARTIKELSTANDARDPRIS                     
008800        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
008900*                                 BESTÄLLNINGSPRIS I KRONOR               
009000        05 REDIRLEV          OCCURS 2 TIMES                               
009100                             PIC S9V9(2)         COMP-3.                  
009200*                                 DIREKTLEVERANSANDEL                     
009300        05 TIFINLV           PIC S9(5)           COMP-3.                  
009400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
009500        05 KVPB-PLAN         PIC S9(6)V9(1)      COMP-3.                  
009600*                                 PLANERAT PERIODBEHOV                    
009700     03 UTDATA.                                                           
009800        05 FLMANQ            PIC X.                                       
009900*                                 MANUELL HEMTAGNINGSKVANTITET            
010000        05 FLMPB             OCCURS 2 TIMES                               
010100                             PIC X.                                       
010200*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
010300        05 KDAVT             PIC S9              COMP-3.                  
010400*                                 AVTALSMÄRKNING                          
010500        05 KDFREKKL          PIC X.                                       
010600*                                 FREKVENSKLASS                           
010700        05 KDLTK             PIC S9              COMP-3.                  
010800*                                 LAGERTILLHÖRIGHETSKOD                   
010900        05 KDPRISKL          PIC X.                                       
011000*                                 PRISKLASS                               
011100        05 KDVVKL            PIC S9              COMP-3.                  
011200*                                 VOLYMVÄRDESKLASS                        
011300        05 KVAP              PIC S9(7)           COMP-3.                  
011400*                                 ANNULLATIONSPUNKT                       
011500        05 KVBK              PIC S9(7)           COMP-3.                  
011600*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
011700        05 KVKP              PIC S9(7)           COMP-3.                  
011800*                                 KÖPPUNKT                                
011900        05 KVMAD-SEP         OCCURS 2 TIMES                               
012000                             PIC S9(6)V9(1)      COMP-3.                  
012100*                                 SEPARAT PROGNOSFEL                      
012200        05 KVMAD-TOT         OCCURS 2 TIMES                               
012300                             PIC S9(6)V9(1)      COMP-3.                  
012400*                                 TOTALT PROGNOSFEL                       
012500        05 KVMP              OCCURS 2 TIMES                               
012600                             PIC S9(7)           COMP-3.                  
012700*                                 MAXPUNKT                                
012800        05 KVPB-SEP          OCCURS 2 TIMES                               
012900                             PIC S9(6)V9(1)      COMP-3.                  
013000*                                 SEPARAT PERIODBEHOV                     
013100        05 KVQ               PIC S9(7)           COMP-3.                  
013200*                                 EKONOMISK HEMTAGNINGSKVANTITET          
013300        05 KVQ-JUST          PIC S9(7)           COMP-3.                  
013400*                                 NY EKON HEMTAGNINGSKVANTITET            
013500        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
013600*                                 ANTAL VECKOR FRYSNINGSTID               
013700        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
013800*                                 ANTAL VECKOR BESTÄLLNINGSTID            
013900        05 KVSLAGER          OCCURS 2 TIMES                               
014000                             PIC S9(7)           COMP-3.                  
014100*                                 SÄKERHETSLAGER                          
014200        05 RESLJUST          OCCURS 2 TIMES                               
014300                             PIC S9(2)V9(1)      COMP-3.                  
014400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
014500        05 TIQJUST           PIC S9(5)           COMP-3.                  
014600*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
014700        05 TISLJUST          OCCURS 2 TIMES                               
014800                             PIC S9(5)           COMP-3.                  
014900*                                 VECKA DÅ JUSTERING AV SÄKER-            
015000*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
015100     03 EOQ.                                                              
015200        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
015300*                                 LAGEROMRÅDE                             
015400        05 BEFT              PIC S9(3)           COMP-3.                  
015500*                                 FÖRPACKNINGSTYP                         
015600        05 FLNYBER           PIC X.                                       
015700*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
015800        05 KVEOQ             PIC S9(7)           COMP-3.                  
015900*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
016000*                                 ET                                      
016100        05 KVSLAGER-OPT      PIC S9(7)           COMP-3.                  
016200*                                 OPTIMALT SÄKERHETSLAGER                 
016300        05 KVULOAD           PIC S9(7)           COMP-3.                  
016400*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
016500        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
016600*                                 ORDERSÄRKOSTNAD                         
016700        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
016800*                                 ARTIKELVOLYM NETTO (CM3)                
016900        05 KVVECKOR-LVAR     PIC S9(2)V9(1)      COMP-3.                  
017000*                                 VARIANS I LEDTIDEN                      
017100*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
