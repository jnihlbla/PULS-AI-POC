000100 01  W2O10201.                                                            
000200*                                 COPYTEXT FÖR MOD W2O10201               
000300     03 TRANS-NUMMER         PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE              PIC X(41).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 ARTIKEL-UT.                                                       
001000        05 IDARTNR-UT        PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 STRECK-1          PIC X.                                       
001300        05 REKSIFFR          PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 BEART-SVE            PIC X(25).                                   
001600*                                 SVENSK ARTIKELBENÄMNING                 
001700     03 AREA-OUTPUT.                                                      
001800*                                                                         
001900        05 IDANSK            PIC Z9(2).                                   
002000*                                 ANSKAFFARNUMMER                         
002100        05 IDLEVNR-SHIP      PIC X(5).                                    
002200*                                 SKEPPANDE LEVERANTÖR                    
002300        05 VIP-GRP.                                                       
002400*                                                                         
002500           07 KDLTK          PIC 9.                                       
002600*                                 LAGERTILLHÖRIGHETSKOD                   
002700           07 KDUART         PIC X.                                       
002800*                                 UNDANTAGSARTIKEL                        
002900        05 KDGK              PIC 9.                                       
003000*                                 GODSMOTTAGAREKOD                        
003100        05 KDAVT             PIC 9.                                       
003200*                                 AVTALSMÄRKNING                          
003300        05 KDKSP             PIC 9.                                       
003400*                                 KÖPSPÄRR                                
003500        05 PRARTSTD          PIC -(7)9.9(2).                              
003600*                                 ARTIKELSTANDARDPRIS                     
003700        05 PRARTBES          PIC -(7)9.9(2).                              
003800*                                 BESTÄLLNINGSPRIS I KRONOR               
003900        05 TIAVIDAT-SEN      PIC Z9(6).                                   
004000*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
004100        05 KVAVIS-SEN        PIC Z(6)9.                                   
004200*                                 SENAST AVISERAT ANTAL                   
004300        05 KDANSKSEG         PIC 9(4).                                    
004400*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
004500        05 IDLEVNR-SEN       PIC X(5).                                    
004600*                                 SENASTE LEVERANTÖR                      
004700        05 IDFS-SEN          PIC X(8).                                    
004800*                                 FÖLJESEDELSNUMMER SENASTE INLEV         
004900        05 KOLUMN-1-3.                                                    
005000*                                                                         
005100           07 KVLS           OCCURS 3 TIMES                               
005200                             PIC -(7)9.                                   
005300*                                 LAGERSALDO                              
005400           07 KVRESS         OCCURS 3 TIMES                               
005500                             PIC -(7)9.                                   
005600*                                 RESERVERAT ANTAL ARTIKLAR               
005700           07 DISP           OCCURS 3 TIMES                               
005800                             PIC -(7)9.                                   
005900*                                 DISPONIBELT LAGER                       
006000           07 KVOKS          OCCURS 3 TIMES                               
006100                             PIC -(7)9.                                   
006200*                                 ORDERKÖSALDO                            
006300           07 KVAKS-LAGER    OCCURS 3 TIMES                               
006400                             PIC -(7)9.                                   
006500*                                 ANKOMSTSALDO                            
006600           07 KVAKS-PAV      OCCURS 3 TIMES                               
006700                             PIC -(7)9.                                   
006800*                                 DEL AV AK PÅ VÄG                        
006900           07 KVAKS-T        OCCURS 3 TIMES                               
007000                             PIC -(7)9.                                   
007100*                                 DEL AV AK I EN TERMINAL                 
007200           07 KVART-FORAVIS  OCCURS 3 TIMES                               
007300                             PIC -(7)9.                                   
007400*                                 ANKOMSTSALDO                            
007500           07 SUTPO-TOT      OCCURS 3 TIMES                               
007600                             PIC -(7)9.                                   
007700*                                 TPO-KVANTITET, TOTAL                    
007800           07 KVROS          OCCURS 3 TIMES                               
007900                             PIC -(7)9.                                   
008000*                                 RESTORDERSALDO                          
008100           07 ARB-SALDO      OCCURS 3 TIMES                               
008200                             PIC -(7)9.                                   
008300*                                 LAGERTILLGÅNG                           
008400           07 KDERS          OCCURS 3 TIMES                               
008500                             PIC -(7)9.                                   
008600*                                 ERSÄTTNINGSKOD        KDERS-003         
008700           07 KVBR-TOT       PIC -(7)9.                                   
008800*                                 TOT BEST REST                           
008900           07 KVBR-OVR       PIC -(6)9.                                   
009000*                                 BEST. REST FÖRUTOM HUVUDLEV.            
009100*                                                                         
009200        05 KOLUMN-4-6.                                                    
009300*                                                                         
009400           07 KVBUFF         PIC Z(7).                                    
009500*                                 FÖRÄDLAT BUFFERSALDO                    
009600           07 KVSLUTKP       PIC Z(6)9.                                   
009700*                                 SLUTKÖPSSALDO                           
009800           07 KVPB-SEP       OCCURS 3 TIMES                               
009900                             PIC Z(7)9.9.                                 
010000*                                 SEPARAT PERIODBEHOV                     
010100           07 KVPB-SATS      OCCURS 3 TIMES                               
010200                             PIC Z(7)9.9.                                 
010300*                                 SATS-PERIODBEHOV                        
010400           07 KVSLAGER       OCCURS 3 TIMES                               
010500                             PIC -(9)9.                                   
010600*                                 SÄKERHETS LAGER    KVSLAGER-002         
010700           07 RESLJUST       OCCURS 3 TIMES                               
010800                             PIC -(6)9.9(2).                              
010900*                                                    RESLJUST-002         
011000*                                 SÄKERHETSLAGER-JUST.                    
011100           07 KVSPANT        OCCURS 3 TIMES                               
011200                             PIC -(9)9.                                   
011300*                                 SPÄRR.KVANT         KVSPANT-002         
011400           07 KVMP           OCCURS 3 TIMES                               
011500                             PIC -(9)9.                                   
011600*                                 MAXPUNKT               KVMP-002         
011700           07 LAGERPLATS.                                                 
011800              09 ADLAGOMR    PIC Z(2)9.                                   
011900*                                 LAGEROMRÅDE                             
012000              09 ADGANG      PIC Z(2)9.                                   
012100*                                 GÅNG                                    
012200              09 ADPLATS     PIC Z(5)9.                                   
012300*                                                     ADPLATS-002         
012400*                                 LAGERPLATS OMR. GÅNG PLATS              
012500           07 TIINVDAT       PIC Z(5)9(5).                                
012600*                                 INV/JUST DATUM     TIINVDAT-002         
012700           07 KVEFRS-CDC     PIC -(7)9.                                   
012800*                                 EJ FAKTURERAT ANTAL STYCK               
012900           07 KVEFRS-SDC     PIC -(7)9.                                   
013000*                                 EJ FAKTURERAT ANTAL STYCK               
013100           07 KVEFRS-NDC     PIC -(7)9.                                   
013200*                                 EJ FAKTURERAT ANTAL STYCK               
013300           07 KVUTRS         OCCURS 3 TIMES                               
013400                             PIC -(9)9.                                   
013500*                                 UTREDNINGSSALDO                         
013600           07 KVLS-SDC-OVER  PIC -(7)9.                                   
013700*                                 LAGERSALDO                              
013800     03 KVLS-REM             PIC -(6)9.                                   
013900*                                 LAGERSALDO                              
014000     03 KVVORKO              PIC -(6)9.                                   
014100*                                 VOR-KÖ KVANT                            
014200     03 FLCDART              PIC X.                                       
014300*                                 CROSS-DOCKING PART                      
014400     03 TEMFSINF             PIC X(55).                                   
014500*                                 INFORMATIONSMEDDELANDE                  
014600*** END OF VILMAII-COPY LENGTH= 807 BYTES                                 
