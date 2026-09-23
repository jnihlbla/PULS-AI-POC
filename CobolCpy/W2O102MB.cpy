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
002100        05 IDLEVNR           PIC Z(4)9.                                   
002200*                                 LEVERANTÖRNUMMER                        
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
003900        05 TIAVIDAT-SEN      PIC Z(6)9.                                   
004000*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
004100        05 KVAVIS-SEN        PIC Z(6)9.                                   
004200*                                 SENAST AVISERAT ANTAL                   
004300        05 IDLEVNR-SEN       PIC Z(4)9.                                   
004400*                                 SENASTE LEVERANTÖR                      
004500        05 IDFS-SEN          PIC X(8).                                    
004600*                                 FÖLJESEDELSNUMMER SENASTE INLEV         
004700        05 KOLUMN-1-3.                                                    
004800*                                                                         
004900           07 KVLS           OCCURS 3 TIMES                               
005000                             PIC -(7)9.                                   
005100*                                 LAGERSALDO                              
005200           07 KVRESS         OCCURS 3 TIMES                               
005300                             PIC -(7)9.                                   
005400*                                 RESERVERAT ANTAL ARTIKLAR               
005500           07 DISP           OCCURS 3 TIMES                               
005600                             PIC -(7)9.                                   
005700*                                 DISPONIBELT LAGER                       
005800           07 KVOKS          OCCURS 3 TIMES                               
005900                             PIC -(7)9.                                   
006000*                                 ORDERKÖSALDO                            
006100           07 KVAKS-LAGER    OCCURS 3 TIMES                               
006200                             PIC -(7)9.                                   
006300*                                 ANKOMSTSALDO                            
006400           07 KVAKS-PAV      OCCURS 3 TIMES                               
006500                             PIC -(7)9.                                   
006600*                                 DEL AV AK PÅ VÄG                        
006700           07 KVAKS-T        OCCURS 3 TIMES                               
006800                             PIC -(7)9.                                   
006900*                                 DEL AV AK I EN TERMINAL                 
007000           07 KVART-FORAVIS  OCCURS 3 TIMES                               
007100                             PIC -(7)9.                                   
007200*                                 ANKOMSTSALDO                            
007300           07 SUTPO-TOT      OCCURS 3 TIMES                               
007400                             PIC -(7)9.                                   
007500*                                 TPO-KVANTITET, TOTAL                    
007600           07 KVROS          OCCURS 3 TIMES                               
007700                             PIC -(7)9.                                   
007800*                                 RESTORDERSALDO                          
007900           07 ARB-SALDO      OCCURS 3 TIMES                               
008000                             PIC -(7)9.                                   
008100*                                 LAGERTILLGÅNG                           
008200           07 KDERS          OCCURS 3 TIMES                               
008300                             PIC -(7)9.                                   
008400*                                 ERSÄTTNINGSKOD        KDERS-003         
008500           07 KVBR-TOT       PIC -(7)9.                                   
008600*                                 TOT BEST REST                           
008700           07 KVBR-OVR       PIC -(6)9.                                   
008800*                                 BEST. REST FÖRUTOM HUVUDLEV.            
008900*                                                                         
009000        05 KOLUMN-4-6.                                                    
009100*                                                                         
009200           07 KVBUFF         PIC Z(6)9.                                   
009300*                                 FÖRÄDLAT BUFFERSALDO                    
009400           07 KVSLUTKP       PIC -(6)9.                                   
009500*                                 SLUTKÖPSSALDO                           
009600           07 KVPB-SEP       OCCURS 3 TIMES                               
009700                             PIC Z(7)9.9.                                 
009800*                                 SEPARAT PERIODBEHOV                     
009900           07 KVPB-SATS      OCCURS 3 TIMES                               
010000                             PIC Z(7)9.9.                                 
010100*                                 SATS-PERIODBEHOV                        
010200           07 KVSLAGER       OCCURS 3 TIMES                               
010300                             PIC -(9)9.                                   
010400*                                 SÄKERHETS LAGER    KVSLAGER-002         
010500           07 RESLJUST       OCCURS 3 TIMES                               
010600                             PIC -(6)9.9(2).                              
010700*                                                    RESLJUST-002         
010800*                                 SÄKERHETSLAGER-JUST.                    
010900           07 KVSPANT        OCCURS 3 TIMES                               
011000                             PIC -(9)9.                                   
011100*                                 SPÄRR.KVANT         KVSPANT-002         
011200           07 KVMP           OCCURS 3 TIMES                               
011300                             PIC -(9)9.                                   
011400*                                 MAXPUNKT               KVMP-002         
011500           07 LAGERPLATS.                                                 
011600              09 ADLAGOMR    PIC Z(2)9.                                   
011700*                                 LAGEROMRÅDE                             
011800              09 ADGANG      PIC Z(2)9.                                   
011900*                                 GÅNG                                    
012000              09 ADPLATS     PIC Z(5)9.                                   
012100*                                                     ADPLATS-002         
012200*                                 LAGERPLATS OMR. GÅNG PLATS              
012300           07 TIINVDAT       PIC Z(5)9(5).                                
012400*                                 INV/JUST DATUM     TIINVDAT-002         
012500           07 KVEFRS-CDC     PIC -(7)9.                                   
012600*                                 EJ FAKTURERAT ANTAL STYCK               
012700           07 KVEFRS-SDC     PIC -(7)9.                                   
012800*                                 EJ FAKTURERAT ANTAL STYCK               
012900           07 KVEFRS-NDC     PIC -(7)9.                                   
013000*                                 EJ FAKTURERAT ANTAL STYCK               
013100           07 KVUTRS         OCCURS 3 TIMES                               
013200                             PIC -(9)9.                                   
013300*                                 UTREDNINGSSALDO                         
013400           07 KVLS-SDC-OVER  PIC -(7)9.                                   
013500*                                 LAGERSALDO                              
013600     03 TEMFSINF             PIC X(55).                                   
013700*                                 INFORMATIONSMEDDELANDE                  
013800*** END OF VILMAII-COPY LENGTH= 788 BYTES                                 
