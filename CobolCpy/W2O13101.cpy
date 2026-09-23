000100 01  W2O13101.                                                            
000200*                                 COPYTEXT FÖR MOD W2O13101               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE-RAD1         PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN           PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 BEART-SVE            PIC X(25).                                   
001200*                                 SVENSK ARTIKELBENÄMNING                 
001300     03 IDLEVNR-FRAM-ATTR-UT PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 IDLEVNR-FRAM-UT      PIC X(5).                                    
001600*                                 FRAMTIDA LEVERANTÖRNUMMER               
001700     03 IDLEVNR-FRAM-ATTR-IN PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 IDLEVNR-FRAM-IN      PIC X(5).                                    
002000*                                 FRAMTIDA LEVERANTÖRNUMMER               
002100     03 IDLEVNR-SHIP-FRAM-ATTR-UT                                         
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 IDLEVNR-SHIP-FRAM-UT PIC X(5).                                    
002500*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
002600     03 IDLEVNR-SHIP-FRAM-ATTR-IN                                         
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 IDLEVNR-SHIP-FRAM-IN PIC X(5).                                    
003000*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
003100     03 IDANSK-ATTR-UT       PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 IDANSK-UT            PIC Z(2)9.                                   
003400*                                 ANSKAFFARNUMMER                         
003500     03 IDPLANGR-LEV-ATTR-UT PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 IDPLANGR-LEV-UT      PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900     03 KVSPANT-C1-ATTR-UT   PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 KVSPANT-C1-UT        PIC Z(6)9.                                   
004200*                                 SPÄRRAT ANTAL                           
004300     03 KVSPANT-C2-ATTR-UT   PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 KVSPANT-C2-UT        PIC Z(6)9.                                   
004600*                                 SPÄRRAT ANTAL                           
004700     03 IDANSK-ATTR-IN       PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 IDANSK-IN            PIC Z(2)9.                                   
005000*                                 ANSKAFFARNUMMER                         
005100     03 IDPLANGR-LEV-ATTR-IN PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 IDPLANGR-LEV-IN      PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500     03 KVSPANT-C1-ATTR-IN   PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 KVSPANT-C1-IN        PIC Z(6)9.                                   
005800*                                 SPÄRRAT ANTAL                           
005900     03 KVSPANT-C2-ATTR-IN   PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 KVSPANT-C2-IN        PIC Z(6)9.                                   
006200*                                 SPÄRRAT ANTAL                           
006300     03 IDPLANGR-AG-ATTR-UT  PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 IDPLANGR-AG-UT       PIC X(2).                                    
006600*                                 MFS BEHANDLING AV INPUTFÄLT             
006700     03 FLREFILL-ATTR-UT     PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 FLREFILL-UT          PIC X(2).                                    
007000*                                 MFS BEHANDLING AV INPUTFÄLT             
007100     03 TIREFSTO-ATTR-UT     PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 TIREFSTO-UT          PIC 9(6).                                    
007400*                                 BEORDRINGSSTOPPAD T.OM.                 
007500     03 IDPLANGR-AG-ATTR-IN  PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 IDPLANGR-AG-IN       PIC X(2).                                    
007800*                                 MFS BEHANDLING AV INPUTFÄLT             
007900     03 FLREFILL-ATTR-IN     PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 FLREFILL-IN          PIC X(2).                                    
008200*                                 MFS BEHANDLING AV INPUTFÄLT             
008300     03 TIREFSTO-ATTR-IN     PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 TIREFSTO-IN          PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700     03 KVVECKOR-LT-ATTR-UT  PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 KVVECKOR-LT-UT       PIC Z(2)9.                                   
009000*                                 ANTAL VECKOR LEDTID                     
009100     03 FLMANLT-ATTR-UT      PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 FLMANLT-UT           PIC X(2).                                    
009400*                                 MFS BEHANDLING AV INPUTFÄLT             
009500     03 RESLJUST-C1-ATTR-UT  PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 RESLJUST-C1-UT       PIC X(3).                                    
009800*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
009900     03 RESLJUST-C2-ATTR-UT  PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 RESLJUST-C2-UT       PIC X(3).                                    
010200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
010300     03 KVVECKOR-LT-ATTR-IN  PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 KVVECKOR-LT-IN       PIC Z(2)9.                                   
010600*                                 ANTAL VECKOR LEDTID                     
010700     03 FLMANLT-ATTR-IN      PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 FLMANLT-IN           PIC X(2).                                    
011000*                                 MFS BEHANDLING AV INPUTFÄLT             
011100     03 RESLJUST-C1-ATTR-IN  PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 RESLJUST-C1-IN       PIC X(3).                                    
011400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
011500     03 RESLJUST-C2-ATTR-IN  PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 RESLJUST-C2-IN       PIC X(3).                                    
011800*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
011900     03 KVVECKOR-AT-ATTR-UT  PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 KVVECKOR-AT-UT       PIC Z(2)9.                                   
012200*                                 ANTAL VECKOR ANSKAFFNINGSTID            
012300     03 FLMANAT-ATTR-UT      PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 FLMANAT-UT           PIC X(2).                                    
012600*                                 MFS BEHANDLING AV INPUTFÄLT             
012700     03 TISLJUST-C1-ATTR-UT  PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 TISLJUST-C1-UT       PIC X(4).                                    
013000*                                 VECKA DÅ JUSTERING AV SÄKER-            
013100*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
013200     03 TISLJUST-C2-ATTR-UT  PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 TISLJUST-C2-UT       PIC X(4).                                    
013500*                                 VECKA DÅ JUSTERING AV SÄKER-            
013600*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
013700     03 KVVECKOR-AT-ATTR-IN  PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900     03 KVVECKOR-AT-IN       PIC Z(2)9.                                   
014000*                                 ANTAL VECKOR ANSKAFFNINGSTID            
014100     03 FLMANAT-ATTR-IN      PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300     03 FLMANAT-IN           PIC X(2).                                    
014400*                                 MFS BEHANDLING AV INPUTFÄLT             
014500     03 TISLJUST-C1-ATTR-IN  PIC X(2).                                    
014600*                                 MFS ATTRIBUTFÄLT                        
014700     03 TISLJUST-C1-IN       PIC X(4).                                    
014800*                                 VECKA DÅ JUSTERING AV SÄKER-            
014900*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
015000     03 TISLJUST-C2-ATTR-IN  PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200     03 TISLJUST-C2-IN       PIC X(4).                                    
015300*                                 VECKA DÅ JUSTERING AV SÄKER-            
015400*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
015500     03 KVBK-ATTR-UT         PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700     03 KVBK-UT              PIC Z(6)9.                                   
015800*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
015900     03 FLMANBK-ATTR-UT      PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 FLMANBK-UT           PIC X(2).                                    
016200*                                 MFS BEHANDLING AV INPUTFÄLT             
016300     03 KVSLAGER-C1-ATTR-UT  PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500     03 KVSLAGER-C1-UT       PIC Z(6)9.                                   
016600*                                 SÄKERHETSLAGER                          
016700     03 KVSLAGER-C2-ATTR-UT  PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900     03 KVSLAGER-C2-UT       PIC Z(6)9.                                   
017000*                                 SÄKERHETSLAGER                          
017100     03 KVBK-ATTR-IN         PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300     03 KVBK-IN              PIC Z(6)9.                                   
017400*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
017500     03 FLMANBK-ATTR-IN      PIC X(2).                                    
017600*                                 MFS ATTRIBUTFÄLT                        
017700     03 FLMANBK-IN           PIC X(2).                                    
017800*                                 MFS BEHANDLING AV INPUTFÄLT             
017900     03 KVSLAGER-C1-ATTR-IN  PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100     03 KVSLAGER-C1-IN       PIC X(7).                                    
018200*                                 SÄKERHETSLAGER                          
018300     03 KVSLAGER-C2-ATTR-IN  PIC X(2).                                    
018400*                                 MFS ATTRIBUTFÄLT                        
018500     03 KVSLAGER-C2-IN       PIC X(7).                                    
018600*                                 SÄKERHETSLAGER                          
018700     03 KVQ-ATTR-UT          PIC X(2).                                    
018800*                                 MFS ATTRIBUTFÄLT                        
018900     03 KVQ-UT               PIC Z(6)9.                                   
019000*                                 EKONOMISK HEMTAGNINGSKVANTITET          
019100     03 FLMANQ-ATTR-UT       PIC X(2).                                    
019200*                                 MFS ATTRIBUTFÄLT                        
019300     03 FLMANQ-UT            PIC X(2).                                    
019400*                                 MFS BEHANDLING AV INPUTFÄLT             
019500     03 KVULOAD-ATTR-UT      PIC X(2).                                    
019600*                                 MFS ATTRIBUTFÄLT                        
019700     03 KVULOAD-UT           PIC Z(6)9.                                   
019800*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
019900     03 KVQ-ATTR-IN          PIC X(2).                                    
020000*                                 MFS ATTRIBUTFÄLT                        
020100     03 KVQ-IN               PIC Z(6)9.                                   
020200*                                 EKONOMISK HEMTAGNINGSKVANTITET          
020300     03 FLMANQ-ATTR-IN       PIC X(2).                                    
020400*                                 MFS ATTRIBUTFÄLT                        
020500     03 FLMANQ-IN            PIC X(2).                                    
020600*                                 MFS BEHANDLING AV INPUTFÄLT             
020700     03 KVULOAD-ATTR-IN      PIC X(2).                                    
020800*                                 MFS ATTRIBUTFÄLT                        
020900     03 KVULOAD-IN           PIC Z(6)9.                                   
021000*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
021100     03 KVQ-JUST-ATTR-UT     PIC X(2).                                    
021200*                                 MFS ATTRIBUTFÄLT                        
021300     03 KVQ-JUST-UT          PIC Z(6)9.                                   
021400*                                 EKONOMISK HEMTAGNINGSKVANTITET          
021500     03 TIQJUST-ATTR-UT      PIC X(2).                                    
021600*                                 MFS ATTRIBUTFÄLT                        
021700     03 TIQJUST-UT           PIC X(4).                                    
021800*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
021900     03 KVPALL-ATTR-UT       PIC X(2).                                    
022000*                                 MFS ATTRIBUTFÄLT                        
022100     03 KVPALL-UT            PIC Z(6)9.                                   
022200*                                 ANTAL I PALL                            
022300     03 KVQ-JUST-ATTR-IN     PIC X(2).                                    
022400*                                 MFS ATTRIBUTFÄLT                        
022500     03 KVQ-JUST-IN          PIC X(7).                                    
022600*                                 EKONOMISK HEMTAGNINGSKVANTITET          
022700     03 TIQJUST-ATTR-IN      PIC X(2).                                    
022800*                                 MFS ATTRIBUTFÄLT                        
022900     03 TIQJUST-IN           PIC X(4).                                    
023000*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
023100     03 KVPALL-ATTR-IN       PIC X(2).                                    
023200*                                 MFS ATTRIBUTFÄLT                        
023300     03 KVPALL-IN            PIC X(7).                                    
023400*                                 ANTAL I PALL                            
023500     03 MESSAGE-RAD23        PIC X(79).                                   
023600*                                 MEDDELANDEFÄLT PÅ RAD 23                
023700*** END OF VILMAII-COPY LENGTH= 503 BYTES                                 
