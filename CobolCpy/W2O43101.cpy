000100 01  W2O43101.                                                            
000200*                                 COPYTEXT FÖR MOD W2O43101               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 IDARTNR.                                                          
001000        05 IDARTNR-UT        PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 DASH-1            PIC X.                                       
001300        05 REKSIFFR          PIC X.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 IDDC-IN              PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 IDDC-UT              PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 BEART-ENG            PIC X(25).                                   
002000*                                 ENGELSK ARTIKELBENÄMNING                
002100     03 IDANSK-ATTR-UT       PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 IDANSK-UT            PIC Z(2)9.                                   
002400*                                 ANSKAFFARNUMMER                         
002500     03 IDPLANGR-AG-ATTR-UT  PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 IDPLANGR-AG-UT       PIC 9.                                       
002800*                                 PLANERINGSGRUPP ANSKAFFARE              
002900     03 IDINK-ATTR-UT        PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 IDINK-UT             PIC X(3).                                    
003200*                                 INKÖPARNUMMER                           
003300     03 IDLEVNR-FRAM-ATTR-UT PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 IDLEVNR-FRAM-UT      PIC X(5).                                    
003600*                                 FRAMTIDA LEVERANTÖRNUMMER               
003700     03 IDLEVNR-SHIP-FRAM-ATTR-UT                                         
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 IDLEVNR-SHIP-FRAM-UT PIC X(5).                                    
004100*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
004200     03 TILEVDAT-ATTR-UT     PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 TILEVDAT-UT          PIC 9(6).                                    
004500*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
004600     03 IDANSK-ATTR-IN       PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 IDANSK-IN            PIC Z(2)9.                                   
004900*                                 ANSKAFFARNUMMER                         
005000     03 IDPLANGR-AG-ATTR-IN  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 IDPLANGR-AG-IN       PIC 9.                                       
005300*                                 PLANERINGSGRUPP ANSKAFFARE              
005400     03 IDINK-ATTR-IN        PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 IDINK-IN             PIC X(3).                                    
005700*                                 INKÖPARNUMMER                           
005800     03 IDLEVNR-FRAM-ATTR-IN PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 IDLEVNR-FRAM-IN      PIC X(5).                                    
006100*                                 FRAMTIDA LEVERANTÖRNUMMER               
006200     03 IDLEVNR-SHIP-FRAM-ATTR-IN                                         
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 IDLEVNR-SHIP-FRAM-IN PIC X(5).                                    
006600*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
006700     03 TILEVDAT-ATTR-IN     PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 TILEVDAT-IN          PIC 9(6).                                    
007000*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
007100     03 KVSPANT-ATTR-UT      PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 KVSPANT-UT           PIC Z(6)9.                                   
007400*                                 SPÄRRAT ANTAL                           
007500     03 TIREFSTO-LOC-ATTR-UT PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 TIREFSTO-LOC-UT      PIC 9(6).                                    
007800*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
007900     03 FLJIT-ATTR-UT        PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 FLJIT-UT             PIC X.                                       
008200*                                 JUST-IN-TIME FLAGGA                     
008300     03 FLWILSON-ATTR-UT     PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 FLWILSON-UT          PIC X.                                       
008600*                                 WILSONFORMEL                            
008700     03 IDREFTAB-ATTR-UT     PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 IDREFTAB-UT          PIC X.                                       
009000*                                 IDENTITET REFILLTABELL                  
009100     03 KVSPANT-ATTR-IN      PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 KVSPANT-IN           PIC Z(6)9.                                   
009400*                                 SPÄRRAT ANTAL                           
009500     03 TIREFSTO-LOC-ATTR-IN PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 TIREFSTO-LOC-IN      PIC 9(6).                                    
009800*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
009900     03 FLJIT-ATTR-IN        PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 FLJIT-IN             PIC X.                                       
010200*                                 JUST-IN-TIME FLAGGA                     
010300     03 FLWILSON-ATTR-IN     PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 FLWILSON-IN          PIC X.                                       
010600*                                 WILSONFORMEL                            
010700     03 IDREFTAB-ATTR-IN     PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 IDREFTAB-IN          PIC X.                                       
011000*                                 IDENTITET REFILLTABELL                  
011100     03 KVSLAGER-ATTR-UT     PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 KVSLAGER-UT          PIC Z(6)9.                                   
011400*                                 SÄKERHETSLAGER                          
011500     03 TIMANSEC-ATTR-UT     PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 TIMANSEC-UT          PIC 9(6).                                    
011800*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
011900     03 KVPALL-ATTR-UT       PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 KVPALL-UT            PIC Z(6)9.                                   
012200*                                 ANTAL I PALL                            
012300     03 KDAVT-ATTR-UT        PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 KDAVT-UT             PIC 9.                                       
012600*                                 AVTALSMÄRKNING                          
012700     03 KVSLAGER-ATTR-IN     PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 KVSLAGER-IN          PIC Z(6)9.                                   
013000*                                 SÄKERHETSLAGER                          
013100     03 TIMANSEC-ATTR-IN     PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 TIMANSEC-IN          PIC 9(6).                                    
013400*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
013500     03 KVPALL-ATTR-IN       PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 KVPALL-IN            PIC Z(6)9.                                   
013800*                                 ANTAL I PALL                            
013900     03 KDAVT-ATTR-IN        PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 KDAVT-IN             PIC 9.                                       
014200*                                 AVTALSMÄRKNING                          
014300     03 KVREFBER-ATTR-UT     PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 KVREFBER-UT          PIC Z(6)9.                                   
014600*                                 BERÄKNAD REFILLINGKVANTITET             
014700     03 TIREFPAF-ATTR-UT     PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900     03 TIREFPAF-UT          PIC 9(6).                                    
015000*                                 DATUM MANUELL PÅFYLLNADSKVANT           
015100     03 KVULOAD-ATTR-UT      PIC X(2).                                    
015200*                                 MFS ATTRIBUTFÄLT                        
015300     03 KVULOAD-UT           PIC Z(6)9.                                   
015400*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
015500     03 TISTODAT-LARM-ATTR-UT                                             
015600                             PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800     03 TISTODAT-LARM-UT     PIC 9(6).                                    
015900*                                 STOPPDATUM FÖR LARM-223                 
016000     03 KVREFBER-ATTR-IN     PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 KVREFBER-IN          PIC Z(6)9.                                   
016300*                                 BERÄKNAD REFILLINGKVANTITET             
016400     03 TIREFPAF-ATTR-IN     PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 TIREFPAF-IN          PIC 9(6).                                    
016700*                                 DATUM MANUELL PÅFYLLNADSKVANT           
016800     03 KVULOAD-ATTR-IN      PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 KVULOAD-IN           PIC Z(6)9.                                   
017100*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
017200     03 TISTODAT-LARM-ATTR-IN                                             
017300                             PIC X(2).                                    
017400*                                 MFS ATTRIBUTFÄLT                        
017500     03 TISTODAT-LARM-IN     PIC 9(6).                                    
017600*                                 STOPPDATUM FÖR LARM-223                 
017700     03 KVVECKOR-LT-ATTR-UT  PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900     03 KVVECKOR-LT-UT       PIC Z9.                                      
018000*                                 ANTAL VECKOR LEDTID                     
018100     03 TIMANLED-ATTR-UT     PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 TIMANLED-UT          PIC 9(6).                                    
018400*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
018500     03 KVSLUTKP-ATTR-UT     PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 KVSLUTKP-UT          PIC Z(6)9.                                   
018800*                                 SLUTKÖPSSALDO                           
018900     03 TISLUTKP-ATTR-UT     PIC X(2).                                    
019000*                                 MFS ATTRIBUTFÄLT                        
019100     03 TISLUTKP-UT          PIC X(6).                                    
019200*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
019300     03 KVVECKOR-LT-ATTR-IN  PIC X(2).                                    
019400*                                 MFS ATTRIBUTFÄLT                        
019500     03 KVVECKOR-LT-IN       PIC Z9.                                      
019600*                                 ANTAL VECKOR LEDTID                     
019700     03 TIMANLED-ATTR-IN     PIC X(2).                                    
019800*                                 MFS ATTRIBUTFÄLT                        
019900     03 TIMANLED-IN          PIC 9(6).                                    
020000*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
020100     03 KVSLUTKP-ATTR-IN     PIC X(2).                                    
020200*                                 MFS ATTRIBUTFÄLT                        
020300     03 KVSLUTKP-IN          PIC Z(6)9.                                   
020400*                                 SLUTKÖPSSALDO                           
020500     03 TISLUTKP-ATTR-IN     PIC X(2).                                    
020600*                                 MFS ATTRIBUTFÄLT                        
020700     03 TISLUTKP-IN          PIC X(6).                                    
020800*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
020900     03 KDOPPLAN-ATTR-UT     PIC X(2).                                    
021000*                                 MFS ATTRIBUTFÄLT                        
021100     03 KDOPPLAN-UT          PIC X.                                       
021200*                                 OPTIMAL PLAN INOM FRYSTID               
021300     03 DAPUBL-ATTR-UT       PIC X(2).                                    
021400*                                 MFS ATTRIBUTFÄLT                        
021500     03 DAPUBL-UT            PIC 9(5).                                    
021600     03 DISPATCH-DAY         OCCURS 5 TIMES.                              
021700*                                                                         
021800        05 DISP-DAY-ATTR-IN  PIC X(2).                                    
021900*                                 MFS ATTRIBUTFÄLT                        
022000        05 DISP-DAY          PIC X(2).                                    
022100     03 SAVE-DISP-DAY        OCCURS 5 TIMES                               
022200                             PIC X(2).                                    
022300     03 KDOPPLAN-ATTR-IN     PIC X(2).                                    
022400*                                 MFS ATTRIBUTFÄLT                        
022500     03 KDOPPLAN-IN          PIC X.                                       
022600*                                 OPTIMAL PLAN INOM FRYSTID               
022700     03 DAPUBL-ATTR-IN       PIC X(2).                                    
022800*                                 MFS ATTRIBUTFÄLT                        
022900     03 DAPUBL-IN            PIC 9(5).                                    
023000     03 NOTES-1-ATTR-IN      PIC X(2).                                    
023100*                                 MFS ATTRIBUTFÄLT                        
023200     03 NOTES-1              PIC X(36).                                   
023300     03 NOTES-2-ATTR-IN      PIC X(2).                                    
023400*                                 MFS ATTRIBUTFÄLT                        
023500     03 NOTES-2              PIC X(36).                                   
023600     03 TEMFSINF             PIC X(55).                                   
023700*                                 INFORMATIONSMEDDELANDE                  
023800*** END OF VILMAII-COPY LENGTH= 573 BYTES                                 
