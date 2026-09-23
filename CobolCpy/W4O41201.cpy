000100 01  MOD-W4O41201.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-FLDISTCUST-IN    PIC X.                                       
001600     03 MOD-FLDISTCUST-UT    PIC X.                                       
001700     03 MOD-TEWARNING-ATTR   PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-TEWARNING        PIC X(43).                                   
002000     03 MOD-FLAUTORD-ATTR    PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-FLAUTORD         PIC X.                                       
002300*                                 FLAGGAN STYR OM AUTOMATORDER-           
002400*                                 NUMMER SKALL SKAPAS                     
002500     03 MOD-IDRFTAB-ATTR     PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-IDRFTAB          PIC X(3).                                    
002800*                                 RANSONERINGSFAKTORTABELL                
002900     03 MOD-FLNC-ATTR        PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-FLNC             PIC X.                                       
003200*                                 NEW CONCEPT FLAGGA                      
003300     03 MOD-KDORDING-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-KDORDING         PIC 9.                                       
003600*                                 UPPDATERING ORDERINGÅNG                 
003700     03 MOD-FLVR-ATTR        PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-FLVR             PIC X.                                       
004000*                                 ANSLUTEN TILL VR-SYST                   
004100     03 MOD-KVVECKOR-OB-UT   PIC Z(2)9.                                   
004200*                                 LAGRINGSTID I VECKOR, ORDBEKR.          
004300     03 MOD-KVVECKOR-OB-IN-ATTR                                           
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KVVECKOR-OB-IN   PIC Z(2)9.                                   
004700*                                 LAGRINGSTID I VECKOR, ORDBEKR.          
004800     03 MOD-FLURSRAP-ATTR    PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-FLURSRAP         PIC X.                                       
005100*                                 URSPRUNGSRAPPORTERING VID               
005200*                                 PACKNING                                
005300     03 MOD-TVSVOR           OCCURS 16 TIMES.                             
005400*                                 GODKÄNDA DC FÖR TVS VOR-SLÄPP           
005500        05 MOD-IDDC-TVSVOR-ATTR                                           
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDDC-TVSVOR   PIC X(2).                                    
005900*                                 TVÅNGSSTYRNING AV VOR-SLÄPP             
006000*                                 FRÅN DC                                 
006100     03 MOD-FLSAMFAK-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-FLSAMFAK         PIC X.                                       
006400*                                 SAMFAKTURERING NEW CONCEPT              
006500     03 MOD-FLSWCONS-ATTR    PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-FLSWCONS         PIC X.                                       
006800     03 MOD-PREPLAN          OCCURS 8 TIMES.                              
006900*                                 PRE-PLANNED RELEASE TO DC               
007000        05 MOD-IDDC-PREPLAN-ATTR                                          
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-IDDC-PREPLAN  PIC X(2).                                    
007400*                                 PRE-PLANNED RELEASE TO DC               
007500     03 MOD-FLPRERS-ATTR     PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLPRERS          PIC X.                                       
007800*                                 PRISERSÄTTNINGSFLAGGA                   
007900     03 MOD-FLFAKVKT-ATTR    PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-FLFAKVKT         PIC X.                                       
008200*                                 VIKT ANGES I FAKTURAN                   
008300     03 MOD-REAVDRAG-UT      PIC Z9.9.                                    
008400*                                 AVDRAGSPROCENT                          
008500     03 MOD-REAVDRAG-IN-ATTR PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-REAVDRAG-IN      PIC Z9.9.                                    
008800*                                 AVDRAGSPROCENT                          
008900     03 MOD-FLFAKURS-ATTR    PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-FLFAKURS         PIC X.                                       
009200*                                 URSPRUNGLAND I FAKTURA                  
009300     03 MOD-REEMBHNT-UT      PIC Z9.9.                                    
009400*                                 EMB OCH HANTERINGSKOST (%)              
009500     03 MOD-REEMBHNT-IN-ATTR PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-REEMBHNT-IN      PIC Z9.9.                                    
009800*                                 EMB OCH HANTERINGSKOST (%)              
009900     03 MOD-KDSTATNR-ATTR    PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-KDSTATNR         PIC X.                                       
010200*                                 STATNUMMER TYP                          
010300*                                 1 = NORSKT                              
010400*                                 2 = ENGELSKT                            
010500*                                 3 = BELGISKT                            
010600*                                 4 = PERUANSKT                           
010700*                                 5 = SVENSKT                             
010800*                                 6 =                                     
010900     03 MOD-KDHBLKRV-ATTR    PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-KDHBLKRV         PIC X.                                       
011200*                                 HANDELSBLOCK                            
011300     03 MOD-KDSPRAK-ATTR     PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-KDSPRAK          PIC X.                                       
011600*                                 SPRÅKKOD                                
011700     03 MOD-IDSKYLT-UT       PIC X(3).                                    
011800*                                 NATIONALITETSTECKEN                     
011900*                                 SPRÅKIDENTIFIKATION                     
012000     03 MOD-IDSKYLT-IN-ATTR  PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200     03 MOD-IDSKYLT-IN       PIC X(3).                                    
012300*                                 NATIONALITETSTECKEN                     
012400*                                 SPRÅKIDENTIFIKATION                     
012500     03 MOD-IDPARTNR-ATTR    PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-IDPARTNR         PIC X(9).                                    
012800*                                 FINANCIELL KUND                         
012900     03 MOD-IDFTG-ATTR       PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-IDFTG            PIC 9(2).                                    
013200*                                 FÖRETAGSID EKONOM REDOVISNING           
013300     03 MOD-IDLEVNR-UT       PIC X(5).                                    
013400*                                 LEVERANTÖRNUMMER                        
013500     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
013800*                                 LEVERANTÖRNUMMER                        
013900     03 MOD-BETEXT-INFO      PIC X(39).                                   
014000     03 MOD-TEMFSINF         PIC X(55).                                   
014100*                                 INFORMATIONSMEDDELANDE                  
014200*** END OF VILMAII-COPY LENGTH= 408 BYTES                                 
