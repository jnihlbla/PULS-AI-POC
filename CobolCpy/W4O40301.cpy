000100 01  MOD-W4O40301.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MOD-IDDC-UT          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 MOD-FLDCRET          PIC X.                                       
001600*                                 GODKÄND FÖR RETUR                       
001700*                                 RETURN APPROVED                         
001800     03 MOD-FLDCRET-IN-ATTR  PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-FLDCRET-IN       PIC X.                                       
002100*                                 GODKÄND FÖR RETUR                       
002200*                                 RETURN APPROVED                         
002300     03 MOD-FLFSEDEL         PIC X.                                       
002400*                                 FÖLJESEDELS FLAGGA                      
002500*                                 J = FÖLJESEDEL SKALL SKAPAS             
002600*                                 N = FÖLJESEDEL SKALL EJ SKAPAS          
002700     03 MOD-FLFSEDEL-IN-ATTR PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLFSEDEL-IN      PIC X.                                       
003000*                                 FÖLJESEDELS FLAGGA                      
003100*                                 J = FÖLJESEDEL SKALL SKAPAS             
003200*                                 N = FÖLJESEDEL SKALL EJ SKAPAS          
003300     03 MOD-FLINLREP         PIC X.                                       
003400*                                 FLAGGA AVVIKELSERAPPORTER               
003500*                                 DEVIATION REPORT FLAG                   
003600     03 MOD-FLINLREP-IN-ATTR PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-FLINLREP-IN      PIC X.                                       
003900*                                 FLAGGA AVVIKELSERAPPORTER               
004000*                                 DEVIATION REPORT FLAG                   
004100     03 MOD-FLINLHIST        PIC X.                                       
004200*                                 OM INLEVERANSHIST. SKA SKAPAS           
004300*                                 IF HIST. OF INBOUND BE CREATED          
004400     03 MOD-FLINLHIST-IN-ATTR                                             
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FLINLHIST-IN     PIC X.                                       
004800*                                 OM INLEVERANSHIST. SKA SKAPAS           
004900*                                 IF HIST. OF INBOUND BE CREATED          
005000     03 MOD-FLPRISSPR        PIC X.                                       
005100*                                 PRISSPÄRRSFLAGGA                        
005200*                                 BLOCKED PRICE MARK                      
005300     03 MOD-FLPRISSPR-IN-ATTR                                             
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-FLPRISSPR-IN     PIC X.                                       
005700*                                 PRISSPÄRRSFLAGGA                        
005800*                                 BLOCKED PRICE MARK                      
005900     03 MOD-FLBINNUT         PIC X.                                       
006000*                                 BINNING RAPPORT UTSKRIFTFLAGGA          
006100*                                 FLAG FOR BINNING REPORT                 
006200     03 MOD-FLBINNUT-IN-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-FLBINNUT-IN      PIC X.                                       
006500*                                 BINNING RAPPORT UTSKRIFTFLAGGA          
006600*                                 FLAG FOR BINNING REPORT                 
006700     03 MOD-FLSEASBER        PIC X.                                       
006800*                                 SÄSONGSBERÄKNGSFLAGGA                   
006900*                                 SEASON MARK COMPUTING                   
007000     03 MOD-FLSEASBER-IN-ATTR                                             
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-FLSEASBER-IN     PIC X.                                       
007400*                                 SÄSONGSBERÄKNGSFLAGGA                   
007500*                                 SEASON MARK COMPUTING                   
007600     03 MOD-FLTYP6JU         PIC X.                                       
007700*                                 TYPE-6 ADJUSTMENT                       
007800*                                 TYPE-6 ADJUSTMENT                       
007900     03 MOD-FLTYP6JU-IN-ATTR PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-FLTYP6JU-IN      PIC X.                                       
008200*                                 TYPE-6 ADJUSTMENT                       
008300*                                 TYPE-6 ADJUSTMENT                       
008400     03 MOD-FLRSI            PIC X.                                       
008500*                                 FLAGGA SKEPPNINGSINFO TILL VIPS         
008600*                                 FLAG SHIPMENT INFO TO VIPS              
008700     03 MOD-FLRSI-IN-ATTR    PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-FLRSI-IN         PIC X.                                       
009000*                                 FLAGGA SKEPPNINGSINFO TILL VIPS         
009100*                                 FLAG SHIPMENT INFO TO VIPS              
009200     03 MOD-KDPORDL          PIC X.                                       
009300*                                 PACKAD ORDERLISTA MÖJLIG                
009400*                                 PACKED ORDER LIST POSSIBLE              
009500     03 MOD-KDPORDL-IN-ATTR  PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-KDPORDL-IN       PIC X.                                       
009800*                                 PACKAD ORDERLISTA MÖJLIG                
009900*                                 PACKED ORDER LIST POSSIBLE              
010000     03 MOD-IDLISTNR         PIC 9(3).                                    
010100*                                 LISTNUMMER                              
010200*                                 LISTNUMMER                              
010300     03 MOD-IDLISTNR-IN-ATTR PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-IDLISTNR-IN      PIC 9(3).                                    
010600*                                 LISTNUMMER                              
010700*                                 LISTNUMMER                              
010800     03 MOD-FLSAMPAK         PIC X.                                       
010900*                                 SAMPACKNING AV KOLLI                    
011000*                                 CO-PACKING OF CASE                      
011100     03 MOD-FLSAMPAK-IN-ATTR PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-FLSAMPAK-IN      PIC X.                                       
011400*                                 SAMPACKNING AV KOLLI                    
011500*                                 CO-PACKING OF CASE                      
011600     03 MOD-FLEXCP1-PRIO     PIC X.                                       
011700*                                 UNDANTAGSREGEL I PRIOBERÄKNING          
011800*                                 EXC. RULE WHEN CALC. PRIORITY           
011900     03 MOD-FLEXCP1-PRIO-IN-ATTR                                          
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200     03 MOD-FLEXCP1-PRIO-IN  PIC X.                                       
012300*                                 UNDANTAGSREGEL I PRIOBERÄKNING          
012400*                                 EXC. RULE WHEN CALC. PRIORITY           
012500     03 MOD-FLEXCP1-REFBEO   PIC X.                                       
012600*                                 UND.REGEL AKT. REFILLBEORDRING          
012700*                                 IT WILL BE COMPARED WITH ORDER          
012800*                                 HITS ROLL 12 PERIODS                    
012900     03 MOD-FLEXCP1-REFBEO-IN-ATTR                                        
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-FLEXCP1-REFBEO-IN                                             
013300                             PIC X.                                       
013400*                                 UND.REGEL AKT. REFILLBEORDRING          
013500*                                 IT WILL BE COMPARED WITH ORDER          
013600*                                 HITS ROLL 12 PERIODS                    
013700     03 MOD-FLEXCP4-REFBEO   PIC X.                                       
013800*                                 UND.REGEL AKT. REFILLBEORDRING          
013900*                                 IT WILL BE COMPARED WITH ORDER          
014000*                                 HITS ROLL 12 PERIODS                    
014100     03 MOD-FLEXCP4-REFBEO-IN-ATTR                                        
014200                             PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 MOD-FLEXCP4-REFBEO-IN                                             
014500                             PIC X.                                       
014600*                                 UND.REGEL AKT. REFILLBEORDRING          
014700*                                 IT WILL BE COMPARED WITH ORDER          
014800*                                 HITS ROLL 12 PERIODS                    
014900     03 MOD-FLEXCP2-REFBEO   PIC X.                                       
015000*                                 UND.REGEL AKT. REFILLBEORDRING          
015100*                                 WILL BE COMPARED WITH PRICE KLA         
015200*                                 SS                                      
015300     03 MOD-FLEXCP2-REFBEO-IN-ATTR                                        
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600     03 MOD-FLEXCP2-REFBEO-IN                                             
015700                             PIC X.                                       
015800*                                 UND.REGEL AKT. REFILLBEORDRING          
015900*                                 WILL BE COMPARED WITH PRICE KLA         
016000*                                 SS                                      
016100     03 MOD-FLEXCP3-REFBEO   PIC X.                                       
016200*                                 UND.REGEL AKT. REFILLBEORDRING          
016300*                                 USED AS A FLAG WITH PRODUCT GRO         
016400*                                 UP 15                                   
016500     03 MOD-FLEXCP3-REFBEO-IN-ATTR                                        
016600                             PIC X(2).                                    
016700*                                 MFS ATTRIBUTFÄLT                        
016800     03 MOD-FLEXCP3-REFBEO-IN                                             
016900                             PIC X.                                       
017000*                                 UND.REGEL AKT. REFILLBEORDRING          
017100*                                 USED AS A FLAG WITH PRODUCT GRO         
017200*                                 UP 15                                   
017300     03 MOD-FLEXCP1-REFBER   PIC X.                                       
017400*                                 UND.REGEL I REFILLBERÄKNINGEN           
017500*                                 EXC. RULE CALC. REFILLORDERS            
017600     03 MOD-FLEXCP1-REFBER-IN-ATTR                                        
017700                             PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900     03 MOD-FLEXCP1-REFBER-IN                                             
018000                             PIC X.                                       
018100*                                 UND.REGEL I REFILLBERÄKNINGEN           
018200*                                 EXC. RULE CALC. REFILLORDERS            
018300     03 MOD-FLEXCP2-REFBER   PIC X.                                       
018400*                                 UND.REGEL I REFILLBERÄKNINGEN           
018500*                                 EXC. RULE CALC. REFILLORDERS            
018600     03 MOD-FLEXCP2-REFBER-IN-ATTR                                        
018700                             PIC X(2).                                    
018800*                                 MFS ATTRIBUTFÄLT                        
018900     03 MOD-FLEXCP2-REFBER-IN                                             
019000                             PIC X.                                       
019100*                                 UND.REGEL I REFILLBERÄKNINGEN           
019200*                                 EXC. RULE CALC. REFILLORDERS            
019300     03 MOD-KVINVAUT         PIC Z(8)9.                                   
019400*                                 GRÄNS AUTOMATISK INVENTERING            
019500*                                 LIMIT FOR AUTOMATIC INV.                
019600     03 MOD-KVINVAUT-IN-ATTR PIC X(2).                                    
019700*                                 MFS ATTRIBUTFÄLT                        
019800     03 MOD-KVINVAUT-IN      PIC Z(8)9.                                   
019900*                                 GRÄNS AUTOMATISK INVENTERING            
020000*                                 LIMIT FOR AUTOMATIC INV.                
020100     03 MOD-IDPRTLST-INVA    PIC X(8).                                    
020200*                                 PRINTER FÖR INV.ANMODAN                 
020300*                                 IDENTITY OF INVENTORY PRINTER           
020400     03 MOD-IDPRTLST-INVA-IN-ATTR                                         
020500                             PIC X(2).                                    
020600*                                 MFS ATTRIBUTFÄLT                        
020700     03 MOD-IDPRTLST-INVA-IN PIC X(8).                                    
020800*                                 PRINTER FÖR INV.ANMODAN                 
020900*                                 IDENTITY OF INVENTORY PRINTER           
021000     03 MOD-KVDAGAR-CROSS    PIC 9.                                       
021100*                                 ANTAL DAGAR FÖRDR. CROSS-DOCK           
021200     03 MOD-KVDAGAR-CROSS-IN-ATTR                                         
021300                             PIC X(2).                                    
021400*                                 MFS ATTRIBUTFÄLT                        
021500     03 MOD-KVDAGAR-CROSS-IN PIC 9.                                       
021600*                                 ANTAL DAGAR FÖRDR. CROSS-DOCK           
021700     03 MOD-IDPRTLST-INVAB   PIC X(8).                                    
021800*                                 PRINTER FÖR INV.ANMODAN/BYTES           
021900*                                 IDENTITY OF INVENTORY/? PRINTER         
022000     03 MOD-IDPRTLST-INVAB-IN-ATTR                                        
022100                             PIC X(2).                                    
022200*                                 MFS ATTRIBUTFÄLT                        
022300     03 MOD-IDPRTLST-INVAB-IN                                             
022400                             PIC X(8).                                    
022500*                                 PRINTER FÖR INV.ANMODAN/BYTES           
022600*                                 IDENTITY OF INVENTORY/? PRINTER         
022700     03 MOD-IDPRTLST-INL     PIC X(8).                                    
022800*                                 LOGISK PRINTER+LISTA IDENTITET          
022900*                                 LOGICAL PRINTER+LIST IDENTITY           
023000     03 MOD-IDPRTLST-INL-IN-ATTR                                          
023100                             PIC X(2).                                    
023200*                                 MFS ATTRIBUTFÄLT                        
023300     03 MOD-IDPRTLST-INL-IN  PIC X(8).                                    
023400*                                 LOGISK PRINTER+LISTA IDENTITET          
023500*                                 LOGICAL PRINTER+LIST IDENTITY           
023600     03 MOD-IDPRTLST-INLA    PIC X(8).                                    
023700*                                 LOGISK PRINTER+LISTA IDENTITET          
023800*                                 LOGICAL PRINTER+LIST IDENTITY           
023900     03 MOD-IDPRTLST-INLA-IN-ATTR                                         
024000                             PIC X(2).                                    
024100*                                 MFS ATTRIBUTFÄLT                        
024200     03 MOD-IDPRTLST-INLA-IN PIC X(8).                                    
024300*                                 LOGISK PRINTER+LISTA IDENTITET          
024400*                                 LOGICAL PRINTER+LIST IDENTITY           
024500     03 MOD-KDDCSTYR-BUY     PIC Z(4)9.                                   
024600*                                 REGELVERK VID BUYERTILLDELNING          
024700*                                 RULES OF BUYER                          
024800     03 MOD-KDDCSTYR-BUY-IN-ATTR                                          
024900                             PIC X(2).                                    
025000*                                 MFS ATTRIBUTFÄLT                        
025100     03 MOD-KDDCSTYR-BUY-IN  PIC Z(4)9.                                   
025200*                                 REGELVERK VID BUYERTILLDELNING          
025300*                                 RULES OF BUYER                          
025400     03 MOD-KDDCSTYR-KUND    PIC Z(4)9.                                   
025500*                                 REGELVERK FÖR TILLD. AV KUNDNR          
025600*                                 RULES OF CUSTOMER NO                    
025700     03 MOD-KDDCSTYR-KUND-IN-ATTR                                         
025800                             PIC X(2).                                    
025900*                                 MFS ATTRIBUTFÄLT                        
026000     03 MOD-KDDCSTYR-KUND-IN PIC Z(4)9.                                   
026100*                                 REGELVERK FÖR TILLD. AV KUNDNR          
026200*                                 RULES OF CUSTOMER NO                    
026300     03 MOD-KDDCSTYR-REFTAB  PIC Z(4)9.                                   
026400*                                 REGELVERK TILLD AV REFILLTABELL         
026500*                                 RULES WHICH REFILLTABLE                 
026600     03 MOD-KDDCSTYR-REFTAB-IN-ATTR                                       
026700                             PIC X(2).                                    
026800*                                 MFS ATTRIBUTFÄLT                        
026900     03 MOD-KDDCSTYR-REFTAB-IN                                            
027000                             PIC Z(4)9.                                   
027100*                                 REGELVERK TILLD AV REFILLTABELL         
027200*                                 RULES WHICH REFILLTABLE                 
027300     03 MOD-KVDAGAR-POKS     PIC Z9.                                      
027400*                                 ANT DGR FÖRE RFS ATT NOLL OKSPR         
027500*                                 DAYS BEFORE RFS FOR MOVE OKSPR          
027600     03 MOD-KVDAGAR-POKS-IN-ATTR                                          
027700                             PIC X(2).                                    
027800*                                 MFS ATTRIBUTFÄLT                        
027900     03 MOD-KVDAGAR-POKS-IN  PIC Z9.                                      
028000*                                 ANT DGR FÖRE RFS ATT NOLL OKSPR         
028100*                                 DAYS BEFORE RFS FOR MOVE OKSPR          
028200     03 MOD-KVDAGAR-PP       PIC Z9.                                      
028300*                                 DAGAR FÖR REPARAIONSDATUM               
028400*                                 DAYS BEFORE REPAIR DATE                 
028500     03 MOD-KVDAGAR-PP-IN-ATTR                                            
028600                             PIC X(2).                                    
028700*                                 MFS ATTRIBUTFÄLT                        
028800     03 MOD-KVDAGAR-PP-IN    PIC Z9.                                      
028900*                                 DAGAR FÖR REPARAIONSDATUM               
029000*                                 DAYS BEFORE REPAIR DATE                 
029100     03 MOD-KDAKDISP-DAG     PIC 9.                                       
029200*                                 HUR AKS RÄKNAS I DISPONIBELT            
029300*                                 HOW AKS COUNTS IN AVAILABILITY          
029400     03 MOD-KDAKDISP-DAG-IN-ATTR                                          
029500                             PIC X(2).                                    
029600*                                 MFS ATTRIBUTFÄLT                        
029700     03 MOD-KDAKDISP-DAG-IN  PIC 9.                                       
029800*                                 HUR AKS RÄKNAS I DISPONIBELT            
029900*                                 HOW AKS COUNTS IN AVAILABILITY          
030000     03 MOD-KDAKDISP-BULK    PIC 9.                                       
030100*                                 HUR AKS RÄKNAS I DISPONIBELT            
030200*                                 HOW AKS COUNTS IN AVAILABILITY          
030300     03 MOD-KDAKDISP-BULK-IN-ATTR                                         
030400                             PIC X(2).                                    
030500*                                 MFS ATTRIBUTFÄLT                        
030600     03 MOD-KDAKDISP-BULK-IN PIC 9.                                       
030700*                                 HUR AKS RÄKNAS I DISPONIBELT            
030800*                                 HOW AKS COUNTS IN AVAILABILITY          
030900     03 MOD-FLCLEAR-BULK     PIC X.                                       
031000*                                 BULK ORDERRAD CLEAR FLAGGA              
031100*                                 CLEARING FLAG FOR BULK ORDER            
031200     03 MOD-FLCLEAR-BULK-IN-ATTR                                          
031300                             PIC X(2).                                    
031400*                                 MFS ATTRIBUTFÄLT                        
031500     03 MOD-FLCLEAR-BULK-IN  PIC X.                                       
031600*                                 BULK ORDERRAD CLEAR FLAGGA              
031700*                                 CLEARING FLAG FOR BULK ORDER            
031800     03 MOD-KDSKRMET         PIC 9.                                       
031900*                                 SKROTMETOD                              
032000*                                 SCRAP METHOD                            
032100     03 MOD-KDSKRMET-IN-ATTR PIC X(2).                                    
032200*                                 MFS ATTRIBUTFÄLT                        
032300     03 MOD-KDSKRMET-IN      PIC 9.                                       
032400*                                 SKROTMETOD                              
032500*                                 SCRAP METHOD                            
032600     03 MOD-REQXBRYT         PIC Z9.                                      
032700*                                 KVANTBRYTNING                           
032800*                                 NUMBER                                  
032900     03 MOD-REQXBRYT-IN-ATTR PIC X(2).                                    
033000*                                 MFS ATTRIBUTFÄLT                        
033100     03 MOD-REQXBRYT-IN      PIC Z9.                                      
033200*                                 KVANTBRYTNING                           
033300*                                 NUMBER                                  
033400     03 MOD-REWILSON         PIC 9.9(2).                                  
033500*                                 PROCENTREGEL FÖR WILSONFORMEL           
033600     03 MOD-REWILSON-IN-ATTR PIC X(2).                                    
033700*                                 MFS ATTRIBUTFÄLT                        
033800     03 MOD-REWILSON-IN      PIC X(4).                                    
033900*                                 PROCENTREGEL FÖR WILSONFORMEL           
034000     03 MOD-IDDC-REF         PIC X(2).                                    
034100*                                 SÄNDANDE LAGER FÖR REFILL               
034200*                                 SENDING WAREHOUSE FOR REFILL            
034300     03 MOD-IDDC-REF-IN-ATTR PIC X(2).                                    
034400*                                 MFS ATTRIBUTFÄLT                        
034500     03 MOD-IDDC-REF-IN      PIC X(2).                                    
034600*                                 SÄNDANDE LAGER FÖR REFILL               
034700*                                 SENDING WAREHOUSE FOR REFILL            
034800     03 MOD-TID-RETOS        PIC X(2).                                    
034900*                                 VECKODAG FÖR AUTOMATRETUR MÅN=1         
035000*                                 WEEK DAY FOR AUTOM.RETURNS              
035100     03 MOD-TID-RETOS-IN-ATTR                                             
035200                             PIC X(2).                                    
035300*                                 MFS ATTRIBUTFÄLT                        
035400     03 MOD-TID-RETOS-IN     PIC X(2).                                    
035500*                                 VECKODAG FÖR AUTOMATRETUR MÅN=1         
035600*                                 WEEK DAY FOR AUTOM.RETURNS              
035700     03 MOD-TID-RET98        PIC X(2).                                    
035800*                                 VECKODAG FÖR AUTOM.RETUR LO 98          
035900*                                 WEEKDAY AUTOM.RETURNS AREA 98           
036000     03 MOD-TID-RET98-IN-ATTR                                             
036100                             PIC X(2).                                    
036200*                                 MFS ATTRIBUTFÄLT                        
036300     03 MOD-TID-RET98-IN     PIC X(2).                                    
036400*                                 VECKODAG FÖR AUTOM.RETUR LO 98          
036500*                                 WEEKDAY AUTOM.RETURNS AREA 98           
036600     03 MOD-PRARTSTD-SKRLO98 PIC Z(2)9.                                   
036700*                                 PRISGRÄNS FÖR SKROTNING PÅ LO98         
036800*                                 PRICE LIMIT FOR SCRAPING AREA98         
036900     03 MOD-PRARTSTD-SKRLO98-IN-ATTR                                      
037000                             PIC X(2).                                    
037100*                                 MFS ATTRIBUTFÄLT                        
037200     03 MOD-PRARTSTD-SKRLO98-IN                                           
037300                             PIC 9(3).                                    
037400*                                 PRISGRÄNS FÖR SKROTNING PÅ LO98         
037500*                                 PRICE LIMIT FOR SCRAPING AREA98         
037600     03 MOD-FLTRLO88-MAN     PIC X.                                       
037700*                                 FLAGGA LO-88 TRANSFER-MÅN OK?           
037800*                                 FLAG TRANSFER-MON TO AREA-88 OK         
037900     03 MOD-FLTRLO88-MAN-IN-ATTR                                          
038000                             PIC X(2).                                    
038100*                                 MFS ATTRIBUTFÄLT                        
038200     03 MOD-FLTRLO88-MAN-IN  PIC X.                                       
038300*                                 FLAGGA LO-88 TRANSFER-MÅN OK?           
038400*                                 FLAG TRANSFER-MON TO AREA-88 OK         
038500     03 MOD-FLTRLO88-TIS     PIC X.                                       
038600*                                 FLAGGA LO-88 TRANSFER-TIS OK?           
038700*                                 FLAG TRANSFER-TUE TO AREA-88 OK         
038800     03 MOD-FLTRLO88-TIS-IN-ATTR                                          
038900                             PIC X(2).                                    
039000*                                 MFS ATTRIBUTFÄLT                        
039100     03 MOD-FLTRLO88-TIS-IN  PIC X.                                       
039200*                                 FLAGGA LO-88 TRANSFER-TIS OK?           
039300*                                 FLAG TRANSFER-TUE TO AREA-88 OK         
039400     03 MOD-FLTRLO88-ONS     PIC X.                                       
039500*                                 FLAGGA LO-88 TRANSFER-ONS OK?           
039600*                                 FLAG TRANSFER-WEN TO AREA-88 OK         
039700     03 MOD-FLTRLO88-ONS-IN-ATTR                                          
039800                             PIC X(2).                                    
039900*                                 MFS ATTRIBUTFÄLT                        
040000     03 MOD-FLTRLO88-ONS-IN  PIC X.                                       
040100*                                 FLAGGA LO-88 TRANSFER-ONS OK?           
040200*                                 FLAG TRANSFER-WEN TO AREA-88 OK         
040300     03 MOD-FLTRLO88-TOR     PIC X.                                       
040400*                                 FLAGGA LO-88 TRANSFER-TOR OK?           
040500*                                 FLAG TRANSFER-THU TO AREA-88 OK         
040600     03 MOD-FLTRLO88-TOR-IN-ATTR                                          
040700                             PIC X(2).                                    
040800*                                 MFS ATTRIBUTFÄLT                        
040900     03 MOD-FLTRLO88-TOR-IN  PIC X.                                       
041000*                                 FLAGGA LO-88 TRANSFER-TOR OK?           
041100*                                 FLAG TRANSFER-THU TO AREA-88 OK         
041200     03 MOD-FLTRLO88-FRE     PIC X.                                       
041300*                                 FLAGGA LO-88 TRANSFER-FRE OK?           
041400*                                 FLAG TRANSFER-FRI TO AREA-88 OK         
041500     03 MOD-FLTRLO88-FRE-IN-ATTR                                          
041600                             PIC X(2).                                    
041700*                                 MFS ATTRIBUTFÄLT                        
041800     03 MOD-FLTRLO88-FRE-IN  PIC X.                                       
041900*                                 FLAGGA LO-88 TRANSFER-FRE OK?           
042000*                                 FLAG TRANSFER-FRI TO AREA-88 OK         
042100     03 MOD-FLKNDVAL         PIC X.                                       
042200*                                 FLAGGA AVVIKELSERAPPORTER               
042300*                                 DEVIATION REPORT FLAG                   
042400     03 MOD-FLKNDVAL-IN-ATTR PIC X(2).                                    
042500*                                 MFS ATTRIBUTFÄLT                        
042600     03 MOD-FLKNDVAL-IN      PIC X.                                       
042700*                                 FLAGGA AVVIKELSERAPPORTER               
042800*                                 DEVIATION REPORT FLAG                   
042900     03 MOD-KDFAKTDC         PIC X.                                       
043000*                                 FAKT. STATISTIK URVAL PER DC            
043100*                                 IF INVOICE STATISTIC PER DC             
043200     03 MOD-KDFAKTDC-IN-ATTR PIC X(2).                                    
043300*                                 MFS ATTRIBUTFÄLT                        
043400     03 MOD-KDFAKTDC-IN      PIC X.                                       
043500*                                 FAKT. STATISTIK URVAL PER DC            
043600*                                 IF INVOICE STATISTIC PER DC             
043700     03 MOD-FLOVRLAGBER      PIC X.                                       
043800*                                 SDC ÖVERLAGER INKL CDC                  
043900*                                 SDC OVERSTOCK INCL CDC                  
044000     03 MOD-FLOVRLAGBER-IN-ATTR                                           
044100                             PIC X(2).                                    
044200*                                 MFS ATTRIBUTFÄLT                        
044300     03 MOD-FLOVRLAGBER-IN   PIC X.                                       
044400*                                 SDC ÖVERLAGER INKL CDC                  
044500*                                 SDC OVERSTOCK INCL CDC                  
044600     03 MOD-SUINVGRANS       PIC Z(8)9.                                   
044700*                                 GRÄNS INVENTERING DESCREPENCY           
044800*                                 LIMIT FOR INVENTORY DESCREPENCY         
044900     03 MOD-SUINVGRANS-IN-ATTR                                            
045000                             PIC X(2).                                    
045100*                                 MFS ATTRIBUTFÄLT                        
045200     03 MOD-SUINVGRANS-IN    PIC Z(8)9.                                   
045300*                                 GRÄNS INVENTERING DESCREPENCY           
045400*                                 LIMIT FOR INVENTORY DESCREPENCY         
045500     03 MOD-FLARTADD         PIC X.                                       
045600*                                 VISAR OM ART SKA ISRT PÅ K711           
045700*                                 SHOWS IF PART MUST ISRT ON K711         
045800     03 MOD-FLARTADD-IN-ATTR PIC X(2).                                    
045900*                                 MFS ATTRIBUTFÄLT                        
046000     03 MOD-FLARTADD-IN      PIC X.                                       
046100*                                 VISAR OM ART SKA ISRT PÅ K711           
046200*                                 SHOWS IF PART MUST ISRT ON K711         
046300     03 MOD-TEMFSINF         PIC X(55).                                   
046400*                                 INFORMATIONSMEDDELANDE                  
046500*                                 INFORMATION MESSAGE                     
046600*** END OF VILMAII-COPY LENGTH= 441 BYTES                                 
