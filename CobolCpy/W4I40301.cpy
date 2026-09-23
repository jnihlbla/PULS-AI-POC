000100 01  MID-W4I40301.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 MID-INPUT.                                                        
000700        05 MID-FLDCRET-IN    PIC X.                                       
000800*                                 GODKÄND FÖR RETUR                       
000900*                                 RETURN APPROVED                         
001000        05 MID-FLFSEDEL-IN   PIC X.                                       
001100*                                 FÖLJESEDELS FLAGGA                      
001200*                                 J = FÖLJESEDEL SKALL SKAPAS             
001300*                                 N = FÖLJESEDEL SKALL EJ SKAPAS          
001400        05 MID-FLINLREP-IN   PIC X.                                       
001500*                                 FLAGGA AVVIKELSERAPPORTER               
001600*                                 DEVIATION REPORT FLAG                   
001700        05 MID-FLINLHIST-IN  PIC X.                                       
001800*                                 OM INLEVERANSHIST. SKA SKAPAS           
001900*                                 IF HIST. OF INBOUND BE CREATED          
002000        05 MID-FLPRISSPR-IN  PIC X.                                       
002100*                                 PRISSPÄRRSFLAGGA                        
002200*                                 BLOCKED PRICE MARK                      
002300        05 MID-FLBINNUT-IN   PIC X.                                       
002400*                                 BINNING RAPPORT UTSKRIFTFLAGGA          
002500*                                 FLAG FOR BINNING REPORT                 
002600        05 MID-FLSEASBER-IN  PIC X.                                       
002700*                                 SÄSONGSBERÄKNGSFLAGGA                   
002800*                                 SEASON MARK COMPUTING                   
002900        05 MID-FLTYP6JU-IN   PIC X.                                       
003000*                                 TYPE-6 ADJUSTMENT                       
003100*                                 TYPE-6 ADJUSTMENT                       
003200        05 MID-FLRSI-IN      PIC X.                                       
003300*                                 FLAGGA SKEPPNINGSINFO TILL VIPS         
003400*                                 FLAG SHIPMENT INFO TO VIPS              
003500        05 MID-KDPORDL-IN    PIC X.                                       
003600*                                 PACKAD ORDERLISTA MÖJLIG                
003700*                                 PACKED ORDER LIST POSSIBLE              
003800        05 MID-IDLISTNR-IN   PIC 9(3).                                    
003900*                                 LISTNUMMER                              
004000*                                 LISTNUMMER                              
004100        05 MID-FLSAMPAK-IN   PIC X.                                       
004200*                                 SAMPACKNING AV KOLLI                    
004300*                                 CO-PACKING OF CASE                      
004400        05 MID-FLEXCP1-PRIO-IN                                            
004500                             PIC X.                                       
004600*                                 UNDANTAGSREGEL I PRIOBERÄKNING          
004700*                                 EXC. RULE WHEN CALC. PRIORITY           
004800        05 MID-FLEXCP1-REFBEO-IN                                          
004900                             PIC X.                                       
005000*                                 UND.REGEL AKT. REFILLBEORDRING          
005100*                                 IT WILL BE COMPARED WITH ORDER          
005200*                                 HITS ROLL 12 PERIODS                    
005300        05 MID-FLEXCP4-REFBEO-IN                                          
005400                             PIC X.                                       
005500*                                 UND.REGEL AKT. REFILLBEORDRING          
005600*                                 IT WILL BE COMPARED WITH ORDER          
005700*                                 HITS ROLL 12 PERIODS                    
005800        05 MID-FLEXCP2-REFBEO-IN                                          
005900                             PIC X.                                       
006000*                                 UND.REGEL AKT. REFILLBEORDRING          
006100*                                 WILL BE COMPARED WITH PRICE KLA         
006200*                                 SS                                      
006300        05 MID-FLEXCP3-REFBEO-IN                                          
006400                             PIC X.                                       
006500*                                 UND.REGEL AKT. REFILLBEORDRING          
006600*                                 USED AS A FLAG WITH PRODUCT GRO         
006700*                                 UP 15                                   
006800        05 MID-FLEXCP1-REFBER-IN                                          
006900                             PIC X.                                       
007000*                                 UND.REGEL I REFILLBERÄKNINGEN           
007100*                                 EXC. RULE CALC. REFILLORDERS            
007200        05 MID-FLEXCP2-REFBER-IN                                          
007300                             PIC X.                                       
007400*                                 UND.REGEL I REFILLBERÄKNINGEN           
007500*                                 EXC. RULE CALC. REFILLORDERS            
007600        05 MID-KVINVAUT-IN   PIC 9(9).                                    
007700*                                 GRÄNS AUTOMATISK INVENTERING            
007800*                                 LIMIT FOR AUTOMATIC INV.                
007900        05 MID-IDPRTLST-INVA-IN                                           
008000                             PIC X(8).                                    
008100*                                 PRINTER FÖR INV.ANMODAN                 
008200*                                 IDENTITY OF INVENTORY PRINTER           
008300        05 MID-KVDAGAR-CROSS-IN                                           
008400                             PIC 9.                                       
008500*                                 ANTAL DAGAR FÖRDR. CROSS-DOCK           
008600        05 MID-IDPRTLST-INVAB-IN                                          
008700                             PIC X(8).                                    
008800*                                 PRINTER FÖR INV.ANMODAN/BYTES           
008900*                                 IDENTITY OF INVENTORY/? PRINTER         
009000        05 MID-IDPRTLST-INL-IN                                            
009100                             PIC X(8).                                    
009200*                                 LOGISK PRINTER+LISTA IDENTITET          
009300*                                 LOGICAL PRINTER+LIST IDENTITY           
009400        05 MID-IDPRTLST-INLA-IN                                           
009500                             PIC X(8).                                    
009600*                                 LOGISK PRINTER+LISTA IDENTITET          
009700*                                 LOGICAL PRINTER+LIST IDENTITY           
009800        05 MID-KDDCSTYR-BUY-IN                                            
009900                             PIC 9(5).                                    
010000*                                 REGELVERK VID BUYERTILLDELNING          
010100*                                 RULES OF BUYER                          
010200        05 MID-KDDCSTYR-KUND-IN                                           
010300                             PIC 9(5).                                    
010400*                                 REGELVERK FÖR TILLD. AV KUNDNR          
010500*                                 RULES OF CUSTOMER NO                    
010600        05 MID-KDDCSTYR-REFTAB-IN                                         
010700                             PIC 9(5).                                    
010800*                                 REGELVERK TILLD AV REFILLTABELL         
010900*                                 RULES WHICH REFILLTABLE                 
011000        05 MID-KVDAGAR-POKS-IN                                            
011100                             PIC 9(2).                                    
011200*                                 ANT DGR FÖRE RFS ATT NOLL OKSPR         
011300*                                 DAYS BEFORE RFS FOR MOVE OKSPR          
011400        05 MID-KVDAGAR-PP-IN PIC 9(2).                                    
011500*                                 DAGAR FÖR REPARAIONSDATUM               
011600*                                 DAYS BEFORE REPAIR DATE                 
011700        05 MID-KDAKDISP-DAG-IN                                            
011800                             PIC 9.                                       
011900*                                 HUR AKS RÄKNAS I DISPONIBELT            
012000*                                 HOW AKS COUNTS IN AVAILABILITY          
012100        05 MID-KDAKDISP-BULK-IN                                           
012200                             PIC 9.                                       
012300*                                 HUR AKS RÄKNAS I DISPONIBELT            
012400*                                 HOW AKS COUNTS IN AVAILABILITY          
012500        05 MID-FLCLEAR-BULK-IN                                            
012600                             PIC X.                                       
012700*                                 BULK ORDERRAD CLEAR FLAGGA              
012800*                                 CLEARING FLAG FOR BULK ORDER            
012900        05 MID-KDSKRMET-IN   PIC 9.                                       
013000*                                 SKROTMETOD                              
013100*                                 SCRAP METHOD                            
013200        05 MID-REQXBRYT-IN   PIC 9(2).                                    
013300*                                 KVANTBRYTNING                           
013400*                                 NUMBER                                  
013500        05 MID-REWILSON-IN   PIC X(4).                                    
013600*                                 PROCENTREGEL FÖR WILSONFORMEL           
013700        05 MID-IDDC-REF-IN   PIC X(2).                                    
013800*                                 SÄNDANDE LAGER FÖR REFILL               
013900*                                 SENDING WAREHOUSE FOR REFILL            
014000        05 MID-TID-RETOS-IN  PIC X(2).                                    
014100*                                 VECKODAG FÖR AUTOMATRETUR MÅN=1         
014200*                                 WEEK DAY FOR AUTOM.RETURNS              
014300        05 MID-TID-RET98-IN  PIC X(2).                                    
014400*                                 VECKODAG FÖR AUTOM.RETUR LO 98          
014500*                                 WEEKDAY AUTOM.RETURNS AREA 98           
014600        05 MID-PRARTSTD-SKRLO98-IN                                        
014700                             PIC 9(3).                                    
014800*                                 PRISGRÄNS FÖR SKROTNING PÅ LO98         
014900*                                 PRICE LIMIT FOR SCRAPING AREA98         
015000        05 MID-FLTRLO88-MAN-IN                                            
015100                             PIC X.                                       
015200*                                 FLAGGA LO-88 TRANSFER-MÅN OK?           
015300*                                 FLAG TRANSFER-MON TO AREA-88 OK         
015400        05 MID-FLTRLO88-TIS-IN                                            
015500                             PIC X.                                       
015600*                                 FLAGGA LO-88 TRANSFER-TIS OK?           
015700*                                 FLAG TRANSFER-TUE TO AREA-88 OK         
015800        05 MID-FLTRLO88-ONS-IN                                            
015900                             PIC X.                                       
016000*                                 FLAGGA LO-88 TRANSFER-ONS OK?           
016100*                                 FLAG TRANSFER-WEN TO AREA-88 OK         
016200        05 MID-FLTRLO88-TOR-IN                                            
016300                             PIC X.                                       
016400*                                 FLAGGA LO-88 TRANSFER-TOR OK?           
016500*                                 FLAG TRANSFER-THU TO AREA-88 OK         
016600        05 MID-FLTRLO88-FRE-IN                                            
016700                             PIC X.                                       
016800*                                 FLAGGA LO-88 TRANSFER-FRE OK?           
016900*                                 FLAG TRANSFER-FRI TO AREA-88 OK         
017000        05 MID-FLKNDVAL-IN   PIC X.                                       
017100*                                 STYRNING PÅ KUND ELLER DISTRIKT         
017200*                                 STEERING CUSTOMOR OR DISTRICT           
017300        05 MID-KDFAKTDC-IN   PIC X.                                       
017400*                                 FAKT. STATISTIK URVAL PER DC            
017500*                                 IF INVOICE STATISTIC PER DC             
017600        05 MID-FLOVRLAGBER-IN                                             
017700                             PIC X.                                       
017800*                                 SDC ÖVERLAGER INKL CDC                  
017900*                                 SDC OVERSTOCK INCL CDC                  
018000        05 MID-SUINVGRANS-IN PIC 9(9).                                    
018100*                                 GRÄNS INVENTERING DESCREPENCY           
018200*                                 LIMIT FOR INVENTORY DESCREPENCY         
018300        05 MID-FLARTADD-IN   PIC X.                                       
018400*                                 VISAR OM ART SKA ISRT PÅ K711           
018500*                                 SHOWS IF PART MUST ISRT ON K711         
018600*** END OF VILMAII-COPY LENGTH= 121 BYTES                                 
