000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9011300.                                                
000300 AUTHOR.         SVANTE BJÖRKBERG, ÄNDRAT AV KATARINA KYMMER.             
000400 DATE-WRITTEN.   NOV 1987/FEB 1990/AUG 1990                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET SVARAR PÅ OM LEVERANS FÖR ÖNSKAT ANTAL                
001100*        KAN TILLGODOSES.                                                 
001200*                                                                         
001300*        ÄNDRAT VINTERN 2017/2018 FÖR ATT FÅ GENSAM LOGIK                 
001400*        MED W9011600 OCH W9033100 GENOM ANROP AV W911SLDO                
001500*        DÄR KONTROLL OM SLADO FINNS GÖRS                                 
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W9T113                                              
001900*        MID:         W9I11301                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:        W9O11301                                             
002300*    STORY 2498359 : CALL NEW SUBMODULE FOR NEW ETA                       
002400*                    IF PF9,DISPLAY VALUES FROM W911ETD                   
002500*                                                                         
002600*                                                                         
002700 ENVIRONMENT DIVISION.                                                    
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77    IDPGM                     PIC X(8)    VALUE 'W9011300'.            
003400 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
003500 77    JA                        PIC X       VALUE 'J'.                   
003600 77    NEJ                       PIC X       VALUE 'N'.                   
003700 77    IDARTNR-WS                PIC X(9)    VALUE SPACE.                 
003800 77    IDDISTR-WS                PIC X(4)    VALUE SPACE.                 
003900 77    IDKUNDNR-WS               PIC X(6)    VALUE SPACE.                 
004000 77    KVBEART-WS                PIC X(6)    VALUE SPACE.                 
004100 77    KDORDKL-WS                PIC X(1)    VALUE SPACE.                 
004200       88 DAGORDER                           VALUE 'D'.                   
004300       88 BULKORDER                          VALUE 'B'.                   
004400 77    IX                        PIC S9(3)   VALUE +0   COMP-3.           
004500 77    SPRAK-IX                  PIC S9(3)   VALUE +0   COMP-3.           
004600 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +288 COMP SYNC.        
004700 77    SW-ALLT-OK                PIC X       VALUE 'J'.                   
004800                                                                          
004900 01    WS-KVAVBART               PIC 9(7)    VALUE ZERO.                  
005000 01    FILLER REDEFINES WS-KVAVBART.                                      
005100   03  FILLER                    PIC X.                                   
005200   03  WS-KVAVBART-X             PIC X(6).                                
005300                                                                          
005400 01    WS-TIaammdd               PIC 9(7)    VALUE ZERO.                  
005500 01    FILLER REDEFINES WS-TIaammdd.                                      
005600   03  FILLER                    PIC X.                                   
005700   03  WS-TIaammdd-X             PIC X(6).                                
005800                                                                          
005900 01  WS-MED                      PIC 9(3).                                
006000 77  WS-IDMFSMED                 PIC X(3)   VALUE SPACE.                  
006100   88  DLEV                                 VALUE '095'.                  
006200                                                                          
006300 77    NYCKLAR-SW                PIC X       VALUE SPACE.                 
006400   88  NYCKLAR-OK                            VALUE 'J'.                   
006500                                                                          
006600                                                                          
006700 01  FELTEXTTABELL.                                                       
006800   03  FILLER            PIC X(3)  VALUE 'B10'.                           
006900   03  FILLER.                                                            
007000     05  FILLER          PIC X(37)                                        
007100               VALUE 'KUNDINFO SAKNAS PÅ REGISTER          '.             
007200     05  FILLER          PIC X(37)                                        
007300               VALUE 'CUSTOMER INFORMATION IS MISSING     '.              
007400                                                                          
007500   03  FILLER            PIC X(3)  VALUE 'B01'.                           
007600   03  FILLER.                                                            
007700     05  FILLER          PIC X(37)                                        
007800               VALUE 'FEL NYCKEL                           '.             
007900     05  FILLER          PIC X(37)                                        
008000               VALUE 'WRONG KEY                           '.              
008100                                                                          
008200   03  FILLER            PIC X(3)  VALUE '010'.                           
008300   03  FILLER.                                                            
008400     05  FILLER          PIC X(37)                                        
008500               VALUE 'ÖNSKAT ANTAL KAN LEVERERAS           '.             
008600     05  FILLER          PIC X(37)                                        
008700               VALUE 'REQUESTED QUANTITY CAN BE DELIVERED '.              
008800                                                                          
008900   03  FILLER            PIC X(3)  VALUE '041'.                           
009000   03  FILLER.                                                            
009100     05  FILLER          PIC X(37)                                        
009200               VALUE 'ENTYDIG ERSÄTTNING                   '.             
009300     05  FILLER          PIC X(37)                                        
009400               VALUE 'FULLY INTERCHANGEABLE               '.              
009500                                                                          
009600   03  FILLER            PIC X(3)  VALUE '052'.                           
009700   03  FILLER.                                                            
009800     05  FILLER          PIC X(37)                                        
009900               VALUE 'PROVARTIKEL                          '.             
010000     05  FILLER          PIC X(37)                                        
010100               VALUE 'SAMPLE PART                         '.              
010200                                                                          
010300   03  FILLER            PIC X(3)  VALUE '053'.                           
010400   03  FILLER.                                                            
010500     05  FILLER          PIC X(37)                                        
010600               VALUE 'TVÅNGSTYRNING EJ MÖJLIG              '.             
010700     05  FILLER          PIC X(37)                                        
010800               VALUE 'STEERING NOT POSSIBLE               '.              
010900                                                                          
011000                                                                          
011100   03  FILLER            PIC X(3)  VALUE '054'.                           
011200   03  FILLER.                                                            
011300     05  FILLER          PIC X(37)                                        
011400               VALUE 'UTGÅNGEN ARTIKEL                     '.             
011500     05  FILLER          PIC X(37)                                        
011600               VALUE 'OBSOLETE PART                       '.              
011700                                                                          
011800   03  FILLER            PIC X(3)  VALUE '055'.                           
011900   03  FILLER.                                                            
012000     05  FILLER          PIC X(37)                                        
012100               VALUE 'MARKNADSSPÄRRAD                      '.             
012200     05  FILLER          PIC X(37)                                        
012300               VALUE 'NOT APPROVED FOR USE IN YOUR COUNTRY'.              
012400                                                                          
012500   03  FILLER            PIC X(3)  VALUE '057'.                           
012600   03  FILLER.                                                            
012700     05  FILLER          PIC X(37)                                        
012800               VALUE 'SATSARTIKEL                          '.             
012900     05  FILLER          PIC X(37)                                        
013000               VALUE 'PART IN A KIT                       '.              
013100                                                                          
013200   03  FILLER            PIC X(3)  VALUE '058'.                           
013300   03  FILLER.                                                            
013400     05  FILLER          PIC X(37)                                        
013500               VALUE 'ARTIKEL OKÄND                        '.             
013600     05  FILLER          PIC X(37)                                        
013700               VALUE 'PART UNKNOWN                        '.              
013800                                                                          
013900   03  FILLER            PIC X(3)  VALUE '061'.                           
014000   03  FILLER.                                                            
014100     05  FILLER          PIC X(37)                                        
014200               VALUE 'EJ ENTYDIG ERSÄTTNING                '.             
014300     05  FILLER          PIC X(37)                                        
014400               VALUE 'VARIABLE REPLACEMENT                '.              
014500                                                                          
014600                                                                          
014700   03  FILLER            PIC X(3)  VALUE '066'.                           
014800   03  FILLER.                                                            
014900     05  FILLER          PIC X(37)                                        
015000               VALUE 'MILITÄRARTIKEL                       '.             
015100     05  FILLER          PIC X(37)                                        
015200               VALUE 'MILITARY PART                       '.              
015300                                                                          
015400   03  FILLER            PIC X(3)  VALUE '067'.                           
015500   03  FILLER.                                                            
015600     05  FILLER          PIC X(37)                                        
015700               VALUE 'SPECIELL ORDERINFO BEHÖVS            '.             
015800     05  FILLER          PIC X(37)                                        
015900               VALUE 'FURTHER INFO IS REQUIRED            '.              
016000                                                                          
016100   03  FILLER            PIC X(3)  VALUE '070'.                           
016200   03  FILLER.                                                            
016300     05  FILLER          PIC X(37)                                        
016400               VALUE 'STORT UTTAG ELLER UNDANTAGSARTIKEL   '.             
016500     05  FILLER          PIC X(37)                                        
016600               VALUE 'BIG QUANT OR EXEPTIONAL PART         '.             
016700                                                                          
016800   03  FILLER            PIC X(3)  VALUE '080'.                           
016900   03  FILLER.                                                            
017000     05  FILLER          PIC X(37)                                        
017100               VALUE 'ANTALET KAN EJ LEVERERAS            '.              
017200     05  FILLER          PIC X(37)                                        
017300               VALUE 'QUANTITY NOT AVAILABLE              '.              
017400                                                                          
017500   03  FILLER            PIC X(3)  VALUE '090'.                           
017600   03  FILLER.                                                            
017700     05  FILLER          PIC X(37)                                        
017800               VALUE 'KVANTITET RESTNOTERAS                '.             
017900     05  FILLER          PIC X(37)                                        
018000               VALUE 'QUANTITY WILL BE BACKORDERED        '.              
018100                                                                          
018200   03  FILLER            PIC X(3)  VALUE '095'.                           
018300   03  FILLER.                                                            
018400     05  FILLER          PIC X(37)                                        
018500               VALUE 'DIREKTLEVERANS                       '.             
018600     05  FILLER          PIC X(37)                                        
018700               VALUE 'DIRECT DELIVERY                     '.              
018800                                                                          
018900   03  FILLER            PIC X(3)  VALUE '098'.                           
019000   03  FILLER.                                                            
019100     05  FILLER          PIC X(37)                                        
019200               VALUE 'EJ KOMPLETT UPPDATERAD ARTIKEL       '.             
019300     05  FILLER          PIC X(37)                                        
019400               VALUE 'PART NOT FULLY UPDATED              '.              
019500                                                                          
019600 01  FILLER REDEFINES FELTEXTTABELL.                                      
019700   03  FELTEXT                      OCCURS 19.                            
019800     05  KOD             PIC X(3).                                        
019900     05  TEXTER          PIC X(74).                                       
020000     05  FILLER REDEFINES TEXTER.                                         
020100       07  MEDDELANDE    PIC X(37)  OCCURS 2.                             
020200                                                                          
020300                                                                          
020400 01  DYNAMISKA-SUBPROGRAM.                                                
020500   03  CBLTDLI                    PIC X(8)   VALUE 'CBLTDLI '.            
020600   03  FELLOG                     PIC X(8)   VALUE 'FELLOG  '.            
020700   03  W005INIT                   PIC X(8)   VALUE 'W005INIT'.            
020800   03  W911SLDO                   PIC X(8)   VALUE 'W911SLDO'.            
020900   03  W911ETD                    PIC X(8)   VALUE 'W911ETD '.            
021000                                                                          
021100                                                                          
021200*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
021300                                                                          
021400 01   FILLER             PIC X(5)  VALUE 'SLDO '.                         
021500*   -COPY W911SLDO                                                        
021600                                                                          
021700 01   FILLER             PIC X(5)  VALUE 'ETD  '.                         
021800*   -COPY W911ETD                                                         
021900                                                                          
022000*                   ****   PARAMETRAR TILL W005INIT                       
022100*01 -COPY WMSGINIT                                                        
022200                                                                          
022300******************************************************************        
022400*                                                                         
022500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
022600*                                                                         
022700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
022800                                                                          
022900*01    MID -COPY W9I11301.                                                
023000                                                                          
023100*01    -COPY WMSGAREA                                                     
023200                                                                          
023300*  03    MOD -COPY W9O11301  -RED MSG-AREA.                               
023400                                                                          
023500*01    -COPY WMFSAREA                                                     
023600                                                                          
023700******************************************************************        
023800*                                                                         
023900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024000*                                                                         
024100 01    IMS-WS.                                                            
024200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
024300                                                                          
024400*                        **** STATUS-KOD FRÅN IMS                         
024500   03    STATUS-WS               PIC XX.                                  
024600     88    SEGMENT-FINNS                     VALUE '  '.                  
024700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
024800                                                                          
024900   03    GODK-STATUSKODER.                                                
025000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
025100                                                                          
025200*                            IMS FUNKTIONSKODER                           
025300*01    -COPY W0003                                                        
025400                                                                          
025500                                                                          
025600 LINKAGE SECTION.                                                         
025700*01    -COPY W0009     -PRE MSG-                                          
025800*01    -COPY W0008     -PRE USEA-                                         
025900     05  FILLER                  PIC X.                                   
026000                                                                          
026100 01  SLDO-WDF1-PCB               PIC X.                                   
026200 01  SLDO-WDF2-PCB               PIC X.                                   
026300 01  SLDO-WDF2A-PCB              PIC X.                                   
026400 01  SLDO-WDK7-PCB               PIC X.                                   
026500 01  SLDO-WDK6-PCB               PIC X.                                   
026600 01  SLDO-XXKJ-PCB               PIC X.                                   
026700 01  SLDO-WDD7-PCB               PIC X.                                   
026800 01  SLDO-BENA-PCB               PIC X.                                   
026900 01  SLDO-WDB3-PCB               PIC X.                                   
027000 01  SLDO-WDR6-PCB               PIC X.                                   
027100 01  SLDO-WDB2-PCB               PIC X.                                   
027110 01  SLDO-WDB1-PCB               PIC X.                                   
027200                                                                          
027800 01  SLDO-KVAN-WDB2-PCB          PIC X.                                   
027900 01  SLDO-KVAN-WDC1-PCB          PIC X.                                   
028000                                                                          
028100 01  SLDO-AREG-WDK6-PCB          PIC X.                                   
028200 01  SLDO-AREG-WDK7-PCB          PIC X.                                   
028300                                                                          
028400 01  SLDO-DLEV-LEVF-PCB          PIC X.                                   
028500 01  SLDO-DLEV-LEVG-PCB          PIC X.                                   
028600 01  SLDO-DLEV-LEVA-PCB          PIC X.                                   
028700 01  SLDO-DLEV-ARTS-PCB          PIC X.                                   
028800 01  SLDO-DLEV-WDB6-PCB          PIC X.                                   
028900 01  SLDO-DLEV-FILA-PCB          PIC X.                                   
029000                                                                          
029100 01  SLDO-SPAR-WDF8-PCB          PIC X.                                   
029200 01  SLDO-SPAR-WDF8A-PCB         PIC X.                                   
029300 01  SLDO-SPAR-WDK6-PCB          PIC X.                                   
029400                                                                          
029500 01  SLDO-SDCA-ARTS-PCB          PIC X.                                   
029600 01  SLDO-SDCA-WDB6-PCB          PIC X.                                   
029700 01  SLDO-SDCA-WDK9-PCB          PIC X.                                   
029800 01  SLDO-SDCA-WDR6-PCB          PIC X.                                   
029900 01  SLDO-SDCA-WDK6-PCB          PIC X.                                   
030000 01  SLDO-SDCA-WDQ4B-PCB         PIC X.                                   
030100 01  SLDO-SDCA-WDQ2-PCB          PIC X.                                   
030200 01  SLDO-SDCA-WDQ4-PCB          PIC X.                                   
030300 01  SLDO-SDCA-WDB6-2-PCB        PIC X.                                   
030400 01  SLDO-SDCA-WDK6-2-PCB        PIC X.                                   
030500 01  SLDO-SDCA-WDK7-2-PCB        PIC X.                                   
030600 01  SLDO-SDCA-WDK7-3-PCB        PIC X.                                   
030700                                                                          
030800 01  SLDO-NDCA-USEA-PCB          PIC X.                                   
030900 01  SLDO-NDCA-WDK7-PCB          PIC X.                                   
031000 01  SLDO-NDCA-WDL6-PCB          PIC X.                                   
031100 01  SLDO-NDCA-WDB6-PCB          PIC X.                                   
031200                                                                          
031300 01  SLDO-RANS-XXKM-PCB          PIC X.                                   
031400 01  SLDO-RANS-ARTM-PCB          PIC X.                                   
031500 01  SLDO-RANS-ARTS-PCB          PIC X.                                   
031600                                                                          
031700 01  SLDO-CDCA-ARTM-PCB          PIC X.                                   
031800 01  SLDO-CDCA-INLB-PCB          PIC X.                                   
031900 01  SLDO-CDCA-WDB2-PCB          PIC X.                                   
032000 01  SLDO-CDCA-WDC1-PCB          PIC X.                                   
032100                                                                          
032200 01  SLDO-CLDC-WDB6-PCB          PIC X.                                   
032300                                                                          
032400 01  SLDO-ETA-ARTC-PCB           PIC X.                                   
032500 01  SLDO-ETA-WDK7-PCB           PIC X.                                   
032600 01  SLDO-ETA-inlc-PCB           PIC X.                                   
032700 01  SLDO-ETA-LEVA-PCB           PIC X.                                   
032800 01  SLDO-ETA-WDB6-PCB           PIC X.                                   
032900 01  SLDO-ETA-WDD9-PCB           PIC X.                                   
033000                                                                          
033100 01  SLDO-XDCA-USEA-PCB          PIC X.                                   
033200 01  SLDO-XDCA-WDB6-PCB          PIC X.                                   
033300 01  SLDO-XDCA-WDK6-PCB          PIC X.                                   
033400 01  SLDO-XDCA-WDK7-PCB          PIC X.                                   
033500 01  SLDO-XDCA-WDK9-PCB          PIC X.                                   
033600 01  SLDO-XDCA-WDL6-PCB          PIC X.                                   
033700 01  SLDO-XDCA-WDQ4B-PCB         PIC X.                                   
033800 01  SLDO-XDCA-WDQ2-PCB          PIC X.                                   
033900 01  SLDO-XDCA-WDQ4-PCB          PIC X.                                   
034000 01  SLDO-XDCA-WDR6-PCB          PIC X.                                   
034100 01  SLDO-XDCA-WDB6-2-PCB        PIC X.                                   
034200 01  SLDO-XDCA-WDK6-2-PCB        PIC X.                                   
034300 01  SLDO-XDCA-WDK7-2-PCB        PIC X.                                   
034400 01  SLDO-XDCA-WDK7-3-PCB        PIC X.                                   
034500                                                                          
034600 01  ETD-WDF1-PCB               PIC X.                                    
034700 01  ETD-WDK6-PCB               PIC X.                                    
034800 01  ETD-WDK7-PCB               PIC X.                                    
034900 01  ETD-XXKJ-PCB               PIC X.                                    
035000 01  ETD-WDD7-PCB               PIC X.                                    
035100 01  ETD-BENA-PCB               PIC X.                                    
035200 01  ETD-WDB3-PCB               PIC X.                                    
035300 01  ETD-WDK9-PCB               PIC X.                                    
035400 01  ETD-WDA5-PCB               PIC X.                                    
035500 01  ETD-WDA6J-PCB              PIC X.                                    
035600 01  ETD-WDD9-PCB               PIC X.                                    
035700 01  ETD-WDL6-PCB               PIC X.                                    
035800 01  ETD-WDB6-PCB               PIC X.                                    
035900 01  ETD-WDF2-PCB               PIC X.                                    
036000 01  ETD-WDF2A-PCB              PIC X.                                    
036010 01  ETD-WDB2-PCB               PIC X.                                    
036020 01  ETD-WDB1-PCB               PIC X.                                    
036100                                                                          
036700 01  ETD-KVAN-WDB2-PCB          PIC X.                                    
036800 01  ETD-KVAN-WDC1-PCB          PIC X.                                    
036900                                                                          
037000 01  ETD-AREG-WDK6-PCB          PIC X.                                    
037100 01  ETD-AREG-WDK7-PCB          PIC X.                                    
037200                                                                          
037300 01  ETD-SPAR-WDF8-PCB          PIC X.                                    
037400 01  ETD-SPAR-WDF8A-PCB         PIC X.                                    
037500 01  ETD-SPAR-WDK6-PCB          PIC X.                                    
037600                                                                          
037700 01  ETD-RANS-XXKM-PCB          PIC X.                                    
037800 01  ETD-RANS-ARTM-PCB          PIC X.                                    
037900 01  ETD-RANS-ARTS-PCB          PIC X.                                    
038000                                                                          
038100                                                                          
038200                                                                          
038300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB   SLDO-WDF1-PCB               
038400                           SLDO-WDF2-PCB SLDO-WDF2A-PCB                   
038500                           SLDO-WDK7-PCB                                  
038600                           SLDO-WDK6-PCB SLDO-XXKJ-PCB                    
038700                           SLDO-WDD7-PCB SLDO-BENA-PCB                    
038800                           SLDO-WDB3-PCB SLDO-WDR6-PCB                    
038810                           SLDO-WDB2-PCB SLDO-WDB1-PCB                    
038900                                                                          
039300                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
039400                                                                          
039500                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
039600                                                                          
039700                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
039800                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
039900                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
040000                                                                          
040100                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
040200                           SLDO-SPAR-WDK6-PCB                             
040300                                                                          
040400                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
040500                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
040600                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
040700                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
040800                           SLDO-SDCA-WDB6-2-PCB                           
040900                           SLDO-SDCA-WDK6-2-PCB                           
041000                           SLDO-SDCA-WDK7-2-PCB                           
041100                           SLDO-SDCA-WDK7-3-PCB                           
041200                                                                          
041300                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
041400                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
041500                                                                          
041600                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
041700                           SLDO-RANS-ARTS-PCB                             
041800                                                                          
041900                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
042000                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
042100                                                                          
042200                           SLDO-CLDC-WDB6-PCB                             
042300                                                                          
042400                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
042500                           SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB           
042600                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
042700                                                                          
042800                           SLDO-XDCA-USEA-PCB                             
042900                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
043000                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
043100                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
043200                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
043300                           SLDO-XDCA-WDR6-PCB                             
043400                           SLDO-XDCA-WDB6-2-PCB                           
043500                           SLDO-XDCA-WDK6-2-PCB                           
043600                           SLDO-XDCA-WDK7-2-PCB                           
043700                           SLDO-XDCA-WDK7-3-PCB                           
043800                                                                          
043900                           ETD-WDF1-PCB ETD-WDK6-PCB                      
044000                                                                          
044100                           ETD-WDK7-PCB ETD-XXKJ-PCB                      
044200                           ETD-WDD7-PCB ETD-BENA-PCB                      
044300                           ETD-WDB3-PCB ETD-WDK9-PCB ETD-WDA5-PCB         
044400                           ETD-WDA6J-PCB ETD-WDD9-PCB                     
044500                           ETD-WDL6-PCB ETD-WDB6-PCB                      
044600                           ETD-WDF2-PCB ETD-WDF2A-PCB                     
044610                           ETD-WDB2-PCB ETD-WDB1-PCB                      
044700                                                                          
045100                           ETD-KVAN-WDB2-PCB ETD-KVAN-WDC1-PCB            
045200                                                                          
045300                           ETD-AREG-WDK6-PCB ETD-AREG-WDK7-PCB            
045400                                                                          
045500                           ETD-SPAR-WDF8-PCB ETD-SPAR-WDF8A-PCB           
045600                           ETD-SPAR-WDK6-PCB                              
045700                                                                          
045800                           ETD-RANS-XXKM-PCB ETD-RANS-ARTM-PCB            
045900                           ETD-RANS-ARTS-PCB.                             
046000                                                                          
046100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB   SLDO-WDF1-PCB               
046200                           SLDO-WDF2-PCB SLDO-WDF2A-PCB                   
046300                           SLDO-WDK7-PCB                                  
046400                           SLDO-WDK6-PCB SLDO-XXKJ-PCB                    
046500                           SLDO-WDD7-PCB SLDO-BENA-PCB                    
046600                           SLDO-WDB3-PCB SLDO-WDR6-PCB                    
046610                           SLDO-WDB2-PCB SLDO-WDB1-PCB                    
046700                                                                          
047100                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
047200                                                                          
047300                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
047400                                                                          
047500                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
047600                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
047700                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
047800                                                                          
047900                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
048000                           SLDO-SPAR-WDK6-PCB                             
048100                                                                          
048200                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
048300                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
048400                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
048500                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
048600                           SLDO-SDCA-WDB6-2-PCB                           
048700                           SLDO-SDCA-WDK6-2-PCB                           
048800                           SLDO-SDCA-WDK7-2-PCB                           
048900                           SLDO-SDCA-WDK7-3-PCB                           
049000                                                                          
049100                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
049200                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
049300                                                                          
049400                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
049500                           SLDO-RANS-ARTS-PCB                             
049600                                                                          
049700                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
049800                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
049900                                                                          
050000                           SLDO-CLDC-WDB6-PCB                             
050100                                                                          
050200                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
050300                           SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB           
050400                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
050500                                                                          
050600                           SLDO-XDCA-USEA-PCB                             
050700                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
050800                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
050900                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
051000                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
051100                           SLDO-XDCA-WDR6-PCB                             
051200                           SLDO-XDCA-WDB6-2-PCB                           
051300                           SLDO-XDCA-WDK6-2-PCB                           
051400                           SLDO-XDCA-WDK7-2-PCB                           
051500                           SLDO-XDCA-WDK7-3-PCB                           
051600                                                                          
051700                           ETD-WDF1-PCB ETD-WDK6-PCB                      
051800                           ETD-WDK7-PCB ETD-XXKJ-PCB                      
051900                           ETD-WDD7-PCB ETD-BENA-PCB                      
052000                           ETD-WDB3-PCB ETD-WDK9-PCB ETD-WDA5-PCB         
052100                           ETD-WDA6J-PCB ETD-WDD9-PCB                     
052200                           ETD-WDL6-PCB ETD-WDB6-PCB                      
052300                           ETD-WDF2-PCB ETD-WDF2A-PCB                     
052310                           ETD-WDB2-PCB ETD-WDB1-PCB                      
052400                                                                          
052800                           ETD-KVAN-WDB2-PCB ETD-KVAN-WDC1-PCB            
052900                                                                          
053000                           ETD-AREG-WDK6-PCB ETD-AREG-WDK7-PCB            
053100                                                                          
053200                           ETD-SPAR-WDF8-PCB ETD-SPAR-WDF8A-PCB           
053300                           ETD-SPAR-WDK6-PCB                              
053400                                                                          
053500                           ETD-RANS-XXKM-PCB ETD-RANS-ARTM-PCB            
053600                           ETD-RANS-ARTS-PCB.                             
053700                                                                          
053800                                                                          
053900     PERFORM IMS-GET-MSG                                                  
054000     IF SEGMENT-FINNS                                                     
054100        PERFORM A-INIT-SPARA-INPUT                                        
054200        IF NYCKLAR-OK                                                     
054300           IF MFS-IDPFK = '9'                                             
054400              PERFORM C-GET-ETD                                           
054500           ELSE                                                           
054600              PERFORM B-BEHANDLA-RAD                                      
054700           END-IF                                                         
054800        END-IF                                                            
054900        PERFORM Z-FINIT                                                   
055000                                                                          
055100*       STRING ' ! ' W-IDDC ' ! ' JA-SW ' ! ' SPACE-SW ' ! '              
055200*        DELIMITED BY SIZE INTO MOD-TEMFSINF                              
055300        MOVE    MAX-MOD-LAENGD TO MSG-KVLL                                
055400        PERFORM IMS-INSERT-MSG                                            
055500     END-IF                                                               
055600     MOVE ZERO                TO RETURN-CODE                              
055700     GOBACK                                                               
055800     .                                                                    
055900                                                                          
056000                                                                          
056100 A-INIT-SPARA-INPUT SECTION.                                              
056200     MOVE 'A-INIT' TO CURRENT-SECTION                                     
056300                                                                          
056400     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I11301                    
056500     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
056600     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
056700     MOVE MSG-IDPFK                    TO MFS-IDPFK                       
056800     MOVE LOW-VALUE                    TO MSG-AREA                        
056900                                                                          
057000     MOVE 'W9O113N1'                   TO MFS-IDMOD                       
057100     MOVE '9113'                       TO MOD-IDTRANS                     
057200                                                                          
057300     MOVE MFS-RENSA-FAELT              TO MOD-TEMFSINF                    
057400                                          MOD-TEMFSFEL                    
057500     MOVE JA                           TO NYCKLAR-SW                      
057600                                                                          
057700     MOVE +2                           TO SPRAK-IX                        
057800                                                                          
057900     PERFORM AA-KOLLA-NYCKLAR                                             
058000     .                                                                    
058100                                                                          
058200                                                                          
058300 AA-KOLLA-NYCKLAR SECTION.                                                
058400     MOVE 'AA-KOLLA-NYCKLAR' TO CURRENT-SECTION                           
058500                                                                          
058600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
058700     MOVE '001'              TO MSGI-KDCALL                               
058800     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
058900     MOVE '9113'             TO MSGI-IDTRANS                              
059000     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
059100                                                                          
059200     IF MFS-IDTRANS = '9113'                                              
059300        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                            
059400                                MOD-IDDISTR-IN                            
059500                                MOD-IDKUNDNR-IN                           
059600                                MOD-KVBEART-IN                            
059700                                MOD-KDORDKL-IN                            
059800        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
059900        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
060000        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
060100        MOVE MID-KVBEART-IN  TO MSGI-KVBEART                              
060200        MOVE MID-KDORDKL-IN  TO MSGI-KDORDKL                              
060300     ELSE                                                                 
060400                                                                          
060500        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                            
060600                                MOD-IDARTNR-UT                            
060700                                MOD-STRECK                                
060800                                MOD-REKSIFFR                              
060900                                MOD-IDDISTR-IN                            
061000                                MOD-IDDISTR-UT                            
061100                                MOD-IDKUNDNR-IN                           
061200                                MOD-IDKUNDNR-UT                           
061300                                MOD-KVBEART-IN                            
061400                                MOD-KVBEART-UT                            
061500                                MOD-KDORDKL-IN                            
061600                                MOD-KDORDKL-UT                            
061700                                MOD-KVAVBART                              
061800                                MOD-TIDISPIN                              
061900                                MOD-TIklar                                
062000*        MOVE NEJ            TO NYCKLAR-SW                                
062100       IF MID-IDARTNR-IN NUMERIC                                          
062200       AND MID-IDARTNR-IN > ZERO                                          
062300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
062400         MOVE SPACE          TO MSGI-IDDISTR                              
062500                                MSGI-IDKUNDNR                             
062600                                MSGI-KVBEART                              
062700                                MSGI-KDORDKL                              
062800       END-IF                                                             
062900     END-IF                                                               
063000                                                                          
063100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063200                                                                          
063300     MOVE MSGI-IDARTNR       TO IDARTNR-WS                                
063400     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
063500     MOVE IDARTNR-WS         TO MOD-IDARTNR-UT                            
063600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
063700                                                                          
063800     MOVE MSGI-IDDISTR       TO IDDISTR-WS                                
063900     INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                       
064000     MOVE IDDISTR-WS         TO MOD-IDDISTR-UT                            
064100     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
064200                                                                          
064300     MOVE MSGI-IDKUNDNR      TO IDKUNDNR-WS                               
064400     INSPECT IDKUNDNR-WS REPLACING ALL SPACE BY ZERO                      
064500     MOVE IDKUNDNR-WS        TO MOD-IDKUNDNR-UT                           
064600     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
064700     IF IDKUNDNR-WS = ZERO                                                
064800        MOVE '     0'        TO MOD-IDKUNDNR-UT                           
064900     END-IF                                                               
065000                                                                          
065100     IF MSGI-KVBEART   NUMERIC                                            
065200        MOVE MSGI-KVBEART   TO KVBEART-WS                                 
065300        INSPECT KVBEART-WS REPLACING LEADING SPACE BY ZERO                
065400     ELSE                                                                 
065500        MOVE ZERO           TO KVBEART-WS                                 
065600     END-IF                                                               
065700     MOVE KVBEART-WS         TO MOD-KVBEART-UT                            
065800     INSPECT MOD-KVBEART-UT REPLACING LEADING ZERO BY SPACE               
065900     IF KVBEART-WS = ZERO                                                 
066000        MOVE '     0'        TO MOD-KVBEART-UT                            
066100     END-IF                                                               
066200                                                                          
066300     IF MID-KDORDKL-IN = ALL '+'                                          
066400       MOVE MSGI-KDORDKL     TO KDORDKL-WS                                
066500     ELSE                                                                 
066600       MOVE MID-KDORDKL-IN   TO KDORDKL-WS                                
066700     END-IF                                                               
066800     IF KDORDKL-WS = 'B' OR 'D'                                           
066900        CONTINUE                                                          
067000     ELSE                                                                 
067100        MOVE 'D'             TO  KDORDKL-WS                               
067200     END-IF                                                               
067300     MOVE KDORDKL-WS         TO MOD-KDORDKL-UT                            
067400                                                                          
067500     IF IDARTNR-WS  NUMERIC AND IDARTNR-WS > ZERO   AND                   
067600        IDDISTR-WS  NUMERIC AND IDDISTR-WS > ZERO   AND                   
067700        IDKUNDNR-WS NUMERIC AND                                           
067800        KVBEART-WS  NUMERIC AND KVBEART-WS > ZERO   AND                   
067900       (DAGORDER OR BULKORDER)                                            
068000                                                                          
068100        CONTINUE                                                          
068200     ELSE                                                                 
068300        MOVE NEJ             TO NYCKLAR-SW                                
068400        MOVE 'B01'           TO WS-IDMFSMED                               
068500     END-IF                                                               
068600     .                                                                    
068700                                                                          
068800                                                                          
068900 B-BEHANDLA-RAD SECTION.                                                  
069000     MOVE 'B-BEHANDLA-RAD' TO CURRENT-SECTION                             
069100                                                                          
069200     MOVE IDARTNR-WS       TO SLDO-IDARTNR-IN                             
069300     MOVE IDDISTR-WS       TO SLDO-IDDISTR-IN                             
069400     MOVE IDKUNDNR-WS      TO SLDO-IDKUNDNR-IN                            
069500     IF DAGORDER                                                          
069600        MOVE 1             TO SLDO-KDORDKL-IN                             
069700     ELSE                                                                 
069800        MOVE 4             TO SLDO-KDORDKL-IN                             
069900     END-IF                                                               
070000     MOVE KVBEART-WS       TO SLDO-KVBEART-IN                             
070100     MOVE ZERO             TO SLDO-KVAVBART                               
070200                              SLDO-TIREGDAT                               
070300                              SLDO-TIDISPIN                               
070400                              SLDO-KDORDBEK                               
070500     MOVE SPACE            TO SLDO-IDDC                                   
070600                              SLDO-IDMFSMED                               
070700                              SLDO-FLTPO1                                 
070800                                                                          
070900     CALL W911SLDO USING SLDO-W911SLDO SLDO-WDF1-PCB                      
071000                         SLDO-WDF2-PCB SLDO-WDF2A-PCB                     
071100                         SLDO-WDK7-PCB                                    
071200                         SLDO-WDK6-PCB SLDO-XXKJ-PCB                      
071300                         SLDO-WDD7-PCB SLDO-BENA-PCB                      
071400                         SLDO-WDB3-PCB SLDO-WDR6-PCB                      
071410                         SLDO-WDB2-PCB SLDO-WDB1-PCB                      
071500                                                                          
071900                         SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB            
072000                                                                          
072100                         SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB            
072200                                                                          
072300                         SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB            
072400                         SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB            
072500                         SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB            
072600                                                                          
072700                         SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB           
072800                         SLDO-SPAR-WDK6-PCB                               
072900                                                                          
073000                         SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB            
073100                         SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB            
073200                         SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB           
073300                         SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB            
073400                         SLDO-SDCA-WDB6-2-PCB                             
073500                         SLDO-SDCA-WDK6-2-PCB                             
073600                         SLDO-SDCA-WDK7-2-PCB                             
073700                         SLDO-SDCA-WDK7-3-PCB                             
073800                                                                          
073900                         SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB            
074000                         SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB            
074100                                                                          
074200                         SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB            
074300                         SLDO-RANS-ARTS-PCB                               
074400                                                                          
074500                         SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB            
074600                         SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB            
074700                                                                          
074800                         SLDO-CLDC-WDB6-PCB                               
074900                                                                          
075000                         SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB             
075100                         SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB             
075200                         SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB             
075300                                                                          
075400                         SLDO-XDCA-USEA-PCB                               
075500                         SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB            
075600                         SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB            
075700                         SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB           
075800                         SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB            
075900                         SLDO-XDCA-WDR6-PCB                               
076000                         SLDO-XDCA-WDB6-2-PCB                             
076100                         SLDO-XDCA-WDK6-2-PCB                             
076200                         SLDO-XDCA-WDK7-2-PCB                             
076300                         SLDO-XDCA-WDK7-3-PCB                             
076400                                                                          
076500     MOVE SLDO-KVAVBART TO WS-KVAVBART                                    
076600     INSPECT WS-KVAVBART-X REPLACING LEADING ZERO BY SPACE                
076700     MOVE WS-KVAVBART-X TO MOD-KVAVBART                                   
076800     IF SLDO-TIREGDAT > 0                                                 
076900        MOVE SLDO-TIREGDAT TO MOD-TIREGDAT                                
077000     END-IF                                                               
077100     IF SLDO-TIREGDAT  = -99 or zero                                      
077200        MOVE SPACE         TO MOD-TEREGDAT                                
077300     ELSE                                                                 
077400       IF MFS-KDMFSFOR = '1'                                              
077500        MOVE 'Senast uppdaterad DDGS'                                     
077600                           TO MOD-TEREGDAT                                
077700       ELSE                                                               
077800        MOVE '     Last updated DDGS'                                     
077900                           TO MOD-TEREGDAT                                
078000       END-IF                                                             
078100     END-IF                                                               
078200                                                                          
078300     IF SLDO-TIDISPIN > 0                                                 
078400        MOVE SLDO-TIDISPIN   TO WS-TIAAMMDD                               
078500        MOVE WS-TIAAMMDD-X   TO MOD-TIDISPIN                              
078600     END-IF                                                               
078700     IF SLDO-TIKLAR > 0                                                   
078800        MOVE SLDO-TIKLAR     TO WS-TIAAMMDD                               
078900        MOVE WS-TIAAMMDD-X   TO MOD-TIKLAR                                
079000     END-IF                                                               
079100                                                                          
079200     MOVE SLDO-IDMFSMED      TO WS-IDMFSMED                               
079300                                                                          
079400     MOVE SLDO-IDDC          TO MOD-IDDC                                  
079500     MOVE SLDO-KDORDBEK      TO MOD-KDORDBEK                              
079600     MOVE SLDO-TEORDBEK      TO MOD-TEORDBEK                              
079700     MOVE SLDO-BEART         TO MOD-BEART                                 
079800     MOVE SLDO-KDSORT        TO MOD-KDSORT                                
079900     MOVE SLDO-IDARTNR-TILLK TO MOD-IDARTNR-TILLK                         
080000     .                                                                    
080100                                                                          
080200 C-GET-ETD SECTION.                                                       
080300     MOVE 'C-GET-ETD     ' TO CURRENT-SECTION                             
080400                                                                          
080500     MOVE IDARTNR-WS       TO ETD-IDARTNR-IN                              
080600     MOVE IDDISTR-WS       TO ETD-IDDISTR-IN                              
080700     MOVE IDKUNDNR-WS      TO ETD-IDKUNDNR-IN                             
080800     IF DAGORDER                                                          
080900        MOVE 1             TO ETD-KDORDKL-IN                              
081000     ELSE                                                                 
081100        MOVE 4             TO ETD-KDORDKL-IN                              
081200     END-IF                                                               
081300     MOVE KVBEART-WS       TO ETD-KVBEART-IN                              
081400     MOVE ZERO             TO ETD-KVAVBART                                
081500                              ETD-TIREGDAT                                
081600                              ETD-TIDISPIN                                
081700                              ETD-KDORDBEK                                
081800     MOVE SPACE            TO ETD-IDDC                                    
081900                              ETD-IDMFSMED                                
082000                              ETD-FLTPO1                                  
082100                                                                          
082200     CALL W911ETD USING ETD-W911ETD  ETD-WDF1-PCB ETD-WDK6-PCB            
082300                        ETD-WDK7-PCB ETD-XXKJ-PCB                         
082400                        ETD-WDD7-PCB ETD-BENA-PCB                         
082500                        ETD-WDB3-PCB ETD-WDK9-PCB ETD-WDA5-PCB            
082600                        ETD-WDA6J-PCB ETD-WDD9-PCB                        
082700                        ETD-WDL6-PCB ETD-WDB6-PCB                         
082800                        ETD-WDF2-PCB ETD-WDF2A-PCB                        
082810                        ETD-WDB2-PCB ETD-WDB1-PCB                         
082900                                                                          
083300                        ETD-KVAN-WDB2-PCB ETD-KVAN-WDC1-PCB               
083400                                                                          
083500                        ETD-AREG-WDK6-PCB ETD-AREG-WDK7-PCB               
083600                                                                          
083700                        ETD-SPAR-WDF8-PCB ETD-SPAR-WDF8A-PCB              
083800                        ETD-SPAR-WDK6-PCB                                 
083900                        ETD-RANS-XXKM-PCB ETD-RANS-ARTM-PCB               
084000                        ETD-RANS-ARTS-PCB                                 
084100                                                                          
084200     MOVE ETD-KVAVBART TO WS-KVAVBART                                     
084300     INSPECT WS-KVAVBART-X REPLACING LEADING ZERO BY SPACE                
084400     MOVE WS-KVAVBART-X TO MOD-KVAVBART-NEW                               
084500     IF ETD-KVAVBART < 0                                                  
084600     MOVE 'NEGATIVE QUANTITY' TO MOD-TEMFSINF                             
084700     END-IF                                                               
084800*REMOVE THE ABOVE IF AT THE END *****RC*                                  
084900*IF THE QTY < 0 ,"NEGATIVE" IS DISPLAYED ON LEFT BOTTOM CORNER            
085000                                                                          
085100     IF ETD-TIDISPIN > 0                                                  
085200        MOVE ETD-TIDISPIN   TO WS-TIAAMMDD                                
085300        MOVE WS-TIAAMMDD-X   TO MOD-TIDISPIN-NEW                          
085400     END-IF                                                               
085500     IF ETD-TIKLAR > 0                                                    
085600        MOVE ETD-TIKLAR     TO WS-TIAAMMDD                                
085700        MOVE WS-TIAAMMDD-X   TO MOD-TIKLAR-NEW                            
085800     END-IF                                                               
085900                                                                          
086000     MOVE ETD-IDMFSMED      TO WS-IDMFSMED                                
086100                                                                          
086200     MOVE ETD-IDDC          TO MOD-IDDC-NEW                               
086300     MOVE ETD-KDORDBEK      TO MOD-KDORDBEK-NEW                           
086400     MOVE ETD-TEORDBEK      TO MOD-TEORDBEK                               
086500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVAVBART                          
086600                                    MOD-TIDISPIN                          
086700                                    MOD-TIKLAR                            
086800                                    MOD-IDDC                              
086900                                    MOD-KDORDBEK                          
087000                                    MOD-BEART                             
087100                                    MOD-KDSORT                            
087200*                                   MOD-TEORDBEK                          
087300                                    MOD-IDARTNR-TILLK                     
087400                                    MOD-TEREGDAT                          
087500                                    MOD-TIREGDAT                          
087600     .                                                                    
087700                                                                          
087800                                                                          
087900 Z-FINIT SECTION.                                                         
088000     MOVE 'Z-FINIT ' TO CURRENT-SECTION                                   
088100                                                                          
088200     MOVE +1 TO IX                                                        
088300     PERFORM UNTIL IX > +19                                               
088400       IF WS-IDMFSMED = KOD (IX)                                          
088500         MOVE MEDDELANDE (IX, SPRAK-IX) TO MOD-TEMFSFEL                   
088600         ADD  +19 TO IX                                                   
088700       END-IF                                                             
088800       ADD +1 TO IX                                                       
088900     END-PERFORM                                                          
089000     .                                                                    
089100                                                                          
089200                                                                          
089300* IMS SEKTIONER                                                           
089400                                                                          
089500 IMS-GET-MSG SECTION.                                                     
089600     MOVE '  QC' TO GODK-STATUSKODER                                      
089700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
089800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089900     PERFORM IMS-STATUSKONTROLL                                           
090000     .                                                                    
090100                                                                          
090200 IMS-INSERT-MSG SECTION.                                                  
090300     MOVE 'N' TO MFS-KDHUVOMR                                             
090400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
090500     MOVE SPACE TO GODK-STATUSKODER                                       
090600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
090700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090800     PERFORM IMS-STATUSKONTROLL                                           
090900     .                                                                    
091000                                                                          
091100 IMS-STATUSKONTROLL SECTION.                                              
091200                                                                          
091300     SET STATUS-IX TO 1                                                   
091400     SEARCH GODK-STATUS                                                   
091500       AT END                                                             
091600         CALL FELLOG                                                      
091700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
091800       CONTINUE                                                           
091900     END-SEARCH                                                           
092000     .                                                                    
