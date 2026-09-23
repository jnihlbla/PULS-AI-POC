000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010300.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/11/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:CARPARTS.LDC.WL0103                                             
000800*    WEB-LDC: WL010300 PROGRAM IS A REPLICA OF W6030300 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                     
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        INLÄGGNING LDC                                                   
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: WL0103U                                             
001600*                     WL0103X                                             
001700*        REQUEST      WZ01REQ2                                            
001800*                     WL0103I1                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        RESPONSE:    WZ01RES2                                            
002200*                     WL0103O1                                            
002300*                                                                         
002400*                                                                         
002500*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
002600*                              WLINLD (WDL6)                              
002700*                              WLARTC (WDK6)                              
002800*                              WDK7                                       
002900*                              WLBENA (WDD3)                              
003000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
003100*                              WDK7                                       
003200*                              WLINLC (WDL6)                              
003300*                              WLINLE (WDL2)                              
003400*                              WLFILC (WDR3)                              
003500*                              WLKOMA (WDP8)                              
003600*                              W6LOPA (W6G1)                              
003700*                              WLLOGA (WDL9)                              
003800*                              WDL301 (WDL3)                              
003900*                              WLLOCB (WDJ9)                              
004000*                              WLSAPA (WDR9)                              
004100*                              WLFILB (WDR8)                              
004200*                              WLFILB (WDR8)                              
004300*                                                                         
004400*                                                                         
004500*  2012-07-10  E'TRACKER 8200058 MANAGEMENT SCRAPPING FOLLOW UP           
004600*                                                                         
004700*  2014-08-28  E'TRACKER 10238938 CHANGE RULES SIMILAR TO 6303            
004800*                                 WHEN SCRAP QTY IS UPDATED.DO            
004900*                                 NOT UPDATE BALANCE WITH SCRAP           
005000*                                 FOR NDC-NA.                             
005100*  2015-09-14  E'TRACKER 10263662 FIX U9999 ABEND WHEN MULTIPLE           
005200*                                 USERS TRY TO UPDATE SAME DATA.          
005300*                                                                         
005400*  2015-10-15  E'TRACKER 10266973 FIX ERROR WHEN BIN FROM WL0102.         
005500**                                                                        
005600     SKIP3                                                                
005700 ENVIRONMENT DIVISION.                                                    
005800     SKIP2                                                                
005900 INPUT-OUTPUT SECTION.                                                    
006000 FILE-CONTROL.                                                            
006100     EJECT                                                                
006200 DATA DIVISION.                                                           
006300     SKIP3                                                                
006400 FILE SECTION.                                                            
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700*    -- CHECKED BY WY2000                                                 
006800 77  WS-ADRESS                   PIC X(50)                                
006900       VALUE 'CARPARTS.LDC.DCBINNING'.                                    
007000 77  WS-ADRESS-WL0102            PIC X(50)                                
007100       VALUE 'CARPARTS.LDC.DCUNLOADING'.                                  
007200 77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
007300       VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
007400 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
007500       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
007600 77  IDPGM                       PIC X(08)   VALUE 'WL010300'.            
007700 77  WS-RESP-AREA                PIC S9(5)   VALUE ZERO COMP-3.           
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000 77  NOO                         PIC X       VALUE 'N'.                   
008100 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
008200 77  INDX-DISPLAY                PIC 999     VALUE ZERO.                  
008300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
008400 77  KDRC-DISPLAY                PIC Z(5).                                
008500 77  WS-KVANT                    PIC S9(5) VALUE ZERO COMP-3.             
008600 77  RAD-SPAR                    PIC 9(5)  VALUE ZERO.                    
008700 77  INDX                        PIC S9(4) VALUE +0    COMP SYNC.         
008800 77  INDX-1                      PIC S9(4) VALUE +0    COMP SYNC.         
008900 77  INDX-2                      PIC S9(4) VALUE +0    COMP SYNC.         
009000 77  INDX-3                      PIC S9(4) VALUE +0    COMP SYNC.         
009100 77  MAX-INDX                    PIC S9(4) VALUE +1100 COMP SYNC.         
009200 77  MAX-INDX-2                  PIC S9(4) VALUE +1100 COMP SYNC.         
009300 77  ORAD-IX                     PIC S9(9) VALUE +0    COMP SYNC.         
009400 77  ORAD-IX-MAX                 PIC S9(9) VALUE +5    COMP SYNC.         
009500 77  TAB-IX                      PIC S9(4) VALUE +0    COMP-3.            
009600 77  TAB-IX-MAX                  PIC S9(4) VALUE +1100 COMP-3.            
009700 77  IX                          PIC S9(4) VALUE +0    COMP-3.            
009800 77  IX2                         PIC S9(4) VALUE +0    COMP-3.            
009900 77  LNG-P-TO-P-PREFIX           PIC S9(4) VALUE +17   COMP SYNC.         
010000 77  W-IDARTNR-EDIT-X            PIC Z(9).                                
010100 77  WS-KVANTMOT                 PIC X(6)  VALUE SPACE.                   
010200 77  WS-KVSKROT                  PIC X(7)  VALUE SPACE.                   
010300 77  WS-ADLAGOMR                 PIC X(2)  VALUE SPACE.                   
010400 77  WS-ADGANG                   PIC X(2)  VALUE SPACE.                   
010500 77  WS-ADPLATS                  PIC X(5)  VALUE SPACE.                   
010600 77  WS-IDARTNR                  PIC X(9)  VALUE SPACE.                   
010700 77  WS-KDSORT                   PIC X(2)  VALUE SPACE.                   
010800 77  WS-DIFF-KVANT               PIC 9(7)  VALUE ZERO.                    
010900 77  W-IDORDER                   PIC 9(7)  VALUE ZERO.                    
011000 77  W-IDKOLLI                   PIC 9(5)  VALUE ZERO.                    
011100 77  WS-RED-ADLAGOMR             PIC 9(2)  VALUE ZERO.                    
011200 77  WS-RED-ADGANG               PIC 9(2)  VALUE ZERO.                    
011300 77  WS-RED-ADPLATS              PIC 9(5)  VALUE ZERO.                    
011400 77  WS-KOLL-ADLAGOMR            PIC 9(2)  VALUE ZERO.                    
011500 77  WS-KOLL-ADGANG              PIC 9(2)  VALUE ZERO.                    
011600 77  WS-KOLL-ADPLATS             PIC 9(5)  VALUE ZERO.                    
011700 77  WS-BAATORDER                PIC X     VALUE SPACE.                   
011800 77  WS-FLYGORDER                PIC X     VALUE SPACE.                   
011900 77  X-SW                        PIC X     VALUE SPACE.                   
012000 77  SEND-WS-IDDC                PIC X(2)  VALUE SPACE.                   
012100 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
012200 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
012300 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
012400 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
012500 77  WS-FAKTURA-DATUM            PIC X(16) VALUE SPACE.                   
012600 77  WS-FAKTURA-DATUM2           PIC S9(16) COMP-3 VALUE ZERO.            
012700 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
012800 77  WS-IDTRACK-OK               PIC X(1)    VALUE 'N'.                   
012900 77  WS-TRCK-KVANTMOT            PIC S9(7)  COMP-3.                       
013000 77  WS-TRCK-KVAVIS              PIC S9(7)  COMP-3.                       
013100 77  WS-TRCK-KVTRACK-KVAR        PIC S9(7)  COMP-3.                       
013200 77  WS-WDL3-TRCK-KVTRACK-KVAR   PIC S9(7)  COMP-3.                       
013300 77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
013400     88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
013500     88  FIRST-REC-TRANS                     VALUE 'J'.                   
013600 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
013700                                                                          
013800 01  -COPY WWDCKONS                                                       
013900                                                                          
014000 01  -COPY WWDC99                                                         
014100                                                                          
014200 01  -COPY WWPRODSL                                                       
014300                                                                          
014400 77  INDATA-SW                   PIC X     VALUE 'J'.                     
014500     88  INDATA-OK                         VALUE 'J'.                     
014600     88  INDATA-FEL                        VALUE 'N'.                     
014700                                                                          
014800 77  IDTRACK-SW                  PIC X     VALUE 'J'.                     
014900     88  IDTRACK-OK                        VALUE 'J'.                     
015000     88  IDTRACK-FEL                       VALUE 'N'.                     
015100                                                                          
015200 77  KEYS-SW                   PIC X      VALUE 'J'.                      
015300     88  KEYS-OK                          VALUE 'J'.                      
015400     88  KEYS-WRONG                       VALUE 'N'.                      
015500                                                                          
015600 77  IDUSER-SW                   PIC X     VALUE 'J'.                     
015700     88  IDUSER-OK                         VALUE 'J'.                     
015800     88  IDUSER-FEL                        VALUE 'N'.                     
015900                                                                          
016000 77  INDATA-FINNS-SW             PIC X     VALUE 'N'.                     
016100     88  INDATA-FINNS                      VALUE 'J'.                     
016200     88  INDATA-SAKNAS                     VALUE 'N'.                     
016300                                                                          
016400 77  FAKTURA-FINNS-SW            PIC X     VALUE 'N'.                     
016500     88  FAKTURA-FINNS                     VALUE 'J'.                     
016600     88  FAKTURA-SAKNAS                    VALUE 'N'.                     
016700                                                                          
016800 77  NYUPPLAEGG-SW               PIC X     VALUE 'N'.                     
016900     88  NYUPPLAEGG                        VALUE 'J'.                     
017000                                                                          
017100 77  NY-BEFINTLIG-ART-SW         PIC X     VALUE 'N'.                     
017200     88  NY-BEFINTLIG-ART                  VALUE 'J'.                     
017300                                                                          
017400 77  NY-NYUPPLAEGG-ART-SW        PIC X     VALUE 'N'.                     
017500     88  NY-NYUPPLAEGG-ART                 VALUE 'J'.                     
017600                                                                          
017700 77  NY-SKROTNING-SW             PIC X     VALUE 'N'.                     
017800     88  NY-SKROTNING                      VALUE 'J'.                     
017900                                                                          
018000 77  NY-SKROT-SW                 PIC X     VALUE 'N'.                     
018100     88  NY-SKROT                          VALUE 'J'.                     
018200                                                                          
018300 77  NY-KVANTMOT-SW              PIC X     VALUE 'N'.                     
018400     88  NY-KVANTMOT                       VALUE 'J'.                     
018500                                                                          
018600 77  NYCKLAR-SW                  PIC X     VALUE 'J'.                     
018700     88  NYCKLAR-OK                        VALUE 'J'.                     
018800     88  NYCKLAR-FEL                       VALUE 'N'.                     
018900                                                                          
019000 77  TRANS-OHUVUD-DAM-SKAPAD-SW  PIC X     VALUE 'N'.                     
019100     88  TRANS-OHUVUD-DAM-SKAPAD           VALUE 'J'.                     
019200 77  DATE-INTERVAL-SW            PIC X       VALUE 'J'.                   
019300     88  W-DATE-INTERVAL-OK                  VALUE 'J'.                   
019400     88  W-DATE-INTERVAL-EJ                  VALUE 'N'.                   
019500                                                                          
019600 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
019700 01  FILLER REDEFINES WS-DAREGDAT.                                        
019800     03  WS-SEKEL-D              PIC 9(2).                                
019900     03  WS-AAMMDD               PIC 9(6).                                
020000                                                                          
020100 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
020200 01  FILLER REDEFINES DAGENS-DATUM.                                       
020300     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
020400     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
020500                                                                          
020600 01  W-DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                
020700 01  FILLER REDEFINES W-DAGENS-DATUM.                                     
020800     03  DAGENS-AA               PIC 9(2).                                
020900     03  DAGENS-MM               PIC 9(2).                                
021000     03  DAGENS-DD               PIC 9(2).                                
021100                                                                          
021200 01  WS-IDDC-LOCAL.                                                       
021300     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
021400     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
021500     03  FILLER                  PIC X(1)   VALUE SPACE.                  
021600                                                                          
021700 77  W-DAGENS-TID                  PIC 9(8)    VALUE ZERO.                
021800                                                                          
021900 01  WS-IDKUNDRF-GRP.                                                     
022000     03  WS-IDKUNDRF             PIC X(10).                               
022100     03  FILLER REDEFINES WS-IDKUNDRF.                                    
022200         05  WS-IDORDNR5         PIC 9(5).                                
022300         05  WS-IDORDNR5-FILLER  PIC X(5).                                
022400     03  FILLER REDEFINES WS-IDKUNDRF.                                    
022500         05  WS-IDORDNR7         PIC 9(7).                                
022600         05  FILLER              PIC X(3).                                
022700                                                                          
022800 01  WS-IDKUNDRF-IDORDNR5        PIC 9(5).                                
022900                                                                          
023000 01  STEXT     PIC X(3)  VALUE SPACE.                                     
023100                                                                          
023200 01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
023300*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
023400     EJECT                                                                
023500                                                                          
023600 01  W.                                                                   
023700     05  W-IDFKNGRP              PIC  9(5).                               
023800     05  W-KDPRODSL              PIC  9(2).                               
023900     05  W-KDPRODSL-LOC          PIC  9(2).                               
024000     05  W-TEMP-KVANT            PIC S9(7)         VALUE ZERO.            
024100     05  W-DIFF-KVANT            PIC S9(7)         VALUE ZERO.            
024200     05  W-KDFRAKT               PIC S9(3) COMP-3  VALUE ZERO.            
024300     05  W-FLSKAKOL              PIC X             VALUE SPACE.           
024400     05  WS-KVTILLGANG           PIC S9(7)V9(1)    VALUE ZERO.            
024500     05  WS-OLD-KVLS             PIC S9(7)         VALUE ZERO.            
024600     05  WS-OLD-KVEFRS           PIC S9(7)         VALUE ZERO.            
024700     05  WS-KVBEHOV              PIC S9(7)V9(1)    VALUE ZERO.            
024800     05  WS-DIFF                 PIC S9(7)V9(1)    VALUE ZERO.            
024900     05  WS-FIXAD-PRARTNTO       PIC S9(7)V9(2)    VALUE ZERO.            
025000     05  WS-6308-TF-PRARTNTO     PIC S9(7)V9(2)    VALUE ZERO.            
025100     05  WS-6308-TF-KDVALISO     PIC X(3)          VALUE SPACE.           
025200     05  WS-KVAR-AVISERAT        PIC S9(7)  COMP-3 VALUE ZERO.            
025300     05  WS-FAKTURA-KLAR         PIC X             VALUE SPACE.           
025400     05  WS-A03-SKAPAD           PIC X             VALUE SPACE.           
025500     05  WS-KDVALISO             PIC X(3)          VALUE SPACE.           
025600     05  W-KDVALISO              PIC X(3)          VALUE SPACE.           
025700     05  W-PRARTNTO              PIC S9(7)V9(2)    VALUE ZERO.            
025800     05  WS-VAERDE-DIFF          PIC S9(11)V9(2)   VALUE ZERO.            
025900     05  WS-KVDISP               PIC S9(7)  COMP-3 VALUE ZERO.            
026000     05  W-TID                   PIC 9(8)          VALUE ZERO.            
026100     05  DAGENS-DATUM            PIC 9(8)          VALUE ZERO.            
026200     05  WS-TID                  PIC 9(9)          VALUE ZERO.            
026300     05  WS-KVLS                 PIC S9(7)         VALUE ZERO.            
026400     05  WS-LOGG-DATUM           PIC S9(8)         VALUE ZERO.            
026500     05  WS-LOGG-TID             PIC S9(7)         VALUE ZERO.            
026600     05  WS-PRIME-LOCATION       PIC X             VALUE 'P'.             
026700     05  WS-SAP-IDDISTR          PIC 9(5)          VALUE ZERO.            
026800     05  WS-SAP-IDKUNDNR         PIC 9(7)          VALUE ZERO.            
026900     05  WS-SAP-IDFAKT           PIC 9(7)          VALUE ZERO.            
027000     05  WS-SAP-X-IDFAKT         PIC X(7)          VALUE ZERO.            
027100     05  WS-SAP-PRARTSTD         PIC S9(7)V9(2)    VALUE 0 COMP-3.        
027200     05  NOLL-RAKNARE            PIC S9(5)         VALUE 0 COMP-3.        
027300     05  WS-IDUSER-003           PIC X(5)          VALUE SPACE.           
027400     05  W-SPAR-REQU-IDFAKT-2    OCCURS 1100                              
027500                                 PIC 9(7)          VALUE ZERO.            
027600     05  W-SPAR-REQU-AREA        OCCURS 1100.                             
027700         10  W-SPAR-REQU-IDDISTR   PIC 9(5)        VALUE ZERO.            
027800         10  W-SPAR-REQU-IDKUNDRF  PIC X(10)       VALUE ZERO.            
027900         10  W-SPAR-REQU-IDKUNDNR  PIC 9(7)        VALUE ZERO.            
028000         10  W-SPAR-REQU-IDKOLLI   PIC 9(5)        VALUE ZERO.            
028100         10  W-SPAR-REQU-IDFAKT    PIC 9(7)        VALUE ZERO.            
028200     05  W-SPAR-FAELT.                                                    
028300         10  W-SPAR-IDKUNDRF     PIC X(10).                               
028400         10  W-SPAR-IDKUNDNR     PIC 9(7).                                
028500         10  W-SPAR-IDKOLLI      PIC 9(5).                                
028600         10  W-SPAR-IDFAKT       PIC 9(7).                                
028700         10  W-SPAR-IDSPRAAK     PIC X(3).                                
028800     05  W-NEXT-FAELT.                                                    
028900         10  W-NEXT-IDARTNR      PIC 9(9).                                
029000         10  W-NEXT-DAINLEV      PIC 9(16).                               
029100     05  W-IDORDNR-X.                                                     
029200         10  FILLER              PIC 9(2)  VALUE ZERO.                    
029300         10  W-IDORDNR-VV        PIC 9(2).                                
029400         10  W-IDORDNR-D         PIC 9(1).                                
029500         10  W-IDORDNR-TT        PIC 9(2).                                
029600                                                                          
029700     05  W-IDKUNDNR-RETUR        PIC 9(6).                                
029800     05  W-IDDISTR-RETUR         PIC 9(4).                                
029900     05  W-IDKUNDNR-REFILL       PIC 9(6).                                
030000     05  W-IDDISTR-REFILL        PIC 9(4).                                
030100     05  W-IDSEKVNR              PIC S9(3)  VALUE 0   COMP-3.             
030200     05  W-IDSEKVNR-A03          PIC S9(3)  VALUE 0   COMP-3.             
030300     05  W-IDSEKVNR-SAP          PIC S9(3)  VALUE 0   COMP-3.             
030400     05  WS-SAP-AAAAMMDD         PIC 9(8)   VALUE ZERO.                   
030500     05  WS-SAP-TTMMSSTH         PIC 9(8)   VALUE ZERO.                   
030600     05  W-TIKLOCK               PIC S9(9)  VALUE 0   COMP-3.             
030700     05  W-KVAVIS                PIC S9(7)            COMP-3.             
030800     05  W-KVKOLLI-MOT           PIC S9(5)            COMP-3.             
030900     05  W-KVRADER               PIC S9(7)            COMP-3.             
031000     05  W-KVSKROT-6-X.                                                   
031100         10  W-KVSKROT-6         PIC 9(6).                                
031200     05  W-CMD                   PIC X(3).                                
031300     05  W-TEMFSINF              PIC X(40)  VALUE SPACE.                  
031400     05  W-IDARTNR-INM           PIC S9(9)            COMP-3.             
031500     05  W-KVANTMOT-INM          PIC S9(7)            COMP-3.             
031600     05  W-KVANTMOT              OCCURS 1100                              
031700                                 PIC S9(7)            COMP-3.             
031800     05  W-KVSKROT               OCCURS 1100                              
031900                                 PIC S9(7)            COMP-3.             
032000     05  WS-KVSKROT-INM          PIC X(7)   VALUE SPACE.                  
032100     05  WS-KVANTMOT-INM         PIC X(6)   VALUE SPACE.                  
032200     05  WS-SUMMA-KVANT          PIC S9(7)  VALUE ZERO COMP-3.            
032210     05  WS-NOTF-KVANT           PIC S9(7)  VALUE ZERO COMP-3.            
032300     05  WS-KVSKROT-INM-NUM      PIC 9(7)   VALUE ZERO.                   
032400     05  WS-KVANTMOT-INM-NUM     PIC 9(6)   VALUE ZERO.                   
032500     05  WS-RO-KVANTMOT          PIC 9(7)   VALUE ZERO.                   
032600                                                                          
032700     05  W-LAGERPLATS-SPAR.                                               
032800         10  W-ADLAGOMR-SPAR     PIC S9(3)            COMP-3.             
032900         10  W-ADGANG-SPAR       PIC S9(3)            COMP-3.             
033000         10  W-ADPLATS-SPAR      PIC S9(5)            COMP-3.             
033100                                                                          
033200     05  W-LAGERPLATS            OCCURS 1100.                             
033300         10  W-ADLAGOMR          PIC S9(3)            COMP-3.             
033400         10  W-ADGANG            PIC S9(3)            COMP-3.             
033500         10  W-ADPLATS           PIC S9(5)            COMP-3.             
033600                                                                          
033700     05  W-LAGERPLATS-INM.                                                
033800         10  W-ADLAGOMR-INM      PIC S9(3)            COMP-3.             
033900         10  W-ADGANG-INM        PIC S9(3)            COMP-3.             
034000         10  W-ADPLATS-INM       PIC S9(5)            COMP-3.             
034100                                                                          
034200     05  W-LAGERPLATS-LOCB.                                               
034300         10  W-ADLAGOMR-LOCB     PIC S9(3)            COMP-3.             
034400         10  W-ADGANG-LOCB       PIC S9(3)            COMP-3.             
034500         10  W-ADPLATS-LOCB      PIC S9(5)            COMP-3.             
034600                                                                          
034700     03  AKTUELL-TID.                                                     
034800         05  AKTUELL-TTMM        PIC 9(4).                                
034900         05  FILLER              PIC 9(4).                                
035000                                                                          
035100     03  WS-SEKEL-KOLL           PIC 9(6).                                
035200     03  FILLER REDEFINES WS-SEKEL-KOLL.                                  
035300         05  WS-SEKEL            PIC 9(1).                                
035400         05  FILLER              PIC 9(5).                                
035500                                                                          
035600     03  WS-SEKEL-EKOA03.                                                 
035700         05  WS-EKOA03-SS            PIC 9(2).                            
035800         05  WS-EKOA03-AAMMDD        PIC 9(6).                            
035900     03  WS-AAAAMMDD REDEFINES WS-SEKEL-EKOA03 PIC 9(8).                  
036000                                                                          
036100     03  WS-SEKEL-TEST           PIC 9(6).                                
036200     03  FILLER REDEFINES WS-SEKEL-TEST.                                  
036300         05  WS-SEK              PIC 9(1).                                
036400         05  FILLER              PIC 9(5).                                
036500                                                                          
036600     03  WS-SEKEL-DIFF.                                                   
036700         05  WS-DIFF-SS          PIC 9(2).                                
036800         05  WS-DIFF-AAMMDD      PIC 9(6).                                
036900     03  WS-DIFF-AAAAMMDD REDEFINES WS-SEKEL-DIFF PIC 9(8).               
037000                                                                          
037100     03  WS-BILLIT-KOLL              PIC 9(6).                            
037200     03  FILLER REDEFINES WS-BILLIT-KOLL.                                 
037300         05  WS-BILLIT-SEKEL         PIC 9(1).                            
037400         05  FILLER                  PIC 9(5).                            
037500                                                                          
037600     03  WS-SEKEL-BILLIT.                                                 
037700         05  WS-BILLIT-SS            PIC 9(2).                            
037800         05  WS-BILLIT-AAMMDD        PIC 9(6).                            
037900     03  WS-BILLIT-AAAAMMDD REDEFINES WS-SEKEL-BILLIT PIC 9(8).           
038000                                                                          
038100     03  WS-IDDC-KOLL.                                                    
038200         05  FILLER              PIC X(5) VALUE 'WIDDC'.                  
038300         05  WS-IDDC-TID         PIC X(2) VALUE SPACE.                    
038400         05  FILLER              PIC X    VALUE SPACE.                    
038500                                                                          
038600     05  W-TIME-X.                                                        
038700         10  W-TIME-TT           PIC 9(2).                                
038800         10  FILLER              PIC 9(6).                                
038900     05  W-TIME-N                REDEFINES W-TIME-X                       
039000                                 PIC 9(8).                                
039100                                                                          
039200     05  W-TIAAAAMMDDTTMMSSTH    PIC 9(16)   VALUE ZERO.                  
039300     05  FILLER                  REDEFINES W-TIAAAAMMDDTTMMSSTH.          
039400         10  W-TIAAAAMMDDTTMMSSTH-DATE                                    
039500                                 PIC 9(8).                                
039600         10  W-TIAAAAMMDDTTMMSSTH-TIME                                    
039700                                 PIC 9(8).                                
039800     05  W-IDLOPNRM              PIC 9(9)    VALUE ZERO.                  
039900     05  W-0VVDLLLLK             REDEFINES W-IDLOPNRM.                    
040000         10 FILLER               PIC 9(1).                                
040100         10 W-VVD                PIC 9(3).                                
040200         10 W-LLLL               PIC 9(4).                                
040300         10 W-K                  PIC 9(1).                                
040400     05  W-IDKONTO.                                                       
040500         10 FILLER               PIC X(4) VALUE '5022'.                   
040600         10 W-IDKONTO-IDDC       PIC X(2).                                
040700         10 W-IDKONTO-KDPRODSL   PIC X(2).                                
040800         10 FILLER               PIC X(2) VALUE '32'.                     
040900                                                                          
041000 01  KONTROLL-SIFFRA.                                                     
041100     03  REK-IDARTNR             PIC 9(9) VALUE 0.                        
041200     03  REK-LNGD                PIC 9(1) VALUE 9.                        
041300     03  REK-REKSIFFR            PIC 9(1) VALUE 0.                        
041400 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
041500     03 FLT-LGD                  PIC S9(1) COMP SYNC VALUE +7.            
041600     03 VAEGNINGSTAL             PIC 9(7) VALUE 2121212.                  
041700     03 VAEGNTAL-LGD             PIC S9 COMP SYNC VALUE +7.               
041800     03 MODUL-10-11              PIC 9(2) VALUE 10.                       
041900     03 ALT-A-B                  PIC X(1) VALUE 'B'.                      
042000*                                                                         
042100 01  W-IDARTNR-NYCKEL-SPAR       PIC S9(9) VALUE ZERO COMP-3.             
042200 01  W-IDDC-NYCKEL-SPAR          PIC X(2)  VALUE SPACE.                   
042300 01  W-DAINLEV-NYCKEL-SPAR       PIC 9(16) VALUE ZERO.                    
042400 01  WS-SPARAT-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
042500 01  WS-SPARAT-IDDC              PIC X(2)  VALUE SPACE.                   
042600******                                                                    
042700 01  RAD-TABELL.                                                          
042800     03 RAD-RAD OCCURS 1100.                                              
042900        05  RAD-SPAR-SPAR        PIC 9(3).                                
043000******                                                                    
043100                                                                          
043200 01  BIN-TABELL.                                                          
043300     03 BIN-TAB-RAD OCCURS 1100.                                          
043400        05  BIN-TAB-IDFAKT       PIC 9(7).                                
043500        05  BIN-TAB-IDKUNDNR     PIC 9(7).                                
043600        05  BIN-TAB-IDKUNDRF     PIC X(10).                               
043700        05  BIN-TAB-IDKOLLI      PIC 9(5).                                
043800        05  BIN-TAB-CMD-IN       PIC X(3).                                
043900                                                                          
044000*    --- DISTRIKT                                                         
044100*01 -COPY WWDIST35                                                        
044200     EJECT                                                                
044300*    --- EKONOMITRANS                                                     
044400*01 -COPY W510A03               -PRE EKOTRA03-                            
044500     EJECT                                                                
044600*01  -COPY WDATAREA                                                       
044700     EJECT                                                                
044800*01 -COPY W335PRIS                                                        
044900     EJECT                                                                
045000*01 -COPY W61236                -PRE FILC-                                
045100     EJECT                                                                
045200*01 -COPY W510AVG                                                         
045300     EJECT                                                                
045400*01 -COPY W61244                -PRE FILC2-                               
045500     EJECT                                                                
045600*01 -COPY W61247                -PRE FILC3-                               
045700     EJECT                                                                
045800*    NOTAFISCAL                                                           
045900 01  NOTF-AREA.                                                           
046000*    03  -COPY W611NOTF                                                   
046100     EJECT                                                                
046200 01  GENERELLA-SUBPROGRAM.                                                
046300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
046400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
046500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
046600     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
046700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
046800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
046900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
047000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
047100     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
047200     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
047300     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
047400     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
047500     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
047600     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
047700     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
047800     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
047900     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
048000     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
048100     EJECT                                                                
048200 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
048300*01  -COPY WZ04PROP                                                       
048400     EJECT                                                                
048500 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
048600*01  -COPY WTRAUTF8                                                       
048700     EJECT                                                                
048800*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
048900*01 -COPY W005WDK7                                                        
049000     EJECT                                                                
049100*    --- PARAMETRAR TILL SUBPROGRAM W005WDL7                              
049200*01 -COPY W005WDL7                                                        
049300     EJECT                                                                
049400*    --- PARAMETRAR TILL WDAGKONV                                         
049500*01  -COPY WDAGAREA                                                       
049600 01  MESSAGE-CODES.                                                       
049700     03  UPDATE-DONE             PIC X(3)   VALUE '001'.                  
049800     03  ERR-UNAUTHORIZED        PIC X(3)   VALUE '00A'.                  
049900     03  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
050000     03  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
050100     03  IS-INVALID              PIC X(3)   VALUE '023'.                  
050200     03  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
050300     03  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
050400     03  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
050500     03  CASE-NOT-FOUND          PIC X(3)   VALUE '287'.                  
050600     03  KOLLI-EJ-RAPPORTERAT    PIC X(3)   VALUE '105'.                  
050700     03  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
050800     03  MUST-ENTER-EMP-ID       PIC X(3)   VALUE '026'.                  
050900     03  UPD-NOT-ALLOWED         PIC X(3)   VALUE '007'.                  
051000     03  TRACKING-ID-MISSING     PIC X(3)   VALUE '422'.                  
051100     EJECT                                                                
051200*    --- PARAMETRAR TILL ABEND                                            
051300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
051400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
051500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
051600     EJECT                                                                
051700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
051800*01  -COPY WZ01SUB                                                        
051900     EJECT                                                                
052000 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
052100*01  -COPY WMSGCONV                                                       
052200     EJECT                                                                
052300 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
052400*01  -COPY WZ01AUTH                                                       
052500     EJECT                                                                
052600*01  -COPY WL01TIDZ                                                       
052700     EJECT                                                                
052800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
052900     SKIP3                                                                
053000*01  -COPY WMSGAREA                                                       
053100     EJECT                                                                
053200     EJECT                                                                
053300 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
053400 01  P-TO-P-AREA.                                                         
053500     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
053600     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
053700     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
053800     03  P-TO-P-TRANSKOD         PIC  X(7).                               
053900     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
054000     03  P-TO-P-FROM-MID         PIC  X(4).                               
054100     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
054200     03  P-TO-P-DATA             PIC  X(1000).                            
054300     EJECT                                                                
054400*    --- AREOR FÖR W006KOM SUBMODUL                                       
054500 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
054600*01  -COPY WMSGKOM                                                        
054700     EJECT                                                                
054800 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
054900 01  KOM-IO-AREA.                                                         
055000   03  KOM-AREA                     PIC X(2457) VALUE SPACE.              
055100   03  OHUV     REDEFINES KOM-AREA.                                       
055200*    05      -COPY W4I25101   -PRE OHUV-                                  
055300     EJECT                                                                
055400   03  ORAD     REDEFINES KOM-AREA.                                       
055500*    05      -COPY W4I25201   -PRE ORAD-                                  
055600     EJECT                                                                
055700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
055800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
055900     SKIP3                                                                
056000 01  NYCKLAR-TILL-DLI.                                                    
056100     03  W-IDARTNR-X.                                                     
056200         05  W-IDARTNR           PIC S9(9)              COMP-3.           
056300                                                                          
056400     03  W-IDKUNDNR-X.                                                    
056500         05  W-IDKUNDNR          PIC S9(7)              COMP-3.           
056600                                                                          
056700     03  W-DAINLEV-X.                                                     
056800         05  W-DAINLEV           PIC 9(16).                               
056900                                                                          
057000     03  W-IDTRACK-X.                                                     
057100         05  W-IDTRACK           PIC X(25).                               
057200                                                                          
057300     03  W-IDDC-X.                                                        
057400         05  W-IDDC71            PIC X(2)    VALUE SPACE.                 
057500     03  W-IDDC                  PIC X(2).                                
057600     03  W-IDDC-B6-X.                                                     
057700         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
057800     03  W-IDDC-B6-SEND-X.                                                
057900         05  W-IDDC-B6-SEND      PIC X(2)   VALUE SPACE.                  
058000     03  W-IDPTYP                PIC X(3).                                
058100     03  W-IDSKYLT               PIC X(3).                                
058200     03  W-IDKUNDRF              PIC X(10).                               
058300                                                                          
058400     03  W-WDL6A1KY-MIN.                                                  
058500         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
058600         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
058700         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
058800         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
058900         05  W-SEQA-IDARTNR-MIN   PIC S9(9)             COMP-3.           
059000         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
059100                                                                          
059200     03  W-WDL6A1KY-MAX.                                                  
059300         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
059400         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
059500         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
059600         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
059700         05  W-SEQA-IDARTNR-MAX   PIC S9(9)             COMP-3.           
059800         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
059900                                                                          
060000     03  W-WDL6A1KY-MIN2.                                                 
060100         05  W-IDFAKT-MIN         PIC S9(7) COMP-3.                       
060200         05  FILLER               PIC X(38).                              
060300                                                                          
060400     03  W-WDL6A1KY-MAX2.                                                 
060500         05  W-IDFAKT-MAX         PIC S9(7) COMP-3.                       
060600         05  FILLER               PIC X(38).                              
060700     03  W-W6D211KY-X.                                                    
060800         05  W-DAREGDAT-9KOMPL   PIC 9(8)   VALUE 79000001.               
060900         05  W-TIKLOCK-9KOMPL    PIC S9(9)  VALUE ZERO COMP-3.            
061000                                                                          
061100     03  W-IDFAKT-X.                                                      
061200         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
061300                                                                          
061400     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
061500                                                                          
061600     03  W-6017KEY-X.                                                     
061700         05  W-6017-IDHTYP      PIC X(4)     VALUE '6017'.                
061800         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
061900                                                                          
062000     03  W-6301KEY-X.                                                     
062100         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
062200         05  W-6301-IDDC        PIC X(2).                                 
062300         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
062400                                                                          
062500     03  W-6305KEY-X.                                                     
062600         05  W-6305-IDHTYP      PIC X(4)     VALUE '6305'.                
062700         05  W-6305-IDDC        PIC X(2)     VALUE LOW-VALUE.             
062800         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
062900                                                                          
063000     03  W-WDB301KY-X.                                                    
063100         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
063200         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
063300         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
063400                                                                          
063500     03  W-WDB301KY-DEF-X.                                                
063600         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
063700         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
063800         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
063900                                                                          
064000     03  W-4505-KEY-X.                                                    
064100         05  FILLER              PIC X(4)    VALUE '4505'.                
064200         05  4505-IDDC           PIC X(2)    VALUE SPACE.                 
064300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
064400                                                                          
064500**** INSERT FOR 6351 SEGMENTS!                                            
064600     03  W-WDGX6351-X.                                                    
064700         05  W-IDHTYP-6351       PIC X(4)     VALUE '6351'.               
064800         05  W-IDDC-HAC          PIC X(2)     VALUE '86'.                 
064900         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
065000                                                                          
065100     03  W-WDJ911KY-X.                                                    
065200         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
065300         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
065400         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
065500         05  W-ADLAGOMR-WDJ9     PIC 9(2)    VALUE ZERO.                  
065600         05  W-ADGANG-WDJ9       PIC 9(2)    VALUE ZERO.                  
065700         05  W-ADPLATS-WDJ9      PIC 9(5)    VALUE ZERO.                  
065800                                                                          
065900     03 W-WDQ2C1KY-X.                                                     
066000        05  W-SEQC-IDDISTR      PIC S9(5)   VALUE +0 COMP-3.              
066100        05  W-SEQC-IDKUNDNR     PIC S9(7)   VALUE +0 COMP-3.              
066200        05  W-SEQC-IDKUNDRF.                                              
066300          07  W-SEQC-IDORDNR7   PIC 9(7)    VALUE ZERO.                   
066400          07  FILLER            PIC X(3)    VALUE SPACE.                  
066500                                                                          
066600     03  W-WDL6D1KY-MIN.                                                  
066700         05  W-SEQD-IDDC-MIN     PIC X(2).                                
066800         05  W-SEQD-IDDISTR-MIN  PIC S9(5) COMP-3.                        
066900         05  W-SEQD-IDKUNDNR-MIN PIC S9(7) COMP-3.                        
067000         05  W-SEQD-IDKUNDRF-MIN PIC X(10).                               
067100         05  W-SEQD-IDKOLLI-MIN  PIC S9(5) COMP-3.                        
067200         05  W-SEQD-IDARTNR-MIN  PIC S9(9) COMP-3.                        
067300         05  W-SEQD-DAINLEV-MIN  PIC 9(16).                               
067400                                                                          
067500     03  W-WDL6D1KY-MAX.                                                  
067600         05  W-SEQD-IDDC-MAX     PIC X(2).                                
067700         05  W-SEQD-IDDISTR-MAX  PIC S9(5) COMP-3.                        
067800         05  W-SEQD-IDKUNDNR-MAX PIC S9(7) COMP-3.                        
067900         05  W-SEQD-IDKUNDRF-MAX PIC X(10).                               
068000         05  W-SEQD-IDKOLLI-MAX  PIC S9(5) COMP-3.                        
068100         05  W-SEQD-IDARTNR-MAX  PIC S9(9) COMP-3.                        
068200         05  W-SEQD-DAINLEV-MAX  PIC 9(16).                               
068300                                                                          
068400     03  W-KDKVAINF-X.                                                    
068500         05  W-KDKVAINF          PIC  X(1)   VALUE 'R'.                   
068600                                                                          
068700     EJECT                                                                
068800*    --- STATUS-KOD FRÅN IMS                                              
068900 01  STATUS-WS                   PIC XX.                                  
069000     88  SEGMENT-FINNS                       VALUE '  '.                  
069100     88  INSERT-OK                           VALUE '  '.                  
069200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
069300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
069400     SKIP2                                                                
069500 01  GODK-STATUSKODER.                                                    
069600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
069700     SKIP3                                                                
069800 01  SSA1                        PIC X(160).                              
069900 01  SSA2                        PIC X(128).                              
070000 01  SSA3                        PIC X(128).                              
070100     EJECT                                                                
070200*    --- IMS FUNKTIONSKODER                                               
070300*01  -COPY W0003                                                          
070400     EJECT                                                                
070500*    ---  DLI INPUT-OUTPUT AREOR                                          
070600 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
070700*01  WLLOGA01  -COPY WDL901                                               
070800     EJECT                                                                
070900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL301'.                      
071000 01  DLI-IO-WDL301.                                                       
071100*    03  -COPY WDL301                                                     
071200     EJECT                                                                
071300 01  FILLER                      PIC X(16)                                
071400                                    VALUE 'DLI-IO-AREA-WDL6'.             
071500     SKIP3                                                                
071600 01  DLI-IO-AREA-WDL6.                                                    
071700     03  IO-AREA-WDL6            PIC X(300)  VALUE SPACE.                 
071800     SKIP3                                                                
071900     03  WLINLC01 REDEFINES IO-AREA-WDL6.                                 
072000*        05  -COPY WDL601                                                 
072100     EJECT                                                                
072200     03  WLINLC11 REDEFINES IO-AREA-WDL6.                                 
072300*        05  -COPY WDL611                                                 
072400     EJECT                                                                
072500     03  WLINLD01 REDEFINES IO-AREA-WDL6.                                 
072600*        05  -COPY WDL6A1                                                 
072700     EJECT                                                                
072800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL623'.                      
072900 01  DLI-IO-WDL623.                                                       
073000*    03  -COPY WDL623                                                     
073100     EJECT                                                                
073200 01  FILLER                      PIC X(16)                                
073300                                     VALUE 'DLI-IO-WDK701'.               
073400 01  DLI-IO-AREA-WDK701.                                                  
073500*    03  -COPY WDK701                                                     
073600     EJECT                                                                
073700                                                                          
073800 01  FILLER                      PIC X(16)                                
073900                                     VALUE 'DLI-IO-AREA-WDK7'.            
074000 01  DLI-IO-AREA-WDK7.                                                    
074100*    03  -COPY WDK711                                                     
074200     EJECT                                                                
074300 01  FILLER                      PIC X(16)                                
074400                                     VALUE 'DLI-IO-AREA-WDL7'.            
074500 01  DLI-IO-WDL711.                                                       
074600*    03  -COPY WDL711                                                     
074700     EJECT                                                                
074800                                                                          
074900 01  DLI-IO-WDK728.                                                       
075000*    03  -COPY WDK728                                                     
075100     EJECT                                                                
075200 01  FILLER                      PIC X(16)                                
075300                                      VALUE 'DLI-IO-WDK601'.              
075400 01  DLI-IO-WDK601.                                                       
075500*    03  -COPY WDK601   -PRE K6-                                          
075600     EJECT                                                                
075700 01  FILLER                      PIC X(16)                                
075800                                      VALUE 'DLI-IO-WDK611'.              
075900 01  DLI-IO-WDK611.                                                       
076000*    03  -COPY WDK611                                                     
076100     EJECT                                                                
076200 01  FILLER                      PIC X(16)   VALUE                        
076300     'DLI-IO-AREA-WDGX'.                                                  
076400     SKIP3                                                                
076500 01  DLI-IO-AREA-WDGX.                                                    
076600     03  IO-AREA-WDGX            PIC X(300)  VALUE SPACE.                 
076700     SKIP3                                                                
076800     03  WL630101 REDEFINES IO-AREA-WDGX.                                 
076900*        05  -COPY WDGX6301                                               
077000     EJECT                                                                
077100     03  WL630111 REDEFINES IO-AREA-WDGX.                                 
077200*        05  -COPY WDGX6302                                               
077300     EJECT                                                                
077400 01  DLI-IO-AREA-WDGX2.                                                   
077500     03  IO-AREA-WDGX2           PIC X(300)  VALUE SPACE.                 
077600     SKIP3                                                                
077700     03  WL630511 REDEFINES IO-AREA-WDGX2.                                
077800*        05  -COPY WDGX6306                                               
077900     EJECT                                                                
078000     03  WL630521 REDEFINES IO-AREA-WDGX2.                                
078100*        05  -COPY WDGX6308                                               
078200     EJECT                                                                
078300 01  DLI-IO-AREA-6352.                                                    
078400     03  IO-AREA-6352            PIC X(300)  VALUE SPACE.                 
078500     SKIP3                                                                
078600     03  WDGX6352 REDEFINES IO-AREA-6352.                                 
078700*        05  -COPY WDGX6352                                               
078800     EJECT                                                                
078900 01  FILLER                      PIC X(16)   VALUE                        
079000     'DLI-IO-AREA-WDL2'.                                                  
079100     SKIP3                                                                
079200 01  DLI-IO-AREA-WDL2.                                                    
079300     03  IO-AREA-WDL2            PIC X(300)  VALUE SPACE.                 
079400     SKIP3                                                                
079500     03  WLINLE01 REDEFINES IO-AREA-WDL2.                                 
079600*        05  -COPY WDL201        -PRE INLE-                               
079700     EJECT                                                                
079800     03  WLINLE11 REDEFINES IO-AREA-WDL2.                                 
079900*        05  -COPY WDL211        -PRE INLE-                               
080000     EJECT                                                                
080100     03  WLINLE22 REDEFINES IO-AREA-WDL2.                                 
080200*        05  -COPY WDL222        -PRE INLE-                               
080300     EJECT                                                                
080400 01  DLI-IO-AREA-WDD3.                                                    
080500     03  IO-AREA-WDD3            PIC X(300)  VALUE SPACE.                 
080600     SKIP3                                                                
080700     03  WLBENA11 REDEFINES IO-AREA-WDD3.                                 
080800*        05  -COPY WDD311                                                 
080900     EJECT                                                                
081000 01  FILLER                      PIC X(16)   VALUE                        
081100     'DLI-IO-AREA-W6GX'.                                                  
081200     SKIP3                                                                
081300 01  DLI-IO-AREA-W6GX.                                                    
081400     03  IO-AREA-W6GX            PIC X(300)  VALUE SPACE.                 
081500     SKIP3                                                                
081600     03  W6LOPA11 REDEFINES IO-AREA-W6GX.                                 
081700*        05  -COPY W6GX6018                                               
081800     EJECT                                                                
081900 01  FILLER                      PIC X(16)   VALUE                        
082000     'DLI-IO-AREA-WDB3'.                                                  
082100     SKIP3                                                                
082200 01  DLI-IO-AREA-WDB3.                                                    
082300     03  IO-AREA-WDB3            PIC X(300)  VALUE SPACE.                 
082400     SKIP3                                                                
082500     03  WLGMTB01 REDEFINES IO-AREA-WDB3.                                 
082600*        05  -COPY WDB301                                                 
082700     EJECT                                                                
082800 01  DLI-IO-AREA-4505.                                                    
082900     03  IO-AREA-4505            PIC X(300)  VALUE SPACE.                 
083000     SKIP3                                                                
083100     03  WL450611 REDEFINES IO-AREA-4505.                                 
083200*        05  -COPY WDGX4506                                               
083300     EJECT                                                                
083400 01  DLI-IO-AREA-FILC.                                                    
083500     03  IO-AREA-FILC            PIC X(300)  VALUE SPACE.                 
083600     SKIP3                                                                
083700     03  WLFILC01 REDEFINES IO-AREA-FILC.                                 
083800*        05  -COPY WDR301       -PRE FILC-                                
083900 01  DLI-IO-AREA-FILC2.                                                   
084000     03  IO-AREA-FILC2           PIC X(300)  VALUE SPACE.                 
084100     SKIP3                                                                
084200     03  WLFILC01 REDEFINES IO-AREA-FILC2.                                
084300*        05  -COPY WDR301       -PRE FILC2-                               
084400     EJECT                                                                
084500 01  DLI-IO-AREA-FILC3.                                                   
084600     03  IO-AREA-FILC3           PIC X(300)  VALUE SPACE.                 
084700     SKIP3                                                                
084800     03  WLFILC01 REDEFINES IO-AREA-FILC3.                                
084900*        05  -COPY WDR301       -PRE FILC3-                               
085000     EJECT                                                                
085100 01  DLI-IO-AREA-LOCB.                                                    
085200     03  IO-AREA-LOCB            PIC X(300)  VALUE SPACE.                 
085300     SKIP3                                                                
085400     03  WLLOCB01 REDEFINES IO-AREA-LOCB.                                 
085500*        05  -COPY WDJ901       -PRE LOCB-                                
085600     EJECT                                                                
085700     03  WLLOCB11 REDEFINES IO-AREA-LOCB.                                 
085800*        05  -COPY WDJ911       -PRE LOCB-                                
085900     EJECT                                                                
086000 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
086100 01  DLI-IO-WLSAPA01.                                                     
086200*    03  WLSAPA01  -COPY WDR901                                           
086300*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
086400     EJECT                                                                
086500 01  DLI-IO-WLFILB01.                                                     
086600*    03  WLFILB01  -COPY WDR801                                           
086700*    07  -COPY W510EKHA  -PRE R8- -RED FIL-WDR801-DATA                    
086800     EJECT                                                                
086900 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDQ2C1'.                
087000 01  DLI-IO-WDQ2C1.                                                       
087100*    03  WDQ2C1 -COPY WDQ2C1                                              
087200     EJECT                                                                
087300 01  FILLER            PIC X(16)    VALUE 'WDB601 AREA  '.                
087400 01  DLI-IO-AREA-B601.                                                    
087500*    03  -COPY WDB601                                                     
087600     EJECT                                                                
087700 01  FILLER            PIC X(16)    VALUE 'WDB6 SENDAREA'.                
087800 01  DLI-IO-AREA-B601-SEND.                                               
087900*    03  -COPY WDB601  -PRE SEND-                                         
088000     EJECT                                                                
088100 01  FILLER               PIC X(16)   VALUE 'DLI-IO-L6A1'.                
088200     SKIP3                                                                
088300 01  DLI-IO-L6A1.                                                         
088400*    03  -COPY WDL6A1   -PRE WDL6A1-                                      
088500  01  DLI-IO-L6D1.                                                        
088600*    03  -COPY WDL6D1                                                     
088700 01  FILLER                      PIC X(16) VALUE 'W6D211   AREA'.         
088800 01   DLI-IO-AREA-6D21.                                                   
088900*     03  -COPY W6D211                                                    
089000                                                                          
089100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
089200 01  REQU-AREA.                                                           
089300*    03  -COPY WZ01REQ2                                                   
089400*    03  -COPY WL0103I1                                                   
089500                                                                          
089600 01  REQU-AREA-X.                                                         
089700*    03  -COPY WZ01REQ2 -PRE X-                                           
089800*    03  -COPY WL0102I1 -PRE X-                                           
089900                                                                          
090000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
090100 01  RESP-AREA.                                                           
090200*    03  -COPY WZ01RES2                                                   
090300*    03  -COPY WL0103O1                                                   
090400                                                                          
090500 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
090600*01  -COPY WZ01SEND                                                       
090700                                                                          
090800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
090900 01  SEND-AREA.                                                           
091000*    03  -COPY WZ01REQ2 -PRE SEND-                                        
091100*    03  -COPY WL0102I1 -PRE SEND-                                        
091200                                                                          
091300 LINKAGE SECTION.                                                         
091400                                                                          
091500*01  -COPY W0009  -PRE MSG-                                               
091600     EJECT                                                                
091700*01  -COPY W0009  -PRE ALT-                                               
091800     EJECT                                                                
091900*01  -COPY W0009  -PRE HOPP-                                              
092000     EJECT                                                                
092100 01  MQASYNC-PCB                 PIC X.                                   
092200     EJECT                                                                
092300 01  ATAB-PCB                    PIC X.                                   
092400     EJECT                                                                
092500*01  -COPY W0008  -PRE 6301-                                              
092600     05  FILLER                  PIC X.                                   
092700     EJECT                                                                
092800*01  -COPY W0008  -PRE INLC-                                              
092900     05  FILLER                  PIC X.                                   
093000     EJECT                                                                
093100*01  -COPY W0008  -PRE INLD-                                              
093200     05  FILLER                  PIC X.                                   
093300     EJECT                                                                
093400*01  -COPY W0008  -PRE INLE-                                              
093500     05  FILLER                  PIC X.                                   
093600     EJECT                                                                
093700*01  -COPY W0008  -PRE ARTC-                                              
093800     05  FILLER                  PIC X.                                   
093900     EJECT                                                                
094000*01  -COPY W0008  -PRE WDK7-                                              
094100     05  FILLER                  PIC X.                                   
094200     EJECT                                                                
094300*01  -COPY W0008  -PRE PRIS-WDK7-                                         
094400     05  FILLER                  PIC X.                                   
094500     EJECT                                                                
094600*01  -COPY W0008  -PRE BENA-                                              
094700     05  FILLER                  PIC X.                                   
094800     EJECT                                                                
094900*01  -COPY W0008  -PRE LOPA-                                              
095000     05  FILLER                  PIC X.                                   
095100     EJECT                                                                
095200*01  -COPY W0008  -PRE KOMA-                                              
095300     05  FILLER                  PIC X.                                   
095400     EJECT                                                                
095500*01  -COPY W0008  -PRE 6305-                                              
095600     05  FILLER                  PIC X.                                   
095700     EJECT                                                                
095800*01  -COPY W0008  -PRE 9305-                                              
095900     05  FILLER                  PIC X.                                   
096000     EJECT                                                                
096100*01  -COPY W0008  -PRE AVG-WDB6-                                          
096200     05  FILLER                  PIC X.                                   
096300     EJECT                                                                
096400*01  -COPY W0008  -PRE FILC-                                              
096500     05  FILLER                  PIC X.                                   
096600     EJECT                                                                
096700*01  -COPY W0008  -PRE WLLOGA-                                            
096800     05  FILLER                  PIC X.                                   
096900     EJECT                                                                
097000*01  -COPY W0008  -PRE WDL3-                                              
097100     05  FILLER                  PIC X.                                   
097200     EJECT                                                                
097300*01  -COPY W0008  -PRE LOCB-                                              
097400     05  FILLER                  PIC X.                                   
097500     EJECT                                                                
097600*01  -COPY W0008  -PRE SAPA-                                              
097700     05  FILLER                  PIC X.                                   
097800     EJECT                                                                
097900*01  -COPY W0008  -PRE WDQ2C-                                             
098000     05  FILLER                  PIC X.                                   
098100*01  -COPY W0008  -PRE WDB6-                                              
098200     05  FILLER                  PIC X.                                   
098300     EJECT                                                                
098400*01  -COPY W0008  -PRE FILB-                                              
098500     05  FILLER                  PIC X.                                   
098600     EJECT                                                                
098700*01  -COPY W0008  -PRE GMTA-                                              
098800     05  FILLER                  PIC X.                                   
098900     EJECT                                                                
099000*01  -COPY W0008  -PRE BETA-                                              
099100     05  FILLER                  PIC X.                                   
099200     EJECT                                                                
099300*01  -COPY W0008  -PRE GPRIA-                                             
099400     05  FILLER                  PIC X.                                   
099500     EJECT                                                                
099600*01  -COPY W0008  -PRE GPRIB-                                             
099700     05  FILLER                  PIC X.                                   
099800     EJECT                                                                
099900*01  -COPY W0008  -PRE KNDB-                                              
100000     05  FILLER                  PIC X.                                   
100100     EJECT                                                                
100200*01  -COPY W0008  -PRE 4505-                                              
100300     05  FILLER                  PIC X.                                   
100400     EJECT                                                                
100500 01  PRIS-COST-WDK6-PCB          PIC X.                                   
100600 01  PRIS-COST-WDK7-PCB          PIC X.                                   
100700 01  PRIS-COST-WDF1-PCB          PIC X.                                   
100800 01  PRIS-COST-9305-PCB          PIC X.                                   
100900 01  PRIS-COST-WDK72-PCB         PIC X.                                   
101000 01  PRIS-COST-WDB6-PCB          PIC X.                                   
101100*01  -COPY W0008  -PRE  OIGA-                                             
101200     05  FILLER                  PIC X.                                   
101300     EJECT                                                                
101400*01  -COPY W0008  -PRE WDL6A-                                             
101500     05  FILLER                  PIC X.                                   
101600     EJECT                                                                
101700*01  -COPY W0008  -PRE WDL6-                                              
101800     05  FILLER                  PIC X.                                   
101900     EJECT                                                                
102000*01  -COPY W0008  -PRE WDR5-                                              
102100     05  FILLER                  PIC X.                                   
102200     EJECT                                                                
102300*01  -COPY W0008  -PRE WDL6D-                                             
102400     05  FILLER                  PIC X.                                   
102500*01  -COPY W0008   -PRE KVAH-                                             
102600     05  FILLER                  PIC X.                                   
102700     EJECT                                                                
102800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB HOPP-PCB MQASYNC-PCB           
102900                           ATAB-PCB 6301-PCB                              
103000                           INLC-PCB INLD-PCB INLE-PCB                     
103100                           ARTC-PCB WDK7-PCB PRIS-WDK7-PCB                
103200                           BENA-PCB LOPA-PCB KOMA-PCB                     
103300                           6305-PCB 9305-PCB AVG-WDB6-PCB                 
103400                           FILC-PCB                                       
103500                           WLLOGA-PCB WDL3-PCB LOCB-PCB                   
103600                           SAPA-PCB WDQ2C-PCB WDB6-PCB                    
103700                           FILB-PCB GMTA-PCB BETA-PCB                     
103800                           GPRIA-PCB GPRIB-PCB KNDB-PCB 4505-PCB          
103900                           PRIS-COST-WDK6-PCB                             
104000                           PRIS-COST-WDK7-PCB                             
104100                           PRIS-COST-WDF1-PCB                             
104200                           PRIS-COST-9305-PCB                             
104300                           PRIS-COST-WDK72-PCB                            
104400                           PRIS-COST-WDB6-PCB                             
104500                           OIGA-PCB WDL6A-PCB WDL6-PCB                    
104600                           WDR5-PCB WDL6D-PCB KVAH-PCB.                   
104700 MAIN SECTION.                                                            
104800                                                                          
104900     PERFORM S16-HAEMTA-ANROPSDATA                                        
105000     IF SUB-KDRC = 0                                                      
105100        PERFORM A-INIT                                                    
105200        IF REQU-KDPGMACT = 'X'                                            
105300           MOVE REQU-AREA TO REQU-AREA-X                                  
105400        ELSE                                                              
105500           PERFORM B-KOLLA-NYCKLAR                                        
105600        END-IF                                                            
105700        IF NYCKLAR-OK                                                     
105800           IF REQU-KDPGMACT = 'E' OR 'X'                                  
105900              PERFORM I-KOLLA-INDATA-FINNS                                
106000              PERFORM G-KOLLA-INPUT                                       
106100              IF INDATA-OK AND IDUSER-OK                                  
106200                 PERFORM H-UPPDATERA                                      
106300              END-IF                                                      
106400           END-IF                                                         
106500           IF REQU-KDPGMACT = 'X'                                         
106600              CONTINUE                                                    
106700           ELSE                                                           
106800              PERFORM F-LAES-VISA-INFO                                    
106900           END-IF                                                         
107000                                                                          
107100           IF REQU-KDPGMACT = 'X'                                         
107200              MOVE X-REQU-WL0102I1 TO SEND-REQU-WL0102I1                  
107300              MOVE W-SPAR-IDFAKT TO SEND-REQU-IDFAKT-KEY                  
107400              INSPECT SEND-REQU-IDFAKT-KEY                                
107500                          REPLACING LEADING ZERO BY SPACE                 
107600              MOVE ZERO  TO SEND-REQU-IDRESVER                            
107700              MOVE SPACE TO SEND-REQU-IDUSER                              
107800              MOVE 'X'   TO SEND-REQU-KDPGMACT                            
107900              PERFORM S12-SEND-OPEN                                       
108000              PERFORM S13-SEND-MESSAGE                                    
108100              PERFORM S14-SEND-CLOSE                                      
108200           ELSE                                                           
108300              IF SUB-KDTRANS(1:6) = 'WLA103'                              
108400                PERFORM S11-MSG-CONV                                      
108500              END-IF                                                      
108600              PERFORM S17-RETURNERA-SVAR                                  
108700           END-IF                                                         
108800        ELSE                                                              
108900           IF SUB-KDTRANS(1:6) = 'WLA103'                                 
109000             PERFORM S11-MSG-CONV                                         
109100           END-IF                                                         
109200           PERFORM S17-RETURNERA-SVAR                                     
109300        END-IF                                                            
109400     END-IF                                                               
109500     MOVE ZERO TO RETURN-CODE                                             
109600     GOBACK                                                               
109700     .                                                                    
109800     EJECT                                                                
109900                                                                          
110000 A-INIT SECTION.                                                          
110100     MOVE ALL '+' TO RESP-AREA                                            
110200     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
110300                     RESP-IDMSG-INFO                                      
110400                     RESP-IDELMT-ERROR                                    
110500     MOVE '001'   TO RESP-IDRESVER                                        
110600     MOVE ZERO    TO RESP-KVRADER-MAX1                                    
110700     MOVE ZERO  TO W-IDLOPNRM                                             
110800     MOVE NEJ TO TRANS-OHUVUD-DAM-SKAPAD-SW                               
110900                 INDATA-FINNS-SW                                          
111000     MOVE JA TO INDATA-SW                                                 
111100     IF SUB-KDTRANS(1:6) = 'WLA103'                                       
111200       MOVE 001                  TO AUTH-KDCALL                           
111300       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
111400                                    REQU-WZ01REQ2                         
111500       IF AUTH-KDRC > 0                                                   
111600         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
111700         MOVE NOO                TO KEYS-SW                               
111800       END-IF                                                             
111900       MOVE FUNCTION UPPER-CASE (REQU-IDUSER-003) TO                      
112000                                 REQU-IDUSER-003                          
112100       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)   TO                      
112200                                 REQU-IDDC-KEY                            
112300       MOVE FUNCTION UPPER-CASE (REQU-KOLLI-KLART) TO                     
112400                                 REQU-KOLLI-KLART                         
112500       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)   TO                      
112600                                 REQU-KDPGMACT                            
112700       MOVE +1 TO INDX                                                    
112800       PERFORM UNTIL INDX > MAX-INDX                                      
112900        MOVE FUNCTION UPPER-CASE (REQU-CMD-IN(INDX)) TO                   
113000                                  REQU-CMD-IN(INDX)                       
113100        ADD +1 TO INDX                                                    
113200       END-PERFORM                                                        
113300     END-IF                                                               
113400                                                                          
113500     MOVE 'IDAG'          TO DAT-KDDATFORM                                
113600     CALL WDATKONV USING     DAT-KDDATFORM                                
113700                             DAT-I-TIDATUM                                
113800                             DAT-O-TIDATUM                                
113900                             DAT-KDSVAR                                   
114000                                                                          
114100     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
114200     ACCEPT W-DAGENS-TID  FROM TIME                                       
114300         MOVE REQU-IDDC-KEY TO W-IDDC                                     
114400******** ADAPT DATE AND TIME FOR TIMEZONES                                
114500         MOVE W-IDDC        TO W-IDDC-B6                                  
114600         PERFORM IMS-GU-WDB601                                            
114700                                                                          
114800******** MOVE DCS-FLTRACK TO RESP-FLTRACK                                 
114900         IF DCS-FLTRACK = 'J'                                             
115000            MOVE 'J'  TO WS-FLTRACK                                       
115100         ELSE                                                             
115200            MOVE 'N'  TO WS-FLTRACK                                       
115300         END-IF                                                           
115400                                                                          
115500         MOVE '011'                TO MSGI-KDCALL                         
115600         MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                       
115700         MOVE DCS-IDDC             TO MSGI-IDDC                           
115800         MOVE W-DAGENS-DATUM       TO MSGI-TILOKDAT                       
115900         MOVE W-DAGENS-TID         TO MSGI-TILOKTID                       
116000         CALL WL01TIDZ USING          MSGI-WL01TIDZ                       
116100           MOVE MSGI-TILOKDAT(1:6) TO W-DAGENS-DATUM                      
116200           MOVE MSGI-TILOKTID(1:4) TO W-DAGENS-TID(1:4)                   
116300********                                                                  
116400     MOVE W-DAGENS-TID     TO W-TIME-X                                    
116500     MOVE W-TIME-N         TO W-TIKLOCK                                   
116600                                                                          
116700     MOVE 'WL010300'       TO FILC-FIL-IDPGM                              
116800                              FILC2-FIL-IDPGM                             
116900                              FILC3-FIL-IDPGM                             
117000     MOVE W-DAGENS-DATUM   TO FILC-FIL-TIREGDAT                           
117100                              FILC2-FIL-TIREGDAT                          
117200                              FILC3-FIL-TIREGDAT                          
117300     MOVE 'W61236  '       TO FILC-FIL-IDCPYTXT                           
117400     MOVE 'W61244  '       TO FILC2-FIL-IDCPYTXT                          
117500     MOVE 'W61247  '       TO FILC3-FIL-IDCPYTXT                          
117600     MOVE ZERO             TO FILC-FIL-TIKLOCK                            
117700                              FILC2-FIL-TIKLOCK                           
117800                              FILC3-FIL-TIKLOCK                           
117900                                                                          
118000     MOVE +1 TO TAB-IX                                                    
118100     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
118200        MOVE ZERO TO  BIN-TAB-IDFAKT(TAB-IX)                              
118300                      BIN-TAB-IDKUNDNR(TAB-IX)                            
118400                      BIN-TAB-IDKOLLI(TAB-IX)                             
118500        MOVE SPACE TO BIN-TAB-IDKUNDRF(TAB-IX)                            
118600                      BIN-TAB-CMD-IN(TAB-IX)                              
118700        ADD +1 TO TAB-IX                                                  
118800     END-PERFORM                                                          
118900                                                                          
119000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
119100     .                                                                    
119200     EJECT                                                                
119300                                                                          
119400 B-KOLLA-NYCKLAR SECTION.                                                 
119500     MOVE JA                 TO NYCKLAR-SW                                
119600                                                                          
119700***  KONTROLL AV REQU-KDPGMACT                                            
119800     IF REQU-KDPGMACT = 'S' OR 'E' OR 'X'                                 
119900        CONTINUE                                                          
120000     ELSE                                                                 
120100        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
120200        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
120300        MOVE NEJ TO NYCKLAR-SW                                            
120400     END-IF                                                               
120500                                                                          
120600***  KONTROLL AV REQU-IDKUNDRF                                            
120700     IF REQU-IDKUNDRF-KEY  = ALL '+'                                      
120800        MOVE NEJ        TO NYCKLAR-SW                                     
120900        MOVE 'IDKUNDRF' TO RESP-IDELMT-ERROR                              
121000     ELSE                                                                 
121100        IF REQU-IDKUNDRF-KEY > SPACE                                      
121200           MOVE REQU-IDKUNDRF-KEY TO W-SPAR-IDKUNDRF                      
121300        ELSE                                                              
121400           MOVE NEJ        TO NYCKLAR-SW                                  
121500           MOVE 'IDKUNDRF' TO RESP-IDELMT-ERROR                           
121600        END-IF                                                            
121700     END-IF                                                               
121800                                                                          
121900***  KONTROLL AV REQU-IDKUNDNR                                            
122000     IF REQU-IDKUNDNR-KEY = ALL '+'                                       
122100        MOVE NEJ        TO NYCKLAR-SW                                     
122200        MOVE 'IDKUNDNR' TO RESP-IDELMT-ERROR                              
122300     ELSE                                                                 
122400        INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING SPACE BY ZERO         
122500        IF REQU-IDKUNDNR-KEY NUMERIC                                      
122600           MOVE REQU-IDKUNDNR-KEY TO W-SPAR-IDKUNDNR                      
122700        ELSE                                                              
122800           MOVE NEJ        TO NYCKLAR-SW                                  
122900           MOVE 'IDKUNDNR' TO RESP-IDELMT-ERROR                           
123000        END-IF                                                            
123100     END-IF                                                               
123200                                                                          
123300***  KONTROLL AV REQU-IDKOLLI                                             
123400     IF REQU-IDKOLLI-KEY = ALL '+'                                        
123500        MOVE NEJ       TO NYCKLAR-SW                                      
123600        MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                               
123700     ELSE                                                                 
123800        INSPECT REQU-IDKOLLI-KEY REPLACING LEADING SPACE BY ZERO          
123900        IF REQU-IDKOLLI-KEY NUMERIC                                       
124000        AND REQU-IDKOLLI-KEY > ZERO                                       
124100           MOVE REQU-IDKOLLI-KEY TO W-SPAR-IDKOLLI                        
124200        ELSE                                                              
124300           MOVE NEJ       TO NYCKLAR-SW                                   
124400           MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                            
124500        END-IF                                                            
124600     END-IF                                                               
124700                                                                          
124800*    IN CASE OF API TRANS (CURR USED IN HANDHELD), WE FETCH THE           
124900*    INVOICE USING DISTRICT AND OTHER FIELDS                              
125000     IF SUB-KDTRANS(1:6) = 'WLA103'                                       
125100***  KONTROLL AV REQU-IDDISTR                                             
125200       IF REQU-IDDISTR-KEY = ALL '+'                                      
125300         MOVE NEJ                TO NYCKLAR-SW                            
125400         MOVE 'IDDISTR'          TO RESP-IDELMT-ERROR                     
125500       ELSE                                                               
125600         INSPECT REQU-IDDISTR-KEY REPLACING LEADING SPACE BY ZERO         
125700         IF REQU-IDDISTR-KEY NUMERIC                                      
125800           CONTINUE                                                       
125900         ELSE                                                             
126000           MOVE NEJ              TO NYCKLAR-SW                            
126100           MOVE 'IDDISTR'       TO RESP-IDELMT-ERROR                      
126200         END-IF                                                           
126300       END-IF                                                             
126400                                                                          
126500**     INPUT FROM HANDHELD IS CURRENTLY BASED ON THE                      
126600**     CASE ID (DIST + CUST + ORDER + CASE NO), WHERE                     
126700**     ORDER IS A 7 DIGIT NUMBER. HOWEVER WDL6 HAS A                      
126800**     5 DIGIT ORDER NUMBER.                                              
126900       MOVE REQU-IDKUNDRF-KEY    TO WS-IDKUNDRF-GRP                       
127000       IF WS-IDORDNR5-FILLER = SPACES                                     
127100         CONTINUE                                                         
127200       ELSE                                                               
127300         MOVE WS-IDORDNR7        TO WS-IDKUNDRF-IDORDNR5                  
127400         MOVE WS-IDKUNDRF-IDORDNR5                                        
127500                                 TO REQU-IDKUNDRF-KEY                     
127600                                    W-SPAR-IDKUNDRF                       
127700       END-IF                                                             
127800       IF NYCKLAR-OK                                                      
127900         IF REQU-KDPGMACT = 'S'                                           
128000           PERFORM BA-FETCH-IDFAKT                                        
128100         ELSE                                                             
128200           PERFORM BB-FETCH-IDFAKT                                        
128300         END-IF                                                           
128400       END-IF                                                             
128500     END-IF                                                               
128600***  KONTROLL AV REQU-IDFAKT                                              
128700     IF NYCKLAR-OK                                                        
128800       IF REQU-IDFAKT-KEY = ALL '+'                                       
128900         MOVE NEJ      TO NYCKLAR-SW                                      
129000         MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                               
129100       ELSE                                                               
129200         INSPECT REQU-IDFAKT-KEY REPLACING LEADING SPACE BY ZERO          
129300         IF REQU-IDFAKT-KEY  NUMERIC AND                                  
129400            REQU-IDFAKT-KEY  > ZERO                                       
129500           MOVE REQU-IDFAKT-KEY TO W-SPAR-IDFAKT                          
129600         ELSE                                                             
129700           MOVE NEJ      TO NYCKLAR-SW                                    
129800           MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                             
129900         END-IF                                                           
130000       END-IF                                                             
130100     END-IF                                                               
130200                                                                          
130300***  KONTROLL AV REQU-IDDC                                                
130400     MOVE REQU-IDDC-KEY       TO RESP-IDDC-KEY                            
130410                                 WS-IDDC                                  
130500                                                                          
130600     MOVE REQU-IDFAKT-KEY     TO RESP-IDFAKT-KEY                          
130700     MOVE REQU-IDKUNDRF-KEY   TO RESP-IDKUNDRF-KEY                        
130800     MOVE REQU-IDKUNDNR-KEY   TO RESP-IDKUNDNR-KEY                        
130900     MOVE REQU-IDKOLLI-KEY    TO RESP-IDKOLLI-KEY                         
131000     INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE            
131100     INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE             
131200                                                                          
131300     IF NYCKLAR-FEL                                                       
131400        IF RESP-IDMSG-ERROR = SPACES                                      
131500           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
131600        END-IF                                                            
131700     END-IF                                                               
131800     .                                                                    
131900     EJECT                                                                
132000                                                                          
132100 BA-FETCH-IDFAKT SECTION.                                                 
132200     MOVE LOW-VALUE                  TO W-WDL6D1KY-MIN                    
132300     MOVE HIGH-VALUE                 TO W-WDL6D1KY-MAX                    
132400     MOVE REQU-IDKUNDRF-KEY          TO W-SEQD-IDKUNDRF-MIN               
132500                                        W-SEQD-IDKUNDRF-MAX               
132600     MOVE REQU-IDKUNDNR-KEY          TO W-SEQD-IDKUNDNR-MIN               
132700                                        W-SEQD-IDKUNDNR-MAX               
132800     MOVE REQU-IDKOLLI-KEY           TO W-SEQD-IDKOLLI-MIN                
132900                                        W-SEQD-IDKOLLI-MAX                
133000     MOVE REQU-IDDC-KEY              TO W-SEQD-IDDC-MIN                   
133100                                        W-SEQD-IDDC-MAX                   
133200     MOVE REQU-IDDISTR-KEY           TO W-SEQD-IDDISTR-MIN                
133300                                        W-SEQD-IDDISTR-MAX                
133400     PERFORM IMS-GU-WDL6D1                                                
133500     IF SEGMENT-FINNS                                                     
133600       MOVE SEQD-IDFAKT              TO REQU-IDFAKT-KEY                   
133700     ELSE                                                                 
133800       MOVE NEJ                      TO NYCKLAR-SW                        
133900       MOVE CASE-NOT-FOUND           TO RESP-IDMSG-ERROR                  
134000     END-IF                                                               
134100     .                                                                    
134200     EJECT                                                                
134300 BB-FETCH-IDFAKT SECTION.                                                 
134400     MOVE +1                         TO INDX                              
134500     MOVE REQU-KVRADER-MAX1          TO MAX-INDX                          
134600     MOVE REQU-IDARTNR (INDX)        TO W-IDARTNR                         
134700     MOVE REQU-DAINLEV (INDX)        TO W-DAINLEV                         
134800     MOVE REQU-IDDC-KEY              TO W-IDDC                            
134900     PERFORM IMS-GU-WLINLC11-GE                                           
135000     PERFORM UNTIL INDX > MAX-INDX OR NYCKLAR-FEL                         
135100       IF SEGMENT-FINNS                                                   
135200         IF INDX = 1                                                      
135300           MOVE INL-IDFAKT           TO REQU-IDFAKT-KEY                   
135400         END-IF                                                           
135500         MOVE INL-IDDISTR            TO W-SPAR-REQU-IDDISTR(INDX)         
135600         MOVE INL-IDKUNDRF           TO W-SPAR-REQU-IDKUNDRF(INDX)        
135700         MOVE INL-IDKUNDNR           TO W-SPAR-REQU-IDKUNDNR(INDX)        
135800         MOVE INL-IDKOLLI            TO W-SPAR-REQU-IDKOLLI(INDX)         
135900         MOVE INL-IDFAKT             TO W-SPAR-REQU-IDFAKT(INDX)          
136000         PERFORM BBA-SAVE-IDFAKT                                          
136100         ADD +1                      TO INDX                              
136200         IF INDX > MAX-INDX                                               
136300           CONTINUE                                                       
136400         ELSE                                                             
136500           MOVE REQU-IDARTNR (INDX)    TO W-IDARTNR                       
136600           MOVE REQU-DAINLEV (INDX)    TO W-DAINLEV                       
136700           PERFORM IMS-GU-WLINLC11-GE                                     
136800         END-IF                                                           
136900       ELSE                                                               
137000         MOVE NEJ                    TO NYCKLAR-SW                        
137100         MOVE CASE-NOT-FOUND         TO RESP-IDMSG-ERROR                  
137200       END-IF                                                             
137300     END-PERFORM                                                          
137400     .                                                                    
137500     EJECT                                                                
137600 BBA-SAVE-IDFAKT SECTION.                                                 
137700     MOVE +1 TO INDX-2                                                    
137800     PERFORM UNTIL INL-IDFAKT = W-SPAR-REQU-IDFAKT-2(INDX-2) OR           
137900                   W-SPAR-REQU-IDFAKT-2(INDX-2) = ZERO                    
138000       ADD +1 TO INDX-2                                                   
138100     END-PERFORM                                                          
138200     IF W-SPAR-REQU-IDFAKT-2(INDX-2) = ZERO                               
138300       MOVE INL-IDFAKT TO W-SPAR-REQU-IDFAKT-2(INDX-2)                    
138400       MOVE INDX-2 TO MAX-INDX-2                                          
138500     END-IF                                                               
138600     .                                                                    
138700     EJECT                                                                
138800 F-LAES-VISA-INFO SECTION.                                                
138900     MOVE 500  TO MAX-INDX                                                
139000     MOVE ZERO TO W-NEXT-IDARTNR                                          
139100                  W-NEXT-DAINLEV                                          
139200                  WS-KVANT                                                
139300     IF SUB-KDTRANS(1:6) = 'WLA103' AND                                   
139400        REQU-KDPGMACT = 'E'                                               
139500       MOVE W-SPAR-REQU-IDKUNDRF(1) TO W-SPAR-IDKUNDRF                    
139600       MOVE W-SPAR-REQU-IDKUNDNR(1) TO W-SPAR-IDKUNDNR                    
139700       MOVE W-SPAR-REQU-IDKOLLI(1)  TO W-SPAR-IDKOLLI                     
139800       MOVE W-SPAR-REQU-IDFAKT(1)   TO W-SPAR-IDFAKT                      
139900     END-IF                                                               
140000     PERFORM FA-LAES-GRUNDDATA                                            
140100                                                                          
140200     IF SEGMENT-SAKNAS                                                    
140300        IF REQU-KDPGMACT = 'E'                                            
140400           CONTINUE                                                       
140500        ELSE                                                              
140600           MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
140700        END-IF                                                            
140800     ELSE                                                                 
140900        IF SEQA-IDDC = REQU-IDDC-KEY                                      
141000           IF SEQA-IDPTYP  = 'R30'                                        
141100              MOVE KOLLI-EJ-RAPPORTERAT TO RESP-IDMSG-INFO                
141200           END-IF                                                         
141300           PERFORM FB-LAES-RADDATA                                        
141400        ELSE                                                              
141500           MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
141600        END-IF                                                            
141700     END-IF                                                               
141800     .                                                                    
141900     EJECT                                                                
142000                                                                          
142100 FA-LAES-GRUNDDATA SECTION.                                               
142200     MOVE JA              TO FAKTURA-FINNS-SW                             
142300     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
142400     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
142500     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
142600                             W-SEQA-IDFAKT-MAX                            
142700     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
142800                             W-SEQA-IDKUNDRF-MAX                          
142900     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
143000                             W-SEQA-IDKUNDNR-MAX                          
143100     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
143200                             W-SEQA-IDKOLLI-MAX                           
143300     PERFORM IMS-GU-WLINLD01-FIRST                                        
143400     .                                                                    
143500     EJECT                                                                
143600                                                                          
143700 FB-LAES-RADDATA SECTION.                                                 
143800     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
143900     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
144000     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
144100                             W-SEQA-IDFAKT-MAX                            
144200     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
144300                             W-SEQA-IDKUNDRF-MAX                          
144400     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
144500                             W-SEQA-IDKUNDNR-MAX                          
144600     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
144700                             W-SEQA-IDKOLLI-MAX                           
144800                                                                          
144900     MOVE W-NEXT-IDARTNR TO W-SEQA-IDARTNR-MIN                            
145000     MOVE W-NEXT-DAINLEV TO W-SEQA-DAINLEV-MIN                            
145100     MOVE SEQA-IDPTYP TO W-IDPTYP                                         
145200     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
145300                                                                          
145400     IF REQU-KDPGMACT = 'S'                                               
145500        MOVE SPACE TO RESP-KOLLI-KLART                                    
145600                      RESP-IDMSG-ERROR-INM                                
145700                      RESP-KVANTMOT-INM                                   
145800                      RESP-IDARTNR-INM                                    
145900                      RESP-ADLAGOMR-INM                                   
146000                      RESP-ADGANG-INM                                     
146100                      RESP-ADPLATS-INM                                    
146200                      RESP-KVSKROT-INM                                    
146300                      RESP-CMD-INM                                        
146400     END-IF                                                               
146500                                                                          
146600     MOVE +1 TO INDX                                                      
146700     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                      
146800       IF REQU-CMD-IN(INDX) = ALL '+' OR SPACE                            
146900          OR LOW-VALUE                                                    
147000            MOVE SPACE TO RESP-CMD-IN(INDX)                               
147100       END-IF                                                             
147200       IF REQU-KVANTMOT-IN(INDX) = ALL '+' OR SPACE                       
147300          OR LOW-VALUE                                                    
147400            MOVE SPACE TO RESP-KVANTMOT-IN(INDX)                          
147500       END-IF                                                             
147600       IF REQU-KVSKROT-IN(INDX) = ALL '+' OR SPACE                        
147700          OR LOW-VALUE                                                    
147800            MOVE SPACE TO RESP-KVSKROT-IN(INDX)                           
147900       END-IF                                                             
148000       IF REQU-KDPGMACT = 'S'                                             
148100          MOVE SPACE TO RESP-IDMSG-ERROR-LINE(INDX)                       
148200       END-IF                                                             
148300                                                                          
148400       MOVE SEQA-IDARTNR TO W-IDARTNR                                     
148500                            RESP-IDARTNR(INDX)                            
148600       MOVE SEQA-DAINLEV  TO W-DAINLEV                                    
148700                             RESP-DAINLEV(INDX)                           
148800       MOVE SEQA-IDDC     TO W-IDDC                                       
148900       PERFORM IMS-GU-WLINLC11                                            
149000       MOVE INL-KVAVIS    TO RESP-KVAVIS(INDX)                            
149100                                                                          
149200       PERFORM IMS-GET-WLARTC11                                           
149300       IF DCS-CDC                                                         
149400         MOVE CLAG-ADLAGOMR   TO WS-RED-ADLAGOMR                          
149500         MOVE WS-RED-ADLAGOMR TO RESP-ADLAGOMR(INDX)                      
149600         MOVE CLAG-ADGANG     TO WS-RED-ADGANG                            
149700         MOVE WS-RED-ADGANG   TO RESP-ADGANG(INDX)                        
149800         MOVE CLAG-ADPLATS    TO WS-RED-ADPLATS                           
149900         MOVE WS-RED-ADPLATS  TO RESP-ADPLATS(INDX)                       
150000       ELSE                                                               
150100         PERFORM IMS-GET-WDK711                                           
150200         MOVE SLAG-ADLAGOMR   TO WS-RED-ADLAGOMR                          
150300         MOVE WS-RED-ADLAGOMR TO RESP-ADLAGOMR(INDX)                      
150400         MOVE SLAG-ADGANG     TO WS-RED-ADGANG                            
150500         MOVE WS-RED-ADGANG   TO RESP-ADGANG(INDX)                        
150600         MOVE SLAG-ADPLATS    TO WS-RED-ADPLATS                           
150700         MOVE WS-RED-ADPLATS  TO RESP-ADPLATS(INDX)                       
150800       END-IF                                                             
150900                                                                          
151000       IF CLAG-KDFARLIG = 4 OR CLAG-KDFARLIG = 7                          
151100         MOVE 'J'             TO RESP-FLFARLIG(INDX)                      
151200       ELSE                                                               
151300         MOVE 'N'             TO RESP-FLFARLIG(INDX)                      
151400       END-IF                                                             
151500                                                                          
151600       IF INL-FLPRIO = 'Y' OR 'J'                                         
151700          MOVE 'Y'        TO RESP-KDPRIO (INDX)                           
151800       ELSE                                                               
151900          MOVE SPACE      TO RESP-KDPRIO (INDX)                           
152000       END-IF                                                             
152100                                                                          
152200       IF REQU-KDPGMACT = 'S'                                             
152300       OR (REQU-KDPGMACT = 'E' AND INDATA-OK)                             
152400          MOVE SPACE TO RESP-CMD-IN(INDX)                                 
152500                        RESP-KVANTMOT-IN(INDX)                            
152600                        RESP-KVSKROT-IN(INDX)                             
152700       END-IF                                                             
152800                                                                          
152900       MOVE REQU-IDDC-KEY         TO WS-IDDC                              
153000       MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                            
153100       IF DCS-UNICODE-IDSKYLT                                             
153200          MOVE 'UTF8'             TO TRAUTF8-KDCP                         
153300       ELSE                                                               
153400          MOVE '278 '             TO TRAUTF8-KDCP                         
153500       END-IF                                                             
153600       PERFORM IMS-GU-WLBENA11                                            
153700       IF SEGMENT-FINNS                                                   
153800          MOVE TEXT-BEARTEXT      TO TRAUTF8-TECONV-FROM                  
153900       ELSE                                                               
154000          MOVE SPACE              TO TRAUTF8-TECONV-FROM                  
154100       END-IF                                                             
154200       IF TRAUTF8-TECONV-FROM = SPACES                                    
154300        MOVE 'GB'  TO W-IDSKYLT                                           
154400        MOVE '278' TO TRAUTF8-KDCP                                        
154500        PERFORM IMS-GU-WLBENA11                                           
154600        MOVE TEXT-BEARTEXT TO TRAUTF8-TECONV-FROM                         
154700       END-IF                                                             
154800*      -- STRIP SPACE OR CONVERT TO UNICODE                               
154900       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
155000                                                                          
155100*      -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                      
155200       MOVE TRAUTF8-TECONV-TO     TO RESP-BEART(INDX)                     
155300       PERFORM IMS-GU-W6KVAH11                                            
155400       IF SEGMENT-FINNS                                                   
155500        IF INFO-DAREGDAT-9KOMPL > INL-DAINLEV(1:8)                        
155600         CONTINUE                                                         
155700        ELSE                                                              
155800         IF INFO-TEKVAINF-EXT(1) NOT = SPACE                              
155900           MOVE INFO-TEKVAINF-EXT(1)  TO                                  
156000                  RESP-TEKVAINF-EXT (INDX,1)                              
156100         END-IF                                                           
156200         IF INFO-TEKVAINF-EXT(2) NOT = SPACE                              
156300           MOVE INFO-TEKVAINF-EXT(2)  TO                                  
156400                  RESP-TEKVAINF-EXT (INDX,2)                              
156500         END-IF                                                           
156600         IF INFO-TEKVAINF-EXT(3) NOT = SPACE                              
156700           MOVE INFO-TEKVAINF-EXT(3)  TO                                  
156800                  RESP-TEKVAINF-EXT (INDX,3)                              
156900         END-IF                                                           
157000         IF INFO-TEKVAINF-EXT(4) NOT = SPACE                              
157100           MOVE INFO-TEKVAINF-EXT(4)  TO                                  
157200                  RESP-TEKVAINF-EXT (INDX,4)                              
157300         END-IF                                                           
157400         IF INFO-TEKVAINF-EXT(5) NOT = SPACE                              
157500           MOVE INFO-TEKVAINF-EXT(5)  TO                                  
157600                  RESP-TEKVAINF-EXT (INDX,5)                              
157700         END-IF                                                           
157800         IF INFO-TEKVAINF-EXT(6) NOT = SPACE                              
157900           MOVE INFO-TEKVAINF-EXT(6)  TO                                  
158000                  RESP-TEKVAINF-EXT (INDX,6)                              
158100         END-IF                                                           
158200         IF INFO-TEKVAINF-EXT(7) NOT = SPACE                              
158300           MOVE INFO-TEKVAINF-EXT(7)  TO                                  
158400                  RESP-TEKVAINF-EXT (INDX,7)                              
158500         END-IF                                                           
158600        END-IF                                                            
158700       END-IF                                                             
158800                                                                          
158900       ADD 1 TO WS-KVANT                                                  
159000                                                                          
159100       ADD +1 TO INDX                                                     
159200       PERFORM IMS-GN-WLINLD01                                            
159300     END-PERFORM                                                          
159400                                                                          
159500*    -- LÄS FÄRDIGT FÖR ATT FÅ TOTALA ANTALET RADER I BASEN               
159600     PERFORM UNTIL SEGMENT-SAKNAS                                         
159700        ADD 1 TO WS-KVANT                                                 
159800        PERFORM IMS-GN-WLINLD01                                           
159900     END-PERFORM                                                          
160000                                                                          
160100     MOVE WS-KVANT TO RESP-KVRADER-MAX1                                   
160200     IF WS-KVANT > 500                                                    
160300        MOVE 500 TO RESP-KVRADER-MAX1                                     
160400        MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                           
160500     END-IF                                                               
160600     .                                                                    
160700     EJECT                                                                
160800                                                                          
160900 G-KOLLA-INPUT SECTION.                                                   
161000     MOVE JA TO INDATA-SW                                                 
161100     MOVE NEJ TO NY-NYUPPLAEGG-ART-SW                                     
161200                 NY-BEFINTLIG-ART-SW                                      
161300                 NY-KVANTMOT-SW                                           
161400                                                                          
161500     IF REQU-KVRADER-MAX1 NUMERIC                                         
161600     AND REQU-KVRADER-MAX1 > ZERO                                         
161700        MOVE REQU-KVRADER-MAX1 TO MAX-INDX                                
161800     END-IF                                                               
161900                                                                          
162000     IF INDATA-SAKNAS                                                     
162100        MOVE NEJ TO INDATA-SW                                             
162200        IF REQU-IDUSER-003 = ALL '+' OR SPACE                             
162300           MOVE SPACE           TO RESP-IDUSER-003                        
162400        ELSE                                                              
162500           MOVE REQU-IDUSER-003 TO RESP-IDUSER-003                        
162600           MOVE NEJ TO INDATA-SW                                          
162700           MOVE 'IDUSER' TO RESP-IDELMT-ERROR                             
162800        END-IF                                                            
162900     ELSE                                                                 
163000        IF REQU-KDPGMACT = 'X'                                            
163100           PERFORM GI-KOLLA-BIN                                           
163200        ELSE                                                              
163300           PERFORM GA-KOLLA-KOLLI-KLART                                   
163400                                                                          
163500           MOVE +1 TO INDX                                                
163600           PERFORM UNTIL INDX > MAX-INDX                                  
163700              IF REQU-IDARTNR(INDX) NOT NUMERIC                           
163800                 MOVE ZERO TO REQU-IDARTNR(INDX)                          
163900              END-IF                                                      
164000              PERFORM GB-KOLLA-CMD                                        
164100              PERFORM GC-KOLLA-KVANTMOT                                   
164200              PERFORM GE-KOLLA-KVSKROT                                    
164300              MOVE ZERO TO W-ADLAGOMR(INDX)                               
164400                           W-ADGANG(INDX)                                 
164500                           W-ADPLATS(INDX)                                
164600              IF W-KVANTMOT(INDX) > ZERO OR W-KVSKROT(INDX) > ZERO        
164700              OR REQU-CMD-IN(INDX) = 'ADR'                                
164800                 IF W-KVANTMOT(INDX) = ZERO                               
164900                 AND REQU-CMD-IN(INDX) = 'DAM'                            
165000                    CONTINUE                                              
165100                 ELSE                                                     
165200                    PERFORM GD-KOLLA-LAGERPLATS                           
165300                 END-IF                                                   
165400              END-IF                                                      
165500              IF REQU-KOLLI-KLART = 'X'                                   
165600              OR REQU-KOLLI-KLART = 'Y'                                   
165700                 IF WS-FLTRACK = 'J'                                      
165800                    PERFORM GG-KOLLA-IDTRACK                              
165900                 END-IF                                                   
166000              END-IF                                                      
166100              IF REQU-KVANTMOT-IN(INDX) NOT = ALL '+'                     
166200**              IF (REQU-CMD-IN(INDX) = '+++' OR '   ')                   
166300                  IF WS-FLTRACK = 'J'                                     
166400                     PERFORM GG-KOLLA-IDTRACK                             
166500                  END-IF                                                  
166600**              END-IF                                                    
166700              END-IF                                                      
166800                                                                          
166810              IF INDATA-OK AND NDC-BR                                     
166811                IF REQU-CMD-IN(INDX) = 'DAM'                              
166812                  MOVE REQU-IDARTNR (INDX)    TO W-IDARTNR                
166813                  MOVE REQU-DAINLEV (INDX)    TO W-DAINLEV                
166814                  PERFORM IMS-GU-WLINLC11                                 
166815                  COMPUTE WS-NOTF-KVANT =                                 
166816                          ((W-KVANTMOT(INDX) + W-KVSKROT(INDX)) -         
166817                          INL-KVAVIS)                                     
166818                  IF WS-NOTF-KVANT > ZERO                                 
166819                    MOVE NEJ TO INDATA-SW                                 
166820                    MOVE INDX TO INDX-DISPLAY                             
166821                    MOVE SPACE TO RESP-IDELMT-ERROR                       
166822                  STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE        
166823                    INTO RESP-IDELMT-ERROR                                
166824                    MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)             
166825                  END-IF                                                  
166826                ELSE                                                      
166827                  IF W-KVANTMOT(INDX) > ZERO                              
166828                    MOVE REQU-IDARTNR (INDX)    TO W-IDARTNR              
166829                    MOVE REQU-DAINLEV (INDX)    TO W-DAINLEV              
166830                    PERFORM IMS-GU-WLINLC11                               
166831                    COMPUTE WS-NOTF-KVANT =                               
166832                            W-KVANTMOT(INDX) - INL-KVAVIS                 
166833                  END-IF                                                  
166834                  IF WS-NOTF-KVANT > ZERO                                 
166835                    MOVE NEJ TO INDATA-SW                                 
166836                    MOVE INDX TO INDX-DISPLAY                             
166837                    MOVE SPACE TO RESP-IDELMT-ERROR                       
166838                 STRING 'KVANTMOT*' INDX-DISPLAY DELIMITED BY SIZE        
166839                    INTO RESP-IDELMT-ERROR                                
166840                    MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)             
166841                  END-IF                                                  
166850                END-IF                                                    
166860              END-IF                                                      
166900                                                                          
167000              ADD 1 TO INDX                                               
167100           END-PERFORM                                                    
167200                                                                          
167300           PERFORM X-KOLLA-INDATA-IGEN                                    
167400           PERFORM GF-KOLLA-NY-ARTIKEL                                    
167500                                                                          
167600           IF INDATA-FEL                                                  
167700              IF INDATA-SAKNAS                                            
167800                 MOVE NO-DATA-ENTERED TO RESP-IDMSG-INFO                  
167900              ELSE                                                        
168000                 IF IDTRACK-FEL                                           
168100                    MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR          
168200                 ELSE                                                     
168300                    MOVE IS-INVALID TO RESP-IDMSG-ERROR                   
168400                 END-IF                                                   
168500              END-IF                                                      
168600           END-IF                                                         
168700           IF IDUSER-FEL                                                  
168800              MOVE 'IDANSTNR'         TO RESP-IDELMT-ERROR                
168900              MOVE MUST-ENTER-EMP-ID  TO RESP-IDMSG-ERROR                 
169000           END-IF                                                         
169100           MOVE LOW-VALUE             TO W-WDL6A1KY-MIN                   
169200           MOVE HIGH-VALUE            TO W-WDL6A1KY-MAX                   
169300           MOVE W-SPAR-IDFAKT         TO W-SEQA-IDFAKT-MIN                
169400                                         W-SEQA-IDFAKT-MAX                
169500           MOVE W-SPAR-IDKUNDRF       TO W-SEQA-IDKUNDRF-MIN              
169600                                         W-SEQA-IDKUNDRF-MAX              
169700           MOVE W-SPAR-IDKUNDNR       TO W-SEQA-IDKUNDNR-MIN              
169800                                         W-SEQA-IDKUNDNR-MAX              
169900           MOVE W-SPAR-IDKOLLI        TO W-SEQA-IDKOLLI-MIN               
170000                                         W-SEQA-IDKOLLI-MAX               
170100           PERFORM IMS-GU-WLINLD01-FIRST                                  
170200           IF SEGMENT-SAKNAS                                              
170300             MOVE UPD-NOT-ALLOWED     TO RESP-IDMSG-ERROR                 
170400             MOVE NEJ                 TO INDATA-SW                        
170500           ELSE                                                           
170600             IF SEQA-IDPTYP  = 'R30'                                      
170700               MOVE KOLLI-EJ-RAPPORTERAT                                  
170800                                 TO RESP-IDMSG-ERROR                      
170900               MOVE NEJ          TO INDATA-SW                             
171000             END-IF                                                       
171100           END-IF                                                         
171200        END-IF                                                            
171300     END-IF                                                               
171400     .                                                                    
171500     EJECT                                                                
171600 GA-KOLLA-KOLLI-KLART SECTION.                                            
171700                                                                          
171800     IF REQU-KOLLI-KLART NOT = ALL '+'                                    
171900        MOVE REQU-KOLLI-KLART TO RESP-KOLLI-KLART                         
172000     ELSE                                                                 
172100        MOVE SPACE            TO RESP-KOLLI-KLART                         
172200     END-IF                                                               
172300                                                                          
172400     IF REQU-KOLLI-KLART NOT = 'X'                                        
172500     AND REQU-KOLLI-KLART NOT = 'Y'                                       
172600        IF  REQU-KOLLI-KLART  NOT = ALL '+'                               
172700        AND REQU-KOLLI-KLART  NOT = SPACE                                 
172800           MOVE NEJ TO INDATA-SW                                          
172900           MOVE 'KOLLI-KLAR'  TO RESP-IDELMT-ERROR                        
173000         END-IF                                                           
173100     ELSE                                                                 
173200         MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                           
173300         MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                           
173400         MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                        
173500                                 W-SEQA-IDFAKT-MAX                        
173600         MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                      
173700                                 W-SEQA-IDKUNDRF-MAX                      
173800         MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                      
173900                                 W-SEQA-IDKUNDNR-MAX                      
174000         MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                       
174100                                 W-SEQA-IDKOLLI-MAX                       
174200         MOVE '310'           TO W-IDPTYP                                 
174300         PERFORM IMS-GU-WLINLD01-FIRST-310                                
174400                                                                          
174500         PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                       
174600             MOVE SEQA-IDARTNR  TO W-IDARTNR                              
174700             MOVE SEQA-IDDC     TO W-IDDC                                 
174800             IF DCS-CDC                                                   
174900               PERFORM IMS-GET-WLARTC11                                   
175000               IF CLAG-ADLAGOMR = ZERO                                    
175100                 MOVE NEJ       TO INDATA-SW                              
175200                 MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                      
175300               END-IF                                                     
175400             ELSE                                                         
175500               PERFORM IMS-GET-WDK711                                     
175600                                                                          
175700               IF SLAG-ADLAGOMR  = ZERO                                   
175800                  MOVE NEJ       TO INDATA-SW                             
175900                  MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                     
176000               END-IF                                                     
176100             END-IF                                                       
176200             PERFORM IMS-GN-WLINLD01                                      
176300         END-PERFORM                                                      
176400         PERFORM S35-KOLLA-IDUSER                                         
176500     END-IF                                                               
176600     .                                                                    
176700     EJECT                                                                
176800 GB-KOLLA-CMD SECTION.                                                    
176900                                                                          
177000     IF REQU-CMD-IN(INDX) NOT = ALL '+'                                   
177100        MOVE REQU-CMD-IN(INDX) TO RESP-CMD-IN(INDX)                       
177200     ELSE                                                                 
177300        MOVE SPACE             TO RESP-CMD-IN(INDX)                       
177400     END-IF                                                               
177500                                                                          
177600     IF REQU-KOLLI-KLART = 'X'                                            
177700     OR REQU-KOLLI-KLART = 'Y'                                            
177800        IF  REQU-CMD-IN (INDX) NOT = ALL '+'                              
177900        AND REQU-CMD-IN (INDX) NOT = SPACE                                
178000           MOVE NEJ TO INDATA-SW                                          
178100           MOVE INDX TO INDX-DISPLAY                                      
178200           MOVE SPACE TO RESP-IDELMT-ERROR                                
178300           STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                   
178400                  INTO RESP-IDELMT-ERROR                                  
178500           MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                      
178600        END-IF                                                            
178700     ELSE                                                                 
178800        IF  REQU-CMD-IN (INDX) NOT = ALL '+'                              
178900        AND REQU-CMD-IN (INDX) NOT = 'DAM'                                
179000        AND REQU-CMD-IN (INDX) NOT = 'ADR'                                
179100        AND REQU-CMD-IN (INDX) NOT = 'HAC'                                
179200        AND REQU-CMD-IN (INDX) NOT = SPACE                                
179300           MOVE NEJ TO INDATA-SW                                          
179400           MOVE INDX TO INDX-DISPLAY                                      
179500           MOVE SPACE TO RESP-IDELMT-ERROR                                
179600           STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                   
179700                  INTO RESP-IDELMT-ERROR                                  
179800           MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                      
179900        END-IF                                                            
180000                                                                          
180100        IF REQU-CMD-IN (INDX) = 'DAM' OR 'HAC'                            
180200           IF REQU-IDARTNR(INDX) = ZERO                                   
180300           OR REQU-IDARTNR(INDX) = ALL '+'                                
180400           OR REQU-IDARTNR(INDX) = SPACE                                  
180500           OR REQU-IDARTNR(INDX) = LOW-VALUE                              
180600              MOVE NEJ TO INDATA-SW                                       
180700              MOVE INDX TO INDX-DISPLAY                                   
180800              MOVE SPACE TO RESP-IDELMT-ERROR                             
180900              STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                
181000                   INTO RESP-IDELMT-ERROR                                 
181100              MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                   
181200           END-IF                                                         
181300           PERFORM S35-KOLLA-IDUSER                                       
181400        END-IF                                                            
181500     END-IF                                                               
181600     .                                                                    
181700     EJECT                                                                
181800 GC-KOLLA-KVANTMOT SECTION.                                               
181900                                                                          
182000     IF REQU-KVANTMOT-IN(INDX) NOT = ALL '+'                              
182100        MOVE REQU-KVANTMOT-IN(INDX) TO RESP-KVANTMOT-IN(INDX)             
182200     END-IF                                                               
182300                                                                          
182400     MOVE ZERO TO W-KVANTMOT(INDX)                                        
182500                                                                          
182600     IF REQU-KOLLI-KLART = 'X'                                            
182700     OR REQU-KOLLI-KLART = 'Y'                                            
182800        IF  REQU-KVANTMOT-IN(INDX) NOT = ALL '+'                          
182900        AND REQU-KVANTMOT-IN(INDX) NOT = SPACE                            
183000        AND REQU-KVANTMOT-IN(INDX) NOT = LOW-VALUE                        
183100           MOVE NEJ TO INDATA-SW                                          
183200           MOVE INDX TO INDX-DISPLAY                                      
183300           MOVE SPACE TO RESP-IDELMT-ERROR                                
183400           STRING 'KVANTMOT*' INDX-DISPLAY DELIMITED BY SIZE              
183500                INTO RESP-IDELMT-ERROR                                    
183600           MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                      
183700        END-IF                                                            
183800     ELSE                                                                 
183900        IF REQU-KVANTMOT-IN(INDX) NOT = ALL '+'                           
184000        AND REQU-KVANTMOT-IN(INDX) NOT = SPACE                            
184100        AND REQU-KVANTMOT-IN(INDX) NOT = LOW-VALUE                        
184200           MOVE REQU-KVANTMOT-IN(INDX) TO WS-KVANTMOT                     
184300           INSPECT WS-KVANTMOT REPLACING LEADING SPACE BY ZERO            
184400           IF WS-KVANTMOT NOT NUMERIC                                     
184500              MOVE NEJ TO INDATA-SW                                       
184600              MOVE INDX TO INDX-DISPLAY                                   
184700              MOVE SPACE TO RESP-IDELMT-ERROR                             
184800              STRING 'KVANTMOT*' INDX-DISPLAY DELIMITED BY SIZE           
184900                   INTO RESP-IDELMT-ERROR                                 
185000              MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                   
185100           ELSE                                                           
185200              MOVE WS-KVANTMOT TO W-KVANTMOT (INDX)                       
185300           END-IF                                                         
185400           PERFORM S35-KOLLA-IDUSER                                       
185500        END-IF                                                            
185600     END-IF                                                               
185700                                                                          
185800     IF REQU-CMD-IN (INDX) = 'DAM'                                        
185900       IF REQU-KVANTMOT-IN(INDX) = ALL '+'                                
186000       OR REQU-KVANTMOT-IN(INDX) = SPACE                                  
186100           MOVE NEJ TO INDATA-SW                                          
186200           MOVE INDX TO INDX-DISPLAY                                      
186300           MOVE SPACE TO RESP-IDELMT-ERROR                                
186400           STRING 'KVANTMOT*' INDX-DISPLAY DELIMITED BY SIZE              
186500                INTO RESP-IDELMT-ERROR                                    
186600**         MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                      
186700           MOVE 'BIN QTY MISSING' TO RESP-IDMSG-ERROR-LINE(INDX)          
186800       END-IF                                                             
186900     END-IF                                                               
187000     .                                                                    
187100     EJECT                                                                
187200 GD-KOLLA-LAGERPLATS SECTION.                                             
187300                                                                          
187400     MOVE ZERO TO RAD-SPAR                                                
187500     MOVE ZERO TO W-ADLAGOMR(INDX)                                        
187600                  W-ADGANG(INDX)                                          
187700                  W-ADPLATS(INDX)                                         
187800                                                                          
187900     IF REQU-ADLAGOMR(INDX) NOT = ALL '+'                                 
188000     AND (REQU-ADLAGOMR(INDX) NOT = SPACE)                                
188100     AND (REQU-ADLAGOMR(INDX) NOT = LOW-VALUE)                            
188200        MOVE REQU-ADLAGOMR(INDX) TO RESP-ADLAGOMR(INDX)                   
188300        MOVE INDX TO RAD-SPAR                                             
188400        IF REQU-IDARTNR(INDX) = ZERO                                      
188500        OR REQU-IDARTNR(INDX) = ALL '+'                                   
188600        OR REQU-IDARTNR(INDX) = SPACE                                     
188700        OR REQU-IDARTNR(INDX) = LOW-VALUE                                 
188800           MOVE NEJ TO INDATA-SW                                          
188900           MOVE INDX TO INDX-DISPLAY                                      
189000           MOVE SPACE TO RESP-IDELMT-ERROR                                
189100           STRING 'ADLAGOMR*' INDX-DISPLAY DELIMITED BY SIZE              
189200                 INTO RESP-IDELMT-ERROR                                   
189300           MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(INDX)                      
189400        END-IF                                                            
189500     END-IF                                                               
189600     IF REQU-ADGANG(INDX) NOT = ALL '+'                                   
189700     AND REQU-ADGANG(INDX) NOT = SPACE                                    
189800     AND REQU-ADGANG(INDX) NOT = LOW-VALUE                                
189900        MOVE REQU-ADGANG(INDX) TO RESP-ADGANG(INDX)                       
190000        MOVE INDX TO RAD-SPAR                                             
190100        IF REQU-IDARTNR(INDX) = ZERO                                      
190200        OR REQU-IDARTNR(INDX) = ALL '+'                                   
190300        OR REQU-IDARTNR(INDX) = SPACE                                     
190400        OR REQU-IDARTNR(INDX) = LOW-VALUE                                 
190500           MOVE NEJ TO INDATA-SW                                          
190600           MOVE INDX TO INDX-DISPLAY                                      
190700           MOVE SPACE TO RESP-IDELMT-ERROR                                
190800           STRING 'ADGANG*' INDX-DISPLAY DELIMITED BY SIZE                
190900                 INTO RESP-IDELMT-ERROR                                   
191000           MOVE 'PL'     TO RESP-IDMSG-ERROR-LINE(INDX)                   
191100        END-IF                                                            
191200     END-IF                                                               
191300     IF REQU-ADPLATS(INDX) NOT = ALL '+'                                  
191400     AND REQU-ADPLATS(INDX) NOT = SPACE                                   
191500     AND REQU-ADPLATS(INDX) NOT = LOW-VALUE                               
191600        MOVE INDX TO RAD-SPAR                                             
191700        MOVE REQU-ADPLATS(INDX) TO RESP-ADPLATS(INDX)                     
191800        IF REQU-IDARTNR(INDX) = ZERO                                      
191900        OR REQU-IDARTNR(INDX) = ALL '+'                                   
192000        OR REQU-IDARTNR(INDX) = SPACE                                     
192100        OR REQU-IDARTNR(INDX) = LOW-VALUE                                 
192200           MOVE NEJ TO INDATA-SW                                          
192300           MOVE SPACE TO RESP-IDELMT-ERROR                                
192400           MOVE INDX TO INDX-DISPLAY                                      
192500           STRING 'ADPLATS*' INDX-DISPLAY DELIMITED BY SIZE               
192600                 INTO RESP-IDELMT-ERROR                                   
192700           MOVE 'PL'     TO RESP-IDMSG-ERROR-LINE(INDX)                   
192800        END-IF                                                            
192900     END-IF                                                               
193000                                                                          
193100     IF REQU-KOLLI-KLART  = 'X'                                           
193200     OR REQU-KOLLI-KLART  = 'Y'                                           
193300        IF REQU-ADLAGOMR (INDX) NOT = ALL '+'                             
193400           MOVE NEJ TO INDATA-SW                                          
193500           MOVE SPACE TO RESP-IDELMT-ERROR                                
193600           MOVE INDX TO INDX-DISPLAY                                      
193700           STRING 'ADLAGOMR*' INDX-DISPLAY DELIMITED BY SIZE              
193800                 INTO RESP-IDELMT-ERROR                                   
193900           MOVE 'OMR'    TO   RESP-IDMSG-ERROR-LINE(INDX)                 
194000        END-IF                                                            
194100     ELSE                                                                 
194200        IF REQU-ADLAGOMR(INDX) NOT = ALL '+'                              
194300        AND (REQU-ADLAGOMR(INDX) NOT = SPACE)                             
194400        AND (REQU-ADLAGOMR(INDX) NOT = LOW-VALUE)                         
194500           MOVE REQU-ADLAGOMR (INDX) TO WS-ADLAGOMR                       
194600           INSPECT WS-ADLAGOMR REPLACING LEADING SPACE BY ZERO            
194700           IF WS-ADLAGOMR NOT NUMERIC                                     
194800           OR WS-ADLAGOMR NOT > ZERO                                      
194900              MOVE NEJ TO INDATA-SW                                       
195000              MOVE INDX TO INDX-DISPLAY                                   
195100              MOVE SPACE TO RESP-IDELMT-ERROR                             
195200              STRING 'ADLAGOMR*' INDX-DISPLAY DELIMITED BY SIZE           
195300                   INTO RESP-IDELMT-ERROR                                 
195400              MOVE 'OMR'    TO   RESP-IDMSG-ERROR-LINE(INDX)              
195500           ELSE                                                           
195600              MOVE WS-ADLAGOMR TO W-ADLAGOMR (INDX)                       
195700           END-IF                                                         
195800        END-IF                                                            
195900     END-IF                                                               
196000                                                                          
196100     IF REQU-KOLLI-KLART = 'X'                                            
196200     OR REQU-KOLLI-KLART = 'Y'                                            
196300        IF REQU-ADGANG (INDX) NOT = ALL '+'                               
196400           MOVE NEJ TO INDATA-SW                                          
196500           MOVE INDX TO INDX-DISPLAY                                      
196600           MOVE SPACE TO RESP-IDELMT-ERROR                                
196700           STRING 'ADLAGOMR*' INDX-DISPLAY DELIMITED BY SIZE              
196800                  INTO RESP-IDELMT-ERROR                                  
196900           MOVE 'PL' TO RESP-IDMSG-ERROR-LINE(INDX)                       
197000        END-IF                                                            
197100     ELSE                                                                 
197200        IF REQU-ADGANG(INDX) NOT = ALL '+'                                
197300           MOVE REQU-ADGANG(INDX) TO WS-ADGANG                            
197400           INSPECT WS-ADGANG REPLACING LEADING SPACE BY ZERO              
197500           IF WS-ADGANG NOT NUMERIC                                       
197600              MOVE INDX TO INDX-DISPLAY                                   
197700              MOVE SPACE TO RESP-IDELMT-ERROR                             
197800              STRING 'ADGANG*' INDX-DISPLAY DELIMITED BY SIZE             
197900                    INTO RESP-IDELMT-ERROR                                
198000              MOVE 'PL' TO RESP-IDMSG-ERROR-LINE(INDX)                    
198100              MOVE NEJ TO INDATA-SW                                       
198200           ELSE                                                           
198300              MOVE WS-ADGANG TO W-ADGANG (INDX)                           
198400           END-IF                                                         
198500        END-IF                                                            
198600     END-IF                                                               
198700                                                                          
198800     IF REQU-KOLLI-KLART  = 'X'                                           
198900     OR REQU-KOLLI-KLART  = 'Y'                                           
199000        IF REQU-ADPLATS(INDX) NOT = ALL '+'                               
199100           MOVE NEJ TO INDATA-SW                                          
199200           MOVE INDX TO INDX-DISPLAY                                      
199300           MOVE SPACE TO RESP-IDELMT-ERROR                                
199400           STRING 'ADPLATS*' INDX-DISPLAY DELIMITED BY SIZE               
199500                  INTO RESP-IDELMT-ERROR                                  
199600           MOVE 'PL' TO RESP-IDMSG-ERROR-LINE(INDX)                       
199700        END-IF                                                            
199800     ELSE                                                                 
199900        IF REQU-ADPLATS(INDX) NOT = ALL '+'                               
200000           MOVE REQU-ADPLATS (INDX) TO WS-ADPLATS                         
200100           INSPECT WS-ADPLATS REPLACING LEADING SPACE BY ZERO             
200200           IF WS-ADPLATS NOT NUMERIC                                      
200300              MOVE INDX TO INDX-DISPLAY                                   
200400              MOVE SPACE TO RESP-IDELMT-ERROR                             
200500              STRING 'ADPLATS*' INDX-DISPLAY DELIMITED BY SIZE            
200600                  INTO RESP-IDELMT-ERROR                                  
200700              MOVE 'PL' TO RESP-IDMSG-ERROR-LINE(INDX)                    
200800              MOVE NEJ TO INDATA-SW                                       
200900           ELSE                                                           
201000              MOVE WS-ADPLATS TO W-ADPLATS (INDX)                         
201100           END-IF                                                         
201200        END-IF                                                            
201300     END-IF                                                               
201400                                                                          
201500     IF REQU-KOLLI-KLART  = 'X'                                           
201600     OR REQU-KOLLI-KLART  = 'Y'                                           
201700        CONTINUE                                                          
201800     ELSE                                                                 
201900        IF REQU-KVANTMOT-IN(INDX) NOT = ALL '+'                           
202000        AND REQU-KVANTMOT-IN (INDX) NOT = SPACE                           
202100           IF INDATA-OK                                                   
202200           AND W-KVANTMOT(INDX) = ZERO                                    
202300              CONTINUE                                                    
202400           ELSE                                                           
202500              IF REQU-IDARTNR(INDX) NOT = ZERO                            
202600                IF REQU-IDARTNR(INDX)   = ALL '+'                         
202700                OR REQU-IDARTNR(INDX)  = SPACE                            
202800                OR REQU-IDARTNR(INDX)  = LOW-VALUE                        
202900                  MOVE INDX TO INDX-DISPLAY                               
203000                  MOVE SPACE TO RESP-IDELMT-ERROR                         
203100                  STRING 'IDARTNR' INDX-DISPLAY                           
203200                     DELIMITED BY SIZE                                    
203300                     INTO RESP-IDELMT-ERROR                               
203400                  MOVE 'ART'                                              
203500                         TO RESP-IDMSG-ERROR-LINE(INDX)                   
203600                  MOVE NEJ TO INDATA-SW                                   
203700                ELSE                                                      
203800                  MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                   
203900                  MOVE REQU-IDDC-KEY TO W-IDDC                            
204000                  IF DCS-CDC                                              
204100                    PERFORM IMS-GET-WLARTC11                              
204200                    IF CLAG-ADLAGOMR = ZERO                               
204300                      IF REQU-ADLAGOMR(INDX) = ALL '+' OR SPACE           
204400                        MOVE INDX TO INDX-DISPLAY                         
204500                        MOVE SPACE TO RESP-IDELMT-ERROR                   
204600                        STRING 'ADLAGOMR' INDX-DISPLAY                    
204700                           DELIMITED BY SIZE                              
204800                           INTO RESP-IDELMT-ERROR                         
204900                        MOVE 'OMR'                                        
205000                               TO RESP-IDMSG-ERROR-LINE(INDX)             
205100                        MOVE NEJ TO INDATA-SW                             
205200                      END-IF                                              
205300                    END-IF                                                
205400                  ELSE                                                    
205500                    PERFORM IMS-GET-WDK711-GE                             
205600                    IF SEGMENT-FINNS                                      
205700                      IF SLAG-ADLAGOMR = ZERO                             
205800                        IF REQU-ADLAGOMR(INDX) = ALL '+' OR SPACE         
205900                           MOVE INDX TO INDX-DISPLAY                      
206000                           MOVE SPACE TO RESP-IDELMT-ERROR                
206100                           STRING 'ADLAGOMR' INDX-DISPLAY                 
206200                              DELIMITED BY SIZE                           
206300                              INTO RESP-IDELMT-ERROR                      
206400                           MOVE 'OMR'                                     
206500                                  TO RESP-IDMSG-ERROR-LINE(INDX)          
206600                           MOVE NEJ TO INDATA-SW                          
206700                        END-IF                                            
206800                      END-IF                                              
206900                    END-IF                                                
207000                  END-IF                                                  
207100                END-IF                                                    
207200              END-IF                                                      
207300           END-IF                                                         
207400        END-IF                                                            
207500     END-IF                                                               
207600     .                                                                    
207700     EJECT                                                                
207800 GE-KOLLA-KVSKROT SECTION.                                                
207900                                                                          
208000     IF REQU-KVSKROT-IN(INDX) = ALL '+' OR SPACE OR                       
208100        LOW-VALUE                                                         
208200           CONTINUE                                                       
208300     ELSE                                                                 
208400        MOVE REQU-KVSKROT-IN(INDX) TO RESP-KVSKROT-IN(INDX)               
208500     END-IF                                                               
208600                                                                          
208700************                                                              
208800     MOVE ZERO TO W-KVSKROT (INDX)                                        
208900                                                                          
209000     IF REQU-KOLLI-KLART = 'X'                                            
209100     OR REQU-KOLLI-KLART = 'Y'                                            
209200        IF  REQU-KVSKROT-IN (INDX) NOT = ALL '+'                          
209300        AND REQU-KVSKROT-IN (INDX) NOT = SPACE                            
209400           MOVE NEJ TO INDATA-SW                                          
209500           MOVE INDX TO INDX-DISPLAY                                      
209600           MOVE SPACE TO RESP-IDELMT-ERROR                                
209700           STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE               
209800                  INTO RESP-IDELMT-ERROR                                  
209900           MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                      
210000        END-IF                                                            
210100     ELSE                                                                 
210200        IF  REQU-KVSKROT-IN (INDX) NOT = ALL '+'                          
210300        AND REQU-KVSKROT-IN (INDX) NOT = SPACE                            
210400        AND REQU-KVSKROT-IN (INDX) NOT = ZEROES                           
210500        AND REQU-KVSKROT-IN (INDX) NOT = LOW-VALUES                       
210600           IF REQU-CMD-IN (INDX) NOT = 'DAM'                              
210700              MOVE NEJ TO INDATA-SW                                       
210800              MOVE INDX TO INDX-DISPLAY                                   
210900              MOVE SPACE TO RESP-IDELMT-ERROR                             
211000              STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE            
211100                   INTO RESP-IDELMT-ERROR                                 
211200              MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                   
211300           ELSE                                                           
211400              PERFORM S35-KOLLA-IDUSER                                    
211500              MOVE REQU-KVSKROT-IN (INDX) TO WS-KVSKROT                   
211600              INSPECT WS-KVSKROT                                          
211700                      REPLACING LEADING SPACE BY ZERO                     
211800              IF WS-KVSKROT NOT NUMERIC                                   
211900                 MOVE NEJ TO INDATA-SW                                    
212000                 MOVE INDX TO INDX-DISPLAY                                
212100                 MOVE SPACE TO RESP-IDELMT-ERROR                          
212200                 STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE         
212300                      INTO RESP-IDELMT-ERROR                              
212400                 MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                
212500              ELSE                                                        
212600                 MOVE WS-KVSKROT TO W-KVSKROT (INDX)                      
212700                 IF W-KVSKROT (INDX) = ZERO                               
212800                    MOVE NEJ TO INDATA-SW                                 
212900                    MOVE INDX TO INDX-DISPLAY                             
213000                    MOVE SPACE TO RESP-IDELMT-ERROR                       
213100                    STRING 'KVSKROT*' INDX-DISPLAY                        
213200                      DELIMITED BY SIZE                                   
213300                      INTO RESP-IDELMT-ERROR                              
213400                    MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)             
213500                 END-IF                                                   
213600              END-IF                                                      
213700           END-IF                                                         
213800        ELSE                                                              
213900           IF REQU-CMD-IN (INDX) = 'DAM'                                  
214000              MOVE NEJ TO INDATA-SW                                       
214100              MOVE INDX TO INDX-DISPLAY                                   
214200              MOVE SPACE TO RESP-IDELMT-ERROR                             
214300              STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE            
214400                      INTO RESP-IDELMT-ERROR                              
214500              MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                   
214600           END-IF                                                         
214700        END-IF                                                            
214800     END-IF                                                               
214900                                                                          
215000     IF REQU-CMD-IN (INDX) = 'DAM'                                        
215100        IF  REQU-KVSKROT-IN (INDX) = ALL '+'                              
215200        OR  REQU-KVSKROT-IN (INDX) = SPACE                                
215300        OR  REQU-KVSKROT-IN (INDX) = ZEROES                               
215400            MOVE NEJ TO INDATA-SW                                         
215500            MOVE INDX TO INDX-DISPLAY                                     
215600            MOVE SPACE TO RESP-IDELMT-ERROR                               
215700            STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE              
215800                    INTO RESP-IDELMT-ERROR                                
215900            MOVE 'QTY' TO RESP-IDMSG-ERROR-LINE(INDX)                     
216000        END-IF                                                            
216100     END-IF                                                               
216200     .                                                                    
216300     EJECT                                                                
216400                                                                          
216500 GF-KOLLA-NY-ARTIKEL SECTION.                                             
216600     IF REQU-CMD-INM NOT = ALL '+'                                        
216700        MOVE REQU-CMD-INM TO RESP-CMD-INM                                 
216800     END-IF                                                               
216900     IF REQU-KVANTMOT-INM NOT = ALL '+'                                   
217000        MOVE REQU-KVANTMOT-INM TO RESP-KVANTMOT-INM                       
217100     END-IF                                                               
217200     IF REQU-IDARTNR-INM NOT = ALL '+'                                    
217300        MOVE REQU-IDARTNR-INM TO RESP-IDARTNR-INM                         
217400     END-IF                                                               
217500     IF REQU-ADLAGOMR-INM NOT = ALL '+'                                   
217600        MOVE REQU-ADLAGOMR-INM TO RESP-ADLAGOMR-INM                       
217700     END-IF                                                               
217800     IF REQU-ADGANG-INM NOT = ALL '+'                                     
217900        MOVE REQU-ADGANG-INM TO RESP-ADGANG-INM                           
218000     END-IF                                                               
218100     IF REQU-ADPLATS-INM NOT = ALL '+'                                    
218200        MOVE REQU-ADPLATS-INM TO RESP-ADPLATS-INM                         
218300     END-IF                                                               
218400     IF REQU-KVSKROT-INM NOT = ALL '+'                                    
218500        MOVE REQU-KVSKROT-INM TO RESP-KVSKROT-INM                         
218600     END-IF                                                               
218700                                                                          
218800     MOVE ZERO TO W-KVANTMOT-INM                                          
218900                  W-IDARTNR-INM                                           
219000     MOVE REQU-IDDC-KEY TO W-6301-IDDC                                    
219100     MOVE W-SPAR-IDFAKT TO W-IDFAKT                                       
219200                                                                          
219300     PERFORM IMS-GHU-WL630111-GE                                          
219400     IF SEGMENT-FINNS                                                     
219500        MOVE 6302-IDDC-SEND TO W-IDDC-B6-SEND                             
219600        MOVE 6302-IDDISTR   TO DIST35-IDDISTR                             
219700     ELSE                                                                 
219800        MOVE SPACE TO W-IDDC-B6-SEND                                      
219900     END-IF                                                               
220000     IF W-IDDC-B6-SEND NOT = SEND-DCS-IDDC                                
220100        PERFORM IMS-GU-WDB601-SEND                                        
220200     END-IF                                                               
220300                                                                          
220400     IF REQU-KOLLI-KLART = 'X'                                            
220500     OR REQU-KOLLI-KLART = 'Y'                                            
220600        IF REQU-IDARTNR-INM NOT = ALL '+'                                 
220700        AND REQU-IDARTNR-INM NOT = SPACE                                  
220800        AND REQU-IDARTNR-INM NOT = LOW-VALUE                              
220900           MOVE NEJ TO INDATA-SW                                          
221000           MOVE 'ART' TO RESP-IDELMT-ERROR                                
221100           MOVE 'IDARTNR' TO RESP-IDMSG-ERROR-INM                         
221200        END-IF                                                            
221300     ELSE                                                                 
221400        IF REQU-IDARTNR-INM NOT = ALL '+'                                 
221500        AND REQU-IDARTNR-INM NOT = SPACE                                  
221600        AND REQU-IDARTNR-INM NOT = LOW-VALUE                              
221610         IF NDC-BR                                                        
                 MOVE NEJ                 TO INDATA-SW                          
                 MOVE UPD-NOT-ALLOWED     TO RESP-IDMSG-INFO                    
222100           MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                            
222200           MOVE 'ART'     TO RESP-IDMSG-ERROR-INM                         
221620         ELSE                                                             
221700           MOVE REQU-IDARTNR-INM TO WS-IDARTNR                            
221800           INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO             
221900           IF WS-IDARTNR NOT NUMERIC                                      
222000              MOVE NEJ TO INDATA-SW                                       
222100              MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                         
222200              MOVE 'ART'     TO RESP-IDMSG-ERROR-INM                      
222300           ELSE                                                           
222400              MOVE WS-IDARTNR TO W-IDARTNR-INM                            
222500                                 W-IDARTNR                                
222600              IF W-IDARTNR-INM = 100                                      
222700                 MOVE NEJ TO INDATA-SW                                    
222800                 MOVE 'ART'     TO RESP-IDMSG-ERROR-INM                   
222900                 MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                      
223000              ELSE                                                        
223100                 MOVE REQU-IDDC-KEY TO W-IDDC                             
223200                 IF DCS-CDC                                               
223300                   PERFORM IMS-GET-WLARTC01                               
223400                   IF SEGMENT-SAKNAS                                      
223500                    MOVE NEJ TO INDATA-SW                                 
223600                    MOVE 'ART'     TO RESP-IDMSG-ERROR-INM                
223700                    MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                   
223800                   ELSE                                                   
223900                    MOVE JA TO NY-BEFINTLIG-ART-SW                        
224000                    PERFORM GFC-KOLLA-BEF-ART-CDC                         
224100                    PERFORM S35-KOLLA-IDUSER                              
224200                   END-IF                                                 
224300                 ELSE                                                     
224400                   PERFORM IMS-GHU-WDK701                                 
224500                   IF SEGMENT-SAKNAS                                      
224600                      MOVE JA TO NY-NYUPPLAEGG-ART-SW                     
224700                      PERFORM GFA-KOLLA-NYUPPLAGG                         
224800                      PERFORM S35-KOLLA-IDUSER                            
224900                   ELSE                                                   
225000                      PERFORM IMS-GNP-WDK711                              
225100                      IF SEGMENT-SAKNAS                                   
225200                        MOVE JA TO NY-NYUPPLAEGG-ART-SW                   
225300                        PERFORM GFA-KOLLA-NYUPPLAGG                       
225400                        PERFORM S35-KOLLA-IDUSER                          
225500                      ELSE                                                
225600                        MOVE JA TO NY-BEFINTLIG-ART-SW                    
225700                        PERFORM GFB-KOLLA-BEFINTLIG-ARTIKEL               
225800                        PERFORM S35-KOLLA-IDUSER                          
225900                      END-IF                                              
226000                   END-IF                                                 
226100                 END-IF                                                   
226200              END-IF                                                      
226300           END-IF                                                         
226400                                                                          
226500           IF INDATA-OK                                                   
226600              IF REQU-ADLAGOMR-INM NOT = ALL '+'                          
226700                 MOVE REQU-ADLAGOMR-INM TO WS-ADLAGOMR                    
226800                 INSPECT WS-ADLAGOMR REPLACING LEADING                    
226900                        SPACE BY ZERO                                     
227000                 IF WS-ADLAGOMR NOT NUMERIC                               
227100                 OR WS-ADLAGOMR NOT > ZERO                                
227200                    MOVE NEJ TO INDATA-SW                                 
227300                    MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                  
227400                    MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                
227500                 ELSE                                                     
227600                    MOVE WS-ADLAGOMR TO W-ADLAGOMR-INM                    
227700                 END-IF                                                   
227800               END-IF                                                     
227900               IF REQU-ADGANG-INM NOT = ALL '+'                           
228000                  MOVE REQU-ADGANG-INM TO WS-ADGANG                       
228100                  INSPECT WS-ADGANG REPLACING LEADING                     
228200                         SPACE BY ZERO                                    
228300                  IF WS-ADGANG NOT NUMERIC                                
228400                     MOVE NEJ TO INDATA-SW                                
228500                     MOVE 'ADGANG' TO RESP-IDELMT-ERROR                   
228600                     MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                
228700                  ELSE                                                    
228800                     MOVE WS-ADGANG TO W-ADGANG-INM                       
228900                  END-IF                                                  
229000               END-IF                                                     
229100               IF REQU-ADPLATS-INM NOT = ALL '+'                          
229200                  MOVE REQU-ADPLATS-INM TO WS-ADPLATS                     
229300                  INSPECT WS-ADPLATS REPLACING LEADING                    
229400                         SPACE BY ZERO                                    
229500                  IF WS-ADPLATS NOT NUMERIC                               
229600                     MOVE NEJ TO INDATA-SW                                
229700                     MOVE 'ADPLATS' TO RESP-IDELMT-ERROR                  
229800                     MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                
229900                  ELSE                                                    
230000                     MOVE WS-ADPLATS TO W-ADPLATS-INM                     
230100                  END-IF                                                  
230200              END-IF                                                      
230300           END-IF                                                         
230400           IF INDATA-OK                                                   
230500**            IF REQU-CMD-INM = ALL '+' OR SPACE                          
230600               IF REQU-KVANTMOT-INM NOT = ALL '+' OR SPACE                
230700                IF WS-FLTRACK = 'J'                                       
230800                 PERFORM GFD-KOLLA-IDTRACK                                
230900                END-IF                                                    
231000               END-IF                                                     
231100**            END-IF                                                      
231200           END-IF                                                         
231300         END-IF                                                           
231310        END-IF                                                            
231400     END-IF                                                               
231500     .                                                                    
231600     EJECT                                                                
231700 GFA-KOLLA-NYUPPLAGG SECTION.                                             
231800                                                                          
231900     MOVE ZERO TO WS-KVANTMOT-INM-NUM                                     
232000                  WS-KVSKROT-INM-NUM                                      
232100                  WS-KVANTMOT-INM                                         
232200                  WS-KVSKROT-INM                                          
232300                                                                          
232400     PERFORM IMS-GET-WLARTC01                                             
232500     IF SEGMENT-FINNS                                                     
232600        MOVE K6-ART-KDSORT TO WS-KDSORT                                   
232700        MOVE K6-ART-KDPRODSL TO TEST-KDPRODSL                             
232800        IF KDPRODSL-VOLVO-BIMA                                            
232900           CONTINUE                                                       
233000        ELSE                                                              
233100           MOVE NEJ TO INDATA-SW                                          
233200           MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                         
233300           MOVE 'ART'        TO RESP-IDMSG-ERROR-INM                      
233400        END-IF                                                            
233500        IF K6-ART-KDERS-UTG = +0                                          
233600           IF (REQU-ADLAGOMR-INM = ALL '+')                               
233700           OR (REQU-ADGANG-INM = ALL '+')                                 
233800           OR (REQU-ADPLATS-INM = ALL '+')                                
233900              MOVE NEJ TO INDATA-SW                                       
234000              MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                        
234100              MOVE 'OMR'    TO RESP-IDMSG-ERROR-INM                       
234200           END-IF                                                         
234300                                                                          
234400           IF ((REQU-KVANTMOT-INM = ALL '+') OR                           
234500              (REQU-KVANTMOT-INM = SPACE))                                
234600           AND ((REQU-KVSKROT-INM = ALL '+') OR                           
234700              (REQU-KVSKROT-INM = SPACE))                                 
234800                 MOVE NEJ TO INDATA-SW                                    
234900                 MOVE 'KVANTMOT' TO RESP-IDELMT-ERROR                     
235000                 MOVE 'QTY'    TO RESP-IDMSG-ERROR-INM                    
235100           ELSE                                                           
235200              IF REQU-KVANTMOT-INM NOT = ALL '+'                          
235300                 MOVE REQU-KVANTMOT-INM TO WS-KVANTMOT-INM                
235400                 INSPECT REQU-KVANTMOT-INM REPLACING ALL                  
235500                         SPACE BY ZERO                                    
235600                 IF WS-KVANTMOT-INM NOT NUMERIC                           
235700                    MOVE NEJ TO INDATA-SW                                 
235800                    MOVE 'KVANTMOT' TO RESP-IDELMT-ERROR                  
235900                    MOVE 'QTY'    TO RESP-IDMSG-ERROR-INM                 
236000                 ELSE                                                     
236100                    IF WS-KVANTMOT-INM > +0                               
236200                       MOVE JA TO NY-KVANTMOT-SW                          
236300                       MOVE WS-KVANTMOT-INM TO                            
236400                           WS-KVANTMOT-INM-NUM                            
236500                    END-IF                                                
236600                 END-IF                                                   
236700                                                                          
236800                 IF REQU-KVSKROT-INM NOT = ALL '+'                        
236900                    MOVE REQU-KVSKROT-INM TO WS-KVSKROT-INM               
237000                    INSPECT WS-KVSKROT-INM REPLACING ALL                  
237100                              SPACE BY ZERO                               
237200                    IF WS-KVSKROT-INM NOT NUMERIC                         
237300                       MOVE NEJ TO INDATA-SW                              
237400                       MOVE 'KVSKROT' TO RESP-IDELMT-ERROR                
237500                       MOVE 'QTY'    TO RESP-IDMSG-ERROR-INM              
237600                    ELSE                                                  
237700                       IF WS-KVSKROT-INM > +0                             
237800                          IF REQU-CMD-INM NOT = 'DAM'                     
237900                             MOVE NEJ TO INDATA-SW                        
238000                             MOVE 'CMD' TO RESP-IDELMT-ERROR              
238100                                           RESP-IDMSG-ERROR-INM           
238200                          ELSE                                            
238300                             MOVE JA TO NY-SKROT-SW                       
238400                             MOVE WS-KVSKROT-INM TO                       
238500                                   WS-KVSKROT-INM-NUM                     
238600                          END-IF                                          
238700                       ELSE                                               
238800                          MOVE NEJ TO INDATA-SW                           
238900                          MOVE 'KVSKROT' TO RESP-IDELMT-ERROR             
239000                          MOVE 'QTY'    TO RESP-IDMSG-ERROR-INM           
239100                       END-IF                                             
239200                    END-IF                                                
239300                 END-IF                                                   
239400                                                                          
239500                 IF REQU-CMD-INM = ALL '+' OR SPACE                       
239600                    CONTINUE                                              
239700                 ELSE                                                     
239800                    IF REQU-CMD-INM NOT = 'DAM'                           
239900                       MOVE NEJ TO INDATA-SW                              
240000                       MOVE 'CMD' TO RESP-IDELMT-ERROR                    
240100                                     RESP-IDMSG-ERROR-INM                 
240200                    END-IF                                                
240300                 END-IF                                                   
240400                                                                          
240500                 COMPUTE WS-SUMMA-KVANT = WS-KVANTMOT-INM-NUM             
240600                    + WS-KVSKROT-INM-NUM                                  
240700                 END-COMPUTE                                              
240800                 IF WS-SUMMA-KVANT = +0                                   
240900                    MOVE NEJ TO INDATA-SW                                 
241000                    MOVE 'KVSKROT' TO RESP-IDELMT-ERROR                   
241100                    MOVE 'CMD' TO RESP-IDELMT-ERROR                       
241200                    MOVE 'CMD'    TO RESP-IDMSG-ERROR-INM                 
241300                 END-IF                                                   
241400              END-IF                                                      
241500           END-IF                                                         
241600        ELSE                                                              
241700           MOVE NEJ TO INDATA-SW                                          
241800           MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                            
241900           MOVE 'ART'    TO RESP-IDMSG-ERROR-INM                          
242000        END-IF                                                            
242100     ELSE                                                                 
242200        MOVE NEJ TO INDATA-SW                                             
242300        MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                               
242400        MOVE 'ART'    TO RESP-IDMSG-ERROR-INM                             
242500     END-IF                                                               
242600     .                                                                    
242700     EJECT                                                                
242800 GFB-KOLLA-BEFINTLIG-ARTIKEL SECTION.                                     
242900                                                                          
243000     MOVE ZERO TO WS-KVANTMOT-INM-NUM                                     
243100                  WS-KVSKROT-INM-NUM                                      
243200                  WS-KVANTMOT-INM                                         
243300                  WS-KVSKROT-INM                                          
243400                                                                          
243500     PERFORM IMS-GET-WLARTC01                                             
243600     MOVE K6-ART-KDSORT TO WS-KDSORT                                      
243700     IF SEGMENT-FINNS                                                     
243800        MOVE K6-ART-KDPRODSL TO TEST-KDPRODSL                             
243900        IF KDPRODSL-VOLVO-BIMA                                            
244000           CONTINUE                                                       
244100        ELSE                                                              
244200           MOVE NEJ          TO INDATA-SW                                 
244300           MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                         
244400           MOVE 'ART'        TO RESP-IDMSG-ERROR-INM                      
244500        END-IF                                                            
244600     END-IF                                                               
244700                                                                          
244800     IF REQU-KVANTMOT-INM NOT = ALL '+'                                   
244900     AND REQU-KVANTMOT-INM NOT = SPACE                                    
245000        MOVE REQU-KVANTMOT-INM TO WS-KVANTMOT-INM                         
245100        INSPECT WS-KVANTMOT-INM REPLACING LEADING SPACE BY ZERO           
245200        IF WS-KVANTMOT-INM NOT NUMERIC                                    
245300           MOVE NEJ TO INDATA-SW                                          
245400           MOVE 'KVANTMOT' TO RESP-IDELMT-ERROR                           
245500           MOVE 'QTY'     TO RESP-IDMSG-ERROR-INM                         
245600        ELSE                                                              
245700           MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                    
245800        END-IF                                                            
245900     END-IF                                                               
246000                                                                          
246100     IF (REQU-ADLAGOMR-INM NOT = ALL '+')                                 
246200     OR (REQU-ADGANG-INM NOT = ALL '+')                                   
246300     OR (REQU-ADPLATS-INM NOT = ALL '+')                                  
246400        IF SLAG-ADLAGOMR NOT = ZERO                                       
246500           MOVE NEJ TO INDATA-SW                                          
246600           MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                           
246700           MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                         
246800        END-IF                                                            
246900     ELSE                                                                 
247000        IF SLAG-ADLAGOMR = ZERO                                           
247100           MOVE NEJ TO INDATA-SW                                          
247200           MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                           
247300           MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                         
247400        END-IF                                                            
247500     END-IF                                                               
247600                                                                          
247700     IF (REQU-ADLAGOMR-INM = ALL '+' OR SPACE)                            
247800     AND (REQU-ADGANG-INM  = ALL '+' OR SPACE)                            
247900     AND (REQU-ADPLATS-INM = ALL '+' OR SPACE)                            
248000         IF SLAG-ADLAGOMR  = ZERO                                         
248100         AND SLAG-ADGANG   = ZERO                                         
248200         AND SLAG-ADPLATS  = ZERO                                         
248300           MOVE NEJ TO INDATA-SW                                          
248400           MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                           
248500           MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                         
248600        END-IF                                                            
248700     ELSE                                                                 
248800        IF SLAG-ADLAGOMR  = ZERO                                          
248900        AND SLAG-ADGANG   = ZERO                                          
249000        AND SLAG-ADPLATS  = ZERO                                          
249100            IF REQU-ADLAGOMR-INM = ALL '+' OR SPACE                       
249200               MOVE NEJ TO INDATA-SW                                      
249300               MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                       
249400               MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                     
249500            END-IF                                                        
249600            IF REQU-ADGANG-INM = ALL '+' OR SPACE                         
249700               MOVE NEJ TO INDATA-SW                                      
249800               MOVE 'ADGANG' TO RESP-IDELMT-ERROR                         
249900               MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                      
250000            END-IF                                                        
250100            IF REQU-ADPLATS-INM = ALL '+' OR SPACE                        
250200               MOVE NEJ TO INDATA-SW                                      
250300               MOVE 'ADPLATS' TO RESP-IDELMT-ERROR                        
250400               MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                      
250500            END-IF                                                        
250600        ELSE                                                              
250700           MOVE NEJ TO INDATA-SW                                          
250800           MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                          
250900           MOVE 'ADGANG' TO RESP-IDELMT-ERROR                             
251000        END-IF                                                            
251100     END-IF                                                               
251200                                                                          
251300     IF REQU-KVSKROT-INM NOT = ALL '+'                                    
251400     AND REQU-KVSKROT-INM NOT = ALL SPACE                                 
251500        MOVE REQU-KVSKROT-INM TO WS-KVSKROT-INM                           
251600        INSPECT WS-KVSKROT-INM REPLACING LEADING SPACE BY ZERO            
251700        IF WS-KVSKROT-INM NOT NUMERIC                                     
251800           MOVE NEJ TO INDATA-SW                                          
251900           MOVE 'KVSKROT' TO RESP-IDELMT-ERROR                            
252000           MOVE 'QTY'    TO RESP-IDMSG-ERROR-INM                          
252100        ELSE                                                              
252200           IF REQU-CMD-INM NOT = 'DAM'                                    
252300              MOVE NEJ TO INDATA-SW                                       
252400              MOVE 'CMD' TO RESP-IDELMT-ERROR                             
252500                            RESP-IDMSG-ERROR-INM                          
252600           ELSE                                                           
252700              MOVE WS-KVSKROT-INM TO WS-KVSKROT-INM-NUM                   
252800              MOVE JA TO NY-SKROT-SW                                      
252900           END-IF                                                         
253000        END-IF                                                            
253100     END-IF                                                               
253200                                                                          
253300     IF REQU-CMD-INM = ALL '+' OR SPACE                                   
253400        CONTINUE                                                          
253500     ELSE                                                                 
253600        IF REQU-CMD-INM NOT = 'DAM'                                       
253700           MOVE NEJ TO INDATA-SW                                          
253800           MOVE 'CMD' TO RESP-IDELMT-ERROR                                
253900                         RESP-IDMSG-ERROR-INM                             
254000        END-IF                                                            
254100     END-IF                                                               
254200                                                                          
254300     COMPUTE WS-SUMMA-KVANT = (WS-KVSKROT-INM-NUM +                       
254400         WS-KVANTMOT-INM-NUM)                                             
254500     END-COMPUTE                                                          
254600     IF WS-SUMMA-KVANT = ZERO                                             
254700        MOVE NEJ TO INDATA-SW                                             
254800        MOVE 'KVANTMOT' TO RESP-IDELMT-ERROR                              
254900        MOVE 'QTY'      TO   RESP-IDMSG-ERROR-INM                         
255000     END-IF                                                               
255100                                                                          
255200     IF INDATA-OK                                                         
255300         IF WS-KVSKROT-INM-NUM > +0                                       
255400            MOVE JA TO NY-SKROT-SW                                        
255500         END-IF                                                           
255600         IF WS-KVANTMOT-INM-NUM > +0                                      
255700            MOVE JA TO NY-KVANTMOT-SW                                     
255800         END-IF                                                           
255900     END-IF                                                               
256000     .                                                                    
256100     EJECT                                                                
256200 GFC-KOLLA-BEF-ART-CDC SECTION.                                           
256300                                                                          
256400     MOVE ZERO TO WS-KVANTMOT-INM-NUM                                     
256500                  WS-KVSKROT-INM-NUM                                      
256600                  WS-KVANTMOT-INM                                         
256700                  WS-KVSKROT-INM                                          
256800                                                                          
256900     PERFORM IMS-GET-WLARTC01                                             
257000     MOVE K6-ART-KDSORT TO WS-KDSORT                                      
257100     IF SEGMENT-FINNS                                                     
257200        MOVE K6-ART-KDPRODSL TO TEST-KDPRODSL                             
257300        IF KDPRODSL-VOLVO-BIMA                                            
257400           CONTINUE                                                       
257500        ELSE                                                              
257600           MOVE NEJ          TO INDATA-SW                                 
257700           MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                         
257800           MOVE 'ART'        TO RESP-IDMSG-ERROR-INM                      
257900        END-IF                                                            
258000     END-IF                                                               
258100                                                                          
258200     PERFORM IMS-GNP-WLARTC11                                             
258300                                                                          
258400     IF REQU-KVANTMOT-INM NOT = ALL '+'                                   
258500     AND REQU-KVANTMOT-INM NOT = SPACE                                    
258600        MOVE REQU-KVANTMOT-INM TO WS-KVANTMOT-INM                         
258700        INSPECT WS-KVANTMOT-INM REPLACING LEADING SPACE BY ZERO           
258800        IF WS-KVANTMOT-INM NOT NUMERIC                                    
258900           MOVE NEJ TO INDATA-SW                                          
259000           MOVE 'KVANTMOT' TO RESP-IDELMT-ERROR                           
259100           MOVE 'QTY'     TO RESP-IDMSG-ERROR-INM                         
259200        ELSE                                                              
259300           MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                    
259400        END-IF                                                            
259500     END-IF                                                               
259600                                                                          
259700     IF (REQU-ADLAGOMR-INM NOT = ALL '+')                                 
259800     OR (REQU-ADGANG-INM NOT = ALL '+')                                   
259900     OR (REQU-ADPLATS-INM NOT = ALL '+')                                  
260000        IF CLAG-ADLAGOMR NOT = ZERO                                       
260100           MOVE NEJ TO INDATA-SW                                          
260200           MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                           
260300           MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                         
260400        END-IF                                                            
260500     ELSE                                                                 
260600        IF CLAG-ADLAGOMR = ZERO                                           
260700           MOVE NEJ TO INDATA-SW                                          
260800           MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                           
260900           MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                         
261000        END-IF                                                            
261100     END-IF                                                               
261200                                                                          
261300     IF (REQU-ADLAGOMR-INM = ALL '+' OR SPACE)                            
261400     AND (REQU-ADGANG-INM  = ALL '+' OR SPACE)                            
261500     AND (REQU-ADPLATS-INM = ALL '+' OR SPACE)                            
261600         IF CLAG-ADLAGOMR  = ZERO                                         
261700         AND CLAG-ADGANG   = ZERO                                         
261800         AND CLAG-ADPLATS  = ZERO                                         
261900           MOVE NEJ TO INDATA-SW                                          
262000           MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                           
262100           MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                         
262200        END-IF                                                            
262300     ELSE                                                                 
262400        IF CLAG-ADLAGOMR  = ZERO                                          
262500        AND CLAG-ADGANG   = ZERO                                          
262600        AND CLAG-ADPLATS  = ZERO                                          
262700            IF REQU-ADLAGOMR-INM = ALL '+' OR SPACE                       
262800               MOVE NEJ TO INDATA-SW                                      
262900               MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                       
263000               MOVE 'OMR'     TO RESP-IDMSG-ERROR-INM                     
263100            END-IF                                                        
263200            IF REQU-ADGANG-INM = ALL '+' OR SPACE                         
263300               MOVE NEJ TO INDATA-SW                                      
263400               MOVE 'ADGANG' TO RESP-IDELMT-ERROR                         
263500               MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                      
263600            END-IF                                                        
263700            IF REQU-ADPLATS-INM = ALL '+' OR SPACE                        
263800               MOVE NEJ TO INDATA-SW                                      
263900               MOVE 'ADPLATS' TO RESP-IDELMT-ERROR                        
264000               MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                      
264100            END-IF                                                        
264200        ELSE                                                              
264300           MOVE NEJ TO INDATA-SW                                          
264400           MOVE 'PL'     TO RESP-IDMSG-ERROR-INM                          
264500           MOVE 'ADGANG' TO RESP-IDELMT-ERROR                             
264600        END-IF                                                            
264700     END-IF                                                               
264800                                                                          
264900     IF REQU-KVSKROT-INM NOT = ALL '+'                                    
265000     AND REQU-KVSKROT-INM NOT = ALL SPACE                                 
265100        MOVE REQU-KVSKROT-INM TO WS-KVSKROT-INM                           
265200        INSPECT WS-KVSKROT-INM REPLACING LEADING SPACE BY ZERO            
265300        IF WS-KVSKROT-INM NOT NUMERIC                                     
265400           MOVE NEJ TO INDATA-SW                                          
265500           MOVE 'KVSKROT' TO RESP-IDELMT-ERROR                            
265600           MOVE 'QTY'    TO RESP-IDMSG-ERROR-INM                          
265700        ELSE                                                              
265800           IF REQU-CMD-INM NOT = 'DAM'                                    
265900              MOVE NEJ TO INDATA-SW                                       
266000              MOVE 'CMD' TO RESP-IDELMT-ERROR                             
266100                            RESP-IDMSG-ERROR-INM                          
266200           ELSE                                                           
266300              MOVE WS-KVSKROT-INM TO WS-KVSKROT-INM-NUM                   
266400              MOVE JA TO NY-SKROT-SW                                      
266500           END-IF                                                         
266600        END-IF                                                            
266700     END-IF                                                               
266800                                                                          
266900     IF REQU-CMD-INM = ALL '+' OR SPACE                                   
267000        CONTINUE                                                          
267100     ELSE                                                                 
267200        IF REQU-CMD-INM NOT = 'DAM'                                       
267300           MOVE NEJ TO INDATA-SW                                          
267400           MOVE 'CMD' TO RESP-IDELMT-ERROR                                
267500                         RESP-IDMSG-ERROR-INM                             
267600        END-IF                                                            
267700     END-IF                                                               
267800                                                                          
267900     COMPUTE WS-SUMMA-KVANT = (WS-KVSKROT-INM-NUM +                       
268000         WS-KVANTMOT-INM-NUM)                                             
268100     END-COMPUTE                                                          
268200     IF WS-SUMMA-KVANT = ZERO                                             
268300        MOVE NEJ TO INDATA-SW                                             
268400        MOVE 'KVANTMOT' TO RESP-IDELMT-ERROR                              
268500        MOVE 'QTY'      TO   RESP-IDMSG-ERROR-INM                         
268600     END-IF                                                               
268700                                                                          
268800     IF INDATA-OK                                                         
268900         IF WS-KVSKROT-INM-NUM > +0                                       
269000            MOVE JA TO NY-SKROT-SW                                        
269100         END-IF                                                           
269200         IF WS-KVANTMOT-INM-NUM > +0                                      
269300            MOVE JA TO NY-KVANTMOT-SW                                     
269400         END-IF                                                           
269500     END-IF                                                               
269600     .                                                                    
269700     EJECT                                                                
269800 GFD-KOLLA-IDTRACK SECTION.                                               
269900                                                                          
270000     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
270100     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
270200     MOVE 000000000       TO W-SEQA-IDARTNR-MIN                           
270300     MOVE 999999999       TO W-SEQA-IDARTNR-MAX                           
270400     MOVE 0000000000000000 TO W-SEQA-DAINLEV-MAX                          
270500     MOVE 9999999999999999 TO W-SEQA-DAINLEV-MIN                          
270600     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
270700                             W-SEQA-IDFAKT-MAX                            
270800     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
270900                             W-SEQA-IDKUNDRF-MAX                          
271000     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
271100                             W-SEQA-IDKUNDNR-MAX                          
271200     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
271300                             W-SEQA-IDKOLLI-MAX                           
271400     MOVE 'N' TO WS-IDTRACK-OK                                            
271500     PERFORM IMS-GU-WLINLD01-FIRST                                        
271600                                                                          
271700     PERFORM UNTIL SEGMENT-SAKNAS OR WS-IDTRACK-OK = 'J'                  
271800       MOVE SEQA-IDARTNR  TO W-IDARTNR                                    
271900       MOVE SEQA-IDDC     TO W-IDDC                                       
272000       MOVE SEQA-DAINLEV  TO W-DAINLEV                                    
272100       PERFORM IMS-GU-WDL623                                              
272200       IF SEGMENT-FINNS                                                   
272300          MOVE 'J' TO WS-IDTRACK-OK                                       
272400          MOVE TINL-IDTRACK TO W-IDTRACK                                  
272500       END-IF                                                             
272600                                                                          
272700       PERFORM IMS-GN-WLINLD01-FIRST                                      
272800     END-PERFORM                                                          
272900                                                                          
273000     IF SEGMENT-SAKNAS AND WS-IDTRACK-OK = 'N'                            
273100        MOVE NEJ TO IDTRACK-SW                                            
273200                    INDATA-SW                                             
273300        MOVE 'IDTRACK' TO RESP-IDELMT-ERROR                               
273400        MOVE 'BIN'     TO RESP-IDMSG-ERROR-INM                            
273500        MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                      
273600     END-IF                                                               
273700                                                                          
273800     .                                                                    
273900     EJECT                                                                
274000 GI-KOLLA-BIN SECTION.                                                    
274100** THE SAME PARA USED FOR BIN AND ALSO HAC ONLY FOR TURKEY                
274200                                                                          
274300     MOVE X-REQU-KVRADER TO MAX-INDX                                      
274400     MOVE X-REQU-IDUSER-003 TO WS-IDUSER-003                              
274500     MOVE +1 TO INDX                                                      
274600                TAB-IX                                                    
274700     PERFORM UNTIL INDX > MAX-INDX                                        
274800        MOVE ZERO TO W-ADLAGOMR(INDX)                                     
274900                     W-ADGANG(INDX)                                       
275000                     W-ADPLATS(INDX)                                      
275100        IF  (X-REQU-IDKUNDRF(INDX) = ALL '+' OR LOW-VALUE)                
275200        AND (X-REQU-IDKUNDNR(INDX) = ALL '+' OR LOW-VALUE)                
275300        AND (X-REQU-IDKOLLI (INDX) = ALL '+' OR LOW-VALUE)                
275400            CONTINUE                                                      
275500        ELSE                                                              
275600           INSPECT X-REQU-IDFAKT-KEY REPLACING                            
275700                   LEADING SPACE BY ZERO                                  
275800           MOVE X-REQU-IDFAKT-KEY    TO BIN-TAB-IDFAKT(TAB-IX)            
275900           MOVE BIN-TAB-IDFAKT(1) TO W-SPAR-IDFAKT                        
276000           MOVE X-REQU-IDKUNDRF(INDX)                                     
276100                               TO BIN-TAB-IDKUNDRF(TAB-IX)                
276200           INSPECT X-REQU-IDKUNDNR(INDX) REPLACING                        
276300                   LEADING SPACE BY ZERO                                  
276400           MOVE X-REQU-IDKUNDNR(INDX) TO BIN-TAB-IDKUNDNR(TAB-IX)         
276500           INSPECT X-REQU-IDKOLLI(INDX) REPLACING                         
276600                   LEADING SPACE BY ZERO                                  
276700           MOVE X-REQU-IDKOLLI(INDX)                                      
276800                                TO BIN-TAB-IDKOLLI(TAB-IX)                
276900           MOVE X-REQU-CMD-IN(INDX)                                       
277000                                TO BIN-TAB-CMD-IN(TAB-IX)                 
277100                                                                          
277200           IF  BIN-TAB-IDFAKT(INDX) NUMERIC                               
277300           AND BIN-TAB-IDKUNDNR(INDX) NUMERIC                             
277400           AND BIN-TAB-IDKOLLI(INDX) NUMERIC                              
277500              PERFORM GIA-KOLLA-VIDARE-BIN                                
277600              ADD +1 TO TAB-IX                                            
277700           ELSE                                                           
277800              MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                     
277900              CALL FELLOG                                                 
278000           END-IF                                                         
278100        END-IF                                                            
278200        ADD +1 TO INDX                                                    
278300     END-PERFORM                                                          
278400     .                                                                    
278500     EJECT                                                                
278600 GIA-KOLLA-VIDARE-BIN SECTION.                                            
278700                                                                          
278800     MOVE LOW-VALUE                TO W-WDL6A1KY-MIN                      
278900     MOVE HIGH-VALUE               TO W-WDL6A1KY-MAX                      
279000     MOVE BIN-TAB-IDFAKT(TAB-IX)   TO W-SEQA-IDFAKT-MIN                   
279100                                      W-SEQA-IDFAKT-MAX                   
279200     MOVE BIN-TAB-IDKUNDRF(TAB-IX) TO W-SEQA-IDKUNDRF-MIN                 
279300                                      W-SEQA-IDKUNDRF-MAX                 
279400     MOVE BIN-TAB-IDKUNDNR(TAB-IX) TO W-SEQA-IDKUNDNR-MIN                 
279500                                      W-SEQA-IDKUNDNR-MAX                 
279600     MOVE BIN-TAB-IDKOLLI(TAB-IX)  TO W-SEQA-IDKOLLI-MIN                  
279700                                      W-SEQA-IDKOLLI-MAX                  
279800     MOVE '310'                    TO W-IDPTYP                            
279900                                                                          
280000     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
280100     IF SEGMENT-SAKNAS                                                    
280200        MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                           
280300        CALL FELLOG                                                       
280400     ELSE                                                                 
280500        PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                        
280600           MOVE SEQA-IDARTNR TO W-IDARTNR                                 
280700           MOVE SEQA-IDDC    TO W-IDDC                                    
280800           MOVE SEQA-DAINLEV TO W-DAINLEV                                 
280900                                                                          
281000           IF WS-FLTRACK = 'J'                                            
281100              PERFORM IMS-GU-WDL623                                       
281200              IF SEGMENT-FINNS                                            
281300                 MOVE TINL-IDTRACK TO W-IDTRACK                           
281400              ELSE                                                        
281500                 MOVE NEJ TO IDTRACK-SW                                   
281600                             INDATA-SW                                    
281700                 MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR             
281800              END-IF                                                      
281900           END-IF                                                         
282000                                                                          
282100           IF SEQA-IDDC NOT =  REQU-IDDC-KEY                              
282200              MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                     
282300              CALL FELLOG                                                 
282400           END-IF                                                         
282500                                                                          
282600           IF DCS-CDC                                                     
282700             PERFORM IMS-GET-WLARTC11                                     
282800             IF CLAG-ADLAGOMR  = ZERO                                     
282900              MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                     
283000              CALL FELLOG                                                 
283100             END-IF                                                       
283200           ELSE                                                           
283300             PERFORM IMS-GET-WDK711                                       
283400             IF SLAG-ADLAGOMR  = ZERO                                     
283500              MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                     
283600              CALL FELLOG                                                 
283700             END-IF                                                       
283800           END-IF                                                         
283900           PERFORM IMS-GN-WLINLD01                                        
284000        END-PERFORM                                                       
284100     END-IF                                                               
284200     .                                                                    
284300     EJECT                                                                
284400 GG-KOLLA-IDTRACK SECTION.                                                
284500                                                                          
284600     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
284700     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
284800     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
284900                             W-SEQA-IDFAKT-MAX                            
285000     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
285100                             W-SEQA-IDKUNDRF-MAX                          
285200     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
285300                             W-SEQA-IDKUNDNR-MAX                          
285400     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
285500                             W-SEQA-IDKOLLI-MAX                           
285600     IF REQU-IDARTNR(INDX) NOT = ALL '+'                                  
285700        MOVE REQU-IDARTNR(INDX) TO W-SEQA-IDARTNR-MIN                     
285800                                   W-SEQA-IDARTNR-MAX                     
285900     END-IF                                                               
286000     MOVE '310'           TO W-IDPTYP                                     
286100     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
286200                                                                          
286300     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
286400       MOVE SEQA-IDARTNR  TO W-IDARTNR                                    
286500       MOVE SEQA-IDDC     TO W-IDDC                                       
286600       MOVE SEQA-DAINLEV  TO W-DAINLEV                                    
286700       PERFORM IMS-GU-WDL623                                              
286800       IF SEGMENT-SAKNAS                                                  
286900          MOVE NEJ TO IDTRACK-SW                                          
287000                      INDATA-SW                                           
287100          MOVE INDX TO INDX-DISPLAY                                       
287200          IF REQU-IDARTNR(INDX) NOT = ALL '+'                             
287300             MOVE 'BIN' TO RESP-IDMSG-ERROR-LINE(INDX)                    
287400          END-IF                                                          
287500          MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                    
287600       END-IF                                                             
287700                                                                          
287800       PERFORM IMS-GN-WLINLD01                                            
287900     END-PERFORM                                                          
288000     .                                                                    
288100     EJECT                                                                
288200 H-UPPDATERA SECTION.                                                     
288300                                                                          
288400     MOVE REQU-IDDC-KEY  TO W-6301-IDDC                                   
288500                           WS-IDDC                                        
288600     MOVE W-SPAR-IDFAKT  TO W-IDFAKT                                      
288700     PERFORM IMS-GHU-WL630111                                             
288800     MOVE 6302-IDDISTR   TO DIST35-IDDISTR                                
288900     MOVE 6302-IDDC-SEND TO W-IDDC-B6-SEND                                
289000                            SEND-WS-IDDC                                  
289100     IF W-IDDC-B6-SEND NOT = SEND-DCS-IDDC                                
289200        PERFORM IMS-GU-WDB601-SEND                                        
289300     END-IF                                                               
289400     MOVE NEJ            TO NYUPPLAEGG-SW                                 
289500                            WS-A03-SKAPAD                                 
289600                                                                          
289700     IF REQU-KDPGMACT = 'X'                                               
289800        MOVE NEJ TO WS-A03-SKAPAD                                         
289900        PERFORM K-UPPDATERA-X-TRANS                                       
290000     ELSE                                                                 
290100        IF REQU-KOLLI-KLART   = 'X'                                       
290200        OR REQU-KOLLI-KLART   = 'Y'                                       
290300* NOT VCCS                                                                
290400           IF XDC-NON-VCC-OWNED                                           
290500**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
290600**** REST OF FLOWS TO US IS IN LAB.                                       
290700**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
290800**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
290900           OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                      
291000              PERFORM HA-UPPDATERA-KOLLI-KLART                            
291100           ELSE                                                           
291200* VCCS                                                                    
291300              MOVE NEJ TO WS-A03-SKAPAD                                   
291400              PERFORM HA-UPPDATERA-KOLLI-KLART-PV                         
291500           END-IF                                                         
291600           MOVE UPDATE-DONE TO RESP-IDMSG-INFO                            
291700        ELSE                                                              
291800           MOVE +1 TO INDX                                                
291900           MOVE NEJ             TO WS-A03-SKAPAD                          
292000           IF SUB-KDTRANS(1:6) = 'WLA103'                                 
292100             MOVE W-SPAR-REQU-IDKUNDRF(INDX) TO W-SPAR-IDKUNDRF           
292200             MOVE W-SPAR-REQU-IDKUNDNR(INDX) TO W-SPAR-IDKUNDNR           
292300             MOVE W-SPAR-REQU-IDKOLLI(INDX)  TO W-SPAR-IDKOLLI            
292400             MOVE W-SPAR-REQU-IDFAKT(INDX)   TO W-SPAR-IDFAKT             
292500           END-IF                                                         
292600           PERFORM UNTIL INDX > MAX-INDX                                  
292700             IF REQU-CMD-IN(INDX) = 'HAC'                                 
292800               MOVE REQU-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN             
292900                                           W-SEQA-IDARTNR-MAX             
293000               PERFORM HA-UPPDATERA-RAD-HAC                               
293100               MOVE UPDATE-DONE TO RESP-IDMSG-INFO                        
293200             ELSE                                                         
293300               IF REQU-CMD-IN(INDX) = 'DAM'                               
293400* NOT VCCS                                                                
293500                 IF XDC-NON-VCC-OWNED                                     
293600**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
293700**** REST OF FLOWS TO US IS IN LAB.                                       
293800**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
293900**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
294000                 OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                
294100                   PERFORM HB-KOLLI-DAMAGED                               
294200                 ELSE                                                     
294300* VCCS                                                                    
294400                   PERFORM HB-KOLLI-DAMAGED-PV                            
294500                 END-IF                                                   
294600                 MOVE UPDATE-DONE TO RESP-IDMSG-INFO                      
294700               ELSE                                                       
294800                 IF REQU-KVANTMOT-IN (INDX) NOT = ALL '+'                 
294900                 AND REQU-KVANTMOT-IN (INDX) NOT = SPACE                  
295000* NOT VCCS                                                                
295100                   IF XDC-NON-VCC-OWNED                                   
295200**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
295300**** REST OF FLOWS TO US IS IN LAB.                                       
295400**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
295500**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
295600                   OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)              
295700                     PERFORM HC-OOVER-UNDER-LEVERANS                      
295800                   ELSE                                                   
295900* VCCS                                                                    
296000                     PERFORM HC-OOVER-UNDER-LEVERANS-PV                   
296100                   END-IF                                                 
296200                   MOVE UPDATE-DONE TO RESP-IDMSG-INFO                    
296300                 ELSE                                                     
296400                   IF REQU-CMD-IN(INDX) = 'ADR'                           
296500                     PERFORM HF-ENDAST-LAGERPLATS                         
296600                     MOVE UPDATE-DONE TO RESP-IDMSG-INFO                  
296700                   END-IF                                                 
296800                 END-IF                                                   
296900               END-IF                                                     
297000             END-IF                                                       
297100             ADD 1 TO INDX                                                
297200             IF SUB-KDTRANS(1:6) = 'WLA103'                               
297300               IF INDX > MAX-INDX                                         
297400                CONTINUE                                                  
297500               ELSE                                                       
297600                MOVE W-SPAR-REQU-IDKUNDRF(INDX) TO W-SPAR-IDKUNDRF        
297700                MOVE W-SPAR-REQU-IDKUNDNR(INDX) TO W-SPAR-IDKUNDNR        
297800                MOVE W-SPAR-REQU-IDKOLLI(INDX)  TO W-SPAR-IDKOLLI         
297900                MOVE W-SPAR-REQU-IDFAKT(INDX)   TO W-SPAR-IDFAKT          
298000               END-IF                                                     
298100             END-IF                                                       
298200           END-PERFORM                                                    
298300                                                                          
298400           IF NY-BEFINTLIG-ART                                            
298500* NOT VCCS                                                                
298600              IF XDC-NON-VCC-OWNED                                        
298700**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
298800**** REST OF FLOWS TO US IS IN LAB.                                       
298900**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
299000**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
299100              OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                   
299200                 PERFORM HD-NYUPPLAEGG                                    
299300              ELSE                                                        
299400* VCCS                                                                    
299500                 PERFORM HD-NYUPPLAEGG-PV                                 
299600              END-IF                                                      
299700              MOVE UPDATE-DONE TO RESP-IDMSG-INFO                         
299800              MOVE SPACE TO RESP-IDMSG-ERROR-INM                          
299900                            RESP-KVANTMOT-INM                             
300000                            RESP-IDARTNR-INM                              
300100                            RESP-ADLAGOMR-INM                             
300200                            RESP-ADGANG-INM                               
300300                            RESP-ADPLATS-INM                              
300400                            RESP-KVSKROT-INM                              
300500                            RESP-CMD-INM                                  
300600           ELSE                                                           
300700              IF NY-NYUPPLAEGG-ART                                        
300800* NOT VCCS                                                                
300900                 IF XDC-NON-VCC-OWNED                                     
301000**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
301100**** REST OF FLOWS TO US IS IN LAB.                                       
301200**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
301300**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
301400                 OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                
301500                    PERFORM HG-NY-NYUPPLAEGG-ART                          
301600                 ELSE                                                     
301700                    PERFORM HG-NY-NYUPPLAEGG-ART-PV                       
301800* VCCS                                                                    
301900                 END-IF                                                   
302000                 MOVE UPDATE-DONE TO RESP-IDMSG-INFO                      
302100                 MOVE SPACE TO RESP-IDMSG-ERROR-INM                       
302200                               RESP-KVANTMOT-INM                          
302300                               RESP-IDARTNR-INM                           
302400                               RESP-ADLAGOMR-INM                          
302500                               RESP-ADGANG-INM                            
302600                               RESP-ADPLATS-INM                           
302700                               RESP-KVSKROT-INM                           
302800                               RESP-CMD-INM                               
302900              END-IF                                                      
303000           END-IF                                                         
303100                                                                          
303200           IF W-IDLOPNRM NOT = ZERO                                       
303300              MOVE W-IDLOPNRM TO 6018-IDLOPNRM                            
303400              PERFORM IMS-REPL-W6LOPA                                     
303500           END-IF                                                         
303600                                                                          
303700           IF TRANS-OHUVUD-DAM-SKAPAD AND ORAD-IX > +1                    
303800              MOVE 'J' TO ORAD-MID-FLSLUT                                 
303900              PERFORM S03-CALL-W006KOM                                    
304000           END-IF                                                         
304100        END-IF                                                            
304200     END-IF                                                               
304300     IF SUB-KDTRANS(1:6) = 'WLA103'                                       
304400       PERFORM HE-UPPDATERA-WL630111-APP                                  
304500     ELSE                                                                 
304600       PERFORM HE-UPPDATERA-WL630111                                      
304700     END-IF                                                               
304800                                                                          
304900     IF WS-A03-SKAPAD = JA                                                
305000        IF WS-FAKTURA-KLAR = JA AND                                       
305100           W-SPAR-IDFAKT = EKOTRA03-IDFAKT                                
305200           MOVE 'Y' TO EKOTRA03-FLSLUT                                    
305300        END-IF                                                            
305400        PERFORM S091-SKRIV-EKOTRANS-A03                                   
305500     END-IF                                                               
305600     IF REQU-KDPGMACT = 'X'                                               
305700        CONTINUE                                                          
305800     ELSE                                                                 
305900        CONTINUE                                                          
306000     END-IF                                                               
306100     IF DCS-KDTRADP = 'BR12' AND FIRST-REC-TRANS                          
306200        PERFORM S43-SEND-CLOSE                                            
306300     END-IF                                                               
306400     .                                                                    
306500     EJECT                                                                
306600 HA-UPPDATERA-KOLLI-KLART-PV SECTION.                                     
306700******************************************************************        
306800* DENNA SEKTION ANVÄNDS OCKSÅ FRÅN K-UPPDATERA-X-TRANS                    
306900******************************************************************        
307000     MOVE ZERO            TO W-KVRADER                                    
307100                                                                          
307200     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
307300     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
307400     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
307500                             W-SEQA-IDFAKT-MAX                            
307600     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
307700                             W-SEQA-IDKUNDRF-MAX                          
307800     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
307900                             W-SEQA-IDKUNDNR-MAX                          
308000     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
308100                             W-SEQA-IDKOLLI-MAX                           
308200     MOVE '310'           TO W-IDPTYP                                     
308300     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
308400                                                                          
308500     PERFORM UNTIL SEGMENT-SAKNAS                                         
308600        IF WS-A03-SKAPAD = JA                                             
308700           PERFORM S091-SKRIV-EKOTRANS-A03                                
308800        END-IF                                                            
308900        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
309000        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
309100        MOVE SEQA-IDDC    TO W-IDDC                                       
309200        PERFORM IMS-GHU-WLINLC11                                          
309300        IF DCS-CDC                                                        
309400          PERFORM IMS-GET-WLARTC11                                        
309500          MOVE CLAG-ADLAGOMR   TO INL-ADLAGOMR                            
309600          MOVE CLAG-ADGANG     TO INL-ADGANG                              
309700          MOVE CLAG-ADPLATS    TO INL-ADPLATS                             
309800        ELSE                                                              
309900          PERFORM IMS-GET-WDK711                                          
310000          MOVE SLAG-ADLAGOMR   TO INL-ADLAGOMR                            
310100          MOVE SLAG-ADGANG     TO INL-ADGANG                              
310200          MOVE SLAG-ADPLATS    TO INL-ADPLATS                             
310300        END-IF                                                            
310400                                                                          
310500        MOVE INL-KVAVIS     TO W-KVAVIS                                   
310600        MOVE 'R32'          TO INL-IDPTYP                                 
310700        IF REQU-KDPGMACT = 'X'                                            
310800           IF WS-IDUSER-003 NOT = SPACE                                   
310900              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
311000           END-IF                                                         
311100        ELSE                                                              
311200           IF REQU-IDUSER-003 = ALL '+' OR SPACE                          
311300              CONTINUE                                                    
311400           ELSE                                                           
311500              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
311600           END-IF                                                         
311700        END-IF                                                            
311800        MOVE W-TIME-N       TO AKTUELL-TID                                
311900        MOVE W-DAGENS-DATUM TO INL-TIINLINL                               
312000        MOVE AKTUELL-TTMM   TO INL-TIINLITI                               
312100        MOVE INL-KVAVIS     TO INL-KVANTMOT                               
312200        ADD +1              TO W-KVRADER                                  
312300                                                                          
312400        PERFORM IMS-REPL-WLINLC11                                         
312500                                                                          
312600        PERFORM IMS-GU-WLARTC01                                           
312700        MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                            
312800        MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                            
312900        PERFORM IMS-GHNP-WLARTC11                                         
313000        MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                        
313100                                                                          
313200        IF DCS-CDC                                                        
313300          ADD W-KVAVIS             TO CLAG-KVLS                           
313400          SUBTRACT W-KVAVIS      FROM CLAG-KVAKS-CDC                      
313500        ELSE                                                              
313600          MOVE SLAG-KVLS           TO WS-OLD-KVLS                         
313700          ADD W-KVAVIS             TO SLAG-KVLS                           
313800          SUBTRACT W-KVAVIS      FROM SLAG-KVAKS-SDC                      
313900          MOVE SLAG-KVEFRS         TO WS-OLD-KVEFRS                       
314000        END-IF                                                            
314100        MOVE W-KVAVIS              TO WS-RO-KVANTMOT                      
314200                                                                          
314300        IF DCS-NDC-NA OR DCS-AUSTRALIA OR DCS-JAPAN                       
314400           PERFORM S19-EV-RO-TACKNING                                     
314500         ELSE                                                             
314600           IF DCS-CDC                                                     
314700            PERFORM S19-EV-RO-TACKNING-CDC                                
314800           END-IF                                                         
314900        END-IF                                                            
315000        IF DCS-NDC-NA                                                     
315100           MOVE INL-KVANTMOT       TO EKOTRA03-KVANTMOT                   
315200                                      AVG-KVANTMOT                        
315300           MOVE SPACE              TO EKOTRA03-KDANMORS                   
315400           MOVE ZERO               TO EKOTRA03-KVSKROT                    
315500           MOVE SLAG-PRAVCOST      TO EKOTRA03-PRAVCOST-OLD               
315600           MOVE 'ETT'  TO STEXT                                           
315700           PERFORM S10-OMRAKN-MEDELPRIS-NA                                
315800           MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                       
315900                                      EKOTRA03-PRAVCOST                   
316000           MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                       
316100           MOVE AVG-REMARKUP       TO EKOTRA03-REMARKUP                   
316200           PERFORM S09-SKAPA-EKOTRANS-A03                                 
316300        END-IF                                                            
316400        IF DCS-CDC                                                        
316500          PERFORM IMS-REPL-WLARTC11                                       
316600        ELSE                                                              
316700          PERFORM IMS-REPL-WDK711                                         
316800        END-IF                                                            
316900** WRITE WDK728 WHEN FLTRACK='J'                                          
317000        IF WS-FLTRACK = 'J'                                               
317100           MOVE INL-KVAVIS  TO WS-TRCK-KVANTMOT                           
317200           MOVE INL-KVAVIS  TO WS-TRCK-KVAVIS                             
317300           PERFORM S39-UPDATE-WDK728                                      
317400        END-IF                                                            
317500                                                                          
317600        PERFORM S21-SALDOLOGG-DATA                                        
317700        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
317800        IF DCS-CDC                                                        
317900          MOVE CLAG-KVLS          TO LOGG-KVLS                            
318000          COMPUTE LOGG-KVAKS       = CLAG-KVAKS-CDC                       
318100                                   + CLAG-KVAKS-T                         
318200          MOVE CLAG-KVEFRS        TO LOGG-KVEFRS                          
318300          MOVE CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
318400        ELSE                                                              
318500          MOVE SLAG-KVLS          TO LOGG-KVLS                            
318600          MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                           
318700          MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                          
318800          MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
318900        END-IF                                                            
319000        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
319100        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
319200        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
319300        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
319400        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
319500        PERFORM S22-ISRT-SALDOLOGG                                        
319600                                                                          
319700        PERFORM IMS-GN-WLINLD01                                           
319800     END-PERFORM                                                          
319900                                                                          
320000     PERFORM S08-UPPDATERA-6301                                           
320100     .                                                                    
320200     EJECT                                                                
320300                                                                          
320400 HA-UPPDATERA-KOLLI-KLART-HAC SECTION.                                    
320500******************************************************************        
320600* DENNA SEKTION ANVÄNDS OCKSÅ FRÅN K-UPPDATERA-X-TRANS (HAC)              
320700******************************************************************        
320800     MOVE ZERO            TO W-KVRADER                                    
320900                                                                          
321000     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
321100     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
321200     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
321300                             W-SEQA-IDFAKT-MAX                            
321400     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
321500                             W-SEQA-IDKUNDRF-MAX                          
321600     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
321700                             W-SEQA-IDKUNDNR-MAX                          
321800     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
321900                             W-SEQA-IDKOLLI-MAX                           
322000     MOVE '310'           TO W-IDPTYP                                     
322100     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
322200                                                                          
322300     PERFORM UNTIL SEGMENT-SAKNAS                                         
322400        IF WS-A03-SKAPAD = JA                                             
322500           PERFORM S091-SKRIV-EKOTRANS-A03                                
322600        END-IF                                                            
322700        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
322800                             6352-IDARTNR                                 
322900        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
323000        MOVE SEQA-IDDC    TO W-IDDC                                       
323100        PERFORM IMS-GHU-WLINLC11                                          
323200                                                                          
323300        MOVE INL-KVAVIS     TO W-KVAVIS                                   
323400        MOVE 'R32'          TO INL-IDPTYP                                 
323500        MOVE 'J'            TO INL-FLTULLST                               
323600        IF REQU-KDPGMACT = 'X'                                            
323700           IF WS-IDUSER-003 NOT = SPACE                                   
323800              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
323900           END-IF                                                         
324000        ELSE                                                              
324100           IF REQU-IDUSER-003 = ALL '+' OR SPACE                          
324200              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
324300           ELSE                                                           
324400              MOVE REQU-IDUSER-003 TO INL-IDUSER-003                      
324500           END-IF                                                         
324600        END-IF                                                            
324700        MOVE W-TIME-N       TO AKTUELL-TID                                
324800        MOVE W-DAGENS-DATUM TO INL-TIINLINL                               
324900        MOVE AKTUELL-TTMM   TO INL-TIINLITI                               
325000        IF DCS-CDC                                                        
325100          PERFORM IMS-GET-WLARTC11                                        
325200          MOVE CLAG-ADLAGOMR   TO INL-ADLAGOMR                            
325300          MOVE CLAG-ADGANG     TO INL-ADGANG                              
325400          MOVE CLAG-ADPLATS    TO INL-ADPLATS                             
325500        ELSE                                                              
325600        PERFORM IMS-GET-WDK711                                            
325700        MOVE SLAG-ADLAGOMR  TO INL-ADLAGOMR                               
325800        MOVE SLAG-ADGANG    TO INL-ADGANG                                 
325900        MOVE SLAG-ADPLATS   TO INL-ADPLATS                                
326000        END-IF                                                            
326100        MOVE ZERO           TO INL-KVANTMOT                               
326200        ADD +1              TO W-KVRADER                                  
326300                                                                          
326400        PERFORM IMS-REPL-WLINLC11                                         
326500        IF DCS-CDC                                                        
326600         SUBTRACT W-KVAVIS      FROM CLAG-KVAKS-CDC                       
326700        ELSE                                                              
326800         MOVE SLAG-KVLS    TO WS-OLD-KVLS                                 
326900         MOVE SLAG-KVEFRS  TO WS-OLD-KVEFRS                               
327000         SUBTRACT W-KVAVIS FROM SLAG-KVAKS-SDC                            
327100        END-IF                                                            
327200        MOVE W-KVAVIS     TO WS-RO-KVANTMOT                               
327300                                                                          
327400        PERFORM IMS-GU-WLARTC01                                           
327500        MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                            
327600        MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                            
327700        PERFORM IMS-GHNP-WLARTC11                                         
327800        MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                        
327900        PERFORM S19-EV-RO-TACKNING                                        
328000                                                                          
328100* SENDING CDC                                                             
328200* BERÄKNING AVGCOST                                                       
328300        MOVE INL-KVANTMOT       TO AVG-KVANTMOT                           
328400        PERFORM S10-OMRAKN-MEDELPRIS                                      
328500        MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                          
328600        MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                          
328700                                                                          
328800        MOVE FUNCTION CURRENT-DATE (1:16)                                 
328900                                 TO 6352-DAINLEV                          
329000        MOVE  INL-IDFAKT         TO 6352-IDFAKT                           
329100        MOVE  INL-IDKUNDNR       TO 6352-IDKUNDNR                         
329200        MOVE  INL-IDKOLLI        TO 6352-IDOKOLLI                         
329300        MOVE  INL-IDORDNR5       TO 6352-IDORDNR7                         
329400        MOVE  INL-KVAVIS         TO 6352-KVAVIS                           
329500        MOVE  AVG-PRAVCOST-NEW   TO 6352-PRAVCOST                         
329600        MOVE  INL-DAINLEV        TO 6352-DAINLEV-9KOMPL                   
329700** INSERT WDGX6352 SEGMENT FOR HAC                                        
329800        IF NDC-MX                                                         
329900         MOVE '53' TO W-IDDC-HAC                                          
330000        END-IF                                                            
330100        PERFORM IMS-ISRT-6352                                             
330200        PERFORM UNTIL SEGMENT-FINNS                                       
330300           ADD 1 TO 6352-DAINLEV                                          
330400           PERFORM IMS-ISRT-6352                                          
330500        END-PERFORM                                                       
330600                                                                          
330700        IF DCS-CDC                                                        
330800          PERFORM IMS-REPL-WLARTC11                                       
330900        ELSE                                                              
331000        PERFORM IMS-REPL-WDK711                                           
331100        IF WS-FLTRACK = 'J'AND NDC-MX                                     
331200         PERFORM IMS-GHNP-WDK728                                          
331300         IF SEGMENT-FINNS                                                 
331400           SUBTRACT INL-KVAVIS FROM TRCK-KVAVIS                           
331500           IF TRCK-KVAVIS > ZERO                                          
331600             PERFORM IMS-REPL-WDK728                                      
331700           ELSE                                                           
331800             PERFORM IMS-DLET-WDK728                                      
331900           END-IF                                                         
332000         END-IF                                                           
332100        END-IF                                                            
332200        END-IF                                                            
332300** HELD AT CUSTOM WILL CREATE A BOOKING TO SAP                            
332400        PERFORM S31A-SKAPA-SAP-TRANS-HAC                                  
332500                                                                          
332600        PERFORM S21-SALDOLOGG-DATA                                        
332700        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
332800        IF DCS-CDC                                                        
332900          MOVE CLAG-KVLS          TO LOGG-KVLS                            
333000          COMPUTE LOGG-KVAKS       = CLAG-KVAKS-CDC                       
333100                                   + CLAG-KVAKS-T                         
333200          MOVE CLAG-KVEFRS        TO LOGG-KVEFRS                          
333300          MOVE CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
333400        ELSE                                                              
333500        MOVE SLAG-KVLS            TO LOGG-KVLS                            
333600        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
333700        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
333800        MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                        
333900        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
334000        END-IF                                                            
334100        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
334200        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
334300        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
334400        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
334500        PERFORM S22-ISRT-SALDOLOGG                                        
334600                                                                          
334700        PERFORM IMS-GN-WLINLD01                                           
334800     END-PERFORM                                                          
334900                                                                          
335000     PERFORM S08-UPPDATERA-6301                                           
335100     .                                                                    
335200     EJECT                                                                
335300 HA-UPPDATERA-RAD-HAC SECTION.                                            
335400******************************************************************        
335500     MOVE ZERO            TO W-KVRADER                                    
335600                                                                          
335700     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
335800     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
335900     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
336000                             W-SEQA-IDFAKT-MAX                            
336100     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
336200                             W-SEQA-IDKUNDRF-MAX                          
336300     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
336400                             W-SEQA-IDKUNDNR-MAX                          
336500     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
336600                             W-SEQA-IDKOLLI-MAX                           
336700     MOVE '310'           TO W-IDPTYP                                     
336800     MOVE REQU-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                       
336900                                 W-SEQA-IDARTNR-MAX                       
337000     PERFORM IMS-GU-WLINLD01                                              
337100                                                                          
337200     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
337300                          6352-IDARTNR                                    
337400     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
337500     MOVE SEQA-IDDC    TO W-IDDC                                          
337600     PERFORM IMS-GHU-WLINLC11                                             
337700     PERFORM IMS-GET-WDK711                                               
337800                                                                          
337900     MOVE INL-KVAVIS     TO W-KVAVIS                                      
338000     MOVE 'R32'          TO INL-IDPTYP                                    
338100     MOVE 'J'            TO INL-FLTULLST                                  
338200     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
338300         CONTINUE                                                         
338400     ELSE                                                                 
338500         MOVE WS-IDUSER-003 TO INL-IDUSER-003                             
338600     END-IF                                                               
338700     MOVE W-TIME-N       TO AKTUELL-TID                                   
338800     MOVE W-DAGENS-DATUM TO INL-TIINLINL                                  
338900     MOVE AKTUELL-TTMM   TO INL-TIINLITI                                  
339000     MOVE SLAG-ADLAGOMR  TO INL-ADLAGOMR                                  
339100     MOVE SLAG-ADGANG    TO INL-ADGANG                                    
339200     MOVE SLAG-ADPLATS   TO INL-ADPLATS                                   
339300     MOVE ZERO           TO INL-KVANTMOT                                  
339400     ADD +1              TO W-KVRADER                                     
339500                                                                          
339600     PERFORM IMS-REPL-WLINLC11                                            
339700                                                                          
339800     MOVE SLAG-KVLS    TO WS-OLD-KVLS                                     
339900     MOVE SLAG-KVEFRS  TO WS-OLD-KVEFRS                                   
340000     SUBTRACT W-KVAVIS FROM SLAG-KVAKS-SDC                                
340100     MOVE W-KVAVIS     TO WS-RO-KVANTMOT                                  
340200                                                                          
340300     PERFORM IMS-GU-WLARTC01                                              
340400     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
340500     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
340600     PERFORM IMS-GHNP-WLARTC11                                            
340700     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
340800                                                                          
340900     PERFORM S19-EV-RO-TACKNING                                           
341000                                                                          
341100* SENDING CDC                                                             
341200* BERÄKNING AVGCOST                                                       
341300     MOVE INL-KVANTMOT       TO AVG-KVANTMOT                              
341400     PERFORM S10-OMRAKN-MEDELPRIS                                         
341500     MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                             
341600     MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                             
341700                                                                          
341800     MOVE FUNCTION CURRENT-DATE (1:16)                                    
341900                              TO 6352-DAINLEV                             
342000     MOVE  INL-IDFAKT         TO 6352-IDFAKT                              
342100     MOVE  INL-IDKUNDNR       TO 6352-IDKUNDNR                            
342200     MOVE  INL-IDKOLLI        TO 6352-IDOKOLLI                            
342300     MOVE  INL-IDORDNR5       TO 6352-IDORDNR7                            
342400     MOVE  INL-KVAVIS         TO 6352-KVAVIS                              
342500     MOVE  AVG-PRAVCOST-NEW   TO 6352-PRAVCOST                            
342600     MOVE  INL-DAINLEV        TO 6352-DAINLEV-9KOMPL                      
342700** INSERT WDGX6352 SEGMENT FOR HAC                                        
342800     IF NDC-MX                                                            
342900      MOVE '53' TO W-IDDC-HAC                                             
343000     END-IF                                                               
343100     PERFORM IMS-ISRT-6352                                                
343200     PERFORM UNTIL INSERT-OK                                              
343300        ADD 1 TO 6352-DAINLEV                                             
343400        PERFORM IMS-ISRT-6352                                             
343500     END-PERFORM                                                          
343600                                                                          
343700     PERFORM IMS-REPL-WDK711                                              
343800     IF WS-FLTRACK = 'J'AND NDC-MX                                        
343900      PERFORM IMS-GHNP-WDK728                                             
344000      IF SEGMENT-FINNS                                                    
344100        SUBTRACT INL-KVAVIS FROM TRCK-KVAVIS                              
344200        IF TRCK-KVAVIS > ZERO                                             
344300          PERFORM IMS-REPL-WDK728                                         
344400        ELSE                                                              
344500          PERFORM IMS-DLET-WDK728                                         
344600        END-IF                                                            
344700      END-IF                                                              
344800     END-IF                                                               
344900                                                                          
345000** HELD AT CUSTOM WILL CREATE A BOOKING TO SAP                            
345100     PERFORM S31A-SKAPA-SAP-TRANS-HAC                                     
345200                                                                          
345300     PERFORM S21-SALDOLOGG-DATA                                           
345400     MOVE REQU-IDDC-KEY        TO LOGG-IDDC                               
345500     MOVE SLAG-KVLS            TO LOGG-KVLS                               
345600     MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                              
345700     MOVE W-KVAVIS             TO LOGG-KVART-SALDO                        
345800     MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                     
345900     MOVE '+'                  TO LOGG-IDTECKEN-KVLS                      
346000     MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                             
346100     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
346200     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
346300     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
346400     PERFORM S22-ISRT-SALDOLOGG                                           
346500                                                                          
346600     PERFORM S08-UPPDATERA-6301                                           
346700     .                                                                    
346800     EJECT                                                                
346900                                                                          
347000 HA-UPPDATERA-KOLLI-KLART SECTION.                                        
347100******************************************************************        
347200* DENNA SEKTION ANVÄNDS OCKSÅ FRÅN K-UPPDATERA-X-TRANS                    
347300******************************************************************        
347400     MOVE ZERO            TO W-KVRADER                                    
347500                                                                          
347600     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
347700     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
347800     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
347900                             W-SEQA-IDFAKT-MAX                            
348000     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
348100                             W-SEQA-IDKUNDRF-MAX                          
348200     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
348300                             W-SEQA-IDKUNDNR-MAX                          
348400     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
348500                             W-SEQA-IDKOLLI-MAX                           
348600     MOVE '310'           TO W-IDPTYP                                     
348700     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
348800                                                                          
348900     PERFORM UNTIL SEGMENT-SAKNAS                                         
349000        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
349100        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
349200        MOVE SEQA-IDDC    TO W-IDDC                                       
349300        PERFORM IMS-GHU-WLINLC11                                          
349400        PERFORM IMS-GET-WDK711                                            
349500                                                                          
349600        MOVE INL-KVAVIS     TO W-KVAVIS                                   
349700        MOVE 'R32'          TO INL-IDPTYP                                 
349800        IF REQU-KDPGMACT = 'X'                                            
349900           IF WS-IDUSER-003 NOT = SPACE                                   
350000              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
350100           END-IF                                                         
350200        ELSE                                                              
350300           IF REQU-IDUSER-003 = ALL '+' OR SPACE                          
350400              CONTINUE                                                    
350500           ELSE                                                           
350600              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
350700           END-IF                                                         
350800        END-IF                                                            
350900        MOVE W-TIME-N       TO AKTUELL-TID                                
351000                                                                          
351100        MOVE W-DAGENS-DATUM    TO INL-TIINLINL                            
351200        MOVE W-DAGENS-TID(1:4) TO INL-TIINLITI                            
351300                                                                          
351400        MOVE SLAG-ADLAGOMR  TO INL-ADLAGOMR                               
351500        MOVE SLAG-ADGANG    TO INL-ADGANG                                 
351600        MOVE SLAG-ADPLATS   TO INL-ADPLATS                                
351700        MOVE INL-KVAVIS     TO INL-KVANTMOT                               
351800        ADD +1              TO W-KVRADER                                  
351900                                                                          
352000        PERFORM IMS-REPL-WLINLC11                                         
352100                                                                          
352200        MOVE SLAG-KVLS    TO WS-OLD-KVLS                                  
352300        MOVE SLAG-KVEFRS  TO WS-OLD-KVEFRS                                
352400        ADD W-KVAVIS      TO SLAG-KVLS                                    
352500        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-SDC                             
352600        MOVE W-KVAVIS     TO WS-RO-KVANTMOT                               
352700                                                                          
352800        PERFORM IMS-GU-WLARTC01                                           
352900        MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                            
353000        MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                            
353100        PERFORM IMS-GHNP-WLARTC11                                         
353200        MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                        
353300                                                                          
353400        PERFORM S19-EV-RO-TACKNING                                        
353500                                                                          
353600* SENDING CDC                                                             
353700* BERÄKNING AVGCOST                                                       
353800        MOVE INL-KVANTMOT       TO AVG-KVANTMOT                           
353900        PERFORM S10-OMRAKN-MEDELPRIS                                      
354000        MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                          
354100        MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                          
354200        PERFORM IMS-REPL-WDK711                                           
354300** WRITE WDK728 WHEN FLTRACK='J'                                          
354400        IF WS-FLTRACK = 'J'                                               
354500           MOVE INL-KVAVIS  TO WS-TRCK-KVANTMOT                           
354600           MOVE INL-KVAVIS  TO WS-TRCK-KVAVIS                             
354700           PERFORM S39-UPDATE-WDK728                                      
354800        END-IF                                                            
354900                                                                          
355000        PERFORM S21-SALDOLOGG-DATA                                        
355100        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
355200        MOVE SLAG-KVLS            TO LOGG-KVLS                            
355300        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
355400        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
355500        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
355600        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
355700        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
355800        MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                        
355900        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
356000        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
356100        PERFORM S22-ISRT-SALDOLOGG                                        
356200                                                                          
356300        PERFORM IMS-GN-WLINLD01                                           
356400     END-PERFORM                                                          
356500                                                                          
356600     PERFORM S08-UPPDATERA-6301                                           
356700     .                                                                    
356800     EJECT                                                                
356900                                                                          
357000 HB-KOLLI-DAMAGED-PV      SECTION.                                        
357100     MOVE ZERO               TO W-KVRADER                                 
357200     IF REQU-DAINLEV (INDX) NOT = ALL '+'                                 
357300     MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                                
357400                          W-IDARTNR-NYCKEL-SPAR                           
357500     MOVE REQU-DAINLEV (INDX) TO W-DAINLEV                                
357600                          W-DAINLEV-NYCKEL-SPAR                           
357700     MOVE REQU-IDDC-KEY TO W-IDDC                                         
357800                          W-IDDC-NYCKEL-SPAR                              
357900     ELSE                                                                 
358000     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
358100     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
358200     MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                         
358300                                W-SEQA-IDFAKT-MAX                         
358400     MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                       
358500                                W-SEQA-IDKUNDRF-MAX                       
358600     MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                       
358700                                W-SEQA-IDKUNDNR-MAX                       
358800     MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                        
358900                                W-SEQA-IDKOLLI-MAX                        
359000     MOVE '310'              TO W-IDPTYP                                  
359100     MOVE REQU-IDARTNR(INDX) TO W-SEQA-IDARTNR-MIN                        
359200                                W-SEQA-IDARTNR-MAX                        
359300     PERFORM IMS-GU-WLINLD01                                              
359400     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
359500                          W-IDARTNR-NYCKEL-SPAR                           
359600     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
359700                          W-DAINLEV-NYCKEL-SPAR                           
359800     MOVE SEQA-IDDC    TO W-IDDC                                          
359900                          W-IDDC-NYCKEL-SPAR                              
360000     END-IF                                                               
360100     PERFORM IMS-GHU-WLINLC11                                             
360200     IF DCS-CDC                                                           
360300       CONTINUE                                                           
360400     ELSE                                                                 
360500       PERFORM IMS-GET-WDK711                                             
360600     END-IF                                                               
360700                                                                          
360800     PERFORM IMS-GU-WLARTC01                                              
360900     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
361000     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
361100     PERFORM IMS-GHNP-WLARTC11                                            
361200     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
361300     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
361400                                                                          
361500     MOVE INL-KVAVIS   TO W-KVAVIS                                        
361600     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
361700     MOVE INL-FLSKAKOL TO W-FLSKAKOL                                      
361800     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
361900     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
362000                                                                          
362100     IF INL-KVANTMOT > ZERO                                               
362200        IF DCS-CDC                                                        
362300          ADD W-KVANTMOT(INDX)    TO CLAG-KVLS                            
362400          SUBTRACT W-KVANTMOT(INDX)                                       
362500                                FROM CLAG-KVAKS-CDC                       
362600        ELSE                                                              
362700          ADD W-KVANTMOT(INDX)       TO SLAG-KVLS                         
362800          SUBTRACT W-KVANTMOT(INDX)  FROM SLAG-KVAKS-SDC                  
362900        END-IF                                                            
363000        IF W-KVANTMOT(INDX) NOT = 0                                       
363100           PERFORM S21-SALDOLOGG-DATA                                     
363200           MOVE REQU-IDDC-KEY      TO LOGG-IDDC                           
363300           IF DCS-CDC                                                     
363400              MOVE CLAG-KVLS       TO LOGG-KVLS                           
363500              COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                      
363600                                    + CLAG-KVAKS-T                        
363700              MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                         
363800              MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
363900           ELSE                                                           
364000              MOVE SLAG-KVLS          TO LOGG-KVLS                        
364100              MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                       
364200              MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                      
364300              MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                   
364400           END-IF                                                         
364500           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
364600           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
364700           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
364800           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
364900           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
365000           PERFORM S22-ISRT-SALDOLOGG                                     
365100        END-IF                                                            
365200        IF DCS-CDC                                                        
365300           SUBTRACT W-KVSKROT (INDX) FROM CLAG-KVAKS-CDC                  
365400        ELSE                                                              
365500           SUBTRACT W-KVSKROT (INDX) FROM SLAG-KVAKS-SDC                  
365600        END-IF                                                            
365700        IF DCS-CDC                                                        
365800           ADD W-KVSKROT (INDX)    TO CLAG-KVLS                           
365900        END-IF                                                            
366000        IF DCS-SDC OR DCS-AUSTRALIA OR DCS-JAPAN                          
366100           ADD W-KVSKROT (INDX)    TO SLAG-KVLS                           
366200        END-IF                                                            
366300        IF W-KVSKROT(INDX) NOT = 0                                        
366400           PERFORM S21-SALDOLOGG-DATA                                     
366500           MOVE REQU-IDDC-KEY      TO LOGG-IDDC                           
366600           IF DCS-CDC                                                     
366700              MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                         
366800              MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
366900              COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                      
367000                                    + CLAG-KVAKS-T                        
367100              MOVE CLAG-KVLS       TO LOGG-KVLS                           
367200           ELSE                                                           
367300              MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                      
367400              MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                   
367500              MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                       
367600              MOVE SLAG-KVLS          TO LOGG-KVLS                        
367700           END-IF                                                         
367800           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
367900           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
368000           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
368100           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
368200                                                                          
368300           IF DCS-SDC OR DCS-AUSTRALIA OR DCS-JAPAN OR DCS-CDC            
368400               MOVE '+'            TO LOGG-IDTECKEN-KVLS                  
368500           ELSE                                                           
368600               MOVE SPACE          TO LOGG-IDTECKEN-KVLS                  
368700           END-IF                                                         
368800           PERFORM S22-ISRT-SALDOLOGG                                     
368900                                                                          
369000           IF DCS-SDC OR DCS-AUSTRALIA OR DCS-JAPAN                       
369100              IF SEND-DCS-DDC                                             
369200                 MOVE W-KVSKROT(INDX) TO FILC2-KVANTAL                    
369300                 MOVE 'DAM'           TO FILC2-AVVIKELSETYP               
369400                 PERFORM S32-SKAPA-DIFF-TRANS                             
369500              END-IF                                                      
369600              MOVE W-KVAVIS           TO FILC3-KVAVIS                     
369700              MOVE W-KVSKROT(INDX)    TO FILC3-KVANTAL                    
369800              MOVE 'DAM'              TO FILC3-AVVIKELSETYP               
369900              MOVE 5                  TO FILC3-KDSORT1                    
370000              PERFORM S33-SKAPA-LDC-TRANS                                 
370100           END-IF                                                         
370200        END-IF                                                            
370300        ADD W-KVANTMOT(INDX)       TO INL-KVANTMOT                        
370400        MOVE INL-KVANTMOT          TO W-KVANTMOT(INDX)                    
370500        ADD W-KVSKROT(INDX)        TO INL-KVART-SKROT                     
370600        IF DCS-CDC                                                        
370700         CONTINUE                                                         
370800        ELSE                                                              
370900         COMPUTE WS-OLD-KVLS = SLAG-KVLS - INL-KVANTMOT                   
371000         END-COMPUTE                                                      
371100         MOVE SLAG-KVEFRS      TO WS-OLD-KVEFRS                           
371200        END-IF                                                            
371300     ELSE                                                                 
371400        IF DCS-CDC                                                        
371500          ADD W-KVANTMOT (INDX)   TO CLAG-KVLS                            
371600        ELSE                                                              
371700          MOVE SLAG-KVLS          TO WS-OLD-KVLS                          
371800          ADD W-KVANTMOT(INDX)    TO SLAG-KVLS                            
371900          MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                        
372000        END-IF                                                            
372100        IF W-KVANTMOT(INDX) NOT = 0                                       
372200           PERFORM S21-SALDOLOGG-DATA                                     
372300           MOVE REQU-IDDC-KEY      TO LOGG-IDDC                           
372400           IF DCS-CDC                                                     
372500              MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                         
372600              MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
372700              MOVE CLAG-KVLS       TO LOGG-KVLS                           
372800              COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                      
372900                                    + CLAG-KVAKS-T                        
373000           ELSE                                                           
373100              MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                      
373200              MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                   
373300              MOVE SLAG-KVLS          TO LOGG-KVLS                        
373400              MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                       
373500           END-IF                                                         
373600           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
373700           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
373800           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
373900           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
374000           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
374100           PERFORM S22-ISRT-SALDOLOGG                                     
374200        END-IF                                                            
374300                                                                          
374400        IF DCS-SDC OR DCS-AUSTRALIA OR DCS-JAPAN                          
374500           ADD W-KVSKROT (INDX)    TO SLAG-KVLS                           
374600        END-IF                                                            
374700        IF DCS-CDC                                                        
374800           ADD W-KVSKROT (INDX)    TO CLAG-KVLS                           
374900        END-IF                                                            
375000                                                                          
375100        IF (W-KVSKROT(INDX) NOT = 0 AND DCS-SDC)                          
375200        OR (W-KVSKROT(INDX) NOT = 0 AND DCS-AUSTRALIA)                    
375300        OR (W-KVSKROT(INDX) NOT = 0 AND DCS-JAPAN)                        
375400           PERFORM S21-SALDOLOGG-DATA                                     
375500           MOVE REQU-IDDC-KEY      TO LOGG-IDDC                           
375600           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
375700           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
375800           MOVE SLAG-KVLS          TO LOGG-KVLS                           
375900           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
376000           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
376100           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
376200           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
376300           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
376400           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
376500           PERFORM S22-ISRT-SALDOLOGG                                     
376600           IF SEND-DCS-DDC                                                
376700              MOVE W-KVSKROT(INDX) TO FILC2-KVANTAL                       
376800              MOVE 'DAM'           TO FILC2-AVVIKELSETYP                  
376900              PERFORM S32-SKAPA-DIFF-TRANS                                
377000           END-IF                                                         
377100           MOVE W-KVAVIS        TO FILC3-KVAVIS                           
377200           MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                          
377300           MOVE 'DAM'           TO FILC3-AVVIKELSETYP                     
377400           MOVE 5               TO FILC3-KDSORT1                          
377500           PERFORM S33-SKAPA-LDC-TRANS                                    
377600        END-IF                                                            
377700        IF DCS-CDC                                                        
377800           SUBTRACT W-KVAVIS  FROM CLAG-KVAKS-CDC                         
377900        ELSE                                                              
378000           SUBTRACT W-KVAVIS  FROM SLAG-KVAKS-SDC                         
378100        END-IF                                                            
378200        MOVE W-KVANTMOT(INDX) TO INL-KVANTMOT                             
378300        MOVE W-KVSKROT (INDX) TO INL-KVART-SKROT                          
378400     END-IF                                                               
378500                                                                          
378600                                                                          
378700     IF W-KVAVIS NOT = 0                                                  
378800        PERFORM S21-SALDOLOGG-DATA                                        
378900        MOVE REQU-IDDC-KEY      TO LOGG-IDDC                              
379000        IF DCS-CDC                                                        
379100           MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                            
379200           MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                         
379300           MOVE CLAG-KVLS       TO LOGG-KVLS                              
379400           COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                         
379500                                 + CLAG-KVAKS-T                           
379600        ELSE                                                              
379700           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
379800           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
379900           MOVE SLAG-KVLS          TO LOGG-KVLS                           
380000           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
380100        END-IF                                                            
380200        MOVE W-KVAVIS           TO LOGG-KVART-SALDO                       
380300        MOVE '-'                TO LOGG-IDTECKEN-KVAKS                    
380400        MOVE SPACE              TO LOGG-IDTECKEN-KVLS                     
380500        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
380600        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
380700        PERFORM S22-ISRT-SALDOLOGG                                        
380800     END-IF                                                               
380900                                                                          
381000     IF W-ADLAGOMR (INDX) > ZERO                                          
381100      IF DCS-CDC                                                          
381200        IF W-ADLAGOMR(INDX) = CLAG-ADLAGOMR                               
381300        AND W-ADGANG(INDX) = CLAG-ADGANG                                  
381400        AND W-ADPLATS(INDX) = CLAG-ADPLATS                                
381500           CONTINUE                                                       
381600        ELSE                                                              
381700           MOVE W-ADLAGOMR (INDX) TO CLAG-ADLAGOMR                        
381800           MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                      
381900           MOVE W-ADGANG (INDX)   TO CLAG-ADGANG                          
382000           MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                        
382100           MOVE W-ADPLATS (INDX)  TO CLAG-ADPLATS                         
382200           MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                       
382300           PERFORM S30-UPPDATERA-WDJ9                                     
382400        END-IF                                                            
382500      ELSE                                                                
382600        IF W-ADLAGOMR(INDX) = SLAG-ADLAGOMR                               
382700        AND W-ADGANG(INDX) = SLAG-ADGANG                                  
382800        AND W-ADPLATS(INDX) = SLAG-ADPLATS                                
382900           CONTINUE                                                       
383000        ELSE                                                              
383100           MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                        
383200           MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                      
383300           MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                          
383400           MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                        
383500           MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                         
383600           MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                       
383700           PERFORM S30-UPPDATERA-WDJ9                                     
383800        END-IF                                                            
383900      END-IF                                                              
384000     END-IF                                                               
384100                                                                          
384200     ADD +1 TO W-KVRADER                                                  
384300     MOVE 'R32' TO INL-IDPTYP                                             
384400     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
384500        CONTINUE                                                          
384600     ELSE                                                                 
384700        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
384800     END-IF                                                               
384900     MOVE W-TIME-N         TO AKTUELL-TID                                 
385000     MOVE W-DAGENS-DATUM   TO INL-TIINLINL                                
385100     MOVE AKTUELL-TTMM     TO INL-TIINLITI                                
385200     IF DCS-CDC                                                           
385300        MOVE CLAG-ADLAGOMR TO INL-ADLAGOMR                                
385400        MOVE CLAG-ADGANG   TO INL-ADGANG                                  
385500        MOVE CLAG-ADPLATS  TO INL-ADPLATS                                 
385600        PERFORM IMS-REPL-WLARTC11                                         
385700     ELSE                                                                 
385800        MOVE SLAG-ADLAGOMR    TO INL-ADLAGOMR                             
385900        MOVE SLAG-ADGANG      TO INL-ADGANG                               
386000        MOVE SLAG-ADPLATS     TO INL-ADPLATS                              
386100        PERFORM IMS-REPL-WDK711                                           
386200     END-IF                                                               
386300     PERFORM IMS-REPL-WLINLC11                                            
386400                                                                          
386500     PERFORM IMS-GU-WLARTC01                                              
386600     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
386700     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
386800     PERFORM IMS-GHNP-WLARTC11                                            
386900     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
387000     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
387100                                                                          
387200     IF DCS-AUSTRALIA OR DCS-JAPAN                                        
387300        IF W-KVANTMOT(INDX) > 0                                           
387400           MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                      
387500           PERFORM S19-EV-RO-TACKNING                                     
387600        END-IF                                                            
387700     END-IF                                                               
387800     IF DCS-CDC                                                           
387900        IF W-KVANTMOT(INDX) > 0                                           
388000           MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                      
388100           PERFORM S19-EV-RO-TACKNING-CDC                                 
388200        END-IF                                                            
388300     END-IF                                                               
388400                                                                          
388500     IF DCS-NDC-NA                                                        
388600        PERFORM S09-SKAPA-EKOTRANS-A03                                    
388700        MOVE SPACE            TO EKOTRA03-KDANMORS                        
388800        MOVE SLAG-PRAVCOST    TO EKOTRA03-PRAVCOST-OLD                    
388900        MOVE W-KVANTMOT(INDX) TO AVG-KVANTMOT                             
389000                                 EKOTRA03-KVANTMOT                        
389100        MOVE W-KVSKROT(INDX)  TO EKOTRA03-KVSKROT                         
389200                                                                          
389300        PERFORM IMS-GET-WDK711                                            
389400        MOVE 'TVA'  TO STEXT                                              
389500        PERFORM S10-OMRAKN-MEDELPRIS-NA                                   
389600        MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                          
389700                                   EKOTRA03-PRAVCOST                      
389800        MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                          
389900        MOVE AVG-REMARKUP       TO EKOTRA03-REMARKUP                      
390000                                                                          
390100        IF W-KVANTMOT(INDX) > 0                                           
390200           MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                      
390300           PERFORM S19-EV-RO-TACKNING                                     
390400           PERFORM IMS-REPL-WDK711                                        
390500        ELSE                                                              
390600           MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST                
390700        END-IF                                                            
390800        PERFORM HBC-NDC-KOLLI-DAMAGED                                     
390900                                                                          
391000        MOVE W-KVAVIS        TO FILC3-KVAVIS                              
391100        MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                             
391200        MOVE 'DAM'           TO FILC3-AVVIKELSETYP                        
391300        MOVE 5               TO FILC3-KDSORT1                             
391400        MOVE DCS-FLINLREP    TO FILC3-FLINLREP                            
391500        PERFORM S33-SKAPA-LDC-TRANS                                       
391600                                                                          
391700     END-IF                                                               
391800                                                                          
391900     IF DCS-CDC                                                           
392000        PERFORM HBC-NDC-KOLLI-DAMAGED                                     
392100     END-IF                                                               
392200                                                                          
392300     IF (W-KVANTMOT (INDX) + W-KVSKROT (INDX))                            
392400                           NOT = W-KVAVIS                                 
392500        ADD W-KVANTMOT (INDX) W-KVSKROT (INDX)                            
392600                           GIVING W-TEMP-KVANT                            
392700        MOVE ZERO TO W-DIFF-KVANT                                         
392800        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
392900                               - W-TEMP-KVANT                             
393000        END-COMPUTE                                                       
393100                                                                          
393200        IF DCS-NDC-NA                                                     
393300           PERFORM S17-NDC-OOVER-UNDER                                    
393400           MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                        
393500           MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                        
393600           MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                           
393700           PERFORM IMS-GHU-WLINLC11                                       
393800**** BOUNCE INVOICE HAS TO HAVE A DIFFERENT BOOKING FOR THIS              
393900**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
394000**** REST OF FLOWS TO US IS IN LAB.                                       
394100**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
394200**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
394300           IF DIST35-NONVCC-NONVCC-REFILL OR                              
394400              DIST35-NONVCC-NONVCC-TRANSFER                               
394500             IF SEND-DCS-CDC                                              
394600               MOVE INL-PRARTNTO        TO R8-EKH-PRARTNTO                
394700               PERFORM S31-SKAPA-SAP-TRANS                                
394800             END-IF                                                       
394900           ELSE                                                           
395000**** THE LAB BOOKING DOESNT NEED A EXTRA BOOKING                          
395100             CONTINUE                                                     
395200           END-IF                                                         
395300        ELSE                                                              
395400           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
395500           OR DIST35-NONVCC-VCC-REFILL                                    
395600           OR DIST35-NONVCC-VCC-TRANSFER                                  
395700              IF SEND-DCS-CDC                                             
395800                PERFORM IMS-GHU-WLARTC11                                  
395900                MOVE CLAG-KVLS    TO WS-KVLS                              
396000                COMPUTE CLAG-KVLS = CLAG-KVLS                             
396100                                  + (W-KVAVIS                             
396200                                  -  W-KVANTMOT (INDX)                    
396300                                  -  W-KVSKROT  (INDX))                   
396400                END-COMPUTE                                               
396500                PERFORM IMS-REPL-WLARTC11                                 
396600                PERFORM HBA-SALDOLOGG-DATA                                
396700                PERFORM S07-SKAPA-HISTORIK                                
396800              ELSE                                                        
396900                IF SEND-DCS-DDC                                           
397000                   MOVE W-DIFF-KVANT       TO WS-DIFF-KVANT               
397100                   MOVE WS-DIFF-KVANT      TO FILC2-KVANTAL               
397200                   IF W-KVAVIS < W-TEMP-KVANT                             
397300                      MOVE 'ÖVERLEV.'      TO FILC2-AVVIKELSETYP          
397400                   ELSE                                                   
397500                      MOVE 'UNDERLEV.'     TO FILC2-AVVIKELSETYP          
397600                   END-IF                                                 
397700                   PERFORM S32-SKAPA-DIFF-TRANS                           
397800                END-IF                                                    
397900              END-IF                                                      
398000              PERFORM S31-SKAPA-SAP-TRANS-VCCS                            
398100           ELSE                                                           
398200              IF DCS-CDC                                                  
398300                 IF SEND-DCS-NDC-CN OR SEND-DCS-USA                       
398400*-REFILL FRÅN EXPORT TILL CDC.UT PÅ RAPPORT W41841-001.                   
398500                                                                          
398600                   IF W-KVAVIS < W-TEMP-KVANT                             
398700                     MOVE '11'         TO 6308-KDANMORS                   
398800                   ELSE                                                   
398900                     MOVE '00'         TO 6308-KDANMORS                   
399000                   END-IF                                                 
399100                   MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                   
399200                   MOVE WS-DIFF-KVANT  TO 6308-KVLEVANM                   
399300                   PERFORM S11-SKAPA-LEVANM-TRANS                         
399400                                                                          
399500                   PERFORM S31-SKAPA-SAP-TRANS-VCCS                       
399600                 END-IF                                                   
399700              END-IF                                                      
399800           END-IF                                                         
399900        END-IF                                                            
400000        IF DIST35-NONVCC-VCC-REFILL OR                                    
400100           DIST35-NONVCC-VCC-TRANSFER                                     
400200         IF W-FLSKAKOL = 'J'                                              
400300            MOVE '63' TO 6308-KDANMORS                                    
400400         ELSE                                                             
400500            MOVE '43' TO 6308-KDANMORS                                    
400600         END-IF                                                           
400700         MOVE W-KVSKROT(INDX) TO 6308-KVLEVANM                            
400800         PERFORM S11-SKAPA-LEVANM-TRANS                                   
400900        END-IF                                                            
401000********                                                                  
401100        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
401200        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
401300        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
401400        IF W-KVAVIS < W-TEMP-KVANT                                        
401500           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
401600           MOVE 3           TO FILC3-KDSORT1                              
401700        ELSE                                                              
401800           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
401900           MOVE 4           TO FILC3-KDSORT1                              
402000        END-IF                                                            
402100        PERFORM S33-SKAPA-LDC-TRANS                                       
402200********                                                                  
402300        IF SEND-DCS-AUSTRALIA OR SEND-DCS-JAPAN OR SEND-DCS-SDC           
402400            MOVE W-IDDC TO WS-SPARAT-IDDC                                 
402500            MOVE W-IDDC-B6-SEND TO W-IDDC                                 
402600            PERFORM IMS-GET-WDK711                                        
402700            MOVE SLAG-KVLS    TO WS-KVLS                                  
402800            COMPUTE SLAG-KVLS = SLAG-KVLS                                 
402900                              + (W-KVAVIS                                 
403000                              -  W-KVANTMOT (INDX)                        
403100                              -  W-KVSKROT  (INDX))                       
403200            END-COMPUTE                                                   
403300            PERFORM IMS-REPL-WDK711                                       
403400            MOVE WS-SPARAT-IDDC TO W-IDDC                                 
403500            PERFORM HBB-SALDOLOGG-DATA                                    
403600            PERFORM S13-SKAPA-NDC-HIST-SANDANDE                           
403700            PERFORM S31-SKAPA-SAP-TRANS-VCCS                              
403800        END-IF                                                            
403900     END-IF                                                               
404000                                                                          
404100** WRITE WDK728 WHEN FLTRACK='J'                                          
404200     IF REQU-KVANTMOT-IN (INDX) NOT = ALL '+'                             
404300     AND REQU-KVANTMOT-IN (INDX) NOT = SPACE                              
404400        IF WS-FLTRACK = 'J'                                               
404500           MOVE W-KVANTMOT(INDX)  TO WS-TRCK-KVANTMOT                     
404600           MOVE W-KVAVIS          TO WS-TRCK-KVAVIS                       
404700           PERFORM S39-UPDATE-WDK728                                      
404800        END-IF                                                            
404900     END-IF                                                               
405000                                                                          
405100     IF DCS-NDC-NA                                                        
405200        CONTINUE                                                          
405300     ELSE                                                                 
405400        MOVE NEJ TO NY-SKROT-SW                                           
405500        PERFORM S99-SKAPA-SKROT-ORDER                                     
405600     END-IF                                                               
405700                                                                          
405800     PERFORM S08-UPPDATERA-6301                                           
405900     .                                                                    
406000     EJECT                                                                
406100                                                                          
406200 HB-KOLLI-DAMAGED         SECTION.                                        
406300     MOVE ZERO               TO W-KVRADER                                 
406400                                                                          
406500     IF REQU-DAINLEV (INDX) NOT = ALL '+'                                 
406600     MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                                
406700                          W-IDARTNR-NYCKEL-SPAR                           
406800     MOVE REQU-DAINLEV (INDX) TO W-DAINLEV                                
406900                          W-DAINLEV-NYCKEL-SPAR                           
407000     MOVE REQU-IDDC-KEY TO W-IDDC                                         
407100                          W-IDDC-NYCKEL-SPAR                              
407200     ELSE                                                                 
407300     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
407400     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
407500     MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                         
407600                                W-SEQA-IDFAKT-MAX                         
407700     MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                       
407800                                W-SEQA-IDKUNDRF-MAX                       
407900     MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                       
408000                                W-SEQA-IDKUNDNR-MAX                       
408100     MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                        
408200                                W-SEQA-IDKOLLI-MAX                        
408300     MOVE '310'              TO W-IDPTYP                                  
408400     MOVE REQU-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                       
408500                                 W-SEQA-IDARTNR-MAX                       
408600     PERFORM IMS-GU-WLINLD01                                              
408700     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
408800                          W-IDARTNR-NYCKEL-SPAR                           
408900     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
409000                          W-DAINLEV-NYCKEL-SPAR                           
409100     MOVE SEQA-IDDC    TO W-IDDC                                          
409200                          W-IDDC-NYCKEL-SPAR                              
409300     END-IF                                                               
409400     PERFORM IMS-GHU-WLINLC11                                             
409500     PERFORM IMS-GET-WDK711                                               
409600                                                                          
409700     MOVE INL-KVAVIS   TO W-KVAVIS                                        
409800     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
409900     MOVE INL-KVAVIS   TO W-KVAVIS                                        
410000     MOVE INL-FLSKAKOL TO W-FLSKAKOL                                      
410100     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
410200     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
410300     MOVE INL-IDORDNR5 TO W-IDORDER                                       
410400     MOVE INL-IDKOLLI  TO W-IDKOLLI                                       
410500                                                                          
410600     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
410700        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
410800        PERFORM IMS-GU-WDB601-SEND                                        
410900     END-IF                                                               
411000                                                                          
411100     IF INL-KVANTMOT > ZERO                                               
411200        ADD W-KVANTMOT(INDX)       TO SLAG-KVLS                           
411300        SUBTRACT W-KVANTMOT(INDX)  FROM SLAG-KVAKS-SDC                    
411400*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
411500*   ---FÖR ATT UPPDATERA I WDL9                                           
411600        IF W-KVANTMOT(INDX) NOT = 0                                       
411700           PERFORM S21-SALDOLOGG-DATA                                     
411800           MOVE REQU-IDDC-KEY        TO LOGG-IDDC                         
411900           MOVE SLAG-KVLS          TO LOGG-KVLS                           
412000           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
412100           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
412200           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
412300           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
412400           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
412500           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
412600           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
412700           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
412800           PERFORM S22-ISRT-SALDOLOGG                                     
412900        END-IF                                                            
413000        SUBTRACT W-KVSKROT (INDX)  FROM SLAG-KVAKS-SDC                    
413100        IF SEND-DCS-CHINA                                                 
413200           ADD W-KVSKROT (INDX)    TO SLAG-KVLS                           
413300        END-IF                                                            
413400*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
413500*   ---FÖR ATT UPPDATERA I WDL9                                           
413600        IF W-KVSKROT(INDX) NOT = 0                                        
413700           PERFORM S21-SALDOLOGG-DATA                                     
413800           MOVE REQU-IDDC-KEY        TO LOGG-IDDC                         
413900           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
414000           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
414100           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
414200           MOVE SLAG-KVLS          TO LOGG-KVLS                           
414300           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
414400           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
414500           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
414600           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
414700           IF SEND-DCS-CHINA                                              
414800              MOVE '+'             TO LOGG-IDTECKEN-KVLS                  
414900           ELSE                                                           
415000              MOVE SPACE           TO LOGG-IDTECKEN-KVLS                  
415100           END-IF                                                         
415200           PERFORM S22-ISRT-SALDOLOGG                                     
415300           IF SEND-DCS-CHINA                                              
415400              IF SEND-DCS-DDC                                             
415500                 MOVE W-KVSKROT(INDX) TO FILC2-KVANTAL                    
415600                 MOVE 'DAM'           TO FILC2-AVVIKELSETYP               
415700                 PERFORM S32-SKAPA-DIFF-TRANS                             
415800              END-IF                                                      
415900              MOVE W-KVAVIS        TO FILC3-KVAVIS                        
416000              MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                       
416100              MOVE 'DAM'           TO FILC3-AVVIKELSETYP                  
416200              MOVE 5               TO FILC3-KDSORT1                       
416300              MOVE DCS-FLINLREP    TO FILC3-FLINLREP                      
416400              PERFORM S33-SKAPA-LDC-TRANS                                 
416500           END-IF                                                         
416600        END-IF                                                            
416700        ADD W-KVANTMOT(INDX)       TO INL-KVANTMOT                        
416800        MOVE INL-KVANTMOT          TO W-KVANTMOT(INDX)                    
416900        ADD W-KVSKROT(INDX)        TO INL-KVART-SKROT                     
417000        COMPUTE WS-OLD-KVLS = SLAG-KVLS - INL-KVANTMOT                    
417100        END-COMPUTE                                                       
417200        MOVE SLAG-KVEFRS      TO WS-OLD-KVEFRS                            
417300     ELSE                                                                 
417400        MOVE SLAG-KVLS        TO WS-OLD-KVLS                              
417500        MOVE SLAG-KVEFRS      TO WS-OLD-KVEFRS                            
417600        ADD W-KVANTMOT(INDX)  TO SLAG-KVLS                                
417700*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
417800*   ---FÖR ATT UPPDATERA I WDL9                                           
417900        IF W-KVANTMOT(INDX) NOT = 0                                       
418000           PERFORM S21-SALDOLOGG-DATA                                     
418100           MOVE REQU-IDDC-KEY        TO LOGG-IDDC                         
418200           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
418300           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
418400           MOVE SLAG-KVLS          TO LOGG-KVLS                           
418500           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
418600           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
418700           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
418800           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
418900           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
419000           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
419100           PERFORM S22-ISRT-SALDOLOGG                                     
419200        END-IF                                                            
419300*   ---SALDFÖRÄNDRINGAR SLUT                                              
419400        IF SEND-DCS-CHINA                                                 
419500           ADD W-KVSKROT (INDX)    TO SLAG-KVLS                           
419600        END-IF                                                            
419700*  ---KOD FÖR SALDOUPPDATERING I WDL9                                     
419800        IF (W-KVSKROT(INDX) NOT = 0) AND SEND-DCS-CHINA                   
419900           PERFORM S21-SALDOLOGG-DATA                                     
420000           MOVE REQU-IDDC-KEY        TO LOGG-IDDC                         
420100           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
420200           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
420300           MOVE SLAG-KVLS          TO LOGG-KVLS                           
420400           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
420500           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
420600           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
420700           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
420800           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
420900           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
421000           PERFORM S22-ISRT-SALDOLOGG                                     
421100           IF SEND-DCS-DDC                                                
421200              MOVE W-KVSKROT(INDX) TO FILC2-KVANTAL                       
421300              MOVE 'DAM'           TO FILC2-AVVIKELSETYP                  
421400              PERFORM S32-SKAPA-DIFF-TRANS                                
421500           END-IF                                                         
421600           MOVE W-KVAVIS        TO FILC3-KVAVIS                           
421700           MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                          
421800           MOVE 'DAM'           TO FILC3-AVVIKELSETYP                     
421900           MOVE 5               TO FILC3-KDSORT1                          
422000           MOVE DCS-FLINLREP    TO FILC3-FLINLREP                         
422100           PERFORM S33-SKAPA-LDC-TRANS                                    
422200        END-IF                                                            
422300*  ---SALDOFÖRÄNDRING SLUT                                                
422400                                                                          
422500        SUBTRACT W-KVAVIS     FROM SLAG-KVAKS-SDC                         
422600        MOVE W-KVANTMOT(INDX) TO INL-KVANTMOT                             
422700        MOVE W-KVSKROT (INDX) TO INL-KVART-SKROT                          
422800     END-IF                                                               
422900                                                                          
423000                                                                          
423100*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
423200*   ---FÖR ATT UPPDATERA I WDL9                                           
423300     IF W-KVAVIS        NOT = 0                                           
423400        PERFORM S21-SALDOLOGG-DATA                                        
423500        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
423600        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
423700        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
423800        MOVE SLAG-KVLS          TO LOGG-KVLS                              
423900        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
424000        MOVE W-KVAVIS           TO LOGG-KVART-SALDO                       
424100        MOVE '-'                TO LOGG-IDTECKEN-KVAKS                    
424200        MOVE SPACE              TO LOGG-IDTECKEN-KVLS                     
424300        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
424400        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
424500        PERFORM S22-ISRT-SALDOLOGG                                        
424600     END-IF                                                               
424700* ---SALDOFÖRÄNDRINGAR SLUT                                               
424800                                                                          
424900     IF W-ADLAGOMR (INDX) > ZERO                                          
425000        MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                           
425100        MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                         
425200        MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                             
425300        MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                           
425400        MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                            
425500        MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                          
425600     END-IF                                                               
425700                                                                          
425800     ADD +1                TO W-KVRADER                                   
425900     MOVE 'R32'            TO INL-IDPTYP                                  
426000     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
426100        CONTINUE                                                          
426200     ELSE                                                                 
426300        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
426400     END-IF                                                               
426500     MOVE W-TIME-N         TO AKTUELL-TID                                 
426600     MOVE W-DAGENS-DATUM    TO INL-TIINLINL                               
426700     MOVE W-DAGENS-TID(1:4) TO INL-TIINLITI                               
426800                                                                          
426900     MOVE SLAG-ADLAGOMR    TO INL-ADLAGOMR                                
427000     MOVE SLAG-ADGANG      TO INL-ADGANG                                  
427100     MOVE SLAG-ADPLATS     TO INL-ADPLATS                                 
427200                                                                          
427300     PERFORM IMS-REPL-WLINLC11                                            
427400     PERFORM IMS-REPL-WDK711                                              
427500                                                                          
427600     PERFORM IMS-GU-WLARTC01                                              
427700     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
427800     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
427900     PERFORM IMS-GHNP-WLARTC11                                            
428000     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
428100     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
428200                                                                          
428300*  BERÄKNING AVGCOST                                                      
428400     MOVE W-KVANTMOT(INDX)   TO AVG-KVANTMOT                              
428500     PERFORM S10-OMRAKN-MEDELPRIS                                         
428600     MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                             
428700     MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                             
428800                                                                          
428900     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
429000     OR DIST35-VCC-NONVCC-REFILL                                          
429100     OR DIST35-VCC-NONVCC-TRANSFER                                        
429200        IF XDC-NON-VCC-OWNED                                              
429300**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
429400**** REST OF FLOWS TO US IS IN LAB.                                       
429500**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
429600**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
429700        OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                         
429800           IF W-KVANTMOT(INDX) > 0                                        
429900              MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                   
430000              PERFORM S19-EV-RO-TACKNING                                  
430100              PERFORM IMS-REPL-WDK711                                     
430200           END-IF                                                         
430300        END-IF                                                            
430400                                                                          
430500        IF W-KVSKROT(INDX) > 0                                            
430600          MOVE INL-PRARTNTO    TO R8-EKH-PRARTNTO                         
430700          PERFORM S31-SKAPA-SAP-TRANS-DAM                                 
430800        END-IF                                                            
430900                                                                          
431000        PERFORM HBC-NDC-KOLLI-DAMAGED                                     
431100                                                                          
431200        MOVE W-KVAVIS        TO FILC3-KVAVIS                              
431300        MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                             
431400        MOVE 'DAM'           TO FILC3-AVVIKELSETYP                        
431500        MOVE 5               TO FILC3-KDSORT1                             
431600        MOVE DCS-FLINLREP    TO FILC3-FLINLREP                            
431700        PERFORM S33-SKAPA-LDC-TRANS                                       
431800     END-IF                                                               
431900                                                                          
432000** WRITE WDK728 WHEN FLTRACK='J'                                          
432100     IF REQU-KVANTMOT-IN (INDX) NOT = ALL '+'                             
432200     AND REQU-KVANTMOT-IN (INDX) NOT = SPACE                              
432300        IF WS-FLTRACK = 'J'                                               
432400           MOVE W-KVANTMOT(INDX)  TO WS-TRCK-KVANTMOT                     
432500           MOVE W-KVAVIS          TO WS-TRCK-KVAVIS                       
432600           PERFORM S39-UPDATE-WDK728                                      
432700        END-IF                                                            
432800     END-IF                                                               
432900                                                                          
433000     IF (W-KVANTMOT (INDX) + W-KVSKROT (INDX))                            
433100                           NOT = W-KVAVIS                                 
433200        ADD W-KVANTMOT (INDX) W-KVSKROT (INDX)                            
433300                           GIVING W-TEMP-KVANT                            
433400        MOVE ZERO TO W-DIFF-KVANT                                         
433500        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
433600                               - W-TEMP-KVANT                             
433700        END-COMPUTE                                                       
433800                                                                          
433900        IF SEND-DCS-CHINA                                                 
434000           MOVE W-IDDC TO WS-SPARAT-IDDC                                  
434100           MOVE SEND-WS-IDDC TO W-IDDC                                    
434200           PERFORM IMS-GET-WDK711                                         
434300           MOVE SLAG-KVLS    TO WS-KVLS                                   
434400           COMPUTE SLAG-KVLS = SLAG-KVLS                                  
434500                             + (W-KVAVIS                                  
434600                             -  W-KVANTMOT (INDX)                         
434700                             -  W-KVSKROT  (INDX))                        
434800           END-COMPUTE                                                    
434900           PERFORM IMS-REPL-WDK711                                        
435000           MOVE WS-SPARAT-IDDC TO W-IDDC                                  
435100           PERFORM HBB-SALDOLOGG-DATA                                     
435200           PERFORM S37-SKAPA-NDC-HIST-SANDANDE                            
435300           MOVE INL-PRARTNTO          TO R8-EKH-PRARTNTO                  
435400           PERFORM S31-SKAPA-SAP-TRANS                                    
435500        ELSE                                                              
435600           PERFORM S17-NDC-OOVER-UNDER                                    
435700           MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                        
435800           MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                        
435900           MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                           
436000           PERFORM IMS-GHU-WLINLC11                                       
436100           MOVE INL-PRARTNTO          TO R8-EKH-PRARTNTO                  
436200           PERFORM S31-SKAPA-SAP-TRANS                                    
436300        END-IF                                                            
436400                                                                          
436500        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
436600        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
436700        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
436800        IF W-KVAVIS < W-TEMP-KVANT                                        
436900           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
437000           MOVE 3           TO FILC3-KDSORT1                              
437100        ELSE                                                              
437200           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
437300           MOVE 4           TO FILC3-KDSORT1                              
437400        END-IF                                                            
437500        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
437600        PERFORM S33-SKAPA-LDC-TRANS                                       
437700                                                                          
437800     END-IF                                                               
437900                                                                          
438000     IF SEND-DCS-CHINA                                                    
438100        MOVE NEJ TO NY-SKROT-SW                                           
438200        PERFORM S99-SKAPA-SKROT-ORDER                                     
438300     END-IF                                                               
438400                                                                          
438500     PERFORM S08-UPPDATERA-6301                                           
438600     .                                                                    
438700     EJECT                                                                
438800                                                                          
438900 HBA-SALDOLOGG-DATA SECTION.                                              
439000     PERFORM S21-SALDOLOGG-DATA                                           
439100     MOVE WC-CDC-SE           TO LOGG-IDDC                                
439200     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
439300     MOVE 'R34'               TO LOGG-IDSUBTYP                            
439400     MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                              
439500     MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
439600     MOVE CLAG-KVLS           TO LOGG-KVLS                                
439700     COMPUTE LOGG-KVAKS       =  CLAG-KVAKS-CDC                           
439800                              +  CLAG-KVAKS-T                             
439900     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
440000                              -  W-KVANTMOT(INDX)                         
440100                              -  W-KVSKROT(INDX)                          
440200     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
440300     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
440400     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
440500     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
440600                                                                          
440700     PERFORM S22-ISRT-SALDOLOGG                                           
440800     .                                                                    
440900     EJECT                                                                
441000                                                                          
441100 HBB-SALDOLOGG-DATA SECTION.                                              
441200     PERFORM S21-SALDOLOGG-DATA                                           
441300     MOVE W-IDDC-B6-SEND      TO LOGG-IDDC                                
441400     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
441500     MOVE 'R34'               TO LOGG-IDSUBTYP                            
441600     MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                              
441700     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
441800     MOVE SLAG-KVLS           TO LOGG-KVLS                                
441900     MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                               
442000     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
442100                              -  W-KVANTMOT(INDX)                         
442200                              -  W-KVSKROT(INDX)                          
442300     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
442400     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
442500     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
442600     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
442700                                                                          
442800     PERFORM S22-ISRT-SALDOLOGG                                           
442900     .                                                                    
443000     EJECT                                                                
443100                                                                          
443200 HBC-NDC-KOLLI-DAMAGED SECTION.                                           
443300     MOVE NEJ TO WS-FLYGORDER                                             
443400                 WS-BAATORDER                                             
443500                                                                          
443600     MOVE 6302-IDDC-SEND TO W-IDDC-WDB3                                   
443700                            W-IDDC-WDB3-DEF                               
443800     MOVE INL-IDDISTR    TO W-IDDISTR-WDB3                                
443900                            W-IDDISTR-WDB3-DEF                            
444000     MOVE 6302-IDKUNDNR  TO W-IDKUNDNR-WDB3                               
444100                                                                          
444200     PERFORM IMS-GU-WDB301                                                
444300     IF SEGMENT-FINNS                                                     
444400        IF W-KDFRAKT = DC-KDGENFRA-VOR                                    
444500           MOVE JA TO WS-FLYGORDER                                        
444600        ELSE                                                              
444700           IF W-KDFRAKT = DC-KDGENFRA-MO                                  
444800              MOVE JA TO WS-BAATORDER                                     
444900           END-IF                                                         
445000        END-IF                                                            
445100     END-IF                                                               
445200                                                                          
445300     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
445400        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
445500        PERFORM IMS-GU-WDB601-SEND                                        
445600     END-IF                                                               
445700                                                                          
445800*- KONTROLL AV FLYGORDER BORTTAGEN FÖR KINA ENLIGT SUSSI 20120412         
445900**** FLYGORDERKONTROLL SKALL EJ LÄGGAS TILL FÖR NA,GLOBAL EXPORT.         
446000     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
446100     OR DIST35-VCC-NONVCC-REFILL OR DIST35-NONVCC-CDC-REFILL              
446200     OR DIST35-VCC-NONVCC-TRANSFER                                        
446300       IF W-FLSKAKOL = 'J'                                                
446400          MOVE '63' TO 6308-KDANMORS                                      
446500       ELSE                                                               
446600          MOVE '43' TO 6308-KDANMORS                                      
446700       END-IF                                                             
446800       MOVE W-KVSKROT(INDX) TO 6308-KVLEVANM                              
446900       PERFORM S11-SKAPA-LEVANM-TRANS                                     
447000     END-IF                                                               
447100     .                                                                    
447200     EJECT                                                                
447300                                                                          
447400 HC-OOVER-UNDER-LEVERANS-PV SECTION.                                      
447500     MOVE ZERO               TO W-KVRADER                                 
447600     IF REQU-DAINLEV (INDX) NOT = ALL '+'                                 
447700      MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                               
447800                           W-IDARTNR-NYCKEL-SPAR                          
447900      MOVE REQU-DAINLEV (INDX) TO W-DAINLEV                               
448000                           W-DAINLEV-NYCKEL-SPAR                          
448100      MOVE REQU-IDDC-KEY TO W-IDDC                                        
448200                           W-IDDC-NYCKEL-SPAR                             
448300     ELSE                                                                 
448400      MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                           
448500      MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                           
448600      MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                        
448700                                 W-SEQA-IDFAKT-MAX                        
448800      MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                      
448900                                 W-SEQA-IDKUNDRF-MAX                      
449000      MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                      
449100                                 W-SEQA-IDKUNDNR-MAX                      
449200      MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                       
449300                                 W-SEQA-IDKOLLI-MAX                       
449400      MOVE '310'              TO W-IDPTYP                                 
449500      MOVE REQU-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                      
449600                                  W-SEQA-IDARTNR-MAX                      
449700      PERFORM IMS-GU-WLINLD01                                             
449800      IF SEGMENT-SAKNAS                                                   
449900        MOVE UPD-NOT-ALLOWED                                              
450000                         TO RESP-IDMSG-ERROR                              
450100        MOVE SPACES      TO RESP-IDELMT-ERROR                             
450200        PERFORM IMS-ROLLBACK                                              
450300        IF SUB-KDTRANS(1:6) = 'WLA103'                                    
450400          PERFORM S11-MSG-CONV                                            
450500        END-IF                                                            
450600        PERFORM S17-RETURNERA-SVAR                                        
450700        MOVE ZERO TO RETURN-CODE                                          
450800        GOBACK                                                            
450900      END-IF                                                              
451000      MOVE SEQA-IDARTNR TO W-IDARTNR                                      
451100                           W-IDARTNR-NYCKEL-SPAR                          
451200      MOVE SEQA-DAINLEV TO W-DAINLEV                                      
451300                           W-DAINLEV-NYCKEL-SPAR                          
451400      MOVE SEQA-IDDC    TO W-IDDC                                         
451500                           W-IDDC-NYCKEL-SPAR                             
451600     END-IF                                                               
451700                                                                          
451800     PERFORM IMS-GHU-WLINLC11                                             
451900     IF INL-IDPTYP NOT = '310'                                            
452000      MOVE UPD-NOT-ALLOWED                                                
452100                       TO RESP-IDMSG-ERROR                                
452200      MOVE SPACES      TO RESP-IDELMT-ERROR                               
452300      PERFORM IMS-ROLLBACK                                                
452400      IF SUB-KDTRANS(1:6) = 'WLA103'                                      
452500        PERFORM S11-MSG-CONV                                              
452600      END-IF                                                              
452700      PERFORM S17-RETURNERA-SVAR                                          
452800      MOVE ZERO TO RETURN-CODE                                            
452900      GOBACK                                                              
453000     END-IF                                                               
453100     IF DCS-CDC                                                           
453200        CONTINUE                                                          
453300     ELSE                                                                 
453400        PERFORM IMS-GET-WDK711                                            
453500     END-IF                                                               
453600                                                                          
453700     PERFORM IMS-GU-WLARTC01                                              
453800     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
453900     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
454000     PERFORM IMS-GHNP-WLARTC11                                            
454100     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
454200     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
454300                                                                          
454400     MOVE INL-KVAVIS   TO W-KVAVIS                                        
454500     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
454600     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
454700     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
454800                                                                          
454900     IF W-ADLAGOMR (INDX) > ZERO                                          
455000      IF DCS-CDC                                                          
455100        IF W-ADLAGOMR(INDX) = CLAG-ADLAGOMR                               
455200        AND W-ADGANG(INDX) = CLAG-ADGANG                                  
455300        AND W-ADPLATS(INDX) = CLAG-ADPLATS                                
455400           CONTINUE                                                       
455500        ELSE                                                              
455600          MOVE W-ADLAGOMR (INDX) TO CLAG-ADLAGOMR                         
455700          MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                       
455800          MOVE W-ADGANG (INDX)   TO CLAG-ADGANG                           
455900          MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                         
456000          MOVE W-ADPLATS (INDX)  TO CLAG-ADPLATS                          
456100          MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                        
456200          PERFORM S30-UPPDATERA-WDJ9                                      
456300        END-IF                                                            
456400      ELSE                                                                
456500        IF W-ADLAGOMR(INDX) = SLAG-ADLAGOMR                               
456600        AND W-ADGANG(INDX) = SLAG-ADGANG                                  
456700        AND W-ADPLATS(INDX) = SLAG-ADPLATS                                
456800           CONTINUE                                                       
456900        ELSE                                                              
457000          MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                         
457100          MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                       
457200          MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                           
457300          MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                         
457400          MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                          
457500          MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                        
457600          PERFORM S30-UPPDATERA-WDJ9                                      
457700        END-IF                                                            
457800      END-IF                                                              
457900     END-IF                                                               
458000                                                                          
458100     IF INL-KVANTMOT > ZERO                                               
458200        IF DCS-CDC                                                        
458300           SUBTRACT W-KVANTMOT(INDX)                                      
458400                                FROM CLAG-KVAKS-CDC                       
458500           ADD W-KVANTMOT(INDX)   TO CLAG-KVLS                            
458600        ELSE                                                              
458700          SUBTRACT W-KVANTMOT(INDX)  FROM SLAG-KVAKS-SDC                  
458800          ADD W-KVANTMOT(INDX)       TO SLAG-KVLS                         
458900          COMPUTE WS-OLD-KVLS =  SLAG-KVLS - INL-KVANTMOT                 
459000          END-COMPUTE                                                     
459100          MOVE SLAG-KVEFRS           TO WS-OLD-KVEFRS                     
459200        END-IF                                                            
459300        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
459400        ADD W-KVANTMOT(INDX)      TO INL-KVANTMOT                         
459500        MOVE INL-KVANTMOT         TO W-KVANTMOT(INDX)                     
459600        PERFORM S21-SALDOLOGG-DATA                                        
459700        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
459800        IF DCS-CDC                                                        
459900           MOVE CLAG-KVLS         TO LOGG-KVLS                            
460000           COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                       
460100                                   + CLAG-KVAKS-T                         
460200           MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                          
460300           MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
460400        ELSE                                                              
460500           MOVE SLAG-KVLS            TO LOGG-KVLS                         
460600           MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                        
460700           MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                       
460800           MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                    
460900        END-IF                                                            
461000        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
461100        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
461200        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
461300        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
461400        PERFORM S22-ISRT-SALDOLOGG                                        
461500     ELSE                                                                 
461600        IF DCS-CDC                                                        
461700           ADD W-KVANTMOT(INDX)    TO CLAG-KVLS                           
461800        ELSE                                                              
461900           MOVE SLAG-KVLS          TO WS-OLD-KVLS                         
462000           MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                       
462100           ADD W-KVANTMOT(INDX)    TO SLAG-KVLS                           
462200        END-IF                                                            
462300        MOVE W-KVANTMOT(INDX)      TO INL-KVANTMOT                        
462400        PERFORM S21-SALDOLOGG-DATA                                        
462500        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
462600        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
462700        IF DCS-CDC                                                        
462800           MOVE CLAG-KVLS         TO LOGG-KVLS                            
462900           COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                       
463000                                   + CLAG-KVAKS-T                         
463100           MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                          
463200           MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
463300        ELSE                                                              
463400           MOVE SLAG-KVLS         TO LOGG-KVLS                            
463500           MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                           
463600           MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                          
463700           MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
463800        END-IF                                                            
463900        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
464000        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
464100        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
464200        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                  
464300        PERFORM S22-ISRT-SALDOLOGG                                        
464400        IF DCS-CDC                                                        
464500           SUBTRACT W-KVAVIS       FROM CLAG-KVAKS-CDC                    
464600        ELSE                                                              
464700           SUBTRACT W-KVAVIS       FROM SLAG-KVAKS-SDC                    
464800        END-IF                                                            
464900        PERFORM S21-SALDOLOGG-DATA                                        
465000        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
465100        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
465200        IF DCS-CDC                                                        
465300           MOVE CLAG-KVLS         TO LOGG-KVLS                            
465400           COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                       
465500                                   + CLAG-KVAKS-T                         
465600           MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                          
465700           MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
465800        ELSE                                                              
465900           MOVE SLAG-KVLS         TO LOGG-KVLS                            
466000           MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                           
466100           MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                          
466200           MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
466300        END-IF                                                            
466400        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
466500        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
466600        MOVE SPACE                TO LOGG-IDTECKEN-KVLS                   
466700        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
466800        PERFORM S22-ISRT-SALDOLOGG                                        
466900     END-IF                                                               
467000                                                                          
467100     ADD +1                TO W-KVRADER                                   
467200     MOVE 'R32'            TO INL-IDPTYP                                  
467300     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
467400        CONTINUE                                                          
467500     ELSE                                                                 
467600        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
467700     END-IF                                                               
467800     MOVE W-TIME-N         TO AKTUELL-TID                                 
467900     MOVE W-DAGENS-DATUM   TO INL-TIINLINL                                
468000     MOVE AKTUELL-TTMM     TO INL-TIINLITI                                
468100     IF DCS-CDC                                                           
468200        MOVE CLAG-ADLAGOMR TO INL-ADLAGOMR                                
468300        MOVE CLAG-ADGANG   TO INL-ADGANG                                  
468400        MOVE CLAG-ADPLATS  TO INL-ADPLATS                                 
468500     ELSE                                                                 
468600        MOVE SLAG-ADLAGOMR TO INL-ADLAGOMR                                
468700        MOVE SLAG-ADGANG   TO INL-ADGANG                                  
468800        MOVE SLAG-ADPLATS  TO INL-ADPLATS                                 
468900     END-IF                                                               
469000                                                                          
469100     PERFORM IMS-REPL-WLINLC11                                            
469200                                                                          
469300     IF DCS-AUSTRALIA OR DCS-JAPAN                                        
469400        MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                         
469500        PERFORM S19-EV-RO-TACKNING                                        
469600     END-IF                                                               
469700     IF DCS-CDC                                                           
469800        MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                         
469900        PERFORM S19-EV-RO-TACKNING-CDC                                    
470000     END-IF                                                               
470100                                                                          
470200     IF DCS-NDC-NA                                                        
470300        PERFORM S09-SKAPA-EKOTRANS-A03                                    
470400        MOVE SPACE              TO EKOTRA03-KDANMORS                      
470500        MOVE SLAG-PRAVCOST      TO EKOTRA03-PRAVCOST-OLD                  
470600        MOVE W-KVANTMOT(INDX)   TO AVG-KVANTMOT                           
470700                                   EKOTRA03-KVANTMOT                      
470800        MOVE 'TRE'  TO STEXT                                              
470900        PERFORM S10-OMRAKN-MEDELPRIS-NA                                   
471000        IF W-KVANTMOT(INDX) > 0                                           
471100           MOVE AVG-PRAVCOST-NEW      TO SLAG-PRAVCOST                    
471200                                         EKOTRA03-PRAVCOST                
471300           MOVE MSGI-TILOKDAT         TO SLAG-TIAVCOST                    
471400        ELSE                                                              
471500           MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST                
471600        END-IF                                                            
471700        MOVE AVG-REMARKUP       TO EKOTRA03-REMARKUP                      
471800        MOVE ZERO               TO EKOTRA03-KVSKROT                       
471900                                                                          
472000        MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                         
472100        PERFORM S19-EV-RO-TACKNING                                        
472200     END-IF                                                               
472300     IF DCS-CDC                                                           
472400        PERFORM IMS-REPL-WLARTC11                                         
472500     ELSE                                                                 
472600        PERFORM IMS-REPL-WDK711                                           
472700     END-IF                                                               
472800                                                                          
472900** WRITE WDK728 WHEN FLTRACK='J'                                          
473000     IF WS-FLTRACK = 'J'                                                  
473100        MOVE W-KVANTMOT(INDX)  TO WS-TRCK-KVANTMOT                        
473200        MOVE W-KVAVIS          TO WS-TRCK-KVAVIS                          
473300        PERFORM S39-UPDATE-WDK728                                         
473400     END-IF                                                               
473500                                                                          
473600     IF W-KVANTMOT (INDX) NOT = W-KVAVIS                                  
473700        MOVE W-KVANTMOT (INDX) TO W-TEMP-KVANT                            
473800        MOVE ZERO TO W-DIFF-KVANT                                         
473900        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
474000                               - W-TEMP-KVANT                             
474100        END-COMPUTE                                                       
474200                                                                          
474300        IF DCS-NDC-NA                                                     
474400           MOVE NEJ TO WS-A03-SKAPAD                                      
474500           PERFORM S17-NDC-OOVER-UNDER                                    
474600           MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                        
474700           MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                        
474800           MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                           
474900           PERFORM IMS-GHU-WLINLC11                                       
475000           PERFORM S09-SKAPA-EKOTRANS-A03                                 
475100        ELSE                                                              
475200           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
475300           OR DIST35-NONVCC-VCC-REFILL                                    
475400           OR DIST35-NONVCC-VCC-TRANSFER                                  
475500              IF SEND-DCS-CDC                                             
475600                 PERFORM IMS-GHU-WLARTC11                                 
475700                 MOVE CLAG-KVLS    TO WS-KVLS                             
475800                 COMPUTE CLAG-KVLS = CLAG-KVLS                            
475900                                   + (W-KVAVIS                            
476000                                   -  W-KVANTMOT (INDX))                  
476100                 END-COMPUTE                                              
476200                 PERFORM IMS-REPL-WLARTC11                                
476300                 PERFORM HCA-SALDOLOGG-DATA                               
476400                 PERFORM S07-SKAPA-HISTORIK                               
476500              ELSE                                                        
476600                IF SEND-DCS-DDC                                           
476700                   MOVE W-DIFF-KVANT       TO WS-DIFF-KVANT               
476800                   MOVE WS-DIFF-KVANT      TO FILC2-KVANTAL               
476900                   IF W-KVAVIS < W-TEMP-KVANT                             
477000                      MOVE 'ÖVERLEV.'      TO FILC2-AVVIKELSETYP          
477100                   ELSE                                                   
477200                      MOVE 'UNDERLEV.'     TO FILC2-AVVIKELSETYP          
477300                   END-IF                                                 
477400                   PERFORM S32-SKAPA-DIFF-TRANS                           
477500                 END-IF                                                   
477600              END-IF                                                      
477700              PERFORM S31-SKAPA-SAP-TRANS-VCCS                            
477800           ELSE                                                           
477900              IF DCS-CDC                                                  
478000                IF SEND-DCS-NDC-CN OR SEND-DCS-USA                        
478100                  PERFORM S31-SKAPA-SAP-TRANS-VCCS                        
478200                END-IF                                                    
478300              END-IF                                                      
478400           END-IF                                                         
478500                                                                          
478600           IF DIST35-NONVCC-VCC-REFILL OR DCS-CDC OR                      
478700              DIST35-NONVCC-VCC-TRANSFER                                  
478800            IF W-KVAVIS < W-TEMP-KVANT                                    
478900               MOVE '11' TO 6308-KDANMORS                                 
479000            ELSE                                                          
479100               MOVE '00' TO 6308-KDANMORS                                 
479200            END-IF                                                        
479300            MOVE W-DIFF-KVANT TO 6308-KVLEVANM                            
479400            PERFORM S11-SKAPA-LEVANM-TRANS                                
479500           END-IF                                                         
479600                                                                          
479700        END-IF                                                            
479800********                                                                  
479900        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
480000        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
480100        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
480200        IF W-KVAVIS < W-TEMP-KVANT                                        
480300           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
480400           MOVE 3           TO FILC3-KDSORT1                              
480500        ELSE                                                              
480600           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
480700           MOVE 4           TO FILC3-KDSORT1                              
480800        END-IF                                                            
480900        PERFORM S33-SKAPA-LDC-TRANS                                       
481000********                                                                  
481100        IF SEND-DCS-AUSTRALIA OR SEND-DCS-JAPAN OR SEND-DCS-SDC           
481200            MOVE W-IDDC TO WS-SPARAT-IDDC                                 
481300            MOVE W-IDDC-B6-SEND TO W-IDDC                                 
481400            PERFORM IMS-GET-WDK711                                        
481500            MOVE SLAG-KVLS    TO WS-KVLS                                  
481600            COMPUTE SLAG-KVLS = SLAG-KVLS                                 
481700                              + (W-KVAVIS                                 
481800                   -  W-KVANTMOT (INDX)                                   
481900                   -  W-KVSKROT  (INDX))                                  
482000            END-COMPUTE                                                   
482100            PERFORM IMS-REPL-WDK711                                       
482200            MOVE WS-SPARAT-IDDC TO W-IDDC                                 
482300            PERFORM HCB-SALDOLOGG-DATA                                    
482400            PERFORM S13-SKAPA-NDC-HIST-SANDANDE                           
482500            PERFORM S31-SKAPA-SAP-TRANS-VCCS                              
482600        END-IF                                                            
482700************* 990127                                                      
482800     END-IF                                                               
482900                                                                          
483000     PERFORM S08-UPPDATERA-6301                                           
483100     .                                                                    
483200     EJECT                                                                
483300                                                                          
483400 HC-OOVER-UNDER-LEVERANS SECTION.                                         
483500     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
483600        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
483700        PERFORM IMS-GU-WDB601-SEND                                        
483800     END-IF                                                               
483900                                                                          
484000     MOVE ZERO               TO W-KVRADER                                 
484100     IF REQU-DAINLEV (INDX) NOT = ALL '+'                                 
484200      MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                               
484300                          W-IDARTNR-NYCKEL-SPAR                           
484400      MOVE REQU-DAINLEV (INDX) TO W-DAINLEV                               
484500                          W-DAINLEV-NYCKEL-SPAR                           
484600      MOVE REQU-IDDC-KEY TO W-IDDC                                        
484700                          W-IDDC-NYCKEL-SPAR                              
484800     ELSE                                                                 
484900      MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                           
485000      MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                           
485100      MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                        
485200                                 W-SEQA-IDFAKT-MAX                        
485300      MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                      
485400                                 W-SEQA-IDKUNDRF-MAX                      
485500      MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                      
485600                                 W-SEQA-IDKUNDNR-MAX                      
485700      MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                       
485800                                 W-SEQA-IDKOLLI-MAX                       
485900      MOVE '310'              TO W-IDPTYP                                 
486000      MOVE REQU-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                      
486100                                  W-SEQA-IDARTNR-MAX                      
486200      PERFORM IMS-GU-WLINLD01                                             
486300      IF SEGMENT-SAKNAS                                                   
486400        MOVE UPD-NOT-ALLOWED                                              
486500                         TO RESP-IDMSG-ERROR                              
486600        MOVE SPACES      TO RESP-IDELMT-ERROR                             
486700        PERFORM IMS-ROLLBACK                                              
486800        IF SUB-KDTRANS(1:6) = 'WLA103'                                    
486900          PERFORM S11-MSG-CONV                                            
487000        END-IF                                                            
487100        PERFORM S17-RETURNERA-SVAR                                        
487200        MOVE ZERO TO RETURN-CODE                                          
487300        GOBACK                                                            
487400      END-IF                                                              
487500      MOVE SEQA-IDARTNR TO W-IDARTNR                                      
487600                           W-IDARTNR-NYCKEL-SPAR                          
487700      MOVE SEQA-DAINLEV TO W-DAINLEV                                      
487800                           W-DAINLEV-NYCKEL-SPAR                          
487900      MOVE SEQA-IDDC    TO W-IDDC                                         
488000                           W-IDDC-NYCKEL-SPAR                             
488100     END-IF                                                               
488200     PERFORM IMS-GHU-WLINLC11                                             
488300     IF INL-IDPTYP NOT = '310'                                            
488400      MOVE UPD-NOT-ALLOWED                                                
488500                       TO RESP-IDMSG-ERROR                                
488600      MOVE SPACES      TO RESP-IDELMT-ERROR                               
488700      PERFORM IMS-ROLLBACK                                                
488800      IF SUB-KDTRANS(1:6) = 'WLA103'                                      
488900        PERFORM S11-MSG-CONV                                              
489000      END-IF                                                              
489100      PERFORM S17-RETURNERA-SVAR                                          
489200      MOVE ZERO TO RETURN-CODE                                            
489300      GOBACK                                                              
489400     END-IF                                                               
489500     PERFORM IMS-GET-WDK711                                               
489600                                                                          
489700     MOVE INL-KVAVIS   TO W-KVAVIS                                        
489800     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
489900     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
490000     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
490100     MOVE INL-IDORDNR5 TO W-IDORDER                                       
490200     MOVE INL-IDKOLLI  TO W-IDKOLLI                                       
490300                                                                          
490400     IF W-ADLAGOMR (INDX) > ZERO                                          
490500                                                                          
490600        IF W-ADLAGOMR (INDX) =  SLAG-ADLAGOMR AND                         
490700           W-ADGANG   (INDX) =  SLAG-ADGANG   AND                         
490800           W-ADPLATS  (INDX) =  SLAG-ADPLATS                              
490900           CONTINUE                                                       
491000        ELSE                                                              
491100           MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                        
491200                                     W-ADLAGOMR-LOCB                      
491300           MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                          
491400                                     W-ADGANG-LOCB                        
491500           MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                         
491600                                     W-ADPLATS-LOCB                       
491700           PERFORM S30-UPPDATERA-WDJ9                                     
491800        END-IF                                                            
491900     END-IF                                                               
492000                                                                          
492100     IF INL-KVANTMOT > ZERO                                               
492200        SUBTRACT W-KVANTMOT(INDX) FROM SLAG-KVAKS-SDC                     
492300        ADD W-KVANTMOT(INDX)      TO SLAG-KVLS                            
492400        COMPUTE WS-OLD-KVLS     = SLAG-KVLS - INL-KVANTMOT                
492500        END-COMPUTE                                                       
492600        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
492700        ADD W-KVANTMOT(INDX)      TO INL-KVANTMOT                         
492800        MOVE INL-KVANTMOT         TO W-KVANTMOT(INDX)                     
492900        MOVE SLAG-KVEFRS          TO WS-OLD-KVEFRS                        
493000*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
493100        PERFORM S21-SALDOLOGG-DATA                                        
493200        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
493300        MOVE SLAG-KVLS            TO LOGG-KVLS                            
493400        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
493500        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
493600        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
493700        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
493800        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
493900        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
494000        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
494100        PERFORM S22-ISRT-SALDOLOGG                                        
494200*   ---SALDOFÖRÄNDRINGAR SLUT                                             
494300     ELSE                                                                 
494400        MOVE SLAG-KVLS             TO WS-OLD-KVLS                         
494500        MOVE SLAG-KVEFRS           TO WS-OLD-KVEFRS                       
494600        MOVE W-KVANTMOT(INDX)      TO INL-KVANTMOT                        
494700        ADD W-KVANTMOT(INDX)       TO SLAG-KVLS                           
494800*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
494900        PERFORM S21-SALDOLOGG-DATA                                        
495000        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
495100        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
495200        MOVE SLAG-KVLS            TO LOGG-KVLS                            
495300        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
495400        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
495500        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
495600        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
495700        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
495800        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
495900        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                  
496000        PERFORM S22-ISRT-SALDOLOGG                                        
496100*   ---SALDOFÖRÄNDRINGAR SLUT                                             
496200                                                                          
496300        SUBTRACT W-KVAVIS          FROM SLAG-KVAKS-SDC                    
496400                                                                          
496500*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
496600        PERFORM S21-SALDOLOGG-DATA                                        
496700        MOVE REQU-IDDC-KEY        TO LOGG-IDDC                            
496800        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
496900        MOVE SLAG-KVLS            TO LOGG-KVLS                            
497000        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
497100        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
497200        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
497300        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
497400        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
497500        MOVE SPACE                TO LOGG-IDTECKEN-KVLS                   
497600        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
497700        PERFORM S22-ISRT-SALDOLOGG                                        
497800*   ---SALDOFÖRÄNDRINGAR SLUT                                             
497900     END-IF                                                               
498000                                                                          
498100     ADD +1                TO W-KVRADER                                   
498200     MOVE 'R32'            TO INL-IDPTYP                                  
498300     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
498400        CONTINUE                                                          
498500     ELSE                                                                 
498600        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
498700     END-IF                                                               
498800     MOVE W-TIME-N         TO AKTUELL-TID                                 
498900     MOVE W-DAGENS-DATUM    TO INL-TIINLINL                               
499000     MOVE W-DAGENS-TID(1:4) TO INL-TIINLITI                               
499100                                                                          
499200     MOVE SLAG-ADLAGOMR    TO INL-ADLAGOMR                                
499300     MOVE SLAG-ADGANG      TO INL-ADGANG                                  
499400     MOVE SLAG-ADPLATS     TO INL-ADPLATS                                 
499500                                                                          
499600     PERFORM IMS-REPL-WLINLC11                                            
499700                                                                          
499800                                                                          
499900     PERFORM IMS-GU-WLARTC01                                              
500000     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
500100     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
500200     PERFORM IMS-GHNP-WLARTC11                                            
500300     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
500400     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
500500                                                                          
500600     IF ((SEND-DCS-CDC OR SEND-DCS-DDC) AND XDC-NON-VCC-OWNED)            
500700                                                                          
500800        MOVE W-KVANTMOT(INDX)      TO WS-RO-KVANTMOT                      
500900        PERFORM S19-EV-RO-TACKNING                                        
501000     END-IF                                                               
501100                                                                          
501200* BERÄKNING AVGCOST                                                       
501300     MOVE W-KVANTMOT(INDX)   TO AVG-KVANTMOT                              
501400     PERFORM S10-OMRAKN-MEDELPRIS                                         
501500     IF W-KVANTMOT(INDX) > 0                                              
501600       MOVE AVG-PRAVCOST-NEW      TO SLAG-PRAVCOST                        
501700       MOVE MSGI-TILOKDAT         TO SLAG-TIAVCOST                        
501800     END-IF                                                               
501900                                                                          
502000     PERFORM IMS-REPL-WDK711                                              
502100                                                                          
502200** WRITE WDK728 WHEN FLTRACK='J'                                          
502300     IF WS-FLTRACK = 'J'                                                  
502400        MOVE W-KVANTMOT(INDX)  TO WS-TRCK-KVANTMOT                        
502500        MOVE W-KVAVIS          TO WS-TRCK-KVAVIS                          
502600        PERFORM S39-UPDATE-WDK728                                         
502700     END-IF                                                               
502800                                                                          
502900     IF W-KVANTMOT (INDX) NOT = W-KVAVIS                                  
503000        MOVE W-KVANTMOT (INDX) TO W-TEMP-KVANT                            
503100        MOVE ZERO TO W-DIFF-KVANT                                         
503200        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
503300                               - W-TEMP-KVANT                             
503400        END-COMPUTE                                                       
503500                                                                          
503600        IF SEND-DCS-CHINA                                                 
503700           MOVE W-IDDC TO WS-SPARAT-IDDC                                  
503800           MOVE SEND-WS-IDDC TO W-IDDC                                    
503900           PERFORM IMS-GET-WDK711                                         
504000           MOVE SLAG-KVLS    TO WS-KVLS                                   
504100           COMPUTE SLAG-KVLS = SLAG-KVLS                                  
504200                             + (W-KVAVIS                                  
504300                             -  W-KVANTMOT (INDX))                        
504400           END-COMPUTE                                                    
504500           PERFORM IMS-REPL-WDK711                                        
504600           MOVE WS-SPARAT-IDDC TO W-IDDC                                  
504700           PERFORM HCB-SALDOLOGG-DATA                                     
504800           PERFORM S37-SKAPA-NDC-HIST-SANDANDE                            
504900           MOVE INL-PRARTNTO          TO R8-EKH-PRARTNTO                  
505000           PERFORM S31-SKAPA-SAP-TRANS                                    
505100        ELSE                                                              
505200           PERFORM S17-NDC-OOVER-UNDER                                    
505300           MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                        
505400           MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                        
505500           MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                           
505600           PERFORM IMS-GHU-WLINLC11                                       
505700           MOVE INL-PRARTNTO          TO R8-EKH-PRARTNTO                  
505800           PERFORM S31-SKAPA-SAP-TRANS                                    
505900        END-IF                                                            
506000                                                                          
506100        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
506200        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
506300        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
506400        IF W-KVAVIS < W-TEMP-KVANT                                        
506500           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
506600           MOVE 3           TO FILC3-KDSORT1                              
506700        ELSE                                                              
506800           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
506900           MOVE 4           TO FILC3-KDSORT1                              
507000        END-IF                                                            
507100        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
507200        PERFORM S33-SKAPA-LDC-TRANS                                       
507300     END-IF                                                               
507400                                                                          
507500     PERFORM S08-UPPDATERA-6301                                           
507600     .                                                                    
507700     EJECT                                                                
507800                                                                          
507900 HCA-SALDOLOGG-DATA SECTION.                                              
508000     PERFORM S21-SALDOLOGG-DATA                                           
508100     MOVE WC-CDC-SE           TO LOGG-IDDC                                
508200     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
508300     MOVE 'R34'               TO LOGG-IDSUBTYP                            
508400     MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                              
508500     MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
508600     MOVE CLAG-KVLS           TO LOGG-KVLS                                
508700     COMPUTE LOGG-KVAKS       =  CLAG-KVAKS-CDC                           
508800                              +  CLAG-KVAKS-T                             
508900     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
509000                              -  W-KVANTMOT(INDX)                         
509100     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
509200     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
509300     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
509400     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
509500                                                                          
509600     PERFORM S22-ISRT-SALDOLOGG                                           
509700     .                                                                    
509800     EJECT                                                                
509900                                                                          
510000 HCB-SALDOLOGG-DATA SECTION.                                              
510100     PERFORM S21-SALDOLOGG-DATA                                           
510200     MOVE W-IDDC-B6-SEND      TO LOGG-IDDC                                
510300     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
510400     MOVE 'R34'               TO LOGG-IDSUBTYP                            
510500     MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                              
510600     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
510700     MOVE SLAG-KVLS           TO LOGG-KVLS                                
510800     MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                               
510900     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
511000                              -  W-KVANTMOT(INDX)                         
511100     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
511200     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
511300     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
511400     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
511500                                                                          
511600     PERFORM S22-ISRT-SALDOLOGG                                           
511700     .                                                                    
511800     EJECT                                                                
511900                                                                          
512000 HD-NYUPPLAEGG-PV SECTION.                                                
512100     MOVE ZERO            TO W-KVRADER                                    
512200     ADD +1               TO W-KVRADER                                    
512300     MOVE JA              TO NYUPPLAEGG-SW                                
512400     MOVE W-IDARTNR-INM   TO W-IDARTNR                                    
512500     MOVE REQU-IDDC-KEY   TO W-IDDC                                       
512600     IF DCS-CDC                                                           
512700        PERFORM IMS-GHU-WLARTC11                                          
512800        MOVE CLAG-ADLAGOMR   TO W-ADLAGOMR-SPAR                           
512900        MOVE CLAG-ADGANG     TO W-ADGANG-SPAR                             
513000        MOVE CLAG-ADPLATS    TO W-ADPLATS-SPAR                            
513100     ELSE                                                                 
513200        PERFORM IMS-GET-WDK711                                            
513300        MOVE SLAG-ADLAGOMR   TO W-ADLAGOMR-SPAR                           
513400        MOVE SLAG-ADGANG     TO W-ADGANG-SPAR                             
513500        MOVE SLAG-ADPLATS    TO W-ADPLATS-SPAR                            
513600     END-IF                                                               
513700                                                                          
513800     IF REQU-ADLAGOMR-INM = ALL '+' OR SPACE                              
513900        CONTINUE                                                          
514000     ELSE                                                                 
514100        MOVE W-ADLAGOMR-INM  TO W-ADLAGOMR-SPAR                           
514200                                W-ADLAGOMR-LOCB                           
514300     END-IF                                                               
514400     IF REQU-ADGANG-INM = ALL '+' OR SPACE                                
514500        CONTINUE                                                          
514600     ELSE                                                                 
514700        MOVE W-ADGANG-INM    TO W-ADGANG-SPAR                             
514800                                W-ADGANG-LOCB                             
514900     END-IF                                                               
515000     IF REQU-ADPLATS-INM = ALL '+' OR SPACE                               
515100        CONTINUE                                                          
515200     ELSE                                                                 
515300        MOVE W-ADPLATS-INM   TO W-ADPLATS-SPAR                            
515400                                W-ADPLATS-LOCB                            
515500     END-IF                                                               
515600                                                                          
515700     IF DCS-CDC                                                           
515800        MOVE W-ADLAGOMR-SPAR TO CLAG-ADLAGOMR                             
515900        MOVE W-ADGANG-SPAR   TO CLAG-ADGANG                               
516000        MOVE W-ADPLATS-SPAR  TO CLAG-ADPLATS                              
516100     ELSE                                                                 
516200        MOVE W-ADLAGOMR-SPAR TO SLAG-ADLAGOMR                             
516300        MOVE W-ADGANG-SPAR   TO SLAG-ADGANG                               
516400        MOVE W-ADPLATS-SPAR  TO SLAG-ADPLATS                              
516500     END-IF                                                               
516600                                                                          
516700     IF W-ADLAGOMR-LOCB NUMERIC                                           
516800     AND W-ADGANG-LOCB NUMERIC                                            
516900     AND W-ADPLATS-LOCB NUMERIC                                           
517000        IF W-ADLAGOMR-LOCB > ZERO                                         
517100        OR W-ADGANG-LOCB > ZERO                                           
517200        OR W-ADPLATS-LOCB > ZERO                                          
517300           PERFORM S30-UPPDATERA-WDJ9                                     
517400        END-IF                                                            
517500     END-IF                                                               
517600                                                                          
517700     MOVE WS-KVANTMOT-INM    TO WS-KVANTMOT-INM-NUM                       
517800     MOVE WS-KVSKROT-INM     TO WS-KVSKROT-INM-NUM                        
517900     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM                        
518000             + WS-KVSKROT-INM-NUM)                                        
518100     END-COMPUTE                                                          
518200     IF DCS-CDC                                                           
518300      ADD WS-SUMMA-KVANT      TO CLAG-KVLS                                
518400     ELSE                                                                 
518500      MOVE SLAG-KVLS          TO WS-OLD-KVLS                              
518600      MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                            
518700      ADD WS-SUMMA-KVANT      TO SLAG-KVLS                                
518800     END-IF                                                               
518900     PERFORM S21-SALDOLOGG-DATA                                           
519000     MOVE REQU-IDDC-KEY        TO LOGG-IDDC                               
519100     MOVE WS-SUMMA-KVANT       TO LOGG-KVART-SALDO                        
519200     IF DCS-CDC                                                           
519300        MOVE CLAG-KVLS         TO LOGG-KVLS                               
519400        COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                          
519500                                + CLAG-KVAKS-T                            
519600        MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                             
519700        MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                          
519800     ELSE                                                                 
519900        MOVE SLAG-KVLS         TO LOGG-KVLS                               
520000        MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                              
520100        MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                             
520200        MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                          
520300     END-IF                                                               
520400     MOVE '+'                  TO LOGG-IDTECKEN-KVLS                      
520500     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                     
520600     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
520700     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
520800     PERFORM S22-ISRT-SALDOLOGG                                           
520900                                                                          
521000     IF DCS-NDC-NA                                                        
521100        SUBTRACT WS-KVSKROT-INM-NUM FROM SLAG-KVLS                        
521200        MOVE WS-KVANTMOT-INM-NUM TO WS-RO-KVANTMOT                        
521300        PERFORM S19-EV-RO-TACKNING                                        
521400        PERFORM S21-SALDOLOGG-DATA                                        
521500        MOVE REQU-IDDC-KEY      TO LOGG-IDDC                              
521600        MOVE WS-KVSKROT-INM-NUM TO LOGG-KVART-SALDO                       
521700        MOVE SLAG-KVLS          TO LOGG-KVLS                              
521800        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
521900        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
522000        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
522100        MOVE '-'                TO LOGG-IDTECKEN-KVLS                     
522200        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                    
522300        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
522400        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
522500        PERFORM S22-ISRT-SALDOLOGG                                        
522600* ---SLUT SALDOFÖRÄNDRINGAR                                               
522700     END-IF                                                               
522800     IF DCS-CDC                                                           
522900        PERFORM IMS-REPL-WLARTC11                                         
523000     ELSE                                                                 
523100        PERFORM IMS-REPL-WDK711                                           
523200     END-IF                                                               
523300     MOVE WS-SUMMA-KVANT     TO W-TEMP-KVANT                              
523400     MOVE ZERO               TO W-KVAVIS                                  
523500                                                                          
523600** WRITE WDK728 WHEN FLTRACK='J'                                          
523700     IF REQU-KVANTMOT-INM NOT = ALL '+'  OR SPACE                         
523800        IF WS-FLTRACK = 'J'                                               
523900           MOVE WS-KVANTMOT-INM-NUM TO WS-TRCK-KVANTMOT                   
524000           MOVE ZERO                TO WS-TRCK-KVAVIS                     
524100           MOVE W-IDTRACK           TO TINL-IDTRACK                       
524200           PERFORM S39-UPDATE-WDK728                                      
524300        END-IF                                                            
524400     END-IF                                                               
524500                                                                          
524600     IF DCS-NDC-NA                                                        
524700        MOVE DCS-FLINLREP      TO FILC3-FLINLREP                          
524800        MOVE ZERO              TO FILC3-KVAVIS                            
524900        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
525000        MOVE 'NY'              TO FILC3-AVVIKELSETYP                      
525100        MOVE 6                 TO FILC3-KDSORT1                           
525200        PERFORM S33-SKAPA-LDC-TRANS                                       
525300        IF WS-KVSKROT-INM-NUM > ZERO                                      
525400           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
525500           MOVE 'DAM'                 TO FILC3-AVVIKELSETYP               
525600           MOVE 5                     TO FILC3-KDSORT1                    
525700           PERFORM S33-SKAPA-LDC-TRANS                                    
525800        END-IF                                                            
525900                                                                          
526000        PERFORM IMS-GU-WLARTC01                                           
526100        MOVE K6-ART-KDPRODSL       TO W-KDPRODSL                          
526200        MOVE K6-ART-IDFKNGRP       TO W-IDFKNGRP                          
526300        PERFORM IMS-GHNP-WLARTC11                                         
526400        MOVE CLAG-KDPSLLOC         TO W-KDPRODSL-LOC                      
526500        MOVE CLAG-PRARTSTD         TO WS-SAP-PRARTSTD                     
526600                                                                          
526700        IF SEND-DCS-CDC OR SEND-DCS-DDC                                   
526800           PERFORM S16-PRIS-TILLAMPNING                                   
526900           MOVE PRIS-PRARTNTO          TO WS-FIXAD-PRARTNTO               
527000           MOVE 'SEK'                  TO WS-KDVALISO                     
527100           IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                   
527200             MOVE PRIS-KDVALISO        TO WS-KDVALISO                     
527300           END-IF                                                         
527400           IF SEND-DCS-DDC                                                
527500              MOVE '1441 '       TO INL-IDLEVNR                           
527600           ELSE                                                           
527700              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
527800           END-IF                                                         
527900                                                                          
528000           PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                          
528100           PERFORM S09-SKAPA-EKOTRANS-A03                                 
528200                                                                          
528300           PERFORM IMS-GET-WDK711                                         
528400           MOVE SLAG-PRAVCOST          TO EKOTRA03-PRAVCOST-OLD           
528500           MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                       
528600                                       EKOTRA03-KVANTMOT                  
528700           MOVE ZERO                   TO EKOTRA03-KVSKROT                
528800           MOVE '21'                   TO 6308-KDANMORS                   
528900                                       EKOTRA03-KDANMORS                  
529000           MOVE WS-SUMMA-KVANT         TO 6308-KVLEVANM                   
529100                                                                          
529200           PERFORM S11-SKAPA-LEVANM-TRANS                                 
529300                                                                          
529400           MOVE 'FYR'     TO STEXT                                        
529500           PERFORM S10-OMRAKN-MEDELPRIS-NA                                
529600           MOVE W-TIME-N            TO AKTUELL-TID                        
529700           MOVE MSGI-TILOKDAT       TO SLAG-TIAVCOST                      
529800           MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                     
529900                                    SLAG-PRAVCOST                         
530000           MOVE AVG-REMARKUP        TO EKOTRA03-REMARKUP                  
530100           MOVE AVG-PRKURS          TO EKOTRA03-PRKURS                    
530200                                                                          
530300           MOVE 6302-IDDISTR        TO WS-SAP-IDDISTR                     
530400           MOVE W-SPAR-IDKUNDNR     TO WS-SAP-IDKUNDNR                    
530500                                                                          
530600           IF WS-KVANTMOT-INM     > 0                                     
530700              PERFORM IMS-REPL-WDK711                                     
530800           ELSE                                                           
530900              MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST             
531000           END-IF                                                         
531100                                                                          
531200           IF REQU-CMD-INM = 'DAM'                                        
531300              MOVE '43'                   TO 6308-KDANMORS                
531400              MOVE WS-KVSKROT-INM-NUM     TO 6308-KVLEVANM                
531500                                          EKOTRA03-KVSKROT                
531600              PERFORM S11-SKAPA-LEVANM-TRANS                              
531700           END-IF                                                         
531800        ELSE                                                              
531900                                                                          
532000          IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                           
532100             AND (DCS-NDC-NA AND DCS-CANADA)                              
532200          OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                        
532300             AND (DCS-NDC-NA AND DCS-USA)                                 
532400                                                                          
532500              MOVE SEND-WS-IDDC     TO W-IDDC                             
532600              PERFORM IMS-GET-WDK711                                      
532700              MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                     
532800              IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                      
532900                 MOVE 'CAD'         TO WS-KDVALISO                        
533000              ELSE                                                        
533100                 MOVE 'USD'         TO WS-KDVALISO                        
533200              END-IF                                                      
533300                                                                          
533400              MOVE WS-IDDC TO W-IDDC                                      
533500              PERFORM IMS-GET-WDK711                                      
533600              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
533700              PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                       
533800                                                                          
533900              PERFORM S09-SKAPA-EKOTRANS-A03                              
534000                                                                          
534100              MOVE SLAG-PRAVCOST          TO EKOTRA03-PRAVCOST-OLD        
534200              MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                    
534300                                          EKOTRA03-KVANTMOT               
534400              MOVE ZERO                   TO EKOTRA03-KVSKROT             
534500              MOVE '21'                   TO 6308-KDANMORS                
534600                                          EKOTRA03-KDANMORS               
534700              MOVE WS-SUMMA-KVANT         TO 6308-KVLEVANM                
534800              PERFORM S11-SKAPA-LEVANM-TRANS                              
534900                                                                          
535000              MOVE 'FEM'     TO STEXT                                     
535100              PERFORM S10-OMRAKN-MEDELPRIS-NA                             
535200              MOVE W-TIME-N            TO AKTUELL-TID                     
535300              MOVE MSGI-TILOKDAT       TO SLAG-TIAVCOST                   
535400              MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                  
535500                                       SLAG-PRAVCOST                      
535600              MOVE AVG-REMARKUP        TO EKOTRA03-REMARKUP               
535700              MOVE AVG-PRKURS          TO EKOTRA03-PRKURS                 
535800                                                                          
535900              IF WS-KVANTMOT-INM     > 0                                  
536000                 PERFORM IMS-REPL-WDK711                                  
536100              ELSE                                                        
536200                 MOVE EKOTRA03-PRAVCOST-OLD                               
536300                                    TO EKOTRA03-PRAVCOST                  
536400              END-IF                                                      
536500                                                                          
536600              IF REQU-CMD-INM = 'DAM'                                     
536700                 MOVE '42'           TO 6308-KDANMORS                     
536800                 MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM                 
536900                                            EKOTRA03-KVSKROT              
537000                 PERFORM S11-SKAPA-LEVANM-TRANS                           
537100              END-IF                                                      
537200           ELSE                                                           
537300                                                                          
537400              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
537500              AND (DCS-NDC-NA AND DCS-USA)                                
537600                                                                          
537700                 MOVE SEND-WS-IDDC TO W-IDDC                              
537800                 PERFORM IMS-GET-WDK711                                   
537900                 SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                   
538000                 MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                  
538100                 MOVE 'USD'            TO WS-KDVALISO                     
538200                 PERFORM IMS-REPL-WDK711                                  
538300* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
538400                 PERFORM S21-SALDOLOGG-DATA                               
538500                 MOVE SEND-WS-IDDC      TO LOGG-IDDC                      
538600                 MOVE 'MISC'            TO LOGG-IDHUVTYP                  
538700                 MOVE 'R34'             TO LOGG-IDSUBTYP                  
538800                 MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                  
538900                 MOVE SLAG-KVLS         TO LOGG-KVLS                      
539000                 MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                        
539100                 MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                    
539200                 MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                    
539300                 MOVE '-'               TO LOGG-IDTECKEN-KVLS             
539400                 MOVE SPACE             TO LOGG-IDTECKEN-KVAKS            
539500                 MOVE SPACE             TO LOGG-IDTECKEN-KVAKS-PAV        
539600                 MOVE SPACE             TO LOGG-IDTECKEN-KVEFRS           
539700                 PERFORM S22-ISRT-SALDOLOGG                               
539800* ---SLUT SALDOFÖRÄNDRINGAR                                               
539900                                                                          
540000                 MOVE WS-IDDC TO W-IDDC                                   
540100                 PERFORM IMS-GET-WDK711                                   
540200                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
540300                 PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                    
540400                                                                          
540500                 PERFORM S09-SKAPA-EKOTRANS-A03                           
540600                 MOVE SLAG-PRAVCOST                                       
540700                               TO EKOTRA03-PRAVCOST-OLD                   
540800                 MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                 
540900                                             EKOTRA03-KVANTMOT            
541000                 MOVE ZERO                   TO EKOTRA03-KVSKROT          
541100                 MOVE '21'                   TO EKOTRA03-KDANMORS         
541200                                                                          
541300                 MOVE 'SEX'     TO STEXT                                  
541400                 PERFORM S10-OMRAKN-MEDELPRIS-NA                          
541500                                                                          
541600                 MOVE W-TIME-N            TO AKTUELL-TID                  
541700                 MOVE MSGI-TILOKDAT       TO SLAG-TIAVCOST                
541800                 MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST               
541900                                          SLAG-PRAVCOST                   
542000                 MOVE AVG-REMARKUP        TO EKOTRA03-REMARKUP            
542100                 MOVE AVG-PRKURS          TO EKOTRA03-PRKURS              
542200                 MOVE 1.0                 TO EKOTRA03-PRKURS              
542300                                                                          
542400                 IF WS-KVANTMOT-INM     > 0                               
542500                    PERFORM IMS-REPL-WDK711                               
542600                 ELSE                                                     
542700                    MOVE EKOTRA03-PRAVCOST-OLD                            
542800                                       TO EKOTRA03-PRAVCOST               
542900                 END-IF                                                   
543000                                                                          
543100                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
543200                 PERFORM S13-SKAPA-NDC-HIST-SANDANDE                      
543300                                                                          
543400                 IF REQU-CMD-INM = 'DAM'                                  
543500                   MOVE WS-KVSKROT-INM-NUM TO EKOTRA03-KVSKROT            
543600                 END-IF                                                   
543700              END-IF                                                      
543800           END-IF                                                         
543900        END-IF                                                            
544000     ELSE                                                                 
544100        IF DCS-CDC                                                        
544200           IF SEND-DCS-NDC-CN OR SEND-DCS-USA                             
544300             PERFORM IMS-GU-WLARTC01                                      
544400             MOVE K6-ART-KDPRODSL      TO W-KDPRODSL                      
544500             MOVE K6-ART-IDFKNGRP      TO W-IDFKNGRP                      
544600             PERFORM IMS-GHNP-WLARTC11                                    
544700             MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                    
544800             MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                   
544900             MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                      
545000             MOVE ZERO          TO WS-FIXAD-PRARTNTO                      
545100             MOVE SPACE         TO WS-KDVALISO                            
545200             PERFORM S16-PRIS-TILLAMPNING                                 
545300             PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                           
545400             MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                      
545500             MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                     
545600             PERFORM S31-SKAPA-SAP-TRANS-VCCS                             
545700                                                                          
545800             MOVE '21'             TO 6308-KDANMORS                       
545900             MOVE WS-SUMMA-KVANT   TO 6308-KVLEVANM                       
546000                                                                          
546100             PERFORM S11-SKAPA-LEVANM-TRANS                               
546200                                                                          
546300             IF REQU-CMD-INM = 'DAM'                                      
546400                MOVE JA TO NY-SKROT-SW                                    
546500                PERFORM S99-SKAPA-SKROT-ORDER                             
546600             END-IF                                                       
546700           END-IF                                                         
546800        ELSE                                                              
546900           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
547000             PERFORM IMS-GU-WLARTC01                                      
547100             MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                       
547200             MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                       
547300             PERFORM IMS-GHNP-WLARTC11                                    
547400             SUBTRACT WS-SUMMA-KVANT FROM CLAG-KVLS                       
547500             MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                   
547600             MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                  
547700             PERFORM IMS-REPL-WLARTC11                                    
547800             PERFORM S21-SALDOLOGG-DATA                                   
547900             MOVE WC-CDC-SE           TO LOGG-IDDC                        
548000             MOVE 'MISC'              TO LOGG-IDHUVTYP                    
548100             MOVE 'R34'               TO LOGG-IDSUBTYP                    
548200             MOVE WS-SUMMA-KVANT      TO LOGG-KVART-SALDO                 
548300             MOVE CLAG-KVLS           TO LOGG-KVLS                        
548400             MOVE CLAG-KVAKS-CDC      TO LOGG-KVAKS                       
548500             MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                      
548600             MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                   
548700             MOVE '-'                 TO LOGG-IDTECKEN-KVLS               
548800             MOVE SPACE               TO LOGG-IDTECKEN-KVAKS              
548900             MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV          
549000             MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS             
549100             PERFORM S22-ISRT-SALDOLOGG                                   
549200             PERFORM S07-SKAPA-HISTORIK                                   
549300           ELSE                                                           
549400              PERFORM IMS-GU-WLARTC01                                     
549500              MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                       
549600              MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                       
549700              PERFORM IMS-GHNP-WLARTC11                                   
549800              MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                   
549900              MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                  
550000                                                                          
550100              IF SEND-DCS-AUSTRALIA OR SEND-DCS-JAPAN OR                  
550200                 SEND-DCS-SDC                                             
550300                 MOVE W-IDDC TO WS-SPARAT-IDDC                            
550400                 MOVE W-IDDC-B6-SEND TO W-IDDC                            
550500                 PERFORM IMS-GET-WDK711                                   
550600                 SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                   
550700                 PERFORM IMS-REPL-WDK711                                  
550800                 MOVE WS-SPARAT-IDDC     TO W-IDDC                        
550900******* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                
551000                 PERFORM S21-SALDOLOGG-DATA                               
551100                 MOVE W-IDDC-B6-SEND     TO LOGG-IDDC                     
551200                 MOVE 'MISC'             TO LOGG-IDHUVTYP                 
551300                 MOVE 'R34'              TO LOGG-IDSUBTYP                 
551400                 MOVE WS-SUMMA-KVANT     TO LOGG-KVART-SALDO              
551500                 MOVE SLAG-KVLS          TO LOGG-KVLS                     
551600                 MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                    
551700                 MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                   
551800                 MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                
551900                 MOVE '-'                TO LOGG-IDTECKEN-KVLS            
552000                 MOVE SPACE              TO LOGG-IDTECKEN-KVAKS           
552100                 MOVE SPACE             TO LOGG-IDTECKEN-KVAKS-PAV        
552200                 MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS          
552300                 PERFORM S22-ISRT-SALDOLOGG                               
552400                 PERFORM S13-SKAPA-NDC-HIST-SANDANDE                      
552500              END-IF                                                      
552600           END-IF                                                         
552700                                                                          
552800           IF SEND-DCS-DDC                                                
552900              MOVE '1441 '             TO INL-IDLEVNR                     
553000           ELSE                                                           
553100              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
553200           END-IF                                                         
553300           MOVE ZERO          TO WS-FIXAD-PRARTNTO                        
553400           MOVE SPACE         TO WS-KDVALISO                              
553500           PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                          
553600           MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                        
553700           MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                       
553800           PERFORM S31-SKAPA-SAP-TRANS-VCCS                               
553900                                                                          
554000           IF SEND-DCS-DDC                                                
554100              MOVE WS-SUMMA-KVANT        TO FILC2-KVANTAL                 
554200              MOVE 'ÖVERLEV.'            TO FILC2-AVVIKELSETYP            
554300              PERFORM S32-SKAPA-DIFF-TRANS                                
554400              IF WS-KVSKROT-INM-NUM > ZERO                                
554500                 MOVE WS-KVSKROT-INM-NUM TO FILC2-KVANTAL                 
554600                 MOVE 'DAM'              TO FILC2-AVVIKELSETYP            
554700                 PERFORM S32-SKAPA-DIFF-TRANS                             
554800              END-IF                                                      
554900           END-IF                                                         
555000                                                                          
555100           MOVE ZERO           TO FILC3-KVAVIS                            
555200           MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                           
555300           MOVE 'NY'           TO FILC3-AVVIKELSETYP                      
555400           MOVE 6              TO FILC3-KDSORT1                           
555500           PERFORM S33-SKAPA-LDC-TRANS                                    
555600           IF WS-KVSKROT-INM-NUM > ZERO                                   
555700              MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                    
555800              MOVE 'DAM'              TO FILC3-AVVIKELSETYP               
555900              MOVE 5                  TO FILC3-KDSORT1                    
556000              PERFORM S33-SKAPA-LDC-TRANS                                 
556100           END-IF                                                         
556200           IF REQU-CMD-INM = 'DAM'                                        
556300              MOVE JA TO NY-SKROT-SW                                      
556400              PERFORM S99-SKAPA-SKROT-ORDER                               
556500           END-IF                                                         
556600        END-IF                                                            
556700     END-IF                                                               
556800                                                                          
556900     PERFORM S08-UPPDATERA-6301                                           
557000     .                                                                    
557100     EJECT                                                                
557200                                                                          
557300 HD-NYUPPLAEGG      SECTION.                                              
557400     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
557500        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
557600        PERFORM IMS-GU-WDB601-SEND                                        
557700     END-IF                                                               
557800                                                                          
557900     MOVE ZERO            TO W-KVRADER                                    
558000     ADD +1               TO W-KVRADER                                    
558100     MOVE JA              TO NYUPPLAEGG-SW                                
558200     MOVE W-IDARTNR-INM   TO W-IDARTNR                                    
558300     MOVE REQU-IDDC-KEY   TO W-IDDC                                       
558400     PERFORM IMS-GET-WDK711                                               
558500                                                                          
558600     IF REQU-ADLAGOMR-INM = ALL '+' OR SPACE                              
558700        MOVE SLAG-ADLAGOMR   TO W-ADLAGOMR-SPAR                           
558800     ELSE                                                                 
558900        MOVE W-ADLAGOMR-INM  TO W-ADLAGOMR-SPAR                           
559000                                SLAG-ADLAGOMR                             
559100                                W-ADLAGOMR-LOCB                           
559200     END-IF                                                               
559300     IF REQU-ADGANG-INM = ALL '+' OR SPACE                                
559400        MOVE SLAG-ADGANG     TO W-ADGANG-SPAR                             
559500     ELSE                                                                 
559600        MOVE W-ADGANG-INM    TO W-ADGANG-SPAR                             
559700                                SLAG-ADGANG                               
559800                                W-ADGANG-LOCB                             
559900     END-IF                                                               
560000     IF REQU-ADPLATS-INM = ALL '+' OR SPACE                               
560100        MOVE SLAG-ADPLATS    TO W-ADPLATS-SPAR                            
560200     ELSE                                                                 
560300        MOVE W-ADPLATS-INM   TO W-ADPLATS-SPAR                            
560400                                SLAG-ADPLATS                              
560500                                W-ADPLATS-LOCB                            
560600     END-IF                                                               
560700                                                                          
560800     IF W-ADLAGOMR-LOCB NUMERIC                                           
560900     AND W-ADGANG-LOCB NUMERIC                                            
561000     AND W-ADPLATS-LOCB NUMERIC                                           
561100        IF W-ADLAGOMR-LOCB > ZERO                                         
561200        OR W-ADGANG-LOCB > ZERO                                           
561300        OR W-ADPLATS-LOCB > ZERO                                          
561400           PERFORM S30-UPPDATERA-WDJ9                                     
561500        END-IF                                                            
561600     END-IF                                                               
561700                                                                          
561800     MOVE SLAG-KVLS          TO WS-OLD-KVLS                               
561900     MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                             
562000                                                                          
562100     MOVE WS-KVANTMOT-INM    TO WS-KVANTMOT-INM-NUM                       
562200     MOVE WS-KVSKROT-INM     TO WS-KVSKROT-INM-NUM                        
562300     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM                        
562400             + WS-KVSKROT-INM-NUM)                                        
562500     END-COMPUTE                                                          
562600     ADD WS-SUMMA-KVANT      TO SLAG-KVLS                                 
562700* ---NEDANSTÅENDE KOD LOGGAR                                              
562800* ---SALDOFÖRÄNDRINGAR PÅ WDL9                                            
562900     PERFORM S21-SALDOLOGG-DATA                                           
563000     MOVE REQU-IDDC-KEY        TO LOGG-IDDC                               
563100     MOVE WS-SUMMA-KVANT       TO LOGG-KVART-SALDO                        
563200     MOVE SLAG-KVLS            TO LOGG-KVLS                               
563300     MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                              
563400     MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                             
563500     MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                          
563600     MOVE '+'                  TO LOGG-IDTECKEN-KVLS                      
563700     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                     
563800     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
563900     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
564000     PERFORM S22-ISRT-SALDOLOGG                                           
564100* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
564200                                                                          
564300     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
564400     OR DIST35-VCC-NONVCC-REFILL OR DIST35-VCC-NONVCC-TRANSFER            
564500        SUBTRACT WS-KVSKROT-INM-NUM FROM SLAG-KVLS                        
564600        IF XDC-NON-VCC-OWNED                                              
564700**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
564800**** REST OF FLOWS TO US IS IN LAB.                                       
564900**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
565000**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
565100        OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                         
565200           MOVE WS-KVANTMOT-INM-NUM TO WS-RO-KVANTMOT                     
565300           PERFORM S19-EV-RO-TACKNING                                     
565400        END-IF                                                            
565500* ---NEDANSTÅENDE KOD LOGGAR                                              
565600* ---SALDOFÖRÄNDRINGAR PÅ WDL9                                            
565700        PERFORM S21-SALDOLOGG-DATA                                        
565800        MOVE REQU-IDDC-KEY      TO LOGG-IDDC                              
565900        MOVE WS-KVSKROT-INM-NUM TO LOGG-KVART-SALDO                       
566000        MOVE SLAG-KVLS          TO LOGG-KVLS                              
566100        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
566200        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
566300        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
566400        MOVE '-'                TO LOGG-IDTECKEN-KVLS                     
566500        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                    
566600        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
566700        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
566800        PERFORM S22-ISRT-SALDOLOGG                                        
566900* ---SLUT SALDOFÖRÄNDRINGAR                                               
567000     END-IF                                                               
567100                                                                          
567200     PERFORM IMS-REPL-WDK711                                              
567300                                                                          
567400     MOVE WS-SUMMA-KVANT     TO W-TEMP-KVANT                              
567500     MOVE ZERO               TO W-KVAVIS                                  
567600                                                                          
567700** WRITE WDK728 WHEN FLTRACK='J'                                          
567800     IF REQU-KVANTMOT-INM NOT = ALL '+'  OR SPACE                         
567900        IF WS-FLTRACK = 'J'                                               
568000           MOVE WS-KVANTMOT-INM-NUM TO WS-TRCK-KVANTMOT                   
568100           MOVE ZERO                TO WS-TRCK-KVAVIS                     
568200           MOVE W-IDTRACK           TO TINL-IDTRACK                       
568300           PERFORM S39-UPDATE-WDK728                                      
568400        END-IF                                                            
568500     END-IF                                                               
568600                                                                          
568700     IF SEND-DCS-CHINA                                                    
568800        PERFORM IMS-GU-WLARTC01                                           
568900        MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                             
569000        MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                             
569100        PERFORM IMS-GHNP-WLARTC11                                         
569200        MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                         
569300        MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                        
569400        MOVE W-IDDC TO WS-SPARAT-IDDC                                     
569500        MOVE SEND-WS-IDDC TO W-IDDC                                       
569600        PERFORM IMS-GET-WDK711                                            
569700        SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                            
569800        PERFORM IMS-REPL-WDK711                                           
569900        MOVE WS-SPARAT-IDDC     TO W-IDDC                                 
570000******* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                
570100        PERFORM S21-SALDOLOGG-DATA                                        
570200        MOVE SEND-WS-IDDC       TO LOGG-IDDC                              
570300        MOVE 'MISC'             TO LOGG-IDHUVTYP                          
570400        MOVE 'R34'              TO LOGG-IDSUBTYP                          
570500        MOVE WS-SUMMA-KVANT     TO LOGG-KVART-SALDO                       
570600        MOVE SLAG-KVLS          TO LOGG-KVLS                              
570700        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
570800        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
570900        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
571000        MOVE '-'                TO LOGG-IDTECKEN-KVLS                     
571100        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                    
571200        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
571300        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
571400        PERFORM S22-ISRT-SALDOLOGG                                        
571500        PERFORM S37-SKAPA-NDC-HIST-SANDANDE                               
571600        IF SEND-DCS-DDC                                                   
571700           MOVE '1441 '    TO INL-IDLEVNR                                 
571800        ELSE                                                              
571900           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
572000        END-IF                                                            
572100        MOVE ZERO            TO WS-FIXAD-PRARTNTO                         
572200        MOVE SPACE           TO WS-KDVALISO                               
572300        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                
572400        MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                            
572500        MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                           
572600                                                                          
572700        PERFORM S16-PRIS-TILLAMPNING                                      
572800        MOVE PRIS-PRARTNTO   TO R8-EKH-PRARTNTO                           
572900        PERFORM S31-SKAPA-SAP-TRANS                                       
573000                                                                          
573100        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
573200        MOVE ZERO           TO FILC3-KVAVIS                               
573300        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
573400        MOVE 'NY'           TO FILC3-AVVIKELSETYP                         
573500        MOVE 6              TO FILC3-KDSORT1                              
573600        PERFORM S33-SKAPA-LDC-TRANS                                       
573700        IF WS-KVSKROT-INM-NUM > ZERO                                      
573800           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
573900           MOVE 'DAM'              TO FILC3-AVVIKELSETYP                  
574000           MOVE 5                  TO FILC3-KDSORT1                       
574100           PERFORM S33-SKAPA-LDC-TRANS                                    
574200        END-IF                                                            
574300                                                                          
574400        IF REQU-CMD-INM = 'DAM'                                           
574500           MOVE JA TO NY-SKROT-SW                                         
574600           PERFORM S99-SKAPA-SKROT-ORDER                                  
574700        END-IF                                                            
574800                                                                          
574900     ELSE                                                                 
575000        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
575100        MOVE ZERO           TO FILC3-KVAVIS                               
575200        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
575300        MOVE 'NY'           TO FILC3-AVVIKELSETYP                         
575400        MOVE 6              TO FILC3-KDSORT1                              
575500        PERFORM S33-SKAPA-LDC-TRANS                                       
575600        IF WS-KVSKROT-INM-NUM > ZERO                                      
575700           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
575800           MOVE 'DAM'              TO FILC3-AVVIKELSETYP                  
575900           MOVE 5                  TO FILC3-KDSORT1                       
576000           PERFORM S33-SKAPA-LDC-TRANS                                    
576100        END-IF                                                            
576200                                                                          
576300        PERFORM IMS-GU-WLARTC01                                           
576400        MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                             
576500        MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                             
576600        PERFORM IMS-GHNP-WLARTC11                                         
576700        MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                         
576800        MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                        
576900                                                                          
577000        PERFORM S16-PRIS-TILLAMPNING                                      
577100        MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO                     
577200        MOVE DCS-KDVALISO        TO WS-KDVALISO                           
577300        IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                      
577400          MOVE PRIS-KDVALISO     TO WS-KDVALISO                           
577500        END-IF                                                            
577600        IF SEND-DCS-DDC                                                   
577700           MOVE '1441 '    TO INL-IDLEVNR                                 
577800        ELSE                                                              
577900           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
578000        END-IF                                                            
578100                                                                          
578200        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                
578300                                                                          
578400        PERFORM IMS-GET-WDK711                                            
578500        MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                          
578600        MOVE '21'                TO 6308-KDANMORS                         
578700        MOVE WS-SUMMA-KVANT      TO 6308-KVLEVANM                         
578800                                                                          
578900        PERFORM S11-SKAPA-LEVANM-TRANS                                    
579000                                                                          
579100        PERFORM S16-PRIS-TILLAMPNING                                      
579200        MOVE PRIS-PRARTNTO    TO R8-EKH-PRARTNTO                          
579300        PERFORM S31-SKAPA-SAP-TRANS                                       
579400                                                                          
579500        MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                           
579600        MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                          
579700                                                                          
579800        IF WS-KVANTMOT-INM  > 0                                           
579900           PERFORM IMS-REPL-WDK711                                        
580000        END-IF                                                            
580100                                                                          
580200        IF REQU-CMD-INM = 'DAM'                                           
580300           MOVE '43'                TO 6308-KDANMORS                      
580400           MOVE WS-KVSKROT-INM-NUM  TO 6308-KVLEVANM                      
580500           PERFORM S11-SKAPA-LEVANM-TRANS                                 
580600        END-IF                                                            
580700     END-IF                                                               
580800                                                                          
580900* BERÄKNING AVGCOST                                                       
581000     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
581100     PERFORM IMS-GET-WDK711                                               
581200     MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                             
581300     PERFORM S10-OMRAKN-MEDELPRIS                                         
581400     MOVE W-TIME-N         TO AKTUELL-TID                                 
581500     MOVE W-DAGENS-DATUM   TO SLAG-TIAVCOST                               
581600     MOVE AVG-PRAVCOST-NEW TO SLAG-PRAVCOST                               
581700     PERFORM IMS-REPL-WDK711                                              
581800                                                                          
581900     PERFORM S08-UPPDATERA-6301                                           
582000     .                                                                    
582100     EJECT                                                                
582200                                                                          
582300 HE-UPPDATERA-WL630111    SECTION.                                        
582400     MOVE NEJ             TO WS-FAKTURA-KLAR                              
582500     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
582600     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
582700     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
582800                             W-SEQA-IDFAKT-MAX                            
582900     MOVE '310'           TO W-IDPTYP                                     
583000     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
583100                                                                          
583200     IF SEGMENT-SAKNAS                                                    
583300        MOVE W-SPAR-IDFAKT TO W-IDFAKT                                    
583400        MOVE REQU-IDDC-KEY TO W-6301-IDDC                                 
583500        PERFORM IMS-GHU-WL630111                                          
583600                                                                          
583700        MOVE LOW-VALUE     TO W-WDL6A1KY-MIN                              
583800        MOVE HIGH-VALUE    TO W-WDL6A1KY-MAX                              
583900        MOVE W-SPAR-IDFAKT TO W-SEQA-IDFAKT-MIN                           
584000                              W-SEQA-IDFAKT-MAX                           
584100        MOVE 'R30'         TO W-IDPTYP                                    
584200        PERFORM IMS-GU-WLINLD01-FIRST-310                                 
584300                                                                          
584400        IF SEGMENT-SAKNAS                                                 
584500           MOVE JA TO WS-FAKTURA-KLAR                                     
584600           PERFORM IMS-DLET-WL630111                                      
584700           PERFORM IMS-GHU-WL630511                                       
584800           IF SEGMENT-FINNS                                               
584900              MOVE JA TO 6306-FLKLAR                                      
585000              PERFORM IMS-REPL-WL630511                                   
585100           END-IF                                                         
585200        END-IF                                                            
585300     END-IF                                                               
585400     .                                                                    
585500     EJECT                                                                
585600 HE-UPPDATERA-WL630111-APP SECTION.                                       
585700     MOVE +1 TO INDX-2                                                    
585800     MOVE W-SPAR-REQU-IDFAKT-2(INDX-2) TO W-SPAR-IDFAKT                   
585900     PERFORM UNTIL INDX-2 > MAX-INDX-2                                    
586000       MOVE NEJ             TO WS-FAKTURA-KLAR                            
586100       MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                             
586200       MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                             
586300       MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                          
586400                               W-SEQA-IDFAKT-MAX                          
586500       MOVE '310'           TO W-IDPTYP                                   
586600       PERFORM IMS-GU-WLINLD01-FIRST-310                                  
586700                                                                          
586800       IF SEGMENT-SAKNAS                                                  
586900          MOVE W-SPAR-IDFAKT TO W-IDFAKT                                  
587000          MOVE REQU-IDDC-KEY TO W-6301-IDDC                               
587100          PERFORM IMS-GHU-WL630111                                        
587200                                                                          
587300          MOVE LOW-VALUE     TO W-WDL6A1KY-MIN                            
587400          MOVE HIGH-VALUE    TO W-WDL6A1KY-MAX                            
587500          MOVE W-SPAR-IDFAKT TO W-SEQA-IDFAKT-MIN                         
587600                                W-SEQA-IDFAKT-MAX                         
587700          MOVE 'R30'         TO W-IDPTYP                                  
587800          PERFORM IMS-GU-WLINLD01-FIRST-310                               
587900                                                                          
588000          IF SEGMENT-SAKNAS                                               
588100            MOVE JA TO WS-FAKTURA-KLAR                                    
588200            PERFORM IMS-DLET-WL630111                                     
588300            PERFORM IMS-GHU-WL630511                                      
588400            IF SEGMENT-FINNS                                              
588500              MOVE JA TO 6306-FLKLAR                                      
588600              PERFORM IMS-REPL-WL630511                                   
588700            END-IF                                                        
588800          END-IF                                                          
588900       END-IF                                                             
589000       ADD +1 TO INDX-2                                                   
589100       IF INDX-2 > MAX-INDX-2                                             
589200         CONTINUE                                                         
589300       ELSE                                                               
589400         MOVE W-SPAR-REQU-IDFAKT-2(INDX-2) TO W-SPAR-IDFAKT               
589500       END-IF                                                             
589600     END-PERFORM                                                          
589700     .                                                                    
589800     EJECT                                                                
589900 HF-ENDAST-LAGERPLATS SECTION.                                            
590000     MOVE REQU-IDARTNR(INDX) TO W-IDARTNR                                 
590100     MOVE REQU-IDDC-KEY      TO W-IDDC                                    
590200     IF DCS-CDC                                                           
590300        PERFORM IMS-GHU-WLARTC11                                          
590400     ELSE                                                                 
590500        PERFORM IMS-GET-WDK711                                            
590600     END-IF                                                               
590700                                                                          
590800     IF W-ADLAGOMR (INDX) > ZERO                                          
590900        IF DCS-CDC                                                        
591000           MOVE W-ADLAGOMR (INDX) TO CLAG-ADLAGOMR                        
591100           MOVE W-ADGANG (INDX)   TO CLAG-ADGANG                          
591200           MOVE W-ADPLATS (INDX)  TO CLAG-ADPLATS                         
591300        ELSE                                                              
591400           MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                        
591500           MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                          
591600           MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                         
591700        END-IF                                                            
591800        MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                         
591900        MOVE W-ADGANG (INDX) TO W-ADGANG-LOCB                             
592000        MOVE W-ADPLATS (INDX) TO W-ADPLATS-LOCB                           
592100        PERFORM S30-UPPDATERA-WDJ9                                        
592200        IF DCS-CDC                                                        
592300           PERFORM IMS-REPL-WLARTC11                                      
592400        ELSE                                                              
592500           PERFORM IMS-REPL-WDK711                                        
592600        END-IF                                                            
592700     END-IF                                                               
592800     .                                                                    
592900     EJECT                                                                
593000                                                                          
593100 HG-NY-NYUPPLAEGG-ART-PV SECTION.                                         
593200     MOVE ZERO            TO W-KVRADER                                    
593300     ADD +1               TO W-KVRADER                                    
593400     MOVE JA              TO NYUPPLAEGG-SW                                
593500     MOVE REQU-IDDC-KEY   TO W-IDDC                                       
593600     MOVE ZERO            TO WS-OLD-KVLS                                  
593700     MOVE ZERO            TO WS-OLD-KVEFRS                                
593800                                                                          
593900     MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                          
594000     MOVE WS-KVSKROT-INM  TO WS-KVSKROT-INM-NUM                           
594100     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM +                      
594200           WS-KVSKROT-INM-NUM)                                            
594300     END-COMPUTE                                                          
594400     MOVE WS-SUMMA-KVANT  TO W-TEMP-KVANT                                 
594500     MOVE ZERO            TO W-KVAVIS                                     
594600                                                                          
594700     MOVE ALL '+'         TO WDK7-W005WDK7                                
594800     MOVE 'WDK711'        TO WDK7-IDSEGM                                  
594900     MOVE W-IDARTNR-INM   TO WDK7-IDARTNR-KFB                             
595000     MOVE REQU-IDDC-KEY   TO WDK7-IDDC-KFB                                
595100                             WDK7-IDDC                                    
595200     MOVE W-ADLAGOMR-INM  TO WDK7-ADLAGOMR                                
595300                             W-ADLAGOMR-SPAR                              
595400                             W-ADLAGOMR-LOCB                              
595500     MOVE W-ADGANG-INM    TO WDK7-ADGANG                                  
595600                             W-ADGANG-SPAR                                
595700                             W-ADGANG-LOCB                                
595800     MOVE W-ADPLATS-INM   TO WDK7-ADPLATS                                 
595900                             W-ADPLATS-SPAR                               
596000                             W-ADPLATS-LOCB                               
596100     MOVE WS-SUMMA-KVANT  TO WDK7-KVLS                                    
596200                                                                          
596300     IF DCS-NDC-NA OR DCS-AUSTRALIA OR DCS-JAPAN                          
596400        MOVE JA           TO WDK7-FLORDSP                                 
596500                             WDK7-FLSPBULK                                
596600     END-IF                                                               
596700     MOVE WS-SUMMA-KVANT  TO WDK7-KVLS                                    
596800                                                                          
596900     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
597000                                       ARTC-PCB WDK7-PCB                  
597100     MOVE WDK7-WDK711     TO SLAG-WDK711                                  
597200                                                                          
597300     MOVE W-ADLAGOMR-INM         TO W-ADLAGOMR-SPAR                       
597400                                    W-ADLAGOMR-LOCB                       
597500     MOVE W-ADGANG-INM           TO W-ADGANG-SPAR                         
597600                                    W-ADGANG-LOCB                         
597700     MOVE W-ADPLATS-INM          TO W-ADPLATS-SPAR                        
597800                                    W-ADPLATS-LOCB                        
597900** WRITE WDK728 WHEN FLTRACK='J'                                          
598000     IF REQU-KVANTMOT-INM NOT = ALL '+'  OR SPACE                         
598100        IF WS-FLTRACK = 'J'                                               
598200           MOVE WS-KVANTMOT-INM-NUM TO WS-TRCK-KVANTMOT                   
598300           MOVE ZERO                TO WS-TRCK-KVAVIS                     
598400           MOVE W-IDTRACK           TO TINL-IDTRACK                       
598500           PERFORM S39-UPDATE-WDK728                                      
598600        END-IF                                                            
598700     END-IF                                                               
598800                                                                          
598900     PERFORM S30-UPPDATERA-WDJ9                                           
599000                                                                          
599100     PERFORM S21-SALDOLOGG-DATA                                           
599200     MOVE REQU-IDDC-KEY  TO LOGG-IDDC                                     
599300     MOVE SLAG-KVLS      TO LOGG-KVLS                                     
599400     MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                                    
599500     MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                                   
599600     MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                                
599700     MOVE '+'            TO LOGG-IDTECKEN-KVLS                            
599800     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                           
599900     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                       
600000     MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                          
600100        MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                           
600200     PERFORM S22-ISRT-SALDOLOGG                                           
600300                                                                          
600400     IF DCS-NDC-NA                                                        
600500                                                                          
600600        MOVE DCS-FLINLREP      TO FILC3-FLINLREP                          
600700        MOVE ZERO              TO FILC3-KVAVIS                            
600800        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
600900        MOVE 'NY'              TO FILC3-AVVIKELSETYP                      
601000        MOVE 6                 TO FILC3-KDSORT1                           
601100        PERFORM S33-SKAPA-LDC-TRANS                                       
601200        IF WS-KVSKROT-INM-NUM > ZERO                                      
601300           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
601400           MOVE 'DAM'                 TO FILC3-AVVIKELSETYP               
601500           MOVE 5                     TO FILC3-KDSORT1                    
601600           PERFORM S33-SKAPA-LDC-TRANS                                    
601700        END-IF                                                            
601800                                                                          
601900        PERFORM IMS-GU-WLARTC01                                           
602000        MOVE K6-ART-KDPRODSL       TO W-KDPRODSL                          
602100        MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                             
602200        PERFORM IMS-GHNP-WLARTC11                                         
602300        MOVE CLAG-KDPSLLOC         TO W-KDPRODSL-LOC                      
602400        MOVE CLAG-PRARTSTD         TO WS-SAP-PRARTSTD                     
602500                                                                          
602600        IF SEND-DCS-CDC OR SEND-DCS-DDC                                   
602700                                                                          
602800           PERFORM S16-PRIS-TILLAMPNING                                   
602900           MOVE PRIS-PRARTNTO           TO WS-FIXAD-PRARTNTO              
603000           MOVE 'SEK'                   TO WS-KDVALISO                    
603100           IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                   
603200             MOVE PRIS-KDVALISO         TO WS-KDVALISO                    
603300           END-IF                                                         
603400           IF SEND-DCS-DDC                                                
603500              MOVE '1441 '       TO INL-IDLEVNR                           
603600           ELSE                                                           
603700              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
603800           END-IF                                                         
603900                                                                          
604000           PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                          
604100           MOVE 6302-IDDISTR       TO WS-SAP-IDDISTR                      
604200           MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                        
604300           PERFORM S09-SKAPA-EKOTRANS-A03                                 
604400                                                                          
604500           PERFORM IMS-GET-WDK711                                         
604600           MOVE SLAG-PRAVCOST           TO EKOTRA03-PRAVCOST-OLD          
604700           MOVE WS-KVANTMOT-INM-NUM     TO AVG-KVANTMOT                   
604800                                        EKOTRA03-KVANTMOT                 
604900           MOVE ZERO                    TO EKOTRA03-KVSKROT               
605000           MOVE '21'                    TO EKOTRA03-KDANMORS              
605100                                        6308-KDANMORS                     
605200           MOVE WS-SUMMA-KVANT          TO 6308-KVLEVANM                  
605300                                                                          
605400           PERFORM S11-SKAPA-LEVANM-TRANS                                 
605500           MOVE 'SJU'     TO STEXT                                        
605600           PERFORM S10-OMRAKN-MEDELPRIS-NA                                
605700                                                                          
605800           MOVE W-TIME-N                TO AKTUELL-TID                    
605900           MOVE MSGI-TILOKDAT           TO SLAG-TIAVCOST                  
606000           MOVE AVG-PRAVCOST-NEW        TO EKOTRA03-PRAVCOST              
606100                                        SLAG-PRAVCOST                     
606200           MOVE AVG-REMARKUP            TO EKOTRA03-REMARKUP              
606300           MOVE AVG-PRKURS              TO EKOTRA03-PRKURS                
606400                                                                          
606500           IF WS-KVANTMOT-INM     > 0                                     
606600              PERFORM IMS-REPL-WDK711                                     
606700           ELSE                                                           
606800              MOVE EKOTRA03-PRAVCOST-OLD                                  
606900                                     TO EKOTRA03-PRAVCOST                 
607000           END-IF                                                         
607100                                                                          
607200           IF REQU-CMD-INM = 'DAM'                                        
607300              MOVE '43'                  TO 6308-KDANMORS                 
607400              MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM                    
607500                                         EKOTRA03-KVSKROT                 
607600              PERFORM S11-SKAPA-LEVANM-TRANS                              
607700           END-IF                                                         
607800           PERFORM S20-SKAPA-REFILLTRANS                                  
607900                                                                          
608000        ELSE                                                              
608100                                                                          
608200          IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                           
608300              AND (DCS-NDC-NA AND DCS-CANADA)                             
608400          OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                        
608500              AND (DCS-NDC-NA AND DCS-USA)                                
608600                                                                          
608700              MOVE SEND-WS-IDDC TO W-IDDC                                 
608800              PERFORM IMS-GET-WDK711                                      
608900              MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                     
609000              IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                      
609100                 MOVE 'CAD'         TO WS-KDVALISO                        
609200              ELSE                                                        
609300                 MOVE 'USD'         TO WS-KDVALISO                        
609400              END-IF                                                      
609500                                                                          
609600              MOVE WS-IDDC TO W-IDDC                                      
609700              PERFORM IMS-GET-WDK711                                      
609800              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
609900              PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                       
610000                                                                          
610100              PERFORM S09-SKAPA-EKOTRANS-A03                              
610200                                                                          
610300              MOVE SLAG-PRAVCOST          TO EKOTRA03-PRAVCOST-OLD        
610400              MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                    
610500                                          EKOTRA03-KVANTMOT               
610600              MOVE ZERO                   TO EKOTRA03-KVSKROT             
610700              MOVE '21'                   TO EKOTRA03-KDANMORS            
610800                                          6308-KDANMORS                   
610900              MOVE WS-SUMMA-KVANT         TO 6308-KVLEVANM                
611000                                                                          
611100              PERFORM S11-SKAPA-LEVANM-TRANS                              
611200              MOVE 'ATT'     TO STEXT                                     
611300              PERFORM S10-OMRAKN-MEDELPRIS-NA                             
611400                                                                          
611500              MOVE W-TIME-N               TO AKTUELL-TID                  
611600              MOVE MSGI-TILOKDAT          TO SLAG-TIAVCOST                
611700              MOVE AVG-PRAVCOST-NEW       TO EKOTRA03-PRAVCOST            
611800                                          SLAG-PRAVCOST                   
611900              MOVE AVG-REMARKUP           TO EKOTRA03-REMARKUP            
612000              MOVE AVG-PRKURS             TO EKOTRA03-PRKURS              
612100                                                                          
612200              IF WS-KVANTMOT-INM     > 0                                  
612300                 PERFORM IMS-REPL-WDK711                                  
612400              ELSE                                                        
612500                 MOVE EKOTRA03-PRAVCOST-OLD                               
612600                                       TO EKOTRA03-PRAVCOST               
612700              END-IF                                                      
612800                                                                          
612900              IF REQU-CMD-INM = 'DAM'                                     
613000                 MOVE '42'                  TO 6308-KDANMORS              
613100                 MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM                 
613200                                            EKOTRA03-KVSKROT              
613300                 PERFORM S11-SKAPA-LEVANM-TRANS                           
613400              END-IF                                                      
613500                                                                          
613600              PERFORM S20-SKAPA-REFILLTRANS                               
613700          ELSE                                                            
613800                                                                          
613900              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
614000              AND (DCS-NDC-NA AND DCS-USA)                                
614100                                                                          
614200                 MOVE SEND-WS-IDDC TO W-IDDC                              
614300                 PERFORM IMS-GET-WDK711                                   
614400                 SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                   
614500                 MOVE SLAG-PRAVCOST         TO WS-FIXAD-PRARTNTO          
614600                 MOVE 'USD'                 TO WS-KDVALISO                
614700                 PERFORM IMS-REPL-WDK711                                  
614800* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
614900                 PERFORM S21-SALDOLOGG-DATA                               
615000                 MOVE SEND-WS-IDDC      TO LOGG-IDDC                      
615100                 MOVE 'MISC'            TO LOGG-IDHUVTYP                  
615200                 MOVE 'R34'             TO LOGG-IDSUBTYP                  
615300                 MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                  
615400                 MOVE SLAG-KVLS         TO LOGG-KVLS                      
615500                 MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                        
615600                 MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                    
615700                 MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                    
615800                 MOVE '-'               TO LOGG-IDTECKEN-KVLS             
615900                 MOVE SPACE             TO LOGG-IDTECKEN-KVAKS            
616000                 MOVE SPACE             TO LOGG-IDTECKEN-KVAKS-PAV        
616100                 MOVE SPACE             TO LOGG-IDTECKEN-KVEFRS           
616200                 PERFORM S22-ISRT-SALDOLOGG                               
616300* ---SLUT SALDOFÖRÄNDRINGAR                                               
616400                                                                          
616500                 MOVE WS-IDDC TO W-IDDC                                   
616600                 PERFORM IMS-GET-WDK711                                   
616700                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
616800                 PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                    
616900                                                                          
617000                 PERFORM S09-SKAPA-EKOTRANS-A03                           
617100                                                                          
617200                 MOVE SLAG-PRAVCOST TO EKOTRA03-PRAVCOST-OLD              
617300                 MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                 
617400                                             EKOTRA03-KVANTMOT            
617500                 MOVE ZERO                   TO EKOTRA03-KVSKROT          
617600                 MOVE '21'                   TO EKOTRA03-KDANMORS         
617700                                                                          
617800                 MOVE 'NIO'     TO STEXT                                  
617900                 PERFORM S10-OMRAKN-MEDELPRIS-NA                          
618000                                                                          
618100                 MOVE W-TIME-N               TO AKTUELL-TID               
618200                 MOVE MSGI-TILOKDAT          TO SLAG-TIAVCOST             
618300                 MOVE AVG-PRAVCOST-NEW       TO EKOTRA03-PRAVCOST         
618400                                             SLAG-PRAVCOST                
618500                 MOVE AVG-REMARKUP           TO EKOTRA03-REMARKUP         
618600                 MOVE AVG-PRKURS             TO EKOTRA03-PRKURS           
618700                 MOVE 1.0                    TO EKOTRA03-PRKURS           
618800                                                                          
618900                 IF WS-KVANTMOT-INM     > 0                               
619000                    PERFORM IMS-REPL-WDK711                               
619100                 ELSE                                                     
619200                    MOVE EKOTRA03-PRAVCOST-OLD                            
619300                                          TO EKOTRA03-PRAVCOST            
619400                 END-IF                                                   
619500                                                                          
619600                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
619700                 PERFORM S13-SKAPA-NDC-HIST-SANDANDE                      
619800                                                                          
619900                 IF REQU-CMD-INM = 'DAM'                                  
620000                    MOVE WS-KVSKROT-INM-NUM TO EKOTRA03-KVSKROT           
620100                 END-IF                                                   
620200                                                                          
620300                 PERFORM S20-SKAPA-REFILLTRANS                            
620400                                                                          
620500              END-IF                                                      
620600           END-IF                                                         
620700        END-IF                                                            
620800     ELSE                                                                 
620900        IF SEND-DCS-CDC                                                   
621000           PERFORM IMS-GU-WLARTC01                                        
621100           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
621200           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
621300           PERFORM IMS-GHNP-WLARTC11                                      
621400           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
621500           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
621600                                                                          
621700           SUBTRACT WS-SUMMA-KVANT FROM CLAG-KVLS                         
621800           PERFORM IMS-REPL-WLARTC11                                      
621900           PERFORM S21-SALDOLOGG-DATA                                     
622000           MOVE WC-CDC-SE      TO LOGG-IDDC                               
622100           MOVE 'MISC'         TO LOGG-IDHUVTYP                           
622200           MOVE 'R34'          TO LOGG-IDSUBTYP                           
622300           MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                        
622400           MOVE CLAG-KVLS      TO LOGG-KVLS                               
622500           MOVE CLAG-KVAKS-CDC TO LOGG-KVAKS                              
622600           MOVE CLAG-KVEFRS    TO LOGG-KVEFRS                             
622700           MOVE CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                          
622800           MOVE '-'            TO LOGG-IDTECKEN-KVLS                      
622900           MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                     
623000           MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                 
623100           MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                    
623200           PERFORM S22-ISRT-SALDOLOGG                                     
623300           PERFORM S07-SKAPA-HISTORIK                                     
623400           IF DCS-AUSTRALIA OR DCS-JAPAN                                  
623500              PERFORM S20-SKAPA-REFILLTRANS                               
623600           END-IF                                                         
623700        ELSE                                                              
623800           PERFORM IMS-GU-WLARTC01                                        
623900           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
624000           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
624100           PERFORM IMS-GHNP-WLARTC11                                      
624200           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
624300           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
624400                                                                          
624500           IF SEND-DCS-AUSTRALIA OR SEND-DCS-JAPAN OR SEND-DCS-SDC        
624600              MOVE W-IDDC TO WS-SPARAT-IDDC                               
624700              MOVE W-IDDC-B6-SEND TO W-IDDC                               
624800              PERFORM IMS-GET-WDK711                                      
624900              SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                      
625000              PERFORM IMS-REPL-WDK711                                     
625100              MOVE WS-SPARAT-IDDC     TO W-IDDC                           
625200******* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                
625300              PERFORM S21-SALDOLOGG-DATA                                  
625400              MOVE W-IDDC-B6-SEND TO LOGG-IDDC                            
625500              MOVE 'MISC'         TO LOGG-IDHUVTYP                        
625600              MOVE 'R34'          TO LOGG-IDSUBTYP                        
625700              MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                     
625800              MOVE SLAG-KVLS      TO LOGG-KVLS                            
625900              MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                           
626000              MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                          
626100              MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                       
626200              MOVE '-'            TO LOGG-IDTECKEN-KVLS                   
626300              MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                  
626400              MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV              
626500              MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                 
626600              PERFORM S22-ISRT-SALDOLOGG                                  
626700              PERFORM S13-SKAPA-NDC-HIST-SANDANDE                         
626800              IF DCS-AUSTRALIA OR DCS-JAPAN                               
626900                 PERFORM S20-SKAPA-REFILLTRANS                            
627000              END-IF                                                      
627100           END-IF                                                         
627200        END-IF                                                            
627300                                                                          
627400        MOVE ZERO  TO WS-FIXAD-PRARTNTO                                   
627500        MOVE SPACE TO WS-KDVALISO                                         
627600        IF SEND-DCS-DDC                                                   
627700           MOVE '1441 '             TO INL-IDLEVNR                        
627800        ELSE                                                              
627900           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
628000        END-IF                                                            
628100        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT-NA                             
628200        MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                            
628300        MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                           
628400        PERFORM S31-SKAPA-SAP-TRANS-VCCS                                  
628500        IF SEND-DCS-DDC                                                   
628600           MOVE WS-SUMMA-KVANT        TO FILC2-KVANTAL                    
628700           MOVE 'ÖVERLEV.'            TO FILC2-AVVIKELSETYP               
628800           PERFORM S32-SKAPA-DIFF-TRANS                                   
628900           IF WS-KVSKROT-INM-NUM > ZERO                                   
629000              MOVE WS-KVSKROT-INM-NUM TO FILC2-KVANTAL                    
629100              MOVE 'DAM'              TO FILC2-AVVIKELSETYP               
629200              PERFORM S32-SKAPA-DIFF-TRANS                                
629300           END-IF                                                         
629400        END-IF                                                            
629500                                                                          
629600        MOVE ZERO           TO FILC3-KVAVIS                               
629700        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
629800        MOVE 'NY'           TO FILC3-AVVIKELSETYP                         
629900        MOVE 6              TO FILC3-KDSORT1                              
630000        PERFORM S33-SKAPA-LDC-TRANS                                       
630100        IF WS-KVSKROT-INM-NUM > ZERO                                      
630200           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
630300           MOVE 'DAM'              TO FILC3-AVVIKELSETYP                  
630400           MOVE 5                  TO FILC3-KDSORT1                       
630500           PERFORM S33-SKAPA-LDC-TRANS                                    
630600        END-IF                                                            
630700                                                                          
630800        IF REQU-CMD-INM = 'DAM'                                           
630900           MOVE JA TO NY-SKROT-SW                                         
631000           PERFORM S99-SKAPA-SKROT-ORDER                                  
631100        END-IF                                                            
631200     END-IF                                                               
631300                                                                          
631400     PERFORM S08-UPPDATERA-6301                                           
631500     .                                                                    
631600     EJECT                                                                
631700                                                                          
631800 HG-NY-NYUPPLAEGG-ART      SECTION.                                       
631900     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
632000        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
632100        PERFORM IMS-GU-WDB601-SEND                                        
632200     END-IF                                                               
632300                                                                          
632400     MOVE ZERO            TO W-KVRADER                                    
632500     ADD +1               TO W-KVRADER                                    
632600     MOVE JA              TO NYUPPLAEGG-SW                                
632700     MOVE REQU-IDDC-KEY   TO W-IDDC                                       
632800     MOVE ZERO            TO WS-OLD-KVLS                                  
632900     MOVE ZERO            TO WS-OLD-KVEFRS                                
633000                                                                          
633100     MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                          
633200     MOVE WS-KVSKROT-INM  TO WS-KVSKROT-INM-NUM                           
633300     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM +                      
633400           WS-KVSKROT-INM-NUM)                                            
633500     END-COMPUTE                                                          
633600     MOVE WS-SUMMA-KVANT  TO W-TEMP-KVANT                                 
633700     MOVE ZERO            TO W-KVAVIS                                     
633800                                                                          
633900     MOVE ALL '+'         TO WDK7-W005WDK7                                
634000     MOVE 'WDK711'        TO WDK7-IDSEGM                                  
634100     MOVE W-IDARTNR-INM   TO WDK7-IDARTNR-KFB                             
634200     MOVE REQU-IDDC-KEY   TO WDK7-IDDC-KFB                                
634300                             WDK7-IDDC                                    
634400     MOVE W-ADLAGOMR-INM  TO WDK7-ADLAGOMR                                
634500                             W-ADLAGOMR-SPAR                              
634600                             W-ADLAGOMR-LOCB                              
634700     MOVE W-ADGANG-INM    TO WDK7-ADGANG                                  
634800                             W-ADGANG-SPAR                                
634900                             W-ADGANG-LOCB                                
635000     MOVE W-ADPLATS-INM   TO WDK7-ADPLATS                                 
635100                             W-ADPLATS-SPAR                               
635200                             W-ADPLATS-LOCB                               
635300                                                                          
635400     IF SEND-DCS-CHINA                                                    
635500       MOVE WS-SUMMA-KVANT      TO WDK7-KVLS                              
635600     ELSE                                                                 
635700* (SWEDEN TO CHINA)                                                       
635800       MOVE WS-KVANTMOT-INM-NUM TO WDK7-KVLS                              
635900     END-IF                                                               
636000     IF XDC-NON-VCC-OWNED                                                 
636100**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
636200**** REST OF FLOWS TO US IS IN LAB.                                       
636300**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
636400**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
636500     OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                            
636600        MOVE JA                  TO WDK7-FLORDSP                          
636700                                    WDK7-FLSPBULK                         
636800     END-IF                                                               
636900                                                                          
637000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
637100                                       ARTC-PCB WDK7-PCB                  
637200     MOVE WDK7-WDK711     TO SLAG-WDK711                                  
637300** WRITE WDK728 WHEN FLTRACK='J'                                          
637400     IF REQU-KVANTMOT-INM NOT = ALL '+'  OR SPACE                         
637500        IF WS-FLTRACK = 'J'                                               
637600           MOVE WS-KVANTMOT-INM-NUM TO WS-TRCK-KVANTMOT                   
637700           MOVE ZERO                TO WS-TRCK-KVAVIS                     
637800           MOVE W-IDTRACK           TO TINL-IDTRACK                       
637900           PERFORM S39-UPDATE-WDK728                                      
638000        END-IF                                                            
638100     END-IF                                                               
638200                                                                          
638300     PERFORM S30-UPPDATERA-WDJ9                                           
638400                                                                          
638500     MOVE REQU-IDDC-KEY     TO W-IDDC                                     
638600     PERFORM IMS-GET-WDK711-GE                                            
638700                                                                          
638800* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
638900     PERFORM S21-SALDOLOGG-DATA                                           
639000     MOVE REQU-IDDC-KEY  TO LOGG-IDDC                                     
639100     MOVE SLAG-KVLS      TO LOGG-KVLS                                     
639200     MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                                    
639300     MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                                   
639400     MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                                
639500     MOVE '+'            TO LOGG-IDTECKEN-KVLS                            
639600     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                           
639700     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                       
639800     MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                          
639900     IF SEND-DCS-CHINA                                                    
640000        MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                           
640100     ELSE                                                                 
640200        MOVE WS-KVANTMOT-INM-NUM TO LOGG-KVART-SALDO                      
640300     END-IF                                                               
640400     PERFORM S22-ISRT-SALDOLOGG                                           
640500* ---SLUT SALDOFÖRÄNDRINGAR                                               
640600                                                                          
640700     IF SEND-DCS-CHINA                                                    
640800        PERFORM IMS-GU-WLARTC01                                           
640900        MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                             
641000        MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                             
641100        PERFORM IMS-GHNP-WLARTC11                                         
641200        MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                         
641300        MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                        
641400        MOVE W-IDDC TO WS-SPARAT-IDDC                                     
641500        MOVE SEND-WS-IDDC TO W-IDDC                                       
641600        PERFORM IMS-GET-WDK711                                            
641700        SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                            
641800        PERFORM IMS-REPL-WDK711                                           
641900        MOVE WS-SPARAT-IDDC     TO W-IDDC                                 
642000******* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                
642100        PERFORM S21-SALDOLOGG-DATA                                        
642200        MOVE SEND-WS-IDDC   TO LOGG-IDDC                                  
642300        MOVE 'MISC'         TO LOGG-IDHUVTYP                              
642400        MOVE 'R34'          TO LOGG-IDSUBTYP                              
642500        MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                           
642600        MOVE SLAG-KVLS      TO LOGG-KVLS                                  
642700        MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                                 
642800        MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                                
642900        MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                             
643000        MOVE '-'            TO LOGG-IDTECKEN-KVLS                         
643100        MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                        
643200        MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                    
643300        MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                       
643400        PERFORM S22-ISRT-SALDOLOGG                                        
643500        PERFORM S37-SKAPA-NDC-HIST-SANDANDE                               
643600******* ---SLUT SALDOFÖRÄNDRINGAR                                         
643700        MOVE ZERO  TO WS-FIXAD-PRARTNTO                                   
643800        MOVE SPACE TO WS-KDVALISO                                         
643900        IF SEND-DCS-DDC                                                   
644000           MOVE '1441 '    TO INL-IDLEVNR                                 
644100        ELSE                                                              
644200           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
644300        END-IF                                                            
644400        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                
644500        MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                            
644600        MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                           
644700                                                                          
644800        PERFORM S16-PRIS-TILLAMPNING                                      
644900        MOVE PRIS-PRARTNTO   TO R8-EKH-PRARTNTO                           
645000        PERFORM S31-SKAPA-SAP-TRANS                                       
645100                                                                          
645200        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
645300        MOVE ZERO           TO FILC3-KVAVIS                               
645400        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
645500        MOVE 'NY'           TO FILC3-AVVIKELSETYP                         
645600        MOVE 6              TO FILC3-KDSORT1                              
645700        PERFORM S33-SKAPA-LDC-TRANS                                       
645800        IF WS-KVSKROT-INM-NUM > ZERO                                      
645900           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
646000           MOVE 'DAM'              TO FILC3-AVVIKELSETYP                  
646100           MOVE 5                  TO FILC3-KDSORT1                       
646200           PERFORM S33-SKAPA-LDC-TRANS                                    
646300        END-IF                                                            
646400                                                                          
646500        IF REQU-CMD-INM = 'DAM'                                           
646600           MOVE JA TO NY-SKROT-SW                                         
646700           PERFORM S99-SKAPA-SKROT-ORDER                                  
646800        END-IF                                                            
646900     ELSE                                                                 
647000                                                                          
647100        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
647200        MOVE ZERO           TO FILC3-KVAVIS                               
647300        MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                              
647400        MOVE 'NY'           TO FILC3-AVVIKELSETYP                         
647500        MOVE 6              TO FILC3-KDSORT1                              
647600        PERFORM S33-SKAPA-LDC-TRANS                                       
647700        IF WS-KVSKROT-INM-NUM > ZERO                                      
647800           MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                       
647900           MOVE 'DAM'              TO FILC3-AVVIKELSETYP                  
648000           MOVE 5                  TO FILC3-KDSORT1                       
648100           PERFORM S33-SKAPA-LDC-TRANS                                    
648200        END-IF                                                            
648300                                                                          
648400        PERFORM IMS-GU-WLARTC01                                           
648500        MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                             
648600        MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                             
648700        PERFORM IMS-GHNP-WLARTC11                                         
648800        MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                         
648900        MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                        
649000        IF NOT SEND-DCS-CDC AND                                           
649100           NOT SEND-DCS-DDC                                               
649200         MOVE W-IDDC TO WS-SPARAT-IDDC                                    
649300         MOVE SEND-WS-IDDC TO W-IDDC                                      
649400         PERFORM IMS-GET-WDK711                                           
649500         SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                           
649600         PERFORM IMS-REPL-WDK711                                          
649700         MOVE WS-SPARAT-IDDC     TO W-IDDC                                
649800        END-IF                                                            
649900                                                                          
650000        PERFORM S16-PRIS-TILLAMPNING                                      
650100        MOVE PRIS-PRARTNTO        TO WS-FIXAD-PRARTNTO                    
650200        MOVE DCS-KDVALISO         TO WS-KDVALISO                          
650300        IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                      
650400          MOVE PRIS-KDVALISO      TO WS-KDVALISO                          
650500        END-IF                                                            
650600        IF SEND-DCS-DDC                                                   
650700           MOVE '1441 '    TO INL-IDLEVNR                                 
650800        ELSE                                                              
650900           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
651000        END-IF                                                            
651100                                                                          
651200        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                
651300        MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                            
651400        MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                           
651500                                                                          
651600        PERFORM IMS-GET-WDK711                                            
651700        MOVE WS-KVANTMOT-INM-NUM  TO AVG-KVANTMOT                         
651800        MOVE '21'                 TO 6308-KDANMORS                        
651900        MOVE WS-SUMMA-KVANT       TO 6308-KVLEVANM                        
652000                                                                          
652100        PERFORM S11-SKAPA-LEVANM-TRANS                                    
652200                                                                          
652300        PERFORM S16-PRIS-TILLAMPNING                                      
652400        MOVE PRIS-PRARTNTO        TO R8-EKH-PRARTNTO                      
652500        PERFORM S31-SKAPA-SAP-TRANS                                       
652600                                                                          
652700        IF WS-KVANTMOT-INM  > 0                                           
652800           PERFORM IMS-REPL-WDK711                                        
652900        END-IF                                                            
653000                                                                          
653100        IF REQU-CMD-INM = 'DAM'                                           
653200           MOVE '43'               TO 6308-KDANMORS                       
653300           MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM                       
653400           PERFORM S11-SKAPA-LEVANM-TRANS                                 
653500        END-IF                                                            
653600        PERFORM S20-SKAPA-REFILLTRANS                                     
653700     END-IF                                                               
653800                                                                          
653900* BERÄKNING AVGCOST                                                       
654000     MOVE REQU-IDDC-KEY        TO W-IDDC                                  
654100     PERFORM IMS-GET-WDK711                                               
654200     MOVE WS-KVANTMOT-INM-NUM  TO AVG-KVANTMOT                            
654300     PERFORM S10-OMRAKN-MEDELPRIS                                         
654400     MOVE W-TIME-N             TO AKTUELL-TID                             
654500     MOVE W-DAGENS-DATUM       TO SLAG-TIAVCOST                           
654600     MOVE AVG-PRAVCOST-NEW     TO SLAG-PRAVCOST                           
654700     PERFORM IMS-REPL-WDK711                                              
654800                                                                          
654900     PERFORM S08-UPPDATERA-6301                                           
655000     .                                                                    
655100     EJECT                                                                
655200 I-KOLLA-INDATA-FINNS SECTION.                                            
655300                                                                          
655400     MOVE NEJ TO INDATA-FINNS-SW                                          
655500                                                                          
655600     IF REQU-KDPGMACT = 'X'                                               
655700        MOVE +1 TO INDX                                                   
655800        PERFORM UNTIL INDX > MAX-INDX OR INDATA-FINNS                     
655900            IF  X-REQU-IDKUNDRF(INDX) NOT = ALL '+'                       
656000            OR  X-REQU-IDKUNDNR(INDX) NOT = ALL '+'                       
656100            OR  X-REQU-IDKOLLI (INDX) NOT = ALL '+'                       
656200                MOVE JA      TO INDATA-FINNS-SW                           
656300            END-IF                                                        
656400            ADD +1 TO INDX                                                
656500        END-PERFORM                                                       
656600     END-IF                                                               
656700                                                                          
656800     IF  REQU-KOLLI-KLART NOT = ALL '+'                                   
656900     AND REQU-KOLLI-KLART NOT = SPACE                                     
657000         MOVE JA TO INDATA-FINNS-SW                                       
657100     END-IF                                                               
657200                                                                          
657300     MOVE +1 TO INDX                                                      
657400     PERFORM UNTIL INDX > MAX-INDX OR INDATA-FINNS                        
657500         IF  REQU-CMD-IN (INDX) NOT = ALL '+'                             
657600         OR  REQU-KVANTMOT-IN (INDX) NOT = ALL '+'                        
657700         OR  REQU-KVSKROT-IN (INDX) NOT = ALL '+'                         
657800         OR  REQU-ADLAGOMR (INDX) NOT = ALL '+'                           
657900         OR  REQU-ADGANG (INDX) NOT = ALL '+'                             
658000         OR  REQU-ADPLATS (INDX) NOT = ALL '+'                            
658100             MOVE JA TO INDATA-FINNS-SW                                   
658200         END-IF                                                           
658300         ADD +1 TO INDX                                                   
658400     END-PERFORM                                                          
658500                                                                          
658600     IF  INDATA-SAKNAS                                                    
658700         IF  REQU-IDARTNR-INM NOT = ALL '+'                               
658800         OR  REQU-KVANTMOT-INM NOT = ALL '+'                              
658900         OR  REQU-KVSKROT-INM NOT = ALL '+'                               
659000         OR  REQU-ADLAGOMR-INM NOT = ALL '+'                              
659100         OR  REQU-ADGANG-INM NOT = ALL '+'                                
659200         OR  REQU-ADPLATS-INM NOT = ALL '+'                               
659300         OR  REQU-CMD-INM NOT    = ALL '+'                                
659400             MOVE JA TO INDATA-FINNS-SW                                   
659500         END-IF                                                           
659600     END-IF                                                               
659700     .                                                                    
659800     EJECT                                                                
659900 K-UPPDATERA-X-TRANS SECTION.                                             
660000                                                                          
660100     MOVE +1 TO TAB-IX                                                    
660200     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
660300       IF BIN-TAB-IDFAKT(TAB-IX) > ZERO                                   
660400           MOVE BIN-TAB-IDFAKT(TAB-IX)     TO W-SPAR-IDFAKT               
660500           MOVE BIN-TAB-IDKUNDRF(TAB-IX)   TO W-SPAR-IDKUNDRF             
660600           MOVE BIN-TAB-IDKUNDNR(TAB-IX)   TO W-SPAR-IDKUNDNR             
660700           MOVE BIN-TAB-IDKOLLI(TAB-IX)    TO W-SPAR-IDKOLLI              
660800                                                                          
660900           IF XDC-NON-VCC-OWNED                                           
661000**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
661100**** REST OF FLOWS TO US IS IN LAB.                                       
661200**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
661300**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
661400           OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                      
661500              IF BIN-TAB-CMD-IN(TAB-IX) = 'HAC'                           
661600                 PERFORM HA-UPPDATERA-KOLLI-KLART-HAC                     
661700              ELSE                                                        
661800                 PERFORM HA-UPPDATERA-KOLLI-KLART                         
661900              END-IF                                                      
662000           ELSE                                                           
662100              PERFORM HA-UPPDATERA-KOLLI-KLART-PV                         
662200           END-IF                                                         
662300       END-IF                                                             
662400       ADD +1 TO TAB-IX                                                   
662500     END-PERFORM                                                          
662600     .                                                                    
662700     EJECT                                                                
662800 X-KOLLA-INDATA-IGEN SECTION.                                             
662900                                                                          
663000     MOVE NEJ TO X-SW                                                     
663100                                                                          
663200     IF  REQU-KOLLI-KLART NOT = ALL '+'                                   
663300     AND REQU-KOLLI-KLART NOT = SPACE                                     
663400         MOVE JA TO X-SW                                                  
663500     END-IF                                                               
663600                                                                          
663700     MOVE +1 TO INDX                                                      
663800     PERFORM UNTIL INDX > MAX-INDX                                        
663900         IF  REQU-CMD-IN (INDX) NOT = ALL '+'                             
664000         OR  REQU-KVANTMOT-IN (INDX) NOT = ALL '+'                        
664100         OR  REQU-KVSKROT-IN (INDX) NOT = ALL '+'                         
664200             MOVE JA TO X-SW                                              
664300         END-IF                                                           
664400         ADD +1 TO INDX                                                   
664500     END-PERFORM                                                          
664600                                                                          
664700     IF  X-SW = NEJ                                                       
664800         IF  REQU-IDARTNR-INM NOT = ALL '+'                               
664900         OR  REQU-KVANTMOT-INM NOT = ALL '+'                              
665000         OR  REQU-KVSKROT-INM NOT = ALL '+'                               
665100         OR  REQU-ADLAGOMR-INM NOT = ALL '+'                              
665200         OR  REQU-ADGANG-INM NOT = ALL '+'                                
665300         OR  REQU-ADPLATS-INM NOT = ALL '+'                               
665400         OR  REQU-CMD-INM NOT    = ALL '+'                                
665500             MOVE JA TO X-SW                                              
665600         END-IF                                                           
665700     END-IF                                                               
665800                                                                          
665900     IF X-SW = NEJ                                                        
666000        MOVE NEJ TO INDATA-SW                                             
666100        MOVE 'CMD' TO RESP-IDELMT-ERROR                                   
666200     END-IF                                                               
666300     .                                                                    
666400     EJECT                                                                
666500                                                                          
666600 S01-DISTRIKT-RETUR         SECTION.                                      
666700     MOVE REQU-IDDC-KEY      TO W-IDDC-B6                                 
666800     PERFORM IMS-GU-WDB601                                                
666900     IF DCS-CHINA OR DCS-CDC                                              
667000       IF DCS-CDC                                                         
667100         MOVE DCS-IDDISTR-RSKROT TO W-IDDISTR-RETUR                       
667200         IF SEND-DCS-NDC-CN                                               
667300           MOVE '000071'         TO W-IDKUNDNR-RETUR                      
667400         ELSE                                                             
667500           MOVE DCS-IDKUNDNR-RSKROT TO W-IDKUNDNR-RETUR                   
667600         END-IF                                                           
667700       ELSE                                                               
667800         MOVE DCS-IDDISTR-RSKROT  TO W-IDDISTR-RETUR                      
667900         MOVE DCS-IDKUNDNR-RSKROT TO W-IDKUNDNR-RETUR                     
668000       END-IF                                                             
668100     ELSE                                                                 
668200       MOVE DCS-IDDISTR-QSKROT    TO W-IDDISTR-RETUR                      
668300       MOVE DCS-IDKUNDNR-QSKROT   TO W-IDKUNDNR-RETUR                     
668400     END-IF                                                               
668500                                                                          
668600     .                                                                    
668700     EJECT                                                                
668800 S03-CALL-W006KOM SECTION.                                                
668900     MOVE KOM-AREA        TO P-TO-P-DATA                                  
669000     CALL W006KOM         USING MSG-PCB                                   
669100                                ALT-PCB                                   
669200                                KOMA-PCB                                  
669300                                MSG-KOM-WMSGKOM                           
669400                                P-TO-P-AREA                               
669500     .                                                                    
669600     EJECT                                                                
669700                                                                          
669800 S04-SKAPA-DAINLEV-IDLOPNRM SECTION.                                      
669900     IF W-IDLOPNRM = ZERO                                                 
670000        PERFORM IMS-GHU-W6LOPA11                                          
670100        MOVE 6018-IDLOPNRM TO W-IDLOPNRM                                  
670200     END-IF                                                               
670300                                                                          
670400     IF DAT-TIAAVVD-GRP (3:3) = W-VVD                                     
670500       ADD +1 TO W-LLLL                                                   
670600     ELSE                                                                 
670700       MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                                
670800       MOVE +1 TO W-LLLL                                                  
670900     END-IF                                                               
671000                                                                          
671100     CALL CHECK USING W-IDLOPNRM (2:7) FLT-LGD                            
671200          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
671300                                                                          
671400     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
671500     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
671600     COMPUTE W-DAINLEV          = 9999999999999999                        
671700                                 - W-TIAAAAMMDDTTMMSSTH                   
671800     END-COMPUTE                                                          
671900     .                                                                    
672000     EJECT                                                                
672100                                                                          
672200 S07-SKAPA-HISTORIK SECTION.                                              
672300     PERFORM S04-SKAPA-DAINLEV-IDLOPNRM                                   
672400                                                                          
672500     PERFORM IMS-GU-WLINLE01                                              
672600     IF SEGMENT-SAKNAS                                                    
672700       MOVE W-IDARTNR TO INLE-ART-IDARTNR                                 
672800       PERFORM IMS-ISRT-WLINLE01                                          
672900     END-IF                                                               
673000                                                                          
673100     MOVE W-DAINLEV TO INLE-INL-DAINLEV                                   
673200     PERFORM IMS-ISRT-WLINLE11                                            
673300     PERFORM UNTIL INSERT-OK                                              
673400       SUBTRACT 1 FROM W-DAINLEV                                          
673500       MOVE W-DAINLEV TO INLE-INL-DAINLEV                                 
673600       PERFORM IMS-ISRT-WLINLE11                                          
673700     END-PERFORM                                                          
673800                                                                          
673900     MOVE 'R34'            TO INLE-DIR-IDPTYP                             
674000     MOVE W-IDLOPNRM       TO INLE-DIR-IDLOPNRM                           
674100     MOVE W-SPAR-IDFAKT    TO INLE-DIR-IDAVINR                            
674200     MOVE W-DAGENS-DATUM   TO INLE-DIR-TIAVSDAT                           
674300     MOVE WC-CDC-SE        TO INLE-DIR-IDDC                               
674400     MOVE +8               TO INLE-DIR-KDRT                               
674500     MOVE SPACE            TO INLE-DIR-IDKST                              
674600     MOVE SPACE            TO INLE-DIR-IDANALYS                           
674700                                                                          
674800     SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                                  
674900                       GIVING INLE-DIR-KVAVIS                             
675000     IF W-KVAVIS < W-TEMP-KVANT                                           
675100        MOVE SEND-DCS-IDLEVNR-DC TO INLE-DIR-IDLEVNR                      
675200        MOVE DCS-IDLEVNR-DC      TO INLE-DIR-IDKONTO                      
675300     ELSE                                                                 
675400        MOVE DCS-IDLEVNR-DC      TO INLE-DIR-IDLEVNR                      
675500        MOVE SEND-DCS-IDLEVNR-DC TO INLE-DIR-IDKONTO                      
675600     END-IF                                                               
675700     MOVE ZERO             TO INLE-DIR-IDDISTR                            
675800                              INLE-DIR-IDPRODNR                           
675900     MOVE W-SPAR-IDKUNDNR  TO INLE-DIR-IDKUNDNR                           
676000     MOVE W-SPAR-IDKUNDRF  TO INLE-DIR-IDKUNDRF                           
676100     MOVE W-SPAR-IDFAKT    TO INLE-DIR-IDFAKT                             
676200                                                                          
676300     PERFORM IMS-ISRT-WLINLE22                                            
676400     .                                                                    
676500     EJECT                                                                
676600                                                                          
676700 S08-UPPDATERA-6301 SECTION.                                              
676800     MOVE ZERO            TO W-KVKOLLI-MOT                                
676900     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
677000     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
677100     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
677200                             W-SEQA-IDFAKT-MAX                            
677300     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
677400                             W-SEQA-IDKUNDRF-MAX                          
677500     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
677600                             W-SEQA-IDKUNDNR-MAX                          
677700     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
677800                             W-SEQA-IDKOLLI-MAX                           
677900                                                                          
678000     PERFORM IMS-GU-WLINLD01-FIRST                                        
678100     IF SEGMENT-SAKNAS                                                    
678200        MOVE +1 TO W-KVKOLLI-MOT                                          
678300     END-IF                                                               
678400                                                                          
678500     MOVE W-SPAR-IDFAKT TO W-IDFAKT                                       
678600     MOVE REQU-IDDC-KEY TO W-6301-IDDC                                    
678700     PERFORM IMS-GHU-WL630111                                             
678800     ADD W-KVRADER      TO 6302-KVRADER-MOT                               
678900     ADD W-KVKOLLI-MOT  TO 6302-KVKOLLI-MOT                               
679000     PERFORM IMS-REPL-WL630111                                            
679100     .                                                                    
679200     EJECT                                                                
679300                                                                          
679400 S09-SKAPA-EKOTRANS-A03 SECTION.                                          
679500     IF WS-A03-SKAPAD = JA                                                
679600        PERFORM S091-SKRIV-EKOTRANS-A03                                   
679700     END-IF                                                               
679800                                                                          
679900     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
680000        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
680100        PERFORM IMS-GU-WDB601-SEND                                        
680200     END-IF                                                               
680300                                                                          
680400     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
680500        MOVE WC-CDC-SE    TO EKOTRA03-IDDC-SEND                           
680600        MOVE INL-IDDISTR  TO DIST35-IDDISTR                               
680700                             TEST-IDDISTR                                 
680800        IF DIST35-REFILL-NA                                               
680900           MOVE 'T10'           TO EKOTRA03-KDEKOHT                       
681000           IF DIST35-CDC-NDC51-REFILL                                     
681100              MOVE 54           TO EKOTRA03-IDFTG                         
681200           ELSE                                                           
681300              MOVE 53           TO EKOTRA03-IDFTG                         
681400           END-IF                                                         
681500        ELSE                                                              
681600           IF DIST35-REFILL-NA-JAP                                        
681700              MOVE 'I20'        TO EKOTRA03-KDEKOHT                       
681800              IF DIST35-JAP-NDC41-REFILL                                  
681900              OR DIST35-JAP-NDC43-REFILL                                  
682000              OR DIST35-JAP-NDC44-REFILL                                  
682100                 MOVE 53         TO EKOTRA03-IDFTG                        
682200              ELSE                                                        
682300                 MOVE 54         TO EKOTRA03-IDFTG                        
682400              END-IF                                                      
682500           END-IF                                                         
682600        END-IF                                                            
682700        IF DIST79-DEALER-PRICE                                            
682800          MOVE INL-KDVALISO  TO EKOTRA03-KDVALISO                         
682900        ELSE                                                              
683000          MOVE 'SEK'         TO EKOTRA03-KDVALISO                         
683100        END-IF                                                            
683200     ELSE                                                                 
683300        MOVE 6302-IDDC-SEND  TO EKOTRA03-IDDC-SEND                        
683400        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
683500           AND (DCS-NDC-NA AND DCS-CANADA)                                
683600             MOVE 'T50'      TO EKOTRA03-KDEKOHT                          
683700             MOVE 54         TO EKOTRA03-IDFTG                            
683800             MOVE 'USD'      TO EKOTRA03-KDVALISO                         
683900        ELSE                                                              
684000           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
684100           AND (DCS-NDC-NA AND DCS-USA)                                   
684200             MOVE 'T40'      TO EKOTRA03-KDEKOHT                          
684300             MOVE 53         TO EKOTRA03-IDFTG                            
684400             MOVE 'CAD'      TO EKOTRA03-KDVALISO                         
684500           ELSE                                                           
684600              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
684700              AND (DCS-NDC-NA AND DCS-USA)                                
684800                  MOVE 'T30' TO EKOTRA03-KDEKOHT                          
684900                  MOVE 53    TO EKOTRA03-IDFTG                            
685000                  MOVE 'USD' TO EKOTRA03-KDVALISO                         
685100              END-IF                                                      
685200           END-IF                                                         
685300        END-IF                                                            
685400     END-IF                                                               
685500                                                                          
685600     MOVE 'A03'           TO EKOTRA03-IDPTYP                              
685700     MOVE INL-IDDISTR     TO EKOTRA03-IDDISTR                             
685800     MOVE INL-IDKUNDNR    TO EKOTRA03-IDKUNDNR                            
685900     MOVE 6302-TIFAKT     TO WS-SEKEL-KOLL                                
686000                             WS-EKOA03-AAMMDD                             
686100     IF WS-SEKEL = 9                                                      
686200        MOVE 19           TO WS-EKOA03-SS                                 
686300     ELSE                                                                 
686400        MOVE 20           TO WS-EKOA03-SS                                 
686500     END-IF                                                               
686600     MOVE WS-AAAAMMDD     TO EKOTRA03-DAFAKT                              
686700                                                                          
686800     MOVE ZERO            TO EKOTRA03-IDORDNR7                            
686900     MOVE INL-IDORDNR5    TO EKOTRA03-IDORDNR7                            
687000     MOVE INL-PRARTNTO    TO EKOTRA03-PRARTNTO                            
687100                                                                          
687200     MOVE INL-TIINLINL    TO WS-SEKEL-KOLL                                
687300                             WS-EKOA03-AAMMDD                             
687400     IF WS-SEKEL = 9                                                      
687500        MOVE 19           TO WS-EKOA03-SS                                 
687600     ELSE                                                                 
687700        MOVE 20           TO WS-EKOA03-SS                                 
687800     END-IF                                                               
687900     MOVE WS-AAAAMMDD     TO EKOTRA03-DAINLINL                            
688000                                                                          
688100     MOVE INL-KVAVIS      TO EKOTRA03-KVLEVART                            
688200     MOVE INL-PRKURS      TO EKOTRA03-PRKURS                              
688300     MOVE INL-IDKOLLI     TO EKOTRA03-IDKOLLI                             
688400     MOVE WS-OLD-KVLS     TO EKOTRA03-KVLS-OLD                            
688500     MOVE REQU-IDDC-KEY   TO EKOTRA03-IDDC-REC                            
688600     MOVE W-SPAR-IDFAKT   TO EKOTRA03-IDFAKT                              
688700     MOVE W-IDARTNR       TO EKOTRA03-IDARTNR                             
688800     MOVE W-KDPRODSL      TO EKOTRA03-KDPRODSL                            
688900     MOVE W-KDPRODSL-LOC  TO EKOTRA03-KDPSLLOC                            
689000     MOVE SPACE           TO EKOTRA03-FLSLUT                              
689100                                                                          
689200     MOVE JA              TO WS-A03-SKAPAD                                
689300     .                                                                    
689400     EJECT                                                                
689500                                                                          
689600 S091-SKRIV-EKOTRANS-A03 SECTION.                                         
689700     MOVE 'WL010300'        TO FIL-IDPGM IN FIL-WDR801                    
689800     MOVE W-DAGENS-DATUM    TO FIL-TIREGDAT                               
689900     ADD +1                 TO W-TIKLOCK                                  
690000     MOVE W-TIKLOCK         TO FIL-TIKLOCK IN FIL-WDR801                  
690100     ADD +1                 TO W-IDSEKVNR-A03                             
690200     MOVE W-IDSEKVNR-A03    TO FIL-IDSEKVNR IN FIL-WDR801                 
690300     MOVE 'W510'            TO FIL-CT-IDSYSTEM IN FIL-WDR801              
690400     MOVE 'A03'             TO FIL-CT-IDPTYP IN FIL-WDR801                
690500     MOVE ' '               TO FIL-CT-IDVTYP IN FIL-WDR801                
690600     MOVE EKOTRA03-W510A03  TO FIL-WDR801-DATA                            
690700                                                                          
690800     PERFORM IMS-ISRT-WLFILB01                                            
690900                                                                          
691000     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
691100        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
691200        PERFORM IMS-ISRT-WLFILB01                                         
691300     END-PERFORM                                                          
691400                                                                          
691500     MOVE NEJ TO WS-A03-SKAPAD                                            
691600     .                                                                    
691700     EJECT                                                                
691800                                                                          
691900 S10-OMRAKN-MEDELPRIS-NA SECTION.                                         
692000     MOVE REQU-IDDC-KEY  TO AVG-IDDC                                      
692100     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
692200     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
692300     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
692400     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
692500     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
692600     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
692700                                                                          
692800     IF NY-BEFINTLIG-ART                                                  
692900     OR NY-NYUPPLAEGG-ART                                                 
693000        MOVE WS-KDVALISO TO AVG-KDVALISO                                  
693100     END-IF                                                               
693200                                                                          
693300     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
693400        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
693500        PERFORM IMS-GU-WDB601-SEND                                        
693600     END-IF                                                               
693700                                                                          
693800     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
693900        IF DIST79-DEALER-PRICE                                            
694000          MOVE '010'       TO AVG-KDCALL                                  
694100          MOVE 1.0         TO AVG-PRKURS                                  
694200          MOVE 'USD'       TO AVG-KDVALISO                                
694300        ELSE                                                              
694400          MOVE '010'       TO AVG-KDCALL                                  
694500          MOVE INL-PRKURS  TO AVG-PRKURS                                  
694600        END-IF                                                            
694700     ELSE                                                                 
694800       IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                              
694900       AND (DCS-NDC-NA AND DCS-CANADA)                                    
695000         IF DIST79-DEALER-PRICE                                           
695100           MOVE '030'       TO AVG-KDCALL                                 
695200           MOVE 1.0         TO AVG-PRKURS                                 
695300           MOVE 'USD'       TO AVG-KDVALISO                               
695400         ELSE                                                             
695500           MOVE '030'      TO AVG-KDCALL                                  
695600           MOVE ZERO       TO AVG-PRKURS                                  
695700           MOVE 'USD'      TO AVG-KDVALISO                                
695800         END-IF                                                           
695900       ELSE                                                               
696000           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
696100           AND (DCS-NDC-NA AND DCS-USA)                                   
696200             IF DIST79-DEALER-PRICE                                       
696300               MOVE '030'      TO AVG-KDCALL                              
696400               MOVE 1.0         TO AVG-PRKURS                             
696500               MOVE 'USD'       TO AVG-KDVALISO                           
696600             ELSE                                                         
696700               MOVE '030'      TO AVG-KDCALL                              
696800               MOVE ZERO       TO AVG-PRKURS                              
696900               MOVE 'CAD'      TO AVG-KDVALISO                            
697000             END-IF                                                       
697100           ELSE                                                           
697200              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
697300              AND (DCS-NDC-NA AND DCS-USA)                                
697400                IF DIST79-DEALER-PRICE                                    
697500                  MOVE '020'       TO AVG-KDCALL                          
697600                  MOVE 1.0         TO AVG-PRKURS                          
697700                  MOVE 'USD'       TO AVG-KDVALISO                        
697800                ELSE                                                      
697900                  MOVE '020'      TO AVG-KDCALL                           
698000                  MOVE ZERO       TO AVG-PRKURS                           
698100                END-IF                                                    
698200              END-IF                                                      
698300           END-IF                                                         
698400       END-IF                                                             
698500     END-IF                                                               
698600                                                                          
698700     MOVE W-DAGENS-DATUM(1:2) TO AVG-TIAA                                 
698800     MOVE W-DAGENS-DATUM(3:2) TO AVG-TIMM                                 
698900     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
699000                        AVG-WDB6-PCB                                      
699100     IF AVG-KDSVAR = SPACE OR '4'                                         
699200        CONTINUE                                                          
699300     ELSE                                                                 
699400        MOVE KEYS-ARE-MISSING                                             
699500                         TO RESP-IDMSG-ERROR                              
699600        MOVE 'MARKUP IS' TO RESP-IDELMT-ERROR                             
699700        PERFORM IMS-ROLLBACK                                              
699800        IF SUB-KDTRANS(1:6) = 'WLA103'                                    
699900          PERFORM S11-MSG-CONV                                            
700000        END-IF                                                            
700100        PERFORM S17-RETURNERA-SVAR                                        
700200        MOVE ZERO TO RETURN-CODE                                          
700300        GOBACK                                                            
700400     END-IF                                                               
700500     .                                                                    
700600     EJECT                                                                
700700                                                                          
700800 S10-OMRAKN-MEDELPRIS    SECTION.                                         
700900     MOVE REQU-IDDC-KEY  TO AVG-IDDC                                      
701000     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
701100     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
701200     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
701300     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
701400     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
701500     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
701600                                                                          
701700     IF NY-BEFINTLIG-ART                                                  
701800     OR NY-NYUPPLAEGG-ART                                                 
701900        MOVE WS-KDVALISO TO AVG-KDVALISO                                  
702000     END-IF                                                               
702100                                                                          
702200     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
702300        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
702400        PERFORM IMS-GU-WDB601-SEND                                        
702500     END-IF                                                               
702600                                                                          
702700     MOVE W-DAGENS-DATUM(1:2) TO AVG-TIAA                                 
702800     MOVE W-DAGENS-DATUM(3:2) TO AVG-TIMM                                 
702900                                                                          
703000     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
703100     OR DIST35-VCC-NONVCC-REFILL OR DIST35-VCC-NONVCC-TRANSFER            
703200     OR DIST35-NONVCC-NONVCC-REFILL                                       
703300     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
703400     OR DIST35-NONVCC-VCC-TRANSFER                                        
703500**** HÄMTA RÄTT FAKTURAMÅNAD FÖR ATT RÄKNA UT RÄTT AVERAGECOST            
703600        COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                      
703700                                  - INL-DAINLEV                           
703800        MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                    
703900        MOVE WS-FAKTURA-DATUM(3:2) TO AVG-TIAA                            
704000        MOVE WS-FAKTURA-DATUM(5:2) TO AVG-TIMM                            
704100****                                                                      
704200        IF DIST79-DEALER-PRICE                                            
704300          IF XDC-NON-VCC-OWNED                                            
704400            MOVE SEND-DCS-KDVALISO   TO AVG-KDVALISO                      
704500            MOVE 1.0                 TO AVG-PRKURS                        
704600            IF NDC-IN                                                     
704700              MOVE '012'     TO AVG-KDCALL                                
704800            ELSE                                                          
704900              MOVE '011'     TO AVG-KDCALL                                
705000            END-IF                                                        
705100          END-IF                                                          
705200          IF NDC-US                                                       
705300            MOVE '010'     TO AVG-KDCALL                                  
705400          END-IF                                                          
705500        ELSE                                                              
705600          IF XDC-NON-VCC-OWNED                                            
705700            IF NDC-IN                                                     
705800              MOVE '012'     TO AVG-KDCALL                                
705900              MOVE ZERO      TO AVG-PRKURS                                
706000            ELSE                                                          
706100              MOVE '011'     TO AVG-KDCALL                                
706200              MOVE ZERO      TO AVG-PRKURS                                
706300            END-IF                                                        
706400          END-IF                                                          
706500          IF NDC-US                                                       
706600            MOVE '010'     TO AVG-KDCALL                                  
706700            MOVE ZERO      TO AVG-PRKURS                                  
706800          END-IF                                                          
706900        END-IF                                                            
707000     ELSE                                                                 
707100       IF ((SEND-DCS-NDC-CN)                                              
707200       AND (NDC-CN))                                                      
707300           IF DIST79-DEALER-PRICE                                         
707400             MOVE SEND-DCS-KDVALISO   TO AVG-KDVALISO                     
707500             MOVE 1.0                 TO AVG-PRKURS                       
707600             MOVE '021'     TO AVG-KDCALL                                 
707700           ELSE                                                           
707800             MOVE '021'    TO AVG-KDCALL                                  
707900             MOVE ZERO     TO AVG-PRKURS                                  
708000           END-IF                                                         
708100       END-IF                                                             
708200     END-IF                                                               
708300                                                                          
708400     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
708500                        AVG-WDB6-PCB                                      
708600     IF AVG-KDSVAR = SPACE OR '4'                                         
708700        CONTINUE                                                          
708800     ELSE                                                                 
708900        MOVE KEYS-ARE-MISSING                                             
709000                         TO RESP-IDMSG-ERROR                              
709100        MOVE 'MARKUP IS' TO RESP-IDELMT-ERROR                             
709200        PERFORM IMS-ROLLBACK                                              
709300        IF SUB-KDTRANS(1:6) = 'WLA103'                                    
709400          PERFORM S11-MSG-CONV                                            
709500        END-IF                                                            
709600        PERFORM S17-RETURNERA-SVAR                                        
709700        MOVE ZERO TO RETURN-CODE                                          
709800        GOBACK                                                            
709900     END-IF                                                               
710000     .                                                                    
710100     EJECT                                                                
710200                                                                          
710300 S11-MSG-CONV SECTION.                                                    
710400     MOVE SPACES                  TO RESP-MESSAGES (1)                    
710500                                     RESP-MESSAGES (2)                    
710600     MOVE 1                       TO MSG-IX                               
710700*    REQUEST OK                                                           
710800     MOVE 200                     TO RESP-KDSTATUS-API                    
710900     IF RESP-IDMSG-INFO > SPACE                                           
711000       MOVE SPACES                TO MSG-CONV-AREA                        
711100       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
711200       CALL WMSGCONV           USING MSG-CONV-AREA                        
711300       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
711400       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
711500       ADD 1                      TO MSG-IX                               
711600     END-IF                                                               
711700     IF RESP-IDMSG-ERROR > SPACE                                          
711800*      BAD REQUEST                                                        
711900       MOVE 400                   TO RESP-KDSTATUS-API                    
712000       MOVE SPACES                TO MSG-CONV-AREA                        
712100       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
712200       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
712300       CALL WMSGCONV           USING MSG-CONV-AREA                        
712400       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
712500       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
712600     END-IF                                                               
712700     .                                                                    
712800 S11-SKAPA-LEVANM-TRANS SECTION.                                          
712900                                                                          
713000     MOVE 6302-TIFAKT       TO 6306-TIFAKT                                
713100     MOVE 6302-IDFAKT       TO 6306-IDFAKT                                
713200     MOVE REQU-IDDC-KEY     TO 6306-IDDC-REC                              
713300     MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                             
713400     MOVE 6302-IDDC-LEV     TO 6306-IDDC-LEV                              
713500                                                                          
713600     PERFORM IMS-GHU-WL630511                                             
713700     IF SEGMENT-FINNS                                                     
713800        MOVE 6302-IDDISTR       TO 6308-IDDISTR                           
713900                                   TEST-IDDISTR                           
714000        MOVE 6302-IDKUNDNR      TO 6308-IDKUNDNR                          
714100        MOVE 6302-IDFAKT        TO 6308-IDRAPPNR                          
714200                                                                          
714300        IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                          
714400           MOVE ZERO              TO 6308-KDFRAKT                         
714500           MOVE W-SPAR-IDKUNDRF   TO 6308-IDKUNDRF                        
714600           MOVE W-SPAR-IDKOLLI    TO 6308-IDKOLLI                         
714700        ELSE                                                              
714800           MOVE INL-KDFRAKT       TO 6308-KDFRAKT                         
714900           MOVE INL-IDKUNDRF      TO 6308-IDKUNDRF                        
715000           MOVE INL-IDKOLLI       TO 6308-IDKOLLI                         
715100           MOVE INL-IDDISTR       TO 6308-IDDISTR                         
715200                                     TEST-IDDISTR                         
715300           MOVE INL-IDKUNDNR      TO 6308-IDKUNDNR                        
715400        END-IF                                                            
715500                                                                          
715600**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
715700**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA .REFILLDISTRIKT             
715800**- 8141-8143, 8151,8541-8543, 8551.TRANSFER-DISTR.USA-CAN/CAN-USA        
715900**- 874X, 8751 ANVÄNDER PRAVCOST OCH BERÖRS EJ ENLIGT SUSSI 2/2-05        
716000**--GLOBAL EXPORT 2018-11-01                                              
716100**- 914X, 927X FAKT 1 ANVÄNDER PRAVCOST OCH USD/CNY.                      
716200**- 914X, 927X FAKT 2 ANVÄNDER PRARTNTO OCH SEK.                          
716300**- OBS! OM INL-PRARTNTO = 0 SÅ ÄR WDK711-PRAVCOST = 0                    
716400**- FAKTURAN HÄMTAR PRIS FRÅN ORDERRADEN(FIX PRIS)WDL6 FRÅN K711          
716500**--------------------------------------------------------------          
716600                                                                          
716700        IF DIST35-US-CAN-TRANSFER OR DIST35-CAN-US-TRANSFER               
716800            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
716900            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
717000            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
717100        ELSE                                                              
717200          IF DIST79-DEALER-PRICE                                          
717300            MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                     
717400            MOVE ZERO            TO 6308-PRARTBTO                         
717500            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
717600          ELSE                                                            
717700            IF DIST35-NONVCC-CDC-REFILL                                   
717800            OR DIST35-NONVCC-VCC-REFILL                                   
717900            OR DIST35-NONVCC-VCC-TRANSFER                                 
718000              MOVE INL-PRARTNTO    TO 6308-PRARTBTO                       
718100              MOVE ZERO            TO 6308-PRARTBTO-LOC                   
718200              IF 6308-KDANMORS = '21'                                     
718300                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
718400                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
718500                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
718600              ELSE                                                        
718700                IF DIST35-NONVCC-CDC-REFILL                               
718800                OR DIST35-NONVCC-VCC-REFILL                               
718900                OR DIST35-NONVCC-VCC-TRANSFER                             
719000                  MOVE DCS-KDVALISO TO 6308-KDVALISO                      
719100                END-IF                                                    
719200              END-IF                                                      
719300            ELSE                                                          
719400              IF 6308-KDANMORS = '21'                                     
719500                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
719600                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
719700                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
719800              ELSE                                                        
719900                MOVE INL-PRARTNTO    TO 6308-PRARTBTO                     
720000                MOVE ZERO            TO 6308-PRARTBTO-LOC                 
720100                MOVE 'SEK'           TO 6308-KDVALISO                     
720200              END-IF                                                      
720300            END-IF                                                        
720400          END-IF                                                          
720500        END-IF                                                            
720600                                                                          
720700        MOVE W-IDARTNR         TO 6308-IDARTNR                            
720800        MOVE ZERO              TO 6308-IDRADNR                            
720900        MOVE W-DAGENS-DATUM    TO 6308-TILEVANM                           
721000        IF 6308-KDANMORS = '43'                                           
721100           MOVE 2              TO 6308-KDEMBLEV                           
721200        ELSE                                                              
721300           MOVE ZERO           TO 6308-KDEMBLEV                           
721400        END-IF                                                            
721500                                                                          
721600        PERFORM IMS-ISRT-WL630521                                         
721700        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
721800           ADD +1 TO 6308-IDRADNR                                         
721900           PERFORM IMS-ISRT-WL630521                                      
722000        END-PERFORM                                                       
722100     ELSE                                                                 
722200        MOVE 'N'            TO 6306-FLKLAR                                
722300                               6306-FLDIRLEV                              
722400        PERFORM IMS-ISRT-WL630511                                         
722500        MOVE 6302-IDDISTR   TO 6308-IDDISTR                               
722600                               TEST-IDDISTR                               
722700        MOVE 6302-IDKUNDNR  TO 6308-IDKUNDNR                              
722800        MOVE 6302-IDFAKT    TO 6308-IDRAPPNR                              
722900                                                                          
723000        IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                          
723100           MOVE ZERO              TO 6308-KDFRAKT                         
723200           MOVE W-SPAR-IDKUNDRF   TO 6308-IDKUNDRF                        
723300           MOVE W-SPAR-IDKOLLI    TO 6308-IDKOLLI                         
723400        ELSE                                                              
723500           MOVE INL-IDDISTR       TO 6308-IDDISTR                         
723600                                     TEST-IDDISTR                         
723700           MOVE INL-IDKUNDNR      TO 6308-IDKUNDNR                        
723800           MOVE INL-KDFRAKT       TO 6308-KDFRAKT                         
723900           MOVE INL-IDKUNDRF      TO 6308-IDKUNDRF                        
724000           MOVE INL-IDKOLLI       TO 6308-IDKOLLI                         
724100        END-IF                                                            
724200                                                                          
724300**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
724400**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA .REFILLDISTRIKT             
724500**- 8141-8143, 8151,8541-8543, 8551.TRANSFER-DISTR.USA-CAN/CAN-USA        
724600**- 874X, 8751 ANVÄNDER PRAVCOST OCH BERÖRS EJ ENLIGT SUSSI 2/2-05        
724700**--GLOBAL EXPORT 2018-11-01                                              
724800**- 914X, 927X FAKT 1 ANVÄNDER PRAVCOST OCH USD/CNY.                      
724900**- 914X, 927X FAKT 2 ANVÄNDER PRARTNTO OCH SEK.                          
725000                                                                          
725100        IF DIST35-US-CAN-TRANSFER OR DIST35-CAN-US-TRANSFER               
725200            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
725300            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
725400            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
725500        ELSE                                                              
725600          IF DIST79-DEALER-PRICE                                          
725700            MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                     
725800            MOVE ZERO            TO 6308-PRARTBTO                         
725900            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
726000          ELSE                                                            
726100            IF DIST35-NONVCC-CDC-REFILL                                   
726200            OR DIST35-NONVCC-VCC-REFILL                                   
726300            OR DIST35-NONVCC-VCC-TRANSFER                                 
726400              MOVE INL-PRARTNTO    TO 6308-PRARTBTO                       
726500              MOVE ZERO            TO 6308-PRARTBTO-LOC                   
726600              IF 6308-KDANMORS = '21'                                     
726700                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
726800                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
726900                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
727000              ELSE                                                        
727100                IF DIST35-NONVCC-CDC-REFILL                               
727200                OR DIST35-NONVCC-VCC-REFILL                               
727300                OR DIST35-NONVCC-VCC-TRANSFER                             
727400                  MOVE DCS-KDVALISO TO 6308-KDVALISO                      
727500                END-IF                                                    
727600              END-IF                                                      
727700            ELSE                                                          
727800              IF 6308-KDANMORS = '21'                                     
727900                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
728000                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
728100                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
728200              ELSE                                                        
728300                MOVE INL-PRARTNTO    TO 6308-PRARTBTO                     
728400                MOVE ZERO            TO 6308-PRARTBTO-LOC                 
728500                MOVE 'SEK'           TO 6308-KDVALISO                     
728600              END-IF                                                      
728700            END-IF                                                        
728800          END-IF                                                          
728900        END-IF                                                            
729000                                                                          
729100        MOVE W-IDARTNR      TO 6308-IDARTNR                               
729200        MOVE ZERO           TO 6308-IDRADNR                               
729300        MOVE W-DAGENS-DATUM TO 6308-TILEVANM                              
729400        IF 6308-KDANMORS = '43'                                           
729500           MOVE 2           TO 6308-KDEMBLEV                              
729600        ELSE                                                              
729700           MOVE ZERO        TO 6308-KDEMBLEV                              
729800        END-IF                                                            
729900                                                                          
730000        PERFORM IMS-ISRT-WL630521                                         
730100        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
730200           ADD +1 TO 6308-IDRADNR                                         
730300           PERFORM IMS-ISRT-WL630521                                      
730400        END-PERFORM                                                       
730500     END-IF                                                               
730600     .                                                                    
730700     EJECT                                                                
730800                                                                          
730900 S13-SKAPA-NDC-HIST-SANDANDE SECTION.                                     
731000**** NYTT IDLEVNR 970918                                                  
731100     MOVE DCS-IDLEVNR-DC    TO INL-IDLEVNR                                
731200****                                                                      
731300     MOVE W-IDARTNR         TO ART-IDARTNR                                
731400     PERFORM IMS-ISRT-WLINLC01                                            
731500     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
731600     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
731700                                                                          
731800     COMPUTE W-DAINLEV = 9999999999999999                                 
731900                       - W-TIAAAAMMDDTTMMSSTH                             
732000     END-COMPUTE                                                          
732100                                                                          
732200     MOVE W-DAINLEV         TO INL-DAINLEV                                
732300     MOVE ZERO              TO INL-ADLAGOMR                               
732400                               INL-ADGANG                                 
732500                               INL-ADPLATS                                
732600     MOVE SPACE             TO INL-FLMAKUL                                
732700                               INL-FLSKAKOL                               
732800     MOVE 'N'               TO INL-FLPRIO                                 
732900     MOVE 'N'               TO INL-FLTULLST                               
733000     MOVE W-IDDC-B6-SEND    TO INL-IDDC                                   
733100     MOVE SPACE             TO INL-IDDC-LEV                               
733200     MOVE ZERO              TO INL-IDLOPNRM                               
733300     MOVE W-SPAR-IDFAKT     TO INL-IDFAKT                                 
733400     MOVE 6302-IDDISTR      TO INL-IDDISTR                                
733500     MOVE 6302-IDKUNDNR     TO INL-IDKUNDNR                               
733600     MOVE W-SPAR-IDKUNDRF   TO INL-IDKUNDRF                               
733700     MOVE W-SPAR-IDKOLLI    TO INL-IDKOLLI                                
733800     MOVE 'R34'             TO INL-IDPTYP                                 
733900     MOVE SPACE             TO INL-IDUSER-003                             
734000     MOVE ZERO              TO INL-KDFRAKT                                
734100     MOVE SPACE             TO INL-KDKOLLI                                
734200                               INL-KDVALISO                               
734300                               INL-ADINLOMR                               
734400                               INL-IDANALYS                               
734500                               INL-IDKST                                  
734600     MOVE ZERO              TO INL-KVAVIS                                 
734700                               INL-KVART-SKROT                            
734800                               INL-KDRT                                   
734900                               INL-IDKONTO                                
735000     SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                                  
735100                      GIVING   INL-KVANTMOT                               
735200     MOVE ZERO              TO INL-PRARTNTO                               
735300                               INL-PRKURS                                 
735400                               INL-TIBERANK                               
735500                               INL-TIINLINL                               
735600                               INL-TIINLMOT                               
735700                               INL-TIINLMTI                               
735800                               INL-TIINLITI                               
735900                               INL-KVRETUR                                
736000                               INL-KVTULRET                               
736100                               INL-KDAVVANT                               
736200                               INL-TIAVIDAT                               
736300                                                                          
736400     PERFORM IMS-ISRT-WLINLC11                                            
736500     PERFORM UNTIL INSERT-OK                                              
736600       SUBTRACT 1      FROM W-DAINLEV                                     
736700       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
736800       PERFORM IMS-ISRT-WLINLC11                                          
736900     END-PERFORM                                                          
737000     .                                                                    
737100     EJECT                                                                
737200                                                                          
737300 S12-SEND-OPEN SECTION.                                                   
737400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
737500     MOVE WS-ADRESS-WL0102           TO SEND-ADDISPABS                    
737600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
737700                                                                          
737800     IF SEND-KDRC > 0                                                     
737900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
738000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
738100       DELIMITED BY SIZE INTO FELTEXT                                     
738200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
738300     END-IF                                                               
738400     .                                                                    
738500     EJECT                                                                
738600                                                                          
738700 S13-SEND-MESSAGE SECTION.                                                
738800     MOVE 'PUT'                      TO SEND-KDFUNC                       
738900     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
739000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
739100                                                                          
739200     IF SEND-KDRC > 0                                                     
739300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
739400       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
739500       DELIMITED BY SIZE INTO FELTEXT                                     
739600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
739700     END-IF                                                               
739800     .                                                                    
739900     EJECT                                                                
740000                                                                          
740100 S14-SEND-CLOSE SECTION.                                                  
740200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
740300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
740400                                                                          
740500     IF SEND-KDRC > 0                                                     
740600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
740700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
740800       DELIMITED BY SIZE INTO FELTEXT                                     
740900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
741000     END-IF                                                               
741100     .                                                                    
741200     EJECT                                                                
741300                                                                          
741400 S15-SKAPA-SDC-NDC-HIST-MOT-NA SECTION.                                   
741500     MOVE W-IDARTNR-INM TO ART-IDARTNR                                    
741600     PERFORM IMS-ISRT-WLINLC01                                            
741700                                                                          
741800     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
741900     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
742000                                                                          
742100     COMPUTE W-DAINLEV = 9999999999999999                                 
742200                       - W-TIAAAAMMDDTTMMSSTH                             
742300     END-COMPUTE                                                          
742400                                                                          
742500     MOVE W-DAINLEV           TO INL-DAINLEV                              
742600     MOVE W-ADLAGOMR-SPAR     TO INL-ADLAGOMR                             
742700     MOVE W-ADGANG-SPAR       TO INL-ADGANG                               
742800     MOVE W-ADPLATS-SPAR      TO INL-ADPLATS                              
742900     MOVE REQU-IDDC-KEY       TO INL-IDDC                                 
743000     MOVE SPACE               TO INL-IDDC-LEV                             
743100     MOVE W-SPAR-IDFAKT       TO INL-IDFAKT                               
743200                                                                          
743300     MOVE 6302-IDDISTR        TO INL-IDDISTR                              
743400     MOVE W-SPAR-IDKUNDNR     TO INL-IDKUNDNR                             
743500     MOVE W-SPAR-IDKUNDRF     TO INL-IDKUNDRF                             
743600     MOVE W-SPAR-IDKOLLI      TO INL-IDKOLLI                              
743700     MOVE 'R32'               TO INL-IDPTYP                               
743800     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
743900        MOVE SPACE            TO INL-IDUSER-003                           
744000     ELSE                                                                 
744100        MOVE WS-IDUSER-003    TO INL-IDUSER-003                           
744200     END-IF                                                               
744300     MOVE WS-KVANTMOT-INM-NUM TO INL-KVANTMOT                             
744400     MOVE WS-KVSKROT-INM-NUM  TO INL-KVART-SKROT                          
744500     MOVE ZERO                TO INL-KVAVIS                               
744600     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
744700     MOVE ZERO                TO INL-IDLOPNRM                             
744800                                 INL-KDFRAKT                              
744900                                 INL-PRKURS                               
745000                                 INL-TIBERANK                             
745100                                 INL-KDRT                                 
745200                                 INL-IDKONTO                              
745300                                 INL-KVRETUR                              
745400                                 INL-KVTULRET                             
745500                                 INL-KDAVVANT                             
745600                                 INL-TIAVIDAT                             
745700     MOVE SPACE               TO INL-KDVALISO                             
745800                                 INL-KDKOLLI                              
745900                                 INL-FLMAKUL                              
746000                                 INL-FLSKAKOL                             
746100                                 INL-IDKST                                
746200                                 INL-ADINLOMR                             
746300                                 INL-IDANALYS                             
746400     MOVE 'N'                 TO INL-FLPRIO                               
746500     MOVE 'N'                 TO INL-FLTULLST                             
746600     MOVE W-TIME-N            TO AKTUELL-TID                              
746700     MOVE W-DAGENS-DATUM      TO INL-TIINLINL                             
746800                                 INL-TIINLMOT                             
746900     MOVE AKTUELL-TTMM        TO INL-TIINLITI                             
747000                                 INL-TIINLMTI                             
747100                                                                          
747200     IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                             
747300        MOVE WS-KDVALISO TO INL-KDVALISO                                  
747400     END-IF                                                               
747500                                                                          
747600     PERFORM IMS-ISRT-WLINLC11                                            
747700     PERFORM UNTIL INSERT-OK                                              
747800       SUBTRACT 1      FROM W-DAINLEV                                     
747900       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
748000       PERFORM IMS-ISRT-WLINLC11                                          
748100     END-PERFORM                                                          
748200                                                                          
748300** WRITE WDL623 IDTRACK WHEN FLTRACK='J' FOR NEW RECORD                   
748400     IF REQU-KVANTMOT-INM NOT = ALL '+'  OR SPACE                         
748500        IF WS-FLTRACK = 'J'                                               
748600           MOVE W-IDTRACK TO TINL-IDTRACK                                 
748700           PERFORM IMS-ISRT-WDL623                                        
748800        END-IF                                                            
748900     END-IF                                                               
749000     .                                                                    
749100     EJECT                                                                
749200                                                                          
749300 S15-SKAPA-SDC-NDC-HIST-MOT    SECTION.                                   
749400     MOVE W-IDARTNR-INM TO ART-IDARTNR                                    
749500                           W-IDARTNR                                      
749600     PERFORM IMS-ISRT-WLINLC01                                            
749700                                                                          
749800     MOVE LOW-VALUE           TO W-WDL6A1KY-MIN2                          
749900     MOVE HIGH-VALUE          TO W-WDL6A1KY-MAX2                          
750000     MOVE W-SPAR-IDFAKT       TO W-IDFAKT-MIN                             
750100                                 W-IDFAKT-MAX                             
750200     PERFORM IMS-GU-WDL6A1                                                
750300     IF SEGMENT-SAKNAS                                                    
750400       MOVE FUNCTION CURRENT-DATE (1:8)                                   
750500       TO W-TIAAAAMMDDTTMMSSTH-DATE                                       
750600       ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                         
750700                                                                          
750800       COMPUTE W-DAINLEV = 9999999999999999                               
750900                       - W-TIAAAAMMDDTTMMSSTH                             
751000       END-COMPUTE                                                        
751100     ELSE                                                                 
751200       MOVE WDL6A1-SEQA-DAINLEV TO W-DAINLEV                              
751300     END-IF                                                               
751400     MOVE W-DAINLEV           TO INL-DAINLEV                              
751500     MOVE W-ADLAGOMR-SPAR     TO INL-ADLAGOMR                             
751600     MOVE W-ADGANG-SPAR       TO INL-ADGANG                               
751700     MOVE W-ADPLATS-SPAR      TO INL-ADPLATS                              
751800     MOVE REQU-IDDC-KEY       TO INL-IDDC                                 
751900     MOVE SPACE               TO INL-IDDC-LEV                             
752000     MOVE W-SPAR-IDFAKT       TO INL-IDFAKT                               
752100                                                                          
752200     MOVE 6302-IDDISTR        TO INL-IDDISTR                              
752300     MOVE W-SPAR-IDKUNDNR     TO INL-IDKUNDNR                             
752400     MOVE W-SPAR-IDKUNDRF     TO INL-IDKUNDRF                             
752500     MOVE INL-IDORDNR5        TO W-IDORDER                                
752600     MOVE W-SPAR-IDKOLLI      TO INL-IDKOLLI                              
752700                                 W-IDKOLLI                                
752800     MOVE 'R32'               TO INL-IDPTYP                               
752900     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
753000        MOVE SPACE            TO INL-IDUSER-003                           
753100     ELSE                                                                 
753200        MOVE WS-IDUSER-003    TO INL-IDUSER-003                           
753300     END-IF                                                               
753400     MOVE WS-KVANTMOT-INM-NUM TO INL-KVANTMOT                             
753500     MOVE WS-KVSKROT-INM-NUM  TO INL-KVART-SKROT                          
753600     MOVE ZERO                TO INL-KVAVIS                               
753700     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
753800     MOVE ZERO                TO INL-IDLOPNRM                             
753900                                 INL-KDFRAKT                              
754000                                 INL-PRKURS                               
754100                                 INL-TIBERANK                             
754200                                 INL-KDRT                                 
754300                                 INL-IDKONTO                              
754400                                 INL-KVRETUR                              
754500                                 INL-KVTULRET                             
754600                                 INL-KDAVVANT                             
754700                                 INL-TIAVIDAT                             
754800     MOVE SPACE               TO INL-KDVALISO                             
754900                                 INL-KDKOLLI                              
755000                                 INL-IDKST                                
755100                                 INL-FLMAKUL                              
755200                                 INL-FLSKAKOL                             
755300                                 INL-ADINLOMR                             
755400                                 INL-IDANALYS                             
755500     MOVE 'N'                 TO INL-FLPRIO                               
755600     MOVE 'N'                 TO INL-FLTULLST                             
755700     MOVE W-TIME-N            TO AKTUELL-TID                              
755800     MOVE W-DAGENS-DATUM      TO INL-TIINLINL                             
755900                                 INL-TIINLMOT                             
756000     MOVE AKTUELL-TTMM        TO INL-TIINLITI                             
756100                                 INL-TIINLMTI                             
756200                                                                          
756300     IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                             
756400        MOVE WS-KDVALISO TO INL-KDVALISO                                  
756500     END-IF                                                               
756600                                                                          
756700     PERFORM IMS-ISRT-WLINLC11                                            
756800     PERFORM UNTIL INSERT-OK                                              
756900       SUBTRACT 1      FROM W-DAINLEV                                     
757000       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
757100       PERFORM IMS-ISRT-WLINLC11                                          
757200     END-PERFORM                                                          
757300                                                                          
757400** WRITE WDL623 IDTRACK WHEN FLTRACK='J' FOR NEW RECORD                   
757500     IF REQU-KVANTMOT-INM NOT = ALL '+'  OR SPACE                         
757600        IF WS-FLTRACK = 'J'                                               
757700           MOVE W-IDTRACK TO TINL-IDTRACK                                 
757800           PERFORM IMS-ISRT-WDL623                                        
757900        END-IF                                                            
758000     END-IF                                                               
758100     .                                                                    
758200     EJECT                                                                
758300                                                                          
758400 S16-PRIS-TILLAMPNING SECTION.                                            
758500                                                                          
758600*--- KOD 21 FÖR CN-TO-US, US-TO-CN, CDC-TO-US, CDC-TO-CN                  
758700*--- ÄVEN FÖR FÖR DISTR 9111 OCH 9211 EXP-TO-CDC                          
758800     MOVE ZERO   TO WS-6308-TF-PRARTNTO                                   
758900     MOVE SPACE  TO WS-6308-TF-KDVALISO                                   
759000                                                                          
759100     MOVE NEJ TO WS-FLYGORDER                                             
759200                 WS-BAATORDER                                             
759300                                                                          
759400     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
759500        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
759600        PERFORM IMS-GU-WDB601-SEND                                        
759700     END-IF                                                               
759800                                                                          
759900     IF SEND-DCS-DDC                                                      
760000        MOVE WC-CDC-SE   TO W-IDDC-WDB3                                   
760100                            W-IDDC-WDB3-DEF                               
760200     ELSE                                                                 
760300        MOVE 6302-IDDC-SEND TO W-IDDC-WDB3                                
760400                               W-IDDC-WDB3-DEF                            
760500     END-IF                                                               
760600     MOVE 6302-IDDISTR   TO W-IDDISTR-WDB3                                
760700                            W-IDDISTR-WDB3-DEF                            
760800     MOVE 6302-IDKUNDNR  TO W-IDKUNDNR-WDB3                               
760900                                                                          
761000     PERFORM IMS-GU-WDB301                                                
761100     IF SEGMENT-FINNS                                                     
761200        IF W-KDFRAKT = DC-KDGENFRA-VOR                                    
761300           MOVE JA TO WS-FLYGORDER                                        
761400        ELSE                                                              
761500           MOVE NEJ TO WS-FLYGORDER                                       
761600        END-IF                                                            
761700     END-IF                                                               
761800                                                                          
761900     MOVE 1                   TO PRIS-KDCALL                              
762000     MOVE 'WL010300'          TO PRIS-IDPGM                               
762100     MOVE W-IDARTNR           TO PRIS-IDARTNR                             
762200     MOVE 6302-IDDISTR        TO PRIS-IDDISTR                             
762300     MOVE 6302-IDKUNDNR       TO PRIS-IDKUNDNR                            
762400     IF WS-FLYGORDER = JA                                                 
762500        MOVE +1               TO PRIS-KDORDKL                             
762600     ELSE                                                                 
762700        MOVE +4               TO PRIS-KDORDKL                             
762800     END-IF                                                               
762900     MOVE +1                  TO PRIS-KVBEART                             
763000     MOVE SEND-WS-IDDC        TO PRIS-IDDC                                
763100     MOVE NEJ                 TO PRIS-FLINVEST                            
763200                                                                          
763300     CALL W335PRIS USING PRIS-W335PRIS ARTC-PCB                           
763400                                   PRIS-WDK7-PCB GMTA-PCB                 
763500                                   BETA-PCB GPRIA-PCB GPRIB-PCB           
763600                                   PRIS-COST-WDK6-PCB                     
763700                                   PRIS-COST-WDK7-PCB                     
763800                                   PRIS-COST-WDF1-PCB                     
763900                                   PRIS-COST-9305-PCB                     
764000                                   PRIS-COST-WDK72-PCB                    
764100                                   PRIS-COST-WDB6-PCB                     
764200                                                                          
764300     IF PRIS-KDSVAR = SPACE                                               
764400        MOVE PRIS-PRARTNTO    TO WS-FIXAD-PRARTNTO                        
764500                                                                          
764600        IF PRIS-PRAVCOST > ZERO                                           
764700          MOVE PRIS-PRAVCOST  TO WS-6308-TF-PRARTNTO                      
764800        ELSE                                                              
764900          MOVE PRIS-PRARTNTO  TO WS-6308-TF-PRARTNTO                      
765000        END-IF                                                            
765100        MOVE PRIS-KDVALISO    TO WS-6308-TF-KDVALISO                      
765200                                                                          
765300     ELSE                                                                 
765400        MOVE 'FEL RETURKOD FRÅN W335PRIS' TO FELTEXT                      
765500        CALL FELLOG                                                       
765600     END-IF                                                               
765700     .                                                                    
765800     EJECT                                                                
765900                                                                          
766000 S16-HAEMTA-ANROPSDATA SECTION.                                           
766100     MOVE 'GETARG'               TO SUB-KDFUNC                            
766200     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
766300*    MOVE MAX-INDX(1100) TO REQU-KVRADER-MAX1 SO THAT THE                 
766400*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
766500*    POSSIBLE INPUT                                                       
766600     MOVE MAX-INDX               TO REQU-KVRADER-MAX1                     
766700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
766800                                                                          
766900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
767000                                                                          
767100     IF SUB-KDRC > 0                                                      
767200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
767300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
767400       DELIMITED BY SIZE INTO FELTEXT                                     
767500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
767600     END-IF                                                               
767700     .                                                                    
767800     EJECT                                                                
767900                                                                          
768000 S17-RETURNERA-SVAR SECTION.                                              
768100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
768200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
768300                                                                          
768400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
768500                                                                          
768600     IF SUB-KDRC > 0                                                      
768700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
768800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
768900       DELIMITED BY SIZE INTO FELTEXT                                     
769000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
769100     END-IF                                                               
769200     .                                                                    
769300     EJECT                                                                
769400                                                                          
769500 S17-NDC-OOVER-UNDER SECTION.                                             
769600     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
769700        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
769800        PERFORM IMS-GU-WDB601-SEND                                        
769900     END-IF                                                               
770000                                                                          
770100     IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                                
770200        AND (DCS-NDC-NA AND DCS-USA)                                      
770300        MOVE SEND-WS-IDDC TO W-IDDC                                       
770400        PERFORM IMS-GET-WDK711                                            
770500        IF REQU-CMD-IN(INDX) = 'DAM'                                      
770600           COMPUTE SLAG-KVLS = SLAG-KVLS                                  
770700                               + (W-KVAVIS                                
770800                               - W-KVANTMOT(INDX)                         
770900                               - W-KVSKROT (INDX))                        
771000           END-COMPUTE                                                    
771100           COMPUTE LOGG-KVART-SALDO =                                     
771200                                 (W-KVAVIS                                
771300                               - W-KVANTMOT(INDX)                         
771400                               - W-KVSKROT (INDX))                        
771500           END-COMPUTE                                                    
771600        ELSE                                                              
771700           COMPUTE SLAG-KVLS = SLAG-KVLS                                  
771800                               + (W-KVAVIS                                
771900                               - W-KVANTMOT(INDX))                        
772000           END-COMPUTE                                                    
772100           COMPUTE LOGG-KVART-SALDO =                                     
772200                                 (W-KVAVIS                                
772300                               - W-KVANTMOT(INDX))                        
772400           END-COMPUTE                                                    
772500        END-IF                                                            
772600        PERFORM S21-SALDOLOGG-DATA                                        
772700        MOVE SLAG-IDDC            TO LOGG-IDDC                            
772800        MOVE 'MISC'               TO LOGG-IDHUVTYP                        
772900        MOVE 'R34'                TO LOGG-IDSUBTYP                        
773000        MOVE SLAG-KVLS            TO LOGG-KVLS                            
773100        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
773200        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
773300        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
773400        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
773500        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
773600        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                  
773700        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
773800        PERFORM S22-ISRT-SALDOLOGG                                        
773900        PERFORM IMS-REPL-WDK711                                           
774000                                                                          
774100        MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                           
774200        PERFORM S13-SKAPA-NDC-HIST-SANDANDE                               
774300     ELSE                                                                 
774400        IF W-KVAVIS < W-TEMP-KVANT                                        
774500           MOVE '11' TO 6308-KDANMORS                                     
774600        ELSE                                                              
774700           MOVE '00' TO 6308-KDANMORS                                     
774800        END-IF                                                            
774900        MOVE W-DIFF-KVANT TO 6308-KVLEVANM                                
775000        PERFORM S11-SKAPA-LEVANM-TRANS                                    
775100     END-IF                                                               
775200     .                                                                    
775300     EJECT                                                                
775400                                                                          
775500 S19-EV-RO-TACKNING SECTION.                                              
775600     IF SLAG-KVROS-BULK > ZERO                                            
775700     OR SLAG-KVROS-DAG > ZERO                                             
775800        IF SLAG-KDLEVSP = ZERO                                            
775900           MOVE W-IDARTNR      TO 4506-IDARTNR                            
776000           MOVE +1             TO 4506-KDTAKORS                           
776100           MOVE WS-RO-KVANTMOT TO 4506-KVANTMOT                           
776200           MOVE WS-IDDC        TO 4505-IDDC                               
776300           PERFORM IMS-ISRT-4506                                          
776400           MOVE SPACE          TO 4506-WDGX4506                           
776500        END-IF                                                            
776600     END-IF                                                               
776700     .                                                                    
776800     EJECT                                                                
776900                                                                          
777000 S19-EV-RO-TACKNING-CDC SECTION.                                          
777100     IF CLAG-KVROS > ZERO                                                 
777200        IF CLAG-KDLEVSP = ZERO                                            
777300           MOVE W-IDARTNR      TO 4506-IDARTNR                            
777400           MOVE +1             TO 4506-KDTAKORS                           
777500           MOVE WS-RO-KVANTMOT TO 4506-KVANTMOT                           
777600           MOVE WS-IDDC        TO 4505-IDDC                               
777700           PERFORM IMS-ISRT-4506                                          
777800           MOVE SPACE          TO 4506-WDGX4506                           
777900        END-IF                                                            
778000     END-IF                                                               
778100     .                                                                    
778200                                                                          
778300 S20-SKAPA-REFILLTRANS SECTION.                                           
778400     MOVE W-IDARTNR           TO FILC-IDARTNR                             
778500     MOVE WS-KVANTMOT-INM-NUM TO FILC-KVLEVANM                            
778600     MOVE WS-KVSKROT-INM-NUM  TO FILC-KVSKROT                             
778700     MOVE SEND-WS-IDDC        TO FILC-IDDC-SEND                           
778800     MOVE REQU-IDDC-KEY       TO FILC-IDDC-REC                            
778900     MOVE W-DAGENS-DATUM      TO FILC-TILEVANM                            
779000     MOVE FILC-W61236         TO FILC-FIL-WDR301-DATA                     
779100     ACCEPT W-TID FROM TIME                                               
779200     IF W-TID = FILC-FIL-TIKLOCK                                          
779300        ADD +1                TO FILC-FIL-IDSEKVNR                        
779400     ELSE                                                                 
779500        MOVE W-TID            TO FILC-FIL-TIKLOCK                         
779600        MOVE +1               TO FILC-FIL-IDSEKVNR                        
779700     END-IF                                                               
779800     PERFORM IMS-ISRT-WLFILC                                              
779900     .                                                                    
780000     EJECT                                                                
780100                                                                          
780200 S21-SALDOLOGG-DATA SECTION.                                              
780300     ACCEPT WS-TID                   FROM TIME                            
780400     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
780500     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
780600     MOVE W-IDARTNR          TO LOGG-IDARTNR                              
780700     MOVE 9                  TO LOGG-IDSEKVNR                             
780800     MOVE 'INBO'             TO LOGG-IDHUVTYP                             
780900     MOVE 'R32'              TO LOGG-IDSUBTYP                             
781000     MOVE 'WL010300'         TO LOGG-IDPGM                                
781100     MOVE 'L103'             TO LOGG-IDTRANS                              
781200     MOVE MSG-SIGNON-USERID  TO LOGG-IDUSER                               
781300     MOVE SPACE              TO LOGG-REF                                  
781400     MOVE W-SPAR-IDFAKT      TO LOGG-IDFAKT                               
781500     MOVE W-SPAR-IDKUNDRF    TO LOGG-IDKUNDRF                             
781600     MOVE W-SPAR-IDKUNDNR    TO LOGG-IDKUNDNR                             
781700     MOVE '00000000'         TO LOGG-DAREGDAT-LADD                        
781800     .                                                                    
781900     EJECT                                                                
782000                                                                          
782100 S22-ISRT-SALDOLOGG SECTION.                                              
782200     PERFORM IMS-ISRT-WDL901                                              
782300     IF SEGMENT-FINNS-REDAN                                               
782400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
782500         SUBTRACT 1 FROM LOGG-IDSEKVNR                                    
782600         PERFORM IMS-ISRT-WDL901                                          
782700       END-PERFORM                                                        
782800     END-IF                                                               
782900     .                                                                    
783000     EJECT                                                                
783100                                                                          
783200 S23-TRACKLOG-DATA SECTION.                                               
783300     INITIALIZE LOGT-WDL301                                               
783400     ACCEPT WS-TID                   FROM TIME                            
783500     COMPUTE LOGT-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
783600     COMPUTE LOGT-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
783700     MOVE W-IDARTNR          TO LOGT-IDARTNR                              
783800     MOVE 9                  TO LOGT-IDSEKVNR                             
783900     MOVE 'INBO'             TO LOGT-IDHUVTYP                             
784000     MOVE 'R32'              TO LOGT-IDSUBTYP                             
784100     MOVE 'WL010300'         TO LOGT-IDPGM                                
784200     MOVE 'L103'             TO LOGT-IDTRANS                              
784300     MOVE MSG-SIGNON-USERID  TO LOGT-IDUSER                               
784400**   MOVE SPACE              TO LOGT-REF                                  
784500     MOVE W-SPAR-IDFAKT      TO LOGT-IDFAKT                               
784600     MOVE W-SPAR-IDKUNDRF    TO LOGT-IDKUNDRF                             
784700     MOVE W-SPAR-IDKUNDNR    TO LOGT-IDKUNDNR                             
784800     MOVE '00000000'         TO LOGT-DAREGDAT-LADD                        
784900     MOVE REQU-IDDC-KEY      TO LOGT-IDDC                                 
785000     MOVE WS-TRCK-KVANTMOT   TO LOGT-KVART-SALDO                          
785100     MOVE SLAG-KVLS          TO LOGT-KVLS                                 
785200     MOVE '+'                TO LOGT-IDTECKEN-KVLS                        
785300     MOVE '+'                TO LOGT-IDTECKEN-KVTRACK-KVAR                
785400     MOVE WS-WDL3-TRCK-KVTRACK-KVAR                                       
785500                             TO LOGT-KVTRACK-KVAR                         
785600     MOVE W-IDTRACK          TO LOGT-IDTRACK                              
785700     .                                                                    
785800     EJECT                                                                
785900                                                                          
786000 S24-ISRT-TRACKLOG SECTION.                                               
786100     PERFORM IMS-ISRT-WDL301                                              
786200     IF SEGMENT-FINNS-REDAN                                               
786300       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
786400         SUBTRACT 1 FROM LOGT-IDSEKVNR                                    
786500         PERFORM IMS-ISRT-WDL301                                          
786600       END-PERFORM                                                        
786700     END-IF                                                               
786800     .                                                                    
786900     EJECT                                                                
787000                                                                          
787100 S30-UPPDATERA-WDJ9 SECTION.                                              
787200     PERFORM IMS-GU-LOCB01                                                
787300     IF SEGMENT-SAKNAS                                                    
787400        MOVE W-IDARTNR TO LOCB-ART-IDARTNR                                
787500        PERFORM IMS-ISRT-LOCB01                                           
787600        PERFORM IMS-GU-LOCB01                                             
787700     END-IF                                                               
787800     IF SEGMENT-FINNS                                                     
787900        PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'P'             
788000           PERFORM IMS-GHNP-LOCB11                                        
788100             IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'P'                   
788200                MOVE FUNCTION CURRENT-DATE(1:8)                           
788300                                      TO LOCB-HIST-DASTODAT               
788400                MOVE MSG-SIGNON-USERID                                    
788500                                      TO LOCB-HIST-IDUSER-STO             
788600                PERFORM IMS-REPL-LOCB11                                   
788700             END-IF                                                       
788800        END-PERFORM                                                       
788900          MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-LOGG-DATUM               
789000          MOVE FUNCTION CURRENT-DATE(9:6)  TO WS-LOGG-TID                 
789100          COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                  
789200                                                 WS-LOGG-DATUM            
789300          COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - WS-LOGG-TID        
789400          MOVE REQU-IDDC-KEY               TO LOCB-HIST-IDDC              
789500          MOVE WS-PRIME-LOCATION           TO LOCB-HIST-KDLOC             
789600          MOVE W-ADLAGOMR-LOCB             TO LOCB-HIST-ADLAGOMR          
789700          MOVE W-ADGANG-LOCB               TO LOCB-HIST-ADGANG            
789800          MOVE W-ADPLATS-LOCB              TO LOCB-HIST-ADPLATS           
789900          MOVE MSG-SIGNON-USERID          TO LOCB-HIST-IDUSER             
790000          MOVE SPACE                       TO LOCB-HIST-IDUSER-STO        
790100          MOVE ZERO                        TO LOCB-HIST-DASTODAT          
790200                                                                          
790300          PERFORM IMS-ISRT-LOCB11                                         
790400     END-IF                                                               
790500     .                                                                    
790600     EJECT                                                                
790700                                                                          
790800 S31-SKAPA-SAP-TRANS-VCCS SECTION.                                        
790900******************************************************************        
791000* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
791100* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
791200*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
791300*         LIGGER KVAR TILLS VIDARE (WDR8).                                
791400******************************************************************        
791500     IF  W-KVAVIS         < W-TEMP-KVANT                                  
791600*****    ÖVERLEVERANS                                                     
791700         IF (DIST35-NONVCC-VCC-REFILL) OR                                 
791800            (DIST35-NONVCC-VCC-TRANSFER) OR                               
791900            (DCS-CDC AND                                                  
792000            (SEND-DCS-NDC-CN OR SEND-DCS-USA))                            
792100           MOVE '102'                 TO EKH-KDEKHHT                      
792200           MOVE '122'                 TO EKH-KDEKSHT                      
792300         ELSE                                                             
792400           MOVE '503'                 TO EKH-KDEKHHT                      
792500           MOVE '502'                 TO EKH-KDEKSHT                      
792600         END-IF                                                           
792700         SUBTRACT W-KVAVIS FROM W-TEMP-KVANT                              
792800                           GIVING EKH-KVANTAL                             
792900     ELSE                                                                 
793000*****    UNDERLEVERANS                                                    
793100         IF (DIST35-NONVCC-VCC-REFILL) OR                                 
793200            (DIST35-NONVCC-VCC-TRANSFER) OR                               
793300            (DCS-CDC AND                                                  
793400            (SEND-DCS-NDC-CN OR SEND-DCS-USA))                            
793500           MOVE '102'                 TO EKH-KDEKHHT                      
793600           MOVE '122'                 TO EKH-KDEKSHT                      
793700         ELSE                                                             
793800           MOVE '503'                 TO EKH-KDEKHHT                      
793900           MOVE '503'                 TO EKH-KDEKSHT                      
794000         END-IF                                                           
794100         SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                              
794200                           GIVING EKH-KVANTAL                             
794300         COMPUTE EKH-KVANTAL = EKH-KVANTAL * -1                           
794400     END-IF                                                               
794500     MOVE 'WL010300'                  TO FIL-IDPGM IN FIL-WDR901          
794600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
794700     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
794800                                         EKH-DAVERDAT                     
794900     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
795000     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
795100     ADD +1                           TO W-IDSEKVNR-SAP                   
795200     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR IN FIL-WDR901         
795300     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
795400     MOVE W-IDDC-B6-SEND              TO EKH-IDDC-SEND                    
795500     MOVE REQU-IDDC-KEY               TO EKH-IDDC-REC                     
795600     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
795700     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
795800     MOVE ZERO TO NOLL-RAKNARE                                            
795900     MOVE W-SPAR-IDFAKT               TO WS-SAP-IDFAKT                    
796000     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
796100     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
796200          FOR LEADING ZERO                                                
796300     ADD +1 TO NOLL-RAKNARE                                               
796400     UNSTRING WS-SAP-X-IDFAKT      INTO EKH-IDVERGL                       
796500          WITH POINTER NOLL-RAKNARE                                       
796600     MOVE WS-SAP-PRARTSTD             TO EKH-PRARTSTD                     
796700     MOVE W-KDPRODSL                  TO EKH-KDPRODSL                     
796800     MOVE ZERO                        TO EKH-KDPSLLOC                     
796900                                         EKH-PRARTNTO                     
797000                                         EKH-PRHEMTAG                     
797100                                         EKH-PRARTSJK                     
797200                                         EKH-PRINK                        
797300                                         EKH-PRDIRLON                     
797400                                         EKH-PRDMTRL                      
797500                                         EKH-PROVRPAL                     
797600                                         EKH-SUBEL                        
797700     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
797800     MOVE SPACE                       TO EKH-FLLSBOK                      
797900     MOVE 'SEK'                       TO EKH-KDVALISO                     
798000     MOVE 1.00                        TO EKH-PRKURS                       
798100     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT IN                  
798200                                                    FIL-WDR901            
798300     MOVE 'L103'                      TO EKH-IDTRANS                      
798400     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER IN FIL-WDR901         
798500     MOVE ZERO                        TO EKH-BEVAT                        
798600                                         EKH-IDANALYS                     
798700                                         EKH-IDKONTO                      
798800                                         EKH-KDANMORS                     
798900                                         EKH-SUVAT                        
799000                                         EKH-KDFRAKT                      
799100                                         EKH-PRLANDCO                     
799200                                         EKH-DAAVIDAT                     
799300                                         EKH-IDAVINR                      
799400                                         EKH-KDAVVTYP                     
799500                                         EKH-KDRT                         
799600                                         EKH-KVANTMOT                     
799700                                         EKH-KVAVIS                       
799800     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
799900     MOVE 'SEPV'                     TO  EKH-KDTRADP                      
800000     MOVE SPACE                      TO  EKH-FLDCET                       
800100     MOVE SPACE                      TO  EKH-IDLEVNR                      
800200                                         EKH-IDKST                        
800300     MOVE SPACE                      TO  EKH-IDKUNDRF                     
800400     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
800500                                                                          
800600     PERFORM IMS-ISRT-WLSAPA01                                            
800700                                                                          
800800     PERFORM UNTIL SEGMENT-FINNS                                          
800900         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
801000         PERFORM IMS-ISRT-WLSAPA01                                        
801100     END-PERFORM                                                          
801200     .                                                                    
801300     EJECT                                                                
801400                                                                          
801500 S31-SKAPA-SAP-TRANS SECTION.                                             
801600******************************************************************        
801700* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
801800* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
801900*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
802000*         LIGGER KVAR TILLS VIDARE (WDR8).                                
802100******************************************************************        
802200     IF  W-KVAVIS         < W-TEMP-KVANT                                  
802300*****    ÖVERLEVERANS                                                     
802400         IF SEND-DCS-CDC OR SEND-DCS-DDC                                  
802500         OR DIST35-VCC-NONVCC-REFILL                                      
802600         OR DIST35-VCC-NONVCC-TRANSFER                                    
802700           IF DIST35-NONVCC-NONVCC-REFILL OR                              
802800              DIST35-NONVCC-NONVCC-TRANSFER                               
802900             MOVE '102'               TO R8-EKH-KDEKHHT                   
803000             MOVE '132'               TO R8-EKH-KDEKSHT                   
803100           ELSE                                                           
803200             MOVE '102'               TO R8-EKH-KDEKHHT                   
803300             MOVE '122'               TO R8-EKH-KDEKSHT                   
803400           END-IF                                                         
803500         ELSE                                                             
803600           MOVE '503'                 TO R8-EKH-KDEKHHT                   
803700           MOVE '502'                 TO R8-EKH-KDEKSHT                   
803800         END-IF                                                           
803900         SUBTRACT W-KVAVIS FROM W-TEMP-KVANT                              
804000                           GIVING R8-EKH-KVANTAL                          
804100     ELSE                                                                 
804200*****    UNDERLEVERANS                                                    
804300         IF SEND-DCS-CDC OR SEND-DCS-DDC                                  
804400         OR DIST35-VCC-NONVCC-REFILL                                      
804500         OR DIST35-VCC-NONVCC-TRANSFER                                    
804600           IF DIST35-NONVCC-NONVCC-REFILL OR                              
804700              DIST35-NONVCC-NONVCC-TRANSFER                               
804800             MOVE '102'               TO R8-EKH-KDEKHHT                   
804900             MOVE '132'               TO R8-EKH-KDEKSHT                   
805000           ELSE                                                           
805100             MOVE '102'               TO R8-EKH-KDEKHHT                   
805200             MOVE '122'               TO R8-EKH-KDEKSHT                   
805300           END-IF                                                         
805400           SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                            
805500                             GIVING R8-EKH-KVANTAL                        
805600           COMPUTE R8-EKH-KVANTAL = R8-EKH-KVANTAL * -1                   
805700         ELSE                                                             
805800           MOVE '503'                 TO R8-EKH-KDEKHHT                   
805900           MOVE '503'                 TO R8-EKH-KDEKSHT                   
806000           SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                            
806100                             GIVING R8-EKH-KVANTAL                        
806200           COMPUTE R8-EKH-KVANTAL = R8-EKH-KVANTAL * -1                   
806300         END-IF                                                           
806400     END-IF                                                               
806500     MOVE 'WL010300'                  TO FIL-IDPGM IN FIL-WDR801          
806600     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
806700     ACCEPT FIL-TIKLOCK IN FIL-WDR801 FROM TIME                           
806800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
806900     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
807000     ADD +1                           TO W-IDSEKVNR-SAP                   
807100     MOVE W-IDSEKVNR-SAP          TO FIL-IDSEKVNR IN FIL-WDR801           
807200     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
807300     MOVE W-IDDC-B6-SEND              TO R8-EKH-IDDC-SEND                 
807400     MOVE REQU-IDDC-KEY               TO R8-EKH-IDDC-REC                  
807500     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
807600     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
807700     MOVE ZERO TO NOLL-RAKNARE                                            
807800     MOVE W-SPAR-IDFAKT               TO WS-SAP-IDFAKT                    
807900     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
808000     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
808100          FOR LEADING ZERO                                                
808200     ADD +1 TO NOLL-RAKNARE                                               
808300     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
808400          WITH POINTER NOLL-RAKNARE                                       
808500     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
808600     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
808700     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
808800                                                                          
808900     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
809000                                         R8-EKH-PRARTSJK                  
809100                                         R8-EKH-PRINK                     
809200                                         R8-EKH-PRDIRLON                  
809300                                         R8-EKH-PRDMTRL                   
809400                                         R8-EKH-PROVRPAL                  
809500                                         R8-EKH-SUBEL                     
809600     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
809700     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
809800     MOVE 1.00                        TO R8-EKH-PRKURS                    
809900     MOVE 'L103'                      TO R8-EKH-IDTRANS                   
810000     MOVE ZERO                        TO R8-EKH-BEVAT                     
810100                                         R8-EKH-IDANALYS                  
810200                                         R8-EKH-IDKONTO                   
810300                                         R8-EKH-KDANMORS                  
810400                                         R8-EKH-SUVAT                     
810500                                         R8-EKH-KDFRAKT                   
810600                                         R8-EKH-PRLANDCO                  
810700                                         R8-EKH-DAAVIDAT                  
810800                                         R8-EKH-IDAVINR                   
810900                                         R8-EKH-KDAVVTYP                  
811000                                         R8-EKH-KDRT                      
811100                                         R8-EKH-KVANTMOT                  
811200                                         R8-EKH-KVAVIS                    
811300     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
811400     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
811500     MOVE SPACE                      TO  R8-EKH-FLDCET                    
811600                                         R8-EKH-IDKST                     
811700     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
811800     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
811900                                                                          
812000     MOVE DCS-KDVALISO               TO  R8-EKH-KDVALISO                  
812100     MOVE DCS-KDTRADP                TO  R8-EKH-KDTRADP                   
812200     IF NDC-CN                                                            
812300       MOVE 'W570'                   TO  FIL-IDCPYTXT                     
812400                                     IN  FIL-WDR801(1:4)                  
812500     ELSE                                                                 
812600       IF NDC-IN                                                          
812700         MOVE 'W515'                 TO  FIL-IDCPYTXT                     
812800                                     IN  FIL-WDR801(1:4)                  
812900       ELSE                                                               
813000         MOVE DCS-KDTRADP            TO  FIL-IDCPYTXT                     
813100                                     IN  FIL-WDR801(1:4)                  
813200       END-IF                                                             
813300     END-IF                                                               
813400     MOVE 'EKHA'                     TO  FIL-IDCPYTXT                     
813500                                     IN  FIL-WDR801(5:4)                  
813600     PERFORM IMS-ISRT-WLFILB01                                            
813700                                                                          
813800     PERFORM UNTIL SEGMENT-FINNS                                          
813900         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
814000         PERFORM IMS-ISRT-WLFILB01                                        
814100     END-PERFORM                                                          
814200     IF DCS-KDTRADP = 'BR12'                                              
814300         PERFORM S44-FILL-NOTF-AREA                                       
814400     END-IF                                                               
814500     .                                                                    
814600     EJECT                                                                
814700                                                                          
814800 S31A-SKAPA-SAP-TRANS-HAC SECTION.                                        
814900******************************************************************        
815000*         HOLD AT CUSTOM SHOULD BE BOOKED AT NDC TR                       
815100*         WDR8 FOR ALL THE QTY                                            
815200******************************************************************        
815300     MOVE '102'                       TO R8-EKH-KDEKHHT                   
815400     MOVE '126'                       TO R8-EKH-KDEKSHT                   
815500     MOVE W-KVAVIS                    TO R8-EKH-KVANTAL                   
815600     MOVE 'WL010300'                  TO FIL-IDPGM IN FIL-WDR801          
815700     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
815800     ACCEPT FIL-TIKLOCK IN FIL-WDR801 FROM TIME                           
815900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
816000     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
816100     ADD +1                           TO W-IDSEKVNR-SAP                   
816200     MOVE W-IDSEKVNR-SAP          TO FIL-IDSEKVNR IN FIL-WDR801           
816300     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
816400     MOVE W-IDDC-B6-SEND              TO R8-EKH-IDDC-SEND                 
816500     MOVE REQU-IDDC-KEY               TO R8-EKH-IDDC-REC                  
816600     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
816700     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
816800     MOVE ZERO TO NOLL-RAKNARE                                            
816900     MOVE W-SPAR-IDFAKT               TO WS-SAP-IDFAKT                    
817000     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
817100     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
817200          FOR LEADING ZERO                                                
817300     ADD +1 TO NOLL-RAKNARE                                               
817400     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
817500          WITH POINTER NOLL-RAKNARE                                       
817600     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
817700     MOVE INL-PRARTNTO                TO R8-EKH-PRARTNTO                  
817800     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
817900     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
818000                                                                          
818100     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
818200                                         R8-EKH-PRARTSJK                  
818300                                         R8-EKH-PRINK                     
818400                                         R8-EKH-PRDIRLON                  
818500                                         R8-EKH-PRDMTRL                   
818600                                         R8-EKH-PROVRPAL                  
818700                                         R8-EKH-SUBEL                     
818800     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
818900     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
819000     MOVE 1.00                        TO R8-EKH-PRKURS                    
819100     MOVE 'L103'                      TO R8-EKH-IDTRANS                   
819200     MOVE ZERO                        TO R8-EKH-BEVAT                     
819300                                         R8-EKH-IDANALYS                  
819400                                         R8-EKH-IDKONTO                   
819500                                         R8-EKH-KDANMORS                  
819600                                         R8-EKH-SUVAT                     
819700                                         R8-EKH-KDFRAKT                   
819800                                         R8-EKH-PRLANDCO                  
819900                                         R8-EKH-DAAVIDAT                  
820000                                         R8-EKH-IDAVINR                   
820100                                         R8-EKH-KDAVVTYP                  
820200                                         R8-EKH-KDRT                      
820300                                         R8-EKH-KVANTMOT                  
820400                                         R8-EKH-KVAVIS                    
820500     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
820600     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
820700     MOVE SPACE                      TO  R8-EKH-FLDCET                    
820800                                         R8-EKH-IDKST                     
820900     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
821000     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
821100                                                                          
821200     MOVE DCS-KDVALISO               TO  R8-EKH-KDVALISO                  
821300     MOVE DCS-KDTRADP                TO  R8-EKH-KDTRADP                   
821400     IF NDC-CN                                                            
821500       MOVE 'W570'                   TO  FIL-IDCPYTXT                     
821600                                     IN  FIL-WDR801(1:4)                  
821700     ELSE                                                                 
821800       IF NDC-IN                                                          
821900         MOVE 'W515'                 TO  FIL-IDCPYTXT                     
822000                                     IN  FIL-WDR801(1:4)                  
822100       ELSE                                                               
822200         MOVE DCS-KDTRADP            TO  FIL-IDCPYTXT                     
822300                                     IN  FIL-WDR801(1:4)                  
822400       END-IF                                                             
822500     END-IF                                                               
822600     MOVE 'EKHA'                     TO  FIL-IDCPYTXT                     
822700                                     IN  FIL-WDR801(5:4)                  
822800     PERFORM IMS-ISRT-WLFILB01                                            
822900                                                                          
823000     PERFORM UNTIL SEGMENT-FINNS                                          
823100         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
823200         PERFORM IMS-ISRT-WLFILB01                                        
823300     END-PERFORM                                                          
823400     .                                                                    
823500     EJECT                                                                
823600 S31-SKAPA-SAP-TRANS-DAM SECTION.                                         
823700******************************************************************        
823800* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
823900* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
824000*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
824100*         LIGGER KVAR TILLS VIDARE (WDR8).                                
824200******************************************************************        
824300                                                                          
824400     MOVE 'WL010300'                  TO FIL-IDPGM IN FIL-WDR801          
824500     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
824600     ACCEPT FIL-TIKLOCK IN FIL-WDR801  FROM TIME                          
824700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
824800     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
824900     ADD +1                           TO W-IDSEKVNR-SAP                   
825000     MOVE W-IDSEKVNR-SAP           TO FIL-IDSEKVNR IN FIL-WDR801          
825100     MOVE '102'                       TO R8-EKH-KDEKHHT                   
825200     IF DIST35-NONVCC-NONVCC-REFILL OR                                    
825300        DIST35-NONVCC-NONVCC-TRANSFER                                     
825400       MOVE '132'                     TO R8-EKH-KDEKSHT                   
825500     ELSE                                                                 
825600       MOVE '122'                     TO R8-EKH-KDEKSHT                   
825700       MOVE 'DAM'                     TO R8-EKH-CMD                       
825800     END-IF                                                               
825900     COMPUTE R8-EKH-KVANTAL =                                             
826000             W-KVSKROT(INDX) * -1                                         
826100     END-COMPUTE                                                          
826200     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
826300     MOVE W-IDDC-B6-SEND              TO R8-EKH-IDDC-SEND                 
826400     MOVE REQU-IDDC-KEY               TO R8-EKH-IDDC-REC                  
826500     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
826600     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
826700     MOVE ZERO TO NOLL-RAKNARE                                            
826800     MOVE W-SPAR-IDFAKT               TO WS-SAP-IDFAKT                    
826900     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
827000     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
827100          FOR LEADING ZERO                                                
827200     ADD +1 TO NOLL-RAKNARE                                               
827300     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
827400          WITH POINTER NOLL-RAKNARE                                       
827500     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
827600     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
827700     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
827800                                                                          
827900     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
828000                                         R8-EKH-PRARTSJK                  
828100                                         R8-EKH-PRINK                     
828200                                         R8-EKH-PRDIRLON                  
828300                                         R8-EKH-PRDMTRL                   
828400                                         R8-EKH-PROVRPAL                  
828500                                         R8-EKH-SUBEL                     
828600     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
828700     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
828800     MOVE 1.00                        TO R8-EKH-PRKURS                    
828900     MOVE 'L103'                      TO R8-EKH-IDTRANS                   
829000     MOVE ZERO                        TO R8-EKH-BEVAT                     
829100                                         R8-EKH-IDANALYS                  
829200                                         R8-EKH-IDKONTO                   
829300                                         R8-EKH-KDANMORS                  
829400                                         R8-EKH-SUVAT                     
829500                                         R8-EKH-KDFRAKT                   
829600                                         R8-EKH-PRLANDCO                  
829700                                         R8-EKH-DAAVIDAT                  
829800                                         R8-EKH-IDAVINR                   
829900                                         R8-EKH-KDAVVTYP                  
830000                                         R8-EKH-KDRT                      
830100                                         R8-EKH-KVANTMOT                  
830200                                         R8-EKH-KVAVIS                    
830300     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
830400     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
830500                                         R8-EKH-IDKST                     
830600     MOVE SPACE                      TO  R8-EKH-FLDCET                    
830700     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
830800     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
830900     MOVE DCS-KDVALISO               TO  R8-EKH-KDVALISO                  
831000     MOVE DCS-KDTRADP                TO  R8-EKH-KDTRADP                   
831100     IF NDC-CN                                                            
831200       MOVE 'W570'                   TO  FIL-IDCPYTXT                     
831300                                     IN  FIL-WDR801(1:4)                  
831400     ELSE                                                                 
831500       IF NDC-IN                                                          
831600         MOVE 'W515'                 TO  FIL-IDCPYTXT                     
831700                                     IN  FIL-WDR801(1:4)                  
831800       ELSE                                                               
831900         MOVE DCS-KDTRADP            TO  FIL-IDCPYTXT                     
832000                                     IN  FIL-WDR801(1:4)                  
832100       END-IF                                                             
832200     END-IF                                                               
832300     MOVE 'EKHA'                     TO  FIL-IDCPYTXT                     
832400                                     IN  FIL-WDR801(5:4)                  
832500                                                                          
832600     PERFORM IMS-ISRT-WLFILB01                                            
832700                                                                          
832800     PERFORM UNTIL SEGMENT-FINNS                                          
832900         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
833000         PERFORM IMS-ISRT-WLFILB01                                        
833100     END-PERFORM                                                          
833200     IF DCS-KDTRADP = 'BR12'                                              
833300         PERFORM S44-FILL-NOTF-AREA                                       
833400     END-IF                                                               
833500     .                                                                    
833600     EJECT                                                                
833700                                                                          
833800 S32-SKAPA-DIFF-TRANS SECTION.                                            
833900     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
834000                             WS-DIFF-AAMMDD                               
834100     IF WS-SEK = 9                                                        
834200        MOVE 19           TO WS-DIFF-SS                                   
834300     ELSE                                                                 
834400        MOVE 20           TO WS-DIFF-SS                                   
834500     END-IF                                                               
834600     MOVE WS-DIFF-AAAAMMDD  TO FILC2-DAFAKT                               
834700     MOVE W-IDARTNR         TO FILC2-IDARTNR                              
834800     MOVE W-IDDC-B6-SEND    TO FILC2-IDDC-SEND                            
834900     MOVE REQU-IDDC-KEY     TO FILC2-IDDC-REC                             
835000     MOVE W-SPAR-IDFAKT     TO FILC2-IDFAKT                               
835100     MOVE W-SPAR-IDKUNDRF   TO FILC2-IDKUNDRF                             
835200     MOVE ZERO              TO FILC2-PRARTBES-PR                          
835300     MOVE 6302-IDLEVNR      TO FILC2-IDLEVNR                              
835400     MOVE FILC2-W61244      TO FILC2-FIL-WDR301-DATA                      
835500     ACCEPT W-TID FROM TIME                                               
835600     IF W-TID = FILC2-FIL-TIKLOCK                                         
835700        ADD +1              TO FILC2-FIL-IDSEKVNR                         
835800     ELSE                                                                 
835900        MOVE W-TID          TO FILC2-FIL-TIKLOCK                          
836000        MOVE +1             TO FILC2-FIL-IDSEKVNR                         
836100     END-IF                                                               
836200     PERFORM IMS-ISRT-WLFILC2                                             
836300     .                                                                    
836400     EJECT                                                                
836500                                                                          
836600 S33-SKAPA-LDC-TRANS SECTION.                                             
836700     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
836800                             WS-DIFF-AAMMDD                               
836900     IF WS-SEK = 9                                                        
837000        MOVE 19           TO WS-DIFF-SS                                   
837100     ELSE                                                                 
837200        MOVE 20           TO WS-DIFF-SS                                   
837300     END-IF                                                               
837400     MOVE 'J'               TO FILC3-FLINLREP                             
837500     MOVE WS-DIFF-AAAAMMDD  TO FILC3-DAFAKT                               
837600     MOVE W-IDARTNR         TO FILC3-IDARTNR                              
837700     IF 6302-IDDC-LEV NOT = SPACE                                         
837800        MOVE 6302-IDDC-LEV  TO FILC3-IDDC-SEND                            
837900     ELSE                                                                 
838000        MOVE W-IDDC-B6-SEND TO FILC3-IDDC-SEND                            
838100     END-IF                                                               
838200     MOVE REQU-IDDC-KEY     TO FILC3-IDDC-REC                             
838300     MOVE W-SPAR-IDFAKT     TO FILC3-IDFAKT                               
838400     MOVE W-SPAR-IDKUNDRF   TO FILC3-IDKUNDRF                             
838500     MOVE W-SPAR-IDKUNDNR   TO FILC3-IDKUNDNR                             
838600     MOVE W-SPAR-IDKOLLI    TO FILC3-IDKOLLI                              
838700     MOVE ZERO              TO FILC3-PRARTSTD                             
838800     MOVE DAGENS-DATUM      TO FILC3-DAREGDAT                             
838900     IF REQU-IDUSER-003 = ALL '+'                                         
839000        MOVE SPACE          TO FILC3-IDUSER                               
839100     ELSE                                                                 
839200        MOVE REQU-IDUSER-003 TO FILC3-IDUSER                              
839300     END-IF                                                               
839400     MOVE FILC3-W61247      TO FILC3-FIL-WDR301-DATA                      
839500     ACCEPT W-TID FROM TIME                                               
839600     IF W-TID = FILC3-FIL-TIKLOCK                                         
839700        ADD +1              TO FILC3-FIL-IDSEKVNR                         
839800     ELSE                                                                 
839900        MOVE W-TID          TO FILC3-FIL-TIKLOCK                          
840000        MOVE +1             TO FILC3-FIL-IDSEKVNR                         
840100     END-IF                                                               
840200     PERFORM IMS-ISRT-WLFILC3                                             
840300     .                                                                    
840400     EJECT                                                                
840500                                                                          
840600 S35-KOLLA-IDUSER SECTION.                                                
840700     MOVE JA                 TO IDUSER-SW                                 
840800     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
840900        IF DCS-FLBINNUT = JA                                              
841000           MOVE NEJ          TO IDUSER-SW                                 
841100        END-IF                                                            
841200     ELSE                                                                 
841300        MOVE REQU-IDUSER-003 TO WS-IDUSER-003                             
841400     END-IF                                                               
841500     .                                                                    
841600     EJECT                                                                
841700                                                                          
841800 S37-SKAPA-NDC-HIST-SANDANDE SECTION.                                     
841900**** NYTT IDLEVNR 970918                                                  
842000     MOVE DCS-IDLEVNR-DC     TO INL-IDLEVNR                               
842100****                                                                      
842200     MOVE W-IDARTNR         TO ART-IDARTNR                                
842300     PERFORM IMS-ISRT-WLINLC01                                            
842400     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
842500     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
842600                                                                          
842700     COMPUTE W-DAINLEV = 9999999999999999                                 
842800                       - W-TIAAAAMMDDTTMMSSTH                             
842900     END-COMPUTE                                                          
843000                                                                          
843100     MOVE W-DAINLEV         TO INL-DAINLEV                                
843200     MOVE ZERO              TO INL-ADLAGOMR                               
843300                               INL-ADGANG                                 
843400                               INL-ADPLATS                                
843500     MOVE SPACE             TO INL-FLMAKUL                                
843600                               INL-FLSKAKOL                               
843700     MOVE 'N'               TO INL-FLPRIO                                 
843800     MOVE 'N'               TO INL-FLTULLST                               
843900     MOVE SEND-WS-IDDC      TO INL-IDDC                                   
844000     MOVE SPACE             TO INL-IDDC-LEV                               
844100     MOVE ZERO              TO INL-IDLOPNRM                               
844200     MOVE W-SPAR-IDFAKT     TO INL-IDFAKT                                 
844300     MOVE 6302-IDDISTR      TO INL-IDDISTR                                
844400     MOVE 6302-IDKUNDNR     TO INL-IDKUNDNR                               
844500     MOVE W-SPAR-IDKUNDRF   TO INL-IDKUNDRF                               
844600     MOVE W-SPAR-IDKOLLI    TO INL-IDKOLLI                                
844700     MOVE 'R34'             TO INL-IDPTYP                                 
844800     MOVE SPACE             TO INL-IDUSER-003                             
844900     MOVE ZERO              TO INL-KDFRAKT                                
845000     MOVE SPACE             TO INL-KDKOLLI                                
845100                               INL-KDVALISO                               
845200                               INL-ADINLOMR                               
845300                               INL-IDANALYS                               
845400                               INL-IDKST                                  
845500     MOVE ZERO              TO INL-KVAVIS                                 
845600                               INL-KVART-SKROT                            
845700                               INL-KDRT                                   
845800                               INL-IDKONTO                                
845900                               INL-KVRETUR                                
846000                               INL-KVTULRET                               
846100                               INL-KDAVVANT                               
846200                               INL-TIAVIDAT                               
846300     SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                                  
846400                            GIVING INL-KVANTMOT                           
846500     MOVE ZERO              TO INL-PRARTNTO                               
846600                               INL-PRKURS                                 
846700                               INL-TIBERANK                               
846800                               INL-TIINLINL                               
846900                               INL-TIINLMOT                               
847000                               INL-TIINLMTI                               
847100                               INL-TIINLITI                               
847200                                                                          
847300     PERFORM IMS-ISRT-WLINLC11                                            
847400     PERFORM UNTIL INSERT-OK                                              
847500       SUBTRACT 1      FROM W-DAINLEV                                     
847600       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
847700       PERFORM IMS-ISRT-WLINLC11                                          
847800     END-PERFORM                                                          
847900     .                                                                    
848000     EJECT                                                                
848100                                                                          
848200 S39-UPDATE-WDK728 SECTION.                                               
848300     MOVE TINL-IDTRACK TO W-IDTRACK                                       
848400     PERFORM IMS-GHNP-WDK728                                              
848500     IF SEGMENT-SAKNAS                                                    
848600        MOVE ALL '+'           TO WDK7-W005WDK7                           
848700        MOVE 'WDK728'          TO WDK7-IDSEGM                             
848800        MOVE W-IDARTNR         TO WDK7-IDARTNR-KFB                        
848900        MOVE REQU-IDDC-KEY     TO WDK7-IDDC-KFB                           
849000        MOVE WS-TRCK-KVANTMOT  TO WDK7-KVANTMOT                           
849100        MOVE WS-TRCK-KVAVIS    TO WDK7-KVAVIS                             
849200        MOVE WS-TRCK-KVANTMOT  TO WDK7-KVTRACK-KVAR                       
849300                                  WS-WDL3-TRCK-KVTRACK-KVAR               
849400        MOVE W-IDTRACK         TO WDK7-IDTRACK                            
849500        IF WS-TRCK-KVANTMOT > 0                                           
849600           CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                     
849700                                             ARTC-PCB WDK7-PCB            
849800           MOVE WDK7-WDK728       TO TRCK-WDK728                          
849900        END-IF                                                            
850000     ELSE                                                                 
850100        ADD WS-TRCK-KVANTMOT  TO TRCK-KVANTMOT                            
850200        ADD WS-TRCK-KVAVIS    TO TRCK-KVAVIS                              
850300        ADD WS-TRCK-KVANTMOT  TO TRCK-KVTRACK-KVAR                        
850400        PERFORM IMS-REPL-WDK728                                           
850500        MOVE TRCK-KVTRACK-KVAR TO WS-WDL3-TRCK-KVTRACK-KVAR               
850600     END-IF                                                               
850700                                                                          
850800**** UPDATE WDL3 LOGG DATABASE ****                                       
850900     PERFORM S23-TRACKLOG-DATA                                            
851000     PERFORM S24-ISRT-TRACKLOG                                            
851100     .                                                                    
851200     EJECT                                                                
851300                                                                          
851400 S40-SEND-OPEN SECTION.                                                   
851500     MOVE 'OPEN'                        TO SEND-KDFUNC                    
851600     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
851700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
851800                         SEND-OPEN-AREA                                   
851900     IF SEND-KDRC > 0                                                     
852000        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
852100        STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                     
852200            DELIMITED BY SIZE INTO FELTEXT                                
852300        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
852400     END-IF                                                               
852500     MOVE SEND-IDCOM                  TO WZ04-SEND-IDCOM                  
852600     .                                                                    
852700     EJECT                                                                
852800 S41-SEND-PUT-PROP SECTION.                                               
852900                                                                          
853000     SET PROP-IX                 TO +1                                    
853100*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
853200     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
853300     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
853400     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
853500                                                                          
853600     SET PROP-IX              UP BY +1                                    
853700*    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
853800     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
853900     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
854000     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
854100                                                                          
854200*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
854300*    IS CALCULATED.                                                       
854400     SET PROP-KVANTAL            TO PROP-IX                               
854500                                                                          
854600     MOVE 'PUT'                            TO SEND-KDFUNC                 
854700     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
854800     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
854900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
855000                         SEND-KVDLEN                                      
855100                         PROP-WZ04PROP                                    
855200     IF SEND-KDRC > 1                                                     
855300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
855400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
855500       DELIMITED BY SIZE INTO FELTEXT                                     
855600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
855700     END-IF                                                               
855800     .                                                                    
855900     EJECT                                                                
856000 S42-SEND-PUT SECTION.                                                    
856100                                                                          
856200     MOVE 'PUT'                            TO SEND-KDFUNC                 
856300     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
856400     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
856500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
856600                         SEND-KVDLEN                                      
856700                         NOTF-AREA                                        
856800     IF SEND-KDRC > 1                                                     
856900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
857000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
857100           DELIMITED BY SIZE INTO FELTEXT                                 
857200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
857300     END-IF                                                               
857400     .                                                                    
857500     EJECT                                                                
857600 S43-SEND-CLOSE SECTION.                                                  
857700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
857800     MOVE WZ04-SEND-IDCOM            TO SEND-IDCOM                        
857900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
858000                                                                          
858100     IF SEND-KDRC > 0                                                     
858200        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
858300        STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                    
858400             DELIMITED BY SIZE INTO FELTEXT                               
858500        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
858600     END-IF                                                               
858700     .                                                                    
858800     EJECT                                                                
858900 S44-FILL-NOTF-AREA SECTION.                                              
859000     IF NOT-FIRST-REC-TRANS                                               
859100       MOVE ZERO TO NOTF-IDSEKVNR                                         
859200       PERFORM S40-SEND-OPEN                                              
859300       PERFORM S41-SEND-PUT-PROP                                          
859400       MOVE JA  TO FIRST-REC-TRANS-SW                                     
859500     END-IF                                                               
859600     MOVE R8-EKH-KDEKHHT TO NOTF-KDEKHHT                                  
859700     MOVE R8-EKH-KDEKSHT TO NOTF-KDEKSHT                                  
859800     MOVE R8-EKH-DAVERDAT TO NOTF-DAVERDAT                                
859900     MOVE W-DAGENS-TID (1:6)  TO NOTF-TIREGTID                            
860000     MOVE R8-EKH-IDVERGL TO NOTF-IDVERGL                                  
860100     MOVE R8-EKH-IDDC-SEND TO NOTF-IDDC                                   
860200     MOVE WS-SAP-IDFAKT TO NOTF-IDFAKT                                    
860300     MOVE R8-EKH-IDKUNDNR TO NOTF-IDKUNDNR                                
860400     MOVE W-IDORDER TO NOTF-IDORDER                                       
860500     MOVE W-IDKOLLI TO NOTF-IDKOLLI                                       
860600     MOVE R8-EKH-IDARTNR TO W-IDARTNR-EDIT-X                              
860700     MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                         
860800                         TO NOTF-IDARTNR20                                
860900     MOVE R8-EKH-KVANTAL TO NOTF-KVANTAL                                  
861000     ADD +1 TO NOTF-IDSEKVNR                                              
861100     PERFORM S42-SEND-PUT                                                 
861200     .                                                                    
861300     EJECT                                                                
861400 S99-SKAPA-SKROT-ORDER SECTION.                                           
861500     IF NOT TRANS-OHUVUD-DAM-SKAPAD                                       
861600       PERFORM S991-SKAPA-TRANS-ORDERHUVUD                                
861700       PERFORM S992-SKAPA-HUVUD-ORDERRADER                                
861800       MOVE 1           TO ORAD-IX                                        
861900     END-IF                                                               
862000                                                                          
862100     PERFORM S993-EDIT-TRANS-ORDERRADER                                   
862200                                                                          
862300* EFTERSOM PÅ BILDEN KAN MAN HA 12 RADER OCH 4252-RAD-TABELLEN            
862400* HAR MINSKATS TILL 6 SÅ MÅSTE MAN BRYTA HÄR OCH ANROPA W006KOM!          
862500                                                                          
862600     IF ORAD-IX = ORAD-IX-MAX                                             
862700       PERFORM S03-CALL-W006KOM                                           
862800       PERFORM S992-SKAPA-HUVUD-ORDERRADER                                
862900       MOVE 0             TO ORAD-IX                                      
863000     END-IF                                                               
863100                                                                          
863200     ADD +1               TO ORAD-IX                                      
863300     .                                                                    
863400     EJECT                                                                
863500                                                                          
863600 S991-SKAPA-TRANS-ORDERHUVUD SECTION.                                     
863700     MOVE JA              TO TRANS-OHUVUD-DAM-SKAPAD-SW                   
863800                                                                          
863900     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
864000     MOVE +54             TO MSG-KOM-KVLL                                 
864100     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
864200     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
864300     MOVE SPACE           TO MSG-KOM-KDTRANS                              
864400     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
864500     IF DCS-CDC                                                           
864600       MOVE 'CDC-RET '      TO MSG-KOM-IDSNDNOD                           
864700     ELSE                                                                 
864800       MOVE 'SDC-RET '      TO MSG-KOM-IDSNDNOD                           
864900     END-IF                                                               
865000     MOVE 'WL010300'      TO MSG-KOM-IDSNDJOB                             
865100                                                                          
865200     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
865300     ACCEPT MSG-KOM-TIKLOCK FROM TIME                                     
865400     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
865500                             MSG-KOM-KDSVAR                               
865600     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
865700                          LENGTH OF OHUV-MID-W4I25101                     
865800     MOVE LOW-VALUE       TO P-TO-P-Z1                                    
865900     MOVE LOW-VALUE       TO P-TO-P-Z2                                    
866000     MOVE 'W4T251X'       TO P-TO-P-TRANSKOD                              
866100     MOVE '4251'          TO P-TO-P-FROM-MID                              
866200     MOVE 2               TO P-TO-P-KDMFSFOR                              
866300                                                                          
866400     PERFORM S01-DISTRIKT-RETUR                                           
866500                                                                          
866600     MOVE SPACE            TO KOM-AREA                                    
866700     MOVE 'W603'           TO OHUV-MID-IDSYSTEM                           
866800     MOVE W-IDDISTR-RETUR  TO OHUV-MID-IDDISTR                            
866900     MOVE W-IDKUNDNR-RETUR TO OHUV-MID-IDKUNDNR                           
867000                                                                          
867100     MOVE DAT-TIVV         TO W-IDORDNR-VV                                
867200     MOVE DAT-TID          TO W-IDORDNR-D                                 
867300     ACCEPT W-TIME-X       FROM TIME                                      
867400     MOVE W-TIME-TT       TO W-IDORDNR-TT                                 
867500                                                                          
867600*********** KONTROLL OM ORDERNR FINNS                                     
867700                                                                          
867800     MOVE ZERO             TO W-SEQC-IDDISTR                              
867900                              W-SEQC-IDKUNDNR                             
868000                              W-SEQC-IDORDNR7                             
868100     MOVE W-IDDISTR-RETUR  TO W-SEQC-IDDISTR                              
868200     MOVE W-IDKUNDNR-RETUR TO W-SEQC-IDKUNDNR                             
868300     MOVE W-IDORDNR-X      TO W-SEQC-IDORDNR7                             
868400     PERFORM IMS-GET-WDQ2C                                                
868500     PERFORM UNTIL SEGMENT-SAKNAS                                         
868600        ADD +1 TO W-IDORDNR-TT                                            
868700        MOVE W-IDORDNR-X TO W-SEQC-IDORDNR7                               
868800        PERFORM IMS-GET-WDQ2C                                             
868900     END-PERFORM                                                          
869000                                                                          
869100     MOVE W-IDORDNR-X     TO OHUV-MID-IDORDNR                             
869200     MOVE '1'             TO OHUV-MID-KDORDKL                             
869300                                                                          
869400     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
869500                             OHUV-MID-TIRFS                               
869600     MOVE SPACE           TO OHUV-MID-BEKUNDRF                            
869700                             OHUV-MID-KDFAKTYP                            
869800     MOVE NEJ             TO OHUV-MID-FLRESTN                             
869900     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
870000                             OHUV-MID-TITPO                               
870100                             OHUV-MID-BELAGINS                            
870200                             OHUV-MID-BEGMT                               
870300                             OHUV-MID-ADGMT-GATA                          
870400                             OHUV-MID-ADGMT-PADR                          
870500                             OHUV-MID-KDROPACK                            
870600                             OHUV-MID-BEVARREF                            
870700                             OHUV-MID-KDTULLVE                            
870800                             OHUV-MID-KDNOTES                             
870900     MOVE W-IDDC          TO W-IDKONTO-IDDC                               
871000     MOVE W-KDPRODSL      TO W-IDKONTO-KDPRODSL                           
871100     MOVE SPACE           TO OHUV-MID-IDKONTO                             
871200     MOVE SPACE           TO OHUV-MID-IDKST                               
871300     MOVE JA              TO OHUV-MID-FLAUTFAK                            
871400     MOVE JA              TO OHUV-MID-FLAUTPAC                            
871500     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
871600     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
871700     MOVE '57'            TO OHUV-MID-IDFTG                               
871800     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
871900                             OHUV-MID-ADBET                               
872000                             OHUV-MID-BEBET                               
872100                             OHUV-MID-IDSKYLT                             
872200                             OHUV-MID-FLLSBOK                             
872300     MOVE W-IDDC          TO OHUV-MID-IDDC                                
872400     MOVE SPACE           TO OHUV-MID-IDANALYS                            
872500     MOVE ZERO            TO OHUV-MID-IDDEPT                              
872600     MOVE SPACE           TO OHUV-MID-KDORDTYP-LDC                        
872700                             OHUV-MID-IDBILREG                            
872800                             OHUV-MID-IDVIN                               
872900                             OHUV-MID-IDCISNR                             
873000     MOVE ZERO            TO OHUV-MID-TIREPDAT                            
873100                             OHUV-MID-IDGROSS                             
873200     MOVE NEJ             TO OHUV-MID-FLFORBI                             
873300                             OHUV-MID-FLFORBI                             
873400                                                                          
873500     PERFORM S03-CALL-W006KOM                                             
873600     .                                                                    
873700     EJECT                                                                
873800                                                                          
873900 S992-SKAPA-HUVUD-ORDERRADER SECTION.                                     
874000     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
874100                          LENGTH OF ORAD-MID-W4I25201                     
874200     MOVE LOW-VALUE        TO P-TO-P-Z1                                   
874300     MOVE LOW-VALUE        TO P-TO-P-Z2                                   
874400     MOVE 'W4T252X'        TO P-TO-P-TRANSKOD                             
874500     MOVE '4252'           TO P-TO-P-FROM-MID                             
874600     MOVE 2                TO P-TO-P-KDMFSFOR                             
874700     MOVE SPACE            TO KOM-AREA                                    
874800     MOVE 'W603'           TO ORAD-MID-IDSYSTEM                           
874900     MOVE W-IDDISTR-RETUR  TO ORAD-MID-IDDISTR                            
875000     MOVE W-IDKUNDNR-RETUR TO ORAD-MID-IDKUNDNR                           
875100     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
875200     MOVE SPACE            TO ORAD-MID-BEVOLREF                           
875300     MOVE 'N'              TO ORAD-MID-FLSLUT                             
875400     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
875500     .                                                                    
875600     EJECT                                                                
875700                                                                          
875800 S993-EDIT-TRANS-ORDERRADER SECTION.                                      
875900     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR(ORAD-IX)                    
876000                             REK-IDARTNR                                  
876100     MOVE 9               TO REK-LNGD                                     
876200     MOVE 0               TO REK-REKSIFFR                                 
876300                                                                          
876400     CALL W009KSIF        USING REK-IDARTNR                               
876500                                REK-LNGD                                  
876600                                REK-REKSIFFR                              
876700                                                                          
876800     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR(ORAD-IX)                  
876900     IF NY-SKROT                                                          
877000         MOVE WS-KVSKROT-INM TO W-KVSKROT-6                               
877100         MOVE W-KVSKROT-6-X TO ORAD-MID-KVBEART(ORAD-IX)                  
877200         MOVE NEJ TO NY-SKROT-SW                                          
877300     ELSE                                                                 
877400         MOVE W-KVSKROT (INDX) TO W-KVSKROT-6                             
877500         MOVE W-KVSKROT-6-X    TO ORAD-MID-KVBEART(ORAD-IX)               
877600     END-IF                                                               
877700     MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)                
877800                              ORAD-MID-TITPO     (ORAD-IX)                
877900                              ORAD-MID-FLRESTN   (ORAD-IX)                
878000                              ORAD-MID-KDKVBRYT  (ORAD-IX)                
878100                              ORAD-MID-FLINVEST  (ORAD-IX)                
878200     MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)                
878300     MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)                
878400                              ORAD-MID-IDKST     (ORAD-IX)                
878500                              ORAD-MID-BERADREF  (ORAD-IX)                
878600                              ORAD-MID-KDDSP     (ORAD-IX)                
878700                              ORAD-MID-IDBIL     (ORAD-IX)                
878800     MOVE NEJ              TO ORAD-MID-FLSLATT   (ORAD-IX)                
878900     MOVE SPACE         TO ORAD-MID-PRARTNTO-LOC (ORAD-IX)                
879000     MOVE SPACE         TO ORAD-MID-PRARTBTO-LOC (ORAD-IX)                
879100     MOVE SPACE         TO     ORAD-MID-KDVALISO (ORAD-IX)                
879200     MOVE SPACE         TO     ORAD-MID-KDVAT    (ORAD-IX)                
879300     MOVE 0             TO     ORAD-MID-RERAB    (ORAD-IX)                
879400     MOVE SPACE         TO     ORAD-MID-KDRAB    (ORAD-IX)                
879500     MOVE SPACE         TO ORAD-MID-BEART-VIPS   (ORAD-IX)                
879600     MOVE ZERO          TO ORAD-MID-ADLAGOMR-CD  (ORAD-IX)                
879700                           ORAD-MID-ADGANG-CD    (ORAD-IX)                
879800                           ORAD-MID-ADPLATS-CD   (ORAD-IX)                
879900     MOVE SPACE         TO ORAD-MID-IDKUNDRF-WIP (ORAD-IX)                
880000                                                                          
880100     MOVE REQU-IDDC-KEY TO WS-IDDC                                        
880200                                                                          
880300     IF DCS-FLWEBDC = JA                                                  
880400       IF REQU-CMD-IN(INDX) = 'DAM'                                       
880500         MOVE 'REFSCRAP'   TO ORAD-MID-BERADREF  (ORAD-IX)                
880600       END-IF                                                             
880700                                                                          
880800       IF REQU-CMD-INM = 'DAM'                                            
880900         MOVE 'REFSCRAP'   TO ORAD-MID-BERADREF  (ORAD-IX)                
881000       END-IF                                                             
881100     END-IF                                                               
881200     .                                                                    
881300     EJECT                                                                
881400* --- IMS SEKTIONER ---                                                   
881500 IMS-ISRT-4506 SECTION.                                                   
881600     STRING 'WL450501(WDGXKEY  =' W-4505-KEY-X ')'                        
881700          DELIMITED BY SIZE INTO SSA1                                     
881800     MOVE 'WL450511 ' TO SSA2                                             
881900     MOVE '  ' TO GODK-STATUSKODER                                        
882000     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA-4505 SSA1 SSA2          
882100     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
882200     PERFORM IMS-STATUSKONTROLL                                           
882300     .                                                                    
882400     SKIP3                                                                
882500 IMS-GU-WDL6D1   SECTION.                                                 
882600     STRING 'WDL6D1  (WDL6D1KY=>' W-WDL6D1KY-MIN                          
882700                     '&WDL6D1KY=<' W-WDL6D1KY-MAX ')'                     
882800     DELIMITED BY SIZE INTO SSA1                                          
882900     MOVE '  GE' TO GODK-STATUSKODER                                      
883000     CALL CBLTDLI USING GU WDL6D-PCB DLI-IO-L6D1 SSA1                     
883100     MOVE WDL6D-STATUS-CODE TO STATUS-WS                                  
883200     PERFORM IMS-STATUSKONTROLL                                           
883300     .                                                                    
883400     SKIP3                                                                
883500 IMS-ISRT-6352 SECTION.                                                   
883600     STRING 'WDR501  (WDGXKEY  =' W-WDGX6351-X ')'                        
883700          DELIMITED BY SIZE INTO SSA1                                     
883800     MOVE 'WDGX6352 ' TO SSA2                                             
883900     MOVE '  II' TO GODK-STATUSKODER                                      
884000     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-6352 SSA1 SSA2          
884100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
884200     PERFORM IMS-STATUSKONTROLL                                           
884300     .                                                                    
884400     SKIP3                                                                
884500 IMS-GU-WDB301 SECTION.                                                   
884600     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
884700                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
884800          DELIMITED BY SIZE INTO SSA1                                     
884900     MOVE '  GE' TO GODK-STATUSKODER                                      
885000     CALL CBLTDLI USING GU KNDB-PCB DLI-IO-AREA-WDB3 SSA1                 
885100     MOVE KNDB-STATUS-CODE TO STATUS-WS                                   
885200     PERFORM IMS-STATUSKONTROLL                                           
885300     .                                                                    
885400     SKIP3                                                                
885500 IMS-GHU-WL630111 SECTION.                                                
885600     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
885700          DELIMITED BY SIZE INTO SSA1                                     
885800     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
885900          DELIMITED BY SIZE INTO SSA2                                     
886000     MOVE SPACE TO GODK-STATUSKODER                                       
886100     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-AREA-WDGX SSA1 SSA2           
886200     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
886300     PERFORM IMS-STATUSKONTROLL                                           
886400     .                                                                    
886500     SKIP3                                                                
886600 IMS-REPL-WL630111 SECTION.                                               
886700     MOVE SPACE           TO GODK-STATUSKODER                             
886800     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-AREA-WDGX                    
886900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
887000     PERFORM IMS-STATUSKONTROLL                                           
887100     .                                                                    
887200     SKIP3                                                                
887300 IMS-DLET-WL630111 SECTION.                                               
887400     MOVE SPACE           TO GODK-STATUSKODER                             
887500     CALL CBLTDLI USING DLET 6301-PCB DLI-IO-AREA-WDGX                    
887600     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
887700     PERFORM IMS-STATUSKONTROLL                                           
887800     .                                                                    
887900     EJECT                                                                
888000 IMS-GU-WLINLD01-FIRST SECTION.                                           
888100     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
888200                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
888300          DELIMITED BY SIZE INTO SSA1                                     
888400     MOVE '  GE' TO GODK-STATUSKODER                                      
888500     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
888600     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
888700     PERFORM IMS-STATUSKONTROLL                                           
888800     .                                                                    
888900     SKIP3                                                                
889000 IMS-GN-WLINLD01-FIRST SECTION.                                           
889100     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
889200                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
889300          DELIMITED BY SIZE INTO SSA1                                     
889400     MOVE '  GE' TO GODK-STATUSKODER                                      
889500     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
889600     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
889700     PERFORM IMS-STATUSKONTROLL                                           
889800     .                                                                    
889900     SKIP3                                                                
890000 IMS-GU-WLINLD01-FIRST-310 SECTION.                                       
890100     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
890200                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
890300                    '&IDPTYP  = ' W-IDPTYP ')'                            
890400          DELIMITED BY SIZE INTO SSA1                                     
890500     MOVE '  GE' TO GODK-STATUSKODER                                      
890600     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
890700     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
890800     PERFORM IMS-STATUSKONTROLL                                           
890900     .                                                                    
891000     SKIP3                                                                
891100 IMS-GN-WLINLD01 SECTION.                                                 
891200     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
891300                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
891400                    '&IDPTYP  = ' W-IDPTYP ')'                            
891500          DELIMITED BY SIZE INTO SSA1                                     
891600     MOVE '  GE' TO GODK-STATUSKODER                                      
891700     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
891800                                                                          
891900     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
892000     PERFORM IMS-STATUSKONTROLL                                           
892100     .                                                                    
892200     EJECT                                                                
892300 IMS-GU-WLINLD01 SECTION.                                                 
892400     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
892500                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
892600                    '&IDPTYP  = ' W-IDPTYP ')'                            
892700          DELIMITED BY SIZE INTO SSA1                                     
892800     MOVE '  GE' TO GODK-STATUSKODER                                      
892900     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
893000     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
893100     PERFORM IMS-STATUSKONTROLL                                           
893200     .                                                                    
893300     SKIP3                                                                
893400 IMS-GU-WDL623    SECTION.                                                
893500     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
893600          DELIMITED BY SIZE INTO SSA1                                     
893700     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
893800          DELIMITED BY SIZE INTO SSA2                                     
893900     MOVE 'WDL623 ' TO SSA3                                               
894000     MOVE '  GE' TO GODK-STATUSKODER                                      
894100     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL623                         
894200                             SSA1 SSA2 SSA3                               
894300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
894400     PERFORM IMS-STATUSKONTROLL                                           
894500     .                                                                    
894600     EJECT                                                                
894700 IMS-ISRT-WDL623    SECTION.                                              
894800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
894900          DELIMITED BY SIZE INTO SSA1                                     
895000     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
895100          DELIMITED BY SIZE INTO SSA2                                     
895200     MOVE 'WDL623 ' TO SSA3                                               
895300     MOVE '  II' TO GODK-STATUSKODER                                      
895400     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL623                       
895500                             SSA1 SSA2 SSA3                               
895600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
895700     PERFORM IMS-STATUSKONTROLL                                           
895800     .                                                                    
895900     EJECT                                                                
896000 IMS-GHU-WDK701 SECTION.                                                  
896100     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
896200          DELIMITED BY SIZE INTO SSA1                                     
896300     MOVE 'GE  ' TO GODK-STATUSKODER                                      
896400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK701 SSA1              
896500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
896600     PERFORM IMS-STATUSKONTROLL                                           
896700     .                                                                    
896800     EJECT                                                                
896900 IMS-GNP-WDK711   SECTION.                                                
897000     STRING 'WDK711  (IDDC    = ' W-IDDC  ')'                             
897100          DELIMITED BY SIZE INTO SSA1                                     
897200     MOVE '  GE' TO GODK-STATUSKODER                                      
897300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK7 SSA1                
897400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
897500     PERFORM IMS-STATUSKONTROLL                                           
897600     .                                                                    
897700     SKIP3                                                                
897800 IMS-GET-WDK711 SECTION.                                                  
897900     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
898000          DELIMITED BY SIZE INTO SSA1                                     
898100     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
898200          DELIMITED BY SIZE INTO SSA2                                     
898300     MOVE SPACE TO GODK-STATUSKODER                                       
898400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
898500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
898600     PERFORM IMS-STATUSKONTROLL                                           
898700     .                                                                    
898800     EJECT                                                                
898900 IMS-REPL-WDK711 SECTION.                                                 
899000     MOVE '  ' TO GODK-STATUSKODER                                        
899100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
899200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
899300     PERFORM IMS-STATUSKONTROLL                                           
899400     .                                                                    
899500     SKIP3                                                                
899600 IMS-GU-WLARTC01 SECTION.                                                 
899700     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
899800          DELIMITED BY SIZE INTO SSA1                                     
899900     MOVE SPACE  TO GODK-STATUSKODER                                      
900000     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WDK601 SSA1                   
900100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
900200     PERFORM IMS-STATUSKONTROLL                                           
900300     .                                                                    
900400     SKIP3                                                                
900500 IMS-GET-WLARTC01 SECTION.                                                
900600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
900700          DELIMITED BY SIZE INTO SSA1                                     
900800     MOVE 'GE  ' TO GODK-STATUSKODER                                      
900900     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WDK601 SSA1                   
901000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
901100     PERFORM IMS-STATUSKONTROLL                                           
901200     .                                                                    
901300     EJECT                                                                
901400 IMS-GET-WLARTC11   SECTION.                                              
901500                                                                          
901600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
901700          DELIMITED BY SIZE INTO SSA1                                     
901800     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
901900          DELIMITED BY SIZE INTO SSA2                                     
902000     MOVE SPACE  TO GODK-STATUSKODER                                      
902100     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WDK611 SSA1 SSA2              
902200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
902300     PERFORM IMS-STATUSKONTROLL                                           
902400     .                                                                    
902500     SKIP3                                                                
902600 IMS-GNP-WLARTC11   SECTION.                                              
902700     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
902800          DELIMITED BY SIZE INTO SSA1                                     
902900     MOVE SPACE  TO GODK-STATUSKODER                                      
903000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK611 SSA1                   
903100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
903200     PERFORM IMS-STATUSKONTROLL                                           
903300     .                                                                    
903400     SKIP3                                                                
903500 IMS-GHNP-WLARTC11   SECTION.                                             
903600     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
903700          DELIMITED BY SIZE INTO SSA1                                     
903800     MOVE SPACE  TO GODK-STATUSKODER                                      
903900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WDK611 SSA1                  
904000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
904100     PERFORM IMS-STATUSKONTROLL                                           
904200     .                                                                    
904300     SKIP3                                                                
904400 IMS-GHU-WLARTC11 SECTION.                                                
904500     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
904600          DELIMITED BY SIZE INTO SSA1                                     
904700     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
904800          DELIMITED BY SIZE INTO SSA2                                     
904900     MOVE SPACE  TO GODK-STATUSKODER                                      
905000     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-WDK611 SSA1 SSA2             
905100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
905200     PERFORM IMS-STATUSKONTROLL                                           
905300     .                                                                    
905400     SKIP3                                                                
905500 IMS-REPL-WLARTC11 SECTION.                                               
905600     MOVE '  ' TO GODK-STATUSKODER                                        
905700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WDK611                       
905800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
905900     PERFORM IMS-STATUSKONTROLL                                           
906000     .                                                                    
906100     EJECT                                                                
906200 IMS-GHU-WLARTC11-GE SECTION.                                             
906300                                                                          
906400     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
906500          DELIMITED BY SIZE INTO SSA1                                     
906600     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
906700          DELIMITED BY SIZE INTO SSA2                                     
906800     MOVE '  GE' TO GODK-STATUSKODER                                      
906900     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-WDK611 SSA1 SSA2             
907000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
907100     PERFORM IMS-STATUSKONTROLL                                           
907200     .                                                                    
907300     SKIP3                                                                
907400 IMS-GU-WLINLC11 SECTION.                                                 
907500     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
907600          DELIMITED BY SIZE INTO SSA1                                     
907700     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
907800          DELIMITED BY SIZE INTO SSA2                                     
907900     MOVE SPACE  TO GODK-STATUSKODER                                      
908000     CALL CBLTDLI USING GU  INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2           
908100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
908200     PERFORM IMS-STATUSKONTROLL                                           
908300     .                                                                    
908400     SKIP3                                                                
908500 IMS-GU-WLINLC11-GE SECTION.                                              
908600     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
908700          DELIMITED BY SIZE INTO SSA1                                     
908800     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
908900          DELIMITED BY SIZE INTO SSA2                                     
909000     MOVE '  GE' TO GODK-STATUSKODER                                      
909100     CALL CBLTDLI USING GU  INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2           
909200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
909300     PERFORM IMS-STATUSKONTROLL                                           
909400     .                                                                    
909500     SKIP3                                                                
909600 IMS-GHU-WLINLC11 SECTION.                                                
909700     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
909800          DELIMITED BY SIZE INTO SSA1                                     
909900     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
910000          DELIMITED BY SIZE INTO SSA2                                     
910100     MOVE SPACE  TO GODK-STATUSKODER                                      
910200     CALL CBLTDLI USING GHU  INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2          
910300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
910400     PERFORM IMS-STATUSKONTROLL                                           
910500     .                                                                    
910600     SKIP3                                                                
910700 IMS-REPL-WLINLC11 SECTION.                                               
910800     MOVE '  ' TO GODK-STATUSKODER                                        
910900     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-WDL6                    
911000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
911100     PERFORM IMS-STATUSKONTROLL                                           
911200     .                                                                    
911300     EJECT                                                                
911400 IMS-ISRT-WLINLC01 SECTION.                                               
911500     MOVE 'WLINLC01 '      TO SSA1                                        
911600     MOVE '  II'           TO GODK-STATUSKODER                            
911700     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1               
911800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
911900     PERFORM IMS-STATUSKONTROLL                                           
912000     .                                                                    
912100     SKIP3                                                                
912200 IMS-ISRT-WLINLC11 SECTION.                                               
912300     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
912400          DELIMITED BY SIZE INTO SSA1                                     
912500     MOVE 'WLINLC11 '         TO SSA2                                     
912600     MOVE '  II'              TO GODK-STATUSKODER                         
912700     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2          
912800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
912900     PERFORM IMS-STATUSKONTROLL                                           
913000     .                                                                    
913100     SKIP3                                                                
913200 IMS-ISRT-WLINLE01 SECTION.                                               
913300     MOVE 'WLINLE01 ' TO SSA1                                             
913400     MOVE '  ' TO GODK-STATUSKODER                                        
913500     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-WDL2 SSA1               
913600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
913700     PERFORM IMS-STATUSKONTROLL                                           
913800     .                                                                    
913900     SKIP3                                                                
914000 IMS-GU-WLINLE01 SECTION.                                                 
914100     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
914200             DELIMITED BY SIZE INTO SSA1                                  
914300     MOVE '  GE' TO GODK-STATUSKODER                                      
914400     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA-WDL2 SSA1                 
914500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
914600     PERFORM IMS-STATUSKONTROLL                                           
914700     .                                                                    
914800     EJECT                                                                
914900 IMS-ISRT-WLINLE11 SECTION.                                               
915000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
915100          DELIMITED BY SIZE INTO SSA1                                     
915200     MOVE 'WLINLE11 ' TO SSA2                                             
915300     MOVE '  II' TO GODK-STATUSKODER                                      
915400     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-WDL2 SSA1 SSA2          
915500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
915600     PERFORM IMS-STATUSKONTROLL                                           
915700     .                                                                    
915800     SKIP3                                                                
915900 IMS-ISRT-WLINLE22 SECTION.                                               
916000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
916100          DELIMITED BY SIZE INTO SSA1                                     
916200     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
916300          DELIMITED BY SIZE INTO SSA2                                     
916400     MOVE 'WLINLE22 ' TO SSA3                                             
916500     MOVE '  ' TO GODK-STATUSKODER                                        
916600     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-WDL2                    
916700                        SSA1 SSA2 SSA3                                    
916800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
916900     PERFORM IMS-STATUSKONTROLL                                           
917000     .                                                                    
917100     SKIP3                                                                
917200 IMS-GU-WLBENA11 SECTION.                                                 
917300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
917400            DELIMITED BY SIZE INTO SSA1                                   
917500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT ')'                           
917600            DELIMITED BY SIZE INTO SSA2                                   
917700     MOVE '  GE' TO GODK-STATUSKODER                                      
917800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-WDD3 SSA1 SSA2            
917900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
918000     PERFORM IMS-STATUSKONTROLL                                           
918100     .                                                                    
918200     EJECT                                                                
918300 IMS-GHU-W6LOPA11 SECTION.                                                
918400     STRING 'W6LOPA01(W6GXKEY  =' W-6017KEY-X ')'                         
918500          DELIMITED BY SIZE INTO SSA1                                     
918600     MOVE 'W6LOPA11 ' TO SSA2                                             
918700     MOVE '  ' TO GODK-STATUSKODER                                        
918800     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA-W6GX SSA1 SSA2           
918900     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
919000     PERFORM IMS-STATUSKONTROLL                                           
919100     .                                                                    
919200     SKIP3                                                                
919300 IMS-REPL-W6LOPA SECTION.                                                 
919400     MOVE '  ' TO GODK-STATUSKODER                                        
919500     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA-W6GX                    
919600     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
919700     PERFORM IMS-STATUSKONTROLL                                           
919800     .                                                                    
919900     SKIP3                                                                
920000 IMS-GHU-WL630511 SECTION.                                                
920100     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
920200          DELIMITED BY SIZE INTO SSA1                                     
920300     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X  ')'                         
920400          DELIMITED BY SIZE INTO SSA2                                     
920500     MOVE 'GE  ' TO GODK-STATUSKODER                                      
920600     CALL CBLTDLI USING GHU 6305-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2          
920700     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
920800     PERFORM IMS-STATUSKONTROLL                                           
920900     .                                                                    
921000     EJECT                                                                
921100 IMS-ISRT-WL630521 SECTION.                                               
921200     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X    ')'                      
921300          DELIMITED BY SIZE INTO SSA1                                     
921400     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X      ')'                     
921500          DELIMITED BY SIZE INTO SSA2                                     
921600     MOVE 'WL630521 ' TO SSA3                                             
921700     MOVE 'II  ' TO GODK-STATUSKODER                                      
921800     CALL CBLTDLI USING ISRT 6305-PCB DLI-IO-AREA-WDGX2                   
921900                                      SSA1 SSA2 SSA3                      
922000     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
922100     PERFORM IMS-STATUSKONTROLL                                           
922200     .                                                                    
922300     SKIP3                                                                
922400 IMS-ISRT-WL630511 SECTION.                                               
922500     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X  ')'                        
922600          DELIMITED BY SIZE INTO SSA1                                     
922700     MOVE 'WL630511 ' TO SSA2                                             
922800     MOVE '  ' TO GODK-STATUSKODER                                        
922900     CALL CBLTDLI USING ISRT 6305-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2         
923000     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
923100     PERFORM IMS-STATUSKONTROLL                                           
923200     .                                                                    
923300     EJECT                                                                
923400 IMS-REPL-WL630511 SECTION.                                               
923500     MOVE SPACE TO GODK-STATUSKODER                                       
923600     CALL CBLTDLI USING REPL 6305-PCB DLI-IO-AREA-WDGX2                   
923700     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
923800     PERFORM IMS-STATUSKONTROLL                                           
923900     .                                                                    
924000     EJECT                                                                
924100 IMS-GET-WDK711-GE SECTION.                                               
924200     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
924300          DELIMITED BY SIZE INTO SSA1                                     
924400     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
924500          DELIMITED BY SIZE INTO SSA2                                     
924600     MOVE '  GE' TO GODK-STATUSKODER                                      
924700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
924800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
924900     PERFORM IMS-STATUSKONTROLL                                           
925000     .                                                                    
925100     SKIP3                                                                
925200 IMS-GHNP-WDK728 SECTION.                                                 
925300                                                                          
925400     STRING 'WDK728  (IDTRACK  =' W-IDTRACK ')'                           
925500          DELIMITED BY SIZE INTO SSA1                                     
925600     MOVE '  GE' TO GODK-STATUSKODER                                      
925700     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728                       
925800                            SSA1                                          
925900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
926000     PERFORM IMS-STATUSKONTROLL                                           
926100     .                                                                    
926200     EJECT                                                                
926300 IMS-REPL-WDK728 SECTION.                                                 
926400                                                                          
926500     MOVE '  ' TO GODK-STATUSKODER                                        
926600     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
926700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
926800     PERFORM IMS-STATUSKONTROLL                                           
926900     .                                                                    
927000     EJECT                                                                
927100 IMS-DLET-WDK728 SECTION.                                                 
927200                                                                          
927300     MOVE '  ' TO GODK-STATUSKODER                                        
927400     CALL CBLTDLI USING DLET WDK7-PCB DLI-IO-WDK728                       
927500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
927600     PERFORM IMS-STATUSKONTROLL                                           
927700     .                                                                    
927800     EJECT                                                                
927900 IMS-ISRT-WDL901 SECTION.                                                 
928000     MOVE 'WLLOGA01 ' TO SSA1                                             
928100     MOVE '  II' TO GODK-STATUSKODER                                      
928200     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
928300     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
928400     PERFORM IMS-STATUSKONTROLL                                           
928500     .                                                                    
928600     EJECT                                                                
928700 IMS-ISRT-WDL301 SECTION.                                                 
928800     MOVE 'WDL301 ' TO SSA1                                               
928900     MOVE '  II' TO GODK-STATUSKODER                                      
929000     CALL CBLTDLI USING ISRT WDL3-PCB DLI-IO-WDL301 SSA1                  
929100     MOVE WDL3-STATUS-CODE TO STATUS-WS                                   
929200     PERFORM IMS-STATUSKONTROLL                                           
929300     .                                                                    
929400     EJECT                                                                
929500 IMS-GU-LOCB01 SECTION.                                                   
929600     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
929700          DELIMITED BY SIZE INTO SSA1                                     
929800     MOVE '  GE' TO GODK-STATUSKODER                                      
929900     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA-LOCB SSA1                 
930000     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
930100     PERFORM IMS-STATUSKONTROLL                                           
930200     .                                                                    
930300     SKIP3                                                                
930400 IMS-ISRT-LOCB01 SECTION.                                                 
930500     MOVE 'WLLOCB01 ' TO SSA1                                             
930600     MOVE '  ' TO GODK-STATUSKODER                                        
930700     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1               
930800     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
930900     PERFORM IMS-STATUSKONTROLL                                           
931000     .                                                                    
931100     SKIP3                                                                
931200 IMS-GHNP-LOCB11 SECTION.                                                 
931300     STRING 'WLLOCB11(IDDC     =' W-IDDC   ')'                            
931400             DELIMITED BY SIZE INTO SSA1                                  
931500     MOVE '  GE' TO GODK-STATUSKODER                                      
931600     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA-LOCB SSA1               
931700     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
931800     PERFORM IMS-STATUSKONTROLL                                           
931900     .                                                                    
932000     EJECT                                                                
932100 IMS-REPL-LOCB11 SECTION.                                                 
932200     MOVE '  ' TO GODK-STATUSKODER                                        
932300     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA-LOCB                    
932400     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
932500     PERFORM IMS-STATUSKONTROLL                                           
932600     .                                                                    
932700     SKIP3                                                                
932800 IMS-ISRT-LOCB11 SECTION.                                                 
932900     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
933000          DELIMITED BY SIZE INTO SSA1                                     
933100     MOVE 'WLLOCB11 ' TO SSA2                                             
933200     MOVE '  II' TO GODK-STATUSKODER                                      
933300     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1 SSA2          
933400     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
933500     PERFORM IMS-STATUSKONTROLL                                           
933600     .                                                                    
933700     SKIP3                                                                
933800 IMS-ISRT-WLSAPA01 SECTION.                                               
933900     MOVE 'WLSAPA01 ' TO SSA1                                             
934000     MOVE '  II' TO GODK-STATUSKODER                                      
934100     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
934200     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
934300     PERFORM IMS-STATUSKONTROLL                                           
934400     .                                                                    
934500     EJECT                                                                
934600 IMS-ISRT-WLFILB01 SECTION.                                               
934700     MOVE 'WLFILB01 ' TO SSA1                                             
934800     MOVE '  II' TO GODK-STATUSKODER                                      
934900     CALL CBLTDLI USING ISRT FILB-PCB WLFILB01 SSA1                       
935000     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
935100     PERFORM IMS-STATUSKONTROLL                                           
935200     .                                                                    
935300     EJECT                                                                
935400 IMS-ISRT-WLFILC2 SECTION.                                                
935500     STRING 'WLFILC01    '                                                
935600          DELIMITED BY SIZE INTO SSA1                                     
935700     MOVE '   ' TO GODK-STATUSKODER                                       
935800     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC2 SSA1              
935900     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
936000     PERFORM IMS-STATUSKONTROLL                                           
936100     .                                                                    
936200     SKIP3                                                                
936300 IMS-ISRT-WLFILC3 SECTION.                                                
936400     STRING 'WLFILC01    '                                                
936500          DELIMITED BY SIZE INTO SSA1                                     
936600     MOVE '   ' TO GODK-STATUSKODER                                       
936700     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC3 SSA1              
936800     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
936900     PERFORM IMS-STATUSKONTROLL                                           
937000     .                                                                    
937100     EJECT                                                                
937200 IMS-GET-WDQ2C SECTION.                                                   
937300     STRING 'WLORQL01(WDQ2C1KY =' W-WDQ2C1KY-X ')'                        
937400          DELIMITED BY SIZE INTO SSA1                                     
937500     MOVE '  GE' TO GODK-STATUSKODER                                      
937600     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ2C1 SSA1                   
937700     MOVE WDQ2C-STATUS-CODE TO STATUS-WS                                  
937800     PERFORM IMS-STATUSKONTROLL                                           
937900     .                                                                    
938000     EJECT                                                                
938100 IMS-GHU-WL630111-GE SECTION.                                             
938200     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
938300          DELIMITED BY SIZE INTO SSA1                                     
938400     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
938500          DELIMITED BY SIZE INTO SSA2                                     
938600     MOVE '  GE' TO GODK-STATUSKODER                                      
938700     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-AREA-WDGX SSA1 SSA2           
938800     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
938900     PERFORM IMS-STATUSKONTROLL                                           
939000     .                                                                    
939100     SKIP3                                                                
939200 IMS-GU-WDB601 SECTION.                                                   
939300                                                                          
939400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
939500         DELIMITED BY SIZE INTO SSA1                                      
939600     MOVE '  GE' TO GODK-STATUSKODER                                      
939700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
939800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
939900     PERFORM IMS-STATUSKONTROLL                                           
940000     IF SEGMENT-SAKNAS                                                    
940100        MOVE SPACE TO DCS-KDDC                                            
940200     END-IF                                                               
940300     .                                                                    
940400     SKIP3                                                                
940500 IMS-GU-WDB601-SEND SECTION.                                              
940600                                                                          
940700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-SEND-X ')'                    
940800         DELIMITED BY SIZE INTO SSA1                                      
940900     MOVE '  GE' TO GODK-STATUSKODER                                      
941000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
941100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
941200     PERFORM IMS-STATUSKONTROLL                                           
941300     IF SEGMENT-SAKNAS                                                    
941400        MOVE SPACE TO SEND-DCS-KDDC                                       
941500                      SEND-DCS-IDLEVNR-DC                                 
941600     END-IF                                                               
941700     .                                                                    
941800     SKIP3                                                                
941900 IMS-ISRT-WLFILC SECTION.                                                 
942000     STRING 'WLFILC01    '                                                
942100          DELIMITED BY SIZE INTO SSA1                                     
942200     MOVE '   ' TO GODK-STATUSKODER                                       
942300     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
942400     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
942500     PERFORM IMS-STATUSKONTROLL                                           
942600     .                                                                    
942700     EJECT                                                                
942800 IMS-GU-WDK711 SECTION.                                                   
942900                                                                          
943000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
943100          DELIMITED BY SIZE INTO SSA1                                     
943200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
943300          DELIMITED BY SIZE INTO SSA2                                     
943400     MOVE '  GE' TO GODK-STATUSKODER                                      
943500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7   SSA1 SSA2          
943600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
943700     PERFORM IMS-STATUSKONTROLL                                           
943800     .                                                                    
943900     EJECT                                                                
944000 IMS-GHU-DC71-WDL711 SECTION.                                             
944100                                                                          
944200     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
944300          DELIMITED BY SIZE INTO SSA1                                     
944400     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
944500          DELIMITED BY SIZE INTO SSA2                                     
944600     MOVE '  GE' TO GODK-STATUSKODER                                      
944700     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-WDL711 SSA1 SSA2              
944800     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
944900     PERFORM IMS-STATUSKONTROLL                                           
945000     .                                                                    
945100     SKIP3                                                                
945200 IMS-GU-WDL6A1   SECTION.                                                 
945300     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN2                         
945400                    '&WDL6A1KY=<' W-WDL6A1KY-MAX2 ')'                     
945500          DELIMITED BY SIZE INTO SSA1                                     
945600     MOVE '  GE' TO GODK-STATUSKODER                                      
945700     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-L6A1 SSA1                     
945800     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
945900     PERFORM IMS-STATUSKONTROLL                                           
946000     .                                                                    
946100     SKIP3                                                                
946200  IMS-GU-W6KVAH11     SECTION.                                            
946300                                                                          
946400      STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                        
946500           DELIMITED BY SIZE INTO SSA1                                    
946600      STRING 'W6KVAH11(W6D211KY >' W-W6D211KY-X ')'                       
946700           DELIMITED BY SIZE INTO SSA2                                    
946800      MOVE '  GE' TO GODK-STATUSKODER                                     
946900      CALL CBLTDLI USING GU  KVAH-PCB DLI-IO-AREA-6D21 SSA1 SSA2          
947000      MOVE KVAH-STATUS-CODE TO STATUS-WS                                  
947100      PERFORM IMS-STATUSKONTROLL                                          
947200      .                                                                   
947300      EJECT                                                               
947400 IMS-ROLLBACK    SECTION.                                                 
947500     MOVE '  ' TO GODK-STATUSKODER                                        
947600     CALL CBLTDLI USING ROLB    MSG-PCB                                   
947700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
947800     PERFORM IMS-STATUSKONTROLL                                           
947900     .                                                                    
948000     SKIP3                                                                
948100                                                                          
948200 IMS-STATUSKONTROLL SECTION.                                              
948300     SET STATUS-IX TO 1                                                   
948400     SEARCH GODK-STATUS                                                   
948500       AT END                                                             
948600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
948700         DELIMITED BY SIZE INTO FELTEXT                                   
948800         CALL FELLOG                                                      
948900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
949000         CONTINUE                                                         
949100     END-SEARCH                                                           
949200     .                                                                    
