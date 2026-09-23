000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4069500.                                                
000300 AUTHOR.         LARS THELL.                                              
000400 DATE-WRITTEN.   90/11/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        RADERNA FLYTTAS ÖVER FRÅN WDQ4 TILL WDE4 OCH WDE6                
001100*        NÄR DE ÄR UTSKRIVNA.                                             
001200*                                                                         
001300*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
001400*        PROGRAMMET LÄSER     WLARTS (WDK7)                               
001500*        PROGRAMMET LÄSER     WLARTN (WDD5)                               
001600*        PROGRAMMET LÄSER     WDF1                                        
001700*        PROGRAMMET LÄSER     WDB2                                        
001800*        PROGRAMMET UPPATERAR WLORQI (WDQ2)                               
001900*        PROGRAMMET UPPATERAR WLORQA (WDQ3)                               
002000*        PROGRAMMET UPPATERAR WLORQF (WDQ4)                               
002100*        PROGRAMMET UPPATERAR WDE4                                        
002200*        PROGRAMMET UPPATERAR WDE6                                        
002300*        PROGRAMMET UPPATERAR WDF6                                        
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T695                                              
002700*        MID:         W4I69501                                            
002800*                                                                         
002900* CHANGE LOG:                                                             
003000*                                                                         
003100                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W4069500'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 01  KDRC-DISPLAY                PIC Z(5).                                
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004800                                                                          
004900 77  WS-IDDC-SPAR                PIC X(2)    VALUE SPACE.                 
005000 01  -COPY WWDCKONS                                                       
005100                                                                          
005200 77  RADER                       PIC X       VALUE SPACE.                 
005400 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005500 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
005600 77  WS-IDKUNDNR                 PIC 9(7)    VALUE ZERO.                  
005700 77  WS-IDKUNDNR6                PIC 9(6)    VALUE ZERO.                  
005800 77  WS-IDORDNR5                 PIC 9(5)    VALUE ZERO.                  
005900 77  WS-IDORDNR7                 PIC 9(7)    VALUE ZERO.                  
006000 77  WS-IDPRODNR                 PIC 9(7)    VALUE ZERO.                  
006100 77  WS-IDPLKLST                 PIC 9(3)    VALUE ZERO.                  
006200 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006300 77  WS-IDKUNDNR-X               PIC Z(7)    VALUE SPACES.                
006400                                                                          
006500 77  IX                          PIC S9(9)   VALUE ZERO COMP SYNC.        
006600 77  IX-MAX                      PIC S9(9)   VALUE +7   COMP SYNC.        
006700                                                                          
006800*    --- SWITCHAR                                                         
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007400     88  ALLT-OK                             VALUE 'J'.                   
007500                                                                          
007600 77  TACDIS-POC-SW               PIC X       VALUE 'N'.                   
007700     88  TACDIS-POC                          VALUE 'J'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '4695'.                
008100     88  GODK-MID                            VALUE '4695'                 
008200                                                   '4293'                 
008300                                                   '4205'.                
008400                                                                          
008500 77  WS-DAP-HDR-SW               PIC X       VALUE 'N'.                   
008600     88 WS-DAP-HDR-SW-YES                    VALUE 'J'.                   
008700     88 WS-DAP-HDR-SW-NEJ                    VALUE 'N'.                   
008800                                                                          
008900 77  WS-DATUM-TID                PIC 9(11)   VALUE ZERO.                  
009000 77  WS-NEW-TIREPDAT             PIC 9(6)    VALUE ZERO.                  
009100 77  WS-DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                  
009200 77  WS-DAGENS-DATUM-Y2K         PIC 9(8)    VALUE ZERO.                  
009300 77  WS-DATUM-9KOMPL             PIC 9(8)    VALUE ZERO.                  
009400 77  WS-TID                      PIC 9(8)    VALUE ZERO.                  
009500 77  W-IDSID                     PIC 9(3)    VALUE ZERO.                  
009600 77  W-KVRADER                   PIC S9(5)   VALUE ZERO COMP-3.           
009700 77  W-IDKONTO                   PIC 9(11)   VALUE ZERO.                  
009800 77  W-IDFTG                     PIC 9(2)    VALUE ZERO.                  
009900 77  W-TIRFS                     PIC 9(11)   VALUE ZERO.                  
010000 77  MAX-RAD                     PIC S9(3)   VALUE +55  COMP-3.           
010100 77  RAD-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
010200 77  SID-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
010300 77  IX1                         PIC S9(9)   VALUE +0  COMP SYNC.         
010400 77  W-BEART                     PIC X(25)   VALUE SPACE.                 
010500                                                                          
010600 77  SOFT-SW                     PIC X       VALUE 'N'.                   
010700     88  SOFT                                VALUE 'J'.                   
010800                                                                          
010900 01  WS-DASNDDAT                  PIC 9(8)   VALUE 20000000.              
011000 01  FILLER REDEFINES WS-DASNDDAT.                                        
011100     03  FILLER                   PIC 9(2).                               
011200     03  WS-TISNDDAT              PIC 9(6).                               
011300                                                                          
011400 01  WS-DASKEPPN                  PIC 9(8)   VALUE 20000000.              
011500 01  FILLER REDEFINES WS-DASKEPPN.                                        
011600     03  FILLER                   PIC 9(2).                               
011700     03  WS-TISKEPPN              PIC 9(6).                               
011800                                                                          
011900 01  WS-TIAAAAMMDD               PIC  9(8).                               
012000 01  FILLER REDEFINES WS-TIAAAAMMDD.                                      
012100     03 WS-TIAA                  PIC  9(2).                               
012200     03 WS-TIAAMMDD              PIC  9(6).                               
012300                                                                          
012400***  TABELL FÖR ÖVERSÄTTNING AV  2 SIFFRIGA FÖRETAGS-                     
012500***  KODER TILL 1 SIFFRIGA                                                
012600 01      WS-FTGKODER.                                                     
012700   03    FILLER                  PIC  9(3)    VALUE 022.                  
012800   03    FILLER                  PIC  9(3)    VALUE 033.                  
012900   03    FILLER                  PIC  9(3)    VALUE 044.                  
013000   03    FILLER                  PIC  9(3)    VALUE 055.                  
013100   03    FILLER                  PIC  9(3)    VALUE 077.                  
013200   03    FILLER                  PIC  9(3)    VALUE 088.                  
013300   03    FILLER                  PIC  9(3)    VALUE 099.                  
013400   03    FILLER                  PIC  9(3)    VALUE 111.                  
013500   03    FILLER                  PIC  9(3)    VALUE 191.                  
013600   03    FILLER                  PIC  9(3)    VALUE 383.                  
013700   03    FILLER                  PIC  9(3)    VALUE 575.                  
013800   03    FILLER                  PIC  9(3)    VALUE 666.                  
013900   03    FILLER                  PIC  9(3)    VALUE 686.                  
014000   03    FILLER                  PIC  9(3)    VALUE 909.                  
014100                                                                          
014200 01      WS-TAB-FTGKODER REDEFINES WS-FTGKODER.                           
014300   03    FILLER                  OCCURS 14.                               
014400     05  WS-TAB-IDFTG-2POS       PIC  9(2).                               
014500     05  WS-TAB-IDFTG-1POS       PIC  9(1).                               
014600                                                                          
014700 01  WS-SAVE-IDCOM-MAIL          PIC S9(9)  COMP VALUE ZERO.              
014800 01  WS-SAVE-IDCOM-TACD          PIC S9(9)  COMP VALUE ZERO.              
014900                                                                          
015000*    --- DDGS NEW REPAIR DATE MAIL REPORT ---                             
015100 01  WS-REPORT                   PIC X(80) VALUE SPACES.                  
015200 01  WS-HEADING-1.                                                        
015300     03  FILLER                  PIC X(10) VALUE SPACES.                  
015400     03  FILLER                  PIC X(70) VALUE 'FOLLOWING LINES         
015500-    'HAVE ORDER CONF CODE 97 - REP DATE TOO CLOSE'.                      
015600 01  WS-HEADING-2.                                                        
015700     03  FILLER                  PIC X(08) VALUE 'DISTRICT'.              
015800     03  FILLER                  PIC X(02) VALUE SPACES.                  
015900     03  FILLER                  PIC X(08) VALUE 'CUSTOMER'.              
016000     03  FILLER                  PIC X(02) VALUE SPACES.                  
016100     03  FILLER                  PIC X(07) VALUE '  ORDER'.               
016200     03  FILLER                  PIC X(02) VALUE SPACES.                  
016300     03  FILLER                  PIC X(11) VALUE 'PART NUMBER'.           
016400     03  FILLER                  PIC X(02) VALUE SPACES.                  
016500     03  FILLER                  PIC X(14) VALUE 'OLD REPAIR DAT'.        
016600     03  FILLER                  PIC X(02) VALUE SPACES.                  
016700     03  FILLER                  PIC X(14) VALUE 'NEW REPAIR DAT'.        
016800     03  FILLER                  PIC X(01) VALUE SPACES.                  
016900 01  WS-LINE1.                                                            
017000     03  WS-LINE1-IDDISTR        PIC Z(08) VALUE SPACES.                  
017100     03  FILLER                  PIC X(02) VALUE SPACES.                  
017200     03  WS-LINE1-IDKUNDNR       PIC Z(08) VALUE SPACES.                  
017300     03  FILLER                  PIC X(02) VALUE SPACES.                  
017400     03  WS-LINE1-IDORDNR7       PIC Z(07) VALUE SPACES.                  
017500     03  FILLER                  PIC X(02) VALUE SPACES.                  
017600     03  WS-LINE1-IDARTNR        PIC Z(11) VALUE SPACES.                  
017700     03  FILLER                  PIC X(02) VALUE SPACES.                  
017800     03  WS-LINE1-TIREPDAT       PIC Z(14) VALUE SPACES.                  
017900     03  FILLER                  PIC X(02) VALUE SPACES.                  
018000     03  WS-LINE1-TIREPDAT-NEW   PIC Z(14) VALUE SPACES.                  
018100     03  FILLER                  PIC X(01) VALUE SPACES.                  
018200                                                                          
018300 01  FILLER                      PIC X(16)   VALUE '*W402TACD**'.         
018400*01  -COPY W402TACD                                                       
018500                                                                          
018600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018700 01  GENERELLA-SUBPROGRAM.                                                
018800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
019000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
019300     03  W009CIA                 PIC X(8)   VALUE 'W009CIA'.              
019400*                                                                         
019500 01  FILLER                      PIC X(16)   VALUE '**W009CIA**'.         
019600*01  -COPY W009CIA                                                        
019700                                                                          
019800 01  FILLER                      PIC X(24) VALUE 'WORKDAY-START '.        
019900*01  -COPY WORKAREA                                                       
020000                                                                          
020100 01  FILLER                      PIC X(16) VALUE 'WZ01SEND-AREA'.         
020200*01  -COPY WZ01SEND                                                       
020300                                                                          
020400 01  HDR-AREA.                                                            
020500*    03  -COPY WZ01REQU                                                   
020600*    03  -COPY WZ04HDR                                                    
020700                                                                          
020800*    --- PARAMETRAR FÖR ABEND                                             
020900 01  ERROR-TEXT                  PIC X(80).                               
021000 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
021100                                                                          
021200 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
021300 01     FILLER REDEFINES TEST-IDDISTR.                                    
021400*  03   -COPY WWDIST35.                                                   
021500                                                                          
021600 01     FILLER REDEFINES TEST-IDDISTR.                                    
021700*  03   -COPY WWDIST88.                                                   
021800                                                                          
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
022100     -COPY WWDIST57                                                       
022200                                                                          
022300*                                                                         
022400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022500*01  MID -COPY W4I69501                                                   
022600                                                                          
022700 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
022800*01  -COPY WMSGAREA                                                       
022900                                                                          
023000*    MSG-AREA FÖR HOPP TILL W40690                                        
023100 01  FILLER            PIC X(16)   VALUE '4690-MSG-IO-AREA'.              
023200 01  W-PROG-TO-PROG-SW.                                                   
023300     03  4690-KVLL               PIC S9(4) COMP SYNC.                     
023400     03  4690-Z1                 PIC X       VALUE LOW-VALUE.             
023500     03  4690-Z2                 PIC X       VALUE LOW-VALUE.             
023600     03  4690-TRANSKOD           PIC X(8)    VALUE 'W4T690X '.            
023700     03  4690-IDTRANS            PIC X(4)    VALUE '4695'.                
023800     03  4690-KDMFSFOR           PIC X       VALUE '1'.                   
023900*    03  -COPY W4I69001    -PRE 4690-                                     
024000                                                                          
024100                                                                          
024200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024300*                                                                         
024400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024500                                                                          
024600 01  NYCKLAR-TILL-DLI.                                                    
024700     03  W-IDORDER-X.                                                     
024800         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
024900     03  W-IDLEVNR-X.                                                     
025000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
025100     03  W-KDSEGKEY-X.                                                    
025200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
025300     03  W-IDDC-X.                                                        
025400         05  W-IDDC              PIC X(2)    VALUE '  '.                  
025500     03  W-IDDC-ARTS11-X.                                                 
025600         05  W-IDDC-ARTS11       PIC X(2)    VALUE '  '.                  
025800     03  W-IDARTNR-X.                                                     
025900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026000     03  W-IDPRODNR-X.                                                    
026100         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
026200     03  W-IDSKYLT-X.                                                     
026300         05  W-IDSKYLT           PIC  X(3).                               
026400                                                                          
026500     03  W-WDQ301KY-MIN-X.                                                
026600         05 W-Q301KY-MIN-IDORDER     PIC S9(7)  VALUE ZERO COMP-3.        
026700         05 W-Q301KY-MIN-IDDC        PIC X(2)   VALUE '  '.               
026800         05 W-Q301KY-MIN-IDPRODNR    PIC S9(7)  VALUE ZERO COMP-3.        
026900         05 W-Q301KY-MIN-IDPLKLST    PIC S9(3)  VALUE ZERO COMP-3.        
027000                                                                          
027100     03  W-WDQ301KY-MAX-X.                                                
027200         05 W-Q301KY-MAX-IDORDER     PIC S9(7)  VALUE ZERO COMP-3.        
027300         05 W-Q301KY-MAX-IDDC        PIC X(2)   VALUE '  '.               
027400         05 W-Q301KY-MAX-IDPRODNR    PIC S9(7)  VALUE ZERO COMP-3.        
027500         05 W-Q301KY-MAX-IDPLKLST    PIC S9(3)  VALUE ZERO COMP-3.        
027600                                                                          
027700     03  W-WDQ301KY-MIN.                                                  
027800         05 W-Q301KY-IDORDER-MIN     PIC S9(7)  VALUE ZERO COMP-3.        
027900         05 W-Q301KY-IDDC-MIN        PIC X(2)   VALUE '  '.               
028000         05 FILLER                   PIC X(6)   VALUE LOW-VALUE.          
028100                                                                          
028200     03  W-WDQ301KY-MAX.                                                  
028300         05 W-Q301KY-IDORDER-MAX     PIC S9(7)  VALUE ZERO COMP-3.        
028400         05 W-Q301KY-IDDC-MAX        PIC X(2)   VALUE '  '.               
028500         05 FILLER                   PIC X(6)   VALUE HIGH-VALUE.         
028600                                                                          
028700     03  W-WDQ401KY-MIN-X.                                                
028800         05 W-Q401KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO COMP-3.          
028900         05 W-Q401KY-MIN-IDDC     PIC X(2)    VALUE '  '.                 
029000         05 W-Q401KY-MIN-ADLAGOMR PIC S9(3)   VALUE ZERO COMP-3.          
029100         05 W-Q401KY-MIN-ADGANG   PIC S9(3)   VALUE ZERO COMP-3.          
029200         05 W-Q401KY-MIN-ADPLATS  PIC S9(5)   VALUE ZERO COMP-3.          
029300         05 W-Q401KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO COMP-3.          
029400         05 W-Q401KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO COMP-3.          
029500                                                                          
029600     03  W-WDQ401KY-MAX-X.                                                
029700         05 W-Q401KY-MAX-IDORDER  PIC S9(7)   VALUE ZERO COMP-3.          
029800         05 W-Q401KY-MAX-IDDC     PIC X(2)    VALUE '  '.                 
029900         05 W-Q401KY-MAX-ADLAGOMR PIC S9(3)   VALUE ZERO COMP-3.          
030000         05 W-Q401KY-MAX-ADGANG   PIC S9(3)   VALUE ZERO COMP-3.          
030100         05 W-Q401KY-MAX-ADPLATS  PIC S9(5)   VALUE ZERO COMP-3.          
030200         05 W-Q401KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO COMP-3.          
030300         05 W-Q401KY-MAX-IDLOPNR  PIC S9(3)   VALUE ZERO COMP-3.          
030400                                                                          
030500     03  W-WDE401KY-X.                                                    
030600         05 W-E4KEY-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.          
030700         05 W-E4KEY-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.          
030800         05 W-E4KEY-IDKUNDRF      PIC X(10)   VALUE SPACE.                
030900         05 W-E4KEY-IDPRODNR      PIC S9(7)   VALUE ZERO COMP-3.          
031000         05 W-E4KEY-IDPLKLST      PIC S9(3)   VALUE ZERO COMP-3.          
031100                                                                          
031200     03  W-WDE6KEY-X.                                                     
031300         05 W-E6KEY-IDPRODNR      PIC S9(7)   VALUE ZERO COMP-3.          
031400                                                                          
031500     03  W-IDDC-B6-X.                                                     
031600         05 W-IDDC-B6                  PIC X(2).                          
031700                                                                          
031800     03  W-IDLEVNR-F1-X.                                                  
031900         05 W-IDLEVNR-F1       PIC X(5).                                  
032000                                                                          
032100     03  W-WDF118KY-X.                                                    
032200         05  W-IDDISTR-F1      PIC S9(5)     VALUE ZERO COMP-3.           
032300         05  W-IDKUNDNR-F1     PIC S9(7)     VALUE ZERO COMP-3.           
032400         05  W-KDORDKL-F1      PIC S9        VALUE ZERO COMP-3.           
032500                                                                          
032600*--------------------WDB2                                                 
032700     03  W-IDGMT-X.                                                       
032800         05  W-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.            
032900         05  W-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.            
033000                                                                          
033100*    --- STATUS-KOD FRÅN IMS                                              
033200 01  STATUS-WS                   PIC XX.                                  
033300     88  SEGMENT-FINNS                       VALUE '  '.                  
033400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
033700                                                                          
033800 01  GODK-STATUSKODER.                                                    
033900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034000                                                                          
034100 01  ALL-SSA.                                                             
034200     03 SSA1                     PIC X(128).                              
034300     03 SSA2                     PIC X(160).                              
034400     03 SSA3                     PIC X(96).                               
034500                                                                          
034600                                                                          
034700*    --- IMS FUNKTIONSKODER                                               
034800*01  -COPY W0003                                                          
034900                                                                          
035000*    ---  DLI INPUT-OUTPUT AREA                                           
035500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ201'.        
035600 01  DLI-IO-WDQ201.                                                       
035700*    03  -COPY WDQ201                                                     
035800                                                                          
035900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ211'.        
036000 01  DLI-IO-WDQ211.                                                       
036100*    03  -COPY WDQ211                                                     
036200                                                                          
036500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ212'.        
036600 01  DLI-IO-WDQ212.                                                       
036700*    03  -COPY WDQ212                                                     
036900                                                                          
037000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ301'.        
037100 01  DLI-IO-WDQ301.                                                       
037200*    03  -COPY WDQ301                                                     
038100                                                                          
038200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ401'.        
038300 01  DLI-IO-WDQ401.                                                       
038400*    03  -COPY WDQ401                                                     
038500                                                                          
038600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E401'.         
038700 01  DLI-IO-E401.                                                         
038800*    03  -COPY WDE401                                                     
038900                                                                          
039000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E411'.         
039100 01  DLI-IO-E411.                                                         
039200*    03  -COPY WDE411                                                     
039300                                                                          
039400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
039500 01  DLI-IO-E601.                                                         
039600*    03  -COPY WDE601                                                     
039700                                                                          
039800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDF601'.        
039900 01  DLI-IO-WDF601.                                                       
040000*    03  -COPY WDF601                                                     
040100                                                                          
040200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDF611'.        
040300 01  DLI-IO-WDF611.                                                       
040400*    03  -COPY WDF611                                                     
040500                                                                          
040600                                                                          
040700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
040800 01  DLI-IO-AREA8.                                                        
040900     03  IO-AREA8                PIC X(1300)  VALUE SPACE.                
041000     03  WLBENA11 REDEFINES IO-AREA8.                                     
041100*        05  -COPY WDD311                                                 
041200                                                                          
041300     03  WLARTC11 REDEFINES IO-AREA8.                                     
041400*        05  -COPY WDK611                                                 
041500                                                                          
041600 01  DLI-IO-AREA11.                                                       
041700     03  IO-AREA11               PIC X(200)  VALUE SPACE.                 
041800                                                                          
041900     03  WLARTN01 REDEFINES IO-AREA11.                                    
042000*        05  -COPY WDD501.                                                
042100                                                                          
042200 01  DLI-IO-AREA12.                                                       
042300     03  IO-AREA12               PIC X(900)  VALUE SPACE.                 
042400     03  WLARTC11 REDEFINES IO-AREA12.                                    
042500*        05  -COPY WDK611       -PRE ARTC11-.                             
042600                                                                          
042700 01  DLI-IO-AREA-13.                                                      
042800     03  IO-AREA-13              PIC X(300)  VALUE SPACE.                 
042900     03  WLARTS11 REDEFINES IO-AREA-13.                                   
043000*        05  -COPY WDK711                                                 
043100                                                                          
043200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
043300 01   DLI-IO-AREA-B601.                                                   
043400*     03  -COPY WDB601                                                    
043500                                                                          
043600 01  FILLER               PIC X(16)   VALUE 'WDF118 AREA'.                
043700 01   DLI-IO-AREA-F118.                                                   
043800*     03  -COPY WDF118                                                    
043900                                                                          
044000 01  FILLER                  PIC X(16)   VALUE 'WDQ101 AREA'.             
044100 01  DLI-IO-AREA-Q101.                                                    
044200*    03   -COPY WDQ101                                                    
044300                                                                          
044400 01  FILLER                  PIC X(16)   VALUE 'IO-WDB201'.               
044500 01  DLI-IO-AREA-WDB201.                                                  
044600     03  WDB201.                                                          
044700         05  -COPY WDB201                                                 
044800                                                                          
044900 LINKAGE SECTION.                                                         
045000                                                                          
045100*01  -COPY W0009      -PRE MSG-                                           
045200                                                                          
045300*01  -COPY W0009      -PRE DISTRDOC-                                      
045400                                                                          
045500*01  -COPY W0009      -PRE 4690-                                          
045600                                                                          
045701*01  -COPY W0008      -PRE ORQI-                                          
045800     05  FILLER                  PIC X.                                   
045900                                                                          
046000*01  -COPY W0008      -PRE ORQA-                                          
046100     05  FILLER                  PIC X.                                   
046200                                                                          
046300*01  -COPY W0008      -PRE ORQF-                                          
046400     05  FILLER                  PIC X.                                   
046500                                                                          
046600*01  -COPY W0008      -PRE WDE4-                                          
046700     05  FILLER                  PIC X.                                   
046800                                                                          
046900*01  -COPY W0008      -PRE WDE6-                                          
047000     05  FILLER                  PIC X.                                   
047100                                                                          
047200*01  -COPY W0008      -PRE WDF6-                                          
047300     05  FILLER                  PIC X.                                   
047400                                                                          
047500*01  -COPY W0008      -PRE BENA-                                          
047600     05  FILLER                  PIC X.                                   
047700                                                                          
047800*01  -COPY W0008      -PRE ARTC-                                          
047900     05  FILLER                  PIC X.                                   
048000                                                                          
048100*01  -COPY W0008      -PRE ARTS-                                          
048200     05  FILLER                  PIC X.                                   
048300                                                                          
048400*01  -COPY W0008      -PRE ARTN-                                          
048500     05  FILLER                  PIC X.                                   
048600*01  -COPY W0008      -PRE WDB6-                                          
048700     05  FILLER                  PIC X.                                   
048800                                                                          
048900*01  -COPY W0008      -PRE WDF1-                                          
049000     05  FILLER                  PIC X.                                   
049100                                                                          
049200*01  -COPY W0008      -PRE WDQ1-                                          
049300     05  FILLER                  PIC X.                                   
049400                                                                          
049500*01  -COPY W0008      -PRE WDB2-                                          
049600     05  FILLER                  PIC X.                                   
049700                                                                          
049800                                                                          
049900 PROCEDURE DIVISION  USING MSG-PCB                                        
050000                           DISTRDOC-PCB                                   
050100                           4690-PCB                                       
050201                           ORQI-PCB                                       
050300                           ORQA-PCB                                       
050400                           ORQF-PCB                                       
050500                           WDE4-PCB                                       
050600                           WDE6-PCB                                       
050700                           WDF6-PCB                                       
050800                           BENA-PCB                                       
050900                           ARTC-PCB                                       
051000                           ARTS-PCB                                       
051100                           ARTN-PCB                                       
051200                           WDB6-PCB                                       
051300                           WDF1-PCB                                       
051400                           WDQ1-PCB                                       
051500                           WDB2-PCB.                                      
051600     ENTRY 'DLITCBL' USING MSG-PCB                                        
051700                           DISTRDOC-PCB                                   
051800                           4690-PCB                                       
051901                           ORQI-PCB                                       
052000                           ORQA-PCB                                       
052100                           ORQF-PCB                                       
052200                           WDE4-PCB                                       
052300                           WDE6-PCB                                       
052400                           WDF6-PCB                                       
052500                           BENA-PCB                                       
052600                           ARTC-PCB                                       
052700                           ARTS-PCB                                       
052800                           ARTN-PCB                                       
052900                           WDB6-PCB                                       
053000                           WDF1-PCB                                       
053100                           WDQ1-PCB                                       
053200                           WDB2-PCB.                                      
053300                                                                          
053400     PERFORM IMS-GET-MSG                                                  
053500     IF SEGMENT-FINNS                                                     
053600       PERFORM A-INIT                                                     
053700       IF GODK-MID                                                        
053800         PERFORM C-BEHANDLA-DIRLEV                                        
053900       END-IF                                                             
054000     END-IF                                                               
054100                                                                          
054200     IF WS-DAP-HDR-SW = JA                                                
054300       MOVE ALL '-'            TO WS-REPORT                               
054400       PERFORM S25-PUT-LINE-MAIL                                          
054500       PERFORM S29-SEND-CLOSE-MAIL                                        
054600       IF WS-SAVE-IDCOM-TACD > ZERO                                       
054700          PERFORM S39-SEND-CLOSE-TACD                                     
054800       END-IF                                                             
054900     END-IF                                                               
055000                                                                          
055100     MOVE ZERO TO RETURN-CODE                                             
055200     GOBACK                                                               
055300     .                                                                    
055400                                                                          
055500                                                                          
055600 A-INIT SECTION.                                                          
055700     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
055800                                                                          
055900     IF MSG-DUBBLA-TRANSKODER                                             
056000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I69501                 
056100       MOVE MSG-IDTRANS-2 TO W-IDTRANS                                    
056200       MOVE MSG-KDMFSFOR-2 TO 4690-KDMFSFOR                               
056300     ELSE                                                                 
056400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I69501                  
056500       MOVE MSG-IDTRANS-1 TO W-IDTRANS                                    
056600       MOVE MSG-KDMFSFOR-1 TO 4690-KDMFSFOR                               
056700     END-IF                                                               
056800                                                                          
056900     MOVE LOW-VALUE TO MSG-AREA                                           
057000                                                                          
057100     MOVE NEJ                  TO  SOFT-SW                                
057200     MOVE ZERO                 TO  W-KVRADER                              
057300                                                                          
057400     ACCEPT WS-DAGENS-DATUM    FROM DATE                                  
057500     ACCEPT WS-TID             FROM TIME                                  
057600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM-Y2K              
057700     .                                                                    
057800                                                                          
057900 C-BEHANDLA-DIRLEV       SECTION.                                         
058000     MOVE 'C-BEHANDLA-DIRL ' TO CURRENT-SECTION                           
058100                                                                          
058200     MOVE MID-IDORDER          TO W-IDORDER                               
058300                                                                          
058400     MOVE MID-IDDC             TO W-IDDC-B6                               
058500                                  WS-IDDC-SPAR                            
058600     PERFORM IMS-GU-WDB601                                                
058700                                                                          
058800     IF DCS-DDC                                                           
058900       MOVE WC-CDC-SE          TO W-IDDC                                  
059000     ELSE                                                                 
059100       MOVE MID-IDDC           TO W-IDDC                                  
059200                                  W-Q301KY-MIN-IDDC                       
059300                                  W-Q301KY-MAX-IDDC                       
059400                                  W-Q301KY-IDDC-MAX                       
059500     END-IF                                                               
059700                                                                          
059800     PERFORM IMS-GU-ORQI01                                                
059900     PERFORM IMS-GNP-ORQI12                                               
060200                                                                          
060300     PERFORM CA-KOLLA-TACDIS-POC                                          
060400                                                                          
060500     PERFORM IMS-GHNP-ORQI11-F                                            
060600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
060700                                                                          
060800         MOVE NEJ                  TO  SOFT-SW                            
060900                                                                          
061000         IF ARB-TIRFS          >  ZERO  OR                                
061100            ARB-DATRPAVD       >  ZERO  OR                                
061200            ARB-TIHHMM         >  ZERO                                    
061300             IF DIRL-KVRADER   >  ZERO                                    
061400                 IF DCS-IDDC NOT = DIRL-IDDC                              
061500                    MOVE DIRL-IDDC TO W-IDDC-B6                           
061600                                      WS-IDDC-SPAR                        
061700                    PERFORM IMS-GU-WDB601                                 
061800                 END-IF                                                   
061900                                                                          
062000                 IF DIRL-IDLEVNR = '1441 ' OR 'BP2TW'                     
062100                   MOVE JA          TO  SOFT-SW                           
062200                 END-IF                                                   
062300                                                                          
062400                 PERFORM CB-SKAPA-PACKUNDERLAG                            
062500                 PERFORM CC-UPPDATERA-WDQ301                              
062600                 PERFORM CD-UPPDATERA-WDQ211                              
062700             END-IF                                                       
062800         END-IF                                                           
062900         PERFORM IMS-GHNP-ORQI11                                          
063000     END-PERFORM                                                          
063100     PERFORM CF-UPPDATERA-WDQ201                                          
063200     .                                                                    
063300                                                                          
063400 CA-KOLLA-TACDIS-POC   SECTION.                                           
063500     MOVE 'CA-KOLLA-TACDIS ' TO CURRENT-SECTION                           
063600                                                                          
063700     MOVE OHUV-IDDISTR   TO W-IDDISTR                                     
063800     MOVE OHUV-IDKUNDNR  TO W-IDKUNDNR                                    
063900     PERFORM IMS-GU-WDB201                                                
064000     IF SEGMENT-FINNS AND                                                 
064100        GMT-FLOBKR-TACD = JA                                              
064200        MOVE JA  TO TACDIS-POC-SW                                         
064300     ELSE                                                                 
064400        MOVE NEJ TO TACDIS-POC-SW                                         
064500     END-IF                                                               
064600     .                                                                    
064700                                                                          
064800 CB-SKAPA-PACKUNDERLAG   SECTION.                                         
064900     MOVE 'CB-SKAPA-PACKUND' TO CURRENT-SECTION                           
065000                                                                          
065100     MOVE ZERO                 TO W-IDSID                                 
065200     MOVE LOW-VALUE            TO W-WDQ301KY-MIN-X                        
065300     MOVE HIGH-VALUE           TO W-WDQ301KY-MAX-X                        
065400                                                                          
065500     MOVE W-IDORDER            TO W-Q301KY-MIN-IDORDER                    
065600                                  W-Q301KY-MAX-IDORDER                    
065700                                  W-Q301KY-IDORDER-MIN                    
065800                                  W-Q301KY-IDORDER-MAX                    
065900                                                                          
066000     MOVE DIRL-IDLEVNR         TO W-IDLEVNR                               
066100                                                                          
066200     PERFORM IMS-GET-ORQA01                                               
066300                                                                          
066400     MOVE 'S  '                TO W-IDSKYLT                               
066500                                                                          
066600     PERFORM CBA-SKRIV-WDE401                                             
066700     PERFORM CBB-SKAPA-WDE401-NYCKEL                                      
066800                                                                          
066900     IF SOFT                                                              
067000       CONTINUE                                                           
067100     ELSE                                                                 
067200       PERFORM CBI-SKAPA-WDF601-SEGMENT                                   
067300     END-IF                                                               
067400*                                                                         
067500     IF DCS-IDDC NOT = WS-IDDC-SPAR                                       
067600        MOVE WS-IDDC-SPAR TO W-IDDC-B6                                    
067700        PERFORM IMS-GU-WDB601                                             
067800     END-IF                                                               
067900                                                                          
068000     PERFORM CBF-SKAPA-PU-RADER                                           
068100     PERFORM CBG-SKRIV-WDE601                                             
068200     .                                                                    
068300                                                                          
068400                                                                          
068500 CBA-SKRIV-WDE401              SECTION.                                   
068600     MOVE 'CBA-SKRIV-WDE401' TO CURRENT-SECTION                           
068700                                                                          
068800     MOVE OHUV-IDDISTR         TO  KORD-IDDISTR                           
068900                                   TEST-IDDISTR                           
069000     MOVE OHUV-IDKUNDNR        TO  KORD-IDKUNDNR                          
069100     MOVE SPACE                TO  KORD-IDKUNDRF                          
069200     MOVE ODEL-IDKUNDRF(3:5)   TO  KORD-IDORDNR5                          
069300     MOVE ODEL-IDPRODNR        TO  KORD-IDPRODNR                          
069400     MOVE ODEL-IDPLKLST        TO  KORD-IDPLKLST                          
069500     MOVE ODEL-KDVALISO        TO  KORD-KDVALISO                          
069600     MOVE OHUV-IDORDER         TO  KORD-IDORDER                           
069700     IF DCS-DDC                                                           
069800        MOVE DIRL-IDLEVNR     TO KORD-IDUSER                              
069900     END-IF                                                               
070000     MOVE ODEL-IDDC            TO  KORD-IDDC                              
070100     MOVE ZERO                 TO  KORD-KDFAKPAP                          
070200     IF OHUV-FLEMBORD          =   JA                                     
070300        MOVE 1                 TO  KORD-KDFAKPAP                          
070400     ELSE                                                                 
070500        MOVE ZERO              TO  KORD-KDFAKPAP                          
070600     END-IF                                                               
070700     MOVE NEJ                  TO  KORD-FLLSBOK                           
070800     MOVE OHUV-FLOVRLEV        TO  KORD-FLOVRLEV                          
070900     MOVE OHUV-FLORDSPE        TO  KORD-FLORDSPE                          
071000     MOVE OHUV-KDFAKTYP        TO  KORD-KDFAKTYP                          
071100     IF SOFT AND DIST88-SW-FREIGHTCODE                                    
071200       MOVE +88                TO  KORD-KDFRAKT                           
071300     ELSE                                                                 
071400       MOVE ARB-KDFRAKT        TO  KORD-KDFRAKT                           
071500     END-IF                                                               
071600     MOVE OHUV-KDORDKL         TO  KORD-KDORDKL                           
071700     MOVE OHUV-TIREGDAT        TO  KORD-TIORDREG                          
071800     MOVE DIRL-VKORDNTO        TO  KORD-VKORDNTO                          
071900     MOVE DIRL-VLORDNTO        TO  KORD-VLORDNTO                          
072000     MOVE NEJ                  TO  KORD-FLPAFEL                           
072100     MOVE ZERO                 TO  KORD-KDPAKOLL                          
072200                                   KORD-KVORDRAD-PACK                     
072300                                   KORD-SUORDV                            
072400                                   KORD-SUORDV-EXP                        
072500                                   KORD-SUORDV-LOC                        
072600                                   KORD-SUORDV-LOCPREL                    
072700                                   KORD-KVORDRAD                          
072800                                   KORD-KVORDRAD-VO                       
072900                                   KORD-IDARTNR-SATS                      
073000                                   KORD-KVBEART-SATS                      
073100                                   KORD-KDPERSON                          
073200                                                                          
073300     MOVE ODEL-KDFDKRAV        TO  KORD-KDFDKRAV                          
073400     MOVE ARB-TIRFS            TO  W-TIRFS                                
073500     MOVE W-TIRFS (1:7)        TO  KORD-TIBEGPAC                          
073600     MOVE WS-DAGENS-DATUM      TO  KORD-TIUTSKR                           
073700     MOVE DIRL-KVRADER         TO  KORD-KVORDRAD-LEVPL                    
073800                                   KORD-KVORDRAD-VO-LEVPL                 
073900     MOVE DIRL-SUORDV          TO  KORD-SUORDV-LEVPL                      
074000     MOVE DIRL-SUORDV-LOC      TO  KORD-SUORDV-LEVPL-LOC                  
074100     MOVE DIRL-SUORDV-LOCPREL  TO  KORD-SUORDV-LEVPL-LOCPREL              
074200     MOVE SPACE                TO  KORD-KDVALISO-EXP                      
074300                                                                          
074400     PERFORM IMS-ISRT-WDE401                                              
074500     .                                                                    
074600                                                                          
074700                                                                          
074800 CBB-SKAPA-WDE401-NYCKEL       SECTION.                                   
074900     MOVE 'CBB-SKAPA-WDE401' TO CURRENT-SECTION                           
075000                                                                          
075100     MOVE KORD-IDDISTR         TO  W-E4KEY-IDDISTR                        
075200                                   WS-IDDISTR                             
075300     MOVE KORD-IDKUNDNR        TO  W-E4KEY-IDKUNDNR                       
075400                                   WS-IDKUNDNR                            
075500                                   WS-IDKUNDNR6                           
075600     MOVE KORD-IDKUNDRF        TO  W-E4KEY-IDKUNDRF                       
075700     MOVE KORD-IDORDNR5        TO  WS-IDORDNR5                            
075800                                   WS-IDORDNR7                            
075900     MOVE KORD-IDPRODNR        TO  W-E4KEY-IDPRODNR                       
076000                                   WS-IDPRODNR                            
076100     MOVE KORD-IDPLKLST        TO  W-E4KEY-IDPLKLST                       
076200                                   WS-IDPLKLST                            
076300                                                                          
076400     .                                                                    
076500                                                                          
076600 CBF-SKAPA-PU-RADER            SECTION.                                   
076700     MOVE 'CBF-SKAPA-PU-RAD' TO CURRENT-SECTION                           
076800                                                                          
076900     MOVE LOW-VALUE            TO W-WDQ401KY-MIN-X                        
077000     MOVE HIGH-VALUE           TO W-WDQ401KY-MAX-X                        
077100     MOVE OHUV-IDORDER         TO W-Q401KY-MIN-IDORDER                    
077200                                  W-Q401KY-MAX-IDORDER                    
077300     PERFORM IMS-GET-ORQF01-GU                                            
077400     MOVE ZERO                 TO W-KVRADER                               
077500     PERFORM UNTIL  W-KVRADER   =  DIRL-KVRADER                           
077600        IF ORAD-IDLEVNR        IN ORAD-WDQ401                             
077700                               =  DIRL-IDLEVNR                            
077800            ADD +1             TO W-KVRADER                               
077900                                                                          
078000            MOVE ORAD-IDARTNR  IN ORAD-WDQ401                             
078100                               TO W-IDARTNR                               
078200            PERFORM CBFA-HAMTA-BEART                                      
078300                                                                          
078400            IF SOFT                                                       
078500              CONTINUE                                                    
078600            ELSE                                                          
078700              IF DCS-DDC                                                  
078800                 PERFORM CBFE-SKAPA-WDF611-SEGMENT                        
078900              END-IF                                                      
079000            END-IF                                                        
079100                                                                          
079200            PERFORM CBFD-SKRIV-WDE411                                     
079300            IF WS-NEW-TIREPDAT > ZERO                                     
079400              PERFORM CBFG-WRITE-WDQ101                                   
079500              PERFORM CBFH-SEND-MAIL                                      
079600              IF OBKR-IDSYSTEM = ('LDC ' OR 'TACD') AND                   
079700                 TACDIS-POC                                               
079800                                                                          
079900                 PERFORM CBFI-SKAPA-TACD-402                              
080000              END-IF                                                      
080100            END-IF                                                        
080200            PERFORM IMS-DLET-ORQF01                                       
080300        END-IF                                                            
080400        PERFORM IMS-GET-ORQF01-GN                                         
080500     END-PERFORM                                                          
080600     .                                                                    
080700                                                                          
080800                                                                          
080900 CBFA-HAMTA-BEART              SECTION.                                   
081000     MOVE 'CBFA-HAMTA-BEART' TO CURRENT-SECTION                           
081100                                                                          
081200     PERFORM IMS-GET-BENA11                                               
081300     IF SEGMENT-SAKNAS                                                    
081400         MOVE 'BENÄMN.SAKN'    TO W-BEART                                 
081500      ELSE                                                                
081600         MOVE TEXT-BEART       TO W-BEART                                 
081700     END-IF                                                               
081800     .                                                                    
081900                                                                          
082000                                                                          
082100 CBFD-SKRIV-WDE411             SECTION.                                   
082200     MOVE 'CBFD-SKIV-WDE411' TO CURRENT-SECTION                           
082300                                                                          
082400     MOVE W-KVRADER            TO  ORAD-IDPURAD                           
082500                               IN  ORAD-WDE411                            
082600     MOVE ORAD-IDARTNR         IN  ORAD-WDQ401                            
082700                               TO  ORAD-IDARTNR                           
082800                               IN  ORAD-WDE411                            
082900     MOVE ORAD-IDBIL           IN  ORAD-WDQ401                            
083000                               TO  ORAD-IDBIL                             
083100                               IN  ORAD-WDE411                            
083200     MOVE ORAD-IDKLIENT        IN  ORAD-WDQ401                            
083300                               TO  ORAD-IDKLIENT                          
083400                               IN  ORAD-WDE411                            
083500     MOVE ORAD-IDARBREF        IN  ORAD-WDQ401                            
083600                               TO  ORAD-IDARBREF                          
083700                               IN  ORAD-WDE411                            
083800     MOVE ORAD-IDVIN           IN  ORAD-WDQ401                            
083900                               TO  ORAD-IDVIN                             
084000                               IN  ORAD-WDE411                            
084100     MOVE ORAD-IDDC-RO         IN  ORAD-WDQ401                            
084200                               TO  ORAD-IDDC-RO                           
084300                               IN  ORAD-WDE411                            
084400     MOVE ORAD-REKSIFFR        IN  ORAD-WDQ401                            
084500                               TO  ORAD-REKSIFFR                          
084600                               IN  ORAD-WDE411                            
084700     MOVE ORAD-ADLAGOMR        IN  ORAD-WDQ401                            
084800                               TO  ORAD-ADLAGOMR                          
084900                               IN  ORAD-WDE411                            
085000     MOVE ZERO                 TO  ORAD-ADLEVPL                           
085100     MOVE ORAD-ADLAGOMR        IN  ORAD-WDQ401                            
085200                               TO  ORAD-ADLAGOMR                          
085300                               IN  ORAD-WDE411                            
085400     MOVE ORAD-BERADREF        IN  ORAD-WDQ401                            
085500                               TO  ORAD-BERADREF                          
085600                               IN  ORAD-WDE411                            
085700     MOVE JA                   TO  ORAD-FLDIRLEV                          
085800     MOVE ORAD-KDORDING        IN  ORAD-WDQ401                            
085900                               TO  ORAD-KDORDING                          
086000                               IN  ORAD-WDE411                            
086100     MOVE ORAD-KDOI            IN  ORAD-WDQ401                            
086200                               TO  ORAD-KDOI                              
086300                               IN  ORAD-WDE411                            
086400     MOVE ORAD-CLEARGROUP      IN  ORAD-WDQ401                            
086500                               TO  ORAD-CLEARGROUP                        
086600                               IN  ORAD-WDE411                            
086700     MOVE ORAD-FLRESTN         IN  ORAD-WDQ401                            
086800                               TO  ORAD-FLRESTN                           
086900                               IN  ORAD-WDE411                            
087000     MOVE ORAD-IDLEVNR         IN  ORAD-WDQ401                            
087100                               TO  ORAD-IDLEVNR                           
087200                               IN  ORAD-WDE411                            
087300     MOVE ODEL-IDPRODNR        TO  ORAD-IDPRODNR                          
087400     MOVE ORAD-KDVRINFO        IN  ORAD-WDQ401                            
087500                               TO  ORAD-KDVRINFO                          
087600                               IN  ORAD-WDE411                            
087700     MOVE SPACE                TO  ORAD-IDKUNDRF-RO                       
087800                               IN  ORAD-WDE411                            
087900     MOVE ORAD-IDKUNDRF-RO     IN  ORAD-WDQ401 (3:5)                      
088000                               TO  ORAD-IDKUNDRF-RO                       
088100                               IN  ORAD-WDE411                            
088200     MOVE ORAD-KDARTURS        IN  ORAD-WDQ401                            
088300                               TO  ORAD-KDARTURS                          
088400                               IN  ORAD-WDE411                            
088500     MOVE ORAD-KDARTURS        IN  ORAD-WDQ401                            
088600                               TO  ORAD-KDARTURS                          
088700                               IN  ORAD-WDE411                            
088800     MOVE ORAD-KDDSP           IN  ORAD-WDQ401                            
088900                               TO  ORAD-KDDSP                             
089000                               IN  ORAD-WDE411                            
089100     MOVE ORAD-KDFARLIG        IN  ORAD-WDQ401                            
089200                               TO  ORAD-KDFARLIG                          
089300                               IN  ORAD-WDE411                            
089400     IF SOFT AND DIST88-SW-FREIGHTCODE                                    
089500       MOVE +88                TO  ORAD-KDFRAKT                           
089600                               IN  ORAD-WDE411                            
089700     ELSE                                                                 
089800       MOVE ARB-KDFRAKT        TO  ORAD-KDFRAKT                           
089900                               IN  ORAD-WDE411                            
090000     END-IF                                                               
090100     MOVE ORAD-KDKVBRYT        IN  ORAD-WDQ401                            
090200                               TO  ORAD-KDKVBRYT                          
090300                               IN  ORAD-WDE411                            
090400     MOVE ZERO                 TO  ORAD-KDOFFERT                          
090500     MOVE ORAD-KDORDKL         IN  ORAD-WDQ401                            
090600                               TO  ORAD-KDORDKL                           
090700                               IN  ORAD-WDE411                            
090800     MOVE ZERO                 TO  ORAD-KDORDTYP                          
090900     MOVE ZERO                 TO  ORAD-KDQPACK                           
091000     MOVE ORAD-KDPRODSL        IN  ORAD-WDQ401                            
091100                               TO  ORAD-KDPRODSL                          
091200                               IN  ORAD-WDE411                            
091300     MOVE 3                    TO  ORAD-KDRADSTA                          
091400                               IN  ORAD-WDE411                            
091500     MOVE ZERO                 TO  ORAD-KVANNANT                          
091600     MOVE ORAD-KVBEART-Q       TO  ORAD-KVAVBART                          
091700     MOVE ORAD-KVBEART-Q       TO  ORAD-KVBEART                           
091800                               IN  ORAD-WDE411                            
091900     MOVE ZERO                 TO  ORAD-KVFLAMP                           
092000                                   ORAD-KVLEVART                          
092100     MOVE ORAD-KVSLATT         IN  ORAD-WDQ401                            
092200                               TO  ORAD-KVSLATT                           
092300                               IN  ORAD-WDE411                            
092400     MOVE ORAD-PRARTNTO        IN  ORAD-WDQ401                            
092500                               TO  ORAD-PRARTNTO                          
092600                               IN  ORAD-WDE411                            
092700     MOVE ZERO                 TO  ORAD-PRARTULL                          
092800     MOVE ORAD-TIPRIS          IN  ORAD-WDQ401                            
092900                               TO  ORAD-TIPRIS                            
093000                               IN  ORAD-WDE411                            
093100     MOVE ORAD-TIRODAT         IN  ORAD-WDQ401                            
093200                               TO  ORAD-TIRODAT                           
093300                               IN  ORAD-WDE411                            
093400     MOVE WS-DAGENS-DATUM      TO  ORAD-TIUTSKR                           
093500     COMPUTE ORAD-VKARTNTO     =   ORAD-VKART / 1000                      
093600     IF ORAD-VKART-NTO NUMERIC                                            
094000     COMPUTE ORAD-VKART-NTO-KG =   ORAD-VKART-NTO / 1000                  
095000     ELSE                                                                 
096000      MOVE ZERO  TO        ORAD-VKART-NTO-KG                              
096100     END-IF                                                               
096200     COMPUTE ORAD-VLARTNTO     IN  ORAD-WDE411                            
096300                               =   ORAD-VLARTNTO                          
096400                               IN  ORAD-WDQ401 / 1000000                  
096500     MOVE NEJ                  TO  ORAD-FLFYSAVV                          
096600     MOVE W-BEART              TO  ORAD-BEART                             
096700     MOVE ORAD-BEVOLREF        IN  ORAD-WDQ401                            
096800                               TO  ORAD-BEVOLREF                          
096900                               IN  ORAD-WDE411                            
097000     MOVE ORAD-FLINVEST        IN  ORAD-WDQ401                            
097100                               TO  ORAD-FLINVEST                          
097200                               IN  ORAD-WDE411                            
097300     MOVE ORAD-FLPRTILL        IN  ORAD-WDQ401                            
097400                               TO  ORAD-FLPRTILL                          
097500                               IN  ORAD-WDE411                            
097600     MOVE ORAD-FLTILLK         IN  ORAD-WDQ401                            
097700                               TO  ORAD-FLTILLK                           
097800                               IN  ORAD-WDE411                            
097900     MOVE ORAD-FLSDCLEV        IN  ORAD-WDQ401                            
098000                               TO  ORAD-FLSDCLEV                          
098100                               IN  ORAD-WDE411                            
098200     MOVE ORAD-IDLOPNR         IN  ORAD-WDQ401                            
098300                               TO  ORAD-IDLOPNR-RO                        
098400                               IN  ORAD-WDE411                            
098500     MOVE ORAD-IDSYSTEM        IN  ORAD-WDQ401                            
098600                               TO  ORAD-IDSYSTEM                          
098700                               IN  ORAD-WDE411                            
098800     MOVE ORAD-KDPRTYP         IN  ORAD-WDQ401                            
098900                               TO  ORAD-KDPRTYP                           
099000                               IN  ORAD-WDE411                            
099100     MOVE NEJ                  TO  ORAD-FLNOLLJ                           
099200                               IN  ORAD-WDE411                            
099300     MOVE ORAD-IDKAMPRF        IN  ORAD-WDQ401                            
099400                               TO  ORAD-IDKAMPRF                          
099500                               IN  ORAD-WDE411                            
099600     MOVE OHUV-IDANALYS        TO  ORAD-IDANALYS                          
099700                               IN  ORAD-WDE411                            
099800     MOVE ZERO                 TO  ORAD-KDANNULL                          
099900                                   ORAD-TISLULEV                          
100000     MOVE ORAD-DEAL-PR-LINE    IN  ORAD-WDQ401                            
100100                               TO  ORAD-DEAL-PR-LINE                      
100200                               IN  ORAD-WDE411                            
100300                                                                          
100400*    LDCN GK START *                                                      
100500     MOVE ORAD-IDKUNDRF-WIP    IN  ORAD-WDQ401                            
100600                               TO  ORAD-IDKUNDRF-WIP                      
100700                               IN  ORAD-WDE411                            
100800*    LDCN GK END   *                                                      
100900                                                                          
101000     MOVE ORAD-PRAVCOST        IN  ORAD-WDQ401                            
101100                               TO  ORAD-PRAVCOST                          
101200                               IN  ORAD-WDE411                            
101300     IF ORAD-PRAVCOST IN ORAD-WDQ401 > ZERO                               
101400        MOVE ORAD-KDVALISO IN ORAD-WDQ401                                 
101500                               TO  ORAD-KDVALISO-EXP                      
101600        MOVE 'SEK'             TO  ORAD-KDVALISO                          
101700                               IN  ORAD-WDE411                            
101800     ELSE                                                                 
101900        MOVE SPACE             TO  ORAD-KDVALISO-EXP                      
102000        MOVE ORAD-KDVALISO IN ORAD-WDQ401                                 
102100                               TO  ORAD-KDVALISO                          
102200                               IN  ORAD-WDE411                            
102300     END-IF                                                               
102400     PERFORM CBFDB-JUSTERA-FTGKOD                                         
102500     PERFORM CBFDC-JUSTERA-KONTO                                          
102600     PERFORM CBFDD-JUSTERA-FARLIGT-GODS                                   
102700     MOVE W-IDKONTO (2:1)      TO  ORAD-KDFTG                             
102800                                                                          
102900     MOVE +20                  TO  ORAD-KDTVA                             
103000     PERFORM IMS-ISRT-WDE411                                              
103100                                                                          
103200     IF SOFT                                                              
103300       PERFORM CBFDE-PIE-TRANS                                            
103400     END-IF                                                               
103500     .                                                                    
103600                                                                          
103700                                                                          
103800 CBFDB-JUSTERA-FTGKOD SECTION.                                            
103900     MOVE 'CBFDB-JUST-FTGKO' TO CURRENT-SECTION                           
104000                                                                          
104100     MOVE ZERO TO W-IDFTG                                                 
104200                                                                          
104300     MOVE 1 TO IX1                                                        
104400     PERFORM UNTIL IX1 > 14                                               
104500        IF WS-TAB-IDFTG-2POS (IX1) = OHUV-IDFTG                           
104600           MOVE WS-TAB-IDFTG-1POS (IX1) TO W-IDFTG                        
104700           MOVE 99                      TO IX1                            
104800        ELSE                                                              
104900           ADD 1                        TO IX1                            
105000        END-IF                                                            
105100     END-PERFORM                                                          
105200     .                                                                    
105300                                                                          
105400                                                                          
105500 CBFDC-JUSTERA-KONTO SECTION.                                             
105600     MOVE 'CBFDC-JUST-KONTO' TO CURRENT-SECTION                           
105700                                                                          
105800     MOVE OHUV-IDKONTO         TO W-IDKONTO                               
105900     MOVE OHUV-IDKST           TO ORAD-IDKST                              
106000                               IN ORAD-WDE411                             
106100                                                                          
106200     MOVE W-IDFTG              TO W-IDKONTO (1:2)                         
106300     MOVE W-IDKONTO            TO ORAD-IDKONTO                            
106400                               IN ORAD-WDE411                             
106500     .                                                                    
106600                                                                          
106700                                                                          
106800 CBFDD-JUSTERA-FARLIGT-GODS SECTION.                                      
106900     MOVE 'CBFDD-FARLIGT-GO' TO CURRENT-SECTION                           
107000                                                                          
107100     PERFORM IMS-GU-ARTC11-CLAG                                           
107200     IF ARTC11-CLAG-IDPSN > 0                                             
107300        MOVE ARTC11-CLAG-IDPSN     TO  ORAD-IDPSN                         
107400        PERFORM IMS-GU-ARTN01                                             
107500        IF SEGMENT-FINNS                                                  
107600           MOVE ART-VKART-FG    TO  ORAD-VKART-FG                         
107700           MOVE ART-VLFG        TO  ORAD-VLFG                             
107800           MOVE ART-SUEQFG      TO  ORAD-SUEQFG                           
107900        ELSE                                                              
108000           MOVE ZERO            TO ORAD-IDPSN                             
108100                                   ORAD-VKART-FG                          
108200                                   ORAD-VLFG                              
108300                                   ORAD-SUEQFG                            
108400        END-IF                                                            
108500     ELSE                                                                 
108600        MOVE ZERO               TO ORAD-IDPSN                             
108700                                   ORAD-VKART-FG                          
108800                                   ORAD-VLFG                              
108900                                   ORAD-SUEQFG                            
109000     END-IF                                                               
109100     .                                                                    
109200                                                                          
109300 CBFDE-PIE-TRANS SECTION.                                                 
109400     MOVE 'CBFDE-PIE-TRANS ' TO CURRENT-SECTION                           
109500                                                                          
109600     COMPUTE 4690-KVLL = LENGTH OF 4690-MID-W4I69001 + 17                 
109700                                                                          
109800     MOVE 'W4T690X '                  TO 4690-TRANSKOD                    
109900     MOVE '4695'                      TO 4690-IDTRANS                     
110000     MOVE WS-IDDISTR                  TO 4690-MID-IDDISTR                 
110100     MOVE WS-IDKUNDNR                 TO 4690-MID-IDKUNDNR                
110200     MOVE WS-IDORDNR5                 TO 4690-MID-IDORDNR5                
110300     MOVE WS-IDPRODNR                 TO 4690-MID-IDPRODNR                
110400     MOVE ORAD-IDARTNR                IN ORAD-WDE411                      
110500                                      TO 4690-MID-IDARTNR                 
110600     MOVE ORAD-IDPURAD                IN ORAD-WDE411                      
110700                                      TO 4690-MID-IDPURAD                 
110800     MOVE ORAD-KVBEART                IN ORAD-WDE411                      
110900                                      TO 4690-MID-KVBEART                 
111000     MOVE ORAD-IDBIL                  IN ORAD-WDE411                      
111100                                      TO 4690-MID-IDBIL                   
111200                                                                          
111300     PERFORM IMS-PURG-ALTMSG                                              
111400     .                                                                    
111500                                                                          
111600                                                                          
111700 CBFE-SKAPA-WDF611-SEGMENT       SECTION.                                 
111800     MOVE 'CBFE-SK-WDF611  ' TO CURRENT-SECTION                           
111900                                                                          
112000     MOVE W-KVRADER            TO PUDR-IDPURAD                            
112100                                                                          
112200     IF DIST35-REFILL-NA      OR                                          
112300        DIST35-REFILL-CN      OR                                          
112400        DIST35-CDC-IN-REFILL  OR                                          
112500        DIST35-CDC-KR-REFILL  OR                                          
112600        DIST35-CDC-AE-REFILL  OR                                          
112700        DIST35-CDC-TR-REFILL  OR                                          
112800        DIST35-CDC-MY-REFILL  OR                                          
112810        DIST35-CDC-MX-REFILL  OR                                          
112820        DIST35-CDC-BR-REFILL                                              
112900       SEARCH ALL DIST57-REFILL-DC                                        
113000         AT END                                                           
113100           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
113200                               TO FELTEXT                                 
113300           CALL FELLOG                                                    
113400         WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR                
113500           MOVE DIST57-REFILL-TO-DC(DIST57-IX)                            
113600                               TO W-IDDC-ARTS11                           
113700       END-SEARCH                                                         
113800       PERFORM IMS-GU-ARTS11                                              
113900       MOVE SLAG-ADLAGOMR      TO PUDR-ADLAGOMR                           
114000       MOVE SLAG-ADGANG        TO PUDR-ADGANG                             
114100       MOVE SLAG-ADPLATS       TO PUDR-ADPLATS                            
114200     ELSE                                                                 
114300       MOVE ORAD-ADLAGOMR      IN ORAD-WDQ401                             
114400                               TO PUDR-ADLAGOMR                           
114500       MOVE ORAD-ADGANG        IN ORAD-WDQ401                             
114600                               TO PUDR-ADGANG                             
114700       MOVE ORAD-ADPLATS       IN ORAD-WDQ401                             
114800                               TO PUDR-ADPLATS                            
114900     END-IF                                                               
115000     MOVE W-BEART              TO PUDR-BEART                              
115100     MOVE ORAD-BERADREF        IN ORAD-WDQ401                             
115200                               TO PUDR-BERADREF                           
115300     MOVE ORAD-IDARTNR         IN ORAD-WDQ401                             
115400                               TO PUDR-IDARTNR                            
115500     MOVE ORAD-KDARTURS        IN ORAD-WDQ401                             
115600                               TO PUDR-KDARTURS                           
115700     MOVE ORAD-KVBEART-Q       IN ORAD-WDQ401                             
115800                               TO PUDR-KVBEART                            
115900     MOVE ORAD-PRARTNTO        IN ORAD-WDQ401                             
116000                               TO PUDR-PRARTNTO                           
116100                                                                          
116200     MOVE ORAD-IDDISTR         TO TEST-IDDISTR                            
116300     MOVE ORAD-DEAL-PR-LINE    IN ORAD-WDQ401                             
116400                               TO PUDR-DEAL-PR-LINE                       
116500     PERFORM IMS-ISRT-WDF611                                              
116600     .                                                                    
116700                                                                          
116800 CBFG-WRITE-WDQ101           SECTION.                                     
116900     MOVE 'CBFG-WRITE-WDQ101'    TO CURRENT-SECTION                       
117000                                                                          
117100     MOVE KORD-IDORDER           TO OBKR-IDORDER                          
117200     MOVE ORAD-IDARTNR           IN ORAD-WDQ401                           
117300                                 TO OBKR-IDARTNR                          
117400     MOVE 1                      TO OBKR-IDLOPNR                          
117500     MOVE 1                      TO OBKR-IDSEKVNR                         
117600     MOVE KORD-IDDC              TO OBKR-IDDC                             
117700     MOVE 97                     TO OBKR-KDORDBEK                         
117800     MOVE SPACE                  TO OBKR-BEERS                            
117900     MOVE OHUV-BEKUNDRF          TO OBKR-BEKUNDRF                         
118000     MOVE ORAD-BERADREF          IN ORAD-WDQ401                           
118100                                 TO OBKR-BERADREF                         
118200     MOVE ORAD-BEVOLREF          IN ORAD-WDQ401                           
118300                                 TO OBKR-BEVOLREF                         
118400     MOVE ORAD-IDKAMPRF          IN ORAD-WDQ401                           
118500                                 TO OBKR-IDKAMPRF                         
118600     MOVE 0                      TO OBKR-DIERS-KVOT                       
118700     MOVE NEJ                    TO OBKR-FLAKPLOC                         
118800     MOVE NEJ                    TO OBKR-FLSLATT                          
118900     MOVE ORAD-FLINVEST          IN ORAD-WDQ401                           
119000                                 TO OBKR-FLINVEST                         
119100     MOVE JA                     TO OBKR-FLOBOK                           
119200     MOVE NEJ                    TO OBKR-FLOBTRAN                         
119300     MOVE NEJ                    TO OBKR-FLOBPRT                          
119400     MOVE ORAD-FLPRTILL          IN ORAD-WDQ401                           
119500                                 TO OBKR-FLPRTILL                         
119600     MOVE ORAD-FLRESTN           IN ORAD-WDQ401                           
119700                                 TO OBKR-FLRESTN                          
119800     MOVE NEJ                    TO OBKR-FLTILLK                          
119900     MOVE 0                      TO OBKR-IDARTNR-TILLK                    
120000     MOVE ORAD-IDDC-RO           IN ORAD-WDQ401                           
120100                                 TO OBKR-IDDC-RO                          
120200     MOVE KORD-IDDISTR           TO OBKR-IDDISTR                          
120300     MOVE KORD-IDKUNDNR          TO OBKR-IDKUNDNR                         
120400     MOVE OHUV-IDKUNDRF          TO OBKR-IDKUNDRF                         
120500                                                                          
120600     MOVE ORAD-IDKUNDRF-RO       IN ORAD-WDQ401                           
120700                                 TO OBKR-IDKUNDRF-RO                      
120800                                                                          
120900     MOVE ORAD-IDLEVNR           IN ORAD-WDQ401                           
121000                                 TO OBKR-IDLEVNR                          
121100     MOVE ORAD-IDLOPNR-RO        IN ORAD-WDQ401                           
121200                                 TO OBKR-IDLOPNR-RO                       
121300     MOVE IDPGM                  TO OBKR-IDPGM                            
121400     MOVE ORAD-IDSYSTEM          IN ORAD-WDQ401                           
121500                                 TO OBKR-IDSYSTEM                         
121600     MOVE ORAD-KDDSP             IN ORAD-WDQ401                           
121700                                 TO OBKR-KDDSP                            
121800     MOVE 0                      TO OBKR-KDERS                            
121900     MOVE ORAD-KDOI              IN ORAD-WDQ401                           
122000                                 TO OBKR-KDOI                             
122100     MOVE ORAD-CLEARGROUP        IN ORAD-WDQ401                           
122200                                 TO OBKR-CLEARGROUP                       
122300     MOVE ORAD-KDKVBRYT          IN ORAD-WDQ401                           
122400                                 TO OBKR-KDKVBRYT                         
122500     MOVE ORAD-KDPRTYP           IN ORAD-WDQ401                           
122600                                 TO OBKR-KDPRTYP                          
122700     MOVE 0                      TO OBKR-KDTPOTYP                         
122800     MOVE ORAD-KDVRINFO          IN ORAD-WDQ401                           
122900                                 TO OBKR-KDVRINFO                         
123000                                                                          
123100     MOVE ZERO                   TO OBKR-KVANNANT                         
123200     MOVE ORAD-KVAVBART          TO OBKR-KVAVBART                         
123300     MOVE ORAD-KVBEART           IN ORAD-WDQ401                           
123400                                 TO OBKR-KVBEART                          
123500                                    OBKR-KVBEART-Q                        
123600     MOVE 0                      TO OBKR-KVBEART-TILLK                    
123700     MOVE 0                      TO OBKR-KVPREAVB                         
123800     MOVE 0                      TO OBKR-KVPRERO                          
123900     MOVE ZERO                   TO OBKR-KVQPACK                          
124000     MOVE 0                      TO OBKR-KVRO                             
124100     MOVE ZERO                   TO OBKR-TIRODAT                          
124200     MOVE ORAD-KVSLATT           IN ORAD-WDQ401                           
124300                                 TO OBKR-KVSLATT                          
124400     MOVE ORAD-PRARTNTO          IN ORAD-WDQ401                           
124500                                 TO OBKR-PRARTNTO                         
124600     MOVE ORAD-DEAL-PR-LINE      IN ORAD-WDQ401                           
124700                                 TO OBKR-DEAL-PR-LINE                     
124800     MOVE 0                      TO OBKR-PRBPRIS                          
124900     MOVE ORAD-REKSIFFR          IN ORAD-WDQ401                           
125000                                 TO OBKR-REKSIFFR                         
125100     MOVE 0                      TO OBKR-REKSIFFR-TILLK                   
125200     MOVE 0                      TO OBKR-RERF-RAD                         
125300     MOVE +0                     TO OBKR-TIDISPIN                         
125400     MOVE KORD-TIORDREG          TO OBKR-TIORDREG                         
125500                                    WS-DATUM-9KOMPL                       
125600     MOVE FUNCTION CURRENT-DATE (1:2)                                     
125700                                 TO WS-DATUM-9KOMPL (1:2)                 
125800     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
125900     END-COMPUTE                                                          
126000     MOVE ORAD-TIPRIS            IN ORAD-WDQ401                           
126100                                 TO OBKR-TIPRIS                           
126200                                                                          
126300     MOVE WS-DAGENS-DATUM        TO OBKR-TIREGDAT                         
126400     MOVE WS-TID (1:6)           TO OBKR-TIREGTID                         
126500     MOVE KORD-TIORDREG          TO WS-DATUM-9KOMPL                       
126600     MOVE FUNCTION CURRENT-DATE (1:2)                                     
126700                                 TO WS-DATUM-9KOMPL (1:2)                 
126800                                                                          
126900     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
127000     END-COMPUTE                                                          
127100     MOVE 0                      TO OBKR-TITPO                            
127200     MOVE ORAD-KDFRAKT           TO OBKR-KDFRAKT                          
127300     MOVE ORAD-KDORDKL           IN ORAD-WDQ401                           
127400                                 TO OBKR-KDORDKL                          
127500     MOVE ORAD-IDBIL             IN ORAD-WDQ401                           
127600                                 TO OBKR-IDBIL                            
127700                                                                          
127800     MOVE OHUV-KDORDTYP-LDC      TO OBKR-KDORDTYP-LDC                     
127900     MOVE WS-NEW-TIREPDAT        TO OBKR-TIREPDAT                         
128000     MOVE ORAD-IDKUNDRF-WIP      IN ORAD-WDQ401                           
128100                                 TO OBKR-IDKUNDRF-WIP                     
128200     MOVE WS-NEW-TIREPDAT        TO OBKR-TIDLEVDAT                        
128300     MOVE ORAD-PRAVCOST          IN ORAD-WDQ401                           
128400                                 TO OBKR-PRAVCOST                         
128500                                                                          
128600     PERFORM IMS-ISRT-WDQ101                                              
128700     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
128800        ADD +1                            TO OBKR-IDLOPNR                 
128900        PERFORM IMS-ISRT-WDQ101                                           
129000     END-PERFORM                                                          
129100                                                                          
129200     .                                                                    
129300                                                                          
129400 CBFH-SEND-MAIL SECTION.                                                  
129500     MOVE 'CBFH-SEND-MAIL  '     TO CURRENT-SECTION                       
129600                                                                          
129700     IF WS-DAP-HDR-SW = NEJ                                               
129800       PERFORM S21-SEND-OPEN-MAIL                                         
129900       PERFORM S22-PUT-HEADER-MAIL                                        
130000       MOVE WS-HEADING-1       TO WS-REPORT                               
130100       PERFORM S25-PUT-LINE-MAIL                                          
130200       MOVE ALL '-'            TO WS-REPORT                               
130300       PERFORM S25-PUT-LINE-MAIL                                          
130400       MOVE WS-HEADING-2       TO WS-REPORT                               
130500       PERFORM S25-PUT-LINE-MAIL                                          
130600       MOVE ALL '-'            TO WS-REPORT                               
130700       PERFORM S25-PUT-LINE-MAIL                                          
130800       MOVE JA                 TO WS-DAP-HDR-SW                           
130900       IF OBKR-IDSYSTEM = ('LDC ' OR 'TACD') AND                          
131000          TACDIS-POC                                                      
131100*      WE ALSO NEED TO OPEN FOR TACDIS                                    
131200          PERFORM S31-SEND-OPEN-TACD                                      
131300          PERFORM S32-PUT-HEADER-TACD                                     
131400       END-IF                                                             
131500     END-IF                                                               
131600                                                                          
131700     MOVE OBKR-IDDISTR         TO WS-LINE1-IDDISTR                        
131800     MOVE OBKR-IDKUNDNR        TO WS-LINE1-IDKUNDNR                       
131900     MOVE OBKR-IDORDNR7        TO WS-LINE1-IDORDNR7                       
132000     MOVE OBKR-IDARTNR         TO WS-LINE1-IDARTNR                        
132100     MOVE OHUV-TIREPDAT        TO WS-LINE1-TIREPDAT                       
132200     MOVE OBKR-TIREPDAT        TO WS-LINE1-TIREPDAT-NEW                   
132300     MOVE WS-LINE1             TO WS-REPORT                               
132400     PERFORM S25-PUT-LINE-MAIL                                            
132500     .                                                                    
132600                                                                          
132700 CBFI-SKAPA-TACD-402          SECTION.                                    
132800                                                                          
132900     MOVE 'PU1'                   TO 402-IDPTYP                           
133000     MOVE 01                      TO 402-IDVTYP-TACDIS                    
133100     MOVE 20                      TO WS-TIAA                              
133200     MOVE WS-DAGENS-DATUM         TO WS-TIAAMMDD                          
133300     MOVE WS-TIAAAAMMDD           TO 402-DAREGDAT                         
133400     MOVE OBKR-IDDISTR            TO 402-IDDISTR                          
133500     MOVE OBKR-IDKUNDNR           TO 402-IDKUNDNR                         
133600     MOVE OBKR-IDORDNR7           TO 402-IDORDNR7                         
133700                                                                          
133800     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
133900     MOVE OBKR-IDARTNR            TO CIA-IDARTBET-IN                      
134000     CALL W009CIA              USING CIA-W009CIA                          
134100     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET                         
134200                                                                          
134300     MOVE OBKR-KDORDBEK           TO 402-KDORDBEK                         
134400     MOVE OBKR-KVBEART            TO 402-KVBEART                          
134500     MOVE 1                       TO 402-IDSEKVNR                         
134600                                                                          
134700     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
134800     MOVE OBKR-IDARTNR-TILLK      TO CIA-IDARTBET-IN                      
134900     CALL W009CIA              USING CIA-W009CIA                          
135000     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET-TILLK                   
135100                                                                          
135200     MOVE OBKR-KVBEART-Q          TO 402-KVLEVART                         
135300     MOVE OBKR-IDDC               TO 402-IDDC                             
135400     MOVE OBKR-TIDLEVDAT          TO 402-DADLEVDAT                        
135500     IF 402-DADLEVDAT > ZERO                                              
135600        ADD 20000000              TO 402-DADLEVDAT                        
135700     END-IF                                                               
135800                                                                          
135900     PERFORM S35-PUT-LINE-TACD                                            
136000     .                                                                    
136100     EJECT                                                                
136200 CBG-SKRIV-WDE601             SECTION.                                    
136300     MOVE 'CBG-SKRIV-WDE601' TO CURRENT-SECTION                           
136400                                                                          
136500     MOVE ODEL-IDPRODNR        TO  VORD-IDPRODNR                          
136600     MOVE OHUV-IDDISTR         TO  VORD-IDDISTR                           
136700     MOVE OHUV-IDKUNDNR        TO  VORD-IDKUNDNR                          
136800     MOVE OHUV-KDORDKL         TO  VORD-KDORDKL                           
136900     MOVE ZERO                 TO  VORD-IDLOTNR                           
137000     MOVE SPACE                TO  VORD-KDORDLOT                          
137100     MOVE ZERO                 TO  VORD-ADLEVPL                           
137200                                   VORD-KDPERSON                          
137300     MOVE NEJ                  TO  VORD-FLCONTL                           
137400     MOVE JA                   TO  VORD-FLDIRLEV                          
137500     MOVE NEJ                  TO  VORD-FLSPARR                           
137600     MOVE NEJ                  TO  VORD-FLMANORD                          
137700     MOVE ODEL-IDDC            TO  VORD-IDDC                              
137800     MOVE ODEL-IDDC-EXP        TO  VORD-IDDC-EXP                          
137900     MOVE OHUV-KDFAKTYP        TO  VORD-KDFAKTYP                          
138000     IF SOFT AND DIST88-SW-FREIGHTCODE                                    
138100       MOVE +88                TO  VORD-KDFRAKT                           
138200     ELSE                                                                 
138300       MOVE ARB-KDFRAKT        TO  VORD-KDFRAKT                           
138400     END-IF                                                               
138500     MOVE ARB-BEGMRK           TO  VORD-BEGMRK                            
138600     MOVE ZERO                 TO  VORD-KDMETOD                           
138700     MOVE 1                    TO  VORD-KDORDSTA                          
138800     MOVE ZERO                 TO  VORD-KVKOLLI                           
138900                                   VORD-KVKOLLI-FAKT                      
139000                                   VORD-KVKOLLI-LAST                      
139100                                   VORD-KVKOLLI-FL                        
139200                                   VORD-KVKOLPAC                          
139300                                   VORD-KVORDRAD-PACK                     
139400     MOVE DIRL-KVRADER         TO  VORD-KVORDRAD                          
139500     MOVE DIRL-SUORDV          TO  VORD-SUORDV                            
139600     MOVE DIRL-SUORDV-LOC      TO  VORD-SUORDV-LOC                        
139700     MOVE DIRL-SUORDV-LOCPREL  TO  VORD-SUORDV-LOCPREL                    
139800     MOVE ZERO                 TO  VORD-SUORDV-PACK                       
139900                                   VORD-SUORDV-PACK-LOC                   
140000                                   VORD-SUORDV-PACK-LOCPREL               
140100                                   VORD-SUORDV-FL                         
140200                                   VORD-SUORDV-FL-LOC                     
140300                                   VORD-SUORDV-FL-LOCPREL                 
140400     MOVE OHUV-TIREGTID        TO  VORD-TIREGTID                          
140500     MOVE ARB-TIRFS            TO  W-TIRFS                                
140600     MOVE W-TIRFS (2:6)        TO  VORD-DABEGPAC                          
140700     IF W-TIRFS (2:6) NOT = ZERO                                          
140800       IF W-TIRFS (2:6) < 500000                                          
140900         MOVE 20               TO  VORD-DABEGPAC (1:2)                    
141000       ELSE                                                               
141100         IF W-TIRFS (2:6) < 999999                                        
141200           MOVE 19             TO  VORD-DABEGPAC (1:2)                    
141300         ELSE                                                             
141400           MOVE 99999999       TO  VORD-DABEGPAC                          
141500         END-IF                                                           
141600       END-IF                                                             
141700     END-IF                                                               
141800     MOVE ZERO                 TO  VORD-TIFAKT-SK                         
141900                                   VORD-TILASTN-SK                        
142000                                   VORD-TIPACKN-SK                        
142100     MOVE WS-DAGENS-DATUM      TO  VORD-TIUTSKR                           
142200     MOVE WS-TID (1:6)         TO  VORD-TIUTSTID                          
142300     MOVE ZERO                 TO  VORD-VKORDBTO                          
142400                                   VORD-VKORDBTO-FL                       
142500     MOVE DIRL-VKORDNTO        TO  VORD-VKORDNTO                          
142600     MOVE ZERO                 TO  VORD-VLORDBTO                          
142700                                   VORD-VLORDBTO-FL                       
142800     MOVE DIRL-VLORDNTO        TO  VORD-VLORDNTO                          
142900     MOVE OHUV-FLAUTFAK        TO  VORD-FLAUTFAK                          
143000     MOVE NEJ                  TO  VORD-FLFRAKTS                          
143100     MOVE ZERO                 TO  VORD-IDPRODNR-SAMP                     
143200     MOVE ODEL-DARFS           TO  VORD-DARFS                             
143300     MOVE DIRL-IDLEVNR         TO  VORD-IDLEVNR                           
143400     MOVE DIRL-KDVIA           TO  VORD-KDVIA                             
143500     MOVE ZERO                 TO  VORD-SUORDV-EXP                        
143600     MOVE SPACE                TO  VORD-KDVALISO-EXP                      
143700                                                                          
143800     PERFORM IMS-ISRT-WDE601                                              
143900     .                                                                    
144000                                                                          
144100                                                                          
144200 CBI-SKAPA-WDF601-SEGMENT   SECTION.                                      
144300     MOVE 'CBI-SKAPA-WDF601' TO CURRENT-SECTION                           
144400                                                                          
144500     MOVE ODEL-IDPRODNR        TO  W-IDPRODNR                             
144600                                   PUDH-IDPRODNR                          
144700                                                                          
144800     PERFORM CBIA-JUSTERA-TISKEPPN-DDGS                                   
144900     MOVE WS-DASKEPPN          TO  PUDH-DASKEPPN                          
145000     MOVE WS-DASNDDAT          TO  PUDH-DASNDDAT                          
145100     MOVE ZERO                 TO  PUDH-DAUTSKR                           
145200     MOVE DIRL-IDDC            TO  PUDH-IDDC                              
145300     MOVE OHUV-IDDEPOT         TO  PUDH-IDDEPOT                           
145400     MOVE OHUV-IDDISTR         TO  PUDH-IDDISTR                           
145500     MOVE OHUV-IDKUNDNR        TO  PUDH-IDKUNDNR                          
145600     MOVE DIRL-IDLEVNR         TO  PUDH-IDLEVNR                           
145700     MOVE ODEL-IDORDNR7        TO  PUDH-IDORDNR7                          
145800     MOVE OHUV-IDORDER         TO  PUDH-IDORDER                           
145900     MOVE OHUV-IDROUTE         TO  PUDH-IDROUTE                           
146000     MOVE DCS-IDVAT            TO  PUDH-IDVAT                             
146100     MOVE OHUV-IDZON           TO  PUDH-IDZON                             
146200     MOVE ARB-KDFRAKT          TO  PUDH-KDFRAKT                           
146300     MOVE OHUV-KDORDKL         TO  PUDH-KDORDKL                           
146400     MOVE DIRL-KDVIA           TO  PUDH-KDVIA                             
146500     IF WS-NEW-TIREPDAT > ZERO                                            
146600       MOVE WS-NEW-TIREPDAT    TO  PUDH-TIREPDAT                          
146700     ELSE                                                                 
146800       MOVE OHUV-TIREPDAT      TO  PUDH-TIREPDAT                          
146900     END-IF                                                               
147000     MOVE ZERO                 TO  PUDH-TISNDTID                          
147100     MOVE OHUV-TITPO           TO  PUDH-TITPO                             
147200     MOVE OHUV-BELAGINS-DEL1   TO  PUDH-BELAGINS-DIR                      
147300     MOVE ARB-BEGMRK           TO  PUDH-BEGMRK                            
147400     MOVE OHUV-BEGMT           TO  PUDH-BEGMT                             
147500     MOVE OHUV-ADGMT           TO  PUDH-ADGMT                             
147600     MOVE DIRL-SUORDV-LOC      TO  PUDH-SUORDV-LOC                        
147700     MOVE DIRL-SUORDV-LOCPREL  TO  PUDH-SUORDV-LOCPREL                    
147800     MOVE DIRL-KDVALISO        TO  PUDH-KDVALISO                          
147900     PERFORM IMS-ISRT-WDF601                                              
148000     .                                                                    
148100                                                                          
148200                                                                          
148300 CBIA-JUSTERA-TISKEPPN-DDGS    SECTION.                                   
148400     MOVE 'CBIA-JUSTERA-DAT' TO CURRENT-SECTION                           
148500                                                                          
148600     MOVE WS-DAGENS-DATUM-Y2K       TO  WS-DASNDDAT                       
148700                                                                          
148800     IF OHUV-KDTPOTYP = +2                                                
148900     OR OHUV-TIREPDAT > ZERO                                              
149000        MOVE DIRL-IDLEVNR           TO  W-IDLEVNR-F1                      
149100        MOVE OHUV-IDDISTR           TO  W-IDDISTR-F1                      
149200        MOVE OHUV-IDKUNDNR          TO  W-IDKUNDNR-F1                     
149300        MOVE OHUV-KDORDKL           TO  W-KDORDKL-F1                      
149400        PERFORM IMS-GU-WDF118                                             
149500        IF SEGMENT-SAKNAS                                                 
149600           MOVE 999999              TO W-IDKUNDNR-F1                      
149700           PERFORM IMS-GU-WDF118                                          
149800        END-IF                                                            
149900        IF SEGMENT-SAKNAS                                                 
150000           MOVE 9999                TO W-IDDISTR-F1                       
150100           MOVE 999999              TO W-IDKUNDNR-F1                      
150200           PERFORM IMS-GU-WDF118                                          
150300        END-IF                                                            
150400                                                                          
150500        MOVE '11'                   TO WORK-IDDC                          
150600        MOVE 1                      TO WORK-KVWORKD                       
150700        IF OHUV-KDTPOTYP = +2                                             
150800           MOVE OHUV-TITPO          TO WORK-TIAAMMDD-TOM                  
150900        ELSE                                                              
151000           MOVE OHUV-TIREPDAT       TO WORK-TIAAMMDD-TOM                  
151100        END-IF                                                            
151200        MOVE 003                    TO WORK-KDCALL                        
151300        CALL WORKDAY    USING  WORK-KDCALL                                
151400                               WORK-DATE-AREA                             
151500                               WORK-KDSVAR                                
151600        END-CALL                                                          
151700        IF WORK-KDSVAR-FEL                                                
151800           MOVE ' FEL FRÅN WORKDAY (W4069500) 1' TO ERROR-TEXT            
151900           CALL ABEND USING RKOD-ABEND                                    
152000        ELSE                                                              
152100           MOVE DSTY-KVDAGAR-DIFF TO WORK-KVWORKD                         
152200           ADD 1                  TO WORK-KVWORKD                         
152300           MOVE WORK-TIAAMMDD-FOM TO WORK-TIAAMMDD-TOM                    
152400           MOVE 003               TO WORK-KDCALL                          
152500           CALL WORKDAY USING WORK-KDCALL                                 
152600                                WORK-DATE-AREA                            
152700                                WORK-KDSVAR                               
152800           END-CALL                                                       
152900           IF WORK-KDSVAR-FEL                                             
153000              MOVE ' FEL FRÅN WORKDAY (W4069500) 2'                       
153100                                     TO ERROR-TEXT                        
153200              CALL ABEND USING RKOD-ABEND                                 
153300           ELSE                                                           
153400              MOVE WORK-TIAAMMDD-FOM TO WS-TISKEPPN                       
153500                                        WORK-TIAAMMDD-TOM                 
153600              MOVE DSTY-KVDAGAR-DIFF TO DIRL-KVDAGAR-DIFF                 
153700              MOVE DSTY-KVDAGAR-TPO  TO WORK-KVWORKD                      
153800              ADD 1                  TO WORK-KVWORKD                      
153900              CALL WORKDAY USING WORK-KDCALL                              
154000                                 WORK-DATE-AREA                           
154100                                 WORK-KDSVAR                              
154200              END-CALL                                                    
154300              IF WORK-KDSVAR-FEL                                          
154400                 MOVE ' FEL FRÅN WORKDAY (W4069500) 3'                    
154500                                     TO ERROR-TEXT                        
154600                 CALL ABEND USING RKOD-ABEND                              
154700              ELSE                                                        
154800                 MOVE WORK-TIAAMMDD-FOM TO WS-TISNDDAT                    
154900              END-IF                                                      
155000           END-IF                                                         
155100        END-IF                                                            
155200     ELSE                                                                 
155300        MOVE DIRL-TISKEPPN-DDC TO WS-TISKEPPN                             
155400     END-IF                                                               
155500                                                                          
155600     PERFORM CBIAA-KOLLA-JUSTERA-DATUM                                    
155700     MOVE WS-TISKEPPN  TO DIRL-TISKEPPN-DDC                               
155800     .                                                                    
155900                                                                          
156000                                                                          
156100 CBIAA-KOLLA-JUSTERA-DATUM  SECTION.                                      
156200     MOVE 'CBIAA-KOLL-DATUM' TO CURRENT-SECTION                           
156300                                                                          
156400*    DET KAN HANDA ATT VI FÅR ETT SÄNDDATUM SOM REDAN ÄR PASSERAT         
156500*    SÅ KAN VI NATURLIGTVIS INTE HA DET                                   
156600*    I DET FALLET SÄTTER VI SÄNDDATUM TILL DAGENS DATUM                   
156700*    OCH RÄKNAR OM SKEPPNINGSDATUM MED UTGÅNGSPUNKT FRÅN DET              
156800                                                                          
156900     IF DIRL-TISKEPPN-DDC > WS-TISKEPPN                                   
157000        MOVE WS-DAGENS-DATUM-Y2K TO WS-DASNDDAT                           
157100        MOVE DIRL-TISKEPPN-DDC   TO WS-TISKEPPN                           
157200        IF OHUV-IDSYSTEM = 'LDC '                                         
157300          MOVE '11'              TO WORK-IDDC                             
157400          MOVE DSTY-KVDAGAR-DIFF TO WORK-KVWORKD                          
157500          ADD 1                  TO WORK-KVWORKD                          
157600          MOVE WS-TISKEPPN       TO WORK-TIAAMMDD-FOM                     
157700          MOVE 002               TO WORK-KDCALL                           
157800          CALL WORKDAY        USING WORK-KDCALL                           
157900                                    WORK-DATE-AREA                        
158000                                    WORK-KDSVAR                           
158100          END-CALL                                                        
158200          IF WORK-KDSVAR-FEL                                              
158300            MOVE ' FEL FRÅN WORKDAY (W4069500) 6'                         
158400                                      TO ERROR-TEXT                       
158500            CALL ABEND        USING RKOD-ABEND                            
158600          END-IF                                                          
158700          MOVE WORK-TIAAMMDD-TOM TO WS-NEW-TIREPDAT                       
158800        END-IF                                                            
158900     ELSE                                                                 
159000        IF WS-DAGENS-DATUM-Y2K > WS-DASNDDAT                              
159100           MOVE WS-DAGENS-DATUM-Y2K    TO WS-DASNDDAT                     
159200        END-IF                                                            
159300     END-IF                                                               
159400     .                                                                    
159500                                                                          
159600 CC-UPPDATERA-WDQ301           SECTION.                                   
159700     MOVE 'CC-UPD-WDQ301   ' TO CURRENT-SECTION                           
159800                                                                          
159900     MOVE 'U'                  TO  ODEL-KDODELSTA                         
160000     MOVE WS-DAGENS-DATUM-Y2K  TO  ODEL-DAUTSKR                           
160100     MOVE WS-TID (1:6)         TO  ODEL-TIUTSTID                          
160200     PERFORM IMS-REPL-ORQA01                                              
160300     .                                                                    
160400                                                                          
160500                                                                          
160600 CD-UPPDATERA-WDQ211           SECTION.                                   
160700     MOVE 'CD-UPD-WDQ211   ' TO CURRENT-SECTION                           
160800                                                                          
160900     MOVE ZERO                   TO  DIRL-KVRADER                         
161000                                     DIRL-VLORDNTO                        
161100                                     DIRL-VKORDNTO                        
161200     IF DCS-DDC                                                           
161300       MOVE 'U '                 TO  DIRL-KDORDSTA                        
161400     END-IF                                                               
161500                                                                          
161600     PERFORM IMS-REPL-ORQI11                                              
161700     .                                                                    
161800                                                                          
161900 CF-UPPDATERA-WDQ201           SECTION.                                   
162000     MOVE 'CF-UPD-WDQ201   ' TO CURRENT-SECTION                           
162100                                                                          
164400*    NÄR DET FINNS DIREKTLEVERANSER PÅ ORDERN KAN DEN                     
164500*    INTE LÄNGRE VARA AKTUELL FÖR TVINGANDE TILLÄGG                       
164600*    DÄRFÖR FLYTTAR VI NEJ TILL FLORDTIL                                  
164700*                                                                         
164800     PERFORM IMS-GHU-ORQI01                                               
164900     MOVE NEJ TO OHUV-FLORDTIL                                            
165000     MOVE +0  TO OHUV-KDTPOTYP                                            
165100     MOVE 0   TO OHUV-TITPO                                               
165300     PERFORM IMS-REPL-ORQI01                                              
230300     .                                                                    
231500                                                                          
231600 S21-SEND-OPEN-MAIL SECTION.                                              
231700     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
231800     MOVE 'OPEN'                  TO SEND-KDFUNC                          
231900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
232000                                     SEND-OPEN-AREA                       
232100     IF SEND-KDRC > ZERO                                                  
232200       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
232300       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
232400       DELIMITED BY SIZE INTO FELTEXT                                     
232500       DISPLAY FELTEXT                                                    
232600       CALL FELLOG                                                        
232700     END-IF                                                               
232800     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-MAIL                    
232900     .                                                                    
233000     EJECT                                                                
233100 S22-PUT-HEADER-MAIL SECTION.                                             
233200     MOVE 1                       TO REQU-IDMSGVER                        
233300     MOVE 'R'                     TO REQU-KDPGMACT                        
233400     MOVE IDPGM                   TO REQU-IDUSER                          
233500     MOVE 'DDGSNEWREPDATE'        TO HDR-IDOUTTYPE                        
233600     MOVE OBKR-IDDISTR            TO HDR-IDOUTREC                         
233700     MOVE OBKR-IDKUNDNR           TO WS-IDKUNDNR-X                        
233800     MOVE WS-IDKUNDNR-X           TO HDR-IDLIST                           
233900     MOVE 'PUT'                   TO SEND-KDFUNC                          
234000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
234100     MOVE WS-SAVE-IDCOM-MAIL      TO SEND-IDCOM                           
234200     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
234300                                     SEND-KVDLEN                          
234400                                     HDR-AREA                             
234500     IF SEND-KDRC > ZERO                                                  
234600       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
234700       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
234800       DELIMITED BY SIZE       INTO FELTEXT                               
234900       DISPLAY FELTEXT                                                    
235000       CALL FELLOG                                                        
235100     END-IF                                                               
235200     .                                                                    
235300     EJECT                                                                
235400 S25-PUT-LINE-MAIL SECTION.                                               
235500     MOVE 'PUT'                   TO SEND-KDFUNC                          
235600     MOVE LENGTH OF WS-REPORT     TO SEND-KVDLEN                          
235700     MOVE WS-SAVE-IDCOM-MAIL      TO SEND-IDCOM                           
235800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
235900                                     SEND-KVDLEN                          
236000                                     WS-REPORT                            
236100     IF SEND-KDRC > ZERO                                                  
236200       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
236300       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
236400       DELIMITED BY SIZE       INTO FELTEXT                               
236500       DISPLAY FELTEXT                                                    
236600       CALL FELLOG                                                        
236700     END-IF                                                               
236800     MOVE SPACES                  TO WS-REPORT                            
236900     .                                                                    
237000     SKIP2                                                                
237100 S29-SEND-CLOSE-MAIL SECTION.                                             
237200     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
237300     MOVE WS-SAVE-IDCOM-MAIL      TO SEND-IDCOM                           
237400     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
237500     IF SEND-KDRC > 0                                                     
237600       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
237700       STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                    
237800       DELIMITED BY SIZE       INTO FELTEXT                               
237900       DISPLAY FELTEXT                                                    
238000       CALL FELLOG                                                        
238100     END-IF                                                               
238200     .                                                                    
238300     EJECT                                                                
238400                                                                          
238500 S31-SEND-OPEN-TACD SECTION.                                              
238600     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
238700     MOVE 'OPEN'                  TO SEND-KDFUNC                          
238800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
238900                                     SEND-OPEN-AREA                       
239000     IF SEND-KDRC > ZERO                                                  
239100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
239200       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
239300       DELIMITED BY SIZE INTO FELTEXT                                     
239400       DISPLAY FELTEXT                                                    
239500       CALL FELLOG                                                        
239600     END-IF                                                               
239700     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-TACD                    
239800     .                                                                    
239900     EJECT                                                                
240000 S32-PUT-HEADER-TACD SECTION.                                             
240100     MOVE 1                       TO REQU-IDMSGVER                        
240200     MOVE 'R'                     TO REQU-KDPGMACT                        
240300     MOVE IDPGM                   TO REQU-IDUSER                          
240400     MOVE 'ORDERCONF'             TO HDR-IDOUTTYPE                        
240500     MOVE WS-IDKUNDNR6            TO HDR-IDOUTREC                         
240600     MOVE WS-IDORDNR7             TO HDR-IDLIST                           
240700     MOVE 'PUT'                   TO SEND-KDFUNC                          
240800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
240900     MOVE WS-SAVE-IDCOM-TACD      TO SEND-IDCOM                           
241000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
241100                                     SEND-KVDLEN                          
241200                                     HDR-AREA                             
241300     IF SEND-KDRC > ZERO                                                  
241400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
241500       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
241600       DELIMITED BY SIZE       INTO FELTEXT                               
241700       DISPLAY FELTEXT                                                    
241800       CALL FELLOG                                                        
241900     END-IF                                                               
242000     .                                                                    
242100     EJECT                                                                
242200 S35-PUT-LINE-TACD SECTION.                                               
242300     MOVE 'PUT'                   TO SEND-KDFUNC                          
242400     MOVE LENGTH OF 402-W402TACD  TO SEND-KVDLEN                          
242500     MOVE WS-SAVE-IDCOM-TACD      TO SEND-IDCOM                           
242600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
242700                                     SEND-KVDLEN                          
242800                                     402-W402TACD                         
242900     IF SEND-KDRC > ZERO                                                  
243000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
243100       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
243200       DELIMITED BY SIZE       INTO FELTEXT                               
243300       DISPLAY FELTEXT                                                    
243400       CALL FELLOG                                                        
243500     END-IF                                                               
243600     .                                                                    
243700     SKIP2                                                                
243800 S39-SEND-CLOSE-TACD SECTION.                                             
243900                                                                          
244000     MOVE 'CLOSE'                TO SEND-KDFUNC                           
244100     MOVE WS-SAVE-IDCOM-TACD     TO SEND-IDCOM                            
244200     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
244300     IF SEND-KDRC > 0                                                     
244400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
244500       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
244600       DELIMITED BY SIZE       INTO FELTEXT                               
244700       DISPLAY FELTEXT                                                    
244800       CALL FELLOG                                                        
244900     END-IF                                                               
245000     .                                                                    
245100     EJECT                                                                
245200                                                                          
245300* --- IMS SEKTIONER ---                                                   
245400                                                                          
245500 IMS-GET-MSG SECTION.                                                     
245600                                                                          
245700     MOVE '  QC' TO GODK-STATUSKODER                                      
245800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
245900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
246000     PERFORM IMS-STATUSKONTROLL                                           
246100     .                                                                    
246200                                                                          
246300 IMS-PURG-ALTMSG SECTION.                                                 
246400     MOVE LOW-VALUE TO 4690-Z1 4690-Z2                                    
246500     MOVE SPACE TO GODK-STATUSKODER                                       
246600     CALL CBLTDLI USING PURG 4690-PCB W-PROG-TO-PROG-SW                   
246700     MOVE 4690-STATUS-CODE TO STATUS-WS                                   
246800     PERFORM IMS-STATUSKONTROLL                                           
246900     .                                                                    
247000                                                                          
247100 IMS-GU-ORQI01      SECTION.                                              
247200     MOVE 'IMS-GU-ORQI01  ' TO CURRENT-IMS-SECTION                        
247300                                                                          
247400     MOVE SPACE               TO ALL-SSA                                  
247500     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
247600          DELIMITED BY SIZE INTO SSA1                                     
247700     MOVE '  '                TO GODK-STATUSKODER                         
247801     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ201 SSA1                    
247901     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
248000     PERFORM IMS-STATUSKONTROLL                                           
248100     .                                                                    
248200                                                                          
248300                                                                          
248400 IMS-GHU-ORQI01                 SECTION.                                  
248500     MOVE 'IMS-GHU-ORQI01  ' TO CURRENT-IMS-SECTION                       
248600                                                                          
248700     MOVE SPACE               TO ALL-SSA                                  
248800     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
248900          DELIMITED BY SIZE INTO SSA1                                     
249000     MOVE '    '              TO GODK-STATUSKODER                         
249101     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-WDQ201 SSA1                   
249201     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
249300     PERFORM IMS-STATUSKONTROLL                                           
249400     .                                                                    
249500                                                                          
249600                                                                          
249700 IMS-GHNP-ORQI11-F    SECTION.                                            
249800     MOVE 'IMS-GHNP-ORQI11-F' TO CURRENT-IMS-SECTION                      
249900                                                                          
250000     MOVE SPACE              TO ALL-SSA                                   
250100     MOVE   'WLORQI11*F'     TO SSA1                                      
250200     MOVE '  GE'           TO GODK-STATUSKODER                            
250301     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-WDQ211 SSA1                  
250401     MOVE ORQI-STATUS-CODE   TO STATUS-WS                                 
250500     PERFORM IMS-STATUSKONTROLL                                           
250600     .                                                                    
250700                                                                          
250800 IMS-GHNP-ORQI11      SECTION.                                            
250900     MOVE 'IMS-GHNP-ORQI11-F' TO CURRENT-IMS-SECTION                      
251000                                                                          
251100     MOVE SPACE              TO ALL-SSA                                   
251200     MOVE   'WLORQI11'     TO SSA1                                        
251300     MOVE '  GE'           TO GODK-STATUSKODER                            
251401     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-WDQ211 SSA1                  
251501     MOVE ORQI-STATUS-CODE   TO STATUS-WS                                 
251600     PERFORM IMS-STATUSKONTROLL                                           
251700     .                                                                    
251800                                                                          
251900                                                                          
252000 IMS-REPL-ORQI01     SECTION.                                             
252100     MOVE 'IMS-REPL-ORQI01 ' TO CURRENT-IMS-SECTION                       
252200                                                                          
252300     MOVE SPACE              TO ALL-SSA                                   
252400     MOVE '  '               TO GODK-STATUSKODER                          
252501     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-WDQ201                       
252601     MOVE ORQI-STATUS-CODE   TO STATUS-WS                                 
252700     PERFORM IMS-STATUSKONTROLL                                           
252800     .                                                                    
252900                                                                          
253000                                                                          
253100 IMS-REPL-ORQI11     SECTION.                                             
253200     MOVE 'IMS-REPL-ORQI11 ' TO CURRENT-IMS-SECTION                       
253300                                                                          
253400     MOVE SPACE              TO ALL-SSA                                   
253500     MOVE '  '               TO GODK-STATUSKODER                          
253601     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-WDQ211                       
253701     MOVE ORQI-STATUS-CODE   TO STATUS-WS                                 
253800     PERFORM IMS-STATUSKONTROLL                                           
253900     .                                                                    
254000                                                                          
254100                                                                          
254200 IMS-GNP-ORQI12      SECTION.                                             
254300     MOVE 'IMS-GNP-ORQI12  ' TO CURRENT-IMS-SECTION                       
254400                                                                          
254500     MOVE SPACE               TO ALL-SSA                                  
254600     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
254700          DELIMITED BY SIZE INTO SSA1                                     
254800     MOVE '  ' TO GODK-STATUSKODER                                        
254901     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-WDQ212 SSA1                  
255001     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
255100     PERFORM IMS-STATUSKONTROLL                                           
255200     .                                                                    
258000                                                                          
259600 IMS-GET-ORQA01      SECTION.                                             
259700     MOVE 'IMS-GET-ORQA01  ' TO CURRENT-IMS-SECTION                       
259800                                                                          
259900     MOVE SPACE               TO ALL-SSA                                  
260000     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
260100                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
260200                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
260300          DELIMITED BY SIZE INTO SSA1                                     
260400     MOVE '  '                TO GODK-STATUSKODER                         
260500     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-WDQ301 SSA1                   
260600     MOVE ORQA-STATUS-CODE    TO STATUS-WS                                
260700     PERFORM IMS-STATUSKONTROLL                                           
260800     .                                                                    
260900                                                                          
261000                                                                          
261100 IMS-REPL-ORQA01    SECTION.                                              
261200     MOVE 'IMS-REPL-ORQA01 ' TO CURRENT-IMS-SECTION                       
261300                                                                          
261400     MOVE SPACE              TO ALL-SSA                                   
261500     MOVE '  '               TO GODK-STATUSKODER                          
261600     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-WDQ301                       
261700     MOVE ORQA-STATUS-CODE   TO STATUS-WS                                 
261800     PERFORM IMS-STATUSKONTROLL                                           
261900     .                                                                    
262000                                                                          
262100                                                                          
262200 IMS-GET-ORQF01-GU    SECTION.                                            
262300     MOVE 'IMS-GET-ORQF01-U' TO CURRENT-IMS-SECTION                       
262400                                                                          
262500     MOVE SPACE               TO ALL-SSA                                  
262600     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
262700                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
262800          DELIMITED BY SIZE INTO SSA1                                     
262900     MOVE '  '                TO GODK-STATUSKODER                         
263000     CALL CBLTDLI USING GHU ORQF-PCB DLI-IO-WDQ401 SSA1                   
263100     MOVE ORQF-STATUS-CODE    TO STATUS-WS                                
263200     PERFORM IMS-STATUSKONTROLL                                           
263300     .                                                                    
263400                                                                          
263500                                                                          
263600 IMS-GET-ORQF01-GN    SECTION.                                            
263700     MOVE 'IMS-GET-ORQF01-N' TO CURRENT-IMS-SECTION                       
263800                                                                          
263900     MOVE SPACE               TO ALL-SSA                                  
264000     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
264100                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
264200          DELIMITED BY SIZE INTO SSA1                                     
264300     MOVE '  GBGE'            TO GODK-STATUSKODER                         
264400     CALL CBLTDLI USING GHN ORQF-PCB DLI-IO-WDQ401 SSA1                   
264500     MOVE ORQF-STATUS-CODE    TO STATUS-WS                                
264600     PERFORM IMS-STATUSKONTROLL                                           
264700     .                                                                    
264800                                                                          
264900                                                                          
265000 IMS-DLET-ORQF01   SECTION.                                               
265100     MOVE 'IMS-DLET-ORQF01 ' TO CURRENT-IMS-SECTION                       
265200                                                                          
265300     MOVE SPACE              TO ALL-SSA                                   
265400     MOVE '  '               TO GODK-STATUSKODER                          
265500     CALL CBLTDLI USING DLET ORQF-PCB DLI-IO-WDQ401                       
265600     MOVE ORQF-STATUS-CODE   TO STATUS-WS                                 
265700     PERFORM IMS-STATUSKONTROLL                                           
265800     .                                                                    
265900                                                                          
266000                                                                          
266100 IMS-ISRT-WDE401      SECTION.                                            
266200     MOVE 'IMS-ISRT-WDE401 ' TO CURRENT-IMS-SECTION                       
266300                                                                          
266400     MOVE SPACE              TO ALL-SSA                                   
266500     MOVE 'WDE401 '          TO SSA1                                      
266600     MOVE '  '               TO GODK-STATUSKODER                          
266700     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E401 SSA1                    
266800     MOVE WDE4-STATUS-CODE   TO STATUS-WS                                 
266900     PERFORM IMS-STATUSKONTROLL                                           
267000     .                                                                    
267100                                                                          
267200                                                                          
267300 IMS-ISRT-WDE411      SECTION.                                            
267400     MOVE 'IMS-ISRT-WDE411 ' TO CURRENT-IMS-SECTION                       
267500                                                                          
267600     MOVE SPACE               TO ALL-SSA                                  
267700     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
267800          DELIMITED BY SIZE INTO SSA1                                     
267900     MOVE 'WDE411 '           TO SSA2                                     
268000     MOVE '  '                TO GODK-STATUSKODER                         
268100     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E411 SSA1 SSA2               
268200     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
268300     PERFORM IMS-STATUSKONTROLL                                           
268400     .                                                                    
268500                                                                          
268600                                                                          
268700 IMS-ISRT-WDE601      SECTION.                                            
268800     MOVE 'IMS-ISRT-WDE601 ' TO CURRENT-IMS-SECTION                       
268900                                                                          
269000     MOVE SPACE              TO ALL-SSA                                   
269100     MOVE 'WDE601 '          TO SSA1                                      
269200     MOVE '  '               TO GODK-STATUSKODER                          
269300     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-E601 SSA1                    
269400     MOVE WDE6-STATUS-CODE   TO STATUS-WS                                 
269500     PERFORM IMS-STATUSKONTROLL                                           
269600     .                                                                    
269700                                                                          
269800 IMS-ISRT-WDF601      SECTION.                                            
269900     MOVE 'IMS-ISRT-WDF601 ' TO CURRENT-IMS-SECTION                       
270000                                                                          
270100     MOVE SPACE              TO ALL-SSA                                   
270200     MOVE 'WDF601 '          TO SSA1                                      
270300     MOVE '  '               TO GODK-STATUSKODER                          
270400     CALL CBLTDLI USING ISRT WDF6-PCB DLI-IO-WDF601 SSA1                  
270500     MOVE WDF6-STATUS-CODE   TO STATUS-WS                                 
270600     PERFORM IMS-STATUSKONTROLL                                           
270700     .                                                                    
270800                                                                          
270900                                                                          
271000 IMS-ISRT-WDF611      SECTION.                                            
271100     MOVE 'IMS-ISRT-WDF611 ' TO CURRENT-IMS-SECTION                       
271200                                                                          
271300     MOVE SPACE               TO ALL-SSA                                  
271400     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-X ')'                        
271500          DELIMITED BY SIZE INTO SSA1                                     
271600     MOVE 'WDF611 '           TO SSA2                                     
271700     MOVE '  '                TO GODK-STATUSKODER                         
271800     CALL CBLTDLI USING ISRT WDF6-PCB DLI-IO-WDF611 SSA1 SSA2             
271900     MOVE WDF6-STATUS-CODE    TO STATUS-WS                                
272000     PERFORM IMS-STATUSKONTROLL                                           
272100     .                                                                    
272200                                                                          
272300                                                                          
272400 IMS-GET-BENA11     SECTION.                                              
272500     MOVE 'IMS-GET-BENA11  ' TO CURRENT-IMS-SECTION                       
272600                                                                          
272700     MOVE SPACE               TO ALL-SSA                                  
272800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
272900          DELIMITED BY SIZE INTO SSA1                                     
273000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
273100          DELIMITED BY SIZE INTO SSA2                                     
273200     MOVE '  GE'              TO GODK-STATUSKODER                         
273300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA8 SSA1 SSA2                
273400     MOVE BENA-STATUS-CODE    TO STATUS-WS                                
273500     PERFORM IMS-STATUSKONTROLL                                           
273600     .                                                                    
273700                                                                          
273800 IMS-GU-ARTC11-CLAG SECTION.                                              
273900     MOVE 'IMS-GU-ARTC11-CL' TO CURRENT-IMS-SECTION                       
274000                                                                          
274100     MOVE SPACE               TO ALL-SSA                                  
274200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
274300          DELIMITED BY SIZE INTO SSA1                                     
274400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
274500          DELIMITED BY SIZE INTO SSA2                                     
274600     MOVE '    '              TO GODK-STATUSKODER                         
274700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA12 SSA1 SSA2               
274800     MOVE ARTC-STATUS-CODE    TO STATUS-WS                                
274900     PERFORM IMS-STATUSKONTROLL                                           
275000     .                                                                    
275100                                                                          
275200                                                                          
275300 IMS-GU-ARTS11 SECTION.                                                   
275400     MOVE 'IMS-GU-ARTS11   ' TO CURRENT-IMS-SECTION                       
275500                                                                          
275600     MOVE SPACE               TO ALL-SSA                                  
275700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
275800          DELIMITED BY SIZE INTO SSA1                                     
275900     STRING 'WLARTS11(IDDC     =' W-IDDC-ARTS11-X ')'                     
276000          DELIMITED BY SIZE INTO SSA2                                     
276100     MOVE '  '                TO GODK-STATUSKODER                         
276200     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-13 SSA1 SSA2              
276300     MOVE ARTS-STATUS-CODE    TO STATUS-WS                                
276400     PERFORM IMS-STATUSKONTROLL                                           
276500     .                                                                    
276600                                                                          
276700                                                                          
276800 IMS-GU-ARTN01 SECTION.                                                   
276900     MOVE 'IMS-GU-ARTN01   ' TO CURRENT-IMS-SECTION                       
277000                                                                          
277100     MOVE SPACE               TO ALL-SSA                                  
277200     STRING 'WLARTN01(IDARTNR  =' W-IDARTNR-X ')'                         
277300          DELIMITED BY SIZE INTO SSA1                                     
277400     MOVE '  GE'              TO GODK-STATUSKODER                         
277500     CALL CBLTDLI USING GU ARTN-PCB DLI-IO-AREA11 SSA1                    
277600     MOVE ARTN-STATUS-CODE    TO STATUS-WS                                
277700     PERFORM IMS-STATUSKONTROLL                                           
277800     .                                                                    
277900                                                                          
278000 IMS-GU-WDB601    SECTION.                                                
278100     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
278200                                                                          
278300     MOVE SPACE               TO ALL-SSA                                  
278400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
278500          DELIMITED BY SIZE INTO SSA1                                     
278600     MOVE '    '              TO GODK-STATUSKODER                         
278700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
278800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
278900     PERFORM IMS-STATUSKONTROLL                                           
279000     .                                                                    
279100                                                                          
279200                                                                          
279300 IMS-GU-WDF118    SECTION.                                                
279400     MOVE 'IMS-GU-WDF118   ' TO CURRENT-IMS-SECTION                       
279500                                                                          
279600     MOVE SPACE               TO ALL-SSA                                  
279700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-F1-X ')'                      
279800          DELIMITED BY SIZE INTO SSA1                                     
279900     STRING 'WDF118  (WDF118KY =' W-WDF118KY-X ')'                        
280000          DELIMITED BY SIZE INTO SSA2                                     
280100     MOVE '  GE'              TO GODK-STATUSKODER                         
280200     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F118 SSA1 SSA2            
280300     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
280400     PERFORM IMS-STATUSKONTROLL                                           
280500     .                                                                    
280600                                                                          
280700 IMS-ISRT-WDQ101 SECTION.                                                 
280800     MOVE 'IMS-ISRT-WDQ101 '      TO CURRENT-IMS-SECTION                  
280900                                                                          
281000     MOVE SPACE            TO ALL-SSA                                     
281100     MOVE 'WDQ101   '      TO SSA1                                        
281200     MOVE '  II'           TO GODK-STATUSKODER                            
281300     CALL CBLTDLI USING ISRT WDQ1-PCB OBKR-WDQ101 SSA1                    
281400     MOVE WDQ1-STATUS-CODE TO STATUS-WS                                   
281500     PERFORM IMS-STATUSKONTROLL                                           
281600     .                                                                    
281700                                                                          
281800 IMS-GU-WDB201 SECTION.                                                   
281900                                                                          
282000     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
282100          DELIMITED BY SIZE INTO SSA1                                     
282200     MOVE '  GE' TO GODK-STATUSKODER                                      
282300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
282400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
282500     PERFORM IMS-STATUSKONTROLL                                           
282600     .                                                                    
282700     EJECT                                                                
282800 IMS-STATUSKONTROLL SECTION.                                              
282900                                                                          
283000     SET STATUS-IX TO 1                                                   
283100     SEARCH GODK-STATUS                                                   
283200       AT END                                                             
283300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
283400         DELIMITED BY SIZE INTO FELTEXT                                   
283500         CALL FELLOG                                                      
283600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
284000     END-SEARCH                                                           
290000     .                                                                    
