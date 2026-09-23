000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4029300.                                                
000400 AUTHOR.         LARS THELL   CAP GEMINI LOGIC.                           
000500 DATE-WRITTEN.   SEPT  90.                                                
000600*                                                                         
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET SKAPAR TRANSAKTIONER FÖR EN ORDER.                    
001100*        FLKLAR I ORDERHUVUDET SÄTTS TILL J.                              
001200*        OM DET ÄR SPECIALORDER ELLER ÖVERLEVERANSORDER                   
001300*        SPARKAR MAN PÅ UTSKRIFTEN 4353 X-TRANS.                          
001400*                                                                         
001500*        PROGRAMMET BEHANDLAR 100 RADER I TAGET, FINNS DET                
001600*        FLER RADER SCHEDULERAR PROGRAMMET OM SIG SJÄLV.                  
001700*                                                                         
001800*        FINNS DET DIREKTLEVERANSRADER PÅ ORDERN, DVS                     
001900*        DET FINNS NÅGOT WDQ211 SEGMENT, STARTAS                          
002000*        W40695 UPP. DETTA PROGRAM SKRIVER UT PACK-                       
002100*        UNDERLAG OCH FLYTTAR RADERNA TILL WDE4/6.                        
002200*                                                                         
002300*        PROGRAMMET ÄR EN BAKGRUNDS-MPP                                   
002400*                                                                         
002500*        PROGRAMMET STARTAR W20109 VIA EN PPSW                            
002600*                                                                         
002700*        UPPDATERAR  WDQ2                                                 
002800*                    WDQ1                                                 
002900*                    WDQ4                                                 
003000*                    WDR6                                                 
003100*                                                                         
003200*    INDATA.                                                              
003300*        TRANSAKTION: W4T293X                                             
003400*        MID:         W4I29301                                            
003500*                                                                         
003600*    UTDATA.                                                              
003700*        TRANSAKTION: W4T293X                                             
003800*                     W4T695X                                             
003900*                     W4T353X                                             
004000*                     W2T109X                                             
004100*        MID:         W4I29301                                            
004200*                     W4I69501                                            
004300*                     W4I35301                                            
004400*                     W2I10902                                            
004500* STORY 2375089 / ADD IDSYSTEM VOUI, ECOM                                 
004600*                                                                         
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     SKIP3                                                                
005000 DATA DIVISION.                                                           
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300*    -- CHECKED BY WY2000                                                 
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W4029300'.            
005600 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
005700 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005800                                                                          
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  YES                         PIC X       VALUE 'Y'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  DEF-IDROLL                  PIC X(5)    VALUE 'VOR99'.               
006300 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006400 77  SOEK-IDDC                   PIC X(2)    VALUE SPACE.                 
006500 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
006600 77  WS-IDDISTR-X                PIC Z(5)    VALUE SPACE.                 
006700 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
006800 77  WS-IDORDNR7                 PIC 9(7).                                
006900 77  WS-KV402                    PIC S9(4)   VALUE ZERO.                  
007000 77  WS-KVCNO                    PIC S9(4)   VALUE ZERO.                  
007100 77  MAX-RAD                     PIC S9(7)   VALUE +100 COMP-3.           
007200 77  MAX-2109-IX                 PIC S9(4)   VALUE +18  COMP SYNC.        
007300 77  2109-IX                     PIC S9(4)   VALUE +0   COMP SYNC.        
007400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
007600 77  VOR-TID-ORAD                PIC 9(8)    VALUE ZERO.                  
007700 77  VOR-TID-OBKR                PIC 9(8)    VALUE ZERO.                  
007800 77  WS-TIREGTID                 PIC 9(6)    VALUE ZERO.                  
007900 77  S04-IDARTNR                 PIC 9(9)    VALUE ZERO COMP-3.           
008000 77  S04-KVVORKO                 PIC S9(7)   VALUE ZERO COMP-3.           
008100 77  S04-IDANSK                  PIC 9(3)    VALUE ZERO COMP-3.           
008200 77  S04-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
008300                                                                          
008400 77  CURRENT-IDARTNR             PIC 9(9)    VALUE ZERO COMP-3.           
008500 77  CURRENT-IDARTNR-TILLK       PIC 9(9)    VALUE ZERO COMP-3.           
008600 77  WS-TACDIS-SEQ               PIC 9(3)    VALUE ZERO.                  
008700 77  WS-NY-TILLK                 PIC X(1)    VALUE 'N'.                   
008800                                                                          
008900 01  WS-SAVE-IDCOM-OC            PIC S9(9)  COMP VALUE ZERO.              
009000 01  WS-SAVE-IDCOM-CN            PIC S9(9)  COMP VALUE ZERO.              
009100 01  WS-SAVE-IDCOM-MIC           PIC S9(9)  COMP VALUE ZERO.              
009200                                                                          
009300 01  WS-IDEVENTREC               PIC X(4)   VALUE SPACE.                  
009400                                                                          
009500 01  WS-TIMESTAMP.                                                        
009600     03 WS-SEKEL                 PIC 9(2)   VALUE 20.                     
009700     03 WS-DATE                  PIC 9(6).                                
009800     03 WS-TIME                  PIC 9(8).                                
009900                                                                          
010000*    UNDANTAGSTABELL FÖR MIC                                              
010100 01  W-SOK-DC-DIST.                                                       
010200     03  W-SOK-IDDC              PIC X(2).                                
010300     03  FILLER                  PIC X.                                   
010400     03  W-SOK-IDDISTR           PIC 9(5).                                
010500                                                                          
010600*    -COPY WMICEXCP                                                       
010700                                                                          
010800 01    WS-KDORDBEK               PIC 9(2)    VALUE ZERO.                  
010900   88  EVENT-OC                              VALUE 21 22 40 41 43         
011000                                                   52 54 55 57 61         
011100                                                   65 67 70 83 85         
011200                                                   87 90 91 92 93.        
011400                                                                          
011500 01  WS-IDAPIORDREF.                                                      
011600     03 WS-IDDISTR-EVENT         PIC 9(4).                                
011700     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
011800     03 WS-IDORDNR7-EVENT        PIC 9(7).                                
011900     03 WS-TIREGDAT-EVENT        PIC 9(6).                                
012000                                                                          
012100* ----- INDEXFÄLT                                                         
012200 77  IX-RAD                      PIC S9(9)   VALUE +0  COMP SYNC.         
012700                                                                          
012800* ----- SWITCHAR                                                          
012900*                                                                         
013000 77  VOR-K611-FINNS-SW           PIC X(1)    VALUE 'N'.                   
013100     88 VOR-K611-FINNS                       VALUE 'J'.                   
013200*                                                                         
013300 77  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
013400     88 TRAFF-VORKO                          VALUE 'J'.                   
013500*                                                                         
013600 77  DIRLEV-SW                   PIC X(1)    VALUE 'N'.                   
013700     88 DIRLEV-FINNS                         VALUE 'J'.                   
013800*                                                                         
013900 77  TRAFF-SW                    PIC X(1)    VALUE 'N'.                   
014000     88 TRAFF                                VALUE 'J'.                   
014100*                                                                         
014200 77  MIC-SW                      PIC X(1)   VALUE SPACE.                  
014300     88 SEND-TO-MIC                         VALUE 'J'.                    
014400*                                                                         
014500 77  MIC-ORDERPART-SW             PIC X(1)   VALUE 'N'.                   
014600     88 ORDERPART-SENT                       VALUE 'J'.                   
014700*                                                                         
014800 77  API-EVENT-SW                 PIC X(1)   VALUE 'N'.                   
014900     88 API-EVENT                            VALUE 'J'.                   
014910                                                                          
014911 77  NON-API-EVENT-SW             PIC X(1)   VALUE 'N'.                   
014912     88 NON-API-EVENT                        VALUE 'J'.                   
014913                                                                          
014920 77  LYNK-NONAPI-REG-EVENT-SW     PIC X(1)   VALUE 'N'.                   
014930     88 LYNK-NONAPI-REG-EVENT                VALUE 'J'.                   
014940                                                                          
014950 77  NON-API-OTH-EVENT-SW         PIC X(1)   VALUE 'N'.                   
014960     88 NON-API-OTH-EVENT                    VALUE 'J'.                   
014970                                                                          
014980 77  VOR-SW                       PIC X(1)   VALUE 'N'.                   
014990     88 VOR                                  VALUE 'J'.                   
014991                                                                          
014992 77  NON-API-BO-RELEASED-SW       PIC X(1)   VALUE 'N'.                   
014993     88 NON-API-BO-RELEASED                  VALUE 'J'.                   
014994                                                                          
014995 77  BO-RELEASE-EVENT-SW          PIC X(1)   VALUE 'N'.                   
014996     88 BO-RELEASE-EVENT                     VALUE 'J'.                   
014997                                                                          
015000*                                                                         
015100 01  S05-IDANSK-X.                                                        
015200     03 S05-IDANSK               PIC 9(3).                                
015300                                                                          
015400 01  S05-IDARTNR-X.                                                       
015500     03 S05-IDARTNR              PIC 9(9).                                
015600                                                                          
015700 01  S05-IDDISTR-X.                                                       
015800     03 S05-IDDISTR              PIC 9(4).                                
015900                                                                          
016000 01  S05-IDKUNDNR-X.                                                      
016100     03 S05-IDKUNDNR             PIC 9(6).                                
016200                                                                          
016300 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
016400 01  FILLER REDEFINES WS-TIHHMMSS.                                        
016500     03  WS-TIHHMM               PIC 9(4).                                
016600     03  FILLER                  PIC 9(2).                                
016700*                                                                         
016800 01  WS-TIAAAAMMDD               PIC  9(8).                               
016900 01  FILLER REDEFINES WS-TIAAAAMMDD.                                      
017000     03 WS-TIAA                  PIC  9(2).                               
017100     03 WS-TIAAMMDD              PIC  9(6).                               
017200*                                                                         
017300 01  KDRC-DISPLAY                PIC Z(5).                                
017400 01  FELTEXT.                                                             
017500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017700*                                                                         
017800                                                                          
017900*01  -COPY WWDCKONS                                                       
018000                                                                          
018100*01  -COPY WWDIST07                                                       
018200                                                                          
018300*01  -COPY WWBYT03                                                        
018400                                                                          
018500 01  W-IDDISTR-MIC               PIC 9(5)     COMP-3.                     
018600*    -COPY WWDIST35 -RED W-IDDISTR-MIC                                    
018700*    -COPY WWDIST47 -RED W-IDDISTR-MIC                                    
018800 EJECT                                                                    
018900* ----- ARBETSFÄLT FÖR TRANSAKTIONS SEGMENT                               
019000*                                                                         
019100 77  W-IDLOPNR                   PIC  9(3)    VALUE ZERO.                 
019200 77  W-ADGANG                    PIC  9(3)    VALUE ZERO.                 
019300 77  W-ADLAGOMR                  PIC  9(3)    VALUE ZERO.                 
019400*                                                                         
019500 77  W-IDTRANS                   PIC  X(4)    VALUE SPACE.                
019600     88 GODK-MID                              VALUE '4298'                
019700                                                    '4293'.               
020700                                                                          
020800 01  W-TIRFSDAT-FULL             PIC 9(10)  VALUE ZERO.                   
020900 01  FILLER REDEFINES W-TIRFSDAT-FULL.                                    
021000     03  W-TIRFSDAT              PIC 9(6).                                
021100     03  FILLER                  PIC 9(4).                                
021200                                                                          
021300 01  KUNDRF-IX                   PIC 9(3)   VALUE ZERO.                   
021400 01  KUNDRF-IX-MAX               PIC 9(3)   VALUE 100.                    
021500 01  W-IDKUNDRF-TAB.                                                      
021600  03 FILLER            OCCURS 100.                                        
021700     05  W-IDKUNDRF              PIC X(10)  VALUE SPACE.                  
021800     EJECT                                                                
021900* ----- GENERELLA  SUBPROGRAM                                             
022000 01  CBLTDLI                     PIC X(8)   VALUE 'CBLTDLI'.              
022100 01  FELLOG                      PIC X(8)   VALUE 'FELLOG '.              
022200 01  ABEND                       PIC X(8)   VALUE 'ABEND  '.              
022300 01  W005INIT                    PIC X(8)   VALUE 'W005INIT'.             
022400 01  W006KOM                     PIC X(8)   VALUE 'W006KOM '.             
022500 01  W009CIA                     PIC X(8)   VALUE 'W009CIA'.              
022600 01  WZ01SEND                    PIC X(8)   VALUE 'WZ01SEND'.             
022700*                                                                         
022800*    --- PARAMETERS TO ABEND                                              
022900 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
023000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
023100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
023200                                                                          
023300*01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-WMSGKOM        
023400 01  KOM-IO-AREA.                                                         
023500     03  -COPY WMSGKOM                                                    
023600                                                                          
023700*01  FILLER                      PIC X(16)   VALUE 'Z430-REQU-AREA        
023800*01  -COPY WZ0430I1  -PRE Z430-                                           
023900*    03  -COPY WAPIORD  -RED Z430-REQU-EVENT-DATA -PRE Z430-              
024000                                                                          
024100                                                                          
024200 01  FILLER                      PIC X(16)   VALUE '**W009CIA**'.         
024300*01  -COPY W009CIA                                                        
024400                                                                          
024500 01  FILLER                      PIC X(16)   VALUE '*WZ01SEND**'.         
024600*01  -COPY WZ01SEND                                                       
024700                                                                          
024800 01  FILLER                 PIC X(16)   VALUE '*WZ01SEND CN-DMS'.         
024900*01  -COPY WZ01SEND  -PRE CN-                                             
025000                                                                          
025100 01  HDR-AREA.                                                            
025200*    03  -COPY WZ01REQU                                                   
025300*    03  -COPY WZ04HDR                                                    
025400                                                                          
025500 01  FILLER                      PIC X(16)   VALUE '*W402TACD**'.         
025600*01  -COPY W402TACD                                                       
025700                                                                          
025800 01  FILLER                      PIC X(16)   VALUE '*W402CNO**'.          
025900*01  -COPY W402CNO                                                        
026000                                                                          
026100                                                                          
026200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
026300     SKIP3                                                                
026400 01  SEND-AREA                   PIC X(1000).                             
026500                                                                          
026600 01  FILLER                      PIC X(16)   VALUE '*W402MICO*'.          
026700*01  -COPY W402MICO                                                       
026800                                                                          
026900 01  FILLER                      PIC X(16)   VALUE '*W402MICA*'.          
027000*01  -COPY W402MICA                                                       
027100                                                                          
027200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
027300*01 -COPY WMSGINIT                                                        
027400     EJECT                                                                
027500*                                                                         
027600 01  NYCKLAR-TILL-DLI.                                                    
027700     SKIP2                                                                
027800   03    W-IDORDER-X.                                                     
027900     05  W-OHUV-IDORDER          PIC S9(7)   VALUE ZERO  COMP-3.          
028000     SKIP2                                                                
028100   03  W-WDQ2CSEQ.                                                        
028200     05  W-IDDISTR-CSEQ          PIC S9(5)   VALUE ZERO COMP-3.           
028300     05  W-IDKUNDNR-CSEQ         PIC S9(7)   VALUE ZERO COMP-3.           
028400     05  FILLER                  PIC 9(2)    VALUE ZERO.                  
028500     05  W-IDORDNR5-CSEQ         PIC 9(5).                                
028600     05  FILLER                  PIC X(3)    VALUE SPACE.                 
028700     SKIP2                                                                
028800   03    W-WDQ401KY-X.                                                    
028900     05    W-Q401KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
029000     05    W-Q401KY-IDDC         PIC X(2)    VALUE ZERO.                  
029100     05    W-Q401KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
029200     05    W-Q401KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
029300     05    W-Q401KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
029400     05    W-Q401KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
029500     05    W-Q401KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
029600     SKIP2                                                                
029700   03    W-WDQ401KY-MIN-X.                                                
029800     05    W-Q401KY-MIN-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
029900     05    W-Q401KY-MIN-IDDC     PIC  X(2)   VALUE ZERO.                  
030000     05    W-Q401KY-MIN-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
030100     05    W-Q401KY-MIN-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
030200     05    W-Q401KY-MIN-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
030300     05    W-Q401KY-MIN-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
030400     05    W-Q401KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
030500     SKIP2                                                                
030600   03    W-WDQ401KY-MAX-X.                                                
030700     05    W-Q401KY-MAX-IDORDER  PIC S9(7)   VALUE ZERO  COMP-3.          
030800     05    W-Q401KY-MAX-IDDC     PIC  X(2)   VALUE ZERO.                  
030900     05    W-Q401KY-MAX-ADLAGOMR PIC S9(3)   VALUE ZERO  COMP-3.          
031000     05    W-Q401KY-MAX-ADGANG   PIC S9(3)   VALUE ZERO  COMP-3.          
031100     05    W-Q401KY-MAX-ADPLATS  PIC S9(5)   VALUE ZERO  COMP-3.          
031200     05    W-Q401KY-MAX-IDARTNR  PIC S9(9)   VALUE ZERO  COMP-3.          
031300     05    W-Q401KY-MIN-IDLOPNR  PIC S9(3)   VALUE ZERO  COMP-3.          
031400     SKIP2                                                                
031500   03    W-WDQ101KY-X.                                                    
031600     05    W-Q101KY-IDORDER        PIC S9(7)   VALUE ZERO  COMP-3.        
031700     05    W-Q101KY-IDARTNR        PIC S9(9)   VALUE ZERO  COMP-3.        
031800     05    W-Q101KY-IDLOPNR        PIC S9(3)   VALUE ZERO  COMP-3.        
031900     05    W-Q101KY-IDSEKVNR       PIC S9(3)   VALUE ZERO  COMP-3.        
032000     05    W-Q101KY-IDDC           PIC  X(2)   VALUE ZERO.                
032100     05    W-Q101KY-KDORDBEK       PIC  9(2)   VALUE ZERO.                
032200     SKIP2                                                                
032300   03    W-WDQ101KY-MIN-X.                                                
032400     05    W-Q101KY-MIN-IDORDER    PIC S9(7)   VALUE ZERO  COMP-3.        
032500     05    W-Q101KY-MIN-IDARTNR    PIC S9(9)   VALUE ZERO  COMP-3.        
032600     05    W-Q101KY-MIN-IDLOPNR    PIC S9(3)   VALUE ZERO  COMP-3.        
032700     05    W-Q101KY-MIN-IDSEKVNR   PIC S9(3)   VALUE ZERO  COMP-3.        
032800     05    W-Q101KY-MIN-IDDC       PIC  X(2)   VALUE ZERO.                
032900     05    W-Q101KY-MIN-KDORDBEK   PIC  9(2)   VALUE ZERO.                
033000     SKIP2                                                                
033100   03    W-WDQ101KY-MAX-X.                                                
033200     05    W-Q101KY-MAX-IDORDER    PIC S9(7)   VALUE ZERO  COMP-3.        
033300     05    W-Q101KY-MAX-IDARTNR    PIC S9(9)   VALUE ZERO  COMP-3.        
033400     05    W-Q101KY-MAX-IDLOPNR    PIC S9(3)   VALUE ZERO  COMP-3.        
033500     05    W-Q101KY-MAX-IDSEKVNR   PIC S9(3)   VALUE ZERO  COMP-3.        
033600     05    W-Q101KY-MAX-IDDC       PIC  X(2)   VALUE ZERO.                
033700     05    W-Q101KY-MAX-KDORDBEK   PIC  9(2)   VALUE ZERO.                
033800     SKIP2                                                                
033900   03    W-WDA6BSEQ-MIN-X.                                                
034000     05    W-A6BSEQ-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
034100     05    W-A6BSEQ-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
034200     05    W-A6BSEQ-MIN-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
034300     05    W-A6BSEQ-MIN-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
034400     05    W-A6BSEQ-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
034500     05    FILLER                    PIC X(14) VALUE SPACE.               
034600     SKIP2                                                                
034700   03    W-WDA6BSEQ-MAX-X.                                                
034800     05    W-A6BSEQ-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
034900     05    W-A6BSEQ-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
035000     05    W-A6BSEQ-MAX-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
035100     05    W-A6BSEQ-MAX-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
035200     05    W-A6BSEQ-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
035300     05    FILLER                    PIC X(14) VALUE SPACE.               
035400     SKIP2                                                                
035500   03    W-IDARTNR-X.                                                     
035600     05  W-IDARTNR                 PIC S9(9)   VALUE ZERO  COMP-3.        
035700*--------------------WDB1                                                 
035800   03  W-WDB101KY-X.                                                      
035900       05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                  
036000       05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                   
036100                                                                          
036200*--------------------WDB2                                                 
036300   03    W-IDGMT-X.                                                       
036400     05  W-IDDISTR                 PIC S9(5)   VALUE ZERO  COMP-3.        
036500     05  W-IDKUNDNR                PIC S9(7)   VALUE ZERO  COMP-3.        
036600                                                                          
036700   03  W-IDDC-B6-X.                                                       
036800       05 W-IDDC-B6                  PIC X(2) VALUE SPACE.                
036900                                                                          
036901   03  W-IDDC-X.                                                          
036902       05 W-IDDC                     PIC X(2) VALUE SPACE.                
036903                                                                          
037000   03  W-IDDISTR-P4-X.                                                    
037100       05 W-IDDISTR-P4             PIC S9(5)   VALUE ZERO  COMP-3.        
037200                                                                          
037300   03  W-WDGXKEY-X.                                                       
037400       05  W-IDHTYP                PIC X(4)    VALUE '4253'.              
037500       05  FILLER                  PIC X(26)   VALUE LOW-VALUE.           
037600                                                                          
037700   03  W-IDARTNR-4254-X.                                                  
037800       05  W-IDARTNR-4254          PIC S9(9)   VALUE ZERO COMP-3.         
037900                                                                          
038000   03  W-WDA501KY-A5-MIN-X.                                               
038100       05  W-IDDISTR-A5-MIN        PIC S9(5) VALUE ZERO COMP-3.           
038200       05  W-IDKUNDNR-A5-MIN       PIC S9(7) VALUE ZERO COMP-3.           
038300       05  FILLER                  PIC X(17) VALUE LOW-VALUE.             
038400                                                                          
038500   03  W-WDA501KY-A5-MAX-X.                                               
038600       05  W-IDDISTR-A5-MAX        PIC S9(5) VALUE ZERO COMP-3.           
038700       05  W-IDKUNDNR-A5-MAX       PIC S9(7) VALUE ZERO COMP-3.           
038800       05  FILLER                  PIC X(17) VALUE HIGH-VALUE.            
038900                                                                          
039000   03  W-KDORDKL-X.                                                       
039100       05  W-KDORDKL               PIC S9    VALUE ZERO COMP-3.           
039200                                                                          
039300   03    W-IDKUNDRF-LEV-X.                                                
039400     05  W-IDKUNDRF-LEV            PIC X(10)   VALUE SPACE.               
039500                                                                          
039600     EJECT                                                                
039700******************************************************************        
039800*                                                                         
039900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
040000*                                                                         
040100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
040200     SKIP3                                                                
040300*01    MID -COPY W4I29301                                                 
040400     EJECT                                                                
040500*01    -COPY WMSGAREA                                                     
040600     EJECT                                                                
040700******************************************************************        
040800*    MID-AREA FÖR W2T109                                         *        
040900******************************************************************        
041000*01  -COPY  W2I10902  -PRE 2109-                                          
041100     EJECT                                                                
041200******************************************************************        
041300*    MID-AREA FÖR W2T191                                         *        
041400******************************************************************        
041500*01  -COPY  W2I19101  -PRE 2191-                                          
041600     EJECT                                                                
041700******************************************************************        
041800*    MOD-AREA FÖR W4T4293                                        *        
041900******************************************************************        
042000*01  -COPY  W4I29301  -PRE MOD4293-                                       
042100     EJECT                                                                
042200******************************************************************        
042300*    MOD-AREA FÖR W4T695                                         *        
042400******************************************************************        
042500*01  -COPY  W4I69501  -PRE MOD4695-                                       
042600     EJECT                                                                
042700******************************************************************        
042800*    MOD-AREA FÖR W4T353                                         *        
042900******************************************************************        
043000*01  -COPY  W4I35301  -PRE MOD4353-                                       
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
043300*01    -COPY WMFSAREA                                                     
043400     EJECT                                                                
043500******************************************************************        
043600*                                                                         
043700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043800*                                                                         
043900 01    IMS-WS.                                                            
044000   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
044100     SKIP3                                                                
044200*                        **** STATUS-KOD FRÅN IMS                         
044300   03    STATUS-WS               PIC XX.                                  
044400     88    ISRT-OK                           VALUE '  '.                  
044500     88    SEGMENT-FINNS                     VALUE '  '.                  
044600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
044700     88    END-OF-DATA                       VALUE 'GB'.                  
044800     SKIP3                                                                
044900   03    GODK-STATUSKODER.                                                
045000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
045100     SKIP3                                                                
045200 01    SSA1                      PIC X(150).                              
045300 01    SSA2                      PIC X(64).                               
045400     EJECT                                                                
045500*                            IMS FUNKTIONSKODER                           
045600*01    -COPY W0003                                                        
045700     EJECT                                                                
045800*                            DLI INPUT-OUTPUT AREA                        
045900 01  FILLER                      PIC X(16)   VALUE 'Q201-AREA'.           
046000 01    DLI-IO-AREA-WDQ201.                                                
046100     SKIP3                                                                
046200*  03    WLORQI01 -COPY WDQ201                                            
046300     EJECT                                                                
046400 01  FILLER                      PIC X(16)   VALUE 'R601-AREA'.           
046500 01    DLI-IO-AREA2.                                                      
046600*  03    WLFILA01  -COPY WDR601                                           
046700     EJECT                                                                
046800*    07  W414200   -COPY W414200A          -RED FIL-WDR601-DATA.          
046900     EJECT                                                                
047000*    07  W414201   -COPY W414201A          -RED FIL-WDR601-DATA.          
047100     EJECT                                                                
047200*    07  W414202   -COPY W414202A          -RED FIL-WDR601-DATA.          
047300     EJECT                                                                
047400 01  FILLER                      PIC X(16)   VALUE 'Q211-AREA'.           
047500 01    DLI-IO-AREA-WDQ211.                                                
047600*  03    WLORQI11  -COPY WDQ211                                           
047700     EJECT                                                                
047800 01  FILLER                      PIC X(16)   VALUE 'Q212-AREA'.           
047900 01    DLI-IO-AREA-WDQ212.                                                
048000*  03    WLORQI12 -COPY WDQ212                                            
048100     EJECT                                                                
048200 01  FILLER                      PIC X(16)   VALUE 'Q401-AREA'.           
048300 01    DLI-IO-AREA5.                                                      
048400*  03    WLORQF01 -COPY WDQ401                                            
048500     EJECT                                                                
048600 01  FILLER                      PIC X(16)   VALUE 'Q101-AREA'.           
048700 01    DLI-IO-AREA6.                                                      
048800*  03    WLORQM01 -COPY WDQ101                                            
048900     EJECT                                                                
049000 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
049100 01    DLI-IO-AREA7.                                                      
049200*  03    WDA601   -COPY WDA601                                            
049300     EJECT                                                                
049400 01  FILLER                      PIC X(16)   VALUE 'K601-AREA'.           
049500 01    DLI-IO-AREA10.                                                     
049600*  03    WDK601   -COPY WDK601                                            
049700     EJECT                                                                
049800 01  FILLER                      PIC X(16)   VALUE 'K611-AREA'.           
049900 01    DLI-IO-AREA11.                                                     
050000*  03    WDK611   -COPY WDK611                                            
050100     EJECT                                                                
050200 01  FILLER                      PIC X(16)   VALUE 'IO-WDB101'.           
050300 01  DLI-IO-AREA-WDB101.                                                  
050400     03  WDB101.                                                          
050500*        05  -COPY WDB101                                                 
050600     EJECT                                                                
050700 01  FILLER                      PIC X(16)   VALUE 'IO-WDB201'.           
050800 01  DLI-IO-AREA-WDB201.                                                  
051000*     03  -COPY WDB201                                                    
051100     EJECT                                                                
051200                                                                          
051300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
051400 01   DLI-IO-AREA-B601.                                                   
051500*     03  -COPY WDB601                                                    
051600                                                                          
051700 01  FILLER               PIC X(16)   VALUE 'WDP4A1 AREA'.                
051800 01   DLI-IO-AREA-WDP4A1.                                                 
051900*     03  -COPY WDP4A1                                                    
052000                                                                          
052100 01  FILLER               PIC X(16)   VALUE 'WDGX4254 AREA'.              
052200 01   DLI-IO-AREA-WDGX4254.                                               
052300*     03  -COPY WDGX4254                                                  
052400                                                                          
052500 01  FILLER               PIC X(16)   VALUE 'WDQ201   AREA'.              
052600 01   DLI-IO-AREA-Q201.                                                   
052700*     03  -COPY WDQ201 -PRE Q2-                                           
052800                                                                          
052900 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDA501'.              
053000 01  DLI-IO-WDA501.                                                       
053100*    03  -COPY WDA501                                                     
053200                                                                          
053300                                                                          
053400 LINKAGE SECTION.                                                         
053500*01    -COPY W0009     -PRE MSG-                                          
053600     EJECT                                                                
053700 01  0693-PCB                PIC X.                                       
053800     EJECT                                                                
053900*01    -COPY W0009     -PRE DISTRDOC-                                     
054000     EJECT                                                                
054100*01    -COPY W0009     -PRE ALT1-                                         
054200     EJECT                                                                
054300*01    -COPY W0009     -PRE ALT2-                                         
054400     EJECT                                                                
054500*01    -COPY W0009     -PRE ALT3-                                         
054600     EJECT                                                                
054700*01    -COPY W0009     -PRE ALT4-                                         
054800     EJECT                                                                
054900*01    -COPY W0009     -PRE ALT5-                                         
055000     EJECT                                                                
055100*01    -COPY W0008     -PRE USEA-                                         
055200     05  FILLER                  PIC X.                                   
055300*01    -COPY W0008     -PRE WDQ2-                                         
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600*01    -COPY W0008     -PRE ORQI-                                         
055700     05  FILLER                  PIC X.                                   
055800     EJECT                                                                
055900*01    -COPY W0008     -PRE ORQF-                                         
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01    -COPY W0008     -PRE ORQM-                                         
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01    -COPY W0008     -PRE FILA-                                         
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800*01    -COPY W0008     -PRE WDA6-                                         
056900     05  FILLER                  PIC X.                                   
057000     EJECT                                                                
057100*01    -COPY W0008     -PRE WDA6B-                                        
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400*01    -COPY W0008     -PRE WDK6-                                         
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700*01    -COPY W0008     -PRE WDB2-                                         
057800     05  FILLER                  PIC X.                                   
057900     EJECT                                                                
058000*01    -COPY W0008     -PRE WDB6-                                         
058100     05  FILLER                  PIC X.                                   
058200     EJECT                                                                
058300*01    -COPY W0008     -PRE WDP4A-                                        
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600*01    -COPY W0008     -PRE WDB1-                                         
058700     05  FILLER                  PIC X.                                   
058800     EJECT                                                                
058900*01    -COPY W0008     -PRE WDR5-                                         
059000     05  FILLER                  PIC X.                                   
059100     EJECT                                                                
059200 01  WDP8-PCB                PIC X.                                       
059300                                                                          
059400*01  -COPY W0008     -PRE WDA5-                                           
059500     05  FILLER                  PIC X.                                   
059600     EJECT                                                                
059700 PROCEDURE DIVISION  USING MSG-PCB 0693-PCB                               
059800                           DISTRDOC-PCB                                   
059900                           ALT1-PCB ALT2-PCB ALT3-PCB ALT4-PCB            
060000                           ALT5-PCB USEA-PCB WDQ2-PCB                     
060100                           ORQI-PCB ORQF-PCB ORQM-PCB FILA-PCB            
060200                           WDA6-PCB WDA6B-PCB WDK6-PCB                    
060300                           WDB2-PCB WDB6-PCB                              
060400                           WDP4A-PCB WDB1-PCB WDR5-PCB                    
060500                           WDP8-PCB WDA5-PCB.                             
060600                                                                          
060700     ENTRY 'DLITCBL' USING MSG-PCB 0693-PCB                               
060800                           DISTRDOC-PCB                                   
060900                           ALT1-PCB ALT2-PCB ALT3-PCB ALT4-PCB            
061000                           ALT5-PCB USEA-PCB WDQ2-PCB                     
061100                           ORQI-PCB ORQF-PCB ORQM-PCB FILA-PCB            
061200                           WDA6-PCB WDA6B-PCB WDK6-PCB                    
061300                           WDB2-PCB WDB6-PCB                              
061400                           WDP4A-PCB WDB1-PCB WDR5-PCB                    
061500                           WDP8-PCB WDA5-PCB.                             
061600                                                                          
061700 STYR      SECTION.                                                       
061800                                                                          
061900     PERFORM IMS-GET-MSG                                                  
062000     IF SEGMENT-FINNS                                                     
062100         PERFORM A-INIT                                                   
062200         IF GODK-MID                                                      
062300             PERFORM IMS-GET-ORQI01-OHUV-KVAL                             
062400             IF SEGMENT-FINNS                                             
062500                 PERFORM B-FIXA-LOKAL-TID                                 
062600                 PERFORM C-GET-KDKUNDKAT-CR-EVENT                         
062700                 PERFORM D-BEHANDLA-RADER                                 
062800                 IF SEGMENT-SAKNAS OR END-OF-DATA                         
062900                     PERFORM E-UPPD-OHUV-SKAPA-HTR                        
063000                     IF OHUV-IDSYSTEM = 'W216'                            
063100                       CONTINUE                                           
063200                     ELSE                                                 
063300                       IF OHUV-FLORDSPE =  JA  OR                         
063400                          OHUV-FLOVRLEV =  JA                             
063500                          PERFORM G-STARTA-W4T353X                        
063600                       END-IF                                             
063700                     END-IF                                               
063800                     PERFORM H-BEHANDLA-DIRLEV                            
063900                     IF DIRLEV-FINNS                                      
064010                         PERFORM I-STARTA-W4T695X                         
064100                     END-IF                                               
064200                  ELSE                                                    
064300                     PERFORM J-STARTA-W4T293X                             
064400                  END-IF                                                  
064500             END-IF                                                       
064600         END-IF                                                           
064700     END-IF                                                               
064800                                                                          
064900*    SKICKA DE ARTIKELRADER SOM FINNS I MID:EN TILL PGM W20109            
065000*    INNAN EV. OMSKEDULERING                                              
065100     IF 2109-MID2-KVANTART > ZERO                                         
065200       PERFORM S02-STARTA-W2T109X                                         
065300     END-IF                                                               
065400                                                                          
065500     IF API-EVENT OR NON-API-OTH-EVENT                                    
065600        MOVE OHUV-IDDISTR           TO WS-IDDISTR-EVENT                   
065700        MOVE OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT                  
065800        MOVE OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT                  
065900        MOVE OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT                  
066000        MOVE OHUV-IDSYSTEM          TO WS-IDEVENTREC                      
066100        PERFORM S15-CREATE-API-EVENT                                      
066200     END-IF                                                               
066300     MOVE ZERO                 TO RETURN-CODE                             
066400     GOBACK                                                               
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 A-INIT     SECTION.                                                      
067000     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I29301                    
067100     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
067200     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
067300     MOVE ' '                          TO MFS-KDTRTYP                     
067400     MOVE LOW-VALUE                    TO MSG-AREA                        
067500                                                                          
067600     MOVE SPACE                        TO MOD4293-MID-IDDB                
067700     MOVE MID-IDORDER                  TO W-OHUV-IDORDER                  
067800                                                                          
067900     MOVE MFS-IDTRANS                  TO W-IDTRANS                       
069100                                                                          
069200     MOVE NEJ                  TO  DIRLEV-SW                              
069300                                                                          
069400     MOVE IDPGM                TO  FIL-IDPGM                              
069500     ACCEPT DAGENS-DATUM       FROM  DATE                                 
069600     ACCEPT FIL-TIREGDAT       FROM  DATE                                 
069700     ACCEPT DAGENS-TID         FROM  TIME                                 
069800     MOVE DAGENS-TID           TO VOR-TID-ORAD                            
069900                                  VOR-TID-OBKR                            
070000     ACCEPT FIL-TIKLOCK        FROM  TIME                                 
070100     MOVE ZERO                 TO  FIL-IDSEKVNR                           
070200     MOVE 'W414'               TO  FIL-CT-IDSYSTEM                        
070300     MOVE 'A'                  TO  FIL-CT-IDVTYP                          
070400                                                                          
070500     MOVE SPACE                TO   2191-MID-W2I19101                     
070600                                                                          
070700     MOVE SPACE                TO   2109-MID2-W2I10902                    
070800     MOVE +1                   TO   2109-IX                               
070900                                                                          
071000     MOVE ZERO                 TO WS-KV402                                
071100     MOVE SPACE                TO W-IDKUNDRF-TAB                          
071200                                  MICO-IDDC                               
071300     MOVE ZERO                 TO WS-SAVE-IDCOM-MIC                       
071400     MOVE NEJ                  TO API-EVENT-SW                            
071401                                  NON-API-EVENT-SW                        
071410                                  LYNK-NONAPI-REG-EVENT-SW                
071420                                  VOR-SW                                  
071430*                                 NON-API-BO-RELEASED-SW                  
071440*                                 BO-RELEASE-EVENT-SW                     
071500                                                                          
071600*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
071700*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
071800     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
071900     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
072000     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
072100     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
072200     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
072300     MOVE 'W4029300'                 TO MSG-KOM-IDSNDJOB                  
072400     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
072500*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH ORDER                     
072600*    -- THIS IS THE START VALUE                                           
072700     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
072800                                                                          
072900*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
073000     MOVE LOW-VALUE                  TO MSG-KDZ1                          
073100     MOVE LOW-VALUE                  TO MSG-KDZ2                          
073200     .                                                                    
073300     EJECT                                                                
073400                                                                          
073500 B-FIXA-LOKAL-TID SECTION.                                                
073600                                                                          
073700     MOVE ALL '+'            TO MSGI-WMSGINIT                             
073800     MOVE '013'              TO MSGI-KDCALL                               
073900     MOVE 'WIDDC   '         TO MSGI-IDUSER                               
074000     MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                          
074100     MOVE '4293'             TO MSGI-IDTRANS                              
074200     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
074300                                                                          
074400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
074500     .                                                                    
074600     EJECT                                                                
074610                                                                          
074700 C-GET-KDKUNDKAT-CR-EVENT SECTION.                                        
074800                                                                          
074900     MOVE MID-IDDISTR        TO W-IDDISTR                                 
075000     MOVE MID-IDKUNDNR       TO W-IDKUNDNR                                
075200                                                                          
075300     PERFORM IMS-GU-WDB201                                                
075400                                                                          
075410     MOVE NEJ                TO LYNK-NONAPI-REG-EVENT-SW                  
075420                                NON-API-EVENT-SW                          
075430                                                                          
075500     IF SEGMENT-FINNS                                                     
075600       IF GMT-KDKUNDKAT = 03                                              
075700          IF OHUV-KDORDKL = 0                                             
075710             SET VOR              TO TRUE                                 
075720          END-IF                                                          
075721          IF OHUV-IDSYSTEM(1:3) NOT = 'LYN'                               
075740            MOVE JA                   TO LYNK-NONAPI-REG-EVENT-SW         
075741                                         NON-API-EVENT-SW                 
075742            MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                 
075743            MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT                
075744            MOVE OHUV-IDORDNR7        TO WS-IDORDNR7-EVENT                
075745            MOVE OHUV-TIREGDAT        TO WS-TIREGDAT-EVENT                
075746            MOVE OHUV-IDSYSTEM        TO WS-IDEVENTREC                    
075747            PERFORM S15-CREATE-API-EVENT                                  
075748            MOVE NEJ                  TO LYNK-NONAPI-REG-EVENT-SW         
075749          END-IF                                                          
075750       END-IF                                                             
075751     END-IF                                                               
075800     .                                                                    
075900     EJECT                                                                
076000                                                                          
076400 D-BEHANDLA-RADER         SECTION.                                        
076500                                                                          
076600     MOVE +1                   TO IX-RAD                                  
076700                                                                          
076800     EVALUATE TRUE                                                        
076900     WHEN MID-IDDB             =  SPACE OR 'WLORQF'                       
077000         PERFORM DA-BEHANDLA-ORDERKOE                                     
077100         IF IX-RAD             >  MAX-RAD                                 
077200             IF SEGMENT-FINNS                                             
077300                 PERFORM DB-SPARA-WDQ401-NYCKLAR                          
077400             ELSE                                                         
077500                PERFORM DC-BEHANDLA-ORDERBEK                              
077600                IF IX-RAD      > MAX-RAD                                  
077700                   IF SEGMENT-FINNS                                       
077800                      PERFORM DD-SPARA-WDQ101-NYCKLAR                     
077900                   END-IF                                                 
078000                END-IF                                                    
078100             END-IF                                                       
078200          ELSE                                                            
078300             PERFORM DC-BEHANDLA-ORDERBEK                                 
078400             IF IX-RAD         > MAX-RAD                                  
078500                IF SEGMENT-FINNS                                          
078600                   PERFORM DD-SPARA-WDQ101-NYCKLAR                        
078700                END-IF                                                    
078800             END-IF                                                       
078900         END-IF                                                           
079000                                                                          
079100     WHEN MID-IDDB             =  'WLORQM'                                
079200         PERFORM DC-BEHANDLA-ORDERBEK                                     
079300         IF IX-RAD         > MAX-RAD                                      
079400            IF SEGMENT-FINNS                                              
079500               PERFORM DD-SPARA-WDQ101-NYCKLAR                            
079600            END-IF                                                        
079700         END-IF                                                           
079800                                                                          
079900     END-EVALUATE                                                         
080000     .                                                                    
080100     EJECT                                                                
080200 DA-BEHANDLA-ORDERKOE              SECTION.                               
080410     PERFORM DAA-KOLLA-MID-LAS-WDQ401                                     
080500                                                                          
080600     PERFORM UNTIL SEGMENT-SAKNAS             OR                          
080700                   END-OF-DATA                OR                          
080800                   IX-RAD      > MAX-RAD                                  
080900                                                                          
081000****  TILLFÄLLIG ABEND  ELAINE                                            
081100       IF ORAD-KDVALISO = SPACE OR LOW-VALUE                              
081200          MOVE '** KDVALISO EJ IFYLLT PÅ WDQ4 **'                         
081300                                  TO FELTEXT                              
081500          CALL FELLOG                                                     
081600       END-IF                                                             
081700****  TILLFÄLLIG ABEND SLUT                                               
081800                                                                          
081900       IF ORAD-IDDC NOT = DCS-IDDC                                        
082000          MOVE ORAD-IDDC TO W-IDDC-B6                                     
082100          PERFORM IMS-GU-WDB601                                           
082200       END-IF                                                             
082300                                                                          
082400       IF ORAD-FLOBTRAN      =  JA                                        
082500          PERFORM DAB-SKAPA-WDQ4-TRANSAR                                  
082600                                                                          
082700          IF  OHUV-FLVORKO NOT = JA                                       
082800          AND ORAD-KDORDKL = 0                                            
082900          AND NOT DCS-NDC                                                 
083000               PERFORM DAE-TILL-VORKO                                     
083100          END-IF                                                          
083200                                                                          
083300          IF (ORAD-KDOI NOT = SPACE)  AND                                 
083400            (ORAD-IDKUNDRF-RO = '0000000   ' OR                           
083500             ORAD-FLORDING = NEJ)                                         
083600*         ORAD-FLORDING = NEJ ÄR EN SPECIAL FÖR ORDERINGÅNG               
083700*         PÅ KONSOLIDERAD RAD, DEN HAR IDKUNDREF-RO IFYLLD                
083800           MOVE ORAD-TIREGTID TO WS-TIREGTID                              
083900**         IF OHUV-IDSYSTEM = '4202' OR '4244' OR '4255'                  
084000**           IF (ORAD-TIREGDAT = DAGENS-DATUM) AND                        
084100**              (WS-TIREGTID(1:4) = DAGENS-TID(1:4))                      
084200**              PERFORM DAD-SKAPA-2109                                    
084300**           END-IF                                                       
084400**         ELSE                                                           
084500            IF OHUV-IDSYSTEM = ('LDC ' OR 'TACD') AND                     
084600               MID-FLORDING = JA                                          
084700*HÄR KOLLAR MAN OM MAN REDAN HAR BERÄKNAT ORDERINGÅNGEN OCH INTE          
084800*SKALL GÖRA DET IGEN. TINA 050215                                         
084900              CONTINUE                                                    
085000            ELSE                                                          
085100               PERFORM DAD-SKAPA-2109                                     
085200            END-IF                                                        
085300          END-IF                                                          
085400          PERFORM DAC-UPPDATERA-FLOBTRAN                                  
085500       END-IF                                                             
085600                                                                          
085700       MOVE OHUV-IDDISTR  TO DIST07-IDDISTR                               
085800       IF DCS-CHINA                       AND                             
085900*         OHUV-IDSYSTEM    = 'VIPS'       AND                             
086000          DIST07-KINA                     AND                             
086100          ORAD-IDKUNDRF-RO = '0000000   '                                 
086200                                                                          
086300          PERFORM DAF-SKAPA-KINA-CNO                                      
086400       END-IF                                                             
086500                                                                          
086600                                                                          
086700       PERFORM DAG-CHECK-MIC-FILE                                         
086800       IF SEND-TO-MIC                                                     
086900          PERFORM DAH-SEND-MIC-ROW                                        
087000       END-IF                                                             
087100                                                                          
087200*      CHECK FOR API-EVENT                                                
087300       IF ((ORAD-IDSYSTEM(1:3) = 'LYN' OR 'POL' OR 'ECO' OR               
087311                                 'TAD' OR 'ACC' OR 'APA' OR               
087312                                 'APB' OR 'APC' OR 'APD' OR               
087313                                 'APE' OR 'APF' OR 'APG' OR               
087314                                 'APH' OR 'API' OR 'APJ' )                
087316             OR NON-API-EVENT) AND (BO-RELEASE-EVENT-SW = NEJ)            
087320          MOVE ZERO TO WS-KDORDBEK                                        
087330          PERFORM S10-BO-EVENT                                            
087340       END-IF                                                             
087350       PERFORM IMS-GET-ORQF01-ORAD-OKVAL                                  
087360     END-PERFORM                                                          
087370                                                                          
087380     PERFORM S39-SEND-CLOSE-CN                                            
087390     IF SEND-TO-MIC                                                       
087400        PERFORM S43-SEND-CLOSE-MIC                                        
087500       MOVE SPACE TO MIC-SW                                               
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900                                                                          
088000 DAA-KOLLA-MID-LAS-WDQ401  SECTION.                                       
088100                                                                          
088200     IF MID-IDDB               =  'WLORQF'                                
088300         MOVE MID-IDORDER      TO  W-Q401KY-IDORDER                       
088400         MOVE MID-IDDC         TO  W-Q401KY-IDDC                          
088500         MOVE MID-ADLAGOMR     TO  W-Q401KY-ADLAGOMR                      
088600         MOVE MID-ADGANG       TO  W-Q401KY-ADGANG                        
088700         MOVE MID-ADPLATS      TO  W-Q401KY-ADPLATS                       
088800         MOVE MID-IDARTNR      TO  W-Q401KY-IDARTNR                       
088900         MOVE MID-IDLOPNR      TO  W-Q401KY-IDLOPNR                       
089000                                                                          
089100         PERFORM IMS-GET-ORQF01-ORAD-KVAL                                 
089200                                                                          
089300         MOVE LOW-VALUE        TO  W-WDQ401KY-MIN-X                       
089400         MOVE HIGH-VALUE       TO  W-WDQ401KY-MAX-X                       
089500         MOVE MID-IDORDER      TO  W-Q401KY-MIN-IDORDER                   
089600                                   W-Q401KY-MAX-IDORDER                   
089700      ELSE                                                                
089800         MOVE LOW-VALUE        TO  W-WDQ401KY-MIN-X                       
089900         MOVE HIGH-VALUE       TO  W-WDQ401KY-MAX-X                       
090000         MOVE MID-IDORDER      TO  W-Q401KY-MIN-IDORDER                   
090100                                   W-Q401KY-MAX-IDORDER                   
090200         PERFORM IMS-GET-ORQF01-ORAD-OKVAL                                
090300     END-IF                                                               
090400                                                                          
090500     .                                                                    
090600     EJECT                                                                
090700 DAB-SKAPA-WDQ4-TRANSAR        SECTION.                                   
090800                                                                          
090900     MOVE SPACE                TO  FIL-WDR601-DATA                        
091000     MOVE ORAD-IDARTNR         TO  201-IDARTNR                            
091100     MOVE ORAD-BERADREF        TO  201-BERADREF                           
091200     MOVE OHUV-BEVARREF        TO  201-BEVARREF                           
091300     MOVE ORAD-BEVOLREF        TO  201-BEVOLREF                           
091400     MOVE ORAD-FLINVEST        TO  201-FLINVEST                           
091500     MOVE OHUV-FLOVRLEV        TO  201-FLOVRLEV                           
091600     MOVE ORAD-FLPRTILL        TO  201-FLPRTILL                           
091700     MOVE ORAD-FLTILLK         TO  201-FLTILLK                            
091800     MOVE NEJ                  TO  201-FLRADTVS                           
091900     MOVE ORAD-IDDISTR         TO  201-IDDISTR                            
092000     MOVE ORAD-IDBIL           TO  201-IDBIL                              
092100     MOVE ORAD-IDKLIENT        TO  201-IDKLIENT                           
092200     MOVE ORAD-IDARBREF        TO  201-IDARBREF                           
092300     MOVE ORAD-IDVIN           TO  201-IDVIN                              
092400     MOVE ORAD-IDKAMPRF        TO  201-IDKAMPRF                           
092500     MOVE OHUV-IDKONTO         TO  201-IDKONTO                            
092600     MOVE OHUV-IDKST           TO  201-IDKST                              
092700     MOVE ORAD-IDKUNDNR        TO  201-IDKUNDNR                           
092800     MOVE ORAD-IDKUNDRF        TO  201-IDKUNDRF                           
092900     MOVE ORAD-IDKUNDRF-RO     TO  201-IDKUNDRF-RO                        
093000     MOVE ORAD-IDLOPNR         TO  201-IDLOPNR                            
093100     MOVE ORAD-IDORDER         TO  201-IDORDER                            
093200     MOVE ORAD-IDSYSTEM        TO  201-IDSYSTEM                           
093300     MOVE OHUV-IDSYSTEM        TO  201-IDSYSTEM-OHUV                      
093400     MOVE OHUV-IDUSER          TO  201-IDUSER                             
093500     MOVE ORAD-IDDC            TO  201-IDDC                               
093600     MOVE OHUV-IDDC-PRIM       TO  201-IDDC-CLEAR                         
093700     MOVE ORAD-KDDSP           TO  201-KDDSP                              
093800     MOVE OHUV-KDFAKTYP        TO  201-KDFAKTYP                           
093810                                                                          
093900     MOVE ORAD-IDDC            TO  SOEK-IDDC                              
094000     PERFORM S01-HAEMTA-FRAN-RAETT-ARBTAB                                 
094400     MOVE ARB-KDFRAKT          TO  201-KDFRAKT                            
094500                                                                          
094600     MOVE ORAD-KDKVBRYT        TO  201-KDKVBRYT                           
094700     MOVE ORAD-KDORDING        TO  201-KDORDING                           
094800     MOVE ORAD-KDORDKL         TO  201-KDORDKL                            
094900     MOVE ORAD-KDPRTYP         TO  201-KDPRTYP                            
095000     MOVE ORAD-KDTPOTYP        TO  201-KDTPOTYP                           
095100     MOVE ORAD-KDVRINFO        TO  201-KDVRINFO                           
095200     MOVE ORAD-KVBEART         TO  201-KVBEART                            
095300     MOVE ORAD-KVBEART-Q       TO  201-KVBEART-Q                          
095400     MOVE ORAD-PRARTNTO        TO  201-PRARTNTO                           
095500     MOVE ORAD-PRBPRIS         TO  201-PRBPRIS                            
095600     MOVE ORAD-REKSIFFR        TO  201-REKSIFFR                           
095700     MOVE ORAD-TIREGDAT        TO  201-TIREGDAT                           
095800     MOVE ORAD-TIRODAT         TO  201-TIRODAT                            
095900                                                                          
096000     ADD +1                    TO  IX-RAD                                 
096100     ADD +1                    TO  FIL-IDSEKVNR                           
096200     MOVE '201'                TO  FIL-CT-IDPTYP                          
096300     PERFORM IMS-ISRT-FILA01                                              
096400     PERFORM UNTIL SEGMENT-FINNS                                          
096500        ADD +1 TO FIL-IDSEKVNR                                            
096600        PERFORM IMS-ISRT-FILA01                                           
096700     END-PERFORM                                                          
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 DAC-UPPDATERA-FLOBTRAN        SECTION.                                   
097200                                                                          
097300     MOVE NEJ                  TO  ORAD-FLOBTRAN                          
097400     PERFORM IMS-REPL-ORQF01-ORAD                                         
097500     .                                                                    
097600     EJECT                                                                
097700 DAD-SKAPA-2109 SECTION.                                                  
097800                                                                          
097900*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
098000     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
098100     IF NOT BYT03-OBJEKT                                                  
098200                                                                          
098300        MOVE 2109-IX            TO 2109-MID2-KVANTART                     
098400        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR   (2109-IX)          
098500        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC      (2109-IX)          
098600        MOVE '+'                TO 2109-MID2-KDTECKEN  (2109-IX)          
098700        MOVE ORAD-KDOI          TO 2109-MID2-KDOI      (2109-IX)          
098800        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-IX)          
098900        MOVE ORAD-KVBEART-Q     TO 2109-MID2-KVOI      (2109-IX)          
099000        MOVE ORAD-TIREGDAT      TO 2109-MID2-TIUPPDAT  (2109-IX)          
099100                                                                          
099200        ADD 1                   TO 2109-IX                                
099300        IF 2109-IX > MAX-2109-IX                                          
099400           PERFORM S02-STARTA-W2T109X                                     
099500        END-IF                                                            
099600     END-IF                                                               
099700     .                                                                    
099800     EJECT                                                                
099900 DAE-TILL-VORKO SECTION.                                                  
100100     MOVE ORAD-IDARTNR TO S04-IDARTNR                                     
100200     PERFORM S04-BESTAM-LENVR-ANSK                                        
100300                                                                          
100400     MOVE ORAD-IDDISTR      TO VOR-IDDISTR                                
100500     MOVE ORAD-IDKUNDNR     TO VOR-IDKUNDNR                               
100600     MOVE ORAD-IDKUNDRF     TO VOR-IDKUNDRF                               
100700     MOVE ORAD-TIREGDAT     TO VOR-TIREGDAT-URSP                          
100800     MOVE ORAD-IDARTNR      TO VOR-IDARTNR                                
100900     MOVE VOR-TID-ORAD      TO VOR-TIREGTID-URSP                          
101000     MOVE 0                 TO VOR-TIREGDAT-AVV                           
101100     MOVE 0                 TO VOR-TIREGTID-AVV                           
101200     SUBTRACT 0            FROM 9999999                                   
101300                            GIVING VOR-TIREGDAT-AVV9                      
101400     SUBTRACT 0            FROM 999999999                                 
101500                            GIVING VOR-TIREGTID-AVV9                      
101600     MOVE ORAD-IDKUNDRF     TO VOR-IDKUNDRF-LEV                           
101700     MOVE ORAD-TIREGDAT     TO VOR-TIREGDAT-LEV                           
101800     MOVE VOR-TID-ORAD      TO VOR-TIREGTID-LEV                           
101900     MOVE S04-IDANSK        TO VOR-IDANSK                                 
102000                                                                          
102100*    IF W-IDDISTR NOT = VOR-IDDISTR                                       
102200        MOVE VOR-IDDISTR    TO W-IDDISTR-P4                               
102300        PERFORM IMS-GU-WDP4A1                                             
102400        IF SEGMENT-SAKNAS                                                 
102500           MOVE DEF-IDROLL  TO SEQA-IDROLL                                
102600        END-IF                                                            
102700*    END-IF                                                               
102800     MOVE SEQA-IDROLL       TO VOR-IDROLL                                 
102900                                                                          
103000     MOVE S04-IDLEVNR       TO VOR-IDLEVNR                                
103100     MOVE ORAD-BERADREF     TO VOR-BERADREF                               
103200     MOVE ORAD-KVBEART      TO VOR-KVBEART-URSP                           
103300                               VOR-KVBEART                                
103400     MOVE ORAD-KVBEART-Q    TO VOR-KVBEART-Q                              
103500     MOVE ORAD-KVPREAVB     TO VOR-KVPREAVB                               
103600                                                                          
103700*--FÖR ATT LEV ANTAL SKALL BLI KORREKT PÅ 4228                            
103800*                                                                         
103900     IF OHUV-FLORDSPE = JA                                                
104000        MOVE ORAD-KVBEART-Q TO VOR-KVPREAVB                               
104100     END-IF                                                               
104200     MOVE ORAD-IDDC         TO VOR-IDDC                                   
104300     MOVE SPACE             TO VOR-IDUSER                                 
104400     MOVE 0                 TO VOR-KDORDBEK                               
104500     MOVE ORAD-KDPRTYP      TO VOR-KDPRTYP                                
104600     MOVE '3'               TO VOR-KDVORATG                               
104700     MOVE ORAD-PRARTNTO     TO VOR-PRARTNTO                               
104800     MOVE '  '              TO VOR-TEVORMRK                               
104900     MOVE '  '              TO VOR-TEVORMRK-SC                            
105000     MOVE 0                 TO VOR-TIKLAR                                 
105100     MOVE 0                 TO VOR-TIKLATID                               
105200     MOVE 0                 TO VOR-TIUPPDAT                               
105300     MOVE 0                 TO VOR-TIUPPTID                               
105400     MOVE ORAD-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                           
105500     MOVE OHUV-FLVORFK      TO VOR-FLVORFK                                
105600     PERFORM IMS-ISRT-WDA601                                              
105700     PERFORM UNTIL ISRT-OK                                                
105800        ADD +1              TO VOR-TIREGTID-URSP                          
105900                               VOR-TIREGTID-LEV                           
106000        PERFORM IMS-ISRT-WDA601                                           
106100     END-PERFORM                                                          
106200     PERFORM DAEA-VOR-NA-PF                                               
106300     .                                                                    
106400     EJECT                                                                
106500 DAEA-VOR-NA-PF  SECTION.                                                 
106600     IF ORAD-IDDC NOT = DCS-IDDC                                          
106700        MOVE ORAD-IDDC TO W-IDDC-B6                                       
106800        PERFORM IMS-GU-WDB601                                             
106900     END-IF                                                               
107000     IF DCS-CDC AND ORAD-IDDISTR > 5000                                   
107100       MOVE ORAD-IDDISTR    TO W-IDDISTR                                  
107200       MOVE ORAD-IDKUNDNR   TO W-IDKUNDNR                                 
107300       PERFORM IMS-GU-WDB201                                              
107400       IF GMT-IDDC-DAY(1) NOT = ORAD-IDDC                                 
107500*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
107600        MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                          
107700        IF NOT BYT03-OBJEKT                                               
107800                                                                          
107900          MOVE 2109-IX          TO   2109-MID2-KVANTART                   
108000          MOVE ORAD-IDARTNR     TO   2109-MID2-IDARTNR  (2109-IX)         
108100          MOVE GMT-IDDC-DAY(1)  TO   2109-MID2-IDDC     (2109-IX)         
108200          MOVE '+'              TO   2109-MID2-KDTECKEN (2109-IX)         
108300          MOVE 'NN'             TO   2109-MID2-KDOI     (2109-IX)         
108400          MOVE ORAD-KVBEART-Q   TO   2109-MID2-KVOI     (2109-IX)         
108500          MOVE ORAD-TIREGDAT    TO   2109-MID2-TIUPPDAT (2109-IX)         
108600                                                                          
108700          ADD 1                 TO   2109-IX                              
108800          IF 2109-IX > MAX-2109-IX                                        
108900             PERFORM S02-STARTA-W2T109X                                   
109000          END-IF                                                          
109100        END-IF                                                            
109200                                                                          
109300       END-IF                                                             
109400     END-IF                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 DAF-SKAPA-KINA-CNO          SECTION.                                     
109800     MOVE 'CNO'                   TO CNO-IDPTYP                           
109900     MOVE OHUV-IDDISTR            TO CNO-IDDISTR                          
110000     MOVE OHUV-IDKUNDNR           TO CNO-IDKUNDNR                         
110100     MOVE OHUV-IDORDNR7           TO CNO-IDORDNR                          
110200                                                                          
110300     MOVE OHUV-TIREGDAT           TO CNO-TIREGDAT                         
110400     MOVE ORAD-IDARTNR            TO CNO-IDARTNR                          
110500     MOVE ORAD-KVBEART-Q          TO CNO-KVBEART                          
110600     MOVE ORAD-KVPRERO            TO CNO-KVPRERO                          
110700     MOVE ORAD-IDDC               TO SOEK-IDDC                            
110800     PERFORM S01-HAEMTA-FRAN-RAETT-ARBTAB                                 
110900     MOVE ARB-KDFRAKT             TO CNO-KDFRAKT                          
111400                                                                          
111500     IF WS-KVCNO = 0                                                      
111600       PERFORM S31-SEND-OPEN-CN                                           
111700       PERFORM S32-PUT-HEADER-CN                                          
111800     END-IF                                                               
111900     ADD +1                       TO WS-KVCNO                             
112000     PERFORM S35-PUT-LINE-CN                                              
112100     .                                                                    
112200     EJECT                                                                
112300 DAG-CHECK-MIC-FILE          SECTION.                                     
112400                                                                          
112500     MOVE JA               TO MIC-SW                                      
112600     PERFORM DAGA-GET-WDB101                                              
112700     PERFORM DAGB-GET-WDK611                                              
112800                                                                          
112900*    REFILL-, RETUR-, TRANSFER- OCH INTERNORDER                           
113000*    SKALL INTE TILL MIC, SÄTT MIC-SW = NEJ                               
113100                                                                          
113200     MOVE OHUV-IDDISTR     TO W-IDDISTR-MIC                               
113300     IF DIST35-RETUR                                                      
113400     OR DIST35-NA-TRANSFER                                                
113500     OR DIST35-PACIFIC-TRANSFER                                           
113600     OR DIST35-REFILL-INOM-JP                                             
113700     OR DIST35-CN-TRANSFER                                                
113800     OR DIST47-INTERNA                                                    
113900        MOVE NEJ           TO MIC-SW                                      
114000     END-IF                                                               
114100                                                                          
114200*    VI BLOCKAR HELA DISTRIKT SOM INTE SKALL TILL MIC                     
114300*    SÄTT MIC-SW = NEJ                                                    
114400                                                                          
114500     IF SEND-TO-MIC                                                       
114600        MOVE OHUV-IDDISTR     TO MICEXCP-IDDISTR                          
114700        IF MICEXCP-DISTRICT                                               
114800           MOVE NEJ           TO MIC-SW                                   
114900        END-IF                                                            
115000     END-IF                                                               
115100                                                                          
115200*    OM DC / DISTRIKT FINNS I UNDANTAGSTABELL                             
115300*    SÄTT MIC-SW = NEJ                                                    
115400                                                                          
115500     IF SEND-TO-MIC                                                       
115600        MOVE SPACE         TO W-SOK-DC-DIST                               
115700        MOVE ORAD-IDDC     TO W-SOK-IDDC                                  
115800        MOVE OHUV-IDDISTR  TO W-SOK-IDDISTR                               
115900        SEARCH ALL MICEXCP-DC-DIST                                        
116000          WHEN MIC-SOK-DC-DIST(MICEXCP-IX) = W-SOK-DC-DIST                
116100               MOVE NEJ    TO MIC-SW                                      
116200         END-SEARCH                                                       
116300     END-IF                                                               
116400                                                                          
116500     IF SEND-TO-MIC                                                       
116600        MOVE ART-KDSORT    TO MICEXCP-KDSORT                              
116700        IF MICEXCP-SW                                                     
116800           MOVE NEJ        TO MIC-SW                                      
116900        END-IF                                                            
117000     END-IF                                                               
117100                                                                          
117200     IF SEND-TO-MIC                                                       
117300        MOVE ART-IDFKNGRP  TO MICEXCP-IDFKNGRP                            
117400        IF MICEXCP-ZERO                                                   
117500           MOVE NEJ        TO MIC-SW                                      
117600        END-IF                                                            
117700     END-IF                                                               
117800                                                                          
117900     IF SEND-TO-MIC                                                       
118000        MOVE ART-IDARTNR   TO W-IDARTNR-4254                              
118100        PERFORM IMS-GU-WDGX4254                                           
118200        IF SEGMENT-FINNS                                                  
118300           IF 4254-TISTODAT > ZERO                                        
118400              CONTINUE                                                    
118500           ELSE                                                           
118600              MOVE NEJ     TO MIC-SW                                      
118700           END-IF                                                         
118800        END-IF                                                            
118900     END-IF                                                               
119000                                                                          
119100*    FIX FIX FOR STOPPING ALL SENDING TO MIC                              
119200*    'N' TO MIC-SW TO AVOID SENDIING TO MIC                               
119300*    INSTALLED LIKE THIS UNTIL MIC IS READY TO                            
119400*    HANDLE THE TRANSACTION                                               
119500                                                                          
119600*    MOVE NEJ        TO MIC-SW                                            
119700*                                                                         
119800*                                                                         
119900     .                                                                    
120000     EJECT                                                                
120100                                                                          
120200 DAGA-GET-WDB101            SECTION.                                      
120300                                                                          
120400     IF GMT-IDDISTR    NUMERIC AND                                        
120500        GMT-IDKUNDNR   NUMERIC AND                                        
120600        GMT-IDDISTR  = OHUV-IDDISTR AND                                   
120700        GMT-IDKUNDNR = OHUV-IDKUNDNR                                      
120800        CONTINUE                                                          
120900     ELSE                                                                 
121000        MOVE OHUV-IDDISTR    TO W-IDDISTR                                 
121100        MOVE OHUV-IDKUNDNR   TO W-IDKUNDNR                                
121200        PERFORM IMS-GU-WDB201                                             
121300        IF SEGMENT-SAKNAS                                                 
121400           MOVE ZERO         TO GMT-IDDISTR                               
121500                                GMT-IDKUNDNR                              
121600        END-IF                                                            
121700     END-IF                                                               
121800                                                                          
121900     IF GMT-IDDISTR = ZERO                                                
122000        MOVE SPACE          TO BET-IDLANDX2                               
122100     ELSE                                                                 
122200        MOVE GMT-IDPARTNR   TO W-WDB1-IDPARTNR                            
122300        MOVE GMT-IDFTG      TO W-WDB1-IDFTG                               
122400        PERFORM IMS-GU-WDB101                                             
122500        IF SEGMENT-SAKNAS                                                 
122600           MOVE SPACE       TO BET-IDLANDX2                               
122700        END-IF                                                            
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 DAGB-GET-WDK611            SECTION.                                      
123200                                                                          
123300     IF ART-IDARTNR NUMERIC AND                                           
123400        ART-IDARTNR = ORAD-IDARTNR AND                                    
123500        CLAG-KDSEGKEY = '1'                                               
123600        CONTINUE                                                          
123700     ELSE                                                                 
123800        MOVE ORAD-IDARTNR TO W-IDARTNR                                    
123900        PERFORM IMS-GU-WDK601                                             
124000        IF SEGMENT-FINNS                                                  
124100           PERFORM IMS-GNP-WDK611                                         
124200           IF SEGMENT-SAKNAS                                              
124300              MOVE ZERO   TO CLAG-PRARTSTD                                
124400           END-IF                                                         
124500        ELSE                                                              
124600           MOVE ZERO      TO CLAG-PRARTSTD                                
124700        END-IF                                                            
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100 DAH-SEND-MIC-ROW           SECTION.                                      
125200                                                                          
125300                                                                          
125400     IF NOT ORDERPART-SENT OR                                             
125500        ORAD-IDDC NOT = MICO-IDDC                                         
125600*    A NEW ORDER PART, A NEW MESSAGE SHOULD BE SENT                       
125700*    WE HAVE TO CLOSE AND OPEN MIC CONNECTION                             
125800        IF ORDERPART-SENT                                                 
125900           PERFORM S43-SEND-CLOSE-MIC                                     
126000           PERFORM S41-SEND-OPEN-MIC                                      
126100        ELSE                                                              
126200           PERFORM S41-SEND-OPEN-MIC                                      
126300           MOVE JA TO MIC-ORDERPART-SW                                    
126400        END-IF                                                            
126500        MOVE '001'              TO MICO-IDPTYP                            
126600        ACCEPT WS-DATE FROM DATE                                          
126700        ACCEPT WS-TIME FROM TIME                                          
126800        MOVE WS-TIMESTAMP       TO MICO-TIMESTAMP                         
126900        MOVE ORAD-IDDC          TO MICO-IDDC                              
127000        MOVE 'PULS'             TO MICO-IDSYSTEM                          
127100        MOVE DCS-IDLANDX2       TO MICO-IDLAND-FOM                        
127200        MOVE BET-IDLANDX2       TO MICO-IDLAND-TOM                        
127300        MOVE OHUV-IDORDNR7      TO MICO-IDORDNR7                          
127400        MOVE ORAD-IDDC         TO  SOEK-IDDC                              
127500        PERFORM S01-HAEMTA-FRAN-RAETT-ARBTAB                              
127600        MOVE ARB-TIRFS          TO W-TIRFSDAT-FULL                        
127700        MOVE W-TIRFSDAT         TO MICO-TIRFSDAT                          
127800                                                                          
128300        MOVE GMT-IDPARTNR       TO MICO-IDPARTNR                          
128400        MOVE OHUV-IDDISTR       TO MICO-IDDISTR                           
128500        MOVE OHUV-IDKUNDNR      TO MICO-IDKUNDNR                          
128600        MOVE GMT-IDPARTNER      TO MICO-IDPARTNER                         
128700        MOVE GMT-BEGMT-RAD1     TO MICO-BEGMT-1-1                         
128800        MOVE GMT-BEGMT-RAD2     TO MICO-BEGMT-1-2                         
128900        MOVE GMT-ADGMT-GATA     TO MICO-ADGMT-GATA-1                      
129000        IF GMT-KDPOSTNR = 'L'                                             
129100           MOVE GMT-ADGMT-PADR(1:10)                                      
129200                                TO MICO-ADPOSTNR                          
129300           MOVE GMT-ADGMT-PADR(11:25)                                     
129400                                TO MICO-ADCITY                            
129500        ELSE                                                              
129600           MOVE GMT-ADGMT-PADR(26:10)                                     
129700                                TO MICO-ADPOSTNR                          
129800           MOVE GMT-ADGMT-PADR(1:25)                                      
129900                                TO MICO-ADCITY                            
130000        END-IF                                                            
130100        MOVE OHUV-BEGMT-RAD1    TO MICO-BEGMT-2-1                         
130200        MOVE OHUV-BEGMT-RAD2    TO MICO-BEGMT-2-2                         
130300        MOVE OHUV-ADGMT-GATA    TO MICO-ADGMT-GATA-2                      
130400        MOVE OHUV-ADGMT-PADR    TO MICO-ADGMT-PADR-2                      
130500                                                                          
130600        PERFORM S42O-SEND-MESSAGE-MIC-ORDER                               
130700     END-IF                                                               
130800                                                                          
130900     MOVE '002'                 TO MICA-IDPTYP                            
131000     MOVE ORAD-IDARTNR          TO MICA-IDARTNR                           
131100     IF ART-KDSORT = 'L '                                                 
131200        MOVE 'LTR'              TO MICA-KDSORT                            
131300     ELSE                                                                 
131400        IF ART-KDSORT = 'M '                                              
131500           MOVE 'MTR'           TO MICA-KDSORT                            
131600        ELSE                                                              
131700           MOVE 'PCE'           TO MICA-KDSORT                            
131800        END-IF                                                            
131900     END-IF                                                               
132000     MOVE ORAD-KVBEART-Q        TO MICA-KVLEVART                          
132100     MOVE CLAG-PRARTSTD         TO MICA-PRARTSTD                          
132200     COMPUTE MICA-SUARTSTD = ORAD-KVBEART-Q * CLAG-PRARTSTD               
132300     MOVE ORAD-KDVALISO         TO MICA-KDVALISO                          
132400     MOVE ORAD-KDARTURS         TO MICA-KDARTURS                          
132500                                                                          
132600     PERFORM S42A-SEND-MESSAGE-MIC-ARTIKEL                                
132700     .                                                                    
132800     EJECT                                                                
132900 DB-SPARA-WDQ401-NYCKLAR    SECTION.                                      
133000                                                                          
133100     MOVE 'WLORQF'             TO  MOD4293-MID-IDDB                       
133200     MOVE ORAD-IDORDER         TO  MOD4293-MID-IDORDER                    
133300     MOVE ORAD-IDDC            TO  MOD4293-MID-IDDC                       
133400     MOVE ORAD-IDARTNR         TO  MOD4293-MID-IDARTNR                    
133500     MOVE ORAD-ADGANG          TO  W-ADGANG                               
133600     MOVE W-ADGANG (2:2)       TO  MOD4293-MID-ADGANG                     
133700     MOVE ORAD-ADLAGOMR        TO  W-ADLAGOMR                             
133800     MOVE W-ADLAGOMR (2:2)     TO  MOD4293-MID-ADLAGOMR                   
133900     MOVE ORAD-ADPLATS         TO  MOD4293-MID-ADPLATS                    
134000     MOVE ORAD-IDLOPNR         TO  W-IDLOPNR                              
134100     MOVE W-IDLOPNR (2:2)      TO  MOD4293-MID-IDLOPNR                    
134200     MOVE ZERO                 TO  MOD4293-MID-KDORDBEK                   
134300     MOVE ZERO                 TO  MOD4293-MID-IDSEKVNR                   
134400     MOVE MID-IDDISTR          TO  MOD4293-MID-IDDISTR                    
134500     MOVE MID-IDKUNDNR         TO  MOD4293-MID-IDKUNDNR                   
134600     MOVE MID-IDKUNDRF         TO  MOD4293-MID-IDKUNDRF                   
134700     .                                                                    
134800     EJECT                                                                
134900 DC-BEHANDLA-ORDERBEK       SECTION.                                      
135000                                                                          
135100     PERFORM DCA-KOLLA-MID-LAS-WDQ101                                     
135200                                                                          
135300     PERFORM UNTIL SEGMENT-SAKNAS             OR                          
135400                   END-OF-DATA                OR                          
135500                   IX-RAD      > MAX-RAD                                  
135600                                                                          
135700****  TILLFÄLLIG ABEND  ELAINE                                            
135800**       IF OBKR-KDVALISO = SPACE OR LOW-VALUE                            
135900**          MOVE '** KDVALISO EJ IFYLLT PÅ WDQ1 **'                       
136000**                                  TO FELTEXT                            
136200**          CALL FELLOG                                                   
136300**       END-IF                                                           
136400****  TILLFÄLLIG ABEND SLUT                                               
136600         IF (OBKR-IDSYSTEM(1:3) = 'LYN' OR 'POL' OR 'ECO' OR              
136710                                  'TAD' OR 'ACC' OR 'APA' OR              
136720                                  'APB' OR 'APC' OR 'APD' OR              
136730                                  'APE' OR 'APF' OR 'APG' OR              
136740                                  'APH' OR 'API' OR 'APJ' )               
136750            OR NON-API-EVENT                                              
136800            MOVE OBKR-KDORDBEK TO WS-KDORDBEK                             
136900            IF EVENT-OC                                                   
136910               IF NON-API-EVENT                                           
136930                  MOVE JA       TO NON-API-OTH-EVENT-SW                   
136940               ELSE                                                       
137000                  MOVE JA TO API-EVENT-SW                                 
137010               END-IF                                                     
137500            END-IF                                                        
137600         END-IF                                                           
137710         IF OBKR-FLOBTRAN      =  JA                                      
137800             PERFORM DCB-SKAPA-WDQ1-TRANSAR                               
137900                                                                          
138000             PERFORM DCH-LAES-KUNDREGISTER                                
138100             IF  (OBKR-IDSYSTEM = 'LDC' OR 'TACD')                        
138200             AND GMT-FLOBKR-TACD = JA                                     
138310                                                                          
138400               IF (OBKR-KDORDBEK = 41 OR 61)                              
138500               AND OBKR-IDARTNR-TILLK = ZERO                              
138600                 MOVE OBKR-KVBEART TO 402-KVBEART                         
138700               ELSE                                                       
138800                 PERFORM DCG-SKAPA-TACD-402                               
138900               END-IF                                                     
139000             END-IF                                                       
139100                                                                          
139200             IF OBKR-IDDC NOT = DCS-IDDC                                  
139300                MOVE OBKR-IDDC TO W-IDDC-B6                               
139400                PERFORM IMS-GU-WDB601                                     
139500             END-IF                                                       
139600*-------------------------------------- TILL VORKÖ, HAR INTE              
139700*                                       VARIT DÄR FÖRUT                   
139800             IF OHUV-FLVORKO NOT = JA                                     
139900             AND OBKR-KDORDKL = 0                                         
140000             AND NOT DCS-NDC                                              
140100             AND (OBKR-KDORDBEK = 51                                      
140200              OR OBKR-KDORDBEK = 52                                       
140300              OR OBKR-KDORDBEK = 53                                       
140400              OR OBKR-KDORDBEK = 54                                       
140500              OR OBKR-KDORDBEK = 55                                       
140600              OR OBKR-KDORDBEK = 57                                       
140700              OR OBKR-KDORDBEK = 67                                       
140800              OR OBKR-KDORDBEK = 92)                                      
140900                  PERFORM DCE-TILL-VORKO                                  
141000             END-IF                                                       
141100*-------------------------------------- TILL VORKÖ, HAR                   
141200*                                       VARIT DÄR FÖRUT                   
141300             IF OHUV-FLVORKO  = JA                                        
141400             AND OBKR-KDORDKL = 0                                         
141500             AND NOT DCS-NDC                                              
141600             AND (OBKR-KDORDBEK = 51                                      
141700              OR OBKR-KDORDBEK = 52                                       
141800              OR OBKR-KDORDBEK = 53                                       
141900              OR OBKR-KDORDBEK = 54                                       
142000              OR OBKR-KDORDBEK = 55                                       
142100              OR OBKR-KDORDBEK = 57                                       
142200              OR OBKR-KDORDBEK = 67                                       
142300              OR OBKR-KDORDBEK = 92)                                      
142400                  PERFORM DCF-TILL-VORKO                                  
142500             END-IF                                                       
142600                                                                          
142700             PERFORM DCC-UPPDATERA-FLOBTRAN                               
142800                                                                          
142900          IF (OBKR-KDOI NOT = SPACE)  AND                                 
143000            (OBKR-KDORDBEK = 71 OR 77) AND                                
143100            (OBKR-KDTPOTYP = 2 )                                          
143200            MOVE OBKR-TIREGTID TO WS-TIREGTID                             
143300            IF (OBKR-TIREGDAT = DAGENS-DATUM) AND                         
143400              (WS-TIREGTID(1:4) = DAGENS-TID(1:4))                        
143500              PERFORM DCD-SKAPA-2109                                      
143600            END-IF                                                        
143700          END-IF                                                          
143800         END-IF                                                           
143900                                                                          
144000         IF OHUV-BEKUNDRF = 'DL REL' AND                                  
144100            OBKR-BEKUNDRF = 'DL REL' AND                                  
144200            OBKR-KDORDBEK =  10                                           
144300                                                                          
144400            PERFORM DCI-SPARA-ORDERID                                     
144500         END-IF                                                           
144600                                                                          
144700         PERFORM IMS-GET-ORQM01-OBKR-OKVAL                                
144800     END-PERFORM                                                          
144900                                                                          
145000     PERFORM S29-SEND-CLOSE                                               
145100     .                                                                    
145200     EJECT                                                                
145300 DCA-KOLLA-MID-LAS-WDQ101   SECTION.                                      
145400                                                                          
145500     IF MID-IDDB               =  'WLORQM'                                
145600         MOVE MID-IDORDER      TO  W-Q101KY-IDORDER                       
145700         MOVE MID-IDARTNR      TO  W-Q101KY-IDARTNR                       
145800         MOVE MID-IDLOPNR      TO  W-Q101KY-IDLOPNR                       
145900         MOVE MID-IDSEKVNR     TO  W-Q101KY-IDSEKVNR                      
146000         MOVE MID-IDDC         TO  W-Q101KY-IDDC                          
146100         MOVE MID-KDORDBEK     TO  W-Q101KY-KDORDBEK                      
146200                                                                          
146300         PERFORM IMS-GET-ORQM01-OBKR-KVAL                                 
146400                                                                          
146500         MOVE LOW-VALUE        TO  W-WDQ101KY-MIN-X                       
146600         MOVE HIGH-VALUE       TO  W-WDQ101KY-MAX-X                       
146700         MOVE MID-IDORDER      TO  W-Q101KY-MIN-IDORDER                   
146800                                   W-Q101KY-MAX-IDORDER                   
146900      ELSE                                                                
147000         MOVE LOW-VALUE        TO  W-WDQ101KY-MIN-X                       
147100         MOVE HIGH-VALUE       TO  W-WDQ101KY-MAX-X                       
147200         MOVE MID-IDORDER      TO  W-Q101KY-MIN-IDORDER                   
147300                                   W-Q101KY-MAX-IDORDER                   
147400         PERFORM IMS-GET-ORQM01-OBKR-OKVAL                                
147500     END-IF                                                               
147600     .                                                                    
147700     EJECT                                                                
147800 DCB-SKAPA-WDQ1-TRANSAR        SECTION.                                   
147900                                                                          
148000     MOVE SPACE                TO  FIL-WDR601-DATA                        
148100     MOVE OBKR-IDARTNR         TO  202-IDARTNR                            
148200     MOVE OBKR-BEERS           TO  202-BEERS                              
148300     MOVE OBKR-BERADREF        TO  202-BERADREF                           
148400     MOVE OHUV-BEVARREF        TO  202-BEVARREF                           
148500     MOVE OBKR-BEVOLREF        TO  202-BEVOLREF                           
148600     MOVE OBKR-DIERS-KVOT      TO  202-DIERS-KVOT                         
148700     MOVE OBKR-FLINVEST        TO  202-FLINVEST                           
148800     MOVE OHUV-FLOVRLEV        TO  202-FLOVRLEV                           
148900     MOVE OBKR-FLPRTILL        TO  202-FLPRTILL                           
149000     MOVE OBKR-FLTILLK         TO  202-FLTILLK                            
149100     MOVE OBKR-IDARTNR-TILLK   TO  202-IDARTNR-TILLK                      
149200     MOVE OBKR-IDBIL           TO  202-IDBIL                              
149300     MOVE OBKR-IDDISTR         TO  202-IDDISTR                            
149400     MOVE OHUV-IDKONTO         TO  202-IDKONTO                            
149500     MOVE OHUV-IDKST           TO  202-IDKST                              
149600     MOVE OBKR-IDKUNDNR        TO  202-IDKUNDNR                           
149700     MOVE OBKR-IDKUNDRF        TO  202-IDKUNDRF                           
149800     MOVE OBKR-IDKUNDRF-RO     TO  202-IDKUNDRF-RO                        
149900     MOVE OBKR-IDLOPNR         TO  202-IDLOPNR                            
150000     MOVE OBKR-IDORDER         TO  202-IDORDER                            
150100     MOVE OBKR-IDSEKVNR        TO  202-IDSEKVNR                           
150200     MOVE OBKR-IDSYSTEM        TO  202-IDSYSTEM                           
150300     MOVE OBKR-IDDC            TO  202-IDDC                               
150400     MOVE OBKR-KDDSP           TO  202-KDDSP                              
150500     MOVE OBKR-KDERS           TO  202-KDERS                              
150600     MOVE OHUV-KDFAKTYP        TO  202-KDFAKTYP                           
150700     MOVE OBKR-IDDC            TO  SOEK-IDDC                              
150800     PERFORM S01-HAEMTA-FRAN-RAETT-ARBTAB                                 
150900     MOVE ARB-KDFRAKT          TO  202-KDFRAKT                            
151000                                                                          
151400     MOVE OBKR-KDKVBRYT        TO  202-KDKVBRYT                           
151500     MOVE OBKR-KDORDBEK        TO  202-KDORDBEK                           
151600     MOVE OBKR-KDORDKL         TO  202-KDORDKL                            
151700     MOVE OBKR-KDPRTYP         TO  202-KDPRTYP                            
151800     MOVE OBKR-KDTPOTYP        TO  202-KDTPOTYP                           
151900     MOVE OBKR-KDVRINFO        TO  202-KDVRINFO                           
152000     MOVE OBKR-KVBEART         TO  202-KVBEART                            
152100     MOVE OBKR-KVBEART-Q       TO  202-KVBEART-Q                          
152200     MOVE OBKR-KVBEART-TILLK   TO  202-KVBEART-TILLK                      
152300     MOVE OBKR-KVPREAVB        TO  202-KVPREAVB                           
152400     MOVE OBKR-KVPRERO         TO  202-KVPRERO                            
152500     MOVE OBKR-KVQPACK         TO  202-KVQPACK-1                          
152600     MOVE OBKR-PRARTNTO        TO  202-PRARTNTO                           
152700     MOVE OBKR-REKSIFFR        TO  202-REKSIFFR                           
152800     MOVE OBKR-REKSIFFR-TILLK  TO  202-REKSIFFR-TILLK                     
152900     MOVE OBKR-TIDISPIN        TO  202-TIDISPIN                           
153000     MOVE OBKR-TIORDREG        TO  202-TIORDREG                           
153100     MOVE OBKR-TIREGDAT        TO  202-TIREGDAT                           
153200     MOVE OBKR-TITPO           TO  202-TITPO                              
153300     IF OBKR-TIREPDAT IS NUMERIC                                          
153400        MOVE OBKR-TIREPDAT     TO  202-TIREPDAT                           
153500     ELSE                                                                 
153600        MOVE +0                TO  202-TIREPDAT                           
153700     END-IF                                                               
153800                                                                          
153900     ADD +1                    TO  IX-RAD                                 
154000     ADD +1                    TO  FIL-IDSEKVNR                           
154100     MOVE '202'                TO  FIL-CT-IDPTYP                          
154200     PERFORM IMS-ISRT-FILA01                                              
154300     PERFORM UNTIL SEGMENT-FINNS                                          
154400        ADD +1 TO FIL-IDSEKVNR                                            
154500        PERFORM IMS-ISRT-FILA01                                           
154600     END-PERFORM                                                          
154700                                                                          
154800     .                                                                    
154900     EJECT                                                                
155000 DCC-UPPDATERA-FLOBTRAN       SECTION.                                    
155100                                                                          
155200     MOVE NEJ                  TO  OBKR-FLOBTRAN                          
155300     PERFORM IMS-REPL-ORQM01-OBKR                                         
155400     .                                                                    
155500     EJECT                                                                
155600                                                                          
155700 DCD-SKAPA-2109 SECTION.                                                  
155800                                                                          
155900*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
156000     MOVE OBKR-IDARTNR       TO BYT03-IDARTNR                             
156100     IF NOT BYT03-OBJEKT                                                  
156200                                                                          
156300        MOVE 2109-IX          TO   2109-MID2-KVANTART                     
156400        MOVE OBKR-IDARTNR     TO   2109-MID2-IDARTNR   (2109-IX)          
156500        MOVE OBKR-IDDC        TO   2109-MID2-IDDC      (2109-IX)          
156600        MOVE '+'              TO   2109-MID2-KDTECKEN  (2109-IX)          
156700        MOVE OBKR-KDOI        TO   2109-MID2-KDOI      (2109-IX)          
156800        MOVE OBKR-CLEARGROUP  TO   2109-MID2-CLEARGROUP(2109-IX)          
156900        MOVE OBKR-KVBEART-Q   TO   2109-MID2-KVOI      (2109-IX)          
157000        MOVE OBKR-TIREGDAT    TO   2109-MID2-TIUPPDAT  (2109-IX)          
157100                                                                          
157200        ADD 1                 TO   2109-IX                                
157300        IF 2109-IX > MAX-2109-IX                                          
157400           PERFORM S02-STARTA-W2T109X                                     
157500        END-IF                                                            
157600     END-IF                                                               
157700     .                                                                    
157800     EJECT                                                                
157900                                                                          
158000 DCE-TILL-VORKO SECTION.                                                  
158100                                                                          
158200*------------------------------------- RADEN KOMMER EJ FRÅN VORKÖN        
158300                                                                          
158500     IF OBKR-IDARTNR-TILLK > 0                                            
158600       MOVE OBKR-IDARTNR-TILLK TO S04-IDARTNR                             
158700     ELSE                                                                 
158800       MOVE OBKR-IDARTNR      TO S04-IDARTNR                              
158900     END-IF                                                               
159000     PERFORM S04-OBKR-BESTAM-LENVR-ANSK                                   
159100                                                                          
159200     IF  OBKR-KVPREAVB = 0                                                
159300       MOVE OBKR-IDDISTR        TO VOR-IDDISTR                            
159400       MOVE OBKR-IDKUNDNR       TO VOR-IDKUNDNR                           
159500       MOVE OBKR-IDKUNDRF       TO VOR-IDKUNDRF                           
159600       MOVE OBKR-TIREGDAT       TO VOR-TIREGDAT-URSP                      
159700       IF OBKR-IDARTNR-TILLK > 0                                          
159800         MOVE OBKR-IDARTNR-TILLK TO VOR-IDARTNR                           
159900       ELSE                                                               
160000         MOVE OBKR-IDARTNR      TO VOR-IDARTNR                            
160100       END-IF                                                             
160200       MOVE VOR-TID-ORAD        TO VOR-TIREGTID-URSP                      
160300       MOVE 0                   TO VOR-TIREGDAT-AVV                       
160400       MOVE 0                   TO VOR-TIREGTID-AVV                       
160500       SUBTRACT 0               FROM 9999999                              
160600                                GIVING VOR-TIREGDAT-AVV9                  
160700       SUBTRACT 0               FROM 999999999                            
160800                                GIVING VOR-TIREGTID-AVV9                  
160900       MOVE OBKR-IDKUNDRF       TO VOR-IDKUNDRF-LEV                       
161000       MOVE OBKR-TIREGDAT       TO VOR-TIREGDAT-LEV                       
161100       MOVE VOR-TID-ORAD        TO VOR-TIREGTID-LEV                       
161200       MOVE S04-IDANSK          TO VOR-IDANSK                             
161300                                                                          
161400*      IF W-IDDISTR NOT = VOR-IDDISTR                                     
161500          MOVE VOR-IDDISTR  TO W-IDDISTR-P4                               
161600          PERFORM IMS-GU-WDP4A1                                           
161700          IF SEGMENT-SAKNAS                                               
161800             MOVE DEF-IDROLL TO SEQA-IDROLL                               
161900          END-IF                                                          
162000*      END-IF                                                             
162100       MOVE SEQA-IDROLL     TO VOR-IDROLL                                 
162200                                                                          
162300       MOVE S04-IDLEVNR         TO VOR-IDLEVNR                            
162400       MOVE OBKR-BERADREF       TO VOR-BERADREF                           
162500       MOVE OBKR-KVPRERO        TO VOR-KVBEART-URSP                       
162600                                   VOR-KVBEART                            
162700       MOVE 0                   TO VOR-KVPREAVB                           
162800                                   VOR-KVBEART-Q                          
162900       MOVE OBKR-IDDC           TO VOR-IDDC                               
163000       MOVE SPACE               TO VOR-IDUSER                             
163100       MOVE OBKR-KDORDBEK       TO VOR-KDORDBEK                           
163200       MOVE OBKR-KDPRTYP        TO VOR-KDPRTYP                            
163300       MOVE '7'                 TO VOR-KDVORATG                           
163400       MOVE OBKR-PRARTNTO       TO VOR-PRARTNTO                           
163500       MOVE '  '                TO VOR-TEVORMRK                           
163600       MOVE '  '                TO VOR-TEVORMRK-SC                        
163700       MOVE DAGENS-DATUM        TO VOR-TIKLAR                             
163800       COMPUTE VOR-TIKLATID = VOR-TID-OBKR                                
163900                              / 100                                       
164000       END-COMPUTE                                                        
164100       MOVE 0                   TO VOR-TIUPPDAT                           
164200       MOVE 0                   TO VOR-TIUPPTID                           
164300       MOVE OBKR-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                         
164400       MOVE OHUV-FLVORFK        TO VOR-FLVORFK                            
164500                                                                          
164600       PERFORM IMS-ISRT-WDA601                                            
164700       PERFORM UNTIL ISRT-OK                                              
164800          ADD +1              TO VOR-TIREGTID-URSP                        
164900                                 VOR-TIREGTID-LEV                         
165000          PERFORM IMS-ISRT-WDA601                                         
165100       END-PERFORM                                                        
165200     END-IF                                                               
165400     MOVE OBKR-IDDISTR          TO VOR-IDDISTR                            
165500     MOVE OBKR-IDKUNDNR         TO VOR-IDKUNDNR                           
165600     MOVE OBKR-IDKUNDRF         TO VOR-IDKUNDRF                           
165700     MOVE OBKR-TIREGDAT         TO VOR-TIREGDAT-URSP                      
165800     IF OBKR-IDARTNR-TILLK > 0                                            
165900       MOVE OBKR-IDARTNR-TILLK TO VOR-IDARTNR                             
166000     ELSE                                                                 
166100       MOVE OBKR-IDARTNR      TO VOR-IDARTNR                              
166200     END-IF                                                               
166300     MOVE VOR-TID-ORAD          TO VOR-TIREGTID-URSP                      
166400     MOVE DAGENS-DATUM          TO VOR-TIREGDAT-AVV                       
166500     ADD +1                     TO VOR-TID-OBKR                           
166600     MOVE VOR-TID-OBKR          TO VOR-TIREGTID-AVV                       
166700     SUBTRACT DAGENS-DATUM FROM 9999999                                   
166800                            GIVING VOR-TIREGDAT-AVV9                      
166900     SUBTRACT VOR-TID-OBKR FROM 999999999                                 
167000                            GIVING VOR-TIREGTID-AVV9                      
167100     MOVE '0000000   '          TO VOR-IDKUNDRF-LEV                       
167200     MOVE 0                     TO VOR-TIREGDAT-LEV                       
167300     MOVE 0                     TO VOR-TIREGTID-LEV                       
167400     MOVE S04-IDANSK            TO VOR-IDANSK                             
167500                                                                          
167600*    IF W-IDDISTR NOT = VOR-IDDISTR                                       
167700        MOVE VOR-IDDISTR  TO W-IDDISTR-P4                                 
167800        PERFORM IMS-GU-WDP4A1                                             
167900        IF SEGMENT-SAKNAS                                                 
168000           MOVE DEF-IDROLL TO SEQA-IDROLL                                 
168100        END-IF                                                            
168200*    END-IF                                                               
168300     MOVE SEQA-IDROLL     TO VOR-IDROLL                                   
168400                                                                          
168500     MOVE S04-IDLEVNR           TO VOR-IDLEVNR                            
168600     MOVE OBKR-BERADREF         TO VOR-BERADREF                           
168700     IF  OBKR-KDORDBEK = 92                                               
168800       ADD OBKR-KVPREAVB                                                  
168900           OBKR-KVPRERO     GIVING VOR-KVBEART-URSP                       
169000       MOVE OBKR-KVPRERO        TO VOR-KVBEART                            
169100                                   VOR-KVBEART-Q                          
169200       MOVE 0                   TO VOR-KVPREAVB                           
169300     ELSE                                                                 
169400       MOVE OBKR-KVBEART        TO VOR-KVBEART-URSP                       
169500                                   VOR-KVBEART                            
169600                                   VOR-KVBEART-Q                          
169700       MOVE 0                   TO VOR-KVPREAVB                           
169800     END-IF                                                               
169900     MOVE OBKR-IDDC             TO VOR-IDDC                               
170000     MOVE SPACE                 TO VOR-IDUSER                             
170100     MOVE OBKR-KDORDBEK         TO VOR-KDORDBEK                           
170200     MOVE OBKR-KDPRTYP          TO VOR-KDPRTYP                            
170300     MOVE '0'                   TO VOR-KDVORATG                           
170400     MOVE OBKR-PRARTNTO         TO VOR-PRARTNTO                           
170500     MOVE '  '                  TO VOR-TEVORMRK                           
170600     MOVE '  '                  TO VOR-TEVORMRK-SC                        
170700     MOVE 0                     TO VOR-TIKLAR                             
170800     MOVE 0                     TO VOR-TIKLATID                           
170900     MOVE 0                     TO VOR-TIUPPDAT                           
171000     MOVE 0                     TO VOR-TIUPPTID                           
171100     MOVE OBKR-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                           
171200     MOVE OHUV-FLVORFK          TO VOR-FLVORFK                            
171300                                                                          
171400     COMPUTE CLAG-KVVORKO       = S04-KVVORKO                             
171500                                + VOR-KVBEART-Q                           
171600                                - VOR-KVPREAVB                            
171700     END-COMPUTE                                                          
171800                                                                          
171900     PERFORM IMS-ISRT-WDA601                                              
172000     PERFORM UNTIL ISRT-OK                                                
172100        ADD +1                  TO VOR-TID-OBKR                           
172200                                   VOR-TIREGTID-URSP                      
172300        MOVE VOR-TID-OBKR       TO VOR-TIREGTID-AVV                       
172400        SUBTRACT VOR-TID-OBKR   FROM 999999999                            
172500                            GIVING VOR-TIREGTID-AVV9                      
172600        PERFORM IMS-ISRT-WDA601                                           
172700     END-PERFORM                                                          
172800                                                                          
172900     IF  VOR-K611-FINNS                                                   
173000         PERFORM IMS-REPL-WDK611                                          
173100     END-IF                                                               
173200                                                                          
173300     MOVE OBKR-IDDISTR          TO S05-IDDISTR                            
173400     MOVE OBKR-IDKUNDNR         TO S05-IDKUNDNR                           
173500     MOVE OBKR-IDARTNR          TO S05-IDARTNR                            
173600     MOVE S04-IDANSK            TO S05-IDANSK                             
173700     PERFORM S05-STARTA-W2T191X                                           
173800     .                                                                    
173900     EJECT                                                                
174000                                                                          
174100 DCF-TILL-VORKO SECTION.                                                  
174200                                                                          
174300*--------------------------------------- RADEN KOMMER FRÅN VORKÖN         
174500     IF OBKR-IDARTNR-TILLK > 0                                            
174600       MOVE OBKR-IDARTNR-TILLK TO S04-IDARTNR                             
174700     ELSE                                                                 
174800       MOVE OBKR-IDARTNR      TO S04-IDARTNR                              
174900     END-IF                                                               
175000     PERFORM S04-OBKR-BESTAM-LENVR-ANSK                                   
175100                                                                          
175200     PERFORM S06-SOK-RAD-VORKO                                            
175300                                                                          
175400     IF  TRAFF-VORKO                                                      
175500         IF  OBKR-KDORDBEK = 92                                           
175600           SUBTRACT OBKR-KVPRERO  FROM VOR-KVPREAVB                       
175700           SUBTRACT OBKR-KVPRERO  FROM VOR-KVBEART-Q                      
175800         ELSE                                                             
175900           SUBTRACT OBKR-KVBEART  FROM VOR-KVPREAVB                       
176000           SUBTRACT OBKR-KVBEART  FROM VOR-KVBEART-Q                      
176100         END-IF                                                           
176200                                                                          
176300         MOVE SPACE             TO VOR-TEVORMRK                           
176400         MOVE SPACE             TO VOR-TEVORMRK-SC                        
176500                                                                          
176600         IF  VOR-KVPREAVB = 0                                             
176700             MOVE '7'              TO VOR-KDVORATG                        
176800             IF VOR-TIKLAR = ZERO                                         
176900                MOVE DAGENS-DATUM  TO VOR-TIKLAR                          
177000                COMPUTE VOR-TIKLATID = VOR-TID-OBKR                       
177100                                     / 100                                
177200                END-COMPUTE                                               
177300             END-IF                                                       
177400         END-IF                                                           
177500         PERFORM IMS-REPL-SEQB-WDA601                                     
177600                                                                          
177700         MOVE DAGENS-DATUM      TO VOR-TIREGDAT-AVV                       
177800         ADD +1                 TO VOR-TID-OBKR                           
177900         MOVE VOR-TID-OBKR      TO VOR-TIREGTID-AVV                       
178000         SUBTRACT DAGENS-DATUM FROM 9999999                               
178100                                GIVING VOR-TIREGDAT-AVV9                  
178200         SUBTRACT VOR-TID-OBKR FROM 999999999                             
178300                                GIVING VOR-TIREGTID-AVV9                  
178400         MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                       
178500         MOVE 0                 TO VOR-TIREGDAT-LEV                       
178600         MOVE 0                 TO VOR-TIREGTID-LEV                       
178700         MOVE 0                 TO VOR-TIKLAR                             
178800         MOVE 0                 TO VOR-TIKLATID                           
178900         IF  OBKR-KDORDBEK = 92                                           
179000           MOVE OBKR-KVPRERO    TO VOR-KVBEART                            
179100                                   VOR-KVBEART-Q                          
179200         ELSE                                                             
179300           MOVE OBKR-KVBEART    TO VOR-KVBEART                            
179400                                   VOR-KVBEART-Q                          
179500         END-IF                                                           
179600         MOVE 0                 TO VOR-KVPREAVB                           
179700         MOVE OBKR-IDDC         TO VOR-IDDC                               
179800         MOVE SPACE             TO VOR-IDUSER                             
179900         MOVE OBKR-KDORDBEK     TO VOR-KDORDBEK                           
180000         MOVE '0'               TO VOR-KDVORATG                           
180100         MOVE SPACE             TO VOR-TEVORMRK                           
180200         MOVE SPACE             TO VOR-TEVORMRK-SC                        
180300         MOVE OHUV-FLVORFK      TO VOR-FLVORFK                            
180400                                                                          
180500         PERFORM IMS-ISRT-WDA601                                          
180600         PERFORM UNTIL ISRT-OK                                            
180700            ADD +1              TO VOR-TID-OBKR                           
180800                                   VOR-TIREGTID-URSP                      
180900            MOVE VOR-TID-OBKR   TO VOR-TIREGTID-AVV                       
181000            SUBTRACT VOR-TID-OBKR FROM 999999999                          
181100                                GIVING VOR-TIREGTID-AVV9                  
181200            PERFORM IMS-ISRT-WDA601                                       
181300         END-PERFORM                                                      
181400     ELSE                                                                 
181500*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
181600*                                        BORDE NOG INTE FÖREKOMMA         
181700       MOVE OBKR-IDDISTR        TO VOR-IDDISTR                            
181800       MOVE OBKR-IDKUNDNR       TO VOR-IDKUNDNR                           
181900       MOVE OBKR-IDKUNDRF       TO VOR-IDKUNDRF                           
182000       MOVE OBKR-TIREGDAT       TO VOR-TIREGDAT-URSP                      
182100       MOVE OBKR-IDARTNR        TO VOR-IDARTNR                            
182200       MOVE VOR-TID-ORAD        TO VOR-TIREGTID-URSP                      
182300       MOVE 0                   TO VOR-TIREGDAT-AVV                       
182400       MOVE 0                   TO VOR-TIREGTID-AVV                       
182500       SUBTRACT 0            FROM 9999999                                 
182600                              GIVING VOR-TIREGDAT-AVV9                    
182700       SUBTRACT 0            FROM 999999999                               
182800                              GIVING VOR-TIREGTID-AVV9                    
182900       MOVE OBKR-IDKUNDRF       TO VOR-IDKUNDRF-LEV                       
183000       MOVE OBKR-TIREGDAT       TO VOR-TIREGDAT-LEV                       
183100       MOVE VOR-TID-ORAD        TO VOR-TIREGTID-LEV                       
183200       MOVE S04-IDANSK          TO VOR-IDANSK                             
183300                                                                          
183400*      IF W-IDDISTR NOT = VOR-IDDISTR                                     
183500          MOVE VOR-IDDISTR  TO W-IDDISTR-P4                               
183600          PERFORM IMS-GU-WDP4A1                                           
183700          IF SEGMENT-SAKNAS                                               
183800             MOVE DEF-IDROLL TO SEQA-IDROLL                               
183900          END-IF                                                          
184000*      END-IF                                                             
184100       MOVE SEQA-IDROLL     TO VOR-IDROLL                                 
184200                                                                          
184300       MOVE S04-IDLEVNR         TO VOR-IDLEVNR                            
184400       MOVE OBKR-BERADREF       TO VOR-BERADREF                           
184500       IF  OBKR-KDORDBEK = 92                                             
184600         MOVE OBKR-KVPRERO      TO VOR-KVBEART-URSP                       
184700                                   VOR-KVBEART                            
184800       ELSE                                                               
184900         MOVE OBKR-KVBEART      TO VOR-KVBEART-URSP                       
185000                                   VOR-KVBEART                            
185100       END-IF                                                             
185200       MOVE 0                   TO VOR-KVPREAVB                           
185300                                   VOR-KVBEART-Q                          
185400       MOVE OBKR-IDDC           TO VOR-IDDC                               
185500       MOVE SPACE               TO VOR-IDUSER                             
185600       MOVE OBKR-KDORDBEK       TO VOR-KDORDBEK                           
185700       MOVE OBKR-KDPRTYP        TO VOR-KDPRTYP                            
185800       MOVE '7'                  TO VOR-KDVORATG                          
185900       MOVE OBKR-PRARTNTO       TO VOR-PRARTNTO                           
186000       MOVE '  '                TO VOR-TEVORMRK                           
186100       MOVE '  '                TO VOR-TEVORMRK-SC                        
186200       MOVE 0                   TO VOR-TIUPPDAT                           
186300       MOVE 0                   TO VOR-TIUPPTID                           
186400       IF VOR-TIKLAR = ZERO                                               
186500          MOVE DAGENS-DATUM     TO VOR-TIKLAR                             
186600          COMPUTE VOR-TIKLATID  = VOR-TID-OBKR                            
186700                                   / 100                                  
186800          END-COMPUTE                                                     
186900       END-IF                                                             
187000       MOVE OBKR-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                         
187100       MOVE OHUV-FLVORFK        TO VOR-FLVORFK                            
187200       PERFORM IMS-ISRT-WDA601                                            
187300       PERFORM UNTIL ISRT-OK                                              
187400          ADD +1              TO VOR-TIREGTID-URSP                        
187500                                 VOR-TIREGTID-LEV                         
187600          PERFORM IMS-ISRT-WDA601                                         
187700       END-PERFORM                                                        
187800                                                                          
187900*--------------------------------------- EJ TRÄFF, AVVIK RAD              
188000       MOVE DAGENS-DATUM      TO VOR-TIREGDAT-AVV                         
188100       ADD +1                 TO VOR-TID-OBKR                             
188200       MOVE VOR-TID-OBKR      TO VOR-TIREGTID-AVV                         
188300       SUBTRACT DAGENS-DATUM FROM 9999999                                 
188400                              GIVING VOR-TIREGDAT-AVV9                    
188500       SUBTRACT VOR-TID-OBKR FROM 999999999                               
188600                              GIVING VOR-TIREGTID-AVV9                    
188700       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
188800       MOVE 0                 TO VOR-TIREGDAT-LEV                         
188900       MOVE 0                 TO VOR-TIREGTID-LEV                         
189000       IF  OBKR-KDORDBEK = 92                                             
189100         MOVE OBKR-KVPRERO      TO VOR-KVBEART                            
189200                                   VOR-KVBEART-Q                          
189300       ELSE                                                               
189400         MOVE OBKR-KVBEART      TO VOR-KVBEART                            
189500                                   VOR-KVBEART-Q                          
189600       END-IF                                                             
189700       MOVE 0                 TO VOR-KVPREAVB                             
189800       MOVE OBKR-IDDC         TO VOR-IDDC                                 
189900       MOVE SPACE             TO VOR-IDUSER                               
190000       MOVE OBKR-KDORDBEK     TO VOR-KDORDBEK                             
190100       MOVE '0'               TO VOR-KDVORATG                             
190200       MOVE 0                 TO VOR-TIKLAR                               
190300       MOVE 0                 TO VOR-TIKLATID                             
190400       MOVE OHUV-FLVORFK      TO VOR-FLVORFK                              
190600       PERFORM IMS-ISRT-WDA601                                            
190700       PERFORM UNTIL ISRT-OK                                              
190800          ADD +1              TO VOR-TID-OBKR                             
190900                                 VOR-TIREGTID-URSP                        
191000          MOVE VOR-TID-OBKR   TO VOR-TIREGTID-AVV                         
191100          SUBTRACT VOR-TID-OBKR FROM 999999999                            
191200                              GIVING VOR-TIREGTID-AVV9                    
191300          PERFORM IMS-ISRT-WDA601                                         
191400       END-PERFORM                                                        
191500     END-IF                                                               
191600                                                                          
191700     COMPUTE CLAG-KVVORKO       = S04-KVVORKO                             
191800                                + VOR-KVBEART-Q                           
191900                                - VOR-KVPREAVB                            
192000     END-COMPUTE                                                          
192100                                                                          
192200     IF  VOR-K611-FINNS                                                   
192300         PERFORM IMS-REPL-WDK611                                          
192400     END-IF                                                               
192500                                                                          
192600     MOVE OBKR-IDDISTR          TO S05-IDDISTR                            
192700     MOVE OBKR-IDKUNDNR         TO S05-IDKUNDNR                           
192800     MOVE OBKR-IDARTNR          TO S05-IDARTNR                            
192900     MOVE S04-IDANSK            TO S05-IDANSK                             
193000     PERFORM S05-STARTA-W2T191X                                           
193100     .                                                                    
193200     EJECT                                                                
193300 DCG-SKAPA-TACD-402          SECTION.                                     
193400                                                                          
193500     IF OBKR-IDARTNR NOT = CURRENT-IDARTNR                                
193600        MOVE OBKR-IDARTNR         TO CURRENT-IDARTNR                      
193700        MOVE ZERO                 TO CURRENT-IDARTNR-TILLK                
193800        MOVE ZERO                 TO WS-TACDIS-SEQ                        
193900     END-IF                                                               
194000     IF OBKR-IDARTNR-TILLK NOT = ZERO                                     
194100        IF OBKR-IDARTNR-TILLK NOT = CURRENT-IDARTNR-TILLK                 
194200           ADD 1 TO WS-TACDIS-SEQ                                         
194300           MOVE JA TO WS-NY-TILLK                                         
194400           MOVE OBKR-IDARTNR-TILLK   TO CURRENT-IDARTNR-TILLK             
194500        ELSE                                                              
194600           MOVE NEJ TO WS-NY-TILLK                                        
194700        END-IF                                                            
194800     ELSE                                                                 
194900        MOVE NEJ                  TO WS-NY-TILLK                          
195000     END-IF                                                               
195100     MOVE 'PU1'                   TO 402-IDPTYP                           
195200     MOVE 01                      TO 402-IDVTYP-TACDIS                    
195300     MOVE 20                      TO WS-TIAA                              
195400     MOVE DAGENS-DATUM            TO WS-TIAAMMDD                          
195500     MOVE WS-TIAAAAMMDD           TO 402-DAREGDAT                         
195610     MOVE OBKR-IDDISTR            TO 402-IDDISTR                          
195700     MOVE OBKR-IDKUNDNR           TO 402-IDKUNDNR                         
195800     MOVE OBKR-IDORDNR7           TO 402-IDORDNR7                         
195900                                                                          
196000     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
196100     MOVE OBKR-IDARTNR            TO CIA-IDARTBET-IN                      
196200     CALL W009CIA              USING CIA-W009CIA                          
196300     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET                         
196400                                                                          
196500     MOVE OBKR-KDORDBEK           TO 402-KDORDBEK                         
196600     IF OBKR-KDORDBEK = 41 OR 61                                          
196700*!>> 402-KVBEART SKA VARA DEN ERSATTA ART. BESTÄLLDA ANTAL VARS           
196800*!>> TRANS EJ SKICKAS TILL TACDIS. DÄRMED LAGRING I DC-SECTION            
196900       CONTINUE                                                           
197000     ELSE                                                                 
197100       MOVE OBKR-KVBEART          TO 402-KVBEART                          
197200     END-IF                                                               
197300                                                                          
197400*    LDC ARTIKELBYTE FÅR BARA EN Q1-POST OCH GER LOW-VALUE                
197500*    I 402-KVBEART                                                        
197600*    VI FÖRSÖKER RÄTTA TILL DET                                           
197700                                                                          
197800     IF 402-KVBEART NOT NUMERIC AND                                       
197900        OBKR-KDORDBEK = 41      AND                                       
198000        OBKR-IDARTNR-TILLK > ZERO                                         
198100        MOVE OBKR-KVBEART-TILLK   TO 402-KVBEART                          
198200     END-IF                                                               
198300                                                                          
198400     IF WS-NY-TILLK = JA                                                  
198500        MOVE WS-TACDIS-SEQ        TO 402-IDSEKVNR                         
198600     ELSE                                                                 
198700        MOVE 1                    TO 402-IDSEKVNR                         
198800     END-IF                                                               
198900                                                                          
199000     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
199100     MOVE OBKR-IDARTNR-TILLK      TO CIA-IDARTBET-IN                      
199200     CALL W009CIA              USING CIA-W009CIA                          
199300     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET-TILLK                   
199400                                                                          
199500     MOVE OBKR-KVBEART-Q          TO 402-KVLEVART                         
199600     MOVE OBKR-IDDC               TO 402-IDDC                             
199700     MOVE OBKR-TIDLEVDAT          TO 402-DADLEVDAT                        
199800     IF 402-DADLEVDAT > ZERO                                              
199900        ADD 20000000              TO 402-DADLEVDAT                        
200000     END-IF                                                               
200100                                                                          
200200     IF WS-KV402 = 0                                                      
200300       PERFORM S21-SEND-OPEN                                              
200400       MOVE OBKR-IDKUNDNR         TO WS-IDKUNDNR                          
200500       MOVE OBKR-IDORDNR7         TO WS-IDORDNR7                          
200600       PERFORM S22-PUT-HEADER                                             
200700     END-IF                                                               
200800     ADD +1                       TO WS-KV402                             
200900     PERFORM S25-PUT-LINE                                                 
201000     .                                                                    
201100     EJECT                                                                
201200 DCH-LAES-KUNDREGISTER       SECTION.                                     
201300                                                                          
201400     MOVE OBKR-IDDISTR  TO W-IDDISTR                                      
201500     MOVE OBKR-IDKUNDNR TO W-IDKUNDNR                                     
201600                                                                          
201700     PERFORM IMS-GU-WDB201                                                
201800     IF SEGMENT-SAKNAS                                                    
201900        MOVE NEJ TO GMT-FLOBKR-TACD                                       
202000     END-IF                                                               
202100     .                                                                    
202200     EJECT                                                                
202300 DCI-SPARA-ORDERID  SECTION.                                              
202400                                                                          
202500     MOVE 1 TO KUNDRF-IX                                                  
202600     PERFORM UNTIL KUNDRF-IX > KUNDRF-IX-MAX OR                           
202700                   W-IDKUNDRF(KUNDRF-IX) = SPACE OR                       
202800                   W-IDKUNDRF(KUNDRF-IX) = OBKR-IDKUNDRF-RO               
202900                                                                          
203000        ADD +1 TO KUNDRF-IX                                               
203100     END-PERFORM                                                          
203200                                                                          
203300     IF KUNDRF-IX NOT > KUNDRF-IX-MAX AND                                 
203400        W-IDKUNDRF(KUNDRF-IX) = SPACE                                     
203500                                                                          
203600        MOVE OBKR-IDKUNDRF-RO TO W-IDKUNDRF(KUNDRF-IX)                    
203700     END-IF                                                               
203800     .                                                                    
203900     EJECT                                                                
204000 DD-SPARA-WDQ101-NYCKLAR     SECTION.                                     
204100                                                                          
204200     MOVE 'WLORQM'             TO  MOD4293-MID-IDDB                       
204300     MOVE OBKR-IDORDER         TO  MOD4293-MID-IDORDER                    
204400     MOVE OBKR-IDARTNR         TO  MOD4293-MID-IDARTNR                    
204500     MOVE OBKR-IDLOPNR         TO  W-IDLOPNR                              
204600     MOVE W-IDLOPNR (2:2)      TO  MOD4293-MID-IDLOPNR                    
204700     MOVE OBKR-IDSEKVNR        TO  MOD4293-MID-IDSEKVNR                   
204800     MOVE OBKR-IDDC            TO  MOD4293-MID-IDDC                       
204900     MOVE OBKR-KDORDBEK        TO  MOD4293-MID-KDORDBEK                   
205000     MOVE ZERO                 TO  MOD4293-MID-ADGANG                     
205100                                   MOD4293-MID-ADLAGOMR                   
205200                                   MOD4293-MID-ADPLATS                    
205300     MOVE MID-IDDISTR          TO  MOD4293-MID-IDDISTR                    
205400     MOVE MID-IDKUNDNR         TO  MOD4293-MID-IDKUNDNR                   
205500     MOVE MID-IDKUNDRF         TO  MOD4293-MID-IDKUNDRF                   
205600     .                                                                    
205700     EJECT                                                                
205800                                                                          
205900 E-UPPD-OHUV-SKAPA-HTR             SECTION.                               
206000                                                                          
206100     PERFORM EA-SKAPA-TRANS                                               
206200                                                                          
206300     PERFORM IMS-GET-ORQI01-OHUV-KVAL                                     
206400                                                                          
206500     MOVE JA                   TO OHUV-FLKLAR                             
206600     IF OHUV-KVORDTIL IS NUMERIC                                          
206700        ADD +1                 TO OHUV-KVORDTIL                           
206800     ELSE                                                                 
206900        MOVE +1                TO OHUV-KVORDTIL                           
207000     END-IF                                                               
207100                                                                          
207200     IF OHUV-BEKUNDRF = 'DL REL'                                          
207300        PERFORM EB-JUSTERA-KVORDTIL                                       
207400     END-IF                                                               
207500                                                                          
210700                                                                          
210800     IF OHUV-TIREGDAT-STO = ZERO                                          
210900       MOVE MSGI-TILOKDAT            TO OHUV-TIREGDAT-STO                 
211000     END-IF                                                               
211100                                                                          
211200     IF OHUV-TIREGTID-STO = ZERO                                          
211300       MOVE MSGI-TILOKTID            TO WS-TIHHMM                         
211400       MOVE WS-TIHHMMSS              TO OHUV-TIREGTID-STO                 
211500     END-IF                                                               
211600                                                                          
211700     PERFORM IMS-REPL-ORQI01-OHUV                                         
211800                                                                          
211900**Order status validation/update                                          
212000     PERFORM IMS-GHNP-ORQI12-OKVAL-FIRST                                  
212100     PERFORM UNTIL SEGMENT-SAKNAS                                         
212200        MOVE ARB-KDORDSTA-O       TO ARB-KDORDSTA                         
212300        MOVE SPACE                TO ARB-KDORDSTA-O                       
212400        IF ARB-KDORDSTA = SPACE                                           
212500          IF ARB-KDTRPKAT = 'A'                                           
212600             MOVE 'R'    TO ARB-KDORDSTA                                  
212700          ELSE                                                            
212800             IF ARB-TIRFS  = ZERO AND                                     
212900                ARB-DATRPAVD = ZERO AND                                   
213000                ARB-TIHHMM   = ZERO                                       
213100                MOVE ARB-KDTRPKAT     TO ARB-KDORDSTA                     
213200             ELSE                                                         
213300                MOVE 'R'              TO ARB-KDORDSTA                     
213400             END-IF                                                       
213500          END-IF                                                          
213600        END-IF                                                            
213700        PERFORM IMS-REPL-ORQI12                                           
213800        PERFORM IMS-GHNP-ORQI12-OKVAL                                     
213900     END-PERFORM                                                          
213901**                                                                        
213902     .                                                                    
213903     EJECT                                                                
213904 EA-SKAPA-TRANS                  SECTION.                                 
213905                                                                          
213906     MOVE SPACE                TO  FIL-WDR601-DATA                        
213907     MOVE OHUV-BEKUNDRF        TO  200-BEKUNDRF                           
213908     MOVE OHUV-BEVARREF        TO  200-BEVARREF                           
213909     MOVE OHUV-FLRESTN         TO  200-FLRESTN                            
213910     MOVE OHUV-IDDISTR         TO  200-IDDISTR                            
213911     MOVE OHUV-IDKONTO         TO  200-IDKONTO                            
213912     MOVE OHUV-IDKST           TO  200-IDKST                              
213913     MOVE OHUV-IDKUNDNR        TO  200-IDKUNDNR                           
213914     MOVE MID-IDKUNDRF         TO  200-IDKUNDRF                           
213915     MOVE OHUV-IDORDER         TO  200-IDORDER                            
213916     MOVE OHUV-IDSKYLT         TO  200-IDSKYLT                            
213917     IF OHUV-IDDC-TVS > ZERO                                              
213918        MOVE OHUV-IDDC-TVS     TO WS-IDDC                                 
213919     ELSE                                                                 
213920        MOVE OHUV-IDDC-PRIM     TO WS-IDDC                                
213921     END-IF                                                               
213922     MOVE WS-IDDC              TO  200-IDDC                               
213923     MOVE OHUV-KDFAKTYP        TO  200-KDFAKTYP                           
213924                                                                          
214000     MOVE WS-IDDC              TO  SOEK-IDDC                              
214100     PERFORM S01-HAEMTA-FRAN-RAETT-ARBTAB                                 
214200     MOVE ARB-KDFRAKT          TO  200-KDFRAKT                            
214300     MOVE ARB-KDROPACK         TO  200-KDROPACK                           
214400     MOVE ARB-TIRFS            TO  200-TIRFS                              
214500     MOVE ARB-DATRPAVD (3:6)   TO  200-TIAAMMDD                           
214600     MOVE ARB-TIHHMM           TO  200-TIHHMM                             
214610                                                                          
214700     MOVE OHUV-KDORDKL         TO  200-KDORDKL                            
214800     MOVE OHUV-KDTULLVE        TO  200-KDTULLVE                           
214900     MOVE OHUV-TIREGDAT        TO  200-TIREGDAT                           
216100                                                                          
216200     ADD +1                    TO  IX-RAD                                 
216300     ADD +1                    TO  FIL-IDSEKVNR                           
216400     MOVE '200'                TO  FIL-CT-IDPTYP                          
216500     PERFORM IMS-ISRT-FILA01                                              
216600     PERFORM UNTIL SEGMENT-FINNS                                          
216700        ADD +1 TO FIL-IDSEKVNR                                            
216800        PERFORM IMS-ISRT-FILA01                                           
216900     END-PERFORM                                                          
217000     .                                                                    
217100     EJECT                                                                
217200                                                                          
217300 EB-JUSTERA-KVORDTIL        SECTION.                                      
217400                                                                          
217500     MOVE 1 TO KUNDRF-IX                                                  
217600     PERFORM UNTIL KUNDRF-IX > KUNDRF-IX-MAX OR                           
217700                   W-IDKUNDRF(KUNDRF-IX) = SPACE                          
217800        ADD 1 TO KUNDRF-IX                                                
217900     END-PERFORM                                                          
218000                                                                          
218100     SUBTRACT 1 FROM KUNDRF-IX                                            
218200     MOVE KUNDRF-IX TO OHUV-KVORDTIL                                      
218300     .                                                                    
218400     EJECT                                                                
218500 G-STARTA-W4T353X           SECTION.                                      
218600                                                                          
218700     MOVE ALL '+'                 TO MOD4353-MID-W4I35301                 
218800     MOVE SPACE                   TO MOD4353-MID-IDPRC-UT                 
218900                                     MOD4353-MID-IDPLKLST-UT              
219000                                     MOD4353-MID-IDTRP-UT                 
219100                                     MOD4353-MID-TIAAMMDD-UT              
219200                                     MOD4353-MID-TIHHMM-UT                
219300                                     MOD4353-MID-IDDISTR-UT               
219400                                     MOD4353-MID-IDKUNDNR-UT              
219500                                     MOD4353-MID-IDORDNR7-UT              
219600     MOVE NEJ                     TO MOD4353-MID-MIXAT                    
219700     MOVE '000000'                TO MOD4353-MID-ANTPLK                   
219800     MOVE OHUV-IDDISTR            TO WS-IDDISTR                           
219900     MOVE WS-IDDISTR              TO MOD4353-MID-IDDISTR-IN               
220000     MOVE OHUV-IDKUNDNR           TO WS-IDKUNDNR                          
220100     MOVE WS-IDKUNDNR             TO MOD4353-MID-IDKUNDNR-IN              
220200     MOVE MID-IDKUNDRF(1:7)       TO MOD4353-MID-IDORDNR7-IN              
220300     MOVE WS-IDDC                 TO MOD4353-MID-IDDC-IN                  
220400     MOVE '999'                   TO MOD4353-MID-KDPRT-PU                 
220500     MOVE '999'                   TO MOD4353-MID-KDPRT-PLE                
220600                                                                          
220700     COMPUTE MSG-KVLL = LENGTH OF MOD4353-MID-W4I35301 + 17               
220800     MOVE    'W4T353X '           TO MSG-KDTRANS-1                        
220900     MOVE    '4293'               TO MSG-IDTRANS-1                        
221000     MOVE    '1'                  TO MSG-KDMFSFOR-1                       
221100     MOVE    MOD4353-MID-W4I35301 TO MSG-INDATA-MINUS-1-TRANSKOD          
221200     PERFORM IMS-ISRT-ALT2-MSG-4353                                       
221300     .                                                                    
221400     EJECT                                                                
221500 H-BEHANDLA-DIRLEV          SECTION.                                      
221600                                                                          
221700     PERFORM IMS-GNP-ORQI11-FIRST                                         
221800     PERFORM UNTIL SEGMENT-SAKNAS OR DIRLEV-FINNS                         
221900       IF DIRL-KVRADER > ZERO                                             
222010         MOVE JA               TO DIRLEV-SW                               
222100       END-IF                                                             
222200       PERFORM IMS-GNP-ORQI11                                             
222300     END-PERFORM                                                          
222400     .                                                                    
222500     EJECT                                                                
222600 I-STARTA-W4T695X           SECTION.                                      
222700                                                                          
222800     COMPUTE MSG-KVLL = LENGTH OF MOD4695-MID-W4I69501 + 17               
222900     MOVE    'W4T695X '           TO MSG-KDTRANS-1                        
223000     MOVE    '4293'               TO MSG-IDTRANS-1                        
223100     MOVE    '1'                  TO MSG-KDMFSFOR-1                       
223200     MOVE    OHUV-IDORDER         TO MOD4695-MID-IDORDER                  
223300     MOVE    DIRL-IDDC            TO MOD4695-MID-IDDC                     
223400     MOVE    MOD4695-MID-W4I69501 TO MSG-INDATA-MINUS-1-TRANSKOD          
223500                                                                          
223600     PERFORM IMS-ISRT-ALT3-MSG-4695                                       
223700     .                                                                    
223800     EJECT                                                                
223900 J-STARTA-W4T293X           SECTION.                                      
224000                                                                          
224100     COMPUTE MSG-KVLL = LENGTH OF MOD4293-MID-W4I29301 + 17               
224200     MOVE    'W4T293X '           TO MSG-KDTRANS-1                        
224300     MOVE    '4293'               TO MSG-IDTRANS-1                        
224400     MOVE    '1'                  TO MSG-KDMFSFOR-1                       
224500     MOVE    MOD4293-MID-W4I29301 TO MSG-INDATA-MINUS-1-TRANSKOD          
224600                                                                          
224700     PERFORM IMS-ISRT-ALT1-MSG-4293                                       
224800     .                                                                    
224900     EJECT                                                                
225000 S02-STARTA-W2T109X  SECTION.                                             
225100                                                                          
225200     COMPUTE MSG-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                 
225300     MOVE 'W2T109X '            TO MSG-KDTRANS-1                          
225400     MOVE '4293'                TO MSG-IDTRANS-1                          
225500     MOVE '1'                   TO MSG-KDMFSFOR-1                         
225600     MOVE 2109-MID2-W2I10902    TO MSG-INDATA-MINUS-1-TRANSKOD            
225700                                                                          
225800     PERFORM IMS-PURG-ALT4-MSG-2109                                       
225900                                                                          
226000     MOVE SPACE                 TO 2109-MID2-W2I10902                     
226100     MOVE +1                    TO 2109-IX                                
226200     .                                                                    
226300     EJECT                                                                
226400 S05-STARTA-W2T191X  SECTION.                                             
226500                                                                          
226600     MOVE +1                    TO 2191-MID-KDCLAGER                      
226700     MOVE S05-IDARTNR-X         TO 2191-MID-IDARTNR                       
226800     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
226900                                   2191-MID-TISENBEK-KL                   
227000     MOVE SPACE                 TO 2191-MID-IDKR                          
227100     MOVE S05-IDANSK-X          TO 2191-MID-IDANSK                        
227200     MOVE '500'                 TO 2191-MID-KDLARM                        
227300     MOVE S05-IDDISTR-X         TO 2191-MID-IDDISTR                       
227400     MOVE S05-IDKUNDNR-X        TO 2191-MID-IDKUNDNR                      
227500     MOVE OBKR-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
227600     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
227700     MOVE WC-CDC-SE             TO 2191-MID-IDDC                          
227800     MOVE SPACE                 TO 2191-MID-IDLEVNR                       
227900                                                                          
228000     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
228100     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
228200     MOVE '4293'                TO MSG-IDTRANS-1                          
228300     MOVE '1'                   TO MSG-KDMFSFOR-1                         
228400     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
228500                                                                          
228600     PERFORM IMS-PURG-ALT5-MSG-2191                                       
228700                                                                          
228800     MOVE SPACE                 TO 2191-MID-W2I19101                      
228900     .                                                                    
229000     EJECT                                                                
229100 S01-HAEMTA-FRAN-RAETT-ARBTAB SECTION.                                    
229200                                                                          
229300     IF SOEK-IDDC = ARB-IDDC                                              
229400*WDQ212 ALREADY HAS THE CORRECT DC DATA, NO NEED TO FETCH AGAIN           
229500        CONTINUE                                                          
229600     ELSE                                                                 
229700        MOVE SOEK-IDDC  TO W-IDDC                                         
229800        PERFORM IMS-GHNP-ORQI12-KVAL-FIRST                                
229900        IF SEGMENT-SAKNAS                                                 
230000           MOVE OHUV-IDDC-PRIM TO W-IDDC                                  
230100           PERFORM IMS-GHNP-ORQI12-KVAL-FIRST                             
230200           IF SEGMENT-SAKNAS                                              
230300              MOVE '** HOME DC DOESNT EXIST IN WDQ212 **'                 
230400                                      TO FELTEXT                          
230600              CALL FELLOG                                                 
230700           END-IF                                                         
230800        END-IF                                                            
230801     END-IF                                                               
230802     .                                                                    
230803     EJECT                                                                
230804 S04-BESTAM-LENVR-ANSK SECTION.                                           
230805                                                                          
230806     MOVE S04-IDARTNR    TO W-IDARTNR                                     
230807     PERFORM IMS-GU-WDK601                                                
230808     MOVE ART-IDLEVNR    TO S04-IDLEVNR                                   
230900                                                                          
231000     PERFORM IMS-GNP-WDK611                                               
231100     IF  SEGMENT-FINNS                                                    
231200        MOVE CLAG-IDANSK TO S04-IDANSK                                    
231300     ELSE                                                                 
231400        MOVE 0           TO S04-IDANSK                                    
231500     END-IF                                                               
231600     .                                                                    
231700     EJECT                                                                
231800 S04-OBKR-BESTAM-LENVR-ANSK SECTION.                                      
231900                                                                          
232000     MOVE S04-IDARTNR    TO W-IDARTNR                                     
232100     PERFORM IMS-GU-WDK601                                                
232200     MOVE ART-IDLEVNR    TO S04-IDLEVNR                                   
232300                                                                          
232400     PERFORM IMS-GHNP-WDK611                                              
232500     IF  SEGMENT-FINNS                                                    
232600        MOVE JA           TO VOR-K611-FINNS-SW                            
232700        MOVE CLAG-IDANSK  TO S04-IDANSK                                   
232800        MOVE CLAG-KVVORKO TO S04-KVVORKO                                  
232900     ELSE                                                                 
233000        MOVE NEJ          TO VOR-K611-FINNS-SW                            
233100        MOVE 0            TO S04-IDANSK                                   
233200        MOVE 0            TO S04-KVVORKO                                  
233300     END-IF                                                               
233400     .                                                                    
233500     EJECT                                                                
233600 S06-SOK-RAD-VORKO SECTION.                                               
233700                                                                          
233800     MOVE LOW-VALUE      TO W-WDA6BSEQ-MIN-X                              
233900     MOVE HIGH-VALUE     TO W-WDA6BSEQ-MAX-X                              
234000                                                                          
234100     MOVE OBKR-IDDISTR   TO W-A6BSEQ-MIN-IDDISTR                          
234200                            W-A6BSEQ-MAX-IDDISTR                          
234300     MOVE OBKR-IDKUNDNR  TO W-A6BSEQ-MIN-IDKUNDNR                         
234400                            W-A6BSEQ-MAX-IDKUNDNR                         
234500     MOVE OBKR-IDKUNDRF  TO W-A6BSEQ-MIN-IDKUNDRF-LEV                     
234600                            W-A6BSEQ-MAX-IDKUNDRF-LEV                     
234700     MOVE OBKR-TIREGDAT  TO W-A6BSEQ-MIN-TIREGDAT-LEV                     
234800                            W-A6BSEQ-MAX-TIREGDAT-LEV                     
234900     MOVE OBKR-IDARTNR   TO W-A6BSEQ-MIN-IDARTNR                          
235000                            W-A6BSEQ-MAX-IDARTNR                          
235100     MOVE NEJ            TO TRAFF-VORKO-SW                                
235200                                                                          
235300     PERFORM IMS-GHU-SEQB-WDA601                                          
235400     PERFORM UNTIL SEGMENT-SAKNAS                                         
235500                OR END-OF-DATA                                            
235600                OR TRAFF-VORKO                                            
235700       IF  OBKR-KDORDBEK = 92                                             
235800       AND OBKR-KVPREAVB + OBKR-KVPRERO = VOR-KVPREAVB                    
235900           MOVE JA       TO TRAFF-VORKO-SW                                
236000       ELSE                                                               
236100         IF  OBKR-KVBEART = VOR-KVPREAVB                                  
236200             MOVE JA       TO TRAFF-VORKO-SW                              
236300         ELSE                                                             
236400            PERFORM IMS-GHN-SEQB-WDA601                                   
236500         END-IF                                                           
236600       END-IF                                                             
236700     END-PERFORM                                                          
236800     .                                                                    
236900     EJECT                                                                
237000 S10-BO-EVENT SECTION.                                                    
237100                                                                          
238600     IF (ORAD-IDSYSTEM = 'LYNB'  OR 'POLB' OR 'ECOB' OR                   
238700                         'TADB'  OR 'ACCB' OR 'APAB' OR                   
238710                         'APBB'  OR 'APCB' OR 'APDB' OR                   
238720                         'APEB'  OR 'APFB' OR 'APGB' OR                   
238730                         'APHB'  OR 'APIB' OR 'APJB')                     
238740           OR (NON-API-EVENT AND (VOR-SW = NEJ))                          
238800        PERFORM S10A-GET-ORDER-FROM-WDA5                                  
238900     ELSE                                                                 
239000        IF (ORAD-IDSYSTEM = 'LYNV' OR 'POLV' OR 'ECOV' OR                 
239110                            'TADV' OR 'ACCV' OR 'APAV' OR                 
239120                            'APBV' OR 'APCV' OR 'APDV' OR                 
239130                            'APEV' OR 'APFV' OR 'APGV' OR                 
239140                            'APHV' OR 'APIV' OR 'APJV')                   
239150           OR (NON-API-EVENT AND VOR)                                     
239200           PERFORM S10B-GET-ORDER-FROM-WDA6                               
239300        END-IF                                                            
239300     END-IF                                                               
239600     .                                                                    
239700     EJECT                                                                
239800 S10A-GET-ORDER-FROM-WDA5 SECTION.                                        
239900                                                                          
240000     MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                        
240100                                  W-IDDISTR-A5-MAX                        
240200     MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN                       
240300                                  W-IDKUNDNR-A5-MAX                       
240400     MOVE OHUV-KDORDKL         TO W-KDORDKL                               
240500     MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                          
240600                                                                          
240700     PERFORM IMS-GU-WDA501                                                
240800     IF SEGMENT-FINNS                                                     
240801        PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                       
240900           MOVE OHUV-IDDISTR          TO WS-IDDISTR-EVENT                 
241000           MOVE OHUV-IDKUNDNR         TO WS-IDKUNDNR-EVENT                
241100           MOVE RAD-IDORDNR5          TO WS-IDORDNR7-EVENT                
241200           MOVE RAD-TIREGDAT          TO WS-TIREGDAT-EVENT                
241300           MOVE RAD-IDSYSTEM          TO WS-IDEVENTREC                    
241310           SET NON-API-BO-RELEASED  TO TRUE                               
241400           PERFORM S15-CREATE-API-EVENT                                   
241420           PERFORM IMS-GN-WDA501                                          
241430        END-PERFORM                                                       
241440        IF SEGMENT-SAKNAS OR END-OF-DATA                                  
241450           SET BO-RELEASE-EVENT  TO TRUE                                  
241460        END-IF                                                            
241500     END-IF                                                               
241600     .                                                                    
241700     EJECT                                                                
241800 S10B-GET-ORDER-FROM-WDA6 SECTION.                                        
241900                                                                          
242000     MOVE LOW-VALUE      TO W-WDA6BSEQ-MIN-X                              
242100     MOVE HIGH-VALUE     TO W-WDA6BSEQ-MAX-X                              
242200                                                                          
242300     MOVE OHUV-IDDISTR   TO W-A6BSEQ-MIN-IDDISTR                          
242400                            W-A6BSEQ-MAX-IDDISTR                          
242500     MOVE OHUV-IDKUNDNR  TO W-A6BSEQ-MIN-IDKUNDNR                         
242600                            W-A6BSEQ-MAX-IDKUNDNR                         
242700     MOVE OHUV-IDKUNDRF  TO W-A6BSEQ-MIN-IDKUNDRF-LEV                     
242800                            W-A6BSEQ-MAX-IDKUNDRF-LEV                     
242900     PERFORM IMS-GHU-SEQB-WDA601                                          
243000     IF SEGMENT-FINNS                                                     
243100        MOVE OHUV-IDDISTR      TO WS-IDDISTR-EVENT                        
243200        MOVE OHUV-IDKUNDNR     TO WS-IDKUNDNR-EVENT                       
243300        MOVE VOR-IDKUNDRF(1:7) TO WS-IDORDNR7-EVENT                       
243400        MOVE VOR-TIREGDAT-URSP TO WS-TIREGDAT-EVENT                       
243500        MOVE OHUV-IDSYSTEM     TO WS-IDEVENTREC                           
243510        SET NON-API-BO-RELEASED  TO TRUE                                  
243600        PERFORM S15-CREATE-API-EVENT                                      
243610        SET BO-RELEASE-EVENT     TO TRUE                                  
243700     END-IF                                                               
243800     .                                                                    
243900     EJECT                                                                
244000 S15-CREATE-API-EVENT SECTION.                                            
244100                                                                          
244200     MOVE '001'                     TO Z430-REQU-IDMSGVER                 
244300     MOVE 'PurchaseOrder'           TO Z430-REQU-IDEVENT                  
244400     IF WS-IDEVENTREC(1:3) = 'LYN' OR NON-API-EVENT                       
244500        MOVE 'LYNK'                 TO WS-IDEVENTREC                      
244600     ELSE                                                                 
244700        IF WS-IDEVENTREC(1:3) = 'ECO'                                     
244800           MOVE 'ECOM'              TO WS-IDEVENTREC                      
244900        ELSE                                                              
245000           IF WS-IDEVENTREC(1:3) = 'TAD'                                  
245100              MOVE 'TAD '           TO WS-IDEVENTREC                      
245200           ELSE                                                           
245300             IF WS-IDEVENTREC(1:3) = 'ACC'                                
245400                MOVE 'ACC '           TO WS-IDEVENTREC                    
245500             ELSE                                                         
245510               IF WS-IDEVENTREC(1:3) = 'APA'                              
245520                  MOVE 'APA '         TO WS-IDEVENTREC                    
245530               ELSE                                                       
245540                 IF WS-IDEVENTREC(1:3) = 'APB'                            
245550                    MOVE 'APB '       TO WS-IDEVENTREC                    
245560                 ELSE                                                     
245570                   IF WS-IDEVENTREC(1:3) = 'APC'                          
245580                      MOVE 'APC '     TO WS-IDEVENTREC                    
245590                   ELSE                                                   
245591                     IF WS-IDEVENTREC(1:3) = 'APD'                        
245592                        MOVE 'APD '   TO WS-IDEVENTREC                    
245593                     ELSE                                                 
245594                       IF WS-IDEVENTREC(1:3) = 'APE'                      
245595                          MOVE 'APE ' TO WS-IDEVENTREC                    
245596                       ELSE                                               
245597                         IF WS-IDEVENTREC(1:3) = 'APF'                    
245598                            MOVE 'APF '     TO WS-IDEVENTREC              
245599                         ELSE                                             
245600                           IF WS-IDEVENTREC(1:3) = 'APG'                  
245601                              MOVE 'APG '     TO WS-IDEVENTREC            
245602                           ELSE                                           
245603                             IF WS-IDEVENTREC(1:3) = 'APH'                
245604                                MOVE 'APH '       TO WS-IDEVENTREC        
245605                             ELSE                                         
245606                               IF WS-IDEVENTREC(1:3) = 'API'              
245607                                  MOVE 'API '                             
245608                                                  TO WS-IDEVENTREC        
245609                               ELSE                                       
245610                                 IF WS-IDEVENTREC(1:3) = 'APJ'            
245611                                    MOVE 'APJ '                           
245612                                                  TO WS-IDEVENTREC        
245613                                 ELSE                                     
245614                                    MOVE 'POLE'                           
245615                                                  TO WS-IDEVENTREC        
245616                                 END-IF                                   
245617                             END-IF                                       
245618                           END-IF                                         
245619                         END-IF                                           
245620                       END-IF                                             
245621                     END-IF                                               
245622                   END-IF                                                 
245623                 END-IF                                                   
245624               END-IF                                                     
245625             END-IF                                                       
245626           END-IF                                                         
245627          END-IF                                                          
245630        END-IF                                                            
245640     END-IF                                                               
245641                                                                          
245650     MOVE WS-IDEVENTREC             TO Z430-REQU-IDEVENTREC               
245660     MOVE 'UPDATE'                  TO Z430-REQU-IDEVENTTYP               
245670     MOVE FUNCTION CURRENT-DATE     TO Z430-REQU-TIMESTAMP                
245680     MOVE 'WAPIORD'                 TO Z430-REQU-IDCPYTXT                 
245690     MOVE WS-IDAPIORDREF            TO Z430-IDAPIORDREF                   
245691     IF NON-API-EVENT                                                     
245692        IF LYNK-NONAPI-REG-EVENT                                          
245694           MOVE '149'                  TO Z430-IDMSG                      
245695           MOVE 'NEW ORDER REGISTERED'      TO Z430-TEMFSINF              
245696        ELSE                                                              
245697           IF NON-API-BO-RELEASED                                         
245700              MOVE '153'                  TO Z430-IDMSG                   
245701              MOVE 'BACKORDER LINE RELEASED' TO Z430-TEMFSINF             
245702           ELSE                                                           
245703              IF NON-API-OTH-EVENT                                        
245705                 MOVE '150'                  TO Z430-IDMSG                
245706                 MOVE 'NEW ORDER CONFIRMATIONS' TO Z430-TEMFSINF          
245707              END-IF                                                      
245708           END-IF                                                         
245709        END-IF                                                            
245710     ELSE                                                                 
245800       IF (ORAD-IDSYSTEM = 'LYNB' OR 'POLB' OR 'LYNV' OR 'POLV' OR        
245900                           'ECOB' OR 'ECOV' OR 'TADB' OR 'TADV' OR        
245910                           'ACCB' OR 'ACCV' OR 'APAB' OR 'APAV' OR        
245920                           'APBB' OR 'APBV' OR 'APCB' OR 'APCV' OR        
245930                           'APDB' OR 'APDV' OR 'APEB' OR 'APEV' OR        
245940                           'APFB' OR 'APFV' OR 'APGB' OR 'APGV' OR        
245960                           'APHB' OR 'APHV' OR 'APIB' OR 'APIV' OR        
245970                           'APJB' OR 'APJV' )                             
246000         MOVE '153'                  TO Z430-IDMSG                        
246100         MOVE 'BACKORDER LINE RELEASED' TO Z430-TEMFSINF                  
246200       ELSE                                                               
246300         MOVE '150'                  TO Z430-IDMSG                        
246400         MOVE 'NEW ORDER CONFIRMATIONS' TO Z430-TEMFSINF                  
246500       END-IF                                                             
246510     END-IF                                                               
246600                                                                          
246700*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
246800*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
246900     MOVE 'WZ0430X '          TO MSG-KDTRANS-1                            
247000     MOVE 'Z430'              TO MSG-IDTRANS-1                            
247100     MOVE '1'                 TO MSG-KDMFSFOR-1                           
247200     MOVE 'WZ0430I1'          TO MSG-KOM-IDCPYTXT                         
247300     MOVE OHUV-IDDISTR        TO WS-IDDISTR                               
247400     IF WS-IDEVENTREC = 'LYNK'                                            
247500        STRING 'EVEL' WS-IDDISTR                                          
247600          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
247700     ELSE                                                                 
247800        IF WS-IDEVENTREC = 'ECOM'                                         
247900           STRING 'EVEE' WS-IDDISTR                                       
248000             DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                      
248100        ELSE                                                              
248200           IF WS-IDEVENTREC = 'TAD '                                      
248300              STRING 'EVET' WS-IDDISTR                                    
248400                DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                   
248500           ELSE                                                           
248510              IF WS-IDEVENTREC = 'ACC '                                   
248520                 STRING 'EVEA' WS-IDDISTR                                 
248530                   DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                
248540              ELSE                                                        
248550                IF WS-IDEVENTREC = 'APA '                                 
248560                   STRING 'EVEK' WS-IDDISTR                               
248570                   DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                
248580                ELSE                                                      
248590                  IF WS-IDEVENTREC = 'APB '                               
248591                     STRING 'EVEB' WS-IDDISTR                             
248592                     DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD              
248593                  ELSE                                                    
248594                     IF WS-IDEVENTREC = 'APC '                            
248595                        STRING 'EVEC' WS-IDDISTR                          
248596                        DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD           
248597                     ELSE                                                 
248598                       IF WS-IDEVENTREC = 'APD '                          
248599                          STRING 'EVED' WS-IDDISTR                        
248600                          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD         
248601                       ELSE                                               
248602                         IF WS-IDEVENTREC = 'APE '                        
248603                            STRING 'EVEM' WS-IDDISTR                      
248604                            DELIMITED BY SIZE                             
248605                                             INTO MSG-KOM-IDSNDNOD        
248606                         ELSE                                             
248607                           IF WS-IDEVENTREC = 'APF '                      
248608                              STRING 'EVEF' WS-IDDISTR                    
248609                              DELIMITED BY SIZE                           
248610                                             INTO MSG-KOM-IDSNDNOD        
248611                           ELSE                                           
248612                             IF WS-IDEVENTREC = 'APG '                    
248613                                STRING 'EVEG' WS-IDDISTR                  
248614                                DELIMITED BY SIZE                         
248615                                             INTO MSG-KOM-IDSNDNOD        
248616                             ELSE                                         
248617                               IF WS-IDEVENTREC = 'APH '                  
248618                                  STRING 'EVEH' WS-IDDISTR                
248619                                  DELIMITED BY SIZE                       
248620                                             INTO MSG-KOM-IDSNDNOD        
248621                               ELSE                                       
248622                                 IF WS-IDEVENTREC = 'API '                
248623                                    STRING 'EVEI' WS-IDDISTR              
248625                                    DELIMITED BY SIZE                     
248626                                             INTO MSG-KOM-IDSNDNOD        
248627                                 ELSE                                     
248628                                  IF WS-IDEVENTREC = 'APJ '               
248629                                      STRING 'EVEJ' WS-IDDISTR            
248630                                      DELIMITED BY SIZE                   
248631                                             INTO MSG-KOM-IDSNDNOD        
248632                                  ELSE                                    
248640                                     STRING 'EVEP' WS-IDDISTR             
248700                                     DELIMITED BY SIZE                    
248701                                             INTO MSG-KOM-IDSNDNOD        
248710                                  END-IF                                  
248720                                 END-IF                                   
248730                               END-IF                                     
248740                             END-IF                                       
248750                           END-IF                                         
248760                          END-IF                                          
248770                        END-IF                                            
248780                      END-IF                                              
248790                   END-IF                                                 
248791                END-IF                                                    
248792              END-IF                                                      
248800           END-IF                                                         
248900        END-IF                                                            
249000     END-IF                                                               
249100                                                                          
249200     ADD  1                   TO MSG-KOM-TIKLOCK                          
249300     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
249400     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
249500                                                                          
249600     CALL W006KOM USING MSG-PCB                                           
249700                        0693-PCB                                          
249800                        WDP8-PCB                                          
249900                        MSG-KOM-WMSGKOM                                   
250000                        MSG-IO-AREA                                       
250100                                                                          
250200     IF MSG-KOM-IDMFSMED  = '120'                                         
250400        MOVE 'W40293I1'       TO    MSG-KOM-IDSNDJOB                      
250400        MOVE SPACE            TO    MSG-KOM-IDMFSMED                      
250500         CALL W006KOM USING MSG-PCB                                       
250600                            0693-PCB                                      
250700                            WDP8-PCB                                      
250710                            MSG-KOM-WMSGKOM                               
250720                            MSG-IO-AREA                                   
250810         IF MSG-KOM-IDMFSMED NOT = SPACE                                  
250820*           ERROR UPDATING COMMUNICATION DB                               
250830            STRING ' ERROR FROM W006KOM. ' MSG-KOM-IDMFSMED               
250840              DELIMITED BY SIZE INTO ERROR-TEXT                           
250850            CALL ABEND USING RKOD-ABEND-NO-DUMP                           
250860         END-IF                                                           
250870     ELSE                                                                 
250871         IF MSG-KOM-IDMFSMED NOT = SPACE                                  
250872*           ERROR UPDATING COMMUNICATION DB                               
250873            STRING ' ERROR FROM W006KOM. ' MSG-KOM-IDMFSMED               
250874              DELIMITED BY SIZE INTO ERROR-TEXT                           
250875            CALL ABEND USING RKOD-ABEND-NO-DUMP                           
250883         END-IF                                                           
250890     END-IF                                                               
250900     .                                                                    
251000     EJECT                                                                
251100                                                                          
251200 S21-SEND-OPEN SECTION.                                                   
251300     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
251400     MOVE 'OPEN'                  TO SEND-KDFUNC                          
251500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
251600                                     SEND-OPEN-AREA                       
251700     IF SEND-KDRC > ZERO                                                  
251800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
251900       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
252000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
252200       CALL FELLOG                                                        
252300     END-IF                                                               
252400     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-OC                      
252500     .                                                                    
252600     EJECT                                                                
252700 S22-PUT-HEADER SECTION.                                                  
252800     MOVE 1                       TO REQU-IDMSGVER                        
252900     MOVE 'R'                     TO REQU-KDPGMACT                        
253000     MOVE IDPGM                   TO REQU-IDUSER                          
253100     MOVE 'ORDERCONF'             TO HDR-IDOUTTYPE                        
253200     MOVE WS-IDKUNDNR             TO HDR-IDOUTREC                         
253300     MOVE WS-IDORDNR7             TO HDR-IDLIST                           
253400     MOVE 'PUT'                   TO SEND-KDFUNC                          
253500     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
253600     MOVE WS-SAVE-IDCOM-OC        TO SEND-IDCOM                           
253700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
253800                                     SEND-KVDLEN                          
253900                                     HDR-AREA                             
254000     IF SEND-KDRC > ZERO                                                  
254100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
254200       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
254300       DELIMITED BY SIZE       INTO FELTEXT-STR                           
254500       CALL FELLOG                                                        
254600     END-IF                                                               
254700     .                                                                    
254800     EJECT                                                                
254900 S25-PUT-LINE SECTION.                                                    
255000     MOVE 'PUT'                   TO SEND-KDFUNC                          
255100     MOVE LENGTH OF 402-W402TACD  TO SEND-KVDLEN                          
255200     MOVE WS-SAVE-IDCOM-OC        TO SEND-IDCOM                           
255300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
255400                                     SEND-KVDLEN                          
255500                                     402-W402TACD                         
255600     IF SEND-KDRC > ZERO                                                  
255700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
255800       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
255900       DELIMITED BY SIZE       INTO FELTEXT-STR                           
256100       CALL FELLOG                                                        
256200     END-IF                                                               
256300     .                                                                    
256400     SKIP2                                                                
256500 S29-SEND-CLOSE SECTION.                                                  
256600     IF WS-KV402 > 0                                                      
256700       MOVE 'CLOSE'               TO SEND-KDFUNC                          
256800       MOVE WS-SAVE-IDCOM-OC      TO SEND-IDCOM                           
256900       CALL WZ01SEND           USING SEND-CONTROL-AREA                    
257000       IF SEND-KDRC > 0                                                   
257100         MOVE SEND-KDRC          TO KDRC-DISPLAY                          
257200         STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                  
257300         DELIMITED BY SIZE     INTO FELTEXT-STR                           
257500         CALL FELLOG                                                      
257600       END-IF                                                             
257700     END-IF                                                               
257800     .                                                                    
257900     EJECT                                                                
258000                                                                          
258100 S31-SEND-OPEN-CN SECTION.                                                
258200     MOVE 'CARPARTS.DAP.DISTRDOC' TO CN-SEND-ADDISPABS                    
258300     MOVE 'OPEN'                  TO CN-SEND-KDFUNC                       
258400     CALL WZ01SEND             USING CN-SEND-CONTROL-AREA                 
258500                                     CN-SEND-OPEN-AREA                    
258600     IF CN-SEND-KDRC > ZERO                                               
258700       MOVE CN-SEND-KDRC          TO KDRC-DISPLAY                         
258800       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
258900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
259100       CALL FELLOG                                                        
259200     END-IF                                                               
259300     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-CN                      
259400     .                                                                    
259500     EJECT                                                                
259600 S32-PUT-HEADER-CN SECTION.                                               
259700     MOVE 1                       TO REQU-IDMSGVER                        
259800     MOVE 'R'                     TO REQU-KDPGMACT                        
259900     MOVE IDPGM                   TO REQU-IDUSER                          
260000     MOVE 'CHINADMS'              TO HDR-IDOUTTYPE                        
260100     MOVE '4293'                  TO HDR-IDOUTREC                         
260200     MOVE SPACE                   TO HDR-IDLIST                           
260300     MOVE 'PUT'                   TO CN-SEND-KDFUNC                       
260400     MOVE LENGTH OF HDR-AREA      TO CN-SEND-KVDLEN                       
260500     MOVE WS-SAVE-IDCOM-CN        TO SEND-IDCOM                           
260600     CALL WZ01SEND             USING CN-SEND-CONTROL-AREA                 
260700                                     CN-SEND-KVDLEN                       
260800                                     HDR-AREA                             
260900     IF CN-SEND-KDRC > ZERO                                               
261000       MOVE CN-SEND-KDRC          TO KDRC-DISPLAY                         
261100       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
261200       DELIMITED BY SIZE       INTO FELTEXT-STR                           
261400       CALL FELLOG                                                        
261500     END-IF                                                               
261600     .                                                                    
261700     EJECT                                                                
261800 S35-PUT-LINE-CN SECTION.                                                 
261900     MOVE 'PUT'                   TO CN-SEND-KDFUNC                       
262000     MOVE LENGTH OF CNO-W402CNO   TO CN-SEND-KVDLEN                       
262100     MOVE WS-SAVE-IDCOM-CN        TO SEND-IDCOM                           
262200     CALL WZ01SEND             USING CN-SEND-CONTROL-AREA                 
262300                                     CN-SEND-KVDLEN                       
262400                                     CNO-W402CNO                          
262500     IF CN-SEND-KDRC > ZERO                                               
262600       MOVE CN-SEND-KDRC          TO KDRC-DISPLAY                         
262700       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
262800       DELIMITED BY SIZE       INTO FELTEXT-STR                           
263000       CALL FELLOG                                                        
263100     END-IF                                                               
263200     .                                                                    
263300     SKIP2                                                                
263400 S39-SEND-CLOSE-CN SECTION.                                               
263500     IF WS-KVCNO > 0                                                      
263600       MOVE 'CLOSE'               TO CN-SEND-KDFUNC                       
263700       MOVE WS-SAVE-IDCOM-CN      TO SEND-IDCOM                           
263800       CALL WZ01SEND           USING CN-SEND-CONTROL-AREA                 
263900       IF CN-SEND-KDRC > 0                                                
264000         MOVE CN-SEND-KDRC        TO KDRC-DISPLAY                         
264100         STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                  
264200         DELIMITED BY SIZE     INTO FELTEXT-STR                           
264400         CALL FELLOG                                                      
264500       END-IF                                                             
264600     END-IF                                                               
264700     .                                                                    
264800                                                                          
264900                                                                          
265000 S41-SEND-OPEN-MIC SECTION.                                               
265100                                                                          
265200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
265300     MOVE 'CARPARTS.FLS.EXPORTORDER' TO SEND-ADDISPABS                    
265400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
265500                                                                          
265600     IF SEND-KDRC > 0                                                     
265700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
265800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
265900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
266000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
266100     END-IF                                                               
266200     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-MIC                     
266300     .                                                                    
266400                                                                          
266500                                                                          
266600 S42O-SEND-MESSAGE-MIC-ORDER SECTION.                                     
266700                                                                          
266800     MOVE 'PUT'                      TO SEND-KDFUNC                       
266900     MOVE MICO-W402MICO              TO SEND-AREA                         
267100     MOVE LENGTH OF MICO-W402MICO    TO SEND-KVDLEN                       
267200*    -- LENGTH COMPUTED IN C- SECTIONS                                    
267300     MOVE WS-SAVE-IDCOM-MIC       TO SEND-IDCOM                           
267400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
267500                                                                          
267600     IF SEND-KDRC > 0                                                     
267700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
267800       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
267900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
268000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
268100     END-IF                                                               
268200     .                                                                    
268300                                                                          
268400 S42A-SEND-MESSAGE-MIC-ARTIKEL SECTION.                                   
268500                                                                          
268600     MOVE 'PUT'                      TO SEND-KDFUNC                       
268700     MOVE MICA-W402MICA              TO SEND-AREA                         
268900     MOVE LENGTH OF MICA-W402MICA    TO SEND-KVDLEN                       
269000*    -- LENGTH COMPUTED IN C- SECTIONS                                    
269100     MOVE WS-SAVE-IDCOM-MIC       TO SEND-IDCOM                           
269200     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
269300                                                                          
269400     IF SEND-KDRC > 0                                                     
269500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
269600       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
269700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
269800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
269900     END-IF                                                               
270000     .                                                                    
270100                                                                          
270200 S43-SEND-CLOSE-MIC SECTION.                                              
270300                                                                          
270400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
270500     MOVE WS-SAVE-IDCOM-MIC          TO SEND-IDCOM                        
270600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
270700                                                                          
270800     IF SEND-KDRC > 0                                                     
270900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
271000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
271100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
271200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
271300     END-IF                                                               
271400     .                                                                    
271500* IMS SEKTIONER                                                           
271600     SKIP3                                                                
271700 IMS-GET-MSG SECTION.                                                     
271800     MOVE '  QC' TO GODK-STATUSKODER                                      
271900     CALL  CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                          
272000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
272100     PERFORM IMS-STATUSKONTROLL                                           
272200     .                                                                    
272300     SKIP2                                                                
272400 IMS-ISRT-ALT1-MSG-4293  SECTION.                                         
272500     MOVE SPACE TO GODK-STATUSKODER                                       
272600     CALL  CBLTDLI  USING ISRT ALT1-PCB MSG-IO-AREA                       
272700     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
272800     PERFORM IMS-STATUSKONTROLL                                           
272900     .                                                                    
273000     SKIP2                                                                
273100 IMS-ISRT-ALT2-MSG-4353    SECTION.                                       
273200     MOVE SPACE TO GODK-STATUSKODER                                       
273300     CALL  CBLTDLI  USING ISRT ALT2-PCB MSG-IO-AREA                       
273400     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
273500     PERFORM IMS-STATUSKONTROLL                                           
273600     .                                                                    
273700     SKIP2                                                                
273800 IMS-ISRT-ALT3-MSG-4695  SECTION.                                         
273900     MOVE SPACE TO GODK-STATUSKODER                                       
274000     CALL  CBLTDLI  USING ISRT ALT3-PCB MSG-IO-AREA                       
274100     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
274200     PERFORM IMS-STATUSKONTROLL                                           
274300     .                                                                    
274400     EJECT                                                                
274500 IMS-PURG-ALT4-MSG-2109  SECTION.                                         
274600     MOVE SPACE TO GODK-STATUSKODER                                       
274700     CALL  CBLTDLI  USING PURG ALT4-PCB MSG-IO-AREA                       
274800     MOVE ALT4-STATUS-CODE TO STATUS-WS                                   
274900     PERFORM IMS-STATUSKONTROLL                                           
275000     .                                                                    
275100     EJECT                                                                
275200 IMS-PURG-ALT5-MSG-2191  SECTION.                                         
275300     MOVE SPACE TO GODK-STATUSKODER                                       
275400     CALL  CBLTDLI  USING PURG ALT5-PCB MSG-IO-AREA                       
275500     MOVE ALT5-STATUS-CODE TO STATUS-WS                                   
275600     PERFORM IMS-STATUSKONTROLL                                           
275700     .                                                                    
275800     EJECT                                                                
275900 IMS-GET-ORQI01-OHUV-KVAL          SECTION.                               
276000     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
276100            DELIMITED BY SIZE INTO SSA1                                   
276200     MOVE '  GE' TO GODK-STATUSKODER                                      
276300     CALL  CBLTDLI  USING GHU ORQI-PCB DLI-IO-AREA-WDQ201 SSA1            
276400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
276500     PERFORM IMS-STATUSKONTROLL                                           
276600     .                                                                    
276700     SKIP3                                                                
276800 IMS-REPL-ORQI01-OHUV               SECTION.                              
276900     MOVE '  ' TO GODK-STATUSKODER                                        
277000     CALL  CBLTDLI  USING REPL ORQI-PCB DLI-IO-AREA-WDQ201                
277100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
277200     PERFORM IMS-STATUSKONTROLL                                           
277300     .                                                                    
277400     EJECT                                                                
277500 IMS-GNP-ORQI11-FIRST               SECTION.                              
277600     MOVE 'WLORQI11*F'         TO SSA1                                    
277700     MOVE '  GE'               TO GODK-STATUSKODER                        
277800     CALL  CBLTDLI  USING GNP ORQI-PCB DLI-IO-AREA-WDQ211 SSA1            
277900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
278000     PERFORM IMS-STATUSKONTROLL                                           
278100     .                                                                    
278200     SKIP3                                                                
278300 IMS-GNP-ORQI11                     SECTION.                              
278400     MOVE 'WLORQI11'           TO SSA1                                    
278500     MOVE '  GE'               TO GODK-STATUSKODER                        
278600     CALL  CBLTDLI  USING GNP ORQI-PCB DLI-IO-AREA-WDQ211 SSA1            
278700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
278800     PERFORM IMS-STATUSKONTROLL                                           
278900     .                                                                    
279000     SKIP3                                                                
279100 IMS-GHNP-ORQI12-OKVAL-FIRST        SECTION.                              
279200     MOVE 'WLORQI12*F'       TO SSA1                                      
279300     MOVE '  GE'   TO GODK-STATUSKODER                                    
279400     CALL  CBLTDLI  USING GHNP ORQI-PCB DLI-IO-AREA-WDQ212 SSA1           
279500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
279600     PERFORM IMS-STATUSKONTROLL                                           
279700     .                                                                    
279800     EJECT                                                                
279801 IMS-GHNP-ORQI12-OKVAL              SECTION.                              
279802     MOVE 'WLORQI12'         TO SSA1                                      
279803     MOVE '  GE'   TO GODK-STATUSKODER                                    
279804     CALL  CBLTDLI  USING GHNP ORQI-PCB DLI-IO-AREA-WDQ212 SSA1           
279805     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
279806     PERFORM IMS-STATUSKONTROLL                                           
279807     .                                                                    
279808     EJECT                                                                
279809 IMS-GHNP-ORQI12-KVAL-FIRST         SECTION.                              
279810     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
279811            DELIMITED BY SIZE INTO SSA1                                   
279812     MOVE '  GE'   TO GODK-STATUSKODER                                    
279813     CALL  CBLTDLI  USING GHNP ORQI-PCB DLI-IO-AREA-WDQ212 SSA1           
279814     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
279815     PERFORM IMS-STATUSKONTROLL                                           
279816     .                                                                    
279817     EJECT                                                                
279818 IMS-REPL-ORQI12 SECTION.                                                 
279819     MOVE   '    '            TO GODK-STATUSKODER                         
279820     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-WDQ212                  
279821     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
279822     PERFORM IMS-STATUSKONTROLL                                           
279823     .                                                                    
279824     EJECT                                                                
279900 IMS-GET-ORQF01-ORAD-KVAL            SECTION.                             
280000     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
280100            DELIMITED BY SIZE INTO SSA1                                   
280200     MOVE '  GE'               TO GODK-STATUSKODER                        
280300     CALL  CBLTDLI  USING GHU ORQF-PCB DLI-IO-AREA5 SSA1                  
280400     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
280500     PERFORM IMS-STATUSKONTROLL                                           
280600     .                                                                    
280700     SKIP2                                                                
280800 IMS-GET-ORQF01-ORAD-OKVAL            SECTION.                            
280900     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
281000                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
281100            DELIMITED BY SIZE INTO SSA1                                   
281200     MOVE '  GEGB'             TO GODK-STATUSKODER                        
281300     CALL  CBLTDLI  USING GHN ORQF-PCB DLI-IO-AREA5 SSA1                  
281400     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
281500     PERFORM IMS-STATUSKONTROLL                                           
281600     .                                                                    
281700     SKIP2                                                                
281800 IMS-REPL-ORQF01-ORAD                 SECTION.                            
281900     MOVE 'WLORQF01'           TO SSA1                                    
282000     MOVE '    '               TO GODK-STATUSKODER                        
282100     CALL  CBLTDLI  USING REPL ORQF-PCB DLI-IO-AREA5 SSA1                 
282200     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
282300     PERFORM IMS-STATUSKONTROLL                                           
282400     .                                                                    
282500     EJECT                                                                
282600 IMS-GET-ORQM01-OBKR-KVAL    SECTION.                                     
282700     STRING 'WLORQM01(WDQ101KY =' W-WDQ101KY-X  ')'                       
282800            DELIMITED BY SIZE INTO SSA1                                   
282900     MOVE '  GE'               TO GODK-STATUSKODER                        
283000     CALL  CBLTDLI  USING GHU ORQM-PCB DLI-IO-AREA6 SSA1                  
283100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
283200     PERFORM IMS-STATUSKONTROLL                                           
283300     .                                                                    
283400     SKIP2                                                                
283500 IMS-GET-ORQM01-OBKR-OKVAL         SECTION.                               
283600     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
283700                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
283800            DELIMITED BY SIZE INTO SSA1                                   
283900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
284000     CALL  CBLTDLI  USING GHN ORQM-PCB DLI-IO-AREA6 SSA1                  
284100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
284200     PERFORM IMS-STATUSKONTROLL                                           
284300     .                                                                    
284400     SKIP2                                                                
284500 IMS-REPL-ORQM01-OBKR          SECTION.                                   
284600     MOVE 'WLORQM01'           TO SSA1                                    
284700     MOVE '    '               TO GODK-STATUSKODER                        
284800     CALL  CBLTDLI  USING REPL ORQM-PCB DLI-IO-AREA6 SSA1                 
284900     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
285000     PERFORM IMS-STATUSKONTROLL                                           
285100     .                                                                    
285200     SKIP2                                                                
285300 IMS-GU-WDK601                 SECTION.                                   
285400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
285500            DELIMITED BY SIZE INTO SSA1                                   
285600     MOVE '  '                   TO GODK-STATUSKODER                      
285700     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-AREA10 SSA1                
285800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
285900     PERFORM IMS-STATUSKONTROLL                                           
286000     .                                                                    
286100     SKIP2                                                                
286200 IMS-GNP-WDK611                SECTION.                                   
286300     MOVE 'WDK611  '           TO SSA1                                    
286400     MOVE '  GE'               TO GODK-STATUSKODER                        
286500     CALL  CBLTDLI  USING GNP  WDK6-PCB DLI-IO-AREA11 SSA1                
286600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
286700     PERFORM IMS-STATUSKONTROLL                                           
286800     .                                                                    
286900     SKIP2                                                                
287000 IMS-GHNP-WDK611               SECTION.                                   
287100     MOVE 'WDK611  '           TO SSA1                                    
287200     MOVE '  GE'               TO GODK-STATUSKODER                        
287300     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-AREA11 SSA1                
287400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
287500     PERFORM IMS-STATUSKONTROLL                                           
287600     .                                                                    
287700     SKIP2                                                                
287800 IMS-REPL-WDK611               SECTION.                                   
287900     MOVE 'WDK611  '           TO SSA1                                    
288000     MOVE '    '               TO GODK-STATUSKODER                        
288100     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-AREA11 SSA1                
288200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
288300     PERFORM IMS-STATUSKONTROLL                                           
288400     .                                                                    
288500     SKIP2                                                                
288600 IMS-GHU-SEQB-WDA601            SECTION.                                  
288700     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
288800                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
288900            DELIMITED BY SIZE INTO SSA1                                   
289000     MOVE '  GE'                 TO GODK-STATUSKODER                      
289100     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-AREA7 SSA1               
289200     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
289300     PERFORM IMS-STATUSKONTROLL                                           
289400     .                                                                    
289500     SKIP2                                                                
289600 IMS-GHN-SEQB-WDA601            SECTION.                                  
289700     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
289800                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
289900            DELIMITED BY SIZE INTO SSA1                                   
290000     MOVE '  GEGB'               TO GODK-STATUSKODER                      
290100     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA7 SSA1               
290200     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
290300     PERFORM IMS-STATUSKONTROLL                                           
290400     .                                                                    
290500     SKIP2                                                                
290600 IMS-REPL-SEQB-WDA601                 SECTION.                            
290700     MOVE 'WDA601  '           TO SSA1                                    
290800     MOVE '    '               TO GODK-STATUSKODER                        
290900     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA7 SSA1                
291000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
291100     PERFORM IMS-STATUSKONTROLL                                           
291200     .                                                                    
291300     EJECT                                                                
291400 IMS-ISRT-WDA601            SECTION.                                      
291500     MOVE   'WDA601  '         TO SSA1                                    
291600     MOVE '  IINI' TO GODK-STATUSKODER                                    
291700     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-AREA7 SSA1                 
291800     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
291900     PERFORM IMS-STATUSKONTROLL                                           
292000     .                                                                    
292100     EJECT                                                                
292200 IMS-ISRT-FILA01            SECTION.                                      
292300     MOVE   'WLFILA01'         TO SSA1                                    
292400     MOVE '  II' TO GODK-STATUSKODER                                      
292500     CALL  CBLTDLI  USING ISRT FILA-PCB DLI-IO-AREA2 SSA1                 
292600     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
292700     PERFORM IMS-STATUSKONTROLL                                           
292800     .                                                                    
292900     SKIP3                                                                
293000 IMS-GU-WDB101 SECTION.                                                   
293100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
293200          DELIMITED BY SIZE INTO SSA1                                     
293300     MOVE '  GE' TO GODK-STATUSKODER                                      
293400     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
293500     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
293600     PERFORM IMS-STATUSKONTROLL                                           
293700     .                                                                    
293800     SKIP3                                                                
293900 IMS-GU-WDB201 SECTION.                                                   
294000                                                                          
294100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
294200          DELIMITED BY SIZE INTO SSA1                                     
294300     MOVE '  GE' TO GODK-STATUSKODER                                      
294400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
294500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
294600     PERFORM IMS-STATUSKONTROLL                                           
294700     .                                                                    
294800     EJECT                                                                
294900 IMS-GU-WDB601    SECTION.                                                
295000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
295100          DELIMITED BY SIZE INTO SSA1                                     
295200     MOVE '  GE' TO GODK-STATUSKODER                                      
295300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
295400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
295500     PERFORM IMS-STATUSKONTROLL                                           
295600     IF SEGMENT-SAKNAS                                                    
295700         MOVE SPACE TO DCS-KDDC                                           
295800     END-IF                                                               
295900     .                                                                    
296000                                                                          
296100 IMS-GU-WDP4A1 SECTION.                                                   
296200                                                                          
296300     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
296400                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
296500          DELIMITED BY SIZE INTO SSA1                                     
296600     MOVE '  GE' TO GODK-STATUSKODER                                      
296700     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
296800                           SSA1                                           
296900     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
297000     PERFORM IMS-STATUSKONTROLL                                           
297100     .                                                                    
297200                                                                          
297300 IMS-GU-WDGX4254 SECTION.                                                 
297400                                                                          
297500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
297600          DELIMITED BY SIZE INTO SSA1                                     
297700     STRING 'WDGX4254(IDARTNR = ' W-IDARTNR-4254-X ')'                    
297800          DELIMITED BY SIZE INTO SSA2                                     
297900     MOVE '  GE'              TO GODK-STATUSKODER                         
298000     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-AREA-WDGX4254 SSA1 SSA2        
298100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
298200     PERFORM IMS-STATUSKONTROLL                                           
298300     .                                                                    
298400                                                                          
298500 IMS-GU-WDQ201-CSEQ SECTION.                                              
298600                                                                          
298700     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                          
298800          DELIMITED BY SIZE INTO SSA1                                     
298900     MOVE '  GE'              TO GODK-STATUSKODER                         
299000     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-Q201   SSA1               
299100     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
299200     PERFORM IMS-STATUSKONTROLL                                           
299300     .                                                                    
299400                                                                          
299500 IMS-GU-WDA501 SECTION.                                                   
299600                                                                          
299700     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
299800                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
299900                    '&KDORDKL  =' W-KDORDKL-X                             
300000                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
300100          DELIMITED BY SIZE INTO SSA1                                     
300200     MOVE '  GE'              TO GODK-STATUSKODER                         
300300     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501  SSA1                   
300400     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
300500     PERFORM IMS-STATUSKONTROLL                                           
300600     .                                                                    
300610 IMS-GN-WDA501 SECTION.                                                   
300620                                                                          
300630     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
300640                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
300650                    '&KDORDKL  =' W-KDORDKL-X                             
300660                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
300670          DELIMITED BY SIZE INTO SSA1                                     
300680     MOVE '  GBGE'            TO GODK-STATUSKODER                         
300690     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-WDA501  SSA1                   
300691     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
300693     PERFORM IMS-STATUSKONTROLL                                           
300694     .                                                                    
300700 IMS-STATUSKONTROLL SECTION.                                              
300800     SET STATUS-IX TO 1                                                   
300900     SEARCH GODK-STATUS                                                   
301000       AT END CALL FELLOG                                                 
301100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
301200         CONTINUE                                                         
301300     END-SEARCH                                                           
301400     .                                                                    
