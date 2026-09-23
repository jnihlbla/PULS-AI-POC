000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018800.                                                
000300 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000400 DATE-WRITTEN.   2004/11/24.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        WL018800 PROGRAM IS A REPLICA OF W4067500 PROGRAM                
000900*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001000*        IT IS STARTED BY WL018700 (A WEB REPLICA OF W4066400)            
001100*                                                                         
001200*        THIS PROGRAM STARTS BUILDING THE TWO NEW DATABASES               
001300*        WDE1 AND WDE2 FOR SHIPPING AND INVOICING.                        
001400*        THIS PROGRAM RUNS IN THE BACKGROUND WITHOUT ANY DIALOGUE         
001500*        (KDTRTYP=X) OR AS A NORMAL FORGROUND PROGRAM (KDTRTYP=U)         
001600*        ALLOWS TO START WL0191 WHICH UPDATES THE INFORMATION ON          
001700*        ALLOWS TO START WL0191 WHICH UPDATES THE INFORMATION ON          
001800*        WDE122 AND WDE211.                                               
001900*                                                                         
002000*        THE PROGRAM UPDATES   WDE6                                       
002100*        THE PROGRAM UPDATES   WDE1                                       
002200*        THE PROGRAM UPDATES   WDE2                                       
002300*        THE PROGRAM READS     WDR4                                       
002400*        THE PROGRAM READS     WDR1                                       
002500*        THE PROGRAM READS     WDB6                                       
002600*                                                                         
002700* ADDRESS : 'CARPARTS.LDC.TRANSPSUPPL1BG'                                 
002800*                                                                         
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSACTION: WL0188U                                             
003200*        REQUEST:     WL0188I1                                            
003300*                                                                         
003400*    OUTDATA.                                                             
003500*        RESPONSE:    WLO188I1                                            
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'WL018800'.            
004400                                                                          
004500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004700 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004800 77  CURR-SECTION                PIC X(16)  VALUE 'MAIN'.                 
004900 77  CURR-IMS-SECTION            PIC X(16)  VALUE SPACE.                  
005000                                                                          
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400                                                                          
005500*    --- INDEX FOR SCROLL LINES                                           
005600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005700 77  IDLAND-INDX                 PIC S9(4)  VALUE +0    COMP SYNC.        
005800 77  MAX-IDLAND-INDX             PIC S9(4)  VALUE +23   COMP SYNC.        
005900 77  IDPSN-IX                    PIC S9(3)  VALUE +0    COMP SYNC.        
006000 77  WS-KVRADER                  PIC S9(4)  VALUE +0    COMP SYNC.        
006100 77  MAX-KVRADER                 PIC S9(4)  VALUE +100  COMP SYNC.        
006200                                                                          
006300 77  WS-IDELMT-ERROR             PIC X(16).                               
006400 77  WS-IDMSG-ERROR              PIC X(03).                               
006500 77  WS-IDMSG-INFO               PIC X(03).                               
006600 77  WS-ADRESS    PIC X(50)  VALUE 'CARPARTS.LDC.TRANSPSUPPL1BG'.         
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-WRONG                        VALUE 'N'.                   
007100                                                                          
007200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007300     88  KEYS-OK                             VALUE 'J'.                   
007400     88  KEYS-WRONG                          VALUE 'N'.                   
007500                                                                          
007600 77  4498-MISSING-SW             PIC X       VALUE 'N'.                   
007700     88  4498-MISSING                        VALUE 'J'.                   
007800                                                                          
007900 77  START-4540-SW               PIC X       VALUE 'N'.                   
008000     88  START-4540                          VALUE 'J'.                   
008100                                                                          
008200 77  RESTART-SW                  PIC X(01)   VALUE 'N'.                   
008300     88 RESTART                              VALUE 'J'.                   
008400                                                                          
008500 77  FIRST-PRODNR-SW             PIC X(01)   VALUE 'N'.                   
008600     88 FIRST-PRODNR                         VALUE 'J'.                   
008700                                                                          
008800 77  FIRST-DEALER-SW             PIC X(01)   VALUE 'N'.                   
008900     88 SW-FIRST-DEALER                      VALUE 'J'.                   
009000                                                                          
009100 77  FLSAMFAK-SW                 PIC X(01)   VALUE 'N'.                   
009200     88 FLSAMFAK                             VALUE 'J'.                   
009300                                                                          
009400 77  FLSAMFAK-PREV-SW            PIC X(01)   VALUE 'N'.                   
009500     88 FLSAMFAK-PREV                        VALUE 'J'.                   
009600                                                                          
009700 77  FARLIGT-GOOD-4539           PIC X(01)   VALUE 'N'.                   
009800     88 FARLIGT-GOOD-FOUND-4539              VALUE 'J'.                   
009900                                                                          
010000 77  FARLIGT-GOOD-4540           PIC X(01)   VALUE 'N'.                   
010100     88 FARLIGT-GOOD-FOUND-4540              VALUE 'J'.                   
010200                                                                          
010300 77  W-FLAVSLUTA-SW              PIC X(01)   VALUE ' '.                   
010400     88 FLAVSLUTA-OK                         VALUE 'Y' 'J'.               
010500     EJECT                                                                
010600*  TO CONVERT THE YYMMDD  TO CCYYMMDD                                     
010700 01  W-YYMMDD-DATUM              PIC 9(6).                                
010800 01  W-CCYYMMDD-DATUM.                                                    
010900     03 W-CC                     PIC 9(2).                                
011000     03 W-YYMMDD.                                                         
011100        05 W-YY                  PIC 9(2).                                
011200        05 W-MM                  PIC 9(2).                                
011300        05 W-DD                  PIC 9(2).                                
011400                                                                          
011500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011600 01  FILLER REDEFINES DAGENS-DATUM.                                       
011700     03  DAGENS-AA               PIC 9(2).                                
011800     03  DAGENS-MM               PIC 9(2).                                
011900     03  DAGENS-DD               PIC 9(2).                                
012000                                                                          
012100 01  WS-IDDC-LOCAL.                                                       
012200     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
012300     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
012400     03  FILLER                  PIC X(1)   VALUE SPACE.                  
012500                                                                          
012600 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
012700                                                                          
012800 77  W-UPD-COUNT                 PIC S9(3)   VALUE ZERO.                  
012900 77  W-UPD-MAX                   PIC S9(3)   VALUE +100.                  
013000 77  W-IDDISTR-PREV              PIC S9(5)   COMP-3 VALUE ZERO.           
013100 77  W-IDKUNDNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
013200 77  W-IDPRODNR-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
013300 77  W-IDDEALER-PREV             PIC S9(7)   COMP-3 VALUE ZERO.           
013400 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
013500                                                                          
013600 77  W-KDFARLIG                  PIC X(01)   VALUE SPACE.                 
013700 77  W-FLSKRIV-NU                PIC X(01)   VALUE SPACE.                 
013800                                                                          
013900 77  W-IDDISTR-NUM               PIC  9(4)   VALUE ZERO.                  
014000 77  W-IDKUNDNR-NUM              PIC  9(6)   VALUE ZERO.                  
014100 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
014200                                                                          
014300 77  W-KDORDKL-MAX               PIC S9(1)      COMP-3 VALUE ZERO.        
014400                                                                          
014500     EJECT                                                                
014600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014900                                                                          
015000 01  FILLER                      PIC X(25)                                
015100                                      VALUE 'FOR WDE6 UPDATE'.            
015200 01  FILLER.                                                              
015300     03  KLI-PACK                PIC S9(1) COMP-3 VALUE +1.               
015400     03  KLI-PACK-FAKT           PIC S9(1) COMP-3 VALUE +6.               
015500                                                                          
015600     03 WS-TISKPTID              PIC 9(6)       VALUE ZERO.               
015700     03 WS-TISKPTID-GRP          REDEFINES WS-TISKPTID.                   
015800        05 WS-TISKPTID-HHMM         PIC 9(4).                             
015900        05 WS-TISKPTID-SS           PIC 9(2).                             
016000                                                                          
016100     EJECT                                                                
016200                                                                          
016300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
016400 01  GENERAL-SUBPROGRAMS.                                                 
016500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016800     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
016900     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
017000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
017200     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
017300                                                                          
017400 01  MESSAGE-CODES.                                                       
017500     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '043'.                 
017600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
017700     EJECT                                                                
017800     EJECT                                                                
017900 01  FILLER                      PIC X(25)   VALUE 'DC-WMSGINIT'.         
018000     SKIP3                                                                
018100*01 -COPY WMSGINIT     -PRE  DC-                                          
018200     EJECT                                                                
018300*    --- AREA  FOR W476SHNO ---                                           
018400 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
018500*01  -COPY W476SHNO                                                       
018600                                                                          
018700*01    -COPY WDECAREA                                                     
018800     EJECT                                                                
018900*    --- VALID IDDC CODES ---                                             
019000*                                                                         
019100*01    -COPY WWDC99                                                       
019200       EJECT                                                              
019300                                                                          
019400 01  TEST-IDDISTR                PIC  9(5)  COMP-3.                       
019500*01  FILLER     -COPY WWDIST07    -RED TEST-IDDISTR.                      
019600     SKIP2                                                                
019700*01  FILLER     -COPY WWDIST35    -RED TEST-IDDISTR.                      
019800     SKIP2                                                                
019900*01  FILLER     -COPY WWDIST67    -RED TEST-IDDISTR.                      
020000     SKIP2                                                                
020100*01  FILLER     -COPY WWDIST92    -RED TEST-IDDISTR.                      
020200     SKIP2                                                                
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500     SKIP3                                                                
020600*01  -COPY WMFSAREA                                                       
020700*                                                                         
020800*    --- AREA  FOR WZ01  ------                                           
020900 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
021000*01  -COPY WZ01SEND                                                       
021100                                                                          
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
021400*01  -COPY WZ01SUB                                                        
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
021700*01  -COPY WZ01RECV                                                       
021800     EJECT                                                                
021900                                                                          
022000*    - SEND AREA FOR RESTARTING THIS PROGRAM                              
022100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
022200 01  SEND-AREA-1.                                                         
022300*    03  -COPY WZ01REQU -PRE SEND1-                                       
022400*    03  -COPY WL0188I1 -PRE SEND-                                        
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
022700 01  REQU-AREA.                                                           
022800*    03  -COPY WZ01REQU                                                   
022900*    03  -COPY WL0188I1                                                   
023000                                                                          
023100     EJECT                                                                
023200                                                                          
023300 01  RESP-AREA.                                                           
023400*    03  -COPY WZ01RESP                                                   
023500*    03  -COPY WL0188O1                                                   
023600                                                                          
023700     EJECT                                                                
023800                                                                          
023900*01  -COPY W40636I1   -PRE MOD4636-                                       
024000     EJECT                                                                
024100                                                                          
024200*01  -COPY WL01TIDZ                                                       
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024500     SKIP3                                                                
024600 01  KEYS-TO-DLI.                                                         
024700*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
024800     03  W-4495-X.                                                        
024900         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
025000         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
025100         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
025200         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
025300         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
025400                                                                          
025500     03  W-4498-N-X.                                                      
025600         05  W-IDDISTR-N         PIC S9(5)   VALUE ZERO  COMP-3.          
025700         05  W-IDKUNDNR-N        PIC S9(7)   VALUE ZERO  COMP-3.          
025800         05  FILLER              PIC X(18)   VALUE HIGH-VALUE.            
025900                                                                          
026000     03  W-IDDC-X.                                                        
026100         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
026200                                                                          
026300     03  W-4463-X.                                                        
026400         05  W-IDHTR-4463        PIC X(4)    VALUE '4463'.                
026500         05  W-IDDC-4463         PIC X(2)    VALUE SPACE.                 
026600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
026700                                                                          
026800     03  W-DASKEPPN-X.                                                    
026900         05  W-DASKEPPN-4464     PIC  9(8)   VALUE ZERO.                  
027000                                                                          
027100     03  W-4466-X.                                                        
027200         05  W-IDTRPTNR-4466     PIC S9(3)   VALUE ZERO   COMP-3.         
027300         05  W-IDLBBET-4466      PIC X(12)   VALUE SPACE.                 
027400                                                                          
027500     03  W-4468-X.                                                        
027600         05  W-IDDISTR-4468      PIC S9(5)   VALUE ZERO   COMP-3.         
027700         05  W-IDKUNDNR-4468     PIC S9(7)   VALUE ZERO   COMP-3.         
027800         05  W-IDKUNDRF-4468     PIC X(10).                               
027900         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF-4468.         
028000             07  W-IDORDNR7-4468 PIC 9(07).                               
028100             07  FILLER          PIC X(03).                               
028200         05  W-IDPRODNR-4468     PIC S9(7)   VALUE ZERO  COMP-3.          
028300         05  W-IDKOLLI-4468      PIC S9(5)   VALUE ZERO  COMP-3.          
028400                                                                          
028500     03  W-4513-X.                                                        
028600         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
028700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
028800                                                                          
028900     03  W-4514-X.                                                        
029000         05  W-DASKEPPN-4514     PIC 9(8)    VALUE ZERO.                  
029100                                                                          
029200     03  W-IDPRODNR-X.                                                    
029300         05  W-IDPRODNR-KOLLI    PIC S9(7)   VALUE ZERO COMP-3.           
029400                                                                          
029500     03  W-IDKOLLI-X.                                                     
029600         05  W-IDKOLLI-KOLLI     PIC S9(5)   VALUE ZERO  COMP-3.          
029700                                                                          
029800     03  W-IDSHIPM-X.                                                     
029900         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
030000                                                                          
030100     03  W-WDE111KY-X.                                                    
030200         05  W-WDE111-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
030300         05  W-WDE111-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
030400                                                                          
030500     03  W-WDE211KY-X.                                                    
030600         05  W-WDE211-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
030700         05  W-WDE211-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
030800                                                                          
030900     03  W-IDGMT-X.                                                       
031000         05  W-WDB201-IDDISTR    PIC S9(5)    VALUE ZERO COMP-3.          
031100         05  W-WDB201-IDKUNDNR   PIC S9(7)    VALUE ZERO COMP-3.          
031200                                                                          
031300     03  W-IDDC-B6-X.                                                     
031400         05 W-IDDC-B6                  PIC X(2).                          
031500                                                                          
031600*    --- STATUS-KOD FRÅN IMS                                              
031700 01  STATUS-WS                   PIC XX.                                  
031800     88  SEGMENT-FOUND                       VALUE '  '.                  
031900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
032000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
032100     SKIP2                                                                
032200 01  GOOD-STATUSCODES.                                                    
032300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032400     SKIP3                                                                
032500 01  SSA1                        PIC X(128).                              
032600 01  SSA2                        PIC X(128).                              
032700 01  SSA3                        PIC X(128).                              
032800     EJECT                                                                
032900*    --- IMS FUNCTION CODES                                               
033000*01  -COPY W0003                                                          
033100     EJECT                                                                
033200*    ---  DLI INPUT-OUTPUT AREA                                           
033300                                                                          
033400*                                                                         
033500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4495'.                    
033600 01  DLI-IO-WDGX4495.                                                     
033700*    03  -COPY WDGX4495                                                   
033800                                                                          
033900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4498'.                    
034000 01  DLI-IO-WDGX4498.                                                     
034100*    03  -COPY WDGX4498                                                   
034200                                                                          
034300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4463'.                    
034400 01  DLI-IO-WDGX4463.                                                     
034500*    03  -COPY WDGX4463                                                   
034600                                                                          
034700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4464'.                    
034800 01  DLI-IO-WDGX4464.                                                     
034900*    03  -COPY WDGX4464                                                   
035000                                                                          
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4466'.                    
035200 01  DLI-IO-WDGX4466.                                                     
035300*    03  -COPY WDGX4466                                                   
035400                                                                          
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4468'.                    
035600 01  DLI-IO-WDGX4468.                                                     
035700*    03  -COPY WDGX4468                                                   
035800                                                                          
035900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4514'.                    
036000 01  DLI-IO-WDGX4514.                                                     
036100*    03  -COPY WDGX4514                                                   
036200                                                                          
036300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4516'.                    
036400 01  DLI-IO-WDGX4516.                                                     
036500*    03  -COPY WDGX4516                                                   
036600                                                                          
036700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
036800 01  DLI-IO-WDE601.                                                       
036900*    03  -COPY WDE601                                                     
037000     EJECT                                                                
037100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
037200 01  DLI-IO-WDE611.                                                       
037300*    03  -COPY WDE611                                                     
037400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
037500 01  DLI-IO-WDE101.                                                       
037600*    03  -COPY WDE101                                                     
037700     EJECT                                                                
037800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
037900 01  DLI-IO-WDE111.                                                       
038000*    03  -COPY WDE111                                                     
038100     EJECT                                                                
038200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
038300 01  DLI-IO-WDE121.                                                       
038400*    03  -COPY WDE121                                                     
038500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
038600 01  DLI-IO-WDE201.                                                       
038700*    03  -COPY WDE201                                                     
038800     EJECT                                                                
038900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
039000 01  DLI-IO-WDE211.                                                       
039100*    03  -COPY WDE211                                                     
039200     EJECT                                                                
039300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE221'.                      
039400 01  DLI-IO-WDE221.                                                       
039500*    03  -COPY WDE221                                                     
039600     EJECT                                                                
039700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
039800 01  DLI-IO-WDB201.                                                       
039900*    03  -COPY WDB201                                                     
040000     EJECT                                                                
040100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040200 01   DLI-IO-AREA-B601.                                                   
040300*     03  -COPY WDB601                                                    
040400     EJECT                                                                
040500*---MSG-AREA FOR 4539 FOR FARL.GOOD                                       
040600 01  FILLER                PIC X(16)  VALUE '4539-MSG-IO-AREA'.           
040700 01  4539-MSG-IO-AREA.                                                    
040800     03  4539-LL              PIC S9(4)  VALUE +49 COMP SYNC.             
040900     03  4539-Z1              PIC X.                                      
041000     03  4539-Z2              PIC X.                                      
041100     03  4539-TRANSKOD        PIC X(8)   VALUE 'W4T539X '.                
041200     03  4539-IDTRANS         PIC X(4)   VALUE '4675'.                    
041300     03  4539-SPRAK           PIC X.                                      
041400*    03  -COPY W4I53901   -PRE 4539-                                      
041500     EJECT                                                                
041600*---MSG-AREA FOR 4540 FOR FARL.GOOD                                       
041700 01  FILLER                PIC X(16)  VALUE '4540-MSG-IO-AREA'.           
041800 01  4540-MSG-IO-AREA.                                                    
041900     03  4540-LL              PIC S9(4)  VALUE +49 COMP SYNC.             
042000     03  4540-Z1              PIC X.                                      
042100     03  4540-Z2              PIC X.                                      
042200     03  4540-TRANSKOD        PIC X(8)   VALUE 'W4T540X '.                
042300     03  4540-IDTRANS         PIC X(4)   VALUE '4675'.                    
042400     03  4540-SPRAK           PIC X.                                      
042500*    03  -COPY W4I54001   -PRE 4540-                                      
042600     EJECT                                                                
042700                                                                          
042800 LINKAGE SECTION.                                                         
042900 01  IO-PCB                     PIC X.                                    
043000                                                                          
043100*01  -COPY W0009  -PRE MSG-                                               
043200     EJECT                                                                
043300*01  -COPY W0009  -PRE AD36-                                              
043400     EJECT                                                                
043500*01  -COPY W0009  -PRE ALT3-                                              
043600     EJECT                                                                
043700*01  -COPY W0009  -PRE ALT4-                                              
043800     EJECT                                                                
043900*01  -COPY W0009  -PRE L188-                                              
044000     EJECT                                                                
044100*01  -COPY W0009  -PRE L187-                                              
044200     EJECT                                                                
044300*01  -COPY W0008  -PRE WDE6-                                              
044400     05  FILLER                  PIC X.                                   
044500                                                                          
044600*01  -COPY W0008  -PRE WDE1-                                              
044700     05  FILLER                  PIC X.                                   
044800                                                                          
044900*01  -COPY W0008  -PRE WDE2-                                              
045000     05  FILLER                  PIC X.                                   
045100                                                                          
045200*01  -COPY W0008  -PRE WDB2-                                              
045300     05  FILLER                  PIC X.                                   
045400                                                                          
045500*01  -COPY W0008  -PRE 4495-                                              
045600     05  FILLER                  PIC X.                                   
045700                                                                          
045800*01  -COPY W0008  -PRE 4463-                                              
045900     05  FILLER                  PIC X.                                   
046000                                                                          
046100*01  -COPY W0008  -PRE 4513-                                              
046200     05  FILLER                  PIC X.                                   
046300                                                                          
046400*01  -COPY W0008  -PRE WDB6-                                              
046500     05  FILLER                  PIC X.                                   
046600                                                                          
046700 01  SHNO-4517-PCB               PIC X.                                   
046800     EJECT                                                                
046900 PROCEDURE DIVISION  USING IO-PCB   AD36-PCB ALT3-PCB ALT4-PCB            
047000                           L188-PCB L187-PCB                              
047100                           WDE6-PCB WDE1-PCB                              
047200                           WDE2-PCB WDB2-PCB 4495-PCB 4463-PCB            
047300                           4513-PCB WDB6-PCB SHNO-4517-PCB.               
047400                                                                          
047500 MAIN SECTION.                                                            
047600     ENTRY 'DLITCBL' USING IO-PCB   AD36-PCB ALT3-PCB ALT4-PCB            
047700                           L188-PCB L187-PCB                              
047800                           WDE6-PCB WDE1-PCB                              
047900                           WDE2-PCB WDB2-PCB 4495-PCB 4463-PCB            
048000                           4513-PCB WDB6-PCB SHNO-4517-PCB.               
048100                                                                          
048200     PERFORM S30-HAEMTA-ANROPSDATA                                        
048300                                                                          
048400     PERFORM A-INIT                                                       
048500     PERFORM B-CHECK-KEYS                                                 
048600     IF KEYS-OK                                                           
048700       PERFORM G-CHECK-INPUT                                              
048800       IF INDATA-OK                                                       
048900         IF REQU-KDTRTYP = 'X'                                            
049000*          -- BACKGROUND EXECUTION, ALWAYS RELEASE                        
049100             PERFORM I-REL-TRANSPORT                                      
049200         ELSE                                                             
049300*          -- FOREGROUND EXECUTION, RELEASE OR DISPLAY DATA               
049400           IF REQU-KDPGMACT = 'E'                                         
049500             PERFORM I-REL-TRANSPORT                                      
049600           ELSE                                                           
049700             PERFORM F-READ-SHOW-INFO                                     
049800           END-IF                                                         
049900         END-IF                                                           
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300     IF RESTART                                                           
050400       PERFORM J-RESTART-WL0188                                           
050500     ELSE                                                                 
050600       PERFORM S31-RETURNERA-SVAR                                         
050700     END-IF                                                               
050800     MOVE ZERO TO RETURN-CODE                                             
050900     GOBACK                                                               
051000     .                                                                    
051100     EJECT                                                                
051200                                                                          
051300 A-INIT SECTION.                                                          
051400     MOVE 'A-INIT          ' TO CURR-SECTION                              
051500                                                                          
051600     ACCEPT DAGENS-DATUM FROM DATE                                        
051700     ACCEPT DAGENS-TID   FROM TIME                                        
051800                                                                          
051900     MOVE ZERO        TO W-IDSHIPM                                        
052000     MOVE ZERO        TO W-IDDISTR-N                                      
052100     MOVE ALL '9'     TO W-IDKUNDNR-N                                     
052200                                                                          
052300     MOVE ALL '+' TO RESP-AREA                                            
052400     MOVE SPACE   TO RESP-IDMSG-INFO                                      
052500                     RESP-IDMSG-ERROR                                     
052600                     RESP-IDELMT-ERROR                                    
052700     MOVE 001     TO RESP-IDMSGVER                                        
052800     MOVE ZERO    TO RESP-KVRADER WS-KVRADER                              
052900                                                                          
053000     .                                                                    
053100     EJECT                                                                
053200                                                                          
053300 B-CHECK-KEYS SECTION.                                                    
053400     MOVE 'B-CHECK-KEYS    ' TO CURR-SECTION                              
053500                                                                          
053600     MOVE YES                  TO KEYS-SW                                 
053700                                                                          
053800*    -- CHECK OF WDGXKEY                                                  
053900                                                                          
054000                                                                          
054100     IF REQU-IDTRPTNR-KEY IN REQU-WL0188I1 NUMERIC                        
054200        IF REQU-IDTRPTNR-KEY IN REQU-WL0188I1 > ZERO                      
054300           MOVE REQU-IDTRPTNR-KEY IN REQU-WL0188I1  TO W-IDTRPTNR         
054400        ELSE                                                              
054500          MOVE 'INVALID IDTRPTNR-KEY' TO ERROR-TEXT                       
054600          CALL ABEND USING RKOD-ABEND-WITH-DUMP                           
054700        END-IF                                                            
054800     ELSE                                                                 
054900        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
055000     END-IF                                                               
055100                                                                          
055200     IF REQU-IDLBBET-KEY IN REQU-WL0188I1 = ALL '+'                       
055300        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
055400     ELSE                                                                 
055500       IF REQU-IDLBBET-KEY IN REQU-WL0188I1  > SPACE                      
055600          MOVE REQU-IDLBBET-KEY IN REQU-WL0188I1  TO W-IDLBBET            
055700       ELSE                                                               
055800          MOVE 'INVALID IDLBBET-KEY' TO ERROR-TEXT                        
055900          CALL ABEND USING RKOD-ABEND-WITH-DUMP                           
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300     IF REQU-KDFARLIG-KEY IN REQU-WL0188I1  NOT = ALL '+'                 
056400       MOVE REQU-KDFARLIG-KEY IN REQU-WL0188I1  TO W-KDFARLIG             
056500     END-IF                                                               
056600                                                                          
056700                                                                          
056800     MOVE REQU-IDDC-KEY IN REQU-WL0188I1   TO W-IDDC                      
056900                                              W-IDDC-4495                 
057000                                              W-IDDC-4463                 
057100                                                                          
057200     IF REQU-FLSKRIV-NU IN REQU-WL0188I1  NOT = ALL '+'                   
057300       MOVE REQU-FLSKRIV-NU IN REQU-WL0188I1  TO W-FLSKRIV-NU             
057400     END-IF                                                               
057500                                                                          
057600     IF REQU-IDSHIPM IN REQU-WL0188I1 NUMERIC                             
057700        IF REQU-IDSHIPM IN REQU-WL0188I1  > ZERO                          
057800           MOVE REQU-IDSHIPM IN REQU-WL0188I1  TO W-IDSHIPM               
057900        ELSE                                                              
058000           MOVE ZERO           TO W-IDSHIPM                               
058100        END-IF                                                            
058200     ELSE                                                                 
058300        MOVE ZERO             TO W-IDSHIPM                                
058400     END-IF                                                               
058500                                                                          
058600******** ADAPT DATE AND TIME FOR TIMEZONES                                
058700     MOVE W-IDDC        TO W-IDDC-B6                                      
058800     PERFORM IMS-GU-WDB601                                                
058900                                                                          
059000     MOVE '011'                TO MSGI-KDCALL                             
059100     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
059110     MOVE DCS-IDDC             TO MSGI-IDDC                               
059200     MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                           
059300     MOVE DAGENS-TID           TO MSGI-TILOKTID                           
059400     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
059500     MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                              
059600     MOVE MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                           
059700     MOVE FUNCTION CURRENT-DATE (13:2) TO DAGENS-TID(5:2)                 
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 F-READ-SHOW-INFO  SECTION.                                               
060200     MOVE 'F-READ-SHOW-INFO' TO CURR-SECTION                              
060300                                                                          
060400     PERFORM FA-READ-BASICDATA                                            
060500                                                                          
060600     MOVE W-IDSHIPM              TO RESP-IDSHIPM                          
060700                                                                          
060800     IF SEGMENT-MISSING                                                   
060900       MOVE KEYS-ARE-MISSING     TO RESP-IDMSG-ERROR                      
061000       MOVE ZERO                 TO RESP-KVRADER                          
061100     ELSE                                                                 
061200                                                                          
061300       MOVE +1 TO WS-KVRADER                                              
061400       PERFORM UNTIL WS-KVRADER > MAX-KVRADER OR SEGMENT-MISSING          
061500                                                                          
061510        IF 4498-FLCROSS = NOO      OR                                     
061520           4498-FLCROSS = SPACE    OR                                     
061530           4498-FLCROSS = LOW-VALUE                                       
061600         MOVE 4498-IDDISTR    TO RESP-IDDISTR (WS-KVRADER)                
061700                                 W-WDB201-IDDISTR                         
061800                                 W-WDE211-IDDISTR                         
061900                                 W-IDDISTR-N                              
062000                                 TEST-IDDISTR                             
062100         MOVE 4498-IDKUNDNR   TO RESP-IDKUNDNR (WS-KVRADER)               
062200                                 W-WDB201-IDKUNDNR                        
062300                                 W-WDE211-IDKUNDNR                        
062400*        -- ALSO RETURN ORIGINAL IDKUNDNR VALUE                           
062500*        -- SINCE RESP-IDKUNDNR MAY BE SET TO ZERO BELOW.                 
062600*        -- WL0191 NEEDS THIS VALUE.                                      
062700         MOVE 4498-IDKUNDNR   TO RESP-IDKUNDNR-ORIG (WS-KVRADER)          
062800                                                                          
062900         MOVE NOO                TO FLSAMFAK-SW                           
063000**         FLSAMFAK IS ALWAYS 'N' FOR FOR IDKUNDNR = ZERO                 
063100         IF 4498-IDKUNDNR NOT = ZERO                                      
063200           PERFORM IMS-GU-WDB201                                          
063300           IF SEGMENT-FOUND                                               
063400             IF DIST92-ITALY OR DIST92-GREECE                             
063500               MOVE NOO            TO GMT-FLSAMFAK                        
063600             END-IF                                                       
063700             IF GMT-FLSAMFAK = YES                                        
063800               MOVE GMT-FLSAMFAK TO FLSAMFAK-SW                           
063900               MOVE ZERO         TO RESP-IDKUNDNR (WS-KVRADER)            
064000                                    W-WDE211-IDKUNDNR                     
064100             END-IF                                                       
064200           END-IF                                                         
064300         END-IF                                                           
064400                                                                          
064500         IF FLSAMFAK                                                      
064600*           TO READ NEXT DISTRICT                                         
064700            MOVE ALL '9'           TO W-IDKUNDNR-N                        
064800         ELSE                                                             
064900*           TO READ NEXT CUSTOMER                                         
065000            MOVE 4498-IDKUNDNR     TO W-IDKUNDNR-N                        
065100         END-IF                                                           
065200                                                                          
065300         PERFORM IMS-GHNP-WDGX4498-N                                      
065400         ADD 1 TO WS-KVRADER                                              
065410        ELSE                                                              
065420         PERFORM IMS-GHNP-WDGX4498-N                                      
065430        END-IF                                                            
065500       END-PERFORM                                                        
065600                                                                          
065700       IF SEGMENT-FOUND                                                   
065800         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
065900       END-IF                                                             
066000       SUBTRACT 1 FROM WS-KVRADER                                         
066100       MOVE WS-KVRADER TO RESP-KVRADER                                    
066200                                                                          
066300     END-IF                                                               
066400     .                                                                    
066500     EJECT                                                                
066600 FA-READ-BASICDATA SECTION.                                               
066700                                                                          
066800     PERFORM IMS-GHU-WDR401-4495                                          
066900                                                                          
067000     IF SEGMENT-FOUND                                                     
067100       PERFORM IMS-GHNP-WDGX4498                                          
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600 G-CHECK-INPUT SECTION.                                                   
067700     MOVE 'G-CHECK-INPUT   ' TO CURR-SECTION                              
067800                                                                          
067900                                                                          
068000     MOVE YES  TO INDATA-SW                                               
068100     IF REQU-WL0188I1 = ALL '+'                                           
068200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
068300     ELSE                                                                 
068400       IF REQU-FLAVSLUTA = 'N' OR 'J'                                     
068500         CONTINUE                                                         
068600       ELSE                                                               
068700         MOVE 'INVALID FLAVSLUTA' TO ERROR-TEXT                           
068800         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
068900       END-IF                                                             
069000                                                                          
069100       IF REQU-KDTRTYP = 'X' OR 'U'                                       
069200         CONTINUE                                                         
069300       ELSE                                                               
069400         MOVE 'INVALID KDTRTYP'   TO ERROR-TEXT                           
069500         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
069600       END-IF                                                             
069700                                                                          
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 I-REL-TRANSPORT SECTION.                                                 
070200     MOVE 'I-REL-TRANSPORT ' TO CURR-SECTION                              
070300                                                                          
070400     MOVE ZERO               TO W-IDDISTR-PREV                            
070500                                W-IDKUNDNR-PREV                           
070600                                W-IDDEALER-PREV                           
070700                                W-IDPRODNR-PREV                           
070800                                                                          
070900     MOVE YES                TO FIRST-PRODNR-SW                           
071000                                FIRST-DEALER-SW                           
071100                                                                          
071200     MOVE NOO                TO RESTART-SW                                
071300                                FARLIGT-GOOD-4539                         
071400                                FARLIGT-GOOD-4540                         
071500                                FLSAMFAK-SW                               
071600                                4498-MISSING-SW                           
071700                                START-4540-SW                             
071800                                                                          
071900     MOVE ZERO               TO W-UPD-COUNT                               
072000                                                                          
072200     PERFORM S06-DC-LAND                                                  
072300                                                                          
072400     PERFORM IMS-GHU-WDR401-4495                                          
072500     IF SEGMENT-FOUND                                                     
072600                                                                          
072700        IF W-IDSHIPM = ZERO                                               
072800          PERFORM S01-CREATE-IDSHIPM                                      
072900          PERFORM S02-CREATE-WDE101                                       
073000          PERFORM S03-CREATE-WDE201                                       
073100        ELSE                                                              
073200          PERFORM IMS-GHU-WDE101                                          
073300          PERFORM IMS-GHU-WDE201                                          
073400        END-IF                                                            
073500                                                                          
073600        IF DCS-NDC-NA AND NOT NDC-US-BAT                                  
073700          PERFORM IJ-CREATE-4463-BL                                       
073800        END-IF                                                            
073900                                                                          
074000        PERFORM IMS-GHNP-WDGX4498                                         
074100                                                                          
074200        IF SEGMENT-FOUND                                                  
074300          PERFORM S11-CHECK-FLSAMFAK                                      
074400        ELSE                                                              
074500          MOVE YES TO 4498-MISSING-SW                                     
074600        END-IF                                                            
074700                                                                          
074800        PERFORM UNTIL 4498-MISSING  OR RESTART                            
074900                                                                          
075000          IF 4498-IDPRODNR   NOT =  W-IDPRODNR-PREV                       
075100            IF FIRST-PRODNR-SW  = NOO                                     
075200              PERFORM S04-CHECK-KDORDKL                                   
075300            ELSE                                                          
075400              MOVE NOO     TO FIRST-PRODNR-SW                             
075500            END-IF                                                        
075600                                                                          
075700            MOVE 4498-IDPRODNR TO W-IDPRODNR-PREV                         
075800          END-IF                                                          
075900                                                                          
076000          IF DCS-NDC-NA OR                                                
076100             DCS-SDC    OR                                                
076200             DCS-DDC                                                      
076300            IF FIRST-DEALER-SW = NOO                                      
076400              IF (4498-IDDISTR      NOT =  W-IDDISTR-PREV)  OR            
076500                 (4498-IDDEALER     NOT =  W-IDDEALER-PREV)               
076600                MOVE W-IDDISTR-PREV TO TEST-IDDISTR                       
076700                IF FARLIGT-GOOD-FOUND-4539 AND                            
076800                   DCS-FLWEBDC = JA                                       
076900                  IF DCS-NDC-NA            AND                            
077000                     DCS-CANADA            AND                            
077100                     W-KDORDKL-MAX < +2    AND                            
077200                     (DIST35-NA-TRANSFER    OR                            
077300                      DIST35-NA-CDC-RETURN  OR                            
077400                      DIST35-NA-NDC-RETURNS OR                            
077500                      DIST35-REFILL-INOM-NA OR                            
077600                      DIST07-CAN-RETAILER)                                
077700                    PERFORM S09A-START-W40539                             
077800                    MOVE NOO TO FARLIGT-GOOD-4539                         
077900                  ELSE                                                    
078000                    IF (DCS-SDC             OR                            
078100                       (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                 
078200                      PERFORM S09A-START-W40539                           
078300                      MOVE NOO TO FARLIGT-GOOD-4539                       
078400                    END-IF                                                
078500                  END-IF                                                  
078600                END-IF                                                    
078700              END-IF                                                      
078800            ELSE                                                          
078900              MOVE NOO      TO  FIRST-DEALER-SW                           
079000            END-IF                                                        
079100          END-IF                                                          
079200                                                                          
079300          IF (4498-IDDISTR      NOT = W-IDDISTR-PREV)  OR                 
079400             (4498-IDKUNDNR     NOT = W-IDKUNDNR-PREV)                    
079500                                                                          
079600            MOVE 4498-IDDISTR   TO W-WDE111-IDDISTR                       
079700                                   W-WDE211-IDDISTR                       
079800            MOVE 4498-IDKUNDNR  TO W-WDE111-IDKUNDNR                      
079900                                   W-WDE211-IDKUNDNR                      
080000            PERFORM S15-CREATE-WDE111                                     
080100            PERFORM S16-CREATE-WDE211                                     
080200                                                                          
080300          END-IF                                                          
080400                                                                          
080500          IF START-4540                                                   
080600            IF FARLIGT-GOOD-FOUND-4540                                    
080700              IF (DCS-SDC  OR                                             
080800                 (DCS-DDC  AND DCS-IDLANDX2 = 'SE'))                      
080900                PERFORM S09B-START-W40540                                 
081000                MOVE NOO TO FARLIGT-GOOD-4540                             
081100              END-IF                                                      
081200            END-IF                                                        
081300                                                                          
081400            MOVE NOO               TO START-4540-SW                       
081500                                                                          
081600            MOVE W-IDDISTR-PREV TO TEST-IDDISTR                           
081700          END-IF                                                          
081800                                                                          
081900          IF (4498-IDDISTR      NOT = W-IDDISTR-PREV)  OR                 
082000             (4498-IDKUNDNR     NOT = W-IDKUNDNR-PREV)                    
082100                                                                          
082200            MOVE 4498-IDKUNDNR  TO W-IDKUNDNR-PREV                        
082300            MOVE 4498-IDDISTR   TO W-IDDISTR-PREV                         
082400                                                                          
082500          END-IF                                                          
082600                                                                          
082700          MOVE 4498-IDDISTR TO TEST-IDDISTR                               
082800          PERFORM ID-UPDATE-KOLLI                                         
082900                                                                          
083000          PERFORM IH-CREATE-HTR4513                                       
083100                                                                          
083200          IF DCS-NDC-NA                                                   
083300            PERFORM IG-UPDATE-WDGX4468-BL                                 
083400          END-IF                                                          
083500                                                                          
083600          PERFORM S17-CREATE-WDE121                                       
083700                                                                          
083800          PERFORM S18-CREATE-WDE221                                       
083900                                                                          
084000          MOVE 4498-IDDISTR TO TEST-IDDISTR                               
084100          PERFORM IMS-DLET-WDGX4498                                       
084200                                                                          
084300          MOVE 4498-IDDEALER     TO W-IDDEALER-PREV                       
084400          MOVE FLSAMFAK-SW       TO FLSAMFAK-PREV-SW                      
084500          PERFORM IMS-GHNP-WDGX4498                                       
084600                                                                          
084700          IF SEGMENT-FOUND                                                
084800            IF FLSAMFAK                                                   
084900              IF W-IDTRPTNR < +100                                        
085000                IF (4498-IDDISTR NOT = W-IDDISTR-PREV)   OR               
085100                   (4498-IDKUNDNR NOT = W-IDKUNDNR-PREV)                  
085200                  MOVE YES         TO START-4540-SW                       
085300                END-IF                                                    
085400              ELSE                                                        
085500                IF 4498-IDDISTR NOT = W-IDDISTR-PREV                      
085600                  MOVE YES       TO START-4540-SW                         
085700                  IF W-UPD-COUNT  > W-UPD-MAX                             
085800                    MOVE YES     TO RESTART-SW                            
085900                  END-IF                                                  
086000                END-IF                                                    
086100              END-IF                                                      
086200            ELSE                                                          
086300              IF DIST92-ITALY  OR                                         
086400                 DIST92-GREECE                                            
086500                IF 4498-IDDISTR NOT = W-IDDISTR-PREV                      
086600                  MOVE YES       TO START-4540-SW                         
086700                                                                          
086800                  IF W-UPD-COUNT  > W-UPD-MAX                             
086900                    MOVE YES     TO RESTART-SW                            
087000                  END-IF                                                  
087100                END-IF                                                    
087200              ELSE                                                        
087300                IF (4498-IDDISTR NOT = W-IDDISTR-PREV)   OR               
087400                   (4498-IDKUNDNR NOT = W-IDKUNDNR-PREV)                  
087500                  MOVE YES       TO START-4540-SW                         
087600                  IF W-UPD-COUNT  > W-UPD-MAX                             
087700                    MOVE YES     TO RESTART-SW                            
087800                  END-IF                                                  
087900                END-IF                                                    
088000              END-IF                                                      
088100            END-IF                                                        
088200          ELSE                                                            
088300            MOVE YES TO 4498-MISSING-SW                                   
088400          END-IF                                                          
088500                                                                          
088600          IF NOT RESTART                                                  
088700            IF SEGMENT-FOUND                                              
088800              PERFORM S11-CHECK-FLSAMFAK                                  
088900            END-IF                                                        
089000          END-IF                                                          
089100                                                                          
089200        END-PERFORM                                                       
089300                                                                          
089400        PERFORM S04-CHECK-KDORDKL                                         
089500                                                                          
089600        IF FARLIGT-GOOD-FOUND-4540                                        
089700          IF (DCS-SDC OR                                                  
089800             (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                           
089900            PERFORM S09B-START-W40540                                     
090000                                                                          
090100            MOVE NOO TO FARLIGT-GOOD-4540                                 
090200          END-IF                                                          
090300          IF DCS-NDC-CN AND                                               
090400             DCS-FLWEBDC = JA                                             
090500            PERFORM S09B-START-W40540                                     
090600                                                                          
090700            MOVE NOO TO FARLIGT-GOOD-4540                                 
090800          END-IF                                                          
090900        END-IF                                                            
091000                                                                          
091100        MOVE W-IDDISTR-PREV TO TEST-IDDISTR                               
091200        IF FARLIGT-GOOD-FOUND-4539 AND                                    
091300           DCS-FLWEBDC = JA                                               
091400          IF DCS-NDC-NA               AND                                 
091500             DCS-CANADA               AND                                 
091600             W-KDORDKL-MAX < +2       AND                                 
091700             (DIST35-NA-TRANSFER       OR                                 
091800              DIST35-NA-CDC-RETURN     OR                                 
091900              DIST35-NA-NDC-RETURNS    OR                                 
092000              DIST35-REFILL-INOM-NA    OR                                 
092100              DIST07-CAN-RETAILER)                                        
092200            PERFORM S09A-START-W40539                                     
092300          ELSE                                                            
092400            IF (DCS-SDC                OR                                 
092500               (DCS-DDC AND DCS-IDLANDX2 = 'SE'))                         
092600              PERFORM S09A-START-W40539                                   
092700            END-IF                                                        
092800          END-IF                                                          
092900*       CALL FELLOG                                                       
093000        END-IF                                                            
093100                                                                          
093200        MOVE NOO                TO  FARLIGT-GOOD-4539                     
093300                                                                          
093400        PERFORM IMS-GHU-WDGX4498                                          
093500                                                                          
093600        IF SEGMENT-MISSING                                                
093700           PERFORM IMS-GHU-WDR401-4495                                    
093800            IF SEGMENT-FOUND                                              
093900              PERFORM IMS-DLET-WDR401-4495                                
094000                                                                          
094100              PERFORM IMS-GHU-WDE101                                      
094200              MOVE 'N'           TO  SHIP-KDKLAR                          
094300              MOVE VORD-IDDC-EXP TO SHIP-IDDC-EXP                         
094400              PERFORM IMS-REPL-WDE101                                     
094500                                                                          
094600              PERFORM IMS-GHU-WDE201                                      
094700              MOVE VORD-IDDC-EXP TO BILL-IDDC-EXP                         
094800              PERFORM IMS-REPL-WDE201                                     
094900                                                                          
095000              PERFORM S21-OPEN-WZ01                                       
095100              PERFORM S22-SEND-WZ01                                       
095200              PERFORM S23-CLOSE-WZ01                                      
095300            END-IF                                                        
095400        END-IF                                                            
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800                                                                          
095900 ID-UPDATE-KOLLI   SECTION.                                               
096000     MOVE 'ID-UPDATE-KOLLI ' TO CURR-SECTION                              
096100                                                                          
096200     MOVE 4498-IDPRODNR       TO   W-IDPRODNR-KOLLI                       
096300                                                                          
096400     MOVE 4498-IDKOLLI        TO W-IDKOLLI-KOLLI                          
096500     PERFORM IMS-GHU-WDE611                                               
096600     IF SEGMENT-FOUND                                                     
096700** CHECK KOLLI CONTAIN FARLIGT(HAZARDOUS) GOOD                            
096800       MOVE +1       TO IDPSN-IX                                          
096900       PERFORM UNTIL IDPSN-IX > +10                                       
097000          IF KOLLI-IDPSN(IDPSN-IX) > ZERO                                 
097100             MOVE YES TO FARLIGT-GOOD-4539                                
097200             MOVE YES TO FARLIGT-GOOD-4540                                
097300          END-IF                                                          
097400          ADD +1     TO IDPSN-IX                                          
097500       END-PERFORM                                                        
097600                                                                          
097700       IF 4498-FLCROSS = JA                                               
097800         CONTINUE                                                         
097900       ELSE                                                               
098000                                                                          
098100         IF KOLLI-KDKOLSTA > 6                                            
098200           MOVE 'KOLLI REDAN FAKTURERAT?' TO ERROR-TEXT                   
098300           CALL FELLOG                                                    
098400         END-IF                                                           
098500         MOVE KLI-PACK-FAKT     TO KOLLI-KDKOLSTA                         
098510         MOVE +9999999          TO KOLLI-TILASTID                         
098520         PERFORM IMS-REPL-WDE6                                            
098530         ADD +1                 TO W-UPD-COUNT                            
098540       END-IF                                                             
098550     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800 IJ-CREATE-4463-BL SECTION.                                               
098900                                                                          
099000     PERFORM IMS-GHU-WDR401-4463                                          
099100                                                                          
099200     MOVE MSGI-TILOKDAT               TO W-YYMMDD-DATUM                   
099300     MOVE W-YYMMDD-DATUM              TO W-YYMMDD                         
099400     MOVE '20'                        TO W-CC                             
099500     MOVE W-CCYYMMDD-DATUM            TO W-DASKEPPN-4464                  
099600                                         4464-DASKEPPN                    
099700     PERFORM IMS-ISRT-WDGX4464                                            
099800     ADD  +1                          TO W-UPD-COUNT                      
099900     MOVE W-IDTRPTNR                  TO W-IDTRPTNR-4466                  
100000                                         4466-IDTRPTNR                    
100100     MOVE W-IDLBBET                   TO W-IDLBBET-4466                   
100200                                         4466-IDLBBET                     
100300                                                                          
100400     PERFORM IMS-ISRT-WDGX4466                                            
100500     ADD +1                           TO W-UPD-COUNT                      
100600     .                                                                    
100700     EJECT                                                                
100800 IG-UPDATE-WDGX4468-BL SECTION.                                           
100900                                                                          
101000     MOVE 4498-IDDISTR                TO W-IDDISTR-4468                   
101100                                         4468-IDDISTR                     
101200     MOVE 4498-IDKUNDNR               TO W-IDKUNDNR-4468                  
101300                                         4468-IDKUNDNR                    
101400     MOVE 4498-IDKUNDRF               TO W-IDKUNDRF-4468                  
101500                                         4468-IDKUNDRF                    
101600     MOVE 4498-IDPRODNR               TO W-IDPRODNR-4468                  
101700                                         4468-IDPRODNR                    
101800     MOVE 4498-IDKOLLI                TO W-IDKOLLI-4468                   
101900                                         4468-IDKOLLI                     
102000                                                                          
102100     MOVE +1                          TO IDPSN-IX                         
102200     PERFORM UNTIL IDPSN-IX > +10                                         
102300        IF KOLLI-IDPSN(IDPSN-IX) > ZERO                                   
102400           MOVE KOLLI-IDPSN(IDPSN-IX) TO 4468-IDPSN(IDPSN-IX)             
102500        ELSE                                                              
102600           MOVE ZERO                  TO 4468-IDPSN(IDPSN-IX)             
102700        END-IF                                                            
102800        ADD +1                        TO IDPSN-IX                         
102900     END-PERFORM                                                          
103000                                                                          
103100     MOVE KOLLI-KDORDKL               TO 4468-KDORDKL                     
103200     MOVE 4498-VKORDBTO               TO 4468-VKORDBTO-KOLLI              
103300     MOVE 4498-VLORDBTO               TO 4468-VLORDBTO-KOLLI              
103400     MOVE 4498-KDKOLLI                TO 4468-KDKOLLI                     
103500                                                                          
103600     PERFORM IMS-ISRT-WDGX4468                                            
103700     ADD +1                           TO W-UPD-COUNT                      
103800     .                                                                    
103900     EJECT                                                                
104000 IH-CREATE-HTR4513 SECTION.                                               
104100     MOVE 'IH-CREATE-HTR451' TO CURR-SECTION                              
104200                                                                          
104300     IF FARLIGT-GOOD-FOUND-4539 OR                                        
104400        FARLIGT-GOOD-FOUND-4540                                           
104500       MOVE DAGENS-DATUM                TO W-YYMMDD-DATUM                 
104600       MOVE W-YYMMDD-DATUM              TO W-YYMMDD                       
104700       MOVE '20'                        TO W-CC                           
104800       MOVE W-CCYYMMDD-DATUM            TO 4514-DASKEPPN                  
104900                                           W-DASKEPPN-4514                
105000       PERFORM IMS-ISRT-WDGX4514                                          
105100       ADD +1                           TO W-UPD-COUNT                    
105200                                                                          
105300       MOVE W-IDDC                      TO 4516-IDDC                      
105400       MOVE W-IDSHIPM                   TO 4516-IDSKEPPN                  
105500       MOVE 4498-IDDISTR                TO 4516-IDDISTR                   
105600       MOVE 4498-IDKUNDNR               TO 4516-IDKUNDNR                  
105700       MOVE 4498-IDPRODNR               TO 4516-IDPRODNR                  
105800       MOVE 4498-IDKOLLI                TO 4516-IDKOLLI                   
105900       MOVE 4498-IDKUNDRF               TO 4516-IDKUNDRF                  
106000       PERFORM IMS-ISRT-WDGX4516                                          
106100       ADD +1                           TO W-UPD-COUNT                    
106200     END-IF                                                               
106300     .                                                                    
106400     EJECT                                                                
106500 J-RESTART-WL0188          SECTION.                                       
106600     MOVE 'J-RESTART-WL0188  ' TO CURR-SECTION                            
106700                                                                          
106800     MOVE W-IDTRPTNR          TO  REQU-IDTRPTNR-KEY                       
106900                                  IN REQU-WL0188I1                        
107000     MOVE W-IDLBBET           TO  REQU-IDLBBET-KEY                        
107100                                  IN REQU-WL0188I1                        
107200     MOVE W-IDDC              TO  REQU-IDDC-KEY                           
107300                                  IN REQU-WL0188I1                        
107400     MOVE W-KDFARLIG          TO  REQU-KDFARLIG-KEY                       
107500                                  IN REQU-WL0188I1                        
107600     MOVE W-FLSKRIV-NU        TO  REQU-FLSKRIV-NU                         
107700                                  IN REQU-WL0188I1                        
107800     MOVE W-IDSHIPM           TO  REQU-IDSHIPM                            
107900                                  IN REQU-WL0188I1                        
108000     MOVE YES                 TO  REQU-FLAVSLUTA                          
108100                                  IN REQU-WL0188I1                        
108200                                                                          
108300     MOVE REQU-AREA           TO  SEND-AREA-1                             
108400     PERFORM S28-SEND-TO-RESTART-THIS-PGM                                 
108500                                                                          
108600     .                                                                    
108700     EJECT                                                                
108800 S01-CREATE-IDSHIPM SECTION.                                              
108900     MOVE 'S01-CREATE-IDSHI' TO CURR-SECTION                              
109000                                                                          
109100     CALL W476SHNO USING SHNO-W476SHNO SHNO-4517-PCB                      
109200                                                                          
109300     MOVE SHNO-IDSHIPM            TO W-IDSHIPM                            
109400                                                                          
109500     ADD +1                       TO W-UPD-COUNT                          
109600     .                                                                    
109700     EJECT                                                                
109800                                                                          
109900 S02-CREATE-WDE101 SECTION.                                               
110000     MOVE 'S02-CREATE-WDE10' TO CURR-SECTION                              
110100                                                                          
110200     MOVE W-IDSHIPM           TO SHIP-IDSHIPM                             
110300     MOVE W-IDTRPTNR          TO SHIP-IDTRPTNR                            
110400     MOVE W-IDLBBET           TO SHIP-IDLBBET                             
110500     MOVE W-IDDC              TO SHIP-IDDC                                
110600     MOVE W-IDLANDX2          TO SHIP-IDLANDX3-SEND                       
110700     IF 4498-KDFAKTYP = 'N' OR 'K'                                        
110800       MOVE 'INT'             TO SHIP-KDFINDOC                            
110900     ELSE                                                                 
111000       MOVE 'INV'             TO SHIP-KDFINDOC                            
111100     END-IF                                                               
111200     MOVE DAGENS-DATUM        TO SHIP-TISKEPPN                            
111300     MOVE DAGENS-TID(1:4)     TO WS-TISKPTID-HHMM                         
111400     MOVE DAGENS-TID(5:2)     TO WS-TISKPTID-SS                           
111500     MOVE WS-TISKPTID         TO SHIP-TISKPTID                            
111600     MOVE ZERO                TO SHIP-KVANTEX                             
111700                                 SHIP-SUNTO-TOT                           
111801                                 SHIP-PRKURS-BET                          
111900     MOVE W-FLSKRIV-NU        TO SHIP-FLSKRIV-NU                          
112000** AFTER ALL UPDATE AND RELEASE TRANSPORT SHIP-KDKLAR CHANGES             
112100** TO 'N' AND SHIP-IDDC-EXP IS UPDATED                                    
112200     MOVE 'A'                 TO SHIP-KDKLAR                              
112300     MOVE SPACE               TO SHIP-IDDC-EXP                            
112400                                 SHIP-BELEVVIL                            
112500                                 SHIP-IDSYSTEM                            
112600                                 SHIP-KDVALISO-BET                        
112700     MOVE '0'                 TO SHIP-KDFAKSTA-EXP                        
112710                                                                          
112720     MOVE NOO                 TO SHIP-FLFARLIG                            
112730     MOVE SPACES              TO SHIP-KDVALISO-EXP                        
112740     MOVE ZEROES              TO SHIP-SUORDV-EXP                          
112750                                 SHIP-SUORDV-FAKT                         
112760                                 SHIP-VKORDBTO-FAKT                       
112770                                 SHIP-VLORDBTO-FAKT                       
112800     PERFORM IMS-ISRT-WDE101                                              
112900     ADD  +1                  TO W-UPD-COUNT                              
113000     .                                                                    
113100     EJECT                                                                
113200 S03-CREATE-WDE201 SECTION.                                               
113300     MOVE 'S03-CREATE-WDE20'  TO CURR-SECTION                             
113400                                                                          
113500     MOVE W-IDSHIPM           TO BILL-IDSHIPM                             
113600     MOVE W-IDDC              TO BILL-IDDC                                
113700     MOVE W-IDLANDX2          TO BILL-IDLANDX3-SEND                       
113800     MOVE SPACE               TO BILL-IDLEVNR                             
113900     IF 4498-KDFAKTYP = 'N' OR 'K'                                        
114000       MOVE 'INT'             TO BILL-KDFINDOC                            
114100     ELSE                                                                 
114200       MOVE 'INV'             TO BILL-KDFINDOC                            
114300     END-IF                                                               
114400     MOVE DAGENS-DATUM        TO BILL-TISKEPPN                            
114500     MOVE WS-TISKPTID         TO BILL-TISKPTID                            
114600     MOVE SPACE               TO BILL-IDDC-EXP                            
114700     PERFORM IMS-ISRT-WDE201                                              
114800     ADD  1                   TO W-UPD-COUNT                              
114900     .                                                                    
115000     EJECT                                                                
115100 S04-CHECK-KDORDKL SECTION.                                               
115200     MOVE 'S04-CHECK-KDORDK'  TO CURR-SECTION                             
115300                                                                          
115400     MOVE W-IDPRODNR-PREV     TO W-IDPRODNR-KOLLI                         
115500     PERFORM IMS-GHU-WDE601                                               
115600                                                                          
115700     IF VORD-KDORDKL          >  W-KDORDKL-MAX                            
115800        MOVE VORD-KDORDKL     TO W-KDORDKL-MAX                            
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200                                                                          
116300 S06-DC-LAND  SECTION.                                                    
116400     MOVE 'S06-DC-LAND     ' TO CURR-SECTION                              
116500                                                                          
116600     MOVE W-IDDC TO W-IDDC-B6                                             
116700     PERFORM IMS-GU-WDB601                                                
116800     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
116900     .                                                                    
117000     EJECT                                                                
117100 S09A-START-W40539  SECTION.                                              
117200                                                                          
117300     MOVE W-IDDISTR-PREV       TO W-IDDISTR-NUM                           
117400     MOVE W-IDDISTR-NUM        TO 4539-MID-IDDISTR                        
117500                                                                          
117600     MOVE W-IDDEALER-PREV      TO W-IDKUNDNR-NUM                          
117700     MOVE W-IDKUNDNR-NUM       TO 4539-MID-IDKUNDNR                       
117800                                                                          
117900     MOVE MFS-KDMFSFOR         TO 4539-SPRAK                              
118000     MOVE '99'                 TO 4539-MID-KDFRAKT                        
118100     MOVE W-IDSHIPM            TO 4539-MID-IDSKEPPN                       
118200     MOVE W-IDDC               TO 4539-MID-IDDC                           
118300     MOVE '4675'               TO 4539-MID-IDSYSTEM                       
118400     MOVE W-IDTRPTNR           TO 4539-MID-IDTRPTNR                       
118500                                                                          
118600     PERFORM IMS-PURG-TRANS4539                                           
118700     .                                                                    
118800     EJECT                                                                
118900 S09B-START-W40540  SECTION.                                              
119000                                                                          
119100     MOVE W-IDDISTR-PREV       TO W-IDDISTR-NUM                           
119200                                  TEST-IDDISTR                            
119300     MOVE W-IDDISTR-NUM        TO 4540-MID-IDDISTR                        
119400     MOVE W-IDDC               TO WS-IDDC                                 
119500                                                                          
119600     IF FLSAMFAK-PREV                                                     
119700       IF DIST92-FRANCE OR                                                
119800         (DIST67-FG-SE AND LDC-SE-1A)                                     
119900         MOVE W-IDKUNDNR-PREV  TO W-IDKUNDNR-NUM                          
120000         MOVE W-IDKUNDNR-NUM   TO 4540-MID-IDKUNDNR                       
120100       ELSE                                                               
120200         MOVE ZERO             TO 4540-MID-IDKUNDNR                       
120300       END-IF                                                             
120400     ELSE                                                                 
120500       MOVE W-IDKUNDNR-PREV    TO W-IDKUNDNR-NUM                          
120600       MOVE W-IDKUNDNR-NUM     TO 4540-MID-IDKUNDNR                       
120700     END-IF                                                               
120800                                                                          
120900     MOVE MFS-KDMFSFOR         TO 4540-SPRAK                              
121000                                                                          
121100     IF W-IDTRPTNR < +100                                                 
121200       MOVE '17'               TO 4540-MID-KDFRAKT                        
121300       MOVE W-IDKUNDNR-PREV    TO W-IDKUNDNR-NUM                          
121400       MOVE W-IDKUNDNR-NUM     TO 4540-MID-IDKUNDNR                       
121500     ELSE                                                                 
121600       IF LDC-SE-1A         AND                                           
121700          DIST67-FG-SE      AND                                           
121800          VORD-KDFRAKT = 32                                               
121900         MOVE '32'             TO 4540-MID-KDFRAKT                        
122000       ELSE                                                               
122100         MOVE '99'             TO 4540-MID-KDFRAKT                        
122200       END-IF                                                             
122300     END-IF                                                               
122400                                                                          
122500     MOVE W-IDSHIPM            TO 4540-MID-IDSKEPPN                       
122600     MOVE W-IDDC               TO 4540-MID-IDDC                           
122700     MOVE '4675'               TO 4540-MID-IDSYSTEM                       
122800     MOVE W-IDTRPTNR           TO 4540-MID-IDTRPTNR                       
122900                                                                          
123000     PERFORM IMS-PURG-TRANS4540                                           
123100     .                                                                    
123200     EJECT                                                                
123300 S11-CHECK-FLSAMFAK SECTION.                                              
123400     MOVE 'S11-CHECK-FLSAMF' TO CURR-SECTION                              
123500     SKIP2                                                                
123600*********************************************************                 
123700*                                                       *                 
123800* TO CHECK CUSTOMER FOR FLSAMFAK - CO-INVOICING         *                 
123900*                                                       *                 
124000*********************************************************                 
124100                                                                          
124200** FLSAMFAK IS SAME FOR ALL CUSTOMER IN ONE DISTRICT                      
124300     IF 4498-IDDISTR NOT = W-IDDISTR-PREV                                 
124400       MOVE NOO           TO FLSAMFAK-SW                                  
124500** FLSAMFAK IS ALWAYS 'N' FOR FOR IDKUNDNR = ZERO                         
124600       IF 4498-IDKUNDNR NOT = ZERO                                        
124700         MOVE 4498-IDDISTR  TO W-WDB201-IDDISTR                           
124800                               TEST-IDDISTR                               
124900         MOVE 4498-IDKUNDNR TO W-WDB201-IDKUNDNR                          
125000         PERFORM IMS-GU-WDB201                                            
125100         IF SEGMENT-FOUND                                                 
125200           IF DIST92-ITALY  OR                                            
125300              DIST92-GREECE                                               
125400             MOVE NOO          TO GMT-FLSAMFAK                            
125500           END-IF                                                         
125600           IF GMT-FLSAMFAK = YES                                          
125700             MOVE YES  TO FLSAMFAK-SW                                     
125800           END-IF                                                         
125900         END-IF                                                           
126000       END-IF                                                             
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400                                                                          
126500 S15-CREATE-WDE111 SECTION.                                               
126600     MOVE 'S15-CREATE-WDE11' TO CURR-SECTION                              
126700                                                                          
126800     MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                              
126900     MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                             
127000     MOVE  'N'              TO  SGMT-FLCOD                                
127100     MOVE  W-IDDC           TO  SGMT-IDDC                                 
127200     MOVE  SPACE            TO  SGMT-IDPARTNR                             
127300     MOVE  ZERO             TO  SGMT-KDFORSKN                             
127400                                SGMT-KDFKBIL                              
127500     MOVE  -1               TO  SGMT-KDLEVVIL                             
127600     MOVE  W-KDORDKL-MAX    TO  SGMT-KDORDKL-MAX                          
127700     MOVE  SPACE            TO  SGMT-KDVALISO                             
127800     MOVE  ZERO             TO  SGMT-PRKURS                               
127900     COMPUTE  SGMT-TISKEPPN-9KOMPL                                        
128000                            =   W-9KOMPL - SHIP-TISKEPPN                  
128100     PERFORM IMS-ISRT-WDE111                                              
128200     ADD  1                   TO W-UPD-COUNT                              
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600 S16-CREATE-WDE211 SECTION.                                               
128700     MOVE 'S16-CREATE-WDE21' TO CURR-SECTION                              
128800                                                                          
128900     MOVE 4498-IDDISTR      TO BGMT-IDDISTR                               
129000     MOVE 4498-IDKUNDNR     TO BGMT-IDKUNDNR                              
129100     MOVE SPACE             TO BGMT-IDPARTNR                              
129200                               BGMT-FLSEPINV                              
129300     MOVE 'N'               TO BGMT-FLCOD                                 
129400     MOVE ZERO              TO BGMT-KDLEVVIL                              
129500                               BGMT-PRAVDRAG                              
129600                               BGMT-PREMBHNT                              
129700                               BGMT-PRFOERS                               
129800                               BGMT-PRFRAKT                               
129900                               BGMT-PRLEGKST                              
130000                               BGMT-REAVDRAG                              
130100                               BGMT-REEMBHNT                              
130200                               BGMT-REFOERS                               
130300                               BGMT-RELEGKST                              
130400                               BGMT-REOVKOFF                              
130500                                                                          
130600     PERFORM IMS-ISRT-WDE211                                              
130700     ADD  1                   TO W-UPD-COUNT                              
130800     .                                                                    
130900     EJECT                                                                
131000                                                                          
131100 S17-CREATE-WDE121 SECTION.                                               
131200     MOVE 'S17-CREATE-WDE12' TO CURR-SECTION                              
131300                                                                          
131400     MOVE 4498-IDPRODNR          TO SKOLLI-IDPRODNR                       
131500     MOVE 4498-IDKOLLI           TO SKOLLI-IDKOLLI                        
131600     MOVE 4498-IDKOLLI-SAMP      TO SKOLLI-IDKOLLI-SAMP                   
131700     MOVE ZERO                   TO SKOLLI-DIKOLLIB                       
131800                                    SKOLLI-DIKOLLIH                       
131900                                    SKOLLI-DIKOLLIL                       
132000     MOVE SPACE                  TO SKOLLI-FLDIRLEV                       
132100     MOVE ZERO                   TO SKOLLI-IDORDER                        
132200                                    SKOLLI-KDEMBTYP                       
132300                                    SKOLLI-KDFRAKT                        
132400                                    SKOLLI-KDFARLIG-KOLLI                 
132500     MOVE SPACE                  TO SKOLLI-KDKOLLI                        
132600     MOVE ZERO                   TO SKOLLI-KVFLAMP-KOLLI                  
132700                                    SKOLLI-SUORDV-LOC                     
132800                                    SKOLLI-SUORDV-LOCPREL                 
132900     MOVE SPACE                  TO SKOLLI-KDVALISO                       
133000                                    SKOLLI-KDVALISO-EXP                   
133100     MOVE ZERO                   TO SKOLLI-SUORDV                         
133200                                    SKOLLI-SUORDV-EXP                     
133300                                    SKOLLI-TIPACKN                        
133400                                    SKOLLI-VKORDBTO-KOLLI                 
133500                                    SKOLLI-VKORDNTO-KOLLI                 
133600                                    SKOLLI-VLORDBTO-KOLLI                 
133700     MOVE 4498-IDDISTR           TO SKOLLI-IDDISTR                        
133800     MOVE 4498-IDKUNDNR          TO SKOLLI-IDKUNDNR                       
133900     MOVE 4498-IDKUNDRF          TO SKOLLI-IDKUNDRF                       
134000     MOVE 4498-KDFAKTYP          TO SKOLLI-KDFAKTYP                       
134100     MOVE -1                     TO SKOLLI-KDORDKL                        
134200     MOVE ZERO                   TO SKOLLI-TIORDREG                       
134300                                    SKOLLI-IDFAKT                         
134400                                    SKOLLI-IDFAKT-EXP                     
134410*CO-CD                                                                    
134420     MOVE 4498-FLCROSS           TO SKOLLI-FLCROSS                        
134500                                                                          
134600     PERFORM IMS-ISRT-WDE121                                              
134700     ADD  1                   TO  W-UPD-COUNT                             
134800     .                                                                    
134900     EJECT                                                                
135000                                                                          
135100 S18-CREATE-WDE221 SECTION.                                               
135200     MOVE 'S18-CREATE-WDE22' TO CURR-SECTION                              
135300                                                                          
135400     MOVE 4498-IDPRODNR          TO BKOLLI-IDPRODNR                       
135500     MOVE 4498-IDKOLLI           TO BKOLLI-IDKOLLI                        
135600     MOVE 4498-IDDISTR           TO BKOLLI-IDDISTR                        
135700     MOVE 4498-IDKUNDNR          TO BKOLLI-IDKUNDNR                       
135800     MOVE 4498-IDKUNDRF          TO BKOLLI-IDKUNDRF                       
135900     MOVE SPACE                  TO BKOLLI-FLOVRLEV                       
136000                                    BKOLLI-KDFAKTYP                       
136100     MOVE -1                     TO BKOLLI-KDORDKL                        
136200     MOVE SPACE                  TO BKOLLI-KDPRSTA                        
136300     MOVE ZERO                   TO BKOLLI-VKORDBTO-KOLLI                 
136310*CO-CD                                                                    
136320     MOVE 4498-FLCROSS           TO BKOLLI-FLCROSS                        
136400                                                                          
136500     IF KOLLI-IDKUNDNR NOT = W-WDE211-IDKUNDNR                            
136600*    IF 4498-IDKUNDNR NOT = W-WDE211-IDKUNDNR                             
136700       IF DCS-SDC AND DCS-IDLANDX2 = 'IT'                                 
136800         CONTINUE                                                         
136900       ELSE                                                               
137000         MOVE 'KUND UNDER FEL ROT'  TO ERROR-TEXT                         
137100         CALL FELLOG                                                      
137200       END-IF                                                             
137300     END-IF                                                               
137400     PERFORM IMS-ISRT-WDE221                                              
137500     ADD  1                   TO W-UPD-COUNT                              
137600     .                                                                    
137700     EJECT                                                                
137800                                                                          
137900 S21-OPEN-WZ01 SECTION.                                                   
138000     MOVE 'S21-OPEN-WZ01   ' TO CURR-SECTION                              
138100                                                                          
138200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
138300     MOVE 'CARPARTS.PULS.ADDIT   '   TO SEND-ADDISPABS                    
138400     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
138500                                                                          
138600     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
138700                                SEND-OPEN-AREA                            
138800     IF SEND-KDRC > 0                                                     
138900       MOVE SEND-KDRC           TO KDRC-DISPLAY                           
139000       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISPLAY                      
139100            DELIMITED BY SIZE INTO ERROR-TEXT                             
139200       CALL FELLOG                                                        
139300     END-IF                                                               
139400     .                                                                    
139500     EJECT                                                                
139600 S22-SEND-WZ01 SECTION.                                                   
139700     MOVE 'S22-SEND-WZ01   ' TO CURR-SECTION                              
139800     MOVE W-IDSHIPM          TO MOD4636-MID-IDSHIPM                       
139900     MOVE 'J'                TO MOD4636-MID-KDTRPINF                      
140000     MOVE ZERO               TO MOD4636-MID-IDDISTR                       
140100                                MOD4636-MID-IDKUNDNR                      
140200                                MOD4636-MID-IDPRODNR                      
140300                                MOD4636-MID-IDKOLLI                       
140400                                MOD4636-MID-SUORDV-DIST                   
140500                                MOD4636-MID-SUORDV-DIST-LOC               
140600                                MOD4636-MID-SUORDV-DIST-PREL              
140700                                MOD4636-MID-SUORDV-NOLL                   
140800                                MOD4636-MID-SUORDV-NOLL-LOC               
140900                                MOD4636-MID-SUORDV-NOLL-PREL              
141000                                MOD4636-MID-KDORDKL                       
141100     MOVE 'PUT'                      TO SEND-KDFUNC                       
141200     COMPUTE SEND-KVDLEN = LENGTH OF MOD4636-MID-W40636I1                 
141300                                                                          
141400     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
141500                                SEND-KVDLEN                               
141600                                MOD4636-MID-W40636I1                      
141700     IF SEND-KDRC > 0                                                     
141800       MOVE SEND-KDRC           TO KDRC-DISPLAY                           
141900       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISPLAY                       
142000            DELIMITED BY SIZE INTO ERROR-TEXT                             
142100       CALL FELLOG                                                        
142200     END-IF                                                               
142300     .                                                                    
142400     EJECT                                                                
142500 S23-CLOSE-WZ01  SECTION.                                                 
142600     MOVE 'S22-CLOSE-WZ01  ' TO CURR-SECTION                              
142700                                                                          
142800     MOVE 'CLOSE'               TO SEND-KDFUNC                            
142900                                                                          
143000     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
143100     IF SEND-KDRC > 0                                                     
143200       MOVE SEND-KDRC           TO KDRC-DISPLAY                           
143300       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISPLAY                     
143400            DELIMITED BY SIZE INTO ERROR-TEXT                             
143500       CALL FELLOG                                                        
143600     END-IF                                                               
143700     .                                                                    
143800     EJECT                                                                
143900                                                                          
144000 S28-SEND-TO-RESTART-THIS-PGM  SECTION .                                  
144100     MOVE 'S28-SEND-TO-REST' TO CURR-SECTION                              
144200                                                                          
144300     PERFORM S28-RESTART-OPEN                                             
144400     PERFORM S28-RESTART-SEND                                             
144500     PERFORM S28-RESTART-CLOSE                                            
144600     .                                                                    
144700     SKIP3                                                                
144800 S28-RESTART-OPEN  SECTION.                                               
144900     MOVE 'S28-RESTART-OPEN' TO CURR-SECTION                              
145000                                                                          
145100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
145200     MOVE 'CARPARTS.LDC.TRANSPSUPPL1BG'  TO SEND-ADDISPABS                
145300     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
145400                                                                          
145500     IF SEND-KDRC > 0                                                     
145600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
145700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
145800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
145900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
146000     END-IF                                                               
146100     .                                                                    
146200     SKIP3                                                                
146300 S28-RESTART-SEND SECTION.                                                
146400     MOVE 'S28-RESTART-SEND' TO CURR-SECTION                              
146500                                                                          
146600     MOVE 'PUT'                      TO SEND-KDFUNC                       
146700     MOVE LENGTH OF SEND-AREA-1      TO SEND-KVDLEN                       
146800     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
146900                                           SEND-AREA-1                    
147000                                                                          
147100     IF SEND-KDRC > 0                                                     
147200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
147300       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
147400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
147500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
147600     END-IF                                                               
147700     .                                                                    
147800     SKIP3                                                                
147900 S28-RESTART-CLOSE SECTION.                                               
148000     MOVE 'S28-RESTART-CLOS' TO CURR-SECTION                              
148100                                                                          
148200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
148300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
148400                                                                          
148500     IF SEND-KDRC > 0                                                     
148600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
148700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
148800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
148900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
149000     END-IF                                                               
149100     .                                                                    
149200     EJECT                                                                
149300 S30-HAEMTA-ANROPSDATA SECTION.                                           
149400     MOVE 'S30-HAEMTA-ANROPSDATA' TO CURR-SECTION                         
149500                                                                          
149600     MOVE 'GETARG'               TO SUB-KDFUNC                            
149700     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
149800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
149900                                                                          
150000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
150100                                                                          
150200     IF SUB-KDRC > 0                                                      
150300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
150400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
150500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
150600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
150700     END-IF                                                               
150800     .                                                                    
150900     SKIP3                                                                
151000 S31-RETURNERA-SVAR SECTION.                                              
151100     MOVE 'S31-RETURNERA-SVAR   ' TO CURR-SECTION                         
151200                                                                          
151300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
151400     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA                             
151500             - (100 - WS-KVRADER) * LENGTH OF RESP-RADER                  
151600                                                                          
151700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
151800                                                                          
151900     IF SUB-KDRC > 0                                                      
152000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
152100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
152200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
152300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
152400     END-IF                                                               
152500     .                                                                    
152600     EJECT                                                                
152700                                                                          
152800 IMS-PURG-TRANS4539 SECTION.                                              
152900                                                                          
153000     MOVE LOW-VALUE            TO 4539-Z1                                 
153100                                  4539-Z2                                 
153200     MOVE '    '               TO GOOD-STATUSCODES                        
153300     CALL CBLTDLI USING PURG ALT3-PCB 4539-MSG-IO-AREA                    
153400     MOVE ALT3-STATUS-CODE   TO STATUS-WS                                 
153500     PERFORM IMS-STATUSCHECK                                              
153600     .                                                                    
153700     EJECT                                                                
153800 IMS-PURG-TRANS4540 SECTION.                                              
153900                                                                          
154000     MOVE LOW-VALUE            TO 4540-Z1                                 
154100                                  4540-Z2                                 
154200     MOVE '    '               TO GOOD-STATUSCODES                        
154300     CALL CBLTDLI USING PURG ALT4-PCB 4540-MSG-IO-AREA                    
154400     MOVE ALT4-STATUS-CODE   TO STATUS-WS                                 
154500     PERFORM IMS-STATUSCHECK                                              
154600     .                                                                    
154700     EJECT                                                                
154800 IMS-GHU-WDR401-4495 SECTION.                                             
154900     MOVE 'GHU-WDR401-4495 ' TO CURR-IMS-SECTION                          
155000                                                                          
155100     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
155200                      DELIMITED BY SIZE INTO SSA1                         
155300     MOVE '  GE' TO GOOD-STATUSCODES                                      
155400     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-WDGX4495 SSA1                 
155500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
155600     PERFORM IMS-STATUSCHECK                                              
155700     .                                                                    
155800     SKIP2                                                                
155900                                                                          
156000 IMS-DLET-WDR401-4495 SECTION.                                            
156100     MOVE 'DLET-WDR401-4495' TO CURR-IMS-SECTION                          
156200     MOVE '    ' TO GOOD-STATUSCODES                                      
156300     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-WDGX4495                     
156400     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
156500     PERFORM IMS-STATUSCHECK                                              
156600     .                                                                    
156700     SKIP2                                                                
156800                                                                          
156900 IMS-GHNP-WDGX4498    SECTION.                                            
157000     MOVE 'GHNP-WDGX4498   ' TO CURR-IMS-SECTION                          
157100                                                                          
157200     MOVE 'WDGX4498' TO SSA1                                              
157300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
157400     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-WDGX4498  SSA1               
157500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
157600     PERFORM IMS-STATUSCHECK                                              
157700     .                                                                    
157800     SKIP2                                                                
157900                                                                          
158000 IMS-GHNP-WDGX4498-N     SECTION.                                         
158100                                                                          
158200     STRING 'WDGX4498(WDGXKEY  >' W-4498-N-X ')'                          
158300                      DELIMITED BY SIZE INTO SSA1                         
158400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
158500     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-WDGX4498  SSA1               
158600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
158700     PERFORM IMS-STATUSCHECK                                              
158800     .                                                                    
158900     SKIP2                                                                
159000                                                                          
159100 IMS-GHU-WDGX4498        SECTION.                                         
159200     MOVE 'GHU-WDGX4498    ' TO CURR-IMS-SECTION                          
159300                                                                          
159400     STRING 'WDR401  (WDGXKEY  =' W-4495-X ')'                            
159500                      DELIMITED BY SIZE INTO SSA1                         
159600     MOVE 'WDGX4498 '  TO  SSA2                                           
159700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
159800     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-WDGX4498 SSA1 SSA2            
159900     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
160000     PERFORM IMS-STATUSCHECK                                              
160100     .                                                                    
160200     SKIP2                                                                
160300 IMS-DLET-WDGX4498    SECTION.                                            
160400     MOVE 'DLET-WDGX4498   ' TO CURR-IMS-SECTION                          
160500     MOVE '    ' TO GOOD-STATUSCODES                                      
160600     CALL CBLTDLI USING DLET 4495-PCB DLI-IO-WDGX4498                     
160700     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
160800     PERFORM IMS-STATUSCHECK                                              
160900     .                                                                    
161000     SKIP2                                                                
161100                                                                          
161200 IMS-GHU-WDR401-4463  SECTION.                                            
161300                                                                          
161400     STRING 'WDR401  (WDGXKEY  =' W-4463-X ')'                            
161500             DELIMITED BY SIZE INTO SSA1                                  
161600     MOVE '  ' TO GOOD-STATUSCODES                                        
161700     CALL CBLTDLI USING GHU 4463-PCB DLI-IO-WDGX4463 SSA1                 
161800     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
161900     PERFORM IMS-STATUSCHECK                                              
162000     .                                                                    
162100     SKIP3                                                                
162200 IMS-ISRT-WDGX4464 SECTION.                                               
162300                                                                          
162400     STRING 'WDR401  (WDGXKEY  =' W-4463-X ')'                            
162500             DELIMITED BY SIZE INTO SSA1                                  
162600     MOVE 'WDGX4464' TO SSA2                                              
162700     MOVE '  II' TO GOOD-STATUSCODES                                      
162800     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WDGX4464 SSA1 SSA2           
162900     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
163000     PERFORM IMS-STATUSCHECK                                              
163100     .                                                                    
163200     EJECT                                                                
163300 IMS-ISRT-WDGX4466 SECTION.                                               
163400                                                                          
163500     STRING 'WDR401  (WDGXKEY  =' W-4463-X ')'                            
163600             DELIMITED BY SIZE INTO SSA1                                  
163700     STRING 'WDGX4464(DASKEPPN =' W-DASKEPPN-X ')'                        
163800             DELIMITED BY SIZE INTO SSA2                                  
163900     MOVE   'WDGX4466' TO SSA3                                            
164000     MOVE '  II' TO GOOD-STATUSCODES                                      
164100     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WDGX4466 SSA1 SSA2           
164200                                                      SSA3                
164300     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
164400     PERFORM IMS-STATUSCHECK                                              
164500     .                                                                    
164600     SKIP3                                                                
164700 IMS-ISRT-WDGX4468 SECTION.                                               
164800                                                                          
164900     STRING 'WDGX4464(DASKEPPN =' W-DASKEPPN-X ')'                        
165000             DELIMITED BY SIZE INTO SSA1                                  
165100     STRING 'WDGX4466(KY4466   =' W-4466-X ')'                            
165200             DELIMITED BY SIZE INTO SSA2                                  
165300     MOVE 'WDGX4468' TO SSA3                                              
165400     MOVE '  ' TO GOOD-STATUSCODES                                        
165500     CALL CBLTDLI USING ISRT 4463-PCB DLI-IO-WDGX4468                     
165600                        SSA1 SSA2 SSA3                                    
165700     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
165800     PERFORM IMS-STATUSCHECK                                              
165900     .                                                                    
166000     EJECT                                                                
166100 IMS-GHU-WDE601 SECTION.                                                  
166200     MOVE 'GHU-WDE601      ' TO CURR-IMS-SECTION                          
166300                                                                          
166400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
166500            DELIMITED BY SIZE INTO SSA1                                   
166600     MOVE '  ' TO GOOD-STATUSCODES                                        
166700     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
166800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
166900     PERFORM IMS-STATUSCHECK                                              
167000     .                                                                    
167100     EJECT                                                                
167200 IMS-GHU-WDE611 SECTION.                                                  
167300     MOVE 'GHU-WDE611      ' TO CURR-IMS-SECTION                          
167400                                                                          
167500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
167600            DELIMITED BY SIZE INTO SSA1                                   
167700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
167800            DELIMITED BY SIZE INTO SSA2                                   
167900     MOVE '  GE' TO GOOD-STATUSCODES                                      
168000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
168100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
168200     PERFORM IMS-STATUSCHECK                                              
168300     .                                                                    
168400     EJECT                                                                
168500 IMS-REPL-WDE6 SECTION.                                                   
168600     MOVE 'REPL-WDE6       ' TO CURR-IMS-SECTION                          
168700                                                                          
168800     MOVE '  ' TO GOOD-STATUSCODES                                        
168900     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
169000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
169100     PERFORM IMS-STATUSCHECK                                              
169200     .                                                                    
169300     EJECT                                                                
169400 IMS-ISRT-WDGX4514 SECTION.                                               
169500     MOVE 'ISRT-WDGX4514   ' TO CURR-IMS-SECTION                          
169600                                                                          
169700     STRING 'WDR401  (WDGXKEY  =' W-4513-X ')'                            
169800             DELIMITED BY SIZE INTO SSA1                                  
169900     MOVE 'WDGX4514' TO SSA2                                              
170000     MOVE '  II' TO GOOD-STATUSCODES                                      
170100     CALL CBLTDLI USING ISRT 4513-PCB DLI-IO-WDGX4514                     
170200                             SSA1 SSA2                                    
170300     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
170400     PERFORM IMS-STATUSCHECK                                              
170500     SKIP3                                                                
170600     .                                                                    
170700 IMS-ISRT-WDGX4516 SECTION.                                               
170800     MOVE 'ISRT-WDGX4516   ' TO CURR-IMS-SECTION                          
170900                                                                          
171000     STRING 'WDR401  (WDGXKEY  =' W-4513-X ')'                            
171100             DELIMITED BY SIZE INTO SSA1                                  
171200     STRING 'WDGX4514(DASKEPPN =' W-4514-X ')'                            
171300             DELIMITED BY SIZE INTO SSA2                                  
171400     MOVE 'WDGX4516' TO SSA3                                              
171500     MOVE '  ' TO GOOD-STATUSCODES                                        
171600     CALL CBLTDLI USING ISRT 4513-PCB DLI-IO-WDGX4516                     
171700                             SSA1 SSA2 SSA3                               
171800     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
171900     PERFORM IMS-STATUSCHECK                                              
172000     .                                                                    
172100     EJECT                                                                
172200 IMS-GU-WDB201 SECTION.                                                   
172300     MOVE 'GU-WDB201       ' TO CURR-IMS-SECTION                          
172400                                                                          
172500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
172600          DELIMITED BY SIZE INTO SSA1                                     
172700     MOVE '  GE' TO GOOD-STATUSCODES                                      
172800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
172900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
173000     PERFORM IMS-STATUSCHECK                                              
173100     .                                                                    
173200     SKIP3                                                                
173300 IMS-GHU-WDE101 SECTION.                                                  
173400     MOVE 'GHU-WDE101      ' TO CURR-IMS-SECTION                          
173500                                                                          
173600     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
173700          DELIMITED BY SIZE INTO SSA1                                     
173800     MOVE '  ' TO GOOD-STATUSCODES                                        
173900     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
174000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
174100     PERFORM IMS-STATUSCHECK                                              
174200     .                                                                    
174300     EJECT                                                                
174400 IMS-REPL-WDE101 SECTION.                                                 
174500     MOVE 'REPL-WDE101     ' TO CURR-IMS-SECTION                          
174600                                                                          
174700     MOVE '  ' TO GOOD-STATUSCODES                                        
174800     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
174900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
175000     PERFORM IMS-STATUSCHECK                                              
175100     .                                                                    
175200     EJECT                                                                
175300 IMS-ISRT-WDE101 SECTION.                                                 
175400     MOVE 'ISRT-WDE101     ' TO CURR-IMS-SECTION                          
175500                                                                          
175600     MOVE 'WDE101  ' TO SSA1                                              
175700     MOVE '  II' TO GOOD-STATUSCODES                                      
175800     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
175900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
176000     PERFORM IMS-STATUSCHECK                                              
176100     .                                                                    
176200     EJECT                                                                
176300 IMS-ISRT-WDE111 SECTION.                                                 
176400     MOVE 'ISRT-WDE111     ' TO CURR-IMS-SECTION                          
176500                                                                          
176600     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
176700          DELIMITED BY SIZE INTO SSA1                                     
176800     MOVE 'WDE111  ' TO SSA2                                              
176900     MOVE '  II' TO GOOD-STATUSCODES                                      
177000     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
177100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
177200     PERFORM IMS-STATUSCHECK                                              
177300     .                                                                    
177400     EJECT                                                                
177500 IMS-ISRT-WDE121 SECTION.                                                 
177600     MOVE 'ISRT-WDE121     ' TO CURR-IMS-SECTION                          
177700                                                                          
177800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
177900          DELIMITED BY SIZE INTO SSA1                                     
178000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
178100          DELIMITED BY SIZE INTO SSA2                                     
178200     MOVE 'WDE121 ' TO SSA3                                               
178300     MOVE '  II' TO GOOD-STATUSCODES                                      
178400     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3        
178500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
178600     PERFORM IMS-STATUSCHECK                                              
178700     .                                                                    
178800     EJECT                                                                
178900 IMS-GHU-WDE201 SECTION.                                                  
179000     MOVE 'GHU-WDE201      ' TO CURR-IMS-SECTION                          
179100                                                                          
179200     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
179300          DELIMITED BY SIZE INTO SSA1                                     
179400     MOVE '  ' TO GOOD-STATUSCODES                                        
179500     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
179600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
179700     PERFORM IMS-STATUSCHECK                                              
179800     .                                                                    
179900     EJECT                                                                
180000                                                                          
180100 IMS-ISRT-WDE201 SECTION.                                                 
180200     MOVE 'ISRT-WDE201     ' TO CURR-IMS-SECTION                          
180300                                                                          
180400     MOVE 'WDE201  ' TO SSA1                                              
180500     MOVE '  II' TO GOOD-STATUSCODES                                      
180600     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
180700     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
180800     PERFORM IMS-STATUSCHECK                                              
180900     .                                                                    
181000     EJECT                                                                
181100 IMS-REPL-WDE201 SECTION.                                                 
181200     MOVE 'REPL-WDE201     ' TO CURR-IMS-SECTION                          
181300                                                                          
181400     MOVE '  ' TO GOOD-STATUSCODES                                        
181500     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE201                       
181600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
181700     PERFORM IMS-STATUSCHECK                                              
181800     .                                                                    
181900     EJECT                                                                
182000 IMS-ISRT-WDE211 SECTION.                                                 
182100     MOVE 'ISRT-WDE211     ' TO CURR-IMS-SECTION                          
182200                                                                          
182300     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
182400          DELIMITED BY SIZE INTO SSA1                                     
182500     MOVE 'WDE211  ' TO SSA2                                              
182600     MOVE '  II' TO GOOD-STATUSCODES                                      
182700     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
182800     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
182900     PERFORM IMS-STATUSCHECK                                              
183000     .                                                                    
183100     EJECT                                                                
183200                                                                          
183300 IMS-ISRT-WDE221 SECTION.                                                 
183400     MOVE 'ISRT-WDE221     ' TO CURR-IMS-SECTION                          
183500                                                                          
183600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
183700          DELIMITED BY SIZE INTO SSA1                                     
183800     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
183900          DELIMITED BY SIZE INTO SSA2                                     
184000     MOVE 'WDE221  ' TO SSA3                                              
184100     MOVE '  II' TO GOOD-STATUSCODES                                      
184200     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE221 SSA1 SSA2 SSA3        
184300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
184400     PERFORM IMS-STATUSCHECK                                              
184500     .                                                                    
184600     EJECT                                                                
184700 IMS-GU-WDB601    SECTION.                                                
184800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
184900          DELIMITED BY SIZE INTO SSA1                                     
185000     MOVE '    ' TO GOOD-STATUSCODES                                      
185100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
185200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
185300     PERFORM IMS-STATUSCHECK                                              
185400     .                                                                    
185500 IMS-STATUSCHECK SECTION.                                                 
185600                                                                          
185700     SET STATUS-IX TO 1                                                   
185800     SEARCH GOOD-STATUS                                                   
185900       AT END                                                             
186000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
186100         DELIMITED BY SIZE INTO ERROR-TEXT                                
186200         CALL FELLOG                                                      
186300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
186400         CONTINUE                                                         
186500     END-SEARCH                                                           
186600     .                                                                    
