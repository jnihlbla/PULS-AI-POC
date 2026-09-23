000100*COMPOPT DB2BIND=YES            -- REMOVE IF PGM USES DB2 DIRECTLY        
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL060200.                                                
000400 AUTHOR.         ASPFJÄLL MARKUS.                                         
000500 DATE-WRITTEN.   08/04/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       FREELOCATIONQUERY                                        
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        FREE LOCTION UPDATE                                              
001200*        UPPDATERA OLIKA LAGERPLATSER FÖR LDC                             
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDK7                                       
001500*        PROGRAMMET LÄSER      WDD8 BUFFER                                
001600*        PROGRAMMET LÄSER      WDR2 HTYP 6335, 6341 OCH 6345              
001700*        PROGRAMMET UPPDATERAR WDJ8                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: WL0602T                                             
002100*        REQUEST:     WL0602I1                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        RESPONSE:    WL0602O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'WL060200'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  WS-DATUM                    PIC 9(8).                                
004800 77  INDX                        PIC S9(3)  VALUE ZERO COMP-3.            
004900 77  TAB-INDX                    PIC  9(3)  VALUE ZERO.                   
005000 77  MAX-INDX                    PIC S9(3)  VALUE +500 COMP-3.            
005100 77  W-KVPLATS-WDJ8              PIC S9(5)  VALUE +0   COMP-3.            
005200 77  W-KVPLATS-WDK7              PIC 9(3)   VALUE 0.                      
005300 77  W-KVPLATS-WDD8              PIC 9(3)   VALUE 0.                      
005400 77  W-KVPLATS-TOT               PIC 9(3)   VALUE 0.                      
005500 77  W-KVPLATS                   PIC 9(3)   VALUE 0.                      
005600 77  WS-ADPLATS-COMP             PIC S9(5)  VALUE +0 COMP-3.              
005700 77  WS-ADPLATS-NUM              PIC  9(5).                               
005800                                                                          
005900 77  WS-SEQA-ADLAGOMR            PIC S9(3) VALUE +0 COMP-3.               
006000 77  WS-SEQA-ADGANG              PIC S9(3) VALUE +0 COMP-3.               
006100 77  WS-SEQA-ADPLATS             PIC S9(5) VALUE +0 COMP-3.               
006200 77  WS-REDCBEL                  PIC 9(3)V9(1) VALUE ZERO.                
006300                                                                          
006400                                                                          
006500 01  WS-ADPLATS.                                                          
006600     03 WS-BAY                    PIC 9(2) VALUE ZERO.                    
006700     03 WS-ADLEVEL                PIC 9(2) VALUE ZERO.                    
006800     03 WS-ADSEQ                  PIC 9(1) VALUE ZERO.                    
006900                                                                          
007000 01  WS-ADPLATS-MIN.                                                      
007100     03 WS-BAY-MIN                PIC 9(2) VALUE ZERO.                    
007200     03 WS-ADLEVEL-MIN            PIC 9(2) VALUE ZERO.                    
007300     03 WS-ADSEQ-MIN              PIC 9(1) VALUE ZERO.                    
007400                                                                          
007500 01  WS-ADPLATS-MAX.                                                      
007600     03 WS-BAY-MAX                PIC 9(2) VALUE 99.                      
007700     03 WS-ADLEVEL-MAX            PIC 9(2) VALUE 99.                      
007800     03 WS-ADSEQ-MAX              PIC 9(1) VALUE 9.                       
007900                                                                          
008000                                                                          
008100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008200     88  NYCKLAR-OK                          VALUE 'J'.                   
008300     88  NYCKLAR-FEL                         VALUE 'N'.                   
008400                                                                          
008500 77  INDATA-SW                   PIC X       VALUE 'N'.                   
008600     88  INDATA-OK                           VALUE 'J'.                   
008700     88  INDATA-FEL                          VALUE 'N'.                   
008800                                                                          
008900 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
009000     88  POST-FINNS                          VALUE 'J'.                   
009100     88  POST-SAKNAS                         VALUE 'N'.                   
009200                                                                          
009300 77  TABELL-1-SW                 PIC X       VALUE 'N'.                   
009400     88  TABELL-1                            VALUE 'J'.                   
009500                                                                          
009600 77  TABELL-2-SW                 PIC X       VALUE 'N'.                   
009700     88  TABELL-2                            VALUE 'J'.                   
009800                                                                          
009900 77  DATA-SW                     PIC X       VALUE 'N'.                   
010000     88  DATA-FINNS                          VALUE 'J'.                   
010100     88  DATA-SAKNAS                         VALUE 'N'.                   
010200                                                                          
010300                                                                          
010400 01  WS-YYMMDDHHMM.                                                       
010500     03 WS-YYMMDD                PIC  9(6).                               
010600     03 WS-TIME                  PIC  9(4).                               
010700                                                                          
010800 01  WS-HHMMSSTH                 PIC  9(8).                               
010900 01  FILLER REDEFINES WS-HHMMSSTH.                                        
011000       03  WS-HHMM               PIC 9(4).                                
011100       03  WS-SSTH               PIC 9(4).                                
011200     EJECT                                                                
011300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011400 01  GENERELLA-SUBPROGRAM.                                                
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
011800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012000     SKIP3                                                                
012100*    --- PARAMETRAR TILL ABEND                                            
012200                                                                          
012300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012600     SKIP3                                                                
012700 01  MESSAGE-CODES.                                                       
012800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
012900     EJECT                                                                
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013200     SKIP3                                                                
013300*01  -COPY WZ01SUB                                                        
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
013600     SKIP3                                                                
013700 01  REQU-AREA.                                                           
013800*    03  -COPY WZ01REQU                                                   
013900*    03  -COPY WL0602I1                                                   
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014200     SKIP3                                                                
014300 01  RESP-AREA.                                                           
014400*    03  -COPY WZ01RESP                                                   
014500*    03  -COPY WL0602O1                                                   
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014800     SKIP3                                                                
014900*01  -COPY WZ01SEND                                                       
015000 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
015100 01  HDR-AREA.                                                            
015200*    03  -COPY WZ01REQU  -PRE HDR-                                        
015300*    03  -COPY WZ04HDR                                                    
015400*                                                                         
015500 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
015600 01  DOC-AREA.                                                            
015700*    03  -COPY WL06021                                                    
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
016000     SKIP3                                                                
016100     EJECT                                                                
016200 01  NYCKLAR-TILL-DLI.                                                    
016300     03  W-IDARTNR-X.                                                     
016400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016500     03  W-ADLAGOMR-X.                                                    
016600         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
016700     03  W-ADGANG-X.                                                      
016800         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
016900     03  W-BAY-X.                                                         
017000         05  W-BAY               PIC 9(2)    VALUE ZERO.                  
017100     03  W-IDDC-X.                                                        
017200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017300                                                                          
017400     03  W-WDJ8KEY-X.                                                     
017500         05  W-LOC-IDDC          PIC X(2)   VALUE SPACE.                  
017600         05  W-LOC-ADLAGOMR      PIC 9(2)   VALUE ZERO.                   
017700         05  W-LOC-ADGANG        PIC 9(2)   VALUE ZERO.                   
017800         05  W-LOC-ADPLATS.                                               
017900             07 W-LOC-BAY        PIC 9(2) VALUE ZERO.                     
018000             07 W-LOC-ADLEVEL    PIC 9(2) VALUE ZERO.                     
018100             07 W-LOC-ADSEQ      PIC 9(1) VALUE ZERO.                     
018200                                                                          
018300     03  W-WDJ8KEY-MIN-X.                                                 
018400         05  W-LOC-IDDC-MIN       PIC X(2)   VALUE SPACE.                 
018500         05  W-LOC-ADLAGOMR-MIN   PIC 9(2)   VALUE ZERO.                  
018600         05  W-LOC-ADGANG-MIN     PIC 9(2)   VALUE ZERO.                  
018700         05  W-LOC-ADPLATS-MIN.                                           
018800             07 W-LOC-BAY-MIN     PIC 9(2) VALUE ZERO.                    
018900             07 W-LOC-ADLEVEL-MIN PIC 9(2) VALUE ZERO.                    
019000             07 W-LOC-ADSEQ-MIN   PIC 9(1) VALUE ZERO.                    
019100     03  W-WDJ8KEY-MAX-X.                                                 
019200         05  W-LOC-IDDC-MAX       PIC X(2)   VALUE SPACE.                 
019300         05  W-LOC-ADLAGOMR-MAX   PIC 9(2)   VALUE 99.                    
019400         05  W-LOC-ADGANG-MAX     PIC 9(2)   VALUE 99.                    
019500         05  W-LOC-ADPLATS-MAX.                                           
019600             07 W-LOC-BAY-MAX     PIC 9(2) VALUE 99.                      
019700             07 W-LOC-ADLEVEL-MAX PIC 9(2) VALUE 99.                      
019800             07 W-LOC-ADSEQ-MAX   PIC 9(1) VALUE 9.                       
019900                                                                          
020000     03   W-WDD8A1KY-MIN-X.                                               
020100          05 W-IDDC-D8-MIN-X     PIC X(2)           VALUE SPACE.          
020200          05 W-ADBUFFOM-D8-MIN-X PIC S9(3)  COMP-3  VALUE ZERO.           
020300          05 W-ADBUFGAN-D8-MIN-X PIC S9(3)  COMP-3  VALUE ZERO.           
020400          05 W-ADBUFPL-D8-MIN-X  PIC S9(5)  COMP-3  VALUE ZERO.           
020500          05 W-DABUFPAF-D8-MIN-X PIC 9(8)           VALUE ZERO.           
020600          05 W-IDARTNR-D8-MIN-X  PIC S9(9)  COMP-3  VALUE ZERO.           
020700                                                                          
020800     03 W-WDD8A1KY-MAX-X.                                                 
020900        05 W-IDDC-D8-MAX-X     PIC X(2)         VALUE SPACE.              
021000        05 W-ADBUFFOM-D8-MAX-X PIC S9(3) COMP-3 VALUE +999.               
021100        05 W-ADBUFGAN-D8-MAX-X PIC S9(3) COMP-3 VALUE +999.               
021200        05 W-ADBUFPL-D8-MAX-X  PIC S9(5) COMP-3 VALUE +99999.             
021300        05 W-DABUFPAF-D8-MAX-X PIC 9(8)       VALUE 99999999.             
021400        05 W-IDARTNR-D8-MAX-X  PIC S9(9) COMP-3 VALUE +999999999.         
021500                                                                          
021600     03 WDK7A1KY-MIN-X.                                                   
021700        05 W-IDDC-K7-MIN        PIC X(2)         VALUE SPACE.             
021800        05 W-ADART-K7-MIN.                                                
021900          07  W-ADLAGOMR-K7-MIN PIC S9(3)  COMP-3  VALUE ZERO.            
022000          07  W-ADGANG-K7-MIN   PIC S9(3)  COMP-3  VALUE ZERO.            
022100          07  W-ADPLATS-K7-MIN  PIC S9(5)  COMP-3  VALUE ZERO.            
022200        05 W-IDARTNR-K7-MIN     PIC S9(9)  COMP-3  VALUE ZERO.            
022300                                                                          
022400     03 WDK7A1KY-MAX-X.                                                   
022500        05  W-IDDC-K7-MAX       PIC X(2)           VALUE SPACE.           
022600        05  W-ADART-K7-MAX.                                               
022700          07  W-ADLAGOMR-K7-MAX PIC S9(3) COMP-3 VALUE +999.              
022800          07  W-ADGANG-K7-MAX   PIC S9(3) COMP-3 VALUE +999.              
022900          07  W-ADPLATS-K7-MAX  PIC S9(5) COMP-3 VALUE +99999.            
023000        05 W-IDARTNR-K7-MAX     PIC S9(9) COMP-3 VALUE +999999999.        
023100                                                                          
023200     03  W-WDGXKEY-6335-X.                                                
023300         05  W-IDHTYP            PIC X(4)    VALUE '6335'.                
023400         05  W-6335-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
023500                                                                          
023600     03  W-WDGXKEY-6336-X.                                                
023700         05  W-IDDC-6336         PIC X(2)    VALUE SPACE.                 
023800                                                                          
023900     03  W-WDGXKEY-6338-X.                                                
024000      05  W-IDARTNR-6338      PIC S9(9)  COMP-3 VALUE ZERO.               
024100      05  W-ADLAGOMR-6338     PIC S9(3)  COMP-3 VALUE ZERO.               
024200      05  W-ADGANG-6338       PIC S9(3)  COMP-3 VALUE ZERO.               
024300      05  W-ADPLATS-6338      PIC S9(5)  COMP-3 VALUE ZERO.               
024400                                                                          
024500     03  W-WDGXKEY-6338-MIN-X.                                            
024600      05  W-IDARTNR-6338-MIN  PIC S9(9) COMP-3 VALUE +000000000.          
024700      05  W-ADLAGOMR-6338-MIN PIC S9(3) COMP-3 VALUE +000.                
024800      05  W-ADGANG-6338-MIN   PIC S9(3) COMP-3 VALUE +000.                
024900      05  W-ADPLATS-6338-MIN  PIC S9(5) COMP-3 VALUE ZERO.                
025000                                                                          
025100     03  W-WDGXKEY-6338-MAX-X.                                            
025200       05  W-IDARTNR-6338-MAX  PIC S9(9) COMP-3 VALUE +999999999.         
025300       05  W-ADLAGOMR-6338-MAX PIC S9(3) COMP-3 VALUE +999.               
025400       05  W-ADGANG-6338-MAX   PIC S9(3) COMP-3 VALUE +999.               
025500       05  W-ADPLATS-6338-MAX  PIC S9(5) COMP-3 VALUE ZERO.               
025600                                                                          
025700     03  W-WDGXKEY-6341-X.                                                
025800         05  W-IDHTYP            PIC X(4)    VALUE '6341'.                
025900         05  W-6341-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
026000                                                                          
026100     03  W-WDGXKEY-6342-X.                                                
026200         05  W-IDDC-6342         PIC X(2)    VALUE SPACE.                 
026300                                                                          
026400     03  W-WDGXKEY-6344-X.                                                
026500         05  W-ADLAGOMR-6344      PIC S9(3)   VALUE ZERO.                 
026600         05  W-ADGANG-6344        PIC S9(3)   VALUE ZERO.                 
026700         05  W-ADPLDEL-6344       PIC 9(2)    VALUE ZERO.                 
026800                                                                          
026900     03  W-WDGXKEY-6345-X.                                                
027000         05  W-IDHTYP            PIC X(4)    VALUE '6345'.                
027100         05  W-6345-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
027200                                                                          
027300     03  W-WDGXKEY-6346-X.                                                
027400         05  W-IDDC-6346         PIC X(2)    VALUE SPACE.                 
027500                                                                          
027600     03  W-WDGXKEY-6348-X.                                                
027700         05  W-ADLAGOMR-6348      PIC S9(3)   VALUE ZERO.                 
027800         05  W-ADGANG-6348        PIC S9(3)   VALUE ZERO.                 
027900         05  W-ADPLDEL-6348       PIC 9(2)    VALUE ZERO.                 
028000     EJECT                                                                
028100                                                                          
028200 01    FILLER          PIC X(16)   VALUE '     IMS-WS     '.              
028300 01    STATUS-WS       PIC XX.                                            
028400         88  SEGMENT-FINNS       VALUE '  '.                              
028500         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
028600         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
028700         88  END-OF-DB           VALUE 'GB'.                              
028800                                                                          
028900 01    GODK-STATUSKODER.                                                  
029000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029100                                                                          
029200 01      SSA1            PIC X(128) VALUE SPACE.                          
029300 01      SSA2            PIC X(128) VALUE SPACE.                          
029400 01      SSA3            PIC X(128) VALUE SPACE.                          
029500                                                                          
029600*                            IMS FUNKTIONSKODER                           
029700*01      -COPY W0003                                                      
029800                                                                          
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ801'.                      
030000 01  DLI-IO-WDJ801.                                                       
030100*    03  -COPY WDJ801                                                     
030200     EJECT                                                                
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
030400 01  DLI-IO-WDK7A1.                                                       
030500*    03  -COPY WDK7A1.                                                    
030600     EJECT                                                                
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD8A1'.                      
030800 01  DLI-IO-WDD8A1.                                                       
030900*    03  -COPY WDD8A1                                                     
031000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
031100     SKIP3                                                                
031200 01  DLI-IO-WDGX01  .                                                     
031300*        05  -COPY WDGX01                                                 
031400                                                                          
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6336'.                    
031600     SKIP3                                                                
031700 01  DLI-IO-WDGX6336.                                                     
031800*        05  -COPY WDGX6336                                               
031900     EJECT                                                                
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6338'.                    
032100     SKIP3                                                                
032200 01  DLI-IO-WDGX6338.                                                     
032300*        05  -COPY WDGX6338                                               
032400                                                                          
032500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6342'.                    
032600     SKIP3                                                                
032700 01  DLI-IO-WDGX6342.                                                     
032800*        05  -COPY WDGX6342                                               
032900     EJECT                                                                
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6344'.                    
033100     SKIP3                                                                
033200 01  DLI-IO-WDGX6344.                                                     
033300*        05  -COPY WDGX6344                                               
033400                                                                          
033500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6346'.                    
033600     SKIP3                                                                
033700 01  DLI-IO-WDGX6346.                                                     
033800*        05  -COPY WDGX6346                                               
033900     EJECT                                                                
034000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6348'.                    
034100     SKIP3                                                                
034200 01  DLI-IO-WDGX6348.                                                     
034300*        05  -COPY WDGX6348                                               
034400                                                                          
034500     EJECT                                                                
034600 LINKAGE SECTION.                                                         
034700*01  -COPY W0009  -PRE MSG-     PIC X.                                    
034800                                                                          
034900 01  DAP-PCB                    PIC X.                                    
035000                                                                          
035100*01  -COPY W0008  -PRE WDJ8-                                              
035200     05  FILLER                  PIC X.                                   
035300*01  -COPY W0008  -PRE WDK7A-                                             
035400     05  FILLER                  PIC X.                                   
035500*01  -COPY W0008  -PRE WDD8A-                                             
035600     05  FILLER                  PIC X.                                   
035700*01  -COPY W0008  -PRE WDR21-                                             
035800     05  FILLER                  PIC X.                                   
035900*01  -COPY W0008  -PRE WDR22-                                             
036000     05  FILLER                  PIC X.                                   
036100*01  -COPY W0008  -PRE WDR23-                                             
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB                                
036500                     WDJ8-PCB WDK7A-PCB WDD8A-PCB                         
036600                    WDR21-PCB WDR22-PCB WDR23-PCB.                        
036700 MAIN SECTION.                                                            
036800     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB                                
036900                     WDJ8-PCB WDK7A-PCB WDD8A-PCB                         
037000                    WDR21-PCB WDR22-PCB WDR23-PCB.                        
037100                                                                          
037200     PERFORM S01-HAEMTA-ANROPSDATA                                        
037300     IF SUB-KDRC = 0                                                      
037400       PERFORM A-INIT                                                     
037500       PERFORM B-KOLLA-NYCKLAR                                            
037600       IF NYCKLAR-OK                                                      
037700         IF REQU-KDPGMACT = 'S'                                           
037800           PERFORM F-LAES-VISA-INFO                                       
037900*                                                                         
038000         ELSE                                                             
038100           IF REQU-KDPGMACT = 'P'                                         
038200             PERFORM G-PRINT-LISTA                                        
038300           END-IF                                                         
038400         END-IF                                                           
038500       END-IF                                                             
038600       PERFORM S02-RETURNERA-SVAR                                         
038700*      PERFORM S04-SKICKA-OPEN                                            
038800*      PERFORM S04-SKICKA-MEDDELANDE                                      
038900*      PERFORM S04-SKICKA-CLOSE                                           
039000     END-IF                                                               
039100                                                                          
039200                                                                          
039300     MOVE ZERO TO RETURN-CODE                                             
039400     GOBACK                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 A-INIT SECTION.                                                          
039800*    IF REQU-KDPGMACT = 'S'                                               
039900       MOVE ALL '+'   TO RESP-AREA                                        
040000*    ELSE                                                                 
040100*      IF REQU-KDPGMACT = 'E'                                             
040200*        MOVE ALL SPACE TO RESP-WL0602O1                                  
040300*      END-IF                                                             
040400*    END-IF                                                               
040500                                                                          
040600     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
040700                       RESP-IDMSG-INFO                                    
040800                       RESP-IDELMT-ERROR                                  
040900     MOVE 001       TO RESP-IDMSGVER                                      
041000     MOVE ZERO      TO RESP-KVRADER                                       
041100                                                                          
041200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
041300     MOVE REQU-IDDC-KEY TO W-IDDC                                         
041400                           RESP-IDDC-KEY                                  
041500     MOVE 001           TO RESP-IDMSGVER                                  
041600                                                                          
041700     ACCEPT WS-YYMMDD      FROM DATE                                      
041800     ACCEPT WS-HHMMSSTH    FROM TIME                                      
041900     MOVE WS-HHMM          TO WS-TIME                                     
042000                                                                          
042100     .                                                                    
042200     EJECT                                                                
042300 B-KOLLA-NYCKLAR SECTION.                                                 
042400                                                                          
042500     MOVE JA TO NYCKLAR-SW                                                
042600     IF REQU-IDDC-KEY     NOT = ALL '+'                                   
042700       MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                
042800                             W-IDDC                                       
042900                             W-LOC-IDDC                                   
043000                             W-LOC-IDDC-MIN                               
043100                             W-LOC-IDDC-MAX                               
043200                             W-IDDC-K7-MIN                                
043300                             W-IDDC-K7-MAX                                
043400     ELSE                                                                 
043500       MOVE NEJ TO INDATA-SW                                              
043600       MOVE 'IDDC'         TO RESP-IDELMT-ERROR                           
043700     END-IF                                                               
043800     IF REQU-ADLAGOMR-KEY NOT = ALL '+'                                   
043900       IF REQU-ADLAGOMR-KEY NUMERIC                                       
044000         MOVE REQU-ADLAGOMR-KEY  TO W-ADLAGOMR                            
044100                                    RESP-ADLAGOMR-KEY                     
044200                                    W-LOC-ADLAGOMR                        
044300                                    W-LOC-ADLAGOMR-MIN                    
044400                                    W-LOC-ADLAGOMR-MAX                    
044500                                    W-ADLAGOMR-6338-MIN                   
044600                                    W-ADLAGOMR-6338-MAX                   
044700       ELSE                                                               
044800         MOVE NEJ                TO NYCKLAR-SW                            
044900       END-IF                                                             
045000     ELSE                                                                 
045100       MOVE NEJ                  TO NYCKLAR-SW                            
045200       MOVE 'ADLAGOMR'           TO RESP-IDELMT-ERROR                     
045300     END-IF                                                               
045400                                                                          
045500     IF REQU-ADGANG-KEY    NOT = ALL '+'                                  
045600       IF REQU-ADGANG-KEY   NUMERIC                                       
045700         MOVE REQU-ADGANG-KEY    TO W-ADGANG                              
045800                                    RESP-ADGANG-KEY                       
045900                                    W-LOC-ADGANG                          
046000                                    W-LOC-ADGANG-MIN                      
046100                                    W-LOC-ADGANG-MAX                      
046200                                    W-ADGANG-6338-MIN                     
046300                                    W-ADGANG-6338-MAX                     
046400       ELSE                                                               
046500         MOVE NEJ                TO NYCKLAR-SW                            
046600         MOVE 'ADGANG'           TO RESP-IDELMT-ERROR                     
046700       END-IF                                                             
046800     ELSE                                                                 
046900*      MOVE ZERO                 TO RESP-ADGANG-KEY                       
047000       MOVE ZERO                 TO W-ADGANG                              
047100                                                                          
047200     END-IF                                                               
047300                                                                          
047400     IF REQU-BAY-KEY        NOT = ALL '+'                                 
047500       IF REQU-BAY-KEY      NUMERIC                                       
047600         MOVE REQU-BAY-KEY       TO W-BAY                                 
047700                                    WS-BAY                                
047800                                    WS-BAY-MIN                            
047900                                    WS-BAY-MAX                            
048000                                    RESP-BAY-KEY                          
048100                                    W-LOC-BAY                             
048200                                    W-LOC-BAY-MIN                         
048300                                    W-LOC-BAY-MAX                         
048400                                    W-ADPLATS-6338-MIN                    
048500                                    W-ADPLATS-6338-MAX                    
048600       ELSE                                                               
048700         MOVE NEJ                TO NYCKLAR-SW                            
048800         MOVE 'BAY'              TO RESP-IDELMT-ERROR                     
048900       END-IF                                                             
049000     ELSE                                                                 
049100*      MOVE ZERO                 TO RESP-BAY-KEY                          
049200       MOVE ZERO                 TO W-BAY                                 
049300                                                                          
049400     END-IF                                                               
049500     IF REQU-BAY-KEY        NOT = ALL '+'                                 
049600       IF REQU-ADGANG-KEY = ALL '+'                                       
049700         MOVE NEJ TO NYCKLAR-SW                                           
049800         MOVE 'ADGANG' TO RESP-IDELMT-ERROR                               
049900       END-IF                                                             
050000       IF REQU-ADLAGOMR-KEY = ALL '+'                                     
050100         MOVE NEJ TO NYCKLAR-SW                                           
050200         MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                             
050300       END-IF                                                             
050400     END-IF                                                               
050500                                                                          
050600     IF NYCKLAR-FEL                                                       
050700       IF REQU-FL-NOEXIST   = 'Y' OR                                      
050800          REQU-FL-DEVIATION = 'Y' OR                                      
050900          REQU-FL-COVER     = 'Y'                                         
051000         MOVE JA TO NYCKLAR-SW                                            
051100       ELSE                                                               
051200         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
051300       END-IF                                                             
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 F-LAES-VISA-INFO SECTION.                                                
051800     IF REQU-FL-NOEXIST   = 'N' AND                                       
051900        REQU-FL-DEVIATION = 'N' AND                                       
052000        REQU-FL-COVER     = 'N'                                           
052100                                                                          
052200*      PERFORM FA-VANLIG-SOK ***VISA BARA EN TOT RAD                      
052300       PERFORM FB-VANLIG-SOK                                              
052400     END-IF                                                               
052500     IF REQU-FL-NOEXIST   = 'Y' AND                                       
052600        REQU-FL-DEVIATION = 'N' AND                                       
052700        REQU-FL-COVER     = 'N'                                           
052800       PERFORM FC-SOK-NOEXIST                                             
052900     ELSE                                                                 
053000       IF REQU-FL-NOEXIST   = 'N' AND                                     
053100          REQU-FL-DEVIATION = 'Y' AND                                     
053200          REQU-FL-COVER     = 'N'                                         
053300         PERFORM FD-SOK-DEVIATION                                         
053400       ELSE                                                               
053500         IF REQU-FL-NOEXIST   = 'N' AND                                   
053600            REQU-FL-DEVIATION = 'N' AND                                   
053700            REQU-FL-COVER     = 'Y'                                       
053800           PERFORM FE-SOK-COVER                                           
053900**         FILL RATE                                                      
054000         END-IF                                                           
054100       END-IF                                                             
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500 FA-VANLIG-SOK     SECTION.                                               
054600     MOVE '1' TO RESP-WEB-LAYOUT                                          
054700     MOVE NEJ TO DATA-SW                                                  
054800     IF REQU-ADLAGOMR-KEY NUMERIC AND                                     
054900        REQU-ADGANG-KEY   NUMERIC AND                                     
055000        REQU-BAY-KEY      NUMERIC                                         
055100* ALLA 3 NYCKLAR INAMATAT                                                 
055200       PERFORM IMS-GET-WDJ801                                             
055300* BUFFERT  PLATSER                                                        
055400       IF SEGMENT-FINNS                                                   
055500         MOVE LOC-KVPLATS         TO W-KVPLATS-WDJ8                       
055600         MOVE JA                  TO DATA-SW                              
055700                                                                          
055800         MOVE REQU-IDDC-KEY       TO W-IDDC-D8-MIN-X                      
055900                                     W-IDDC-D8-MAX-X                      
056000         MOVE REQU-ADLAGOMR-KEY   TO W-ADBUFFOM-D8-MIN-X                  
056100                                     W-ADBUFFOM-D8-MAX-X                  
056200         MOVE REQU-ADGANG-KEY     TO W-ADBUFGAN-D8-MIN-X                  
056300                                     W-ADBUFGAN-D8-MAX-X                  
056400         MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                       
056500         MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MIN-X                   
056600                                                                          
056700         PERFORM IMS-GU-WDD801                                            
056800                                                                          
056900         IF SEGMENT-FINNS                                                 
057000         MOVE 1 TO INDX                                                   
057100           PERFORM UNTIL SEGMENT-SAKNAS                                   
057200             COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                  
057300*            MOVE SEQA-ADBUFFOMR  TO RESP-ADLAGOMR-UT(INDX)               
057400*            MOVE SEQA-ADBUFFGANG TO RESP-ADGANG-UT  (INDX)               
057500*            MOVE SEQA-ADBUFFPL   TO RESP-ADPLATS-UT (INDX)               
057600*            MOVE W-KVPLATS-WDJ8  TO RESP-KVPLATS-UT (INDX)               
057700*            ADD +1 TO INDX                                               
057800             IF W-KVPLATS-WDJ8 <= 0                                       
057900               MOVE 0 TO W-KVPLATS-WDJ8                                   
058000             END-IF                                                       
058100             PERFORM IMS-GN-WDD801                                        
058200           END-PERFORM                                                    
058300         END-IF                                                           
058400       END-IF                                                             
058500     ELSE                                                                 
058600       IF REQU-ADLAGOMR-KEY NUMERIC AND                                   
058700          (REQU-ADGANG-KEY = ALL '+') OR                                  
058800          (REQU-BAY-KEY = ALL '+')                                        
058900          PERFORM IMS-GET-WDJ801-MIN-MAX                                  
059000*         CALL ABEND                                                      
059100          PERFORM UNTIL SEGMENT-SAKNAS                                    
059200            MOVE JA TO DATA-SW                                            
059300            COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 + LOC-KVPLATS         
059400            PERFORM IMS-GN-WDJ801-MIN-MAX                                 
059500          END-PERFORM                                                     
059600* BUFFERT  PLATSER                                                        
059700          MOVE REQU-IDDC-KEY       TO W-IDDC-D8-MIN-X                     
059800                                      W-IDDC-D8-MAX-X                     
059900          MOVE REQU-ADLAGOMR-KEY   TO W-ADBUFFOM-D8-MIN-X                 
060000                                      W-ADBUFFOM-D8-MAX-X                 
060100          MOVE W-ADGANG            TO W-ADBUFGAN-D8-MIN-X                 
060200                                                                          
060300          MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                      
060400          MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MIN-X                  
060500                                                                          
060600          PERFORM IMS-GU-WDD801                                           
060700                                                                          
060800          IF SEGMENT-FINNS                                                
060900          MOVE 1 TO INDX                                                  
061000            PERFORM UNTIL SEGMENT-SAKNAS                                  
061100              COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                 
061200              IF W-KVPLATS-WDJ8 <= 0                                      
061300                MOVE 0 TO W-KVPLATS-WDJ8                                  
061400              END-IF                                                      
061500              PERFORM IMS-GN-WDD801                                       
061600            END-PERFORM                                                   
061700          END-IF                                                          
061800       END-IF                                                             
061900     END-IF                                                               
062000     IF DATA-FINNS                                                        
062100*      KONTROLLERA WDK7                                                   
062200       MOVE REQU-IDDC-KEY       TO W-IDDC-K7-MIN                          
062300                                   W-IDDC-K7-MAX                          
062400       IF REQU-ADLAGOMR-KEY NOT = ALL '+'                                 
062500         MOVE REQU-ADLAGOMR-KEY   TO W-ADLAGOMR-K7-MIN                    
062600                                     W-ADLAGOMR-K7-MAX                    
062700       END-IF                                                             
062800       IF REQU-ADGANG-KEY  NOT = ALL '+'                                  
062900         MOVE REQU-ADGANG-KEY     TO W-ADGANG-K7-MIN                      
063000                                     W-ADGANG-K7-MAX                      
063100       END-IF                                                             
063200       IF REQU-BAY-KEY NOT = ALL '+'                                      
063300         MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                       
063400         MOVE WS-ADPLATS-NUM      TO W-ADPLATS-K7-MIN                     
063500                                                                          
063600         MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                       
063700         MOVE WS-ADPLATS-NUM      TO W-ADPLATS-K7-MAX                     
063800       END-IF                                                             
063900       PERFORM IMS-GU-WDK7A1                                              
064000                                                                          
064100       IF SEGMENT-FINNS                                                   
064200         MOVE 1 TO INDX                                                   
064300         MOVE SEQA-ADPLATS         TO WS-SEQA-ADPLATS                     
064400         MOVE SEQA-ADGANG          TO WS-SEQA-ADGANG                      
064500         MOVE SEQA-ADLAGOMR        TO WS-SEQA-ADLAGOMR                    
064600                                                                          
064700                                                                          
064800         PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                  
064900*         IF WS-SEQA-ADPLATS NOT = SEQA-ADPLATS                           
065000*                                                                         
065100*          MOVE INDX                    TO RESP-KVRADER                   
065200*          MOVE WS-SEQA-ADLAGOMR        TO RESP-ADLAGOMR-UT (INDX)        
065300*          MOVE WS-SEQA-ADGANG          TO RESP-ADGANG-UT (INDX)          
065400*          MOVE WS-SEQA-ADPLATS         TO RESP-ADPLATS-UT (INDX)         
065500*                                                                         
065600*          MOVE W-KVPLATS-WDJ8          TO RESP-KVPLATS-UT (INDX)         
065700*          ADD 1 TO INDX                                                  
065800*                                                                         
065900*          MOVE SEQA-ADPLATS            TO WS-SEQA-ADPLATS                
066000*          MOVE SEQA-ADGANG             TO WS-SEQA-ADGANG                 
066100*          MOVE SEQA-ADLAGOMR           TO WS-SEQA-ADLAGOMR               
066200*         END-IF                                                          
066300                                                                          
066400          COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                     
066500          IF W-KVPLATS-WDJ8 <= 0                                          
066600            MOVE 0 TO W-KVPLATS-WDJ8                                      
066700          END-IF                                                          
066800          PERFORM IMS-GN-WDK7A1                                           
066900         END-PERFORM                                                      
067000         IF REQU-ADLAGOMR-KEY NOT = ALL '+'                               
067100           MOVE REQU-ADLAGOMR-KEY       TO RESP-ADLAGOMR-UT (1)           
067200         END-IF                                                           
067300         IF REQU-ADGANG-KEY   NOT = ALL '+'                               
067400           MOVE REQU-ADGANG-KEY         TO RESP-ADGANG-UT   (1)           
067500         END-IF                                                           
067600         IF REQU-BAY-KEY      NOT = ALL '+'                               
067700           MOVE REQU-BAY-KEY            TO RESP-ADPLATS-UT  (1)           
067800         END-IF                                                           
067900                                                                          
068000         MOVE W-KVPLATS-WDJ8          TO RESP-KVPLATS-UT  (1)             
068100         MOVE 1                       TO RESP-KVRADER                     
068200*        MOVE INDX                    TO RESP-KVRADER                     
068300*        MOVE WS-SEQA-ADLAGOMR        TO RESP-ADLAGOMR-UT (INDX)          
068400*        MOVE WS-SEQA-ADGANG          TO RESP-ADGANG-UT (INDX)            
068500*        MOVE WS-SEQA-ADPLATS         TO RESP-ADPLATS-UT (INDX)           
068600*        MOVE W-KVPLATS-WDJ8          TO RESP-KVPLATS-UT (INDX)           
068700                                                                          
068800       ELSE                                                               
068900         IF REQU-ADLAGOMR-KEY NOT = ALL '+'                               
069000           MOVE REQU-ADLAGOMR-KEY       TO RESP-ADLAGOMR-UT (1)           
069100         END-IF                                                           
069200         IF REQU-ADGANG-KEY   NOT = ALL '+'                               
069300           MOVE REQU-ADGANG-KEY         TO RESP-ADGANG-UT   (1)           
069400         END-IF                                                           
069500         IF REQU-BAY-KEY      NOT = ALL '+'                               
069600           MOVE REQU-BAY-KEY            TO RESP-ADPLATS-UT  (1)           
069700         END-IF                                                           
069800         MOVE W-KVPLATS-WDJ8          TO RESP-KVPLATS-UT  (1)             
069900         MOVE 1                       TO RESP-KVRADER                     
070000         MOVE INDX TO RESP-KVRADER                                        
070100         MOVE '027' TO RESP-IDMSG-ERROR                                   
070200*        MOVE 'WDK7' TO RESP-IDELMT-ERROR                                 
070300       END-IF                                                             
070400     ELSE                                                                 
070500       MOVE '027' TO RESP-IDMSG-ERROR                                     
070600*      MOVE 'WDJ8' TO RESP-IDELMT-ERROR                                   
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 FB-VANLIG-SOK SECTION.                                                   
071100     MOVE '1' TO RESP-WEB-LAYOUT                                          
071200     MOVE NEJ TO DATA-SW                                                  
071300     MOVE 0 TO INDX                                                       
071400                                                                          
071500     PERFORM IMS-GET-WDJ801-MIN-MAX                                       
071600* BUFFERT  PLATSER                                                        
071700     IF SEGMENT-FINNS                                                     
071800      PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                     
071900       ADD +1 TO INDX                                                     
072000       MOVE LOC-KVPLATS         TO W-KVPLATS-WDJ8                         
072100                                                                          
072200       MOVE W-IDDC              TO W-IDDC-D8-MIN-X                        
072300                                   W-IDDC-D8-MAX-X                        
072400*      IF REQU-ADLAGOMR-KEY NOT = ALL '+'                                 
072500         MOVE LOC-ADLAGOMR      TO W-ADBUFFOM-D8-MIN-X                    
072600                                   W-ADBUFFOM-D8-MAX-X                    
072700*      END-IF                                                             
072800*      IF REQU-ADGANG-KEY NOT = ALL '+'                                   
072900         MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MIN-X                  
073000                                     W-ADBUFGAN-D8-MAX-X                  
073100*      ELSE                                                               
073200*        MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MIN-X                  
073300*      END-IF                                                             
073400                                                                          
073500       MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MIN                             
073600       MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                         
073700       MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MIN-X                     
073800                                                                          
073900       MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MAX                             
074000       MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                         
074100       MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MAX-X                     
074200                                                                          
074300       PERFORM IMS-GU-WDD801                                              
074400                                                                          
074500       IF SEGMENT-FINNS                                                   
074600         PERFORM UNTIL SEGMENT-SAKNAS                                     
074700           COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                    
074800           IF W-KVPLATS-WDJ8 <= 0                                         
074900             MOVE 0 TO W-KVPLATS-WDJ8                                     
075000           END-IF                                                         
075100           PERFORM IMS-GN-WDD801                                          
075200         END-PERFORM                                                      
075300       END-IF                                                             
075400** WDK7 LÄSNINGAR                                                         
075500       MOVE W-IDDC              TO W-IDDC-K7-MIN                          
075600                                   W-IDDC-K7-MAX                          
075700*      IF REQU-ADLAGOMR-KEY NOT = ALL '+'                                 
075800         MOVE LOC-ADLAGOMR        TO W-ADLAGOMR-K7-MIN                    
075900                                     W-ADLAGOMR-K7-MAX                    
076000*      END-IF                                                             
076100*      IF REQU-ADGANG-KEY  NOT = ALL '+'                                  
076200         MOVE LOC-ADGANG          TO W-ADGANG-K7-MIN                      
076300                                     W-ADGANG-K7-MAX                      
076400*      END-IF                                                             
076500*      IF REQU-BAY-KEY NOT = ALL '+'                                      
076600         MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MIN                           
076700         MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                       
076800         MOVE WS-ADPLATS-NUM      TO W-ADPLATS-K7-MIN                     
076900                                                                          
077000         MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MAX                           
077100         MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                       
077200         MOVE WS-ADPLATS-NUM      TO W-ADPLATS-K7-MAX                     
077300*      END-IF                                                             
077400       PERFORM IMS-GU-WDK7A1                                              
077500                                                                          
077600       IF SEGMENT-FINNS                                                   
077700         PERFORM UNTIL SEGMENT-SAKNAS                                     
077800          COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                     
077900          IF W-KVPLATS-WDJ8 <= 0                                          
078000            MOVE 0 TO W-KVPLATS-WDJ8                                      
078100          END-IF                                                          
078200          PERFORM IMS-GN-WDK7A1                                           
078300         END-PERFORM                                                      
078400       END-IF                                                             
078500       IF W-KVPLATS-WDJ8 > 0                                              
078600         MOVE LOC-ADLAGOMR          TO RESP-ADLAGOMR-UT (INDX)            
078700         MOVE LOC-ADGANG            TO RESP-ADGANG-UT   (INDX)            
078800         MOVE LOC-ADPLATS(1:2)      TO RESP-ADPLATS-UT  (INDX)            
078900         MOVE W-KVPLATS-WDJ8        TO RESP-KVPLATS-UT  (INDX)            
079000         MOVE INDX                  TO RESP-KVRADER                       
079100       ELSE                                                               
079200         SUBTRACT  1 FROM INDX                                            
079300       END-IF                                                             
079400       PERFORM IMS-GN-WDJ801-MIN-MAX                                      
079500      END-PERFORM                                                         
079600     ELSE                                                                 
079700       MOVE '027'  TO RESP-IDMSG-ERROR                                    
079800*      MOVE 'WDJ8' TO RESP-IDELMT-ERROR                                   
079900                                                                          
080000     END-IF                                                               
080100                                                                          
080200     .                                                                    
080300     EJECT                                                                
080400 FC-SOK-NOEXIST SECTION.                                                  
080500     MOVE REQU-IDDC-KEY       TO W-IDDC-6336                              
080600     MOVE +0 TO INDX                                                      
080700     MOVE 2  TO RESP-WEB-LAYOUT                                           
080800     PERFORM IMS-GET-WDGX6336                                             
080900     IF SEGMENT-FINNS                                                     
081000       PERFORM IMS-GHNP-WDGX6338-MIN-MAX                                  
081100       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
081200*       IF 6338-ADLAGOMR = REQU-ADLAGOMR-KEY                              
081300         ADD +1 TO INDX                                                   
081400         MOVE INDX TO RESP-KVRADER                                        
081500         MOVE 6338-IDARTNR    TO RESP-IDARTNR-UT  (INDX)                  
081600         MOVE 6338-ADLAGOMR   TO RESP-ADLAGOMR-UT (INDX)                  
081700         MOVE 6338-ADGANG     TO RESP-ADGANG-UT   (INDX)                  
081800         MOVE 6338-ADPLATS    TO RESP-ADPLATS-UT  (INDX)                  
081900*       END-IF                                                            
082000        PERFORM IMS-GHNP-WDGX6338-MIN-MAX                                 
082100       END-PERFORM                                                        
082200       IF INDX = +0                                                       
082300         MOVE '027' TO RESP-IDMSG-ERROR                                   
082400       END-IF                                                             
082500     ELSE                                                                 
082600       MOVE '027' TO RESP-IDMSG-ERROR                                     
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 FD-SOK-DEVIATION SECTION.                                                
083100     MOVE REQU-IDDC-KEY       TO W-IDDC-6342                              
083200     MOVE +0 TO INDX                                                      
083300     MOVE 3  TO RESP-WEB-LAYOUT                                           
083400     PERFORM IMS-GET-WDGX6342                                             
083500     IF SEGMENT-FINNS                                                     
083600       PERFORM IMS-GHNP-WDGX6344                                          
083700       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
083800         ADD +1 TO INDX                                                   
083900         MOVE INDX            TO RESP-KVRADER                             
084000         MOVE 6344-ADLAGOMR   TO RESP-ADLAGOMR-UT  (INDX)                 
084100         MOVE 6344-ADGANG     TO RESP-ADGANG-UT    (INDX)                 
084200         MOVE 6344-ADPLDEL    TO RESP-ADPLATS-UT   (INDX)                 
084300         MOVE 6344-KVPLATS    TO RESP-KVPLATS-UT   (INDX)                 
084400         MOVE 6344-KVANTART   TO RESP-KVANTART-UT  (INDX)                 
084500                                                                          
084600       PERFORM IMS-GHNP-WDGX6344                                          
084700       END-PERFORM                                                        
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 FE-SOK-COVER SECTION.                                                    
085200     MOVE REQU-IDDC-KEY       TO W-IDDC-6346                              
085300     MOVE +0 TO INDX                                                      
085400     MOVE 4  TO RESP-WEB-LAYOUT                                           
085500     PERFORM IMS-GET-WDGX6346                                             
085600     MOVE 6346-REDCBEL TO WS-REDCBEL                                      
085700     IF SEGMENT-FINNS                                                     
085800       PERFORM IMS-GHNP-WDGX6348                                          
085900       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
086000         ADD +1 TO INDX                                                   
086100         MOVE INDX            TO RESP-KVRADER                             
086200         MOVE 6348-ADLAGOMR   TO RESP-ADLAGOMR-UT  (INDX)                 
086300         MOVE 6348-RELOBEL    TO RESP-RELOBEL-UT   (INDX)                 
086400                                                                          
086500       PERFORM IMS-GHNP-WDGX6348                                          
086600       END-PERFORM                                                        
086700       ADD +1 TO INDX                                                     
086800       MOVE INDX              TO RESP-KVRADER                             
086900       MOVE WS-REDCBEL        TO RESP-REDCBEL-UT (INDX)                   
087000                                                                          
087100     ELSE                                                                 
087200       MOVE '027' TO RESP-IDMSG-ERROR                                     
087300     END-IF                                                               
087400     .                                                                    
087500     EJECT                                                                
087600 G-PRINT-LISTA SECTION.                                                   
087700     IF REQU-FL-NOEXIST   = 'N' AND                                       
087800        REQU-FL-DEVIATION = 'N' AND                                       
087900        REQU-FL-COVER     = 'N'                                           
088000       IF REQU-KVRADER > 0                                                
088100         PERFORM GA-PRINT-FREE-LOCA                                       
088200       ELSE                                                               
088300         MOVE '027' TO RESP-IDMSG-ERROR                                   
088400       END-IF                                                             
088500     ELSE                                                                 
088600       IF REQU-FL-NOEXIST   = 'Y'                                         
088700         IF REQU-KVRADER > 0                                              
088800           PERFORM GB-PRINT-NOEXIST                                       
088900         ELSE                                                             
089000           MOVE '027' TO RESP-IDMSG-ERROR                                 
089100         END-IF                                                           
089200       ELSE                                                               
089300         IF REQU-FL-DEVIATION = 'Y'                                       
089400           IF REQU-KVRADER > 0                                            
089500             PERFORM GC-PRINT-DEVIATION                                   
089600           ELSE                                                           
089700             MOVE '027' TO RESP-IDMSG-ERROR                               
089800           END-IF                                                         
089900         ELSE                                                             
090000           IF REQU-FL-COVER = 'Y'                                         
090100             IF REQU-KVRADER > 0                                          
090200               PERFORM GD-PRINT-COVER                                     
090300             ELSE                                                         
090400               MOVE '027' TO RESP-IDMSG-ERROR                             
090500             END-IF                                                       
090600           END-IF                                                         
090700         END-IF                                                           
090800       END-IF                                                             
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200 GA-PRINT-FREE-LOCA SECTION.                                              
091300     PERFORM S04-SKICKA-OPEN                                              
091400     MOVE 'FREE-LOCA'     TO HDR-IDOUTTYPE                                
091500     PERFORM S04-SKAPA-HEADER                                             
091600     PERFORM S04-PUT-DAP-HEADER                                           
091700                                                                          
091800     MOVE '1' TO RESP-WEB-LAYOUT                                          
091900     PERFORM IMS-GET-WDJ801-MIN-MAX                                       
092000     MOVE 0 TO INDX                                                       
092100* BUFFERT  PLATSER                                                        
092200     IF SEGMENT-FINNS                                                     
092300      PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                     
092400       ADD +1 TO INDX                                                     
092500       MOVE LOC-KVPLATS         TO W-KVPLATS-WDJ8                         
092600                                                                          
092700       MOVE W-IDDC              TO W-IDDC-D8-MIN-X                        
092800                                   W-IDDC-D8-MAX-X                        
092900       MOVE LOC-ADLAGOMR        TO W-ADBUFFOM-D8-MIN-X                    
093000                                   W-ADBUFFOM-D8-MAX-X                    
093100                                                                          
093200       MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MIN-X                    
093300                                   W-ADBUFGAN-D8-MAX-X                    
093400                                                                          
093500       MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MIN                             
093600       MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                         
093700       MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MIN-X                     
093800                                                                          
093900       MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MAX                             
094000       MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                         
094100       MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MAX-X                     
094200                                                                          
094300       PERFORM IMS-GU-WDD801                                              
094400                                                                          
094500       IF SEGMENT-FINNS                                                   
094600         PERFORM UNTIL SEGMENT-SAKNAS                                     
094700           COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                    
094800           IF W-KVPLATS-WDJ8 <= 0                                         
094900             MOVE 0 TO W-KVPLATS-WDJ8                                     
095000           END-IF                                                         
095100           PERFORM IMS-GN-WDD801                                          
095200         END-PERFORM                                                      
095300       END-IF                                                             
095400** WDK7 LÄSNINGAR                                                         
095500       MOVE W-IDDC              TO W-IDDC-K7-MIN                          
095600                                   W-IDDC-K7-MAX                          
095700       MOVE LOC-ADLAGOMR        TO W-ADLAGOMR-K7-MIN                      
095800                                   W-ADLAGOMR-K7-MAX                      
095900       MOVE LOC-ADGANG          TO W-ADGANG-K7-MIN                        
096000                                   W-ADGANG-K7-MAX                        
096100       MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MIN                             
096200       MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                         
096300       MOVE WS-ADPLATS-NUM      TO W-ADPLATS-K7-MIN                       
096400                                                                          
096500       MOVE LOC-ADPLATS(1:2)    TO WS-BAY-MAX                             
096600       MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                         
096700       MOVE WS-ADPLATS-NUM      TO W-ADPLATS-K7-MAX                       
096800       PERFORM IMS-GU-WDK7A1                                              
096900                                                                          
097000       IF SEGMENT-FINNS                                                   
097100         PERFORM UNTIL SEGMENT-SAKNAS                                     
097200          COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 - 1                     
097300          IF W-KVPLATS-WDJ8 <= 0                                          
097400            MOVE 0 TO W-KVPLATS-WDJ8                                      
097500          END-IF                                                          
097600          PERFORM IMS-GN-WDK7A1                                           
097700         END-PERFORM                                                      
097800       END-IF                                                             
097900       IF W-KVPLATS-WDJ8 > 0                                              
098000         MOVE 'LINE'                TO DOC-IDAFPRCD                       
098100         MOVE REQU-IDDC-KEY         TO DOC-IDDC                           
098200         MOVE LOC-ADLAGOMR          TO DOC-ADLAGOMR                       
098300         MOVE LOC-ADGANG            TO DOC-ADGANG                         
098400         MOVE LOC-ADPLATS(1:2)      TO DOC-ADPLATS                        
098500         MOVE W-KVPLATS-WDJ8        TO DOC-KVPLATS                        
098600         PERFORM S04-PUT-DOC                                              
098700                                                                          
098800         MOVE LOC-ADLAGOMR          TO RESP-ADLAGOMR-UT (INDX)            
098900         MOVE LOC-ADGANG            TO RESP-ADGANG-UT   (INDX)            
099000         MOVE LOC-ADPLATS(1:2)      TO RESP-ADPLATS-UT  (INDX)            
099100         MOVE W-KVPLATS-WDJ8        TO RESP-KVPLATS-UT  (INDX)            
099200         MOVE INDX                  TO RESP-KVRADER                       
099300       ELSE                                                               
099400         SUBTRACT  1 FROM INDX                                            
099500       END-IF                                                             
099600       PERFORM IMS-GN-WDJ801-MIN-MAX                                      
099700      END-PERFORM                                                         
099800      MOVE '015' TO RESP-IDMSG-INFO                                       
099900      PERFORM S04-SKICKA-CLOSE                                            
100000     END-IF                                                               
100100     .                                                                    
100200     EJECT                                                                
100300                                                                          
100400 GB-PRINT-NOEXIST SECTION.                                                
100500     PERFORM S04-SKICKA-OPEN                                              
100600     MOVE 'NOEXIST'     TO HDR-IDOUTTYPE                                  
100700     MOVE REQU-IDDC-KEY TO DOC-IDDC                                       
100800                                                                          
100900     PERFORM S04-SKAPA-HEADER                                             
101000     PERFORM S04-PUT-DAP-HEADER                                           
101100                                                                          
101200     MOVE REQU-IDDC-KEY       TO W-IDDC-6336                              
101300     MOVE +0 TO INDX                                                      
101400     MOVE 2  TO RESP-WEB-LAYOUT                                           
101500     PERFORM IMS-GET-WDGX6336                                             
101600     IF SEGMENT-FINNS                                                     
101700       PERFORM IMS-GHNP-WDGX6338-MIN-MAX                                  
101800       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
101900*       IF 6338-ADLAGOMR = REQU-ADLAGOMR-KEY                              
102000         ADD +1 TO INDX                                                   
102100         MOVE INDX TO RESP-KVRADER                                        
102200         MOVE 'LINE'          TO DOC-IDAFPRCD                             
102300         MOVE 6338-IDARTNR    TO RESP-IDARTNR-UT  (INDX)                  
102400                                 DOC-IDARTNR                              
102500         MOVE 6338-ADLAGOMR   TO RESP-ADLAGOMR-UT (INDX)                  
102600                                 DOC-ADLAGOMR                             
102700         MOVE 6338-ADGANG     TO RESP-ADGANG-UT   (INDX)                  
102800                                 DOC-ADGANG                               
102900         MOVE 6338-ADPLATS    TO RESP-ADPLATS-UT  (INDX)                  
103000                                 DOC-ADPLATS                              
103100         PERFORM S04-PUT-DOC                                              
103200*       END-IF                                                            
103300        PERFORM IMS-GHNP-WDGX6338-MIN-MAX                                 
103400       END-PERFORM                                                        
103500       MOVE '015' TO RESP-IDMSG-INFO                                      
103600       PERFORM S04-SKICKA-CLOSE                                           
103700     ELSE                                                                 
103800       MOVE '027' TO RESP-IDMSG-ERROR                                     
103900     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200 GC-PRINT-DEVIATION SECTION.                                              
104300     PERFORM S04-SKICKA-OPEN                                              
104400     MOVE 'DEVIATION'     TO HDR-IDOUTTYPE                                
104500     MOVE REQU-IDDC-KEY TO DOC-IDDC                                       
104600                                                                          
104700     PERFORM S04-SKAPA-HEADER                                             
104800     PERFORM S04-PUT-DAP-HEADER                                           
104900     MOVE REQU-IDDC-KEY       TO W-IDDC-6342                              
105000     MOVE +0 TO INDX                                                      
105100     MOVE 3  TO RESP-WEB-LAYOUT                                           
105200     PERFORM IMS-GET-WDGX6342                                             
105300     IF SEGMENT-FINNS                                                     
105400       PERFORM IMS-GHNP-WDGX6344                                          
105500       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
105600         ADD +1 TO INDX                                                   
105700         MOVE 'LINE'          TO DOC-IDAFPRCD                             
105800         MOVE INDX            TO RESP-KVRADER                             
105900         MOVE 6344-ADLAGOMR   TO RESP-ADLAGOMR-UT  (INDX)                 
106000                                 DOC-ADLAGOMR                             
106100         MOVE 6344-ADGANG     TO RESP-ADGANG-UT    (INDX)                 
106200                                 DOC-ADGANG                               
106300         MOVE 6344-ADPLDEL    TO RESP-ADPLATS-UT   (INDX)                 
106400                                 DOC-ADPLATS                              
106500         MOVE 6344-KVPLATS    TO RESP-KVPLATS-UT   (INDX)                 
106600                                 DOC-KVPLATS                              
106700         MOVE 6344-KVANTART   TO RESP-KVANTART-UT  (INDX)                 
106800                                 DOC-KVANTART                             
106900                                                                          
107000         PERFORM S04-PUT-DOC                                              
107100       PERFORM IMS-GHNP-WDGX6344                                          
107200       END-PERFORM                                                        
107300       MOVE '015' TO RESP-IDMSG-INFO                                      
107400       PERFORM S04-SKICKA-CLOSE                                           
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 GD-PRINT-COVER SECTION.                                                  
107900     PERFORM S04-SKICKA-OPEN                                              
108000     MOVE 'FILL-RATE'     TO HDR-IDOUTTYPE                                
108100     MOVE REQU-IDDC-KEY TO DOC-IDDC                                       
108200                                                                          
108300     PERFORM S04-SKAPA-HEADER                                             
108400     PERFORM S04-PUT-DAP-HEADER                                           
108500     MOVE REQU-IDDC-KEY       TO W-IDDC-6346                              
108600     MOVE +0 TO INDX                                                      
108700     MOVE 4  TO RESP-WEB-LAYOUT                                           
108800     PERFORM IMS-GET-WDGX6346                                             
108900     IF SEGMENT-FINNS                                                     
109000       MOVE 6346-REDCBEL TO WS-REDCBEL                                    
109100       PERFORM IMS-GHNP-WDGX6348                                          
109200       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
109300         ADD +1 TO INDX                                                   
109400         MOVE INDX            TO RESP-KVRADER                             
109500         MOVE 'LINE'          TO DOC-IDAFPRCD                             
109600         MOVE 6348-ADLAGOMR   TO RESP-ADLAGOMR-UT  (INDX)                 
109700                                 DOC-ADLAGOMR                             
109800         MOVE 6348-RELOBEL    TO RESP-RELOBEL-UT   (INDX)                 
109900                                 DOC-RELOBEL                              
110000         PERFORM S04-PUT-DOC                                              
110100                                                                          
110200       PERFORM IMS-GHNP-WDGX6348                                          
110300       END-PERFORM                                                        
110400       ADD +1 TO INDX                                                     
110500       MOVE INDX              TO RESP-KVRADER                             
110600       MOVE 'TOT'             TO DOC-IDAFPRCD                             
110700       MOVE WS-REDCBEL        TO RESP-REDCBEL-UT (INDX)                   
110800                                 DOC-REDCBEL                              
110900       PERFORM S04-PUT-DOC                                                
111000                                                                          
111100       MOVE '015' TO RESP-IDMSG-INFO                                      
111200       PERFORM S04-SKICKA-CLOSE                                           
111300     ELSE                                                                 
111400       MOVE '027' TO RESP-IDMSG-ERROR                                     
111500     END-IF                                                               
111600     .                                                                    
111700     EJECT                                                                
111800*    --- DISPATCHER-SEKTIONER                                             
111900 S01-HAEMTA-ANROPSDATA SECTION.                                           
112000                                                                          
112100     MOVE 'GETARG'               TO SUB-KDFUNC                            
112200     MOVE 'CARPARTS.LDC.FREELOCATIONQUERY'   TO SUB-ADDISPABS             
112300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
112400                                                                          
112500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
112600                                                                          
112700     IF SUB-KDRC > 0                                                      
112800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
112900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
113000       DELIMITED BY SIZE INTO FELTEXT                                     
113100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
113200     END-IF                                                               
113300     .                                                                    
113400     SKIP3                                                                
113500 S02-RETURNERA-SVAR SECTION.                                              
113600                                                                          
113700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
113800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
113900                                                                          
114000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
114100                                                                          
114200     IF SUB-KDRC > 0                                                      
114300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
114400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
114500       DELIMITED BY SIZE INTO FELTEXT                                     
114600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000 S04-SKICKA-OPEN SECTION.                                                 
115100                                                                          
115200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
115300     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
115400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
115500                                                                          
115600     IF SEND-KDRC > 0                                                     
115700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
115800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
115900       DELIMITED BY SIZE INTO FELTEXT                                     
116000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
116100     END-IF                                                               
116200     .                                                                    
116300     SKIP3                                                                
116400                                                                          
116500 S04-SKAPA-HEADER   SECTION.                                              
116600     MOVE 001             TO HDR-REQU-IDMSGVER                            
116700     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
116800     MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                              
116900                                                                          
117000*    MOVE 'FREE-LOCA'     TO HDR-IDOUTTYPE                                
117100     MOVE SPACE               TO HDR-IDOUTREC                             
117200     MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                        
117300     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
117400     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
117500     .                                                                    
117600 S04-PUT-DAP-HEADER SECTION.                                              
117700     MOVE 'PUT'                           TO SEND-KDFUNC                  
117800     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
117900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
118000                         SEND-KVDLEN                                      
118100                         HDR-AREA                                         
118200     IF SEND-KDRC > ZERO                                                  
118300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
118400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
118500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
118600       CALL ABEND USING  RKOD-ABEND-MED-DUMP                              
118700     END-IF                                                               
118800     .                                                                    
118900 S04-PUT-DOC      SECTION.                                                
119000     MOVE 'PUT'                           TO SEND-KDFUNC                  
119100     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
119200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
119300                         SEND-KVDLEN                                      
119400                         DOC-AREA                                         
119500     IF SEND-KDRC > ZERO                                                  
119600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
119700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
119800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
119900       CALL ABEND USING  RKOD-ABEND-MED-DUMP                              
120000     END-IF                                                               
120100     .                                                                    
120200 S04-SKICKA-CLOSE SECTION.                                                
120300                                                                          
120400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
120500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
120600                                                                          
120700     IF SEND-KDRC > 0                                                     
120800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
120900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
121000       DELIMITED BY SIZE INTO FELTEXT                                     
121100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
121200     END-IF                                                               
121300     .                                                                    
121400     EJECT                                                                
121500 IMS-GET-WDJ801 SECTION.                                                  
121600                                                                          
121700     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
121800          DELIMITED BY SIZE INTO SSA1                                     
121900     MOVE '  GE' TO GODK-STATUSKODER                                      
122000     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
122100     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
122200     PERFORM IMS-STATUSKONTROLL                                           
122300     .                                                                    
122400     SKIP3                                                                
122500 IMS-GN-WDJ801 SECTION.                                                   
122600                                                                          
122700     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
122800          DELIMITED BY SIZE INTO SSA1                                     
122900     MOVE '  GE' TO GODK-STATUSKODER                                      
123000     CALL CBLTDLI USING GN  WDJ8-PCB DLI-IO-WDJ801 SSA1                   
123100     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
123200     PERFORM IMS-STATUSKONTROLL                                           
123300     .                                                                    
123400     SKIP3                                                                
123500 IMS-GET-WDJ801-MIN-MAX SECTION.                                          
123600                                                                          
123700     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
123800                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
123900          DELIMITED BY SIZE INTO SSA1                                     
124000     MOVE '  GE' TO GODK-STATUSKODER                                      
124100     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
124200     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
124300     PERFORM IMS-STATUSKONTROLL                                           
124400     .                                                                    
124500     SKIP3                                                                
124600 IMS-GN-WDJ801-MIN-MAX SECTION.                                           
124700                                                                          
124800     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
124900                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
125000          DELIMITED BY SIZE INTO SSA1                                     
125100     MOVE '  GE' TO GODK-STATUSKODER                                      
125200     CALL CBLTDLI USING GN WDJ8-PCB DLI-IO-WDJ801 SSA1                    
125300     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
125400     PERFORM IMS-STATUSKONTROLL                                           
125500     .                                                                    
125600     SKIP3                                                                
125700 IMS-GU-WDD801 SECTION.                                                   
125800     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
125900                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
126000                                                                          
126100            DELIMITED BY SIZE INTO SSA1                                   
126200     MOVE '  GE' TO GODK-STATUSKODER                                      
126300     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
126400     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
126500     PERFORM IMS-STATUSKONTROLL                                           
126600     .                                                                    
126700     EJECT                                                                
126800 IMS-GN-WDD801 SECTION.                                                   
126900     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
127000                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
127100                                                                          
127200            DELIMITED BY SIZE INTO SSA1                                   
127300     MOVE '  GE' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING GN WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
127500     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     EJECT                                                                
127900 IMS-GU-WDK7A1 SECTION.                                                   
128000     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
128100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     MOVE '  GE' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
128500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     SKIP3                                                                
128900 IMS-GN-WDK7A1 SECTION.                                                   
129000     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
129100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
129200          DELIMITED BY SIZE INTO SSA1                                     
129300     MOVE '  GE' TO GODK-STATUSKODER                                      
129400     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
129500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
129600     PERFORM IMS-STATUSKONTROLL                                           
129700     .                                                                    
129800     SKIP3                                                                
129900 IMS-GET-WDGX6336 SECTION.                                                
130000                                                                          
130100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6335-X ')'                    
130200          DELIMITED BY SIZE INTO SSA1                                     
130300     STRING 'WDGX6336(IDDC     =' W-WDGXKEY-6336-X ')'                    
130400          DELIMITED BY SIZE INTO SSA2                                     
130500     MOVE '  GE' TO GODK-STATUSKODER                                      
130600     CALL CBLTDLI USING GHU WDR21-PCB DLI-IO-WDGX6336 SSA1 SSA2           
130700     MOVE WDR21-STATUS-CODE TO STATUS-WS                                  
130800     PERFORM IMS-STATUSKONTROLL                                           
130900     .                                                                    
131000     EJECT                                                                
131100 IMS-GHNP-WDGX6338 SECTION.                                               
131200     STRING 'WDGX6336(IDDC     =' W-WDGXKEY-6336-X ')'                    
131300          DELIMITED BY SIZE INTO SSA1                                     
131400                                                                          
131500     MOVE 'WDGX6338 ' TO SSA2                                             
131600     MOVE '  GE' TO GODK-STATUSKODER                                      
131700     CALL CBLTDLI USING GHNP  WDR21-PCB DLI-IO-WDGX6338 SSA1 SSA2         
131800     MOVE WDR21-STATUS-CODE TO STATUS-WS                                  
131900     PERFORM IMS-STATUSKONTROLL                                           
132000     .                                                                    
132100     EJECT                                                                
132200 IMS-GHNP-WDGX6338-MIN-MAX SECTION.                                       
132300     STRING 'WDGX6338(KY6338  >=' W-WDGXKEY-6338-MIN-X                    
132400                    '&KY6338  <=' W-WDGXKEY-6338-MAX-X ')'                
132500          DELIMITED BY SIZE INTO SSA1                                     
132600                                                                          
132700*    MOVE 'WDGX6338 ' TO SSA2                                             
132800     MOVE '  GE' TO GODK-STATUSKODER                                      
132900     CALL CBLTDLI USING GHNP  WDR21-PCB DLI-IO-WDGX6338 SSA1              
133000     MOVE WDR21-STATUS-CODE TO STATUS-WS                                  
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     EJECT                                                                
133400 IMS-GET-WDGX6342 SECTION.                                                
133500                                                                          
133600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6341-X ')'                    
133700          DELIMITED BY SIZE INTO SSA1                                     
133800     STRING 'WDGX6342(IDDC     =' W-WDGXKEY-6342-X ')'                    
133900          DELIMITED BY SIZE INTO SSA2                                     
134000     MOVE '  GE' TO GODK-STATUSKODER                                      
134100     CALL CBLTDLI USING GHU WDR22-PCB DLI-IO-WDGX6342 SSA1 SSA2           
134200     MOVE WDR22-STATUS-CODE TO STATUS-WS                                  
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     EJECT                                                                
134600 IMS-GHNP-WDGX6344 SECTION.                                               
134700     STRING 'WDGX6342(IDDC     =' W-WDGXKEY-6342-X ')'                    
134800          DELIMITED BY SIZE INTO SSA1                                     
134900                                                                          
135000     MOVE 'WDGX6344 ' TO SSA2                                             
135100     MOVE '  GE' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GHNP  WDR22-PCB DLI-IO-WDGX6344 SSA1 SSA2         
135300     MOVE WDR22-STATUS-CODE TO STATUS-WS                                  
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     EJECT                                                                
135700 IMS-GET-WDGX6346 SECTION.                                                
135800                                                                          
135900     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6345-X ')'                    
136000          DELIMITED BY SIZE INTO SSA1                                     
136100     STRING 'WDGX6346(IDDC     =' W-WDGXKEY-6346-X ')'                    
136200          DELIMITED BY SIZE INTO SSA2                                     
136300     MOVE '  GE' TO GODK-STATUSKODER                                      
136400     CALL CBLTDLI USING GHU WDR23-PCB DLI-IO-WDGX6346 SSA1 SSA2           
136500     MOVE WDR23-STATUS-CODE TO STATUS-WS                                  
136600     PERFORM IMS-STATUSKONTROLL                                           
136700     .                                                                    
136800     EJECT                                                                
136900 IMS-GHNP-WDGX6348 SECTION.                                               
137000     STRING 'WDGX6346(IDDC     =' W-WDGXKEY-6346-X ')'                    
137100          DELIMITED BY SIZE INTO SSA1                                     
137200                                                                          
137300     MOVE 'WDGX6348 ' TO SSA2                                             
137400     MOVE '  GE' TO GODK-STATUSKODER                                      
137500     CALL CBLTDLI USING GHNP  WDR23-PCB DLI-IO-WDGX6348 SSA1 SSA2         
137600     MOVE WDR23-STATUS-CODE TO STATUS-WS                                  
137700     PERFORM IMS-STATUSKONTROLL                                           
137800     .                                                                    
137900     EJECT                                                                
138000 IMS-STATUSKONTROLL SECTION.                                              
138100                                                                          
138200     SET STATUS-IX TO 1                                                   
138300     SEARCH GODK-STATUS                                                   
138400       AT END                                                             
138500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
138600         DELIMITED BY SIZE INTO ERROR-TEXT                                
138700         CALL FELLOG                                                      
138800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
138900         CONTINUE                                                         
139000     END-SEARCH                                                           
139100     .                                                                    
