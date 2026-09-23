000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2222900.                                            
000300 AUTHOR.             B SWAHNBERG, DATA LOGIC AB.                          
000400 DATE-WRITTEN.       OKTOBER 1978.                                        
000500     REMARKS.                                                             
000600*                                                                         
000700*    FUNKTION.                                                            
000800*        PROGRAMMET LÄSER ARTIKELDATABASEN (WDK6 AA) I FYS SEKV           
000900*        OCH SKRIVER FIL FÖR UPPDATERING A KVMAD, KVUTJFEL OCH            
001000*        KVPB-PLAN MED HJÄLP AV SENASTE PERIODENS ORDERINGÅNG.            
001100*        MAD-TOT BERÄKNAS ÄVEN MED HÄNSYN TILL REFILL                     
001200*    RETURKODER.                                                          
001300*        0           NORMALT PROGRAMSLUT                                  
001400*       24           TIAAP I POST FRÅN ORDERINGÅNGSREGISTRET              
001500*                    SKILT FRÅN AKTUELL PERIOD I DATUMKORTET.             
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000*                            *** EXTRACT FRÅN ORDERINGÅNGS-  ***          
002100*                            *** REGISTRET                   ***          
002200*                            *** INPUT                       ***          
002300     SELECT W22231 ASSIGN UT-S-W22229D1.                                  
002400                                                                          
002500     SELECT W22229 ASSIGN UT-W22229D2.                                    
002600                                                                          
002700     SELECT W22229L ASSIGN UT-W22229D3.                                   
002800                                                                          
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100                                                                          
003200 FD  W22231                                                               
003300     RECORDING F                                                          
003400     BLOCK 0                                                              
003500     LABEL RECORD STANDARD.                                               
003600*01  W222289-POST  -COPY W22231     -L.                                   
003700*                                                                         
003800                                                                          
003900 FD  W22229                                                               
004000     RECORDING F                                                          
004100     BLOCK 0                                                              
004200     LABEL RECORD STANDARD.                                               
004300*01  RECORD -COPY W22229 -PRE  UT-  -L.                                   
004400*                                                                         
004500     EJECT                                                                
004600 FD  W22229L                                                              
004700     RECORDING F                                                          
004800     BLOCK 0                                                              
004900     LABEL RECORD STANDARD.                                               
005000*01  RECORD -COPY W22229L -PRE  UL-  -L.                                  
005100*                                                                         
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -COPY WY2000W6                                                       
005600                                                                          
005700*    -COPY WY2000W3                                                       
005800                                                                          
005900                                                                          
006000 77  IDPGM                       PIC X(8)    VALUE 'W2222900'.            
006100 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
006200 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
006300                                                                          
006400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006500 01  ERROR-TEXT.                                                          
006600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006800                                                                          
006900 01  RKOD                        PIC S9(4)   VALUE +0  COMP SYNC.         
007000 01  VIKT                        PIC S9(1)V9(1) VALUE +1.0 COMP-3.        
007100 01  LARMA                       PIC X       VALUE 'N'.                   
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES        DAGENS-DATUM.                                
007400     03  DAGENS-AA               PIC 9(2).                                
007500     03  DAGENS-MM               PIC 9(2).                                
007600     03  DAGENS-DD               PIC 9(2).                                
007700                                                                          
007800 01  KONSTANTER.                                                          
007900     03  JA                      PIC X       VALUE 'J'.                   
008000     03  NEJ                     PIC X       VALUE 'N'.                   
008100                                                                          
008200*    INDEX FÖR RESEASON                                                   
008300 01  TREND-IX                    PIC S9(3)   VALUE +0  COMP-3.            
008400*                                                                         
008500 01  EOF-SWITCHAR.                                                        
008600     03  EOF-W22231              PIC X       VALUE 'N'.                   
008700*                                                                         
008800 77  SW-TEST-S03                 PIC X       VALUE 'N'.                   
008900 77  SW-I31BAS-READ              PIC X       VALUE 'N'.                   
009000 77  WS-MASK-PLAN-USED           PIC X(3)    VALUE SPACE.                 
009100 77  SW-MASK-CALCULATED          PIC X       VALUE 'N'.                   
009200 77  SW-MAN-CALCULATED           PIC X       VALUE 'N'.                   
009300 77  SW-LOG                      PIC X(1)    VALUE 'N'.                   
009400*                                                                         
009500*01  -COPY WWPRODSL                                                       
009600     EJECT                                                                
009700 01  DIV-ARB-AREOR.                                                       
009800     03  JUSTERAD-K-VECKA        PIC 9(4).                                
009900     03    FILLER    REDEFINES JUSTERAD-K-VECKA.                          
010000         05  JUST-K-AAR-X.                                                
010100             07  JUST-K-AAR-1    PIC 9.                                   
010200             07  JUST-K-AAR-2    PIC 9.                                   
010300         05  JUST-K-AAR  REDEFINES JUST-K-AAR-X  PIC 9(2).                
010400         05  JUST-K-VV           PIC S9(2).                               
010500*                                                                         
010600     03  KVPB-TOT                PIC S9(8)V9(1) COMP-3 VALUE ZERO.        
010700     03  WSMASK-KVPB-TOT         PIC S9(8)V9(1) COMP-3 VALUE ZERO.        
010800     03  PROGFEL-SEP             PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
010900     03  ABS-PROGFEL-SEP         PIC  9(6)V9(1) COMP-3 VALUE ZERO.        
011000     03  PROGFEL-TOT             PIC S9(6)V9(3) COMP-3 VALUE ZERO.        
011100     03  ABS-PROGFEL-TOT         PIC  9(6)V9(3) COMP-3 VALUE ZERO.        
011200     03  KVPB-EXP                PIC S9(6)V9(3)        COMP-3.            
011300     03  OI                      PIC S9(9)             COMP-3.            
011400     03  WS-OI                   PIC S9(9)V9(2)        COMP-3.            
011500     03  ABS-PLAN-DEM            PIC  9(9)V9(2)        COMP-3.            
011600     03  JFR-KVUTJFEL            PIC S9(6)V9(1)        COMP-3.            
011700     03  TIFINLV                 PIC S9(5).                               
011800     03    FILLER    REDEFINES TIFINLV.                                   
011900       05  TIFINLV-AAVV          PIC 9(4).                                
012000       05    FILLER              PIC S9.                                  
012100     03  W-KVMAD                 PIC  9(6)V9           COMP-3.            
012200     03 WS-KVMAD-TOT-MIN         PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
012300     03 WS-KVPB-PLAN-MASK        PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
012400                                                                          
012500     03  -COPY WDK611  -PRE WS-                                           
012600     EJECT                                                                
012700     03  -COPY WDK611  -PRE WSMASK-                                       
012800     EJECT                                                                
012900 01  GENERELLA-SUBPROGRAM.                                                
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
013200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013300     03  W015RAND                PIC X(8)    VALUE 'W015RAND'.            
013400     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
013500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013900                                                                          
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005   -PRE  POSTSUM-                                         
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
014500*01  -COPY WDATAREA                                                       
014600     EJECT                                                                
014700*01  -COPY W200W001                                                       
014800     EJECT                                                                
014900*    AREOR FÖR BEARBETNING AV ORDER-INGÅNGS-REGISTRET                     
015000*                                                                         
015100 01  W015RAND-IDARTNR        PIC S9(9)                   COMP-3.          
015200*                                                                         
015300 01  W015RAND-RANDKEY        PIC X(4).                                    
015400*                                                                         
015500 01  TRANS-ID.                                                            
015600     03  TRANS-ADR           PIC X(4).                                    
015700     03  TRANS-IDARTNR       PIC S9(9)                   COMP-3.          
015800*                                                                         
015900 01  OI-ID.                                                               
016000     03  OI-ADR              PIC X(4).                                    
016100     03  OI-IDARTNR          PIC S9(9)                   COMP-3.          
016200*                                                                         
016300 01  WDK6                    PIC X(4)    VALUE 'WDK6'.                    
016400                                                                          
016500 01  PARM-SYSIN-AREA-START       PIC X(24)   VALUE                        
016600                                 'PARM-SYSIN-AREA-START'.                 
016700 01  PARM-SYSIN.                                                          
016800     03  PARM-WRITE-LOG-INFO  PIC X(3)  VALUE SPACE.                      
016900     03  FILLER               PIC X(77).                                  
017000*                                                                         
017100 01  I31BAS-AREA-START           PIC X(24)   VALUE                        
017200                                 'I31BAS-AREA-START   '.                  
017300*01  AREA  -COPY W22231     -PRE I31BAS-.                                 
017400     EJECT                                                                
017500*01  AREA  -COPY W22231     -PRE W-I31-.                                  
017600     EJECT                                                                
017700*                                                                         
017800*    PARAMETRAR TILL DATKORT                                              
017900*                                                                         
018000 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22229'.                  
018100 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
018200*01  -COPY WDATKORT                                                       
018300     EJECT                                                                
018400*    AREA FÖR CALL AV KVPB-PLAN                                           
018500*                                                                         
018600*01  -COPY W222PBTO                                                       
018700     EJECT                                                                
018800 01  UT-W22229-AREA-START        PIC X(24)   VALUE                        
018900                                 'UT-W22229-AREA-START'.                  
019000*01  AREA -COPY W22229     -PRE UT-                                       
019100     EJECT                                                                
019200 01  UL-W22229L-AREA-START        PIC X(24)   VALUE                       
019300                                 'UL-W22229L-AREA-START'.                 
019400*01  AREA       -COPY W22229L     -PRE UL-                                
019500     EJECT                                                                
019600*01  INIT-AREA  -COPY W22229L     -PRE LOG-                               
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019900                                                                          
020000 01  KEYS-TILL-DLI.                                                       
020100     03  W-IDARTNR-X.                                                     
020200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020300     03  W-KDERS-0-X.                                                     
020400         05  FILLER              PIC S9(3)   VALUE ZERO COMP-3.           
020500                                                                          
020600*    --- STATUS-KOD FRÅN IMS                                              
020700 01  STATUS-WS                   PIC XX.                                  
020800     88  SEGMENT-FOUND                       VALUE '  '.                  
020900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
021000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
021100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
021200     88  IMS-NOT-OK                          VALUE 'XD'.                  
021300                                                                          
021400 01  GOOD-STATUSCODES.                                                    
021500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021600                                                                          
021700 01  SSA1                PIC X(128).                                      
021800 01  SSA2                PIC X(128).                                      
021900 01  SSA3                PIC X(128).                                      
022000     EJECT                                                                
022100*    --- IMS FUNCTION CODES                                               
022200*01  -COPY W0003                                                          
022300     EJECT                                                                
022400*    ---  DLI INPUT-OUTPUT AREA                                           
022500                                                                          
022600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022700 01  DLI-IO-WDK601.                                                       
022800*    03  -COPY WDK601                                                     
022900     EJECT                                                                
023000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
023100 01  DLI-IO-WDK611.                                                       
023200*    03  -COPY WDK611                                                     
023300     EJECT                                                                
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK626'.                      
023500 01  DLI-IO-WDK626.                                                       
023600*    03  -COPY WDK626                                                     
023700     EJECT                                                                
023800                                                                          
023900 LINKAGE SECTION.                                                         
024000*                                                                         
024100*01  -COPY W0008  -PRE WDK6-                                              
024200     05  FILLER                    PIC X.                                 
024300                                                                          
024400 01  PBTO-WDK6-PCB                 PIC X(1).                              
024500 01  PBTO-WDK7-PCB                 PIC X(1).                              
024600 01  PBTO-ARTM-PCB                 PIC X(1).                              
024700 01  PBTO-2501-PCB                 PIC X(1).                              
024800 01  PBTO-WDB6R-PCB                PIC X(1).                              
024900 01  PBTO-WDK7R-PCB                PIC X(1).                              
025000 01  PBTO-WDB6-PCB                 PIC X(1).                              
025100 01  PBTO-WDD7-PCB                 PIC X(1).                              
025200 01  PBTO-WDK7E-PCB                PIC X(1).                              
025300 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
025400 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
025500 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
025600 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
025700 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
025800 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
025900 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
026000 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
026100     EJECT                                                                
026200 PROCEDURE DIVISION  USING WDK6-PCB                                       
026300                           PBTO-WDK6-PCB  PBTO-WDK7-PCB                   
026400                           PBTO-ARTM-PCB  PBTO-2501-PCB                   
026500                           PBTO-WDB6R-PCB PBTO-WDK7R-PCB                  
026600                           PBTO-WDB6-PCB  PBTO-WDD7-PCB                   
026700                           PBTO-WDK7E-PCB                                 
026800                           PBTO-W222-UTIL-WDK6-PCB                        
026900                           PBTO-W222-UTIL-WDK7-PCB                        
027000                           PBTO-W222-UTIL-WDB6-PCB                        
027100                           PBTO-W222-UTUP-WDK7-PCB                        
027200                           PBTO-W222-UTUP-WDB6-PCB                        
027300                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
027400                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
027500                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
027600                           .                                              
027700     ENTRY 'DLITCBL' USING WDK6-PCB                                       
027800                           PBTO-WDK6-PCB  PBTO-WDK7-PCB                   
027900                           PBTO-ARTM-PCB  PBTO-2501-PCB                   
028000                           PBTO-WDB6R-PCB PBTO-WDK7R-PCB                  
028100                           PBTO-WDB6-PCB  PBTO-WDD7-PCB                   
028200                           PBTO-WDK7E-PCB                                 
028300                           PBTO-W222-UTIL-WDK6-PCB                        
028400                           PBTO-W222-UTIL-WDK7-PCB                        
028500                           PBTO-W222-UTIL-WDB6-PCB                        
028600                           PBTO-W222-UTUP-WDK7-PCB                        
028700                           PBTO-W222-UTUP-WDB6-PCB                        
028800                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
028900                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
029000                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
029100                           .                                              
029200                                                                          
029300     PERFORM A-INIT                                                       
029400                                                                          
029500     PERFORM IMS-GN-WDK601                                                
029600     PERFORM UNTIL SEGMENT-NOMORE                                         
029700       MOVE ART-IDARTNR               TO W-IDARTNR                        
029800       MOVE ART-KDPRODSL              TO TEST-KDPRODSL                    
029900       IF SW-LOG = JA                                                     
030000         MOVE LOG-INIT-AREA           TO UL-AREA                          
030100         MOVE ART-IDARTNR             TO UL-IDARTNR                       
030200         MOVE ART-KDPRODSL            TO UL-KDPRODSL                      
030300       END-IF                                                             
030400       PERFORM IMS-GNP-WDK611                                             
030500       IF SEGMENT-FOUND AND KDPRODSL-VOLVO-BIMA                           
030600         MOVE 1.0                     TO VIKT                             
030700         MOVE CLAG-WDK611             TO WS-CLAG-WDK611                   
030800         PERFORM IMS-GNP-WDK626                                           
030900         IF SEGMENT-MISSING                                               
031000            PERFORM E-SKAPA-NOLL-TREND                                    
031100         END-IF                                                           
031200         PERFORM B-STYR-BEARB-PER-CLAGER                                  
031300       END-IF                                                             
031400                                                                          
031500      PERFORM IMS-GN-WDK601                                               
031600     END-PERFORM                                                          
031700                                                                          
031800     MOVE ZERO TO RETURN-CODE                                             
031900     PERFORM C-CLOSE                                                      
032000     GOBACK.                                                              
032100     EJECT                                                                
032200******************************************************************        
032300*                                                                *        
032400*    ÖPPNAR OUTPUTFILEN, LÄSER DATUMKORT, INITIERAR DIV FÄLT     *        
032500*                                                                *        
032600******************************************************************        
032700                                                                          
032800 A-INIT         SECTION.                                                  
032900     MOVE 'A-INIT                       ' TO CURRENT-SECTION              
033000                                                                          
033100     OPEN INPUT W22231                                                    
033200         OUTPUT W22229                                                    
033300                W22229L                                                   
033400*                                                                         
033500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
033600     MOVE D-AAR TO JUST-K-AAR                                             
033700     IF JUST-K-AAR = ZERO                                                 
033800        IF JUST-K-AAR-2 < K-AAR                                           
033900           MOVE 99 TO JUST-K-AAR                                          
034000        END-IF                                                            
034100     ELSE                                                                 
034200        IF JUST-K-AAR-2 > K-AAR                                           
034300           ADD -1 TO JUST-K-AAR                                           
034400        END-IF                                                            
034500     END-IF                                                               
034600     MOVE K-VECKA TO JUST-K-VV                                            
034700     ADD -5 TO JUST-K-VV                                                  
034800*                                                                         
034900     IF JUST-K-VV NEGATIVE                                                
035000         ADD  +52 TO JUST-K-VV                                            
035100         IF JUST-K-AAR = ZERO                                             
035200            MOVE 99 TO JUST-K-AAR                                         
035300         ELSE                                                             
035400            ADD  -1 TO JUST-K-AAR                                         
035500         END-IF                                                           
035600     END-IF                                                               
035700*                                                                         
035800                                                                          
035900     MOVE D-AAR              TO DAGENS-AA                                 
036000     MOVE D-MAANAD           TO DAGENS-MM                                 
036100     MOVE D-DAG              TO DAGENS-DD                                 
036200     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
036300     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
036400                                                                          
036500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036600                     DAT-O-TIDATUM DAT-KDSVAR                             
036700                                                                          
036800     IF DAT-KDSVAR-OK                                                     
036900****             HÄMTA DAT-TIAARP                                         
037000         CONTINUE                                                         
037100                                                                          
037200     ELSE                                                                 
037300         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
037400         DELIMITED BY SIZE INTO ERROR-TEXT-STR                            
037500         CALL FELLOG                                                      
037600     END-IF                                                               
037700                                                                          
037800     MOVE 'W22229'   TO POSTSUM-PROGNAMN                                  
037900     MOVE ZERO       TO OI-IDARTNR                                        
038000     MOVE LOW-VALUE  TO OI-ADR                                            
038100                                                                          
038200     ACCEPT PARM-SYSIN FROM SYSIN                                         
038300     IF PARM-WRITE-LOG-INFO = 'LOG'                                       
038400        DISPLAY 'LOG INFO WRITTEN TO FILE W22229L'                        
038500        MOVE JA          TO SW-LOG                                        
038600        INITIALIZE LOG-INIT-AREA                                          
038700     ELSE                                                                 
038800        DISPLAY 'LOG INFO NOT WRITTEN TO FILE W22229L'                    
038900        MOVE    'LOG INFO NOT WRITTEN'     TO UL-AREA                     
039000        PERFORM S12-SKRIV-W22229L                                         
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400******************************************************************        
039500*                                                                *        
039600*    STYR BERABETNINGEN PER CLAGER                               *        
039700*                                                                *        
039800******************************************************************        
039900                                                                          
040000 B-STYR-BEARB-PER-CLAGER SECTION.                                         
040100     MOVE 'B-STYR-BEARB-PER-CLAGER' TO CURRENT-SECTION                    
040200                                                                          
040300     MOVE ART-TIFINLV        TO TIFINLV                                   
040400                                                                          
040500     MOVE NEJ                TO SW-I31BAS-READ                            
040600                                SW-MASK-CALCULATED                        
040700                                SW-MAN-CALCULATED                         
040800     MOVE SPACE              TO WS-MASK-PLAN-USED                         
040900     MOVE ZERO               TO WSMASK-KVPB-TOT                           
041000                                WSMASK-CLAG-KVUTJFEL                      
041100                                WSMASK-CLAG-KVMAD-SEP                     
041200                                WSMASK-CLAG-KVMAD-TOT                     
041300                                WS-KVMAD-TOT-MIN                          
041400                                                                          
041500     IF SW-LOG = JA                                                       
041600       MOVE CLAG-DAPBPLAN    TO UL-DAPBPLAN                               
041700       MOVE CLAG-KVPB-PLAN   TO UL-KVPB-PLAN                              
041800       MOVE CLAG-KVPB-SEP    TO UL-KVPB-SEP                               
041810       MOVE CLAG-KVPB-SATS   TO UL-KVPB-SATS                              
041900       MOVE CLAG-KVMAD-TOT   TO UL-KVMAD-TOT                              
042000       MOVE CLAG-RVPROFEL    TO UL-RVPROFEL                               
042100       MOVE CLAG-KVUTJFEL    TO UL-KVUTJFEL                               
042200       MOVE DAT-TIRP         TO UL-TIRP                                   
042300       MOVE CLAG-RESEASON-PLAN (DAT-TIRP)                                 
042400                             TO UL-RESEASON                               
042500       MOVE NEJ              TO UL-FL-OI-ID-EQ-TRANS-ID                   
042600     END-IF                                                               
042700                                                                          
042800     MOVE ZERO               TO PBTO-KVPB-PLAN                            
042900     MOVE TIFINLV-AAVV       TO TMP1-YYWW                                 
043000     MOVE JUSTERAD-K-VECKA   TO TMP2-YYWW                                 
043100     PERFORM WY2000P3                                                     
043200     IF TMP1-YYWW  < TMP2-YYWW                                            
043300       MOVE ART-IDARTNR      TO PBTO-IDARTNR                              
043400       CALL W222PBTO USING PBTO-W222PBTO                                  
043500                           PBTO-WDK6-PCB  PBTO-WDK7-PCB                   
043600                           PBTO-ARTM-PCB  PBTO-2501-PCB                   
043700                           PBTO-WDB6R-PCB PBTO-WDK7R-PCB                  
043800                           PBTO-WDB6-PCB  PBTO-WDD7-PCB                   
043900                           PBTO-WDK7E-PCB                                 
044000                           PBTO-W222-UTIL-WDK6-PCB                        
044100                           PBTO-W222-UTIL-WDK7-PCB                        
044200                           PBTO-W222-UTIL-WDB6-PCB                        
044300                           PBTO-W222-UTUP-WDK7-PCB                        
044400                           PBTO-W222-UTUP-WDB6-PCB                        
044500                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
044600                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
044700                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
044800                                                                          
044900       MOVE PBTO-KVPB-PLAN TO WS-KVPB-PLAN-MASK                           
045000                                                                          
045100       IF  WS-CLAG-DAPBPLAN     = ZERO                                    
045200       AND WS-CLAG-KVPB-PLAN    > ZERO                                    
045300* CALCULATE KVMAD-TOT USING OLD MACHINE (I.E. MASKINELL) PB-PLAN          
045400          MOVE WS-CLAG-KVPB-PLAN TO PBTO-KVPB-PLAN                        
045500          MOVE 'OLD'             TO WS-MASK-PLAN-USED                     
045600       ELSE                                                               
045700* CALCULATE KVMAD-TOT USING NEW MACHINE (I.E. MASKINELL) PB-PLAN          
045800          MOVE 'NEW'             TO WS-MASK-PLAN-USED                     
045900       END-IF                                                             
046000       MOVE PBTO-KVPB-PLAN   TO KVPB-TOT                                  
046100       ADD WS-CLAG-KVPB-SATS TO KVPB-TOT                                  
046200       COMPUTE WS-KVMAD-TOT-MIN ROUNDED = KVPB-TOT * 0.10 / 1.25          
046300       IF KVPB-TOT > ZERO                                                 
046400           IF WS-CLAG-KVPB-SATS > +1 OR                                   
046500              PBTO-KVPB-PLAN  > +0.1                                      
046600              PERFORM S04-LAES-ORDERING                                   
046700              MOVE JA        TO SW-I31BAS-READ                            
046800                                                                          
046900              PERFORM BA-BERAKNA-PROGNOS-FEL                              
047000              MOVE JA        TO SW-MASK-CALCULATED                        
047100           ELSE                                                           
047200             IF PBTO-KVPB-PLAN < 0.2                                      
047300               PERFORM BB-EXP-PROGNOS                                     
047400               MOVE JA        TO SW-MASK-CALCULATED                       
047500             END-IF                                                       
047600           END-IF                                                         
047700                                                                          
047800* CHECK IF RESULTING KVMAD-TOT IS LESS THAN ALLOWED MINIMUM               
047900           IF SW-MASK-CALCULATED = JA                                     
048000              IF WS-CLAG-KVMAD-TOT < WS-KVMAD-TOT-MIN                     
048100                 MOVE WS-KVMAD-TOT-MIN TO WS-CLAG-KVMAD-TOT               
048200              END-IF                                                      
048300           END-IF                                                         
048400       END-IF                                                             
048500                                                                          
048600       IF SW-LOG = JA                                                     
048700          MOVE SW-MASK-CALCULATED  TO UL-FLMASK-CALCULATED                
048800          MOVE PBTO-KVPB-PLAN      TO UL-KVPB-PLAN-MASK                   
048900          MOVE KVPB-TOT            TO UL-KVPB-TOT-MASK                    
049000          MOVE PROGFEL-TOT         TO UL-PROGFEL-TOT-MASK                 
049100          MOVE WS-CLAG-KVUTJFEL    TO UL-KVUTJFEL-MASK                    
049200          MOVE WS-KVMAD-TOT-MIN    TO UL-KVMAD-TOT-MIN-MASK               
049300          MOVE WS-CLAG-KVMAD-TOT   TO UL-KVMAD-TOT-MASK                   
049400       END-IF                                                             
049500                                                                          
049600* SAVE VALUES FOR MACHINE KVPB-PLAN                                       
049700       MOVE KVPB-TOT          TO WSMASK-KVPB-TOT                          
049800       MOVE WS-CLAG-KVUTJFEL  TO WSMASK-CLAG-KVUTJFEL                     
049900       MOVE WS-CLAG-KVMAD-SEP TO WSMASK-CLAG-KVMAD-SEP                    
050000       MOVE WS-CLAG-KVMAD-TOT TO WSMASK-CLAG-KVMAD-TOT                    
050100                                                                          
050200* CHECK IF MANUAL PB-PLAN EXISTS                                          
050300       IF  WS-CLAG-DAPBPLAN     > ZERO                                    
050400                                                                          
050500* RESET TO ORIGNAL WDK611-VALUES FOR NEW CALCULATION                      
050600          MOVE CLAG-KVUTJFEL     TO WS-CLAG-KVUTJFEL                      
050700          MOVE CLAG-KVMAD-SEP    TO WS-CLAG-KVMAD-SEP                     
050800          MOVE CLAG-KVMAD-TOT    TO WS-CLAG-KVMAD-TOT                     
050900                                                                          
051000* CALCULATE KVMAD-TOT USING MANUAL PB-PLAN (IF ONE EXISTS)                
051100          MOVE WS-CLAG-KVPB-PLAN TO PBTO-KVPB-PLAN                        
051200          MOVE PBTO-KVPB-PLAN    TO KVPB-TOT                              
051300          ADD WS-CLAG-KVPB-SATS  TO KVPB-TOT                              
051400          IF KVPB-TOT > ZERO                                              
051500              IF WS-CLAG-KVPB-SATS > +1 OR                                
051600                 PBTO-KVPB-PLAN  > +0.1                                   
051700                 IF SW-I31BAS-READ = NEJ                                  
051800                    PERFORM S04-LAES-ORDERING                             
051900                 END-IF                                                   
052000                                                                          
052100                 PERFORM BA-BERAKNA-PROGNOS-FEL                           
052200                 MOVE JA         TO SW-MAN-CALCULATED                     
052300              ELSE                                                        
052400                IF PBTO-KVPB-PLAN < 0.2                                   
052500                  PERFORM BB-EXP-PROGNOS                                  
052600                  MOVE JA        TO SW-MAN-CALCULATED                     
052700                END-IF                                                    
052800              END-IF                                                      
052900                                                                          
053000* CHECK IF RESULTING KVMAD-TOT IS LESS THAN ALLOWED MINIMUM               
053100              IF SW-MAN-CALCULATED = JA                                   
053200                 IF WS-CLAG-KVMAD-TOT < WS-KVMAD-TOT-MIN                  
053300                    MOVE WS-KVMAD-TOT-MIN TO WS-CLAG-KVMAD-TOT            
053400                 END-IF                                                   
053500              END-IF                                                      
053600          END-IF                                                          
053700                                                                          
053800          IF SW-LOG = JA                                                  
053900             MOVE SW-MAN-CALCULATED   TO UL-FLMAN-CALCULATED              
054000             MOVE PBTO-KVPB-PLAN      TO UL-KVPB-PLAN-MAN                 
054100             MOVE KVPB-TOT            TO UL-KVPB-TOT-MAN                  
054200             MOVE PROGFEL-TOT         TO UL-PROGFEL-TOT-MAN               
054300             MOVE WS-CLAG-KVUTJFEL    TO UL-KVUTJFEL-MAN                  
054400             MOVE WS-KVMAD-TOT-MIN    TO UL-KVMAD-TOT-MIN-MAN             
054500             MOVE WS-CLAG-KVMAD-TOT   TO UL-KVMAD-TOT-MAN                 
054600          END-IF                                                          
054700       END-IF                                                             
054800                                                                          
054900* CHECK IF MANUAL OR MACHINE KVPB-PLAN RESULTED IN LOWER KVMAD-TOT        
055000       EVALUATE TRUE                                                      
055100       WHEN SW-MAN-CALCULATED = JA                                        
055200        AND SW-MASK-CALCULATED = JA                                       
055300        AND WS-CLAG-KVMAD-TOT < WSMASK-CLAG-KVMAD-TOT                     
055400          IF SW-LOG = JA                                                  
055500             MOVE 'MAN PB'            TO UL-KDVALUES-USED                 
055600          END-IF                                                          
055700       WHEN SW-MASK-CALCULATED = JA                                       
055800          MOVE WSMASK-KVPB-TOT        TO KVPB-TOT                         
055900          MOVE WSMASK-CLAG-KVUTJFEL   TO WS-CLAG-KVUTJFEL                 
056000          MOVE WSMASK-CLAG-KVMAD-SEP  TO WS-CLAG-KVMAD-SEP                
056100          MOVE WSMASK-CLAG-KVMAD-TOT  TO WS-CLAG-KVMAD-TOT                
056200          IF SW-LOG = JA                                                  
056300             MOVE 'MASK '             TO UL-KDVALUES-USED                 
056400             MOVE WS-MASK-PLAN-USED   TO UL-KDVALUES-USED(6:3)            
056500          END-IF                                                          
056600       WHEN SW-MAN-CALCULATED = JA                                        
056700          IF SW-LOG = JA                                                  
056800             MOVE 'MAN PB'            TO UL-KDVALUES-USED                 
056900          END-IF                                                          
057000       END-EVALUATE                                                       
057100                                                                          
057200                                                                          
057300       IF KVPB-TOT > ZERO                                                 
057400           PERFORM BC-BERAKNA-NYTT-KVPB-PLAN                              
057500           PERFORM S02-SKRIV-ART-POST                                     
057600       ELSE                                                               
057700         MOVE SPACE      TO POSTSUM-TRANSTYP                              
057800         MOVE 'W22229'   TO POSTSUM-FDNAMN                                
057900         MOVE 'PB-TOT=0' TO POSTSUM-DDNAMN2                               
058000         CALL POSTSUM USING POSTSUM-PARM                                  
058100       END-IF                                                             
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500******************************************************************        
058600*                                                                *        
058700*    BERÄKNAR PROGNOSFEL, KONTROLLERAR STORLEKEN PÅ FELET        *        
058800*    OCH SKAPAR EV PROGNOSLARM-POST                              *        
058900*                                                                *        
059000******************************************************************        
059100                                                                          
059200 BA-BERAKNA-PROGNOS-FEL SECTION.                                          
059300     MOVE 'BA-BERAKNA-PROGNOS-FEL' TO CURRENT-SECTION                     
059400                                                                          
059500     IF  WS-CLAG-KVPB-SEP = ZERO                                          
059510     AND KVPB-TOT         = ZERO                                          
059600         MOVE ZERO    TO PROGFEL-SEP PROGFEL-TOT                          
059700     ELSE                                                                 
059800         PERFORM BAA-UTFOR-PROGFEL-BERAKN                                 
059900     END-IF                                                               
060000     MOVE PROGFEL-SEP TO ABS-PROGFEL-SEP                                  
060100*                                                                         
060200     MOVE PROGFEL-TOT TO ABS-PROGFEL-TOT                                  
060300*                                                                         
060400     PERFORM BAC-BERAKNA-MAD-UTJFEL                                       
060500     .                                                                    
060600     EJECT                                                                
060700 BAA-UTFOR-PROGFEL-BERAKN SECTION.                                        
060800     MOVE 'BAA-UTFOR-PROGFEL-BERAKN ' TO CURRENT-SECTION                  
060900                                                                          
061000*****MOVE I31BAS-KVOI-PROG     TO OI                                      
061100     COMPUTE OI ROUNDED = I31BAS-KVOI-PROG * 4.33 / DAT-KVVIPER           
061200*                                                                         
061300     MULTIPLY JUST-RESEASON (DAT-TIRP)                                    
061400       BY VIKT                                                            
061500       GIVING PROGFEL-SEP ROUNDED                                         
061600                                                                          
061700     MULTIPLY WS-CLAG-KVPB-SEP BY PROGFEL-SEP                             
061800*                          OBS RESULTATET I PROGFEL-SEP !                 
061900                                                                          
062000     SUBTRACT OI FROM PROGFEL-SEP.                                        
062100*                                                                         
062200*                                                                         
062300*****ADD  I31BAS-KVOI-REFILL   TO OI                                      
062400     COMPUTE OI ROUNDED = OI +                                            
062500             I31BAS-KVOI-REFILL * 4.33 / DAT-KVVIPER                      
062600     IF WS-CLAG-KVPB-SATS > ZERO                                          
062700*****ADD  I31BAS-KVOI-SATS     TO OI                                      
062800        COMPUTE OI ROUNDED = OI +                                         
062900             I31BAS-KVOI-SATS * 4.33 / DAT-KVVIPER                        
063000     END-IF                                                               
063100*                                                                         
063200     IF WS-CLAG-DASEASON     > ZERO                                       
063300        MULTIPLY WS-CLAG-RESEASON-PLAN (DAT-TIRP)                         
063400          BY VIKT                                                         
063500          GIVING PROGFEL-TOT ROUNDED                                      
063600     ELSE                                                                 
063700        MULTIPLY 1.0                                                      
063800          BY VIKT                                                         
063900          GIVING PROGFEL-TOT ROUNDED                                      
064000     END-IF                                                               
064100     MULTIPLY PBTO-KVPB-PLAN BY PROGFEL-TOT                               
064200*                          OBS RESULTATET I PROGFEL-TOT !                 
064300     ADD WS-CLAG-KVPB-SATS TO PROGFEL-TOT                                 
064400     SUBTRACT OI FROM PROGFEL-TOT                                         
064500     .                                                                    
064600     EJECT                                                                
064700******************************************************************        
064800*                                                                *        
064900*    UPPDATERAR PROGNOSFEL, KONTROLLERAR URSPÅRAD PROGNOS OCH    *        
065000*    SKAPAR EV PROGNOSLARM-POST                                  *        
065100*                                                                *        
065200******************************************************************        
065300                                                                          
065400 BAC-BERAKNA-MAD-UTJFEL SECTION.                                          
065500                                                                          
065600     COMPUTE WS-CLAG-KVUTJFEL ROUNDED = 0.80 * WS-CLAG-KVUTJFEL +         
065700                                        0.20 * PROGFEL-TOT                
065800*                                                                         
065900     IF WS-CLAG-RVPROFEL = ZERO                                           
066000        PERFORM BACA-UPPDATERA-PROGNOSFEL-MAD                             
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400 BACA-UPPDATERA-PROGNOSFEL-MAD SECTION.                                   
066500     MOVE 'BACA-UPPDATERA-PROGNOSFEL-MAD' TO CURRENT-SECTION              
066600                                                                          
066700     MOVE WS-CLAG-KVMAD-SEP TO W-KVMAD                                    
066800     IF WS-CLAG-KVPB-SEP > +0.1                                           
066900         COMPUTE WS-CLAG-KVMAD-SEP ROUNDED                                
067000              = 0.80 * WS-CLAG-KVMAD-SEP + 0.20 * ABS-PROGFEL-SEP         
067100     ELSE                                                                 
067200         COMPUTE WS-CLAG-KVMAD-SEP ROUNDED                                
067300                 = WS-CLAG-KVPB-SEP ** 0.85                               
067400     END-IF                                                               
067500                                                                          
067600     IF WS-CLAG-KVUTJFEL > +0 AND                                         
067700        WS-CLAG-KVMAD-SEP > W-KVMAD                                       
067800         MOVE W-KVMAD TO WS-CLAG-KVMAD-SEP                                
067900     END-IF                                                               
068000*                                                                         
068100     MOVE WS-CLAG-KVMAD-TOT TO W-KVMAD                                    
068200     IF WS-CLAG-KVPB-SATS > ZERO                                          
068300         COMPUTE WS-CLAG-KVMAD-TOT ROUNDED                                
068400            =  0.80 * WS-CLAG-KVMAD-TOT + 0.20 * ABS-PROGFEL-TOT          
068500     ELSE                                                                 
068600*********MOVE WS-CLAG-KVMAD-SEP TO WS-CLAG-KVMAD-TOT                      
068700       PERFORM BACAA-BER-MAD-TOT-UTAN-SATS                                
068800     END-IF                                                               
068900                                                                          
069000     IF  WS-CLAG-KVMAD-TOT > W-KVMAD                                      
069100     AND OI                < KVPB-TOT * 0.7                               
069200**** FIX FÖR ATT RÄTTA MAD = 0                                            
069300     AND W-KVMAD           > 0                                            
069400         MOVE W-KVMAD TO WS-CLAG-KVMAD-TOT                                
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 BACAA-BER-MAD-TOT-UTAN-SATS    SECTION.                                  
069900     MOVE 'BACAA-BER-MAD-TOT-UTAN-SATS  ' TO CURRENT-SECTION              
070000                                                                          
070100     IF PBTO-KVPB-PLAN   > +0.1                                           
070200         COMPUTE WS-CLAG-KVMAD-TOT ROUNDED                                
070300              = 0.80 * WS-CLAG-KVMAD-TOT + 0.20 * ABS-PROGFEL-TOT         
070400     ELSE                                                                 
070500         COMPUTE WS-CLAG-KVMAD-TOT ROUNDED                                
070600                 = PBTO-KVPB-PLAN ** 0.85                                 
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000*                                                                         
071100******************************************************************        
071200*                                                                *        
071300*    BERÄKNAR PROGNOS M HJ A EXPONENTIERING                      *        
071400*                                                                *        
071500******************************************************************        
071600                                                                          
071700 BB-EXP-PROGNOS SECTION.                                                  
071800     MOVE 'BB-EXP-PROGNOS               ' TO CURRENT-SECTION              
071900                                                                          
072000     COMPUTE WS-CLAG-KVMAD-SEP ROUNDED                                    
072100                               = WS-CLAG-KVPB-SEP ** 0.85                 
072200*                                                                         
072300     COMPUTE WS-CLAG-KVMAD-TOT ROUNDED = (PBTO-KVPB-PLAN +                
072400                                     WS-CLAG-KVPB-SATS) ** 0.85           
072500     .                                                                    
072600     EJECT                                                                
072700******************************************************************        
072800*                                                                *        
072900*    OM KVPB-PLAN EJ MANUELLT, BERÄKNA NYTT VÄRDE FÖR KOMMANDE   *        
073000*    PERIOD                                                      *        
073100*    NYTT VÄRDE FÅS FRÅN DET MASKINELLT FRAMSTÄLLDA              *        
073200*                                                                *        
073300******************************************************************        
073400                                                                          
073500 BC-BERAKNA-NYTT-KVPB-PLAN SECTION.                                       
073600     MOVE 'BC-BERAKNA-NYTT-KVPB-PLAN    ' TO CURRENT-SECTION              
073700                                                                          
073800     IF WS-CLAG-DAPBPLAN = ZERO                                           
073900        MOVE WS-KVPB-PLAN-MASK TO WS-CLAG-KVPB-PLAN                       
074000     END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300******************************************************************        
074400*                                                                *        
074500*    STÄNGER FILER, SKRIVER POST-RÄKNARE                         *        
074600*                                                                *        
074700******************************************************************        
074800                                                                          
074900 C-CLOSE SECTION.                                                         
075000     MOVE 'C-CLOSE                      ' TO CURRENT-SECTION              
075100                                                                          
075200     MOVE 'S' TO POSTSUM-OPKOD                                            
075300     CALL POSTSUM USING POSTSUM-PARM                                      
075400*                                                                         
075500     CLOSE W22231                                                         
075600           W22229                                                         
075700           W22229L                                                        
075800     .                                                                    
075900     EJECT                                                                
076000 E-SKAPA-NOLL-TREND SECTION.                                              
076100     SKIP3                                                                
076200     MOVE ZERO TO JUST-REPBJUST                                           
076300                  JUST-TIPBJUST-CENTR                                     
076400                  JUST-KVPB-JUST (1)                                      
076500                  JUST-KVPB-JUST (2)                                      
076600                  JUST-TIPBJUST (1)                                       
076700                  JUST-TIPBJUST (2)                                       
076800*                                                                         
076900     MOVE 1 TO TREND-IX                                                   
077000*                                                                         
077100     PERFORM UNTIL (TREND-IX  > 12)                                       
077200          MOVE 1.0 TO JUST-RESEASON (TREND-IX)                            
077300          ADD 1 TO TREND-IX                                               
077400     END-PERFORM                                                          
077500     .                                                                    
077600     EJECT                                                                
077700 S02-SKRIV-ART-POST SECTION.                                              
077800     MOVE 'S02-SKRIV-ART-POST          ' TO CURRENT-SECTION               
077900*                                                                         
078000     IF (CLAG-KVPB-PLAN        NOT = WS-CLAG-KVPB-PLAN   OR               
078100         CLAG-KVUTJFEL         NOT = WS-CLAG-KVUTJFEL    OR               
078200         CLAG-KVMAD-SEP        NOT = WS-CLAG-KVMAD-SEP   OR               
078300         CLAG-KVMAD-TOT        NOT = WS-CLAG-KVMAD-TOT)                   
078400                                                                          
078500         MOVE ART-IDARTNR       TO UT-IDARTNR                             
078600         MOVE WS-CLAG-KVUTJFEL  TO UT-KVUTJFEL                            
078700         MOVE WS-CLAG-KVMAD-SEP TO UT-KVMAD-SEP                           
078800         MOVE WS-CLAG-KVMAD-TOT TO UT-KVMAD-TOT                           
078900         MOVE WS-CLAG-KVPB-PLAN TO UT-KVPB-PLAN                           
079000                                                                          
079100         PERFORM S11-SKRIV-W22229                                         
079200     ELSE                                                                 
079300         MOVE SPACE      TO POSTSUM-TRANSTYP                              
079400         MOVE 'W22229'   TO POSTSUM-FDNAMN                                
079500         MOVE 'NOCHANGE' TO POSTSUM-DDNAMN2                               
079600         CALL POSTSUM USING POSTSUM-PARM                                  
079700                                                                          
079800         IF SW-LOG = JA                                                   
079900            MOVE 'NOCHG'         TO UL-KDVALUES-USED(4:5)                 
080000         END-IF                                                           
080100     END-IF                                                               
080200     IF SW-LOG = JA                                                       
080300         PERFORM S12-SKRIV-W22229L                                        
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 S04-LAES-ORDERING SECTION.                                               
080800     MOVE 'S04-LAES-ORDERING            ' TO CURRENT-SECTION              
080900                                                                          
081000     MOVE ART-IDARTNR           TO W015RAND-IDARTNR                       
081100                                                                          
081200     CALL W015RAND USING W015RAND-IDARTNR                                 
081300                         W015RAND-RANDKEY WDK6                            
081400     MOVE W015RAND-IDARTNR      TO TRANS-IDARTNR                          
081500     MOVE W015RAND-RANDKEY      TO TRANS-ADR                              
081600                                                                          
081700     PERFORM UNTIL (EOF-W22231 = JA OR (OI-ID NOT < TRANS-ID))            
081800         READ W22231 INTO W-I31-AREA                                      
081900            AT END                                                        
082000               MOVE JA          TO EOF-W22231                             
082100         END-READ                                                         
082200         IF EOF-W22231 = NEJ                                              
082300             MOVE 'W22231'      TO POSTSUM-FDNAMN                         
082400             MOVE 'W22229D1'    TO POSTSUM-DDNAMN2                        
082500             MOVE W-I31-IDPTYP  TO POSTSUM-TRANSTYP                       
082600             CALL POSTSUM USING POSTSUM-PARM                              
082700             MOVE W-I31-IDARTNR TO OI-IDARTNR                             
082800             MOVE W-I31-RANDKEY TO OI-ADR                                 
082900         ELSE                                                             
083000             MOVE HIGH-VALUE    TO OI-ID                                  
083100         END-IF                                                           
083200     END-PERFORM                                                          
083300     IF EOF-W22231 = NEJ                                                  
083400         MOVE W-I31-AREA        TO I31BAS-AREA                            
083500     END-IF                                                               
083600     IF OI-ID NOT = TRANS-ID                                              
083700         MOVE ZERO              TO I31BAS-KVOI-PROG                       
083800         MOVE ZERO              TO I31BAS-KVOI-REFILL                     
083900         MOVE ZERO              TO I31BAS-KVOI-SATS                       
084000         MOVE DAT-TIAARP        TO I31BAS-TIAAPP                          
084100     END-IF                                                               
084200     IF SW-LOG = JA                                                       
084300         IF OI-ID = TRANS-ID                                              
084400             MOVE JA                  TO UL-FL-OI-ID-EQ-TRANS-ID          
084500             MOVE I31BAS-KVOI-PROG    TO UL-KVOI-PROG                     
084600             MOVE I31BAS-KVOI-REFILL  TO UL-KVOI-REFILL                   
084700             MOVE I31BAS-KVOI-SATS    TO UL-KVOI-SATS                     
084800             MOVE I31BAS-TIAAPP       TO UL-TIAAPP                        
084900         END-IF                                                           
085000     END-IF                                                               
085100     IF DAT-TIAARP NOT = I31BAS-TIAAPP                                    
085200         DISPLAY 'DAT-TIAARP    = ' DAT-TIAARP                            
085300         DISPLAY 'I31BAS-TIAAPP = ' I31BAS-TIAAPP                         
085400         MOVE 24                TO RKOD                                   
085500         CALL ABEND USING RKOD                                            
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900 S11-SKRIV-W22229 SECTION.                                                
086000     MOVE 'S11-SKRIV-W22229             ' TO CURRENT-SECTION              
086100                                                                          
086200     WRITE UT-RECORD FROM UT-AREA                                         
086300                                                                          
086400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
086500     MOVE 'W22229'   TO POSTSUM-FDNAMN                                    
086600     MOVE 'W22229D2' TO POSTSUM-DDNAMN2                                   
086700     CALL POSTSUM USING POSTSUM-PARM                                      
086800     .                                                                    
086900     EJECT                                                                
087000 S12-SKRIV-W22229L SECTION.                                               
087100     MOVE 'S12-SKRIV-W22229L            ' TO CURRENT-SECTION              
087200                                                                          
087300     WRITE UL-RECORD FROM UL-AREA                                         
087400                                                                          
087500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
087600     MOVE 'LOGFIL'   TO POSTSUM-FDNAMN                                    
087700     MOVE 'W22229D3' TO POSTSUM-DDNAMN2                                   
087800     CALL POSTSUM USING POSTSUM-PARM                                      
087900     .                                                                    
088000     EJECT                                                                
088100 IMS-GN-WDK601 SECTION.                                                   
088200     MOVE 'IMS-GN-WDK601              ' TO DBS-SECTION                    
088300                                                                          
088400     STRING 'WDK601  *P(KDERS    =' W-KDERS-0-X ')'                       
088500             DELIMITED BY SIZE INTO SSA1                                  
088600     MOVE '  GB' TO GOOD-STATUSCODES                                      
088700     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK601 SSA1                    
088800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
088900     PERFORM IMS-STATUSCHECK                                              
089000     .                                                                    
089100                                                                          
089200 IMS-GNP-WDK611 SECTION.                                                  
089300     MOVE 'IMS-GNP-WDK611             ' TO DBS-SECTION                    
089400                                                                          
089500     MOVE 'WDK611   ' TO SSA2                                             
089600     MOVE '  ' TO GOOD-STATUSCODES                                        
089700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA2                   
089800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
089900     PERFORM IMS-STATUSCHECK                                              
090000     .                                                                    
090100                                                                          
090200 IMS-GNP-WDK626 SECTION.                                                  
090300     MOVE 'IMS-GNP-WDK626             ' TO DBS-SECTION                    
090400                                                                          
090500     MOVE 'WDK611   ' TO SSA1                                             
090600     MOVE 'WDK626   ' TO SSA2                                             
090700     MOVE '  GE' TO GOOD-STATUSCODES                                      
090800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK626 SSA1 SSA2              
090900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091000     PERFORM IMS-STATUSCHECK                                              
091100     EJECT                                                                
091200     .                                                                    
091300                                                                          
091400 IMS-STATUSCHECK SECTION.                                                 
091500                                                                          
091600     SET STATUS-IX TO 1                                                   
091700     SEARCH GOOD-STATUS                                                   
091800       AT END                                                             
091900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
092000           DELIMITED BY SIZE INTO ERROR-TEXT                              
092100         DISPLAY ERROR-TEXT                                               
092200         CALL FELLOG                                                      
092300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
092400         CONTINUE                                                         
092500     END-SEARCH                                                           
092600     .                                                                    
092700*                                                                         
092800*                                                                         
092900 DISP-STATUS SECTION.                                                     
093000                                                                          
093100     DISPLAY STATUS-WS                                                    
093200     CALL FELLOG                                                          
093300     .                                                                    
093400***************************                                               
093500*    -COPY WY2000P3                                                       
093600     EJECT                                                                
093700*    -COPY WY2000P6                                                       
