000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2214A00.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   19/10/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERING AV NY LEVERANSPLAN                                   
001000*        FIL W2214A FRÅN PGM W2214000 MED ALLA UPPDATERINGAR.             
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK6                                       
001300*                              WDD9                                       
001400*                              WDD6                                       
001500*                                                                         
001900*                                                                         
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- UPPDATERINGSPOSTER FRÅN W2214000                           
003000     SELECT W2214A                     ASSIGN TO W2214AD1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W2214A                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W2214A      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP1                                                                
004400*    -COPY WY2000W1                                                       
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W2214A00'.            
004700 01  CHKP-VAR.                                                            
004800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-ANT                 PIC S9(5)   VALUE +0   COMP-3.           
005300     03 CHKP-MAX                 PIC S9(3)   VALUE +900 COMP-3.           
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005700 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005800                                                                          
005900 01  WC-IDPTYP.                                                           
006000     03  UPPDAT-CLAG             PIC X(3)    VALUE '001'.                 
006100     03  BORTTAG-KOPPLING-LP     PIC X(3)    VALUE '002'.                 
006200     03  BORTTAG-OMSPEC          PIC X(3)    VALUE '003'.                 
006300     03  BORTTAG-AVROP           PIC X(3)    VALUE '004'.                 
006400                                                                          
006500     SKIP2                                                                
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900     SKIP2                                                                
007000 01  RKOD                        PIC S9(4)   VALUE +0  COMP SYNC.         
007100                                                                          
007200 77  W-KVPOST-IN                 PIC S9(7)   VALUE +0  COMP-3.            
007300                                                                          
007400 77  W2214A-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W2214A                       VALUE 'J'.                   
007600     EJECT                                                                
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     EJECT                                                                
008300 01  AKT-DATUM-AAVV      PIC 9(4).                                        
008400 01  FILLER REDEFINES AKT-DATUM-AAVV.                                     
008500     03  AKT-DATUM-AA    PIC 9(2).                                        
008600     03  AKT-DATUM-VV    PIC 9(2).                                        
008700                                                                          
008800 01  W-TIAAVVD-AKT               PIC S9(5)   VALUE ZERO COMP-3.           
008810 01  WS-DAYS-TIDATE1-AAVVD       PIC 9(5)    VALUE ZERO.                  
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009710     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL DATKORT                                          
010000*                                                                         
010100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2214A'.              
010200     SKIP2                                                                
010300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010400     SKIP2                                                                
010500*01  -COPY WDATKORT                                                       
010600     EJECT                                                                
010601                                                                          
010602 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
010603*   -COPY WZ20DAYS                                                        
010610     EJECT                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
010800*                                                                         
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL ABEND                                            
011200                                                                          
011300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011600     EJECT                                                                
011700*- - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                       
011800 01  WDATAREA                    PIC X(8)    VALUE 'WDATAREA'.            
011900*01  -COPY WDATAREA.                                                      
012000     EJECT                                                                
012100 01  IN-AREA-START               PIC X(24)   VALUE                        
012200                                             'IN-AREA-START'.             
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W2214A     -PRE IN-                                       
012600*                                                                         
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-KDSEGKEY-X.                                                    
013200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013300                                                                          
013400     03  W-IDARTNR-X.                                                     
013500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013600                                                                          
013700     03  W-WDD901KY-X.                                                    
013800         05  W-IDARTNR-D9        PIC  S9(9)  VALUE ZERO  COMP-3.          
013900         05  W-IDDC-D9           PIC  X(2)   VALUE SPACE.                 
014000                                                                          
014100     03  W-IDLEVNR-X.                                                     
014200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
014300                                                                          
014310     03  W-KDAVROP-X.                                                     
014320         05  W-KDAVROP           PIC S9      VALUE ZERO COMP-3.           
014340                                                                          
014400     03  W-WDD601KY-MIN-X.                                                
014500         05 W-IDDC-MIN           PIC X(2)    VALUE SPACE.                 
014600         05 W-IDLEVNR-MIN        PIC X(5)    VALUE SPACE.                 
014700         05 W-IDARTNR-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
014800         05 FILLER               PIC X(2)    VALUE LOW-VALUE.             
014900                                                                          
015000     03  W-WDD601KY-MAX-X.                                                
015100         05 W-IDDC-MAX           PIC X(2)    VALUE SPACE.                 
015200         05 W-IDLEVNR-MAX        PIC X(5)    VALUE SPACE.                 
015300         05 W-IDARTNR-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
015310         05 FILLER               PIC X(2)    VALUE HIGH-VALUE.            
015500                                                                          
015600     SKIP2                                                                
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FINNS                       VALUE '  '.                  
016000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016300     88  IMS-EJ-OK                           VALUE 'XD'.                  
016400     SKIP2                                                                
016500 01  GODK-STATUSKODER.                                                    
016600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700     SKIP3                                                                
016800 01  ALL-SSA.                                                             
016900     03  SSA1                        PIC X(64).                           
017000     03  SSA2                        PIC X(64).                           
017010     03  SSA3                        PIC X(64).                           
017100     EJECT                                                                
017200*    --- IMS FUNKTIONSKODER                                               
017300*01  -COPY W0003                                                          
017400     EJECT                                                                
017500*    ---  DLI INPUT-OUTPUT AREA                                           
017600                                                                          
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017800 01  DLI-IO-WDK601.                                                       
017900*    03  -COPY WDK601                                                     
018000     EJECT                                                                
018100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
018200 01  DLI-IO-WDK611.                                                       
018300*    03  -COPY WDK611                                                     
018400     EJECT                                                                
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
018600 01  DLI-IO-WDD901.                                                       
018700*    03  -COPY WDD901   -PRE D901-                                        
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
019000 01  DLI-IO-WDD902.                                                       
019100*    03  -COPY WDD902   -PRE D902-                                        
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
019400 01  DLI-IO-WDD904.                                                       
019500*    03  -COPY WDD904   -PRE D904-                                        
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
019800 01  DLI-IO-WDD905.                                                       
019900*    03  -COPY WDD905   -PRE D905-                                        
020000     SKIP2                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
020200 01  DLI-IO-WDD601.                                                       
020300*    03  -COPY WDD601                                                     
020900                                                                          
021000     EJECT                                                                
021100 LINKAGE SECTION.                                                         
021200                                                                          
021300*01  -COPY W0009   -PRE MSG-                                              
021400                                                                          
021500*01  -COPY W0008  -PRE WDK6-                                              
021600     05  FILLER                  PIC X.                                   
021700     EJECT                                                                
021800*01  -COPY W0008  -PRE WDD9-                                              
021900     05  FILLER                  PIC X.                                   
022000     EJECT                                                                
022100*01  -COPY W0008  -PRE WDD6-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDD9-PCB WDD6-PCB.            
022900                                                                          
023000 MAIN SECTION.                                                            
023100     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDD9-PCB WDD6-PCB.            
023300                                                                          
023400     SKIP2                                                                
023500     PERFORM A-INIT                                                       
023800                                                                          
024200     PERFORM S01-LAES-W2214A                                              
024400                                                                          
024500     PERFORM UNTIL END-OF-W2214A                                          
024600       IF CHKP-ANT > CHKP-MAX                                             
024700         PERFORM X-TAG-CHECKPOINT                                         
024800       END-IF                                                             
024900                                                                          
025000       EVALUATE IN-UPD-IDPTYP                                             
025100          WHEN UPPDAT-CLAG                                                
025200               PERFORM B-UPPDAT-CLAG                                      
025300                                                                          
025400          WHEN BORTTAG-KOPPLING-LP                                        
025500               PERFORM C-BORTTAG-KOPPLING-LP                              
025600                                                                          
025700          WHEN BORTTAG-OMSPEC                                             
025800               PERFORM D-BORTTAG-OMSPEC                                   
025900                                                                          
026000          WHEN BORTTAG-AVROP                                              
026100               PERFORM E-BORTTAG-AVROP                                    
026200                                                                          
026300       END-EVALUATE                                                       
026400                                                                          
026500       PERFORM S01-LAES-W2214A                                            
026600     END-PERFORM                                                          
026800                                                                          
026900     PERFORM Z-FINIT                                                      
027000                                                                          
027100     MOVE ZERO TO RETURN-CODE                                             
027200     GOBACK                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 A-INIT SECTION.                                                          
027600     SKIP2                                                                
027700                                                                          
027800     PERFORM IMS-RESTART                                                  
027900                                                                          
028000     OPEN INPUT W2214A                                                    
028100                                                                          
028200     MOVE +0          TO CHKP-ANT                                         
028300                         W-KVPOST-IN                                      
028400                                                                          
028500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028600     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
028700                         AKT-DATUM-AA                                     
028800     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
028900     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
029000     MOVE D-VECKA     TO AKT-DATUM-VV                                     
029001                                                                          
029010     MOVE AKT-DATUM-AAVV TO W-TIAAVVD-AKT                                 
029100                                                                          
029200     MULTIPLY 10 BY W-TIAAVVD-AKT                                         
029300                                                                          
029400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029500                                                                          
029600     .                                                                    
029700     EJECT                                                                
029800 B-UPPDAT-CLAG SECTION.                                                   
029900     MOVE 'B-UPPDAT-CLAG ' TO CURRENT-SECTION                             
030000*** SE E-UPPDAT-CLAG SECTION. I PGM W2214010                              
030100                                                                          
030200     MOVE IN-UPD-IDARTNR   TO W-IDARTNR                                   
030400                                                                          
030700     PERFORM IMS-GHU-WDK611                                               
030900                                                                          
031000     IF SEGMENT-FINNS                                                     
031100       MOVE IN-UPD-FLMANQ      TO CLAG-FLMANQ                             
031200       MOVE IN-UPD-KDAVT       TO CLAG-KDAVT                              
031300       MOVE IN-UPD-KDKSP       TO CLAG-KDKSP                              
031400       MOVE IN-UPD-KDLPSP      TO CLAG-KDLPSP                             
031500       MOVE IN-UPD-KDVVKL      TO CLAG-KDVVKL                             
031600       MOVE IN-UPD-KDOPPLAN    TO CLAG-KDOPPLAN                           
031700       MOVE IN-UPD-KVAP        TO CLAG-KVAP                               
031800       MOVE IN-UPD-KVBK        TO CLAG-KVBK                               
031900       MOVE IN-UPD-KVKP        TO CLAG-KVKP                               
032000       MOVE IN-UPD-KVOVERF     TO CLAG-KVOVERF                            
032100       MOVE IN-UPD-KVQ         TO CLAG-KVQ                                
032200       MOVE IN-UPD-KVQ-JUST    TO CLAG-KVQ-JUST                           
032300       MOVE IN-UPD-KVSLUTKP    TO CLAG-KVSLUTKP                           
032400       MOVE IN-UPD-TIBESRPT    TO CLAG-TIBESRPT                           
032500       MOVE IN-UPD-TIBESRPT-PAAM TO CLAG-TIBESRPT-PAAM                    
032600       MOVE IN-UPD-TILPSP      TO CLAG-TILPSP                             
032700       MOVE IN-UPD-TIQJUST     TO CLAG-TIQJUST                            
032800       MOVE IN-UPD-KVDAGAR-INLEV TO CLAG-KVDAGAR-INLEV                    
032900       MOVE IN-UPD-KVDAGAR-FFH TO CLAG-KVDAGAR-FFH                        
033000       MOVE IN-UPD-KVVECKOR-LT TO CLAG-KVVECKOR-LT                        
033100       MOVE IN-UPD-KVVECKOR-FT TO CLAG-KVVECKOR-FT                        
033200       MOVE IN-UPD-KVVECKOR-BT TO CLAG-KVVECKOR-BT                        
033300       MOVE IN-UPD-KVVECKOR-AT TO CLAG-KVVECKOR-AT                        
033400       MOVE IN-UPD-FLJIT       TO CLAG-FLJIT                              
033500       MOVE IN-UPD-KDFREKKL    TO CLAG-KDFREKKL                           
033600       MOVE IN-UPD-KDPRISKL    TO CLAG-KDPRISKL                           
033700       MOVE IN-UPD-KDLEVPLF    TO CLAG-KDLEVPLF                           
033800                                                                          
035910       MOVE 'YYWWD'            TO DAYS-KDDATFMT1                          
035920       MOVE 'YYMMDD'           TO DAYS-KDDATFMT2                          
035930       MOVE W-TIAAVVD-AKT      TO WS-DAYS-TIDATE1-AAVVD                   
035931****** DAGNR SÄTTS = 5                                                    
035940       ADD +5                  TO WS-DAYS-TIDATE1-AAVVD                   
035950       MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1                       
035960       MOVE 0                  TO DAYS-KVDAYS                             
035970       MOVE SPACE              TO DAYS-TIDATE2                            
035980                                   DAYS-IDCALEND                          
035990       CALL WZ20DAYS USING DAYS-WZ20DAYS                                  
035996                                                                          
035997       IF DAYS-KDRC = 8                                                   
035998         MOVE 'FEL VID ANROP TILL WZ20DAYS 1'                             
035999                                  TO FELTEXT-STR                          
036000         MOVE 32 TO RKOD                                                  
036001         CALL ABEND USING RKOD                                            
036002       ELSE                                                               
036003         MOVE CLAG-TISLUTKP     TO TMP1-YYMMDD                            
036004         MOVE DAYS-TIDATE2(1:6) TO TMP2-YYMMDD                            
036006         PERFORM WY2000P1                                                 
036007         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
036008           MOVE IN-UPD-KVSLUTKP TO CLAG-KVSLUTKP                          
036009         END-IF                                                           
036010       END-IF                                                             
036011                                                                          
036020******** --------  FIX FÖR ATT ÄNDRA PG-LEV=0 TILL 1 ---------            
036100       IF CLAG-IDPLANGR-LEV = ZERO                                        
036200          MOVE 1 TO CLAG-IDPLANGR-LEV                                     
036300       END-IF                                                             
036400******** -------------- SLUT FIX -----------------------------            
036500                                                                          
036600       MOVE IN-UPD-FLMPB        TO CLAG-FLMPB                             
036700       MOVE IN-UPD-KVMAD-SEP    TO CLAG-KVMAD-SEP                         
036800       MOVE IN-UPD-KVMAD-TOT    TO CLAG-KVMAD-TOT                         
036900       MOVE IN-UPD-KVMP         TO CLAG-KVMP                              
037000       MOVE IN-UPD-KVPB-VESL    TO CLAG-KVPB-VESL                         
037100       MOVE IN-UPD-RESLJUST     TO CLAG-RESLJUST                          
037200       MOVE IN-UPD-TISLJUST     TO CLAG-TISLJUST                          
037300       MOVE IN-UPD-KVPB-SEP     TO CLAG-KVPB-SEP                          
037400       MOVE IN-UPD-RVPROURS     TO CLAG-RVPROURS                          
037500       MOVE IN-UPD-RVPROFEL     TO CLAG-RVPROFEL                          
037600       MOVE IN-UPD-FLMANPB      TO CLAG-FLMANPB                           
037700       MOVE IN-UPD-KVPB-TPO     TO CLAG-KVPB-TPO                          
037800       MOVE IN-UPD-TILTK        TO CLAG-TILTK                             
037900       MOVE IN-UPD-KDLTK        TO CLAG-KDLTK                             
038000       MOVE IN-UPD-KVSLAGER     TO CLAG-KVSLAGER                          
038100       MOVE IN-UPD-DAPBPLAN     TO CLAG-DAPBPLAN                          
038200       MOVE IN-UPD-DASEASON     TO CLAG-DASEASON                          
038300       MOVE IN-UPD-KVULOAD      TO CLAG-KVULOAD                           
038400       MOVE IN-UPD-KVEOQ        TO CLAG-KVEOQ                             
038500       MOVE IN-UPD-KVSLAGER-OPT TO CLAG-KVSLAGER-OPT                      
038600       MOVE IN-UPD-RESEASON-PLAN (1) TO CLAG-RESEASON-PLAN (1)            
038700       MOVE IN-UPD-RESEASON-PLAN (2) TO CLAG-RESEASON-PLAN (2)            
038800       MOVE IN-UPD-RESEASON-PLAN (3) TO CLAG-RESEASON-PLAN (3)            
038900       MOVE IN-UPD-RESEASON-PLAN (4) TO CLAG-RESEASON-PLAN (4)            
039000       MOVE IN-UPD-RESEASON-PLAN (5) TO CLAG-RESEASON-PLAN (5)            
039100       MOVE IN-UPD-RESEASON-PLAN (6) TO CLAG-RESEASON-PLAN (6)            
039200       MOVE IN-UPD-RESEASON-PLAN (7) TO CLAG-RESEASON-PLAN (7)            
039300       MOVE IN-UPD-RESEASON-PLAN (8) TO CLAG-RESEASON-PLAN (8)            
039400       MOVE IN-UPD-RESEASON-PLAN (9) TO CLAG-RESEASON-PLAN (9)            
039500       MOVE IN-UPD-RESEASON-PLAN (10) TO CLAG-RESEASON-PLAN (10)          
039600       MOVE IN-UPD-RESEASON-PLAN (11) TO CLAG-RESEASON-PLAN (11)          
039700       MOVE IN-UPD-RESEASON-PLAN (12) TO CLAG-RESEASON-PLAN (12)          
039800                                                                          
039900       PERFORM IMS-REPL-WDK611                                            
040000     END-IF                                                               
040100                                                                          
040200     .                                                                    
040300     EJECT                                                                
040400 C-BORTTAG-KOPPLING-LP SECTION.                                           
040500     MOVE 'C-BORTTAG-KOPPLING-LP ' TO CURRENT-SECTION                     
040600*** SE T-BORTTAG-KOPPLING-LP  SECTION I PGM W2214010.                     
040700                                                                          
040800     MOVE IN-UPD-IDARTNR  TO W-IDARTNR-D9                                 
040900     MOVE IN-UPD-IDDC     TO W-IDDC-D9                                    
041000                                                                          
041200     PERFORM IMS-GHU-WDD901                                               
041300     IF SEGMENT-FINNS                                                     
041400       PERFORM IMS-DLET-WDD901                                            
041410     DISPLAY 'PERFORM IMS-DLET-WDD901 '                                   
041500     END-IF                                                               
041600                                                                          
041700     .                                                                    
041800     EJECT                                                                
041900 D-BORTTAG-OMSPEC SECTION.                                                
042000     MOVE 'D-BORTTAG-OMSPEC '  TO CURRENT-SECTION                         
042100*** SE J-BORTTAG-OMSPEC SECTION I PGM W2214010                            
042200                                                                          
042300     MOVE IN-UPD-IDARTNR  TO W-IDARTNR-D9                                 
042400     MOVE IN-UPD-IDDC     TO W-IDDC-D9                                    
042410     MOVE IN-UPD-IDLEVNR  TO W-IDLEVNR                                    
042420                                                                          
042900     PERFORM IMS-GHU-WDD904-OMSPEC                                        
043010                                                                          
043100     IF SEGMENT-FINNS                                                     
043300        PERFORM IMS-DLET-WDD904                                           
043400                                                                          
043500*--------- TAG ÄVEN BORT ARTIKELN FRÅN FÖRSLAGS-KÖN                       
043600        MOVE IN-UPD-IDDC    TO W-IDDC-MIN                                 
043700                               W-IDDC-MAX                                 
043800        MOVE IN-UPD-IDLEVNR TO W-IDLEVNR-MIN                              
043900                               W-IDLEVNR-MAX                              
044000        MOVE IN-UPD-IDARTNR TO W-IDARTNR-MIN                              
044100                               W-IDARTNR-MAX                              
044110                                                                          
044200        PERFORM IMS-GHU-WDD601                                            
044300        PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                      
044400     DISPLAY 'WDD601 DLET=' W-IDLEVNR ' ' W-IDARTNR-D9 ' '                
044410                            LPF-IDANSK                                    
044420           PERFORM IMS-DLET-WDD601                                        
044600           PERFORM IMS-GHN-WDD601                                         
044700        END-PERFORM                                                       
044800     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200 E-BORTTAG-AVROP SECTION.                                                 
045300     MOVE 'E-BORTTAG-AVROP '  TO CURRENT-SECTION                          
045400*** SE  R-BORTTAG-AVROP SECTION I PGM W2214010                            
045500                                                                          
045600     MOVE IN-UPD-IDARTNR  TO W-IDARTNR-D9                                 
045700     MOVE IN-UPD-IDDC     TO W-IDDC-D9                                    
045800     PERFORM IMS-GHU-WDD901                                               
045900                                                                          
046000     IF SEGMENT-FINNS                                                     
046200        MOVE IN-UPD-KDAVROP TO W-KDAVROP                                  
046410        PERFORM IMS-GHNP-WDD905-AVROP                                     
046530                                                                          
046600        PERFORM UNTIL SEGMENT-SAKNAS                                      
046700           PERFORM IMS-DLET-WDD905                                        
046910           PERFORM IMS-GHNP-WDD905-AVROP                                  
047000        END-PERFORM                                                       
047100     END-IF                                                               
047200                                                                          
047300     .                                                                    
047400     EJECT                                                                
047500 Z-FINIT SECTION.                                                         
047600                                                                          
047700                                                                          
047800     CLOSE W2214A                                                         
047900     SKIP2                                                                
048000     MOVE 'S' TO POSTSUM-OPKOD                                            
048100     CALL POSTSUM USING POSTSUM-PARM                                      
049100     .                                                                    
049200     EJECT                                                                
049300 S01-LAES-W2214A  SECTION.                                                
049400     SKIP2                                                                
049500     READ W2214A INTO IN-AREA                                             
049600     AT END                                                               
049700        MOVE HIGH-VALUE TO IN-AREA                                        
049800        SET END-OF-W2214A TO TRUE                                         
049900                                                                          
050000     NOT AT END                                                           
050100        MOVE 'W2214A'       TO POSTSUM-FDNAMN                             
050200        MOVE 'W2214AD1'     TO POSTSUM-DDNAMN2                            
050300        MOVE IN-UPD-IDPTYP  TO POSTSUM-TRANSTYP                           
050400        CALL POSTSUM USING POSTSUM-PARM                                   
050500                                                                          
050600        ADD 1 TO W-KVPOST-IN                                              
050700     END-READ                                                             
050800     .                                                                    
050900     EJECT                                                                
052900 X-TAG-CHECKPOINT   SECTION.                                              
053000                                                                          
053100* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
053200* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
053300                                                                          
054300     PERFORM IMS-CHECKPOINT                                               
054400     MOVE ZERO TO CHKP-ANT                                                
054500* --- LÄS OM DATABAS OM DET BEHÖVS                                        
054600     .                                                                    
054700     EJECT                                                                
054800* --- IMS SEKTIONER ---                                                   
054900                                                                          
055000     EJECT                                                                
056300 IMS-GHU-WDK611 SECTION.                                                  
056400     MOVE 'IMS-GHU-WDK611 '  TO DBS-SECTION                               
056500                                                                          
056600     MOVE SPACE               TO ALL-SSA                                  
056610     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
056620          DELIMITED BY SIZE INTO SSA1                                     
056630     MOVE 'WDK611   ' TO SSA2                                             
056900     MOVE '  ' TO GODK-STATUSKODER                                        
057000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
057100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     SKIP3                                                                
057500 IMS-REPL-WDK611 SECTION.                                                 
057600     MOVE 'IMS-REPL-WDK611 '  TO DBS-SECTION                              
057700                                                                          
057800     MOVE '  ' TO GODK-STATUSKODER                                        
057900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
058000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     ADD +3 TO CHKP-ANT                                                   
058300     .                                                                    
058400     EJECT                                                                
059700 IMS-GHU-WDD901 SECTION.                                                  
059800     MOVE 'IMS-GHU-WDD901 '  TO DBS-SECTION                               
059900                                                                          
060000     MOVE SPACE   TO ALL-SSA                                              
060100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
060200          DELIMITED BY SIZE INTO SSA1                                     
060300     MOVE '  GE' TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD901 SSA1                   
060500     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-DLET-WDD901 SECTION.                                                 
061000     MOVE ' IMS-DLET-WDD901 '  TO DBS-SECTION                             
061100                                                                          
061200     MOVE '  ' TO GODK-STATUSKODER                                        
061300     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD901                       
061400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
061500     PERFORM IMS-STATUSKONTROLL                                           
061510     ADD +1 TO CHKP-ANT                                                   
061600     .                                                                    
061700     EJECT                                                                
061710 IMS-GU-WDD902 SECTION.                                                   
061720     MOVE 'IMS-GU-WDD902 '  TO DBS-SECTION                                
061730                                                                          
061740     MOVE SPACE   TO ALL-SSA                                              
061750     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
061760          DELIMITED BY SIZE INTO SSA1                                     
061761     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
061762          DELIMITED BY SIZE INTO SSA2                                     
061770     MOVE '  GE' TO GODK-STATUSKODER                                      
061780     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
061790     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
061791     PERFORM IMS-STATUSKONTROLL                                           
061792     .                                                                    
061793     SKIP3                                                                
063010 IMS-GHU-WDD904-OMSPEC SECTION.                                           
063020     MOVE 'IMS-GHU-WDD904-OMSPEC '  TO DBS-SECTION                        
063030                                                                          
063040     MOVE SPACE   TO ALL-SSA                                              
063041     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
063042          DELIMITED BY SIZE INTO SSA1                                     
063050     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
063060            DELIMITED BY SIZE INTO SSA2                                   
063070     MOVE 'WDD904   ' TO SSA3                                             
063080     MOVE '  GE' TO GODK-STATUSKODER                                      
063090     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
063091     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
063092     PERFORM IMS-STATUSKONTROLL                                           
063093     .                                                                    
063094     SKIP3                                                                
063100 IMS-DLET-WDD904 SECTION.                                                 
063200     MOVE ' IMS-DLET-WDD904 '  TO DBS-SECTION                             
063300                                                                          
063400     MOVE '  ' TO GODK-STATUSKODER                                        
063500     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
063600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     ADD +1 TO CHKP-ANT                                                   
063900     .                                                                    
064000     EJECT                                                                
066810 IMS-GHNP-WDD905-AVROP  SECTION.                                          
066820     MOVE 'IMS-GHNP-WDD905-AVROP ' TO DBS-SECTION                         
066830                                                                          
066840     MOVE SPACE       TO ALL-SSA                                          
066860     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
066870            DELIMITED BY SIZE INTO SSA1                                   
066880     MOVE '  GE' TO GODK-STATUSKODER                                      
066890     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
066891     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
066892     PERFORM IMS-STATUSKONTROLL                                           
066893     .                                                                    
066894     EJECT                                                                
066900 IMS-DLET-WDD905 SECTION.                                                 
067000     MOVE ' IMS-DLET-WDD905 '  TO DBS-SECTION                             
067100                                                                          
067200     MOVE '  ' TO GODK-STATUSKODER                                        
067300     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
067400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     ADD +1 TO CHKP-ANT                                                   
067700     .                                                                    
067800     EJECT                                                                
067900 IMS-GHU-WDD601 SECTION.                                                  
068000     MOVE 'IMS-GHU-WDD601 '  TO DBS-SECTION                               
068100                                                                          
068200     MOVE SPACE   TO ALL-SSA                                              
068300     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
068400                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
068500          DELIMITED BY SIZE INTO SSA1                                     
068600     MOVE '  GE' TO GODK-STATUSKODER                                      
068700     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
068800     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100     EJECT                                                                
069200 IMS-GHN-WDD601 SECTION.                                                  
069300     MOVE 'IMS-GHN-WDD601 '  TO DBS-SECTION                               
069400                                                                          
069500     MOVE SPACE   TO ALL-SSA                                              
069600     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
069700                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
069800          DELIMITED BY SIZE INTO SSA1                                     
069900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
070000     CALL CBLTDLI USING GHN WDD6-PCB DLI-IO-WDD601 SSA1                   
070100     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400     EJECT                                                                
070500 IMS-DLET-WDD601 SECTION.                                                 
070600     MOVE ' IMS-DLET-WDD601 '  TO DBS-SECTION                             
070700                                                                          
070800     MOVE '  ' TO GODK-STATUSKODER                                        
070900     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
071000     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     ADD +1 TO CHKP-ANT                                                   
071300     .                                                                    
071400     EJECT                                                                
073700 IMS-RESTART SECTION.                                                     
073800     SKIP2                                                                
073900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
074000     MOVE '  ' TO GODK-STATUSKODER                                        
074100     CALL CBLTDLI USING XRST MSG-PCB                                      
074200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
074300                        CHKP-AREA-LENGTH CHKP-AREA                        
074400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     SKIP3                                                                
074800 IMS-CHECKPOINT SECTION.                                                  
074900     SKIP2                                                                
075000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
075100     MOVE '  XD' TO GODK-STATUSKODER                                      
075200     CALL CBLTDLI USING CHKP MSG-PCB                                      
075300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
075400                        CHKP-AREA-LENGTH CHKP-AREA                        
075500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075600     PERFORM IMS-STATUSKONTROLL                                           
075700                                                                          
075800     IF IMS-EJ-OK                                                         
075900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
076000       DISPLAY FELTEXT                                                    
076100       CALL FELLOG                                                        
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 IMS-STATUSKONTROLL SECTION.                                              
076600     SKIP2                                                                
076700     SET STATUS-IX TO 1                                                   
076800     SEARCH GODK-STATUS                                                   
076900       AT END                                                             
077000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077100           DELIMITED BY SIZE INTO FELTEXT                                 
077200         DISPLAY FELTEXT                                                  
077300         CALL FELLOG                                                      
077400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
077500         CONTINUE                                                         
077600     END-SEARCH                                                           
077700     .                                                                    
077800     EJECT                                                                
077900*    -COPY WY2000P1                                                       
