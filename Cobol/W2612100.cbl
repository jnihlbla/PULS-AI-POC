000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2612100.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   11/10/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                     (KOPIA W26161)                                      
000800*    FUNKTION:                                                            
000900*        LÄSER NEDLÄST ARTIKELREGISTER                                    
001000*        OCH PLOCKAR UT ARTIKLAR MED                                      
001100*        TIURPROD > ANTAL I ÅR I TABELL WDG2/H-TYP 1143/WDGX1144          
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ARTIKELINFORMATION                                         
002700     SELECT W01160                     ASSIGN TO W26121D1.                
002800     SKIP2                                                                
002900*          --- ARTIKLAR TIURPROD                                          
003000     SELECT W26122                     ASSIGN TO W26121D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W01160                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W01160      -L.                                                
004100     SKIP3                                                                
004200 FD  W26122                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W26122 -PRE  UT-  -L.                                     
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -COPY WY2000W3                                                       
005100     SKIP3                                                                
005200 77  IDPGM                       PIC X(8)    VALUE 'W2612100'.            
005300 77  IMS-SEKTION                 PIC X(8)    VALUE SPACE.                 
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W01160                       VALUE 'J'.                   
005900                                                                          
006000     EJECT                                                                
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600 01  DAGENS-DATUM-AAVV           PIC 9(4)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM-AAVV.                                  
006800     03  DAGENS-DATUM-AA         PIC 9(2).                                
006900     03  DAGENS-DATUM-VV         PIC 9(2).                                
007000                                                                          
007100 01  W-TIAAAAVV                  PIC 9(6).                                
007200 01  FILLER  REDEFINES W-TIAAAAVV.                                        
007300     03  W-TIAAAA                PIC 9(4).                                
007400     03  W-TIVV                  PIC 9(2).                                
007500 01  FILLER  REDEFINES W-TIAAAAVV.                                        
007600     03  W-TISEKEL               PIC 9(2).                                
007700     03  W-TIAAVV                PIC 9(4).                                
007800                                                                          
007900 01  W-TIAAAAVV-AARLF            PIC 9(6).                                
008000 01  FILLER  REDEFINES W-TIAAAAVV-AARLF.                                  
008100     03  W-TISEKEL-AARLF         PIC 9(2).                                
008200     03  W-TIAAVV-AARLF          PIC 9(4).                                
008300                                                                          
008400 01  ARBETSAREOR.                                                         
008500     03  MAX-FKNGRP              PIC S9(5)   COMP-3.                      
008600     03  IX-FKNGRP               PIC S9(5)   COMP-3.                      
008700     03  IX                      PIC S9(5)   COMP-3.                      
008800     03  WS-ANT-VV-JUST          PIC S9(3)   COMP-3  VALUE 5.             
008900     03  WS-TIAAVV               PIC S9(5)   COMP-3  VALUE ZERO.          
009000     03  W-KVAARLF               PIC 9(02)   VALUE ZERO.                  
009100     EJECT                                                                
009200                                                                          
009300 77  MAX-TAB-IX                  PIC S9(4) COMP SYNC VALUE +5000.         
009400 77  TAB-IX                      PIC S9(4) COMP SYNC.                     
009500 01  WDGX1144-TAB.                                                        
009600     03  TAB-WDGX1144 OCCURS 5000.                                        
009700         05 TAB-IDFKNGRP-FOM     PIC 9(04).                               
009800         05 TAB-IDFKNGRP-TOM     PIC 9(04).                               
009900         05 TAB-KVAARLF          PIC 9(02).                               
010000                                                                          
010100*01  -COPY WWPRODSL                                                       
010200                                                                          
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400*                                                                         
010500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010800     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011200     EJECT                                                                
011300*--------------------------------------- PARAMETRAR TILL DATKORT          
011400                                                                          
011500 01  PROGRAM-NAMN            PIC X(8)    VALUE 'W26121'.                  
011600                                                                          
011700 01  DATUMKORT-ID            PIC X(8)    VALUE 'WDATUM'.                  
011800                                                                          
011900*01  -COPY WDATKORT                                                       
012000     SKIP2                                                                
012100*    --- PARAMETRAR TILL ABEND                                            
012200                                                                          
012300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012600     EJECT                                                                
012700*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
012800                                                                          
012900*01  -COPY WDATAREA                                                       
013000     SKIP2                                                                
013100 01  FELTEXT.                                                             
013200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013400                                                                          
013500 01  TABELL.                                                              
013600     03  FILLER OCCURS 3000.                                              
013700         05  TAB-IDFKNGRP        PIC S9(5)   COMP-3.                      
013800         05  TAB-FLFKNPRI        PIC X.                                   
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005   -PRE  POSTSUM-                                         
014300     EJECT                                                                
014400 01  REG-AREA-START              PIC X(24)   VALUE                        
014500                                 'REG-AREA-START  '.                      
014600     SKIP2                                                                
014700                                                                          
014800 01  IN-AREA-START               PIC X(24)   VALUE                        
014900                                 'IN-AREA-START  '.                       
015000     SKIP2                                                                
015100                                                                          
015200*01  AREA -COPY W01160     -PRE IN-                                       
015300     EJECT                                                                
015400 01  UT-AREA-START               PIC X(24)   VALUE                        
015500                                 'UT-AREA-START  '.                       
015600     SKIP2                                                                
015700                                                                          
015800*01  AREA -COPY W26122     -PRE UT-                                       
015900     EJECT                                                                
016000                                                                          
016100 01  ERROR-TEXT.                                                          
016200     03  FILLER                  PIC X(08)   VALUE 'ERR-TEXT'.            
016300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
016400     EJECT                                                                
016500*    --- AREAS FOR IMS-SECTIONS                                           
016600*                                                                         
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016900     SKIP3                                                                
017000 01  KEYS-FOR-DLI.                                                        
017100     03  W-WDGXKEY-1143-X.                                                
017200         05  W-IDHTYP-1143       PIC X(4)    VALUE '1143'.                
017300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
017400                                                                          
017500*    --- STATUS-KOD FRÅN IMS                                              
017600 01  STATUS-WS                   PIC XX.                                  
017700     88  SEGMENT-FOUND                       VALUE '  '.                  
017800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018000     SKIP2                                                                
018100 01  GOOD-STATUSCODES.                                                    
018200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018300     SKIP3                                                                
018400 01  SSA1                        PIC X(64).                               
018500 01  SSA2                        PIC X(64).                               
018600     EJECT                                                                
018700*    --- IMS FUNCTION CODES                                               
018800*01  -COPY W0003                                                          
018900     EJECT                                                                
019000*    ---  DLI INPUT-OUTPUT AREA                                           
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
019200 01  DLI-IO-WDGX01.                                                       
019300*    03  -COPY WDGX01                                                     
019400                                                                          
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1144'.                    
019600 01  DLI-IO-WDGX1144.                                                     
019700*    03  -COPY WDGX1144                                                   
019800                                                                          
019900 LINKAGE SECTION.                                                         
020000                                                                          
020100*01  -COPY W0008   -PRE WDG2-                                             
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400 PROCEDURE DIVISION USING  WDG2-PCB.                                      
020500                                                                          
020600     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
020700 MAIN SECTION.                                                            
020800     SKIP2                                                                
020900                                                                          
021000     PERFORM A-INIT                                                       
021100                                                                          
021200     PERFORM B-WDGX1144-TAB                                               
021300                                                                          
021400     PERFORM S01-LAES-W01160                                              
021500     PERFORM UNTIL END-OF-W01160                                          
021600       IF IN-CLAG-TIURPROD > ZERO                                         
021700          PERFORM C-SKAPA-EV-UTPOST                                       
021800       END-IF                                                             
021900                                                                          
022000       PERFORM S01-LAES-W01160                                            
022100     END-PERFORM                                                          
022200                                                                          
022300                                                                          
022400     PERFORM Z-FINIT                                                      
022500                                                                          
022600     MOVE ZERO TO RETURN-CODE                                             
022700     GOBACK                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100                                                                          
023200     OPEN INPUT  W01160                                                   
023300                                                                          
023400     OPEN OUTPUT W26122                                                   
023500     SKIP2                                                                
023600     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
023700     MOVE ZERO              TO IX-FKNGRP                                  
023800                                                                          
023900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024000                                                                          
024100     COMPUTE DAGENS-DATUM-AAVV = D-AAR * 100 + D-VECKA                    
024200                                                                          
024300     MOVE DAGENS-DATUM-AAVV TO W-TIAAVV                                   
024400     MOVE 20                TO W-TISEKEL                                  
024500*    JUSTERA MED YTTERLIGARE 5 VECKOR FRAMÅT                              
024600     MOVE W-TIAAVV          TO WS-TIAAVV                                  
024700     CALL W009VADD USING WS-TIAAVV WS-ANT-VV-JUST                         
024800     MOVE WS-TIAAVV         TO W-TIAAVV                                   
024900     .                                                                    
025000     EJECT                                                                
025100 B-WDGX1144-TAB SECTION.                                                  
025200                                                                          
025300     MOVE +0                        TO TAB-IX                             
025400     PERFORM IMS-GU-WDGX1143                                              
025500                                                                          
025600     PERFORM IMS-GNP-WDGX1144                                             
025700     PERFORM UNTIL SEGMENT-MISSING                                        
025800        ADD +1                      TO TAB-IX                             
025900        MOVE 1144-IDFKNGRP-FOM      TO TAB-IDFKNGRP-FOM (TAB-IX)          
026000        MOVE 1144-IDFKNGRP-TOM      TO TAB-IDFKNGRP-TOM (TAB-IX)          
026100        MOVE 1144-KVAARLF           TO TAB-KVAARLF      (TAB-IX)          
026200                                                                          
026300        PERFORM IMS-GNP-WDGX1144                                          
026400                                                                          
026500        IF TAB-IX = MAX-TAB-IX                                            
026600           DISPLAY 'TABELL FÖR DB WDGX1144 > 5000 RECORDS.'               
026700           DISPLAY 'ÖKA ANTAL I TAB-WDGX1144 I WORKING STORAGE.'          
026800           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
026900        END-IF                                                            
027000                                                                          
027100     END-PERFORM                                                          
027200     MOVE TAB-IX                    TO MAX-TAB-IX                         
027300     .                                                                    
027400     EJECT                                                                
027500 C-SKAPA-EV-UTPOST SECTION.                                               
027600                                                                          
027700     MOVE IN-CLAG-KDPRODSL     TO TEST-KDPRODSL                           
027800     IF KDPRODSL-VOLVO-PARTS  OR                                          
027900        KDPRODSL-BYTES        OR                                          
028000        KDPRODSL-ACC          OR                                          
028100        KDPRODSL-BRANDON      OR                                          
028200        KDPRODSL-VOLVO-WHEELS OR                                          
028300        KDPRODSL-SERVICES                                                 
028400        PERFORM CB-SOEK-WDGX1144-IDFKNGRP                                 
028500                                                                          
028600        MOVE IN-CLAG-TIURPROD      TO TMP1-YYWW                           
028700        COMPUTE W-TIAAAAVV-AARLF = W-TIAAAAVV - (W-KVAARLF * 100)         
028800        MOVE W-TIAAVV-AARLF        TO TMP2-YYWW                           
028900                                                                          
029000        PERFORM WY2000P3                                                  
029100                                                                          
029200        IF TMP1-YYWW = TMP2-YYWW                                          
029300           PERFORM CA-SKAPA-UTFIL                                         
029400        END-IF                                                            
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 CA-SKAPA-UTFIL    SECTION.                                               
029900                                                                          
030000     MOVE IN-CLAG-IDANSK      TO UT-IDANSK                                
030100     MOVE IN-CLAG-IDARTNR     TO UT-IDARTNR                               
030200     MOVE IN-CLAG-IDLEVNR     TO UT-IDLEVNR                               
030300     MOVE IN-CLAG-IDFKNGRP    TO UT-IDFKNGRP                              
030400     MOVE IN-CLAG-KDPRODSL    TO UT-KDPRODSL                              
030500     MOVE IN-CLAG-TIFINLV     TO UT-TIFINLV                               
030600     MOVE IN-CLAG-TIURPROD    TO UT-TIURPROD                              
030700     MOVE IN-CLAG-KVLS        TO UT-KVLS                                  
030800     MOVE IN-CLAG-PRARTSTD    TO UT-PRARTSTD                              
030900     MOVE IN-CLAG-KDERS       TO UT-KDERS                                 
031000     IF UT-KDERS = ZERO                                                   
031100       MOVE IN-CLAG-KDERS-UTG TO UT-KDERS                                 
031200     END-IF                                                               
031300                                                                          
031400     IF UT-KDERS < 20                                                     
031500        PERFORM S11-SKRIV-W26122                                          
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 CB-SOEK-WDGX1144-IDFKNGRP SECTION.                                       
032000                                                                          
032100     MOVE 15                       TO W-KVAARLF                           
032200     MOVE +1                       TO TAB-IX                              
032300     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
032400       IF IN-CLAG-IDFKNGRP >= TAB-IDFKNGRP-FOM (TAB-IX)                   
032500      AND IN-CLAG-IDFKNGRP <= TAB-IDFKNGRP-TOM (TAB-IX)                   
032600          MOVE TAB-KVAARLF(TAB-IX) TO W-KVAARLF                           
032700          MOVE +5000               TO TAB-IX                              
032800       END-IF                                                             
032900       ADD +1                      TO TAB-IX                              
033000     END-PERFORM                                                          
033100     .                                                                    
033200     EJECT                                                                
033300 Z-FINIT SECTION.                                                         
033400     CLOSE W01160                                                         
033500           W26122                                                         
033600     SKIP2                                                                
033700     MOVE 'S' TO POSTSUM-OPKOD                                            
033800     CALL POSTSUM USING POSTSUM-PARM                                      
033900     .                                                                    
034000     EJECT                                                                
034100 S01-LAES-W01160  SECTION.                                                
034200     READ W01160 INTO IN-AREA                                             
034300     AT END                                                               
034400        SET END-OF-W01160 TO TRUE                                         
034500                                                                          
034600     NOT AT END                                                           
034700        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
034800        MOVE 'W26121D1' TO POSTSUM-DDNAMN2                                
034900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
035000        CALL POSTSUM USING POSTSUM-PARM                                   
035100     END-READ                                                             
035200     .                                                                    
035300     EJECT                                                                
035400 S11-SKRIV-W26122 SECTION.                                                
035500                                                                          
035600     WRITE UT-POST FROM UT-AREA                                           
035700                                                                          
035800     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
035900     MOVE 'W26122'   TO POSTSUM-FDNAMN                                    
036000     MOVE 'W26121D2' TO POSTSUM-DDNAMN2                                   
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     .                                                                    
036300     EJECT                                                                
036400 S99-ABEND SECTION.                                                       
036500                                                                          
036600     SKIP2                                                                
036700     MOVE 'S' TO POSTSUM-OPKOD                                            
036800     CALL POSTSUM USING POSTSUM-PARM                                      
036900     CALL ABEND USING RKOD-ABEND                                          
037000     .                                                                    
037100     EJECT                                                                
037200 IMS-GU-WDGX1143 SECTION.                                                 
037300     MOVE 'IMS-GU-WDGX1143              ' TO IMS-SEKTION                  
037400                                                                          
037500     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
037600            DELIMITED BY SIZE INTO SSA1                                   
037700     MOVE '    '                TO GOOD-STATUSCODES                       
037800     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX01 SSA1                    
037900     MOVE WDG2-STATUS-CODE      TO STATUS-WS                              
038000     PERFORM IMS-STATUSCHECK                                              
038100     .                                                                    
038200                                                                          
038300 IMS-GNP-WDGX1144  SECTION.                                               
038400     MOVE 'IMS-GNP-WDGX1144             ' TO IMS-SEKTION                  
038500                                                                          
038600     MOVE 'WDGX1144 '          TO SSA1                                    
038700     MOVE '  GE'               TO GOOD-STATUSCODES                        
038800     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
038900     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
039000     PERFORM IMS-STATUSCHECK                                              
039100     .                                                                    
039200                                                                          
039300 IMS-STATUSCHECK SECTION.                                                 
039400                                                                          
039500     SET STATUS-IX TO 1                                                   
039600     SEARCH GOOD-STATUS                                                   
039700       AT END                                                             
039800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039900           DELIMITED BY SIZE INTO ERROR-TEXT                              
040000         DISPLAY ERROR-TEXT                                               
040100         CALL FELLOG                                                      
040200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
040300         CONTINUE                                                         
040400     END-SEARCH                                                           
040500     .                                                                    
040600     EJECT                                                                
040700*    -COPY WY2000P3                                                       
