000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2614100.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   09/02/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER NEDLÄST ARTIKELREGISTER                                    
001000*        OCH PLOCKAR UT ARTIKLAR MED                                      
001100*        TIURPROD > ANTAL I ÅR I TABELL WDG2/H-TYP 1143/WDGX1144          
001200*                                                     (BILD 2153).        
001300*        FÖR WARNING LAST CALL                                            
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- ARTIKELINFORMATION                                         
002800     SELECT W01160                     ASSIGN TO W26141D1.                
002900     SKIP2                                                                
003000*          --- ARTIKLAR TIURPROD                                          
003100     SELECT W26143                     ASSIGN TO W26141D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W01160                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W01160      -L.                                                
004200     SKIP3                                                                
004300 FD  W26143                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W26143 -PRE  UT-  -L.                                     
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -COPY WY2000W3                                                       
005200     SKIP3                                                                
005300 77  IDPGM                       PIC X(8)    VALUE 'W2614100'.            
005400 77  IMS-SEKTION                 PIC X(40).                               
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W01160                       VALUE 'J'.                   
006000                                                                          
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700                                                                          
006800 01  W-TIAAAAVV                  PIC 9(6).                                
006900 01  FILLER  REDEFINES W-TIAAAAVV.                                        
007000     03  W-TIAAAA                PIC 9(4).                                
007100     03  W-TIVV                  PIC 9(2).                                
007200 01  FILLER  REDEFINES W-TIAAAAVV.                                        
007300     03  W-TISEKEL               PIC 9(2).                                
007400     03  W-TIAAVV                PIC 9(4).                                
007500                                                                          
007600 01  ARBETSAREOR.                                                         
007700     03  W-KVAARLF               PIC 9(02)   VALUE ZERO.                  
007800                                                                          
007900     03  WS-TIURPROD             PIC 9(6)    VALUE ZERO.                  
008000     03  FILLER  REDEFINES WS-TIURPROD.                                   
008100         05  WS-SEKEL            PIC 9(2).                                
008200         05  WS-TIURPROD-AAVV    PIC 9(4).                                
008300     03  FILLER  REDEFINES WS-TIURPROD.                                   
008400         05  FILLER              PIC 9(2).                                
008500         05  WS-TISKPREL         PIC 9(4).                                
008600     EJECT                                                                
008700 01  W-TIAAAAVV-AARLF            PIC 9(6).                                
008800 01  FILLER  REDEFINES W-TIAAAAVV-AARLF.                                  
008900     03  W-TISEKEL-AARLF         PIC 9(2).                                
009000     03  W-TIAAVV-AARLF          PIC 9(4).                                
009100                                                                          
009200 77  MAX-TAB-IX                  PIC S9(4) COMP SYNC VALUE +5000.         
009300 77  TAB-IX                      PIC S9(4) COMP SYNC.                     
009400 01  WDGX1144-TAB.                                                        
009500     03  TAB-WDGX1144 OCCURS 5000.                                        
009600         05 TAB-IDFKNGRP-FOM     PIC 9(04).                               
009700         05 TAB-IDFKNGRP-TOM     PIC 9(04).                               
009800         05 TAB-KVAARLF          PIC 9(02).                               
009900                                                                          
010000*01  -COPY WWPRODSL                                                       
010100                                                                          
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900     SKIP2                                                                
011000*    --- PARAMETRAR TILL ABEND                                            
011100                                                                          
011200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011500     EJECT                                                                
011600*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
011700                                                                          
011800*01  -COPY WDATAREA                                                       
011900     SKIP2                                                                
012000 01  FELTEXT.                                                             
012100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012300                                                                          
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL POSTSUM                                          
012600*                                                                         
012700*01  -COPY W0005   -PRE  POSTSUM-                                         
012800     EJECT                                                                
012900                                                                          
013000 01  IN-AREA-START               PIC X(24)   VALUE                        
013100                                 'IN-AREA-START  '.                       
013200     SKIP2                                                                
013300                                                                          
013400*01  AREA -COPY W01160     -PRE IN-                                       
013500     EJECT                                                                
013600 01  UT-AREA-START               PIC X(24)   VALUE                        
013700                                 'UT-AREA-START  '.                       
013800     SKIP2                                                                
013900                                                                          
014000*01  AREA -COPY W26143     -PRE UT-                                       
014100     EJECT                                                                
014200                                                                          
014300 01  ERROR-TEXT.                                                          
014400     03  FILLER                  PIC X(08)   VALUE 'ERR-TEXT'.            
014500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
014600     EJECT                                                                
014700                                                                          
014800*    --- AREAS FOR IMS-SECTIONS                                           
014900*                                                                         
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015200     SKIP3                                                                
015300 01  KEYS-FOR-DLI.                                                        
015400     03  W-WDGXKEY-1143-X.                                                
015500         05  W-IDHTYP-1143       PIC X(4)    VALUE '1143'.                
015600         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
015700                                                                          
015800*    --- STATUS-KOD FRÅN IMS                                              
015900 01  STATUS-WS                   PIC XX.                                  
016000     88  SEGMENT-FOUND                       VALUE '  '.                  
016100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016300     SKIP2                                                                
016400 01  GOOD-STATUSCODES.                                                    
016500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016600     SKIP3                                                                
016700 01  SSA1                        PIC X(64).                               
016800 01  SSA2                        PIC X(64).                               
016900     EJECT                                                                
017000*    --- IMS FUNCTION CODES                                               
017100*01  -COPY W0003                                                          
017200     EJECT                                                                
017300*    ---  DLI INPUT-OUTPUT AREA                                           
017400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
017500 01  DLI-IO-WDGX01.                                                       
017600*    03  -COPY WDGX01                                                     
017700                                                                          
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1144'.                    
017900 01  DLI-IO-WDGX1144.                                                     
018000*    03  -COPY WDGX1144                                                   
018100                                                                          
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0008   -PRE WDG2-                                             
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700 PROCEDURE DIVISION USING  WDG2-PCB.                                      
018800                                                                          
018900     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
019000 MAIN SECTION.                                                            
019100                                                                          
019200     PERFORM A-INIT                                                       
019300                                                                          
019400     PERFORM B-WDGX1144-TAB                                               
019500                                                                          
019600     PERFORM S01-LAES-W01160                                              
019700     PERFORM UNTIL END-OF-W01160                                          
019800                                                                          
019900       IF IN-CLAG-TIURPROD > ZERO                                         
020000          PERFORM C-SKAPA-EV-UTPOST                                       
020100       END-IF                                                             
020200                                                                          
020300       PERFORM S01-LAES-W01160                                            
020400     END-PERFORM                                                          
020500                                                                          
020600                                                                          
020700     PERFORM Z-FINIT                                                      
020800                                                                          
020900     MOVE ZERO TO RETURN-CODE                                             
021000     GOBACK                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 A-INIT SECTION.                                                          
021400                                                                          
021500     OPEN INPUT  W01160                                                   
021600                                                                          
021700     OPEN OUTPUT W26143                                                   
021800     SKIP2                                                                
021900     ACCEPT DAGENS-DATUM  FROM DATE                                       
022000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022100                                                                          
022200     MOVE 'IDAG' TO DAT-KDDATFORM                                         
022300     CALL WDATKONV USING DAT-KDDATFORM,                                   
022400                         DAT-I-TIDATUM,                                   
022500                         DAT-O-TIDATUM,                                   
022600                         DAT-KDSVAR                                       
022700                                                                          
022800     IF DAT-KDSVAR-FEL                                                    
022900        DISPLAY '****  FEL I WDATKONV  *******'                           
023000        PERFORM S99-ABEND                                                 
023100     END-IF                                                               
023200                                                                          
023300     MOVE DAT-TIAAVV-GRP TO W-TIAAVV                                      
023400     MOVE DAT-TISEKEL    TO W-TISEKEL                                     
023500     .                                                                    
023600     EJECT                                                                
023700 B-WDGX1144-TAB SECTION.                                                  
023800                                                                          
023900     MOVE +0                        TO TAB-IX                             
024000     PERFORM IMS-GU-WDGX1143                                              
024100                                                                          
024200     PERFORM IMS-GNP-WDGX1144                                             
024300     PERFORM UNTIL SEGMENT-MISSING                                        
024400        ADD +1                      TO TAB-IX                             
024500        MOVE 1144-IDFKNGRP-FOM      TO TAB-IDFKNGRP-FOM (TAB-IX)          
024600        MOVE 1144-IDFKNGRP-TOM      TO TAB-IDFKNGRP-TOM (TAB-IX)          
024700        MOVE 1144-KVAARLF           TO TAB-KVAARLF      (TAB-IX)          
024800                                                                          
024900        PERFORM IMS-GNP-WDGX1144                                          
025000                                                                          
025100        IF TAB-IX = MAX-TAB-IX                                            
025200           DISPLAY 'TABELL FÖR DB WDGX1144 > 5000 RECORDS.'               
025300           DISPLAY 'ÖKA ANTAL I TAB-WDGX1144 I WORKING STORAGE.'          
025400           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
025500        END-IF                                                            
025600                                                                          
025700     END-PERFORM                                                          
025800     MOVE TAB-IX                    TO MAX-TAB-IX                         
025900     .                                                                    
026000     EJECT                                                                
026100 C-SKAPA-EV-UTPOST SECTION.                                               
026200                                                                          
026300     MOVE IN-CLAG-KDPRODSL          TO TEST-KDPRODSL                      
026400     IF KDPRODSL-VOLVO-PARTS  OR                                          
026500        KDPRODSL-BYTES        OR                                          
026600        KDPRODSL-ACC          OR                                          
026700        KDPRODSL-BRANDON      OR                                          
026800        KDPRODSL-VOLVO-WHEELS OR                                          
026900        KDPRODSL-SERVICES                                                 
027000        PERFORM CA-SOEK-WDGX1144-IDFKNGRP                                 
027100                                                                          
027200        MOVE IN-CLAG-TIURPROD       TO TMP1-YYWW                          
027300        COMPUTE W-TIAAAAVV-AARLF =                                        
027400                W-TIAAAAVV - ((W-KVAARLF * 100) - 200)                    
027500        MOVE W-TIAAVV-AARLF         TO TMP2-YYWW                          
027600                                                                          
027700        PERFORM WY2000P3                                                  
027800                                                                          
027900        IF TMP1-YYWW < TMP2-YYWW                                          
028000           PERFORM CB-SKAPA-UTFIL                                         
028100        END-IF                                                            
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 CA-SOEK-WDGX1144-IDFKNGRP SECTION.                                       
028600                                                                          
028700     MOVE 15                        TO W-KVAARLF                          
028800                                                                          
028900     MOVE +1                        TO TAB-IX                             
029000     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
029100       IF IN-CLAG-IDFKNGRP >= TAB-IDFKNGRP-FOM (TAB-IX)                   
029200      AND IN-CLAG-IDFKNGRP <= TAB-IDFKNGRP-TOM (TAB-IX)                   
029300          MOVE TAB-KVAARLF(TAB-IX)  TO W-KVAARLF                          
029400          MOVE 5000                 TO TAB-IX                             
029500       END-IF                                                             
029600       ADD +1                       TO TAB-IX                             
029700     END-PERFORM                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 CB-SKAPA-UTFIL    SECTION.                                               
030100                                                                          
030200     MOVE IN-CLAG-IDANSK      TO UT-IDANSK                                
030300     MOVE IN-CLAG-IDARTNR     TO UT-IDARTNR                               
030400     MOVE IN-CLAG-IDLEVNR     TO UT-IDLEVNR                               
030500     MOVE IN-CLAG-IDFKNGRP    TO UT-IDFKNGRP                              
030600     MOVE IN-CLAG-KDPRODSL    TO UT-KDPRODSL                              
030700     MOVE IN-CLAG-TIURPROD    TO UT-TIURPROD                              
030800     MOVE IN-CLAG-KVLS        TO UT-KVLS                                  
030900     MOVE IN-CLAG-PRARTSTD    TO UT-PRARTSTD                              
031000     MOVE IN-CLAG-KDERS       TO UT-KDERS                                 
031100     IF UT-KDERS = ZERO                                                   
031200        MOVE IN-CLAG-KDERS-UTG TO UT-KDERS                                
031300     END-IF                                                               
031400                                                                          
031500     MOVE IN-CLAG-TIURPROD     TO WS-TIURPROD-AAVV                        
031600     IF IN-CLAG-TIURPROD > 5000                                           
031700        MOVE 19                TO WS-SEKEL                                
031800     ELSE                                                                 
031900        MOVE 20                TO WS-SEKEL                                
032000     END-IF                                                               
032100                                                                          
032200     COMPUTE WS-TIURPROD = IN-CLAG-TIURPROD + (W-KVAARLF * 100)           
032300     MOVE WS-TISKPREL          TO UT-TISKPREL                             
032400                                                                          
032500* FÄLT FLFKNPRI OCH FILLER15 ANVÄNDS INTE I RUTIN W261B5.                 
032600* FÄLTEN ANVÄNDS I RUTIN W261B3.                                          
032700     MOVE SPACE               TO UT-FLFKNPRI                              
032800     MOVE SPACE               TO UT-FLAGGA15                              
032900                                                                          
033000     IF UT-KDERS = ZERO OR 09                                             
033100        PERFORM S11-SKRIV-W26143                                          
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 Z-FINIT SECTION.                                                         
033600     CLOSE W01160                                                         
033700           W26143                                                         
033800     SKIP2                                                                
033900     MOVE 'S' TO POSTSUM-OPKOD                                            
034000     CALL POSTSUM USING POSTSUM-PARM                                      
034100     .                                                                    
034200     EJECT                                                                
034300 S01-LAES-W01160  SECTION.                                                
034400     READ W01160 INTO IN-AREA                                             
034500     AT END                                                               
034600        SET END-OF-W01160 TO TRUE                                         
034700                                                                          
034800     NOT AT END                                                           
034900        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
035000        MOVE 'W26141D2' TO POSTSUM-DDNAMN2                                
035100        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
035200        CALL POSTSUM USING POSTSUM-PARM                                   
035300     END-READ                                                             
035400     .                                                                    
035500     EJECT                                                                
035600 S11-SKRIV-W26143 SECTION.                                                
035700                                                                          
035800     WRITE UT-POST FROM UT-AREA                                           
035900                                                                          
036000     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
036100     MOVE 'W26143'   TO POSTSUM-FDNAMN                                    
036200     MOVE 'W26141D3' TO POSTSUM-DDNAMN2                                   
036300     CALL POSTSUM USING POSTSUM-PARM                                      
036400     .                                                                    
036500     EJECT                                                                
036600 S99-ABEND SECTION.                                                       
036700                                                                          
036800     SKIP2                                                                
036900     MOVE 'S' TO POSTSUM-OPKOD                                            
037000     CALL POSTSUM USING POSTSUM-PARM                                      
037100     CALL ABEND USING RKOD-ABEND                                          
037200     .                                                                    
037300     EJECT                                                                
037400 IMS-GU-WDGX1143 SECTION.                                                 
037500     MOVE 'IMS-GU-WDGX1143              ' TO IMS-SEKTION                  
037600                                                                          
037700     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
037800            DELIMITED BY SIZE INTO SSA1                                   
037900     MOVE '    '                TO GOOD-STATUSCODES                       
038000     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX01 SSA1                    
038100     MOVE WDG2-STATUS-CODE      TO STATUS-WS                              
038200     PERFORM IMS-STATUSCHECK                                              
038300     .                                                                    
038400                                                                          
038500 IMS-GNP-WDGX1144  SECTION.                                               
038600     MOVE 'IMS-GNP-WDGX1144             ' TO IMS-SEKTION                  
038700                                                                          
038800     MOVE 'WDGX1144 '          TO SSA1                                    
038900     MOVE '  GE'               TO GOOD-STATUSCODES                        
039000     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
039100     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
039200     PERFORM IMS-STATUSCHECK                                              
039300     .                                                                    
039400                                                                          
039500 IMS-STATUSCHECK SECTION.                                                 
039600                                                                          
039700     SET STATUS-IX TO 1                                                   
039800     SEARCH GOOD-STATUS                                                   
039900       AT END                                                             
040000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040100           DELIMITED BY SIZE INTO ERROR-TEXT                              
040200         DISPLAY ERROR-TEXT                                               
040300         CALL FELLOG                                                      
040400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
040500         CONTINUE                                                         
040600     END-SEARCH                                                           
040700     .                                                                    
040800     EJECT                                                                
040900*    -COPY WY2000P3                                                       
