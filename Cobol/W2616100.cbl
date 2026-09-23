000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2616100.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   98/04/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
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
002700     SELECT W01160                     ASSIGN TO W26161D1.                
002800     SKIP2                                                                
002900*          --- ARTIKLAR TIURPROD                                          
003000     SELECT W26162                     ASSIGN TO W26161D2.                
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
004200 FD  W26162                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W26163 -PRE  UT-  -L.                                     
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -COPY WY2000W3                                                       
005100     SKIP3                                                                
005200 77  IDPGM                       PIC X(8)    VALUE 'W2616100'.            
005300 77  IMS-SEKTION                 PIC X(40).                               
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
006600                                                                          
006700 01  W-TIAAAAVV                  PIC 9(6).                                
006800 01  FILLER  REDEFINES W-TIAAAAVV.                                        
006900     03  W-TIAAAA                PIC 9(4).                                
007000     03  W-TIVV                  PIC 9(2).                                
007100 01  FILLER  REDEFINES W-TIAAAAVV.                                        
007200     03  W-TISEKEL               PIC 9(2).                                
007300     03  W-TIAAVV                PIC 9(4).                                
007400                                                                          
007500 01  W-TIAAAAVV-AARLF            PIC 9(6).                                
007600 01  FILLER  REDEFINES W-TIAAAAVV-AARLF.                                  
007700     03  W-TISEKEL-AARLF         PIC 9(2).                                
007800     03  W-TIAAVV-AARLF          PIC 9(4).                                
007900                                                                          
008000 01  ARBETSAREOR.                                                         
008100     03  W-KVAARLF               PIC 9(02)   VALUE ZERO.                  
008200                                                                          
008300 77  MAX-TAB-IX                  PIC S9(4) COMP SYNC VALUE +5000.         
008400 77  TAB-IX                      PIC S9(4) COMP SYNC.                     
008500 01  WDGX1144-TAB.                                                        
008600     03  TAB-WDGX1144 OCCURS 5000.                                        
008700         05 TAB-IDFKNGRP-FOM     PIC 9(04).                               
008800         05 TAB-IDFKNGRP-TOM     PIC 9(04).                               
008900         05 TAB-KVAARLF          PIC 9(02).                               
009000                                                                          
009100     EJECT                                                                
009200                                                                          
009300*01  -COPY WWPRODSL                                                       
009400                                                                          
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     SKIP2                                                                
010300*    --- PARAMETRAR TILL ABEND                                            
010400                                                                          
010500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010800     EJECT                                                                
010900*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
011000                                                                          
011100*01  -COPY WDATAREA                                                       
011200     SKIP2                                                                
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600                                                                          
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL POSTSUM                                          
011900*                                                                         
012000*01  -COPY W0005   -PRE  POSTSUM-                                         
012100     EJECT                                                                
012200 01  REG-AREA-START              PIC X(24)   VALUE                        
012300                                 'REG-AREA-START  '.                      
012400     SKIP2                                                                
012500                                                                          
012600 01  IN-AREA-START               PIC X(24)   VALUE                        
012700                                 'IN-AREA-START  '.                       
012800     SKIP2                                                                
012900                                                                          
013000*01  AREA -COPY W01160     -PRE IN-                                       
013100     EJECT                                                                
013200 01  UT-AREA-START               PIC X(24)   VALUE                        
013300                                 'UT-AREA-START  '.                       
013400     SKIP2                                                                
013500                                                                          
013600*01  AREA -COPY W26163     -PRE UT-                                       
013700     EJECT                                                                
013800 01  ERROR-TEXT.                                                          
013900     03  FILLER                  PIC X(08)   VALUE 'ERR-TEXT'.            
014000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
014100     EJECT                                                                
014200                                                                          
014300*    --- AREAS FOR IMS-SECTIONS                                           
014400*                                                                         
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  KEYS-FOR-DLI.                                                        
014900     03  W-WDGXKEY-1143-X.                                                
015000         05  W-IDHTYP-1143       PIC X(4)    VALUE '1143'.                
015100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
015200                                                                          
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FOUND                       VALUE '  '.                  
015600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015800     SKIP2                                                                
015900 01  GOOD-STATUSCODES.                                                    
016000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     SKIP3                                                                
016200 01  SSA1                        PIC X(64).                               
016300 01  SSA2                        PIC X(64).                               
016400     EJECT                                                                
016500*    --- IMS FUNCTION CODES                                               
016600*01  -COPY W0003                                                          
016700     EJECT                                                                
016800*    ---  DLI INPUT-OUTPUT AREA                                           
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
017000 01  DLI-IO-WDGX01.                                                       
017100*    03  -COPY WDGX01                                                     
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1144'.                    
017400 01  DLI-IO-WDGX1144.                                                     
017500*    03  -COPY WDGX1144                                                   
017600                                                                          
017700 LINKAGE SECTION.                                                         
017800                                                                          
017900*01  -COPY W0008   -PRE WDG2-                                             
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200 PROCEDURE DIVISION USING  WDG2-PCB.                                      
018300                                                                          
018400     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
018500 MAIN SECTION.                                                            
018600     SKIP2                                                                
018700                                                                          
018800     PERFORM A-INIT                                                       
018900                                                                          
019000     PERFORM B-WDGX1144-TAB                                               
019100                                                                          
019200     PERFORM S01-LAES-W01160                                              
019300     PERFORM UNTIL END-OF-W01160                                          
019400                                                                          
019500       IF IN-CLAG-TIURPROD > ZERO                                         
019600          PERFORM C-SKAPA-EV-UTPOST                                       
019700       END-IF                                                             
019800                                                                          
019900       PERFORM S01-LAES-W01160                                            
020000     END-PERFORM                                                          
020100                                                                          
020200                                                                          
020300     PERFORM Z-FINIT                                                      
020400                                                                          
020500     MOVE ZERO TO RETURN-CODE                                             
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000                                                                          
021100     OPEN INPUT  W01160                                                   
021200                                                                          
021300     OPEN OUTPUT W26162                                                   
021400     SKIP2                                                                
021500     ACCEPT DAGENS-DATUM  FROM DATE                                       
021600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021700                                                                          
021800     MOVE 'IDAG' TO DAT-KDDATFORM                                         
021900     CALL WDATKONV USING DAT-KDDATFORM,                                   
022000                         DAT-I-TIDATUM,                                   
022100                         DAT-O-TIDATUM,                                   
022200                         DAT-KDSVAR                                       
022300                                                                          
022400     IF DAT-KDSVAR-FEL                                                    
022500        DISPLAY '****  FEL I WDATKONV  *******'                           
022600        PERFORM S99-ABEND                                                 
022700     END-IF                                                               
022800                                                                          
022900     MOVE DAT-TIAAVV-GRP TO W-TIAAVV                                      
023000     MOVE DAT-TISEKEL    TO W-TISEKEL                                     
023100     .                                                                    
023200     EJECT                                                                
023300 B-WDGX1144-TAB SECTION.                                                  
023400                                                                          
023500     MOVE +0                        TO TAB-IX                             
023600     PERFORM IMS-GU-WDGX1143                                              
023700                                                                          
023800     PERFORM IMS-GNP-WDGX1144                                             
023900     PERFORM UNTIL SEGMENT-MISSING                                        
024000        ADD +1                      TO TAB-IX                             
024100        MOVE 1144-IDFKNGRP-FOM      TO TAB-IDFKNGRP-FOM (TAB-IX)          
024200        MOVE 1144-IDFKNGRP-TOM      TO TAB-IDFKNGRP-TOM (TAB-IX)          
024300        MOVE 1144-KVAARLF           TO TAB-KVAARLF      (TAB-IX)          
024400                                                                          
024500        PERFORM IMS-GNP-WDGX1144                                          
024600                                                                          
024700        IF TAB-IX = MAX-TAB-IX                                            
024800           DISPLAY 'TABELL FÖR DB WDGX1144 > 5000 RECORDS.'               
024900           DISPLAY 'ÖKA ANTAL I TAB-WDGX1144 I WORKING STORAGE.'          
025000           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
025100        END-IF                                                            
025200                                                                          
025300     END-PERFORM                                                          
025400     MOVE TAB-IX                    TO MAX-TAB-IX                         
025500     .                                                                    
025600     EJECT                                                                
025700 C-SKAPA-EV-UTPOST SECTION.                                               
025800                                                                          
025900     MOVE IN-CLAG-KDPRODSL          TO TEST-KDPRODSL                      
026000     IF KDPRODSL-VOLVO-PARTS  OR                                          
026100        KDPRODSL-BYTES        OR                                          
026200        KDPRODSL-ACC          OR                                          
026300        KDPRODSL-BRANDON      OR                                          
026400        KDPRODSL-VOLVO-WHEELS OR                                          
026500        KDPRODSL-SERVICES                                                 
026600        PERFORM CA-SOEK-WDGX1144-IDFKNGRP                                 
026700                                                                          
026800        MOVE IN-CLAG-TIURPROD       TO TMP1-YYWW                          
026900                                                                          
027000        COMPUTE W-TIAAAAVV-AARLF = W-TIAAAAVV - (W-KVAARLF * 100)         
027100        MOVE W-TIAAVV-AARLF         TO TMP2-YYWW                          
027200                                                                          
027300        PERFORM WY2000P3                                                  
027400                                                                          
027500        IF TMP1-YYWW < TMP2-YYWW                                          
027600           PERFORM CB-SKAPA-UTFIL                                         
027700        END-IF                                                            
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 CA-SOEK-WDGX1144-IDFKNGRP SECTION.                                       
028200                                                                          
028300     MOVE 15                        TO W-KVAARLF                          
028400                                                                          
028500     MOVE +1                        TO TAB-IX                             
028600     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
028700       IF IN-CLAG-IDFKNGRP >= TAB-IDFKNGRP-FOM (TAB-IX)                   
028800      AND IN-CLAG-IDFKNGRP <= TAB-IDFKNGRP-TOM (TAB-IX)                   
028900          MOVE TAB-KVAARLF(TAB-IX)  TO W-KVAARLF                          
029000          MOVE 5000                 TO TAB-IX                             
029100       END-IF                                                             
029200       ADD +1                       TO TAB-IX                             
029300     END-PERFORM                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 CB-SKAPA-UTFIL    SECTION.                                               
029700                                                                          
029800     MOVE IN-CLAG-IDANSK      TO UT-IDANSK                                
029900     MOVE IN-CLAG-IDARTNR     TO UT-IDARTNR                               
030000     MOVE IN-CLAG-IDLEVNR     TO UT-IDLEVNR                               
030100     MOVE IN-CLAG-IDFKNGRP    TO UT-IDFKNGRP                              
030200     MOVE IN-CLAG-KDPRODSL    TO UT-KDPRODSL                              
030300     MOVE IN-CLAG-TIURPROD    TO UT-TIURPROD                              
030400     MOVE IN-CLAG-TISLUTKP    TO UT-TISLUTKP                              
030500     MOVE IN-CLAG-KVLS        TO UT-KVLS                                  
030600     MOVE IN-CLAG-PRARTSTD    TO UT-PRARTSTD                              
030700     MOVE IN-CLAG-KVSLUTKP    TO UT-KVSLUTKP                              
030800     MOVE IN-CLAG-ADLAGOMR    TO UT-ADLAGOMR                              
030900     MOVE IN-CLAG-ADPLATS     TO UT-ADPLATS                               
031000     MOVE IN-CLAG-KDERS       TO UT-KDERS                                 
031100     IF UT-KDERS = ZERO                                                   
031200        MOVE IN-CLAG-KDERS-UTG   TO UT-KDERS                              
031300     END-IF                                                               
031400* FÄLT FLFKNPRI OCH FILLER15 ANVÄNDS INTE I RUTIN W261V2.                 
031500* FÄLTEN ANVÄNDS I RUTIN W261B3.                                          
031600     MOVE SPACE               TO UT-FLFKNPRI                              
031700     MOVE SPACE               TO UT-FLAGGA15                              
031800                                                                          
031900     IF UT-KDERS = ZERO OR 09                                             
032000        PERFORM S11-SKRIV-W26162                                          
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 Z-FINIT SECTION.                                                         
032500     CLOSE W01160                                                         
032600           W26162                                                         
032700     SKIP2                                                                
032800     MOVE 'S' TO POSTSUM-OPKOD                                            
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200 S01-LAES-W01160  SECTION.                                                
033300     READ W01160 INTO IN-AREA                                             
033400     AT END                                                               
033500        SET END-OF-W01160 TO TRUE                                         
033600                                                                          
033700     NOT AT END                                                           
033800        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
033900        MOVE 'W26161D2' TO POSTSUM-DDNAMN2                                
034000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
034100        CALL POSTSUM USING POSTSUM-PARM                                   
034200     END-READ                                                             
034300     .                                                                    
034400     EJECT                                                                
034500 S11-SKRIV-W26162 SECTION.                                                
034600                                                                          
034700     WRITE UT-POST FROM UT-AREA                                           
034800                                                                          
034900     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
035000     MOVE 'W26162'   TO POSTSUM-FDNAMN                                    
035100     MOVE 'W26161D3' TO POSTSUM-DDNAMN2                                   
035200     CALL POSTSUM USING POSTSUM-PARM                                      
035300     .                                                                    
035400     EJECT                                                                
035500 S99-ABEND SECTION.                                                       
035600                                                                          
035700     SKIP2                                                                
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     CALL ABEND USING RKOD-ABEND                                          
036100     .                                                                    
036200     EJECT                                                                
036300 IMS-GU-WDGX1143 SECTION.                                                 
036400     MOVE 'IMS-GU-WDGX1143              ' TO IMS-SEKTION                  
036500                                                                          
036600     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
036700            DELIMITED BY SIZE INTO SSA1                                   
036800     MOVE '    '                TO GOOD-STATUSCODES                       
036900     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX01 SSA1                    
037000     MOVE WDG2-STATUS-CODE      TO STATUS-WS                              
037100     PERFORM IMS-STATUSCHECK                                              
037200     .                                                                    
037300                                                                          
037400 IMS-GNP-WDGX1144  SECTION.                                               
037500     MOVE 'IMS-GNP-WDGX1144             ' TO IMS-SEKTION                  
037600                                                                          
037700     MOVE 'WDGX1144 '          TO SSA1                                    
037800     MOVE '  GE'               TO GOOD-STATUSCODES                        
037900     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
038000     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
038100     PERFORM IMS-STATUSCHECK                                              
038200     .                                                                    
038300                                                                          
038400 IMS-STATUSCHECK SECTION.                                                 
038500                                                                          
038600     SET STATUS-IX TO 1                                                   
038700     SEARCH GOOD-STATUS                                                   
038800       AT END                                                             
038900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039000           DELIMITED BY SIZE INTO ERROR-TEXT                              
039100         DISPLAY ERROR-TEXT                                               
039200         CALL FELLOG                                                      
039300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
039400         CONTINUE                                                         
039500     END-SEARCH                                                           
039600     .                                                                    
039700     EJECT                                                                
039800*    -COPY WY2000P3                                                       
