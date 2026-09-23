000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6120200.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   95/02/07.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR TVÅ FILER FRÅN WDL6 FÖR VIDARE BEFORDRAN TILL TVÅ         
001000*        ST EPLUS PGM SOM SKRIVER UT SKAMLISTOR.                          
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001300*        PROGRAMMET LÄSER      WLINLD (WDL6)                              
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*        PROGRAMMET LÄSER      WL6301 (WDR5)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- 310 TRANSAR                                                
003000     SELECT W61202                     ASSIGN TO W61202D1.                
003100     SKIP2                                                                
003200*          --- R30 TRANSAR                                                
003300     SELECT W61204                     ASSIGN TO W61202D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W61202                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W6120201 -PRE  UT1-  -L.                                  
004400     SKIP3                                                                
004500 FD  W61204                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W6120202 -PRE  UT2-   -L.                                 
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005201*    -COPY WY2000W9                                                       
005202     SKIP3                                                                
005300 77  IDPGM                       PIC X(8)    VALUE 'W6120200'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600     EJECT                                                                
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006210*      --- VALID IDDC CODES                                               
006220*                                                                         
006230*01    -COPY WWDC99                                                       
006240       EJECT                                                              
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     SKIP2                                                                
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400*01  -COPY WDATAREA                                                       
008500     EJECT                                                                
008600 01  FILLER              PIC X(11)   VALUE    'ARBETSAREOR'.              
008700 77  W-DAINLEV-UPPACK            PIC 9(16)   VALUE ZERO.                  
008710 77  W-TIAVIDAT-9-KOMPL          PIC 9(6)    VALUE ZERO.                  
008800 77  SW-EN-VECKA-GAMMAL          PIC X(1)    VALUE 'N'.                   
008900   88 EN-VECKA-GAMMAL                        VALUE 'J'.                   
009000 01  W-SPAR-TIAADDD-GRP.                                                  
009100   05  W-SPAR-TIAA            PIC 9(2).                                   
009200   05  W-SPAR-TIDDD           PIC 9(3).                                   
009300                                                                          
009400 01  W-TIAADDD-GRP.                                                       
009500   05  W-TIAA            PIC 9(2).                                        
009600   05  W-TIDDD           PIC 9(3).                                        
009700                                                                          
009800 01  UT1-AREA-START              PIC X(24)   VALUE                        
009900                                 'UT1-AREA-START  '.                      
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY W6120201     -PRE UT1-                                    
010300     EJECT                                                                
010400 01  UT2-AREA-START             PIC X(24)   VALUE                         
010500                                 'UT2-AREA-START  '.                      
010600     SKIP2                                                                
010700                                                                          
010800*01  AREA -COPY W6120202     -PRE UT2-                                    
010900     EJECT                                                                
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600     03  W-DAINLEV-X.                                                     
011700         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
011800                                                                          
011900     03  W-IDARTNR-X.                                                     
012000         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
012100                                                                          
012200     03  W-IDSKYLT-X.                                                     
012300         05  W-IDSKYLT           PIC  X(3)   VALUE 'GB '.                 
012400                                                                          
012500     03  W-WDL6A1KY-MIN.                                                  
012600         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
012700         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
012800         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
012900         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
013000         05  W-SEQA-IDARTNO-MIN   PIC S9(9)             COMP-3.           
013100         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
013200                                                                          
013300     03  W-WDL6A1KY-MAX.                                                  
013400         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
013500         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
013600         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
013700         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
013800         05  W-SEQA-IDARTNO-MAX   PIC S9(9)             COMP-3.           
013900         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
014000                                                                          
014100     03  W-WDGXKEY-6301.                                                  
014200         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
014300         05  W-6301-IDDC        PIC X(2).                                 
014400         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
014500                                                                          
014600     03  W-WDGXKEY-6302.                                                  
014700         05  W-6302-DABERANK     PIC 9(8).                                
014800         05  W-6302-IDFAKT       PIC S9(7)              COMP-3.           
014900                                                                          
015000                                                                          
015100     SKIP2                                                                
015200*    --- STATUS-KOD FRÅN IMS                                              
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015610     88  BASEN-SLUT                          VALUE 'GB'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900     SKIP3                                                                
017000 01  DLI-IO-AREA.                                                         
017100     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
017200     SKIP3                                                                
017300     03  WL630101 REDEFINES IO-AREA.                                      
017400*        05  -COPY WDGX6301                                               
017500     EJECT                                                                
017600     03  WL630111 REDEFINES IO-AREA.                                      
017700*        05  -COPY WDGX6302                                               
017800     EJECT                                                                
017900     03  WLINLC11 REDEFINES IO-AREA.                                      
018000*        05  -COPY WDL611                                                 
018100     EJECT                                                                
018200     03  WLINLD01 REDEFINES IO-AREA.                                      
018300*        05  -COPY WDL6A1                                                 
018400     EJECT                                                                
018500*    03  WLBENA11  -COPY WDD311 -PRE BENA11-   -RED IO-AREA.              
018600     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800                                                                          
018900     EJECT                                                                
019000*01  -COPY W0008  -PRE INLC-                                              
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008  -PRE INLD-                                              
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008  -PRE BENA-                                              
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900*01  -COPY W0008  -PRE GX63-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200 PROCEDURE DIVISION  USING INLC-PCB INLD-PCB BENA-PCB GX63-PCB.           
020300     ENTRY 'DLITCBL' USING INLC-PCB INLD-PCB BENA-PCB GX63-PCB.           
020400                                                                          
020500*------------------------                                                 
020600                                                                          
020700     PERFORM A-INIT                                                       
020800     PERFORM IMS-GU-INLD01                                                
020900     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
021000       MOVE SEQA-IDARTNR   TO W-IDARTNR                                   
021100       MOVE SEQA-DAINLEV   TO W-DAINLEV                                   
021200       PERFORM IMS-GU-INLC11                                              
021300                                                                          
021310       MOVE INL-IDDC  TO WS-IDDC                                          
021400       IF INL-IDPTYP  = '310' AND (CDC OR SDC)                            
021500          PERFORM B-SKAPA-UTFIL-310                                       
021600       END-IF                                                             
021700                                                                          
021800       IF INL-IDPTYP  = 'R30' AND (CDC OR SDC)                            
021900          PERFORM C-SKAPA-UTFIL-R30                                       
022000       END-IF                                                             
022100                                                                          
022200       PERFORM IMS-GN-INLD01                                              
022300                                                                          
022400     END-PERFORM                                                          
022500                                                                          
022600                                                                          
022700     PERFORM Z-FINIT                                                      
022800                                                                          
022900     MOVE ZERO TO RETURN-CODE                                             
023000     GOBACK                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 A-INIT SECTION.                                                          
023400                                                                          
023500     OPEN OUTPUT W61202                                                   
023600                 W61204                                                   
023700                                                                          
023800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023900                                                                          
024000     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
024100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
024200                     DAT-O-TIDATUM DAT-KDSVAR                             
024300                                                                          
024400     IF DAT-KDSVAR-OK                                                     
024500          MOVE DAT-TIAADDD-GRP   TO W-SPAR-TIAADDD-GRP                    
024600     ELSE                                                                 
024700          MOVE 'FEL1 FRÅN WDATKONV ' TO FELTEXT                           
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 B-SKAPA-UTFIL-310  SECTION.                                              
025200                                                                          
025300     MOVE INL-IDDC        TO UT1-IDDC                                     
025310     MOVE INL-IDFAKT      TO UT1-IDFAKT                                   
025400     MOVE INL-IDKOLLI     TO UT1-IDKOLLI                                  
025500     MOVE W-IDARTNR       TO UT1-IDARTNR                                  
025600     MOVE INL-KVAVIS      TO UT1-KVAVIS                                   
025700     MOVE INL-TIINLMOT    TO UT1-TIINLMOT                                 
025800                                                                          
025900     PERFORM IMS-GU-BENA11                                                
026000     MOVE BENA11-TEXT-BEART   TO UT1-BEART                                
026100                                                                          
026200     PERFORM S11-SKRIV-W61202                                             
026300     .                                                                    
026400     EJECT                                                                
026500 C-SKAPA-UTFIL-R30  SECTION.                                              
026600                                                                          
026700     MOVE INL-DAINLEV              TO W-DAINLEV-UPPACK                    
026710     MOVE W-DAINLEV-UPPACK(3:6)    TO W-TIAVIDAT-9-KOMPL                  
026800     COMPUTE UT2-TIAVIDAT          =  999999 - W-TIAVIDAT-9-KOMPL         
026900                                                                          
027000     PERFORM CA-KOLLA-OM-EN-VECKA-GAMMAL                                  
027100                                                                          
027200     IF EN-VECKA-GAMMAL                                                   
027300        MOVE INL-IDDC     TO UT2-IDDC                                     
027400        MOVE INL-IDKUNDNR TO UT2-IDKUNDNR                                 
027500        MOVE INL-IDKUNDRF TO UT2-IDKUNDRF                                 
027600        MOVE INL-IDKOLLI  TO UT2-IDKOLLI                                  
027700        MOVE INL-IDFAKT   TO UT2-IDFAKT                                   
027800        MOVE W-IDARTNR    TO UT2-IDARTNR                                  
027900        MOVE INL-KVAVIS   TO UT2-KVAVIS                                   
028100                                                                          
028110        PERFORM CB-HAEMTA-IDLBBET                                         
028120                                                                          
028200        PERFORM IMS-GU-BENA11                                             
028300        MOVE BENA11-TEXT-BEART TO UT2-BEART                               
028400                                                                          
028600                                                                          
028700        PERFORM S12-SKRIV-W61204                                          
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 CA-KOLLA-OM-EN-VECKA-GAMMAL  SECTION.                                    
029200                                                                          
029300     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
029400     MOVE UT2-TIAVIDAT    TO DAT-I-TIDATUM                                
029500                                                                          
029600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
029700                     DAT-O-TIDATUM DAT-KDSVAR                             
029800                                                                          
029900     IF DAT-KDSVAR-OK                                                     
030000        MOVE DAT-TIAADDD-GRP        TO W-TIAADDD-GRP                      
030001        MOVE W-TIAA        TO TMP1-YY                                     
030002        MOVE W-SPAR-TIAA   TO TMP2-YY                                     
030010        PERFORM WY2000P9                                                  
030100        IF TMP1-YY > TMP2-YY                                              
030200           ADD 365                  TO W-TIDDD                            
030300        END-IF                                                            
030400                                                                          
030500        IF W-TIDDD                  >  W-SPAR-TIDDD + 7                   
030600           MOVE JA                  TO SW-EN-VECKA-GAMMAL                 
030700        END-IF                                                            
030800     ELSE                                                                 
030900        MOVE 'FEL2 FRÅN WDATKONV'   TO FELTEXT                            
031000        CALL FELLOG                                                       
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 CB-HAEMTA-IDLBBET            SECTION.                                    
031500                                                                          
031600     MOVE INL-IDDC             TO W-6301-IDDC                             
031700     MOVE UT2-TIAVIDAT         TO W-6302-DABERANK                         
031710     IF UT2-TIAVIDAT NOT = ZERO                                           
031720       IF UT2-TIAVIDAT < 500000                                           
031730         MOVE 20               TO W-6302-DABERANK (1:2)                   
031740       ELSE                                                               
031750         IF UT2-TIAVIDAT < 999999                                         
031760           MOVE 19             TO W-6302-DABERANK (1:2)                   
031770         ELSE                                                             
031780           MOVE 99999999       TO W-6302-DABERANK                         
031790         END-IF                                                           
031791       END-IF                                                             
031792     END-IF                                                               
031800     MOVE INL-IDFAKT           TO W-6302-IDFAKT                           
031900                                                                          
032000     PERFORM IMS-GU-WL6302                                                
032100     IF SEGMENT-FINNS                                                     
032200         MOVE 6302-IDLBBET     TO UT2-IDLBBET                             
032300     ELSE                                                                 
032400         MOVE SPACE            TO UT2-IDLBBET                             
032500     END-IF                                                               
032600                                                                          
032700     .                                                                    
032800     EJECT                                                                
032900 Z-FINIT SECTION.                                                         
033000     CLOSE W61202                                                         
033100           W61204                                                         
033200     SKIP2                                                                
033300     MOVE 'S' TO POSTSUM-OPKOD                                            
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500     .                                                                    
033600     EJECT                                                                
033700 S11-SKRIV-W61202 SECTION.                                                
033800                                                                          
033900     WRITE UT1-POST   FROM UT1-AREA                                       
034000                                                                          
034100     MOVE '310'       TO POSTSUM-TRANSTYP                                 
034200     MOVE 'W61202'    TO POSTSUM-FDNAMN                                   
034300     MOVE 'W61202D1'  TO POSTSUM-DDNAMN2                                  
034400     CALL POSTSUM USING POSTSUM-PARM                                      
034500     .                                                                    
034600     EJECT                                                                
034700 S12-SKRIV-W61204 SECTION.                                                
034800                                                                          
034900     WRITE UT2-POST    FROM UT2-AREA                                      
035000                                                                          
035100     MOVE 'R30'        TO POSTSUM-TRANSTYP                                
035200     MOVE 'W61204'     TO POSTSUM-FDNAMN                                  
035300     MOVE 'W61202D2'   TO POSTSUM-DDNAMN2                                 
035400     CALL POSTSUM USING POSTSUM-PARM                                      
035500     .                                                                    
035600     EJECT                                                                
035700 S99-ABEND SECTION.                                                       
035800                                                                          
035900     SKIP2                                                                
036000     MOVE 'S' TO POSTSUM-OPKOD                                            
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
036300     .                                                                    
036400     EJECT                                                                
036500* --- IMS SEKTIONER ---                                                   
036600     SKIP3                                                                
036700 IMS-GU-INLD01    SECTION.                                                
036800                                                                          
036900     MOVE 'WLINLD01'   TO SSA1                                            
037000     MOVE '  GE' TO GODK-STATUSKODER                                      
037100     CALL CBLTDLI USING GU  INLD-PCB DLI-IO-AREA SSA1                     
037200     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
037300     PERFORM IMS-STATUSKONTROLL                                           
037400     .                                                                    
037500     SKIP3                                                                
037600 IMS-GN-INLD01    SECTION.                                                
037700                                                                          
037800     MOVE 'WLINLD01'   TO SSA1                                            
037900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
038000     CALL CBLTDLI USING GN  INLD-PCB DLI-IO-AREA SSA1                     
038100     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     SKIP3                                                                
038500 IMS-GU-INLC11    SECTION.                                                
038600                                                                          
038700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
038800          DELIMITED BY SIZE INTO SSA1                                     
038900     STRING 'WLINLC11(DAINLEV  =' W-DAINLEV-X ')'                         
039000          DELIMITED BY SIZE INTO SSA2                                     
039100     MOVE '  ' TO GODK-STATUSKODER                                        
039200     CALL CBLTDLI USING GU  INLC-PCB DLI-IO-AREA SSA1 SSA2                
039300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     .                                                                    
039600     EJECT                                                                
039700 IMS-GU-BENA11      SECTION.                                              
039800                                                                          
039900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
040000            DELIMITED BY SIZE INTO SSA1                                   
040100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
040200            DELIMITED BY SIZE INTO SSA2                                   
040300     MOVE '  ' TO GODK-STATUSKODER                                        
040400     CALL CBLTDLI USING GU    BENA-PCB DLI-IO-AREA SSA1 SSA2              
040500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     CONTINUE.                                                            
040800     EJECT                                                                
040900 IMS-GU-WL6302   SECTION.                                                 
041000                                                                          
041100     STRING 'WL630101(WDGXKEY = ' W-WDGXKEY-6301 ')'                      
041200          DELIMITED BY SIZE INTO SSA1                                     
041210     STRING 'WL630111(KEY6302 = ' W-WDGXKEY-6302 ')'                      
041220          DELIMITED BY SIZE INTO SSA2                                     
041300     MOVE '  GE' TO GODK-STATUSKODER                                      
041400     CALL CBLTDLI USING GU GX63-PCB DLI-IO-AREA SSA1 SSA2                 
041500                                                                          
041600     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     .                                                                    
041900     EJECT                                                                
043100 IMS-STATUSKONTROLL SECTION.                                              
043200                                                                          
043300     SET STATUS-IX TO 1                                                   
043400     SEARCH GODK-STATUS                                                   
043500       AT END                                                             
043600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
043700         DISPLAY FELTEXT                                                  
043800         CALL FELLOG                                                      
043900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044000         CONTINUE                                                         
044100     END-SEARCH                                                           
044200     .                                                                    
044210     EJECT                                                                
044400*    -COPY WY2000P9                                                       
