000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4402000.                                        
000300 AUTHOR.                 CARINA VIKTORSSON.                               
000400 DATE-WRITTEN.           JULI 1988.                                       
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER INFILER FRÅN ANSKAFFARNA MED INFORMATION OM                
000900*        ERSATTA/RO-ERS ARTIKLAR.                                         
001000*        SORTERING SKER AV INPOSTERNA FÖR ATT PLOCKA BORT POSTER          
001100*        MED DUBBLA ARTIKELNR/CLAGER.                                     
001200*                                                                         
001300*        ALLA ARTIKLAR LÄGGS UT PÅ UTFILEN TILLSAMMANS MED ALLA EV        
001400*        TILLKOMMANDE ARTIKLAR OCH KOMPLETTERANDE ARTIKELINFORMATI        
001500*                                                                         
001600*    ÄNDRING:                                                             
001700*        JAN-1997 / BOO HAMMARIN, GDC-G / ANPASSNING NDC-LAGER            
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*    ---- INFIL:                                                          
002700     SELECT  INFIL         ASSIGN  W44020D1.                              
002800     SKIP2                                                                
002900*    ---- UTFIL:                                                          
003000     SELECT  UTFIL         ASSIGN  W44020D2.                              
003100     SKIP2                                                                
003200*    ---- SORTFIL:                                                        
003300     SELECT  SORTFIL       ASSIGN  W44020DS.                              
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  INFIL                                                                
004000     LABEL RECORD STANDARD                                                
004100     RECORDING  F                                                         
004200     BLOCK CONTAINS 0.                                                    
004300                                                                          
004400 01  INPOST  -COPY W440004    -L.                                         
004500     EJECT                                                                
004600 FD  UTFIL                                                                
004700     LABEL RECORD STANDARD                                                
004800     RECORDING  F                                                         
004900     BLOCK CONTAINS 0.                                                    
005000                                                                          
005100 01  UTPOST -COPY W440001       -L.                                       
005200     EJECT                                                                
005300 SD  SORTFIL                                                              
005400     RECORDING  V.                                                        
005500                                                                          
005600 01  -COPY W440004       -PRE S-.                                         
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4402000'.                
006200 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
006300                                                                          
006400 77  JA                      PIC X       VALUE 'J'.                       
006500 77  NEJ                     PIC X       VALUE 'N'.                       
006600                                                                          
006700 77  IX-SUP-START            PIC S9(9)   VALUE +0   COMP SYNC.            
006710 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
006800 77  RAD-IX                  PIC S9(9)   VALUE +0   COMP SYNC.            
006900                                                                          
007000 77  INFIL-EOF               PIC X       VALUE 'N'.                       
007100 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
007200                                                                          
007300 77  IDARTNR-WS              PIC S9(9)   VALUE ZERO  COMP-3.              
007400 77  KORTNR-WS               PIC S9(3)   VALUE ZERO  COMP-3.              
007500 77  LOPNR-WS                PIC S9(3)   VALUE ZERO  COMP-3.              
007600 77  DIERS-ERS-WS            PIC S9(4)V9(3) VALUE ZERO COMP-3.            
007700 77  DIERS-TILLK-WS          PIC S9(4)V9(3) VALUE ZERO COMP-3.            
007800                                                                          
007900 77  BRYT-IDDC               PIC X(2)    VALUE SPACE.                     
008000 77  BRYT-IDARTNR            PIC S9(9)   VALUE ZERO  COMP-3.              
008100                                                                          
008200*                                URSPRUNGLIGT ARTNR                       
008300 01  WS-IDARTNR-URS          PIC S9(9)   VALUE ZERO  COMP-3.              
008400*                                URSPRUNGLIG KDUART                       
008500 01  WS-KDUART-URS           PIC X(1).                                    
008600*                                ANTAL KORT I ERSÄTTNING                  
008700 01  WS-KVKORT               PIC S9(3)   VALUE ZERO  COMP-3.              
008800     SKIP1                                                                
008900 01  SW-VARIABEL             PIC X(1).                                    
009000     88  VARIABEL-JA         VALUE 'J'.                                   
009100     88  VARIABEL-NEJ        VALUE 'N'.                                   
009200     SKIP1                                                                
009300 01  SW-SLAUPP               PIC X(1).                                    
009400     88  SLAUPP-JA           VALUE 'J'.                                   
009500     SKIP1                                                                
009600 01  SW-RADTYP               PIC X(1).                                    
009700     88  URS-RAD             VALUE 'U'.                                   
009800     88  EJ-ACCEPT-RAD       VALUE 'E'.                                   
009801     SKIP1                                                                
009810 01  SW-KVLS-TIFINLV-OK      PIC X(1).                                    
009900     EJECT                                                                
010000*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
010100     SKIP3                                                                
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
010400   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
010500   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
010600   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
010610   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
010620   03  W009VADD              PIC X(8)    VALUE 'W009VADD'.                
010700     SKIP3                                                                
010800*    ----  PARAMETRAR TILL POSTSUM                                        
010900     SKIP2                                                                
011000 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16 COMP SYNC.             
011100     SKIP2                                                                
011200*    ----  PARAMETRAR TILL POSTSUM                                        
011300                                                                          
011400 01  -COPY W0005       -PRE POSTSUM-.                                     
011500     EJECT                                                                
011501                                                                          
011502 01  CURR-YYWWD-PLUS-2WEEKS-AREA.                                         
011503     03 CURR-YYWWD-PLUS-2WEEKS       PIC 9(5)       VALUE ZERO.           
011504     03 CURR-YYWWD-PLUS-2WEEKS-GRP                                        
011505                             REDEFINES CURR-YYWWD-PLUS-2WEEKS.            
011506        05 CURR-YYWWD-PLUS-2W-YYWW   PIC 9(4).                            
011507        05 CURR-YYWWD-PLUS-2W-D      PIC 9(1).                            
011508                                                                          
011510*01  -COPY WDATAREA                                                       
011520     EJECT                                                                
011521*    -COPY WY2000W2                                                       
011522     EJECT                                                                
011523                                                                          
011524 01  W009VADD-AREA.                                                       
011525     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
011526     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
011530                                                                          
011600*    ----  AREA FÖR INPOSTER                                              
011700 01  -COPY W440004     -PRE IN-.                                          
011800     SKIP2                                                                
011900*    ----  AREA FÖR UTPOSTER                                              
012000 01  FILLER                  PIC X(24) VALUE 'UTPOST-AREA   '.            
012100     SKIP2                                                                
012200 01  UTAREA -COPY W440001                                                 
012300     EJECT                                                                
012400*    ---- TABELLAREA                                                      
012500 01  TABELL.                                                              
012600     03  TABELLEN            OCCURS 5000.                                 
012700         05  TAB-KOD             PIC X(3).                                
012800         05  TAB-IDARTNR         PIC S9(9)       COMP-3.                  
012900         05  TAB-IDKORTNR        PIC S9(3)       COMP-3.                  
013000         05  TAB-IDLOPNR         PIC S9(3)       COMP-3.                  
013100         05  TAB-KDRESTR         PIC S9(3)       COMP-3.                  
013200         05  TAB-IDDC            PIC X(02).                               
013300         05  TAB-DIERS-ERS       PIC S9(4)V9(3)  COMP-3.                  
013400         05  TAB-DIERS-TILLK     PIC S9(4)V9(3)  COMP-3.                  
013500         05  TAB-IDANSK          PIC S9(3)       COMP-3.                  
013600         05  TAB-KDERS           PIC S9(3)       COMP-3.                  
013700         05  TAB-KDLEVSP         PIC S9(3)       COMP-3.                  
013800         05  TAB-KDPRODSL        PIC S9(3)       COMP-3.                  
013900         05  TAB-PRARTSTD        PIC S9(7)V9(2)  COMP-3.                  
014000         05  TAB-REKSIFFR        PIC S9(1)       COMP-3.                  
014100         05  TAB-KVQPACK-1       PIC S9(5)       COMP-3.                  
014200         05  TAB-BEERS           PIC X(20).                               
014300         05  TAB-KDSORT          PIC X(2).                                
014400         05  TAB-TIDISPIN        PIC S9(7)       COMP-3.                  
014500         05  TAB-REDIRLEV        PIC S9(1)V9(2)  COMP-3.                  
014600         05  TAB-KDUART          PIC X(1).                                
014700         05  TAB-KDUART-URS      PIC X(1).                                
014800         05  TAB-IDARTNR-URS     PIC S9(9)       COMP-3.                  
014900         05  TAB-KVKORT          PIC S9(3)       COMP-3.                  
014910         05  TAB-TIFINLV         PIC S9(5)       COMP-3.                  
014920         05  TAB-KVLS            PIC S9(7)       COMP-3.                  
015000     EJECT                                                                
015100*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
015200                                                                          
015300 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
015400     SKIP3                                                                
015500*    ---- STATUSKOD FRÅN IMS                                              
015600                                                                          
015700 01  STATUS-WS               PIC XX.                                      
015800     88  SEGMENT-FINNS                    VALUE '  '.                     
015900     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
016000     88  SEGMENT-HOGRE                    VALUE 'GA'.                     
016100     SKIP3                                                                
016200 01  GODK-STATUSKODER.                                                    
016300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
016400     SKIP3                                                                
016500 01  SSA1                    PIC X(32).                                   
016600 01  SSA2                    PIC X(32).                                   
016700 01  SSA3                    PIC X(32).                                   
016800     EJECT                                                                
016900*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
017000                                                                          
017100 01  NYCKLAR-TILL-DLI.                                                    
017200                                                                          
017300   03  W-IDARTNR-X.                                                       
017400     05  W-IDARTNR           PIC S9(9)    COMP-3.                         
017500   03  W-IDDC-X.                                                          
017600     05  W-IDDC              PIC X(2).                                    
017700   03  W-IDDC-B6-X.                                                       
017800       05 W-IDDC-B6                  PIC X(2).                            
017900     EJECT                                                                
018000 01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI-IO-AREA                                                     
018300     SKIP3                                                                
018400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC01'.            
018500 01  DLI-IO-ARTC01.                                                       
018600*    03  WLARTC01 -COPY WDK601                                            
018700     EJECT                                                                
018800                                                                          
018900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
019000 01  DLI-IO-ARTC11.                                                       
019100*    03  WLARTC11 -COPY WDK611                                            
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK701'.            
019500 01  DLI-IO-WDK701.                                                       
019600*    03  WDK701 -COPY WDK701                                              
019700     EJECT                                                                
019800                                                                          
019900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK711'.            
020000 01  DLI-IO-WDK711.                                                       
020100*    03  WDK711 -COPY WDK711                                              
020200     EJECT                                                                
020300                                                                          
020310 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK722'.            
020320 01  DLI-IO-WDK722.                                                       
020330*    03  WDK722 -COPY WDK722                                              
020340     EJECT                                                                
020400                                                                          
020500 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ERSA01'.            
020600 01  DLI-IO-ERSA01.                                                       
020700*    03  WLERSA01 -COPY WDD701      -PRE ERSA-                            
020800     EJECT                                                                
020900                                                                          
021000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ERSA11'.            
021100 01  DLI-IO-ERSA11.                                                       
021200*    03  WLERSA11 -COPY WDD702      -PRE ERSA-                            
021300                                                                          
021400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021500 01   DLI-IO-AREA-B601.                                                   
021600*     03  -COPY WDB601                                                    
021700     EJECT                                                                
021800 LINKAGE SECTION.                                                         
021900     SKIP2                                                                
022000 01  -COPY W0008      -PRE  ARTC-                                         
022100       05  FILLER                PIC X.                                   
022200     EJECT                                                                
022300 01  -COPY W0008      -PRE  WDK7-                                         
022400       05  FILLER                PIC X.                                   
022500     EJECT                                                                
022600 01  -COPY W0008      -PRE  ERSA-                                         
022700       05  FILLER                PIC X.                                   
022800     EJECT                                                                
022900 01  -COPY W0008      -PRE  WDB6-                                         
023000       05  FILLER                PIC X.                                   
023100     EJECT                                                                
023200 PROCEDURE DIVISION  USING  ARTC-PCB WDK7-PCB ERSA-PCB WDB6-PCB.          
023300 MAIN SECTION.                                                            
023400     ENTRY 'DLITCBL' USING  ARTC-PCB WDK7-PCB ERSA-PCB WDB6-PCB.          
023500                                                                          
023600     PERFORM A-INIT                                                       
023700                                                                          
023800     SORT SORTFIL                                                         
023900        ASCENDING S-IDDC S-IDARTNR                                        
024000        INPUT  PROCEDURE B-LAS-INPOSTER                                   
024100        OUTPUT PROCEDURE C-BEHANDLA-UTPOSTER.                             
024200                                                                          
024300     IF SORT-RETURN > ZERO                                                
024400        DISPLAY '*** W44020 - FEL VID SORTERING'                          
024500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
024600     ELSE                                                                 
024700        PERFORM Z-FINIT                                                   
024800        MOVE ZERO TO RETURN-CODE                                          
024900        GOBACK                                                            
025000     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 A-INIT SECTION.                                                          
025400     SKIP2                                                                
025500     OPEN INPUT  INFIL                                                    
025600     OPEN OUTPUT UTFIL                                                    
025700                                                                          
025800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
025900     PERFORM AA-NOLLSTALL-TABELL                                          
026000                                                                          
026100     MOVE NEJ TO SORTFIL-EOF                                              
026107                                                                          
026108     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
026109     CALL WDATKONV USING DAT-KDDATFORM                                    
026110                         DAT-I-TIDATUM                                    
026111                         DAT-O-TIDATUM                                    
026112                         DAT-KDSVAR                                       
026113                                                                          
026114     IF DAT-KDSVAR-OK                                                     
026116        MOVE DAT-TIAAVVD             TO CURR-YYWWD-PLUS-2WEEKS            
026117        MOVE CURR-YYWWD-PLUS-2W-YYWW TO VADD-DATUM-AAVV                   
026118        MOVE 2                       TO VADD-ANTAL                        
026120        CALL W009VADD      USING VADD-DATUM-AAVV VADD-ANTAL               
026130        MOVE VADD-DATUM-AAVV         TO CURR-YYWWD-PLUS-2W-YYWW           
026131        DISPLAY 'CURR-YYWWD-PLUS-2WEEKS: ' CURR-YYWWD-PLUS-2WEEKS         
026140     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 AA-NOLLSTALL-TABELL SECTION.                                             
026500     SKIP2                                                                
026600     MOVE +1 TO IX                                                        
026700     PERFORM UNTIL IX > +5000                                             
026800       MOVE ZERO TO          TAB-IDARTNR(IX)                              
026900                             TAB-IDKORTNR(IX)                             
027000                             TAB-IDLOPNR(IX)                              
027100                             TAB-KDRESTR(IX)                              
027200                             TAB-DIERS-ERS(IX)                            
027300                             TAB-DIERS-TILLK(IX)                          
027400                             TAB-IDANSK(IX)                               
027500                             TAB-KDERS(IX)                                
027600                             TAB-KDLEVSP(IX)                              
027700                             TAB-KDPRODSL(IX)                             
027800                             TAB-PRARTSTD(IX)                             
027900                             TAB-REKSIFFR(IX)                             
028000                             TAB-KVQPACK-1(IX)                            
028100                             TAB-TIDISPIN(IX)                             
028200                             TAB-REDIRLEV(IX)                             
028300                             TAB-IDARTNR-URS (IX)                         
028400                             TAB-KVKORT (IX)                              
028410                             TAB-TIFINLV (IX)                             
028420                             TAB-KVLS (IX)                                
028500       MOVE SPACE TO         TAB-IDDC (IX)                                
028600                             TAB-KOD(IX)                                  
028700                             TAB-BEERS(IX)                                
028800                             TAB-KDSORT(IX)                               
028900                             TAB-KDUART (IX)                              
029000                             TAB-KDUART-URS (IX)                          
029100       ADD +1 TO IX                                                       
029200     END-PERFORM                                                          
029300     .                                                                    
029400     EJECT                                                                
029500 B-LAS-INPOSTER SECTION.                                                  
029600     SKIP3                                                                
029700     PERFORM S02-LAES-INPOST                                              
029800     PERFORM UNTIL INFIL-EOF = JA                                         
029900        PERFORM S02-LAES-INPOST                                           
030000     END-PERFORM                                                          
030100     .                                                                    
030200     EJECT                                                                
030300 C-BEHANDLA-UTPOSTER SECTION.                                             
030400                                                                          
030500     PERFORM S01-LAES-SORTPOST                                            
030600                                                                          
030700     PERFORM UNTIL SORTFIL-EOF = JA                                       
030800       IF BRYT-IDDC    NOT = IN-IDDC    OR                                
030900          BRYT-IDARTNR NOT = IN-IDARTNR                                   
031000         MOVE IN-IDDC       TO BRYT-IDDC                                  
031100         MOVE IN-IDARTNR    TO BRYT-IDARTNR                               
031200         ADD +1 TO LOPNR-WS                                               
031300         MOVE IN-IDARTNR TO IDARTNR-WS                                    
031400         MOVE IDARTNR-WS TO WS-IDARTNR-URS                                
031500         MOVE +0 TO KORTNR-WS                                             
031600         MOVE NEJ TO SW-VARIABEL                                          
031610         MOVE JA         TO SW-KVLS-TIFINLV-OK                            
031700                                                                          
031800         MOVE 'U' TO SW-RADTYP                                            
031900         PERFORM CA-LAS-ARTIKEL                                           
032000         IF EJ-ACCEPT-RAD OR SEGMENT-SAKNAS                               
032100           PERFORM CD-RENSA-RAD                                           
032200           IF SEGMENT-SAKNAS                                              
032300              DISPLAY 'ARTIKEL=' W-IDARTNR-X ' SAKNAS PÅ WDK7!'           
032400           END-IF                                                         
032500         ELSE                                                             
032600           PERFORM IMS-GU-WLERSA01                                        
032700           IF SEGMENT-SAKNAS                                              
032800              MOVE ZERO  TO DIERS-ERS-WS                                  
032900              MOVE ZERO  TO WS-KVKORT                                     
033000           ELSE                                                           
033100              MOVE ERSA-DIERS-ERS TO DIERS-ERS-WS                         
033200              MOVE ERSA-KVKORT    TO WS-KVKORT                            
033300           END-IF                                                         
033400           PERFORM CB-TABELL                                              
033410           MOVE IX       TO IX-SUP-START                                  
033500           IF TAB-KDERS(RAD-IX) > +10 AND SEGMENT-FINNS                   
033600              PERFORM IMS-GNP-WLERSA11                                    
033700              PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-HOGRE               
033800                IF ERSA-FLTEXT = NEJ                                      
033900                   MOVE ERSA-IDARTNR-TILLK TO IDARTNR-WS                  
034000                   MOVE ERSA-DIERS-TILLK TO DIERS-TILLK-WS                
034100                   IF DIERS-TILLK-WS < +1                                 
034200                      MOVE +1 TO DIERS-TILLK-WS                           
034300                   END-IF                                                 
034400                   PERFORM CA-LAS-ARTIKEL                                 
034420                   IF TAB-KVLS      (RAD-IX) NOT > ZERO                   
034430                      MOVE NEJ         TO SW-KVLS-TIFINLV-OK              
034431                   ELSE                                                   
034432                      MOVE TAB-TIFINLV(RAD-IX)    TO TMP1-YYWWD           
034433                      MOVE CURR-YYWWD-PLUS-2WEEKS TO TMP2-YYWWD           
034434                      PERFORM WY2000P2                                    
034435                      IF TMP1-YYWWD  > TMP2-YYWWD                         
034437                         MOVE NEJ         TO SW-KVLS-TIFINLV-OK           
034440                      END-IF                                              
034450                   END-IF                                                 
034500                   PERFORM CB-TABELL                                      
034600                   PERFORM IMS-GNP-WLERSA11                               
034700                ELSE                                                      
034800*------------------* TEXT-SEGMENT                                         
034900                   ADD +1 TO RAD-IX                                       
035000                   MOVE WS-IDARTNR-URS TO TAB-IDARTNR-URS (RAD-IX)        
035100                   MOVE WS-KDUART-URS  TO TAB-KDUART-URS (RAD-IX)         
035200                   MOVE +61 TO TAB-KDRESTR(RAD-IX)                        
035300                   MOVE ERSA-BEERS TO TAB-BEERS(RAD-IX)                   
035400                   MOVE 'TIL' TO TAB-KOD(RAD-IX)                          
035500                   MOVE LOPNR-WS TO TAB-IDLOPNR(RAD-IX)                   
035600                   ADD +1 TO KORTNR-WS                                    
035700                   MOVE KORTNR-WS TO TAB-IDKORTNR(RAD-IX)                 
035800                   MOVE IN-IDDC TO TAB-IDDC (RAD-IX)                      
035900                   PERFORM IMS-GNP-WLERSA11                               
036000                END-IF                                                    
036100              END-PERFORM                                                 
036200           END-IF                                                         
036300           IF VARIABEL-NEJ                                                
036400*-------------* IX HAR POSITION PÅ ARTIKELNS ERSATT-TABELL-INGÅNG         
036500*             * EFTERSOM IX SPARATS I CB-TABELL                           
036600              ADD +1 TO IX                                                
036700              PERFORM UNTIL IX = +5001 OR TAB-IDARTNR(IX) = ZERO          
036800                IF TAB-KDERS(IX) > +10                                    
036900                   MOVE TAB-IDARTNR(IX) TO W-IDARTNR-X                    
037000                   PERFORM IMS-GU-WLERSA01                                
037100                   IF SEGMENT-FINNS                                       
037200                      MOVE ERSA-DIERS-ERS TO DIERS-ERS-WS                 
037300                      PERFORM IMS-GNP-WLERSA11                            
037301                      IF SEGMENT-FINNS                                    
037302                         MOVE JA         TO SW-KVLS-TIFINLV-OK            
037303                      END-IF                                              
037400                      PERFORM UNTIL SEGMENT-SAKNAS OR                     
037500                        SEGMENT-HOGRE                                     
037600                        MOVE ERSA-IDARTNR-TILLK TO IDARTNR-WS             
037700                        IF VARIABEL-JA                                    
037800                           MOVE ZERO TO DIERS-TILLK-WS                    
037900                        ELSE                                              
038000                           MOVE ERSA-DIERS-TILLK TO DIERS-TILLK-WS        
038100                           IF DIERS-TILLK-WS < +1                         
038200                              MOVE +1 TO DIERS-TILLK-WS                   
038300                           END-IF                                         
038400                        END-IF                                            
038500                        PERFORM CA-LAS-ARTIKEL                            
038510                        IF TAB-KVLS (RAD-IX) NOT > ZERO                   
038512                         MOVE NEJ    TO SW-KVLS-TIFINLV-OK                
038513                        ELSE                                              
038514                         MOVE TAB-TIFINLV(RAD-IX) TO TMP1-YYWWD           
038515                         MOVE CURR-YYWWD-PLUS-2WEEKS TO TMP2-YYWWD        
038516                         PERFORM WY2000P2                                 
038517                         IF TMP1-YYWWD > TMP2-YYWWD                       
038519                            MOVE NEJ    TO SW-KVLS-TIFINLV-OK             
038520                         END-IF                                           
038521                        END-IF                                            
038600                        PERFORM CB-TABELL                                 
038700                        PERFORM IMS-GNP-WLERSA11                          
038800                      END-PERFORM                                         
038900                   ELSE                                                   
039000                      MOVE ZERO TO DIERS-ERS-WS                           
039100                   END-IF                                                 
039200                   MOVE 'ERS' TO TAB-KOD(IX)                              
039300                   MOVE +41 TO TAB-KDRESTR(IX)                            
039400                END-IF                                                    
039500                ADD +1 TO IX                                              
039600              END-PERFORM                                                 
039700                                                                          
039800              IF IX = +5001                                               
039900                DISPLAY '*** W44020-INTERNTABELL SPRÄNGD, ÖKA ***'        
040000                CALL ABEND USING RKOD-ABEND-UTAN-DUMP                     
040100              END-IF                                                      
040200           END-IF                                                         
040210                                                                          
040220           IF SW-KVLS-TIFINLV-OK = NEJ                                    
040221              PERFORM UNTIL RAD-IX < IX-SUP-START                         
040222                 PERFORM CD-RENSA-RAD                                     
040223              END-PERFORM                                                 
040230           END-IF                                                         
040300         END-IF                                                           
040400         PERFORM S01-LAES-SORTPOST                                        
040500       ELSE                                                               
040600*--------* DUBLETT PÅ ARTNR & DC-LAGER LÄSES FÖRBI                        
040700         PERFORM S01-LAES-SORTPOST                                        
040800       END-IF                                                             
040900     END-PERFORM                                                          
041000     MOVE +1 TO IX                                                        
041100     PERFORM UNTIL IX > +5000 OR TAB-KOD(IX) = SPACE                      
041200       PERFORM CC-SKRIV-UTPOSTER                                          
041300       ADD +1 TO IX                                                       
041400     END-PERFORM                                                          
041500     .                                                                    
041600     EJECT                                                                
041700 CA-LAS-ARTIKEL SECTION.                                                  
041800                                                                          
041900     ADD     +1               TO RAD-IX                                   
042000     MOVE    IDARTNR-WS       TO W-IDARTNR-X                              
042100                                 TAB-IDARTNR   (RAD-IX)                   
042200                                                                          
042300     PERFORM IMS-GU-WLARTC01                                              
042400     MOVE    ART-KDPRODSL     TO TAB-KDPRODSL  (RAD-IX)                   
042500     MOVE    ART-KDSORT       TO TAB-KDSORT    (RAD-IX)                   
042600     MOVE    ART-REKSIFFR     TO TAB-REKSIFFR  (RAD-IX)                   
042610     MOVE    ART-TIFINLV      TO TAB-TIFINLV   (RAD-IX)                   
042700                                                                          
042800     PERFORM IMS-GNP-WLARTC11                                             
042900                                                                          
043000     IF IN-IDDC NOT = DCS-IDDC                                            
043100        MOVE IN-IDDC             TO W-IDDC-B6                             
043200        PERFORM IMS-GU-WDB601                                             
043300     END-IF                                                               
043400     IF DCS-CDC                                                           
043500        MOVE    CLAG-IDANSK      TO TAB-IDANSK    (RAD-IX)                
043600        MOVE    CLAG-KDERS       TO TAB-KDERS     (RAD-IX)                
043700        MOVE    CLAG-KDLEVSP     TO TAB-KDLEVSP   (RAD-IX)                
043800        MOVE    CLAG-KDUART      TO TAB-KDUART    (RAD-IX)                
043900        MOVE    CLAG-KVQPACK-1   TO TAB-KVQPACK-1 (RAD-IX)                
044000        MOVE    CLAG-PRARTSTD    TO TAB-PRARTSTD  (RAD-IX)                
044100        MOVE    CLAG-REDIRLEV    TO TAB-REDIRLEV  (RAD-IX)                
044200        MOVE    CLAG-TIDISPIN    TO TAB-TIDISPIN  (RAD-IX)                
044220        MOVE    CLAG-KVLS        TO TAB-KVLS      (RAD-IX)                
044300                                                                          
044400        IF URS-RAD                                                        
044500           MOVE TAB-KDUART (RAD-IX) TO WS-KDUART-URS                      
044600           IF CLAG-KDERS < +10                                            
044700              MOVE 'E' TO SW-RADTYP                                       
044800           END-IF                                                         
044900        END-IF                                                            
045000                                                                          
045100     ELSE                                                                 
045110        MOVE IN-IDDC             TO W-IDDC                                
045120        PERFORM IMS-GU-WDK722                                             
045130        IF SEGMENT-FINNS AND XLAG-IDANSK > 0                              
045200           MOVE XLAG-IDANSK      TO TAB-IDANSK    (RAD-IX)                
045201        ELSE                                                              
045210           MOVE CLAG-IDANSK      TO TAB-IDANSK    (RAD-IX)                
045220        END-IF                                                            
045300        MOVE    CLAG-KDERS       TO TAB-KDERS     (RAD-IX)                
045400        MOVE    CLAG-KDUART      TO TAB-KDUART    (RAD-IX)                
045500        MOVE    CLAG-KVQPACK-1   TO TAB-KVQPACK-1 (RAD-IX)                
045600        MOVE    CLAG-PRARTSTD    TO TAB-PRARTSTD  (RAD-IX)                
045700        MOVE    CLAG-REDIRLEV    TO TAB-REDIRLEV  (RAD-IX)                
045800        MOVE    CLAG-TIDISPIN    TO TAB-TIDISPIN  (RAD-IX)                
045820        MOVE    CLAG-KVLS        TO TAB-KVLS      (RAD-IX)                
045900                                                                          
046000        IF URS-RAD                                                        
046100           MOVE TAB-KDUART (RAD-IX) TO WS-KDUART-URS                      
046200           IF CLAG-KDERS < +10                                            
046300              MOVE 'E' TO SW-RADTYP                                       
046400           END-IF                                                         
046500        END-IF                                                            
046600     END-IF                                                               
046700                                                                          
046800*    FÄLT PÅ WDK711 ÄR UNIKA FÖR NDC-LAGER                                
046900     IF DCS-NDC                                                           
047000        MOVE IN-IDDC             TO W-IDDC                                
047100        PERFORM IMS-GU-WDK711                                             
047200        MOVE    SLAG-KDLEVSP     TO TAB-KDLEVSP   (RAD-IX)                
047300     END-IF                                                               
047400                                                                          
047500     MOVE IN-IDDC        TO TAB-IDDC (RAD-IX)                             
047600     .                                                                    
047700     EJECT                                                                
047800 CB-TABELL SECTION.                                                       
047900     SKIP2                                                                
048000     MOVE SPACE TO SW-SLAUPP                                              
048100                                                                          
048200     MOVE WS-IDARTNR-URS TO TAB-IDARTNR-URS (RAD-IX)                      
048300     MOVE WS-KDUART-URS  TO TAB-KDUART-URS (RAD-IX)                       
048400                                                                          
048500     IF IN-IDARTNR = IDARTNR-WS                                           
048600*-------* IX SPARAS. ANGER VAR ERSÄTTNINGEN BÖRJAR I TABELLEN.            
048700        MOVE RAD-IX TO IX                                                 
048800        IF TAB-KDERS(RAD-IX) > +10                                        
048900           MOVE 'URS' TO TAB-KOD(RAD-IX)                                  
049000           IF TAB-KDERS(RAD-IX) = +14 OR +24 OR +15 OR +25 OR             
049100              +16 OR +26 OR +18 OR +28                                    
049200              MOVE JA TO SW-VARIABEL                                      
049300              MOVE +61 TO TAB-KDRESTR(RAD-IX)                             
049400           ELSE                                                           
049500              MOVE +41 TO TAB-KDRESTR(RAD-IX)                             
049600           END-IF                                                         
049700           MOVE +1 TO TAB-DIERS-TILLK(RAD-IX)                             
049800           MOVE DIERS-ERS-WS TO TAB-DIERS-ERS(RAD-IX)                     
049900        END-IF                                                            
050000     ELSE                                                                 
050100        MOVE 'TIL' TO TAB-KOD(RAD-IX)                                     
050200        IF TAB-KDERS(RAD-IX) = +14 OR +24 OR +15 OR +25 OR                
050300           +16 OR +26 OR +18 OR +28                                       
050400*----------* URS-ART'S TAB-INGÅNG SÄTTS +61 (EJ ENTYDIG).                 
050500           MOVE JA TO SW-VARIABEL SW-SLAUPP                               
050600           MOVE +61 TO TAB-KDRESTR(IX)                                    
050700        END-IF                                                            
050800        IF VARIABEL-JA                                                    
050900           MOVE +61 TO TAB-KDRESTR(RAD-IX)                                
051000        ELSE                                                              
051100           MOVE +0  TO TAB-KDRESTR(RAD-IX)                                
051200        END-IF                                                            
051300        MOVE DIERS-ERS-WS TO TAB-DIERS-ERS(RAD-IX)                        
051400        MOVE DIERS-TILLK-WS TO TAB-DIERS-TILLK(RAD-IX)                    
051500        IF TAB-DIERS-TILLK(IX) > +0                                       
051600           COMPUTE TAB-DIERS-ERS(RAD-IX) =                                
051700                  TAB-DIERS-ERS(RAD-IX) * TAB-DIERS-TILLK(IX)             
051800           COMPUTE TAB-DIERS-TILLK(RAD-IX) =                              
051900                  TAB-DIERS-TILLK(RAD-IX) * TAB-DIERS-TILLK(IX)           
052000        END-IF                                                            
052100     END-IF                                                               
052200     MOVE LOPNR-WS TO TAB-IDLOPNR(RAD-IX)                                 
052300     ADD +1 TO KORTNR-WS                                                  
052400     MOVE KORTNR-WS TO TAB-IDKORTNR(RAD-IX)                               
052500     MOVE WS-KVKORT TO TAB-KVKORT (RAD-IX)                                
052600                                                                          
052700     IF TAB-KOD(RAD-IX) = 'TIL' AND VARIABEL-JA AND                       
052800        TAB-KDRESTR(RAD-IX) = +61 AND SLAUPP-JA                           
052900*-------* TIL-ART ÄR EJ ENTYDIG. EXTRA TEST-POST SKRIVS.                  
053000        ADD +1 TO RAD-IX                                                  
053100        MOVE WS-IDARTNR-URS TO TAB-IDARTNR-URS (RAD-IX)                   
053200        MOVE WS-KDUART-URS TO TAB-KDUART-URS (RAD-IX)                     
053300        MOVE +61 TO TAB-KDRESTR(RAD-IX)                                   
053400        MOVE 'SLÅ UPP ERSÄTTNING' TO TAB-BEERS(RAD-IX)                    
053500        MOVE 'TIL' TO TAB-KOD(RAD-IX)                                     
053600        MOVE LOPNR-WS TO TAB-IDLOPNR(RAD-IX)                              
053700        ADD +1 TO KORTNR-WS                                               
053800        MOVE KORTNR-WS TO TAB-IDKORTNR(RAD-IX)                            
053900        MOVE WS-KVKORT TO TAB-KVKORT (RAD-IX)                             
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 CC-SKRIV-UTPOSTER SECTION.                                               
054400     SKIP2                                                                
054500     IF TAB-KOD(IX) = 'URS'                                               
054600        MOVE +1 TO FOR-KDRADERS                                           
054700        MOVE +0 TO FOR-KDTILLK                                            
054800        PERFORM CCA-UTPOST                                                
054900     ELSE                                                                 
055000        IF TAB-KOD(IX) = 'TIL'                                            
055100           MOVE +0 TO FOR-KDRADERS                                        
055200           MOVE +1 TO FOR-KDTILLK                                         
055300           PERFORM CCA-UTPOST                                             
055400        END-IF                                                            
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 CCA-UTPOST SECTION.                                                      
055900     SKIP2                                                                
056000     MOVE '440'                     TO FOR-IDPTYP                         
056100     MOVE TAB-DIERS-ERS(IX)         TO FOR-DIERS-ERS                      
056200     MOVE TAB-DIERS-TILLK(IX)       TO FOR-DIERS-TILLK                    
056300     MOVE TAB-IDANSK(IX)            TO FOR-IDANSK                         
056400     MOVE TAB-IDARTNR(IX)           TO FOR-IDARTNR                        
056500     MOVE TAB-IDKORTNR(IX)          TO FOR-IDKORTNR-ERS                   
056600     MOVE TAB-IDLOPNR(IX)           TO FOR-IDLOPNRE                       
056700     MOVE TAB-IDDC(IX)              TO FOR-IDDC                           
056800     MOVE TAB-KDERS(IX)             TO FOR-KDERS                          
056900     MOVE TAB-KDLEVSP(IX)           TO FOR-KDLEVSP                        
057000     MOVE TAB-KDPRODSL(IX)          TO FOR-KDPRODSL                       
057100     MOVE TAB-KDRESTR(IX)           TO FOR-KDRESTR                        
057200     MOVE TAB-PRARTSTD(IX)          TO FOR-PRARTSTD                       
057300     MOVE TAB-REKSIFFR(IX)          TO FOR-REKSIFFR                       
057400     MOVE TAB-KVQPACK-1(IX)         TO FOR-KVQPACK-1                      
057500     MOVE TAB-BEERS(IX)             TO FOR-BEERS                          
057600     MOVE TAB-KDSORT(IX)            TO FOR-KDSORT                         
057700     MOVE TAB-TIDISPIN (IX)         TO FOR-TIDISPIN                       
057800     MOVE TAB-REDIRLEV (IX)         TO FOR-REDIRLEV                       
057900     MOVE TAB-KDUART (IX)           TO FOR-KDUART                         
058000     MOVE TAB-KDUART-URS (IX)       TO FOR-KDUART-URS                     
058100     MOVE TAB-IDARTNR-URS (IX)      TO FOR-IDARTNR-URS                    
058200     MOVE TAB-KVKORT (IX)           TO FOR-KVKORT                         
058300     MOVE SPACE                     TO FOR-BERADREF                       
058400                                       FOR-IDKUNDRF                       
058500                                       FOR-KDFAKTYP                       
058600                                       FOR-FLROSTYR                       
058700                                       FOR-KDSTARAD                       
058800                                       FOR-BEART                          
058900                                       FOR-KDPRTYP                        
059000                                       FOR-BEVOLREF                       
059100                                       FOR-FLINVEST                       
059200                                       FOR-FLPRTILL                       
059300                                       FOR-FLTPOBEK                       
059400                                       FOR-BEKUNDRF                       
059500                                       FOR-IDSYSTEM                       
059600                                       FOR-BEVARREF                       
059700                                       FOR-FLNYLARM                       
059800                                       FOR-IDDC-RO                        
059900                                       FOR-KDOI                           
060000                                       FOR-CLEARGROUP                     
060100                                       FOR-IDLEVNR                        
060200                                       FOR-KDORDTYP-LDC                   
060300                                       FOR-IDKUNDRF-WIP                   
060310                                       FOR-IDKST                          
060320                                       FOR-KDROPACK                       
060330                                       FOR-IDARBREF                       
060400     MOVE ZERO                      TO FOR-IDDISTR                        
060500                                       FOR-IDKONTO                        
060600                                       FOR-IDANALYS                       
060700                                       FOR-IDKUNDNR                       
060900                                       FOR-KDDSP                          
061000                                       FOR-KDFRAKT                        
061100                                       FOR-KDKVBRYT                       
061200                                       FOR-KDORDKL                        
061300                                       FOR-KDRAPRIO                       
061400                                       FOR-KDROO                          
061500                                       FOR-KDTPOTYP                       
061600                                       FOR-KDVRINFO                       
061700                                       FOR-KVART                          
061800                                       FOR-KVRO                           
061900                                       FOR-PRARTNTO                       
062000                                       FOR-TIREGDAT                       
062100                                       FOR-TIRES                          
062200                                       FOR-TIRODAT                        
062300                                       FOR-TITPO                          
062400                                       FOR-FLVR                           
062500                                       FOR-IDLOPNR                        
062600                                       FOR-KDORDING                       
062700                                       FOR-IDKAMPRF                       
062800                                       FOR-KVBEART-Q                      
062900                                       FOR-TIREGTID                       
063000                                       FOR-TIREPDAT                       
063100                                       FOR-TISENBEK-DAG                   
063200                                       FOR-TISENBEK-KL                    
063300                                       FOR-IDORDER                        
063400                                       FOR-TIORDREG                       
063500                                       FOR-IDANSK-LARM                    
063600                                       FOR-KDLARM                         
063700                                       FOR-TIREGDAT-LARM                  
063710                                       FOR-PRAVCOST                       
063800     INITIALIZE                        FOR-DEAL-PR-LINE                   
063900                                                                          
064000                                                                          
064100     WRITE UTPOST FROM UTAREA                                             
064200                                                                          
064300     MOVE 'W44020'           TO POSTSUM-FDNAMN                            
064400     MOVE 'W44020D2'         TO POSTSUM-DDNAMN2                           
064500     MOVE 'UT'               TO POSTSUM-TRANSTYP                          
064600     CALL POSTSUM USING POSTSUM-PARM                                      
064700     .                                                                    
064800     EJECT                                                                
064900 CD-RENSA-RAD SECTION.                                                    
065000     SKIP2                                                                
065100     MOVE ZERO TO          TAB-IDARTNR(RAD-IX)                            
065200                           TAB-IDKORTNR(RAD-IX)                           
065300                           TAB-IDLOPNR(RAD-IX)                            
065400                           TAB-KDRESTR(RAD-IX)                            
065500                           TAB-DIERS-ERS(RAD-IX)                          
065600                           TAB-DIERS-TILLK(RAD-IX)                        
065700                           TAB-IDANSK(RAD-IX)                             
065800                           TAB-KDERS(RAD-IX)                              
065900                           TAB-KDLEVSP(RAD-IX)                            
066000                           TAB-KDPRODSL(RAD-IX)                           
066100                           TAB-PRARTSTD(RAD-IX)                           
066200                           TAB-REKSIFFR(RAD-IX)                           
066300                           TAB-KVQPACK-1(RAD-IX)                          
066400                           TAB-TIDISPIN (RAD-IX)                          
066500                           TAB-REDIRLEV (RAD-IX)                          
066600                           TAB-IDARTNR-URS (RAD-IX)                       
066700                           TAB-KVKORT (RAD-IX)                            
066710                           TAB-TIFINLV (RAD-IX)                           
066720                           TAB-KVLS (RAD-IX)                              
066800     MOVE SPACE TO         TAB-IDDC(RAD-IX)                               
066900                           TAB-KOD(RAD-IX)                                
067000                           TAB-BEERS(RAD-IX)                              
067100                           TAB-KDSORT(RAD-IX)                             
067200                           TAB-KDUART (RAD-IX)                            
067300                           TAB-KDUART-URS (RAD-IX)                        
067400     SUBTRACT 1 FROM RAD-IX                                               
067500     .                                                                    
067600     EJECT                                                                
067700 Z-FINIT SECTION.                                                         
067800     SKIP2                                                                
067900     CLOSE  INFIL                                                         
068000            UTFIL                                                         
068100                                                                          
068200     MOVE 'S' TO POSTSUM-OPKOD                                            
068300     CALL POSTSUM USING POSTSUM-PARM                                      
068400     .                                                                    
068500     EJECT                                                                
068600 S01-LAES-SORTPOST SECTION.                                               
068700     SKIP2                                                                
068800     RETURN SORTFIL INTO IN-W440004                                       
068900       AT END MOVE JA TO SORTFIL-EOF                                      
069000     END-RETURN                                                           
069100                                                                          
069200     IF SORTFIL-EOF = NEJ                                                 
069300       MOVE 'W44020'         TO POSTSUM-FDNAMN                            
069400       MOVE 'W44020DS'       TO POSTSUM-DDNAMN2                           
069500       MOVE 'SORT'           TO POSTSUM-TRANSTYP                          
069600       CALL POSTSUM USING POSTSUM-PARM                                    
069700                                                                          
069800                                                                          
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 S02-LAES-INPOST SECTION.                                                 
070300     SKIP2                                                                
070400     READ INFIL INTO S-W440004                                            
070500       AT END MOVE JA TO INFIL-EOF                                        
070600     END-READ                                                             
070700                                                                          
070800     IF INFIL-EOF = NEJ                                                   
070900       RELEASE S-W440004                                                  
071000       MOVE 'W44020'         TO POSTSUM-FDNAMN                            
071100       MOVE 'W44020D1'       TO POSTSUM-DDNAMN2                           
071200       MOVE 'IN'             TO POSTSUM-TRANSTYP                          
071300       CALL POSTSUM USING POSTSUM-PARM                                    
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700*    ---- IMS SEKTIONER                                                   
071800                                                                          
071900 IMS-GU-WLARTC01 SECTION.                                                 
072000                                                                          
072100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
072200            DELIMITED BY SIZE INTO SSA1                                   
072300     MOVE '  ' TO GODK-STATUSKODER                                        
072400     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC01 SSA1                   
072500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     SKIP3                                                                
072900 IMS-GNP-WLARTC11 SECTION.                                                
073000                                                                          
073100     MOVE 'WLARTC11 ' TO SSA1                                             
073200     MOVE '  ' TO GODK-STATUSKODER                                        
073300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
073400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
073500     PERFORM IMS-STATUSKONTROLL                                           
073600     .                                                                    
073700     EJECT                                                                
073800 IMS-GU-WDK711 SECTION.                                                   
073900                                                                          
074000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
074100            DELIMITED BY SIZE INTO SSA1                                   
074200     STRING 'WDK711  (IDDC     =' W-IDDC-X    ')'                         
074300            DELIMITED BY SIZE INTO SSA2                                   
074400     MOVE '  GE' TO GODK-STATUSKODER                                      
074500     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
074600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900     EJECT                                                                
074910 IMS-GU-WDK722 SECTION.                                                   
074920                                                                          
074930     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
074940            DELIMITED BY SIZE INTO SSA1                                   
074950     STRING 'WDK711  (IDDC     =' W-IDDC-X    ')'                         
074960            DELIMITED BY SIZE INTO SSA2                                   
074961     MOVE 'WDK722 '             TO SSA3                                   
074970     MOVE '  GE' TO GODK-STATUSKODER                                      
074980     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
074990     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
074991     PERFORM IMS-STATUSKONTROLL                                           
074992     .                                                                    
074993     EJECT                                                                
075000 IMS-GU-WLERSA01 SECTION.                                                 
075100                                                                          
075200     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
075300            DELIMITED BY SIZE INTO SSA1                                   
075400     MOVE '  GE' TO GODK-STATUSKODER                                      
075500     CALL CBLTDLI USING GU  ERSA-PCB DLI-IO-ERSA01 SSA1                   
075600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     SKIP3                                                                
076000 IMS-GNP-WLERSA11 SECTION.                                                
076100                                                                          
076200     MOVE 'WLERSA11 ' TO SSA1                                             
076300     MOVE '  GEGA' TO GODK-STATUSKODER                                    
076400     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-ERSA11 SSA1                   
076500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     EJECT                                                                
076900 IMS-GU-WDB601    SECTION.                                                
077000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
077100          DELIMITED BY SIZE INTO SSA1                                     
077200     MOVE '  GE' TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
077400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     IF SEGMENT-SAKNAS                                                    
077700         MOVE SPACE TO DCS-KDDC                                           
077800     END-IF                                                               
077900     .                                                                    
078000 IMS-STATUSKONTROLL SECTION.                                              
078100                                                                          
078200     SET STATUS-IX TO 1                                                   
078300     SEARCH GODK-STATUS                                                   
078400       AT END                                                             
078500         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
078600           DELIMITED BY SIZE INTO FELTEXT                                 
078700         CALL FELLOG                                                      
078800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078900         CONTINUE                                                         
079000     END-SEARCH.                                                          
079100*    -COPY WY2000P2                                                       
