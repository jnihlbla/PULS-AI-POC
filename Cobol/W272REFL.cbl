000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W272REFL.                                                
000400 AUTHOR.         JOHAN NIHLBLAD.                                          
000500 DATE-WRITTEN.   15/10/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBPROGRAM SOM BERÄKNAR PÅFYLLNADS-PUNKT, -KVANTITET SAMT        
001000*        ÖVERLAGERPUNKT                                                   
001100*                                                                         
001200*!!!!!OBS MAN FLYTTAR IN MATERIALPRIS(PRMATRL) TILL BESTÄLLNINGS-         
001300*!!!!!PRIS(PRARTBES) FÖR KINA ARTIKLAR!!!!!!!                             
001400*                                                                         
001500*        PROGRAMMET LÄSER WL2501 (WDR2)                                   
001600*                         WDB6                                            
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800     SKIP3                                                                
003900*    -COPY WY2000W3                                                       
004000     SKIP2                                                                
004100 77  IDPGM                       PIC X(8)    VALUE 'W272REFL'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400     EJECT                                                                
004500                                                                          
004600 01  ARBETSFAELT.                                                         
004700*                                                                         
004800     03 DAGENS-DATUM.                                                     
004900         05 DAGENS-AAR           PIC 9(4).                                
005000         05 DAGENS-MAANAD        PIC 9(2).                                
005100         05 DAGENS-DAG           PIC 9(2).                                
005200     03 DAGENS-AAR-PLUS2         PIC 9(4)    VALUE ZERO.                  
005300     03 JMF-AAAA                 PIC 9(4)    VALUE ZERO.                  
005400     03 WS-DAGENS-DATUM-AAVV     PIC 9(4)    VALUE ZERO.                  
005500     03 WS-CURRWK-AAVV-COMP      PIC S9(5)   VALUE ZERO  COMP-3.          
005600     03 WS-CURRWK-PLUS-1-AAVV    PIC 9(4)    VALUE ZERO.                  
005700                                                                          
005800*                                                                         
005900     03  WS-SNITT-VECKOR         PIC 9(1)V9(2)  VALUE 4.33.               
006000     03  WS-DAGAR-LO2530         PIC 9(1)V9(1)  VALUE 9.5.                
006100     03  WS-LEADTIME-WEEKS       PIC 9(3)     VALUE ZERO.                 
006200     03  WS-CALC-AAVV            PIC 9(4)     VALUE ZERO.                 
006300     03  WS-REST-DAYS            PIC 9(2)     VALUE ZERO.                 
006400     03  WS-TIAAVV               PIC 9(4)     VALUE ZERO.                 
006500     03  WS-TIAAVV-PLUS1         PIC 9(4)     VALUE ZERO.                 
006600     03  WS-TIAAVVD-PLUS1        PIC 9(4)     VALUE ZERO.                 
006700     03  WS-KVDAYS               PIC 9(3)     VALUE ZERO.                 
006800     03  WS-DAT-TID              PIC 9        VALUE ZERO.                 
006900     03  WS-NONEED-DAYS          PIC 9        VALUE ZERO.                 
007000     03  WS-LAST-MINUS-NEED      PIC 9(7)     VALUE ZERO.                 
007100     03  WS-DAY-NEED-LAST        PIC 9(7)     VALUE ZERO.                 
007200     03  WS-TIAAMMDD             PIC 9(6)     VALUE ZERO.                 
007210     03  WS-RESSFAC              PIC 9(1)V9(2)  VALUE 1.00.               
007300*                                                                         
007400     03  WS-N                 PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
007500     03  WS-NP                PIC S9(10)V9(5) VALUE ZERO COMP-3.          
007600     03  WS-UB                PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
007700     03  WS-NP-UB             PIC S9(10)V9(5) VALUE ZERO COMP-3.          
007800     03  WS-LEDTIDSBEHOV      PIC S9(7)V9(2)  VALUE ZERO COMP-3.          
007900     03  WS-TABELLBEHOV       PIC S9(6)V9(3)  VALUE ZERO COMP-3.          
008000     03  WS-PAFYLLNPKT        PIC S9(7)V9(5)  VALUE ZERO COMP-3.          
008100     03  WS-PAFYLLNKVANT      PIC S9(7)V9(5)  VALUE ZERO COMP-3.          
008200     03  WS-OVERLAGERPKT      PIC S9(7)       VALUE ZERO COMP-3.          
008300     03  WS-KVPB-PLAN-DAY-BINN PIC S9(6)V9(5) VALUE ZERO COMP-3.          
008400     03  WS-KVPB-JUST         PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
008500     03  WS-KVPB-UNDER        PIC S9(6)V9(1)  VALUE ZERO COMP-3.          
008600                                                                          
008700     03  WS-SP-IDDC              PIC X(2)    VALUE SPACE.                 
008800*                                                                         
008900     03  WS-KLASS.                                                        
009000         05  WS-KLASS-RAD        PIC 9(2)    VALUE ZERO.                  
009100         05  WS-KLASS-RAD-X REDEFINES WS-KLASS-RAD.                       
009200             07  FILLER          PIC X(2).                                
009300         05  WS-KLASS-KOL        PIC X(1)    VALUE SPACE.                 
009400*                                                                         
009500     03  W-ANTAL-VECKOR            PIC 9(3)       VALUE ZERO              
009600                                                   COMP-3.                
009700     03  WS-PRARTBES               PIC S9(7)V9(2) VALUE ZERO              
009800                                                   COMP-3.                
009900     03  WS-KVPB-TOT               PIC S9(6)V9(1) VALUE ZERO              
010000                                                   COMP-3.                
010100     03  WS-KVPBREOI-DAY           PIC S9(6)V9(5) VALUE ZERO              
010200                                                   COMP-3.                
010300     03  WS-KVREFLIM               PIC S9(5)      VALUE ZERO              
010400                                                   COMP-3.                
010500     03  WS-KVREFKVA               PIC S9(5)      VALUE ZERO              
010600                                                   COMP-3.                
010700     03  WS-BINNDAY-TIAARP     PIC 9(4)    VALUE ZERO.                    
010800     03  FILLER REDEFINES WS-BINNDAY-TIAARP.                              
010900         05 WS-BINNDAY-TIAA    PIC 9(2).                                  
011000         05 WS-BINNDAY-TIRP    PIC 9(2).                                  
011100                                                                          
011200     03  WS-TIAAVVD-FORECAST    PIC 9(5)    VALUE ZERO.                   
011300     03  FILLER REDEFINES WS-TIAAVVD-FORECAST.                            
011400         05 WS-FORECAST-TIAAVV  PIC 9(4).                                 
011500         05 WS-FORECAST-TID     PIC 9(1).                                 
011600                                                                          
011700     03  WS-RADIX                  PIC 9(2)    VALUE ZERO.                
011800     03  WS-PR-KOLIX               PIC 9(2)    VALUE ZERO.                
011900     03  IX                        PIC 9(2)    VALUE ZERO.                
012000     03  PER-IX                    PIC 9(2)    VALUE ZERO.                
012100     03  LT-IX                     PIC 9(3)    VALUE ZERO.                
012200     03  IX-WEEK                   PIC 9(3)    VALUE ZERO.                
012300     03  LAST-IX                   PIC 9(3)    VALUE ZERO.                
012400     03  INDX                      PIC 9(3)    VALUE ZERO.                
012500     03  MAX-INDX                  PIC 9(3)    VALUE ZERO.                
012600     03  TAB-KVARBDAG              PIC 9(3)    VALUE ZERO                 
012700                                   OCCURS 12.                             
012800     03  TAB-WEEK-NEED             PIC 9(7)    VALUE ZERO                 
012900                                   OCCURS 90.                             
013000     03  SPARAREOR.                                                       
013100                                                                          
013200         05  SPARAREA-AKTUELL-TABELL.                                     
013300*            07 -COPY WDGX2502 -PRE AKTTAB-                               
013400                                                                          
013500         05  SPARAREA-GRUNDTABELL.                                        
013600*            07 -COPY WDGX2502 -PRE GRUNDTAB-                             
013700                                                                          
013800     03  TE-TABELL-SAKNAS.                                                
013900         05 TABELL-SAKNAS-TEXT    PIC X(22)                               
014000                           VALUE 'TABELL SAKNAS FÖR DC '.                 
014100         05 TABELL-SAKNAS-DC PIC X(2).                                    
014200                                                                          
014300     03  TE-TABELL-FEL.                                                   
014400         05 TABELL-FEL-TEXT    PIC X(22)                                  
014500                           VALUE 'TABELL FEL FÖR DC    '.                 
014600         05 TABELL-FEL-DC PIC X(2).                                       
014700                                                                          
014800                                                                          
014900 01  FILLER                    PIC X(24)  VALUE 'SWITCHAR'.               
015000                                                                          
015100 77  GRUNDTABELL-SW              PIC X       VALUE 'N'.                   
015200     88  GRUNDTABELL                         VALUE 'J'.                   
015300     88  AKTUELL-TABELL                      VALUE 'N'.                   
015400                                                                          
015500 77  WS-IDREFTAB-ID              PIC X(1).                                
015600     88 VALID-IDREFTAB-ID                    VALUE 'A' THRU 'Z'.          
015700                                                                          
015800                                                                          
015900*      --- VALID IDDC CODES                                               
016000*                                                                         
016100*01    -COPY WWDC99 -PRE REF-                                             
016200       EJECT                                                              
016300*                                                                         
016400     EJECT                                                                
016500 01  DYNAMISKA-SUBPROGRAM.                                                
016600*                                                                         
016700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
017200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017300     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
017400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017500     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
017600     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
017700     EJECT                                                                
017800*    --- PARAMETRAR TILL POSTSUM                                          
017900*                                                                         
018000*01  -COPY W0005   -PRE  POSTSUM-                                         
018100     EJECT                                                                
018200*    ---PARAMETRAR TILL WORKDAY                                           
018300*01  -COPY WORKAREA                                                       
018400     EJECT                                                                
018500*    ---PARAMETRAR TILL DATKONV                                           
018600*01  -COPY WDATAREA                                                       
018700     EJECT                                                                
018800*    ---PARAMETRAR TILL DAGKONV                                           
018900*01  -COPY WDAGAREA                                                       
019000     EJECT                                                                
019100*    --- PARAMETRAR TILL W271UTIL                                         
019200*01 -COPY W271UTIL                                                        
019300*    --- PARAMETRAR TILL WZ20DAYS                                         
019400*01 -COPY WZ20DAYS                                                        
019500     EJECT                                                                
019600* VARIABLES TO SUBPROGRAM W009VADD                                        
019700 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
019800 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
019900     EJECT                                                                
020000*    --- PARAMETRAR TILL ABEND                                            
020100                                                                          
020200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
020400     SKIP2                                                                
020500 01  FELTEXT.                                                             
020600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
020700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020800     EJECT                                                                
020900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021000*                                                                         
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021300     SKIP3                                                                
021400 01  NYCKLAR-TILL-DLI.                                                    
021500     03  W-WDGXKEY-X.                                                     
021600         05  W-IDHTYP           PIC X(4)    VALUE '2501'.                 
021700         05  W-IDDC             PIC X(2)    VALUE ZERO.                   
021800         05  W-LOWVALUE         PIC X(24)   VALUE LOW-VALUE.              
021900     03  W-IDDC-B6-X.                                                     
022000         05  W-IDDC-B6          PIC X(2)    VALUE SPACE.                  
022100     03  W-IDDC-B616-X.                                                   
022200         05  W-IDDC-B616        PIC X(2)    VALUE SPACE.                  
022300*                                                                         
022400     03  W-IDREFTAB-X.                                                    
022500         05  W-IDREFTAB         PIC X(1)    VALUE SPACE.                  
022600*                                                                         
022700     03  W-IDARTNR-X.                                                     
022800         05  W-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.            
022900                                                                          
023000     03  W-KDSEGKEY-X.                                                    
023100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
023200                                                                          
023300     SKIP2                                                                
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  SEGMENT-FINNS                       VALUE '  '.                  
023700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023800     SKIP2                                                                
023900 01  GODK-STATUSKODER.                                                    
024000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024100     SKIP3                                                                
024200 01  ALL-SSA.                                                             
024300     03  SSA1                    PIC X(128).                              
024400     03  SSA2                    PIC X(64).                               
024500     EJECT                                                                
024600*    --- IMS FUNKTIONSKODER                                               
024700*01  -COPY W0003                                                          
024800     EJECT                                                                
024900*    ---  DLI INPUT-OUTPUT AREA                                           
025000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025100     SKIP3                                                                
025200 01  DLI-IO-AREA.                                                         
025300     03  IO-AREA                 PIC X(1036)   VALUE SPACE.               
025400     03  WL250101 REDEFINES IO-AREA.                                      
025500*        05  -COPY WDGX2501                                               
025600                                                                          
025700     03  WL250111 REDEFINES IO-AREA.                                      
025800*        05  -COPY WDGX2502                                               
025900     EJECT                                                                
026000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026100 01   DLI-IO-AREA-B601.                                                   
026200*     03  -COPY WDB601                                                    
026300                                                                          
026400 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
026500 01   DLI-IO-AREA-B616.                                                   
026600*     03  -COPY WDB616                                                    
026700                                                                          
026800 01  FILLER               PIC X(16)   VALUE 'DC-REF AREA'.                
026900 01  TAB-IX               PIC S9(4)   COMP    VALUE ZERO.                 
027000 01  MAX-TAB-IX           PIC S9(4)   COMP    VALUE ZERO.                 
027100 01  IDDC-REF-TABELL.                                                     
027200     03  FILLER OCCURS 30.                                                
027300*      05  -COPY WDB616  -PRE TAB-                                        
027400                                                                          
027500     EJECT                                                                
027600 LINKAGE SECTION.                                                         
027700                                                                          
027800*    -COPY W272REFL                                                       
027900                                                                          
028000     EJECT                                                                
028100*01  -COPY W0008  -PRE 2501-                                              
028200     05  FILLER                  PIC X.                                   
028300     EJECT                                                                
028400*01  -COPY W0008      -PRE WDB6-                                          
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700 01  UTIL-WDK6-PCB               PIC X.                                   
028800 01  UTIL-WDK7-PCB               PIC X.                                   
028900 01  UTIL-WDB6-PCB               PIC X.                                   
029000     EJECT                                                                
029100                                                                          
029200 PROCEDURE DIVISION  USING REFL-W272REFL 2501-PCB                         
029300                           WDB6-PCB                                       
029400                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
029500                           UTIL-WDB6-PCB.                                 
029600                                                                          
029700     PERFORM A-INIT                                                       
029800     PERFORM D-NOLLSTALL-ARBETSFALT                                       
029900                                                                          
030000     PERFORM C-HAEMTA-LEDTID                                              
030100                                                                          
030200*    - NYCKEL TILL 2501/2502                                              
030300     MOVE REFL-IDDC  TO W-IDDC                                            
030400                                                                          
030500     PERFORM B-LAS-2501                                                   
030600                                                                          
031000     PERFORM F-BERAKNA-PUNKTER                                            
031200                                                                          
031300     MOVE ZERO TO RETURN-CODE                                             
031400     GOBACK                                                               
031500     .                                                                    
031600     EJECT                                                                
031700                                                                          
031800                                                                          
031900 A-INIT SECTION.                                                          
032000                                                                          
032100     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
032200     COMPUTE DAGENS-AAR-PLUS2 = DAGENS-AAR + 2                            
032300                                                                          
032400*---                                                                      
032500*--- CHECK DATE AND CONVERT TO FORMAT AAVV                                
032600*---                                                                      
032700     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
032800     MOVE DAGENS-DATUM (3:6)       TO DAT-I-TIDATUM                       
032900                                      WS-TIAAMMDD                         
033000                                                                          
033100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
033200                         DAT-O-TIDATUM DAT-KDSVAR                         
033300                                                                          
033400     IF DAT-KDSVAR-OK                                                     
033500        MOVE DAT-TIAAVV-GRP        TO WS-DAGENS-DATUM-AAVV                
033600        MOVE DAT-TID               TO WS-DAT-TID                          
033700     ELSE                                                                 
033800        STRING ' FEL FRÅN WDATKONV I W272REFL'                            
033900               ' (A-INIT) '                                               
034000        DELIMITED BY SIZE INTO FELTEXT-STR                                
034100        DISPLAY FELTEXT                                                   
034200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
034300     END-IF                                                               
034400*                                                                         
034500     IF REFL-IDDC NOT = W-IDDC-B6                                         
034600       MOVE REFL-IDDC TO W-IDDC-B6                                        
034700       PERFORM IMS-GU-WDB601                                              
034800                                                                          
034900*      SPARA ALLA WDB616-SEGMENT FÖR DETTA DC.                            
035000*      TAB-IX 1 ANVÄNDS NÄR REF-IDDC-REF = SPACE                          
035100*      TAB-IX 2-20 KOMMER FRÅN WDB616                                     
035200*                                                                         
035300       MOVE 1 TO TAB-IX                                                   
035400       MOVE SPACE      TO TAB-REF-IDDC-REF       (TAB-IX)                 
035500       MOVE ZERO       TO TAB-REF-KVDLTID-TOT    (TAB-IX)                 
035600                          TAB-REF-KVDLTID-BOATPAC(TAB-IX)                 
035700                          TAB-REF-KVDLTID-BOATTRP(TAB-IX)                 
035800                          TAB-REF-KVDLTID-BOAT2DC(TAB-IX)                 
035900                          TAB-REF-KVDLTID-BOATINS(TAB-IX)                 
036000                          TAB-REF-KVDLTID-AIRREQ (TAB-IX)                 
036100                          TAB-REF-KVDLTID-AIRETA (TAB-IX)                 
036110       MOVE 1.00       TO TAB-REF-RESSFAC        (TAB-IX)                 
036200                                                                          
036300       PERFORM IMS-GNP-WDB616                                             
036400       PERFORM UNTIL SEGMENT-SAKNAS OR TAB-IX > 30                        
036500         ADD 1 TO TAB-IX                                                  
036600         MOVE DLI-IO-AREA-B616   TO TAB-REF-WDB616 (TAB-IX)               
036700         PERFORM IMS-GNP-WDB616                                           
036800       END-PERFORM                                                        
036900       MOVE TAB-IX TO MAX-TAB-IX                                          
037000       IF TAB-IX > 30                                                     
037100         MOVE 'MER ÄN 30 WDB616. ÖKA TABELL' TO FELTEXT-STR               
037200         DISPLAY FELTEXT                                                  
037300         PERFORM S99-ABEND                                                
037400       END-IF                                                             
037500                                                                          
037600*      -- TRIGGA LÄSNING AV REFILLTABELL                                  
037700       MOVE '?'   TO W-IDREFTAB                                           
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100                                                                          
038200                                                                          
038300 B-LAS-2501 SECTION.                                                      
038400***                                                                       
038500*   LÄS HÄNDELSEBASEN WL2501 MED OLIKA NYCKLAR                            
038600*   REFILLEN HAR VÄRDE 0-9 MED 0 FÖR GRUNDTABELLEN SOM ANVÄNDS            
038700*   NÄR EN SPECIFIK TABELL INTE FINNS                                     
038800***                                                                       
038900                                                                          
039000     IF REFL-IDDC NOT = WS-SP-IDDC                                        
039100*    - KANSKE FÖREGÅENDE GRUNDTABELL VAR FEL SORT (ANSKAFFNING)           
039200     OR GRUNDTAB-2502-IDREFTAB = 'A'                                      
039300*      -- LÄS GRUNDTABELL TYP REFILL FÖR DETTA DC                         
039400       MOVE '0'  TO W-IDREFTAB                                            
039500       PERFORM IMS-GU-2502                                                
039600       IF SEGMENT-SAKNAS                                                  
039700           MOVE REFL-IDDC TO TABELL-SAKNAS-DC                             
039800           MOVE TE-TABELL-SAKNAS TO FELTEXT-STR                           
039900           DISPLAY FELTEXT                                                
040000           PERFORM S99-ABEND                                              
040100       ELSE                                                               
040200          MOVE 2502-WDGX2502 TO SPARAREA-GRUNDTABELL                      
040300       END-IF                                                             
040400     END-IF                                                               
040500                                                                          
040600     IF REFL-IDDC NOT = WS-SP-IDDC                                        
040700     OR REFL-IDREFTAB NOT = W-IDREFTAB                                    
040800*      - LÄS ANGIVEN TABELL FÖR DETTA DC.                                 
040900       MOVE REFL-IDREFTAB TO W-IDREFTAB                                   
041000       PERFORM IMS-GU-2502                                                
041100       IF SEGMENT-FINNS                                                   
041200         MOVE 2502-WDGX2502 TO SPARAREA-AKTUELL-TABELL                    
041300       ELSE                                                               
041400*        - SÄKERSTÄLL ATT AKTTAB INTE ANVÄNDS                             
041500         MOVE '?' TO AKTTAB-2502-IDREFTAB                                 
041600       END-IF                                                             
041700     END-IF                                                               
041800                                                                          
041900*    - KOM IHÅG OM DET ÄR GRUNDTABELLEN ELLER ANGIVEN TABELL              
042000*    - SOM SKA ANVÄNDAS.                                                  
042100     IF REFL-IDREFTAB = AKTTAB-2502-IDREFTAB                              
042200       MOVE NEJ TO GRUNDTABELL-SW                                         
042300     ELSE                                                                 
042400       MOVE JA  TO GRUNDTABELL-SW                                         
042500     END-IF                                                               
042600                                                                          
042700*    - KOM IHÅG TILL NÄSTA ANROP FÖR VILKET DC SOM TABELLER LÄSTS         
042800     MOVE REFL-IDDC TO WS-SP-IDDC                                         
042900     .                                                                    
043000                                                                          
043100     EJECT                                                                
043200                                                                          
043300 C-HAEMTA-LEDTID SECTION.                                                 
043400***                                                                       
043500*   HÄMTA LEDTID FÖR ETT DC FRÅN SPARADE WDB616 SOM ÄNVÄNDS               
043600*   VID BERÄKNINGEN AV PÅFYLLNADSPUNKT                                    
043700***                                                                       
043800                                                                          
043900     MOVE 1 TO TAB-IX                                                     
044000     PERFORM UNTIL TAB-IX > MAX-TAB-IX OR                                 
044100                   REFL-IDDC-REF = TAB-REF-IDDC-REF (TAB-IX)              
044200       ADD 1 TO TAB-IX                                                    
044300     END-PERFORM                                                          
044400     IF TAB-IX <= MAX-TAB-IX                                              
044500*      FLYTTA SPARAT WDB616-SEGMENT TILL DLI-IO-WDB616                    
044600       MOVE TAB-REF-WDB616 (TAB-IX) TO REF-WDB616                         
044700     ELSE                                                                 
044800       MOVE 'EJ TRÄFF I TAB-REF-WDB616' TO FELTEXT-STR                    
044900       DISPLAY FELTEXT                                                    
045000       PERFORM S99-ABEND                                                  
045100     END-IF                                                               
045200                                                                          
045210     MOVE REF-RESSFAC           TO WS-RESSFAC                             
045220                                                                          
045300     IF REFL-FLFLYG = 'J'                                                 
045400       MOVE REF-KVDLTID-AIRETA  TO WS-KVDAYS                              
045500     ELSE                                                                 
045600       MOVE REF-KVDLTID-TOT     TO WS-KVDAYS                              
045700     END-IF                                                               
045800**ADD ONE WEEK TO THE LEADTIME TO GET THE TOTAL FORECAST                  
045900**AFTER THE TOTAL LEADTIME PLUS ONE WEEK                                  
046000     COMPUTE DAYS-KVDAYS = WS-KVDAYS + 6                                  
046100     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
046200     IF REFL-BINNDAY-TIAAMMDD = ZERO                                      
046300       MOVE WS-TIAAMMDD           TO DAT-I-TIDATUM                        
046400     ELSE                                                                 
046500       MOVE REFL-BINNDAY-TIAAMMDD TO DAT-I-TIDATUM                        
046600     END-IF                                                               
046700     CALL WDATKONV USING   DAT-KDDATFORM                                  
046800                           DAT-I-TIDATUM                                  
046900                           DAT-O-TIDATUM                                  
047000                           DAT-KDSVAR                                     
047100                                                                          
047200     IF DAT-KDSVAR-OK                                                     
047300        MOVE DAT-TIAAVVD      TO DAYS-TIDATE1                             
047400     END-IF                                                               
047500                                                                          
047600     MOVE 'YYWWD'                   TO DAYS-KDDATFMT1                     
047700     MOVE 'YYWWD'                   TO DAYS-KDDATFMT2                     
047800     MOVE SPACE                      TO DAYS-TIDATE2                      
047900                                      DAYS-IDCALEND                       
048000                                                                          
048100     INITIALIZE UTIL-W271UTIL                                             
048200                                                                          
048300     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
048400                                                                          
048500     IF DAYS-KDRC = 8                                                     
048600       STRING 'FEL VID ANROP TILL WZ20DAYS - SEC C-'                      
048700       DELIMITED BY SIZE INTO FELTEXT-STR                                 
048800       DISPLAY FELTEXT                                                    
048900       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
049000     ELSE                                                                 
049100       MOVE DAYS-TIDATE2 (1:5)        TO UTIL-TIAAVVD                     
049200     END-IF                                                               
049300     MOVE REFL-IDARTNR              TO UTIL-IDARTNR                       
049400     MOVE REFL-IDDC                 TO UTIL-IDDC                          
049500     MOVE REFL-IDDC-REF             TO UTIL-IDDC-REF                      
049700     MOVE 003                       TO UTIL-KDCALL                        
049800     CALL W271UTIL USING UTIL-W271UTIL                                    
049900                         UTIL-WDK6-PCB                                    
050000                         UTIL-WDK7-PCB                                    
050100                         UTIL-WDB6-PCB                                    
050200                                                                          
050300     IF UTIL-KDSVAR-OK                                                    
050400        MOVE UTIL-KVPB-TOT        TO WS-KVPB-TOT                          
050500        IF WS-KVPB-TOT > ZERO                                             
050600          COMPUTE WS-KVPB-PLAN-DAY-BINN = (WS-KVPB-TOT / 4.33) / 5        
050700        ELSE                                                              
050800          MOVE ZERO     TO WS-KVPB-PLAN-DAY-BINN                          
050900        END-IF                                                            
051000     ELSE                                                                 
051100        MOVE 'FEL FRÅN W271UTIL '                                         
051200                                TO FELTEXT-STR                            
051300        DISPLAY FELTEXT                                                   
051400        PERFORM S99-ABEND                                                 
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900                                                                          
052000 D-NOLLSTALL-ARBETSFALT SECTION.                                          
052100                                                                          
052200     MOVE ZERO TO        WS-N                                             
052300                         WS-NP                                            
052400                         WS-UB                                            
052500                         WS-NP-UB                                         
052600                         WS-LEDTIDSBEHOV                                  
052700                         WS-TABELLBEHOV                                   
052800                         WS-PAFYLLNPKT                                    
052900                         WS-PAFYLLNKVANT                                  
053000                         WS-OVERLAGERPKT                                  
053100                         WS-KVPB-PLAN-DAY-BINN                            
053200                         WS-LEADTIME-WEEKS                                
053300                         WS-REST-DAYS                                     
053400                         WS-CALC-AAVV                                     
053500                         WS-NONEED-DAYS                                   
053600                         WS-LAST-MINUS-NEED                               
053700                         WS-DAY-NEED-LAST                                 
053800                         WS-KVPB-TOT                                      
053900                         WS-KVDAYS                                        
053910     MOVE 1.00        TO WS-RESSFAC                                       
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300                                                                          
055800 F-BERAKNA-PUNKTER SECTION.                                               
055900                                                                          
056000                                                                          
056100     PERFORM S07-PLATS-I-TABELL-NU                                        
056200     PERFORM S06-HAEMTA-KLASS                                             
056300     PERFORM S05-LEDTIDSBEHOV                                             
056400     PERFORM S01-PAFYLLNPUNKT                                             
056500     PERFORM S02-PAFYLLNKVANT                                             
056600     PERFORM S03-OVERLAGERPUNKT                                           
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 S01-PAFYLLNPUNKT SECTION.                                                
057100                                                                          
057200     PERFORM S01B-PAFYLLNPKT-NORMAL                                       
057300     IF REFL-IN-KVREFPKT > 0                                              
057400*MANUELL PÅFYLLNADSPUNKT GÄLLER                                           
057500        MOVE REFL-IN-KVREFPKT TO WS-PAFYLLNPKT                            
057600                                 REFL-KVREFPKT                            
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000                                                                          
058100                                                                          
058200 S01B-PAFYLLNPKT-NORMAL SECTION.                                          
058300                                                                          
058310     IF WS-KVPB-TOT < 0.6                                                 
058311     OR REFL-FLFLYG = JA                                                  
058340        MOVE 1.00     TO WS-RESSFAC                                       
058350     END-IF                                                               
058360                                                                          
058400     IF GRUNDTABELL                                                       
058500        IF GRUNDTAB-2502-KDREFPKT-LIM(WS-RADIX, WS-PR-KOLIX) = 'D'        
058600                                                                          
058700          MOVE GRUNDTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX)             
058800                             TO WS-KVREFLIM                               
058810* IN S11- YOU TAKE SS FACTOR(RESSFAC) IN CONSIDERATION                    
058900          PERFORM S11-TAB-BEHOV-PKT                                       
059000          COMPUTE REFL-KVSLAGER ROUNDED = WS-TABELLBEHOV * 1              
059400          COMPUTE WS-PAFYLLNPKT =                                         
059500                          ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)             
059700                                                                          
059800          IF WS-PAFYLLNPKT > ZERO                                         
059900          AND WS-PAFYLLNPKT < 1.0                                         
060000              MOVE 1         TO REFL-KVREFPKT                             
060100          ELSE                                                            
060200             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
060300          END-IF                                                          
060400                                                                          
060500        ELSE                                                              
060600          COMPUTE WS-TABELLBEHOV =                                        
060700              (GRUNDTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX))            
060710*                                                                         
060800* CALCULATE TABELLBEHOV(SAFETY STOCK) WITH SS FACTOR                      
060810          COMPUTE WS-TABELLBEHOV = WS-TABELLBEHOV *                       
060820                                   WS-RESSFAC                             
060830*                                                                         
060900          MOVE WS-TABELLBEHOV   TO REFL-KVSLAGER                          
061300          COMPUTE WS-PAFYLLNPKT =                                         
061400                          ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)             
061600                                                                          
061700          IF WS-PAFYLLNPKT > ZERO                                         
061800          AND WS-PAFYLLNPKT < 1.0                                         
061900              MOVE 1         TO REFL-KVREFPKT                             
062000          ELSE                                                            
062100             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
062200          END-IF                                                          
062300        END-IF                                                            
062400     ELSE                                                                 
062500        IF AKTTAB-2502-KDREFPKT-LIM (WS-RADIX, WS-PR-KOLIX) = 'D'         
062600                                                                          
062700          MOVE AKTTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX)               
062800                             TO WS-KVREFLIM                               
062810* IN S11- YOU TAKE SS FACTOR(RESSFAC) IN CONSIDERATION                    
062900          PERFORM S11-TAB-BEHOV-PKT                                       
063000                                                                          
063100          COMPUTE REFL-KVSLAGER ROUNDED = WS-TABELLBEHOV * 1              
063500          COMPUTE WS-PAFYLLNPKT =                                         
063600                          ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)             
063800                                                                          
063900          IF WS-PAFYLLNPKT > ZERO                                         
064000          AND WS-PAFYLLNPKT < 1.0                                         
064100              MOVE 1         TO REFL-KVREFPKT                             
064200          ELSE                                                            
064300             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
064400          END-IF                                                          
064500                                                                          
064600        ELSE                                                              
064700          COMPUTE WS-TABELLBEHOV =                                        
064800             (AKTTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX))               
064810*                                                                         
064820* CALCULATE TABELLBEHOV(SAFETY STOCK) WITH SS FACTOR                      
064830          COMPUTE WS-TABELLBEHOV = WS-TABELLBEHOV *                       
064840                                   WS-RESSFAC                             
064850*                                                                         
065000          COMPUTE REFL-KVSLAGER ROUNDED = WS-TABELLBEHOV * 1              
065300          COMPUTE WS-PAFYLLNPKT =                                         
065400                          ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)             
065600                                                                          
065700          IF WS-PAFYLLNPKT > ZERO                                         
065800          AND WS-PAFYLLNPKT < 1.0                                         
065900              MOVE 1         TO REFL-KVREFPKT                             
066000          ELSE                                                            
066100             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
066200          END-IF                                                          
066300        END-IF                                                            
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 S02-PAFYLLNKVANT SECTION.                                                
066900                                                                          
067000                                                                          
067100     IF REFL-IN-KVREFBER > 0                                              
067200*                                                                         
067300*----  MANUELL PÅFYLLNADSKVANTITET ÄR SATT                                
067400*                                                                         
067500        MOVE REFL-IN-KVREFBER TO WS-PAFYLLNKVANT                          
067600                                REFL-KVREFBER                             
067700     ELSE                                                                 
067800        IF REFL-FLWILSON = JA                                             
067900           PERFORM S02B-KVANT-M-WILSON                                    
068000        ELSE                                                              
068100           PERFORM S02C-KVANT-M-TABELL                                    
068200        END-IF                                                            
068300                                                                          
068400        IF WS-PAFYLLNKVANT > ZERO                                         
068500        AND WS-PAFYLLNKVANT < 1.0                                         
068600            MOVE 1           TO REFL-KVREFBER                             
068700        ELSE                                                              
068800           COMPUTE REFL-KVREFBER ROUNDED = WS-PAFYLLNKVANT * 1            
068900        END-IF                                                            
069000     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069300                                                                          
069400                                                                          
069500 S02B-KVANT-M-WILSON SECTION.                                             
069600                                                                          
069700*WILSONFORMEL FÖR BERÄKNING AV PÅFYLLNADSKVANT                            
069800*KVANT = ROTEN UR ((2N * P)/(U * B)                                       
069900*                                                                         
070000*     N = ÅRSBEHOV, P = ORDERSÄRKOSTNAD, U = LAGERSÄRKOSTNAD              
070100*     B = BESTÄLLNINGSPRIS                                                
070200                                                                          
070300     COMPUTE WS-N = (WS-KVPB-TOT * 12)                                    
070400                                                                          
070500     COMPUTE WS-NP = ((2 * WS-N) * 9.25)                                  
070600                                                                          
070700*                    PRARTBES * LAGERRÄNTA                                
070800                                                                          
070900     COMPUTE WS-UB = (REFL-PRARTBES * DCS-REWILSON)                       
071000*                                                                         
071100                                                                          
071200     IF WS-UB = ZERO                                                      
071300        MOVE 1.0 TO WS-UB                                                 
071400     END-IF                                                               
071500                                                                          
071600     COMPUTE WS-NP-UB = (WS-NP / WS-UB)                                   
071700                                                                          
071800     COMPUTE WS-PAFYLLNKVANT ROUNDED = (WS-NP-UB ** 0.5)                  
071900                                                                          
072000     .                                                                    
072100     EJECT                                                                
072200                                                                          
072300                                                                          
072400 S02C-KVANT-M-TABELL SECTION.                                             
072500                                                                          
072600     IF GRUNDTABELL                                                       
072700        IF GRUNDTAB-2502-KDREFPKT-KVA(WS-RADIX, WS-PR-KOLIX)              
072800                                               = 'D'                      
072900                                                                          
073000          MOVE GRUNDTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)             
073100                               TO WS-KVREFKVA                             
073200          PERFORM S12-TAB-BEHOV-KVANT                                     
073300                                                                          
073400        ELSE                                                              
073500          MOVE  GRUNDTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)            
073600                               TO WS-PAFYLLNKVANT                         
073700                                                                          
073800        END-IF                                                            
073900     ELSE                                                                 
074000        IF AKTTAB-2502-KDREFPKT-KVA (WS-RADIX, WS-PR-KOLIX) = 'D'         
074100                                                                          
074200          MOVE AKTTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)               
074300                               TO WS-KVREFKVA                             
074400          PERFORM S12-TAB-BEHOV-KVANT                                     
074500                                                                          
074600        ELSE                                                              
074700          MOVE AKTTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)               
074800                               TO WS-PAFYLLNKVANT                         
074900                                                                          
075000        END-IF                                                            
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500                                                                          
075600                                                                          
075700 S03-OVERLAGERPUNKT SECTION.                                              
075800                                                                          
075900***                                                                       
076000*   BASERAT PÅ ARTIKELNS PÅFYLLNADSKVANTITET BERÄKNAS                     
076100*   ÖVERLAGERPUNKTEN                                                      
076200***                                                                       
076300                                                                          
076400     COMPUTE WS-OVERLAGERPKT = ((REFL-KVREFBER * 2)                       
076500                               + REFL-KVREFPKT)                           
076600                                                                          
076700     MOVE WS-OVERLAGERPKT TO REFL-KVREFOVL                                
076800                                                                          
076900     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200 S05-LEDTIDSBEHOV SECTION.                                                
077300                                                                          
077400     MOVE REFL-IN-LEADTID-BEHOV     TO WS-LEDTIDSBEHOV                    
077500     .                                                                    
077600     EJECT                                                                
077700                                                                          
077800 S06-HAEMTA-KLASS SECTION.                                                
077900***                                                                       
078000*   HÄMTA KLASS FRÅN TABELLEN, KAN VARA 1-12 SAMT A-I                     
078100***                                                                       
078200                                                                          
078300                                                                          
078400     MOVE WS-RADIX TO WS-KLASS-RAD                                        
078500                                                                          
078600     EVALUATE WS-PR-KOLIX                                                 
078700       WHEN 1                                                             
078800         MOVE 'A' TO WS-KLASS-KOL                                         
078900       WHEN 2                                                             
079000         MOVE 'B' TO WS-KLASS-KOL                                         
079100       WHEN 3                                                             
079200         MOVE 'C' TO WS-KLASS-KOL                                         
079300       WHEN 4                                                             
079400         MOVE 'D' TO WS-KLASS-KOL                                         
079500       WHEN 5                                                             
079600         MOVE 'E' TO WS-KLASS-KOL                                         
079700       WHEN 6                                                             
079800         MOVE 'F' TO WS-KLASS-KOL                                         
079900       WHEN 7                                                             
080000         MOVE 'G' TO WS-KLASS-KOL                                         
080100       WHEN 8                                                             
080200         MOVE 'H' TO WS-KLASS-KOL                                         
080300       WHEN OTHER                                                         
080400         MOVE 'I' TO WS-KLASS-KOL                                         
080500     END-EVALUATE                                                         
080600                                                                          
080700     MOVE WS-KLASS TO REFL-KLASS                                          
080800                                                                          
080900     .                                                                    
081000     EJECT                                                                
081100                                                                          
081200                                                                          
081300 S07-PLATS-I-TABELL-NU SECTION.                                           
081400                                                                          
081500     MOVE REFL-PRARTBES TO WS-PRARTBES                                    
081600                                                                          
081700     IF WS-KVPB-TOT > 99999.9                                             
081800        MOVE +99999.9 TO WS-KVPB-TOT                                      
081900     END-IF                                                               
082000     IF REFL-PRARTBES > 9999999.99                                        
082100        MOVE +9999999.99 TO WS-PRARTBES                                   
082200     END-IF                                                               
082300     PERFORM S09-LETA-I-TABELL                                            
082400     .                                                                    
082500     EJECT                                                                
082600                                                                          
082700                                                                          
082800 S09-LETA-I-TABELL SECTION.                                               
082900                                                                          
083000     IF GRUNDTABELL                                                       
083100                                                                          
083200*****   TABELLEN FINNS I DEN SPARADE AREAN FÖR TABELL = 0                 
083300                                                                          
083400        MOVE 1 TO WS-RADIX                                                
083500        PERFORM UNTIL ( GRUNDTAB-2502-PRARTBES (WS-RADIX)                 
083600                                        = WS-PRARTBES                     
083700                       OR GRUNDTAB-2502-PRARTBES (WS-RADIX)               
083800                                        > WS-PRARTBES)                    
083900          ADD 1 TO WS-RADIX                                               
084000        END-PERFORM                                                       
084100                                                                          
084200        MOVE 1 TO WS-PR-KOLIX                                             
084300        PERFORM UNTIL                                                     
084400                (GRUNDTAB-2502-KVPB-REF( WS-PR-KOLIX)                     
084500                                        = WS-KVPB-TOT                     
084600              OR GRUNDTAB-2502-KVPB-REF(WS-PR-KOLIX)                      
084700                                        > WS-KVPB-TOT)                    
084800                                                                          
084900          ADD 1 TO WS-PR-KOLIX                                            
085000        END-PERFORM                                                       
085100                                                                          
085200     ELSE                                                                 
085300                                                                          
085400*****   TABELLEN FINNS I DEN SPARADE AREAN FÖR SAMMA SOM FÖREG.           
085500                                                                          
085600        MOVE 1 TO WS-RADIX                                                
085700***FÖR KINA LIGGER PRMATRL I PRARTBES                                     
085800        PERFORM UNTIL (AKTTAB-2502-PRARTBES (WS-RADIX)                    
085900                                        = WS-PRARTBES                     
086000                       OR AKTTAB-2502-PRARTBES (WS-RADIX)                 
086100                                        > WS-PRARTBES )                   
086200          ADD 1 TO WS-RADIX                                               
086300        END-PERFORM                                                       
086400                                                                          
086500        MOVE 1 TO WS-PR-KOLIX                                             
086600        PERFORM UNTIL                                                     
086700                (AKTTAB-2502-KVPB-REF( WS-PR-KOLIX)                       
086800                                        = WS-KVPB-TOT                     
086900              OR AKTTAB-2502-KVPB-REF( WS-PR-KOLIX)                       
087000                                        > WS-KVPB-TOT )                   
087100                                                                          
087200          ADD 1 TO WS-PR-KOLIX                                            
087300        END-PERFORM                                                       
087400     END-IF                                                               
087500     .                                                                    
087600     EJECT                                                                
087700                                                                          
087800                                                                          
087900 S11-TAB-BEHOV-PKT SECTION.                                               
088000                                                                          
088100     COMPUTE WS-TABELLBEHOV ROUNDED =                                     
088200             WS-KVREFLIM *                                                
088210             WS-KVPB-PLAN-DAY-BINN *                                      
088220             WS-RESSFAC                                                   
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600 S12-TAB-BEHOV-KVANT SECTION.                                             
088700                                                                          
088800       COMPUTE WS-PAFYLLNKVANT ROUNDED =                                  
088900               WS-KVREFKVA * WS-KVPB-PLAN-DAY-BINN                        
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300 S99-ABEND SECTION.                                                       
089400                                                                          
089500     SKIP2                                                                
089600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
089700     .                                                                    
089800     EJECT                                                                
089900                                                                          
090000                                                                          
090100                                                                          
090200* --- IMS SEKTIONER ---                                                   
090300     SKIP3                                                                
090400                                                                          
090500 IMS-GU-2502 SECTION.                                                     
090600     IF 2501-DBD-NAME = 'WDR2'                                            
090700       STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                       
090800            DELIMITED BY SIZE INTO SSA1                                   
090900       STRING 'WDGX2502(IDREFTAB =' W-IDREFTAB-X ')'                      
091000            DELIMITED BY SIZE INTO SSA2                                   
091100     ELSE                                                                 
091200       STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                       
091300            DELIMITED BY SIZE INTO SSA1                                   
091400       STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                      
091500            DELIMITED BY SIZE INTO SSA2                                   
091600     END-IF                                                               
091700     MOVE '  GE' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GU 2501-PCB DLI-IO-AREA SSA1 SSA2                 
091900     MOVE 2501-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     EJECT                                                                
092300                                                                          
092400 IMS-GU-WDB601    SECTION.                                                
092500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
092600          DELIMITED BY SIZE INTO SSA1                                     
092700     MOVE '  ' TO GODK-STATUSKODER                                        
092800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
092900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
093000     PERFORM IMS-STATUSKONTROLL                                           
093100     .                                                                    
093200     SKIP3                                                                
093300 IMS-GNP-WDB616    SECTION.                                               
093400     MOVE   'WDB616  '        TO SSA1                                     
093500     MOVE '  GE'              TO GODK-STATUSKODER                         
093600     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B616 SSA1                
093700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
093800     PERFORM IMS-STATUSKONTROLL                                           
093900     .                                                                    
094000     SKIP3                                                                
094100 IMS-STATUSKONTROLL SECTION.                                              
094200                                                                          
094300     SET STATUS-IX TO 1                                                   
094400     SEARCH GODK-STATUS                                                   
094500       AT END                                                             
094600         STRING 'OTILLÅTEN STATUSKOD FRÅN IMS: ' STATUS-WS                
094700         DELIMITED BY SIZE INTO FELTEXT                                   
094800         CALL FELLOG                                                      
094900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
095000         CONTINUE                                                         
095100     END-SEARCH                                                           
095200     .                                                                    
095300     EJECT                                                                
095400*    -COPY WY2000P3                                                       
