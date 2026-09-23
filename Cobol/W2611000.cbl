000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2611000.                                    
000300 AUTHOR.                     IDK INGVAR CARLSSON.                         
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.               JANUARI 1979.                                
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*    FUNKTION.                                                            
000900                                                                          
001000*        PROGRAMMET, SOM ÄR EN EXIT TILL FSU, LÄSER                       
001100*        ARTIKELREGISTRET WDK6 (SEGMENTEN -01, -11)                       
001200*                               OCH SKAPAR FÖR EJ UT-                     
001300*        GÅNGNA ARTIKLAR POSTER PÅ UTFILEN W26111 FÖR                     
001400*        LTK-UPPFÖLJNING OCH LAGERBALANSERING.                            
001500*        ÄNDRING   880921                                                 
001600*        OMSKRIVET TILL COBOL-II/SB                                       
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*--------------------------------------- POSTER INNEHÅLLANDE              
002300*                                        ARTIKELINFORMATION FÖR           
002400*                                        LTK-UPPFÖLJNING OCH              
002500*                                        LAGERBALANSERING                 
002600*                                        OUTPUT                           
002700                                                                          
002800     SELECT W26111 ASSIGN UT-S-W26110D1.                                  
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200     SKIP2                                                                
003300 FD  W26111                                                               
003400     RECORDING F                                                          
003500     BLOCK 0                                                              
003600     LABEL RECORD STANDARD.                                               
003700*01  POST -COPY W261224    -PRE U11- -L                                   
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP3                                                                
004100*    -COPY WY2000W1                                                       
004200     SKIP3                                                                
004300*--------------------------------------- KONSTANTER                       
004400 01  KONSTANTER.                                                          
004500     05  JA                  PIC X       VALUE 'J'.                       
004600     05  NEJ                 PIC X       VALUE 'N'.                       
004700     05  PROGRAM-NAMN        PIC X(6)    VALUE 'W26110'.                  
004800     SKIP3                                                                
004900*--------------------------------------- INDEXFÄLT                        
005000 01  INDEXFALT.                                                           
005100     05  XCL                  PIC S9(9)               COMP SYNC.          
005200     SKIP3                                                                
005300*--------------------------------------- ALLMÄNNA ARBETSAREOR             
005400*01  WS-SUTPO-TOT             PIC S9(7)  VALUE ZERO   COMP-3.             
005500                                                                          
005600                                                                          
005700 01  W.                                                                   
005800     05  W-KDERS-UTG          PIC S9(3)               COMP-3.             
005900     05  WS-DATUM-GRP.                                                    
006000       07  WS-AAR             PIC S9(2)  VALUE +0.                        
006100       07  WS-MAANAD          PIC S9(2)  VALUE +0.                        
006200       07  WS-DAG             PIC S9(2)  VALUE +0.                        
006300     05  WS-DAGENS-DATUM      PIC S9(6)  VALUE +0.                        
006400     05  W-KDPRODSL           PIC 9(2).                                   
006500     05  FILLER REDEFINES W-KDPRODSL.                                     
006600         07  FILLER           PIC 9.                                      
006700         07  W-IDPROD         PIC 9.                                      
006800     SKIP3                                                                
006900*--------------------------------------- SWITCHAR.                        
007000                                                                          
007100 01  SW.                                                                  
007200     05  SW-FORSTA-ARTIKEL   PIC X       VALUE 'J'.                       
007300     SKIP3                                                                
007400                                                                          
007500*01  -COPY WWPRODSL                                                       
007600                                                                          
007700*--------------------------------------- NYCKLAR TILL DLI                 
007800 01  W-IDARTNR-X.                                                         
007900     05  W-IDARTNR            PIC S9(9)  VALUE ZERO   COMP-3.             
008000                                                                          
008100*--------------------------------------- GENERELLA SUBRUTINER             
008200                                                                          
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
008500     05  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
008600     05  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
008700     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
008800     EJECT                                                                
008900*--------------------------------------- PARAMETRAR TILL POSTSUM          
009000                                                                          
009100*01  -COPY W0005      -PRE POSTSUM-                                       
009200     EJECT                                                                
009300*--------------------------------------- PARAMETRAR TILL DATKORT          
009400 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
009500*    -COPY WDATKORT.                                                      
009600     EJECT                                                                
009700 01  IMS-WS.                                                              
009800     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
009900*----------------------------------STATUSKODER FRÅN IMS                   
010000     03  STATUS-WS   PIC XX.                                              
010100         88  SEGMENT-FINNS       VALUE '  '.                              
010200         88  SEGMENT-SLUT        VALUE 'GB'.                              
010300                                                                          
010400     03  GODK-STATUSKODER.                                                
010500      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
010600                                                                          
010700 01  SSA1                        PIC X(96).                               
010800                                                                          
010900     EJECT                                                                
011000*----------------------------------IMS-CALL FUNKTIONER                    
011100*01              -COPY W0003                                              
011200                                                                          
011300*--------------------------------------- AREA FÖR W26111-POST             
011400                                                                          
011500*01  AREA -COPY W261224    -PRE U11-                                      
011600     EJECT                                                                
011700*--------------------------------------- NOLLAREA FÖR W26111              
011800                                                                          
011900*01  NOLLAREA -COPY W261224    -PRE U11- -L                               
012000     EJECT                                                                
012100 01  START-DLI-IO-AREA.                                                   
012200     03   FILLER     PIC X(16) VALUE 'START DLI-IOAREA'.                  
012300*----------------------------------BASAREA STARTAR WDK6                   
012400 01  FILLER.                                                              
012500 03  DLI-IO-AREA             PIC X(1000).                                 
012600                                                                          
012700*03  ART-AREA    -COPY WDK601      -RED DLI-IO-AREA                       
012800     EJECT                                                                
012900*03  CLAG-AREA   -COPY WDK611      -RED DLI-IO-AREA                       
013000     EJECT                                                                
013100 01  START-DLI-IOAREA2.                                                   
013200     03   FILLER     PIC X(32) VALUE 'START DLI-IO-AREA2'.                
013300*----------------------------------BASAREA STARTAR WDK9                   
013400 01  DLI-IO-AREA2.                                                        
013500*03  AREA -COPY WDK901 -PRE ARTM-                                         
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800     SKIP3                                                                
013900*01    -COPY W0008         -PRE WDK6-                                     
014000          05  FILLER      PIC   XX.                                       
014100     EJECT                                                                
014200     SKIP3                                                                
014300*01    -COPY W0008         -PRE ARTM-                                     
014400          05  FILLER      PIC   XX.                                       
014500     EJECT                                                                
014600 PROCEDURE DIVISION USING  WDK6-PCB ARTM-PCB.                             
014700     ENTRY 'CBLTDLI' USING  WDK6-PCB ARTM-PCB.                            
014800                                                                          
014900     PERFORM A-INITIERING                                                 
015000     PERFORM IMS-GET-WDK6                                                 
015100     PERFORM UNTIL SEGMENT-SLUT                                           
015200         EVALUATE WDK6-SEG-NAME-FB                                        
015300           WHEN  'WDK601  '                                               
015400                     PERFORM B-BEHANDLA-ARTIKEL                           
015500           WHEN  'WDK611  '                                               
015600                     PERFORM C-FLYTTA-MAT-INFO                            
015700                     PERFORM D-FLYTTA-MATCL-INFO                          
015800                     PERFORM E-FLYTTA-EKO-INFO                            
015900                     PERFORM F-FLYTTA-GEM-INFO                            
016000                     PERFORM G-FLYTTA-GEMCL-INFO                          
016100         END-EVALUATE                                                     
016200         PERFORM IMS-GET-WDK6                                             
016300     END-PERFORM                                                          
016400     PERFORM Z-AVSLUTNING                                                 
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     EJECT                                                                
016800******************************************************************        
016900*                                                                *        
017000*    INITIERING                                                  *        
017100*    ÖPPNA FIL, BILDA NOLLPOST                                   *        
017200*                                                                *        
017300******************************************************************        
017400                                                                          
017500     .                                                                    
017600 A-INITIERING SECTION.                                                    
017700                                                                          
017800     OPEN OUTPUT W26111                                                   
017900                                                                          
018000     MOVE SPACE TO U11-POST                                               
018100                   U11-IDLEVNR                                            
018200     MOVE 224 TO U11-IDPTYP                                               
018300     MOVE NEJ TO U11-FLJANEJ-C2                                           
018400     MOVE ZERO TO U11-IDARTNR                                             
018500                    W-IDARTNR                                             
018600                  U11-IDANSK                                              
018700                  U11-KDLTK                                               
018800                  U11-TILTK                                               
018900                  U11-TIFINLV                                             
019000                  U11-KDVVKL                                              
019100                  U11-PRARTSTD                                            
019200                  U11-KDGK                                                
019300                  U11-IDPROD                                              
019400                  U11-KVSKKNST                                            
019500                  U11-TIURPROD                                            
019600                  U11-KVSLUTKP                                            
019700                  U11-KVOVERF                                             
019800                  U11-IDLKTO                                              
019900                  U11-KDPROD                                              
020000                  U11-KDKG                                                
020100                                                                          
020200     MOVE 1 TO XCL                                                        
020300     PERFORM UNTIL XCL > 2                                                
020400         MOVE ZERO TO U11-KDERS (XCL)                                     
020500                      U11-KVLS (XCL)                                      
020600                      U11-KVRESS (XCL)                                    
020700                      U11-KVAKS (XCL)                                     
020800                      U11-KVAKS-E (XCL)                                   
020900                      U11-KVAKS-F (XCL)                                   
021000                      U11-KVOKS-BULK(XCL)                                 
021100                      U11-KVOKS-DAG (XCL)                                 
021200                      U11-KVOKS-VOR (XCL)                                 
021300                      U11-SUTPO-TOT (XCL)                                 
021400                      U11-KVROS (XCL)                                     
021500                      U11-KVPB-SEP (XCL)                                  
021600                      U11-KVPB-SATS (XCL)                                 
021700                      U11-KVMP (XCL)                                      
021800                      U11-KVSLAGER (XCL)                                  
021900                      U11-FLSKROT-BEORD (XCL)                             
022000         ADD +1 TO XCL                                                    
022100     END-PERFORM                                                          
022200                                                                          
022300     MOVE U11-AREA TO U11-NOLLAREA                                        
022400                                                                          
022500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
022600     MOVE 224 TO POSTSUM-TRANSTYP                                         
022700     MOVE 'W26111' TO POSTSUM-FDNAMN                                      
022800     MOVE 'W26110D1' TO POSTSUM-DDNAMN2                                   
022900     EJECT                                                                
023000******************************************************************        
023100*                                                                *        
023200*    BEHANDLA ARTIKEL                                            *        
023300*    AVSLUTA GAMLA ARTIKELN                                      *        
023400*    HÄMTA UPPGIFTER FRÅN ARTIKELSEGMENTET (WDK601) FÖR NYA      *        
023500*    ARTIKELN                                                    *        
023600*                                                                *        
023700******************************************************************        
023800                                                                          
023900     .                                                                    
024000 B-BEHANDLA-ARTIKEL SECTION.                                              
024100                                                                          
024200     IF  SW-FORSTA-ARTIKEL = JA                                           
024300         MOVE NEJ TO SW-FORSTA-ARTIKEL                                    
024400     ELSE                                                                 
024500         MOVE W-KDPRODSL         TO TEST-KDPRODSL                         
024600         IF W-KDERS-UTG = ZERO AND KDPRODSL-VOLVO-BIMA                    
024700             PERFORM S01-SKRIV-U11                                        
024800         END-IF                                                           
024900     END-IF                                                               
025000                                                                          
025100     MOVE U11-NOLLAREA  TO U11-AREA                                       
025200     MOVE ART-IDARTNR   TO U11-IDARTNR                                    
025300                             W-IDARTNR                                    
025400     MOVE ART-TIFINLV   TO U11-TIFINLV                                    
025500     MOVE ART-IDLEVNR   TO U11-IDLEVNR                                    
025600     MOVE ART-KDERS-UTG TO W-KDERS-UTG                                    
025700     MOVE ART-KDPRODSL  TO W-KDPRODSL                                     
025800     MOVE W-IDPROD      TO U11-IDPROD                                     
025900     MOVE ART-TIURPROD  TO U11-TIURPROD                                   
026000     EJECT                                                                
026100******************************************************************        
026200*                                                                *        
026300*    FLYTTA MAT INFO                                             *        
026400*    HÄMTA UPPGIFTER FRÅN MATERIALFÖRSÖRJNINGSSEGMENTET (WDK611) *        
026500*                                                                *        
026600******************************************************************        
026700                                                                          
026800     .                                                                    
026900 C-FLYTTA-MAT-INFO SECTION.                                               
027000                                                                          
027100      MOVE CLAG-IDANSK   TO U11-IDANSK                                    
027200      MOVE CLAG-KDVVKL   TO U11-KDVVKL                                    
027300      MOVE ZERO          TO U11-KVSKKNST                                  
027400      IF CLAG-KVSLUTKP > +0                                               
027500        CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT            
027600        MOVE D-AAR    TO WS-AAR                                           
027700        MOVE D-MAANAD TO WS-MAANAD                                        
027800        MOVE D-DAG    TO WS-DAG                                           
027900        MOVE WS-DATUM-GRP TO WS-DAGENS-DATUM                              
028000        MOVE CLAG-TISLUTKP     TO TMP1-YYMMDD                             
028100        MOVE WS-DAGENS-DATUM   TO TMP2-YYMMDD                             
028200        PERFORM WY2000P1                                                  
028300        IF TMP1-YYMMDD > TMP2-YYMMDD                                      
028400          MOVE +0           TO U11-KVSLUTKP                               
028500        ELSE                                                              
028600          MOVE CLAG-KVSLUTKP TO U11-KVSLUTKP                              
028700        END-IF                                                            
028800      ELSE                                                                
028900        MOVE CLAG-KVSLUTKP  TO U11-KVSLUTKP                               
029000      END-IF                                                              
029100      MOVE CLAG-KVOVERF TO U11-KVOVERF                                    
029200      MOVE ZERO         TO U11-KDPROD                                     
029300      MOVE CLAG-ADLAGOMR TO U11-ADLAGOMR                                  
029400      MOVE CLAG-ADPLATS  TO U11-ADPLATS                                   
029500     EJECT                                                                
029600******************************************************************        
029700*                                                                *        
029800*    FLYTTA MATCL INFO                                           *        
029900*    HÄMTA UPPGIFTER FRÅN MATERIALFÖRSÖRJNINGSSEGMENTET,         *        
030000*                   (WDK611)                                     *        
030100******************************************************************        
030200                                                                          
030300     .                                                                    
030400 D-FLYTTA-MATCL-INFO SECTION.                                             
030500                                                                          
030600      MOVE CLAG-KVPB-SEP   TO U11-KVPB-SEP (1)                            
030700      MOVE CLAG-KVPB-SATS  TO U11-KVPB-SATS (1)                           
030800      MOVE CLAG-KVMP       TO U11-KVMP (1)                                
030900      MOVE CLAG-FLSKROT-BEORD                                             
031000                           TO U11-FLSKROT-BEORD (1)                       
031100                                                                          
031200     EJECT                                                                
031300******************************************************************        
031400*                                                                *        
031500*    FLYTTA EKO INFO                                             *        
031600*    HÄMTA UPPGIFTER FRÅN EKONOMIINFOSEGMENTET, (WDK611)         *        
031700*                                                                *        
031800******************************************************************        
031900                                                                          
032000     .                                                                    
032100 E-FLYTTA-EKO-INFO SECTION.                                               
032200                                                                          
032300      MOVE CLAG-PRARTSTD TO U11-PRARTSTD                                  
032400      MOVE CLAG-KDKG     TO U11-KDKG                                      
032500      MOVE CLAG-IDLKTO   TO U11-IDLKTO                                    
032600     EJECT                                                                
032700******************************************************************        
032800*                                                                *        
032900*    FLYTTA GEM INFO                                             *        
033000*    HÄMTA UPPGIFTER FRÅN GEMENSAMINFOSEGMENTET, (WDK611)        *        
033100*                                                                *        
033200******************************************************************        
033300                                                                          
033400     .                                                                    
033500 F-FLYTTA-GEM-INFO SECTION.                                               
033600                                                                          
033700      MOVE CLAG-KDLTK   TO U11-KDLTK                                      
033800      MOVE CLAG-TILTK   TO U11-TILTK                                      
033900      MOVE CLAG-FLMANGK TO U11-FLMANGK                                    
034000      MOVE CLAG-KDGK    TO U11-KDGK                                       
034100     EJECT                                                                
034200******************************************************************        
034300*                                                                *        
034400*    FLYTTA GEMCL INFO                                           *        
034500*    HÄMTA UPPGIFTER FRÅN GEMENSAMINFOSEGMENTET,                 *        
034600*             (WDK611)                                           *        
034700******************************************************************        
034800                                                                          
034900     .                                                                    
035000 G-FLYTTA-GEMCL-INFO SECTION.                                             
035100                                                                          
035200     MOVE CLAG-KDERS     TO U11-KDERS (1)                                 
035300     MOVE CLAG-KVLS      TO U11-KVLS (1)                                  
035400     MOVE CLAG-KVRESS    TO U11-KVRESS (1)                                
035500     MOVE CLAG-KVAKS-CDC TO U11-KVAKS  (1)                                
035600     ADD  CLAG-KVAKS-PAV TO U11-KVAKS  (1)                                
035700     ADD  CLAG-KVAKS-T   TO U11-KVAKS  (1)                                
035800     MOVE CLAG-KVROS     TO U11-KVROS (1)                                 
035900     MOVE CLAG-KVSLAGER  TO U11-KVSLAGER (1)                              
036000                                                                          
036100     PERFORM IMS-GET-ARTM01                                               
036200     IF SEGMENT-FINNS                                                     
036300       MOVE ARTM-ART-KVOKS-BULK                                           
036400       TO           U11-KVOKS-BULK(1)                                     
036500       MOVE ARTM-ART-KVOKS-DAG                                            
036600       TO           U11-KVOKS-DAG(1)                                      
036700       MOVE ARTM-ART-KVOKS-VOR                                            
036800       TO           U11-KVOKS-VOR(1)                                      
036900       MOVE ARTM-ART-SUTPO-TOT                                            
037000       TO           U11-SUTPO-TOT(1)                                      
037100                                                                          
037200       MOVE ZERO TO U11-KVOKS-BULK(2)                                     
037300       MOVE ZERO TO U11-KVOKS-DAG(2)                                      
037400       MOVE ZERO TO U11-KVOKS-VOR(2)                                      
037500       MOVE ZERO TO U11-SUTPO-TOT(2)                                      
037600     END-IF                                                               
037700     EJECT                                                                
037800******************************************************************        
037900*                                                                *        
038000*    AVSLUTNING                                                  *        
038100*    BEHANDLA SISTA ARTIKELN                                     *        
038200*    STÄNG FIL, SKRIV UT POSTSUMS RÄKNEVERK                      *        
038300*                                                                *        
038400******************************************************************        
038500                                                                          
038600     .                                                                    
038700 Z-AVSLUTNING SECTION.                                                    
038800                                                                          
038900     IF  W-KDERS-UTG = ZERO                                               
039000         PERFORM S01-SKRIV-U11                                            
039100     END-IF                                                               
039200                                                                          
039300     CLOSE W26111                                                         
039400                                                                          
039500     MOVE 'S' TO POSTSUM-OPKOD                                            
039600     CALL POSTSUM USING POSTSUM-PARM                                      
039700     EJECT                                                                
039800******************************************************************        
039900*                                                                *        
040000*    SKRIV W26111                                                *        
040100*    SKRIV POST PÅ W26111, ADDERA TILL POSTRÄKNEVERK             *        
040200*                                                                *        
040300******************************************************************        
040400                                                                          
040500     .                                                                    
040600 S01-SKRIV-U11 SECTION.                                                   
040700                                                                          
040800     WRITE U11-POST FROM U11-AREA                                         
040900                                                                          
041000     CALL POSTSUM USING POSTSUM-PARM                                      
041100     .                                                                    
041200     EJECT                                                                
041300 IMS-GET-WDK6 SECTION.                                                    
041400     SKIP3                                                                
041500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
041600     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
041700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
041800     PERFORM IMS-STATUSKONTROLL                                           
041900                                                                          
042000     .                                                                    
042100 IMS-GET-ARTM01 SECTION.                                                  
042200     SKIP3                                                                
042300     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
042400              DELIMITED BY SIZE INTO SSA1                                 
042500     MOVE '  GE' TO GODK-STATUSKODER                                      
042600     CALL CBLTDLI  USING GU ARTM-PCB DLI-IO-AREA2 SSA1                    
042700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
042800     PERFORM IMS-STATUSKONTROLL                                           
042900     .                                                                    
043000 IMS-STATUSKONTROLL SECTION.                                              
043100     SKIP3                                                                
043200     SET STATUS-IX TO 1                                                   
043300     SEARCH GODK-STATUS AT END CALL FELLOG                                
043400       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
043500       CONTINUE                                                           
043600     END-SEARCH                                                           
043700     .                                                                    
043800     EJECT                                                                
043900*    -COPY WY2000P1                                                       
