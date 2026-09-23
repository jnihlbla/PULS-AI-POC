000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W5138000.                                    
000300 AUTHOR.                     IDK INGVAR CARLSSON.                         
000400 DATE-WRITTEN.               DECEMBER 1978.                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        SLÅR PÅ FLAGGA FLINV85 FÖR ÅTERFÖRINGAR                          
000900*          (ERSÄTTNINGSINVENTERING SKALL GÖRAS)                           
001000*        OCH UPPDATERAR INVENTERINGSREGISTRET (WDH1)                      
001100*        BL.A. MED UPPGIFTER                                              
001200*        FRÅN ARTIKELREGISTRET (WDK6) OCH (WDK7)                          
001300*        UPPDATERAR ARIKELREGISTER (WDK6)                                 
001400*        MED INVENTERINGSDATUM                                            
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*--------------------------------------- ERSÄTTNINGSINVENTERING           
002100*                                        INPUT                            
002200     SELECT W11121 ASSIGN  W51380D1.                                      
002300                                                                          
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W11121                                                               
002900     RECORDING F                                                          
003000     BLOCK 0.                                                             
003100                                                                          
003200*01  POST -COPY W11121     -PRE ERSINV-  -L                               
003300                                                                          
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700                                                                          
003800 77  IDPGM                   PIC X(8)    VALUE 'W5138000'.                
003900                                                                          
004000 01  RKOD                    PIC S9(4)               COMP SYNC.           
004100     SKIP3                                                                
004200*--------------------------------------- KONSTANTER                       
004300 01  KONSTANTER.                                                          
004400     05  JA                  PIC X       VALUE 'J'.                       
004500     05  NEJ                 PIC X       VALUE 'N'.                       
004600                                                                          
004700 77  RETURKOD                PIC S9(4)   COMP SYNC VALUE +0.              
004800                                                                          
004900 77  FLAGGA-INV              PIC X       VALUE 'N'.                       
005000 77  SLAGERINFO-FINNS        PIC X       VALUE 'N'.                       
005100 77  W-FLINVSKR              PIC X       VALUE 'N'.                       
005200 77  CD-IX                   PIC S9(3)   VALUE ZERO COMP-3.               
005300                                                                          
005400 01  CHKP-VAR.                                                            
005500   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
005600   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
005700   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005800   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
005900   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
006000   03  CHKP-MAX                  PIC S9(3)   VALUE +100.                  
006100                                                                          
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500                                                                          
006600 01  ATERFOR-SW              PIC X       VALUE 'N'.                       
006700 01  W11121-EOF-SW           PIC X       VALUE 'N'.                       
006800     88  W11121-SLUT                     VALUE 'J'.                       
006900                                                                          
007000*--------------------------------------- ALLMÄNNA ARBETSAREOR             
007100                                                                          
007200 01  WORKAREA.                                                            
007300     03  W-AAVVD             PIC 9(5).                                    
007400     03  W-AAMMDD            PIC 9(6).                                    
007500     03  W-MINUS-AAVV        PIC 9(4).                                    
007600                                                                          
007700 01  WS-TISEGKEYAREA.                                                     
007800     03  WS-TIAAAAMMDDL      PIC 9(9)    VALUE ZERO.                      
007900     03  FILLER  REDEFINES WS-TIAAAAMMDDL.                                
008000      05 WS-SEKEL            PIC 9(2).                                    
008100      05 WS-TIAAMMDD         PIC 9(6).                                    
008200      05 WS-LOPNR            PIC 9.                                       
008300                                                                          
008400     03  WS-TISEGKEY         PIC S9(9)   VALUE ZERO  COMP-3.              
008500                                                                          
008600 01  WS-INV-DAREGDAT-AREA.                                                
008700     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
008800     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
008900       05  WS-INV-NOLL         PIC 9(1).                                  
009000       05  WS-INV-SEKEL        PIC 9(2).                                  
009100       05  WS-INV-AAMMDD       PIC 9(6).                                  
009200                                                                          
009300 01   SEKEL-DATUM-AREA.                                                   
009400      03 W1-AAAAMMDD            PIC 9(8).                                 
009500      03 FILLER REDEFINES W1-AAAAMMDD.                                    
009600        05 W1-SEKEL             PIC 9(2).                                 
009700        05 W1-AAMMDD            PIC 9(6).                                 
009800*--------------------------------------- DYNAMISKA SUBPROGRAM             
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
010100     05  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
010200     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
010300     05  ABEND               PIC X(8)    VALUE 'ABEND   '.                
010400     05  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
010500     05  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
010600     EJECT                                                                
010700*--------------------------------------- VALID IDDC CODES                 
010800*                                                                         
010900*01  -COPY WWDC99                                                         
011000                                                                          
011100*--------------------------------------- PARAMETRAR TILL DATKORT          
011200                                                                          
011300 01  PROGRAM-NAMN            PIC X(8)    VALUE 'W5138000'.                
011400                                                                          
011500 01  DATUMKORT-ID            PIC X(8)    VALUE 'WDATUM'.                  
011600                                                                          
011700*01  -COPY WDATKORT                                                       
011800     EJECT                                                                
011900*--------------------------------------- PARAMETRAR TILL WDATKONV         
012000                                                                          
012100*01  -COPY WDATAREA                                                       
012200     EJECT                                                                
012300*--------------------------------------- PARAMETRAR TILL POSTSUM          
012400                                                                          
012500*01  -COPY W0005      -PRE POSTSUM-                                       
012600     EJECT                                                                
012700*--------------------------------------- AREA FÖR W11121-POST             
012800                                                                          
012900*01  AREA -COPY W11121     -PRE IN-                                       
013000     EJECT                                                                
013100*--------------------------------------- NYCKLAR TILL BASERNA             
013200 01      W-IDARTNR-X.                                                     
013300   03    W-IDARTNR       PIC S9(9)                  COMP-3.               
013400                                                                          
013500 01      W-IDDC-X.                                                        
013600   03    W-IDDC          PIC X(2).                                        
013700                                                                          
013800 01      W-DASKROT-X.                                                     
013900   03    W-DASKROT       PIC 9(8).                                        
014000                                                                          
014100 01      W-KDERS-0-X.                                                     
014200   03    W-KDERS-0       PIC S9(3) VALUE +0         COMP-3.               
014300                                                                          
014301 01      W-KDSEGKEY-X.                                                    
014302   03    W-KDSEGKEY      PIC X       VALUE '1'.                           
014303                                                                          
014400 01  W-WDH111KY-X.                                                        
014500     03  W-IDDC-WDH1     PIC X(2)  VALUE '00'.                            
014600     03  W-KDINVKAT      PIC S9(3) VALUE ZERO       COMP-3.               
014700     03  W-TISEGKEY      PIC S9(9) VALUE ZERO       COMP-3.               
014800     03  W-DAREGDAT-SORT       PIC 9(8) VALUE ZERO.                       
014900                                                                          
015000 01  W-WDH1KEY-MIN-X.                                                     
015100     03  W-IDDC-WDH1-MIN PIC X(2)  VALUE '00'.                            
015200     03  W-KDINVKAT-MIN  PIC S9(3) VALUE ZERO       COMP-3.               
015300     03  W-TISEGKEY-MIN  PIC S9(9) VALUE ZERO       COMP-3.               
015400     03  W-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                         
015500                                                                          
015600 01  W-WDH1KEY-MAX-X.                                                     
015700     03  W-IDDC-WDH1-MAX PIC X(2)  VALUE '99'.                            
015800     03  W-KDINVKAT-MAX  PIC S9(3) VALUE +999       COMP-3.               
015900     03  W-TISEGKEY-MAX  PIC S9(9) VALUE +999999999 COMP-3.               
016000     03  W-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                     
016100                                                                          
016200                                                                          
016300     EJECT                                                                
016400*--------------------------------------- ARBETSAREOR TILL                 
016500*                                        IMS-SEKTIONERNA                  
016600 01      IMS-WS.                                                          
016700   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
016800     SKIP3                                                                
016900*--------------------------------------- STATUSKOD FRÅN IMS               
017000   03    STATUS-WS       PIC XX.                                          
017100     88  SEGMENT-FINNS               VALUE '  '.                          
017200     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
017300     88  SEGMENT-FINNS-REDAN         VALUE 'II'.                          
017400     88  IMS-EJ-OK                   VALUE 'XD'.                          
017500     SKIP3                                                                
017600   03    SSA1            PIC X(128).                                      
017700   03    SSA2            PIC X(128).                                      
017800   03    SSA3            PIC X(128).                                      
017900                                                                          
018000                                                                          
018100   03    GODK-STATUSKODER.                                                
018200     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
018300                                                                          
018400     EJECT                                                                
018500*01      -COPY W0003                                                      
018600     EJECT                                                                
018700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
018710 01  DLI-IO-WDK601.                                                       
018720*  03  -COPY WDK601.                                                      
018730                                                                          
018740 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
018750 01  DLI-IO-WDK611.                                                       
018760*  03  -COPY WDK611.                                                      
018770                                                                          
018780 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK627'.           
018790 01  DLI-IO-WDK627.                                                       
018791*    03  -COPY WDK627                                                     
018792                                                                          
018793 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
018794 01  DLI-IO-WDK629.                                                       
018795*    03  -COPY WDK629                                                     
018796                                                                          
019400*01  WLARTS01 -COPY WDK701                                                
019500     EJECT                                                                
019600*01  WLARTS11 -COPY WDK711                                                
019700     EJECT                                                                
019800                                                                          
019900*01  WDH101 -COPY WDH101 -PRE INV-                                        
020000     EJECT                                                                
020100*01  WDH111 -COPY WDH111 -PRE INV-                                        
020200                                                                          
020300     EJECT                                                                
020400*01  WDH121 -COPY WDH121 -PRE INV-                                        
020500                                                                          
020600                                                                          
020700     EJECT                                                                
020800 LINKAGE SECTION.                                                         
020900*01  -COPY W0009 -PRE MSG-                                                
021000                                                                          
021100*01  -COPY W0008 -PRE WDH1-                                               
021200     05  FILLER              PIC X.                                       
021300                                                                          
021400*01  -COPY W0008 -PRE WDK6-                                               
021500     05  FILLER              PIC X.                                       
021600      EJECT                                                               
021700*01  -COPY W0008 -PRE WLARTS-                                             
021800     05  FILLER              PIC X.                                       
021900                                                                          
022000                                                                          
022100     EJECT                                                                
022200 PROCEDURE DIVISION USING MSG-PCB WDH1-PCB WDK6-PCB                       
022300                          WLARTS-PCB.                                     
022400 MAIN SECTION.                                                            
022500     ENTRY 'DLITCBL' USING MSG-PCB WDH1-PCB WDK6-PCB                      
022600                           WLARTS-PCB.                                    
022700                                                                          
022800     PERFORM A-INITIERING                                                 
022900                                                                          
023000     PERFORM B-ERSATTNINGS-INVENTERING                                    
023100                                                                          
023200     PERFORM Z-AVSLUTNING                                                 
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500     .                                                                    
023600     EJECT                                                                
023700 A-INITIERING SECTION.                                                    
023800                                                                          
023900     PERFORM IMS-RESTART                                                  
024000                                                                          
024100     OPEN INPUT  W11121                                                   
024200                                                                          
024300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024400                                                                          
024500     MOVE D-AAR        TO W-AAVVD(1:2)                                    
024600     MOVE D-VECKA      TO W-AAVVD(3:2)                                    
024700     MOVE D-DAGNR      TO W-AAVVD(5:1)                                    
024800     DISPLAY 'W-AAVVD     ' W-AAVVD                                       
024900     MOVE D-AAR        TO W-AAMMDD(1:2)                                   
025000     MOVE D-MAANAD     TO W-AAMMDD(3:2)                                   
025100     MOVE D-DAG        TO W-AAMMDD(5:2)                                   
025200     DISPLAY 'W-AAMMDD    ' W-AAMMDD                                      
025300                                                                          
025400     MOVE W-AAMMDD     TO WS-TIAAMMDD                                     
025500     MOVE 20           TO WS-SEKEL                                        
025600                          WS-INV-SEKEL                                    
025700                          W1-SEKEL                                        
025800     MOVE W-AAMMDD     TO WS-INV-AAMMDD                                   
025900                          W1-AAMMDD                                       
026000                                                                          
026100     MOVE D-AAR        TO W-MINUS-AAVV(1:2)                               
026200     MOVE D-VECKA      TO W-MINUS-AAVV(3:2)                               
026300     IF D-VECKA = 01                                                      
026400       MOVE 52         TO W-MINUS-AAVV(3:2)                               
026500       COMPUTE W-MINUS-AAVV = W-MINUS-AAVV - 100                          
026600     ELSE                                                                 
026700       COMPUTE W-MINUS-AAVV = W-MINUS-AAVV - 1                            
026800     END-IF                                                               
026900     DISPLAY '1 VECKA MINUS ' W-MINUS-AAVV                                
027000                                                                          
027100     MOVE 'AAVV '      TO DAT-KDDATFORM                                   
027200     MOVE W-MINUS-AAVV TO DAT-I-TIDATUM                                   
027300                                                                          
027400     CALL WDATKONV USING  DAT-KDDATFORM                                   
027500                          DAT-I-TIDATUM                                   
027600                          DAT-O-TIDATUM                                   
027700                          DAT-KDSVAR                                      
027800                                                                          
027900     IF DAT-KDSVAR = ' '                                                  
028000       MOVE DAT-TIAAMMDD TO W-DASKROT                                     
028100       MOVE DAT-TISEKEL  TO W-DASKROT(1:2)                                
028200       DISPLAY ' W-DASKROT '  W-DASKROT                                   
028300     ELSE                                                                 
028400       MOVE +1000 TO RETURKOD                                             
028500       CALL ABEND USING RETURKOD                                          
028600     END-IF                                                               
028700                                                                          
028800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
028900     .                                                                    
029000     EJECT                                                                
029100 B-ERSATTNINGS-INVENTERING SECTION.                                       
029200                                                                          
029300     PERFORM S01-LAES-W11121                                              
029400                                                                          
029500     PERFORM UNTIL W11121-SLUT                                            
029600       MOVE IN-IDDC            TO WS-IDDC                                 
029700                                                                          
029800       PERFORM BA-TESTA-UPPDATERA-KAT                                     
029900       IF CHKP-ANT > CHKP-MAX                                             
030000         PERFORM X-TAG-CHECKPOINT                                         
030100       END-IF                                                             
030200                                                                          
030300       PERFORM S01-LAES-W11121                                            
030400     END-PERFORM                                                          
030500     .                                                                    
030600     EJECT                                                                
030700 BA-TESTA-UPPDATERA-KAT SECTION.                                          
030800                                                                          
030900******************************************************************        
031000*                                                                *        
031100*    OM INVENTERING AV ARTIKELN EJ PÅGÅR PÅ AKTUELLT LAGER       *        
031200*    DVS OM KATEGORI 1-9  SAKNAS LÄGGS KATEGORI 3 ELLER 5 UPP    *        
031300*                                                                *        
031400******************************************************************        
031500                                                                          
031600     MOVE IN-IDARTNR           TO W-IDARTNR                               
031700     MOVE IN-IDDC              TO W-IDDC-WDH1-MIN                         
031800                                  W-IDDC-WDH1-MAX                         
031900                                  W-IDDC                                  
032000                                  W-IDDC-WDH1                             
032100                                                                          
032200     MOVE NEJ                  TO SLAGERINFO-FINNS                        
032300     PERFORM IMS-GET-ARTS-ROT                                             
032400     IF SEGMENT-FINNS                                                     
032500       PERFORM IMS-GNP-SLAGERINFO                                         
032600       IF SEGMENT-FINNS                                                   
032700         MOVE JA               TO SLAGERINFO-FINNS                        
032800       END-IF                                                             
032900     END-IF                                                               
033000                                                                          
033100     PERFORM IMS-GET-ART-ROT                                              
033200     IF SEGMENT-FINNS                                                     
033300       PERFORM IMS-GNP-CLAGERINFO                                         
033400     END-IF                                                               
033500                                                                          
033600     MOVE NEJ                  TO FLAGGA-INV                              
033700     MOVE NEJ                  TO W-FLINVSKR                              
033800     PERFORM IMS-GET-INV-ROT                                              
033900     IF SEGMENT-FINNS                                                     
034000       PERFORM IMS-GET-INV-SEG                                            
034100       PERFORM UNTIL SEGMENT-SAKNAS                                       
034200         IF (INV-INV-KDINVKAT = +1 OR +2 OR +3 OR +4 OR +5 OR +9)         
034300            AND INV-INV-FLINVBEH = NEJ                                    
034400            MOVE JA            TO FLAGGA-INV                              
034500            IF INV-INV-FLINVSKR = JA AND CDC-SE                           
034600              MOVE JA          TO W-FLINVSKR                              
034700            ELSE                                                          
034800              MOVE JA          TO INV-INV-FLINVBEH                        
034900              PERFORM IMS-REPL-INV-SEG                                    
035000              ADD +1           TO CHKP-ANT                                
035100            END-IF                                                        
035200         END-IF                                                           
035300         PERFORM IMS-GET-INV-SEG                                          
035400       END-PERFORM                                                        
035500     END-IF                                                               
035600                                                                          
035700     IF FLAGGA-INV = JA                                                   
035800** KAT 1, 2, 3, 4, 5 ELLER 9 FINNS MED NÅGON INV. EJ BEHANDLAD            
035900       IF W-FLINVSKR = NEJ                                                
036000         PERFORM BAB-ERSAETT-INV                                          
036100       END-IF                                                             
036200     ELSE                                                                 
036300** INGEN KAT 1, 2, 3, 4, 5 ELLER 9 EJ BEHANDLAD FINNS                     
036400       IF CDC-SE OR SLAGERINFO-FINNS = JA                                 
036500         MOVE IN-IDARTNR           TO W-IDARTNR                           
036600         MOVE IN-IDDC              TO INV-INV-IDDC                        
036700         MOVE IN-KDINVKAT          TO INV-INV-KDINVKAT                    
036800                                      W-KDINVKAT                          
036900         MOVE ZERO                 TO INV-INV-KDINVKAT-OLD                
037000         MOVE 0                    TO WS-LOPNR                            
037100         MOVE WS-TIAAAAMMDDL       TO WS-TISEGKEY                         
037200                                      W-TISEGKEY                          
037300***      COMPUTE INV-INV-DAREGDAT-SORT =                                  
037400***        99999999 - W1-AAAAMMDD                                         
037500         MOVE 99999999             TO  INV-INV-DAREGDAT-SORT              
037600         MOVE  W1-AAAAMMDD         TO  INV-INV-DAREGDAT                   
037700                                                                          
037800         MOVE WS-TISEGKEY          TO INV-INV-TISEGKEY                    
037900         MOVE ART-IDFKNGRP         TO INV-INV-IDFKNGRP                    
038000         MOVE +2                   TO INV-INV-KDINVPRIO                   
038100         MOVE ART-KDPRODSL         TO INV-INV-KDPRODSL                    
038200         MOVE CLAG-KDPSLLOC        TO INV-INV-KDPSLLOC                    
038300         MOVE CLAG-KDVVKL          TO INV-INV-KDVVKL                      
038400                                                                          
038500         IF CDC-SE                                                        
038600            MOVE ZERO TO INV-INV-ADLAGOMR                                 
038700                         INV-INV-ADGANG                                   
038800                         INV-INV-ADPLATS                                  
038900            IF CLAG-ADLAGOMR = ZERO                                       
039000            AND CLAG-ADGANG  = ZERO                                       
039100            AND CLAG-ADPLATS = ZERO                                       
039200               IF CLAG-ADLAGOMR-SVS  = ZERO                               
039300               AND CLAG-ADGANG-SVS   = ZERO                               
039400               AND CLAG-ADPLATS-SVS  = ZERO                               
039500                  MOVE +1 TO CD-IX                                        
039600                  PERFORM UNTIL CD-IX > 4                                 
039700                    IF CLAG-ADLAGOMR-CD(CD-IX) = ZERO                     
039800                       CONTINUE                                           
039900                    ELSE                                                  
040000                       MOVE CLAG-ADLAGOMR-CD(CD-IX)                       
040100                                         TO INV-INV-ADLAGOMR              
040200                       MOVE CLAG-ADGANG-CD(CD-IX)                         
040300                                         TO INV-INV-ADGANG                
040400                       MOVE CLAG-ADPLATS-CD(CD-IX)                        
040500                                         TO INV-INV-ADPLATS               
040600                       MOVE +4 TO CD-IX                                   
040700                    END-IF                                                
040800                    ADD +1 TO CD-IX                                       
040900                  END-PERFORM                                             
041000               ELSE                                                       
041100                  MOVE CLAG-ADLAGOMR-SVS TO INV-INV-ADLAGOMR              
041200                  MOVE CLAG-ADGANG-SVS   TO INV-INV-ADGANG                
041300                  MOVE CLAG-ADPLATS-SVS  TO INV-INV-ADPLATS               
041400               END-IF                                                     
041500            ELSE                                                          
041600               MOVE CLAG-ADLAGOMR     TO INV-INV-ADLAGOMR                 
041700               MOVE CLAG-ADGANG       TO INV-INV-ADGANG                   
041800               MOVE CLAG-ADPLATS      TO INV-INV-ADPLATS                  
041900            END-IF                                                        
042000         ELSE                                                             
042100            MOVE SLAG-ADLAGOMR      TO INV-INV-ADLAGOMR                   
042200            MOVE SLAG-ADGANG        TO INV-INV-ADGANG                     
042300            MOVE SLAG-ADPLATS       TO INV-INV-ADPLATS                    
042400         END-IF                                                           
042500         MOVE NEJ                  TO INV-INV-FLINVBEH                    
042600                                      INV-INV-FLINV85                     
042700         IF SDC OR CDC-SE OR NDC-AU OR NDC-JP                             
042800           PERFORM BAA-KONTR-ATERFOR                                      
042900           IF ATERFOR-SW = JA                                             
043000             MOVE JA               TO INV-INV-FLINVBEH                    
043100                                      INV-INV-FLINV85                     
043200             PERFORM IMS-GHNP-CLAGERINFO                                  
043300             MOVE W-AAVVD          TO CLAG-TIINVDAT                       
043400             PERFORM IMS-REPL-CLAGERINFO                                  
043410**** OM DET ÄR EN REFILLARTIKEL SÅ SKALL FLAGGA SÄTTAS TILL N             
043460             PERFORM IMS-GHU-WDK629                                       
043470             IF SEGMENT-FINNS                                             
043471               IF CREF-FLREFNYO = JA                                      
043480                 MOVE NEJ         TO CREF-FLREFNYO                        
043490                 PERFORM IMS-REPL-WDK629                                  
043491               END-IF                                                     
043492             END-IF                                                       
043493                                                                          
043500           END-IF                                                         
043600         END-IF                                                           
043700         MOVE NEJ                  TO INV-INV-FLINVSKR                    
043800                                      INV-INV-FLINV2B                     
043900                                      INV-INV-FLINV2C                     
044000                                      INV-INV-FLINV2D                     
044100                                      INV-INV-FLINV3E                     
044200                                      INV-INV-FLINV4N                     
044300                                      INV-INV-FLINV4P                     
044400                                      INV-INV-FLINV4R                     
044500         MOVE SPACE                TO INV-INV-TEINVANM                    
044600                                      INV-INV-FILLER1                     
044700                                      INV-INV-FILLER2                     
044800         MOVE ZERO                 TO INV-INV-IDPRTOMG                    
044900                                      INV-INV-KVJUSTKV                    
045000                                      INV-INV-IDLOPNR                     
045100                                      INV-INV-KVAKS-OLD                   
045200                                      INV-INV-KVEFRS-OLD                  
045300                                      INV-INV-KVLS-OLD                    
045400         MOVE WS-INV-DAREGDAT      TO INV-INV-DAREGDAT-CRE                
045500         MOVE ZERO                 TO INV-INV-DAREGDAT-PR1                
045600                                      INV-INV-DAREGDAT-PR2                
045700                                      INV-INV-DAREGDAT-PR3                
045800*        MOVE SPACE                TO INV-INV-IDUSER-PR1                  
045900*                                     INV-INV-IDUSER-PR2                  
046000*                                     INV-INV-IDUSER-PR3                  
046100                                                                          
046200         PERFORM IMS-GET-INV-ROT                                          
046300         IF SEGMENT-SAKNAS                                                
046400           MOVE IN-IDARTNR       TO INV-ART-IDARTNR                       
046500           PERFORM IMS-ISRT-INV-ROT                                       
046600           ADD +1                TO CHKP-ANT                              
046700         END-IF                                                           
046800                                                                          
046900         PERFORM IMS-ISRT-INV-SEG                                         
047000         ADD +1                    TO CHKP-ANT                            
047100                                                                          
047200         IF SEGMENT-FINNS-REDAN                                           
047300           PERFORM UNTIL SEGMENT-FINNS                                    
047400             ADD +1                 TO WS-TISEGKEY                        
047500             MOVE WS-TISEGKEY       TO INV-INV-TISEGKEY                   
047600                                       W-TISEGKEY                         
047700             PERFORM IMS-ISRT-INV-SEG                                     
047800             ADD +1                 TO CHKP-ANT                           
047900           END-PERFORM                                                    
048000         END-IF                                                           
048100*** INSERT PÅ WDH121 SEGMENTET ***                                        
048200         MOVE 'W5138000'         TO INV-INVL-IDUSER                       
048300         MOVE '0'                TO INV-INVL-KDSEGKEY                     
048400         PERFORM IMS-ISRT-INL-SEG                                         
048500         MOVE SPACE              TO INV-INVL-IDUSER                       
048600         MOVE '1'                TO INV-INVL-KDSEGKEY                     
048700         PERFORM IMS-ISRT-INL-SEG                                         
048800         MOVE SPACE              TO INV-INVL-IDUSER                       
048900         MOVE '2'                TO INV-INVL-KDSEGKEY                     
049000         PERFORM IMS-ISRT-INL-SEG                                         
049100         MOVE SPACE              TO INV-INVL-IDUSER                       
049200         MOVE '3'                TO INV-INVL-KDSEGKEY                     
049300         PERFORM IMS-ISRT-INL-SEG                                         
049400                                                                          
049500**** SLUT PÅ INSERT PÅ WDH21 SEGMENT                                      
049600                                                                          
049700       END-IF                                                             
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100******************************************************************        
050200*                                                                *        
050300*    KONTROLLERA FÖR CDC OM                                      *        
050400*                   - ARTIKEL HAR LAGERPLATS                     *        
050500*                   - ARTIKEL HAR LAGERSALDO = 0                 *        
050600*                   - ARTIKEL HAR SKROTATS                       *        
050700*    KONTROLLERA FÖR SDC OM                                      *        
050800*                   - ARTIKEL HAR LAGERSALDO = 0                 *        
050900*                   - ARTIKEL HAR SKROTATS                       *        
051000*                                                                *        
051100******************************************************************        
051200                                                                          
051300 BAA-KONTR-ATERFOR SECTION.                                               
051400                                                                          
051500     MOVE NEJ                 TO ATERFOR-SW                               
051600                                                                          
051700     PERFORM IMS-GET-SKROT-INFO                                           
051800     IF SEGMENT-FINNS                                                     
051900       IF CDC-SE AND CLAG-KVLS = +0                                       
052000         MOVE JA              TO ATERFOR-SW                               
052100       END-IF                                                             
052200       IF (SDC OR NDC-AU OR NDC-JP) AND SLAG-KVLS = +0                    
052300         MOVE JA              TO ATERFOR-SW                               
052400       END-IF                                                             
052500     END-IF                                                               
052600                                                                          
052700     IF CDC-SE                                                            
052800       IF CLAG-KVLS     = +0 AND                                          
052900          CLAG-ADLAGOMR = +0 AND                                          
053000          CLAG-ADGANG   = +0 AND                                          
053100          CLAG-ADPLATS  = +0                                              
053200          MOVE JA             TO ATERFOR-SW                               
053300       END-IF                                                             
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 BAB-ERSAETT-INV  SECTION.                                                
053800                                                                          
053900******************************************************************        
054000*                                                                *        
054100*    ERSÄTT REDAN BEFINTLIGA INVENTERINGAR MED KATEGORI 03 FÖR   *        
054200*    ÖVRIGA ELLER 05 FÖR NDC                                     *        
054300*                                                                *        
054400******************************************************************        
054500                                                                          
054600     MOVE IN-IDARTNR           TO W-IDARTNR                               
054700     MOVE IN-IDDC              TO INV-INV-IDDC                            
054800     MOVE IN-KDINVKAT          TO INV-INV-KDINVKAT                        
054900                                  W-KDINVKAT                              
055000     MOVE ZERO                 TO INV-INV-KDINVKAT-OLD                    
055100     MOVE 0                    TO WS-LOPNR                                
055200     MOVE WS-TIAAAAMMDDL       TO WS-TISEGKEY                             
055300                                  W-TISEGKEY                              
055400     MOVE WS-TISEGKEY          TO INV-INV-TISEGKEY                        
055500***  COMPUTE INV-INV-DAREGDAT-SORT =                                      
055600***    99999999 - W1-AAAAMMDD                                             
055700     MOVE 99999999        TO  INV-INV-DAREGDAT-SORT                       
055800     MOVE INV-INV-DAREGDAT-SORT    TO                                     
055900          W-DAREGDAT-SORT                                                 
056000     IF CDC-SE                                                            
056100        MOVE ZERO TO INV-INV-ADLAGOMR                                     
056200                     INV-INV-ADGANG                                       
056300                     INV-INV-ADPLATS                                      
056400                                                                          
056500        IF CLAG-ADLAGOMR = ZERO                                           
056600        AND CLAG-ADGANG  = ZERO                                           
056700        AND CLAG-ADPLATS = ZERO                                           
056800           IF CLAG-ADLAGOMR-SVS  = ZERO                                   
056900           AND CLAG-ADGANG-SVS   = ZERO                                   
057000           AND CLAG-ADPLATS-SVS  = ZERO                                   
057100              MOVE +1 TO CD-IX                                            
057200              PERFORM UNTIL CD-IX > 4                                     
057300                 IF CLAG-ADLAGOMR-CD(CD-IX) = ZERO                        
057400                    CONTINUE                                              
057500                 ELSE                                                     
057600                    MOVE CLAG-ADLAGOMR-CD(CD-IX)                          
057700                                      TO INV-INV-ADLAGOMR                 
057800                    MOVE CLAG-ADGANG-CD(CD-IX)                            
057900                                      TO INV-INV-ADGANG                   
058000                    MOVE CLAG-ADPLATS-CD(CD-IX)                           
058100                                      TO INV-INV-ADPLATS                  
058200                    MOVE +4 TO CD-IX                                      
058300                 END-IF                                                   
058400                 ADD +1 TO CD-IX                                          
058500              END-PERFORM                                                 
058600           ELSE                                                           
058700              MOVE CLAG-ADLAGOMR-SVS TO INV-INV-ADLAGOMR                  
058800              MOVE CLAG-ADGANG-SVS   TO INV-INV-ADGANG                    
058900              MOVE CLAG-ADPLATS-SVS  TO INV-INV-ADPLATS                   
059000           END-IF                                                         
059100        ELSE                                                              
059200           MOVE CLAG-ADLAGOMR     TO INV-INV-ADLAGOMR                     
059300           MOVE CLAG-ADGANG       TO INV-INV-ADGANG                       
059400           MOVE CLAG-ADPLATS      TO INV-INV-ADPLATS                      
059500        END-IF                                                            
059600     ELSE                                                                 
059700       MOVE SLAG-ADLAGOMR      TO INV-INV-ADLAGOMR                        
059800       MOVE SLAG-ADGANG        TO INV-INV-ADGANG                          
059900       MOVE SLAG-ADPLATS       TO INV-INV-ADPLATS                         
060000     END-IF                                                               
060100     MOVE ART-IDFKNGRP         TO INV-INV-IDFKNGRP                        
060200     MOVE +2                   TO INV-INV-KDINVPRIO                       
060300     MOVE ART-KDPRODSL         TO INV-INV-KDPRODSL                        
060400     MOVE CLAG-KDPSLLOC        TO INV-INV-KDPSLLOC                        
060500     MOVE CLAG-KDVVKL          TO INV-INV-KDVVKL                          
060600     MOVE NEJ                  TO INV-INV-FLINVBEH                        
060700     MOVE NEJ                  TO INV-INV-FLINVSKR                        
060800     MOVE ZERO                 TO INV-INV-KVJUSTKV                        
060900     MOVE SPACE                TO INV-INV-TEINVANM                        
061000     MOVE NEJ                  TO INV-INV-FLINV2B                         
061100     MOVE NEJ                  TO INV-INV-FLINV2C                         
061200     MOVE NEJ                  TO INV-INV-FLINV2D                         
061300     MOVE NEJ                  TO INV-INV-FLINV3E                         
061400     MOVE NEJ                  TO INV-INV-FLINV4N                         
061500     MOVE NEJ                  TO INV-INV-FLINV4P                         
061600     MOVE NEJ                  TO INV-INV-FLINV4R                         
061700     MOVE NEJ                  TO INV-INV-FLINV85                         
061800     MOVE SPACE                TO INV-INV-FILLER1                         
061900                                  INV-INV-FILLER2                         
062000     MOVE ZERO                 TO INV-INV-IDPRTOMG                        
062100                                  INV-INV-IDLOPNR                         
062200                                  INV-INV-KVAKS-OLD                       
062300                                  INV-INV-KVEFRS-OLD                      
062400                                  INV-INV-KVLS-OLD                        
062500     MOVE WS-INV-DAREGDAT      TO INV-INV-DAREGDAT-CRE                    
062600     MOVE ZERO                 TO INV-INV-DAREGDAT-PR1                    
062700                                  INV-INV-DAREGDAT-PR2                    
062800                                  INV-INV-DAREGDAT-PR3                    
062900*    MOVE SPACE                TO INV-INV-IDUSER-PR1                      
063000*                                 INV-INV-IDUSER-PR2                      
063100*                                 INV-INV-IDUSER-PR3                      
063200                                                                          
063300     PERFORM IMS-ISRT-INV-SEG                                             
063400     ADD +1                    TO CHKP-ANT                                
063500                                                                          
063600     IF SEGMENT-FINNS-REDAN                                               
063700       PERFORM UNTIL SEGMENT-FINNS                                        
063800         ADD +1                TO WS-TISEGKEY                             
063900         MOVE WS-TISEGKEY      TO INV-INV-TISEGKEY                        
064000                                  W-TISEGKEY                              
064100         PERFORM IMS-ISRT-INV-SEG                                         
064200         ADD +1                TO CHKP-ANT                                
064300       END-PERFORM                                                        
064400     END-IF                                                               
064500*** INSERT PÅ WDH121 SEGMENTET ***                                        
064600     MOVE 'W5138000'  TO INV-INVL-IDUSER                                  
064700     MOVE '0'         TO INV-INVL-KDSEGKEY                                
064800     PERFORM IMS-ISRT-INL-SEG                                             
064900     MOVE SPACE       TO INV-INVL-IDUSER                                  
065000     MOVE '1'         TO INV-INVL-KDSEGKEY                                
065100     PERFORM IMS-ISRT-INL-SEG                                             
065200     MOVE SPACE       TO INV-INVL-IDUSER                                  
065300     MOVE '2'         TO INV-INVL-KDSEGKEY                                
065400     PERFORM IMS-ISRT-INL-SEG                                             
065500     MOVE SPACE       TO INV-INVL-IDUSER                                  
065600     MOVE '3'         TO INV-INVL-KDSEGKEY                                
065700     PERFORM IMS-ISRT-INL-SEG                                             
065800                                                                          
065900**** SLUT PÅ INSERT PÅ WDH21 SEGMENT                                      
066000     .                                                                    
066100     EJECT                                                                
066200 S01-LAES-W11121  SECTION.                                                
066300                                                                          
066400     READ W11121 INTO IN-AREA                                             
066500     AT END                                                               
066600       MOVE JA TO W11121-EOF-SW                                           
066700     NOT AT END                                                           
066800       MOVE 'W11121'   TO POSTSUM-FDNAMN                                  
066900       MOVE 'W51380D3' TO POSTSUM-DDNAMN2                                 
067000       MOVE 'ERSINV'   TO POSTSUM-TRANSTYP                                
067100       CALL POSTSUM USING POSTSUM-PARM                                    
067200     END-READ                                                             
067300     .                                                                    
067400     EJECT                                                                
067500******************************************************************        
067600*                                                                *        
067700*    AVSLUTNING                                                  *        
067800*    STÄNG FILER                                                 *        
067900*    SKRIV UT POSTSUMS RÄKNEVERK                                 *        
068000*                                                                *        
068100******************************************************************        
068200                                                                          
068300 Z-AVSLUTNING SECTION.                                                    
068400                                                                          
068500     CLOSE W11121                                                         
068600                                                                          
068700     MOVE 'S' TO POSTSUM-OPKOD                                            
068800     CALL POSTSUM USING POSTSUM-PARM                                      
068900     .                                                                    
069000     EJECT                                                                
069100 X-TAG-CHECKPOINT  SECTION.                                               
069200                                                                          
069300     PERFORM IMS-CHECKPOINT                                               
069400     MOVE ZERO TO CHKP-ANT                                                
069500     .                                                                    
069600     EJECT                                                                
069700* IMS SECTIONER                                                           
069800     SKIP2                                                                
069900 IMS-RESTART SECTION.                                                     
070000                                                                          
070100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070200     MOVE '  ' TO GODK-STATUSKODER                                        
070300     CALL CBLTDLI USING XRST MSG-PCB                                      
070400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070500                        CHKP-AREA-LENGTH CHKP-AREA                        
070600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     SKIP3                                                                
071000 IMS-CHECKPOINT SECTION.                                                  
071100                                                                          
071200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
071300     MOVE '  XD' TO GODK-STATUSKODER                                      
071400     CALL CBLTDLI USING CHKP MSG-PCB                                      
071500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
071600                        CHKP-AREA-LENGTH CHKP-AREA                        
071700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071800     PERFORM IMS-STATUSKONTROLL                                           
071900                                                                          
072000     IF IMS-EJ-OK                                                         
072100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
072200       DISPLAY FELTEXT                                                    
072300       CALL FELLOG                                                        
072400     END-IF                                                               
072500     .                                                                    
072600     EJECT                                                                
072700 IMS-GET-INV-ROT SECTION.                                                 
072800                                                                          
072900     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
073000            DELIMITED BY SIZE INTO SSA1                                   
073100     MOVE '  GE' TO GODK-STATUSKODER                                      
073200     CALL CBLTDLI USING GU   WDH1-PCB INV-WDH101 SSA1                     
073300     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
073400     PERFORM IMS-STATUSKONTROLL                                           
073500     .                                                                    
073600     SKIP2                                                                
073700 IMS-GET-INV-SEG SECTION.                                                 
073800                                                                          
073900     STRING 'WDH111  (WDH111KY>=' W-WDH1KEY-MIN-X                         
074000                    '&WDH111KY<=' W-WDH1KEY-MAX-X ')'                     
074100            DELIMITED BY SIZE INTO SSA1                                   
074200     MOVE '  GE' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GHNP WDH1-PCB INV-WDH111 SSA1                     
074400     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     EJECT                                                                
074800 IMS-ISRT-INV-ROT SECTION.                                                
074900                                                                          
075000     MOVE 'WDH101  ' TO SSA1                                              
075100     MOVE '  ' TO GODK-STATUSKODER                                        
075200     CALL CBLTDLI USING ISRT WDH1-PCB INV-WDH101 SSA1                     
075300     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600     SKIP2                                                                
075700 IMS-ISRT-INV-SEG SECTION.                                                
075800                                                                          
075900     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
076000            DELIMITED BY SIZE INTO SSA1                                   
076100     MOVE 'WDH111  ' TO SSA2                                              
076200     MOVE '  GEII' TO GODK-STATUSKODER                                    
076300     CALL CBLTDLI USING ISRT WDH1-PCB INV-WDH111 SSA1 SSA2                
076400     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-ISRT-INL-SEG SECTION.                                                
076900                                                                          
077000*    STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
077100*           DELIMITED BY SIZE INTO SSA1                                   
077200*    STRING 'WDH101  (WDH111KY =' W-WDH111KY-X ')'                        
077300*           DELIMITED BY SIZE INTO SSA2                                   
077400     MOVE 'WDH121  ' TO SSA1                                              
077500     MOVE '  II' TO GODK-STATUSKODER                                      
077600     CALL CBLTDLI USING ISRT WDH1-PCB                                     
077700            INV-WDH121 SSA1                                               
077800     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     EJECT                                                                
078200 IMS-REPL-INV-SEG SECTION.                                                
078300                                                                          
078400     MOVE '  ' TO GODK-STATUSKODER                                        
078500     CALL CBLTDLI USING REPL WDH1-PCB INV-WDH111                          
078600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900     EJECT                                                                
079000 IMS-GET-ART-ROT SECTION.                                                 
079100                                                                          
079200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                             
079300                    '&KDERS    =' W-KDERS-0-X ')'                         
079400            DELIMITED BY SIZE INTO SSA1                                   
079500     MOVE '  GE' TO GODK-STATUSKODER                                      
079600     CALL CBLTDLI USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                  
079700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
079800     PERFORM IMS-STATUSKONTROLL                                           
079900     .                                                                    
080000     SKIP2                                                                
080100 IMS-GNP-CLAGERINFO SECTION.                                              
080200                                                                          
080300     MOVE 'WDK611  *F' TO SSA1                                            
080400     MOVE '  ' TO GODK-STATUSKODER                                        
080500     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK611 SSA1                  
080600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
080700     PERFORM IMS-STATUSKONTROLL                                           
080800     .                                                                    
080900     SKIP2                                                                
081000 IMS-GHNP-CLAGERINFO SECTION.                                             
081100                                                                          
081200     MOVE 'WDK611  *F' TO SSA1                                            
081300     MOVE '  ' TO GODK-STATUSKODER                                        
081400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
081500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     SKIP2                                                                
081900 IMS-REPL-CLAGERINFO  SECTION.                                            
082000                                                                          
082100     MOVE '  ' TO GODK-STATUSKODER                                        
082200     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-WDK611                     
082300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     EJECT                                                                
082700 IMS-GET-SKROT-INFO SECTION.                                              
082800                                                                          
082900     STRING  'WDK627  (DASKROT >=' W-DASKROT-X ')'                        
083000              DELIMITED BY SIZE INTO SSA1                                 
083100     MOVE '  GE' TO GODK-STATUSKODER                                      
083200     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK627 SSA1                  
083300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
083400     PERFORM IMS-STATUSKONTROLL                                           
083500     .                                                                    
083600     EJECT                                                                
083601                                                                          
083610 IMS-GHU-WDK629  SECTION.                                                 
083620     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
083630          DELIMITED BY SIZE INTO SSA1                                     
083640     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
083650          DELIMITED BY SIZE INTO SSA2                                     
083660     MOVE   'WDK629  '        TO SSA3                                     
083680     MOVE '  GE' TO GODK-STATUSKODER                                      
083690     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
083691     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
083692     PERFORM IMS-STATUSKONTROLL                                           
083693     .                                                                    
083694     SKIP3                                                                
083695 IMS-REPL-WDK629 SECTION.                                                 
083696     MOVE '  ' TO GODK-STATUSKODER                                        
083697     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
083698     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
083699     PERFORM IMS-STATUSKONTROLL                                           
083700     .                                                                    
083701     EJECT                                                                
083710 IMS-GET-ARTS-ROT SECTION.                                                
083800                                                                          
083900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
084000     DELIMITED BY SIZE INTO SSA1                                          
084100     MOVE '  GE' TO GODK-STATUSKODER                                      
084200     CALL CBLTDLI USING GU   WLARTS-PCB WLARTS01 SSA1                     
084300     MOVE WLARTS-STATUS-CODE TO STATUS-WS                                 
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     .                                                                    
084600     SKIP2                                                                
084700 IMS-GNP-SLAGERINFO SECTION.                                              
084800                                                                          
084900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
085000            DELIMITED BY SIZE INTO SSA1                                   
085100     MOVE '  GE' TO GODK-STATUSKODER                                      
085200     CALL CBLTDLI USING GNP  WLARTS-PCB WLARTS11 SSA1                     
085300     MOVE WLARTS-STATUS-CODE TO STATUS-WS                                 
085400     PERFORM IMS-STATUSKONTROLL                                           
085500     .                                                                    
085600     SKIP2                                                                
085700 IMS-STATUSKONTROLL SECTION.                                              
085800                                                                          
085900     SET STATUS-IX TO 1                                                   
086000     SEARCH GODK-STATUS AT END CALL FELLOG                                
086100        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
086200        CONTINUE                                                          
086300     END-SEARCH                                                           
086400     .                                                                    
