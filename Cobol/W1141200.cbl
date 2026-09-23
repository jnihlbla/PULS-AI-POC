000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1141200.                                                
000300 AUTHOR.         ÅSGÅRDEN STEFAN.                                         
000400 DATE-WRITTEN.   05/10/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        FÅR ARTIKELDATA FRÅN KDP SOM SKRIVS PÅ NYPONBASEN WDD201         
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDD2                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- KDP-FIL                                                    
002200     SELECT INFIL                      ASSIGN TO W11412D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  INFIL                                                                
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY PC0234F1     -L.                                               
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W1141200'.            
003700 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
003800 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
003900 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
004000 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
004100 77  CHKP-RAKNARE                PIC S9(4)   COMP SYNC VALUE +100.        
004200 77  W-ANT-POSTER-FORBI          PIC S9(7)   COMP-3.                      
004300 77  W-ANT-POSTER                PIC S9(7)   COMP-3.                      
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  INFIL-EOF                   PIC X       VALUE 'N'.                   
004700 01  WS.                                                                  
004800     03 WS-KDSORT                PIC X(2)    VALUE SPACE.                 
004900     03 WS-ANTAL-ISRT-WDD2       PIC 9(9)    VALUE ZERO.                  
005000     03 WS-ANTAL-REPL-WDD2       PIC 9(9)    VALUE ZERO.                  
005100                                                                          
005200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005300     SKIP2                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400                                                                          
006500 01  TIDPUNKT                    PIC 9(8)    VALUE ZERO.                  
006600                                                                          
006700*01  -COPY WWPRODSL                                                       
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL POSTSUM                                          
007700*                                                                         
007800*01  -COPY W0005   -PRE  POSTSUM-                                         
007900     EJECT                                                                
008000 01  INFIL-AREA-START            PIC X(24)   VALUE                        
008100                                             'INFIL-AREA-START'.          
008200     SKIP2                                                                
008300                                                                          
008400*01  AREA -COPY PC0234F1    -PRE IN-                                      
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000     03  W-IDARTNR-X.                                                     
009100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009200     03  W-WDGXKEY-X.                                                     
009300         05  W-IDHTYP            PIC X(4)     VALUE '4579'.               
009400         05  W-IDPGM             PIC X(8)     VALUE 'W1141200'.           
009500         05  W-LOW-VALUE         PIC X(18)    VALUE LOW-VALUE.            
009600     SKIP2                                                                
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010300     88  IMS-EJ-OK                           VALUE 'XD'.                  
010400     SKIP2                                                                
010500 01  GODK-STATUSKODER.                                                    
010600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700     SKIP3                                                                
010800 01  SSA1                        PIC X(64).                               
010900 01  SSA2                        PIC X(64).                               
011000     EJECT                                                                
011100*    --- IMS FUNKTIONSKODER                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500                                                                          
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
011700 01  DLI-IO-WDD201.                                                       
011800*    03  -COPY WDD201                                                     
011900     EJECT                                                                
012000 01  FILLER                  PIC X(16)   VALUE '4579-IO-AREA'.            
012100 01  4579-IO-AREA.                                                        
012200*03  FILLER  -COPY WDGX4579                                               
012300     EJECT                                                                
012400 01  FILLER                  PIC X(16)   VALUE '4580-IO-AREA'.            
012500 01  4580-IO-AREA.                                                        
012600*03  FILLER  -COPY WDGX4580                                               
012700     EJECT                                                                
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012900 01  DLI-IO-WDK601.                                                       
013000*    03  -COPY WDK601  -PRE WDK6-                                         
013100     EJECT                                                                
013200                                                                          
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600*01  -COPY W0009   -PRE MSG-                                              
013700                                                                          
013800*01  -COPY W0008  -PRE WDD2-                                              
013900     05  FILLER                  PIC X.                                   
014000                                                                          
014100*01  -COPY W0008  -PRE WDR4-                                              
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400*01  -COPY W0008  -PRE WDK6-                                              
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700 PROCEDURE DIVISION  USING MSG-PCB WDD2-PCB WDR4-PCB WDK6-PCB.            
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB WDD2-PCB WDR4-PCB WDK6-PCB.            
015000                                                                          
015100     SKIP2                                                                
015200     PERFORM A-INIT                                                       
015300                                                                          
015400     PERFORM IMS-RESTART                                                  
015500     PERFORM IMS-LAS-ATERSTART                                            
015600                                                                          
015700     IF SEGMENT-SAKNAS                                                    
015800        MOVE SPACE        TO 4580-WDGX4580-CTX                            
015900        MOVE '1'          TO 4580-KDSEGKEY                                
016000        MOVE ZERO         TO 4580-KVPOST                                  
016100        MOVE DAGENS-DATUM TO 4580-TIUPPDAT                                
016200        MOVE TIDPUNKT     TO 4580-TIUPPTID                                
016300                                                                          
016400        PERFORM IMS-ISRT-ATERSTART                                        
016500        PERFORM IMS-LAS-ATERSTART                                         
016600     END-IF                                                               
016700                                                                          
016800     IF 4580-KVPOST > +0                                                  
016900        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
017000     ELSE                                                                 
017100        PERFORM S01-LAS-INFIL                                             
017200        MOVE +1 TO W-ANT-POSTER                                           
017300     END-IF                                                               
017400                                                                          
017500     PERFORM UNTIL INFIL-EOF = JA                                         
017600                                                                          
017700       PERFORM C-BEARBETA                                                 
017800                                                                          
017900       PERFORM S01-LAS-INFIL                                              
018000       ADD   +1 TO W-ANT-POSTER                                           
018100                                                                          
018200       IF   INFIL-EOF NOT = JA                                            
018300       AND  W-ANT-POSTER > CHKP-RAKNARE                                   
018400           PERFORM D-TAG-CHECKPOINT                                       
018500           MOVE +1 TO W-ANT-POSTER                                        
018600           ADD   +1 TO TIDPUNKT                                           
018700       END-IF                                                             
018800     END-PERFORM                                                          
018900                                                                          
019000                                                                          
019100     PERFORM Z-FINIT                                                      
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800     SKIP2                                                                
019900                                                                          
020000     OPEN INPUT INFIL                                                     
020100                                                                          
020200     ACCEPT DAGENS-DATUM  FROM DATE                                       
020300     ACCEPT TIDPUNKT      FROM TIME                                       
020400                                                                          
020500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020600     .                                                                    
020700     EJECT                                                                
020800 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
020900                                                                          
021000     MOVE +0              TO W-ANT-POSTER-FORBI                           
021100     PERFORM S01-LAS-INFIL                                                
021200                                                                          
021300     PERFORM UNTIL INFIL-EOF     = JA                                     
021400                OR W-ANT-POSTER-FORBI = 4580-KVPOST                       
021500        PERFORM S01-LAS-INFIL                                             
021600        ADD +1  TO W-ANT-POSTER-FORBI                                     
021700     END-PERFORM                                                          
021800                                                                          
021900     IF INFIL-EOF = JA                                                    
022000        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
022100                      TO FELTEXT                                          
022200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
022300     ELSE                                                                 
022400        MOVE +1  TO W-ANT-POSTER                                          
022500     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 C-BEARBETA SECTION.                                                      
022900                                                                          
023000     IF IN-POSTTYP = 'KD1'                                                
023100        CONTINUE                                                          
023200     ELSE                                                                 
023300        MOVE 'INVALID FILE RECEIVED FROM KDP' TO FELTEXT                  
023400        CALL FELLOG                                                       
023500     END-IF                                                               
023600                                                                          
023700     IF IN-ARTSORT = '    '                                               
023800     OR IN-ARTSORT = 'MM  '                                               
023900     OR IN-ARTSORT = 'KVCM'                                               
024000     OR IN-ARTSORT = 'ML  '                                               
024100     OR IN-ARTSORT = 'G   '                                               
024200                                                                          
024300       IF IN-ARTSORT = '    '                                             
024400         MOVE 'ST'           TO WS-KDSORT                                 
024500       END-IF                                                             
024600                                                                          
024700       IF IN-ARTSORT = 'MM  '                                             
024800         MOVE 'MM'           TO WS-KDSORT                                 
024900       END-IF                                                             
025000                                                                          
025100       IF IN-ARTSORT = 'KVCM'                                             
025200         MOVE 'C2'           TO WS-KDSORT                                 
025300       END-IF                                                             
025400                                                                          
025500       IF IN-ARTSORT = 'ML  '                                             
025600         MOVE 'ML'           TO WS-KDSORT                                 
025700       END-IF                                                             
025800                                                                          
025900       IF IN-ARTSORT = 'G   '                                             
026000         MOVE 'G '           TO WS-KDSORT                                 
026100       END-IF                                                             
026200                                                                          
026300     ELSE                                                                 
026400       MOVE 'ST'             TO WS-KDSORT                                 
026500     END-IF                                                               
026600                                                                          
026700     MOVE IN-ARTNR           TO W-IDARTNR                                 
026800     PERFORM IMS-GHU-D201                                                 
026900                                                                          
027000     IF SEGMENT-FINNS                                                     
027100                                                                          
027200       IF IN-FKNGRUPPNR      NOT = ART-IDFKNGRP                           
027300       OR IN-AONR-T          NOT = ART-IDAO                               
027400       OR IN-ARTNR-LEV       NOT = ART-IDPROENH                           
027500       OR IN-ARTBEN          NOT = ART-BEART-SVE                          
027600       OR IN-RITNNR          NOT = ART-IDRITN                             
027700       OR IN-PROJEKT         NOT = ART-IDPROJK                            
027800       OR WS-KDSORT          NOT = ART-KDSORT                             
027900                                                                          
028000         MOVE IN-FKNGRUPPNR  TO ART-IDFKNGRP                              
028100         MOVE IN-AONR-T      TO ART-IDAO                                  
028200         MOVE IN-ARTNR-LEV   TO ART-IDPROENH                              
028300         MOVE IN-ARTBEN      TO ART-BEART-SVE                             
028400         MOVE IN-RITNNR      TO ART-IDRITN                                
028500         MOVE WS-KDSORT      TO ART-KDSORT                                
028600         PERFORM IMS-GU-WDK601                                            
028700         IF SEGMENT-SAKNAS                                                
028800           MOVE IN-PROJEKT   TO ART-IDPROJK                               
028900         END-IF                                                           
029000                                                                          
029100         PERFORM IMS-REPL-D201                                            
029200         ADD 1               TO WS-ANTAL-REPL-WDD2                        
029300       END-IF                                                             
029400     ELSE                                                                 
029500         PERFORM CA-TILLDELA                                              
029600         PERFORM IMS-ISRT-D201                                            
029700         ADD 1               TO WS-ANTAL-ISRT-WDD2                        
029800         SUBTRACT ART-IDARTNR FROM +999999999                             
029900            GIVING ART-IDARTNR                                            
030000         PERFORM IMS-ISRT-D201                                            
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 CA-TILLDELA SECTION.                                                     
030500                                                                          
030600     MOVE IN-ARTNR           TO ART-IDARTNR                               
030700     MOVE IN-FKNGRUPPNR      TO ART-IDFKNGRP                              
030800     MOVE IN-AONR-T          TO ART-IDAO                                  
030900     MOVE IN-ARTNR-LEV       TO ART-IDPROENH                              
031000     MOVE IN-ARTBEN          TO ART-BEART-SVE                             
031100     MOVE IN-RITNNR          TO ART-IDRITN                                
031200     MOVE IN-PROJEKT         TO ART-IDPROJK                               
031300     MOVE WC-KDPRODSL-VCC-PARTS                                           
031400                             TO ART-KDPRODSL                              
031500     MOVE WS-KDSORT          TO ART-KDSORT                                
031600                                                                          
031700     MOVE ZERO               TO ART-IDANSK                                
031800                                ART-IDANSK-REG                            
031900                                ART-IDARTNR-MOTSV                         
032000                                ART-IDAVD                                 
032100                                ART-IDBERED                               
032200                                ART-KDSTAINK                              
032300                                ART-KVARTAR1                              
032400                                ART-KVARTAR2                              
032500                                ART-KVARTAR3                              
032600                                ART-KVARTVAGN                             
032700                                ART-KVBASL                                
032800                                ART-KVLEVBEG                              
032900                                ART-KVPROG                                
033000                                ART-KVUPB                                 
033100                                ART-PRARTBES                              
033200                                ART-TIANSKREG                             
033300                                ART-DABASL                                
033400                                ART-DAFINLEV                              
033500                                ART-TIINKOP                               
033600                                ART-TILEVBEG                              
033700                                ART-TINEDBRY                              
033800                                ART-TIMOTSI                               
033900                                ART-TIPLAKOP                              
034000                                ART-TIREGDAT                              
034100                                ART-TIRITB                                
034200                                ART-TIRITC                                
034300                                ART-TIRITP                                
034400                                ART-IDRITUTG                              
034500                                ART-TISERLEV (1)                          
034600                                ART-TISERLEV (2)                          
034700                                ART-TISERLEV (3)                          
034800                                ART-TISERLEV (4)                          
034900                                ART-TISERLEV (5)                          
035000                                ART-TISLUBER                              
035100                                ART-TISTABER                              
035200                                ART-TISTOMREG                             
035300                                ART-TITPD                                 
035400                                ART-TIUPB                                 
035500                                ART-TIUPG                                 
035600                                ART-TIUPPDAT                              
035700                                ART-IDINKTEK                              
035800                                ART-TITPD                                 
035900                                ART-TIMOTSI                               
036000                                                                          
036100     MOVE SPACE              TO ART-FLAENDR                               
036200                                ART-FLBASL                                
036300                                ART-FLBERQ                                
036400                                ART-FLBYTES                               
036500                                ART-FLPISK                                
036600                                ART-FLRITB                                
036700                                ART-FLRITC                                
036800                                ART-FLRITP                                
036900                                ART-FLUNIKRD                              
037000                                ART-FLUPG                                 
037100                                ART-IDLEVNR                               
037200                                ART-IDLEVNR-FORB (1)                      
037300                                ART-IDLEVNR-FORB (2)                      
037400                                ART-IDLEVNR-FORB (3)                      
037500                                ART-IDLEVNR-FORB (4)                      
037600                                ART-IDLEVNR-FORB (5)                      
037700                                ART-IDMATKTO                              
037800                                ART-IDPROJ                                
037900                                ART-IDPROJOBJ                             
038000                                ART-IDSTEKN                               
038100                                ART-KDANSKQ                               
038200                                ART-KDARTTYP                              
038300                                ART-KDARTUTG                              
038400                                ART-KDRESBED                              
038500                                ART-KDKOPTYP                              
038600                                ART-KDTPD                                 
038700                                ART-TEANSINK                              
038800                                ART-TEARTNOT                              
038900                                ART-TEARTNOT-BASL                         
039000                                ART-TEORSAK                               
039100                                ART-TETEKNIK                              
039200                                ART-FLUPB                                 
039300                                ART-FLPLAKOP                              
039400                                ART-IDINK                                 
039500                                ART-KDTPD                                 
039600                                ART-IDSTEKN                               
039700     .                                                                    
039800     EJECT                                                                
039900                                                                          
040000 D-TAG-CHECKPOINT SECTION.                                                
040100     SKIP2                                                                
040200*    UPPDATERA ÅTERSTARTREGISTRET                                         
040300     PERFORM IMS-LAS-ATERSTART                                            
040400     ADD  CHKP-RAKNARE   TO 4580-KVPOST                                   
040500     ACCEPT 4580-TIUPPDAT FROM DATE                                       
040600     ACCEPT 4580-TIUPPTID FROM TIME                                       
040700                                                                          
040800     PERFORM IMS-REPL-ATERSTART                                           
040900                                                                          
041000*    TAG CHECKPOINT                                                       
041100     PERFORM IMS-CHECKPOINT                                               
041200     .                                                                    
041300     EJECT                                                                
041400 Z-FINIT SECTION.                                                         
041500                                                                          
041600     CLOSE INFIL                                                          
041700                                                                          
041800*    NOLLA ÅTERSTARTINFORMATIONEN                                         
041900     PERFORM IMS-LAS-ATERSTART                                            
042000     MOVE +0                   TO 4580-KVPOST                             
042100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
042200     ACCEPT 4580-TIUPPTID FROM TIME                                       
042300                                                                          
042400     PERFORM IMS-REPL-ATERSTART                                           
042500                                                                          
042600     SKIP2                                                                
042700     MOVE 'S' TO POSTSUM-OPKOD                                            
042800     CALL POSTSUM USING POSTSUM-PARM                                      
042900     DISPLAY 'ANTAL REPL WDD201: ' WS-ANTAL-REPL-WDD2                     
043000     DISPLAY 'ANTAL ISRT WDD201: ' WS-ANTAL-ISRT-WDD2                     
043100     DISPLAY 'OBS EJ 9-KOMPL     '                                        
043200     .                                                                    
043300     EJECT                                                                
043400                                                                          
043500 S01-LAS-INFIL SECTION.                                                   
043600     SKIP2                                                                
043700     READ INFIL INTO IN-AREA                                              
043800       AT END                                                             
043900          MOVE JA TO INFIL-EOF                                            
044000     END-READ                                                             
044100                                                                          
044200     IF INFIL-EOF = NEJ                                                   
044300        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
044400        MOVE 'W11412D1' TO POSTSUM-DDNAMN2                                
044500        MOVE SPACE          TO POSTSUM-TRANSTYP                           
044600        CALL POSTSUM USING POSTSUM-PARM                                   
044700     END-IF                                                               
044800     .                                                                    
044900                                                                          
045000     EJECT                                                                
045100* --- IMS SEKTIONER ---                                                   
045200                                                                          
045300     EJECT                                                                
045400 IMS-GHU-D201 SECTION.                                                    
045500                                                                          
045600     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
045700          DELIMITED BY SIZE INTO SSA1                                     
045800     MOVE '  GE' TO GODK-STATUSKODER                                      
045900     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
046000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
046100     PERFORM IMS-STATUSKONTROLL                                           
046200     .                                                                    
046300     EJECT                                                                
046400 IMS-ISRT-D201 SECTION.                                                   
046500                                                                          
046600     MOVE 'WDD201 ' TO SSA1                                               
046700     MOVE '  II' TO GODK-STATUSKODER                                      
046800     CALL CBLTDLI USING ISRT WDD2-PCB DLI-IO-WDD201 SSA1                  
046900     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
047000     PERFORM IMS-STATUSKONTROLL                                           
047100     .                                                                    
047200     SKIP3                                                                
047300 IMS-REPL-D201 SECTION.                                                   
047400                                                                          
047500     MOVE '  ' TO GODK-STATUSKODER                                        
047600     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
047700     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
047800     PERFORM IMS-STATUSKONTROLL                                           
047900     .                                                                    
048000     SKIP3                                                                
048100 IMS-GU-WDK601 SECTION.                                                   
048200                                                                          
048300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
048400          DELIMITED BY SIZE INTO SSA1                                     
048500     MOVE '  GE' TO GODK-STATUSKODER                                      
048600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
048700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
048800     PERFORM IMS-STATUSKONTROLL                                           
048900     .                                                                    
049000     EJECT                                                                
049100                                                                          
049200 IMS-RESTART SECTION.                                                     
049300     SKIP2                                                                
049400     MOVE SPACE TO MSG-IO-AREA-1                                          
049500     MOVE '  ' TO GODK-STATUSKODER                                        
049600     CALL CBLTDLI USING XRST MSG-PCB                                      
049700                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
049800                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
049900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050000     PERFORM IMS-STATUSKONTROLL                                           
050100     .                                                                    
050200                                                                          
050300 IMS-CHECKPOINT SECTION.                                                  
050400     MOVE IDPGM        TO MSG-IO-AREA-1                                   
050500     MOVE '  XD'       TO GODK-STATUSKODER                                
050600     CALL CBLTDLI USING CHKP MSG-PCB                                      
050700                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
050800                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
050900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     IF IMS-EJ-OK                                                         
051200       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
051300       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
051400                            TO FELTEXT                                    
051500       CALL FELLOG                                                        
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 IMS-LAS-ATERSTART SECTION.                                               
052000                                                                          
052100     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
052200          DELIMITED BY SIZE INTO SSA1                                     
052300     MOVE 'WDR470   '    TO SSA2                                          
052400     MOVE '  GE'         TO GODK-STATUSKODER                              
052500     CALL CBLTDLI USING GHU WDR4-PCB 4580-IO-AREA SSA1 SSA2               
052600     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
052700     PERFORM IMS-STATUSKONTROLL                                           
052800     .                                                                    
052900                                                                          
053000 IMS-ISRT-ATERSTART SECTION.                                              
053100                                                                          
053200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
053300          DELIMITED BY SIZE INTO SSA1                                     
053400     MOVE 'WDR470   '    TO SSA2                                          
053500     MOVE '  '           TO GODK-STATUSKODER                              
053600     CALL CBLTDLI USING ISRT WDR4-PCB 4580-IO-AREA SSA1 SSA2              
053700     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
053800     PERFORM IMS-STATUSKONTROLL                                           
053900     .                                                                    
054000                                                                          
054100 IMS-REPL-ATERSTART SECTION.                                              
054200                                                                          
054300     MOVE '  '             TO GODK-STATUSKODER                            
054400     CALL CBLTDLI USING REPL WDR4-PCB 4580-IO-AREA                        
054500     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
054600     PERFORM IMS-STATUSKONTROLL                                           
054700     .                                                                    
054800                                                                          
054900 IMS-STATUSKONTROLL SECTION.                                              
055000     SKIP2                                                                
055100     SET STATUS-IX TO 1                                                   
055200     SEARCH GODK-STATUS                                                   
055300       AT END                                                             
055400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055500           DELIMITED BY SIZE INTO FELTEXT                                 
055600         DISPLAY FELTEXT                                                  
055700         CALL FELLOG                                                      
055800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055900         CONTINUE                                                         
056000     END-SEARCH                                                           
056100     .                                                                    
