000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1181700.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   25/07/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        NON SPARE PART INFO FROM TCPLM/PRINS                             
001000*        INSERTS / UPDATES ON NYPON DATABASE - WDD2                       
001100*                                                                         
001200*        THIS IS A COPY OF PGM W11412 (INFO FROM KDP)                     
001300*        ADAPATATION DONE TO INS/UPD KDRESBED AND NOTES                   
001400*                                                                         
001500*        PROCESS ONLY IDPTYP = 'NSP' (NON SPARE PART)                     
001600*                                                                         
001700*        FOR 'NSP' KDPRODSL SET TO '11' IN W11811 PGM                     
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- TCPLM/PRINS FILE                                           
002800     SELECT W11811                     ASSIGN TO W11817D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W11811                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W1181101     -L.                                               
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W1181700'.            
004300 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
004400 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
004500 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
004600 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
004700 77  CHKP-RAKNARE                PIC S9(4)   COMP SYNC VALUE +100.        
004800 77  W-ANT-POSTER-FORBI          PIC S9(7)   COMP-3.                      
004900 77  W-ANT-POSTER                PIC S9(7)   COMP-3.                      
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  W11811-EOF                  PIC X       VALUE 'N'.                   
005300 01  WS.                                                                  
005400     03 WS-KDSORT                PIC X(2)    VALUE SPACE.                 
005500     03 WS-ANTAL-ISRT-WDD2       PIC 9(9)    VALUE ZERO.                  
005600     03 WS-ANTAL-REPL-WDD2       PIC 9(9)    VALUE ZERO.                  
005700                                                                          
005800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300                                                                          
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000                                                                          
007100 01  TIDPUNKT                    PIC 9(8)    VALUE ZERO.                  
007200                                                                          
007300*01  -COPY WWPRODSL                                                       
007400     EJECT                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  W11811-AREA-START            PIC X(24)   VALUE                       
008700                                             'W11811-AREA-START'.         
008800                                                                          
008900*01  AREA -COPY W1181101    -PRE IN-                                      
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                     PIC X(16)   VALUE 'IMS-WS'.               
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-IDARTNR-X.                                                     
009600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009700     03  W-WDGXKEY-X.                                                     
009800         05  W-IDHTYP            PIC X(4)     VALUE '4579'.               
009900         05  W-IDPGM             PIC X(8)     VALUE 'W1181700'.           
010000         05  W-LOW-VALUE         PIC X(18)    VALUE LOW-VALUE.            
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010800     88  IMS-EJ-OK                           VALUE 'XD'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000                                                                          
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
012200 01  DLI-IO-WDD201.                                                       
012300*    03  -COPY WDD201                                                     
012400     EJECT                                                                
012500 01  FILLER                  PIC X(16)   VALUE '4579-IO-AREA'.            
012600 01  4579-IO-AREA.                                                        
012700*03  FILLER  -COPY WDGX4579                                               
012800     EJECT                                                                
012900 01  FILLER                  PIC X(16)   VALUE '4580-IO-AREA'.            
013000 01  4580-IO-AREA.                                                        
013100*03  FILLER  -COPY WDGX4580                                               
013200     EJECT                                                                
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013400 01  DLI-IO-WDK601.                                                       
013500*    03  -COPY WDK601  -PRE WDK6-                                         
013600     EJECT                                                                
013700                                                                          
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0009   -PRE MSG-                                              
014200                                                                          
014300*01  -COPY W0008  -PRE WDD2-                                              
014400     05  FILLER                  PIC X.                                   
014500                                                                          
014600*01  -COPY W0008  -PRE WDR4-                                              
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900*01  -COPY W0008  -PRE WDK6-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING MSG-PCB WDD2-PCB WDR4-PCB WDK6-PCB.            
015300 MAIN SECTION.                                                            
015400     ENTRY 'DLITCBL' USING MSG-PCB WDD2-PCB WDR4-PCB WDK6-PCB.            
015500                                                                          
015600     SKIP2                                                                
015700     PERFORM A-INIT                                                       
015800                                                                          
015900     PERFORM IMS-RESTART                                                  
016000     PERFORM IMS-LAS-ATERSTART                                            
016100                                                                          
016200     IF SEGMENT-SAKNAS                                                    
016300        MOVE SPACE        TO 4580-WDGX4580-CTX                            
016400        MOVE '1'          TO 4580-KDSEGKEY                                
016500        MOVE ZERO         TO 4580-KVPOST                                  
016600        MOVE DAGENS-DATUM TO 4580-TIUPPDAT                                
016700        MOVE TIDPUNKT     TO 4580-TIUPPTID                                
016800                                                                          
016900        PERFORM IMS-ISRT-ATERSTART                                        
017000        PERFORM IMS-LAS-ATERSTART                                         
017100     END-IF                                                               
017200                                                                          
017300     IF 4580-KVPOST > +0                                                  
017400        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
017500     ELSE                                                                 
017600        PERFORM S01-LAS-W11811                                            
017700        MOVE +1           TO W-ANT-POSTER                                 
017800     END-IF                                                               
017900                                                                          
018000     PERFORM UNTIL W11811-EOF = JA                                        
018100                                                                          
018200       PERFORM C-BEARBETA                                                 
018300                                                                          
018400       PERFORM S01-LAS-W11811                                             
018500       ADD   +1           TO W-ANT-POSTER                                 
018600                                                                          
018700       IF   W11811-EOF NOT = JA                                           
018800       AND  W-ANT-POSTER > CHKP-RAKNARE                                   
018900           PERFORM D-TAG-CHECKPOINT                                       
019000           MOVE +1        TO W-ANT-POSTER                                 
019100           ADD  +1        TO TIDPUNKT                                     
019200       END-IF                                                             
019300     END-PERFORM                                                          
019400                                                                          
019500                                                                          
019600     PERFORM Z-FINIT                                                      
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 A-INIT SECTION.                                                          
020300     SKIP2                                                                
020400                                                                          
020500     OPEN INPUT W11811                                                    
020600                                                                          
020700     ACCEPT DAGENS-DATUM  FROM DATE                                       
020800     ACCEPT TIDPUNKT      FROM TIME                                       
020900                                                                          
021000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021100     .                                                                    
021200     EJECT                                                                
021300 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
021400                                                                          
021500     MOVE +0              TO W-ANT-POSTER-FORBI                           
021600     PERFORM S01-LAS-W11811                                               
021700                                                                          
021800     PERFORM UNTIL W11811-EOF    = JA                                     
021900                OR W-ANT-POSTER-FORBI = 4580-KVPOST                       
022000        PERFORM S01-LAS-W11811                                            
022100        ADD +1  TO W-ANT-POSTER-FORBI                                     
022200     END-PERFORM                                                          
022300                                                                          
022400     IF W11811-EOF = JA                                                   
022500        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
022600                           TO FELTEXT                                     
022700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
022800     ELSE                                                                 
022900        MOVE +1           TO W-ANT-POSTER                                 
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 C-BEARBETA SECTION.                                                      
023400                                                                          
023500     IF IN-IDPTYP = 'NSP'                                                 
023600        PERFORM CA-PROCESS-DATA                                           
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000                                                                          
024100 CA-PROCESS-DATA SECTION.                                                 
024200                                                                          
024300     MOVE IN-IDARTNR              TO W-IDARTNR                            
024400     PERFORM IMS-GHU-D201                                                 
024500                                                                          
024600     IF SEGMENT-FINNS                                                     
024700                                                                          
024800       IF IN-IDFKNGRP          NOT = ART-IDFKNGRP                         
024900       OR IN-IDAO              NOT = ART-IDAO                             
025000       OR IN-IDPROENH          NOT = ART-IDPROENH                         
025100       OR IN-BEART             NOT = ART-BEART-SVE                        
025200       OR IN-IDRITN            NOT = ART-IDRITN                           
025300       OR IN-IDPROJK           NOT = ART-IDPROJK                          
025400       OR IN-KDSORT            NOT = ART-KDSORT                           
025500       OR IN-AVSL-MOT          NOT = ART-TEARTNOT                         
025600                                                                          
025700         MOVE IN-IDFKNGRP         TO ART-IDFKNGRP                         
025800         MOVE IN-IDAO             TO ART-IDAO                             
025900         MOVE IN-IDPROENH         TO ART-IDPROENH                         
026000         MOVE IN-BEART            TO ART-BEART-SVE                        
026100         MOVE IN-IDRITN           TO ART-IDRITN                           
026200         MOVE IN-KDSORT           TO ART-KDSORT                           
026300         MOVE IN-AVSL-MOT         TO ART-TEARTNOT                         
026400         PERFORM IMS-GU-WDK601                                            
026500         IF SEGMENT-SAKNAS                                                
026600           MOVE IN-IDPROJK        TO ART-IDPROJK                          
026700         END-IF                                                           
026800                                                                          
026900         PERFORM IMS-REPL-D201                                            
027000         ADD 1                    TO WS-ANTAL-REPL-WDD2                   
027100       END-IF                                                             
027200     ELSE                                                                 
027300         PERFORM CA-TILLDELA                                              
027400         PERFORM IMS-ISRT-D201                                            
027500         ADD 1                    TO WS-ANTAL-ISRT-WDD2                   
027600         SUBTRACT ART-IDARTNR   FROM +999999999                           
027700                              GIVING ART-IDARTNR                          
027800         PERFORM IMS-ISRT-D201                                            
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 CA-TILLDELA SECTION.                                                     
028300                                                                          
028400     MOVE IN-IDARTNR         TO ART-IDARTNR                               
028500     MOVE IN-IDFKNGRP        TO ART-IDFKNGRP                              
028600     MOVE IN-IDAO            TO ART-IDAO                                  
028700     MOVE IN-IDPROENH        TO ART-IDPROENH                              
028800     MOVE IN-BEART           TO ART-BEART-SVE                             
028900     MOVE IN-IDRITN          TO ART-IDRITN                                
029000     MOVE IN-IDPROJK         TO ART-IDPROJK                               
029100     MOVE IN-KDPRODSL        TO ART-KDPRODSL                              
029200     MOVE IN-KDSORT          TO ART-KDSORT                                
029300     MOVE IN-AVSL-MOT        TO ART-TEARTNOT                              
029400     MOVE '-'                TO ART-KDRESBED                              
029500                                                                          
029600*                                                                         
029700***  INITIALIZE BELOW FIELDS IN NYPON                                     
029800*                                                                         
029900     MOVE ZERO               TO ART-IDANSK                                
030000                                ART-IDANSK-REG                            
030100                                ART-IDARTNR-MOTSV                         
030200                                ART-IDAVD                                 
030300                                ART-IDBERED                               
030400                                ART-KDSTAINK                              
030500                                ART-KVARTAR1                              
030600                                ART-KVARTAR2                              
030700                                ART-KVARTAR3                              
030800                                ART-KVARTVAGN                             
030900                                ART-KVBASL                                
031000                                ART-KVLEVBEG                              
031100                                ART-KVPROG                                
031200                                ART-KVUPB                                 
031300                                ART-PRARTBES                              
031400                                ART-TIANSKREG                             
031500                                ART-DABASL                                
031600                                ART-DAFINLEV                              
031700                                ART-TIINKOP                               
031800                                ART-TILEVBEG                              
031900                                ART-TINEDBRY                              
032000                                ART-TIMOTSI                               
032100                                ART-TIPLAKOP                              
032200                                ART-TIREGDAT                              
032300                                ART-TIRITB                                
032400                                ART-TIRITC                                
032500                                ART-TIRITP                                
032600                                ART-IDRITUTG                              
032700                                ART-TISERLEV (1)                          
032800                                ART-TISERLEV (2)                          
032900                                ART-TISERLEV (3)                          
033000                                ART-TISERLEV (4)                          
033100                                ART-TISERLEV (5)                          
033200                                ART-TISLUBER                              
033300                                ART-TISTABER                              
033400                                ART-TISTOMREG                             
033500                                ART-TITPD                                 
033600                                ART-TIUPB                                 
033700                                ART-TIUPG                                 
033800                                ART-TIUPPDAT                              
033900                                ART-IDINKTEK                              
034000                                ART-TITPD                                 
034100                                ART-TIMOTSI                               
034200                                                                          
034300     MOVE SPACE              TO ART-FLAENDR                               
034400                                ART-FLBASL                                
034500                                ART-FLBERQ                                
034600                                ART-FLBYTES                               
034700                                ART-FLPISK                                
034800                                ART-FLRITB                                
034900                                ART-FLRITC                                
035000                                ART-FLRITP                                
035100                                ART-FLUNIKRD                              
035200                                ART-FLUPG                                 
035300                                ART-IDLEVNR                               
035400                                ART-IDLEVNR-FORB (1)                      
035500                                ART-IDLEVNR-FORB (2)                      
035600                                ART-IDLEVNR-FORB (3)                      
035700                                ART-IDLEVNR-FORB (4)                      
035800                                ART-IDLEVNR-FORB (5)                      
035900                                ART-IDMATKTO                              
036000                                ART-IDPROJ                                
036100                                ART-IDPROJOBJ                             
036200                                ART-IDSTEKN                               
036300                                ART-KDANSKQ                               
036400                                ART-KDARTTYP                              
036500                                ART-KDARTUTG                              
036600                                ART-KDKOPTYP                              
036700                                ART-KDTPD                                 
036800                                ART-TEANSINK                              
036900                                ART-TEARTNOT-BASL                         
037000                                ART-TEORSAK                               
037100                                ART-TETEKNIK                              
037200                                ART-FLUPB                                 
037300                                ART-FLPLAKOP                              
037400                                ART-IDINK                                 
037500                                ART-KDTPD                                 
037600                                ART-IDSTEKN                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 D-TAG-CHECKPOINT SECTION.                                                
038100     SKIP2                                                                
038200*    UPPDATERA ÅTERSTARTREGISTRET                                         
038300     PERFORM IMS-LAS-ATERSTART                                            
038400     ADD  CHKP-RAKNARE       TO 4580-KVPOST                               
038500     ACCEPT 4580-TIUPPDAT  FROM DATE                                      
038600     ACCEPT 4580-TIUPPTID  FROM TIME                                      
038700                                                                          
038800     PERFORM IMS-REPL-ATERSTART                                           
038900                                                                          
039000*    TAG CHECKPOINT                                                       
039100     PERFORM IMS-CHECKPOINT                                               
039200     .                                                                    
039300     EJECT                                                                
039400 Z-FINIT SECTION.                                                         
039500                                                                          
039600     CLOSE W11811                                                         
039700                                                                          
039800*    NOLLA ÅTERSTARTINFORMATIONEN                                         
039900     PERFORM IMS-LAS-ATERSTART                                            
040000     MOVE +0                TO 4580-KVPOST                                
040100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
040200     ACCEPT 4580-TIUPPTID FROM TIME                                       
040300                                                                          
040400     PERFORM IMS-REPL-ATERSTART                                           
040500                                                                          
040600     SKIP2                                                                
040700     MOVE 'S' TO POSTSUM-OPKOD                                            
040800     CALL POSTSUM USING POSTSUM-PARM                                      
040900     DISPLAY 'ANTAL REPL WDD201: ' WS-ANTAL-REPL-WDD2                     
041000     DISPLAY 'ANTAL ISRT WDD201: ' WS-ANTAL-ISRT-WDD2                     
041100     DISPLAY 'OBS EJ 9-KOMPL     '                                        
041200     .                                                                    
041300     EJECT                                                                
041400                                                                          
041500 S01-LAS-W11811 SECTION.                                                  
041600     SKIP2                                                                
041700     READ W11811        INTO IN-AREA                                      
041800       AT END                                                             
041900          MOVE JA         TO W11811-EOF                                   
042000     END-READ                                                             
042100                                                                          
042200     IF W11811-EOF = NEJ                                                  
042300        MOVE 'W11811'     TO POSTSUM-FDNAMN                               
042400        MOVE 'W11817D1'   TO POSTSUM-DDNAMN2                              
042500        MOVE SPACE        TO POSTSUM-TRANSTYP                             
042600        CALL POSTSUM   USING POSTSUM-PARM                                 
042700     END-IF                                                               
042800     .                                                                    
042900                                                                          
043000     EJECT                                                                
043100* --- IMS SEKTIONER ---                                                   
043200                                                                          
043300     EJECT                                                                
043400 IMS-GHU-D201 SECTION.                                                    
043500                                                                          
043600     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
043700          DELIMITED BY SIZE INTO SSA1                                     
043800     MOVE '  GE' TO GODK-STATUSKODER                                      
043900     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
044000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     EJECT                                                                
044400 IMS-ISRT-D201 SECTION.                                                   
044500                                                                          
044600     MOVE 'WDD201 ' TO SSA1                                               
044700     MOVE '  II' TO GODK-STATUSKODER                                      
044800     CALL CBLTDLI USING ISRT WDD2-PCB DLI-IO-WDD201 SSA1                  
044900     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     .                                                                    
045200     SKIP3                                                                
045300 IMS-REPL-D201 SECTION.                                                   
045400                                                                          
045500     MOVE '  ' TO GODK-STATUSKODER                                        
045600     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
045700     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000     SKIP3                                                                
046100 IMS-GU-WDK601 SECTION.                                                   
046200                                                                          
046300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
046400          DELIMITED BY SIZE INTO SSA1                                     
046500     MOVE '  GE' TO GODK-STATUSKODER                                      
046600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
046700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     .                                                                    
047000     EJECT                                                                
047100                                                                          
047200 IMS-RESTART SECTION.                                                     
047300     SKIP2                                                                
047400     MOVE SPACE TO MSG-IO-AREA-1                                          
047500     MOVE '  ' TO GODK-STATUSKODER                                        
047600     CALL CBLTDLI USING XRST MSG-PCB                                      
047700                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
047800                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
047900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200                                                                          
048300 IMS-CHECKPOINT SECTION.                                                  
048400     MOVE IDPGM        TO MSG-IO-AREA-1                                   
048500     MOVE '  XD'       TO GODK-STATUSKODER                                
048600     CALL CBLTDLI USING CHKP MSG-PCB                                      
048700                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
048800                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
048900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049000     PERFORM IMS-STATUSKONTROLL                                           
049100     IF IMS-EJ-OK                                                         
049200       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
049300       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
049400                            TO FELTEXT                                    
049500       CALL FELLOG                                                        
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900 IMS-LAS-ATERSTART SECTION.                                               
050000                                                                          
050100     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
050200          DELIMITED BY SIZE INTO SSA1                                     
050300     MOVE 'WDR470   '    TO SSA2                                          
050400     MOVE '  GE'         TO GODK-STATUSKODER                              
050500     CALL CBLTDLI USING GHU WDR4-PCB 4580-IO-AREA SSA1 SSA2               
050600     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900                                                                          
051000 IMS-ISRT-ATERSTART SECTION.                                              
051100                                                                          
051200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
051300          DELIMITED BY SIZE INTO SSA1                                     
051400     MOVE 'WDR470   '    TO SSA2                                          
051500     MOVE '  '           TO GODK-STATUSKODER                              
051600     CALL CBLTDLI USING ISRT WDR4-PCB 4580-IO-AREA SSA1 SSA2              
051700     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
051800     PERFORM IMS-STATUSKONTROLL                                           
051900     .                                                                    
052000                                                                          
052100 IMS-REPL-ATERSTART SECTION.                                              
052200                                                                          
052300     MOVE '  '             TO GODK-STATUSKODER                            
052400     CALL CBLTDLI USING REPL WDR4-PCB 4580-IO-AREA                        
052500     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSKONTROLL                                           
052700     .                                                                    
052800                                                                          
052900 IMS-STATUSKONTROLL SECTION.                                              
053000     SKIP2                                                                
053100     SET STATUS-IX TO 1                                                   
053200     SEARCH GODK-STATUS                                                   
053300       AT END                                                             
053400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
053500           DELIMITED BY SIZE INTO FELTEXT                                 
053600         DISPLAY FELTEXT                                                  
053700         CALL FELLOG                                                      
053800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053900         CONTINUE                                                         
054000     END-SEARCH                                                           
054100     .                                                                    
