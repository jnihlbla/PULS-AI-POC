000100*COMPOPT  STDSUB=YES                                                      
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411TPO5.                                                
000500 AUTHOR.         BOO HAMMARIN, CGLI                                       
000600 DATE-WRITTEN.   APRIL 1991                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET TAR EMOT TPO5-MÄRKTA RADER FRÅN HUVUD-                
001200*        PROGRAMMET. LOGISKA KONTROLLER AV TPO-MÄRKNING,                  
001300*        FRYSTID,TPO-DATUM, OCH RADSTATUS  GÖRS. RADEN                    
001400*        LÄGGS SEDAN UPP PÅ DEN PASSIVA KÖN, WLORDP.                      
001500*                                                                         
001600*        PROGRAMMET LÄSER OCH                                             
001700*                   UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
001800*        PROGRAMMET LÄSER OCH                                             
001900*                   UPPDATERAR WLARTM (WDK9)  ARTIKELREGISTER             
002000*                                                                         
002100*                                                                         
002200*    LÄNKAREA: W411TPO5                                                   
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W411TPO5'.            
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  EXEKV-TID                   PIC X(6).                                
003800                                                                          
003900 77  W-TIREGDAT                  PIC 9(6).                                
004000                                                                          
004100 77  W-KVART                     PIC 9(7).                                
004200                                                                          
004300 01  DAGENS-DAT-TIAAVV           PIC 9(4)    VALUE ZERO.                  
004400 01  DAGENS-DAT REDEFINES DAGENS-DAT-TIAAVV.                              
004500     03  DAGENS-DAT-AA           PIC 9(2).                                
004600     03  DAGENS-DAT-VV           PIC 9(2).                                
004700                                                                          
004800 01  TITPO-TIAAVV                PIC 9(4)    VALUE ZERO.                  
004900 01  W-TIAAVV REDEFINES TITPO-TIAAVV.                                     
005000     03  W-TIAAVV-AA             PIC 9(2).                                
005100     03  W-TIAAVV-VV             PIC 9(2).                                
005200 01  W-TIAVV REDEFINES TITPO-TIAAVV.                                      
005300     03  W-TIAVV-A               PIC 9(1).                                
005400     03  W-TIAVV-AVV             PIC 9(3).                                
005500*      --- VALID IDDC CODES                                               
005600*                                                                         
005700*01    -COPY WWDCKONS                                                     
005800       EJECT                                                              
005900                                                                          
006000 01  GENERELLA-SUBPROGRAM.                                                
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006500                                                                          
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
006800                                                                          
006900 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
007000                                                                          
007100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
007200                                                                          
007300*01  -COPY WDATAREA                                                       
007400                                                                          
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100                                                                          
008200     03  W-WDA501KY-MIN.                                                  
008300         05  W-IDDISTR-MIN       PIC S9(5) COMP-3   VALUE ZERO.           
008400         05  W-IDKUNDNR-MIN      PIC S9(7) COMP-3   VALUE ZERO.           
008500         05  W-IDKUNDRF-MIN      PIC X(10)          VALUE SPACE.          
008600         05  W-IDARTNR-MIN       PIC S9(9) COMP-3   VALUE ZERO.           
008700         05  W-IDLOPNR-MIN       PIC S9(3) COMP-3   VALUE ZERO.           
008800                                                                          
008900     03  W-WDA501KY-MAX.                                                  
009000         05  W-IDDISTR-MAX       PIC S9(5) COMP-3   VALUE ZERO.           
009100         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3   VALUE ZERO.           
009200         05  W-IDKUNDRF-MAX      PIC X(10)          VALUE ZERO.           
009300         05  W-IDARTNR-MAX       PIC S9(9) COMP-3   VALUE ZERO.           
009400         05  W-IDLOPNR-MAX       PIC S9(3) COMP-3   VALUE ZERO.           
009500                                                                          
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
009800                                                                          
009900     03  W-KDSTARAD-X.                                                    
010000         05  W-KDSTARAD          PIC X        VALUE SPACE.                
010100                                                                          
010200     03  W-TITPO-X.                                                       
010300         05  W-TITPO             PIC S9(7)    VALUE ZERO COMP-3.          
010400                                                                          
010500     03  W-WDK911KY-X.                                                    
010600         05  W-DABEHOV           PIC  9(6)    VALUE ZERO.                 
010700                                                                          
010800     EJECT                                                                
010900*    --- AREOR TILL TRANSAR                                               
011000*01  -COPY W092P001  -PRE SORT-                                           
011100     EJECT                                                                
011200*                                                                         
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(150).                              
012400 01  SSA2                        PIC X(150).                              
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013100     SKIP3                                                                
013200 01  DLI-IO-AREA.                                                         
013300     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
013400     SKIP3                                                                
013500     03  WLORDP01 REDEFINES IO-AREA.                                      
013600*        05  -COPY WDA501                                                 
013700     EJECT                                                                
013800     03  WLARTM01 REDEFINES IO-AREA.                                      
013900*        05  -COPY WDK901                                                 
014000     EJECT                                                                
014100     03  WLARTM11 REDEFINES IO-AREA.                                      
014200*        05  -COPY WDK911                                                 
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500                                                                          
014600*                                                                         
014700*   -COPY W411TPO5                                                        
014800*                                                                         
014900     EJECT                                                                
015000*01  -COPY W0008      -PRE ORDP-                                          
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300*01  -COPY W0008      -PRE ARTM-                                          
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING TPO5-W411TPO5 ORDP-PCB                         
015700                           ARTM-PCB.                                      
015800                                                                          
015900     MOVE ZERO TO TPO5-KDORDBEK                                           
016000     MOVE NEJ  TO TPO5-FLKLAR                                             
016100                                                                          
016200     IF TPO5-KDTPOTYP = 5                                                 
016300        PERFORM A-INIT                                                    
016400        IF TPO5-KDORDBEK = ZERO                                           
016500           PERFORM E-ORDERRAD-FRAN-SYSTEM                                 
016600        END-IF                                                            
016700        IF TPO5-KDORDBEK = ZERO                                           
016800           MOVE JA  TO TPO5-FLKLAR                                        
016900        ELSE                                                              
017000           MOVE NEJ TO TPO5-FLKLAR                                        
017100        END-IF                                                            
017200        IF TPO5-FLKLAR = JA                                               
017300           MOVE 71  TO TPO5-KDORDBEK                                      
017400        END-IF                                                            
017500     END-IF                                                               
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 A-INIT SECTION.                                                          
018000                                                                          
018100     ACCEPT EXEKV-TID FROM TIME                                           
018200                                                                          
018300     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
018400     MOVE ZERO     TO DAT-I-TIDATUM                                       
018500     CALL WDATKONV USING DAT-KDDATFORM,                                   
018600                         DAT-I-TIDATUM,                                   
018700                         DAT-O-TIDATUM,                                   
018800                         DAT-KDSVAR                                       
018900     IF DAT-KDSVAR-OK                                                     
019000       MOVE DAT-TIAAVV-GRP TO DAGENS-DAT                                  
019100       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
019200     ELSE                                                                 
019300       MOVE 'FEL FRÅN SUBPROGRAM W411TPO5 I SECTION A' TO FELTEXT         
019400       CALL ABEND USING RKOD-ABEND                                        
019500     END-IF                                                               
019600                                                                          
019700     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
019800     MOVE TPO5-TITPO TO DAT-I-TIDATUM                                     
019900     CALL WDATKONV USING DAT-KDDATFORM,                                   
020000                         DAT-I-TIDATUM,                                   
020100                         DAT-O-TIDATUM,                                   
020200                         DAT-KDSVAR                                       
020300                                                                          
020400     IF DAT-KDSVAR-OK                                                     
020500       MOVE DAT-TIAAVV-GRP TO TITPO-TIAAVV                                
020600     ELSE                                                                 
020700       MOVE 'FEL FRÅN SUBPROGRAM W411TPO5 I SECTION A' TO FELTEXT         
020800       CALL ABEND USING RKOD-ABEND                                        
020900     END-IF                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 E-ORDERRAD-FRAN-SYSTEM SECTION.                                          
021300                                                                          
021400     PERFORM S01-SKAPA-RADKO                                              
021500     PERFORM IMS-ISRT-ORDP-WDA501                                         
021600     IF SEGMENT-FINNS-REDAN                                               
021700        PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                              
021800           ADD 1 TO RAD-IDLOPNR                                           
021900           PERFORM IMS-ISRT-ORDP-WDA501                                   
022000        END-PERFORM                                                       
022100     END-IF                                                               
022200                                                                          
022300     PERFORM S02-UPPDATERA-ARTREG                                         
022400     .                                                                    
022500     EJECT                                                                
022600 S01-SKAPA-RADKO SECTION.                                                 
022700                                                                          
022800     MOVE TPO5-IDDISTR        TO RAD-IDDISTR                              
022900     MOVE TPO5-IDKUNDNR       TO RAD-IDKUNDNR                             
023000     MOVE SPACE               TO RAD-IDKUNDRF                             
023100     MOVE TPO5-IDKUNDRF (3:5) TO RAD-IDORDNR5                             
023200     MOVE TPO5-IDARTNR        TO RAD-IDARTNR                              
023300     MOVE 1                   TO RAD-IDLOPNR                              
023400     MOVE TPO5-BERADREF       TO RAD-BERADREF                             
023500     MOVE NEJ                 TO RAD-FLERS                                
023600     MOVE TPO5-IDANSK         TO RAD-IDANSK                               
023700     MOVE TPO5-IDKONTO        TO RAD-IDKONTO                              
023800     MOVE TPO5-IDKST          TO RAD-IDKST                                
023900     MOVE TPO5-IDANALYS       TO RAD-IDANALYS                             
024000     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
024100     MOVE WC-CDC-SE           TO RAD-IDDC                                 
024200                                 RAD-IDDC-RO                              
024300     MOVE 'DT'                TO RAD-KDOI                                 
024400     MOVE SPACE               TO RAD-CLEARGROUP                           
024500     MOVE TPO5-KDDSP          TO RAD-KDDSP                                
024600     MOVE TPO5-KDFAKTYP       TO RAD-KDFAKTYP                             
024700     MOVE TPO5-KDFRAKT        TO RAD-KDFRAKT                              
024800     MOVE TPO5-KDKVBRYT       TO RAD-KDKVBRYT                             
024900     MOVE TPO5-KDORDING       TO RAD-KDORDING                             
025000     MOVE TPO5-KDORDKL        TO RAD-KDORDKL                              
025100     MOVE TPO5-KDPRODSL       TO RAD-KDPRODSL                             
025200     MOVE 15                  TO RAD-KDRAPRIO                             
025300     MOVE ZERO                TO RAD-KDROO                                
025400     MOVE 1                   TO RAD-KDSTARAD                             
025500     MOVE TPO5-KDTPOTYP       TO RAD-KDTPOTYP                             
025600     MOVE TPO5-KDVRINFO       TO RAD-KDVRINFO                             
025700     MOVE TPO5-KVBEART-Q      TO RAD-KVART                                
025800                                 RAD-KVBEART-Q                            
025900     MOVE ZERO                TO RAD-KVRO                                 
026000     MOVE TPO5-PRARTNTO       TO RAD-PRARTNTO                             
026100     MOVE TPO5-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
026200     MOVE TPO5-REKSIFFR       TO RAD-REKSIFFR                             
026300     MOVE ZERO                TO RAD-TIAVBOKN                             
026400     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
026500     MOVE ZERO                TO RAD-TIRES                                
026600     MOVE ZERO                TO RAD-DARODAT                              
026700     MOVE TPO5-TITPO          TO RAD-TITPO                                
026800     MOVE TPO5-KDPRTYP        TO RAD-KDPRTYP                              
026900     MOVE TPO5-BEVOLREF       TO RAD-BEVOLREF                             
027000     MOVE TPO5-FLINVEST       TO RAD-FLINVEST                             
027100     MOVE TPO5-FLPRTILL       TO RAD-FLPRTILL                             
027200     MOVE JA                  TO RAD-FLTPOBEK                             
027300     MOVE TPO5-BEKUNDRF       TO RAD-BEKUNDRF                             
027400     MOVE TPO5-IDKAMPRF       TO RAD-IDKAMPRF                             
027500     MOVE TPO5-IDLEVNR        TO RAD-IDLEVNR                              
027600     MOVE TPO5-IDSYSTEM       TO RAD-IDSYSTEM                             
027700     MOVE EXEKV-TID           TO RAD-TIREGTID                             
027800     MOVE ZERO                TO RAD-DASENDAT                             
027900                                 RAD-TISENBEK-KL                          
027910     MOVE SPACE               TO RAD-KDROPACK                             
027920     MOVE SPACE               TO RAD-IDARBREF                             
028000                                                                          
028100     MOVE TPO5-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                         
028200     MOVE TPO5-TIREPDAT       TO RAD-TIREPDAT                             
028300     MOVE TPO5-IDKUNDRF-WIP   TO RAD-IDKUNDRF-WIP                         
028500     MOVE +0                  TO RAD-PRAVCOST                             
028600     .                                                                    
028700                                                                          
028800 S02-UPPDATERA-ARTREG SECTION.                                            
028900                                                                          
029000     MOVE TPO5-IDARTNR TO W-IDARTNR                                       
029100     PERFORM IMS-GHU-ARTM-WDK901                                          
029200     ADD TPO5-KVBEART-Q TO ART-SUTPO-TOT                                  
029300     PERFORM IMS-REPL-ARTM-WDK9                                           
029400     MOVE TITPO-TIAAVV  TO W-DABEHOV                                      
029500     IF TITPO-TIAAVV NOT = ZERO                                           
029600       IF TITPO-TIAAVV < 5000                                             
029700         MOVE 20        TO W-DABEHOV (1:2)                                
029800       ELSE                                                               
029900         IF TITPO-TIAAVV < 9999                                           
030000           MOVE 19      TO W-DABEHOV (1:2)                                
030100         ELSE                                                             
030200           MOVE 999999  TO W-DABEHOV                                      
030300         END-IF                                                           
030400       END-IF                                                             
030500     END-IF                                                               
030600     PERFORM IMS-GHNP-ARTM-WDK911                                         
030700     IF SEGMENT-FINNS                                                     
030800       ADD TPO5-KVBEART-Q TO ANT-SUTPO-EJPB                               
030900       PERFORM IMS-REPL-ARTM-WDK9                                         
031000     ELSE                                                                 
031100       MOVE TITPO-TIAAVV   TO ANT-DABEHOV                                 
031200       IF TITPO-TIAAVV NOT = ZERO                                         
031300         IF TITPO-TIAAVV < 5000                                           
031400           MOVE 20        TO ANT-DABEHOV (1:2)                            
031500         ELSE                                                             
031600           IF TITPO-TIAAVV < 9999                                         
031700             MOVE 19      TO ANT-DABEHOV (1:2)                            
031800           ELSE                                                           
031900             MOVE 999999  TO ANT-DABEHOV                                  
032000           END-IF                                                         
032100         END-IF                                                           
032200       END-IF                                                             
032300       MOVE TPO5-KVBEART-Q TO ANT-SUTPO-EJPB                              
032400       MOVE ZERO           TO ANT-SUTPO-PB                                
032500       PERFORM IMS-ISRT-ARTM-WDK9                                         
032600     END-IF                                                               
032700                                                                          
032800     .                                                                    
032900     EJECT                                                                
033000* --- IMS SEKTIONER ---                                                   
033100     SKIP3                                                                
033200 IMS-GHU-ARTM-WDK901 SECTION.                                             
033300                                                                          
033400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
033500          DELIMITED BY SIZE INTO SSA1                                     
033600     MOVE '  ' TO GODK-STATUSKODER                                        
033700     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA SSA1                     
033800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     SKIP3                                                                
034200 IMS-GHNP-ARTM-WDK911 SECTION.                                            
034300                                                                          
034400     STRING 'WLARTM11(DABEHOV  =' W-WDK911KY-X ')'                        
034500          DELIMITED BY SIZE INTO SSA1                                     
034600     MOVE '  GE' TO GODK-STATUSKODER                                      
034700     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA SSA1                    
034800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
034900     PERFORM IMS-STATUSKONTROLL                                           
035000     .                                                                    
035100     SKIP3                                                                
035200 IMS-ISRT-ORDP-WDA501 SECTION.                                            
035300                                                                          
035400     MOVE   'WLORDP01 ' TO SSA1                                           
035500     MOVE '  II' TO GODK-STATUSKODER                                      
035600     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
035700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
035800     PERFORM IMS-STATUSKONTROLL                                           
035900     .                                                                    
036000     EJECT                                                                
036100 IMS-REPL-ARTM-WDK9 SECTION.                                              
036200                                                                          
036300     MOVE '  ' TO GODK-STATUSKODER                                        
036400     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA                         
036500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
036600     PERFORM IMS-STATUSKONTROLL                                           
036700     .                                                                    
036800     EJECT                                                                
036900 IMS-ISRT-ARTM-WDK9 SECTION.                                              
037000                                                                          
037100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
037200          DELIMITED BY SIZE INTO SSA1                                     
037300     MOVE 'WLARTM11 ' TO SSA2                                             
037400     MOVE '  ' TO GODK-STATUSKODER                                        
037500     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA SSA1 SSA2               
037600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
037700     PERFORM IMS-STATUSKONTROLL                                           
037800     .                                                                    
037900     EJECT                                                                
038000 IMS-STATUSKONTROLL SECTION.                                              
038100                                                                          
038200     SET STATUS-IX TO 1                                                   
038300     SEARCH GODK-STATUS                                                   
038400       AT END                                                             
038500       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
038600       DELIMITED BY SIZE INTO FELTEXT                                     
038700       CALL FELLOG                                                        
038800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
038900     END-SEARCH                                                           
039000     .                                                                    
