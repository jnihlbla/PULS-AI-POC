000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411RELS.                                                
000500 AUTHOR.         CHRISTINE LINDQVIST                                      
000600 DATE-WRITTEN.   NOV2000                                                  
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET TAR EMOT RADER SOM ÄR MÄRKTA MED                      
001200*        RELEASESPÄRR(OBEKRKOD 56) (FLRELSP) OCH                          
001300*        RESTNOTERAR (WDA5). TPO-TYP 7                                    
001400*                                                                         
001500*        PROGRAMMET LÄSER OCH                                             
001600*                   UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
001700*        PROGRAMMET LÄSER OCH                                             
001800*                   UPPDATERAR WLARTM (WDK9)  ARTIKELREGISTER             
001900*        ORDERINGÅNGSTRANS W2T109X SKAPAS.                                
002000*                                                                         
002100*    LÄNKAREA: W411RELS                                                   
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W411RELS'.            
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  W-TIREGDAT                  PIC 9(6).                                
003800                                                                          
003900 77  W-KVART                     PIC 9(7).                                
004000                                                                          
004100 77  EXEKV-TID                   PIC X(6).                                
004200                                                                          
004300 77  W-IDLOPNR                   PIC S9(3)    COMP-3.                     
004400                                                                          
004500 01  DAGENS-DAT-TIAAVV           PIC 9(4).                                
004600 01  FILLER REDEFINES DAGENS-DAT-TIAAVV.                                  
004700     03  DAGENS-DAT-AA           PIC 9(2).                                
004800     03  DAGENS-DAT-VV           PIC 9(2).                                
004900                                                                          
005000 01  W-TISENBEK.                                                          
005100     03  W-TISENBEK-DAG          PIC 9(6).                                
005200     03  W-TISENBEK-KL           PIC 9(6).                                
005300                                                                          
005400 01  W-TISENBEK-KL-UPPD.                                                  
005500     03  W-TISENBEK-KL-HH        PIC 9(2)    VALUE ZERO.                  
005600     03  W-TISENBEK-KL-MM        PIC 9(2)    VALUE ZERO.                  
005700     03  W-TISENBEK-KL-SS        PIC 9(2)    VALUE ZERO.                  
005800     EJECT                                                                
005900*      --- VALID IDDC CODES                                               
006000*                                                                         
006100*01    -COPY WWDCKONS                                                     
006101                                                                          
006110*      --- BYTESARTIKLAR                                                  
006120*                                                                         
006130*01    -COPY WWBYT03                                                      
006200       EJECT                                                              
006300                                                                          
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006900                                                                          
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
007400                                                                          
007500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
007600                                                                          
007700*01  -COPY WDATAREA                                                       
007800                                                                          
007900     EJECT                                                                
008000     EJECT                                                                
008100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  NYCKLAR-TILL-DLI.                                                    
008600                                                                          
008700     03  W-IDARTNR-X.                                                     
008800         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
008900                                                                          
009000     03  W-KDSEGKEY-X.                                                    
009100         05  W-KDSEGKEY          PIC X.                                   
009200                                                                          
009300     03  W-DABEHOV-X.                                                     
009400         05  W-DABEHOV           PIC  9(6)    VALUE ZERO.                 
009500     EJECT                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
010000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010200     SKIP2                                                                
010300 01  GODK-STATUSKODER.                                                    
010400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010500     SKIP3                                                                
010600 01  SSA1                        PIC X(64).                               
010700 01  SSA2                        PIC X(64).                               
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011400     SKIP3                                                                
011500 01  DLI-IO-AREA.                                                         
011600     03  WLORDP01.                                                        
011700*        05  -COPY WDA501                                                 
011800     EJECT                                                                
011900 01  DLI-IO-AREA3.                                                        
012000     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
012100     03  WLARTM01 REDEFINES IO-AREA3.                                     
012200*        05  -COPY WDK901                                                 
012300     EJECT                                                                
012400     03  WLARTM11 REDEFINES IO-AREA3.                                     
012500*        05  -COPY WDK911                                                 
012600     EJECT                                                                
012700                                                                          
012800*    MSG-AREA FÖR HOPP TILL W20109                                        
012900 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
013000 01  W-PROG-TO-PROG-SW-1.                                                 
013100     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
013200     03  2109-Z1                   PIC X.                                 
013300     03  2109-Z2                   PIC X.                                 
013400     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
013500     03  2109-IDTRANS              PIC X(4)  VALUE ' ELS'.                
013600     03  2109-KDMFSFOR             PIC X     VALUE '1'.                   
013700*    03  -COPY W2I10902    -PRE 2109-                                     
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000*                                                                         
014100*   -COPY W411RELS                                                        
014200*                                                                         
014300     EJECT                                                                
014400*                                                                         
014500*01  -COPY W0009    -PRE 2109-                                            
014600     EJECT                                                                
014700*01  -COPY W0008      -PRE ORDP-                                          
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000*01  -COPY W0008      -PRE FILA-                                          
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300*01  -COPY W0008      -PRE ARTM-                                          
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING RELS-W411RELS ORDP-PCB                         
015700                           FILA-PCB ARTM-PCB 2109-PCB.                    
015800                                                                          
015900     MOVE NEJ TO RELS-FLKLAR                                              
016000                                                                          
016100     IF RELS-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                      
016200       PERFORM A-INIT                                                     
016300       PERFORM B-RAD-MED-KDORDBEH1                                        
016400       MOVE JA TO RELS-FLKLAR                                             
016500     END-IF                                                               
016600                                                                          
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     ACCEPT EXEKV-TID FROM TIME                                           
017300                                                                          
017400     MOVE 'IDAG' TO DAT-KDDATFORM                                         
017500     MOVE ZERO   TO DAT-I-TIDATUM                                         
017600     CALL WDATKONV USING DAT-KDDATFORM,                                   
017700                         DAT-I-TIDATUM,                                   
017800                         DAT-O-TIDATUM,                                   
017900                         DAT-KDSVAR                                       
018000     IF DAT-KDSVAR-OK                                                     
018100       MOVE DAT-TIAAVV-GRP TO DAGENS-DAT-TIAAVV                           
018200       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
018300     ELSE                                                                 
018400       MOVE 'FEL FRÅN SUBPROGRAM W411RELS I SECTION A' TO FELTEXT         
018500       CALL ABEND USING RKOD-ABEND                                        
018600     END-IF                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 B-RAD-MED-KDORDBEH1 SECTION.                                             
019000                                                                          
019100     MOVE JA  TO RAD-FLTPOBEK                                             
019200     MOVE ZERO TO RAD-DASENDAT                                            
019300                  RAD-TISENBEK-KL                                         
019400     IF RELS-KDORDING < 3                                                 
019500       PERFORM BA-SKAPA-2109-TRANS                                        
019600     END-IF                                                               
019700     PERFORM BB-SKAPA-RADKO                                               
019800     PERFORM IMS-ISRT-ORDP-WDA501                                         
019900     IF SEGMENT-FINNS-REDAN                                               
020000       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
020100         ADD 1 TO RAD-IDLOPNR                                             
020200         PERFORM IMS-ISRT-ORDP-WDA501                                     
020300       END-PERFORM                                                        
020400     END-IF                                                               
020500     PERFORM BC-UPPDATERA-ARTREG                                          
020600                                                                          
020700     .                                                                    
020800     EJECT                                                                
020900 BA-SKAPA-2109-TRANS SECTION.                                             
021000                                                                          
021010*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
021020     MOVE RELS-IDARTNR       TO BYT03-IDARTNR                             
021030     IF NOT BYT03-OBJEKT                                                  
021040                                                                          
021100        MOVE SPACE          TO 2109-MID2-W2I10902                         
021200                                                                          
021300        MOVE 1              TO 2109-MID2-KVANTART                         
021400        MOVE RELS-IDARTNR   TO 2109-MID2-IDARTNR (1)                      
021500        MOVE WC-CDC-SE      TO 2109-MID2-IDDC (1)                         
021600        MOVE '+'            TO 2109-MID2-KDTECKEN (1)                     
021700        MOVE 'CD'           TO 2109-MID2-KDOI (1)                         
021800        MOVE SPACE          TO 2109-MID2-CLEARGROUP(1)                    
021900        MOVE RELS-KVBEART-Q TO 2109-MID2-KVOI (1)                         
022000        MOVE W-TIREGDAT     TO 2109-MID2-TIUPPDAT (1)                     
022100                                                                          
022200        COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17             
022300        PERFORM IMS-PURG-ALT-MSG-2109                                     
022310     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 BB-SKAPA-RADKO SECTION.                                                  
022700                                                                          
022800     MOVE RELS-IDDISTR        TO RAD-IDDISTR                              
022900     MOVE RELS-IDKUNDNR       TO RAD-IDKUNDNR                             
023000     MOVE SPACE               TO RAD-IDKUNDRF                             
023100     MOVE RELS-IDKUNDRF (3:5) TO RAD-IDORDNR5                             
023200     MOVE RELS-IDARTNR        TO RAD-IDARTNR                              
023300     MOVE +1                  TO RAD-IDLOPNR                              
023400     MOVE RELS-BERADREF       TO RAD-BERADREF                             
023500     MOVE NEJ                 TO RAD-FLERS                                
023600     MOVE RELS-IDANSK         TO RAD-IDANSK                               
023700     MOVE RELS-IDKONTO        TO RAD-IDKONTO                              
023800     MOVE RELS-IDKST          TO RAD-IDKST                                
023900     MOVE RELS-IDANALYS       TO RAD-IDANALYS                             
024000     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
024100     MOVE WC-CDC-SE           TO RAD-IDDC                                 
024200                                 RAD-IDDC-RO                              
024300     MOVE 'CD'                TO RAD-KDOI                                 
024400     MOVE SPACE               TO RAD-CLEARGROUP                           
024500     MOVE RELS-KDDSP          TO RAD-KDDSP                                
024600     MOVE RELS-KDFAKTYP       TO RAD-KDFAKTYP                             
024700     MOVE RELS-KDFRAKT        TO RAD-KDFRAKT                              
024800     MOVE RELS-KDKVBRYT       TO RAD-KDKVBRYT                             
024900     MOVE RELS-KDORDING       TO RAD-KDORDING                             
025000     MOVE RELS-KDORDKL        TO RAD-KDORDKL                              
025100     MOVE RELS-KDPRODSL       TO RAD-KDPRODSL                             
025200     MOVE 20                  TO RAD-KDRAPRIO                             
025300     MOVE ZERO                TO RAD-KDROO                                
025400     MOVE 1                   TO RAD-KDSTARAD                             
025500     MOVE RELS-KDTPOTYP       TO RAD-KDTPOTYP                             
025600     MOVE RELS-KDVRINFO       TO RAD-KDVRINFO                             
025700     MOVE RELS-KVBEART-Q      TO RAD-KVART                                
025800                                 RAD-KVBEART-Q                            
025900     MOVE ZERO                TO RAD-KVRO                                 
026000     MOVE RELS-PRARTNTO       TO RAD-PRARTNTO                             
026010     MOVE RELS-PRAVCOST       TO RAD-PRAVCOST                             
026100     MOVE RELS-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
026200     MOVE RELS-REKSIFFR       TO RAD-REKSIFFR                             
026300     MOVE ZERO                TO RAD-TIAVBOKN                             
026400     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
026500     MOVE ZERO                TO RAD-TIRES                                
026600     MOVE ZERO                TO RAD-DARODAT                              
026700     MOVE RELS-TITPO          TO RAD-TITPO                                
026800     MOVE RELS-KDPRTYP        TO RAD-KDPRTYP                              
026900     MOVE RELS-BEVOLREF       TO RAD-BEVOLREF                             
027000     MOVE RELS-FLINVEST       TO RAD-FLINVEST                             
027100     MOVE RELS-FLPRTILL       TO RAD-FLPRTILL                             
027200     MOVE RELS-BEKUNDRF       TO RAD-BEKUNDRF                             
027300     MOVE RELS-IDKAMPRF       TO RAD-IDKAMPRF                             
027400     MOVE RELS-IDLEVNR        TO RAD-IDLEVNR                              
027500     MOVE RELS-IDSYSTEM       TO RAD-IDSYSTEM                             
027600     MOVE EXEKV-TID           TO RAD-TIREGTID                             
027700                                                                          
027800     MOVE RELS-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                         
027900     MOVE RELS-TIREPDAT       TO RAD-TIREPDAT                             
028000     MOVE RELS-IDKUNDRF-WIP   TO RAD-IDKUNDRF-WIP                         
028300     .                                                                    
028400                                                                          
028500 BC-UPPDATERA-ARTREG SECTION.                                             
028600                                                                          
028700     MOVE RELS-IDARTNR TO W-IDARTNR                                       
028800     PERFORM IMS-GHU-ARTM-WDK901                                          
028900     ADD RELS-KVBEART-Q TO ART-SUTPO-TOT                                  
029000     PERFORM IMS-REPL-ARTM-WDK9                                           
029100     MOVE DAGENS-DAT-TIAAVV  TO W-DABEHOV                                 
029200     IF DAGENS-DAT-TIAAVV NOT = ZERO                                      
029300       MOVE 20        TO W-DABEHOV (1:2)                                  
029400     ELSE                                                                 
029500        MOVE 999999  TO W-DABEHOV                                         
029600     END-IF                                                               
029700     PERFORM IMS-GHNP-ARTM-WDK911                                         
029800     IF SEGMENT-SAKNAS                                                    
029900       MOVE DAGENS-DAT-TIAAVV   TO ANT-DABEHOV                            
030000       IF DAGENS-DAT-TIAAVV NOT = ZERO                                    
030100         MOVE 20        TO ANT-DABEHOV (1:2)                              
030200       ELSE                                                               
030300         MOVE 999999  TO ANT-DABEHOV                                      
030400       END-IF                                                             
030500       MOVE RELS-KVBEART-Q TO ANT-SUTPO-PB                                
030600       MOVE ZERO           TO ANT-SUTPO-EJPB                              
030700       PERFORM IMS-ISRT-ARTM-WDK911                                       
030800     ELSE                                                                 
030900       ADD RELS-KVBEART-Q TO ANT-SUTPO-PB                                 
031000       PERFORM IMS-REPL-ARTM-WDK9                                         
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400     EJECT                                                                
031500* --- IMS SEKTIONER ---                                                   
031600 IMS-PURG-ALT-MSG-2109 SECTION.                                           
031700     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
031800     MOVE SPACE TO GODK-STATUSKODER                                       
031900     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
032000     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032300     SKIP3                                                                
032400 IMS-ISRT-ORDP-WDA501 SECTION.                                            
032500                                                                          
032600     MOVE   'WLORDP01 ' TO SSA1                                           
032700     MOVE '  II' TO GODK-STATUSKODER                                      
032800     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
032900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
033000     PERFORM IMS-STATUSKONTROLL                                           
033100     .                                                                    
033200     EJECT                                                                
033300 IMS-GHU-ARTM-WDK901 SECTION.                                             
033400                                                                          
033500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
033600          DELIMITED BY SIZE INTO SSA1                                     
033700     MOVE '  ' TO GODK-STATUSKODER                                        
033800     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA3 SSA1                    
033900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200     EJECT                                                                
034300 IMS-GHNP-ARTM-WDK911 SECTION.                                            
034400                                                                          
034500     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
034600          DELIMITED BY SIZE INTO SSA1                                     
034700     MOVE '  GE' TO GODK-STATUSKODER                                      
034800     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA3 SSA1                   
034900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
035000     PERFORM IMS-STATUSKONTROLL                                           
035100     .                                                                    
035200     EJECT                                                                
035300 IMS-REPL-ARTM-WDK9 SECTION.                                              
035400                                                                          
035500     MOVE '  ' TO GODK-STATUSKODER                                        
035600     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA3                        
035700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
035800     PERFORM IMS-STATUSKONTROLL                                           
035900     .                                                                    
036000     EJECT                                                                
036100 IMS-ISRT-ARTM-WDK911 SECTION.                                            
036200                                                                          
036300     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
036400          DELIMITED BY SIZE INTO SSA1                                     
036500     MOVE 'WLARTM11 ' TO SSA2                                             
036600     MOVE '  ' TO GODK-STATUSKODER                                        
036700     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA3 SSA1 SSA2              
036800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
036900     PERFORM IMS-STATUSKONTROLL                                           
037000     .                                                                    
037100     EJECT                                                                
037200 IMS-STATUSKONTROLL SECTION.                                              
037300                                                                          
037400     SET STATUS-IX TO 1                                                   
037500     SEARCH GODK-STATUS                                                   
037600       AT END                                                             
037700       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
037800       DELIMITED BY SIZE INTO FELTEXT                                     
037900       CALL FELLOG                                                        
038000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
038100     END-SEARCH                                                           
038200     .                                                                    
