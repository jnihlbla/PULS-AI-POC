000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411TPO3.                                                
000500 AUTHOR.         ANNELIE ENGLUND                                          
000600 DATE-WRITTEN.   APRIL 1990                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET TAR EMOT TPO3-MÄRKTA RADER FRÅN HUVUD-                
001200*        PROGRAMMET. DESSA RADER LÄGGS UPP PÅ DEN PASSIVA                 
001300*        ORDERRADSKÖN.                                                    
001400*                                                                         
001500*        PROGRAMMET LÄSER OCH                                             
001600*                   UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
001700*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)  TRANSAKTIONSBAS             
001800*                                                                         
001900*                                                                         
002000*    LÄNKAREA: W411TPO3                                                   
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W411TPO3'.            
003000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500 77  EXEKV-TID                   PIC X(6).                                
003600                                                                          
003700 77  W-TIREGDAT                  PIC 9(6).                                
003800                                                                          
003900 01  TITPO-TIAAVV                PIC 9(4)    VALUE ZERO.                  
004000 01  W-TIAVV REDEFINES TITPO-TIAAVV.                                      
004100     03  W-TIAVV-A               PIC 9(1).                                
004200     03  W-TIAVV-AVV             PIC 9(3).                                
004300*      --- VALID IDDC CODES                                               
004400*                                                                         
004500*01    -COPY WWDCKONS                                                     
004600       EJECT                                                              
004700                                                                          
004800 01  GENERELLA-SUBPROGRAM.                                                
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005300                                                                          
005400     EJECT                                                                
005500*    --- PARAMETRAR TILL ABEND                                            
005600                                                                          
005700 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
005800                                                                          
005900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
006000                                                                          
006100*01  -COPY WDATAREA                                                       
006200                                                                          
006300     EJECT                                                                
006400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006500*                                                                         
006600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006700     SKIP3                                                                
006800 01  NYCKLAR-TILL-DLI.                                                    
006900                                                                          
007000     03  W-WDA501KY-MIN.                                                  
007100         05  W-IDDISTR-MIN       PIC S9(5) COMP-3   VALUE ZERO.           
007200         05  W-IDKUNDNR-MIN      PIC S9(7) COMP-3   VALUE ZERO.           
007300         05  W-IDKUNDRF-MIN      PIC X(10)          VALUE SPACE.          
007400         05  W-IDARTNR-MIN       PIC S9(9) COMP-3   VALUE ZERO.           
007500         05  W-IDLOPNR-MIN       PIC S9(3) COMP-3   VALUE ZERO.           
007600                                                                          
007700     03  W-WDA501KY-MAX.                                                  
007800         05  W-IDDISTR-MAX       PIC S9(5) COMP-3   VALUE ZERO.           
007900         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3   VALUE ZERO.           
008000         05  W-IDKUNDRF-MAX      PIC X(10)          VALUE ZERO.           
008100         05  W-IDARTNR-MAX       PIC S9(9) COMP-3   VALUE ZERO.           
008200         05  W-IDLOPNR-MAX       PIC S9(3) COMP-3   VALUE ZERO.           
008300                                                                          
008400     03  W-IDARTNR-X.                                                     
008500         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
008600                                                                          
008700     03  W-KDSTARAD-X.                                                    
008800         05  W-KDSTARAD          PIC X        VALUE SPACE.                
008900                                                                          
009000     03  W-TITPO-X.                                                       
009100         05  W-TITPO             PIC S9(7)    VALUE ZERO COMP-3.          
009200                                                                          
009300     03  W-KDTPOTYP-X.                                                    
009400         05  W-KDTPOTYP          PIC S9       VALUE ZERO COMP-3.          
009500                                                                          
009600*    --- AREOR TILL TRANSAR                                               
009700     SKIP2                                                                
009800*01  -COPY WDGZRYA                                                        
009900*                                                                         
010000     EJECT                                                                
010100*                                                                         
010200      EJECT                                                               
010300*01  -COPY W092P001  -PRE SORT-                                           
010400*                                                                         
010500      EJECT                                                               
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FINNS                       VALUE '  '.                  
010900     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
011000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(150).                              
011700 01  SSA2                        PIC X(150).                              
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012400     SKIP3                                                                
012500 01  DLI-IO-AREA.                                                         
012600     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
012700     SKIP3                                                                
012800     03  WLORDP01 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDA501                                                 
013000     EJECT                                                                
013100     03  WLZZAC01 REDEFINES IO-AREA.                                      
013200*        05  -COPY WDGZ01     -PRE ZZAC-                                  
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*                                                                         
013600*   -COPY W411TPO3                                                        
013700     EJECT                                                                
013800*01  -COPY W0008      -PRE ORDP-                                          
013900     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100*01  -COPY W0008      -PRE ZZAC-                                          
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400 PROCEDURE DIVISION  USING TPO3-W411TPO3 ORDP-PCB                         
014500                           ZZAC-PCB.                                      
014600                                                                          
014700     MOVE ZERO   TO TPO3-KDORDBEK                                         
014800     MOVE NEJ    TO TPO3-FLKLAR                                           
014900                                                                          
015000     IF TPO3-IDSYSTEM NOT = 'OREL'                                        
015100        IF TPO3-KDTPOTYP = 3 AND TPO3-FLFORBI = NEJ                       
015200           MOVE ZERO TO TPO3-KDORDBEK                                     
015300           PERFORM A-INIT                                                 
015400           PERFORM C-LAGG-UPP-RAD                                         
015500           IF TPO3-KDORDBEK = ZERO                                        
015600               IF TPO3-IDSYSTEM NOT = 'VR'                                
015700                  PERFORM E-SKAPA-TRANS-TILL-VR                           
015800               END-IF                                                     
015900               MOVE JA TO TPO3-FLKLAR                                     
016000           ELSE                                                           
016100              MOVE NEJ TO TPO3-FLKLAR                                     
016200           END-IF                                                         
016300        ELSE                                                              
016400           MOVE NEJ TO TPO3-FLKLAR                                        
016500        END-IF                                                            
016600     END-IF                                                               
016700                                                                          
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     ACCEPT EXEKV-TID FROM TIME                                           
017400                                                                          
017500     MOVE 'IDAG' TO DAT-KDDATFORM                                         
017600     MOVE ZERO   TO DAT-I-TIDATUM                                         
017700     CALL WDATKONV USING DAT-KDDATFORM,                                   
017800                         DAT-I-TIDATUM,                                   
017900                         DAT-O-TIDATUM,                                   
018000                         DAT-KDSVAR                                       
018100     IF DAT-KDSVAR-OK                                                     
018200       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
018300     ELSE                                                                 
018400       MOVE 'FEL FRÅN SUBPROGRAM W411TPO3 I SECTION A' TO FELTEXT         
018500       CALL ABEND USING RKOD-ABEND                                        
018600     END-IF                                                               
018700                                                                          
018800     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
018900     MOVE TPO3-TITPO TO DAT-I-TIDATUM                                     
019000     CALL WDATKONV USING DAT-KDDATFORM,                                   
019100                         DAT-I-TIDATUM,                                   
019200                         DAT-O-TIDATUM,                                   
019300                         DAT-KDSVAR                                       
019400     IF DAT-KDSVAR-OK                                                     
019500       MOVE DAT-TIAAVV-GRP TO TITPO-TIAAVV                                
019600     ELSE                                                                 
019700       MOVE 'FEL FRÅN SUBPROGRAM W411TPO3 I SECTION A' TO FELTEXT         
019800       CALL ABEND USING RKOD-ABEND                                        
019900     END-IF                                                               
020000                                                                          
020100     .                                                                    
020200     EJECT                                                                
020300 C-LAGG-UPP-RAD SECTION.                                                  
020400                                                                          
020500     MOVE TPO3-IDDISTR       TO W-IDDISTR-MIN                             
020600                                W-IDDISTR-MAX                             
020700     MOVE TPO3-IDKUNDNR      TO W-IDKUNDNR-MIN                            
020800                                W-IDKUNDNR-MAX                            
020900     MOVE SPACE              TO W-IDKUNDRF-MIN                            
021000                                W-IDKUNDRF-MAX                            
021100     MOVE TPO3-IDKUNDRF(3:5) TO W-IDKUNDRF-MIN                            
021200                                W-IDKUNDRF-MAX                            
021300     MOVE TPO3-IDARTNR       TO W-IDARTNR-MIN                             
021400                                W-IDARTNR-MAX                             
021500     MOVE ZERO               TO W-IDLOPNR-MIN                             
021600     MOVE 999                TO W-IDLOPNR-MAX                             
021700     MOVE 3                  TO W-KDTPOTYP                                
021800     MOVE 1                  TO W-KDSTARAD                                
021900     MOVE TPO3-TITPO         TO W-TITPO                                   
022000     PERFORM IMS-GU-ORDP-WDA501                                           
022100     IF SEGMENT-FINNS                                                     
022200       MOVE 72 TO TPO3-KDORDBEK                                           
022300     ELSE                                                                 
022400       PERFORM CA-SKAPA-RADKO                                             
022500       PERFORM IMS-ISRT-ORDP-WDA501                                       
022600       IF SEGMENT-FINNS-REDAN                                             
022700         PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                             
022800           ADD 1 TO RAD-IDLOPNR                                           
022900           PERFORM IMS-ISRT-ORDP-WDA501                                   
023000         END-PERFORM                                                      
023100       END-IF                                                             
023200     END-IF                                                               
023300                                                                          
023400     .                                                                    
023500     EJECT                                                                
023600 CA-SKAPA-RADKO SECTION.                                                  
023700                                                                          
023800     MOVE TPO3-IDDISTR        TO RAD-IDDISTR                              
023900     MOVE TPO3-IDKUNDNR       TO RAD-IDKUNDNR                             
024000     MOVE SPACE               TO RAD-IDKUNDRF                             
024100     MOVE TPO3-IDKUNDRF (3:5) TO RAD-IDORDNR5                             
024200     MOVE TPO3-IDARTNR        TO RAD-IDARTNR                              
024300     MOVE 1                   TO RAD-IDLOPNR                              
024400     MOVE TPO3-BERADREF       TO RAD-BERADREF                             
024500     MOVE NEJ                 TO RAD-FLERS                                
024600     MOVE TPO3-IDANSK         TO RAD-IDANSK                               
024700     MOVE TPO3-IDKONTO        TO RAD-IDKONTO                              
024800     MOVE TPO3-IDKST          TO RAD-IDKST                                
024900     MOVE TPO3-IDANALYS       TO RAD-IDANALYS                             
025000     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
025100     MOVE WC-CDC-SE           TO RAD-IDDC                                 
025200                                 RAD-IDDC-RO                              
025300     MOVE 'DT'                TO RAD-KDOI                                 
025400     MOVE SPACE               TO RAD-CLEARGROUP                           
025500     MOVE TPO3-KDDSP          TO RAD-KDDSP                                
025600     MOVE TPO3-KDFAKTYP       TO RAD-KDFAKTYP                             
025700     MOVE TPO3-KDFRAKT        TO RAD-KDFRAKT                              
025800     MOVE TPO3-KDKVBRYT       TO RAD-KDKVBRYT                             
025900     MOVE TPO3-KDORDING       TO RAD-KDORDING                             
026000     MOVE TPO3-KDORDKL        TO RAD-KDORDKL                              
026100     MOVE TPO3-KDPRODSL       TO RAD-KDPRODSL                             
026200     MOVE 20                  TO RAD-KDRAPRIO                             
026300     MOVE ZERO                TO RAD-KDROO                                
026400     MOVE 1                   TO RAD-KDSTARAD                             
026500     MOVE TPO3-KDTPOTYP       TO RAD-KDTPOTYP                             
026600     MOVE TPO3-KDVRINFO       TO RAD-KDVRINFO                             
026700     MOVE TPO3-KVBEART-Q      TO RAD-KVART                                
026800                                 RAD-KVBEART-Q                            
026900     MOVE ZERO                TO RAD-KVRO                                 
027000     MOVE TPO3-PRARTNTO       TO RAD-PRARTNTO                             
027100     MOVE TPO3-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
027200     MOVE TPO3-REKSIFFR       TO RAD-REKSIFFR                             
027300     MOVE ZERO                TO RAD-TIAVBOKN                             
027400     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
027500     MOVE ZERO                TO RAD-TIRES                                
027600     MOVE ZERO                TO RAD-DARODAT                              
027700     MOVE TPO3-TITPO          TO RAD-TITPO                                
027800     MOVE TPO3-KDPRTYP        TO RAD-KDPRTYP                              
027900     MOVE TPO3-BEVOLREF       TO RAD-BEVOLREF                             
028000     MOVE TPO3-FLINVEST       TO RAD-FLINVEST                             
028100     MOVE TPO3-FLPRTILL       TO RAD-FLPRTILL                             
028200     MOVE JA                  TO RAD-FLTPOBEK                             
028300     MOVE TPO3-BEKUNDRF       TO RAD-BEKUNDRF                             
028400     MOVE TPO3-IDKAMPRF       TO RAD-IDKAMPRF                             
028500     MOVE TPO3-IDLEVNR        TO RAD-IDLEVNR                              
028600     MOVE TPO3-IDSYSTEM       TO RAD-IDSYSTEM                             
028700     MOVE EXEKV-TID           TO RAD-TIREGTID                             
028800     MOVE ZERO                TO RAD-DASENDAT                             
028900                                 RAD-TISENBEK-KL                          
028910     MOVE SPACE               TO RAD-KDROPACK                             
028920     MOVE SPACE               TO RAD-IDARBREF                             
029000                                                                          
029100     MOVE TPO3-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                         
029200     MOVE TPO3-TIREPDAT       TO RAD-TIREPDAT                             
029300     MOVE TPO3-IDKUNDRF-WIP   TO RAD-IDKUNDRF-WIP                         
029500     MOVE +0                  TO RAD-PRAVCOST                             
029600     .                                                                    
029700                                                                          
029800 E-SKAPA-TRANS-TILL-VR SECTION.                                           
029900                                                                          
030000     MOVE +1         TO ZZAC-IDLOGLOP                                     
030100     MOVE 'RYA'      TO RYA-IDPTYP                                        
030200                        ZZAC-IDPTYP                                       
030300     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
030400     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
030500     MOVE TPO3-IDDISTR   TO RYA-IDDISTR                                   
030600     MOVE TPO3-IDKUNDNR  TO RYA-IDKUNDNR                                  
030700     MOVE TPO3-IDKUNDRF  TO RYA-IDKUNDRF                                  
030800     MOVE TPO3-IDARTNR   TO RYA-IDARTNR                                   
030900     MOVE TPO3-REKSIFFR  TO RYA-REKSIFFR                                  
031000     MOVE TPO3-KVBEART-Q TO RYA-KVBEART                                   
031100     MOVE TPO3-TITPO     TO RYA-TITPO                                     
031200     MOVE TPO3-KDTPOTYP  TO RYA-KDTPOTYP                                  
031300     MOVE 0              TO RYA-KDVRTPO                                   
031400     MOVE TPO3-KDVRINFO  TO RYA-KDVRINFO                                  
031500                                                                          
031600     MOVE RYA-WDGZRYA   TO ZZAC-LOGGPOST                                  
031700     MOVE SPACE         TO ZZAC-SORTPOST                                  
031800     PERFORM IMS-ISRT-ZZAC-WDG6                                           
031900     IF SEGMENT-FINNS-REDAN                                               
032000       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
032100         ADD +1 TO ZZAC-IDLOGLOP                                          
032200         PERFORM IMS-ISRT-ZZAC-WDG6                                       
032300       END-PERFORM                                                        
032400     END-IF                                                               
032500                                                                          
032600     .                                                                    
032700     EJECT                                                                
032800* --- IMS SEKTIONER ---                                                   
032900 IMS-GU-ORDP-WDA501 SECTION.                                              
033000                                                                          
033100     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN                          
033200                    '&WDA501KY<=' W-WDA501KY-MAX                          
033300                    '&KDSTARAD =' W-KDSTARAD-X                            
033400                    '&KDTPOTYP =' W-KDTPOTYP-X                            
033500                    '&TITPO    =' W-TITPO-X ')'                           
033600          DELIMITED BY SIZE INTO SSA1                                     
033700     MOVE '  GE' TO GODK-STATUSKODER                                      
033800     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA SSA1                      
033900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200     SKIP3                                                                
034300 IMS-ISRT-ORDP-WDA501 SECTION.                                            
034400                                                                          
034500     MOVE   'WLORDP01 ' TO SSA1                                           
034600     MOVE '  II' TO GODK-STATUSKODER                                      
034700     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
034800     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
034900     PERFORM IMS-STATUSKONTROLL                                           
035000     .                                                                    
035100     EJECT                                                                
035200 IMS-ISRT-ZZAC-WDG6 SECTION.                                              
035300                                                                          
035400     MOVE   'WLZZAC01 ' TO SSA1                                           
035500     MOVE '  II' TO GODK-STATUSKODER                                      
035600     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
035700     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
035800     PERFORM IMS-STATUSKONTROLL                                           
035900     .                                                                    
036000     EJECT                                                                
036100 IMS-STATUSKONTROLL SECTION.                                              
036200                                                                          
036300     SET STATUS-IX TO 1                                                   
036400     SEARCH GODK-STATUS                                                   
036500       AT END                                                             
036600       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
036700       DELIMITED BY SIZE INTO FELTEXT                                     
036800       CALL FELLOG                                                        
036900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
037000     END-SEARCH                                                           
037100     .                                                                    
