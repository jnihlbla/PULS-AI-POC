000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2215610.                                    
000300*              COBOL CONVERSION AID PO 5785-ABJ                           
000400*              CONVERSION DATE 05/25/91 19:27:05.                         
000500*AUTHOR.                     IDK, GÖTEBORG.                               
000600*DATE-WRITTEN.               JUNI 1979.                                   
000700*    SKIP3                                                                
000800*REMARKS.                                                                 
000900                                                                          
001000*    FUNKTION.                                                            
001100*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2215600                       
001200*        SOM SKÖTER OM SAMTLIGA IMS-CALL ÅT DETSAMMA                      
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP3                                                                
001600 DATA DIVISION.                                                           
001700     EJECT                                                                
001800 WORKING-STORAGE SECTION.                                                 
001900                                                                          
002000                                                                          
002100*    -- CHECKED BY WY2000                                                 
002200 77  INDENT-I PIC X(40) VALUE                                             
002300                         'W2215610 91/05/25 TIME 12.49 VILMAII'.          
002400 77  W-DEL                     PIC S9(7)   VALUE ZERO  COMP-3.            
002500 77  W-REP                     PIC S9(7)   VALUE ZERO  COMP-3.            
002600     SKIP2                                                                
002700 01  FELTEXT.                                                             
002800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
002900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003000 01  WS-TIAAVV                   PIC 9(04)   VALUE ZERO.                  
003100 01  WS-DATE-TIME.                                                        
003200     03  DAGENS-TID              PIC 9(8)    VALUE ZERO.                  
003300     03  FILLER    REDEFINES DAGENS-TID.                                  
003400         05  DAGENS-HHMMSS       PIC 9(6).                                
003500         05  FILLER              PIC 9(2).                                
003600     03  DAGENS-DATUM            PIC 9(6) VALUE ZERO.                     
003700*                                                                         
003800 01  W.                                                                   
003900     05  W-IDLOPNRM-CHECK-X.                                              
004000         10  W-IDLOPNRM-1-2   PIC 9(2).                                   
004100         10  W-IDLOPNRM-3-9   PIC 9(7).                                   
004200     05  W-IDLOPNRM-CHECK REDEFINES W-IDLOPNRM-CHECK-X PIC 9(9).          
004300                                                                          
004400     05  WS-DAAVROP-AVS       PIC 9(6).                                   
004500     05  FILLER  REDEFINES WS-DAAVROP-AVS.                                
004600         07  WS-DAAVROP-SS    PIC 9(2).                                   
004700         07  WS-DAAVROP-AAVV  PIC 9(4).                                   
004800     05  WS-DALEVBSK-AVS      PIC 9(8).                                   
004900     05  FILLER  REDEFINES WS-DALEVBSK-AVS.                               
005000         07  WS-DALEVBSK-SS   PIC 9(2).                                   
005100         07  WS-DALEVBSK-AAMMDD                                           
005200                              PIC 9(6).                                   
005300                                                                          
005400 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
005500     10  W-IDLOPNRM           PIC 9(8).                                   
005600     10  FILLER REDEFINES W-IDLOPNRM.                                     
005700         20  W-VVDLLLL.                                                   
005800             30  W-DAT-VV     PIC 9(2).                                   
005900             30  W-DAT-DAG    PIC 9.                                      
006000             30  W-LOPNR      PIC 9(4).                                   
006100         20  W-KTRLSIFFRA     PIC 9(1).                                   
006200     10  CHECK-FLTB           PIC S9       VALUE +7   COMP  SYNC.         
006300     10  CHECK-FLTC           PIC 9(7)     VALUE 2121212.                 
006400     10  CHECK-FLTD           PIC S9       VALUE +7   COMP  SYNC.         
006500     10  CHECK-FLTF           PIC 9(2)     VALUE 10.                      
006600     10  CHECK-FLTG           PIC X(1)     VALUE 'B'.                     
006700                                                                          
006800*--------------------------------------- KONSTANTER                       
006900 01  KONSTANTER.                                                          
007000     05  JA                  PIC X       VALUE 'J'.                       
007100     05  NEJ                 PIC X       VALUE 'N'.                       
007200     SKIP3                                                                
007300*--------------------------------------- GENERELLA SUBRUTINER             
007400 01      SUBPROGRAM.                                                      
007500     05    CBLTDLI           PIC X(8)    VALUE 'CBLTDLI '.                
007600     05    FELLOG            PIC X(8)    VALUE 'FELLOG  '.                
007700     05    CHECK             PIC X(8)    VALUE 'CHECK   '.                
007800     03    WDATKONV          PIC X(8)    VALUE 'WDATKONV'.                
007900     EJECT                                                                
008000*01  -COPY WDATAREA                                                       
008100     EJECT                                                                
008200*--------------------------------------- ARBETSAREOR TILL                 
008300*                                        IMS-SEKTIONERNA                  
008400 01      IMS-WS.                                                          
008500     05  FILLER              PIC X(8)    VALUE 'IMS-WS  '.                
008600     SKIP3                                                                
008700*--------------------------------------- STATUSKOD FRÅN IMS               
008800     05  STATUS-WS           PIC XX.                                      
008900         88  SEGMENT-FINNS               VALUE '  '.                      
009000         88  SEGMENT-FINNS-GA            VALUE 'GA'.                      
009100         88  SEGMENT-FINNS-GK            VALUE 'GK'.                      
009200         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
009300     SKIP3                                                                
009400     05  SSA1                PIC X(60).                                   
009500     05  SSA2                PIC X(60).                                   
009600     05  SSA3                PIC X(60).                                   
009700     05  SSA4                PIC X(60).                                   
009800     SKIP3                                                                
009900     05  GODK-STATUSKODER.                                                
010000         10  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.           
010100     SKIP3                                                                
010200 01  W-WDD901KY-X.                                                        
010300     05  W-IDARTNR           PIC S9(9)               COMP-3.              
010400     05  W-IDDC              PIC X(2).                                    
010500                                                                          
010600 01  W-IDLEVNR-X.                                                         
010700     05  W-IDLEVNR           PIC X(5).                                    
010800                                                                          
010900 01  W-DAXLEVSP-X.                                                        
011000     05  W-DAXLEVSP          PIC  9(6).                                   
011100                                                                          
011200 01  W-WDD905KY-X.                                                        
011300     03  W-DAAVROP-X.                                                     
011400         05  W-DAAVROP       PIC  9(6)    VALUE ZERO.                     
011500     03  W-TILEVDAG-X.                                                    
011600         05  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.              
011700                                                                          
011800 01  W-IDLOPNRM-PL-X.                                                     
011900     05  W-IDLOPNRM-PL       PIC S9(9)               COMP-3.              
012000                                                                          
012100 01  W-W6D1BSEQ-X.                                                        
012200     05  W-W6D1BSEQ-IDLOPNRM PIC S9(9)               COMP-3.              
012300                                                                          
012400 01  W-DALEVBSK-X.                                                        
012500     05  W-DALEVBSK          PIC  9(8).                                   
012600                                                                          
012700 01  W-IDLEVBSK-X.                                                        
012800     05  W-IDLEVBSK          PIC S9                  COMP-3.              
012900                                                                          
013000     SKIP2                                                                
013100*01      -COPY W0003                                                      
013200     EJECT                                                                
013300 01  DLI-IO-AREA             PIC X(200)  VALUE SPACE.                     
013400     SKIP3                                                                
013500*01  WLINLB01 -COPY WDD901  -PRE WDD901-     -RED DLI-IO-AREA.            
013600     EJECT                                                                
013700*01  WLINLB11 -COPY WDD902  -PRE WDD902-     -RED DLI-IO-AREA.            
013800     EJECT                                                                
013900*01  WLINLB04 -COPY WDD904  -PRE WDD904-     -RED DLI-IO-AREA.            
014000     EJECT                                                                
014100*01  WLINLB05 -COPY WDD905  -PRE WDD905-     -RED DLI-IO-AREA.            
014200     EJECT                                                                
014300*01  WLINLB06 -COPY WDD906  -PRE WDD906-     -RED DLI-IO-AREA.            
014400     EJECT                                                                
014500*01  WLINLB24 -COPY WDD924  -PRE WDD924-     -RED DLI-IO-AREA             
014600     EJECT                                                                
014700*01  WLINLB25 -COPY WDD925  -PRE WDD925-     -RED DLI-IO-AREA             
014800     EJECT                                                                
014900*01  W6INLA11 -COPY W6D111                   -RED DLI-IO-AREA.            
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200*01  AREA -COPY W221L561 -PRE LINK-                                       
015300     EJECT                                                                
015400*01  -COPY W0008     -PRE INLB-L-.                                        
015500        05  FILLER                       PIC X.                           
015600*01  -COPY W0008     -PRE INLB-U-.                                        
015700        05  FILLER                       PIC X.                           
015800*01  -COPY W0008     -PRE W6INLA-                                         
015900        05  FILLER                       PIC X.                           
016000     EJECT                                                                
016100 PROCEDURE DIVISION  USING LINK-AREA INLB-L-PCB INLB-U-PCB                
016200                                                 W6INLA-PCB.              
016300     ENTRY 'DLITCBL' USING LINK-AREA INLB-L-PCB INLB-U-PCB                
016400                                                 W6INLA-PCB.              
016500                                                                          
016600     MOVE JA  TO LINK-FLAGGA-ANROP                                        
016700                                                                          
016800     ACCEPT DAGENS-DATUM FROM DATE                                        
016900     ACCEPT DAGENS-TID   FROM TIME                                        
017000                                                                          
017100     IF LINK-LAES-ROT       PERFORM A-LAES-ROT                            
017200     ELSE                                                                 
017300     IF LINK-LAES-NASTA-UNDER-ROT                                         
017400                             PERFORM B-LAES-NASTA-UNDER-ROT               
017500     ELSE                                                                 
017600     IF LINK-GHU-WDD902      PERFORM C-GHU-WDD902                         
017700     ELSE                                                                 
017800     IF LINK-GHU-WDD905      PERFORM E-GHU-WDD905                         
017900     ELSE                                                                 
018000     IF LINK-GHU-WDD906      PERFORM F-GHU-WDD906                         
018100     ELSE                                                                 
018200     IF LINK-GHU-WDD924      PERFORM G-GHU-WDD924                         
018300     ELSE                                                                 
018400     IF LINK-REPL-WDD924     PERFORM J-REPL-WDD924                        
018500     ELSE                                                                 
018600     IF LINK-DELETE          PERFORM H-DELETE                             
018700     ELSE                                                                 
018800     IF LINK-LAES-INL-PARTI PERFORM I-LAS-INLEV-REG-PARTISEG              
018900     ELSE                                                                 
019000     IF LINK-GHU-WDD925      PERFORM K-GHU-WDD925                         
019100     END-IF                                                               
019200     END-IF                                                               
019300     END-IF                                                               
019400     END-IF                                                               
019500     END-IF                                                               
019600     END-IF                                                               
019700     END-IF                                                               
019800     END-IF                                                               
019900     END-IF                                                               
020000     END-IF                                                               
020100                                                                          
020200     SKIP3                                                                
020300     MOVE ZERO TO RETURN-CODE                                             
020400     GOBACK                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 A-LAES-ROT SECTION.                                                      
020800                                                                          
020900***  ÄNDRAT ALLA LINK1- TILL LINK- EFTERSOM HUVUDPROGRAMMET               
021000***  ANVÄNDER    LINK1- OCH  LINK2- TILL SAMMA LINK-AREA HIT.             
021100                                                                          
021200     MOVE LOW-VALUE  TO LINK-IO-AREA                                      
021300     MOVE '11'           TO W-IDDC                                        
021400     PERFORM IMS-LAES-ROT                                                 
021500                                                                          
021600     IF  SEGMENT-FINNS                                                    
021700       MOVE WDD901-IDARTNR TO LINK-IDARTNR                                
021800       MOVE WDD901-IDDC    TO LINK-IDDC                                   
021900     ELSE                                                                 
022000       MOVE       NEJ      TO LINK-FLAGGA-ANROP                           
022100     END-IF                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 B-LAES-NASTA-UNDER-ROT SECTION.                                          
022500                                                                          
022600     MOVE LOW-VALUE  TO LINK-IO-AREA                                      
022700     PERFORM IMS-LAES-NASTA-UNDER-ROT                                     
022800                                                                          
022900     IF  SEGMENT-FINNS                                                    
023000     OR  SEGMENT-FINNS-GA                                                 
023100     OR  SEGMENT-FINNS-GK                                                 
023200       IF  INLB-L-SEG-NAME-FB = 'WLINLB11'                                
023300         MOVE WDD902-IDLEVNR TO LINK-IDLEVNR                              
023400         MOVE WDD902-KVBR    TO LINK-KVBR                                 
023500       ELSE                                                               
023600         IF  INLB-L-SEG-NAME-FB = 'WLINLB22'                              
023700           MOVE WDD904-KVBEST-PL TO LINK-KVBEST-PL                        
023800         ELSE                                                             
023900           IF  INLB-L-SEG-NAME-FB = 'WLINLB23'                            
024000             MOVE WDD905-KDAVROP         TO LINK-KDAVROP                  
024100             MOVE WDD905-KVAVROP         TO LINK-KVAVROP                  
024200             MOVE WDD905-DAAVROP-AVS     TO WS-DAAVROP-AVS                
024300             MOVE WS-DAAVROP-AAVV        TO LINK-TIAVROP-AVS              
024400***       IF WDD905-TIAVRDAT-INL > ZERO                                   
024500             MOVE WDD905-TIAVRDAT-INL    TO DAT-I-TIDATUM                 
024600             MOVE 'AAMMDD'               TO DAT-KDDATFORM                 
024700             CALL WDATKONV USING            DAT-KDDATFORM                 
024800                                            DAT-I-TIDATUM                 
024900                                            DAT-O-TIDATUM                 
025000                                            DAT-KDSVAR                    
025100             IF DAT-KDSVAR-FEL                                            
025200               MOVE ' FEL VID ANROP TILL DATKONV 2'                       
025300                                         TO FELTEXT-STR                   
025400               DISPLAY FELTEXT                                            
025500               CALL FELLOG                                                
025600             ELSE                                                         
025700               MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV                     
025800               MOVE WS-TIAAVV            TO LINK-TIAVROP-INL              
025900             END-IF                                                       
026000***       ELSE                                                            
026100***            MOVE ZERO                 TO LINK-TIAVROP-INL              
026200***       END-IF                                                          
026300           ELSE                                                           
026400             IF  INLB-L-SEG-NAME-FB = 'WLINLB31'                          
026500               MOVE WDD906-KVAVROP-AVB     TO LINK-KVAVROP-AVB            
026600               MOVE WDD906-IDLOPNRM-PL     TO LINK-IDLOPNRM-PL            
026700             ELSE                                                         
026800               IF INLB-L-SEG-NAME-FB = 'WLINLB24'                         
026900                 MOVE WDD924-LEV-DALEVBSK-AVS TO                          
027000                 WS-DALEVBSK-AVS                                          
027100                 MOVE WS-DALEVBSK-AAMMDD      TO                          
027200                 LINK-TILEVBSK-AVS                                        
027300                 MOVE WDD924-LEV-TILEVBSK-INL    TO                       
027400                 LINK-TILEVBSK-INLC1                                      
027500                 MOVE ZERO                       TO                       
027600                 LINK-TILEVBSK-INLC2                                      
027700                 MOVE WDD924-LEV-KVAVIS-BSKKVAR    TO                     
027800                 LINK-KVAVIS-BSKKVARC1                                    
027900                 MOVE ZERO                         TO                     
028000                 LINK-KVAVIS-BSKKVARC2                                    
028100                 MOVE WDD924-LEV-FLSENLEV    TO                           
028200                 LINK-FLSENLEVC1                                          
028300                 MOVE ZERO                   TO                           
028400                 LINK-FLSENLEVC2                                          
028500               ELSE                                                       
028600                 IF  INLB-L-SEG-NAME-FB = 'WLINLB25'                      
028700                   MOVE WDD925-INFO-IDLEVBSK TO LINK-IDLEVBSK             
028800                   MOVE WDD925-INFO-TELEVBSK TO LINK-TELEVBSK             
028900                   MOVE WDD925-INFO-TIBORT TO LINK-TIBORT                 
029000                 END-IF                                                   
029100               END-IF                                                     
029200             END-IF                                                       
029300           END-IF                                                         
029400         END-IF                                                           
029500       END-IF                                                             
029600     ELSE                                                                 
029700       MOVE   NEJ  TO LINK-FLAGGA-ANROP                                   
029800     END-IF                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 C-GHU-WDD902 SECTION.                                                    
030200                                                                          
030300     MOVE LINK-IDARTNR   TO W-IDARTNR                                     
030400     MOVE LINK-IDLEVNR   TO W-IDLEVNR                                     
030500     MOVE LOW-VALUE      TO LINK-IO-AREA                                  
030600                                                                          
030700     PERFORM IMS-GHU-WDD902                                               
030800     .                                                                    
030900     EJECT                                                                
031000 E-GHU-WDD905 SECTION.                                                    
031100                                                                          
031200     MOVE LINK-IDARTNR           TO W-IDARTNR                             
031300     MOVE LINK-IDLEVNR           TO W-IDLEVNR                             
031400     MOVE LINK-TIAVROP-AVS       TO WS-DAAVROP-AAVV                       
031500     IF WS-DAAVROP-AAVV > 5000                                            
031600        MOVE 19                  TO WS-DAAVROP-SS                         
031700     ELSE                                                                 
031800        MOVE 20                  TO WS-DAAVROP-SS                         
031900     END-IF                                                               
032000     MOVE WS-DAAVROP-AVS         TO W-DAAVROP                             
032100     MOVE LOW-VALUE              TO LINK-IO-AREA                          
032200                                                                          
032300     PERFORM IMS-GHU-WDD905                                               
032400     .                                                                    
032500     EJECT                                                                
032600 F-GHU-WDD906 SECTION.                                                    
032700                                                                          
032800     MOVE LINK-IDARTNR       TO W-IDARTNR                                 
032900     MOVE LINK-IDLEVNR       TO W-IDLEVNR                                 
033000     MOVE LINK-TIAVROP-AVS   TO WS-DAAVROP-AAVV                           
033100     IF WS-DAAVROP-AAVV > 5000                                            
033200        MOVE 19              TO WS-DAAVROP-SS                             
033300     ELSE                                                                 
033400        MOVE 20              TO WS-DAAVROP-SS                             
033500     END-IF                                                               
033600     MOVE WS-DAAVROP-AVS     TO W-DAAVROP                                 
033700     MOVE LINK-IDLOPNRM-PL   TO W-IDLOPNRM-PL                             
033800     MOVE LOW-VALUE          TO LINK-IO-AREA                              
033900                                                                          
034000     PERFORM IMS-GHU-WDD906                                               
034100     .                                                                    
034200     EJECT                                                                
034300 G-GHU-WDD924 SECTION.                                                    
034400                                                                          
034500     MOVE LINK-IDARTNR       TO W-IDARTNR                                 
034600     MOVE LINK-IDLEVNR       TO W-IDLEVNR                                 
034700     MOVE LINK-TILEVBSK-AVS  TO WS-DALEVBSK-AAMMDD                        
034800     IF WS-DAAVROP-AAVV > 5000                                            
034900        MOVE 19              TO WS-DAAVROP-SS                             
035000     ELSE                                                                 
035100        MOVE 20              TO WS-DAAVROP-SS                             
035200     END-IF                                                               
035300     MOVE WS-DALEVBSK-AVS    TO W-DALEVBSK                                
035400                                                                          
035500     PERFORM IMS-GHU-WDD924                                               
035600     .                                                                    
035700     EJECT                                                                
035800 H-DELETE SECTION.                                                        
035900                                                                          
036000     PERFORM IMS-DELETE                                                   
036100     ADD +1 TO  W-DEL                                                     
036110     DISPLAY  'DELETE' W-DEL  'SEGMENT' INLB-U-SEG-NAME-FB                
036200     MOVE LOW-VALUE          TO LINK-IO-AREA                              
036300     .                                                                    
036400     EJECT                                                                
036500 I-LAS-INLEV-REG-PARTISEG SECTION.                                        
036600                                                                          
036700     MOVE LINK-IDLOPNRM-PL TO W-IDLOPNRM-CHECK                            
036800                                                                          
036900     MOVE W-IDLOPNRM-3-9    TO W-VVDLLLL                                  
037000     CALL CHECK USING W-VVDLLLL    CHECK-FLTB CHECK-FLTC                  
037100          CHECK-FLTD  W-KTRLSIFFRA CHECK-FLTF CHECK-FLTG                  
037200                                                                          
037300     MOVE W-IDLOPNRM        TO W-W6D1BSEQ-IDLOPNRM                        
037400                                                                          
037500     PERFORM IMS-LAS-INLEV-REG-PARTISEG                                   
037600                                                                          
037700     IF  SEGMENT-FINNS                                                    
037800       MOVE ART-FLKLAR TO LINK-FLKLAR                                     
037900     ELSE                                                                 
038000       MOVE     JA     TO LINK-FLKLAR                                     
038100       MOVE     NEJ    TO LINK-FLAGGA-ANROP                               
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 J-REPL-WDD924 SECTION.                                                   
038600     SKIP3                                                                
038700     MOVE LINK-FLSENLEVC1 TO WDD924-LEV-FLSENLEV                          
038800     MOVE DAGENS-DATUM     TO WDD924-LEV-TIREGDAT                         
038900     MOVE DAGENS-HHMMSS    TO WDD924-LEV-TIREGTID                         
039000     PERFORM IMS-INLB-REPLACE                                             
039100     MOVE    JA            TO LINK-FLAGGA-ANROP                           
039110     ADD +1 TO W-REP                                                      
039120     DISPLAY 'REPLACE' W-REP  'SEGMENT' INLB-U-SEG-NAME-FB                
039200     MOVE LOW-VALUE        TO LINK-IO-AREA                                
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 K-GHU-WDD925 SECTION.                                                    
039700                                                                          
039800     MOVE LINK-IDARTNR       TO W-IDARTNR                                 
039900     MOVE LINK-IDLEVNR       TO W-IDLEVNR                                 
040000     MOVE LINK-IDLEVBSK      TO W-IDLEVBSK                                
040100     PERFORM IMS-GHU-WDD925                                               
040200     .                                                                    
040300     EJECT                                                                
040400***  ---  I M S   SEKTIONER ------------------------                      
040500                                                                          
040600 IMS-LAES-ROT SECTION.                                                    
040700                                                                          
040800     STRING 'WLINLB01(IDDC     =' W-IDDC ')'                              
040900     DELIMITED BY SIZE INTO SSA1                                          
041000     MOVE '  GEGB'           TO GODK-STATUSKODER                          
041100     CALL CBLTDLI USING GN INLB-L-PCB DLI-IO-AREA SSA1                    
041200     MOVE INLB-L-STATUS-CODE TO STATUS-WS                                 
041300     PERFORM IMS-STATUSKONTROLL                                           
041400     .                                                                    
041500     SKIP3                                                                
041600 IMS-LAES-NASTA-UNDER-ROT SECTION.                                        
041700                                                                          
041800     MOVE '  GEGAGK'         TO GODK-STATUSKODER                          
041900     CALL CBLTDLI USING GNP INLB-L-PCB DLI-IO-AREA                        
042000     MOVE INLB-L-STATUS-CODE TO STATUS-WS                                 
042100     PERFORM IMS-STATUSKONTROLL                                           
042200     .                                                                    
042300     EJECT                                                                
042400 IMS-GHU-WDD902 SECTION.                                                  
042500                                                                          
042600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
042700     DELIMITED BY SIZE INTO SSA1                                          
042800     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
042900     DELIMITED BY SIZE INTO SSA2                                          
043000     MOVE '  '   TO GODK-STATUSKODER SSA3 SSA4                            
043100     CALL CBLTDLI USING GHU INLB-U-PCB DLI-IO-AREA SSA1 SSA2              
043200     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500     SKIP3                                                                
043600 IMS-GHU-WDD905 SECTION.                                                  
043700                                                                          
043800     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
043900     DELIMITED BY SIZE INTO SSA1                                          
044000     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
044100     DELIMITED BY SIZE INTO SSA2                                          
044200     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-X ')'                         
044300     DELIMITED BY SIZE INTO SSA3                                          
044400     MOVE '  '  TO GODK-STATUSKODER SSA4                                  
044500     CALL CBLTDLI USING GHU INLB-U-PCB DLI-IO-AREA SSA1 SSA2 SSA3         
044600     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     SKIP3                                                                
045000 IMS-GHU-WDD906 SECTION.                                                  
045100                                                                          
045200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
045300     DELIMITED BY SIZE INTO SSA1                                          
045400     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
045500     DELIMITED BY SIZE INTO SSA2                                          
045600     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-X ')'                         
045700     DELIMITED BY SIZE INTO SSA3                                          
045800     STRING 'WLINLB31(IDLOPNRM =' W-IDLOPNRM-PL-X ')'                     
045900     DELIMITED BY SIZE INTO SSA4                                          
046000     MOVE '  '   TO GODK-STATUSKODER                                      
046100     CALL CBLTDLI USING GHU INLB-U-PCB DLI-IO-AREA SSA1 SSA2              
046200                                                   SSA3 SSA4              
046300     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
046400     PERFORM IMS-STATUSKONTROLL                                           
046500     .                                                                    
046600     EJECT                                                                
046700 IMS-GHU-WDD924 SECTION.                                                  
046800                                                                          
046900     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
047000     DELIMITED BY SIZE INTO SSA1                                          
047100     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
047200     DELIMITED BY SIZE INTO SSA2                                          
047300     STRING 'WLINLB24(DALEVBSK =' W-DALEVBSK-X ')'                        
047400     DELIMITED BY SIZE INTO SSA3                                          
047500     MOVE '  '  TO GODK-STATUSKODER SSA4                                  
047600     CALL CBLTDLI USING GHU INLB-U-PCB DLI-IO-AREA SSA1 SSA2 SSA3         
047700     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
047800     PERFORM IMS-STATUSKONTROLL                                           
047900     .                                                                    
048000     SKIP3                                                                
048100 IMS-GHU-WDD925 SECTION.                                                  
048200                                                                          
048300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
048400     DELIMITED BY SIZE INTO SSA1                                          
048500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
048600     DELIMITED BY SIZE INTO SSA2                                          
048700     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-X ')'                        
048800     DELIMITED BY SIZE INTO SSA3                                          
048900     MOVE '  '  TO GODK-STATUSKODER SSA4                                  
049000     CALL CBLTDLI USING GHU INLB-U-PCB DLI-IO-AREA SSA1 SSA2 SSA3         
049100     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-INLB-REPLACE SECTION.                                                
049600     MOVE '  '   TO GODK-STATUSKODER                                      
049700     CALL CBLTDLI USING REPL INLB-U-PCB DLI-IO-AREA                       
049800     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
049900     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
050100     SKIP3                                                                
050200 IMS-DELETE SECTION.                                                      
050300                                                                          
050400     MOVE '  '           TO GODK-STATUSKODER                              
050500     CALL CBLTDLI USING DLET INLB-U-PCB DLI-IO-AREA                       
050600     MOVE INLB-U-STATUS-CODE TO STATUS-WS                                 
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900     EJECT                                                                
051000 IMS-LAS-INLEV-REG-PARTISEG SECTION.                                      
051100                                                                          
051200     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
051300          DELIMITED BY SIZE INTO SSA1                                     
051400     MOVE '  GE' TO GODK-STATUSKODER                                      
051500     MOVE '    ' TO SSA2 SSA3 SSA4                                        
051600     CALL CBLTDLI USING GU W6INLA-PCB DLI-IO-AREA SSA1                    
051700     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
051800     PERFORM IMS-STATUSKONTROLL                                           
051900                                                                          
052000     .                                                                    
052100     SKIP3                                                                
052200 IMS-STATUSKONTROLL SECTION.                                              
052300                                                                          
052400     SET STATUS-IX   TO 1                                                 
052500     SEARCH GODK-STATUS AT END DISPLAY 'IMS-WS =' STATUS-WS               
052600                               DISPLAY 'SSA1   =' SSA1                    
052700                               DISPLAY 'SSA2   =' SSA2                    
052800                               DISPLAY 'SSA3   =' SSA3                    
052900                               DISPLAY 'SSA4   =' SSA4                    
053000                               CALL FELLOG                                
053100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053200         CONTINUE                                                         
053300     END-SEARCH                                                           
053400     CONTINUE                                                             
053500     .                                                                    
