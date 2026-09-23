000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411TPO6.                                                
000500 AUTHOR.         ANNELIE ENGLUND                                          
000600 DATE-WRITTEN.   APRIL 1990                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*                                                                         
001200*        PROGRAMMET TAR EMOT TPO6-MÄRKTA RADER FRÅN SVARS-                
001300*        PROGRAMMEN W402*3, VILKA FALLIT FÖR KONTROLLEN AV                
001400*        STORT UTTAG.                                                     
001500*        RADEN LÄGGS UPP PÅ TPO/RESTORDERRADSKÖN WLORDP.                  
001600*        LARM SKAPAS TILL ANSKAFFARE SOM SEDAN SKA BEDÖMA RADEN.          
001700*        ORDERINGÅNGSTRANS W2T109X SKAPAS.                                
001800*                                                                         
001900*        PROGRAMMET LÄSER OCH                                             
002000*                   UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
002100*        PROGRAMMET LÄSER OCH                                             
002200*                   UPPDATERAR WLXXBU (WDR5)  LARMKÖ                      
002300*        PROGRAMMET LÄSER      WLXXBV (WDR2)  TIDTABELL                   
002400*        PROGRAMMET LÄSER      WLXXBX (WDR2)  ÖVERSÄTTN ANSK-LARM         
002500*                                                                         
002600*                                                                         
002700*    LÄNKAREA: W411TPO6                                                   
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W411TPO6'.            
003600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  WS-KDOI                     PIC X(2).                                
004100 77  W-IDLOPNR                   PIC S9(3)   COMP-3.                      
004200 77  EXEKV-TID                   PIC X(6).                                
004300 77  W-TIREGDAT                  PIC 9(6).                                
004400 77  W-TIAAP-AVBOK               PIC 9(3).                                
004500                                                                          
004600 01  DAGENS-DAT.                                                          
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
006200*      --- VALID IDDC CODES                                               
006300*                                                                         
006400*01    -COPY WWDCKONS                                                     
006500                                                                          
006600*      --- BYTESARTIKLAR                                                  
006700*                                                                         
006800*01    -COPY WWBYT03                                                      
006900       EJECT                                                              
007000                                                                          
007100*01    -COPY WWPRODSL                                                     
007200       EJECT                                                              
007300                                                                          
007400 01  GENERELLA-SUBPROGRAM.                                                
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007900     03  W411TIME                PIC X(8)    VALUE 'W411TIME'.            
008000                                                                          
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL ABEND                                            
008300 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
008400                                                                          
008500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
008600*01  -COPY WDATAREA                                                       
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM W411TIME                              
008900*01  -COPY W411TIME                                                       
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200 01  NYCKLAR-TILL-DLI.                                                    
009300                                                                          
009400     03  W-WDGX2225-X.                                                    
009500         05  W-IDHTYP-2225       PIC X(4)     VALUE '2225'.               
009600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
009700                                                                          
009800     03  W-WDGX2223-X.                                                    
009900         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
010000         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
010100         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
010200                                                                          
010300     03  W-WDGX2224-X.                                                    
010400         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
010500         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
010600         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
010700                                                                          
010800     03  W-WDGX2231-X.                                                    
010900         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
011000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
011100                                                                          
011200     03  W-WDGX2232-X.                                                    
011300         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
011400         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
011500                                                                          
011600     03  W-KDSEGKEY-X.                                                    
011700         05  W-KDSEGKEY          PIC X.                                   
011800                                                                          
011900     03  W-IDARTNR-X.                                                     
012000         05  W-IDARTNR           PIC S9(9)    VALUE +0 COMP-3.            
012100                                                                          
012200     03  W-IDDC-X.                                                        
012300         05  W-IDDC              PIC X(2)     VALUE SPACE.                
012400         EJECT                                                            
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FINNS                       VALUE '  '.                  
012800     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
012900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200 01  FILLER                      PIC X(16)   VALUE 'K711-IO-AREA'.        
014300     SKIP3                                                                
014400 01  DLI-IO-AREA-K711.                                                    
014500*    03  -COPY WDK711                                                     
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014800     SKIP3                                                                
014900 01  DLI-IO-AREA.                                                         
015000     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
015100     SKIP3                                                                
015200     03  WLORDP01 REDEFINES IO-AREA.                                      
015300*        05  -COPY WDA501                                                 
015400     EJECT                                                                
015500     03  WLXXBU01 REDEFINES IO-AREA.                                      
015600*        05  -COPY WDGX2223                                               
015700     EJECT                                                                
015800     03  WLXXBU11 REDEFINES IO-AREA.                                      
015900*        05  -COPY WDGX2224                                               
016000     EJECT                                                                
016100     03  WLXXBV11 REDEFINES IO-AREA.                                      
016200*        05  -COPY WDGX2226                                               
016300     EJECT                                                                
016400     03  WLXXBX11 REDEFINES IO-AREA.                                      
016500*        05  -COPY WDGX2232                                               
016600     EJECT                                                                
016700*    MSG-AREA FÖR HOPP TILL W20109                                        
016800 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
016900 01  W-PROG-TO-PROG-SW-1.                                                 
017000     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
017100     03  2109-Z1                   PIC X.                                 
017200     03  2109-Z2                   PIC X.                                 
017300     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
017400     03  2109-IDTRANS              PIC X(4)  VALUE 'TPO6'.                
017500     03  2109-KDMFSFOR             PIC X     VALUE '1'.                   
017600*    03  -COPY W2I10902    -PRE 2109-                                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900*   -COPY W411TPO6                                                        
018000     EJECT                                                                
018100*                                                                         
018200*01    -COPY W0009    -PRE 2109-                                          
018300     EJECT                                                                
018400*01  -COPY W0008      -PRE ORDP-                                          
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700*01  -COPY W0008      -PRE XXBU-                                          
018800     05  FILLER                  PIC X.                                   
018900     EJECT                                                                
019000*01  -COPY W0008      -PRE XXBV-                                          
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008      -PRE XXBX-                                          
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008      -PRE ARTS-                                          
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900 01  TIME-4437-PCB               PIC X.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING TPO6-W411TPO6 2109-PCB ORDP-PCB                
020200                                         XXBU-PCB XXBV-PCB                
020300                                         XXBX-PCB ARTS-PCB                
020400                                         TIME-4437-PCB.                   
020500                                                                          
020600     IF TPO6-KDTPOTYP = 6 AND TPO6-FLFORBI = NEJ                          
020700       MOVE ZERO     TO TPO6-KDORDBEK                                     
020800                                                                          
020900       PERFORM A-INIT                                                     
021000       PERFORM B-BERAKNA-BEKRTIDPKT                                       
021100       PERFORM D-LAGG-UPP-LARMKO-OCH-RAD                                  
021200                                                                          
021300       MOVE JA       TO TPO6-FLKLAR                                       
021400     ELSE                                                                 
021500       MOVE NEJ      TO TPO6-FLKLAR                                       
021600     END-IF                                                               
021700                                                                          
021800     GOBACK                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 A-INIT SECTION.                                                          
022200                                                                          
022300     ACCEPT EXEKV-TID FROM TIME                                           
022400                                                                          
022500     MOVE 'IDAG' TO DAT-KDDATFORM                                         
022600     MOVE ZERO   TO DAT-I-TIDATUM                                         
022700     CALL WDATKONV USING DAT-KDDATFORM,                                   
022800                         DAT-I-TIDATUM,                                   
022900                         DAT-O-TIDATUM,                                   
023000                         DAT-KDSVAR                                       
023100     IF DAT-KDSVAR-OK                                                     
023200       MOVE DAT-TIAAVV-GRP TO DAGENS-DAT                                  
023300       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
023400       MOVE DAT-TIAAP      TO W-TIAAP-AVBOK                               
023500     ELSE                                                                 
023600       MOVE 'FEL FRÅN SUBPROGRAM W411TPO6 I SECTION A' TO FELTEXT         
023700       CALL ABEND USING RKOD-ABEND                                        
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 B-BERAKNA-BEKRTIDPKT SECTION.                                            
024200                                                                          
024300     MOVE '1' TO W-KDSEGKEY                                               
024400     PERFORM IMS-GU-XXBV-WDR210                                           
024500     MOVE 2226-KVARBTIM(TPO6-KDORDKL , TPO6-KDTPOTYP)                     
024600                        TO TIME-TIARB                                     
024700     MOVE WC-CDC-SE     TO TIME-IDDC                                      
024800     MOVE W-TIREGDAT    TO TIME-START-TIAAMMDD                            
024900     MOVE EXEKV-TID     TO TIME-START-TIHHMMSS                            
025000     MOVE 022           TO TIME-KDCALL                                    
025100     MOVE ZERO          TO TIME-STOPDAT                                   
025200                                                                          
025300     CALL W411TIME USING TIME-W411TIME TIME-4437-PCB.                     
025400                                                                          
025500     IF TIME-KDSVAR-OK                                                    
025600       MOVE TIME-STOP-TIAAMMDD TO W-TISENBEK-DAG                          
025700       MOVE TIME-STOP-TIHHMMSS TO W-TISENBEK-KL                           
025800     ELSE                                                                 
025900       MOVE 'FEL FRÅN SUBPROGRAM W411TPO6 I SECTION B' TO FELTEXT         
026000       CALL ABEND USING RKOD-ABEND                                        
026100     END-IF                                                               
026200                                                                          
026300     .                                                                    
026400     EJECT                                                                
026500 D-LAGG-UPP-LARMKO-OCH-RAD SECTION.                                       
026600                                                                          
026700     MOVE TPO6-IDANSK TO W-IDANSK-2232                                    
026800     PERFORM IMS-GU-XXBX-WDR220                                           
026900     IF SEGMENT-FINNS                                                     
027000       MOVE 2232-IDANSK-LARM TO W-IDANSK-2223                             
027100     ELSE                                                                 
027200       MOVE ZERO TO W-IDANSK-2223                                         
027300     END-IF                                                               
027400     MOVE '2223'        TO 2223-IDHTYP                                    
027500     MOVE W-IDANSK-2223 TO 2223-IDANSK                                    
027600     MOVE LOW-VALUE     TO 2223-LOW-VALUE                                 
027700     PERFORM IMS-ISRT-XXBU-WDR501                                         
027800     PERFORM IMS-GHU-XXBU-WDR501                                          
027900     MOVE W-TISENBEK-DAG TO W-TISENBEK-DAG-2224                           
028000     MOVE W-TISENBEK-KL  TO W-TISENBEK-KL-2224                            
028100     MOVE 100            TO W-KDLARM-2224                                 
028200     PERFORM IMS-GNP-XXBU-WDR550                                          
028300     IF SEGMENT-FINNS                                                     
028400       PERFORM UNTIL SEGMENT-SAKNAS                                       
028500         ADD 1 TO W-TISENBEK-KL-2224                                      
028600         PERFORM DA-KOLLA-TIDEN                                           
028700         PERFORM IMS-GNP-XXBU-WDR550                                      
028800       END-PERFORM                                                        
028900     END-IF                                                               
029000     PERFORM DB-LAGG-UPP-RAD                                              
029100     MOVE W-TISENBEK-DAG-2224 TO 2224-TISENBEK-DAG                        
029200     MOVE W-TISENBEK-KL-2224  TO 2224-TISENBEK-KL                         
029300     MOVE 100                 TO 2224-KDLARM                              
029400     MOVE TPO6-IDARTNR        TO 2224-IDARTNR                             
029500     MOVE WC-CDC-SE           TO 2224-IDDC                                
029600     MOVE JA                  TO 2224-FLNYLARM                            
029700     MOVE TPO6-IDDISTR        TO 2224-IDDISTR                             
029800     MOVE TPO6-IDKUNDNR       TO 2224-IDKUNDNR                            
029900     MOVE TPO6-IDKUNDRF       TO 2224-IDKUNDRF                            
030000     MOVE W-IDLOPNR           TO 2224-IDLOPNR                             
030100     MOVE W-TIREGDAT          TO 2224-TIREGDAT                            
030200     MOVE SPACE               TO 2224-IDTRANS                             
030300                                 2224-KDMFSFOR                            
030400     MOVE ZERO                TO 2224-IDKR                                
030500     MOVE SPACE               TO 2224-IDLEVNR                             
030600     PERFORM IMS-ISRT-XXBU-WDR550                                         
030700     .                                                                    
030800     EJECT                                                                
030900 DA-KOLLA-TIDEN SECTION.                                                  
031000                                                                          
031100     MOVE W-TISENBEK-KL-2224 TO W-TISENBEK-KL-UPPD                        
031200     IF W-TISENBEK-KL-SS > 59                                             
031300       ADD 1 TO W-TISENBEK-KL-MM                                          
031400       MOVE ZERO TO W-TISENBEK-KL-SS                                      
031500       MOVE W-TISENBEK-KL-UPPD TO W-TISENBEK-KL-2224                      
031600       IF W-TISENBEK-KL-MM > 59                                           
031700         ADD 1 TO W-TISENBEK-KL-HH                                        
031800         MOVE ZERO TO W-TISENBEK-KL-MM                                    
031900         MOVE W-TISENBEK-KL-UPPD TO W-TISENBEK-KL-2224                    
032000       END-IF                                                             
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 DB-LAGG-UPP-RAD SECTION.                                                 
032500                                                                          
032600     IF TPO6-KDORDING < 3                                                 
032700       PERFORM DBA-SKAPA-2109-TRANS                                       
032800     END-IF                                                               
032900     PERFORM DBB-SKAPA-RADKO                                              
033000                                                                          
033100     PERFORM IMS-ISRT-ORDP-WDA501                                         
033200     IF SEGMENT-FINNS-REDAN                                               
033300       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
033400         ADD 1 TO RAD-IDLOPNR                                             
033500         PERFORM IMS-ISRT-ORDP-WDA501                                     
033600       END-PERFORM                                                        
033700     END-IF                                                               
033800     MOVE RAD-IDLOPNR TO W-IDLOPNR                                        
033900     .                                                                    
034000     EJECT                                                                
034100 DBA-SKAPA-2109-TRANS SECTION.                                            
034200                                                                          
034300*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
034400     MOVE TPO6-IDARTNR       TO BYT03-IDARTNR                             
034500     IF NOT BYT03-OBJEKT                                                  
034600                                                                          
034700       MOVE SPACE          TO 2109-MID2-W2I10902                          
034800                                                                          
034900       MOVE 1              TO 2109-MID2-KVANTART                          
035000       MOVE TPO6-IDARTNR   TO 2109-MID2-IDARTNR (1)                       
035100       MOVE TPO6-IDDC-DAY  TO 2109-MID2-IDDC (1)                          
035200       MOVE '+'            TO 2109-MID2-KDTECKEN (1)                      
035300                                                                          
035400       IF TPO6-KDOI = 'XX'                                                
035500          MOVE TPO6-KDOI   TO 2109-MID2-KDOI (1)                          
035600          MOVE TPO6-CLEARGROUP                                            
035700                           TO 2109-MID2-CLEARGROUP(1)                     
035800       ELSE                                                               
035900          PERFORM DBAA-FIXA-KDOI                                          
036000          MOVE WS-KDOI     TO 2109-MID2-KDOI (1)                          
036100          MOVE SPACE       TO 2109-MID2-CLEARGROUP(1)                     
036200       END-IF                                                             
036300       MOVE TPO6-KVBEART-Q TO 2109-MID2-KVOI (1)                          
036400       MOVE W-TIREGDAT     TO 2109-MID2-TIUPPDAT (1)                      
036500                                                                          
036600       COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17              
036700       PERFORM IMS-PURG-ALT-MSG-2109                                      
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 DBAA-FIXA-KDOI SECTION.                                                  
037200                                                                          
037300     MOVE TPO6-KDPRODSL       TO TEST-KDPRODSL                            
040700     IF KDPRODSL-VOLVO-EMB OR                                             
040800        TPO6-KDORDING = +1                                                
040900       MOVE 'CD'              TO WS-KDOI                                  
041000     ELSE                                                                 
041100       MOVE 'DT'              TO WS-KDOI                                  
041200     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 DBB-SKAPA-RADKO SECTION.                                                 
041700                                                                          
041800     MOVE TPO6-IDDISTR        TO RAD-IDDISTR                              
041900     MOVE TPO6-IDKUNDNR       TO RAD-IDKUNDNR                             
042000     MOVE SPACE               TO RAD-IDKUNDRF                             
042100     MOVE TPO6-IDKUNDRF (3:5) TO RAD-IDORDNR5                             
042200     MOVE TPO6-IDARTNR        TO RAD-IDARTNR                              
042300     MOVE +1                  TO RAD-IDLOPNR                              
042400     MOVE TPO6-BERADREF       TO RAD-BERADREF                             
042500     MOVE NEJ                 TO RAD-FLERS                                
042600     MOVE TPO6-IDANSK         TO RAD-IDANSK                               
042700     MOVE TPO6-IDKONTO        TO RAD-IDKONTO                              
042800     MOVE TPO6-IDKST          TO RAD-IDKST                                
042900     MOVE TPO6-IDANALYS       TO RAD-IDANALYS                             
043000     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
043100     MOVE WC-CDC-SE           TO RAD-IDDC                                 
043200                                 RAD-IDDC-RO                              
043300     MOVE TPO6-KDDSP          TO RAD-KDDSP                                
043400     MOVE TPO6-KDFAKTYP       TO RAD-KDFAKTYP                             
043500     MOVE TPO6-KDFRAKT        TO RAD-KDFRAKT                              
043600     MOVE TPO6-KDKVBRYT       TO RAD-KDKVBRYT                             
043700     MOVE TPO6-KDORDING       TO RAD-KDORDING                             
043800     IF  TPO6-KDORDING = 3                                                
043900       MOVE SPACE             TO RAD-KDOI                                 
044000       MOVE SPACE             TO RAD-CLEARGROUP                           
044100     ELSE                                                                 
044200       IF TPO6-KDOI = 'XX'                                                
044300         MOVE TPO6-KDOI       TO RAD-KDOI                                 
044400         MOVE TPO6-CLEARGROUP                                             
044500                              TO RAD-CLEARGROUP                           
044600       ELSE                                                               
044700         MOVE WS-KDOI         TO RAD-KDOI                                 
044800         MOVE SPACE           TO RAD-CLEARGROUP                           
044900       END-IF                                                             
045000     END-IF                                                               
045100     MOVE TPO6-KDORDKL        TO RAD-KDORDKL                              
045200     MOVE TPO6-KDPRODSL       TO RAD-KDPRODSL                             
045300     MOVE 20                  TO RAD-KDRAPRIO                             
045400     MOVE ZERO                TO RAD-KDROO                                
045500     MOVE 1                   TO RAD-KDSTARAD                             
045600     MOVE TPO6-KDTPOTYP       TO RAD-KDTPOTYP                             
045700     MOVE TPO6-KDVRINFO       TO RAD-KDVRINFO                             
045800     MOVE TPO6-KVBEART-Q      TO RAD-KVART                                
045900                                 RAD-KVBEART-Q                            
046000     MOVE ZERO                TO RAD-KVRO                                 
046100     MOVE TPO6-PRARTNTO       TO RAD-PRARTNTO                             
046200     MOVE TPO6-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
046300     MOVE TPO6-REKSIFFR       TO RAD-REKSIFFR                             
046400     MOVE ZERO                TO RAD-TIAVBOKN                             
046500     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
046600     MOVE ZERO                TO RAD-TIRES                                
046700     MOVE ZERO                TO RAD-DARODAT                              
046800     MOVE ZERO                TO RAD-TITPO                                
046900     MOVE TPO6-KDPRTYP        TO RAD-KDPRTYP                              
047000     MOVE TPO6-BEVOLREF       TO RAD-BEVOLREF                             
047100     MOVE TPO6-FLINVEST       TO RAD-FLINVEST                             
047200     MOVE TPO6-FLPRTILL       TO RAD-FLPRTILL                             
047300     MOVE NEJ                 TO RAD-FLTPOBEK                             
047400     MOVE TPO6-BEKUNDRF       TO RAD-BEKUNDRF                             
047500     MOVE TPO6-IDKAMPRF       TO RAD-IDKAMPRF                             
047600     MOVE TPO6-IDLEVNR        TO RAD-IDLEVNR                              
047700     MOVE TPO6-IDSYSTEM       TO RAD-IDSYSTEM                             
047800     MOVE EXEKV-TID           TO RAD-TIREGTID                             
047900     MOVE W-TISENBEK-DAG-2224 TO RAD-DASENDAT                             
048000     IF  W-TISENBEK-DAG-2224 NOT = ZERO                                   
048100       IF  W-TISENBEK-DAG-2224 < 500000                                   
048200         MOVE 20              TO RAD-DASENDAT (1:2)                       
048300       ELSE                                                               
048400         IF  W-TISENBEK-DAG-2224 < 999999                                 
048500           MOVE 19            TO RAD-DASENDAT (1:2)                       
048600         ELSE                                                             
048700           MOVE 99999999      TO RAD-DASENDAT                             
048800         END-IF                                                           
048900       END-IF                                                             
049000     END-IF                                                               
049100     MOVE W-TISENBEK-KL-2224  TO RAD-TISENBEK-KL                          
049200                                                                          
049300     MOVE TPO6-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                         
049400     MOVE TPO6-TIREPDAT       TO RAD-TIREPDAT                             
049500     MOVE TPO6-IDKUNDRF-WIP   TO RAD-IDKUNDRF-WIP                         
049700     MOVE +0                  TO RAD-PRAVCOST                             
049710     MOVE SPACE               TO RAD-KDROPACK                             
049720     MOVE SPACE               TO RAD-IDARBREF                             
049800     .                                                                    
049900                                                                          
050000* --- IMS SEKTIONER ---                                                   
050100     SKIP2                                                                
050200 IMS-PURG-ALT-MSG-2109 SECTION.                                           
050300     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
050400     MOVE SPACE TO GODK-STATUSKODER                                       
050500     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
050600     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900     SKIP3                                                                
051000 IMS-GU-XXBV-WDR210 SECTION.                                              
051100                                                                          
051200     STRING 'WLXXBV01(WDGXKEY  =' W-WDGX2225-X ')'                        
051300          DELIMITED BY SIZE INTO SSA1                                     
051400     STRING 'WLXXBV11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
051500          DELIMITED BY SIZE INTO SSA2                                     
051600     MOVE '  ' TO GODK-STATUSKODER                                        
051700     CALL CBLTDLI USING GU XXBV-PCB DLI-IO-AREA SSA1 SSA2                 
051800     MOVE XXBV-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     .                                                                    
052100     EJECT                                                                
052200 IMS-GU-XXBX-WDR220 SECTION.                                              
052300                                                                          
052400     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
052500          DELIMITED BY SIZE INTO SSA1                                     
052600     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
052700          DELIMITED BY SIZE INTO SSA2                                     
052800     MOVE '  GE' TO GODK-STATUSKODER                                      
052900     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA SSA1 SSA2                 
053000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053400 IMS-ISRT-ORDP-WDA501 SECTION.                                            
053500                                                                          
053600     MOVE   'WLORDP01 ' TO SSA1                                           
053700     MOVE '  II' TO GODK-STATUSKODER                                      
053800     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
053900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
054000     PERFORM IMS-STATUSKONTROLL                                           
054100     .                                                                    
054200     EJECT                                                                
054300 IMS-ISRT-XXBU-WDR501 SECTION.                                            
054400                                                                          
054500     MOVE 'WLXXBU01 ' TO SSA1                                             
054600     MOVE '  II' TO GODK-STATUSKODER                                      
054700     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA SSA1                    
054800     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
054900     PERFORM IMS-STATUSKONTROLL                                           
055000     .                                                                    
055100     EJECT                                                                
055200 IMS-GHU-XXBU-WDR501 SECTION.                                             
055300                                                                          
055400     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
055500          DELIMITED BY SIZE INTO SSA1                                     
055600     MOVE '  ' TO GODK-STATUSKODER                                        
055700     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-AREA SSA1                     
055800     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUSKONTROLL                                           
056000     .                                                                    
056100     EJECT                                                                
056200 IMS-GNP-XXBU-WDR550 SECTION.                                             
056300                                                                          
056400     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
056500          DELIMITED BY SIZE INTO SSA1                                     
056600     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
056700          DELIMITED BY SIZE INTO SSA2                                     
056800     MOVE '  GE' TO GODK-STATUSKODER                                      
056900     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1 SSA2                
057000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300     EJECT                                                                
057400 IMS-ISRT-XXBU-WDR550 SECTION.                                            
057500                                                                          
057600     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
057700          DELIMITED BY SIZE INTO SSA1                                     
057800     STRING 'WLXXBU11 '                                                   
057900          DELIMITED BY SIZE INTO SSA2                                     
058000     MOVE '  II' TO GODK-STATUSKODER                                      
058100     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA SSA1 SSA2               
058200     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058500     EJECT                                                                
058600 IMS-GU-ARTS11 SECTION.                                                   
058700                                                                          
058800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
058900          DELIMITED BY SIZE INTO SSA1                                     
059000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
059100          DELIMITED BY SIZE INTO SSA2                                     
059200     MOVE '  GE' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-K711 SSA1 SSA2            
059400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     SKIP3                                                                
059800 IMS-STATUSKONTROLL SECTION.                                              
059900                                                                          
060000     SET STATUS-IX TO 1                                                   
060100     SEARCH GODK-STATUS                                                   
060200       AT END                                                             
060300       STRING 'STATUSKOD FRÅN IMS' STATUS-WS                              
060400       DELIMITED BY SIZE INTO FELTEXT                                     
060500       CALL FELLOG                                                        
060600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
060700     END-SEARCH                                                           
060800     .                                                                    
