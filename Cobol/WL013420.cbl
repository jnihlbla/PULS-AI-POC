000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013420.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   MAJ  2006.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        REDIGERING OCH UTSKRIFT AV PLOCKETIKETTER                        
001000*        UTSKRIFT MED HJÄLP AV D&P                                        
001100*                                                                         
001200*        DETTA SUBPROGRAM REDIGERAR OCH SKRIVER                           
001300*        PLOCKETIKETTER                                                   
001400*        LÄSER AKTUELL PLOCKSATS (WL400311).                              
001500*        BEHANDLAR ALLA PLE-RADER SOM INGÅR I PLOCKSATSEN.                
001600*        PROGRAMMET STARTAS OM EFTER ETT ANTAL BEHANDLADE SIDOR.          
001700*        NÄR ALLA PLE-RADER HAR BEHANDLATS TAGES PLOCKSATS BORT.          
001800*        VARJE RAD I UTSKRIFTEN BESTÅR AV TVÅ ETIKETTER.                  
001900*                                                                         
002000*    LÄNKAREA :       WL013420                                            
002100*                                                                         
002200*    CHANGE LOG                                                           
002300*                                                                         
002400*    2523176 - PRINT PICKING ROUND-WMS                                    
002500*                                                                         
002600 DATA DIVISION.                                                           
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'WL013A20'.            
003100 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
003200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  WS-ABSTRACT-ADDRESS         PIC X(50)                                
003500         VALUE 'CARPARTS.LDC.PRPICKINGLABELBG'.                           
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700                                                                          
003800 77  WS-CURRENT-SECTION          PIC X(16)   VALUE 'MAIN'.                
003900 77  WS-CURRENT-IMS-SECTION      PIC X(16)   VALUE SPACE.                 
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 77  WS-4006-IDPRC-FIRST         PIC X(04)   VALUE SPACE.                 
004600                                                                          
004700 77  TECKEN-IX                   PIC S9(9)   VALUE +0.                    
004800 77  ODEL-IX                     PIC S9(9)   VALUE +0.                    
004900 77  3420-IX                     PIC S9(9)   VALUE +0.                    
005000 77  PL-IX                       PIC S9(9)   VALUE +0.                    
005100 77  3420-PART-COUNT-IX          PIC S9(9)   VALUE +0.                    
005200 77  MAX-ANTAL-ORDERDELAR        PIC S9(3)   VALUE 99    COMP-3.          
005300                                                                          
005400 77  WS-IDLOPNR-ORD              PIC  9(3).                               
005500 77  WS-IDKOLLI                  PIC  9(5).                               
005600                                                                          
005700 01  WS-ADPLATS-ORD              PIC X(5).                                
005800 01  FILLER  REDEFINES WS-ADPLATS-ORD.                                    
005900     03 WS-ADPLATSNR             PIC X(3).                                
006000     03 WS-ADPLNIV               PIC X(2).                                
006100                                                                          
006200 77  PLOCK-SW                    PIC X.                                   
006300     88  PLOCKSATS-EJ-KLAR                   VALUE 'N'.                   
006400     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
006500                                                                          
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007200     03  W488ORCR                PIC X(8)    VALUE 'W488ORCR'.            
007300                                                                          
007400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007500                                                                          
007600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
007700     SKIP3                                                                
007800*01  -COPY WZ01SEND                                                       
007900     EJECT                                                                
008000 01  UT-AREA-START               PIC X(24)   VALUE                        
008100                                 'UT-AREA-START '.                        
008200 01  HDR-AREA.                                                            
008300*    03  -COPY WZ01REQU -PRE HDR-                                         
008400*    03  -COPY WZ04HDR                                                    
008500     EJECT                                                                
008600 01  DOC-LINE-AREA.                                                       
008700*    03  -COPY WL01321                                                    
008800     EJECT                                                                
008900 01  DOC-TOT-AREA.                                                        
009000*    03  -COPY WL01322                                                    
009100 01  FILLER                      PIC X(16)   VALUE 'W488ORCR'.            
009200*01 -COPY W488ORCR                                                        
009300     EJECT                                                                
009400                                                                          
009500 01  NYCKLAR-TILL-DLI.                                                    
009600                                                                          
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(9)  VALUE ZERO  COMP-3.           
009900                                                                          
010000     03  W-4001-IDHTYP-X.                                                 
010100         05  W-4001-IDHTYP       PIC  X(4)  VALUE '4003'.                 
010200         05  W-4001-IDPRODNR     PIC  9(7).                               
010300         05  W-4001-IDPLKLST     PIC  9(3).                               
010400         05  W-4001-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
010500                                                                          
010600     03  W-4006-IDHTYP-X.                                                 
010700         05  W-4006-KDPRT        PIC  X(3).                               
010800         05  W-4006-KDSS-PLE     PIC  X(1).                               
010900         05  W-4006-ADLAGOMR     PIC S9(3) COMP-3.                        
011000         05  W-4006-ADGANG       PIC S9(3) COMP-3.                        
011100         05  W-4006-ADPLATS      PIC S9(5) COMP-3.                        
011200         05  W-4006-IDARTNR      PIC S9(9) COMP-3.                        
011300         05  W-4006-IDLOPNR      PIC S9(3) COMP-3.                        
011400                                                                          
011500     03  W-4006-MAX-KDPRT        PIC X(3)   VALUE '999'.                  
011600                                                                          
011700     03  W-WDQ301KY-X.                                                    
011800         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
011900         05  W-Q301KY-IDDC           PIC X(2).                            
012000         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
012100         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
012200                                                                          
012300     03  W-WDQ3DSEQ-X.                                                    
012400         05  W-Q3DSEQ-IDPRODNR       PIC S9(7)  COMP-3.                   
012500         05  W-Q3DSEQ-IDPLKLST       PIC S9(3)  COMP-3.                   
012600                                                                          
012700     03  W-IDDC-B6-X.                                                     
012800         05 W-IDDC-B6            PIC X(2).                                
012900                                                                          
013000     03  W-IDPRC-B6-X.                                                    
013100         05  W-IDPRC-B6          PIC X(4)    VALUE SPACE.                 
013200                                                                          
013300     03  W-WDE601-IDPRODNR-X.                                             
013400         05  W-IDPRODNR-WDE6     PIC S9(7)   VALUE ZERO COMP-3.           
013500                                                                          
013600     03  W-WDE611-IDKOLLI-X.                                              
013700         05  W-IDKOLLI-WDE6      PIC S9(5)   VALUE ZERO COMP-3.           
013800                                                                          
013900     03  W-IDORDER-X.                                                     
014000         05  W-IDORDER               PIC S9(7)  COMP-3.                   
014100                                                                          
014200     03  W-4535-IDHTYP-X.                                                 
014300         05  W-4535-IDHTYP       PIC  X(4)  VALUE '4535'.                 
014400         05  W-4535-KDFDKRAV     PIC S9(3)  COMP-3.                       
014500         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
014600                                                                          
014700     03  W-4536-IDSKYLT-X.                                                
014800         05  W-4536-IDSKYLT      PIC  X(3).                               
014900         05  W-4536-LOW-VALUE    PIC  X(2)  VALUE LOW-VALUE.              
015000                                                                          
015100 01  FILLER                  PIC X(16) VALUE 'IMS-WS STATUS-WS'.          
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FINNS           VALUE '  '.                              
015400     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
015500                                                                          
015600 01  GODK-STATUSKODER.                                                    
015700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015800                                                                          
015900 01  FILLER                  PIC X(08) VALUE 'SSA-AREA'.                  
016000 01  SSA1                        PIC X(192).                              
016100 01  SSA2                        PIC X(96).                               
016200                                                                          
016300*    --- IMS FUNKTIONSKODER                                               
016400*01  -COPY W0003                                                          
016500                                                                          
016600 01  FILLER               PIC X(16)   VALUE 'WDGX4003'.                   
016700 01  DLI-IO-WDGX4003.                                                     
016800*    03  -COPY WDGX4003                                                   
016900                                                                          
017000 01  FILLER               PIC X(16)   VALUE 'WDGX4004'.                   
017100 01  DLI-IO-WDGX4004.                                                     
017200*    03  -COPY WDGX4004                                                   
017300                                                                          
017400 01  FILLER               PIC X(16)   VALUE 'WDGX4006'.                   
017500 01  DLI-IO-WDGX4006.                                                     
017600*    03  -COPY WDGX4006                                                   
017700                                                                          
017800                                                                          
017900 01  FILLER               PIC X(16)   VALUE 'WDQ301  '.                   
018000 01  DLI-IO-WDQ301.                                                       
018100*    03  -COPY WDQ301                                                     
018200                                                                          
018300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDF502'.              
018400 01  DLI-IO-WDF502.                                                       
018500*  03  -COPY WDF502                                                       
018600                                                                          
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB612'.                      
018800 01  DLI-IO-WDB612.                                                       
018900*    03  -COPY WDB612                                                     
019000                                                                          
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
019200 01  DLI-IO-WDE601.                                                       
019300*    03  -COPY WDE601                                                     
019400                                                                          
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
019600 01  DLI-IO-WDE611.                                                       
019700*    03  -COPY WDE611                                                     
019800                                                                          
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
020000 01  DLI-IO-WDQ201.                                                       
020100*    03  -COPY WDQ201                                                     
020200                                                                          
020300 01  FILLER         PIC X(16) VALUE 'WDGX4536'.                           
020400 01  DLI-IO-WDGX4536.                                                     
020500*    03  -COPY WDGX4536                                                   
020600                                                                          
020700     EJECT                                                                
020800                                                                          
020900 LINKAGE SECTION.                                                         
021000*                                                                         
021100                                                                          
021200*01  -COPY WL013420                                                       
021300                                                                          
021400*01  -COPY WL0134O2                                                       
021500                                                                          
021600 01  DISTRDOC-PCB                PIC X.                                   
021700*01  -COPY W0009      -PRE SYNQ-                                          
021800                                                                          
021900*01  -COPY W0008  -PRE 4003-                                              
022000     05  FILLER                  PIC X.                                   
022100                                                                          
022200*01  -COPY W0008  -PRE WDQ3D-                                             
022300     05  FILLER                  PIC X.                                   
022400                                                                          
022500*01  -COPY W0008  -PRE WDF5-                                              
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008  -PRE WDB6-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE WDE6-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008  -PRE WDQ2-                                              
023500     05  FILLER                  PIC X.                                   
023600                                                                          
023700*01  -COPY W0008  -PRE 4535-                                              
023800     05  FILLER                  PIC X.                                   
023900 01  SYNQ-ATAB-PCB             PIC X.                                     
024000 01  WDQ3-PCB                  PIC X.                                     
024100                                                                          
024200                                                                          
024300  PROCEDURE DIVISION USING 3420-WL013420 RESP-PL-WL0134O2                 
024400                           DISTRDOC-PCB SYNQ-PCB                          
024500                           4003-PCB WDQ3D-PCB                             
024600                           WDF5-PCB WDB6-PCB WDE6-PCB                     
024700                           WDQ2-PCB 4535-PCB SYNQ-ATAB-PCB                
024800                           WDQ3-PCB.                                      
024900                                                                          
025000  MAIN SECTION.                                                           
025100     PERFORM A-INIT                                                       
025200     PERFORM B-LAES-PLOCKSATS                                             
025300                                                                          
025400     IF SEGMENT-FINNS                                                     
025500        PERFORM S90-OPEN-DAP-SEND                                         
025600        PERFORM S91-PUT-DAP-HEADER                                        
025700                                                                          
025800        PERFORM C-BEHANDLA-PLERADER                                       
025900                                                                          
026000*       PERFORM IMS-04-GHU-WDGX4003                                       
026100*       PERFORM IMS-05-DLET-WDGX4003                                      
026200                                                                          
026300        PERFORM S94-CLOSE-DAP-SEND                                        
026400     END-IF                                                               
026500                                                                          
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK.                                                              
026800                                                                          
026900                                                                          
027000 A-INIT SECTION.                                                          
027100                                                                          
027200     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
027300                                                                          
027400     MOVE NEJ TO PLOCK-SW                                                 
027500                                                                          
027600*    -- INITIATE D&P HEADER RECORD (WZ01REQU+WZ04HDR)                     
027700     MOVE 1                   TO HDR-REQU-IDMSGVER                        
027800     MOVE SPACE               TO HDR-REQU-KDPGMACT                        
027900     MOVE 3420-IDUSER         TO HDR-REQU-IDUSER                          
028000                                                                          
028100       MOVE 'PICKING-LABEL2'  TO HDR-IDOUTTYPE                            
028200     MOVE SPACE               TO HDR-IDOUTREC                             
028300     MOVE 3420-IDDC           TO HDR-IDOUTREC(1:2)                        
028400     MOVE 3420-IDUSER         TO HDR-IDOUTREC(3:8)                        
028500     MOVE 3420-IDLIST         TO HDR-IDLIST                               
028600                                                                          
028700                                                                          
028800*L138                                                                     
028900     IF 3420-IDTRANS = 'L138' OR 'A138'                                   
029000                                                                          
029100       MOVE +1                  TO 3420-IX                                
029200       PERFORM UNTIL 3420-IX > MAX-ANTAL-ORDERDELAR                       
029300         IF 3420-IDORDER(3420-IX) > ZERO                                  
029400           ADD +1               TO 3420-PART-COUNT-IX                     
029500         END-IF                                                           
029600         ADD +1                 TO 3420-IX                                
029700       END-PERFORM                                                        
029800     END-IF                                                               
029900     .                                                                    
030000                                                                          
030100 B-LAES-PLOCKSATS SECTION.                                                
030200     MOVE ' B-LAES-PLOCKSATS' TO WS-CURRENT-SECTION                       
030300                                                                          
030400     MOVE 3420-IDPRODNR-KEY   TO W-4001-IDPRODNR                          
030500     MOVE 3420-IDPLKLST-KEY   TO W-4001-IDPLKLST                          
030600     PERFORM IMS-01-GHU-WDGX4004                                          
030700                                                                          
030800     IF SEGMENT-FINNS                                                     
030900        IF 4004-NYCKEL-GRP NOT = LOW-VALUE                                
031000           MOVE 4004-NYCKEL-GRP    TO W-4006-IDHTYP-X                     
031100           PERFORM IMS-02-GNP-WDGX4006-KVAL                               
031200        END-IF                                                            
031300     END-IF                                                               
031400     .                                                                    
031500                                                                          
031600 C-BEHANDLA-PLERADER SECTION.                                             
031700     MOVE 'C-BEHANDLA-PLERADER' TO WS-CURRENT-SECTION                     
031800     MOVE +1                    TO ODEL-IX                                
031900     MOVE +1                    TO 3420-IX                                
032000     MOVE +1                    TO PL-IX                                  
032100                                                                          
032200     PERFORM UNTIL PLOCKSATS-KLAR                                         
032300        PERFORM CA-LAES-PLOCKRAD                                          
032400        IF PLOCKSATS-EJ-KLAR                                              
032500           IF  3420-IDPRODNR-KEY = ZERO                                   
032600           AND 3420-PART-COUNT-IX > +1                                    
032700             CONTINUE                                                     
032800           ELSE                                                           
032900             PERFORM CB-REDIGERA-PRINTRADER                               
033000             PERFORM S92-PUT-DAP-DOC-LINE                                 
033100           END-IF                                                         
033200        ELSE                                                              
033300           PERFORM CC-REDIGERA-TOTAL                                      
033400*L138                                                                     
033500           IF 3420-IDTRANS = 'L138' OR 'A138'                             
033600               MOVE 3420-IDTRPTNR(3420-IX) TO TOTAL-IDTRPTNR              
033700                                              RESP-PL-IDTRPTNR            
033800               MOVE 3420-TIRFSDAT(3420-IX) TO TOTAL-TIRFSDAT              
033900                                              RESP-PL-TIRFSDAT            
034000*          INSPECT TOTAL-TIRFSDAT REPLACING LEADING SPACE BY ZERO         
034100               MOVE 3420-TIRFSTID(3420-IX) TO TOTAL-TIRFSTID              
034200                                              RESP-PL-TIRFSTID            
034300               ADD +1           TO 3420-IX                                
034400           END-IF                                                         
034500           PERFORM S93-PUT-DAP-DOC-TOT                                    
034600        END-IF                                                            
034700        ADD  +1                 TO ODEL-IX                                
034800        ADD  +1                 TO PL-IX                                  
034900     END-PERFORM                                                          
035000     .                                                                    
035100                                                                          
035200 CA-LAES-PLOCKRAD SECTION.                                                
035300     MOVE 'CA-LAES-PLOCKRAD   ' TO WS-CURRENT-SECTION                     
035400                                                                          
035500     PERFORM IMS-03-GNP-WDGX4006-OKVAL                                    
035600     IF SEGMENT-SAKNAS                                                    
035700        MOVE JA TO PLOCK-SW                                               
035800     ELSE                                                                 
035900       IF 3420-IDDC = 11                                                  
036000         IF (4006-ADLAGOMR-ORD = 90 OR 98)                                
036100          IF (4006-IDPRC NOT = '991Q' AND '9920')                         
036200            MOVE 'PCK' TO SYNQ-ORDERTYPE                                  
036300            MOVE 3420-IDDC TO SYNQ-IDDC                                   
036400            MOVE 4006-IDARTNR TO SYNQ-IDARTNR                             
036500            MOVE 4006-KVAVBART TO SYNQ-KVBEST                             
036600            MOVE 4006-TIRFSDAT TO SYNQ-TIRFSDAT                           
036700            MOVE 4006-IDPRODNR TO SYNQ-IDPRODNR                           
036800            MOVE 4006-IDRADNR TO SYNQ-IDRADNR                             
036900            MOVE 4006-KDORDKL TO SYNQ-KDORDKL                             
037000            MOVE 4006-IDBORD TO SYNQ-IDBORD                               
037100            MOVE 4006-IDKUNDRF TO SYNQ-IDKUNDRF                           
037200            MOVE 4006-IDPLKLST TO SYNQ-IDPLKLST                           
037300            MOVE 4006-IDPRC    TO SYNQ-IDPRC                              
037400            MOVE NEJ TO SYNQ-FLSATS                                       
037500            CALL W488ORCR USING  SYNQ-W488ORCR SYNQ-PCB                   
037600                                 SYNQ-ATAB-PCB WDQ3-PCB                   
037700          END-IF                                                          
037800         END-IF                                                           
037900       END-IF                                                             
038000     END-IF                                                               
038100     .                                                                    
038200                                                                          
038300 CB-REDIGERA-PRINTRADER SECTION.                                          
038400     MOVE 'CB-REDIGERA-PRINT'    TO WS-CURRENT-SECTION                    
038500                                                                          
038600     MOVE '1'                    TO LINE-IDAFPRCD                         
038700                                    RESP-PL-IDAFPRCD(PL-IX)               
038800     MOVE 3420-IDDC              TO LINE-IDDC                             
038900                                    RESP-PL-IDDC(PL-IX)                   
039000     MOVE 4006-ADLAGOMR-ORD      TO LINE-ADLAGOMR                         
039100                                    RESP-PL-ADLAGOMR(PL-IX)               
039200     MOVE 4006-ADGANG            TO LINE-ADGANG                           
039300                                    RESP-PL-ADGANG(PL-IX)                 
039400*BA                                                                       
039500     MOVE 4006-ADPLATS-ORD       TO WS-ADPLATS-ORD                        
039600                                    RESP-PL-ADPLATS(PL-IX)                
039700     MOVE WS-ADPLATSNR           TO LINE-ADPLATSNR                        
039800     MOVE WS-ADPLNIV(1:1)        TO LINE-ADPLNIV-LEFT                     
039900     MOVE WS-ADPLNIV(2:1)        TO LINE-ADPLNIV-RIGHT                    
040000                                                                          
040100     IF 4006-FLAKPLOC = JA                                                
040200         MOVE '*'                TO LINE-FLAKPLOC                         
040300     ELSE                                                                 
040400         MOVE SPACE              TO LINE-FLAKPLOC                         
040500     END-IF                                                               
040600     MOVE LINE-FLAKPLOC          TO RESP-PL-FLAKPLOC(PL-IX)               
040700                                                                          
040800     MOVE 4006-IDARTNR           TO LINE-IDARTNR                          
040900                                    RESP-PL-IDARTNR(PL-IX)                
041000     MOVE 4006-BEART             TO LINE-BEART                            
041100                                    RESP-PL-BEART(PL-IX)                  
041200     MOVE 4006-KVAVBART          TO LINE-KVAVBART                         
041300                                    RESP-PL-KVAVBART(PL-IX)               
041400     MOVE 4006-KDSORT            TO LINE-KDSORT                           
041500                                    RESP-PL-KDSORT(PL-IX)                 
041600     MOVE 4006-KDARTURS          TO LINE-KDARTURS                         
041700                                    RESP-PL-KDARTURS(PL-IX)               
041800     MOVE 4006-BERADREF          TO LINE-BERADREF                         
041900                                    RESP-PL-BERADREF(PL-IX)               
042000*LK  ADD LYNK&CO TO PICKING LABEL                                         
042100                                                                          
042200     IF 4006-IDSYSTEM (1:3) = 'LYN'                                       
042300        MOVE 4006-IDARTNR        TO W-IDARTNR                             
042400        PERFORM IMS-GU-WDF502                                             
042500         IF SEGMENT-FINNS                                                 
042600           MOVE XLEV-IDLEVART       TO LINE-IDLEVART                      
042700         END-IF                                                           
042800     ELSE                                                                 
042900        MOVE SPACE               TO LINE-IDLEVART                         
043000     END-IF                                                               
043100     MOVE LINE-IDLEVART          TO RESP-PL-IDLEVART(PL-IX)               
043200     MOVE 1 TO TECKEN-IX                                                  
043300     PERFORM UNTIL TECKEN-IX > 10                                         
043400        IF LINE-BERADREF(TECKEN-IX:1) < SPACE                             
043500           MOVE SPACE TO LINE-BERADREF(TECKEN-IX:1)                       
043600        END-IF                                                            
043700        ADD      1 TO TECKEN-IX                                           
043800     END-PERFORM                                                          
043900                                                                          
044000     MOVE 4006-KDARTHNT          TO LINE-KDARTHNT                         
044100                                    RESP-PL-KDARTHNT(PL-IX)               
044200     MOVE 4006-KDFARLIG          TO LINE-KDFARLIG                         
044300                                    RESP-PL-KDFARLIG(PL-IX)               
044400     MOVE 4006-KDEMBAL           TO LINE-KDEMBAL                          
044500                                    RESP-PL-KDEMBAL(PL-IX)                
044600     MOVE 4006-IDSPECEMB         TO LINE-IDSPECEMB                        
044700                                    RESP-PL-IDSPECEMB(PL-IX)              
044800     MOVE 4006-IDBORD            TO LINE-IDBORD                           
044900                                    RESP-PL-IDBORD(PL-IX)                 
045000     MOVE 4006-IDDISTR           TO LINE-IDDISTR                          
045100                                    RESP-PL-IDDISTR(PL-IX)                
045200     MOVE 4006-IDKUNDNR          TO LINE-IDKUNDNR                         
045300                                    RESP-PL-IDKUNDNR(PL-IX)               
045400     MOVE 4006-IDPRODNR          TO LINE-IDPRODNR                         
045500                                    RESP-PL-IDPRODNR(PL-IX)               
045600     MOVE 4006-IDPLKLST          TO LINE-IDPLKLST                         
045700                                    RESP-PL-IDPLKLST(PL-IX)               
045800     MOVE 4006-IDKUNDRF          TO LINE-IDKUNDRF                         
045900     MOVE 4006-IDKUNDRF(3:5)     TO RESP-PL-IDORDNR7(PL-IX)               
046000     INSPECT LINE-IDKUNDRF REPLACING LEADING ZERO BY SPACE                
046100     MOVE 4006-IDRADNR           TO LINE-IDRADNR                          
046200                                    RESP-PL-IDRADNR(PL-IX)                
046300     MOVE 4006-KDORDKL           TO LINE-KDORDKL                          
046400                                    RESP-PL-KDORDKL(PL-IX)                
046500     MOVE 4006-IDPRC             TO LINE-IDPRC                            
046600                                    RESP-PL-IDPRC(PL-IX)                  
046700     MOVE 4006-IDLOPNR-PL        TO LINE-IDLOPNR-PL                       
046800                                    RESP-PL-IDLOPNR-PL(PL-IX)             
046900     MOVE 4006-IDLOPNR-ORD       TO WS-IDLOPNR-ORD                        
047000     MOVE WS-IDLOPNR-ORD         TO LINE-IDLOPNR-ORD                      
047100                                    RESP-PL-IDLOPNR-ORD(PL-IX)            
047200*DEF CASE TO API *LK                                                      
047300     MOVE 3420-IDDC              TO W-IDDC-B6-X                           
047400     MOVE 4006-IDPRC             TO W-IDPRC-B6-X                          
047500     PERFORM IMS-33-GU-WDB612                                             
047600     IF SEGMENT-FINNS                                                     
047700       MOVE PRC-IDKOLLI-PRCSTA   TO RESP-PL-IDKOLLI(PL-IX)                
047800                                    WS-IDKOLLI                            
047900                                    LINE-IDKOLLI                          
048000       MOVE PRC-KDKOLLI          TO RESP-PL-KDKOLLI(PL-IX)                
048100                                    LINE-KDKOLLI                          
048200     ELSE                                                                 
048300       MOVE '9999'               TO W-IDPRC-B6-X                          
048400                                                                          
048500       PERFORM IMS-33-GU-WDB612                                           
048600                                                                          
048700       IF SEGMENT-FINNS                                                   
048800         MOVE PRC-IDKOLLI-PRCSTA TO RESP-PL-IDKOLLI(PL-IX)                
048900                                    WS-IDKOLLI                            
049000                                    LINE-IDKOLLI                          
049100         MOVE PRC-KDKOLLI        TO RESP-PL-KDKOLLI(PL-IX)                
049200                                    LINE-KDKOLLI                          
049300       ELSE                                                               
049400         MOVE +00010             TO RESP-PL-IDKOLLI(PL-IX)                
049500                                    WS-IDKOLLI                            
049600                                    LINE-IDKOLLI                          
049700         MOVE 'NC'               TO RESP-PL-KDKOLLI(PL-IX)                
049800                                    LINE-KDKOLLI                          
049900       END-IF                                                             
050000     END-IF                                                               
050100                                                                          
050200     MOVE 4006-IDPRODNR               TO W-IDPRODNR-WDE6                  
050300     MOVE WS-IDKOLLI                  TO W-IDKOLLI-WDE6                   
050400                                                                          
050500     PERFORM IMS-34-GU-WDE601                                             
050600     IF SEGMENT-FINNS                                                     
050700       PERFORM IMS-35-GNP-WDE611                                          
050800       IF SEGMENT-FINNS                                                   
050900*LK      IF KOLLI-KDKOLSTA > +0                                           
051000           PERFORM UNTIL SEGMENT-SAKNAS                                   
051100             ADD +1                   TO W-IDKOLLI-WDE6                   
051200             ADD +1                   TO WS-IDKOLLI                       
051300             PERFORM IMS-35-GNP-WDE611                                    
051400           END-PERFORM                                                    
051500                                                                          
051600           MOVE WS-IDKOLLI          TO RESP-PL-IDKOLLI(PL-IX)             
051700                                       LINE-IDKOLLI                       
051800*LK      END-IF                                                           
051900       END-IF                                                             
052000     END-IF                                                               
052100                                                                          
052200*WAREHOUSE INSTRUCTION *LK                                                
052300     MOVE 4006-IDPRODNR  TO W-Q3DSEQ-IDPRODNR                             
052400     MOVE 4006-IDPLKLST  TO W-Q3DSEQ-IDPLKLST                             
052500     PERFORM IMS-05-GU-WDQ3DSEQ-WLORQA01                                  
052600                                                                          
052700     IF SEGMENT-FINNS                                                     
052800        IF ODEL-IDDC NOT = 3420-IDDC                                      
052900           PERFORM IMS-06-GN-WDQ3DSEQ-WLORQA01                            
053000*TILLÄGG PGA AV ATT DET FINNS "LÖSA" WDQ301 DVS ORENSADE WDQ301           
053100*SOM HAR SAMMA PRODNR SOM EN NY ORDER.                                    
053200*RESNINGSFEL? ELLER PGA FELUPPDATERING AV WDQ301 TIDIGARE?                
053300        END-IF                                                            
053400     END-IF                                                               
053500     IF SEGMENT-FINNS                                                     
053600       MOVE ODEL-IDORDER TO W-IDORDER                                     
053700       PERFORM IMS-36-GU-WDQ201                                           
053800                                                                          
053900       IF OHUV-BELAGINS-GRP = SPACE                                       
054000         MOVE SPACE             TO RESP-PL-BELAGINS-DEL1(PL-IX)           
054100         MOVE SPACE             TO RESP-PL-BELAGINS-DEL2(PL-IX)           
054200       ELSE                                                               
054300         IF OHUV-BELAGINS-DEL1 = SPACE                                    
054400           MOVE OHUV-BELAGINS-DEL2 TO RESP-PL-BELAGINS-DEL1(PL-IX)        
054500         ELSE                                                             
054600           MOVE OHUV-BELAGINS-DEL1 TO RESP-PL-BELAGINS-DEL1(PL-IX)        
054700            IF OHUV-BELAGINS-DEL2 NOT = SPACE                             
054800             MOVE OHUV-BELAGINS-DEL2 TO                                   
054900                                  RESP-PL-BELAGINS-DEL2(PL-IX)            
055000            ELSE                                                          
055100             MOVE SPACE        TO RESP-PL-BELAGINS-DEL2(PL-IX)            
055200            END-IF                                                        
055300         END-IF                                                           
055400       END-IF                                                             
055500       MOVE RESP-PL-BELAGINS-GRP (PL-IX)                                  
055600                                 TO LINE-BELAGINS-GRP                     
055700                                                                          
055800*TRANSPORT   PACKING INSTRUCTION *LK                                      
055900       MOVE ODEL-KDFDKRAV         TO W-4535-KDFDKRAV                      
056000       MOVE OHUV-IDSKYLT          TO W-4536-IDSKYLT                       
056100       PERFORM IMS-37-GU-WDGX4536                                         
056200                                                                          
056300       IF SEGMENT-FINNS                                                   
056400         MOVE 4536-BEFDKRAV       TO RESP-PL-BEFDKRAV(PL-IX)              
056500       ELSE                                                               
056600         MOVE SPACES              TO RESP-PL-BEFDKRAV(PL-IX)              
056700       END-IF                                                             
056800       MOVE RESP-PL-BEFDKRAV(PL-IX)                                       
056900                                 TO LINE-BEFDKRAV                         
057000     END-IF                                                               
057100     .                                                                    
057200                                                                          
057300 CC-REDIGERA-TOTAL SECTION.                                               
057400     MOVE 'STA CC-REDIGERA-TOTAL' TO WS-CURRENT-SECTION                   
057500                                                                          
057600     MOVE '2'                   TO TOTAL-IDAFPRCD                         
057700                                   RESP-PL-IDAFPRCD-TOT                   
057800*    MOVE WS-4006-IDPRC-FIRST   TO TOTAL-IDPRC                            
057900     MOVE 3420-IDPRC            TO TOTAL-IDPRC                            
058000                                   RESP-PL-IDPRC-TOT                      
058100     MOVE 4006-IDLOPNR-PL       TO TOTAL-IDLOPNR-PL                       
058200                                   RESP-PL-IDLOPNR-PL-TOT                 
058300     MOVE 4004-KVRADER (100)    TO TOTAL-KVRADER                          
058400                                   RESP-PL-KVRADER-MAX1                   
058500     MOVE 'END CC-REDIGERA-TOTAL' TO WS-CURRENT-SECTION                   
058600     .                                                                    
058700                                                                          
058800 S90-OPEN-DAP-SEND SECTION.                                               
058900     MOVE 'S90-OPEN-DAP-SEND' TO WS-CURRENT-SECTION                       
059000                                                                          
059100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
059200     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
059300     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
059400                                                                          
059500     IF SEND-KDRC > 0                                                     
059600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
059700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
059800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
059900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060000     END-IF                                                               
060100     .                                                                    
060200                                                                          
060300 S91-PUT-DAP-HEADER SECTION.                                              
060400     MOVE 'S91-PUT-DAP-HEADER' TO WS-CURRENT-SECTION                      
060500                                                                          
060600     MOVE 'PUT'                      TO SEND-KDFUNC                       
060700     MOVE LENGTH OF HDR-AREA         TO SEND-KVDLEN                       
060800                                                                          
060900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
061000                         SEND-KVDLEN                                      
061100                         HDR-AREA                                         
061200     IF SEND-KDRC > ZERO                                                  
061300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
061400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
061500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
061600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
061700     END-IF                                                               
061800     .                                                                    
061900                                                                          
062000 S92-PUT-DAP-DOC-LINE SECTION.                                            
062100     MOVE 'S92-PUT-DAP-DOC-LINE'   TO WS-CURRENT-SECTION                  
062200                                                                          
062300     MOVE 'PUT'                    TO SEND-KDFUNC                         
062400     MOVE LENGTH OF DOC-LINE-AREA  TO SEND-KVDLEN                         
062500                                                                          
062600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
062700                         SEND-KVDLEN                                      
062800                         DOC-LINE-AREA                                    
062900     IF SEND-KDRC > ZERO                                                  
063000       MOVE SEND-KDRC              TO KDRC-DISPLAY                        
063100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
063200       DELIMITED BY SIZE         INTO ERROR-TEXT                          
063300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
063400     END-IF                                                               
063500     .                                                                    
063600 S93-PUT-DAP-DOC-TOT SECTION.                                             
063700                                                                          
063800     MOVE 'PUT'                           TO SEND-KDFUNC                  
063900     MOVE LENGTH OF DOC-TOT-AREA          TO SEND-KVDLEN                  
064000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
064100                         SEND-KVDLEN                                      
064200                         DOC-TOT-AREA                                     
064300     IF SEND-KDRC > ZERO                                                  
064400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
064500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
064600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
064700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
064800     END-IF                                                               
064900     .                                                                    
065000                                                                          
065100 S94-CLOSE-DAP-SEND SECTION.                                              
065200     MOVE 'S94-CLOSE-DAP-SEND' TO WS-CURRENT-SECTION                      
065300                                                                          
065400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
065500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
065600                                                                          
065700     IF SEND-KDRC > 0                                                     
065800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
065900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
066000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
066100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
066200     END-IF                                                               
066300     .                                                                    
066400                                                                          
066500                                                                          
066600 IMS-01-GHU-WDGX4004  SECTION.                                            
066700     MOVE 'IMS-01'  TO WS-CURRENT-IMS-SECTION                             
066800                                                                          
066900     STRING 'WL400301(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
067000          DELIMITED BY SIZE INTO SSA1                                     
067100     MOVE 'WL400311 '         TO SSA2                                     
067200     MOVE '  GE'              TO GODK-STATUSKODER                         
067300     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-WDGX4004 SSA1 SSA2            
067400     MOVE 4003-STATUS-CODE    TO STATUS-WS                                
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     .                                                                    
067700                                                                          
067800 IMS-02-GNP-WDGX4006-KVAL SECTION.                                        
067900     MOVE 'IMS-02'  TO WS-CURRENT-IMS-SECTION                             
068000                                                                          
068100     STRING 'WL400321(WDGXKEY  =' W-4006-IDHTYP-X ')'                     
068200          DELIMITED BY SIZE INTO SSA1                                     
068300     MOVE '    '              TO GODK-STATUSKODER                         
068400     CALL CBLTDLI USING GNP 4003-PCB DLI-IO-WDGX4006 SSA1                 
068500     MOVE 4003-STATUS-CODE    TO STATUS-WS                                
068600     PERFORM IMS-STATUSKONTROLL                                           
068700     .                                                                    
068800                                                                          
068900 IMS-03-GNP-WDGX4006-OKVAL SECTION.                                       
069000     MOVE 'IMS-03'  TO WS-CURRENT-IMS-SECTION                             
069100                                                                          
069200     STRING 'WL400321(KDPRT    <' W-4006-MAX-KDPRT ')'                    
069300          DELIMITED BY SIZE INTO SSA1                                     
069400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
069500     CALL CBLTDLI USING GNP 4003-PCB DLI-IO-WDGX4006 SSA1                 
069600     MOVE 4003-STATUS-CODE    TO STATUS-WS                                
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900                                                                          
070000 IMS-GU-WDF502 SECTION.                                                   
070100     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
070200            DELIMITED BY SIZE INTO SSA1                                   
070300     MOVE 'WDF502  '       TO SSA2                                        
070400     MOVE '  GE'           TO GODK-STATUSKODER                            
070500     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
070600     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900                                                                          
071000 IMS-33-GU-WDB612 SECTION.                                                
071100     MOVE 'IMS-GU-WDB612'    TO WS-CURRENT-IMS-SECTION                    
071200                                                                          
071300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
071400          DELIMITED BY SIZE INTO SSA1                                     
071500     STRING 'WDB612  (IDPRC    =' W-IDPRC-B6-X ')'                        
071600          DELIMITED BY SIZE INTO SSA2                                     
071700     MOVE '  GE'             TO GODK-STATUSKODER                          
071800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB612 SSA1 SSA2               
071900     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     EJECT                                                                
072300 IMS-34-GU-WDE601 SECTION.                                                
072400     MOVE 'IMS-34'   TO WS-CURRENT-IMS-SECTION                            
072500                                                                          
072600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
072700          DELIMITED BY SIZE INTO SSA1                                     
072800     MOVE '  GE'              TO GODK-STATUSKODER                         
072900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
073000     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300                                                                          
073400 IMS-35-GNP-WDE611     SECTION.                                           
073500     MOVE 'IMS-35'   TO WS-CURRENT-IMS-SECTION                            
073600                                                                          
073700     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
073800          DELIMITED BY SIZE INTO SSA1                                     
073900     MOVE '  GE'              TO GODK-STATUSKODER                         
074000     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
074100     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
074200     PERFORM IMS-STATUSKONTROLL                                           
074300     .                                                                    
074400                                                                          
074500 IMS-36-GU-WDQ201 SECTION.                                                
074600     MOVE 'IMS-36'    TO WS-CURRENT-IMS-SECTION                           
074700                                                                          
074800     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
074900          DELIMITED BY SIZE INTO SSA1                                     
075000     MOVE '  GE'           TO GODK-STATUSKODER                            
075100     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
075200     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
075300     PERFORM IMS-STATUSKONTROLL                                           
075400     .                                                                    
075500 IMS-37-GU-WDGX4536  SECTION.                                             
075600     MOVE 'IMS-37' TO WS-CURRENT-IMS-SECTION                              
075700                                                                          
075800     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
075900          DELIMITED BY SIZE INTO SSA1                                     
076000     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
076100          DELIMITED BY SIZE INTO SSA2                                     
076200     MOVE '  GE'              TO GODK-STATUSKODER                         
076300     CALL CBLTDLI USING GU 4535-PCB DLI-IO-WDGX4536 SSA1 SSA2             
076400     MOVE 4535-STATUS-CODE    TO STATUS-WS                                
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700                                                                          
076800 IMS-05-GU-WDQ3DSEQ-WLORQA01 SECTION.                                     
076900     MOVE 'IMS-05' TO WS-CURRENT-IMS-SECTION                              
077000                                                                          
077100     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
077200          DELIMITED BY SIZE INTO SSA1                                     
077300     MOVE '  GE'              TO GODK-STATUSKODER                         
077400     CALL CBLTDLI USING GU WDQ3D-PCB DLI-IO-WDQ301 SSA1                   
077500     MOVE WDQ3D-STATUS-CODE    TO STATUS-WS                               
077600     PERFORM IMS-STATUSKONTROLL                                           
077700     .                                                                    
077800                                                                          
077900 IMS-06-GN-WDQ3DSEQ-WLORQA01 SECTION.                                     
078000     MOVE 'IMS-06' TO WS-CURRENT-IMS-SECTION                              
078100                                                                          
078200     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
078300          DELIMITED BY SIZE INTO SSA1                                     
078400     MOVE '  GE'              TO GODK-STATUSKODER                         
078500     CALL CBLTDLI USING GN WDQ3D-PCB DLI-IO-WDQ301 SSA1                   
078600     MOVE WDQ3D-STATUS-CODE    TO STATUS-WS                               
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900                                                                          
079000 IMS-STATUSKONTROLL SECTION.                                              
079100                                                                          
079200     SET STATUS-IX TO 1                                                   
079300     SEARCH GODK-STATUS                                                   
079400       AT END CALL FELLOG                                                 
079500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
079600     END-SEARCH                                                           
079700     .                                                                    
