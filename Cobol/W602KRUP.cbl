000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W602KRUP.                                                
000500*AUTHOR.         KENT JEBSEN                                              
000600*DATE-WRITTEN.   SEPT -00.                                                
000700                                                                          
000800*                                                                         
000900*    REMARKS.                                                             
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        SUBPROGRAM SOM AVSLUTAR KR-RAPPORTERING.                         
001300*        SKRIVER EKONOMISEGMENT.                                          
001400*        ERSÄTTER MPP:ERNA W60205 OCH W60206.                             
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR  W6H7                                      
001700*        PROGRAMMET UPPDATERAR  W6G1                                      
001800*        PROGRAMMET UPPDATERAR  WDR3                                      
001900*        PROGRAMMET LÄSER       WDF1                                      
002000*        PROGRAMMET LÄSER       WDK6                                      
002100*        PROGRAMMET LÄSER       WDR2                                      
002200*                                                                         
002300*    CHANGE LOG:                                                          
002400*      13/11/13 - REDDY RAHUL     - IR CORRECTIONS                        
002500*                                   SCR 3235165                           
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100*    -COPY WY2000W1                                                       
003200                                                                          
003300 77  FELTEXT                     PIC X(50)  VALUE SPACE.                  
003400 77  IDPGM                       PIC X(08)  VALUE 'W602KRUP'.             
003500 77  JA                          PIC X      VALUE 'J'.                    
003600 77  NEJ                         PIC X      VALUE 'N'.                    
003700*      --- VALID IDDC CODES                                               
003800*                                                                         
003900*01    -COPY WWDC99                                                       
004000       EJECT                                                              
004100*01    -COPY WWLNDKON                                                     
004200       EJECT                                                              
004300 77  IX                          PIC 9(2)   VALUE ZERO.                   
004400 77  FLPRIS                      PIC X      VALUE 'N'.                    
004500 77  FLVALUTA                    PIC X      VALUE 'N'.                    
004600 77  FIRST-INLEV                 PIC X      VALUE 'N'.                    
004700 77  FIRST-LEV                   PIC X      VALUE 'N'.                    
004800 77  WS-FLKRLIM                  PIC X      VALUE 'J'.                    
004900 77  WS-CLAG-PRARTSTD            PIC 9(7)V9(2).                           
005000 77  WS-CLAG-PRDIRLON            PIC 9(4)V9(3).                           
005100 77  WS-CLAG-PRDMTRL             PIC 9(6)V9(3).                           
005200 77  WS-CLAG-PROVRPAL            PIC 9(4)V9(3).                           
005300 77  WS-CLAG-PRINK               PIC 9(7)V9(2).                           
005400 77  WS-PRARTBES-PR              PIC S9(9)V999  COMP-3 VALUE ZERO.        
005500 77  WS-SUPRARTBES-PR            PIC S9(9)V999  COMP-3 VALUE ZERO.        
005600 77  WS-SUPRARTSTD               PIC S9(9)V999  COMP-3 VALUE ZERO.        
005700 77  WS-SUPRBESDIFF              PIC S9(7)V99   COMP-3 VALUE ZERO.        
005800 77  WS-SUKALKYLP                PIC S9(9)V99   COMP-3 VALUE ZERO.        
005900 77  WS-SUHEMTKOST               PIC S9(9)V9(7) COMP-3 VALUE ZERO.        
006000 77  WS-SUHEMTKOST-2-DEC         PIC S9(9)V99   COMP-3 VALUE ZERO.        
006100 77  WS-ARBKST-TIM-SEK           PIC S9(4)      VALUE +0775.              
006200 77  WS-ARBKST-TIM-CNY           PIC S9(4)      VALUE +0535.              
006210 77  WS-ARBKST-TIM-USD           PIC S9(4)      VALUE +0075.              
006300 77  WS-ARBKST-TOT-SEK           PIC S9(7)      VALUE ZERO.               
006310 77  WS-WORKTIME                 PIC S9(2)V9(1) VALUE ZERO.               
006400 77  WS-SUMAT                    PIC  9(7)V9(2) VALUE ZERO.               
006500 77  WS-SUOMK-EK                 PIC  9(7)V9(2) VALUE ZERO.               
006600 77  WS-KDVALISO                 PIC X(3)       VALUE SPACE.              
006700 77  WS-PRARTBEL-PR              PIC S9(8)V9(5) VALUE +0.                 
006800 77  WS-PRKURS                   PIC S9(6)V9(5) COMP-3 VALUE +0.          
006900 77  KDVALLEV-NUM                PIC 9(3)       VALUE ZERO.               
007000 77  WS-PRMOMS                   PIC  9(7)V9(2) VALUE ZERO.               
007100 77  R40-HLEV                    PIC X          VALUE 'J'.                
007200 77  6002-IX                     PIC S9(9)   VALUE +0 COMP SYNC.          
007300 77  WS-KVKRRET                  PIC S9(7)      VALUE ZERO.               
007310 77  W-DATE-AAMM                 PIC 9(4)       VALUE ZERO.               
007320 77  WS-KDVALISO-HUV             PIC X(3)       VALUE 'SEK'.              
007400 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +1000 COMP.            
007500                                                                          
007600 01 DAGENS-DATUM                 PIC 9(6).                                
007700 01 FILLER REDEFINES DAGENS-DATUM.                                        
007800     03 DAGENS-TIAA              PIC 9(2).                                
007900     03 DAGENS-TIMM              PIC 9(2).                                
008000     03 DAGENS-TIDD              PIC 9(2).                                
008100                                                                          
008200 01 INLEV-DAT.                                                            
008300     03 INLEV-DATUM              PIC 9(8).                                
008400     03 FILLER REDEFINES INLEV-DATUM.                                     
008500       05 INLEV-SEKEL            PIC 9(2).                                
008600       05 INLEV-TIAAMMDD         PIC 9(6).                                
008700                                                                          
008800 01  TEST-IDLANDX2               PIC X(2).                                
008900*01  FILLER -COPY WWLEV02 -RED TEST-IDLANDX2.                             
009000                                                                          
009100*                                                                         
009200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009300 01  GENERELLA-SUBPROGRAM.                                                
009400     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
009500     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
009600     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
009700     03  W009MOMS                PIC X(8)   VALUE 'W009MOMS'.             
009710     03  W510CURR                PIC X(8)   VALUE 'W510CURR'.             
009800     EJECT                                                                
009900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010000*                                                                         
010100 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
010200                                                                          
010300 01  NYCKLAR-TILL-DLI.                                                    
010400*----> DIREKTNYCKEL TILL W6H701                                           
010500                                                                          
010600     03  W-IDKR-X.                                                        
010700         05  W-IDKR              PIC 9(05)  VALUE ZERO.                   
010800                                                                          
010900*----> NYCKLAR TILL WDK601/701                                            
011000                                                                          
011100     03  W-IDARTNR-X.                                                     
011200         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
011300                                                                          
011400                                                                          
011500*----> LEVREGISTER W6F1                                                   
011600                                                                          
011700     03  W-IDLEVNR-X.                                                     
011800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011900                                                                          
012000     03  W-IDLAND-X.                                                      
012100         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
012200                                                                          
012300     03  W-IDLAND-DEF-X.                                                  
012400         05  W-IDLAND-DEF        PIC X(2)    VALUE 'SE'.                  
012500                                                                          
013500     03 W-KDSEGKEY-X.                                                     
013600         05 W-KDSEGKEY           PIC X(1)    VALUE '1'.                   
013700     03  W-W6GX-6001-KEY-X.                                               
013800         05  W-IDHTYP-6001       PIC X(04)   VALUE '6001'.                
013900         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
014000                                                                          
014100*----> NYCKLAR TILL WDK711/724                                            
014200                                                                          
014300     03  W-IDDC-K7-X.                                                     
014400         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
014500     03  W-IDLEVNR-PR-X.                                                  
014600         05  W-IDLEVNR-PR        PIC X(5)    VALUE ZERO.                  
014700     03  W-DAPRLIST-K7-N.                                                 
014800         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
014900                                                                          
015000**** NYCKLAR TILL WDB6                                                    
015100     03  W-IDDC-B6-X.                                                     
015200         05 W-IDDC-B6                  PIC X(2).                          
015300     03  W-IDLANDX2-X.                                                    
015400         05    W-IDLANDX2              PIC X(2)    VALUE SPACE.           
015500     SKIP2                                                                
015600*    --- STATUS-KOD FRÅN IMS                                              
015700                                                                          
015800 01  STATUS-WS                   PIC  X(02).                              
015900     88  SEGMENT-FINNS                       VALUE '  '.                  
016000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016100     88  END-OF-DATA                         VALUE 'GB'.                  
016200     SKIP2                                                                
016300 01  GODK-STATUSKODER.                                                    
016400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016500                                                                          
016600 01  SSA1                        PIC X(64).                               
016700 01  SSA2                        PIC X(64).                               
016800 01  SSA3                        PIC X(64).                               
016900     EJECT                                                                
017000                                                                          
017100*    ---  COPYTEXT FÖR WLFILC                                             
017200*    REKLAMATIONSFAKTURA/KREDITNOT/FELLISTA                               
017300*01  -COPY W426PKR                                                        
017400     EJECT                                                                
017500*    ---  COPYTEXT FÖR WLFILC                                             
017600*    TRANS TILL EKONOMI                                                   
017700*01  -COPY W426PEK                                                        
017800     EJECT                                                                
017900*01  -COPY W009MOMS                                                       
018000     EJECT                                                                
018010*01  -COPY W510CURR                                                       
018020     EJECT                                                                
018100 01  FILLER                    PIC X(8)    VALUE 'W553LVAL'.              
018200                                                                          
018300*01       -COPY W553LVAL                                                  
018400     EJECT                                                                
018500*                                                                         
018600*    --- IMS FUNKTIONSKODER                                               
018700*01  -COPY W0003                                                          
018800     EJECT                                                                
018900*    ---  DLI INPUT-OUTPUT AREA                                           
019000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
019100                                                                          
019200 01  DLI-IO-AREA1.                                                        
019300                                                                          
019400     03  IO-AREA1                PIC X(900)  VALUE SPACE.                 
019500                                                                          
019600     03  W6H701 REDEFINES IO-AREA1.                                       
019700*        05 -COPY W6H701                                                  
019800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
019900                                                                          
020000 01  DLI-IO-AREA2.                                                        
020100                                                                          
020200     03  IO-AREA2                PIC X(400)  VALUE SPACE.                 
020300     03  W6H712 REDEFINES IO-AREA2.                                       
020400*        05 -COPY W6H712                                                  
020500     EJECT                                                                
020600 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-W6H721'.        
020700                                                                          
020800 01  DLI-IO-W6H721.                                                       
020900                                                                          
021000     03  IO-W6H721               PIC X(240)  VALUE SPACE.                 
021100     03  W6H721 REDEFINES IO-W6H721.                                      
021200*        05 -COPY W6H721                                                  
021300     EJECT                                                                
021400 01 FILLER                       PIC X(16)   VALUE 'DLI-IO-AREA3'.        
021500     SKIP3                                                                
021600 01 DLI-IO-AREA3.                                                         
021700     03 IO-AREA3                 PIC X(900)  VALUE SPACE.                 
021800     SKIP3                                                                
021900     03 WDK601 REDEFINES IO-AREA3.                                        
022000*        05 -COPY WDK601                                                  
022100     SKIP3                                                                
022200     03 WDK611 REDEFINES IO-AREA3.                                        
022300*        05 -COPY WDK611                                                  
022400     SKIP3                                                                
022500     03 WDK621 REDEFINES IO-AREA3.                                        
022600*        05 -COPY WDK621                                                  
022700     SKIP3                                                                
022800 01 FILLER                       PIC X(16)   VALUE 'DLI-IO-AREA4'.        
022900     SKIP3                                                                
023000 01 DLI-IO-AREA4.                                                         
023100     03 IO-AREA4                 PIC X(300)  VALUE SPACE.                 
023200     03 WLLEVA01 REDEFINES IO-AREA4.                                      
023300*        05 -COPY WDF101                                                  
023400     EJECT                                                                
023500     03 WLLEVA11 REDEFINES IO-AREA4.                                      
023600*        05 -COPY WDF102                                                  
023700     EJECT                                                                
023800     03 WLLEVA14 REDEFINES IO-AREA4.                                      
023900*        05 -COPY WDF106                                                  
024000     EJECT                                                                
024100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
024200     SKIP3                                                                
024300 01  DLI-IO-AREA5.                                                        
024400     03  IO-AREA5                PIC X(110) VALUE SPACE.                  
024500     SKIP3                                                                
024600     03  W6LOPB   REDEFINES IO-AREA5.                                     
024700*        05  -COPY W6GX6002                                               
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
025000     SKIP3                                                                
025100 01  DLI-IO-AREA6.                                                        
025200     03  IO-AREA6                PIC X(300)  VALUE SPACE.                 
025300     03  WLFILC01 REDEFINES IO-AREA6.                                     
025400*        05  -COPY WDR301  -PRE FILC-                                     
025500     EJECT                                                                
026000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
026100     SKIP3                                                                
026200 01  DLI-IO-WDK711.                                                       
026300*        05  -COPY WDK711                                                 
026400                                                                          
026500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK724'.             
026600 01  DLI-IO-WDK724.                                                       
026700*        05  -COPY WDK724                                                 
026800                                                                          
026900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
027000 01   DLI-IO-AREA-B601.                                                   
027100*     03  -COPY WDB601                                                    
027200                                                                          
027300 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
027400 01   DLI-IO-AREA-B617.                                                   
027500*     03  -COPY WDB617                                                    
027600     EJECT                                                                
027700 LINKAGE SECTION.                                                         
027800                                                                          
027900*                                                                         
028000*    -COPY W602KRUP                                                       
028100*                                                                         
028200     EJECT                                                                
028300*01  -COPY W0008 -PRE W6H7-                                               
028400     05 FILLER                   PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008 -PRE WDG2-                                               
028700     05 FILLER                   PIC X.                                   
028800     EJECT                                                                
028900*01  -COPY W0008 -PRE LEVA-                                               
029000     05 FILLER                   PIC X.                                   
029100     EJECT                                                                
029200*01  -COPY W0008 -PRE WDK6-                                               
029300     05 FILLER                   PIC X.                                   
029400     EJECT                                                                
029500*01  -COPY W0008  -PRE LOPB-                                              
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01  -COPY W0008  -PRE FILC-                                              
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008  -PRE WDK7-                                              
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008  -PRE WDB6-                                              
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030900     EJECT                                                                
031000 PROCEDURE DIVISION  USING KRUP-W602KRUP                                  
031100                           W6H7-PCB                                       
031200                           WDG2-PCB                                       
031300                           LEVA-PCB                                       
031400                           WDK6-PCB                                       
031500                           LOPB-PCB                                       
031600                           FILC-PCB                                       
031700                           WDK7-PCB                                       
031800                           WDB6-PCB.                                      
032000                                                                          
032100                                                                          
032200     MOVE KRUP-IDKR TO W-IDKR                                             
032300                                                                          
032400     PERFORM IMS-GU-W6H701                                                
032500                                                                          
032600     IF SEGMENT-FINNS                                                     
032700       PERFORM A-INIT                                                     
032800       IF KRUP-FLANNULL = JA                                              
032900         PERFORM B-SKAPA-KREDIT                                           
033000       ELSE                                                               
033100         IF KR-IDLOPNRM = 0                                               
033200           PERFORM F-BEHANDLA-R40                                         
033300         END-IF                                                           
033400         IF KR-FLARBDEB = NEJ                                             
033500           IF (KR-KVART-RET > 0 OR KR-KVART-SKROT > 0  OR                 
033600               KR-KVART-SJUST < 0 OR KR-SUOMK > 0)     AND                
033700               (KR-KVAVIS > 0 OR KR-IDLOPNRM = 0)                         
033800              IF WS-FLKRLIM = JA                                          
033900                PERFORM C-SKAPA-FAKTURA                                   
034000                PERFORM D-SKRIV-TRANS-TILL-SAPR3                          
034100              END-IF                                                      
034200           END-IF                                                         
034300         ELSE                                                             
034400           IF WS-FLKRLIM = JA                                             
034500             PERFORM C-SKAPA-FAKTURA                                      
034600             PERFORM D-SKRIV-TRANS-TILL-SAPR3                             
034700           END-IF                                                         
034800         END-IF                                                           
034900         PERFORM E-UPPDATERA-STATUS                                       
035000       END-IF                                                             
035100     ELSE                                                                 
035200       MOVE 'KONTROLLRAPPORT SAKNAS PÅ W6H701' TO FELTEXT                 
035300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
035400     END-IF                                                               
035500                                                                          
035600     MOVE ZERO TO RETURN-CODE                                             
035700     GOBACK                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 A-INIT SECTION.                                                          
036100     ACCEPT DAGENS-DATUM FROM DATE                                        
036200     MOVE ZERO       TO FILC-FIL-IDSEKVNR                                 
036300     MOVE SPACE      TO KRUP-IDVERNR-OK                                   
036400     MOVE KR-IDARTNR TO W-IDARTNR                                         
036500     MOVE KR-IDLEVNR TO W-IDLEVNR                                         
036600     MOVE KR-IDDC    TO WS-IDDC                                           
036610                        W-IDDC-K7                                         
036700     IF KR-FLKRLIM = NEJ                                                  
036800       MOVE NEJ      TO WS-FLKRLIM                                        
036900     END-IF                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 B-SKAPA-KREDIT SECTION.                                                  
037300     PERFORM IMS-GHU-W6H712                                               
037400     IF SEGMENT-FINNS                                                     
037500       PERFORM IMS-GNP-W6H721                                             
037600       IF SEGMENT-FINNS                                                   
037700         IF KRED-IDPTYP = 'AUT'                                           
037800           MOVE 'REDAN AUTOMATKREDITERAD' TO FELTEXT                      
037900           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
038000         ELSE                                                             
038100           PERFORM IMS-GNP-W6H721                                         
038200           IF SEGMENT-FINNS                                               
038300             MOVE 'REDAN AUTOMATKREDITERAD' TO FELTEXT                    
038400             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
038500           ELSE                                                           
038600             MOVE '-2'            TO FILC-FIL-WDR301-DATA(9:2)            
038700             MOVE 2               TO KRED-IDSEGMNR                        
038800             MOVE '2'             TO KRED-IDVERNR(1:1)                    
038900             COMPUTE KRED-PRARTBEL-PR = (EK-PRARTBEL-PR *                 
039000                                    EK-KVKRRET) - KRED-PRARTBEL-PR        
039100             COMPUTE KRED-SUOMK-INT-5DEC = (EK-SUOMK-INT * -1) -          
039200                                               KRED-SUOMK-INT-5DEC        
039300             COMPUTE KRED-SUOMK-EXT-5DEC = (EK-SUOMK-EXT * -1) -          
039400                                              KRED-SUOMK-EXT-5DEC         
039500             COMPUTE KRED-SUMAT-5DEC = (EK-SUMAT * -1)                    
039600                                                - KRED-SUMAT-5DEC         
039700             COMPUTE KRED-PRMOMS  = (EK-PRMOMS * -1) - KRED-PRMOMS        
039800           END-IF                                                         
039900         END-IF                                                           
040000       ELSE                                                               
040100         MOVE '-1'                  TO FILC-FIL-WDR301-DATA(9:2)          
040200         MOVE 1                     TO KRED-IDSEGMNR                      
040300         MOVE '1'                   TO KRED-IDVERNR(1:1)                  
040400         COMPUTE KRED-PRARTBEL-PR    = EK-PRARTBEL-PR * EK-KVKRRET        
040500         COMPUTE KRED-SUOMK-INT-5DEC = EK-SUOMK-INT * -1                  
040600         COMPUTE KRED-SUOMK-EXT-5DEC = EK-SUOMK-EXT * -1                  
040700         COMPUTE KRED-SUMAT-5DEC     = EK-SUMAT * -1                      
040800         COMPUTE KRED-PRMOMS         = EK-PRMOMS * -1                     
040900       END-IF                                                             
041000       MOVE 'AUT'               TO KRED-IDPTYP                            
041100       MOVE EK-IDVERNR          TO KRED-IDVERNR(2:8)                      
041200       MOVE KR-BEKRANS          TO KRED-BENAEMN                           
041300       MOVE KR-IDKRATLF         TO KRED-IDTFN                             
041400       MOVE DAGENS-DATUM        TO KRED-TIKRED                            
041500       MOVE 'AUTOMATKREDITERAD' TO KRED-TEKREKON-INT                      
041600       MOVE SPACE               TO KRED-TEKREKON-EXT                      
041610                                                                          
041620       IF KRED-PRARTBEL-PR    = 0 AND                                     
041630          KRED-SUOMK-INT-5DEC = 0 AND                                     
041640          KRED-SUOMK-EXT-5DEC = 0 AND                                     
041650          KRED-SUMAT-5DEC     = 0                                         
041660          CONTINUE                                                        
041670       ELSE                                                               
041700          PERFORM IMS-ISRT-W6H721                                         
041800          IF WS-FLKRLIM = JA                                              
041900             PERFORM BA-SKRIV-KRK                                         
042000             PERFORM BB-KRED-TRANS-TILL-SAPR3                             
042100          END-IF                                                          
042110       END-IF                                                             
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042410                                                                          
042500 BA-SKRIV-KRK SECTION.                                                    
042700* KREDITNOTA-TRANS SKRIVS SOM SEGMENT PÅ WLFILC (WDR3)                    
042800* FÖR SENARE NEDLÄSNING AV PGM: W42690 (W426D2-RTN).                      
042900*                                                                         
043000     MOVE 'KRK'           TO PKR-IDPTYP                                   
043100     MOVE  KR-IDKR        TO PKR-IDKR                                     
043200                                                                          
043300     MOVE IDPGM               TO FILC-FIL-IDPGM                           
043400     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
043500     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
043600     ADD  +1              TO FILC-FIL-IDSEKVNR                            
043700     MOVE 'W426KRK '      TO FILC-FIL-IDCPYTXT                            
043800     MOVE W426PKR         TO FILC-FIL-WDR301-DATA(1:8)                    
043900                                                                          
044000     PERFORM IMS-ISRT-WLFILC01                                            
044100     .                                                                    
044200     EJECT                                                                
044210                                                                          
044300 BB-KRED-TRANS-TILL-SAPR3 SECTION.                                        
044500* TRANS TILL SAPR3 SKRIVS SOM SEGMENT PÅ WLFILC (WDR3) FÖR SENARE         
044600* NEDLÄSNING AV PGM: W42695 (W426V1-RTN).                                 
044700*                                                                         
044800     MOVE 'KEK'           TO EK-IDPTYP                                    
044900     MOVE  KR-IDKR        TO EK-IDKR                                      
045000                                                                          
045100     MOVE IDPGM               TO FILC-FIL-IDPGM                           
045200     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
045300     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
045400     ADD  +1              TO FILC-FIL-IDSEKVNR                            
045500     MOVE 'W426KEK '      TO FILC-FIL-IDCPYTXT                            
045600     MOVE W426PEK         TO FILC-FIL-WDR301-DATA(1:8)                    
045700                                                                          
045800     PERFORM IMS-ISRT-WLFILC01                                            
045900     .                                                                    
046000     EJECT                                                                
046010                                                                          
046100 C-SKAPA-FAKTURA SECTION.                                                 
046200     PERFORM IMS-GU-W6LOPB01                                              
046300     PERFORM IMS-GHNP-W6LOPB11                                            
046400     IF NDC-CN                                                            
046500       MOVE +1 TO 6002-IX                                                 
046600     END-IF                                                               
046700     IF CDC-SE                                                            
046800       MOVE +3 TO 6002-IX                                                 
046900     END-IF                                                               
047000     IF NDC-US                                                            
047100       MOVE +4 TO 6002-IX                                                 
047200     END-IF                                                               
047300     IF 6002-IDVERNR-AKT (6002-IX) = 6002-IDVERNR-MAX (6002-IX)           
047400        MOVE 'F' TO KRUP-IDVERNR-OK                                       
047500     END-IF                                                               
047600     IF KRUP-IDVERNR-OK = SPACE                                           
047700       PERFORM CA-HAMTA-PRIS                                              
047800       IF NDC-CN OR NDC-US                                                
047900         PERFORM CC-UPPDATERA-FAKTURA-XX                                  
048000       END-IF                                                             
048100       IF CDC-SE                                                          
048200         PERFORM CC-UPPDATERA-FAKTURA-SE                                  
048210       END-IF                                                             
048400     END-IF                                                               
048500                                                                          
048600     IF WS-FLKRLIM = JA                                                   
048700       PERFORM CD-SKRIV-KRF                                               
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049010                                                                          
049100 CA-HAMTA-PRIS SECTION.                                                   
049200** HÄMTAR I 1:A HAND ARTIKELNS UTLÄNDSKA BESTÄLLNINGSPRIS.                
049300** SAKNAS DETTA SÄTTS PRISET TILL 0.1 OCH RÄKNAS OM TILL                  
049400** LEVERANTÖRENS VALUTA OCH SEGMENT FÖR FELLISTA SKAPAS.                  
049500     MOVE NEJ TO FLPRIS                                                   
049600     MOVE NEJ TO FLVALUTA                                                 
049700     PERFORM IMS-GU-WDK611                                                
049800     IF SEGMENT-FINNS                                                     
049900       MOVE CLAG-PRARTSTD TO WS-CLAG-PRARTSTD                             
050000       MOVE CLAG-PRDIRLON TO WS-CLAG-PRDIRLON                             
050100       MOVE CLAG-PRDMTRL  TO WS-CLAG-PRDMTRL                              
050200       MOVE CLAG-PROVRPAL TO WS-CLAG-PROVRPAL                             
050300       MOVE CLAG-PRINK    TO WS-CLAG-PRINK                                
050400                                                                          
050500       IF NDC-CN OR NDC-US                                                
050600         PERFORM CAA-HAMTA-PRIS-XX                                        
050610         IF NDC-CN                                                        
050700           MOVE WC-LAND-CN TO W-IDLAND                                    
050710         END-IF                                                           
050720         IF NDC-US                                                        
050730           MOVE WC-LAND-US TO W-IDLAND                                    
050740         END-IF                                                           
050800       ELSE                                                               
050900         PERFORM CAB-HAMTA-PRIS-SE                                        
051000         MOVE WC-LAND-SE TO W-IDLAND                                      
051100       END-IF                                                             
051200     END-IF                                                               
051300                                                                          
051400     IF FLPRIS = NEJ AND FLVALUTA = NEJ                                   
051500** HÄMTAR LEVERANTÖRENS NUMERISKA VALUTAKOD FRÅN WDF102                   
051600       PERFORM IMS-GU-WLLEVA11                                            
051700       MOVE TULL-KDVALLEV TO KDVALLEV-NUM                                 
051800       MOVE 1             TO VAL-IX                                       
051900** ÖVERSÄTTER MHA COPYTEXT: W553LVAL TILL ALFANUMERISK VALUTAKOD          
052000** FIX - ANVÄNDER ISTÄLLET EN KOPIA PÅ CTX: WLVALFIX DÄR DE LÄNDER        
052100** SOM ANSLUTIT SIG TILL EURO DEFAULT FÅR DETTA VÄRDE.                    
052200       PERFORM UNTIL KDVALLEV-NUM = VAL-KDVALUTA(VAL-IX) OR               
052300                     VAL-IX > VAL-IX-MAX                                  
052400         ADD 1 TO VAL-IX                                                  
052500       END-PERFORM                                                        
052600                                                                          
052700       IF KDVALLEV-NUM = VAL-KDVALUTA(VAL-IX)                             
052800         MOVE VAL-KDVALISO(VAL-IX) TO WS-KDVALISO                         
052900       ELSE                                                               
053000         MOVE 'VALUTA: '    TO FELTEXT(1:8)                               
053100         MOVE KDVALLEV-NUM  TO FELTEXT(9:3)                               
053200         MOVE ' SAKNAS I ÖVERSÄTTNINGSTABELL' TO FELTEXT(12:29)           
053300         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
053400       END-IF                                                             
053500     END-IF                                                               
053600                                                                          
053700** HÄMTAR INNEVARANDE MÅNAD FÖR LANDETS VALUTAKURS                        
053800     MOVE DAGENS-TIAA       TO W-DATE-AAMM(1:2)                           
053801     MOVE DAGENS-TIMM       TO W-DATE-AAMM(3:2)                           
053802                                                                          
053810     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
053900     MOVE WS-KDVALISO       TO CURR-KDVALISO-ROW                          
054000     MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                          
054100     MOVE 'M'               TO CURR-KDVALTYP                              
054101                                                                          
054110     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
054120     IF CURR-KDSVAR = ' '                                                 
054130       MOVE CURR-PRKURS-NEW TO WS-PRKURS                                  
054140     ELSE                                                                 
054141       MOVE 1               TO WS-PRKURS                                  
054150     END-IF                                                               
054160                                                                          
054200     IF FLPRIS = NEJ                                                      
054300       IF NDC-CN OR NDC-US                                                
054400**  UTLÄNDSKT BESTÄLLNINGSPRIS SAKNAS PÅ WDK724                           
054500         COMPUTE WS-PRARTBEL-PR = 0.1                                     
054600       ELSE                                                               
054700**  UTLÄNDSKT BESTÄLLNINGSPRIS SAKNAS PÅ WDK621                           
054800**** NR 5, ANVÄND STANDARDPRIS OM DET SAKNAS PRISRADER                    
054900         COMPUTE WS-PRARTBEL-PR = WS-CLAG-PRARTSTD / WS-PRKURS            
055000       END-IF                                                             
055100       IF FLVALUTA = NEJ AND                                              
055200          WS-FLKRLIM = JA                                                 
055300         PERFORM S07-SKAPA-FELLISTA                                       
055400       END-IF                                                             
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 CAA-HAMTA-PRIS-XX SECTION.                                               
055900     PERFORM IMS-GU-WDK711                                                
056000     IF SEGMENT-FINNS                                                     
056100       MOVE KR-IDLEVNR                  TO W-IDLEVNR-PR                   
056200       IF KR-TIKRANS > ZERO                                               
056300         MOVE FUNCTION CURRENT-DATE (1:2) TO INLEV-SEKEL                  
056400         MOVE KR-TIKRANS TO INLEV-TIAAMMDD                                
056500         COMPUTE W-DAPRLIST-K7 = 99999999 - INLEV-DATUM                   
056600       ELSE                                                               
056700         MOVE FUNCTION CURRENT-DATE (1:8) TO INLEV-DATUM                  
056800         COMPUTE W-DAPRLIST-K7 = 99999999 - INLEV-DATUM                   
056900       END-IF                                                             
057000       PERFORM IMS-GNP-WDK724                                             
057100                                                                          
057200       PERFORM UNTIL SEGMENT-SAKNAS OR FLPRIS = JA                        
057300         IF SEGMENT-FINNS                                                 
057400           MOVE JA             TO FLPRIS                                  
057500           MOVE SPRL-PRARTBEL-PR TO WS-PRARTBEL-PR                        
057600           MOVE JA             TO FLVALUTA                                
057700           MOVE SPRL-KDVALISO  TO WS-KDVALISO                             
057800         END-IF                                                           
057900       END-PERFORM                                                        
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 CAB-HAMTA-PRIS-SE SECTION.                                               
058500     MOVE NEJ TO FIRST-INLEV                                              
058600     MOVE NEJ TO FIRST-LEV                                                
058700     PERFORM IMS-GNP-WDK621-FIRST                                         
058800     IF SEGMENT-FINNS                                                     
058900**** NR 4, PRIS HÄMTAS OM PRISRAD FINNS                                   
059000       MOVE PRL-KDVALISO    TO WS-KDVALISO                                
059100       MOVE PRL-PRARTBEL-PR TO WS-PRARTBEL-PR                             
059200       MOVE JA TO FLVALUTA                                                
059300       MOVE JA TO FLPRIS                                                  
059400       IF  PRL-SUINLEV-PR > 0                                             
059500       AND PRL-IDLEVNR = KR-IDLEVNR                                       
059600**** NR1, PRIS HÄMTAS OM PRISRAD MED INLEVERANS FÖR                       
059700**** SAMMA LEVERANTÖR FINNS                                               
059800         CONTINUE                                                         
059900       ELSE                                                               
060000         IF  PRL-IDLEVNR = KR-IDLEVNR                                     
060100**** NR2, PRIS HÄMTAS OM PRISRAD MED SAMMA LEVERANTÖR FINNS               
060200           MOVE JA TO FIRST-LEV                                           
060300         END-IF                                                           
060400         IF  PRL-SUINLEV-PR > 0                                           
060500**** NR3, PRIS HÄMTAS OM PRISRAD MED FÖRSTA INLEVERANS FINNS              
060600           MOVE JA TO FIRST-INLEV                                         
060700         END-IF                                                           
060800         PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                      
060900         OR (PRL-SUINLEV-PR > 0                                           
061000         AND PRL-IDLEVNR = KR-IDLEVNR)                                    
061100           PERFORM IMS-GNP-WDK621                                         
061200           IF PRL-SUINLEV-PR > 0                                          
061300           AND PRL-IDLEVNR = KR-IDLEVNR                                   
061400**** NR1, PRIS HÄMTAS OM PRISRAD MED INLEVERANS FÖR                       
061500**** LEVERANTÖR FINNS                                                     
061600              MOVE PRL-KDVALISO    TO WS-KDVALISO                         
061700              MOVE PRL-PRARTBEL-PR TO WS-PRARTBEL-PR                      
061800           ELSE                                                           
061900             IF PRL-IDLEVNR = KR-IDLEVNR                                  
062000             AND FIRST-LEV = NEJ                                          
062100**** NR2, PRIS HÄMTAS OM PRISRAD MED SAMMA LEVERANTÖR FINNS               
062200               MOVE PRL-KDVALISO  TO WS-KDVALISO                          
062300               MOVE PRL-PRARTBEL-PR TO WS-PRARTBEL-PR                     
062400               MOVE JA TO FIRST-LEV                                       
062500             END-IF                                                       
062600             IF PRL-SUINLEV-PR > 0                                        
062700             AND FIRST-INLEV = NEJ                                        
062800             AND FIRST-LEV   = NEJ                                        
062900**** NR3, PRIS HÄMTAS OM PRISRAD MED FÖRSTA INLEVERANS FINNS              
063000               MOVE PRL-KDVALISO  TO WS-KDVALISO                          
063100               MOVE PRL-PRARTBEL-PR TO WS-PRARTBEL-PR                     
063200               MOVE JA TO FIRST-INLEV                                     
063300             END-IF                                                       
063400           END-IF                                                         
063500         END-PERFORM                                                      
063600       END-IF                                                             
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100 CC-UPPDATERA-FAKTURA-SE SECTION.                                         
064200     PERFORM CCA-INITIERA-W6H712                                          
064300                                                                          
064400     MOVE WS-KDVALISO              TO EK-KDVALISO                         
064500     MOVE WS-PRKURS                TO EK-PRKURS                           
064600     MOVE WS-PRARTBEL-PR           TO EK-PRARTBEL-PR                      
064700                                                                          
064800     PERFORM IMS-GU-WLLEVA14                                              
064900     IF SEGMENT-FINNS                                                     
065000       MOVE ADR-IDLANDX2        TO TEST-IDLANDX2                          
065100       IF LEV02-IDLANDX2-SVE                                              
065200         IF CDC-SE                                                        
065300           MOVE 'SE' TO MOMS-IDLANDX2                                     
065310           MOVE '01' TO MOMS-KDVAT                                        
065400           PERFORM S06-HITTA-MOMS                                         
065500           COMPUTE WS-PRMOMS ROUNDED = MOMS-REVAT *                       
065600           ((EK-PRARTBEL-PR * EK-KVKRRET * EK-PRKURS) +                   
065700           (WS-SUOMK-EK + WS-SUMAT))                                      
065800           COMPUTE EK-PRMOMS = WS-PRMOMS * -1                             
065900         END-IF                                                           
066000       END-IF                                                             
066100     END-IF                                                               
066200** 6206                                                                   
066300*ARTIKELVÄRDE                                                             
066400     PERFORM S01-BERAEKNA-SU-STANDARDPRIS                                 
066500     IF WS-SUPRARTSTD > ZERO                                              
066600       MOVE WS-SUPRARTSTD  TO EK-SUARTSTD                                 
066700       COMPUTE EK-SUARTSTD = EK-SUARTSTD * -1                             
066800     END-IF                                                               
066900                                                                          
067000     COMPUTE WS-PRARTBES-PR ROUNDED =                                     
067100            (EK-PRARTBEL-PR * EK-PRKURS)                                  
067200     IF WS-PRARTBES-PR NOT = WS-CLAG-PRARTSTD                             
067300* KALKYLPÅLÄGG                                                            
067400       PERFORM S02-BERAEKNA-SU-KALKYLPAALAEGG                             
067500       IF WS-SUKALKYLP > ZERO                                             
067600         MOVE WS-SUKALKYLP     TO EK-SUKPALAG                             
067700       END-IF                                                             
067800                                                                          
067900* HEMTAGNINGSKOSTNAD                                                      
068000        PERFORM S01-BERAEKNA-SU-STANDARDPRIS                              
068100        PERFORM S03-BERAEKNA-SU-HEMTK                                     
068200        IF WS-SUHEMTKOST-2-DEC > ZERO                                     
068300          MOVE WS-SUHEMTKOST-2-DEC TO EK-SUHEMTAG                         
068400        END-IF                                                            
068500                                                                          
068600* PRISDIFF                                                                
068700        PERFORM S04-BERAEKNA-SU-PRISDIFF                                  
068800        IF WS-SUPRBESDIFF NOT = ZERO                                      
068900          MOVE WS-SUPRBESDIFF TO EK-SUBESDIFF                             
069000          COMPUTE EK-SUBESDIFF = EK-SUBESDIFF * -1                        
069100        END-IF                                                            
069200     END-IF                                                               
069300                                                                          
069400     MOVE 6002-IDVERNR-AKT (6002-IX) TO EK-IDVERNR                        
069500     ADD 1 TO 6002-IDVERNR-AKT (6002-IX)                                  
069600     PERFORM IMS-REPL-W6LOPB11                                            
069700                                                                          
069800     MOVE DAGENS-DATUM TO EK-TIFAKT                                       
069900     MOVE JA           TO EK-FLKLAR                                       
070000                                                                          
070100     PERFORM IMS-ISRT-W6H712                                              
070200     .                                                                    
070300     EJECT                                                                
070400                                                                          
070500 CC-UPPDATERA-FAKTURA-XX SECTION.                                         
070600     PERFORM CCA-INITIERA-W6H712                                          
070700                                                                          
070701     MOVE WS-IDDC      TO W-IDDC-B6                                       
070702     PERFORM IMS-GU-WDB601                                                
070710** HÄMTAR INNEVARANDE MÅNAD FÖR LANDETS VALUTAKURS                        
070720     MOVE DAGENS-TIAA       TO W-DATE-AAMM(1:2)                           
070730     MOVE DAGENS-TIMM       TO W-DATE-AAMM(3:2)                           
070740                                                                          
070750     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
070760     MOVE DCS-KDVALISO      TO CURR-KDVALISO-HUV                          
070761     MOVE WS-KDVALISO       TO CURR-KDVALISO-ROW                          
070780     MOVE 'M'               TO CURR-KDVALTYP                              
070790                                                                          
070791     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
070792     IF CURR-KDSVAR = ' '                                                 
070793       MOVE CURR-PRKURS-NEW TO WS-PRKURS                                  
070794     ELSE                                                                 
070795       MOVE 1               TO WS-PRKURS                                  
070796     END-IF                                                               
070800     MOVE WS-KDVALISO              TO EK-KDVALISO                         
070900     MOVE WS-PRKURS                TO EK-PRKURS                           
071000     MOVE WS-PRARTBEL-PR           TO EK-PRARTBEL-PR                      
071010     MOVE 6002-IDVERNR-AKT (6002-IX) TO EK-IDVERNR                        
071100                                                                          
071200     PERFORM IMS-GU-WLLEVA14                                              
071300     IF SEGMENT-FINNS                                                     
071400       MOVE ADR-IDLANDX2        TO TEST-IDLANDX2                          
071410***** USA HAR INGEN MOMS I DETTA FALL                                     
071500       IF LEV02-IDLANDX2-CN                                               
071600         IF NDC-CN                                                        
071700           MOVE 'CN' TO   MOMS-IDLANDX2                                   
071705           IF EK-IDVERNR < 60000080                                       
071706           AND DAGENS-TIAA = 18                                           
071710             MOVE '01' TO MOMS-KDVAT                                      
071711           ELSE                                                           
071712             IF EK-IDVERNR < 60050026                                     
071713             AND DAGENS-TIAA = 19                                         
071714               MOVE '02' TO MOMS-KDVAT                                    
071720             ELSE                                                         
071721               MOVE '03' TO MOMS-KDVAT                                    
071730             END-IF                                                       
071740           END-IF                                                         
071800           PERFORM S06-HITTA-MOMS                                         
071900           COMPUTE WS-PRMOMS ROUNDED = MOMS-REVAT *                       
072000           ((EK-PRARTBEL-PR * EK-KVKRRET) +                               
072100           (WS-SUOMK-EK + WS-SUMAT))                                      
072200           COMPUTE EK-PRMOMS = WS-PRMOMS * -1                             
072300         END-IF                                                           
072400       END-IF                                                             
072500     END-IF                                                               
072600** 6206                                                                   
072700*ARTIKELVÄRDE                                                             
072800*    PERFORM S01-BERAEKNA-SU-STANDARDPRIS                                 
072900*    IF WS-SUPRARTSTD > ZERO                                              
073000*      MOVE WS-PRARTBES-PR TO EK-SUARTSTD                                 
073100       COMPUTE EK-SUARTSTD = EK-PRARTBEL-PR * EK-KVKRRET                  
073200       COMPUTE EK-SUARTSTD = EK-SUARTSTD * -1                             
073300*    END-IF                                                               
073400                                                                          
073500*    COMPUTE WS-PRARTBES-PR ROUNDED = EK-PRARTBEL-PR                      
073600*    IF WS-PRARTBES-PR NOT = WS-CLAG-PRARTSTD                             
073700* KALKYLPÅLÄGG                                                            
073800       PERFORM S02-BERAEKNA-SU-KALKYL-XX                                  
073900       IF WS-SUKALKYLP > ZERO                                             
074000         MOVE WS-SUKALKYLP     TO EK-SUKPALAG                             
074100       END-IF                                                             
074200                                                                          
074300* HEMTAGNINGSKOSTNAD                                                      
074400*       PERFORM S01-BERAEKNA-SU-STANDARDPRIS                              
074500        PERFORM S03-BERAEKNA-SU-HEMTK-XX                                  
074600        IF WS-SUHEMTKOST-2-DEC > ZERO                                     
074700          MOVE WS-SUHEMTKOST-2-DEC TO EK-SUHEMTAG                         
074800        END-IF                                                            
074900                                                                          
075000* PRISDIFF                                                                
075100*       PERFORM S04-BERAEKNA-SU-PRISDIFF                                  
075200*       IF WS-SUPRBESDIFF NOT = ZERO                                      
075300*         MOVE WS-SUPRBESDIFF TO EK-SUBESDIFF                             
075400*         COMPUTE EK-SUBESDIFF = EK-SUBESDIFF * -1                        
075500*       END-IF                                                            
075600*    END-IF                                                               
075700                                                                          
075800     MOVE 6002-IDVERNR-AKT (6002-IX) TO EK-IDVERNR                        
075900     ADD 1 TO 6002-IDVERNR-AKT (6002-IX)                                  
076000     PERFORM IMS-REPL-W6LOPB11                                            
076100                                                                          
076200     MOVE DAGENS-DATUM TO EK-TIFAKT                                       
076300     MOVE JA           TO EK-FLKLAR                                       
076400                                                                          
076500     PERFORM IMS-ISRT-W6H712                                              
076600     .                                                                    
076700     EJECT                                                                
076800 CCA-INITIERA-W6H712 SECTION.                                             
076900     MOVE SPACE                       TO EK-BENAEMN                       
077000                                         EK-FLKLAR                        
077100                                         EK-IDTFN                         
077200                                         EK-KDVALISO                      
077300                                         EK-TEKREATG                      
077400                                         EK-TEKREKON (1)                  
077500                                         EK-TEKREKON (2)                  
077600     MOVE ZERO                        TO EK-IDBEMONR                      
077700                                         EK-KVKRRET                       
077800                                         EK-PRARTBEL-PR                   
077900                                         EK-PRKURS                        
078000                                         EK-IDVERNR                       
078100                                         EK-KVFFDAG                       
078200                                         EK-TIFAKT                        
078300                                         EK-SUARTSTD                      
078400                                         EK-SUKPALAG                      
078500                                         EK-SUHEMTAG                      
078600                                         EK-SUBESDIFF                     
078700                                         EK-SUOMK-INT                     
078800                                         EK-SUOMK-EXT                     
078900                                         EK-SUMAT                         
079000                                         EK-PRFRAKT                       
079100                                         EK-PRMOMS                        
079200     IF KR-IDKRFEL = 'PA' AND KR-KDKRUTF NOT = '4'                        
079300       COMPUTE WS-KVKRRET = KR-KVART-SJUST - KR-KVART-RET -               
079400                                             KR-KVART-SKROT               
079500       IF WS-KVKRRET < ZERO                                               
079600         COMPUTE EK-KVKRRET = WS-KVKRRET * -1                             
079700       END-IF                                                             
079800     ELSE                                                                 
079900       IF KR-KVART-RET   > +0                                             
080000       OR KR-KVART-SKROT > +0                                             
080100       OR KR-KVART-SJUST > +0                                             
080200       OR KR-KVART-SJUST < +0                                             
080300          COMPUTE EK-KVKRRET ROUNDED =                                    
080400         (KR-KVART-RET + KR-KVART-SKROT) - KR-KVART-SJUST                 
080500       END-IF                                                             
080600     END-IF                                                               
080700                                                                          
080710     COMPUTE WS-WORKTIME = KR-KVKRPACK + KR-KVARBTID                      
080800     IF NDC-CN                                                            
080900       IF KR-FLARBDEB = JA                                                
081000         IF KR-IDKRFEL(1:1) = 'P'                                         
081100            IF WS-WORKTIME > 7                                            
081200              COMPUTE WS-ARBKST-TOT-SEK =                                 
081300                    WS-WORKTIME * WS-ARBKST-TIM-CNY                       
081400            ELSE                                                          
081500              COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-CNY * 7           
081600            END-IF                                                        
081700         ELSE                                                             
081800            IF KR-IDKRFEL(1:1) = 'K'                                      
081810              IF WS-WORKTIME > 4                                          
081820                COMPUTE WS-ARBKST-TOT-SEK =                               
081830                      WS-WORKTIME * WS-ARBKST-TIM-CNY                     
081840              ELSE                                                        
081850                COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-CNY * 4         
081860              END-IF                                                      
081870            ELSE                                                          
081880              IF WS-WORKTIME > 10                                         
081890                COMPUTE WS-ARBKST-TOT-SEK =                               
081891                      WS-WORKTIME * WS-ARBKST-TIM-CNY                     
081892              ELSE                                                        
081893                COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-CNY * 10        
081894              END-IF                                                      
081895            END-IF                                                        
081896         END-IF                                                           
081897       END-IF                                                             
081900     END-IF                                                               
081910     IF CDC-SE                                                            
081920       IF KR-FLARBDEB = JA                                                
081930         IF KR-IDKRFEL(1:1) = 'P'                                         
081940            IF WS-WORKTIME > 7                                            
081950              COMPUTE WS-ARBKST-TOT-SEK =                                 
081960                    WS-WORKTIME * WS-ARBKST-TIM-SEK                       
081970            ELSE                                                          
081980              COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-SEK * 7           
081990            END-IF                                                        
082000         ELSE                                                             
082100            IF KR-IDKRFEL(1:1) = 'K'                                      
082200              IF WS-WORKTIME > 4                                          
082300                COMPUTE WS-ARBKST-TOT-SEK =                               
082400                      WS-WORKTIME * WS-ARBKST-TIM-SEK                     
082500              ELSE                                                        
082600                COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-SEK * 4         
082700              END-IF                                                      
082800            ELSE                                                          
082900              IF WS-WORKTIME > 10                                         
082910                COMPUTE WS-ARBKST-TOT-SEK =                               
082920                      WS-WORKTIME * WS-ARBKST-TIM-SEK                     
082930              ELSE                                                        
082940                COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-SEK * 10        
082950              END-IF                                                      
082960            END-IF                                                        
082970         END-IF                                                           
082980       ELSE                                                               
082990         MOVE ZERO TO WS-ARBKST-TOT-SEK                                   
082991       END-IF                                                             
083000     END-IF                                                               
083010     IF NDC-US                                                            
083020       IF KR-FLARBDEB = JA                                                
083030         IF KR-IDKRFEL(1:1) = 'P'                                         
083040            IF WS-WORKTIME > 7                                            
083050              COMPUTE WS-ARBKST-TOT-SEK =                                 
083060                    WS-WORKTIME * WS-ARBKST-TIM-USD                       
083070            ELSE                                                          
083080              COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-USD * 7           
083090            END-IF                                                        
083091         ELSE                                                             
083092            IF KR-IDKRFEL(1:1) = 'K'                                      
083093              IF WS-WORKTIME > 4                                          
083094                COMPUTE WS-ARBKST-TOT-SEK =                               
083095                      WS-WORKTIME * WS-ARBKST-TIM-USD                     
083096              ELSE                                                        
083097                COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-USD * 4         
083098              END-IF                                                      
083099            ELSE                                                          
083100              IF WS-WORKTIME > 10                                         
083101                COMPUTE WS-ARBKST-TOT-SEK =                               
083102                      WS-WORKTIME * WS-ARBKST-TIM-USD                     
083103              ELSE                                                        
083104                COMPUTE WS-ARBKST-TOT-SEK = WS-ARBKST-TIM-USD * 10        
083105              END-IF                                                      
083106            END-IF                                                        
083107         END-IF                                                           
083108       ELSE                                                               
083109         MOVE ZERO TO WS-ARBKST-TOT-SEK                                   
083110       END-IF                                                             
083111     END-IF                                                               
083120                                                                          
083200     COMPUTE WS-SUOMK-EK ROUNDED =                                        
083300                KR-SUOMK + WS-ARBKST-TOT-SEK                              
083400                                                                          
083500     MOVE KR-SUMAT           TO WS-SUMAT                                  
083600                                                                          
083700     COMPUTE EK-SUOMK-EXT = KR-SUOMK * -1                                 
083800     COMPUTE EK-SUOMK-INT = WS-ARBKST-TOT-SEK * -1                        
083900     COMPUTE EK-SUMAT = WS-SUMAT * -1                                     
084000     .                                                                    
084100     EJECT                                                                
084110                                                                          
084200 CD-SKRIV-KRF SECTION.                                                    
084400* REKLAMATIONSFAKTURA-TRANS SKRIVS SOM SEGMENT PÅ WLFILC (WDR3)           
084500* FÖR SENARE NEDLÄSNING AV PGM: W42690 (W426D2-RTN).                      
084600*                                                                         
084700     MOVE 'KRF'           TO PKR-IDPTYP                                   
084800     MOVE  KR-IDKR        TO PKR-IDKR                                     
084900                                                                          
085000     MOVE IDPGM               TO FILC-FIL-IDPGM                           
085100     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
085200     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
085300     ADD  +1              TO FILC-FIL-IDSEKVNR                            
085400     MOVE 'W426PKRF'      TO FILC-FIL-IDCPYTXT                            
085500     MOVE W426PKR         TO FILC-FIL-WDR301-DATA                         
085600                                                                          
085700     PERFORM IMS-ISRT-WLFILC01                                            
085800     .                                                                    
085900     EJECT                                                                
085910                                                                          
086000 D-SKRIV-TRANS-TILL-SAPR3 SECTION.                                        
086200* TRANS TILL SAPR3 SKRIVS SOM SEGMENT PÅ WLFILC (WDR3) FÖR SENARE         
086300* NEDLÄSNING AV PGM: W42693 (W426V1-RTN).                                 
086400*                                                                         
086500     MOVE 'EK '           TO EK-IDPTYP                                    
086600     MOVE  KR-IDKR        TO EK-IDKR                                      
086700                                                                          
086800     MOVE IDPGM               TO FILC-FIL-IDPGM                           
086900     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
087000     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
087100     ADD  +1              TO FILC-FIL-IDSEKVNR                            
087200     MOVE 'W426PEK '      TO FILC-FIL-IDCPYTXT                            
087300     MOVE W426PEK         TO FILC-FIL-WDR301-DATA                         
087400                                                                          
087500     PERFORM IMS-ISRT-WLFILC01                                            
087600     .                                                                    
087700     EJECT                                                                
087710                                                                          
087800 E-UPPDATERA-STATUS SECTION.                                              
087900     PERFORM IMS-GHU-W6H701                                               
088000     MOVE '5' TO KR-KDKRSTA                                               
088100     PERFORM IMS-REPL-W6H701                                              
088200     .                                                                    
088300     EJECT                                                                
088310                                                                          
088400 F-BEHANDLA-R40 SECTION.                                                  
088500     PERFORM IMS-GU-WDK601                                                
088600     IF KR-IDLEVNR = ART-IDLEVNR                                          
088700** HUVUDLEVERANTÖR                                                        
088800       CONTINUE                                                           
088900     ELSE                                                                 
089000       MOVE NEJ TO R40-HLEV                                               
089100     END-IF                                                               
089200     .                                                                    
089300     EJECT                                                                
089310                                                                          
089400 S01-BERAEKNA-SU-STANDARDPRIS SECTION.                                    
089600     MOVE ZERO TO WS-SUPRARTSTD                                           
089700     COMPUTE WS-SUPRARTSTD ROUNDED = WS-SUPRARTSTD +                      
089800             (WS-CLAG-PRARTSTD * EK-KVKRRET)                              
089900     .                                                                    
090000     EJECT                                                                
090010                                                                          
090100 S02-BERAEKNA-SU-KALKYLPAALAEGG SECTION.                                  
090300     MOVE ZERO TO WS-SUKALKYLP                                            
090400     COMPUTE WS-SUKALKYLP ROUNDED = WS-SUKALKYLP +                        
090500     ((WS-CLAG-PRDIRLON + WS-CLAG-PRDMTRL + WS-CLAG-PROVRPAL)             
090600      * EK-KVKRRET)                                                       
090700     .                                                                    
090800     EJECT                                                                
090810                                                                          
090900 S02-BERAEKNA-SU-KALKYL-XX SECTION.                                       
091000     MOVE ZERO TO WS-SUKALKYLP                                            
091100     MOVE WS-IDDC      TO W-IDDC-B6                                       
091200     PERFORM IMS-GU-WDB601                                                
091300     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
091400     IF SEGMENT-FINNS                                                     
091500       PERFORM IMS-GNP-WDB617                                             
091600       IF SEGMENT-FINNS                                                   
091700**** PRISERNA ÄR I SEK I CLAG SKALL OMVANDLAS TILL LOKAL VALUTA           
091800        COMPUTE WS-SUKALKYLP ROUNDED = (WS-SUKALKYLP +                    
091900           (((PROC-REDIRLON * WS-CLAG-PRDIRLON) +                         
092000             (PROC-REDMTRL  * WS-CLAG-PRDMTRL)) *                         
092100              EK-KVKRRET)) / WS-PRKURS                                    
092200       END-IF                                                             
092300     END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
092510                                                                          
092600 S03-BERAEKNA-SU-HEMTK SECTION.                                           
092800     MOVE WC-LAND-SE TO W-IDLAND                                          
092900     PERFORM IMS-GU-WLLEVA11                                              
093000     MOVE ZERO TO WS-SUHEMTKOST                                           
093100     MOVE ZERO TO WS-SUHEMTKOST-2-DEC                                     
093200     MOVE DAGENS-DATUM   TO TMP1-YYMMDD                                   
093300     MOVE TULL-TITULF    TO TMP2-YYMMDD                                   
093400     PERFORM WY2000P1                                                     
093500     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
093600          COMPUTE WS-SUHEMTKOST ROUNDED = WS-SUHEMTKOST +                 
093700        ((1 - (1 / TULL-RETULF-2)) * WS-CLAG-PRINK) * EK-KVKRRET          
093800     ELSE                                                                 
093900          COMPUTE WS-SUHEMTKOST ROUNDED = WS-SUHEMTKOST +                 
094000        ((1 - (1 / TULL-RETULF-1)) * WS-CLAG-PRINK) * EK-KVKRRET          
094100     END-IF                                                               
094200     COMPUTE WS-SUHEMTKOST-2-DEC ROUNDED = WS-SUHEMTKOST-2-DEC +          
094300             WS-SUHEMTKOST                                                
094400     .                                                                    
094500     EJECT                                                                
094510                                                                          
094600 S03-BERAEKNA-SU-HEMTK-XX SECTION.                                        
094700     IF NDC-CN                                                            
094800       MOVE WC-LAND-CN TO W-IDLAND                                        
094810     END-IF                                                               
094820     IF NDC-US                                                            
094830       MOVE WC-LAND-US TO W-IDLAND                                        
094840     END-IF                                                               
094900     PERFORM IMS-GU-WLLEVA11                                              
095000     MOVE ZERO TO WS-SUHEMTKOST                                           
095100     MOVE ZERO TO WS-SUHEMTKOST-2-DEC                                     
095200     MOVE DAGENS-DATUM   TO TMP1-YYMMDD                                   
095300     MOVE TULL-TITULF    TO TMP2-YYMMDD                                   
095400     PERFORM WY2000P1                                                     
095500     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
095600          COMPUTE WS-SUHEMTKOST ROUNDED = WS-SUHEMTKOST +                 
095700        (((TULL-RETULF-2) - 1) * WS-PRARTBEL-PR) * EK-KVKRRET             
095800     ELSE                                                                 
095900          COMPUTE WS-SUHEMTKOST ROUNDED = WS-SUHEMTKOST +                 
096000        (((TULL-RETULF-1) - 1) * WS-PRARTBEL-PR) * EK-KVKRRET             
096100     END-IF                                                               
096200     COMPUTE WS-SUHEMTKOST-2-DEC ROUNDED = WS-SUHEMTKOST-2-DEC +          
096300             WS-SUHEMTKOST                                                
096400     .                                                                    
096500     EJECT                                                                
096510                                                                          
096600 S04-BERAEKNA-SU-PRISDIFF SECTION.                                        
096800     MOVE ZERO TO WS-SUPRARTBES-PR                                        
096900     COMPUTE WS-PRARTBES-PR ROUNDED =                                     
097000             EK-PRARTBEL-PR * EK-PRKURS                                   
097100     COMPUTE WS-SUPRARTBES-PR ROUNDED =                                   
097200             (EK-PRARTBEL-PR * EK-PRKURS) * EK-KVKRRET                    
097300                                                                          
097400     COMPUTE WS-SUPRBESDIFF ROUNDED =                                     
097500            (WS-SUPRARTBES-PR - WS-SUPRARTSTD +                           
097600             WS-SUKALKYLP + WS-SUHEMTKOST-2-DEC)                          
097700     .                                                                    
097800     EJECT                                                                
097810                                                                          
097900 S06-HITTA-MOMS  SECTION.                                                 
098100     CALL W009MOMS USING MOMS-W009MOMS                                    
098200     .                                                                    
098300     EJECT                                                                
098310                                                                          
098400 S07-SKAPA-FELLISTA SECTION.                                              
098600* FELLISTA-TRANS SKRIVS SOM SEGMENT PÅ WLFILC (WDR3)                      
098700* FÖR SENARE NEDLÄSNING AV PGM: W42698 (W426V1-RTN).                      
098800*                                                                         
098900     MOVE 'KRL'           TO PKR-IDPTYP                                   
099000     MOVE  KR-IDKR        TO PKR-IDKR                                     
099100                                                                          
099200     MOVE IDPGM               TO FILC-FIL-IDPGM                           
099300     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
099400     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
099500     ADD  +1              TO FILC-FIL-IDSEKVNR                            
099600     MOVE 'W426PKR '      TO FILC-FIL-IDCPYTXT                            
099700     MOVE W426PKR         TO FILC-FIL-WDR301-DATA                         
099800                                                                          
099900     PERFORM IMS-ISRT-WLFILC01                                            
100000     .                                                                    
100100     EJECT                                                                
100110                                                                          
100200 IMS-GU-W6H701 SECTION.                                                   
100300     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
100400          DELIMITED BY SIZE INTO SSA1                                     
100500     MOVE '  GE' TO GODK-STATUSKODER                                      
100600     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA1 SSA1                     
100700     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     .                                                                    
101000     SKIP2                                                                
101010                                                                          
101100 IMS-GHU-W6H701 SECTION.                                                  
101200     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
101300          DELIMITED BY SIZE INTO SSA1                                     
101400     MOVE '  GE' TO GODK-STATUSKODER                                      
101500     CALL CBLTDLI USING GHU W6H7-PCB DLI-IO-AREA1 SSA1                    
101600     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
101700     PERFORM IMS-STATUSKONTROLL                                           
101800     .                                                                    
101900     SKIP2                                                                
101910                                                                          
102000 IMS-REPL-W6H701 SECTION.                                                 
102100     MOVE '  ' TO GODK-STATUSKODER                                        
102200     CALL CBLTDLI USING REPL W6H7-PCB DLI-IO-AREA1                        
102300     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
102400     PERFORM IMS-STATUSKONTROLL                                           
102500     .                                                                    
102600     EJECT                                                                
102610                                                                          
102700 IMS-GHU-W6H712 SECTION.                                                  
102800     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
102900          DELIMITED BY SIZE INTO SSA1                                     
103000     MOVE 'W6H712  '      TO SSA2                                         
103100     MOVE '  GE' TO GODK-STATUSKODER                                      
103200     CALL CBLTDLI USING GHU W6H7-PCB DLI-IO-AREA2 SSA1 SSA2               
103300     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
103400     PERFORM IMS-STATUSKONTROLL                                           
103500     .                                                                    
103600     SKIP2                                                                
103610                                                                          
103700 IMS-ISRT-W6H712 SECTION.                                                 
103800     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
103900          DELIMITED BY SIZE INTO SSA1                                     
104000     MOVE 'W6H712  '          TO SSA2                                     
104100     MOVE '  ' TO GODK-STATUSKODER                                        
104200     CALL CBLTDLI USING ISRT W6H7-PCB DLI-IO-AREA2 SSA1 SSA2              
104300     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
104400     PERFORM IMS-STATUSKONTROLL                                           
104500     .                                                                    
104600     SKIP2                                                                
104610                                                                          
104700 IMS-GNP-W6H721 SECTION.                                                  
104800     MOVE 'W6H721  '         TO SSA1                                      
104900     MOVE '  GE' TO GODK-STATUSKODER                                      
105000     CALL CBLTDLI USING GNP W6H7-PCB DLI-IO-W6H721 SSA1                   
105100     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400     SKIP2                                                                
105410                                                                          
105500 IMS-ISRT-W6H721 SECTION.                                                 
105600     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
105700          DELIMITED BY SIZE INTO SSA1                                     
105800     MOVE 'W6H712  '          TO SSA2                                     
105900     MOVE 'W6H721  '          TO SSA3                                     
106000     MOVE '  ' TO GODK-STATUSKODER                                        
106100     CALL CBLTDLI USING ISRT W6H7-PCB DLI-IO-W6H721 SSA1 SSA2 SSA3        
106200     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
106300     PERFORM IMS-STATUSKONTROLL                                           
106400     .                                                                    
106500     SKIP2                                                                
106510                                                                          
106600 IMS-GU-WDK601 SECTION.                                                   
106700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
106800          DELIMITED BY SIZE INTO SSA1                                     
106900     MOVE '  GE' TO GODK-STATUSKODER                                      
107000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA3 SSA1                     
107100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
107200     PERFORM IMS-STATUSKONTROLL                                           
107300     .                                                                    
107400     SKIP2                                                                
107410                                                                          
107500 IMS-GU-WDK611 SECTION.                                                   
107600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
107700          DELIMITED BY SIZE INTO SSA1                                     
107800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
107900          DELIMITED BY SIZE INTO SSA2                                     
108000     MOVE '  GE' TO GODK-STATUSKODER                                      
108100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA3 SSA1 SSA2                
108200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
108300     PERFORM IMS-STATUSKONTROLL                                           
108400     .                                                                    
108500     SKIP2                                                                
108510                                                                          
108600 IMS-GNP-WDK621-FIRST SECTION.                                            
108700     MOVE 'WDK621  *F'       TO SSA1                                      
108800     MOVE '  GE' TO GODK-STATUSKODER                                      
108900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA3 SSA1                    
109000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
109100     PERFORM IMS-STATUSKONTROLL                                           
109200     .                                                                    
109300     SKIP2                                                                
109310                                                                          
109400 IMS-GNP-WDK621 SECTION.                                                  
109500     MOVE 'WDK621  '         TO SSA1                                      
109600     MOVE '  GE' TO GODK-STATUSKODER                                      
109700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA3 SSA1                    
109800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
109900     PERFORM IMS-STATUSKONTROLL                                           
110000     .                                                                    
110100     SKIP2                                                                
110110                                                                          
111400 IMS-GU-WLLEVA11 SECTION.                                                 
111500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
111600              DELIMITED BY SIZE INTO SSA1                                 
111700     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X  ')'                         
111800              DELIMITED BY SIZE INTO SSA2                                 
111900     MOVE '  ' TO GODK-STATUSKODER                                        
112000     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA4 SSA1 SSA2                
112100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
112200     PERFORM IMS-STATUSKONTROLL                                           
112300     .                                                                    
112400     SKIP2                                                                
112410                                                                          
112500 IMS-GU-WLLEVA14 SECTION.                                                 
112600     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
112700          DELIMITED BY SIZE INTO SSA1                                     
112800     MOVE 'WLLEVA14'    TO SSA2                                           
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA4 SSA1 SSA2                
113100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     EJECT                                                                
113410                                                                          
113500 IMS-GU-W6LOPB01 SECTION.                                                 
113600     STRING 'W6LOPB01(W6GXKEY  =' W-W6GX-6001-KEY-X ')'                   
113700          DELIMITED BY SIZE INTO SSA1                                     
113800     MOVE '  ' TO GODK-STATUSKODER                                        
113900     CALL CBLTDLI USING GU LOPB-PCB DLI-IO-AREA5 SSA1                     
114000     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     .                                                                    
114300     EJECT                                                                
114310                                                                          
114400 IMS-GHNP-W6LOPB11 SECTION.                                               
114500     MOVE 'W6LOPB11' TO SSA1                                              
114600     MOVE '  ' TO GODK-STATUSKODER                                        
114700     CALL CBLTDLI USING GHNP LOPB-PCB DLI-IO-AREA5 SSA1                   
114800     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
114900     PERFORM IMS-STATUSKONTROLL                                           
115000     .                                                                    
115100     SKIP2                                                                
115110                                                                          
115200 IMS-REPL-W6LOPB11 SECTION.                                               
115300     MOVE '  ' TO GODK-STATUSKODER                                        
115400     CALL CBLTDLI USING REPL LOPB-PCB DLI-IO-AREA5                        
115500     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
115600     PERFORM IMS-STATUSKONTROLL                                           
115700     .                                                                    
115800     EJECT                                                                
115810                                                                          
115900 IMS-ISRT-WLFILC01 SECTION.                                               
116000     MOVE 'WLFILC01' TO SSA1                                              
116100     MOVE '  ' TO GODK-STATUSKODER                                        
116200     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA6 SSA1                   
116300     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600     EJECT                                                                
116610                                                                          
116700 IMS-GU-WDK711 SECTION.                                                   
116800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
116900          DELIMITED BY SIZE INTO SSA1                                     
117000     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
117100          DELIMITED BY SIZE INTO SSA2                                     
117200     MOVE '  GE' TO GODK-STATUSKODER                                      
117300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711                         
117400          SSA1 SSA2                                                       
117500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117800                                                                          
117900 IMS-GNP-WDK724 SECTION.                                                  
118000     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
118100                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE '  GE' TO GODK-STATUSKODER                                      
118400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
118500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUSKONTROLL                                           
118700     .                                                                    
118800     EJECT                                                                
118900                                                                          
119000 IMS-GU-WDB601    SECTION.                                                
119100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
119200          DELIMITED BY SIZE INTO SSA1                                     
119300     MOVE '  GE' TO GODK-STATUSKODER                                      
119400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
119500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
119600     PERFORM IMS-STATUSKONTROLL                                           
119700     IF SEGMENT-SAKNAS                                                    
119800         MOVE SPACE TO DCS-KDDC                                           
119900     END-IF                                                               
120000     .                                                                    
120100                                                                          
120200 IMS-GNP-WDB617    SECTION.                                               
120300     MOVE 'WDB617   ' TO SSA1                                             
120400     MOVE '  GE' TO GODK-STATUSKODER                                      
120500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
120600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900                                                                          
121000 IMS-STATUSKONTROLL SECTION.                                              
121100     SET STATUS-IX TO 1                                                   
121200     SEARCH GODK-STATUS                                                   
121300       AT END CALL FELLOG                                                 
121400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
121500     END-SEARCH                                                           
121600     .                                                                    
121700     EJECT                                                                
121800*    -COPY WY2000P1                                                       
