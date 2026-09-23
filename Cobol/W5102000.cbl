000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5102000.                                                
000400 AUTHOR.         CHRISTINA BRUHN.                                         
000500 DATE-WRITTEN.   92/10/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER ARTIKELREG (WDK6) OCH SKAPAR LADDNINGSFIL                  
001000*        TILL WDK1.                                                       
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6 MED SB                                
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100     SELECT W51025                     ASSIGN TO W51020D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W51025                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000     SKIP2                                                                
003100*01  POST -COPY WDK101 -PRE  UT-  -L.                                     
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W5102000'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
003910 77  FIRST-INLEV                 PIC X       VALUE 'N'.                   
004000 77  NY-ART-SW                   PIC X       VALUE 'N'.                   
004100     88  NY-ARTIKEL                          VALUE 'J'.                   
004200 77  FIRST-TIME-SW               PIC X       VALUE 'N'.                   
004300     88  FIRST-TIME                          VALUE 'J'.                   
004400 77  MAX-LAES-PROD               PIC S9(9)   VALUE +0  COMP SYNC.         
004500 77  IX                          PIC S9(9)   VALUE +0  COMP SYNC.         
004600 77  IND                         PIC S9(9)   VALUE +0  COMP SYNC.         
004700 77  INDX                        PIC S9(9)   VALUE +0  COMP SYNC.         
004800     EJECT                                                                
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005500     SKIP2                                                                
005600*    --- PARAMETRAR TILL ABEND                                            
005700                                                                          
005800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  DATUM-FAELT.                                                         
006200     03  DAGENS-AAAAMMDD         PIC 9(8)    VALUE ZERO.                  
006210     03  WS-DAPRLIST-MAX         PIC 9(8)    VALUE 99999999.              
006300     03  WS-DAPRLIST             PIC 9(8).                                
006400     03  FILLER  REDEFINES WS-DAPRLIST.                                   
006500      05 FILLER                  PIC 9(2).                                
006600      05 WS-LISTDATUM            PIC 9(6).                                
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600 01  UT-AREA-START               PIC X(24)   VALUE                        
007700                                 'UT-AREA-START  '.                       
007800     SKIP2                                                                
007900                                                                          
008000*01  AREA -COPY WDK101     -PRE UT-                                       
008100     EJECT                                                                
008200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008300*                                                                         
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  BASEN-SLUT                          VALUE 'GB'.                  
009100     SKIP2                                                                
009200 01  GODK-STATUSKODER.                                                    
009300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010400     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
010500     SKIP3                                                                
010600     03  WDK601   REDEFINES IO-AREA.                                      
010700*        05  -COPY WDK601                                                 
010800     EJECT                                                                
010900     03  WDK611   REDEFINES IO-AREA.                                      
011000*        05  -COPY WDK611                                                 
011100     EJECT                                                                
011200     03  WDK621   REDEFINES IO-AREA.                                      
011300*        05  -COPY WDK621                                                 
011400     EJECT                                                                
011500     03  WDK622   REDEFINES IO-AREA.                                      
011600*        05  -COPY WDK622                                                 
011700     EJECT                                                                
011800     03  WDK623   REDEFINES IO-AREA.                                      
011900*        05  -COPY WDK623                                                 
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200     SKIP2                                                                
012300*01  -COPY W0008  -PRE WDK6-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING WDK6-PCB.                                      
012700     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     MOVE JA TO FIRST-TIME-SW                                             
013200     MOVE +0 TO MAX-LAES-PROD                                             
013300     PERFORM IMS-GN-WDK6                                                  
013400     PERFORM UNTIL BASEN-SLUT                                             
013500*    PERFORM UNTIL BASEN-SLUT OR MAX-LAES-PROD > +250                     
013600       EVALUATE WDK6-SEG-NAME-FB                                          
013700         WHEN 'WDK601  '                                                  
013800           IF NOT FIRST-TIME                                              
013900             PERFORM S01-SKRIV-UTFIL                                      
014000           ELSE                                                           
014100             MOVE NEJ TO FIRST-TIME-SW                                    
014200           END-IF                                                         
014300           PERFORM B-NOLLSTAELL-AREA                                      
014400           MOVE JA TO NY-ART-SW                                           
014500           MOVE +1 TO IX IND INDX                                         
014600           MOVE ART-IDARTNR           TO UT-ART-IDARTNR                   
014700           MOVE ART-REKSIFFR          TO UT-ART-REKSIFFR                  
014800           MOVE ART-KDERS-UTG         TO UT-ART-KDERS-UTG                 
014900           MOVE ART-KDPRODSL          TO UT-ART-KDPRODSL                  
015000           ADD +1 TO MAX-LAES-PROD                                        
015100         WHEN 'WDK611  '                                                  
015200           MOVE CLAG-IDINK            TO UT-ART-IDINK                     
015300           MOVE CLAG-IDANSK           TO UT-ART-IDANSK                    
015400           MOVE CLAG-KDAVT            TO UT-ART-KDAVT                     
015500           MOVE CLAG-FLSPKOST         TO UT-ART-FLSPKOST                  
015600           MOVE CLAG-PRHEMTAG         TO UT-ART-PRHEMTAG                  
015700           MOVE CLAG-PRINK            TO UT-ART-PRINK                     
015800           MOVE CLAG-PRARTSTD         TO UT-ART-PRARTSTD                  
015900           MOVE ZERO                  TO UT-ART-PRARTBES                  
016000           MOVE CLAG-PRARTSJK         TO UT-ART-PRARTSJK                  
016100           MOVE CLAG-PRDIRLON         TO UT-ART-PRDIRLON                  
016200           MOVE CLAG-PRDMTRL          TO UT-ART-PRDMTRL                   
016300           MOVE CLAG-PROVRPAL         TO UT-ART-PROVRPAL                  
016400           MOVE CLAG-KDTIPPR          TO UT-ART-KDTIPPR                   
016500           MOVE CLAG-ADLAGOMR         TO UT-ART-ADLAGOMR                  
016600           MOVE CLAG-ADGANG           TO UT-ART-ADGANG                    
016700           MOVE CLAG-ADPLATS          TO UT-ART-ADPLATS                   
016800         WHEN 'WDK621  '                                                  
016900           IF PRL-FLHUVLEV = JA                                           
017000             IF IX < +6                                                   
017100               SUBTRACT PRL-DAPRLIST-9KOMPL FROM WS-DAPRLIST-MAX          
017200               GIVING WS-DAPRLIST                                         
017300               MOVE WS-LISTDATUM        TO UT-ART-TIPRLIST (IX)           
017400               MOVE PRL-IDLEVNR         TO UT-ART-IDLEVNR-PR (IX)         
017500               MOVE PRL-PRARTBES-PR     TO UT-ART-PRARTBES-PR(IX)         
017600               MOVE PRL-PRARTBEL-PR     TO UT-ART-PRARTBEL-PR(IX)         
017700               MOVE PRL-KDSTATUS-PR     TO UT-ART-KDSTATUS-PR(IX)         
017800               MOVE PRL-SUINLEV-PR      TO UT-ART-SUINLEV-PR (IX)         
017900               ADD +1 TO IX                                               
018020* PRARTBES BORTTAGET FRÅN K611 MEN KVAR PÅ W01160                         
018030* DÄRFÖR HÄMTAS PRARTBES-PR FRÅN K621 TILL CLAG-PRARTBES(FILEN)           
018040                                                                          
018041               MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD         
018050               COMPUTE WS-DAPRLIST = 99999999 - DAGENS-AAAAMMDD           
018060                                                                          
018070               IF PRL-DAPRLIST-9KOMPL NOT < WS-DAPRLIST                   
018080                 IF UT-ART-PRARTBES = 0 OR FIRST-INLEV = NEJ              
018090                   IF UT-ART-PRARTBES = 0                                 
018091                     MOVE PRL-PRARTBES-PR                                 
018092                                  TO UT-ART-PRARTBES                      
018093                   END-IF                                                 
018094                   IF PRL-SUINLEV-PR > ZERO                               
018095                     MOVE JA  TO FIRST-INLEV                              
018096                     MOVE PRL-PRARTBES-PR                                 
018097                                  TO UT-ART-PRARTBES                      
018098                   ELSE                                                   
018099                     MOVE NEJ TO FIRST-INLEV                              
018100                   END-IF                                                 
018101                 END-IF                                                   
018102               ELSE                                                       
018103                 MOVE UT-ART-PRARTSTD TO UT-ART-PRARTBES                  
018104               END-IF                                                     
018110             END-IF                                                       
018200           END-IF                                                         
018300         WHEN 'WDK622  '                                                  
018400           IF IND < +6                                                    
018500             IF BEST-KDBEH-BEST = +1 OR +2 OR 5 OR +6                     
018600               MOVE BEST-KDBEH-BEST   TO UT-ART-KDBEH-BEST (IND)          
018700               MOVE BEST-IDBEST       TO UT-ART-IDBEST  (IND)             
018800               MOVE BEST-KVBEST       TO UT-ART-KVBEST  (IND)             
018900               MOVE BEST-TIBEST       TO UT-ART-TIBEST  (IND)             
019000               MOVE BEST-IDLEVNR-BEST TO UT-ART-IDLEVNR-BEST(IND)         
019100               MOVE BEST-KVBEST-BEKR  TO UT-ART-KVBEST-BEKR (IND)         
019200               ADD +1 TO IND                                              
019300             END-IF                                                       
019400           END-IF                                                         
019500         WHEN 'WDK623  '                                                  
019600           IF INDX < +6                                                   
019700             MOVE AVT-IDAVTAL        TO UT-ART-IDAVTAL (INDX)             
019800             MOVE AVT-IDLEVNR-AVT    TO UT-ART-IDLEVNR-AVT (INDX)         
019900             MOVE AVT-KVAVTANT       TO UT-ART-KVAVTANT (INDX)            
020000             MOVE AVT-TIAVTAL        TO UT-ART-TIAVTAL (INDX)             
020100             ADD +1 TO INDX                                               
020200           END-IF                                                         
020300       END-EVALUATE                                                       
020400       PERFORM IMS-GN-WDK6                                                
020500     END-PERFORM                                                          
020600     PERFORM S01-SKRIV-UTFIL                                              
020700     PERFORM Z-FINIT                                                      
020800                                                                          
020900     MOVE ZERO TO RETURN-CODE                                             
021000     GOBACK                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 A-INIT SECTION.                                                          
021400                                                                          
021500     OPEN OUTPUT W51025                                                   
021600                                                                          
021700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021800     .                                                                    
021900     EJECT                                                                
022000 B-NOLLSTAELL-AREA SECTION.                                               
022100                                                                          
022200     MOVE SPACE TO UT-ART-FLSPKOST                                        
022300     MOVE ZERO  TO UT-ART-IDANSK                                          
022400                   UT-ART-IDINK                                           
022500                   UT-ART-PRHEMTAG                                        
022600                   UT-ART-KDAVT                                           
022700                   UT-ART-KDERS-UTG                                       
022800                   UT-ART-KDPRODSL                                        
022900                   UT-ART-KDTIPPR                                         
023000                   UT-ART-PRARTBES                                        
023100                   UT-ART-PRARTSJK                                        
023200                   UT-ART-PRARTSTD                                        
023300                   UT-ART-PRDIRLON                                        
023400                   UT-ART-PRDMTRL                                         
023500                   UT-ART-PRINK                                           
023600                   UT-ART-PROVRPAL                                        
023700                   UT-ART-ADLAGOMR                                        
023800                   UT-ART-ADGANG                                          
023900                   UT-ART-ADPLATS                                         
024000                   UT-ART-IDAVTAL(1)                                      
024100                   UT-ART-IDAVTAL(2)                                      
024200                   UT-ART-IDAVTAL(3)                                      
024300                   UT-ART-IDAVTAL(4)                                      
024400                   UT-ART-IDAVTAL(5)                                      
024500                   UT-ART-IDBEST(1)                                       
024600                   UT-ART-IDBEST(2)                                       
024700                   UT-ART-IDBEST(3)                                       
024800                   UT-ART-IDBEST(4)                                       
024900                   UT-ART-IDBEST(5)                                       
025000                   UT-ART-KDBEH-BEST (1)                                  
025100                   UT-ART-KDBEH-BEST (2)                                  
025200                   UT-ART-KDBEH-BEST (3)                                  
025300                   UT-ART-KDBEH-BEST (4)                                  
025400                   UT-ART-KDBEH-BEST (5)                                  
025500                   UT-ART-KDSTATUS-PR (1)                                 
025600                   UT-ART-KDSTATUS-PR (2)                                 
025700                   UT-ART-KDSTATUS-PR (3)                                 
025800                   UT-ART-KDSTATUS-PR (4)                                 
025900                   UT-ART-KDSTATUS-PR (5)                                 
026000                   UT-ART-KVAVTANT (1)                                    
026100                   UT-ART-KVAVTANT (2)                                    
026200                   UT-ART-KVAVTANT (3)                                    
026300                   UT-ART-KVAVTANT (4)                                    
026400                   UT-ART-KVAVTANT (5)                                    
026500                   UT-ART-KVBEST-BEKR (1)                                 
026600                   UT-ART-KVBEST-BEKR (2)                                 
026700                   UT-ART-KVBEST-BEKR (3)                                 
026800                   UT-ART-KVBEST-BEKR (4)                                 
026900                   UT-ART-KVBEST-BEKR (5)                                 
027000                   UT-ART-KVBEST  (1)                                     
027100                   UT-ART-KVBEST  (2)                                     
027200                   UT-ART-KVBEST  (3)                                     
027300                   UT-ART-KVBEST  (4)                                     
027400                   UT-ART-KVBEST  (5)                                     
027500                   UT-ART-PRARTBEL-PR (1)                                 
027600                   UT-ART-PRARTBEL-PR (2)                                 
027700                   UT-ART-PRARTBEL-PR (3)                                 
027800                   UT-ART-PRARTBEL-PR (4)                                 
027900                   UT-ART-PRARTBEL-PR (5)                                 
028000                   UT-ART-PRARTBES-PR (1)                                 
028100                   UT-ART-PRARTBES-PR (2)                                 
028200                   UT-ART-PRARTBES-PR (3)                                 
028300                   UT-ART-PRARTBES-PR (4)                                 
028400                   UT-ART-PRARTBES-PR (5)                                 
028500                   UT-ART-SUINLEV-PR (1)                                  
028600                   UT-ART-SUINLEV-PR (2)                                  
028700                   UT-ART-SUINLEV-PR (3)                                  
028800                   UT-ART-SUINLEV-PR (4)                                  
028900                   UT-ART-SUINLEV-PR (5)                                  
029000                   UT-ART-TIAVTAL (1)                                     
029100                   UT-ART-TIAVTAL (2)                                     
029200                   UT-ART-TIAVTAL (3)                                     
029300                   UT-ART-TIAVTAL (4)                                     
029400                   UT-ART-TIAVTAL (5)                                     
029500                   UT-ART-TIBEST  (1)                                     
029600                   UT-ART-TIBEST  (2)                                     
029700                   UT-ART-TIBEST  (3)                                     
029800                   UT-ART-TIBEST  (4)                                     
029900                   UT-ART-TIBEST  (5)                                     
030000                   UT-ART-TIPRLIST (1)                                    
030100                   UT-ART-TIPRLIST (2)                                    
030200                   UT-ART-TIPRLIST (3)                                    
030300                   UT-ART-TIPRLIST (4)                                    
030400                   UT-ART-TIPRLIST (5)                                    
030500     MOVE SPACE TO UT-ART-IDLEVNR-AVT (1)                                 
030600                   UT-ART-IDLEVNR-AVT (2)                                 
030700                   UT-ART-IDLEVNR-AVT (3)                                 
030800                   UT-ART-IDLEVNR-AVT (4)                                 
030900                   UT-ART-IDLEVNR-AVT (5)                                 
031000                   UT-ART-IDLEVNR-BEST(1)                                 
031100                   UT-ART-IDLEVNR-BEST(2)                                 
031200                   UT-ART-IDLEVNR-BEST(3)                                 
031300                   UT-ART-IDLEVNR-BEST(4)                                 
031400                   UT-ART-IDLEVNR-BEST(5)                                 
031500                   UT-ART-IDLEVNR-PR (1)                                  
031600                   UT-ART-IDLEVNR-PR (2)                                  
031700                   UT-ART-IDLEVNR-PR (3)                                  
031800                   UT-ART-IDLEVNR-PR (4)                                  
031900                   UT-ART-IDLEVNR-PR (5)                                  
032000                   UT-ART-IDLEVNR-PR (6)                                  
032100      MOVE ZERO TO UT-ART-KDSTATUS-PR (6)                                 
032200                   UT-ART-PRARTBEL-PR (6)                                 
032300                   UT-ART-PRARTBES-PR (6)                                 
032400                   UT-ART-SUINLEV-PR (6)                                  
032500                   UT-ART-TIPRLIST (6)                                    
032600** INDEX 6 PÅ OVANSTÅENDE BÖR TAS BORT MEN DET INNEBÄR ÄNDRING AV         
032700** BAS WDK1 OCH BASCOPYTEXT WDK101 SAMT FILCOPYTEXT W51025 OCH            
032800** ÄNDRING I PROGRAMMEN W5102600 OCH W5011300.                            
032900     .                                                                    
033000     EJECT                                                                
033100 Z-FINIT SECTION.                                                         
033200     SKIP2                                                                
033300     CLOSE W51025                                                         
033400     SKIP2                                                                
033500     MOVE 'S' TO POSTSUM-OPKOD                                            
033600     CALL POSTSUM USING POSTSUM-PARM                                      
033700     .                                                                    
033800     EJECT                                                                
033900 S01-SKRIV-UTFIL SECTION.                                                 
034000     SKIP2                                                                
034100     WRITE UT-POST FROM UT-AREA                                           
034200                                                                          
034300     MOVE 'WDK6'     TO POSTSUM-TRANSTYP                                  
034400     MOVE 'W51025'   TO POSTSUM-FDNAMN                                    
034500     MOVE 'W51020D1' TO POSTSUM-DDNAMN2                                   
034600     CALL POSTSUM USING POSTSUM-PARM                                      
034700     .                                                                    
034800     EJECT                                                                
034900* --- IMS SEKTIONER ---                                                   
035000     SKIP2                                                                
035100 IMS-GN-WDK6    SECTION.                                                  
035200     SKIP2                                                                
035300     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
035400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
035500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800     SKIP2                                                                
035900 IMS-STATUSKONTROLL SECTION.                                              
036000     SKIP2                                                                
036100     SET STATUS-IX TO 1                                                   
036200     SEARCH GODK-STATUS                                                   
036300       AT END                                                             
036400         MOVE 'EJ GODK STATUS' TO FELTEXT-STR                             
036500         DISPLAY FELTEXT                                                  
036600         CALL FELLOG                                                      
036700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036800         CONTINUE                                                         
036900     END-SEARCH                                                           
037000     .                                                                    
