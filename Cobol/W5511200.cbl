000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W5511200.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           SEPT 1991.                                       
000600                                                                          
000700*    REMARKS.                                                             
000800*            PROGRAMMET BYTTE NAMN 23/5 1995 FRÅN W4241200 /KJH           
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PRISSÄTNING AV EMBALLAGE                                         
001200*                                                                         
001300*        INDATA. INFIL  - EXTRAKT UR WDK6 VIA SPIS SAMT MÅLERIART.        
001400*                         BESTÅR AV TVÅ FILER W55111 OCH W55118           
001600*                                                                         
001700*        UTDATA. W55114 - FIL TILL EKONOMISYSTEMET FÖR UPPDATERING        
001800*                         AV WDK6 (R24).                                  
001900*                W55115 - FIL FÖR UTSKRIFT AV LISTOR.                     
002000*                W55116 - FELFIL                                          
002100*                                                                         
002200*  ABENDKODER:                                                            
002300*                                                                         
002400*        U0016     - OM RETURKOD FRÅN SORT                                
002500*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300     SELECT INFIL   ASSIGN TO W55112D1.                                   
003500     SELECT W55114  ASSIGN TO W55112D2.                                   
003600     SELECT W55115  ASSIGN TO W55112D3.                                   
003700     SELECT W55116  ASSIGN TO W55112D4.                                   
003800     SELECT SORTFIL ASSIGN TO W55112DS.                                   
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100                                                                          
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  INFIL                                                                
004500     RECORDING F                                                          
004600     BLOCK CONTAINS 0.                                                    
004700*01  IN-POST  -COPY W55159     -L                                         
004800     SKIP3                                                                
005400 FD  W55114                                                               
005500     RECORDING F                                                          
005600     BLOCK CONTAINS 0.                                                    
005700*01  W55114-POST  -COPY W55124     -L                                     
005800     SKIP3                                                                
005900 FD  W55115                                                               
006000     RECORDING F                                                          
006100     BLOCK CONTAINS 0.                                                    
006200*01  W55115-POST  -COPY W55159     -L                                     
006300     SKIP3                                                                
006400 FD  W55116                                                               
006500     RECORDING F                                                          
006600     BLOCK CONTAINS 0.                                                    
006700*01  W55116-POST  -COPY W55101     -L                                     
006800     EJECT                                                                
006900 SD  SORTFIL.                                                             
007000*01  SORTPOST  -COPY W55159   -PRE S-                                     
007100     EJECT                                                                
007200 WORKING-STORAGE SECTION.                                                 
007300                                                                          
007400                                                                          
007500*    -- CHECKED BY WY2000                                                 
007600 77  IDPGM                   PIC X(8)   VALUE 'W5511200'.                 
007700 77  JA                      PIC X       VALUE 'J'.                       
007800 77  NEJ                     PIC X       VALUE 'N'.                       
007900 77  IX1                     PIC S9(5)   VALUE +0 COMP SYNC.              
008000 77  SORTFIL-SLUT            PIC X       VALUE 'N'.                       
008010 77  W-INFL                  PIC 9V9(3)  VALUE ZERO.                      
008020                                                                          
008200 01  ARBETSFALT.                                                          
008300     03  W-TEXT              PIC X(40) VALUE SPACE.                       
008400     03  W-SUM-PRDIRLON      PIC S9(4)V9(3)   COMP-3.                     
008500     03  W-SUM-PRDMTRL       PIC S9(6)V9(3)   COMP-3.                     
008600     03  W-SUM-PROVRPAL      PIC S9(4)V9(3)   COMP-3.                     
008700     03  W-ROUND-PRDIRLON    PIC S9(4)V9(2)   COMP-3.                     
008800     03  W-ROUND-PRDMTRL     PIC S9(6)V9(2)   COMP-3.                     
008900     03  W-ROUND-PROVRPAL    PIC S9(4)V9(2)   COMP-3.                     
009000     03  W1-SUM-PRDMTRL      PIC S9(6)V9(5)   COMP-3.                     
009100     03  W-PRDIRLON          PIC S9(4)V9(3)   COMP-3.                     
009200     03  W-PRDMTRL           PIC S9(6)V9(3)   COMP-3.                     
009300     03  W-PROVRPAL          PIC S9(4)V9(3)   COMP-3.                     
009400     03  W-FLFPTILL          PIC X.                                       
009500     03  W-KVQPACK-0         PIC S9(5)        COMP-3.                     
009600     03  W-KVQPACK-1         PIC S9(5)        COMP-3.                     
009700     03  W-KVQPACK-2         PIC S9(5)        COMP-3.                     
009800     03  W-KVQPACK-MIN       PIC S9(5)        COMP-3.                     
009900     03  W-KDFORP            PIC  9(4).                                   
010000     03  W-KDFORPUF          PIC S9V9(2)      COMP-3.                     
010100     03  W-KDFORPGP          PIC 9(2).                                    
010200     03  W-INDATA-OK         PIC X.                                       
010300     03  W-DETALJKOD         PIC X.                                       
010400         88 DETALJKOD-FINNS      VALUE 'J'.                               
010500     EJECT                                                                
010600*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
010700     SKIP3                                                                
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
011000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
011100   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
011200   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
011300     SKIP3                                                                
011400*    ----  PARAMETRAR TILL ABEND                                          
011500  01  RETURKODER.                                                         
011600    03  RKOD-ABEND-UTAN-DUMP PIC S9(04) COMP SYNC VALUE +16.              
011700     EJECT                                                                
011800* 01 -COPY W0005            -PRE POSTSUM-                                 
011900     EJECT                                                                
012000*    LÄSER IN VÄRDEN FRÅN WDR1 MED IDHTYP 5135                            
012100*    FRÅN BILD 5142                                                       
012200                                                                          
012300 01  TABELL-1.                                                            
012400     03 TAB1-KOSTNAD-ARTIKEL  OCCURS 1500                                 
012500        ASCENDING KEY IS TAB1-IDARTNR                                     
012600        INDEXED BY INDX-1.                                                
012700        05  TAB1-IDARTNR     PIC S9(09)       COMP-3.                     
012800        05  TAB1-PRDIRLON    PIC S9(04)V9(03) COMP-3.                     
012900        05  TAB1-PROVRPAL    PIC S9(04)V9(03) COMP-3.                     
013000        05  TAB1-PRDMTRL     PIC S9(06)V9(03) COMP-3.                     
013100        05  TAB1-FLFPTILL    PIC X(01).                                   
013200     SKIP3                                                                
013300*    LÄSER IN VÄRDEN FRÅN WDR1 MED IDHTYP 5133                            
013400*    FRÅN BILD 5143                                                       
013500                                                                          
013600 01  TABELL-2.                                                            
013700     03 TAB2-KOSTNAD-DETALJKOD  OCCURS 500                                
013800        ASCENDING KEY IS TAB2-IDARTNR-EMB                                 
013900        INDEXED BY INDX-2.                                                
014000        05  TAB2-IDARTNR-EMB PIC S9(09)       COMP-3.                     
014100        05  TAB2-PRDMTRL     PIC S9(06)V9(03) COMP-3.                     
014200     EJECT                                                                
014300*    LÄSER IN VÄRDEN FRÅN WDR1 MED IDHTYP 5137                            
014400*    FRÅN BILD 5144                                                       
014500                                                                          
014600 01  TABELL-3.                                                            
014700     03 TAB3-KOSTNAD-FORPGRP  OCCURS 100                                  
014800        ASCENDING KEY IS TAB3-KDFORPGP                                    
014900        INDEXED BY INDX-3.                                                
015000        05  TAB3-KDFORPGP    PIC  9(2).                                   
015100        05  TAB3-PRDIRLON    PIC S9(4)V9(3)   COMP-3.                     
015200        05  TAB3-PROVRPAL    PIC S9(4)V9(3)   COMP-3.                     
015300        05  TAB3-PRDMTRL     PIC S9(6)V9(3)   COMP-3.                     
015400     EJECT                                                                
015500*    ----  VÄRDE FÖR UPPRÄKNINGSFAKTOR                                    
015600     SKIP2                                                                
015700*01  -COPY WWKPALAG                                                       
015800     EJECT                                                                
015900*    ----  AREA UTPOST TILL EKONOMISYSTEM                                 
016000     SKIP2                                                                
016100*01  UTPOST -COPY W55124      -PRE UT-R24-                                
016200     EJECT                                                                
016300*    ----  AREA UTPOST  INFO R24                                          
016400     SKIP2                                                                
016500*01  UTPOST -COPY W55159      -PRE UT-                                    
016600     EJECT                                                                
016700*    ----  AREA UTPOST FEL FEL FEL                                        
016800     SKIP2                                                                
016900*01  UTPOST -COPY W55101      -PRE UT-FEL-                                
017000     EJECT                                                                
017700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
017800                                                                          
017900 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
018000     SKIP3                                                                
018100*    ---- STATUSKOD FRÅN IMS                                              
018200                                                                          
018300 01  STATUS-WS               PIC XX.                                      
018400     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
018500     88  SEGMENT-FINNS                    VALUE '  '.                     
018600     SKIP3                                                                
018700 01  GODK-STATUSKODER.                                                    
018800   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
018900     SKIP3                                                                
019000 01  SSA1                    PIC X(64).                                   
019100 01  SSA2                    PIC X(64).                                   
019200 01  SSA3                    PIC X(64).                                   
019300     EJECT                                                                
019400*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
019500 01  FILLER                  PIC X(16)   VALUE 'NYCKLAR-TILL-DLI'.        
019600     SKIP2                                                                
019700 01  NYCKLAR-TILL-DLI.                                                    
019800                                                                          
019900   03  W-IDARTNR-X.                                                       
020000     05  W-IDARTNR           PIC S9(9)  COMP-3.                           
020100                                                                          
020200   03  W-WDGX-5137-KEY-X.                                                 
020300     05  W-IDHTYP-5137       PIC X(4)         VALUE '5137'.               
020400     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
020500                                                                          
020600   03  W-WDGX-5133-KEY-X.                                                 
020700     05  W-IDHTYP-5133       PIC X(4)         VALUE '5133'.               
020800     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
020900                                                                          
021000   03  W-WDGX-5135-KEY-X.                                                 
021100     05  W-IDHTYP-5135       PIC X(4)         VALUE '5135'.               
021200     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
021201                                                                          
021230   03  W-WDGX-5107-KEY-X.                                                 
021240     05  W-IDHTYP-5107       PIC X(4)         VALUE '5107'.               
021250     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
021260                                                                          
021270   03  W-IDFTG-KEY-X.                                                     
021280     05  W-IDFTG             PIC 9(2)         VALUE 57.                   
021300     EJECT                                                                
021400*    -COPY W0003                                                          
021500     EJECT                                                                
021600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
021700 01  DLI-IO-AREA.                                                         
021800   03  IO-AREA               PIC X(900).                                  
021900                                                                          
022000*  03  WDK611   -COPY WDK611            -RED IO-AREA.                     
022100     SKIP3                                                                
022200*  03  WDGX5138 -COPY WDGX5138          -RED IO-AREA.                     
022300     SKIP3                                                                
022400*  03  WDGX5134 -COPY WDGX5134          -RED IO-AREA.                     
022500     SKIP3                                                                
022600*  03  WDGX5136 -COPY WDGX5136          -RED IO-AREA.                     
022610     SKIP3                                                                
022620*  03  WDGX5108 -COPY WDGX5108          -RED IO-AREA.                     
022700     EJECT                                                                
022800 LINKAGE SECTION.                                                         
022900                                                                          
023000*01  -COPY W0008      -PRE  ARTC-                                         
023100       05  FILLER                PIC X.                                   
023200                                                                          
023300*01  -COPY W0008      -PRE  5133-                                         
023400       05  FILLER                PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008      -PRE  5135-                                         
023700       05  FILLER                PIC X.                                   
023800                                                                          
023900*01  -COPY W0008      -PRE  5137-                                         
024000       05  FILLER                PIC X.                                   
024010                                                                          
024020*01  -COPY W0008      -PRE  5108-                                         
024030       05  FILLER                PIC X.                                   
024100     EJECT                                                                
024200 PROCEDURE DIVISION  USING ARTC-PCB 5133-PCB 5135-PCB 5137-PCB            
024210                           5108-PCB.                                      
024300     ENTRY 'DLITCBL' USING ARTC-PCB 5133-PCB 5135-PCB 5137-PCB            
024310                           5108-PCB.                                      
024400                                                                          
024500     PERFORM A-INIT                                                       
024600                                                                          
024700     SORT SORTFIL ASCENDING KEY S-BEFT                                    
024800                                S-IDLEVNR                                 
024900          USING INFIL                                                     
025000                                                                          
025100          OUTPUT PROCEDURE C-BEHANDLING                                   
025200                                                                          
025300     IF SORT-RETURN > 0                                                   
025400        DISPLAY '*** W5511200 FEL VID SORTERING ***'                      
025500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
025600     ELSE                                                                 
025700        PERFORM Z-FINIT                                                   
025800        MOVE ZERO TO RETURN-CODE                                          
025900        GOBACK                                                            
026000     END-IF                                                               
026100     .                                                                    
026200     SKIP3                                                                
026300 A-INIT SECTION.                                                          
026400                                                                          
026500     OPEN OUTPUT W55114 W55115 W55116                                     
026600                                                                          
026700     .                                                                    
026800     EJECT                                                                
026900 C-BEHANDLING SECTION.                                                    
027000                                                                          
027100* GET THE INFLATION FACTOR FROM WDR2(WDGX5108)                            
027110     PERFORM IMS-GU-WDGX5108                                              
027120     IF SEGMENT-FINNS                                                     
027131       COMPUTE W-INFL = 1 + (5108-REEMBINF / 100)                         
027132       DISPLAY 'W-INFL:' W-INFL                                           
027140     ELSE                                                                 
027150       DISPLAY '*** W5511200 INFLATION FACTOR NOT FOUND ***'              
027160       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
027170     END-IF                                                               
027200                                                                          
027300     PERFORM CA-TAB-PRIS-FORPACKNINGSGRUPP                                
027400     PERFORM CB-TAB-EXTRA-KOSTNAD-ARTIKEL                                 
027500     PERFORM CC-TAB-KOSTNAD-DETALJKOD                                     
027600                                                                          
027700     PERFORM S03-SORT-RETURN                                              
027800                                                                          
027900     PERFORM UNTIL SORTFIL-SLUT = JA                                      
028000                                                                          
028100       MOVE JA                    TO W-INDATA-OK                          
028200       MOVE S-IDARTNR             TO W-IDARTNR                            
028300       PERFORM IMS-GU-ARTC11                                              
028400       IF SEGMENT-FINNS                                                   
028500         MOVE CLAG-KVQPACK-0      TO S-KVQPACK-0                          
028600         MOVE CLAG-KVQPACK-1      TO S-KVQPACK-1                          
028700         MOVE CLAG-KVQPACK-2      TO S-KVQPACK-2                          
028800         MOVE CLAG-IDARTNR-EMBQ0  TO S-IDARTNR-EMBQ0                      
028900         MOVE CLAG-IDARTNR-EMBQ1  TO S-IDARTNR-EMBQ1                      
029000         MOVE CLAG-IDARTNR-EMBQ2  TO S-IDARTNR-EMBQ2                      
029100         MOVE CLAG-KDFORP         TO S-KDFORP                             
029200       END-IF                                                             
029300                                                                          
029400       PERFORM CD-SOK-FORPACKNGINSGRP                                     
029500       PERFORM CE-UPPRAKNINGSFAKTOR                                       
029600                                                                          
029700       IF W-INDATA-OK = JA                                                
029800          MOVE +0 TO W-SUM-PRDIRLON W-SUM-PROVRPAL                        
029900                     W-SUM-PRDMTRL  W1-SUM-PRDMTRL                        
030000                                                                          
030100          PERFORM CF-EXTRA-KOSTNAD-ARTIKEL                                
030200                                                                          
030300          PERFORM CG-KOSTNAD-FORPACKNINGSGRP                              
030400                                                                          
030500          IF W-INDATA-OK = JA                                             
030600                                                                          
030700            IF S-BEFT = 30 OR 31                                          
030800              MOVE ZERO           TO W-SUM-PRDMTRL                        
030900            END-IF                                                        
031000                                                                          
031100            IF S-BEFT = 23 OR 28                                          
031110              MOVE ZERO           TO W-SUM-PRDMTRL                        
031120            END-IF                                                        
031130                                                                          
031200            IF S-BEFT = 70 OR 71 OR 72 OR 73 OR 74 OR 75 OR 76 OR         
031300                        77 OR 78 OR 79                                    
031400              IF S-PRDIRLON = 0 AND S-PROVRPAL = 0                        
031500                MOVE S-PRDMTRL    TO W-SUM-PRDMTRL                        
031600              ELSE                                                        
031700                MOVE +0           TO W-SUM-PRDMTRL                        
031800              END-IF                                                      
031900              MOVE +0             TO W-SUM-PRDIRLON                       
032000                                     W-SUM-PROVRPAL                       
032100            END-IF                                                        
032200                                                                          
032300            IF W-SUM-PRDIRLON > +0 OR                                     
032400               W-SUM-PRDMTRL  > +0 OR                                     
032500               W-SUM-PROVRPAL > +0                                        
032600              PERFORM CI-SKRIV-UTFILER                                    
032700            END-IF                                                        
032800          END-IF                                                          
032900       END-IF                                                             
033000       PERFORM S03-SORT-RETURN                                            
033100                                                                          
033200     END-PERFORM                                                          
033300     .                                                                    
033400     EJECT                                                                
033500 CA-TAB-PRIS-FORPACKNINGSGRUPP SECTION.                                   
033600                                                                          
033700     PERFORM IMS-GU-513701                                                
033800                                                                          
033900     PERFORM IMS-GNP-513711                                               
034000                                                                          
034100     MOVE +0                TO IX1                                        
034200     PERFORM UNTIL SEGMENT-SAKNAS                                         
034300       ADD +1               TO IX1                                        
034400       MOVE 5138-KDFORPGP   TO TAB3-KDFORPGP (IX1)                        
034500       MOVE 5138-PRDIRLON   TO TAB3-PRDIRLON (IX1)                        
034600       MOVE 5138-PROVRPAL   TO TAB3-PROVRPAL (IX1)                        
034700       MOVE 5138-PRDMTRL    TO TAB3-PRDMTRL  (IX1)                        
034800                                                                          
034900       PERFORM IMS-GNP-513711                                             
035000     END-PERFORM                                                          
035100                                                                          
035200     PERFORM UNTIL IX1 > +99                                              
035300       ADD +1               TO IX1                                        
035400       MOVE 99              TO TAB3-KDFORPGP (IX1)                        
035500       MOVE +0              TO TAB3-PRDIRLON (IX1)                        
035600       MOVE +0              TO TAB3-PROVRPAL (IX1)                        
035700       MOVE +0              TO TAB3-PRDMTRL  (IX1)                        
035800     END-PERFORM                                                          
035900     .                                                                    
036000     EJECT                                                                
036100 CB-TAB-EXTRA-KOSTNAD-ARTIKEL SECTION.                                    
036200                                                                          
036300     PERFORM IMS-GU-513501                                                
036400                                                                          
036500     PERFORM IMS-GNP-513511                                               
036600                                                                          
036700     MOVE +0                TO IX1                                        
036800     PERFORM UNTIL SEGMENT-SAKNAS                                         
036900       ADD +1               TO IX1                                        
037000       IF IX1 > +1500                                                     
037100          DISPLAY '*** W5511200 FEL TABELL-1 SPRÄNGD ***'                 
037200          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
037300       END-IF                                                             
037400       MOVE 5136-IDARTNR    TO TAB1-IDARTNR  (IX1)                        
037500       MOVE 5136-PRDIRLON   TO TAB1-PRDIRLON (IX1)                        
037600       MOVE 5136-PROVRPAL   TO TAB1-PROVRPAL (IX1)                        
037700       MOVE 5136-PRDMTRL    TO TAB1-PRDMTRL  (IX1)                        
037800       MOVE 5136-FLFPTILL   TO TAB1-FLFPTILL (IX1)                        
037900                                                                          
038000        PERFORM IMS-GNP-513511                                            
038100     END-PERFORM                                                          
038200                                                                          
038300     PERFORM UNTIL IX1 = +1500                                            
038400       ADD +1               TO IX1                                        
038500       MOVE +999999999      TO TAB1-IDARTNR  (IX1)                        
038600       MOVE +0              TO TAB1-PRDIRLON (IX1)                        
038700       MOVE +0              TO TAB1-PROVRPAL (IX1)                        
038800       MOVE +0              TO TAB1-PRDMTRL  (IX1)                        
038900       MOVE SPACE           TO TAB1-FLFPTILL (IX1)                        
039000     END-PERFORM                                                          
039100     .                                                                    
039200     EJECT                                                                
039300 CC-TAB-KOSTNAD-DETALJKOD SECTION.                                        
039400                                                                          
039500     PERFORM IMS-GU-513301                                                
039600                                                                          
039700     PERFORM IMS-GNP-513311                                               
039800                                                                          
039900     MOVE +0                 TO IX1                                       
040000     PERFORM UNTIL SEGMENT-SAKNAS                                         
040100       ADD +1                TO IX1                                       
040200       IF IX1 > +500                                                      
040300          DISPLAY '*** W5511200 FEL TABELL-2 SPRÄNGD ***'                 
040400          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
040500       END-IF                                                             
040600       MOVE 5134-IDARTNR-EMB TO TAB2-IDARTNR-EMB (IX1)                    
040700       MOVE 5134-PRDMTRL     TO TAB2-PRDMTRL     (IX1)                    
040800                                                                          
040900        PERFORM IMS-GNP-513311                                            
041000     END-PERFORM                                                          
041100                                                                          
041200     PERFORM UNTIL IX1 = +500                                             
041300       ADD +1                TO IX1                                       
041400       MOVE +999999999       TO TAB2-IDARTNR-EMB (IX1)                    
041500       MOVE +0               TO TAB2-PRDMTRL     (IX1)                    
041600     END-PERFORM                                                          
041700     .                                                                    
041800     EJECT                                                                
041900 CD-SOK-FORPACKNGINSGRP SECTION.                                          
042000                                                                          
042100     MOVE S-KDFORPGP                 TO W-KDFORPGP                        
042200                                                                          
042300     SEARCH ALL TAB3-KOSTNAD-FORPGRP                                      
042400       AT END                                                             
042500          MOVE NEJ                      TO W-INDATA-OK                    
042600          MOVE 'FÖRPACKNINGSKOD NOLL'   TO W-TEXT                         
042700          PERFORM S02-SKRIV-FELFIL                                        
042800       WHEN TAB3-KDFORPGP (INDX-3) = W-KDFORPGP                           
042900         MOVE TAB3-PRDIRLON (INDX-3)    TO W-PRDIRLON                     
043000         MOVE TAB3-PROVRPAL (INDX-3)    TO W-PROVRPAL                     
043100         MOVE TAB3-PRDMTRL  (INDX-3)    TO W-PRDMTRL                      
043200     END-SEARCH                                                           
043300     .                                                                    
043400     EJECT                                                                
043500 CE-UPPRAKNINGSFAKTOR SECTION.                                            
043600                                                                          
043700     IF S-KDFORPUF = 1                                                    
043800        MOVE KDFORPUF-1           TO W-KDFORPUF                           
043900     ELSE                                                                 
044000       IF S-KDFORPUF = 2                                                  
044100          MOVE KDFORPUF-2         TO W-KDFORPUF                           
044200       ELSE                                                               
044300         IF S-KDFORPUF = 3                                                
044400            MOVE KDFORPUF-3       TO W-KDFORPUF                           
044500         ELSE                                                             
044600           IF S-KDFORPUF = 4                                              
044700              MOVE KDFORPUF-4     TO W-KDFORPUF                           
044800           ELSE                                                           
044900              IF S-KDFORPUF = 5                                           
045000                 MOVE KDFORPUF-5  TO W-KDFORPUF                           
045100              ELSE                                                        
045200                 MOVE NEJ         TO W-INDATA-OK                          
045300                 MOVE 'FELAKTIG UPPRÄKNINGSFAKTOR' TO W-TEXT              
045400                 PERFORM S02-SKRIV-FELFIL                                 
045500                 DISPLAY 'ART ' S-IDARTNR ' FORP ' S-KDFORP               
045600              END-IF                                                      
045700           END-IF                                                         
045800         END-IF                                                           
045900       END-IF                                                             
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 CF-EXTRA-KOSTNAD-ARTIKEL SECTION.                                        
046400                                                                          
046500     MOVE JA TO W-FLFPTILL                                                
046600                                                                          
046700     SEARCH ALL TAB1-KOSTNAD-ARTIKEL                                      
046800       AT END CONTINUE                                                    
046900       WHEN TAB1-IDARTNR (INDX-1) = S-IDARTNR                             
047000         MOVE TAB1-PRDIRLON (INDX-1)    TO W-SUM-PRDIRLON                 
047100         MOVE TAB1-PROVRPAL (INDX-1)    TO W-SUM-PROVRPAL                 
047200         MOVE TAB1-PRDMTRL  (INDX-1)    TO W1-SUM-PRDMTRL                 
047300         MOVE TAB1-FLFPTILL (INDX-1)    TO W-FLFPTILL                     
047400     END-SEARCH                                                           
047500     .                                                                    
047600     EJECT                                                                
047700 CG-KOSTNAD-FORPACKNINGSGRP SECTION.                                      
047800                                                                          
047900     IF W-FLFPTILL = JA                                                   
048000        MOVE +1                  TO W-KVQPACK-MIN                         
048100                                                                          
048200       IF S-KVQPACK-0 > +1                                                
048300       OR S-KVQPACK-1 > +1                                                
048400       OR S-KVQPACK-2 > +1                                                
048500          IF S-KVQPACK-1 > +0                                             
048600             MOVE S-KVQPACK-1    TO W-KVQPACK-MIN                         
048700          END-IF                                                          
048800          IF  S-KVQPACK-0 > +0                                            
048900          AND S-KVQPACK-0 < W-KVQPACK-MIN                                 
049000              MOVE S-KVQPACK-0   TO W-KVQPACK-MIN                         
049100          END-IF                                                          
049200          IF  S-KVQPACK-2 > +0                                            
049300          AND S-KVQPACK-2 < W-KVQPACK-MIN                                 
049400              MOVE S-KVQPACK-2   TO W-KVQPACK-MIN                         
049500           END-IF                                                         
049600       END-IF                                                             
049700                                                                          
049800       COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                          
049900                   (W-PRDMTRL / W-KVQPACK-MIN)                            
050000       COMPUTE W-SUM-PRDIRLON ROUNDED = W-SUM-PRDIRLON +                  
050100                   (W-PRDIRLON * W-KDFORPUF)                              
050200       COMPUTE W-SUM-PROVRPAL ROUNDED = W-SUM-PROVRPAL +                  
050300                   (W-PROVRPAL * W-KDFORPUF)                              
050400     END-IF                                                               
050500                                                                          
050600     PERFORM CGA-PRIS-EMBALLAGE                                           
050700                                                                          
050800     IF W1-SUM-PRDMTRL > +0                                               
050900        COMPUTE W-SUM-PRDMTRL ROUNDED = W1-SUM-PRDMTRL * W-INFL           
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 CGA-PRIS-EMBALLAGE SECTION.                                              
051400                                                                          
051500     IF S-KVQPACK-0 = +0                                                  
051600        MOVE +1             TO W-KVQPACK-0                                
051700     ELSE                                                                 
051800        MOVE S-KVQPACK-0    TO W-KVQPACK-0                                
051900     END-IF                                                               
052000     IF S-KVQPACK-1 = +0                                                  
052100        MOVE +1             TO W-KVQPACK-1                                
052200     ELSE                                                                 
052300        MOVE S-KVQPACK-1    TO W-KVQPACK-1                                
052400     END-IF                                                               
052500     IF S-KVQPACK-2 = +0                                                  
052600        MOVE +1             TO W-KVQPACK-2                                
052700     ELSE                                                                 
052800        MOVE S-KVQPACK-2    TO W-KVQPACK-2                                
052900     END-IF                                                               
053000                                                                          
053100     IF S-IDARTNR-EMBQ0 > +0                                              
053200        MOVE S-IDARTNR-EMBQ0 TO W-IDARTNR                                 
053300        PERFORM S01-KOSTNAD-DETALJKOD                                     
053400        IF DETALJKOD-FINNS                                                
053500           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
053600                           (W-PRDMTRL / W-KVQPACK-0)                      
053700        ELSE                                                              
053800          PERFORM IMS-GU-ARTC11                                           
053900          IF SEGMENT-FINNS                                                
054000             COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                    
054100                     (CLAG-PRARTSTD / W-KVQPACK-0)                        
054200          ELSE                                                            
054300             MOVE NEJ               TO W-INDATA-OK                        
054400             MOVE 'EMB.ART.NR FÖR Q0 SAKNAS'    TO W-TEXT                 
054500             PERFORM S02-SKRIV-FELFIL                                     
054600          END-IF                                                          
054700        END-IF                                                            
054800     END-IF                                                               
054900                                                                          
055000     IF S-IDARTNR-EMBQ1 > +0                                              
055100        MOVE S-IDARTNR-EMBQ1 TO W-IDARTNR                                 
055200        PERFORM S01-KOSTNAD-DETALJKOD                                     
055300        IF DETALJKOD-FINNS                                                
055400           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
055500                            (W-PRDMTRL / W-KVQPACK-1)                     
055600        ELSE                                                              
055700          PERFORM IMS-GU-ARTC11                                           
055800          IF SEGMENT-FINNS                                                
055900             COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                    
056000                     (CLAG-PRARTSTD / W-KVQPACK-1)                        
056100          ELSE                                                            
056200             MOVE NEJ               TO W-INDATA-OK                        
056300             MOVE 'EMB.ART.NR FÖR Q1 SAKNAS' TO W-TEXT                    
056400             PERFORM S02-SKRIV-FELFIL                                     
056500          END-IF                                                          
056600        END-IF                                                            
056700     END-IF                                                               
056800                                                                          
056900     IF S-IDARTNR-EMBQ2 > +0                                              
057000        MOVE S-IDARTNR-EMBQ2 TO W-IDARTNR                                 
057100        PERFORM S01-KOSTNAD-DETALJKOD                                     
057200        IF DETALJKOD-FINNS                                                
057300           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
057400                            (W-PRDMTRL / W-KVQPACK-2)                     
057500        ELSE                                                              
057600          PERFORM IMS-GU-ARTC11                                           
057700          IF SEGMENT-FINNS                                                
057800             COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                    
057900                          (CLAG-PRARTSTD / W-KVQPACK-2)                   
058000          ELSE                                                            
058100             MOVE NEJ               TO W-INDATA-OK                        
058200             MOVE 'EMB.ART.NR FÖR Q2 SAKNAS' TO W-TEXT                    
058300             PERFORM S02-SKRIV-FELFIL                                     
058400          END-IF                                                          
058500        END-IF                                                            
058600     END-IF                                                               
058700     .                                                                    
058800     EJECT                                                                
058900 CI-SKRIV-UTFILER SECTION.                                                
059000                                                                          
059100     MOVE S-IDARTNR           TO UT-R24-IDARTNR                           
059200     MOVE S-KDVTH             TO UT-R24-KDVTH                             
059300                                                                          
059400***   RUNDA AV ALLA PÅLÄGG TILL 2 DECIMALER   *****                       
059500     IF W-SUM-PRDIRLON < +0.01 AND > +0.00000                             
059600       MOVE +0.01             TO W-ROUND-PRDIRLON                         
059700     ELSE                                                                 
059800       COMPUTE W-ROUND-PRDIRLON ROUNDED = W-SUM-PRDIRLON                  
059900     END-IF                                                               
060000                                                                          
060100     IF W-SUM-PRDMTRL < +0.01 AND > +0.00000                              
060200       MOVE +0.01             TO W-ROUND-PRDMTRL                          
060300     ELSE                                                                 
060400       COMPUTE W-ROUND-PRDMTRL ROUNDED = W-SUM-PRDMTRL                    
060500     END-IF                                                               
060600                                                                          
060700     IF W-SUM-PROVRPAL < +0.01 AND > +0.00000                             
060800       MOVE +0.01             TO W-ROUND-PROVRPAL                         
060900     ELSE                                                                 
061000       COMPUTE W-ROUND-PROVRPAL ROUNDED = W-SUM-PROVRPAL                  
061100     END-IF                                                               
061200                                                                          
061300     MOVE W-ROUND-PRDIRLON    TO UT-R24-PRDIRLON                          
061400     MOVE W-ROUND-PRDMTRL     TO UT-R24-PRDMTRL                           
061500     MOVE W-ROUND-PROVRPAL    TO UT-R24-PROVRPAL                          
061600                                                                          
061700     IF UT-R24-PRDIRLON  > +0 OR                                          
061800        UT-R24-PRDMTRL   > +0 OR                                          
061900        UT-R24-PROVRPAL  > +0                                             
062000                                                                          
062100*** SKRIV R24-POSTER FÖR ARTIKELREG. UPPDATERINGAR                        
062200       WRITE W55114-POST       FROM UT-R24-UTPOST                         
062300                                                                          
062400       MOVE ' R24'            TO POSTSUM-TRANSTYP                         
062500       MOVE 'W55114'          TO POSTSUM-FDNAMN                           
062600       MOVE 'W55112D2'        TO POSTSUM-DDNAMN2                          
062700       CALL POSTSUM USING POSTSUM-PARM                                    
062800                                                                          
062900*** SKRIV UT-POSTER FÖR KONTROLLISTORNA                                   
063000       MOVE S-W55159          TO UT-UTPOST                                
063100       MOVE W-ROUND-PRDIRLON  TO UT-PRDIRLON                              
063200       MOVE W-ROUND-PRDMTRL   TO UT-PRDMTRL                               
063300       MOVE W-ROUND-PROVRPAL  TO UT-PROVRPAL                              
063400       MOVE S-KDVTH           TO UT-KDVTH                                 
063500       WRITE W55115-POST  FROM UT-UTPOST                                  
063600                                                                          
063700       MOVE 'LIST'            TO POSTSUM-TRANSTYP                         
063800       MOVE 'W55115'          TO POSTSUM-FDNAMN                           
063900       MOVE 'W55112D3'        TO POSTSUM-DDNAMN2                          
064000       CALL POSTSUM USING POSTSUM-PARM                                    
064100     END-IF                                                               
064200     .                                                                    
064300     SKIP2                                                                
064400 Z-FINIT SECTION.                                                         
064500                                                                          
064600     CLOSE W55114                                                         
064700           W55115 W55116                                                  
064800                                                                          
064900     MOVE 'S' TO POSTSUM-OPKOD                                            
065000     CALL POSTSUM USING POSTSUM-PARM                                      
065100     .                                                                    
065200     EJECT                                                                
065300 S01-KOSTNAD-DETALJKOD SECTION.                                           
065400                                                                          
065500     SEARCH ALL TAB2-KOSTNAD-DETALJKOD                                    
065600       AT END                                                             
065700          MOVE NEJ TO W-DETALJKOD                                         
065800       WHEN TAB2-IDARTNR-EMB(INDX-2) = W-IDARTNR                          
065900          MOVE TAB2-PRDMTRL (INDX-2) TO W-PRDMTRL                         
066000          MOVE JA  TO W-DETALJKOD                                         
066100     END-SEARCH                                                           
066200     .                                                                    
066300     SKIP3                                                                
066400 S03-SORT-RETURN SECTION.                                                 
066500                                                                          
066600     RETURN SORTFIL                                                       
066700     AT END                                                               
066800       MOVE JA TO SORTFIL-SLUT                                            
066900     NOT AT END                                                           
067000       MOVE '    '              TO POSTSUM-TRANSTYP                       
067100       MOVE 'INFIL '            TO POSTSUM-FDNAMN                         
067200       MOVE 'W55112D1'          TO POSTSUM-DDNAMN2                        
067300       CALL POSTSUM USING POSTSUM-PARM                                    
067400     END-RETURN                                                           
067500     .                                                                    
067600     SKIP3                                                                
067700 S02-SKRIV-FELFIL SECTION.                                                
067800                                                                          
067900     MOVE S-KDGK              TO UT-FEL-KDGK                              
068000     MOVE S-IDARTNR           TO UT-FEL-IDARTNR                           
068100     MOVE W-TEXT              TO UT-FEL-TENOTE                            
068200     WRITE W55116-POST FROM UT-FEL-UTPOST                                 
068300     MOVE SPACE               TO W-TEXT                                   
068400                                                                          
068500     MOVE '    '              TO POSTSUM-TRANSTYP                         
068600     MOVE 'W55116'            TO POSTSUM-FDNAMN                           
068700     MOVE 'W55112D4'          TO POSTSUM-DDNAMN2                          
068800     CALL POSTSUM USING POSTSUM-PARM                                      
068900     .                                                                    
069000     EJECT                                                                
069100*    ---- IMS SEKTIONER                                                   
069200                                                                          
069300 IMS-GU-ARTC11 SECTION.                                                   
069400                                                                          
069500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
069600            DELIMITED BY SIZE INTO SSA1                                   
069700     MOVE 'WLARTC11 '           TO SSA2                                   
069800     MOVE '  GE'                TO GODK-STATUSKODER                       
069900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
070000     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     .                                                                    
070300     SKIP3                                                                
070400 IMS-GU-513301 SECTION.                                                   
070500                                                                          
070600     STRING 'WL513301(WDGXKEY  =' W-WDGX-5133-KEY-X ')'                   
070700            DELIMITED BY SIZE INTO SSA1                                   
070800     MOVE '  '                  TO GODK-STATUSKODER                       
070900     CALL CBLTDLI USING GU  5133-PCB DLI-IO-AREA SSA1                     
071000     MOVE 5133-STATUS-CODE      TO STATUS-WS                              
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     .                                                                    
071300     SKIP2                                                                
071400 IMS-GNP-513311 SECTION.                                                  
071500                                                                          
071600     MOVE 'WL513311'            TO SSA1                                   
071700     MOVE '  GE'                TO GODK-STATUSKODER                       
071800     CALL CBLTDLI USING GNP 5133-PCB DLI-IO-AREA SSA1                     
071900     MOVE 5133-STATUS-CODE      TO STATUS-WS                              
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     EJECT                                                                
072300 IMS-GU-513501 SECTION.                                                   
072400                                                                          
072500     STRING 'WL513501(WDGXKEY  =' W-WDGX-5135-KEY-X ')'                   
072600            DELIMITED BY SIZE INTO SSA1                                   
072700     MOVE '  '                  TO GODK-STATUSKODER                       
072800     CALL CBLTDLI USING GU  5135-PCB DLI-IO-AREA SSA1                     
072900     MOVE 5135-STATUS-CODE      TO STATUS-WS                              
073000     PERFORM IMS-STATUSKONTROLL                                           
073100     .                                                                    
073200     SKIP3                                                                
073300 IMS-GNP-513511 SECTION.                                                  
073400                                                                          
073500     MOVE 'WL513511'            TO SSA1                                   
073600     MOVE '  GE'                TO GODK-STATUSKODER                       
073700     CALL CBLTDLI USING GNP 5135-PCB DLI-IO-AREA SSA1                     
073800     MOVE 5135-STATUS-CODE      TO STATUS-WS                              
073900     PERFORM IMS-STATUSKONTROLL                                           
074000     .                                                                    
074100     EJECT                                                                
074200 IMS-GU-513701 SECTION.                                                   
074300                                                                          
074400     STRING 'WL513701(WDGXKEY  =' W-WDGX-5137-KEY-X ')'                   
074500            DELIMITED BY SIZE INTO SSA1                                   
074600     MOVE '  '                  TO GODK-STATUSKODER                       
074700     CALL CBLTDLI USING GU 5137-PCB DLI-IO-AREA SSA1                      
074800     MOVE 5137-STATUS-CODE      TO STATUS-WS                              
074900     PERFORM IMS-STATUSKONTROLL                                           
075000     .                                                                    
075100     SKIP2                                                                
075200 IMS-GNP-513711 SECTION.                                                  
075300                                                                          
075400     MOVE 'WL513711'            TO SSA1                                   
075500     MOVE '  GE'                TO GODK-STATUSKODER                       
075600     CALL CBLTDLI USING GNP 5137-PCB DLI-IO-AREA  SSA1                    
075700     MOVE 5137-STATUS-CODE      TO STATUS-WS                              
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000     SKIP3                                                                
076091 IMS-GU-WDGX5108 SECTION.                                                 
076092                                                                          
076093     STRING 'WDR201  (WDGXKEY  =' W-WDGX-5107-KEY-X ')'                   
076094          DELIMITED BY SIZE INTO SSA1                                     
076095     STRING 'WDGX5108(IDFTG    =' W-IDFTG-KEY-X ')'                       
076096          DELIMITED BY SIZE INTO SSA2                                     
076097     MOVE '  '   TO GODK-STATUSKODER                                      
076098     CALL CBLTDLI USING GU 5108-PCB DLI-IO-AREA SSA1 SSA2                 
076099     MOVE 5108-STATUS-CODE TO STATUS-WS                                   
076100     PERFORM IMS-STATUSKONTROLL                                           
076101     .                                                                    
076102     EJECT                                                                
076110 IMS-STATUSKONTROLL SECTION.                                              
076200                                                                          
076300     SET STATUS-IX TO 1                                                   
076400     SEARCH GODK-STATUS                                                   
076500       AT END CALL FELLOG                                                 
076600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
076700         CONTINUE                                                         
076800     END-SEARCH                                                           
076900     .                                                                    
