000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W5510100.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           JULI 1989.                                       
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PRISSÄTNING AV EMBALLAGE                                         
001100*        PROGRAMMET BYTTE NAMN 23/5 1995 FRÅN W45240100 /KJH              
001200*                                                                         
001300*        INDATA. W55113 - EXTRAKT UR WDK6 (VIA SPIS OCH W55103)           
001500*                W55103 - FAKTOR FÖR SATSARTIKEL                          
001600*                                                                         
001700*        UTDATA. W55105 - FIL TILL EKONOMISYSTEMET SPIS (W551)            
001800*                         FÖR UPPDATERING AV WDK6.                        
001900*                W55106 - FIL FÖR UTSKRIFT AV LISTOR.                     
002000*                W55108   FELFIL                                          
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*                                                                         
002400*        U0016     - OM RETURKOD FRÅN SORT                                
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200     SELECT W55113  ASSIGN TO W55101D1.                                   
003400     SELECT W55103  ASSIGN TO W55101D2.                                   
003500     SELECT W55105  ASSIGN TO W55101D3.                                   
003600     SELECT W55106  ASSIGN TO W55101D4.                                   
003700     SELECT W55108  ASSIGN TO W55101D5.                                   
003800     SELECT SORTFIL ASSIGN TO W55101DS.                                   
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100                                                                          
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W55113                                                               
004500     RECORDING F                                                          
004600     BLOCK CONTAINS 0.                                                    
004700*01  W55113-POST  -COPY W55159   -L                                       
004800     SKIP3                                                                
005400 FD  W55103                                                               
005500     RECORDING F                                                          
005600     BLOCK CONTAINS 0.                                                    
005700*01  W55103-POST  -COPY W5510201 -L                                       
005800     EJECT                                                                
005900 FD  W55105                                                               
006000     RECORDING F                                                          
006100     BLOCK CONTAINS 0.                                                    
006200*01  W55105-POST  -COPY W55124  -L                                        
006300     SKIP3                                                                
006400 FD  W55106                                                               
006500     RECORDING F                                                          
006600     BLOCK CONTAINS 0.                                                    
006700*01  W55106-POST  -COPY W55159   -L                                       
006800     EJECT                                                                
006900 FD  W55108                                                               
007000     RECORDING F                                                          
007100     BLOCK CONTAINS 0.                                                    
007200*01  W55108-POST  -COPY W55101   -L                                       
007300     EJECT                                                                
007400 SD  SORTFIL.                                                             
007500*01  SORTPOST  -COPY W55159   -PRE S-                                     
007600     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
007800                                                                          
007900 77  IDPGM                   PIC X(8)   VALUE 'W5510100'.                 
008000 77  JA                      PIC X       VALUE 'J'.                       
008100 77  NEJ                     PIC X       VALUE 'N'.                       
008110 77  W-INFL                  PIC 9V9(3) VALUE ZERO.                       
008300*    ---- INDEXFÄLT                                                       
008400 77  IX1                     PIC S9(5)   VALUE +0 COMP SYNC.              
008500                                                                          
008600*    ---- END-OF-FILE SWITCHAR                                            
008700 77  INFIL-SLUT              PIC X       VALUE 'N'.                       
008800 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
008900                                                                          
009000*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
009100                                                                          
009200 01  DYNAMISKA-SUBPROGRAM.                                                
009300   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
009400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
009500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
009600   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
009700     SKIP3                                                                
009800*    ----  PARAMETRAR TILL ABEND                                          
009900  01  RETURKODER.                                                         
010000    03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE +16.               
010100     EJECT                                                                
010200* 01 -COPY W0005            -PRE POSTSUM-                                 
010300     EJECT                                                                
010400 01  FILLER                  PIC X(10) VALUE 'ARBETSFÄLT'.                
010500*    ---- ARBETSFÄLT                                                      
010600*                                                                         
010700 01  ARBETSAREA.                                                          
010800     03  W-TEXT              PIC X(40)  VALUE SPACE.                      
010900     03  W-SUM-PRDIRLON      PIC S9(4)V9(5) COMP-3.                       
011000     03  W-SUM-PRDMTRL       PIC S9(6)V9(5) COMP-3.                       
011100     03  W-SUM-PROVRPAL      PIC S9(4)V9(5) COMP-3.                       
011200     03  W-ROUND-PRDIRLON    PIC S9(4)V9(2) COMP-3.                       
011300     03  W-ROUND-PRDMTRL     PIC S9(6)V9(2) COMP-3.                       
011400     03  W-ROUND-PROVRPAL    PIC S9(4)V9(2) COMP-3.                       
011500     03  W1-SUM-PRDMTRL      PIC S9(6)V9(5) COMP-3.                       
011600     03  W-5132-PRDIRLON     PIC S9(4)V9(3) COMP-3  VALUE +0.             
011700     03  W-5132-PRDMTRL      PIC S9(6)V9(3) COMP-3  VALUE +0.             
011800     03  W-5132-PROVRPAL     PIC S9(4)V9(3) COMP-3  VALUE +0.             
011900     03  W-PRDMTRL           PIC S9(6)V9(3) COMP-3.                       
012000     03  W-FLFPTILL          PIC X.                                       
012100     03  K6-IDARTNR-EMBQ0    PIC S9(9)      COMP-3  VALUE +0.             
012200     03  K6-IDARTNR-EMBQ1    PIC S9(9)      COMP-3  VALUE +0.             
012300     03  K6-IDARTNR-EMBQ2    PIC S9(9)      COMP-3  VALUE +0.             
012400     03  K6-KVQPACK-0        PIC S9(5)      COMP-3  VALUE +0.             
012500     03  K6-KVQPACK-1        PIC S9(5)      COMP-3  VALUE +0.             
012600     03  K6-KVQPACK-2        PIC S9(5)      COMP-3  VALUE +0.             
012700     03  W-KVQPACK-0         PIC S9(5)      COMP-3.                       
012800     03  W-KVQPACK-1         PIC S9(5)      COMP-3.                       
012900     03  W-KVQPACK-2         PIC S9(5)      COMP-3.                       
013000     03  W-KVQPACK-MIN       PIC S9(5)      COMP-3  VALUE +0.             
013100     03  W-RESATSFP          PIC S9(2)V9(3) COMP-3.                       
013200     03  W-SKRIV-UTFIL       PIC X.                                       
013300 01  W-DETALJKOD             PIC X.                                       
013400     88 DETALJKOD-FINNS      VALUE 'J'.                                   
013500     EJECT                                                                
013600 01  FILLER                  PIC X(16) VALUE 'HTYP 5135 '.                
013700*    LÄSER IN VÄRDEN FRÅN WDR1 MED IDHTYP 5135 FRÅN BILD 5142             
013800                                                                          
013900 01  TABELL-1.                                                            
014000     03 TAB1-KOSTNAD-ARTIKEL  OCCURS 500                                  
014100        ASCENDING KEY IS TAB1-IDARTNR                                     
014200        INDEXED BY INDX-1.                                                
014300        05  TAB1-IDARTNR     PIC S9(9)         COMP-3.                    
014400        05  TAB1-PRDIRLON    PIC S9(4)V9(3)    COMP-3.                    
014500        05  TAB1-PROVRPAL    PIC S9(4)V9(3)    COMP-3.                    
014600        05  TAB1-PRDMTRL     PIC S9(6)V9(3)    COMP-3.                    
014700        05  TAB1-FLFPTILL    PIC X.                                       
014800     SKIP3                                                                
014900 01  FILLER                  PIC X(16) VALUE 'HTYP 5133 '.                
015000*    LÄSER IN VÄRDEN FRÅN WDR1 MED IDHTYP 5133 FRÅN BILD 5143             
015100                                                                          
015200 01  TABELL-2.                                                            
015300     03 TAB2-KOSTNAD-DETALJKOD  OCCURS 500                                
015400        ASCENDING KEY IS TAB2-IDARTNR-EMB                                 
015500        INDEXED BY INDX-2.                                                
015600        05  TAB2-IDARTNR-EMB PIC S9(9)        COMP-3.                     
015700        05  TAB2-PRDMTRL     PIC S9(6)V9(3)   COMP-3.                     
015800     EJECT                                                                
015900 01  FILLER                  PIC X(16) VALUE 'FAKTOR    '.                
016000*    ----  FAKTOR FÖR PRISSÄTTNING AV SATSARTIKEL                         
016100                                                                          
016200*01  POST -COPY W5510201    -PRE W-SATS-                                  
016300     SKIP3                                                                
016400 01  TABELL-3.                                                            
016500     03 TAB3-SATS-FAKTOR        OCCURS 3000                               
016600        ASCENDING KEY IS TAB3-IDARTNR                                     
016700        INDEXED BY INDX-3.                                                
016800        05  TAB3-IDARTNR     PIC S9(9)        COMP-3.                     
016900        05  TAB3-RESATSFP    PIC S9(2)V9(3)   COMP-3.                     
017000     EJECT                                                                
017100*    ----  AREA UTPOST TILL EKONOMISYSTEM                                 
017200     SKIP2                                                                
017300*01  UTPOST -COPY W55124     -PRE UT-R24-                                 
017400     EJECT                                                                
017500*    ----  AREA UTPOST  INFO R24                                          
017600     SKIP2                                                                
017700*01  UTPOST -COPY W55159      -PRE UT-                                    
017800     EJECT                                                                
017900*    ----  AREA UTPOST FEL FEL FEL                                        
018000     SKIP2                                                                
018100*01  UTPOST -COPY W55101      -PRE UT-FEL-                                
018200     EJECT                                                                
018900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
019000                                                                          
019100 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
019200     SKIP3                                                                
019300*    ---- STATUSKOD FRÅN IMS                                              
019400                                                                          
019500 01  STATUS-WS               PIC XX.                                      
019600     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
019700     88  SEGMENT-FINNS                    VALUE '  '.                     
019800     SKIP3                                                                
019900 01  GODK-STATUSKODER.                                                    
020000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
020100     SKIP3                                                                
020200 01  SSA1                    PIC X(164).                                  
020300 01  SSA2                    PIC X(164).                                  
020400     EJECT                                                                
020500*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
020600 01  FILLER                  PIC X(16)   VALUE 'NYCKLAR-TILL-DLI'.        
020700     SKIP2                                                                
020800 01  NYCKLAR-TILL-DLI.                                                    
020900   03  W-IDARTNR-X.                                                       
021000     05  W-IDARTNR           PIC S9(9) COMP-3.                            
021100                                                                          
021200   03  W-BEFT-X.                                                          
021300     05  W-BEFT              PIC S9(3)        VALUE +99  COMP-3.          
021400                                                                          
021500   03  W-WDGX-5131-KEY-X.                                                 
021600     05  W-IDHTYP-5131       PIC X(4)         VALUE '5131'.               
021700     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
021800                                                                          
021900   03  W-WDGX-5133-KEY-X.                                                 
022000     05  W-IDHTYP-5133       PIC X(4)         VALUE '5133'.               
022100     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
022200                                                                          
022300   03  W-WDGX-5135-KEY-X.                                                 
022400     05  W-IDHTYP-5135       PIC X(4)         VALUE '5135'.               
022500     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
022510                                                                          
022511   03  W-WDGX-5107-KEY-X.                                                 
022512     05  W-IDHTYP-5107       PIC X(4)         VALUE '5107'.               
022513     05  FILLER              PIC X(26)        VALUE LOW-VALUE.            
022514                                                                          
022520   03  W-IDFTG-KEY-X.                                                     
022530     05  W-IDFTG             PIC 9(2)         VALUE 57.                   
022600     EJECT                                                                
022700*    -COPY W0003                                                          
022800     EJECT                                                                
022900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA '.              
023000 01  DLI-IO-AREA.                                                         
023100   03  IO-AREA               PIC X(900).                                  
023200     SKIP3                                                                
023300*  03  WDK611   -COPY WDK611            -RED IO-AREA.                     
023400     SKIP3                                                                
023500*  03  WDGX5132 -COPY WDGX5132          -RED IO-AREA.                     
023600     SKIP3                                                                
023700*  03  WDGX5134 -COPY WDGX5134          -RED IO-AREA.                     
023800     SKIP3                                                                
023900*  03  WDGX5136 -COPY WDGX5136          -RED IO-AREA.                     
023910     SKIP3                                                                
023920*  03  WDGX5108 -COPY WDGX5108          -RED IO-AREA.                     
024000     EJECT                                                                
024100 LINKAGE SECTION.                                                         
024200                                                                          
024300*01  -COPY W0008      -PRE  ARTC-                                         
024400       05  FILLER                PIC X.                                   
024500                                                                          
024600*01  -COPY W0008      -PRE  5131-                                         
024700       05  FILLER                PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008      -PRE  5133-                                         
025000       05  FILLER                PIC X.                                   
025100                                                                          
025200*01  -COPY W0008      -PRE  5135-                                         
025300       05  FILLER                PIC X.                                   
025400     EJECT                                                                
025410*01  -COPY W0008      -PRE  5108-                                         
025420       05  FILLER                PIC X.                                   
025430     EJECT                                                                
025500 PROCEDURE DIVISION  USING ARTC-PCB 5131-PCB 5133-PCB 5135-PCB            
025510                           5108-PCB.                                      
025600     ENTRY 'DLITCBL' USING ARTC-PCB 5131-PCB 5133-PCB 5135-PCB            
025610                           5108-PCB.                                      
025700                                                                          
025800     PERFORM A-INIT                                                       
025900                                                                          
026000     SORT SORTFIL ASCENDING KEY S-BEFT                                    
026100                                S-IDLEVNR                                 
026200          USING W55113                                                    
026300                                                                          
026400          OUTPUT PROCEDURE C-BEHANDLING                                   
026500                                                                          
026600     IF SORT-RETURN > 0                                                   
026700        DISPLAY '*** W5510100 FEL VID SORTERING ***'                      
026800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
026900     ELSE                                                                 
027000        PERFORM Z-FINIT                                                   
027100        MOVE ZERO TO RETURN-CODE                                          
027200        GOBACK                                                            
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 A-INIT SECTION.                                                          
027700                                                                          
027800     OPEN INPUT W55103                                                    
027900         OUTPUT W55105 W55106 W55108                                      
028000                                                                          
028100     MOVE IDPGM         TO POSTSUM-PROGNAMN                               
028200     .                                                                    
028300     EJECT                                                                
028400 C-BEHANDLING  SECTION.                                                   
028500                                                                          
028710* GET THE INFLATION FACTOR FROM WDR2(WDGX5108)                            
028720     PERFORM IMS-GU-WDGX5108                                              
028730     IF SEGMENT-FINNS                                                     
028731       COMPUTE W-INFL = 1 + (5108-REEMBINF / 100)                         
028780     ELSE                                                                 
028793       DISPLAY '*** W5510100 INFLATION FACTOR NOT FOUND ***'              
028794       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
028795     END-IF                                                               
028796                                                                          
028800     PERFORM CA-TAB-SATS-FAKTOR                                           
028900     PERFORM CB-TAB-EXTRA-KOSTNAD-ARTIKEL                                 
029000     PERFORM CC-TAB-KOSTNAD-DETALJKOD                                     
029100                                                                          
029200     PERFORM S04-LAS-SORTPOST                                             
029300                                                                          
029400     PERFORM UNTIL SORTFIL-EOF = JA                                       
029500                                                                          
029600       IF S-BEFT NOT = W-BEFT                                             
029700         MOVE S-BEFT   TO W-BEFT                                          
029800         PERFORM IMS-GU-WL513111                                          
029900         IF SEGMENT-FINNS                                                 
030000            MOVE JA     TO W-SKRIV-UTFIL                                  
030100            MOVE 5132-PRDIRLON    TO W-5132-PRDIRLON                      
030200            MOVE 5132-PRDMTRL     TO W-5132-PRDMTRL                       
030300            MOVE 5132-PROVRPAL    TO W-5132-PROVRPAL                      
030400                                                                          
030500            MOVE +0 TO W-SUM-PRDIRLON W-SUM-PROVRPAL                      
030600                       W-SUM-PRDMTRL  W1-SUM-PRDMTRL                      
030700                                                                          
030800            PERFORM CF-HAMTA-EXTRA-KOSTN-ART                              
030900                                                                          
031000            PERFORM CG-KOSTNAD-FORPACKNINGSTYP                            
031100         ELSE                                                             
031200            MOVE NEJ               TO W-SKRIV-UTFIL                       
031300            MOVE 'FÖRPACKNINGSTYP SAKNAS' TO W-TEXT                       
031400         END-IF                                                           
031500       END-IF                                                             
031600                                                                          
031700       IF W-SKRIV-UTFIL = JA                                              
031800                                                                          
031900          IF S-BEFT = 30 OR 31                                            
032000            MOVE ZERO           TO W-SUM-PRDMTRL                          
032100          END-IF                                                          
032200                                                                          
032300          IF S-BEFT = 23 OR 28                                            
032310            MOVE ZERO           TO W-SUM-PRDMTRL                          
032320          END-IF                                                          
032330                                                                          
032400          IF S-BEFT = 70 OR 71 OR 72 OR 73 OR 74 OR 75 OR 76 OR           
032500                      77 OR 78 OR 79                                      
032600            IF S-PRDIRLON = 0 AND S-PROVRPAL = 0                          
032700              MOVE S-PRDMTRL    TO W-SUM-PRDMTRL                          
032800              MOVE +0           TO W-SUM-PRDIRLON                         
032900                                   W-SUM-PROVRPAL                         
033000            ELSE                                                          
033100              MOVE +0           TO W-SUM-PRDMTRL                          
033200                                   W-SUM-PRDIRLON                         
033300                                   W-SUM-PROVRPAL                         
033400            END-IF                                                        
033500          END-IF                                                          
033600                                                                          
033700          IF W-SUM-PRDIRLON > +0 OR                                       
033800             W-SUM-PRDMTRL  > +0 OR                                       
033900             W-SUM-PROVRPAL > +0                                          
034000             PERFORM CH-SKRIV-UTFILER                                     
034100          END-IF                                                          
034200       ELSE                                                               
034300          PERFORM S05-SKRIV-FELFIL                                        
034400       END-IF                                                             
034500                                                                          
034600       PERFORM S04-LAS-SORTPOST                                           
034700     END-PERFORM                                                          
034800     .                                                                    
034900     EJECT                                                                
035000 CA-TAB-SATS-FAKTOR SECTION.                                              
035100                                                                          
035200     MOVE +0                TO IX1                                        
035300     PERFORM S03-LAS-W55103-SATSFAKTOR                                    
035400     PERFORM UNTIL INFIL-SLUT = JA                                        
035500       ADD +1               TO IX1                                        
035600       IF IX1 > +3000                                                     
035700          DISPLAY '*** W5510100 FEL TABELL-3 SPRÄNGD ***'                 
035800          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
035900       END-IF                                                             
036000       MOVE W-SATS-IDARTNR  TO TAB3-IDARTNR  (IX1)                        
036100       MOVE W-SATS-RESATSFP TO TAB3-RESATSFP (IX1)                        
036200                                                                          
036300       PERFORM S03-LAS-W55103-SATSFAKTOR                                  
036400     END-PERFORM                                                          
036500                                                                          
036600     PERFORM UNTIL IX1 = +3000                                            
036700       ADD +1               TO IX1                                        
036800       MOVE +999999999      TO TAB3-IDARTNR  (IX1)                        
036900       MOVE +0              TO TAB3-RESATSFP (IX1)                        
037000     END-PERFORM                                                          
037100     .                                                                    
037200     EJECT                                                                
037300 CB-TAB-EXTRA-KOSTNAD-ARTIKEL SECTION.                                    
037400                                                                          
037500     PERFORM IMS-GU-WL513501                                              
037600                                                                          
037700     PERFORM IMS-GNP-WL513511                                             
037800                                                                          
037900     MOVE +0                TO IX1                                        
038000     PERFORM UNTIL SEGMENT-SAKNAS                                         
038100       ADD +1               TO IX1                                        
038200       IF IX1 > +500                                                      
038300          DISPLAY '*** W5510100 FEL TABELL-1 SPRÄNGD ***'                 
038400          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
038500       END-IF                                                             
038600       MOVE 5136-IDARTNR    TO TAB1-IDARTNR  (IX1)                        
038700       MOVE 5136-PRDIRLON   TO TAB1-PRDIRLON (IX1)                        
038800       MOVE 5136-PROVRPAL   TO TAB1-PROVRPAL (IX1)                        
038900       MOVE 5136-PRDMTRL    TO TAB1-PRDMTRL  (IX1)                        
039000       MOVE 5136-FLFPTILL   TO TAB1-FLFPTILL (IX1)                        
039100                                                                          
039200        PERFORM IMS-GNP-WL513511                                          
039300     END-PERFORM                                                          
039400                                                                          
039500     PERFORM UNTIL IX1 = +500                                             
039600       ADD +1               TO IX1                                        
039700       MOVE +999999999      TO TAB1-IDARTNR  (IX1)                        
039800       MOVE +0              TO TAB1-PRDIRLON (IX1)                        
039900       MOVE +0              TO TAB1-PROVRPAL (IX1)                        
040000       MOVE +0              TO TAB1-PRDMTRL  (IX1)                        
040100       MOVE SPACE           TO TAB1-FLFPTILL (IX1)                        
040200     END-PERFORM                                                          
040300     .                                                                    
040400     EJECT                                                                
040500 CC-TAB-KOSTNAD-DETALJKOD SECTION.                                        
040600                                                                          
040700     PERFORM IMS-GU-WL513301                                              
040800                                                                          
040900     PERFORM IMS-GNP-WL513311                                             
041000                                                                          
041100     MOVE +0                 TO IX1                                       
041200     PERFORM UNTIL SEGMENT-SAKNAS                                         
041300       ADD +1                TO IX1                                       
041400       IF IX1 > +500                                                      
041500          DISPLAY '*** W5510100 FEL TABELL-2 SPRÄNGD ***'                 
041600          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
041700       END-IF                                                             
041800       MOVE 5134-IDARTNR-EMB TO TAB2-IDARTNR-EMB (IX1)                    
041900       MOVE 5134-PRDMTRL     TO TAB2-PRDMTRL     (IX1)                    
042000                                                                          
042100        PERFORM IMS-GNP-WL513311                                          
042200     END-PERFORM                                                          
042300                                                                          
042400     PERFORM UNTIL IX1 = +500                                             
042500       ADD +1                TO IX1                                       
042600       MOVE +999999999       TO TAB2-IDARTNR-EMB (IX1)                    
042700       MOVE +0               TO TAB2-PRDMTRL     (IX1)                    
042800     END-PERFORM                                                          
042900     .                                                                    
043000     EJECT                                                                
043100 CF-HAMTA-EXTRA-KOSTN-ART SECTION.                                        
043200                                                                          
043300     MOVE JA TO W-FLFPTILL                                                
043400                                                                          
043500     SEARCH ALL TAB1-KOSTNAD-ARTIKEL                                      
043600       AT END CONTINUE                                                    
043700       WHEN TAB1-IDARTNR (INDX-1) = S-IDARTNR                             
043800         ADD  TAB1-PRDIRLON (INDX-1)    TO W-SUM-PRDIRLON                 
043900         ADD  TAB1-PRDMTRL  (INDX-1)    TO W1-SUM-PRDMTRL                 
044000         ADD  TAB1-PROVRPAL (INDX-1)    TO W-SUM-PROVRPAL                 
044100         MOVE TAB1-FLFPTILL (INDX-1)    TO W-FLFPTILL                     
044200     END-SEARCH                                                           
044300     .                                                                    
044400     EJECT                                                                
044500 CG-KOSTNAD-FORPACKNINGSTYP SECTION.                                      
044600                                                                          
044700     IF W-FLFPTILL = JA                                                   
044800       IF S-IDLEVNR = '1002' OR 'C7CUH'                                   
044900                                                                          
045000           SEARCH ALL TAB3-SATS-FAKTOR                                    
045100             AT END                                                       
045200                MOVE +1                     TO W-RESATSFP                 
045300             WHEN TAB3-IDARTNR (INDX-3) = S-IDARTNR                       
045400                MOVE TAB3-RESATSFP (INDX-3) TO W-RESATSFP                 
045500           END-SEARCH                                                     
045600                                                                          
045700           COMPUTE W-SUM-PRDIRLON ROUNDED = W-SUM-PRDIRLON +              
045800                   W-5132-PRDIRLON * W-RESATSFP                           
045900           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
046000                   W-5132-PRDMTRL * W-RESATSFP                            
046100           COMPUTE W-SUM-PROVRPAL ROUNDED = W-SUM-PROVRPAL +              
046200                   W-5132-PROVRPAL * W-RESATSFP                           
046300       ELSE                                                               
046400         MOVE S-IDARTNR             TO W-IDARTNR                          
046500         PERFORM IMS-GU-ARTC11                                            
046600         IF SEGMENT-FINNS                                                 
046700           MOVE CLAG-KVQPACK-0      TO K6-KVQPACK-0                       
046800           MOVE CLAG-KVQPACK-1      TO K6-KVQPACK-1                       
046900           MOVE CLAG-KVQPACK-2      TO K6-KVQPACK-2                       
047000           MOVE CLAG-IDARTNR-EMBQ0  TO K6-IDARTNR-EMBQ0                   
047100           MOVE CLAG-IDARTNR-EMBQ1  TO K6-IDARTNR-EMBQ1                   
047200           MOVE CLAG-IDARTNR-EMBQ2  TO K6-IDARTNR-EMBQ2                   
047300         ELSE                                                             
047400           MOVE ZERO                TO K6-KVQPACK-0                       
047500                                       K6-KVQPACK-1                       
047600                                       K6-KVQPACK-2                       
047700                                       K6-IDARTNR-EMBQ0                   
047800                                       K6-IDARTNR-EMBQ1                   
047900                                       K6-IDARTNR-EMBQ2                   
048000         END-IF                                                           
048100         MOVE ZERO                  TO W-KVQPACK-MIN                      
048200         IF K6-KVQPACK-0 > +0                                             
048300           MOVE K6-KVQPACK-0        TO W-KVQPACK-MIN                      
048400         END-IF                                                           
048500         IF K6-KVQPACK-1 > +0                                             
048600           IF W-KVQPACK-MIN = +0                                          
048700             MOVE K6-KVQPACK-1      TO W-KVQPACK-MIN                      
048800           ELSE                                                           
048900             IF K6-KVQPACK-1 < W-KVQPACK-MIN                              
049000               MOVE K6-KVQPACK-1    TO W-KVQPACK-MIN                      
049100             END-IF                                                       
049200           END-IF                                                         
049300         END-IF                                                           
049400         IF K6-KVQPACK-2 > +0                                             
049500           IF W-KVQPACK-MIN = +0                                          
049600             MOVE K6-KVQPACK-2      TO W-KVQPACK-MIN                      
049700           ELSE                                                           
049800             IF K6-KVQPACK-2 < W-KVQPACK-MIN                              
049900               MOVE K6-KVQPACK-2    TO W-KVQPACK-MIN                      
050000             END-IF                                                       
050100           END-IF                                                         
050200         END-IF                                                           
050300                                                                          
050400         IF W-KVQPACK-MIN = +0                                            
050500           MOVE +1                  TO W-KVQPACK-MIN                      
050600         END-IF                                                           
050700                                                                          
050800         COMPUTE W-SUM-PRDIRLON ROUNDED = W-SUM-PRDIRLON +                
050900                             (W-5132-PRDIRLON / W-KVQPACK-MIN)            
051000         COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                        
051100                             (W-5132-PRDMTRL / W-KVQPACK-MIN)             
051200         COMPUTE W-SUM-PROVRPAL ROUNDED = W-SUM-PROVRPAL +                
051300                             (W-5132-PROVRPAL / W-KVQPACK-MIN)            
051400                                                                          
051500         IF W-5132-PRDIRLON > +0 OR W-5132-PRDMTRL > +0 OR                
051600            W-5132-PROVRPAL > +0                                          
051700           PERFORM CGB-PRIS-EMBALLAGE                                     
051800         END-IF                                                           
051900       END-IF                                                             
052000     END-IF                                                               
052100                                                                          
052200     IF W1-SUM-PRDMTRL > +0                                               
052300        COMPUTE W-SUM-PRDMTRL ROUNDED = W1-SUM-PRDMTRL * W-INFL           
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 CGB-PRIS-EMBALLAGE SECTION.                                              
052800                                                                          
052900     IF K6-KVQPACK-0 = +0                                                 
053000        MOVE +1             TO W-KVQPACK-0                                
053100     ELSE                                                                 
053200        MOVE K6-KVQPACK-0   TO W-KVQPACK-0                                
053300     END-IF                                                               
053400     IF K6-KVQPACK-1 = +0                                                 
053500        MOVE +1             TO W-KVQPACK-1                                
053600     ELSE                                                                 
053700        MOVE K6-KVQPACK-1   TO W-KVQPACK-1                                
053800     END-IF                                                               
053900     IF K6-KVQPACK-2 = +0                                                 
054000        MOVE +1             TO W-KVQPACK-2                                
054100     ELSE                                                                 
054200        MOVE K6-KVQPACK-2   TO W-KVQPACK-2                                
054300     END-IF                                                               
054400                                                                          
054500     IF K6-IDARTNR-EMBQ0 > +0                                             
054600        MOVE K6-IDARTNR-EMBQ0 TO W-IDARTNR                                
054700        PERFORM S01-SOK-KOSTNAD-DETALJKOD                                 
054800        IF DETALJKOD-FINNS                                                
054900           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
055000                           (W-PRDMTRL / W-KVQPACK-0)                      
055100        ELSE                                                              
055200          PERFORM IMS-GU-ARTC11                                           
055300          IF SEGMENT-FINNS                                                
055400             COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                    
055500                     (CLAG-PRARTSTD / W-KVQPACK-0)                        
055600          ELSE                                                            
055700             MOVE NEJ               TO W-SKRIV-UTFIL                      
055800             MOVE 'EMB.ART.NR FÖR Q0 SAKNAS' TO W-TEXT                    
055900          END-IF                                                          
056000        END-IF                                                            
056100     END-IF                                                               
056200                                                                          
056300     IF K6-IDARTNR-EMBQ1 > +0                                             
056400        MOVE K6-IDARTNR-EMBQ1 TO W-IDARTNR                                
056500        PERFORM S01-SOK-KOSTNAD-DETALJKOD                                 
056600        IF DETALJKOD-FINNS                                                
056700           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
056800                            (W-PRDMTRL / W-KVQPACK-1)                     
056900        ELSE                                                              
057000          PERFORM IMS-GU-ARTC11                                           
057100          IF SEGMENT-FINNS                                                
057200             COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                    
057300                     (CLAG-PRARTSTD / W-KVQPACK-1)                        
057400          ELSE                                                            
057500             MOVE NEJ               TO W-SKRIV-UTFIL                      
057600             MOVE 'EMB.ART.NR FÖR Q1 SAKNAS'  TO W-TEXT                   
057700          END-IF                                                          
057800        END-IF                                                            
057900     END-IF                                                               
058000                                                                          
058100     IF K6-IDARTNR-EMBQ2 > +0                                             
058200        MOVE K6-IDARTNR-EMBQ2 TO W-IDARTNR                                
058300        PERFORM S01-SOK-KOSTNAD-DETALJKOD                                 
058400        IF DETALJKOD-FINNS                                                
058500           COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                      
058600                            (W-PRDMTRL / W-KVQPACK-2)                     
058700        ELSE                                                              
058800          PERFORM IMS-GU-ARTC11                                           
058900          IF SEGMENT-FINNS                                                
059000             COMPUTE W1-SUM-PRDMTRL = W1-SUM-PRDMTRL +                    
059100                          (CLAG-PRARTSTD / W-KVQPACK-2)                   
059200          ELSE                                                            
059300             MOVE NEJ               TO W-SKRIV-UTFIL                      
059400             MOVE 'EMB.ART.NR FÖR Q2 SAKNAS'  TO W-TEXT                   
059500          END-IF                                                          
059600        END-IF                                                            
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 CH-SKRIV-UTFILER SECTION.                                                
060100                                                                          
060200     MOVE S-IDARTNR            TO UT-R24-IDARTNR                          
060300     MOVE S-KDVTH              TO UT-R24-KDVTH                            
060400                                                                          
060500     IF W-SUM-PRDIRLON < +0.01 AND > +0.00000                             
060600       MOVE +0.010             TO UT-R24-PRDIRLON                         
060700     ELSE                                                                 
060800       COMPUTE W-ROUND-PRDIRLON ROUNDED = W-SUM-PRDIRLON                  
060900       MOVE W-ROUND-PRDIRLON   TO UT-R24-PRDIRLON                         
061000     END-IF                                                               
061100                                                                          
061200     IF W-SUM-PRDMTRL < +0.01 AND > +0.00000                              
061300       MOVE +0.010             TO UT-R24-PRDMTRL                          
061400     ELSE                                                                 
061500       COMPUTE W-ROUND-PRDMTRL ROUNDED = W-SUM-PRDMTRL                    
061600       MOVE W-ROUND-PRDMTRL    TO UT-R24-PRDMTRL                          
061700     END-IF                                                               
061800                                                                          
061900     IF W-SUM-PROVRPAL < +0.01  AND > +0.00000                            
062000       MOVE +0.010             TO UT-R24-PROVRPAL                         
062100     ELSE                                                                 
062200       COMPUTE W-ROUND-PROVRPAL ROUNDED = W-SUM-PROVRPAL                  
062300       MOVE W-ROUND-PROVRPAL   TO UT-R24-PROVRPAL                         
062400     END-IF                                                               
062500                                                                          
062600     IF UT-R24-PRDIRLON  > +0 OR                                          
062700        UT-R24-PRDMTRL   > +0 OR                                          
062800        UT-R24-PROVRPAL  > +0                                             
062900                                                                          
063000*  SKRIV R24-POSTER FÖR ARTIKELREG.UPPDATERING                            
063100       WRITE W55105-POST FROM UT-R24-UTPOST                               
063200                                                                          
063300       MOVE 'W55105'           TO POSTSUM-FDNAMN                          
063400       MOVE 'W55101D3'         TO POSTSUM-DDNAMN2                         
063500       CALL POSTSUM USING POSTSUM-PARM                                    
063600                                                                          
063700*  SKRIV KONTROLLPOST FÖR LISTOR                                          
063800       MOVE S-SORTPOST         TO UT-UTPOST                               
063900       MOVE UT-R24-PRDIRLON    TO UT-PRDIRLON                             
064000       MOVE UT-R24-PRDMTRL     TO UT-PRDMTRL                              
064100       MOVE UT-R24-PROVRPAL    TO UT-PROVRPAL                             
064200       MOVE S-KDVTH            TO UT-KDVTH                                
064300       WRITE W55106-POST FROM UT-UTPOST                                   
064400                                                                          
064500       MOVE 'W55106'           TO POSTSUM-FDNAMN                          
064600       MOVE 'W55101D4'         TO POSTSUM-DDNAMN2                         
064700       CALL POSTSUM USING POSTSUM-PARM                                    
064800     END-IF                                                               
064900     .                                                                    
065000     SKIP3                                                                
065100 Z-FINIT SECTION.                                                         
065200                                                                          
065300     CLOSE W55103 W55105                                                  
065400           W55106 W55108                                                  
065500                                                                          
065600     MOVE 'S'                 TO POSTSUM-OPKOD                            
065700     CALL POSTSUM USING POSTSUM-PARM                                      
065800     .                                                                    
065900     EJECT                                                                
066000 S01-SOK-KOSTNAD-DETALJKOD SECTION.                                       
066100                                                                          
066200     SEARCH ALL TAB2-KOSTNAD-DETALJKOD                                    
066300       AT END                                                             
066400          MOVE NEJ TO W-DETALJKOD                                         
066500       WHEN TAB2-IDARTNR-EMB(INDX-2) = W-IDARTNR                          
066600          MOVE TAB2-PRDMTRL (INDX-2) TO W-PRDMTRL                         
066700          MOVE JA                    TO W-DETALJKOD                       
066800     END-SEARCH                                                           
066900     .                                                                    
067000     SKIP2                                                                
067100 S03-LAS-W55103-SATSFAKTOR SECTION.                                       
067200                                                                          
067300     READ W55103 INTO W-SATS-POST                                         
067400     AT END                                                               
067500        MOVE JA               TO INFIL-SLUT                               
067600     NOT AT END                                                           
067700        MOVE 'W55103'         TO POSTSUM-FDNAMN                           
067800        MOVE 'W55101D2'       TO POSTSUM-DDNAMN2                          
067900        CALL POSTSUM USING POSTSUM-PARM                                   
068000     END-READ                                                             
068100     .                                                                    
068200     SKIP2                                                                
068300 S04-LAS-SORTPOST SECTION.                                                
068400                                                                          
068500     RETURN SORTFIL                                                       
068600     AT END                                                               
068700       MOVE JA    TO SORTFIL-EOF                                          
068800     NOT AT END                                                           
068900       MOVE 'W55113'         TO POSTSUM-FDNAMN                            
069000       MOVE 'W55101D1'       TO POSTSUM-DDNAMN2                           
069100       CALL POSTSUM USING POSTSUM-PARM                                    
069200     END-RETURN                                                           
069300     .                                                                    
069400     SKIP2                                                                
069500 S05-SKRIV-FELFIL SECTION.                                                
069600                                                                          
069700     MOVE S-KDGK              TO UT-FEL-KDGK                              
069800     MOVE S-IDARTNR           TO UT-FEL-IDARTNR                           
069900     MOVE W-TEXT              TO UT-FEL-TENOTE                            
070000     WRITE W55108-POST FROM UT-FEL-UTPOST                                 
070100                                                                          
070200     MOVE 'W55108'         TO POSTSUM-FDNAMN                              
070300     MOVE 'W55101D5'       TO POSTSUM-DDNAMN2                             
070400     CALL POSTSUM USING POSTSUM-PARM                                      
070500     .                                                                    
070600     EJECT                                                                
070700*    ---- IMS SEKTIONER                                                   
070800                                                                          
070900 IMS-GU-ARTC11 SECTION.                                                   
071000                                                                          
071100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
071200            DELIMITED BY SIZE INTO SSA1                                   
071300     MOVE 'WLARTC11'            TO SSA2                                   
071400     MOVE '  GE'                TO GODK-STATUSKODER                       
071500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
071600     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
071700     PERFORM IMS-STATUSKONTROLL                                           
071800     .                                                                    
071900     SKIP3                                                                
072000 IMS-GU-WL513111 SECTION.                                                 
072100                                                                          
072200     STRING 'WL513101(WDGXKEY  =' W-WDGX-5131-KEY-X ')'                   
072300            DELIMITED BY SIZE INTO SSA1                                   
072400     STRING 'WL513111(BEFT     =' W-BEFT-X ')'                            
072500            DELIMITED BY SIZE INTO SSA2                                   
072600     MOVE '  GE'                TO GODK-STATUSKODER                       
072700     CALL CBLTDLI USING GU 5131-PCB DLI-IO-AREA SSA1 SSA2                 
072800     MOVE 5131-STATUS-CODE      TO STATUS-WS                              
072900     PERFORM IMS-STATUSKONTROLL                                           
073000     .                                                                    
073100     EJECT                                                                
073200 IMS-GU-WL513301 SECTION.                                                 
073300                                                                          
073400     STRING 'WL513301(WDGXKEY  =' W-WDGX-5133-KEY-X ')'                   
073500            DELIMITED BY SIZE INTO SSA1                                   
073600     MOVE '  '                  TO GODK-STATUSKODER                       
073700     CALL CBLTDLI USING GU  5133-PCB DLI-IO-AREA SSA1                     
073800     MOVE 5133-STATUS-CODE      TO STATUS-WS                              
073900     PERFORM IMS-STATUSKONTROLL                                           
074000     .                                                                    
074100     SKIP2                                                                
074200 IMS-GNP-WL513311 SECTION.                                                
074300                                                                          
074400     MOVE 'WL513311'            TO SSA1                                   
074500     MOVE '  GE'                TO GODK-STATUSKODER                       
074600     CALL CBLTDLI USING GNP 5133-PCB DLI-IO-AREA SSA1                     
074700     MOVE 5133-STATUS-CODE      TO STATUS-WS                              
074800     PERFORM IMS-STATUSKONTROLL                                           
074900     .                                                                    
075000     EJECT                                                                
075100 IMS-GU-WL513501 SECTION.                                                 
075200                                                                          
075300     STRING 'WL513501(WDGXKEY  =' W-WDGX-5135-KEY-X ')'                   
075400            DELIMITED BY SIZE INTO SSA1                                   
075500     MOVE '  '                  TO GODK-STATUSKODER                       
075600     CALL CBLTDLI USING GU  5135-PCB DLI-IO-AREA SSA1                     
075700     MOVE 5135-STATUS-CODE      TO STATUS-WS                              
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000     SKIP3                                                                
076100 IMS-GNP-WL513511 SECTION.                                                
076200                                                                          
076300     MOVE 'WL513511'            TO SSA1                                   
076400     MOVE '  GE'                TO GODK-STATUSKODER                       
076500     CALL CBLTDLI USING GNP 5135-PCB DLI-IO-AREA SSA1                     
076600     MOVE 5135-STATUS-CODE      TO STATUS-WS                              
076700     PERFORM IMS-STATUSKONTROLL                                           
076800     .                                                                    
076900     EJECT                                                                
076910 IMS-GU-WDGX5108 SECTION.                                                 
076920                                                                          
076943     STRING 'WDR201  (WDGXKEY  =' W-WDGX-5107-KEY-X ')'                   
076944          DELIMITED BY SIZE INTO SSA1                                     
076945     STRING 'WDGX5108(IDFTG    =' W-IDFTG-KEY-X ')'                       
076946          DELIMITED BY SIZE INTO SSA2                                     
076950     MOVE '  '   TO GODK-STATUSKODER                                      
076960     CALL CBLTDLI USING GU 5108-PCB DLI-IO-AREA SSA1 SSA2                 
076970     MOVE 5108-STATUS-CODE TO STATUS-WS                                   
076980     PERFORM IMS-STATUSKONTROLL                                           
076990     .                                                                    
076991     EJECT                                                                
077000 IMS-STATUSKONTROLL SECTION.                                              
077100                                                                          
077200     SET STATUS-IX TO 1                                                   
077300     SEARCH GODK-STATUS                                                   
077400       AT END CALL FELLOG                                                 
077500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
077600         CONTINUE                                                         
077700     END-SEARCH                                                           
077800     .                                                                    
