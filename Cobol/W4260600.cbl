000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W4260600.                                        
000400 AUTHOR.                 KATARINA KYMMER                                  
000500     DATE-WRITTEN.       APRIL 1990.                                      
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            LÄSER IN INFIL1 I EN TABELL.                                 
001100*            LÄSER LAGERBANDEN FÖR CDC OCH SDC  OCH KONTROLLERAR          
001200*            ATT ARTIKELN FINNS INOM LAGERPLATS-INTERVALLET FRÅN          
001300*            TABELLEN OCH SKRIVER DÅ PÅ UTFILEN.                          
001400*                                                                         
001500*            LÄSER      WDD3, WDK6                                        
001600*            UPPDATERAR W6H6 MED ANTAL KONTROLLERADE ARTIKLAR.            
001700*                                                                         
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*            U0999      - FELLOG                                          
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*                            INFILER:                                     
002900                                                                          
003000     SELECT  W42604                 ASSIGN TO    W42606D1.                
003100                                                                          
003200     SELECT  W01172                 ASSIGN TO    W42606D2.                
003300                                                                          
003400     SELECT  W01184                 ASSIGN TO    W42606D3.                
003500                                                                          
003600*                            UTFILER:                                     
003700     SELECT  W42606                 ASSIGN TO    W42606D4.                
003800                                                                          
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP2                                                                
004400                                                                          
004500 FD  W42604                                                               
004600     LABEL RECORDS STANDARD                                               
004700     RECORDING F                                                          
004800     BLOCK CONTAINS 0.                                                    
004900*  01 W42604-POST -COPY W4260401 -L                                       
005000                                                                          
005100 FD  W01172                                                               
005200     LABEL RECORDS STANDARD                                               
005300     RECORDING F                                                          
005400     BLOCK CONTAINS 0.                                                    
005500*  01  -COPY W011100 -L                                                   
005600                                                                          
005700 FD  W01184                                                               
005800     LABEL RECORDS STANDARD                                               
005900     RECORDING F                                                          
006000     BLOCK CONTAINS 0.                                                    
006100*  01  -COPY W01184  -L                                                   
006200                                                                          
006300 FD  W42606                                                               
006400     LABEL RECORDS STANDARD                                               
006500     RECORDING F                                                          
006600     BLOCK CONTAINS 0.                                                    
006700*  01 W42606-POST  -COPY W4260601 -L                                      
006800                                                                          
006900 WORKING-STORAGE SECTION.                                                 
007000     SKIP2                                                                
007100                                                                          
007200*    -- CHECKED BY WY2000                                                 
007300 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4260600'.               
007400 77  W-TOT-KVART                 PIC S9(07) COMP-3 VALUE +0.              
007500 77  W-SPAR-IDDC                 PIC  XX.                                 
007600 77  W-SPAR-IDKVAOMR             PIC  X.                                  
007700 77  W-SPAR-IDKVAGRP             PIC  9(3).                               
007800 77  W-SPAR-TIREGDAT             PIC S9(7)  COMP-3.                       
007900     SKIP2                                                                
008000*    ---- GENERELLA KONSTANTER                                            
008100                                                                          
008200 77  JA                          PIC X       VALUE 'J'.                   
008300 77  NEJ                         PIC X       VALUE 'N'.                   
008400     SKIP2                                                                
008500*      --- VALID IDDC CODES                                               
008600*                                                                         
008700*01    -COPY WWDC99                                                       
008800       EJECT                                                              
008900*    ---- ARBETSFÄLT                                                      
009000 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
009100 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
009200     SKIP2                                                                
009300 01  TABELL-1.                                                            
009400   03 FILLER OCCURS 300.                                                  
009500     05 TAB-IDDC         PIC  XX.                                         
009600     05 TAB-IDKVAOMR     PIC  X.                                          
009700     05 TAB-BEKVAOMR     PIC  X(10).                                      
009800     05 TAB-IDKVAGRP     PIC  9(3).                                       
009900     05 TAB-TIREGDAT     PIC S9(7)      COMP-3.                           
010000     05 TAB-ADLAGOMR-FOM PIC S9(3)      COMP-3.                           
010100     05 TAB-ADGANG-FOM   PIC S9(3)      COMP-3.                           
010200     05 TAB-ADPLATS-FOM  PIC S9(5)      COMP-3.                           
010300     05 TAB-ADLAGOMR-TOM PIC S9(3)      COMP-3.                           
010400     05 TAB-ADGANG-TOM   PIC S9(3)      COMP-3.                           
010500     05 TAB-ADPLATS-TOM  PIC S9(5)      COMP-3.                           
010600     05 TAB-KVART        PIC S9(7)      COMP-3.                           
010700                                                                          
010800                                                                          
010900*  ---- AREOR IN OCH UT POSTER                                            
011000                                                                          
011100*01  POST -COPY W4260401 -PRE IN-                                         
011200     EJECT                                                                
011300*01  POST -COPY W4260601 -PRE UT-                                         
011400     EJECT                                                                
011500*01  POST -COPY W011100  -PRE W01172-                                     
011600     EJECT                                                                
011700*01  POST -COPY W01184   -PRE W01184-                                     
011800     EJECT                                                                
011900*    ---- INDEX FÄLT                                                      
012000 77  IX                     PIC S9(3)  VALUE +0 COMP SYNC.                
012100 77  IX1                    PIC S9(3)  VALUE +0 COMP SYNC.                
012200 77  IX-URV                 PIC S9(3)  VALUE +0 COMP SYNC.                
012300                                                                          
012400*    ---- END-OF-FILE SWITCHAR                                            
012500 77  W42604-SLUT            PIC X(03)   VALUE SPACE.                      
012600 77  W01172-EOF             PIC X       VALUE 'N'.                        
012700 77  W01184-EOF             PIC X       VALUE 'N'.                        
012800                                                                          
012900*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
013000 01  DYNAMISKA-SUBPROGRAM.                                                
013100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
013300   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
013400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013500   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
013600     SKIP2                                                                
013700     EJECT                                                                
013800*    ---- PARAMETRAR TILL DATUMKORT                                       
013900  01  DATUMKORT-ID           PIC X(6)          VALUE 'WDATUM'.            
014000                                                                          
014100*01  -COPY WDATAREA                                                       
014200                                                                          
014300*    ---- PARAMETRAR TILL ABEND                                           
014400  01 RETURKODER.                                                          
014500    03 RKOD-ABEND-UTAN-DUMP     PIC S9(04) COMP SYNC VALUE +16.           
014600     EJECT                                                                
014700                                                                          
014800*    ---- PARAMETRAR TILL POSTSUM                                         
014900                                                                          
015000*01  -COPY W0005 -PRE POSTSUM-.                                           
015100     EJECT                                                                
015200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015500     SKIP2                                                                
015600*    ---- STATUSKOD FRÅN IMS                                              
015700                                                                          
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FINNS                       VALUE '  '.                  
016000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016100     SKIP2                                                                
016200 01  GODK-STATUSKODER.                                                    
016300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
016400     SKIP2                                                                
016500 01  SSA1                        PIC X(64).                               
016600 01  SSA2                        PIC X(64).                               
016700     SKIP2                                                                
016800*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
016900 01  FILLER                 PIC X(16) VALUE  'NYCKLAR-TILL-DL'.           
017000 01  NYCKLAR-TILL-DLI.                                                    
017100   03  W-W6H601KY-X.                                                      
017200     05  W-IDDC                  PIC XX   VALUE SPACE.                    
017300     05  W-IDKVAOMR              PIC  X   VALUE ZERO.                     
017400     05  W-IDKVAGRP              PIC  9(3) VALUE ZERO.                    
017500     05  W-DAREGDAT              PIC  9(8) VALUE ZERO.                    
017600                                                                          
017700   03 W-IDARTNR-X.                                                        
017800     05 W-IDARTNR                PIC S9(09)               COMP-3.         
017900                                                                          
018000   03 W-IDSKYLT-X.                                                        
018100     05 W-IDSKYLT                PIC X(03).                               
018200     EJECT                                                                
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA1'.          
018600     SKIP2                                                                
018700 01  DLI-IO-AREA1.                                                        
018800   03 IO-AREA1                   PIC X(900)  VALUE SPACE.                 
018900     SKIP2                                                                
019000*    03 W6H601 -COPY W6H601  -RED IO-AREA1.                               
019100     EJECT                                                                
019200*    03 WDK611 -COPY WDK611  -RED IO-AREA1.                               
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
019500     SKIP2                                                                
019600 01  DLI-IO-AREA2.                                                        
019700   03 IO-AREA2                   PIC X(120)  VALUE SPACE.                 
019800     SKIP2                                                                
019900*    03 WDD311 -COPY WDD311  -RED IO-AREA2.                               
020000     EJECT                                                                
020100 LINKAGE SECTION.                                                         
020200*01      -COPY W0009     -PRE MSG-                                        
020300     EJECT                                                                
020400*01      -COPY W0008     -PRE W6H6-                                       
020500      05 FILLER          PIC X.                                           
020600     EJECT                                                                
020700*01      -COPY W0008     -PRE ARTC-                                       
020800      05 FILLER          PIC X.                                           
020900     EJECT                                                                
021000*01      -COPY W0008     -PRE WDD3-                                       
021100      05 FILLER          PIC X.                                           
021200     EJECT                                                                
021300 PROCEDURE DIVISION  USING MSG-PCB W6H6-PCB ARTC-PCB WDD3-PCB.            
021400     ENTRY 'DLITCBL' USING MSG-PCB W6H6-PCB ARTC-PCB WDD3-PCB.            
021500     SKIP2                                                                
021600     PERFORM A-INIT                                                       
021700     PERFORM B-LAESIN-TAB                                                 
021800     IF IX-URV > +0                                                       
021900        PERFORM C-SOEK-TAB-FOR-CDC                                        
022000        PERFORM D-SOEK-TAB-FOR-SDC                                        
022100        PERFORM E-UPPDATERA-W6H6                                          
022200     END-IF                                                               
022300     PERFORM Z-FINIT                                                      
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900     SKIP2                                                                
023000         OPEN INPUT  W42604                                               
023100                     W01172                                               
023200                     W01184                                               
023300              OUTPUT W42606                                               
023400     .                                                                    
023500     EJECT                                                                
023600 B-LAESIN-TAB SECTION.                                                    
023700     SKIP2                                                                
023800     MOVE SPACE TO W42604-SLUT                                            
023900     READ W42604 INTO IN-POST AT END                                      
024000       MOVE 'EOF' TO W42604-SLUT                                          
024100     END-READ                                                             
024200                                                                          
024300     MOVE +1 TO IX-URV                                                    
024400     PERFORM UNTIL W42604-SLUT = 'EOF' OR IX-URV > +300                   
024500       MOVE IN-IDDC         TO TAB-IDDC(IX-URV)                           
024600       MOVE IN-IDKVAOMR     TO TAB-IDKVAOMR(IX-URV)                       
024700       MOVE IN-BEKVAOMR     TO TAB-BEKVAOMR(IX-URV)                       
024800       MOVE IN-IDKVAGRP     TO TAB-IDKVAGRP(IX-URV)                       
024900       MOVE IN-TIREGDAT     TO TAB-TIREGDAT(IX-URV)                       
025000       MOVE IN-ADLAGOMR-FOM TO TAB-ADLAGOMR-FOM(IX-URV)                   
025100       MOVE IN-ADGANG-FOM   TO TAB-ADGANG-FOM(IX-URV)                     
025200       MOVE IN-ADPLATS-FOM  TO TAB-ADPLATS-FOM(IX-URV)                    
025300       MOVE IN-ADLAGOMR-TOM TO TAB-ADLAGOMR-TOM(IX-URV)                   
025400       MOVE IN-ADGANG-TOM   TO TAB-ADGANG-TOM(IX-URV)                     
025500       MOVE IN-ADPLATS-TOM  TO TAB-ADPLATS-TOM(IX-URV)                    
025600       MOVE             +0  TO TAB-KVART(IX-URV)                          
025700                                                                          
025800       READ W42604 INTO IN-POST AT END                                    
025900         MOVE 'EOF' TO W42604-SLUT                                        
026000       END-READ                                                           
026100       ADD +1 TO IX-URV                                                   
026200     END-PERFORM                                                          
026300                                                                          
026400     SUBTRACT +1 FROM IX-URV                                              
026500                                                                          
026600     IF W42604-SLUT NOT = 'EOF'                                           
026700          DISPLAY '*****TABELLEN SPRÄNGD***** IX-URV =' IX-URV            
026800          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
026900     END-IF                                                               
027000                                                                          
027100     .                                                                    
027200     EJECT                                                                
027300 C-SOEK-TAB-FOR-CDC  SECTION.                                             
027400     SKIP2                                                                
027500     PERFORM S01-LAES-W01172                                              
027600     PERFORM UNTIL W01172-EOF = JA                                        
027700       MOVE +1 TO IX                                                      
027800       PERFORM UNTIL IX    > IX-URV                                       
027900         MOVE TAB-IDDC(IX)      TO WS-IDDC                                
028000         IF    CDC-SE                                                     
028100         AND   TAB-ADLAGOMR-FOM(IX) NOT > W01172-ADLAGOMR                 
028200         AND   TAB-ADGANG-FOM  (IX) NOT > W01172-ADGANG                   
028300         AND   TAB-ADPLATS-FOM (IX) NOT > W01172-ADPLATS                  
028400         AND   TAB-ADLAGOMR-TOM(IX) NOT < W01172-ADLAGOMR                 
028500         AND   TAB-ADGANG-TOM  (IX) NOT < W01172-ADGANG                   
028600         AND   TAB-ADPLATS-TOM (IX) NOT < W01172-ADPLATS                  
028700             MOVE TAB-IDDC(IX)          TO UT-IDDC                        
028800             MOVE TAB-IDKVAOMR(IX)      TO UT-IDKVAOMR                    
028900             MOVE TAB-BEKVAOMR(IX)      TO UT-BEKVAOMR                    
029000             MOVE TAB-IDKVAGRP(IX)      TO UT-IDKVAGRP                    
029100             MOVE TAB-TIREGDAT(IX)      TO DAT-I-TIDATUM                  
029200             MOVE 'AAMMDD'              TO DAT-KDDATFORM                  
029300             CALL WDATKONV USING DAT-KDDATFORM  DAT-I-TIDATUM             
029400                                 DAT-O-TIDATUM  DAT-KDSVAR                
029500             MOVE DAT-TIAARP            TO UT-TIAARP                      
029600             MOVE W01172-ADLAGOMR       TO UT-ADLAGOMR                    
029700             MOVE W01172-ADGANG         TO UT-ADGANG                      
029800             MOVE W01172-ADPLATS        TO UT-ADPLATS                     
029900             MOVE W01172-IDARTNR        TO UT-IDARTNR                     
030000                                           W-IDARTNR                      
030100             MOVE W01172-KVLS           TO UT-KVLS                        
030200             MOVE W01172-BEFT           TO UT-BEFT                        
030300                                                                          
030400             IF W01172-BEART-SVE = SPACE                                  
030500               IF TEXT-BEART = SPACE                                      
030600                 MOVE 'S  '            TO W-IDSKYLT                       
030700                 PERFORM IMS-GU-WLBENA11                                  
030800               END-IF                                                     
030900               MOVE TEXT-BEART          TO UT-BEART                       
031000             ELSE                                                         
031100               MOVE W01172-BEART-SVE    TO UT-BEART                       
031200             END-IF                                                       
031300             ADD +1 TO TAB-KVART (IX)                                     
031400             PERFORM S03-SKRIV-UTFIL                                      
031500         END-IF                                                           
031600         ADD +1 TO IX                                                     
031700       END-PERFORM                                                        
031800       PERFORM S01-LAES-W01172                                            
031900     END-PERFORM                                                          
032000     .                                                                    
032100     EJECT                                                                
032200 D-SOEK-TAB-FOR-SDC  SECTION.                                             
032300     SKIP2                                                                
032400     PERFORM S02-LAES-W01184                                              
032500     PERFORM UNTIL W01184-EOF = JA                                        
032600       MOVE +1 TO IX                                                      
032700       PERFORM UNTIL IX  > IX-URV                                         
032800         IF  TAB-IDDC (IX) = W01184-SLAG-IDDC                             
032900         AND TAB-ADLAGOMR-FOM(IX) NOT > W01184-SLAG-ADLAGOMR              
033000         AND TAB-ADGANG-FOM  (IX) NOT > W01184-SLAG-ADGANG                
033100         AND TAB-ADPLATS-FOM (IX) NOT > W01184-SLAG-ADPLATS               
033200         AND TAB-ADLAGOMR-TOM(IX) NOT < W01184-SLAG-ADLAGOMR              
033300         AND TAB-ADGANG-TOM  (IX) NOT < W01184-SLAG-ADGANG                
033400         AND TAB-ADPLATS-TOM (IX) NOT < W01184-SLAG-ADPLATS               
033500             MOVE TAB-IDDC(IX)          TO UT-IDDC                        
033600             MOVE TAB-IDKVAOMR(IX)      TO UT-IDKVAOMR                    
033700             MOVE TAB-BEKVAOMR(IX)      TO UT-BEKVAOMR                    
033800             MOVE TAB-IDKVAGRP(IX)      TO UT-IDKVAGRP                    
033900             MOVE TAB-TIREGDAT(IX)      TO DAT-I-TIDATUM                  
034000             MOVE 'AAMMDD'              TO DAT-KDDATFORM                  
034100             CALL WDATKONV USING DAT-KDDATFORM  DAT-I-TIDATUM             
034200                                 DAT-O-TIDATUM  DAT-KDSVAR                
034300             MOVE DAT-TIAARP            TO UT-TIAARP                      
034400             MOVE W01184-SLAG-ADLAGOMR  TO UT-ADLAGOMR                    
034500             MOVE W01184-SLAG-ADGANG    TO UT-ADGANG                      
034600             MOVE W01184-SLAG-ADPLATS   TO UT-ADPLATS                     
034700             MOVE W01184-SLAG-IDARTNR   TO UT-IDARTNR                     
034800                                           W-IDARTNR                      
034900             MOVE W01184-SLAG-KVLS      TO UT-KVLS                        
035000                                                                          
035100             PERFORM IMS-GET-ARTC11                                       
035200***          --- LÄSER ARTC DÅ ARTS INTE HAR BEFT                         
035300             IF SEGMENT-FINNS                                             
035400               MOVE CLAG-BEFT           TO UT-BEFT                        
035500             ELSE                                                         
035600               MOVE ZERO                TO UT-BEFT                        
035700             END-IF                                                       
035800                                                                          
035900             MOVE TAB-IDDC(IX)          TO WS-IDDC                        
036000             EVALUATE TRUE                                                
036100                WHEN SDC-NL      MOVE 'NL ' TO W-IDSKYLT                  
036200*               WHEN DC-FRA      MOVE 'F  ' TO W-IDSKYLT                  
036300                WHEN SDC-GB      MOVE 'GB ' TO W-IDSKYLT                  
036400                WHEN SDC-ES      MOVE 'E  ' TO W-IDSKYLT                  
036500                WHEN SDC-IT      MOVE 'I  ' TO W-IDSKYLT                  
036600                WHEN SDC-AT      MOVE 'DE ' TO W-IDSKYLT                  
036700                WHEN OTHER       MOVE 'GB ' TO W-IDSKYLT                  
036800             END-EVALUATE                                                 
036900             PERFORM IMS-GU-WLBENA11                                      
037000                                                                          
037100             IF TEXT-BEART = SPACE                                        
037200                MOVE 'GB '              TO W-IDSKYLT                      
037300                PERFORM IMS-GU-WLBENA11                                   
037400             END-IF                                                       
037500                                                                          
037600             MOVE TEXT-BEART            TO UT-BEART                       
037700             ADD   +1  TO TAB-KVART (IX)                                  
037800             PERFORM S03-SKRIV-UTFIL                                      
037900         END-IF                                                           
038000         ADD +1 TO IX                                                     
038100       END-PERFORM                                                        
038200       PERFORM S02-LAES-W01184                                            
038300     END-PERFORM                                                          
038400     .                                                                    
038500     EJECT                                                                
038600 E-UPPDATERA-W6H6 SECTION.                                                
038700     SKIP2                                                                
038800     MOVE +1 TO IX1                                                       
038900     PERFORM UNTIL IX1  > IX-URV                                          
039000     IF IX1 = +1                                                          
039100       MOVE TAB-IDDC (IX1)          TO W-SPAR-IDDC                        
039200       MOVE TAB-IDKVAOMR (IX1)      TO W-SPAR-IDKVAOMR                    
039300       MOVE TAB-IDKVAGRP (IX1)      TO W-SPAR-IDKVAGRP                    
039400       MOVE TAB-TIREGDAT (IX1)      TO W-SPAR-TIREGDAT                    
039500     END-IF                                                               
039600       IF  TAB-IDDC (IX1)     = W-SPAR-IDDC                               
039700       AND TAB-IDKVAOMR (IX1) = W-SPAR-IDKVAOMR                           
039800       AND TAB-IDKVAGRP (IX1) = W-SPAR-IDKVAGRP                           
039900       AND TAB-TIREGDAT (IX1) = W-SPAR-TIREGDAT                           
040000           ADD TAB-KVART (IX1)     TO W-TOT-KVART                         
040100       ELSE                                                               
040200           PERFORM IMS-GHU-W6KVAB01                                       
040300           IF SEGMENT-FINNS                                               
040400              MOVE W-TOT-KVART      TO OMR-KVART                          
040500              PERFORM IMS-REPL-W6KVAB01                                   
040600           END-IF                                                         
040700           MOVE TAB-KVART (IX1)     TO W-TOT-KVART                        
040800       END-IF                                                             
040900       MOVE TAB-IDDC (IX1)          TO W-IDDC     W-SPAR-IDDC             
041000       MOVE TAB-IDKVAOMR(IX1)       TO W-IDKVAOMR W-SPAR-IDKVAOMR         
041100       MOVE TAB-IDKVAGRP(IX1)       TO W-IDKVAGRP W-SPAR-IDKVAGRP         
041200       MOVE TAB-TIREGDAT(IX1)       TO W-DAREGDAT W-SPAR-TIREGDAT         
041300       IF TAB-TIREGDAT(IX1) NOT = ZERO                                    
041400         IF TAB-TIREGDAT(IX1) < 500000                                    
041500           MOVE 20                  TO W-DAREGDAT (1:2)                   
041600         ELSE                                                             
041700           IF TAB-TIREGDAT(IX1) < 999999                                  
041800             MOVE 19                TO W-DAREGDAT (1:2)                   
041900           ELSE                                                           
042000             MOVE 99999999          TO W-DAREGDAT                         
042100           END-IF                                                         
042200         END-IF                                                           
042300       END-IF                                                             
042400       ADD +1 TO IX1                                                      
042500     END-PERFORM                                                          
042600       PERFORM IMS-GHU-W6KVAB01                                           
042700       IF SEGMENT-FINNS                                                   
042800          MOVE W-TOT-KVART      TO OMR-KVART                              
042900          PERFORM IMS-REPL-W6KVAB01                                       
043000        END-IF                                                            
043100     .                                                                    
043200     EJECT                                                                
043300 S01-LAES-W01172 SECTION.                                                 
043400     READ W01172 INTO W01172-POST                                         
043500          AT END                                                          
043600             MOVE JA TO W01172-EOF                                        
043700     END-READ                                                             
043800     .                                                                    
043900 S02-LAES-W01184 SECTION.                                                 
044000     READ W01184 INTO W01184-POST                                         
044100          AT END                                                          
044200             MOVE JA TO W01184-EOF                                        
044300     END-READ                                                             
044400     .                                                                    
044500 S03-SKRIV-UTFIL SECTION.                                                 
044600     SKIP2                                                                
044700     WRITE W42606-POST FROM UT-POST                                       
044800     END-WRITE                                                            
044900                                                                          
045000     MOVE 'W42606' TO  POSTSUM-FDNAMN                                     
045100     MOVE 'W42606D3' TO  POSTSUM-DDNAMN2                                  
045200     CALL POSTSUM USING POSTSUM-PARM                                      
045300     .                                                                    
045400     EJECT                                                                
045500 Z-FINIT   SECTION.                                                       
045600     SKIP2                                                                
045700     CLOSE W42604 W01172 W01184 W42606                                    
045800     .                                                                    
045900     EJECT                                                                
046000* IMS SECTIONER                                                           
046100     SKIP2                                                                
046200 IMS-GHU-W6KVAB01 SECTION.                                                
046300     STRING 'W6KVAB01(W6H601KY =' W-W6H601KY-X ')'                        
046400          DELIMITED BY SIZE INTO SSA1                                     
046500     MOVE '  GE' TO GODK-STATUSKODER                                      
046600     CALL CBLTDLI USING GHU W6H6-PCB DLI-IO-AREA1 SSA1                    
046700     MOVE W6H6-STATUS-CODE TO STATUS-WS                                   
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     .                                                                    
047000     SKIP2                                                                
047100 IMS-REPL-W6KVAB01 SECTION.                                               
047200     MOVE '  ' TO GODK-STATUSKODER                                        
047300     CALL CBLTDLI USING REPL W6H6-PCB DLI-IO-AREA1                        
047400     MOVE W6H6-STATUS-CODE TO STATUS-WS                                   
047500     PERFORM IMS-STATUSKONTROLL                                           
047600     .                                                                    
047700     SKIP2                                                                
047800 IMS-GU-WLBENA11 SECTION.                                                 
047900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
048000         DELIMITED BY SIZE INTO SSA1                                      
048100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
048200         DELIMITED BY SIZE INTO SSA2                                      
048300     MOVE '  ' TO GODK-STATUSKODER                                        
048400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA2 SSA1 SSA2                
048500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
048600     PERFORM IMS-STATUSKONTROLL                                           
048700     .                                                                    
048800     SKIP2                                                                
048900 IMS-GET-ARTC11 SECTION.                                                  
049000                                                                          
049100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
049200          DELIMITED BY SIZE INTO SSA1                                     
049300     MOVE   'WLARTC11 '       TO SSA2                                     
049400     MOVE '  GE'                TO GODK-STATUSKODER                       
049500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
049600     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
049700     PERFORM IMS-STATUSKONTROLL                                           
049800     .                                                                    
049900     SKIP2                                                                
050000 IMS-STATUSKONTROLL SECTION.                                              
050100     SET STATUS-IX TO 1                                                   
050200     SEARCH GODK-STATUS                                                   
050300       AT END CALL FELLOG                                                 
050400       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
050500     END-SEARCH                                                           
050600     .                                                                    
