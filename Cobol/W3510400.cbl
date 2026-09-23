000100 ID DIVISION.                                                             
000200 PROGRAM-ID.       W3510400.                                              
000300 AUTHOR.           RICHARD.                                               
000400 DATE-WRITTEN.     JULI 1987.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       INPOSTER FRÅN VR/DSP KOLLAS                                       
000900*       OCH GODKÄNNS OM KVLS, LEVERERAT I ÅR OCH                          
001000*       FÖRRA ÅRET SAMT BESTÄLLD KVANT INTE ÄR NOLL.                      
001100*       GODKÄNDA POSTER SORTERAS OCH SUMMERAS OCH                         
001200*       AV DESSA SUMMAPOSTER  SKAPAS LADDTRANSAR                          
001300*       TILL BASEN WDK8.                                                  
001400*                                                                         
001500*       SUMMERING SKER PÅ TVÅ NIVÅER OCH LADDTRANSAR SKAPAS:              
001600*          1 TOTALT PER ARTIKELNUMMER.                                    
001700*          2 PER ARTNR OCH AIGRUPP.                                       
001800*                                                                         
001900*       4   UTFILER SKAPAS:                                               
002000*         - TOTALER ENLIGT 1 OCH 2 OVAN                                   
002100*           (W35105)                                                      
002200*                                                                         
002300*         - KUNDPOSTER                                                    
002400*           (W35106)                                                      
002500*                                                                         
002600*         - TOTALER ENLIGT 1 OCH 2 OVAN, SOM STÄMMER MED                  
002700*           ARTIKELREGISTRET I TEST. (TESTARTG)                           
002800*           (W35107)                                                      
002900*                                                                         
003000*         - KUNDPOSTER TEST                                               
003100*           (W35108)                                                      
003200*                                                                         
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900                                                                          
004000*    ---- INFIL:                                                          
004100*                            - FSGPOSTER FRÅN DSP (VCOM)                  
004200     SELECT  W35111        ASSIGN  W35104D1.                              
004300                                                                          
004400*                            - FÖRSÄLJNINGSPOSTER FRÅN VR                 
004500     SELECT  W35113        ASSIGN  W35104D2.                              
004600                                                                          
004700*                            - ARTIKLAR PÅ TESTREGISTRET                  
004800     SELECT  WTESTART      ASSIGN  W35104D3.                              
004900                                                                          
005000                                                                          
005100*    ---- UTFIL:                                                          
005200*                            - LADDPOSTER TILL WDK8:                      
005300*                            - SUMMERINGAR                                
005400     SELECT  W35105        ASSIGN  W35104D5.                              
005500                                                                          
005600*                            - LADDPOSTER TILL WDK8: KUNDPOSTER           
005700     SELECT  W35106        ASSIGN  W35104D6.                              
005800                                                                          
005900*                            - LADDPOSTER TILL TEST-WDK8:                 
006000*                            - SUMMERINGAR                                
006100     SELECT  W35107        ASSIGN  W35104D7.                              
006200                                                                          
006300*                            - LADDPOSTER TILL TEST-WDK8:                 
006400*                            - KUNDPOSTER                                 
006500     SELECT  W35108        ASSIGN  W35104D8.                              
006600                                                                          
006700*    ---- SORTFIL:                                                        
006800*                                                                         
006900     SELECT  SORTFIL       ASSIGN  W35104DS.                              
007000     EJECT                                                                
007100 DATA DIVISION.                                                           
007200                                                                          
007300 FILE SECTION.                                                            
007400                                                                          
007500 FD  W35111                                                               
007600     LABEL RECORD STANDARD                                                
007700     RECORDING  V                                                         
007800     BLOCK CONTAINS 0.                                                    
007900*    -COPY W016001A -L.                                                   
008000*    -COPY W351111A -L.                                                   
008100                                                                          
008200                                                                          
008300 FD  W35113                                                               
008400     LABEL RECORD STANDARD                                                
008500     RECORDING  F                                                         
008600     BLOCK CONTAINS 0.                                                    
008700*    -COPY W351VR    -L.                                                  
008800                                                                          
008900                                                                          
009000 FD  WTESTART                                                             
009100     LABEL RECORD STANDARD                                                
009200     RECORDING  F                                                         
009300     BLOCK CONTAINS 0.                                                    
009400 01  FILLER.                                                              
009500     03  TESTART             PIC S9(9)              COMP-3.               
009600     03  FILLER              PIC X(75).                                   
009700                                                                          
009800     EJECT                                                                
009900 FD  W35105                                                               
010000     LABEL RECORD STANDARD                                                
010100     RECORDING  F                                                         
010200     BLOCK CONTAINS 0.                                                    
010300*01  POST1  -COPY WDK801     -L.                                          
010400                                                                          
010500                                                                          
010600 FD  W35106                                                               
010700     LABEL RECORD STANDARD                                                
010800     RECORDING  F                                                         
010900     BLOCK CONTAINS 0.                                                    
011000*01  POST2  -COPY WDK801     -L.                                          
011100                                                                          
011200                                                                          
011300 FD  W35107                                                               
011400     LABEL RECORD STANDARD                                                
011500     RECORDING  F                                                         
011600     BLOCK CONTAINS 0.                                                    
011700*01  POST3  -COPY WDK801     -L.                                          
011800                                                                          
011900                                                                          
012000 FD  W35108                                                               
012100     LABEL RECORD STANDARD                                                
012200     RECORDING  F                                                         
012300     BLOCK CONTAINS 0.                                                    
012400*01  POST4  -COPY WDK801     -L.                                          
012500                                                                          
012600     EJECT                                                                
012700                                                                          
012800 SD  SORTFIL.                                                             
012900*01  POST -PRE SORT- -COPY WDK801.                                        
013000                                                                          
013100     EJECT                                                                
013200 WORKING-STORAGE SECTION.                                                 
013201                                                                          
013210*    -- CHECKED BY WY2000                                                 
013300 77  IDPGM                   PIC X(8) VALUE 'W3510400'.                   
013400 77  JA                      PIC X       VALUE 'J'.                       
013500 77  NEJ                     PIC X       VALUE 'N'.                       
013600 77  ANT-VR                  PIC S9(9)   VALUE ZERO COMP-3.               
013700 77  ANT-DSP                 PIC S9(9)   VALUE ZERO COMP-3.               
013800 77  ANT-VCOM                PIC S9(9)   VALUE ZERO COMP-3.               
013900 77  ANT-SORT                PIC S9(9)   VALUE ZERO COMP-3.               
014000 77  ANT-TESTART             PIC S9(9)   VALUE ZERO COMP-3.               
014100 77  ANT-TOT                 PIC S9(9)   VALUE ZERO COMP-3.               
014200 77  ANT-KUND                PIC S9(9)   VALUE ZERO COMP-3.               
014300 77  ANT-TESTTOT             PIC S9(9)   VALUE ZERO COMP-3.               
014400 77  ANT-TESTKUND            PIC S9(9)   VALUE ZERO COMP-3.               
014500 77  ANT-NOLL-DSP            PIC S9(9)   VALUE ZERO COMP-3.               
014600 77  ANT-NOLL-VR             PIC S9(9)   VALUE ZERO COMP-3.               
014700 77  WS-DATE                 PIC 9(6)    VALUE ZERO.                      
014800 77  WS-TIREGDAT             PIC 9(6)    VALUE ZERO.                      
014900                                                                          
015000 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
015100 01  W009CIA                 PIC X(8)    VALUE 'W009CIA '.                
015200 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
015300                                                                          
015400 01  WS-TESTIDART.                                                        
015500   03  TEST-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.               
015600   03  FILLER                PIC X(75)   VALUE SPACE.                     
015700                                                                          
015800 01  SPAR-AREA.                                                           
015900   03  SPAR-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.               
016000   03  SPAR-KDSEGKEY         PIC X       VALUE SPACE.                     
016100   03  SPAR-IDAIGRP          PIC X(3)    VALUE SPACE.                     
016200                                                                          
016300 01  WS-IDAIGRP.                                                          
016400   03  WS-IDLANDX2           PIC X(2)   VALUE SPACE.                      
016500   03  WS-KDAIKTYP           PIC X(1)   VALUE SPACE.                      
016600                                                                          
016700 01  SWITCHAR.                                                            
016800   03  EOF-VR                PIC X       VALUE 'N'.                       
016900   03  EOF-DSP               PIC X       VALUE 'N'.                       
017000   03  EOF-SORT              PIC X       VALUE 'N'.                       
017100   03  EOF-TEST              PIC X       VALUE 'N'.                       
017200                                                                          
017300     EJECT                                                                
017400 01  FILLER                  PIC X(24) VALUE 'W009CIA '.                  
017500*01  -COPY W009CIA                                                        
017600                                                                          
017700     EJECT                                                                
017800 01  FILLER                  PIC X(24) VALUE 'IN-AREA VR-AREA '.          
017900*01  AREA  -COPY W351VR  -PRE  VR-.                                       
018000                                                                          
018100     EJECT                                                                
018200 01  FILLER                  PIC X(24) VALUE 'IN-AREA V-COM   '.          
018300 01  VCOM-IN-AREA.                                                        
018400     03  IN-AREA-VCOM            PIC X(100)  VALUE SPACE.                 
018500                                                                          
018600     03  FILLER REDEFINES IN-AREA-VCOM.                                   
018700*        05  AREA -COPY W351111A  -PRE DSP-                               
018800     EJECT                                                                
018900     03  FILLER REDEFINES IN-AREA-VCOM.                                   
019000*        05  FILLER -COPY W016001A                                        
019100                                                                          
019200     EJECT                                                                
019300*01  AREA   -COPY WDK801    -PRE ZERO-.                                   
019400                                                                          
019500     EJECT                                                                
019600 01  FILLER                  PIC X(24) VALUE 'ART-AREA     '.             
019700*01  AREA   -COPY WDK801    -PRE TOT-.                                    
019800                                                                          
019900     EJECT                                                                
020000 01  FILLER                  PIC X(24) VALUE 'AIGRP-AREA'.                
020100*01  AREA   -COPY WDK801    -PRE AIGRP-.                                  
020200                                                                          
020300     EJECT                                                                
020400 PROCEDURE DIVISION.                                                      
020500 MAIN SECTION.                                                            
020600                                                                          
020700     PERFORM A-INIT                                                       
020800                                                                          
020900     SORT SORTFIL                                                         
021000               ASCENDING  SORT-ART-IDARTNR                                
021100                          SORT-ART-KDSEGKEY                               
021200                          SORT-ART-IDAIGRP                                
021300                          SORT-ART-IDKUNDNR                               
021400                                                                          
021500       INPUT  PROCEDURE B-GODKANN-POSTER-TILL-SORT                        
021600       OUTPUT PROCEDURE C-SKAPA-LADDTRANSAR                               
021700                                                                          
021800     IF  SORT-RETURN > ZERO                                               
021900       DISPLAY '*** W3510400 - FEL VID SORTERING'                         
022000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
022100     ELSE                                                                 
022200       PERFORM Z-FINIT                                                    
022300       MOVE ZERO TO RETURN-CODE                                           
022400       GOBACK                                                             
022500     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT       SECTION.                                                    
022900                                                                          
023000     OPEN INPUT  W35111                                                   
023100                 W35113                                                   
023200                 WTESTART                                                 
023300          OUTPUT W35105                                                   
023400                 W35106                                                   
023500                 W35107                                                   
023600                 W35108                                                   
023700                                                                          
023800     ACCEPT WS-DATE FROM DATE                                             
023900     INITIALIZE   ZERO-AREA                                               
024000     MOVE ZERO-AREA TO TOT-AREA                                           
024100                       AIGRP-AREA                                         
024200     .                                                                    
024300     EJECT                                                                
024400 B-GODKANN-POSTER-TILL-SORT SECTION.                                      
024500                                                                          
024600*--- VR-POSTER ---*                                                       
024700                                                                          
024800     PERFORM S01-LAS-VR-POST                                              
024900     PERFORM UNTIL EOF-VR = JA                                            
025000       IF VR-KVLEVART-INNEV = ZERO AND                                    
025100          VR-KVLEVART-FOREG = ZERO AND                                    
025200          VR-KVBEST = ZERO AND                                            
025300          VR-KVLS = ZERO                                                  
025400            ADD +1 TO ANT-NOLL-VR                                         
025500       ELSE                                                               
025600         PERFORM BA-SKAPA-VR-SORTPOST                                     
025700       END-IF                                                             
025800       PERFORM S01-LAS-VR-POST                                            
025900     END-PERFORM                                                          
026000                                                                          
026100*--- DSP-POSTER ---*                                                      
026200                                                                          
026300     PERFORM S02-LAS-DSP-POST                                             
026400     PERFORM UNTIL EOF-DSP = JA                                           
026500       IF 001A-VC-IDSYSTEM = 'W351'                                       
026600         MOVE 001A-VC-IDLANDX2 TO WS-IDLANDX2                             
026700         MOVE 001A-TIREGDAT    TO WS-TIREGDAT                             
026800         ADD +1 TO ANT-VCOM                                               
026900       ELSE                                                               
027000         IF DSP-KVLEVART-INNEV = ZERO AND                                 
027100            DSP-KVLEVART-FOREG = ZERO AND                                 
027200            DSP-KVBEST = ZERO AND                                         
027300            DSP-KVLS = ZERO                                               
027400              ADD +1 TO ANT-NOLL-DSP                                      
027500         ELSE                                                             
027600           PERFORM BB-SKAPA-DSP-SORTPOST                                  
027700         END-IF                                                           
027800       END-IF                                                             
027900       PERFORM S02-LAS-DSP-POST                                           
028000                                                                          
028100     END-PERFORM                                                          
028200     .                                                                    
028300     EJECT                                                                
028400 BA-SKAPA-VR-SORTPOST SECTION.                                            
028500                                                                          
028600     MOVE 'VO '       TO CIA-IDARTPRE-IN                                  
028700     MOVE VR-IDARTBET TO CIA-IDARTBET-IN                                  
028800     CALL W009CIA USING  CIA-W009CIA                                      
028900     IF CIA-IDARTNR NUMERIC                                               
029000       MOVE CIA-IDARTNR TO SORT-ART-IDARTNR                               
029100     ELSE                                                                 
029200       MOVE ZERO TO SORT-ART-IDARTNR                                      
029300       DISPLAY VR-IDAIGRP ' ' VR-IDARTBET ' NOT NUMERIC IN VR'            
029400     END-IF                                                               
029500     MOVE SPACE                  TO SORT-ART-KDSEGKEY                     
029600     MOVE VR-IDAIGRP             TO SORT-ART-IDAIGRP                      
029700     MOVE VR-IDKUNDNR            TO SORT-ART-IDKUNDNR                     
029800     MOVE VR-KVBEST              TO SORT-ART-KVBEST                       
029900     MOVE VR-KVLS                TO SORT-ART-KVLS                         
030000     MOVE VR-KVROS               TO SORT-ART-KVROS                        
030100     MOVE VR-KVPROGNFSG          TO SORT-ART-KVVRPROGN                    
030200     MOVE VR-KVLEVART(1)         TO SORT-ART-KVLEVART(1)                  
030300     MOVE VR-KVLEVART(2)         TO SORT-ART-KVLEVART(2)                  
030400     MOVE VR-KVLEVART-INNEV      TO SORT-ART-KVLEVART-INNEV               
030500     MOVE VR-KVLEVART-FOREG      TO SORT-ART-KVLEVART-FOREG               
030600     MOVE VR-KVLEVART-F-FOREG    TO SORT-ART-KVLEVART-F-FOREG             
030700     MOVE VR-KDSHELFL            TO SORT-ART-KDSHELFL                     
030800     PERFORM S04-RELEASE-SORTPOST                                         
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 BB-SKAPA-DSP-SORTPOST SECTION.                                           
031300                                                                          
031400     MOVE 'VO '        TO CIA-IDARTPRE-IN                                 
031500     MOVE DSP-IDARTBET TO CIA-IDARTBET-IN                                 
031600     CALL W009CIA USING   CIA-W009CIA                                     
031700     IF CIA-IDARTNR NUMERIC                                               
031800       MOVE CIA-IDARTNR TO SORT-ART-IDARTNR                               
031900     ELSE                                                                 
032000       MOVE ZERO TO SORT-ART-IDARTNR                                      
032100       DISPLAY WS-IDAIGRP ' ' DSP-IDARTBET ' NOT NUMERIC IN DSP'          
032200     END-IF                                                               
032300     MOVE SPACE                  TO SORT-ART-KDSEGKEY                     
032400     MOVE DSP-KDAIKTYP           TO WS-KDAIKTYP                           
032500     MOVE WS-IDAIGRP             TO SORT-ART-IDAIGRP                      
032600     MOVE DSP-IDKUNDNR           TO SORT-ART-IDKUNDNR                     
032700     MOVE DSP-KVBEST             TO SORT-ART-KVBEST                       
032800     MOVE DSP-KVLS               TO SORT-ART-KVLS                         
032900     MOVE DSP-KVROS              TO SORT-ART-KVROS                        
033000     MOVE DSP-KVPROGNFSG         TO SORT-ART-KVVRPROGN                    
033100     MOVE DSP-KVLEVART(1)        TO SORT-ART-KVLEVART(1)                  
033200     MOVE DSP-KVLEVART(2)        TO SORT-ART-KVLEVART(2)                  
033300     MOVE DSP-KVLEVART-INNEV     TO SORT-ART-KVLEVART-INNEV               
033400     MOVE DSP-KVLEVART-FOREG     TO SORT-ART-KVLEVART-FOREG               
033500     MOVE DSP-KVLEVART-F-FOREG   TO SORT-ART-KVLEVART-F-FOREG             
033600     MOVE DSP-KDSHELFL           TO SORT-ART-KDSHELFL                     
033700     PERFORM S04-RELEASE-SORTPOST                                         
033800     .                                                                    
033900                                                                          
034000     EJECT                                                                
034100 C-SKAPA-LADDTRANSAR SECTION.                                             
034200                                                                          
034300     PERFORM S03-LAS-TESTARTIKEL                                          
034400     PERFORM S04-RETURN-SORTPOST                                          
034500     IF EOF-SORT = NEJ                                                    
034600       MOVE SORT-ART-IDARTNR  TO SPAR-IDARTNR                             
034700       MOVE SORT-ART-KDSEGKEY TO SPAR-KDSEGKEY                            
034800       MOVE SORT-ART-IDAIGRP  TO SPAR-IDAIGRP                             
034900     END-IF                                                               
035000                                                                          
035100     PERFORM UNTIL EOF-SORT = JA                                          
035200       IF TEST-IDARTNR < SPAR-IDARTNR                                     
035300         PERFORM S03-LAS-TESTARTIKEL                                      
035400       ELSE                                                               
035500         IF SORT-ART-IDARTNR NOT = SPAR-IDARTNR                           
035600           PERFORM CC-SKAPA-AIGRPPOST                                     
035700           PERFORM CD-SKAPA-ARTIKELPOST-TOT                               
035800           MOVE SORT-ART-IDARTNR  TO SPAR-IDARTNR                         
035900           MOVE SORT-ART-KDSEGKEY TO SPAR-KDSEGKEY                        
036000           MOVE SORT-ART-IDAIGRP  TO SPAR-IDAIGRP                         
036100         ELSE                                                             
036200           IF SORT-ART-IDAIGRP NOT = SPAR-IDAIGRP                         
036300             PERFORM CC-SKAPA-AIGRPPOST                                   
036400             MOVE SORT-ART-IDAIGRP TO SPAR-IDAIGRP                        
036500           END-IF                                                         
036600         END-IF                                                           
036700         PERFORM CB-SKAPA-KUNDPOST                                        
036800         PERFORM CA-ADDERA-ACKAR                                          
036900         PERFORM S04-RETURN-SORTPOST                                      
037000       END-IF                                                             
037100     END-PERFORM                                                          
037200                                                                          
037300     PERFORM CC-SKAPA-AIGRPPOST                                           
037400     PERFORM CD-SKAPA-ARTIKELPOST-TOT                                     
037500     .                                                                    
037600     EJECT                                                                
037700                                                                          
037800 CA-ADDERA-ACKAR SECTION.                                                 
037900                                                                          
038000*--- ART-AREA ==> AREA GRAND TOTAL PER ARTIKEL                            
038100*--- ART-AREA ==> AREA TOTAL PER IDAIGRP (ÄVEN PENTA OCH VME)             
038200                                                                          
038300     ADD SORT-ART-KVBEST        TO AIGRP-ART-KVBEST                       
038400                                   TOT-ART-KVBEST                         
038500     ADD SORT-ART-KVLS          TO AIGRP-ART-KVLS                         
038600                                   TOT-ART-KVLS                           
038700     ADD SORT-ART-KVROS         TO AIGRP-ART-KVROS                        
038800                                   TOT-ART-KVROS                          
038900     ADD SORT-ART-KVLEVART(1)   TO AIGRP-ART-KVLEVART(1)                  
039000                                   TOT-ART-KVLEVART(1)                    
039100     ADD SORT-ART-KVLEVART(2)   TO AIGRP-ART-KVLEVART(2)                  
039200                                   TOT-ART-KVLEVART(2)                    
039300     ADD SORT-ART-KVLEVART-INNEV TO AIGRP-ART-KVLEVART-INNEV              
039400                                   TOT-ART-KVLEVART-INNEV                 
039500     ADD SORT-ART-KVLEVART-FOREG TO AIGRP-ART-KVLEVART-FOREG              
039600                                   TOT-ART-KVLEVART-FOREG                 
039700     ADD SORT-ART-KVLEVART-F-FOREG TO AIGRP-ART-KVLEVART-F-FOREG          
039800                                   TOT-ART-KVLEVART-F-FOREG               
039900     ADD SORT-ART-KVVRPROGN     TO AIGRP-ART-KVVRPROGN                    
040000                                   TOT-ART-KVVRPROGN                      
040100     .                                                                    
040200                                                                          
040300     EJECT                                                                
040400 CB-SKAPA-KUNDPOST SECTION.                                               
040500                                                                          
040600     PERFORM S06-SKRIV-KUNDPOST                                           
040700     IF TEST-IDARTNR = SPAR-IDARTNR                                       
040800       PERFORM S08-SKAPA-TESTKUNDPOST                                     
040900     END-IF                                                               
041000     .                                                                    
041100                                                                          
041200     EJECT                                                                
041300 CC-SKAPA-AIGRPPOST SECTION.                                              
041400                                                                          
041500*--- PENTA OCH VME-POSTER BEHANDLAS PRECIS SOM AI-GRP                     
041600                                                                          
041700     MOVE SPAR-IDARTNR          TO AIGRP-ART-IDARTNR                      
041800     MOVE SPAR-KDSEGKEY         TO AIGRP-ART-KDSEGKEY                     
041900     MOVE SPAR-IDAIGRP          TO AIGRP-ART-IDAIGRP                      
042000     MOVE LOW-VALUE             TO AIGRP-ART-KDAIKTYP                     
042100     MOVE WS-TIREGDAT           TO AIGRP-ART-IDKUNDNR                     
042200     MOVE ZERO                  TO AIGRP-ART-KDSHELFL                     
042300     MOVE AIGRP-AREA            TO POST1 POST3                            
042400     PERFORM S05-SKRIV-ART-AIPOST                                         
042500     IF TEST-IDARTNR = SPAR-IDARTNR                                       
042600       PERFORM S07-SKAPA-TESTART-AIPOST                                   
042700     END-IF                                                               
042800     MOVE ZERO-AREA             TO  AIGRP-AREA                            
042900     .                                                                    
043000                                                                          
043100     EJECT                                                                
043200 CD-SKAPA-ARTIKELPOST-TOT SECTION.                                        
043300                                                                          
043400     MOVE SPAR-IDARTNR          TO TOT-ART-IDARTNR                        
043500     MOVE SPACE                 TO TOT-ART-KDSEGKEY                       
043600     MOVE LOW-VALUE             TO TOT-ART-IDAIGRP                        
043700     MOVE WS-DATE               TO TOT-ART-IDKUNDNR                       
043800     MOVE ZERO                  TO TOT-ART-KDSHELFL                       
043900     MOVE TOT-AREA              TO POST1 POST3                            
044000     PERFORM S05-SKRIV-ART-AIPOST                                         
044100     IF TEST-IDARTNR = SPAR-IDARTNR                                       
044200         PERFORM S07-SKAPA-TESTART-AIPOST                                 
044300     END-IF                                                               
044400     MOVE ZERO-AREA             TO  TOT-AREA                              
044500     .                                                                    
044600                                                                          
044700     EJECT                                                                
044800 S01-LAS-VR-POST SECTION.                                                 
044900                                                                          
045000     READ W35113 INTO VR-AREA                                             
045100       AT END                                                             
045200         MOVE JA TO EOF-VR                                                
045300       NOT AT END                                                         
045400         ADD +1 TO ANT-VR                                                 
045500     END-READ                                                             
045600     .                                                                    
045700                                                                          
045800                                                                          
045900                                                                          
046000 S02-LAS-DSP-POST SECTION.                                                
046100                                                                          
046200     READ W35111 INTO DSP-AREA                                            
046300       AT END                                                             
046400         MOVE JA TO EOF-DSP                                               
046500       NOT AT END                                                         
046600         ADD +1 TO ANT-DSP                                                
046700     END-READ                                                             
046800     .                                                                    
046900                                                                          
047000                                                                          
047100                                                                          
047200 S03-LAS-TESTARTIKEL SECTION.                                             
047300                                                                          
047400     READ WTESTART INTO WS-TESTIDART                                      
047500       AT END                                                             
047600         MOVE JA TO EOF-TEST                                              
047700         MOVE +999999999 TO TEST-IDARTNR                                  
047800       NOT AT END                                                         
047900         ADD +1 TO ANT-TESTART                                            
048000     END-READ                                                             
048100     .                                                                    
048200                                                                          
048300     EJECT                                                                
048400 S04-RELEASE-SORTPOST SECTION.                                            
048500                                                                          
048600     RELEASE SORT-POST                                                    
048700                                                                          
048800     ADD +1 TO ANT-SORT                                                   
048900     .                                                                    
049000                                                                          
049100     SKIP3                                                                
049200 S04-RETURN-SORTPOST SECTION.                                             
049300                                                                          
049400     RETURN SORTFIL                                                       
049500       AT END                                                             
049600         MOVE JA TO EOF-SORT                                              
049700     END-RETURN                                                           
049800     .                                                                    
049900                                                                          
050000     EJECT                                                                
050100 S05-SKRIV-ART-AIPOST SECTION.                                            
050200                                                                          
050300     WRITE POST1                                                          
050400                                                                          
050500     ADD +1 TO ANT-TOT                                                    
050600     .                                                                    
050700                                                                          
050800                                                                          
050900 S06-SKRIV-KUNDPOST SECTION.                                              
051000                                                                          
051100     WRITE POST2 FROM SORT-POST                                           
051200                                                                          
051300     ADD +1 TO ANT-KUND                                                   
051400     .                                                                    
051500                                                                          
051600                                                                          
051700 S07-SKAPA-TESTART-AIPOST SECTION.                                        
051800                                                                          
051900     WRITE POST3                                                          
052000                                                                          
052100     ADD +1 TO ANT-TESTTOT                                                
052200     .                                                                    
052300                                                                          
052400                                                                          
052500 S08-SKAPA-TESTKUNDPOST SECTION.                                          
052600                                                                          
052700     WRITE POST4 FROM SORT-POST                                           
052800                                                                          
052900     ADD +1 TO ANT-TESTKUND                                               
053000     .                                                                    
053100                                                                          
053200     EJECT                                                                
053300 Z-FINIT   SECTION.                                                       
053400                                                                          
053500     CLOSE  W35111                                                        
053600            W35113                                                        
053700            WTESTART                                                      
053800            W35105                                                        
053900            W35106                                                        
054000            W35107                                                        
054100            W35108                                                        
054200                                                                          
054300                                                                          
054400     DISPLAY 'W35104D1 W35111   ANT-VCOM   = ' ANT-VCOM                   
054500     DISPLAY 'W35104D1 W35111   ANT-DSP    = ' ANT-DSP                    
054600     DISPLAY 'W35104D2 W35113   ANT-VR     = ' ANT-VR                     
054700     DISPLAY 'W35104D3 TESTART  ANT-TEST   = ' ANT-TESTART                
054800     DISPLAY 'W35104D4 SORT     ANT-SORT   = ' ANT-SORT                   
054900     DISPLAY 'W35104D5 W35105   ANT-TOT    = ' ANT-TOT                    
055000     DISPLAY 'W35104D6 W35106   ANT-KUND   = ' ANT-KUND                   
055100     DISPLAY 'W35104D7 W35107   ANT-T-TOT  = ' ANT-TESTTOT                
055200     DISPLAY 'W35104D8 W35108   ANT-T-KUND = ' ANT-TESTKUND               
055300     DISPLAY 'W35104D1 W35111   ANT-DSP-0  = ' ANT-NOLL-DSP               
055400     DISPLAY 'W35104D2 W35113   ANT-VR-0   = ' ANT-NOLL-VR                
055500     .                                                                    
