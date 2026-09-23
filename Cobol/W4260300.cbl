000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4260300.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           MARS 1990.                                       
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
001000*        UPPDATERING AV LAGERKONTROLL.                                    
001100*        SLUMPMÄSSIGT VÄLJS KONTROLLOMRÅDE OCH KONTROLLGRUPP UT           
001200*        FRÅN LAGERINDELNING (W6H5) OCH UPPDATERAS PÅ W6H6.               
001300*        ANTAL URVAL HÄMTAS FRÅN COPYTEXT WWKVAOMR.                       
001310*                                                                         
001620*        KONTROLLER SKER PÅ CDC + ALLA SDC:ER                             
001700*                                                                         
001800*        INDATA. W6H5                                                     
001900*                                                                         
002000*        UTDATA. W6H6                                                     
002100*                                                                         
002200*        UPPDATERA W6H6                                                   
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003710                                                                          
003720*    -- CHECKED BY WY2000                                                 
003730*                                                                         
003800*    ---- ARBETSVARIABLER                                                 
003900*                                                                         
004000 01  MAX-TAL                 PIC S9(09) COMP.                             
004100 01  SLUMP-TAL               PIC S9(09) COMP.                             
004200                                                                          
004300 77  FELTEXT                PIC  X(80).                                   
004400 77  W-KVKVAURV             PIC  9(01).                                   
004700 77  W-W6KVAB01-UPPD        PIC X(01).                                    
004800 77  WS-IDKVATRG            PIC 9(02) VALUE ZERO.                         
004900 77  WS-MIN-IDKVAGRP        PIC 9(03) VALUE ZERO.                         
005000 77  WS-MAX-IDKVAGRP        PIC 9(03) VALUE ZERO.                         
005100 77  WS-GRUPP               PIC 9(03) VALUE ZERO.                         
005200 77  ANTAL-TORG             PIC S9(2) VALUE +0.                           
005300 77  ANTAL-GRUPP            PIC S9(3) VALUE +0.                           
005400 77  RAKNARE                PIC S9(4) VALUE +0.                           
005410                                                                          
005500 01  IDDC-TAB.                                                            
005510     03 FILLER              PIC X(12) VALUE '112122232425'.               
005530 01  FILLER  REDEFINES IDDC-TAB.                                          
005531     03  WS-IDDC            PIC XX    OCCURS 6.                           
005540                                                                          
005600 77  JA                     PIC X(01) VALUE 'J'.                          
005700 77  NEJ                    PIC X(01) VALUE 'N'.                          
005800                                                                          
005900*    ---- INDEXFÄLT                                                       
006000                                                                          
006100 77  IX1-URV                 PIC S9(3)   VALUE +6   COMP SYNC.            
006200 77  IX1                     PIC S9(3)   VALUE +0   COMP SYNC.            
006300 77  IX2                     PIC S9(3)   VALUE +0   COMP SYNC.            
006510 77  DCIX                    PIC S9(9)   VALUE +0   COMP-3.               
006520                                                                          
006600     EJECT                                                                
006700*    -COPY WWKVAOMR                                                       
006900     EJECT                                                                
007000*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
007100     SKIP3                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
007400   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
007500   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007600   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007700   03  WRANDOM               PIC X(8)    VALUE 'WRANDOM '.                
007800     SKIP3                                                                
007900*    ---- PARAMETRAR TILL ABEND                                           
008000 01  RETURKODER.                                                          
008100   03  RKOD-ABEND-UTAN-DUMP  PIC S9(04) COMP SYNC VALUE +16.              
008200     EJECT                                                                
008300*    ----  PARAMETRAR TILL DATUMKORT                                      
008400                                                                          
008500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
008600     SKIP3                                                                
008700*    -COPY WDATAREA                                                       
008900     EJECT                                                                
009000*    ---- POST-AREOR OCH IMS KOMMUNIKATIONS-AREOR                         
009100     SKIP3                                                                
009200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
009300                                                                          
009400 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
009500     SKIP3                                                                
009600*    ---- STATUSKOD FRÅN IMS                                              
009700                                                                          
009800 01  STATUS-WS               PIC XX.                                      
009900     88  SEGMENT-FINNS                    VALUE '  '.                     
010000     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
010100     88  SEGMENT-SLUT                     VALUE 'GB'.                     
010200     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
010300     SKIP3                                                                
010400 01  GODK-STATUSKODER.                                                    
010500   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
010600     SKIP3                                                                
010700 01  SSA1                    PIC X(224).                                  
010800 01  SSA2                    PIC X(64).                                   
010900     EJECT                                                                
011000*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
011100                                                                          
011200 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
011300 01  NYCKLAR-TILL-DLI.                                                    
011400                                                                          
011500   03  W-W6H501KY-MIN1-X.                                                 
011600      05  W-IDDC-MIN1         PIC  X(02)  VALUE SPACE.                    
011700      05  W-IDKVAOMR-MIN1     PIC  X(01)  VALUE SPACE.                    
011800      05  W-IDKVATRG-MIN1     PIC  9(02)  VALUE ZERO.                     
011900      05  W-IDKVAGRP-MIN1     PIC  9(03)  VALUE ZERO.                     
012000      05  W-ADLAGOMR-FOM-MIN1 PIC S9(03)  VALUE +0   COMP-3.              
012100      05  W-ADGANG-FOM-MIN1   PIC S9(03)  VALUE +0   COMP-3.              
012200      05  W-ADPLATS-FOM-MIN1  PIC S9(05)  VALUE +0   COMP-3.              
012300     SKIP3                                                                
012400   03  W-W6H501KY-MAX1-X.                                                 
012500      05  W-IDDC-MAX1         PIC  X(02)  VALUE SPACE.                    
012600      05  W-IDKVAOMR-MAX1     PIC  X(01)  VALUE SPACE.                    
012700      05  W-IDKVATRG-MAX1     PIC  9(02)  VALUE ZERO.                     
012800      05  W-IDKVAGRP-MAX1     PIC  9(03)  VALUE ZERO.                     
012900      05  W-ADLAGOMR-FOM-MAX1 PIC S9(03)  VALUE +0   COMP-3.              
013000      05  W-ADGANG-FOM-MAX1   PIC S9(03)  VALUE +0   COMP-3.              
013100      05  W-ADPLATS-FOM-MAX1  PIC S9(05)  VALUE +0   COMP-3.              
013200     SKIP3                                                                
013300   03  W-W6H501KY-MIN-X.                                                  
013310      05  W-IDDC-MIN          PIC  X(02)  VALUE SPACE.                    
013500      05  W-IDKVAOMR-MIN     PIC  X(01)  VALUE SPACE.                     
013600      05  W-IDKVATRG-MIN     PIC  9(02)  VALUE ZERO.                      
013700      05  W-IDKVAGRP-MIN     PIC  9(03)  VALUE ZERO.                      
013800      05  W-ADLAGOMR-FOM-MIN PIC S9(03)  VALUE +0   COMP-3.               
013900      05  W-ADGANG-FOM-MIN   PIC S9(03)  VALUE +0   COMP-3.               
014000      05  W-ADPLATS-FOM-MIN  PIC S9(05)  VALUE +0   COMP-3.               
014100     SKIP3                                                                
014200   03  W-W6H501KY-MAX-X.                                                  
014210      05  W-IDDC-MAX          PIC  X(02)  VALUE SPACE.                    
014400      05  W-IDKVAOMR-MAX     PIC  X(01)  VALUE HIGH-VALUE.                
014500      05  W-IDKVATRG-MAX     PIC  9(02)  VALUE 99.                        
014600      05  W-IDKVAGRP-MAX     PIC  9(03)  VALUE 999.                       
014700      05  W-ADLAGOMR-FOM-MAX PIC S9(03)  VALUE +999  COMP-3.              
014800      05  W-ADGANG-FOM-MAX   PIC S9(03)  VALUE +999  COMP-3.              
014900      05  W-ADPLATS-FOM-MAX  PIC S9(05)  VALUE +99999 COMP-3.             
015000     EJECT                                                                
015100*01  -COPY W0003                                                          
015300     EJECT                                                                
015400 01  FILLER                  PIC X(16) VALUE 'LOGG-RECORD'.               
015500     SKIP2                                                                
015600*    -COPY WDGZ01                                                         
015800     EJECT                                                                
015900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA1'.              
016000     SKIP3                                                                
016100 01  DLI-IO-AREA1.                                                        
016200   03  IO-AREA1              PIC X(25).                                   
016300     SKIP3                                                                
016400*    03 W6H501 -COPY W6H501         -RED IO-AREA1.                        
016500     EJECT                                                                
016600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
016700     SKIP3                                                                
016800 01  DLI-IO-AREA2.                                                        
016900   03  IO-AREA2              PIC X(25).                                   
017000     SKIP3                                                                
017100*    03 W6H501 -COPY W6H501     -PRE A2-    -RED IO-AREA2.                
017200     EJECT                                                                
017300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA3'.              
017400     SKIP3                                                                
017500 01  DLI-IO-AREA3.                                                        
017600   03  IO-AREA3              PIC X(27).                                   
017700*    03 W6H601 -COPY W6H601         -RED IO-AREA3.                        
017800     EJECT                                                                
017900 LINKAGE SECTION.                                                         
018000     SKIP2                                                                
018100*01  -COPY W0009      -PRE  MSG-                                          
018300     EJECT                                                                
018400*01  -COPY W0008      -PRE  W6H5-                                         
018600       05  FILLER                PIC X.                                   
018700     EJECT                                                                
018800*01  -COPY W0008      -PRE  W6H5-ALT-                                     
019000       05  FILLER                PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008      -PRE  W6H6-                                         
019400       05  FILLER                PIC X.                                   
019500     EJECT                                                                
019600 PROCEDURE DIVISION  USING MSG-PCB W6H5-PCB W6H5-ALT-PCB W6H6-PCB.        
019700     ENTRY 'DLITCBL' USING MSG-PCB W6H5-PCB W6H5-ALT-PCB W6H6-PCB.        
019800                                                                          
019900     PERFORM A-INIT                                                       
020000                                                                          
020100     PERFORM B-BEHANDLING                                                 
020200                                                                          
020300     IF RAKNARE > +1000                                                   
020400       MOVE 'PROGRAMMET LOOPAR' TO FELTEXT                                
020500       CALL FELLOG                                                        
020600     END-IF                                                               
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300     SKIP2                                                                
021400     MOVE 'IDAG'       TO DAT-KDDATFORM                                   
021500     CALL WDATKONV USING  DAT-KDDATFORM                                   
021600                          DAT-I-TIDATUM                                   
021700                          DAT-O-TIDATUM                                   
021800                          DAT-KDSVAR                                      
021900                                                                          
022000     MOVE 'AARP'       TO DAT-KDDATFORM                                   
022100     MOVE DAT-TIAARP   TO DAT-I-TIDATUM                                   
022200     CALL WDATKONV USING  DAT-KDDATFORM                                   
022300                          DAT-I-TIDATUM                                   
022400                          DAT-O-TIDATUM                                   
022500                          DAT-KDSVAR                                      
022600                                                                          
022800     .                                                                    
022900     EJECT                                                                
023000 B-BEHANDLING SECTION.                                                    
023100     SKIP2                                                                
023200     MOVE  +1   TO DCIX                                                   
023300**** PERFORM UNTIL DCIX > +6  SKALL BARA KÖRA PÅ IDDC 11 TILLSV.          
023301****                        TILLS ÖVRIGA HAR BESTÄMT LAGEROMRÅDEN         
023302****                        OCH WWKVAOMR ÄR UPPDATERAD                    
023310     PERFORM UNTIL DCIX > +1                                              
023400       MOVE +1  TO IX1                                                    
023500       PERFORM UNTIL IX1 > IX1-URV OR RAKNARE > +1000                     
023700         MOVE KVA-KVKVAURV (DCIX IX1) TO W-KVKVAURV                       
024100                                                                          
024200         MOVE LOW-VALUE  TO W-W6H501KY-MIN-X   W-W6H501KY-MIN1-X          
024400         MOVE HIGH-VALUE TO W-W6H501KY-MAX-X   W-W6H501KY-MAX1-X          
024700         MOVE WS-IDDC (DCIX)      TO W-IDDC-MIN1   W-IDDC-MAX1            
024900                                     W-IDDC-MIN    W-IDDC-MAX             
025100         MOVE KVA-IDKVAOMR (DCIX IX1) TO W-IDKVAOMR-MIN1                  
025200                                         W-IDKVAOMR-MAX1                  
025300                                         W-IDKVAOMR-MIN                   
025400                                         W-IDKVAOMR-MAX                   
025600         PERFORM BA-TOTALT-ANTAL-URVAL                                    
025700                                                                          
025800         MOVE +1   TO IX2                                                 
025900         PERFORM IMS-GU-W6KVAA01-MIN-MAX-ALT                              
025901                                                                          
025910         IF ANTAL-TORG = +1                                               
025920           PERFORM S01A-KOLLA-GRUPPINDELNING                              
025930         END-IF                                                           
025940                                                                          
026100         PERFORM UNTIL IX2 > W-KVKVAURV OR RAKNARE > +1000                
026200           IF ANTAL-TORG NOT = +1                                         
026300             PERFORM S01-SLUMPURVAL                                       
026400             PERFORM IMS-GU-W6KVAA01-MIN-MAX                              
026500             MOVE NEJ                  TO W-W6KVAB01-UPPD                 
026600             PERFORM UNTIL W-W6KVAB01-UPPD = JA OR RAKNARE > +1000        
026700               IF SEGMENT-FINNS                                           
026800                  MOVE GRP-IDDC        TO OMR-IDDC                        
026900                  MOVE GRP-IDKVAOMR    TO OMR-IDKVAOMR                    
027100                  MOVE GRP-IDKVAGRP    TO OMR-IDKVAGRP                    
027300                  MOVE DAT-TIAAMMDD    TO OMR-DAREGDAT                    
027310                  MOVE DAT-TISEKEL     TO OMR-DAREGDAT (1:2)              
027400                  MOVE +0              TO OMR-TIKVAKON                    
027500                  MOVE +0              TO OMR-KVART                       
027600                  MOVE ZERO            TO OMR-KDKVASTA                    
027700                  MOVE +0              TO OMR-TIUPPDAT                    
027800                  PERFORM IMS-ISRT-W6KVAB01                               
027900                  IF SEGMENT-FINNS-REDAN                                  
028000                     PERFORM S02-NYTT-SLUMPTAL                            
028100                     PERFORM IMS-GU-W6KVAA01-MIN-MAX                      
028200                  ELSE                                                    
028300                     MOVE JA           TO W-W6KVAB01-UPPD                 
028400                  END-IF                                                  
028500               ELSE                                                       
028600                  PERFORM S02-NYTT-SLUMPTAL                               
028700                  PERFORM IMS-GU-W6KVAA01-MIN-MAX                         
028800               END-IF                                                     
028900               ADD +1 TO RAKNARE                                          
029000             END-PERFORM                                                  
029100           ELSE                                                           
029200             PERFORM BB-URVAL-ETT-TORG                                    
029300           END-IF                                                         
029400           ADD +1                    TO IX2                               
029500           ADD +1 TO RAKNARE                                              
029600         END-PERFORM                                                      
029700         ADD +1                      TO IX1                               
029800         ADD +1 TO RAKNARE                                                
029900       END-PERFORM                                                        
030000       ADD +1    TO DCIX                                                  
030100     END-PERFORM                                                          
030200     .                                                                    
030300     EJECT                                                                
030400 BA-TOTALT-ANTAL-URVAL SECTION.                                           
030600     SKIP2                                                                
030800     MOVE +1                       TO W-IDKVAGRP-MIN                      
030900     MOVE +999                     TO W-IDKVAGRP-MAX                      
031000     PERFORM IMS-GU-W6KVAA01-MIN-MAX-ALT                                  
031010*W6H5                                                                     
031100     MOVE A2-GRP-IDKVATRG TO WS-IDKVATRG                                  
031200                                                                          
031300     MOVE +1                       TO ANTAL-TORG                          
031400     MOVE +0                       TO ANTAL-GRUPP                         
031500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
031600       RAKNARE > +1000                                                    
031700       IF A2-GRP-IDKVATRG NOT = WS-IDKVATRG                               
031800         ADD +1                      TO ANTAL-TORG                        
031900       END-IF                                                             
032000       ADD +1                        TO ANTAL-GRUPP                       
032100       MOVE A2-GRP-IDKVATRG TO WS-IDKVATRG                                
032200       PERFORM IMS-GN-W6KVAA01-MIN-MAX-ALT                                
032300       ADD +1 TO RAKNARE                                                  
032400     END-PERFORM                                                          
032500                                                                          
032600     IF ANTAL-TORG > +1                                                   
032700       IF ANTAL-TORG < W-KVKVAURV                                         
032800          DISPLAY  '*** W4260300 ANT URVAL I COPY-TEXT WWKVAOMR'          
032900          DISPLAY  'STÖRRE ÄN ANTAL UPPDATERADE K-TRG BILD 4121 *'        
033000          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
033100       END-IF                                                             
033200     ELSE                                                                 
033300       IF ANTAL-GRUPP < W-KVKVAURV                                        
033400          DISPLAY  '*** W4260300 ANT URVAL I COPY-TEXT WWKVAOMR'          
033500          DISPLAY  'STÖRRE ÄN ANTAL UPPDATERADE K-GRP BILD 4121 *'        
033600          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
033700       END-IF                                                             
033800     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 BB-URVAL-ETT-TORG SECTION.                                               
035000                                                                          
035100     COMPUTE MAX-TAL = WS-MAX-IDKVAGRP - WS-MIN-IDKVAGRP                  
035200     MOVE NEJ TO W-W6KVAB01-UPPD                                          
035300     PERFORM IMS-GU-W6KVAA01-MIN-MAX                                      
035400     PERFORM UNTIL W-W6KVAB01-UPPD = JA                                   
035500       CALL WRANDOM USING MAX-TAL SLUMP-TAL                               
035800       COMPUTE WS-GRUPP = WS-MIN-IDKVAGRP + SLUMP-TAL                     
035900       MOVE GRP-IDDC        TO OMR-IDDC                                   
036000       MOVE GRP-IDKVAOMR    TO OMR-IDKVAOMR                               
036200       MOVE WS-GRUPP        TO OMR-IDKVAGRP                               
036400       MOVE DAT-TIAAMMDD    TO OMR-DAREGDAT                               
036410       MOVE DAT-TISEKEL     TO OMR-DAREGDAT (1:2)                         
036500       MOVE +0              TO OMR-TIKVAKON                               
036600       MOVE +0              TO OMR-KVART                                  
036700       MOVE ZERO            TO OMR-KDKVASTA                               
036800       MOVE +0              TO OMR-TIUPPDAT                               
036900       PERFORM IMS-ISRT-W6KVAB01                                          
037000       IF SEGMENT-FINNS-REDAN                                             
037100          CONTINUE                                                        
037200       ELSE                                                               
037300          MOVE JA           TO W-W6KVAB01-UPPD                            
037400       END-IF                                                             
037500     END-PERFORM                                                          
037600                                                                          
037800     .                                                                    
037900     EJECT                                                                
038000 S01-SLUMPURVAL  SECTION.                                                 
038200     SKIP2                                                                
038300     PERFORM S01A-KOLLA-GRUPPINDELNING                                    
038400     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
038500       IF A2-GRP-IDKVATRG = +1 AND (IX2 < W-KVKVAURV)                     
038700         PERFORM S02-NYTT-SLUMPTAL                                        
038800       ELSE                                                               
038900         IF WS-MAX-IDKVAGRP = (WS-MIN-IDKVAGRP + 1)                       
039200           MOVE WS-MAX-IDKVAGRP TO WS-GRUPP                               
039300         ELSE                                                             
039400           COMPUTE MAX-TAL = WS-MAX-IDKVAGRP - WS-MIN-IDKVAGRP            
039500           CALL WRANDOM USING MAX-TAL SLUMP-TAL                           
039800           COMPUTE WS-GRUPP = WS-MIN-IDKVAGRP + SLUMP-TAL                 
039900         END-IF                                                           
040000         MOVE WS-GRUPP             TO W-IDKVAGRP-MIN1                     
040100                                      W-IDKVAGRP-MAX1                     
040200       END-IF                                                             
040300     ELSE                                                                 
040400       IF WS-MAX-IDKVAGRP = (WS-MIN-IDKVAGRP + 1)                         
040500         MOVE WS-MAX-IDKVAGRP TO WS-GRUPP                                 
040800       ELSE                                                               
040900         COMPUTE MAX-TAL = WS-MAX-IDKVAGRP - WS-MIN-IDKVAGRP              
041000         CALL WRANDOM USING MAX-TAL SLUMP-TAL                             
041300         COMPUTE WS-GRUPP = WS-MIN-IDKVAGRP + SLUMP-TAL                   
041400       END-IF                                                             
041500       MOVE WS-GRUPP             TO W-IDKVAGRP-MIN1                       
041600                                    W-IDKVAGRP-MAX1                       
041700     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100 S01A-KOLLA-GRUPPINDELNING SECTION.                                       
042200                                                                          
042400     IF SEGMENT-FINNS                                                     
042500       MOVE A2-GRP-IDKVAGRP TO WS-MIN-IDKVAGRP                            
042600       COMPUTE WS-MIN-IDKVAGRP = A2-GRP-IDKVAGRP - 1                      
042700       MOVE A2-GRP-IDKVATRG TO WS-IDKVATRG                                
042800       PERFORM UNTIL (A2-GRP-IDKVATRG NOT = WS-IDKVATRG) OR               
042900         SEGMENT-SAKNAS OR SEGMENT-SLUT OR RAKNARE > +1000                
043000         MOVE A2-GRP-IDKVATRG TO WS-IDKVATRG                              
043100                                  W-IDKVATRG-MIN1                         
043200                                  W-IDKVATRG-MAX1                         
043300         MOVE A2-GRP-IDKVAGRP TO WS-MAX-IDKVAGRP                          
043400         PERFORM IMS-GN-W6KVAA01-MIN-MAX-ALT                              
043500         ADD +1 TO RAKNARE                                                
043600       END-PERFORM                                                        
043700     END-IF                                                               
044000                                                                          
044200     .                                                                    
044300     EJECT                                                                
044400 S02-NYTT-SLUMPTAL SECTION.                                               
044500                                                                          
044700                                                                          
044800     CALL WRANDOM USING MAX-TAL SLUMP-TAL                                 
045100     COMPUTE WS-GRUPP = WS-MIN-IDKVAGRP + SLUMP-TAL                       
045200     MOVE WS-GRUPP             TO W-IDKVAGRP-MIN1                         
045300                                  W-IDKVAGRP-MAX1                         
045400                                                                          
045600     .                                                                    
045700     EJECT                                                                
045800*    ---- IMS SEKTIONER                                                   
045900                                                                          
046000 IMS-GU-W6KVAA01-MIN-MAX SECTION.                                         
046100                                                                          
046200     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN1-X                       
046300                    '&W6H501KY=<' W-W6H501KY-MAX1-X ')'                   
046400          DELIMITED BY SIZE INTO SSA1                                     
046500     MOVE '  GE'                TO GODK-STATUSKODER                       
046600     CALL CBLTDLI USING GU W6H5-PCB DLI-IO-AREA1 SSA1                     
046700     MOVE W6H5-STATUS-CODE      TO STATUS-WS                              
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     .                                                                    
047000     EJECT                                                                
047100 IMS-GU-W6KVAA01-MIN-MAX-ALT SECTION.                                     
047200                                                                          
047300     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
047400                    '&W6H501KY=<' W-W6H501KY-MAX-X ')'                    
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     MOVE '  '              TO GODK-STATUSKODER                           
047700     CALL CBLTDLI USING GU W6H5-ALT-PCB DLI-IO-AREA2 SSA1                 
047800     MOVE W6H5-ALT-STATUS-CODE      TO STATUS-WS                          
047900     PERFORM IMS-STATUSKONTROLL                                           
048000     .                                                                    
048100     SKIP3                                                                
048200 IMS-GN-W6KVAA01-MIN-MAX-ALT SECTION.                                     
048300                                                                          
048400     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
048500                    '&W6H501KY=<' W-W6H501KY-MAX-X ')'                    
048600          DELIMITED BY SIZE INTO SSA1                                     
048700     MOVE '  GEGB'              TO GODK-STATUSKODER                       
048800     CALL CBLTDLI USING GN W6H5-ALT-PCB DLI-IO-AREA2 SSA1                 
048900     MOVE W6H5-ALT-STATUS-CODE      TO STATUS-WS                          
049000     PERFORM IMS-STATUSKONTROLL                                           
049100     .                                                                    
049200     SKIP3                                                                
049300 IMS-ISRT-W6KVAB01 SECTION.                                               
049400                                                                          
049500     MOVE 'W6KVAB01 '           TO SSA2                                   
049600     MOVE 'II'                  TO GODK-STATUSKODER                       
049700     CALL CBLTDLI USING ISRT  W6H6-PCB DLI-IO-AREA3 SSA2                  
049800     MOVE W6H6-STATUS-CODE      TO STATUS-WS                              
049900     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
050100     SKIP3                                                                
050200 IMS-STATUSKONTROLL SECTION.                                              
050300                                                                          
050400     SET STATUS-IX TO 1                                                   
050500     SEARCH GODK-STATUS                                                   
050600       AT END CALL FELLOG                                                 
050700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050800         CONTINUE                                                         
050900     END-SEARCH                                                           
051000     .                                                                    
