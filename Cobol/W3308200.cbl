000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W3308200.                                    
000300 AUTHOR.                     RONNY STENHOLM.                              
000400 DATE-WRITTEN.               JAN-90.                                      
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION.                                                            
000900                                                                          
001000*    ARTIKELSTATISTIK.                                                    
001100*    BEARBETAR POSTER FRÅN FILEN W33081                                   
001200*    SUMMERAR POSTER MED IDGTYP = 2, PER FUNKTIONSGRUPP                   
001300*    ELLER PRODUKTSLAG OCH SKRIVER SUMMERADE POSTER PÅ UTFILERNA          
001400*    POSTER MED IDGTYP = 1, SKRIVES ÖVER PÅ UTFILERNA UTAN                
001500*    BEARBETNING.                                                         
001600*    URVAL + RESULTAT SKRIVS UT PÅ EN AV TRE UTFILER BEROENDE             
001700*    PÅ VILKET VÄRDE KDNIVA HAR I URVALET                                 
001800*    UTFILER = W33083, W33085, W33087.                                    
001900*                                                                         
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*    --- INFILER:                                                         
002800     SELECT SORTFIL                      ASSIGN TO W33082DS.              
002900     SELECT W33035S                      ASSIGN TO W33082D1.              
003000     SELECT W33081S                      ASSIGN TO W33082D2.              
003100*    --- UTFILER:                                                         
003200     SELECT W33083                       ASSIGN TO W33082D3.              
003300     SELECT W33085                       ASSIGN TO W33082D4.              
003400     SELECT W33087                       ASSIGN TO W33082D5.              
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 SD  SORTFIL                                                              
004100     RECORDING      F                                                     
004200     SKIP2                                                                
004300 01  SORTERAD-POST.                                                       
004400   03  SORT-IDUSER                 PIC X(8).                              
004500   03  SORT-DAREGDAT               PIC X(8).                              
004600   03  SORT-TIREGTID               PIC X(7).                              
004700   03  SORT-FOERSTA                PIC X(5).                              
004800   03  SORT-ANDRA                  PIC X(5).                              
004900   03  SORT-TREDJE                 PIC X(5).                              
005000*  03  POST -COPY W33081  -L -PRE SRT81-.                                 
005200     EJECT                                                                
005300 FD  W33035S                                                              
005400     LABEL RECORD   STANDARD                                              
005500     RECORDING      V                                                     
005600     BLOCK CONTAINS 0.                                                    
005700     SKIP2                                                                
005800*01  -COPY W3303503  -L.                                                  
006000     SKIP2                                                                
006100*01  -COPY W3303502  -L.                                                  
006300     EJECT                                                                
006400 FD  W33081S                                                              
006500     LABEL RECORD   STANDARD                                              
006600     RECORDING      F                                                     
006700     BLOCK CONTAINS 0.                                                    
006800     SKIP2                                                                
006900*01  POST -COPY W33081  -L -PRE I81-.                                     
007100     EJECT                                                                
007200 FD  W33083                                                               
007300     LABEL RECORD   STANDARD                                              
007400     RECORDING      V                                                     
007500     BLOCK CONTAINS 0.                                                    
007510     SKIP2                                                                
007520*01  POST -COPY W3303502  -L -PRE U283-.                                  
007600     SKIP2                                                                
008000*01  POST -COPY W33083  -L -PRE U083-.                                    
008500     EJECT                                                                
008600 FD  W33085                                                               
008700     LABEL RECORD   STANDARD                                              
008800     RECORDING      V                                                     
008900     BLOCK CONTAINS 0.                                                    
009000     SKIP2                                                                
009100*01  POST -COPY W3303502  -L -PRE U285-.                                  
009300     SKIP2                                                                
009400*01  POST -COPY W33083  -L -PRE U085-.                                    
009600     EJECT                                                                
009700 FD  W33087                                                               
009800     LABEL RECORD   STANDARD                                              
009900     RECORDING      V                                                     
010000     BLOCK CONTAINS 0.                                                    
010100     SKIP2                                                                
010200*01  POST -COPY W3303502  -L -PRE U287-.                                  
010400     SKIP2                                                                
010500*01  POST -COPY W33083  -L -PRE U087-.                                    
010700     EJECT                                                                
010800 WORKING-STORAGE SECTION.                                                 
010810                                                                          
010820*  --  CHECKED BY WY2000                                                  
010900     SKIP2                                                                
011000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3308200'.            
011100 77  JA                          PIC X(1)    VALUE 'J'.                   
011200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
011300 77  VP1                         PIC X(3)    VALUE 'VP1'.                 
011400 77  I81-EOF                     PIC X(1)    VALUE 'N'.                   
011500 77  I35-EOF                     PIC X(1)    VALUE 'N'.                   
011600 77  SORT-EOF                    PIC X(1)    VALUE 'N'.                   
011700 77  IX                          PIC S9(4)   VALUE +1  COMP SYNC.         
011800 77  WS-ANTAL-URVAL              PIC S9(4)   VALUE +1  COMP SYNC.         
011900 77  WS-IDRADNR                  PIC S9(4)   VALUE +1  COMP SYNC.         
012000 77  FELKOD                      PIC S9(4)   VALUE +16 COMP SYNC.         
012100                                                                          
012200 77  WS-DAFSGVV-FOM          PIC  9(6)      VALUE 0  .                    
012300 77  WS-DAFSGVV-TOM          PIC  9(6)      VALUE 0  .                    
012400 77  WS-DAFSGVV-FOM-FVV      PIC  9(6)      VALUE 0  .                    
012500 77  WS-DAFSGVV-TOM-FVV      PIC  9(6)      VALUE 0  .                    
012600 77  WS-SULEVANT-VV          PIC S9(9)      VALUE 0  COMP-3.              
012700 77  WS-SUARTFSG-VV          PIC S9(9)V9(3) VALUE 0  COMP-3.              
012800 77  WS-RETOTBV-VV           PIC S9(9)V9(2) VALUE 0  COMP-3.              
012900 77  WS-SUARTSJK-VV          PIC S9(9)V9(3) VALUE 0  COMP-3.              
013000 77  WS-SULEVANT-FVV         PIC S9(9)      VALUE 0  COMP-3.              
013100 77  WS-SUARTFSG-FVV         PIC S9(9)V9(3) VALUE 0  COMP-3.              
013200 77  WS-RETOTBV-FVV          PIC S9(9)V9(2) VALUE 0  COMP-3.              
013300 77  WS-SUARTSJK-FVV         PIC S9(9)V9(3) VALUE 0  COMP-3.              
013400                                                                          
013500 77  W-SUARTSJK-VV           PIC S9(9)V9(3) VALUE 0  COMP-3.              
013600 77  W-SUARTSJK-FVV          PIC S9(9)V9(3) VALUE 0  COMP-3.              
013700                                                                          
013800 77  WS-BRYT-VECKA           PIC S9(6)      VALUE 0  COMP-3.              
013900 77  83-FIL                  PIC S9(1)      VALUE 1.                      
014000 77  85-FIL                  PIC S9(1)      VALUE 2.                      
014100 77  87-FIL                  PIC S9(1)      VALUE 3.                      
014200 77  WS-FILFLAGGA            PIC S9(1)      VALUE 0.                      
014300 77  MARK-1-IX               PIC S9(1)      VALUE 0.                      
014400 77  KONC-1-IX               PIC S9(1)      VALUE 0.                      
014500 77  DIST-1-IX               PIC S9(1)      VALUE 0.                      
014600 77  MAX-MARK-1-IX           PIC S9(1)      VALUE 5.                      
014700 77  MAX-KONC-1-IX           PIC S9(1)      VALUE 9.                      
014800 77  MAX-DIST-1-IX           PIC S9(1)      VALUE 5.                      
014900 77  MARK-IX                 PIC S9(3)      VALUE 0.                      
015000 77  KONC-IX                 PIC S9(4)      VALUE 0.                      
015100 77  DIST-IX                 PIC S9(5)      VALUE 0.                      
015200 77  MAX-MARK-IX             PIC S9(3)      VALUE 100.                    
015300 77  MAX-KONC-IX             PIC S9(4)      VALUE 1000.                   
015400 77  MAX-DIST-IX             PIC S9(5)      VALUE 10000.                  
015500                                                                          
015600*FÖR ATT KUNNA SORTERA RÄTT BEROENDE PÅ KD-NIVA ******************        
015700 01  WS-TEST-IDFKNGRP        PIC 9(5)     VALUE 0.                        
015800 01  FILLER    REDEFINES WS-TEST-IDFKNGRP .                               
015900     03  FILLER                  PIC 9(1).                                
016000     03  WS-TEST-IDFKNGRP-TUSEN  PIC 9(1).                                
016100     03  FILLER                  PIC 9(3).                                
016200 01  FILLER    REDEFINES WS-TEST-IDFKNGRP.                                
016300     03  FILLER                  PIC 9(1).                                
016400     03  WS-TEST-IDFKNGRP-HUNDRA PIC 9(2).                                
016500     03  FILLER                  PIC 9(2).                                
016600******************************************************************        
016700                                                                          
016800*01  -COPY W33081   -PRE WS-SPAR-                                         
017000                                                                          
017100 01  FILLER                  PIC X(16)     VALUE 'OMR-FLAGGA'.            
017200                                                                          
017300 01  OMR-FLAGGA              PIC 9(01).                                   
017400    88  DISTRIKT-NIVA                      VALUE 1.                       
017500    88  KONCERN-NIVA                       VALUE 2.                       
017600    88  MARKNAD-NIVA                       VALUE 3.                       
017700    88  UTAN-NIVA                          VALUE 4.                       
017800                                                                          
017900 01  FILLER                  PIC X(16)     VALUE 'SWITCHAR'.              
018000                                                                          
018100                                                                          
018200 01  DYNAMISKA-SUBPROGRAM.                                                
018300*                                                                         
018400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018600     SKIP2                                                                
018700*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
018800*                                                                         
018900 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
019000*01  -COPY W0005 -PRE  POSTSUM-                                           
019200     EJECT                                                                
019300 01  FILLER                       PIC X(16)  VALUE 'I35-FILEN'.           
019400*     FIL W33035     INAREA                                               
019500                                                                          
019600*------I 35FILEN FINNS TVÅ POSTTYPER, MEN EN GENERELL                     
019700*------POSTTYP ANVÄNDS OCKSÅ FÖR ATT FÖRSTA DELEN                         
019800*------I POSTTYPERNA ÄR LIKA OCH DET ÄR                                   
019900*------UR DEN DELEN DETTA PROGRAM HÄMTAR UPPGIFTER.                       
019910*------OBS SEDAN 18/10 -91 HÄMTAS ÄVEN KDSVAR FRÅN 3502                   
020000*------I ÖVRIGT SKRIVS POSTERNA UT DIREKT PÅ UTFIL.                       
020100 01  IN-AREA.                                                             
020200     03  FILLER        PIC X(2611).                                       
020300                                                                          
020400*01   AREA -COPY W3303503  -PRE    I35-    -RED IN-AREA.                  
020600                                                                          
020700*01   AREA -COPY W3303503  -PRE    WG3203- -RED IN-AREA.                  
020900                                                                          
021000*01   AREA -COPY W3303502  -PRE    WG3202- -RED IN-AREA.                  
021200    EJECT                                                                 
021300                                                                          
021400 01  FILLER                       PIC X(16)  VALUE 'I81-FILEN'.           
021500*01  AREA  -COPY W33081  -PRE I81-.                                       
021700    EJECT                                                                 
021800 01  FILLER                       PIC X(16)  VALUE 'WS-SORT-A'.           
021900 01  WS-SORTERAD-AREA.                                                    
022000   03  WS-SORT-IDUSER              PIC X(8).                              
022100   03  WS-SORT-DAREGDAT            PIC 9(8).                              
022200   03  WS-SORT-TIREGTID            PIC 9(7).                              
022300   03  WS-SORT-FOERSTA             PIC 9(5).                              
022400   03  WS-SORT-ANDRA               PIC 9(5).                              
022500   03  WS-SORT-TREDJE              PIC 9(5).                              
022600*  03  AREA -COPY W33081  -PRE SORT81-.                                   
022800     EJECT                                                                
022900                                                                          
023000 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
023100*                                                                         
023200*     FIL W33083                                                          
023300*01  AREA  -COPY W33083  -PRE UTRES-.                                     
023500     EJECT                                                                
023600*01  AREA  -COPY W3303502  -PRE U283-.                                    
023800    EJECT                                                                 
024200 PROCEDURE DIVISION.                                                      
024300    SKIP2                                                                 
024400 STYR SECTION.                                                            
024500     PERFORM A-INIT                                                       
024600     PERFORM S01-LAS-81-FIL                                               
024700     PERFORM S02-LAS-35-FIL                                               
024800     PERFORM UNTIL I35-EOF = JA                                           
024900       IF I81-IDUSER = I35-IDUSER AND                                     
025000          I81-DAREGDAT = I35-DAREGDAT AND                                 
025100          I81-TIREGTID = I35-TIREGTID AND                                 
025200          I35-IDPTYP = VP1                                                
025300         PERFORM B-KONTROLLERA-NIVA                                       
025400         PERFORM C-SPARA-BRYTVAERDEN-URVAL                                
025500         PERFORM S10-SKRIV-MATCHADE-URVAL                                 
025600       SORT SORTFIL                                                       
025700         ASCENDING KEY SORT-IDUSER                                        
025800                       SORT-DAREGDAT                                      
025900                       SORT-TIREGTID                                      
026000                       SORT-FOERSTA                                       
026100                       SORT-ANDRA                                         
026200                       SORT-TREDJE                                        
026300         INPUT PROCEDURE  D-PLOCKA-P-RESULTAT                             
026400         OUTPUT PROCEDURE E-MATCHA-OCH-SKRIV                              
026500         MOVE NEJ TO SORT-EOF                                             
026600       ELSE                                                               
026700         IF I35-IDPTYP = VP1                                              
026800           PERFORM S11-SKRIV-EJ-MATCHADE-URVAL                            
026900         END-IF                                                           
027000       END-IF                                                             
027100       PERFORM S02-LAS-35-FIL                                             
027200     END-PERFORM                                                          
027300     IF SORT-RETURN = ZERO                                                
027400        PERFORM Z-FINIT                                                   
027500        MOVE ZERO TO RETURN-CODE                                          
027600        GOBACK                                                            
027700     ELSE                                                                 
027800        CALL ABEND USING FELKOD                                           
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 A-INIT SECTION.                                                          
028300     SKIP2                                                                
028400     OPEN INPUT  W33035S                                                  
028500                 W33081S                                                  
028600     OPEN OUTPUT W33083                                                   
028700                 W33085                                                   
028800                 W33087                                                   
028900     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
029000     .                                                                    
029100     EJECT                                                                
029200 B-KONTROLLERA-NIVA SECTION.                                              
029300                                                                          
029400     EVALUATE I35-IDGTYP                                                  
029500        WHEN 1                                                            
029600           MOVE 1 TO OMR-FLAGGA                                           
029700        WHEN 2                                                            
029800           MOVE 2 TO OMR-FLAGGA                                           
029900        WHEN 3                                                            
030000           MOVE 3 TO OMR-FLAGGA                                           
030100        WHEN 4                                                            
030200           MOVE 4 TO OMR-FLAGGA                                           
030300     END-EVALUATE.                                                        
030400                                                                          
030500 C-SPARA-BRYTVAERDEN-URVAL SECTION.                                       
030600     SKIP2                                                                
030700* ---NEDAN MOVE  BEHÖVS EJ ?   SE D-PLOCKA                                
030800     MOVE I35-001-GRUPP TO WS-SPAR-001-GRUPP                              
030900     MOVE I35-DAFSGVV-FOM TO WS-BRYT-VECKA                                
031000     .                                                                    
031100     SKIP2                                                                
031200 D-PLOCKA-P-RESULTAT SECTION.                                             
031300     SKIP2                                                                
031400     MOVE ZERO TO WS-SORT-FOERSTA                                         
031500                  WS-SORT-ANDRA                                           
031600                  WS-SORT-TREDJE                                          
031700     PERFORM UNTIL                                                        
031800     I81-IDUSER   NOT = I35-IDUSER   OR                                   
031900     I81-DAREGDAT NOT = I35-DAREGDAT OR                                   
032000     I81-TIREGTID NOT = I35-TIREGTID                                      
032100     OR I81-EOF = JA                                                      
032200       MOVE I81-IDUSER TO WS-SORT-IDUSER                                  
032300       MOVE I81-DAREGDAT TO WS-SORT-DAREGDAT                              
032400       MOVE I81-TIREGTID TO WS-SORT-TIREGTID                              
032401       IF WG3202-IDTRANS = '3202'                                         
032403*--KDSVAR = JA = HOPSLAGNING AV PS                                        
032410         IF WG3202-KDSVAR = JA                                            
032500           MOVE ZERO         TO WS-SORT-FOERSTA                           
032501         ELSE                                                             
032510           MOVE I81-KDPRODSL TO WS-SORT-FOERSTA                           
032520         END-IF                                                           
032530       ELSE                                                               
032540         MOVE I81-KDPRODSL TO WS-SORT-FOERSTA                             
032550       END-IF                                                             
032600       MOVE I81-IDFKNGRP TO WS-TEST-IDFKNGRP                              
032700       IF WG3202-KDNIVA = 0                                               
032800         MOVE WS-TEST-IDFKNGRP-TUSEN TO WS-SORT-ANDRA                     
032900         COMPUTE WS-SORT-ANDRA = WS-SORT-ANDRA * 1000                     
033000       ELSE                                                               
033100         IF WG3202-KDNIVA = 2                                             
033200           MOVE WS-TEST-IDFKNGRP-HUNDRA TO WS-SORT-ANDRA                  
033300           COMPUTE WS-SORT-ANDRA = WS-SORT-ANDRA * 100                    
033400         ELSE                                                             
033500*------------------ KDNIVA = 4 ---------------------------                
033600           MOVE WS-TEST-IDFKNGRP TO WS-SORT-ANDRA                         
033700         END-IF                                                           
033800       END-IF                                                             
033900       IF OMR-FLAGGA = 1                                                  
034000          MOVE I81-IDDISTR TO WS-SORT-TREDJE                              
034100       ELSE                                                               
034200         IF OMR-FLAGGA = 2                                                
034300            MOVE I81-IDKONCNR TO WS-SORT-TREDJE                           
034400         ELSE                                                             
034500           IF OMR-FLAGGA = 3                                              
034600              MOVE I81-KDMARK-BUDG TO WS-SORT-TREDJE                      
034700           ELSE                                                           
034800              IF OMR-FLAGGA = 4                                           
034900                 MOVE ZERO TO SORT-TREDJE                                 
035000              END-IF                                                      
035100           END-IF                                                         
035200         END-IF                                                           
035300       END-IF                                                             
035400       MOVE I81-AREA TO SORT81-AREA                                       
035500       MOVE WS-SORT-ANDRA TO SORT81-IDFKNGRP                              
035600       RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                        
035700       PERFORM S01-LAS-81-FIL                                             
035800     END-PERFORM                                                          
035900     .                                                                    
036000     EJECT                                                                
036100                                                                          
036200 E-MATCHA-OCH-SKRIV  SECTION.                                             
036300     SKIP2                                                                
036400     PERFORM S03-RETURN                                                   
036500     PERFORM UNTIL  SORT-EOF = JA                                         
036600       IF  KONCERN-NIVA                                                   
036700         MOVE SORT81-IDKONCNR TO WS-SPAR-IDKONCNR                         
036800         PERFORM EA-SUMMERA-PA-KONCNR                                     
036900       ELSE                                                               
037000         IF  MARKNAD-NIVA                                                 
037100           MOVE SORT81-KDMARK-BUDG TO WS-SPAR-KDMARK-BUDG                 
037200           PERFORM EB-SUMMERA-PA-MARKNAD                                  
037300         ELSE                                                             
037400           IF  DISTRIKT-NIVA                                              
037500             MOVE SORT81-IDDISTR TO WS-SPAR-IDDISTR                       
037600             PERFORM EC-SUMMERA-PA-DISTRIKT                               
037700           ELSE                                                           
037800             PERFORM ED-SUMMERA-TOTALEN                                   
037900           END-IF                                                         
038000         END-IF                                                           
038100       END-IF                                                             
038200     END-PERFORM                                                          
038300     .                                                                    
038400     EJECT                                                                
038500 EA-SUMMERA-PA-KONCNR SECTION.                                            
038600     SKIP2                                                                
038700     PERFORM UNTIL WS-SPAR-IDKONCNR NOT = SORT81-IDKONCNR                 
038800                 OR SORT-EOF = JA                                         
038900       MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                           
039000       MOVE SORT81-IDFKNGRP TO WS-SPAR-IDFKNGRP                           
039100       PERFORM  S05A-FLYTTA-VAERDEN-T-FIL                                 
039110                                                                          
039200       PERFORM UNTIL WS-SPAR-IDKONCNR NOT = SORT81-IDKONCNR               
039300         OR WS-SPAR-KDPRODSL NOT = SORT81-KDPRODSL                        
039400         OR WS-SPAR-IDFKNGRP NOT = SORT81-IDFKNGRP                        
039500         OR SORT-EOF = JA                                                 
039600         IF WS-BRYT-VECKA > SORT81-DAFSGVV                                
039700           PERFORM S17-SAMLA-FVV                                          
039800         ELSE                                                             
039900           IF WS-BRYT-VECKA NOT > SORT81-DAFSGVV                          
040000             PERFORM S18-SAMLA-VV                                         
040100           END-IF                                                         
040200         END-IF                                                           
040300         PERFORM S03-RETURN                                               
040301*---KDSVAR = SAMMANSLAGNING AV PS                                         
040302         IF WG3202-IDTRANS = '3202'                                       
040304           IF WG3202-KDSVAR = JA                                          
040311             MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                     
040320           END-IF                                                         
040330         END-IF                                                           
040400       END-PERFORM                                                        
040410                                                                          
040500       IF WS-SUARTFSG-FVV NOT = ZERO                                      
040600          PERFORM S19-BERAKNA-SJK-TG-FVV                                  
040700       ELSE                                                               
040800          MOVE ZERO TO WS-RETOTBV-FVV                                     
040900       END-IF                                                             
041000       IF WS-SUARTFSG-VV NOT = ZERO                                       
041100          PERFORM S20-BERAKNA-SJK-TG-VV                                   
041200       ELSE                                                               
041300          MOVE ZERO TO WS-RETOTBV-VV                                      
041400       END-IF                                                             
041500       PERFORM  S05B-FLYTTA-VAERDEN-T-FIL                                 
041600       PERFORM  S22-SKRIV-RES-FIL                                         
041700       PERFORM S21-NOLLSTALL-SUMMAFAELT                                   
041800     END-PERFORM                                                          
041900     .                                                                    
042000     EJECT                                                                
042100                                                                          
042200 EB-SUMMERA-PA-MARKNAD SECTION.                                           
042300     SKIP2                                                                
042400     PERFORM UNTIL WS-SPAR-KDMARK-BUDG NOT =                              
042500                    SORT81-KDMARK-BUDG                                    
042600                 OR SORT-EOF = JA                                         
042700       MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                           
042800       MOVE SORT81-IDFKNGRP TO WS-SPAR-IDFKNGRP                           
042900       PERFORM  S05A-FLYTTA-VAERDEN-T-FIL                                 
042910                                                                          
043000       PERFORM UNTIL                                                      
043100            WS-SPAR-KDMARK-BUDG NOT = SORT81-KDMARK-BUDG                  
043200         OR WS-SPAR-KDPRODSL NOT = SORT81-KDPRODSL                        
043300         OR WS-SPAR-IDFKNGRP NOT = SORT81-IDFKNGRP                        
043400         OR SORT-EOF = JA                                                 
043500         IF WS-BRYT-VECKA > SORT81-DAFSGVV                                
043600           PERFORM S17-SAMLA-FVV                                          
043700         ELSE                                                             
043800           IF WS-BRYT-VECKA NOT > SORT81-DAFSGVV                          
043900             PERFORM S18-SAMLA-VV                                         
044000           END-IF                                                         
044100         END-IF                                                           
044200         PERFORM S03-RETURN                                               
044201*---KDSVAR = SAMMANSLAGNING AV PS                                         
044202         IF WG3202-IDTRANS = '3202'                                       
044203           IF WG3202-KDSVAR = JA                                          
044204             MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                     
044205           END-IF                                                         
044206         END-IF                                                           
044300       END-PERFORM                                                        
044310                                                                          
044400       IF WS-SUARTFSG-FVV NOT = ZERO                                      
044500          PERFORM S19-BERAKNA-SJK-TG-FVV                                  
044600       ELSE                                                               
044700          MOVE ZERO TO WS-RETOTBV-FVV                                     
044800       END-IF                                                             
044900       IF WS-SUARTFSG-VV NOT = ZERO                                       
045000          PERFORM S20-BERAKNA-SJK-TG-VV                                   
045100       ELSE                                                               
045200          MOVE ZERO TO WS-RETOTBV-VV                                      
045300       END-IF                                                             
045400       PERFORM  S05B-FLYTTA-VAERDEN-T-FIL                                 
045500       PERFORM  S22-SKRIV-RES-FIL                                         
045600       PERFORM S21-NOLLSTALL-SUMMAFAELT                                   
045700     END-PERFORM                                                          
045800     .                                                                    
045900     EJECT                                                                
046000                                                                          
046100 EC-SUMMERA-PA-DISTRIKT SECTION.                                          
046200     SKIP2                                                                
046300     PERFORM UNTIL WS-SPAR-IDDISTR NOT =                                  
046400                    SORT81-IDDISTR                                        
046500                 OR SORT-EOF = JA                                         
046600       MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                           
046700       MOVE SORT81-IDFKNGRP TO WS-SPAR-IDFKNGRP                           
046800       PERFORM  S05A-FLYTTA-VAERDEN-T-FIL                                 
046810                                                                          
046900       PERFORM UNTIL                                                      
047000            WS-SPAR-IDDISTR  NOT = SORT81-IDDISTR                         
047100         OR WS-SPAR-KDPRODSL NOT = SORT81-KDPRODSL                        
047200         OR WS-SPAR-IDFKNGRP NOT = SORT81-IDFKNGRP                        
047300         OR SORT-EOF = JA                                                 
047400         IF WS-BRYT-VECKA > SORT81-DAFSGVV                                
047500           PERFORM S17-SAMLA-FVV                                          
047600         ELSE                                                             
047700           IF WS-BRYT-VECKA NOT > SORT81-DAFSGVV                          
047800             PERFORM S18-SAMLA-VV                                         
047900           END-IF                                                         
048000         END-IF                                                           
048100         PERFORM S03-RETURN                                               
048101*---KDSVAR = SAMMANSLAGNING AV PS                                         
048102         IF WG3202-IDTRANS = '3202'                                       
048103           IF WG3202-KDSVAR = JA                                          
048104             MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                     
048105           END-IF                                                         
048106         END-IF                                                           
048200       END-PERFORM                                                        
048210                                                                          
048300       IF WS-SUARTFSG-FVV NOT = ZERO                                      
048400          PERFORM S19-BERAKNA-SJK-TG-FVV                                  
048500       ELSE                                                               
048600          MOVE ZERO TO WS-RETOTBV-FVV                                     
048700       END-IF                                                             
048800       IF WS-SUARTFSG-VV NOT = ZERO                                       
048900          PERFORM S20-BERAKNA-SJK-TG-VV                                   
049000       ELSE                                                               
049100          MOVE ZERO TO WS-RETOTBV-VV                                      
049200       END-IF                                                             
049300       PERFORM  S05B-FLYTTA-VAERDEN-T-FIL                                 
049400       PERFORM  S22-SKRIV-RES-FIL                                         
049500       PERFORM S21-NOLLSTALL-SUMMAFAELT                                   
049600     END-PERFORM                                                          
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 ED-SUMMERA-TOTALEN SECTION.                                              
050100     SKIP2                                                                
050200     PERFORM UNTIL SORT-EOF = JA                                          
050300       MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                           
050400       MOVE SORT81-IDFKNGRP TO WS-SPAR-IDFKNGRP                           
050500       PERFORM  S05A-FLYTTA-VAERDEN-T-FIL                                 
050600                                                                          
050700       PERFORM UNTIL WS-SPAR-KDPRODSL NOT = SORT81-KDPRODSL               
050800         OR WS-SPAR-IDFKNGRP NOT = SORT81-IDFKNGRP                        
050900         OR SORT-EOF = JA                                                 
051000         IF  WS-BRYT-VECKA > SORT81-DAFSGVV                               
051100           PERFORM S17-SAMLA-FVV                                          
051200         ELSE                                                             
051300           IF WS-BRYT-VECKA NOT > SORT81-DAFSGVV                          
051400             PERFORM S18-SAMLA-VV                                         
051500           END-IF                                                         
051600         END-IF                                                           
051700         PERFORM S03-RETURN                                               
051701*---KDSVAR = SAMMANSLAGNING AV PS                                         
051702         IF WG3202-IDTRANS = '3202'                                       
051703           IF WG3202-KDSVAR = JA                                          
051704             MOVE SORT81-KDPRODSL TO WS-SPAR-KDPRODSL                     
051705           END-IF                                                         
051706         END-IF                                                           
051800       END-PERFORM                                                        
051810                                                                          
051900       IF WS-SUARTFSG-FVV NOT = ZERO                                      
052000          PERFORM S19-BERAKNA-SJK-TG-FVV                                  
052100       ELSE                                                               
052200          MOVE ZERO TO WS-RETOTBV-FVV                                     
052300       END-IF                                                             
052400       IF WS-SUARTFSG-VV NOT = ZERO                                       
052500          PERFORM S20-BERAKNA-SJK-TG-VV                                   
052600       ELSE                                                               
052700          MOVE ZERO TO WS-RETOTBV-VV                                      
052800       END-IF                                                             
052900       PERFORM  S05B-FLYTTA-VAERDEN-T-FIL                                 
053000       PERFORM  S22-SKRIV-RES-FIL                                         
053100       PERFORM S21-NOLLSTALL-SUMMAFAELT                                   
053200     END-PERFORM                                                          
053300     .                                                                    
053400     EJECT                                                                
053500                                                                          
053600 Z-FINIT SECTION.                                                         
053700     SKIP2                                                                
053800     CLOSE W33035S                                                        
053900           W33081S                                                        
054000           W33083                                                         
054100           W33085                                                         
054200           W33087                                                         
054300     MOVE 'S' TO POSTSUM-OPKOD                                            
054400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
054500     .                                                                    
054600                                                                          
054700 S01-LAS-81-FIL   SECTION.                                                
054800     SKIP2                                                                
054900     READ W33081S INTO I81-AREA                                           
055000     AT END                                                               
055100       MOVE JA                    TO I81-EOF                              
055200     END-READ                                                             
055300                                                                          
055400     IF I81-EOF = NEJ                                                     
055500       MOVE 'I81 '                TO POSTSUM-TRANSTYP                     
055600       MOVE 'W33081'              TO POSTSUM-FDNAMN                       
055700       MOVE 'W33082D2'            TO POSTSUM-DDNAMN2                      
055800       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 S02-LAS-35-FIL    SECTION.                                               
056300     SKIP2                                                                
056400     READ W33035S INTO IN-AREA                                            
056500     AT END                                                               
056600       MOVE JA                    TO I35-EOF                              
056700     END-READ                                                             
056800                                                                          
056900     IF I35-EOF = NEJ                                                     
057000       MOVE 'I35 '                TO POSTSUM-TRANSTYP                     
057100       MOVE 'W33035'              TO POSTSUM-FDNAMN                       
057200       MOVE 'W33082D1'            TO POSTSUM-DDNAMN2                      
057300       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 S03-RETURN       SECTION.                                                
057800     SKIP2                                                                
057900     RETURN SORTFIL INTO WS-SORTERAD-AREA                                 
058000     AT END                                                               
058100       MOVE JA                    TO SORT-EOF                             
058200     END-RETURN                                                           
058300                                                                          
058400     IF SORT-EOF = NEJ                                                    
058500       MOVE 'SORT'                TO POSTSUM-TRANSTYP                     
058600       MOVE 'SORTER'              TO POSTSUM-FDNAMN                       
058700       MOVE 'W33082DS'            TO POSTSUM-DDNAMN2                      
058800       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 S05A-FLYTTA-VAERDEN-T-FIL SECTION.                                       
059300     SKIP2                                                                
059400     MOVE SORT81-001-GRUPP       TO UTRES-001-GRUPP                       
059500     MOVE SORT81-KDPRODSL        TO UTRES-KDPRODSL                        
059600     MOVE SORT81-BEPRODSL        TO UTRES-BEPRODSL                        
059700     MOVE SORT81-IDFKNGRP        TO UTRES-IDFKNGRP                        
059800     MOVE SORT81-BEFKNGRP        TO UTRES-BEFKNGRP                        
059900     MOVE SORT81-IDDISTR         TO UTRES-IDDISTR                         
060000     MOVE SORT81-IDKONCNR        TO UTRES-IDKONCNR                        
060100     MOVE SORT81-KDMARK-BUDG     TO UTRES-KDMARK-BUDG                     
060200     MOVE SORT81-BEMARK-BUDG     TO UTRES-BEMARK-BUDG                     
060300     .                                                                    
060400                                                                          
060500 S05B-FLYTTA-VAERDEN-T-FIL SECTION.                                       
060600     SKIP2                                                                
060700*-- DE SUMMERADE VÄRDENA -------------------                              
060800     MOVE WS-SULEVANT-VV         TO UTRES-SULEVANT-VV                     
060900     MOVE WS-SUARTFSG-VV         TO UTRES-SUARTFSG-VV                     
061000     MOVE WS-RETOTBV-VV          TO UTRES-RETOTBV-VV                      
061100     MOVE WS-SUARTSJK-VV         TO UTRES-SUARTSJK-VV                     
061200                                                                          
061300     MOVE WS-SULEVANT-FVV        TO UTRES-SULEVANT-FVV                    
061400     MOVE WS-SUARTFSG-FVV        TO UTRES-SUARTFSG-FVV                    
061500     MOVE WS-RETOTBV-FVV         TO UTRES-RETOTBV-FVV                     
061600     MOVE WS-SUARTSJK-FVV        TO UTRES-SUARTSJK-FVV                    
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 S10-SKRIV-MATCHADE-URVAL SECTION.                                        
062100      EVALUATE  I35-IDTRANS                                               
062200        WHEN 3202                                                         
062300          IF WG3202-KDNIVA = 4                                            
062400            PERFORM S12-SKRIV83-3202-POST                                 
062500            MOVE 83-FIL TO WS-FILFLAGGA                                   
062600          ELSE                                                            
062700            IF WG3202-KDNIVA = 2                                          
062800              PERFORM S12-SKRIV85-3202-POST                               
062900              MOVE 85-FIL TO WS-FILFLAGGA                                 
063000            ELSE                                                          
063100*------------------KDNIVA = 0---------------------                        
063200              PERFORM S12-SKRIV87-3202-POST                               
063300              MOVE 87-FIL TO WS-FILFLAGGA                                 
063400            END-IF                                                        
063500          END-IF                                                          
063600*       WHEN 3203                                                         
063700*         PERFORM S13-SKRIV-3203-POST                                     
063800*         MOVE 83-FIL TO WS-FILFLAGGA                                     
063900      END-EVALUATE                                                        
064000      .                                                                   
064100                                                                          
064200 S11-SKRIV-EJ-MATCHADE-URVAL SECTION.                                     
064300      EVALUATE  I35-IDTRANS                                               
064400        WHEN 3202                                                         
064500          IF WG3202-KDNIVA = 4                                            
064600            PERFORM S14-SKRIV83-3202-POST                                 
064700            MOVE 83-FIL TO WS-FILFLAGGA                                   
064800          ELSE                                                            
064900            IF WG3202-KDNIVA = 2                                          
065000              PERFORM S14-SKRIV85-3202-POST                               
065100              MOVE 85-FIL TO WS-FILFLAGGA                                 
065200            ELSE                                                          
065300*------------------KDNIVA = 0---------------------                        
065400              PERFORM S14-SKRIV87-3202-POST                               
065500              MOVE 87-FIL TO WS-FILFLAGGA                                 
065600            END-IF                                                        
065700          END-IF                                                          
065800*       WHEN 3203                                                         
065900*         PERFORM S15-SKRIV-3203-POST                                     
066000*         MOVE 83-FIL TO WS-FILFLAGGA                                     
066100      END-EVALUATE                                                        
066200      .                                                                   
066300                                                                          
066400 S12-SKRIV83-3202-POST SECTION.                                           
066500     SKIP1                                                                
066600     WRITE U283-POST        FROM IN-AREA                                  
066700     MOVE 'URV'              TO POSTSUM-TRANSTYP                          
066800     MOVE 'W33082D3'         TO POSTSUM-DDNAMN2                           
066900     MOVE 'W33083'           TO POSTSUM-FDNAMN                            
067000     CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
067100     .                                                                    
067200                                                                          
067300 S12-SKRIV85-3202-POST SECTION.                                           
067400     SKIP1                                                                
067500     WRITE U285-POST        FROM IN-AREA                                  
067600     MOVE 'URV'              TO POSTSUM-TRANSTYP                          
067700     MOVE 'W33082D4'         TO POSTSUM-DDNAMN2                           
067800     MOVE 'W33085'           TO POSTSUM-FDNAMN                            
067900     CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
068000     .                                                                    
068100                                                                          
068200 S12-SKRIV87-3202-POST SECTION.                                           
068300     SKIP1                                                                
068400     WRITE U287-POST        FROM IN-AREA                                  
068500     MOVE 'URV'              TO POSTSUM-TRANSTYP                          
068600     MOVE 'W33082D5'         TO POSTSUM-DDNAMN2                           
068700     MOVE 'W33087'           TO POSTSUM-FDNAMN                            
068800     CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
068900     .                                                                    
069000     EJECT                                                                
069100*S13-SKRIV-3203-POST SECTION.                                             
069200*    SKIP1                                                                
069300*    WRITE U383-POST         FROM IN-AREA                                 
069400*    MOVE 'URV'              TO POSTSUM-TRANSTYP                          
069500*    MOVE 'W33082D3'         TO POSTSUM-DDNAMN2                           
069600*    MOVE 'W33083'           TO POSTSUM-FDNAMN                            
069700*    CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
069800*    .                                                                    
069900                                                                          
070000 S14-SKRIV83-3202-POST SECTION.                                           
070100     SKIP1                                                                
070200     MOVE ZERO              TO WG3202-IDGTYP                              
070300     WRITE U283-POST        FROM IN-AREA                                  
070400     MOVE 'URV'              TO POSTSUM-TRANSTYP                          
070500     MOVE 'W33082D3'         TO POSTSUM-DDNAMN2                           
070600     MOVE 'W33083'           TO POSTSUM-FDNAMN                            
070700     CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
070800     .                                                                    
070900                                                                          
071000 S14-SKRIV85-3202-POST SECTION.                                           
071100     SKIP1                                                                
071200     MOVE ZERO              TO WG3202-IDGTYP                              
071300     WRITE U285-POST        FROM IN-AREA                                  
071400     MOVE 'URV'              TO POSTSUM-TRANSTYP                          
071500     MOVE 'W33082D4'         TO POSTSUM-DDNAMN2                           
071600     MOVE 'W33085'           TO POSTSUM-FDNAMN                            
071700     CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
071800     .                                                                    
071900                                                                          
072000 S14-SKRIV87-3202-POST SECTION.                                           
072100     SKIP1                                                                
072200     MOVE ZERO              TO WG3202-IDGTYP                              
072300     WRITE U287-POST        FROM IN-AREA                                  
072400     MOVE 'URV'              TO POSTSUM-TRANSTYP                          
072500     MOVE 'W33082D5'         TO POSTSUM-DDNAMN2                           
072600     MOVE 'W33087'           TO POSTSUM-FDNAMN                            
072700     CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
072800     .                                                                    
072900     EJECT                                                                
073000*S15-SKRIV-3203-POST SECTION.                                             
073100*    SKIP1                                                                
073200*    MOVE ZERO              TO WG3203-IDGTYP                              
073300*    WRITE U383-POST         FROM IN-AREA                                 
073400*    MOVE 'URV'              TO POSTSUM-TRANSTYP                          
073500*    MOVE 'W33082D3'         TO POSTSUM-DDNAMN2                           
073600*    MOVE 'W33083'           TO POSTSUM-FDNAMN                            
073700*    CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2                
073800*    .                                                                    
073900                                                                          
074000                                                                          
074100  S17-SAMLA-FVV  SECTION.                                                 
074200     SKIP2                                                                
074300                                                                          
074400     MOVE ZERO TO W-SUARTSJK-FVV                                          
074500     COMPUTE W-SUARTSJK-FVV ROUNDED =                                     
074600       SORT81-SULEVANT * SORT81-PRARTSJK                                  
074700     COMPUTE WS-SUARTSJK-FVV =                                            
074800       WS-SUARTSJK-FVV + W-SUARTSJK-FVV                                   
074900     COMPUTE WS-SUARTFSG-FVV =                                            
075000       WS-SUARTFSG-FVV + SORT81-SUARTFSG                                  
075100     COMPUTE WS-SULEVANT-FVV =                                            
075200       WS-SULEVANT-FVV + SORT81-SULEVANT.                                 
075300                                                                          
075400                                                                          
075500  S18-SAMLA-VV  SECTION.                                                  
075600                                                                          
075700     MOVE ZERO TO  W-SUARTSJK-VV                                          
075800     COMPUTE W-SUARTSJK-VV ROUNDED =                                      
075900       SORT81-SULEVANT * SORT81-PRARTSJK                                  
076000     COMPUTE WS-SUARTSJK-VV =                                             
076100       WS-SUARTSJK-VV + W-SUARTSJK-VV                                     
076200     COMPUTE WS-SUARTFSG-VV =                                             
076300       WS-SUARTFSG-VV + SORT81-SUARTFSG                                   
076400     COMPUTE WS-SULEVANT-VV =                                             
076500       WS-SULEVANT-VV + SORT81-SULEVANT.                                  
076600                                                                          
076700                                                                          
076800                                                                          
076900 S19-BERAKNA-SJK-TG-FVV SECTION.                                          
077000                                                                          
077100     COMPUTE WS-RETOTBV-FVV ROUNDED =                                     
077200       100 * (( WS-SUARTFSG-FVV -                                         
077300       WS-SUARTSJK-FVV ) / WS-SUARTFSG-FVV).                              
077400                                                                          
077500     IF WS-RETOTBV-FVV < - 99.9                                           
077600        MOVE -99.90 TO WS-RETOTBV-FVV                                     
077700     ELSE                                                                 
077800        IF WS-RETOTBV-FVV > 99.9                                          
077900           MOVE 99.90 TO WS-RETOTBV-FVV                                   
078000        END-IF                                                            
078100     END-IF                                                               
078200                                                                          
078300     .                                                                    
078400                                                                          
078500 S20-BERAKNA-SJK-TG-VV SECTION.                                           
078600                                                                          
078700     COMPUTE WS-RETOTBV-VV ROUNDED =                                      
078800       100 * (( WS-SUARTFSG-VV -                                          
078900       WS-SUARTSJK-VV )  / WS-SUARTFSG-VV).                               
079000                                                                          
079100     IF WS-RETOTBV-VV < - 99.9                                            
079200        MOVE -99.90 TO WS-RETOTBV-VV                                      
079300     ELSE                                                                 
079400        IF WS-RETOTBV-VV > 99.9                                           
079500           MOVE 99.90 TO WS-RETOTBV-VV                                    
079600        END-IF                                                            
079700     END-IF                                                               
079800     .                                                                    
079900                                                                          
080000 S21-NOLLSTALL-SUMMAFAELT SECTION.                                        
080100                                                                          
080200     MOVE ZERO TO WS-SUARTSJK-FVV  WS-SULEVANT-FVV                        
080300                  WS-SUARTFSG-FVV  WS-RETOTBV-FVV                         
080400                  WS-SUARTSJK-VV   WS-SULEVANT-VV                         
080500                  WS-SUARTFSG-VV   WS-RETOTBV-VV.                         
080600                                                                          
080700                                                                          
080800 S22-SKRIV-RES-FIL SECTION.                                               
080900     SKIP1                                                                
081000     IF WS-FILFLAGGA = 83-FIL                                             
081100       WRITE U083-POST         FROM UTRES-AREA                            
081200       MOVE 'RES'              TO POSTSUM-TRANSTYP                        
081300       MOVE 'W33082D3'         TO POSTSUM-DDNAMN2                         
081400       MOVE 'W33083'           TO POSTSUM-FDNAMN                          
081500       CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2              
081600     ELSE                                                                 
081700       IF WS-FILFLAGGA = 85-FIL                                           
081800         WRITE U085-POST         FROM UTRES-AREA                          
081900         MOVE 'RES'              TO POSTSUM-TRANSTYP                      
082000         MOVE 'W33082D4'         TO POSTSUM-DDNAMN2                       
082100         MOVE 'W33085'           TO POSTSUM-FDNAMN                        
082200         CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2            
082300       ELSE                                                               
082400*------------- WS-FILFLAGGA = 87-FIL                                      
082500         WRITE U087-POST         FROM UTRES-AREA                          
082600         MOVE 'RES'              TO POSTSUM-TRANSTYP                      
082700         MOVE 'W33082D5'         TO POSTSUM-DDNAMN2                       
082800         MOVE 'W33087'           TO POSTSUM-FDNAMN                        
082900         CALL POSTSUM       USING   POSTSUM-PARM POSTSUM-PARM2            
082910       END-IF                                                             
082920     END-IF                                                               
083000     .                                                                    
083100                                                                          
