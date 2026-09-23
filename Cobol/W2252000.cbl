000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2252000.                                    
000300 AUTHOR.                     IDK, 1978 GÖTEBORG.                          
000400 DATE-WRITTEN.               MAJ 1990.                                    
000410 DATE-COMPILED.                                                           
000900                                                                          
001000*    FUNKTION.                                                            
001100*                                                                         
001200*    1.  LÄS EXTERNTABELL (W22503) TILL KONTOTABELL.                      
001300*                                                                         
001400*    2.  LÄS TRANS (W22519) OCH DÅ TRANSEN INNEHÅLLER KONTONR SOM         
001500*        FINNS I KONTOTABELLEN SKER SUMMERING TILL SUMMERINGS-            
001600*        TABELL.FÖREKOMMER SAMMA KONTONR FLERA GÅNGER GÄLLER FÖRST        
001700*        PÅTRÄFFADE.                                                      
001800*                                                                         
001900*    3   VID EOF PÅ TRANSEN UTFÖRES SERVICEGRADSBERÄKNING.                
002000*                                                                         
002100*    4   HÄMTA INFO FRÅN KONTOTABELL (= TEXTER, BRYTNIVÅ) OCH             
002200*        HÄMTA MOTSVARANDE C1+C2+TOTAL INFO FRÅN SUMMERINGSTABELL.        
002300*                                                                         
002400*    5   NÄR KONTONR = 0 I KONTOTABELL INNEBÄR DETTA ATT BRYTNIVÅ         
002500*        PÅTRÄFFATS OCH TOTAL SKRIVES SAMT EV. SUMMERING TILL             
002600*        ÖVERLIGGANDE NIVÅ.                                               
002700*                                                                         
002800*    RETURKODER.                                                          
002900*    U0024                   FEL I PDS-DATA                               
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200 INPUT-OUTPUT SECTION.                                                    
003300 FILE-CONTROL.                                                            
003400     SKIP3                                                                
003500*--------------------------------------- INFILER                          
003600                                                                          
003700*--------------------------------------- EXTERNTABELL INNEHÅLLANDE        
003800*                                        KONTONR, TEXTER OCH              
003900*                                        BRYTNIVÅER                       
004000     SELECT W22503 ASSIGN      W22520D1.                                  
004100                                                                          
004200*--------------------------------------- TRANSAKTIONER                    
004300                                                                          
004400     SELECT W22519 ASSIGN      W22520D2.                                  
004500     SKIP3                                                                
004600*--------------------------------------- UTFILER                          
004700                                                                          
004800*--------------------------------------- SERVICEGRADSLISTA LV/PV          
004900                                                                          
005000     SELECT SG-LISTA ASSIGN UT-S-W22520D3.                                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300 FILE SECTION.                                                            
005400     SKIP3                                                                
005500 FD  W22503                                                               
005600     RECORDING F                                                          
005700     BLOCK 0                                                              
005800     LABEL RECORDS STANDARD.                                              
005900*    -COPY W225W001    -L                                                 
006100     SKIP3                                                                
006200 FD  W22519                                                               
006300     RECORDING F                                                          
006400     BLOCK 0                                                              
006500     LABEL RECORDS STANDARD.                                              
006600*    -COPY W225LI04    -L                                                 
006800     SKIP3                                                                
006900 FD  SG-LISTA                                                             
007000     RECORDING F                                                          
007100     LABEL RECORDS STANDARD.                                              
007200 01  LISTPOST                PIC X(122).                                  
007300     EJECT                                                                
007400 WORKING-STORAGE SECTION.                                                 
007410                                                                          
007420*    -- CHECKED BY WY2000                                                 
007430*                                                                         
007500                                                                          
007600 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
007700                                                                          
007800 01  KONSTANTER.                                                          
007900     03  JA                  PIC X       VALUE 'J'.                       
008000     03  NEJ                 PIC X       VALUE 'N'.                       
008100                                                                          
008200*------------------------------------------------------SWITCHAR           
008300 01  SWITCHAR.                                                            
008400     03  SW-W22503-EOF       PIC X       VALUE 'N'.                       
008500     03  SW-W22519-EOF       PIC X       VALUE 'N'.                       
008600                                                                          
008700 01  IDEX.                                                                
008800     03  IXKTO               PIC S9(9)               COMP SYNC.           
008900     03  IXKTO-ANT           PIC S9(9)   VALUE ZERO  COMP SYNC.           
009000     03  IXTID               PIC S9(9)               COMP SYNC.           
009100     03  IXTOT               PIC S9(9)               COMP SYNC.           
009200     03  IXTOT2              PIC S9(9)               COMP SYNC.           
009300     EJECT                                                                
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
009700     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
009800     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
009900                                                                          
010000*--------------------------------------- ARBETSFÄLT                       
010100                                                                          
010200 01  W.                                                                   
010300     03  W-IDLKTO-N          PIC 9(7).                                    
010400     03  W-TEXT              PIC X(32).                                   
010500     03  W-SGRAD-VKA1        PIC 9(9)V9.                                  
010600     03  W-SGRAD-NUVKA       PIC 9(9)V9.                                  
010700     03  W-SGRAD-PERIOD      PIC 9(9)V9.                                  
010800     03  W-SUROBEL           PIC S9(7)V9(2).                              
010900     03  W-SUROART           PIC S9(7).                                   
011000     03  W-KVRORAD-1-2       PIC S9(7)V99.                                
011100     03  W-KVRORAD-3-4       PIC S9(7)V99.                                
011200     EJECT                                                                
011300*--------------------------------------- AREA FÖR W22503-POST             
011400                                                                          
011500 01  FILLER                  PIC X(8)    VALUE ALL 'A'.                   
011600                                                                          
011700*01  AREA -COPY W225W001   -PRE I03-                                      
011900     EJECT                                                                
012000*--------------------------------------- AREA FÖR W22514-POST             
012100                                                                          
012200 01  FILLER                       PIC X(8)      VALUE ALL 'B'.            
012300                                                                          
012400*01  AREA -COPY W225LI04   -PRE I19-                                      
012600     EJECT                                                                
012700*--------------------------------------- KONTOTABELL                      
012800                                                                          
012900 01  FILLER                  PIC X(8)    VALUE ALL 'C'.                   
013000     SKIP3                                                                
013100 01  KTOMAX                  PIC S9(3)   VALUE +76.                       
013200                                                                          
013300 01  KTOTAB.                                                              
013400     03  KTOTAB-INGANG       OCCURS 20  INDEXED BY IXKAB.                 
013500         05  KTO-IDLKTO-FROM PIC 9(7).                                    
013600         05  KTO-IDLKTO-TOM  PIC 9(7).                                    
013610         05  KTO-IDPROJ      PIC X(4).                                    
013700         05  KTO-TEXT1       PIC X(32).                                   
013800         05  KTO-TEXT2       PIC X(32).                                   
013900         05  KTO-TEXT3       PIC X(32).                                   
014000         05  KTO-NIVAA2      PIC X(2).                                    
014100         05  KTO-NIVAA3      PIC X(2).                                    
014200         05  KTO-NIVAA4      PIC X(2).                                    
014300         05  KTO-NIVAA5      PIC X(2).                                    
014400         05  KTO-SIDBRYT     PIC X.                                       
014500     EJECT                                                                
014600*--------------------------------------- SUMMERINGSTABELL                 
014700                                                                          
014800     04  SUMTAB.                                                          
014900         05  SUM-SUROBEL-CDC PIC S9(7)V9(2).                              
015000         05  SUM-SUROBEL-SDC PIC S9(7)V9(2).                              
015100         05  SUM-SUROART-CDC PIC S9(7).                                   
015200         05  SUM-SUROART-SDC PIC S9(7).                                   
015300         05  SUM-SUROART-CDCC2 PIC S9(7).                                 
015400         05  SUM-KVRORAD-CDC-1-2 PIC S9(7)V99.                            
015500         05  SUM-KVRORAD-CDC-3-4 PIC S9(7)V99.                            
015600         05  SUM-KVRORAD-SDC-1-2 PIC S9(7)V99.                            
015700         05  SUM-KVRORAD-SDC-3-4 PIC S9(7)V99.                            
015800         05  SUMTAB-INGANG2  OCCURS 3.                                    
015900             07  SUM-KVAVBRAD-CDC PIC S9(7)V99.                           
016000             07  SUM-KVFYSAVV-CDC PIC S9(7)V99.                           
016100             07  SUM-KVINORD-CDC PIC S9(7).                               
016200             07  SUM-KVAVBRAD-SDC PIC S9(7)V99.                           
016300             07  SUM-KVFYSAVV-SDC PIC S9(7)V99.                           
016400             07  SUM-KVINORD-SDC PIC S9(7).                               
016500     EJECT                                                                
016600*--------------------------------------- TOTALTABELL                      
016700                                                                          
016800 01  FILLER                  PIC X(8)   VALUE ALL 'D'.                    
016900                                                                          
017000 01  TOTTAB.                                                              
017100     03  TOTTAB-INGANG       OCCURS 4.                                    
017200         05  TOT-SUROBEL-CDC PIC S9(7)V9(2).                              
017300         05  TOT-SUROBEL-SDC PIC S9(7)V9(2).                              
017400         05  TOT-SUROART-CDC PIC S9(7).                                   
017500         05  TOT-SUROART-SDC PIC S9(7).                                   
017600         05  TOT-SUROART-CDCC2 PIC S9(7).                                 
017700         05  TOT-KVRORAD-CDC-1-2 PIC S9(7)V99.                            
017800         05  TOT-KVRORAD-CDC-3-4 PIC S9(7)V99.                            
017900         05  TOT-KVRORAD-SDC-1-2 PIC S9(7)V99.                            
018000         05  TOT-KVRORAD-SDC-3-4 PIC S9(7)V99.                            
018100         05  TOTTAB-INGANG2  OCCURS 3.                                    
018200             07  TOT-KVAVBRAD-CDC PIC S9(7)V99.                           
018300             07  TOT-KVFYSAVV-CDC PIC S9(7)V99.                           
018400             07  TOT-KVINORD-CDC PIC S9(7).                               
018500             07  TOT-KVAVBRAD-SDC PIC S9(7)V99.                           
018600             07  TOT-KVFYSAVV-SDC PIC S9(7)V99.                           
018700             07  TOT-KVINORD-SDC PIC S9(7).                               
018800     EJECT                                                                
018900*--------------------------------------- PARAMETRAR TILL DATKORT          
019000                                                                          
019100 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22520'.                  
019200 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
019300*    -COPY WDATKORT                                                       
019500     EJECT                                                                
019600*--------------------------------------- PARAMETRAR TILL POSTSUM          
019700                                                                          
019800*    -COPY W0005       -PRE POSTSUM-                                      
020000     EJECT                                                                
020100*--------------------------------------PARAMETRAR FÖR LISTPOSTER          
020200 01  SKRIVRAD.                                                            
020300     03  FILLER                PIC X(4) VALUE SPACE.                      
020400     03  WSKRIV-W-TEXT         PIC X(35).                                 
020500     03  WSKRIV-W-SGRAD-VKA1   PIC Z(7)9.9.                               
020600     03  FILLER                PIC X VALUE SPACE.                         
020700     03  WSKRIV-W-SGRAD-PERIOD PIC Z(6).Z.                                
020800     03  FILLER                PIC X VALUE SPACE.                         
020900     03  WSKRIV-W-SGRAD-NUVKA  PIC Z(6).Z.                                
021000     03  WSKRIV-W-SUROBEL      PIC Z(4)BZ(3)BZ(2)9.99.                    
021100     03  FILLER                PIC X(3) VALUE SPACE.                      
021200     03  WSKRIV-W-SUROART      PIC Z(3)BZ(3)BZ(2)9.                       
021300     03  FILLER                PIC X VALUE SPACE.                         
021400     03  WSKRIV-W-KVRORAD-1-2   PIC Z(2)BZ(3)BZ(2)9.                      
021500     03  FILLER                PIC X(3) VALUE SPACE.                      
021600     03  WSKRIV-W-KVRORAD-3-4  PIC Z(2)BZ(3)BZ(2)9.                       
021700*---------------------------------------------BLANK LISTRAD               
021800 01  FILLER.                                                              
021900     03  W-BLANKRAD              PIC X(122) VALUE SPACE.                  
022000     EJECT                                                                
022100*--------------------------------------PARAMETRAR FÖR RUBRIK1             
022200 01  RUBRIK1.                                                             
022300     03  FILLER          PIC X(3) VALUE SPACE.                            
022400     03  FILLER          PIC X(21) VALUE 'VOLVO CAR, PARTS    '.          
022500     03  FILLER          PIC X(11) VALUE 'W22520-001'.                    
022600     03  FILLER          PIC X(47) VALUE                                  
022700         'SERVICEGRAD UPPDELAD I'.                                        
022800     03  FILLER          PIC X(11) VALUE 'AVSER VECKA'.                   
022900     03  W-RUB1-K-STATVECKA PIC Z9.                                       
023000     03  FILLER          PIC X(3) VALUE SPACE.                            
023100     03  W-RUB1-D-AAR    PIC X(3).                                        
023200     03  W-RUB1-D-MAANAD PIC X(3).                                        
023300     03  W-RUB1-D-DAG    PIC XX.                                          
023400     03  FILLER          PIC X(6) VALUE SPACE.                            
023500     03  FILLER          PIC X(5) VALUE 'SIDA'.                           
023600     03  W-RUB1-SID      PIC Z(2)9.                                       
023700     EJECT                                                                
023800*--------------------------------------PARAMETRAR FÖR RUBRIK2             
023900 01  RUBRIK2.                                                             
024000     03  FILLER                   PIC X(38) VALUE SPACE.                  
024100     03  FILLER                   PIC X(50) VALUE                         
024200         '       PROJEKT TILLHÖRIGHET         '.                          
024300                                                                          
024400*--------------------------------------PARAMETRAR FÖR RUBRIK3             
024500 01  RUBRIK3.                                                             
024600     03  FILLER                   PIC X(3) VALUE SPACE.                   
024700     03  FILLER                   PIC X(31) VALUE                         
024800         '.... MODELL....'.                                               
024810*        '.... KONTO ....'.                                               
024900     03  FILLER                   PIC X(23) VALUE                         
025000         'CL      VECKA-1  MVVA1-'.                                       
025100     03  W-RUB3-K-STATVECKA       PIC X(3).                               
025200     03  FILLER                   PIC X(6) VALUE 'VECKA-'.                
025300     03  W-RUB3B-K-STATVECKA      PIC X(8).                               
025400     03  FILLER                   PIC X(10) VALUE 'RO-VÄRDE'.             
025500     03  FILLER                   PIC X(16) VALUE 'RO-ARTIKELNR'.         
025600     03  FILLER                 PIC X(20) VALUE                           
025700       'VECKANS RO-RADER'.                                                
025800     EJECT                                                                
025900*--------------------------------------PARAMETRAR FÖR RUBRIK4             
026000 01  RUBRIK4.                                                             
026100     03  FILLER                   PIC X(3) VALUE SPACE.                   
026200     03  FILLER                   PIC X(45) VALUE                         
026300       '                 BESKRIVNING'.                                    
026310*      'FR.O.M - T.O.M   BESKRIVNING'.                                    
026400     03  FILLER                   PIC X(9) VALUE '%'.                     
026500     03  FILLER                   PIC X(9) VALUE '%'.                     
026600     03  FILLER                   PIC X(14) VALUE '%'.                    
026700     03  FILLER                   PIC X(20) VALUE 'KR'.                   
026800     03  FILLER                   PIC X(11) VALUE 'KLASS 1-2'.            
026900     03  FILLER                   PIC X(9)  VALUE 'KLASS 3-4'.            
027000     EJECT                                                                
027100*--------------------------------------PARAMETRAR FÖR SIDBRYTNING         
027200 01  FILLER.                                                              
027300     03  W-SIDOR                  PIC 999 VALUE 0.                        
027400     03  W-RADRAKN                PIC 99 VALUE 0.                         
027500******************************************************************        
027600     EJECT                                                                
027700 PROCEDURE DIVISION.                                                      
027800 MAIN SECTION.                                                            
027810                                                                          
027900     MOVE +40 TO W-RADRAKN                                                
028000     PERFORM A-INITIERA                                                   
028100     PERFORM S01-LAS-W22503                                               
028200     MOVE +1 TO IXKTO                                                     
028300     PERFORM UNTIL SW-W22503-EOF = 'J'                                    
028400         PERFORM B-LAGRA-KONTOTABELL                                      
028500         PERFORM S01-LAS-W22503                                           
028600         ADD +1 TO IXKTO                                                  
028700         ADD +1 TO IXKTO-ANT                                              
028800     END-PERFORM                                                          
028900     PERFORM S02-LAS-W22519                                               
029000     PERFORM UNTIL SW-W22519-EOF = 'J'                                    
029100         PERFORM C-SUMMERA-INTRESSANTA-TRANSAR                            
029200         PERFORM S02-LAS-W22519                                           
029300     END-PERFORM                                                          
029400     PERFORM D-BILDA-SG-LISTA                                             
029500     PERFORM Z-AVSLUTA                                                    
029600     IF RKOD NOT = ZERO                                                   
029700         CALL ABEND USING RKOD                                            
029800     ELSE                                                                 
029900         MOVE ZERO TO RETURN-CODE                                         
030000         GOBACK                                                           
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 A-INITIERA SECTION.                                                      
030600***************************************************************           
030700*               ÖPPNA FILER OCH LISTA                         *           
030800***************************************************************           
030900                                                                          
031000     OPEN INPUT W22503 W22519                                             
031100          OUTPUT SG-LISTA                                                 
031200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
031300     MOVE D-AAR    TO W-RUB1-D-AAR                                        
031400     MOVE D-MAANAD TO W-RUB1-D-MAANAD                                     
031500     MOVE D-DAG    TO W-RUB1-D-DAG                                        
031600     MOVE ZERO TO KTOTAB                                                  
031700     MOVE ZERO TO TOTTAB                                                  
031800     .                                                                    
031900     EJECT                                                                
032000                                                                          
032100 B-LAGRA-KONTOTABELL SECTION.                                             
032200                                                                          
032300     IF I03-IDLKTO-FROM NOT NUMERIC                                       
032400     OR I03-IDLKTO-TOM NOT NUMERIC                                        
032500     OR I03-NIVAA2 NOT NUMERIC                                            
032600     OR I03-NIVAA3 NOT NUMERIC                                            
032700     OR I03-NIVAA4 NOT NUMERIC                                            
032800     OR I03-NIVAA5 NOT NUMERIC                                            
032900     OR IXKTO GREATER KTOMAX                                              
033000         MOVE +24 TO RKOD                                                 
033100         PERFORM S99-BRYT                                                 
033200     END-IF                                                               
033300                                                                          
033400     MOVE I03-IDLKTO-FROM TO KTO-IDLKTO-FROM (IXKTO)                      
033500     MOVE I03-IDLKTO-TOM TO KTO-IDLKTO-TOM (IXKTO)                        
033510     MOVE I03-IDPROJ     TO KTO-IDPROJ (IXKTO)                            
033600     MOVE I03-TEXT1 TO KTO-TEXT1 (IXKTO)                                  
033700     MOVE I03-TEXT2 TO KTO-TEXT2 (IXKTO)                                  
033800     MOVE I03-TEXT3 TO KTO-TEXT3 (IXKTO)                                  
033900     MOVE I03-NIVAA2 TO KTO-NIVAA2 (IXKTO)                                
034000     MOVE I03-NIVAA3 TO KTO-NIVAA3 (IXKTO)                                
034100     MOVE I03-NIVAA4 TO KTO-NIVAA4 (IXKTO)                                
034200     MOVE I03-NIVAA5 TO KTO-NIVAA5 (IXKTO)                                
034300     MOVE I03-SIDBRYT TO KTO-SIDBRYT (IXKTO)                              
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700 C-SUMMERA-INTRESSANTA-TRANSAR SECTION.                                   
034800                                                                          
034900     SET IXKAB TO 1                                                       
034911     SEARCH KTOTAB-INGANG                                                 
034912          AT END CONTINUE                                                 
034913          WHEN I19-IDPROJ = KTO-IDPROJ (IXKAB)                            
034914               PERFORM CA-SUMMERA-TRANSAR                                 
034916     END-SEARCH                                                           
034917     .                                                                    
034918     EJECT                                                                
034919                                                                          
034920 CA-SUMMERA-TRANSAR SECTION.                                              
034930                                                                          
035400             ADD I19-SUROBEL-CDC    TO SUM-SUROBEL-CDC (IXKAB)            
035500             ADD I19-SUROBEL-SDC    TO SUM-SUROBEL-SDC (IXKAB)            
035600             IF  I19-SUROBEL-CDC > ZERO                                   
035700                 ADD +1 TO SUM-SUROART-CDC (IXKAB)                        
035800             END-IF                                                       
035900             IF  I19-SUROBEL-SDC > ZERO                                   
036000                 ADD +1 TO SUM-SUROART-SDC (IXKAB)                        
036100             END-IF                                                       
036200             IF  I19-SUROBEL-CDC > ZERO                                   
036300             OR  I19-SUROBEL-SDC > ZERO                                   
036400                 ADD +1 TO SUM-SUROART-CDCC2 (IXKAB)                      
036500             END-IF                                                       
036600             ADD I19-KVRORAD-CDC-1-2 (2)                                  
036610                             TO SUM-KVRORAD-CDC-1-2 (IXKAB)               
036700             ADD I19-KVRORAD-CDC-3-4 (2) TO SUM-KVRORAD-CDC-3-4           
036800                                                          (IXKAB)         
036900             ADD I19-KVRORAD-SDC-1-2 (2)                                  
036910                             TO SUM-KVRORAD-SDC-1-2 (IXKAB)               
037000             ADD I19-KVRORAD-SDC-3-4 (2) TO SUM-KVRORAD-SDC-3-4           
037100                                                          (IXKAB)         
037200             MOVE +1 TO IXTID                                             
037300             PERFORM UNTIL IXTID GREATER 3                                
037400                 ADD I19-KVAVBRAD-CDC (IXTID)                             
037500                             TO SUM-KVAVBRAD-CDC (IXKAB, IXTID)           
037600                 ADD I19-KVFYSAVV-CDC (IXTID)                             
037700                             TO SUM-KVFYSAVV-CDC (IXKAB, IXTID)           
037800                 ADD I19-KVINORD-CDC (IXTID)                              
037900                             TO SUM-KVINORD-CDC (IXKAB, IXTID)            
038000                 ADD I19-KVAVBRAD-SDC (IXTID)                             
038100                             TO SUM-KVAVBRAD-SDC (IXKAB, IXTID)           
038200                 ADD I19-KVFYSAVV-SDC (IXTID)                             
038300                             TO SUM-KVFYSAVV-SDC (IXKAB, IXTID)           
038400                 ADD I19-KVINORD-SDC (IXTID)                              
038500                             TO SUM-KVINORD-SDC (IXKAB, IXTID)            
038600                 ADD +1 TO IXTID                                          
038700             END-PERFORM                                                  
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 D-BILDA-SG-LISTA SECTION.                                                
039700                                                                          
039800     MOVE +1 TO IXKTO                                                     
039900     PERFORM UNTIL IXKTO GREATER IXKTO-ANT                                
040000         IF KTO-IDLKTO-FROM (IXKTO) = ZERO                                
040100         AND KTO-IDLKTO-TOM (IXKTO) = ZERO                                
040200             PERFORM DA-SKAPA-TOTALER                                     
040300         ELSE                                                             
040400             PERFORM DB-SKAPA-DETALJRAD                                   
040500         END-IF                                                           
040600         ADD +1               TO IXKTO                                    
040700     END-PERFORM                                                          
040800     .                                                                    
040900     EJECT                                                                
041000                                                                          
041100 DA-SKAPA-TOTALER SECTION.                                                
041200                                                                          
041300     IF KTO-SIDBRYT (IXKTO) = JA                                          
041400         PERFORM F-FLYTTA-RUBRIK                                          
041500     END-IF                                                               
041600     IF KTO-NIVAA3 (IXKTO) NOT = ZERO                                     
041700         MOVE +1 TO IXTOT                                                 
041800         PERFORM DAA-SKRIV-TOTALRAD                                       
041900         MOVE +2 TO IXTOT2                                                
042000         PERFORM DAB-SUMMERA-TOTALER                                      
042100     ELSE                                                                 
042200         IF KTO-NIVAA4 (IXKTO) NOT = ZERO                                 
042300             MOVE +2 TO IXTOT                                             
042400             PERFORM DAA-SKRIV-TOTALRAD                                   
042500             MOVE +3 TO IXTOT2                                            
042600             PERFORM DAB-SUMMERA-TOTALER                                  
042700         ELSE                                                             
042800             IF KTO-NIVAA5 (IXKTO) NOT = ZERO                             
042900             MOVE +3 TO IXTOT                                             
043000             PERFORM DAA-SKRIV-TOTALRAD                                   
043100             MOVE +4 TO IXTOT2                                            
043200             PERFORM DAB-SUMMERA-TOTALER                                  
043300         ELSE                                                             
043400             MOVE +4 TO IXTOT                                             
043500             PERFORM DAA-SKRIV-TOTALRAD                                   
043600             MOVE ZERO TO TOTTAB-INGANG (IXTOT)                           
043700         END-IF                                                           
043800         END-IF                                                           
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300 DAA-SKRIV-TOTALRAD SECTION.                                              
044400     SKIP3                                                                
044500*---------------------------------------- SKAPA TOTALRAD ,C1              
044600                                                                          
044700     PERFORM EB-FLYTTA-BLANKRAD                                           
044800     MOVE KTO-TEXT1 (IXKTO) TO W-TEXT                                     
044900                                                                          
045000     COMPUTE W-SGRAD-VKA1 ROUNDED = TOT-KVAVBRAD-CDC (IXTOT, 1)           
045100             * 100                                                        
045200             / TOT-KVINORD-CDC (IXTOT, 1)                                 
045300         SIZE ERROR MOVE ZERO TO W-SGRAD-VKA1                             
045400     END-COMPUTE                                                          
045500                                                                          
045600     COMPUTE W-SGRAD-NUVKA ROUNDED = TOT-KVAVBRAD-CDC (IXTOT, 2)          
045700              *  100                                                      
045800              / TOT-KVINORD-CDC (IXTOT, 2)                                
045900         SIZE ERROR MOVE ZERO TO W-SGRAD-NUVKA                            
046000     END-COMPUTE                                                          
046100                                                                          
046200     COMPUTE W-SGRAD-PERIOD ROUNDED = TOT-KVAVBRAD-CDC (IXTOT, 3)         
046300                * 100                                                     
046400                / TOT-KVINORD-CDC (IXTOT, 3)                              
046500         SIZE ERROR MOVE ZERO TO W-SGRAD-PERIOD                           
046600     END-COMPUTE                                                          
046700                                                                          
046800     MOVE TOT-SUROBEL-CDC (IXTOT) TO W-SUROBEL                            
046900     MOVE TOT-SUROART-CDC (IXTOT) TO W-SUROART                            
047000     MOVE TOT-KVRORAD-CDC-1-2 (IXTOT) TO W-KVRORAD-1-2                    
047100     MOVE TOT-KVRORAD-CDC-3-4 (IXTOT) TO W-KVRORAD-3-4                    
047200*    PERFORM E-FLYTTA                                                     
047300     SKIP3                                                                
047400*--------------------------------------- SKAPA TOTALRAD, C2               
047500                                                                          
047600     MOVE KTO-TEXT2 (IXKTO) TO W-TEXT                                     
047700                                                                          
047800     COMPUTE W-SGRAD-VKA1 ROUNDED = TOT-KVAVBRAD-SDC (IXTOT, 1)           
047900            * 100                                                         
048000            / TOT-KVINORD-SDC (IXTOT, 1)                                  
048100         SIZE ERROR MOVE ZERO TO W-SGRAD-VKA1                             
048200     END-COMPUTE                                                          
048300                                                                          
048400     COMPUTE W-SGRAD-NUVKA ROUNDED = TOT-KVAVBRAD-SDC (IXTOT, 2)          
048500             * 100                                                        
048600             / TOT-KVINORD-SDC (IXTOT, 2)                                 
048700         SIZE ERROR MOVE ZERO TO W-SGRAD-NUVKA                            
048800     END-COMPUTE                                                          
048900                                                                          
049000     COMPUTE W-SGRAD-PERIOD ROUNDED = TOT-KVAVBRAD-SDC (IXTOT, 3)         
049100                * 100                                                     
049200                / TOT-KVINORD-SDC (IXTOT, 3)                              
049300         SIZE ERROR MOVE ZERO TO W-SGRAD-PERIOD                           
049400     END-COMPUTE                                                          
049500                                                                          
049600     MOVE TOT-SUROBEL-SDC (IXTOT) TO W-SUROBEL                            
049700     MOVE TOT-SUROART-SDC (IXTOT) TO W-SUROART                            
049800     MOVE TOT-KVRORAD-SDC-1-2 (IXTOT) TO W-KVRORAD-1-2                    
049900     MOVE TOT-KVRORAD-SDC-3-4 (IXTOT) TO W-KVRORAD-3-4                    
050000*    PERFORM E-FLYTTA                                                     
050100     SKIP3                                                                
050200*---------------------------------------- SKAPA TOTALRAD, TOTALT          
050300                                                                          
050400     MOVE KTO-TEXT3 (IXKTO) TO W-TEXT                                     
050500                                                                          
050600     COMPUTE W-SGRAD-VKA1 ROUNDED = (TOT-KVAVBRAD-CDC (IXTOT, 1)          
050700                      + TOT-KVAVBRAD-SDC (IXTOT, 1))                      
050800                      * 100                                               
050900                      / (TOT-KVINORD-CDC (IXTOT, 1)                       
051000                      + TOT-KVINORD-SDC (IXTOT, 1))                       
051100         SIZE ERROR MOVE ZERO TO W-SGRAD-VKA1                             
051200     END-COMPUTE                                                          
051300                                                                          
051400     COMPUTE W-SGRAD-NUVKA ROUNDED = (TOT-KVAVBRAD-CDC (IXTOT, 2)         
051500                     + TOT-KVAVBRAD-SDC (IXTOT, 2))                       
051600                     * 100                                                
051700                     /(TOT-KVINORD-CDC (IXTOT, 2)                         
051800                     + TOT-KVINORD-SDC (IXTOT, 2))                        
051900         SIZE ERROR MOVE ZERO TO W-SGRAD-NUVKA                            
052000     END-COMPUTE                                                          
052100                                                                          
052200     COMPUTE W-SGRAD-PERIOD ROUNDED = (TOT-KVAVBRAD-CDC (IXTOT, 3)        
052300                      + TOT-KVAVBRAD-SDC (IXTOT, 3))                      
052400                      * 100                                               
052500                      / (TOT-KVINORD-CDC (IXTOT, 3)                       
052600                      + TOT-KVINORD-SDC (IXTOT, 3))                       
052700         SIZE ERROR MOVE ZERO TO W-SGRAD-PERIOD                           
052800     END-COMPUTE                                                          
052900                                                                          
053000     ADD TOT-SUROBEL-CDC (IXTOT) TOT-SUROBEL-SDC (IXTOT)                  
053100                                GIVING W-SUROBEL                          
053200     MOVE TOT-SUROART-CDCC2 (IXTOT) TO W-SUROART                          
053300     ADD TOT-KVRORAD-CDC-1-2 (IXTOT) TOT-KVRORAD-SDC-1-2 (IXTOT)          
053400                                GIVING W-KVRORAD-1-2                      
053500     ADD TOT-KVRORAD-CDC-3-4 (IXTOT) TOT-KVRORAD-SDC-3-4 (IXTOT)          
053600                                GIVING W-KVRORAD-3-4                      
053700     PERFORM E-FLYTTA                                                     
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100                                                                          
054200 DAB-SUMMERA-TOTALER SECTION.                                             
054300     ADD TOT-SUROBEL-CDC (IXTOT) TO TOT-SUROBEL-CDC (IXTOT2)              
054400     ADD TOT-SUROBEL-SDC (IXTOT) TO TOT-SUROBEL-SDC (IXTOT2)              
054500     ADD TOT-SUROART-CDC (IXTOT) TO TOT-SUROART-CDC (IXTOT2)              
054600     ADD TOT-SUROART-SDC (IXTOT) TO TOT-SUROART-SDC (IXTOT2)              
054700     ADD TOT-SUROART-CDCC2 (IXTOT) TO TOT-SUROART-CDCC2 (IXTOT2)          
054800     ADD TOT-KVRORAD-CDC-1-2 (IXTOT)                                      
054810                     TO TOT-KVRORAD-CDC-1-2 (IXTOT2)                      
054900     ADD TOT-KVRORAD-CDC-3-4 (IXTOT)                                      
054910                     TO TOT-KVRORAD-CDC-3-4 (IXTOT2)                      
055000     ADD TOT-KVRORAD-SDC-1-2 (IXTOT)                                      
055010                     TO TOT-KVRORAD-SDC-1-2 (IXTOT2)                      
055100     ADD TOT-KVRORAD-SDC-3-4 (IXTOT)                                      
055101                     TO TOT-KVRORAD-SDC-3-4 (IXTOT2)                      
055200     MOVE +1 TO IXTID                                                     
055300     PERFORM UNTIL IXTID GREATER 3                                        
055400     ADD TOT-KVAVBRAD-CDC (IXTOT, IXTID) TO TOT-KVAVBRAD-CDC              
055500                                               (IXTOT2, IXTID)            
055600     ADD TOT-KVFYSAVV-CDC (IXTOT, IXTID) TO TOT-KVFYSAVV-CDC              
055700                                               (IXTOT2, IXTID)            
055800     ADD TOT-KVINORD-CDC (IXTOT, IXTID) TO TOT-KVINORD-CDC                
055900                                               (IXTOT2, IXTID)            
056000     ADD TOT-KVAVBRAD-SDC (IXTOT, IXTID) TO TOT-KVAVBRAD-SDC              
056100                                               (IXTOT2, IXTID)            
056200     ADD TOT-KVFYSAVV-SDC (IXTOT, IXTID) TO TOT-KVFYSAVV-SDC              
056300                                               (IXTOT2, IXTID)            
056400     ADD TOT-KVINORD-SDC (IXTOT, IXTID) TO TOT-KVINORD-SDC                
056500                                               (IXTOT2, IXTID)            
056600     ADD +1 TO IXTID                                                      
056700     END-PERFORM                                                          
056800     MOVE ZERO TO TOTTAB-INGANG (IXTOT)                                   
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200 DB-SKAPA-DETALJRAD SECTION.                                              
057300     SKIP3                                                                
057400*---------------------------------------- SKAPA DETALJRAD, C1             
057500                                                                          
057600     IF KTO-SIDBRYT (IXKTO) = JA                                          
057700         PERFORM F-FLYTTA-RUBRIK                                          
057800     END-IF                                                               
057900     PERFORM EB-FLYTTA-BLANKRAD                                           
058000     MOVE KTO-TEXT1 (IXKTO) TO W-TEXT                                     
058100                                                                          
058200     COMPUTE W-SGRAD-VKA1 ROUNDED = SUM-KVAVBRAD-CDC (IXKTO, 1)           
058300                     * 100                                                
058400                     / SUM-KVINORD-CDC (IXKTO, 1)                         
058500         SIZE ERROR MOVE ZERO TO W-SGRAD-VKA1                             
058600     END-COMPUTE                                                          
058700                                                                          
058800     COMPUTE W-SGRAD-NUVKA ROUNDED =  SUM-KVAVBRAD-CDC (IXKTO, 2)         
058900                   * 100                                                  
059000                   / SUM-KVINORD-CDC (IXKTO, 2)                           
059100         SIZE ERROR MOVE ZERO TO W-SGRAD-NUVKA                            
059200     END-COMPUTE                                                          
059300                                                                          
059400     COMPUTE W-SGRAD-PERIOD ROUNDED =  SUM-KVAVBRAD-CDC (IXKTO, 3)        
059500                    * 100                                                 
059600                    / SUM-KVINORD-CDC (IXKTO, 3)                          
059700         SIZE ERROR MOVE ZERO TO W-SGRAD-PERIOD                           
059800     END-COMPUTE                                                          
059900                                                                          
060000     MOVE SUM-SUROBEL-CDC (IXKTO) TO W-SUROBEL                            
060100     MOVE SUM-SUROART-CDC (IXKTO) TO W-SUROART                            
060200     MOVE SUM-KVRORAD-CDC-1-2 (IXKTO) TO W-KVRORAD-1-2                    
060300     MOVE SUM-KVRORAD-CDC-3-4 (IXKTO) TO W-KVRORAD-3-4                    
060400*    PERFORM E-FLYTTA                                                     
060500     SKIP3                                                                
060600*----------------------------------------- SKAPA DETALJRAD, C2            
060700                                                                          
060800     MOVE KTO-TEXT2 (IXKTO) TO W-TEXT                                     
060900                                                                          
061000     COMPUTE W-SGRAD-VKA1 ROUNDED =  SUM-KVAVBRAD-SDC (IXKTO, 1)          
061100                     * 100                                                
061200                     / SUM-KVINORD-SDC (IXKTO, 1)                         
061300         SIZE ERROR MOVE ZERO TO W-SGRAD-VKA1                             
061400     END-COMPUTE                                                          
061500                                                                          
061600     COMPUTE W-SGRAD-NUVKA ROUNDED =  SUM-KVAVBRAD-SDC (IXKTO, 2)         
061700                     * 100                                                
061800                     / SUM-KVINORD-SDC (IXKTO, 2)                         
061900         SIZE ERROR MOVE ZERO TO W-SGRAD-NUVKA                            
062000     END-COMPUTE                                                          
062100                                                                          
062200     COMPUTE W-SGRAD-PERIOD ROUNDED = SUM-KVAVBRAD-SDC (IXKTO, 3)         
062300                     * 100                                                
062400                     / SUM-KVINORD-SDC (IXKTO, 3)                         
062500         SIZE ERROR MOVE ZERO TO W-SGRAD-PERIOD                           
062600     END-COMPUTE                                                          
062700                                                                          
062800     MOVE SUM-SUROBEL-SDC (IXKTO) TO W-SUROBEL                            
062900     MOVE SUM-SUROART-SDC (IXKTO) TO W-SUROART                            
063000     MOVE SUM-KVRORAD-SDC-1-2 (IXKTO) TO W-KVRORAD-1-2                    
063100     MOVE SUM-KVRORAD-SDC-3-4 (IXKTO) TO W-KVRORAD-3-4                    
063200*    PERFORM E-FLYTTA                                                     
063300     SKIP3                                                                
063400*---------------------------------------- SKAPA DETALJRAD, TOTALT         
063500                                                                          
063600     MOVE KTO-TEXT3 (IXKTO) TO W-TEXT                                     
063700                                                                          
063800     COMPUTE W-SGRAD-VKA1 ROUNDED = (SUM-KVAVBRAD-CDC (IXKTO, 1)          
063900                + SUM-KVAVBRAD-SDC (IXKTO, 1))                            
064000                * 100                                                     
064100                / (SUM-KVINORD-CDC (IXKTO, 1)                             
064200                + SUM-KVINORD-SDC (IXKTO, 1))                             
064300         SIZE ERROR MOVE ZERO TO W-SGRAD-VKA1                             
064400     END-COMPUTE                                                          
064500                                                                          
064600     COMPUTE W-SGRAD-NUVKA ROUNDED = (SUM-KVAVBRAD-CDC (IXKTO, 2)         
064700                + SUM-KVAVBRAD-SDC (IXKTO, 2))                            
064800                * 100                                                     
064900                / (SUM-KVINORD-CDC (IXKTO, 2)                             
065000                + SUM-KVINORD-SDC (IXKTO, 2))                             
065100         SIZE ERROR MOVE ZERO TO W-SGRAD-NUVKA                            
065200     END-COMPUTE                                                          
065300                                                                          
065400     COMPUTE W-SGRAD-PERIOD ROUNDED = (SUM-KVAVBRAD-CDC (IXKTO, 3)        
065500                     + SUM-KVAVBRAD-SDC (IXKTO, 3))                       
065600                     * 100                                                
065700                     / (SUM-KVINORD-CDC (IXKTO, 3)                        
065800                     + SUM-KVINORD-SDC (IXKTO, 3))                        
065900         SIZE ERROR MOVE ZERO TO W-SGRAD-PERIOD                           
066000     END-COMPUTE                                                          
066100                                                                          
066200     ADD SUM-SUROBEL-CDC (IXKTO) SUM-SUROBEL-SDC (IXKTO)                  
066300                             GIVING W-SUROBEL                             
066400     MOVE SUM-SUROART-CDCC2 (IXKTO) TO W-SUROART                          
066500     ADD SUM-KVRORAD-CDC-1-2 (IXKTO) SUM-KVRORAD-SDC-1-2 (IXKTO)          
066600                             GIVING W-KVRORAD-1-2                         
066700     ADD SUM-KVRORAD-CDC-3-4 (IXKTO) SUM-KVRORAD-SDC-3-4 (IXKTO)          
066800                             GIVING W-KVRORAD-3-4                         
066900     PERFORM E-FLYTTA                                                     
067000                                                                          
067100*--------------------------------------- SUMMERA TILL TOTALER             
067200                                                                          
067300     IF KTO-NIVAA2 (IXKTO) NOT = ZERO                                     
067400         MOVE +1 TO IXTOT                                                 
067500         PERFORM DBA-SUMMERA-TILL-TOTALER                                 
067600     ELSE                                                                 
067700         IF KTO-NIVAA3 (IXKTO) NOT = ZERO                                 
067800         MOVE +2 TO IXTOT                                                 
067900         PERFORM DBA-SUMMERA-TILL-TOTALER                                 
068000     ELSE                                                                 
068100         IF KTO-NIVAA4 (IXKTO) NOT = ZERO                                 
068200             MOVE +3 TO IXTOT                                             
068300             PERFORM DBA-SUMMERA-TILL-TOTALER                             
068400         ELSE                                                             
068500             IF KTO-NIVAA5 (IXKTO) NOT = ZERO                             
068600                 MOVE +4 TO IXTOT                                         
068700                 PERFORM DBA-SUMMERA-TILL-TOTALER                         
068800             END-IF                                                       
068900             END-IF                                                       
069000         END-IF                                                           
069100     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069500 DBA-SUMMERA-TILL-TOTALER SECTION.                                        
069600                                                                          
069700     ADD SUM-SUROBEL-CDC (IXKTO) TO TOT-SUROBEL-CDC (IXTOT)               
069800     ADD SUM-SUROBEL-SDC (IXKTO) TO TOT-SUROBEL-SDC (IXTOT)               
069900     ADD SUM-SUROART-CDC (IXKTO) TO TOT-SUROART-CDC (IXTOT)               
070000     ADD SUM-SUROART-SDC (IXKTO) TO TOT-SUROART-SDC (IXTOT)               
070100     ADD SUM-SUROART-CDCC2 (IXKTO) TO TOT-SUROART-CDCC2 (IXTOT)           
070200     ADD SUM-KVRORAD-CDC-1-2 (IXKTO)                                      
070210                     TO TOT-KVRORAD-CDC-1-2 (IXTOT)                       
070300     ADD SUM-KVRORAD-CDC-3-4 (IXKTO)                                      
070310                     TO TOT-KVRORAD-CDC-3-4 (IXTOT)                       
070400     ADD SUM-KVRORAD-SDC-1-2 (IXKTO)                                      
070410                     TO TOT-KVRORAD-SDC-1-2 (IXTOT)                       
070500     ADD SUM-KVRORAD-SDC-3-4 (IXKTO)                                      
070510                     TO TOT-KVRORAD-SDC-3-4 (IXTOT)                       
070600     MOVE +1 TO IXTID                                                     
070700     PERFORM UNTIL IXTID GREATER 3                                        
070800     ADD SUM-KVAVBRAD-CDC (IXKTO, IXTID) TO TOT-KVAVBRAD-CDC              
070900                                           (IXTOT, IXTID)                 
071000     ADD SUM-KVFYSAVV-CDC (IXKTO, IXTID) TO TOT-KVFYSAVV-CDC              
071100                                           (IXTOT, IXTID)                 
071200     ADD SUM-KVINORD-CDC (IXKTO, IXTID) TO TOT-KVINORD-CDC                
071300                                           (IXTOT, IXTID)                 
071400     ADD SUM-KVAVBRAD-SDC (IXKTO, IXTID) TO TOT-KVAVBRAD-SDC              
071500                                           (IXTOT, IXTID)                 
071600     ADD SUM-KVFYSAVV-SDC (IXKTO, IXTID) TO TOT-KVFYSAVV-SDC              
071700                                           (IXTOT, IXTID)                 
071800     ADD SUM-KVINORD-SDC (IXKTO, IXTID) TO TOT-KVINORD-SDC                
071900                                           (IXTOT, IXTID)                 
072000     ADD +1          TO IXTID                                             
072100     END-PERFORM                                                          
072200     .                                                                    
072300     EJECT                                                                
072400                                                                          
072500 E-FLYTTA SECTION.                                                        
072600****************************************************************          
072700*      FLYTTAR OCH SKRIVER LISTRADERNA                         *          
072800****************************************************************          
072900     ADD +1 TO W-RADRAKN                                                  
073000     IF W-RADRAKN > 36                                                    
073100         PERFORM F-FLYTTA-RUBRIK                                          
073200     END-IF                                                               
073300     MOVE W-TEXT TO WSKRIV-W-TEXT                                         
073400     MOVE W-SGRAD-VKA1 TO WSKRIV-W-SGRAD-VKA1                             
073500     MOVE W-SGRAD-PERIOD TO WSKRIV-W-SGRAD-PERIOD                         
073600     MOVE W-SGRAD-NUVKA TO WSKRIV-W-SGRAD-NUVKA                           
073700     MOVE W-SUROBEL TO WSKRIV-W-SUROBEL                                   
073800     MOVE W-SUROART TO WSKRIV-W-SUROART                                   
073900     MOVE W-KVRORAD-1-2 TO WSKRIV-W-KVRORAD-1-2                           
074000     MOVE W-KVRORAD-3-4 TO WSKRIV-W-KVRORAD-3-4                           
074100     WRITE LISTPOST FROM SKRIVRAD                                         
074200     .                                                                    
074300     EJECT                                                                
074400                                                                          
074500 EB-FLYTTA-BLANKRAD SECTION.                                              
074600     ADD +1 TO W-RADRAKN                                                  
074700     WRITE LISTPOST FROM W-BLANKRAD                                       
074800     .                                                                    
074900     EJECT                                                                
075000                                                                          
075100 F-FLYTTA-RUBRIK SECTION.                                                 
075200*****************************************************************         
075300* FLYTTAR OCH SKRIVER RUBRIKER  (RÄKNAR SIDOR)                  *         
075400*****************************************************************         
075500     ADD +1 TO W-SIDOR                                                    
075600     MOVE W-SIDOR TO W-RUB1-SID                                           
075700     MOVE ZERO TO W-RADRAKN                                               
075800     MOVE K-STATVECKA TO W-RUB1-K-STATVECKA                               
075900     MOVE K-STATVECKA TO W-RUB3-K-STATVECKA                               
076000     MOVE K-STATVECKA TO W-RUB3B-K-STATVECKA                              
076100     WRITE LISTPOST FROM RUBRIK1 AFTER PAGE                               
076200     WRITE LISTPOST FROM RUBRIK2                                          
076300     WRITE LISTPOST FROM W-BLANKRAD                                       
076400     WRITE LISTPOST FROM RUBRIK3                                          
076500     WRITE LISTPOST FROM RUBRIK4                                          
076600     WRITE LISTPOST FROM W-BLANKRAD                                       
076700     .                                                                    
076800     EJECT                                                                
076900                                                                          
077000 Z-AVSLUTA SECTION.                                                       
077100**************************************************************            
077200*    AVSLUTA OCCH STÄNGA LISTAN                              *            
077300**************************************************************            
077400                                                                          
077500     CLOSE W22503 W22519 SG-LISTA                                         
077600     MOVE 'S' TO POSTSUM-OPKOD                                            
077700     CALL POSTSUM USING POSTSUM-PARM                                      
077800     .                                                                    
077900     EJECT                                                                
078000                                                                          
078100 S01-LAS-W22503 SECTION.                                                  
078200***************************************************************           
078300*              SEKVENSIELL LÄS RUTIN                          *           
078400***************************************************************           
078500                                                                          
078600     READ W22503 INTO I03-AREA                                            
078700      AT END MOVE 'J' TO SW-W22503-EOF                                    
078800     END-READ                                                             
078900     IF SW-W22503-EOF = 'N'                                               
079000         MOVE 'W22503' TO POSTSUM-FDNAMN                                  
079100         MOVE 'W22503D1' TO POSTSUM-DDNAMN2                               
079200         CALL POSTSUM USING POSTSUM-PARM                                  
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600                                                                          
079700                                                                          
079800 S02-LAS-W22519 SECTION.                                                  
079900***************************************************************           
080000*              SEKVENSIELL LÄS RUTIN                          *           
080100***************************************************************           
080200                                                                          
080300     READ W22519 INTO I19-AREA                                            
080400      AT END MOVE 'J' TO SW-W22519-EOF                                    
080500     END-READ                                                             
080600     IF SW-W22519-EOF = 'N'                                               
080700         MOVE 'W22519' TO POSTSUM-FDNAMN                                  
080800         MOVE 'W22519D2' TO POSTSUM-DDNAMN2                               
080900         CALL POSTSUM USING POSTSUM-PARM                                  
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300                                                                          
081400 S99-BRYT SECTION.                                                        
081500***************************************************************           
081600*  ABEND RUTIN                                                *           
081700***************************************************************           
081800     CALL ABEND USING RKOD                                                
081900     .                                                                    
082000                                                                          
