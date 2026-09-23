000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2214500.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   94/01/31.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UTSKRIFT                                                         
001100*        BESTÄLLNINGS OCH ANNULLATIONS-RAPPORT                            
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)   SEK IX                     
001410*        PROGRAMMET LÄSER              WDD2                               
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- TRANSAKTIONER IN                                           
002900     SELECT W22141                     ASSIGN TO W22145D1.                
003000     SKIP2                                                                
003100*          --- BESTÄLLNINGS OCH ANNULATIONS-RAPPORT                       
003200     SELECT W22145-001                 ASSIGN TO W22145D2.                
003300     SKIP2                                                                
003400*          --- SORTERINGSFIL                                              
003500     SELECT SORTFIL                    ASSIGN TO W22145DS.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W22141                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  -COPY W221LI41      -L.                                              
004600     SKIP3                                                                
004700 FD  W22145-001                                                           
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100 01  W22145-001-RAD              PIC X(121).                              
005200     EJECT                                                                
005300 SD  SORTFIL.                                                             
005400     SKIP2                                                                
005500*01  POST -COPY W221LI41      -PRE SORT-                                  
005600     03  SORT-LISTA             PIC X.                                    
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
005901                                                                          
005910*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W2214500'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 01  W-AARSBEH                   PIC S9(7)   VALUE ZERO COMP-3.           
006410 01  OLD-IDANSK                  PIC S9(3)   VALUE ZERO COMP-3.           
006420 01  OLD-LISTA                   PIC X(1)    VALUE SPACE.                 
006500                                                                          
006600 77  W22141-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W22141                       VALUE 'J'.                   
006800                                                                          
006900 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007000     88  END-OF-SORTFIL                      VALUE 'J'.                   
007100     SKIP3                                                                
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500     SKIP2                                                                
008600*    --- PARAMETRAR TILL ABEND                                            
008700                                                                          
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL DATKORT                                          
009600*                                                                         
009700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22145'.              
009800     SKIP2                                                                
009900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010000     SKIP2                                                                
010100*01  -COPY WDATKORT                                                       
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                 'IN-AREA-START  '.                       
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W221LI41     -PRE IN-                                     
011200     EJECT                                                                
011300 01  W001-AREA-START             PIC X(24)   VALUE                        
011400                                 'W001-AREA-START  '.                     
011500     SKIP2                                                                
011600 01  W001-HJALPAREOR.                                                     
011700*                                                                         
011800     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
011900     03  W001-ANTAL-RADER                                                 
012000                                 PIC 9(3)    VALUE 999.                   
012100     03  W001-MAX-RADER-PER-SIDA                                          
012200                                 PIC 9(3)    VALUE 42.                    
012300     03  W001-MAX-POSITIONER-PER-RAD                                      
012400                                 PIC 9(3)    VALUE 120.                   
012500     03  W001-LISTNR             PIC X(11)   VALUE 'W22145-001'.          
012600     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
012610     03  W001-BESTLIST           PIC X(030)                               
012620                         VALUE ' BESTÄLLNINGAR ANSKAFFARE: '.             
012630     03  W001-ANNULIST           PIC X(030)                               
012640                         VALUE ' ANNULLATIONER ANSKAFFARE: '.             
012700     SKIP3                                                                
012800 01  W001-RAD.                                                            
012900*                                                                         
013000     03  FILLER                  PIC X(121)  VALUE SPACE.                 
013100     EJECT                                                                
013200 01  W001-RUBRIK1.                                                        
013300*                                                                         
013400     03  FILLER                  PIC X(3) VALUE SPACE.                    
013500     03  FILLER                  PIC X(21)                                
013600                                VALUE 'VOLVO CAR AFTERSALES '.            
013700     03  FILLER                  PIC X(12)                                
013800                                 VALUE 'W22145-001'.                      
013900     03  FILLER                  PIC X(56)                                
014000         VALUE 'BESTÄLLNINGS- OCH ANNULLATIONSRAPPORT '.                  
014100     03  W001-DATUM              PIC XXBXXBXX.                            
014200     03  FILLER                  PIC X(6)                                 
014300                                 VALUE '  SID'.                           
014400     03  W001-SID                PIC Z(4)9.                               
014500     SKIP2                                                                
014600 01  W001-RUBRIK2.                                                        
014700     03  W001-LISTTYP            PIC X(030)                               
014800                                 VALUE SPACE.                             
014900     03  W001-IDANSK             PIC Z(3).                                
015000 01  W001-RUBRIK3.                                                        
015100     03  FILLER                  PIC X(39)                                
015200         VALUE '    ARTIKEL  BENÄMNING'.                                  
015300     03  FILLER                  PIC X(55)                                
015400         VALUE ' LEVNR   ÅRSBEHOV KÖPFÖRSL  ORSAK           '.            
015410     03  FILLER                  PIC X(20)                                
015420         VALUE '      DATUM INKÖP   '.                                    
015500     EJECT                                                                
015600 01  W001-DETALJ1.                                                        
015700     03  FILLER                  PIC X(2) VALUE SPACE.                    
015800     03  W001-IDARTNR            PIC Z(9).                                
015900     03  FILLER                  PIC X(2) VALUE SPACE.                    
016000     03  W001-BEART              PIC X(25).                               
016100     03  FILLER                  PIC X(2) VALUE SPACE.                    
016200     03  W001-IDLEVNR            PIC X(5).                                
016300     03  FILLER                  PIC X(2) VALUE SPACE.                    
016400     03  W001-AARSBEH            PIC Z(9).                                
016500     03  FILLER                  PIC X(2) VALUE SPACE.                    
016600     03  W001-KVBEST-BER         PIC Z(7).                                
016700     03  FILLER                  PIC X(2) VALUE SPACE.                    
016800     03  W001-ORSAK              PIC X(9).                                
016900     03  FILLER                  PIC X(2) VALUE SPACE.                    
016910     03  W001-PAAM               PIC X(10).                               
016920     03  FILLER                  PIC X(12) VALUE SPACE.                   
016930     03  W001-TIINKOP            PIC 9(6)  BLANK WHEN ZERO.               
017000     EJECT                                                                
017100 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
017200                                  'SORTWS-AREA-START  '.                  
017300     SKIP2                                                                
017400                                                                          
017500*01  AREA -COPY W221LI41      -PRE SORTWS-                                
017600     03  SORTWS-LISTA            PIC X.                                   
017700 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
017800     EJECT                                                                
017900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018000*                                                                         
018100                                                                          
018200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018300     SKIP3                                                                
018400 01  NYCKLAR-TILL-DLI.                                                    
018500     03  W-IDARTNR-X.                                                     
018600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018700     03  W-KDSEGKEY-X.                                                    
018800         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
019300     03  W-IDSKYLT-X.                                                     
019400         05  W-IDSKYLT           PIC X(3)    VALUE 'S'.                   
019500     SKIP2                                                                
019600*    --- STATUS-KOD FRÅN IMS                                              
019700 01  STATUS-WS                   PIC XX.                                  
019800     88  SEGMENT-FINNS                       VALUE '  '.                  
019900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020100     SKIP2                                                                
020200 01  GODK-STATUSKODER.                                                    
020300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020400     SKIP3                                                                
020500 01  SSA1                        PIC X(64).                               
020600 01  SSA2                        PIC X(64).                               
020700     EJECT                                                                
020800*    --- IMS FUNKTIONSKODER                                               
020900*01  -COPY W0003                                                          
021000     EJECT                                                                
021100*    ---  DLI INPUT-OUTPUT AREA                                           
021200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021300     SKIP3                                                                
021400 01  DLI-IO-AREA-01.                                                      
021500     03  IO-AREA-01              PIC X(150)  VALUE SPACE.                 
021600     SKIP3                                                                
021700     03  WLARTC01 REDEFINES IO-AREA-01.                                   
021800*        05  -COPY WDK601                                                 
021900     EJECT                                                                
021910 01  DLI-IO-AREA-11.                                                      
021920     03  IO-AREA-11              PIC X(900)  VALUE SPACE.                 
021930     SKIP3                                                                
022000     03  WLARTC11 REDEFINES IO-AREA-11.                                   
022100*        05  -COPY WDK611                                                 
022500     EJECT                                                                
022510 01  DLI-IO-AREA.                                                         
022520     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
022530     SKIP3                                                                
022600     03  WLBENA01 REDEFINES IO-AREA.                                      
022700*        05  -COPY WDD301  -PRE BENA01-                                   
022800     EJECT                                                                
022900     03  WLBENA11 REDEFINES IO-AREA.                                      
023000*        05  -COPY WDD311  -PRE BENA11-                                   
023010     EJECT                                                                
023020 01  DLI-IO-AREA-2.                                                       
023040                                                                          
023050     03  WDD201.                                                          
023060*        05  -COPY WDD201  -PRE NYPON-                                    
023100     EJECT                                                                
023200 LINKAGE SECTION.                                                         
023300                                                                          
023400                                                                          
023500*01  -COPY W0008  -PRE ARTC-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE BENA-                                              
023900     05  FILLER                  PIC X.                                   
023910     EJECT                                                                
023920*01  -COPY W0008  -PRE WDD2-                                              
023930     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING ARTC-PCB BENA-PCB WDD2-PCB.                    
024200     ENTRY 'DLITCBL' USING ARTC-PCB BENA-PCB WDD2-PCB.                    
024300                                                                          
024400     SKIP2                                                                
024500     PERFORM A-INIT                                                       
024600                                                                          
024700     SORT SORTFIL ASCENDING KEY SORT-IDANSK                               
024800                                SORT-LISTA                                
024900                                SORT-IDARTNR                              
025000               INPUT  PROCEDURE B-SORT-INPUT                              
025100               OUTPUT PROCEDURE C-SORT-OUTPUT                             
025200                                                                          
025300     IF SORT-RETURN NOT = 0                                               
025400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
025500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
025600           DELIMITED BY SIZE                                              
025700           INTO FELTEXT-STR                                               
025800       DISPLAY FELTEXT                                                    
025900       PERFORM S99-ABEND                                                  
026000     ELSE                                                                 
026100       PERFORM Z-FINIT                                                    
026200                                                                          
026300       MOVE ZERO TO RETURN-CODE                                           
026400       GOBACK                                                             
026500     END-IF                                                               
026600                                                                          
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000                                                                          
027100     OPEN INPUT  W22141                                                   
027200     OPEN OUTPUT W22145-001                                               
027300     SKIP2                                                                
027400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
027500     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
027600     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
027700     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
027710     MOVE DAGENS-DATUM   TO W001-DATUM                                    
027800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027900     .                                                                    
028000     EJECT                                                                
028100 B-SORT-INPUT  SECTION.                                                   
028200     SKIP2                                                                
028300     PERFORM S01-LAES-W22141                                              
028400     PERFORM UNTIL END-OF-W22141                                          
028500         MOVE IN-AREA TO SORTWS-AREA                                      
028600         IF IN-KDBEH-ORSAK = 2 OR IN-KDBEH-ORSAK = 5                      
028700            MOVE '1' TO SORTWS-LISTA                                      
028800         ELSE                                                             
028900            MOVE '2' TO SORTWS-LISTA                                      
029000         END-IF                                                           
029100         PERFORM S31-SORT-RELEASE                                         
029200       PERFORM S01-LAES-W22141                                            
029300     END-PERFORM                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 C-SORT-OUTPUT SECTION.                                                   
029700     SKIP2                                                                
029800     PERFORM S32-SORT-RETURN                                              
029900     PERFORM UNTIL END-OF-SORTFIL                                         
030000       PERFORM CA-SKAPA-LISTA                                             
030010       IF SEGMENT-FINNS                                                   
030100          PERFORM S21-SKRIV-W22145-001                                    
030110       END-IF                                                             
030200       PERFORM S32-SORT-RETURN                                            
030300     END-PERFORM                                                          
030400     .                                                                    
030500     EJECT                                                                
030600 CA-SKAPA-LISTA SECTION.                                                  
030700     SKIP2                                                                
030800     MOVE SORTWS-IDARTNR TO W-IDARTNR                                     
030900     PERFORM IMS-GET-ARTC-ROT                                             
031020     IF SEGMENT-FINNS                                                     
031100        PERFORM IMS-GET-ARTC-CLAG                                         
031110     END-IF                                                               
031200                                                                          
031210     IF SEGMENT-FINNS                                                     
031300        COMPUTE W-AARSBEH ROUNDED = (CLAG-KVPB-SATS +                     
031400                                     CLAG-KVPB-SEP  +                     
031500                                     CLAG-KVPB-TPO) * 12                  
031600        MOVE W-AARSBEH         TO W001-AARSBEH                            
031700                                                                          
031800        PERFORM IMS-GET-BENA-BSEQ                                         
031810        IF SEGMENT-FINNS                                                  
031900           PERFORM IMS-GET-BENA-SPRAK                                     
031901        ELSE                                                              
031902           MOVE SPACE          TO BENA11-TEXT-BEART                       
031903           MOVE SPACE          TO STATUS-WS                               
031910        END-IF                                                            
032000        MOVE BENA11-TEXT-BEART TO W001-BEART                              
032100                                                                          
032101        PERFORM IMS-GET-NYPON-ROT                                         
032110        IF SEGMENT-FINNS                                                  
032120           MOVE NYPON-ART-TIINKOP TO W001-TIINKOP                         
032121        ELSE                                                              
032122           MOVE ZERO              TO W001-TIINKOP                         
032130        END-IF                                                            
032200        MOVE SORTWS-IDARTNR    TO W001-IDARTNR                            
032300        MOVE SORTWS-IDANSK     TO W001-IDANSK                             
032400        MOVE SORTWS-IDLEVNR    TO W001-IDLEVNR                            
032500        MOVE SORTWS-KVBEST-BER TO W001-KVBEST-BER                         
032600                                                                          
032700        IF SORTWS-KDBEH-ORSAK = 2                                         
032800           MOVE 'BEST'         TO W001-ORSAK                              
032900        ELSE                                                              
033000           IF SORTWS-KDBEH-ORSAK = 3                                      
033100              MOVE 'ANNU'         TO W001-ORSAK                           
033200           ELSE                                                           
033300              IF SORTWS-KDBEH-ORSAK = 4                                   
033400                 MOVE 'ERS '         TO W001-ORSAK                        
033500              ELSE                                                        
033600                 IF SORTWS-KDBEH-ORSAK = 5                                
033700                    MOVE 'BEST  MAN'    TO W001-ORSAK                     
033800                 ELSE                                                     
033900                    IF SORTWS-KDBEH-ORSAK = 6                             
034000                       MOVE 'ANNU  MAN'    TO W001-ORSAK                  
034100                    ELSE                                                  
034200                       MOVE '????'         TO W001-ORSAK                  
034300                    END-IF                                                
034400                 END-IF                                                   
034500              END-IF                                                      
034600           END-IF                                                         
034700        END-IF                                                            
034701        IF SORTWS-FLAGGA-PAAM = JA                                        
034702           MOVE 'PÅMINNELSE'               TO W001-PAAM                   
034703        ELSE                                                              
034704           MOVE SPACE                      TO W001-PAAM                   
034705        END-IF                                                            
034710     END-IF                                                               
034800                                                                          
034900     .                                                                    
035000     EJECT                                                                
035100 Z-FINIT SECTION.                                                         
035200     CLOSE W22141                                                         
035300           W22145-001                                                     
035400     SKIP2                                                                
035500     MOVE 'S' TO POSTSUM-OPKOD                                            
035600     CALL POSTSUM USING POSTSUM-PARM                                      
035700     .                                                                    
035800     EJECT                                                                
035900 S01-LAES-W22141  SECTION.                                                
036000     SKIP2                                                                
036100     READ W22141 INTO IN-AREA                                             
036200     AT END                                                               
036300        SET END-OF-W22141 TO TRUE                                         
036400                                                                          
036500     NOT AT END                                                           
036600        MOVE 'W22141'   TO POSTSUM-FDNAMN                                 
036700        MOVE 'W22145D1' TO POSTSUM-DDNAMN2                                
036800        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
036900        CALL POSTSUM USING POSTSUM-PARM                                   
037000     END-READ                                                             
037100     .                                                                    
037200     EJECT                                                                
037300 S21-SKRIV-W22145-001  SECTION.                                           
037400     SKIP2                                                                
037500     MOVE 1 TO W001-SKIP                                                  
037600     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA OR                     
037700        SORTWS-IDANSK NOT = OLD-IDANSK OR                                 
037800        SORTWS-LISTA  NOT = OLD-LISTA                                     
037900       PERFORM S21A-SKRIV-RUBRIKER                                        
037910       MOVE SORTWS-IDANSK TO OLD-IDANSK                                   
037920       MOVE SORTWS-LISTA  TO OLD-LISTA                                    
038000     END-IF                                                               
038100     SKIP2                                                                
038200     WRITE W22145-001-RAD FROM W001-DETALJ1 AFTER W001-SKIP               
038300     SKIP2                                                                
038400     MOVE SPACE TO W001-RAD                                               
038500     ADD  +1 TO W001-ANTAL-RADER                                          
038600     .                                                                    
038700     EJECT                                                                
038800 S21A-SKRIV-RUBRIKER SECTION.                                             
038900     SKIP2                                                                
039000     ADD +1 TO W001-SIDRAKNARE                                            
039100     MOVE W001-SIDRAKNARE TO W001-SID                                     
039200     WRITE W22145-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
039210     IF SORTWS-LISTA = '1'                                                
039220        MOVE W001-BESTLIST TO W001-LISTTYP                                
039230     ELSE                                                                 
039231        MOVE W001-ANNULIST TO W001-LISTTYP                                
039240     END-IF                                                               
039300     WRITE W22145-001-RAD FROM W001-RUBRIK2 AFTER 2                       
039310     WRITE W22145-001-RAD FROM W001-RUBRIK3 AFTER 2                       
039400     MOVE +9 TO W001-ANTAL-RADER                                          
039500     SKIP2                                                                
039600     MOVE 3 TO W001-SKIP                                                  
039700     .                                                                    
039800     EJECT                                                                
039900 S31-SORT-RELEASE  SECTION.                                               
040000     SKIP2                                                                
040100     RELEASE SORT-POST FROM SORTWS-AREA                                   
040200     .                                                                    
040300     EJECT                                                                
040400 S32-SORT-RETURN  SECTION.                                                
040500     SKIP2                                                                
040600     RETURN SORTFIL INTO SORTWS-AREA                                      
040700     AT END                                                               
040800         SET END-OF-SORTFIL TO TRUE                                       
040900     .                                                                    
041000     EJECT                                                                
041100 S99-ABEND SECTION.                                                       
041200     SKIP2                                                                
041300     SKIP2                                                                
041400     MOVE 'S' TO POSTSUM-OPKOD                                            
041500     CALL POSTSUM USING POSTSUM-PARM                                      
041600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
041700     .                                                                    
041800     EJECT                                                                
041900* --- IMS SEKTIONER ---                                                   
042000     SKIP3                                                                
042100 IMS-GET-ARTC-ROT SECTION.                                                
042200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042300          DELIMITED BY SIZE INTO SSA1                                     
042400     MOVE '  GE' TO GODK-STATUSKODER                                      
042500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
042600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042700     PERFORM IMS-STATUSKONTROLL                                           
042800     .                                                                    
042900     EJECT                                                                
043000 IMS-GET-ARTC-CLAG   SECTION.                                             
043100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
043200          DELIMITED BY SIZE INTO SSA1                                     
043300     MOVE '  GE' TO GODK-STATUSKODER                                      
043400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
043500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
044600     .                                                                    
044700     EJECT                                                                
044800 IMS-GET-BENA-BSEQ SECTION.                                               
044900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
045000          DELIMITED BY SIZE INTO SSA1                                     
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
045300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-GET-BENA-SPRAK SECTION.                                              
045800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
045900          DELIMITED BY SIZE INTO SSA1                                     
046000     MOVE '  GE' TO GODK-STATUSKODER                                      
046100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
046200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
046300     PERFORM IMS-STATUSKONTROLL                                           
046310     IF SEGMENT-SAKNAS                                                    
046311        MOVE SPACE             TO BENA11-TEXT-BEART                       
046312        MOVE SPACE             TO STATUS-WS                               
046320     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046510 IMS-GET-NYPON-ROT SECTION.                                               
046520     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
046530          DELIMITED BY SIZE INTO SSA1                                     
046540     MOVE '  GE' TO GODK-STATUSKODER                                      
046550     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-AREA-2 SSA1                    
046560     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
046570     PERFORM IMS-STATUSKONTROLL                                           
046580     .                                                                    
046590     EJECT                                                                
046600 IMS-STATUSKONTROLL SECTION.                                              
046700     SKIP2                                                                
046800     SET STATUS-IX TO 1                                                   
046900     SEARCH GODK-STATUS                                                   
047000       AT END                                                             
047100         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
047200         DISPLAY FELTEXT                                                  
047300         CALL FELLOG                                                      
047400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047500         CONTINUE                                                         
047600     END-SEARCH                                                           
047700     .                                                                    
