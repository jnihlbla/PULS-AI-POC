000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2312000.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.               MAJ 1979.                                    
000600     REMARKS.                                                             
000700*        NEDLÄSNING AV LEVERANSPLAN.                                      
000800*        PROGRAMMET ÄR ETT FSU-EXIT SOM LÄSER ORDERINGÅNGS-               
000900*        REGISTRET WDL8.                                                  
001000*        UTFILEN SKALL ENDAST INEHÅLLA ARTIKLAR MED ORDERINGÅNG           
001100*        DE 12 SISTA PERIODERNA.                                          
001200*                                                                         
001300*        ÄNDRAT TILL SB/COBOL-11    890817 GUNNEL E.                      
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900*                            *** NEDLÄSNING AV ORDERINGÅNG                
002000*                            *** OUTPUT                                   
002100     SELECT W23121  ASSIGN   UT-S-W23120D1.                               
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD  W23121                                                               
002700     LABEL RECORD STANDARD                                                
002800     RECORDING MODE F                                                     
002900     BLOCK CONTAINS 0 RECORDS.                                            
003000     SKIP3                                                                
003100*01  POST   -COPY W231212 -PRE U21ORD- -L.                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500     SKIP3                                                                
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 01  W.                                                                   
003700*----------------------------------------- KONSTANTER                     
003800     03  JA              PIC X       VALUE 'J'.                           
003900     03  NEJ             PIC X       VALUE 'N'.                           
004000     03  MAX-ANT-PERIODER                                                 
004100                         PIC S9(9)   VALUE +12       COMP SYNC.           
004200     03  OING-PROGNOSPAV-C1                                               
004300                         PIC S9(3)   VALUE +11       COMP-3.              
004400     03  OING-DIVERSE-C1 PIC S9(3)   VALUE +13       COMP-3.              
004500     03  OING-SATS-C1    PIC S9(3)   VALUE +14       COMP-3.              
004600     03  OING-PROGNOSPAV-C2                                               
004700                         PIC S9(3)   VALUE +21       COMP-3.              
004800     03  OING-DIVERSE-C2 PIC S9(3)   VALUE +23       COMP-3.              
004900     03  OING-SATS-C2    PIC S9(3)   VALUE +24       COMP-3.              
004901     SKIP3                                                                
005100*----------------------------------------- ARBETSAREOR                    
005200 01  W.                                                                   
005300     SKIP1                                                                
005400     03  IX                  PIC S9(9)               COMP-3.              
005410     03  IX-W                PIC S9(9)               COMP-3.              
005420     03  IX-UT               PIC S9(9)               COMP-3.              
005500     03  IDARTNR-WS          PIC S9(9)               COMP-3.              
005601                                                                          
005602 01  WS-DAGENS-AAVV          PIC 9(4).                                    
005603 01  FILLER REDEFINES WS-DAGENS-AAVV.                                     
005604     03 WS-DAGENS-AA         PIC 9(2).                                    
005605     03 WS-DAGENS-VV         PIC 9(2).                                    
005606                                                                          
005607 01  WS-AARP                 PIC 9(4).                                    
005608 01  FILLER REDEFINES WS-AARP.                                            
005609     03 WS-AA                PIC 9(2).                                    
005610     03 WS-RP                PIC 9(2).                                    
005611                                                                          
005612 01  WS-FORSTA-VECKAN        PIC 9(4).                                    
005613 01  FILLER REDEFINES WS-FORSTA-VECKAN.                                   
005614     03 WS-TIAA              PIC 9(2).                                    
005615     03 WS-TIVV              PIC 9(2).                                    
005616 01  WS-SISTA-VECKAN         PIC 9(4).                                    
005617 01  WS-KVVIPER              PIC 9.                                       
005620                                                                          
005693 01  W-PERIOD-AAAARP         PIC 9(6).                                    
005694 01  FILLER REDEFINES W-PERIOD-AAAARP.                                    
005800     03  W-PERIOD-TISEKEL    PIC 9(2).                                    
005810     03  W-PERIOD-AARP       PIC 9(4).                                    
005900     03  W-PER               REDEFINES W-PERIOD-AARP.                     
006000         05  W-PERIOD-AA     PIC 9(2).                                    
006100         05  W-PERIOD-RP     PIC 9(2).                                    
006110 01  FILLER REDEFINES W-PERIOD-AAAARP.                                    
006130     03  W-PERIOD-AAAA       PIC 9(4).                                    
006140     03  FILLER              PIC 9(2).                                    
006200                                                                          
006201 01  UTPOSTTAB.                                                           
006202   02  FILLER   OCCURS 12.                                                
006210     03  W-SISTA-PER-AAAARP      PIC 9(6).                                
006220     03  FILLER   REDEFINES W-SISTA-PER-AAAARP.                           
006230         05  W-SISTA-PER-AAAA    PIC 9(4).                                
006240         05  W-SISTA-PER-RP      PIC 9(2).                                
006250     03  FILLER   REDEFINES W-SISTA-PER-AAAARP.                           
006251         05  W-SISTA-PER-TISEKEL PIC 9(2).                                
006260         05  W-SISTA-PER-AARP    PIC 9(4).                                
006500                                                                          
006510 01  PER-TAB.                                                             
006520     02  FILLER   OCCURS 12.                                              
006530       03  PER-NRAARP        PIC 9(4).                                    
006540       03  FILLER REDEFINES PER-NRAARP.                                   
006550           05  PER-AA        PIC 9(2).                                    
006560           05  PER-RP        PIC 9(2).                                    
006561       03  PER-VECKA-FOM     PIC 9(4).                                    
006562       03  FILLER REDEFINES PER-VECKA-FOM.                                
006563           05  PER-FOMAAR    PIC 9(2).                                    
006564           05  PER-FOMVKA    PIC 9(2).                                    
006570       03  PER-VECKA-TOM     PIC 9(4).                                    
006580       03  FILLER REDEFINES PER-VECKA-TOM.                                
006590           05  PER-TOMAAR    PIC 9(2).                                    
006591           05  PER-TOMVKA    PIC 9(2).                                    
006592       03  PER-KVVIPER       PIC 9(2).                                    
006593                                                                          
006600 01  SWITCHAR.                                                            
006700     03  SW-FORSTA-SEGMENT   PIC X(1)    VALUE 'J'.                       
006800     03  SW-ORDING-ARTIKEL   PIC X(1)    VALUE 'N'.                       
006900     SKIP3                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
007200     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
007300     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
007400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
007410     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
007500     EJECT                                                                
007600*                            *** PARAMETRAR TILL POSTSUM                  
007700     SKIP3                                                                
007800*01  -COPY W0005        -PRE POSTSUM-.                                    
008000     EJECT                                                                
008100*                            *** PARAMETRAR TILL DATUMKORT                
008200 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W23120'.                  
008300 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
008400     SKIP3                                                                
008500*01  -COPY WDATKORT.                                                      
008600     EJECT                                                                
008610*01  -COPY WDATAREA                                                       
008620     EJECT                                                                
009000*                            *************************************        
009100*                            *** AREA FÖR W23121               ***        
009200*                            *************************************        
009300     SKIP1                                                                
009400*01  AREA  -COPY W231212 -PRE U21ORD-.                                    
009600     EJECT                                                                
009700 01  IMS-WS.                                                              
009800     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
009900*----------------------------------STATUSKODER FRÅN IMS                   
010000     03  STATUS-WS   PIC XX.                                              
010100         88  SEGMENT-FINNS       VALUE '  '.                              
010200         88  SEGMENT-SLUT        VALUE 'GB'.                              
010300                                                                          
010400     03  GODK-STATUSKODER.                                                
010500      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
010600                                                                          
010700     EJECT                                                                
010800*----------------------------------IMS-CALL FUNKTIONER                    
010900*01              -COPY W0003                                              
011100     EJECT                                                                
011200 01  FILLER                     PIC X(16) VALUE 'DLI-IO-AREA'.            
011300 01  DLI-IO-AREA                PIC X(2000).                              
011400*01  WDL801      -COPY WDL801               -RED DLI-IO-AREA.             
011600     EJECT                                                                
011700*01  WDL811      -COPY WDL811               -RED DLI-IO-AREA.             
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400*01    -COPY W0008         -PRE WDL8-                                     
012600          05  FILLER      PIC   XX.                                       
012700     EJECT                                                                
012800 PROCEDURE DIVISION USING WDL8-PCB.                                       
012900     ENTRY 'CBLTDLI'  USING WDL8-PCB.                                     
012910                                                                          
013000     PERFORM A-INITIERA                                                   
013100     PERFORM IMS-GET-WDL8                                                 
013200     PERFORM UNTIL SEGMENT-SLUT                                           
013300         EVALUATE WDL8-SEG-NAME-FB                                        
013400            WHEN 'WDL801  '                                               
013500                  MOVE ART-IDARTNR TO IDARTNR-WS                          
013600                  IF  SW-ORDING-ARTIKEL   = JA                            
013700                      PERFORM S01-SKRIV-W23121                            
013800                      MOVE NEJ TO SW-ORDING-ARTIKEL                       
013900                  END-IF                                                  
014000                  PERFORM S02-INIT-W23121-POST                            
014100            WHEN 'WDL811  '                                               
014210                  PERFORM B-UPPDAT-PERIOD-VAERDE                          
014500         END-EVALUATE                                                     
014600         PERFORM IMS-GET-WDL8                                             
014700     END-PERFORM                                                          
014710                                                                          
014800     PERFORM Z-AVSLUTA                                                    
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-INITIERA SECTION.                                                      
015400                                                                          
015500     OPEN OUTPUT W23121                                                   
015700     MOVE 'W23120' TO POSTSUM-PROGNAMN                                    
015900     MOVE '212' TO U21ORD-IDPTYP                                          
016000                                                                          
016100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016370     MOVE D-AAR   TO WS-DAGENS-AA                                         
016390     MOVE D-VECKA TO WS-DAGENS-VV                                         
016391                                                                          
016392****** DAGENS-PERIOD                                                      
016393     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
016394     MOVE WS-DAGENS-AAVV TO DAT-I-TIDATUM                                 
016395     CALL WDATKONV USING DAT-KDDATFORM                                    
016396                         DAT-I-TIDATUM                                    
016397                         DAT-O-TIDATUM                                    
016398                         DAT-KDSVAR                                       
016399     MOVE DAT-TIAARP     TO WS-AARP                                       
016400     MOVE DAT-TISEKEL    TO W-PERIOD-TISEKEL                              
016401     MOVE WS-AA          TO W-PERIOD-AA                                   
016402     MOVE WS-RP          TO W-PERIOD-RP                                   
016410                                                                          
016500     MOVE NEJ TO SW-FORSTA-SEGMENT                                        
016600                 SW-ORDING-ARTIKEL                                        
016700                                                                          
016800     PERFORM AA-INIT-PERIODVECKO-TABELL                                   
016900     .                                                                    
017000     EJECT                                                                
017100 AA-INIT-PERIODVECKO-TABELL   SECTION.                                    
017200******************************************************************        
017300*                                                                *        
017400*    TIDSANGIVELSE FÖR 12 SISTA PERIODER BERÄKNAS                *        
017500*    FIXA TABELL FÖR SISTA 12 PERIODER                           *        
017510*                                                                *        
017600******************************************************************        
017700                                                                          
017800     MOVE 1 TO IX                                                         
017900     PERFORM UNTIL IX > MAX-ANT-PERIODER                                  
018000         MOVE W-PERIOD-AAAARP TO W-SISTA-PER-AAAARP(IX)                   
018100         SUBTRACT 1 FROM W-PERIOD-AARP                                    
018200         IF  W-PERIOD-RP = ZERO                                           
018300             MOVE MAX-ANT-PERIODER TO W-PERIOD-RP                         
018400             SUBTRACT 1 FROM W-PERIOD-AAAA                                
018500         END-IF                                                           
018600         ADD 1 TO IX                                                      
018700     END-PERFORM                                                          
018701                                                                          
018710     MOVE 1 TO IX                                                         
018720     PERFORM UNTIL IX > MAX-ANT-PERIODER                                  
018721        MOVE ZERO TO PER-NRAARP(IX)                                       
018726                     PER-VECKA-FOM(IX)                                    
018727                     PER-VECKA-TOM(IX)                                    
018734                     PER-KVVIPER(IX)                                      
018735        ADD 1 TO IX                                                       
018736     END-PERFORM                                                          
018737                                                                          
018738     MOVE +1 TO IX                                                        
018739     PERFORM UNTIL IX > MAX-ANT-PERIODER                                  
018741        MOVE 'AARP  '       TO DAT-KDDATFORM                              
018750        MOVE WS-AARP        TO DAT-I-TIDATUM                              
018760        CALL WDATKONV USING DAT-KDDATFORM                                 
018770                            DAT-I-TIDATUM                                 
018780                            DAT-O-TIDATUM                                 
018790                            DAT-KDSVAR                                    
018791        MOVE DAT-TIAA       TO WS-TIAA                                    
018792        MOVE DAT-TIVV       TO WS-TIVV                                    
018793        MOVE DAT-KVVIPER    TO WS-KVVIPER                                 
018794                                                                          
018795        MOVE WS-AARP          TO PER-NRAARP(IX)                           
018796        MOVE WS-FORSTA-VECKAN TO PER-VECKA-FOM(IX)                        
018797        MOVE WS-KVVIPER       TO PER-KVVIPER(IX)                          
018798        MOVE WS-FORSTA-VECKAN TO WS-SISTA-VECKAN                          
018799        ADD WS-KVVIPER        TO WS-SISTA-VECKAN                          
018800        ADD -1                TO WS-SISTA-VECKAN                          
018801        MOVE WS-SISTA-VECKAN  TO PER-VECKA-TOM(IX)                        
018802        ADD -1 TO WS-RP                                                   
018803        IF WS-RP = ZERO                                                   
018804           ADD -1                TO WS-AA                                 
018805           MOVE MAX-ANT-PERIODER TO WS-RP                                 
018806        END-IF                                                            
018807                                                                          
018808        ADD +1 TO IX                                                      
018809     END-PERFORM                                                          
018820     .                                                                    
018900     EJECT                                                                
019000 B-UPPDAT-PERIOD-VAERDE SECTION.                                          
019100                                                                          
019110     IF AAR-TIAAAA = W-SISTA-PER-AAAA (1) OR                              
019120        AAR-TIAAAA = W-SISTA-PER-AAAA (12)                                
019200        MOVE AAR-TIAAAA TO W-PERIOD-AAAA                                  
019210        MOVE 1 TO W-PERIOD-RP                                             
019211        PERFORM UNTIL W-PERIOD-RP > MAX-ANT-PERIODER                      
019220           MOVE 1 TO IX                                                   
019300           PERFORM UNTIL IX > MAX-ANT-PERIODER                            
019400               IF  W-PERIOD-AAAARP = W-SISTA-PER-AAAARP(IX)               
019500                   MOVE IX TO IX-UT                                       
019600                   MOVE JA TO SW-ORDING-ARTIKEL                           
019700                   PERFORM BA-UPPDATERING                                 
019800                   MOVE MAX-ANT-PERIODER TO IX                            
020000               END-IF                                                     
020100               ADD 1 TO IX                                                
020200           END-PERFORM                                                    
020201           ADD +1 TO W-PERIOD-RP                                          
020202        END-PERFORM                                                       
020210     END-IF                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 BA-UPPDATERING SECTION.                                                  
020600                                                                          
020700     MOVE PER-FOMVKA (IX) TO IX-W                                         
020800     PERFORM UNTIL IX-W > PER-TOMVKA (IX)                                 
020810                                                                          
020900          ADD  AAR-KVOI-PROG  (IX-W)                                      
021000                           TO U21ORD-KVOI-PROG     (IX-UT)                
021200          ADD  AAR-KVOI-DIV   (IX-W)                                      
021300                           TO U21ORD-KVOI-DIV      (IX-UT)                
021500          ADD  AAR-KVOI-SATS  (IX-W)                                      
021600                           TO U21ORD-KVOI-SATS     (IX-UT)                
021800          ADD  AAR-KVOI-SDC   (IX-W)                                      
021900                           TO U21ORD-KVOI-SDC      (IX-UT)                
022100          ADD  AAR-KVOI-NDC   (IX-W)                                      
022200                           TO U21ORD-KVOI-NDC      (IX-UT)                
022210          ADD  AAR-KVOI-REFILL(IX-W)                                      
022220                           TO U21ORD-KVOI-REFILL   (IX-UT)                
022300          ADD +1 TO IX-W                                                  
022400     END-PERFORM                                                          
022700     .                                                                    
022800     EJECT                                                                
022900 Z-AVSLUTA SECTION.                                                       
023000                                                                          
023100     IF  SW-ORDING-ARTIKEL = JA                                           
023200         PERFORM S01-SKRIV-W23121                                         
023300     END-IF                                                               
023500     CLOSE W23121                                                         
023600     .                                                                    
023700     EJECT                                                                
023800 S01-SKRIV-W23121 SECTION.                                                
023900                                                                          
024000     WRITE U21ORD-POST FROM U21ORD-AREA                                   
024100                                                                          
024200     MOVE 'W23121' TO POSTSUM-FDNAMN                                      
024300     MOVE 'W23120D1' TO POSTSUM-DDNAMN2                                   
024400     MOVE U21ORD-IDPTYP TO POSTSUM-TRANSTYP                               
024600     CALL POSTSUM USING POSTSUM-PARM                                      
024700     .                                                                    
024800     EJECT                                                                
024900 S02-INIT-W23121-POST SECTION.                                            
025000                                                                          
025100     MOVE IDARTNR-WS TO U21ORD-IDARTNR                                    
025200     MOVE 1 TO IX                                                         
025300     PERFORM UNTIL IX > MAX-ANT-PERIODER                                  
025400         MOVE ZERO TO U21ORD-KVOI-PROG          (IX)                      
025500                      U21ORD-KVOI-DIV           (IX)                      
025600                      U21ORD-KVOI-SATS          (IX)                      
025700                      U21ORD-KVOI-SDC           (IX)                      
025800                      U21ORD-KVOI-NDC           (IX)                      
025900                      U21ORD-KVOI-REFILL        (IX)                      
026000         ADD 1 TO IX                                                      
026100     END-PERFORM                                                          
026200     MOVE NEJ TO SW-ORDING-ARTIKEL                                        
026300     .                                                                    
026400 IMS-GET-WDL8 SECTION.                                                    
026500     SKIP3                                                                
026600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026700     CALL CBLTDLI USING GN WDL8-PCB DLI-IO-AREA                           
026800     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100 IMS-STATUSKONTROLL SECTION.                                              
027200     SKIP3                                                                
027300     SET STATUS-IX TO 1                                                   
027400     SEARCH GODK-STATUS AT END CALL FELLOG                                
027500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
027600       CONTINUE                                                           
027700     END-SEARCH                                                           
027800     .                                                                    
