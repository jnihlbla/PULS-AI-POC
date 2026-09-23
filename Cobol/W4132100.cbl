000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4132100.                                        
000400 AUTHOR.                 BERT ANDERSSON.                                  
000500     DATE-WRITTEN.       90/08/20.                                        
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*       PROGRAMMET STARTAS AV 4364- OCH 4368-BILDERNA.                    
001000*       MED PRC, IDDC, DATUM OCH KDCALL SOM INDATA (VIA DYNAMISKA         
001010*       PARAMETRAR I JCL'EN) LÄGGS EN SEX MÅNADER LÅNG                    
001020*       ARBETSTIDSTABELL UPP FÖR AKTUELL PRC PÅ DB(WL4437).               
001030*       FINNS EN MÅNAD REDAN SKER INGEN UPPDATERING.                      
001040*       PGM LADDAR DAT(WL4437;WDR101 & WDR130) MED ARBETSTIDER            
001050*       FÖR EN PRC SEX MÅNADER FRAM I TIDEN.                              
001060*                                                                         
001070 ENVIRONMENT DIVISION.                                                    
001080                                                                          
001090 INPUT-OUTPUT SECTION.                                                    
001100                                                                          
001200 FILE-CONTROL.                                                            
001300                                                                          
001400*     SYSIN FRÅN JCL                                                      
001500                                                                          
001600      SELECT INDATA          ASSIGN TO SYSIN.                             
001700                                                                          
001800 EJECT.                                                                   
001900 DATA DIVISION.                                                           
002000     SKIP2                                                                
002100 FILE SECTION.                                                            
002200     SKIP2                                                                
002300 FD INDATA                                                                
002400     LABEL RECORD STANDARD                                                
002500     RECORDING  F                                                         
002600     BLOCK CONTAINS 0.                                                    
002700     SKIP2                                                                
002800 01  INPOST                  PIC X(80).                                   
002900     SKIP2                                                                
003000 WORKING-STORAGE SECTION.                                                 
003001     SKIP2                                                                
003100     SKIP2                                                                
003101*    -- CHECKED BY WY2000                                                 
003110     SKIP3                                                                
003200 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4132100'.                
003300 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
003400 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
003700 77  FELTEXT                 PIC X(60)   VALUE SPACE.                     
003800 77  WS-IDPRC                PIC X(4).                                    
003900 77  WS-ISRT-IDPRC           PIC X(4).                                    
004000 77  WS-START-DATUM          PIC 9(6).                                    
004100 77  WS-KDCALL               PIC 9(1).                                    
004200 77  AKTUELL-IDPRC           PIC X(4).                                    
004300 77  JA                      PIC X       VALUE 'J'.                       
004400 77  NEJ                     PIC X       VALUE 'N'.                       
004500 77  EOF                     PIC X       VALUE 'N'.                       
004600 77  KVOT                    PIC S9(2)   VALUE +00.                       
004700 77  REST                    PIC S9(2)   VALUE +00.                       
004800 77  WS-START-MONTH          PIC S9(2)   VALUE +00.                       
004900 77  AKTUELL-IDDC            PIC X(2).                                    
005000*                                                                         
005100*      --- VALID IDDC CODES                                               
005200*                                                                         
005201*01    -COPY WWDC99                                                       
005202       EJECT                                                              
005210*    ---- INDEXFÄLT                                                       
005300 77  INDX                    PIC S9(3)   VALUE +000 COMP SYNC.            
005400 77  PRC-INDX                PIC S9(3)   VALUE +000 COMP SYNC.            
005500 77  PRC-MAXINDX             PIC S9(3)   VALUE +003 COMP SYNC.            
005600 77  DATE-INDX               PIC S9(3)   VALUE +000 COMP SYNC.            
005700 77  DATE-MAXINDX            PIC S9(3)   VALUE +000 COMP SYNC.            
005800 77  MAX-MONTH               PIC S9(3)   VALUE +008 COMP SYNC.            
005900                                                                          
006000*    ----  SWITCHES                                                       
006100 77  IDPRC-FINISH-SW         PIC X       VALUE 'N'.                       
006200     88  IDPRC-FINISH                    VALUE 'J'.                       
006300                                                                          
006400 77  UPDATE-SW               PIC X       VALUE 'J'.                       
006500     88  UPDATE-OK                       VALUE 'J'.                       
006600                                                                          
006700 01  END-YYMMDD              PIC 9(6).                                    
006800 01  FILLER REDEFINES END-YYMMDD.                                         
006900   03  END-YEAR              PIC 9(2).                                    
007000   03  END-MONTH             PIC 9(2).                                    
007100   03  END-DAY               PIC 9(2).                                    
007200                                                                          
007300 01  START-YYMMDD            PIC 9(6).                                    
007400 01  FILLER REDEFINES START-YYMMDD.                                       
007500   03  START-YEAR            PIC 9(2).                                    
007600   03  START-MONTH           PIC 9(2).                                    
007700   03  START-DAY             PIC 9(2).                                    
007800                                                                          
007900 01  WS-YYMMDD               PIC 9(6).                                    
008000 01  FILLER REDEFINES WS-YYMMDD.                                          
008100   03  WS-YEAR               PIC 9(2).                                    
008200   03  WS-MONTH              PIC 9(2).                                    
008300   03  WS-DAY                PIC 9(2).                                    
008410*                                                                         
008420 01  IDDC-USER.                                                           
008430     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
008440     03  IDDC-XX                 PIC X(2).                                
008450     03  FILLER                  PIC X(1) VALUE SPACE.                    
008460*                                                                         
008500 01  WS-MONTH-TABLE.                                                      
008600   03  MONTH-TABLE OCCURS 245 TIMES.                                      
008700     05  MONTH-DATE           PIC 9(6).                                   
008800     05  MONTH-KVKALTIM       PIC 9(3).                                   
008900                                                                          
009000 01  WS-MONTH-VALUE.                                                      
009100   03    FILLER                PIC 9(2)  VALUE 31.                        
009200   03    FILLER                PIC 9(2)  VALUE 28.                        
009300   03    FILLER                PIC 9(2)  VALUE 31.                        
009400   03    FILLER                PIC 9(2)  VALUE 30.                        
009500   03    FILLER                PIC 9(2)  VALUE 31.                        
009600   03    FILLER                PIC 9(2)  VALUE 30.                        
009700   03    FILLER                PIC 9(2)  VALUE 31.                        
009800   03    FILLER                PIC 9(2)  VALUE 31.                        
009900   03    FILLER                PIC 9(2)  VALUE 30.                        
010000   03    FILLER                PIC 9(2)  VALUE 31.                        
010100   03    FILLER                PIC 9(2)  VALUE 30.                        
010200   03    FILLER                PIC 9(2)  VALUE 31.                        
010300                                                                          
010400 01  WS-MONTH-RECORD  REDEFINES WS-MONTH-VALUE.                           
010500   03    WS-MAANAD-TAB         PIC 9(2)  OCCURS 12.                       
010600                                                                          
010700 01  WS-IDPRC-RECORD.                                                     
010800   03  PRC-TABLE OCCURS 3 TIMES.                                          
010900     05  PRC-KVKALTIM          PIC 9(5).                                  
011000     05  PRC-STAPAC            PIC 9(5).                                  
011100     05  PRC-STOPAC            PIC 9(5).                                  
011200     05  PRC-STAADM            PIC 9(5).                                  
011300     05  PRC-STOADM            PIC 9(5).                                  
011400     05  PRC-STALAST           PIC 9(5).                                  
011500     05  PRC-STOLAST           PIC 9(5).                                  
011600   SKIP2                                                                  
011610 77  ABENDMED1               PIC X(60)   VALUE                            
011620     'FEL FRÅN SUBPGM.W41321,DEFAULT IDPRC 999X SAKNAS PÅ WLXXKC'.        
011630   SKIP2                                                                  
011700*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
012000   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
012100   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
012200   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
012300   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
012310   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
012400                                                                          
012500 01  FILLER                  PIC X(10)   VALUE 'INAREA'.                  
012600 01  INAREA.                                                              
012700   03  IN-IDPRC              PIC X(4).                                    
012800   03  FILLER                PIC X.                                       
012900   03  IN-IDDC               PIC X(2).                                    
013000   03  FILLER                PIC X.                                       
013100   03  IN-START-DATUM        PIC X(6).                                    
013200   03  FILLER                PIC X.                                       
013300   03  IN-KDCALL             PIC X(1).                                    
013400   03  FILLER                PIC X(64).                                   
013500     SKIP2                                                                
013600                                                                          
013700 01  RKOD-ABEND              PIC S9(4)   VALUE +33  COMP SYNC.            
013800     SKIP2                                                                
013900*01  -COPY WDATAREA                                                       
013910     SKIP2                                                                
014100*01  -COPY WORKAREA                                                       
014210     SKIP2                                                                
014220*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014230 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
014240*01 -COPY WMSGINIT                                                        
014250*                                                                         
014300*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
014400                                                                          
014500 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
014600     SKIP2                                                                
014700*    ---- STATUSKOD FRÅN IMS                                              
014800                                                                          
014900 01  STATUS-WS               PIC XX.                                      
015000     88  SEGMENT-FINNS                    VALUE '  '.                     
015100     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
015200     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
015300     88  END-OF-DATA                      VALUE 'GB'.                     
015400     SKIP2                                                                
015500 01  GODK-STATUSKODER.                                                    
015600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
015700     SKIP2                                                                
015800 01  SSA1                    PIC X(96).                                   
015900 01  SSA2                    PIC X(96).                                   
016000     SKIP2                                                                
016100*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
016200                                                                          
016300 01  NYCKLAR-TILL-DLI.                                                    
016400                                                                          
016500   03  W-WDGXKEY-4435-X.                                                  
016600     05  W-IDHTYP            PIC X(4)    VALUE '4435'.                    
016700     05  W-IDDC-4435         PIC X(2)    VALUE SPACE.                     
016800     05  W-FILLER-4435       PIC X(24)   VALUE LOW-VALUE.                 
016900   03  W-WDGXKEY-4436-X.                                                  
017000     05  W-IDPRC-4436        PIC X(4).                                    
017100     05  W-KVKALTIM          PIC 9(2).                                    
017200     05  W-FILLER-4436       PIC X(4)    VALUE LOW-VALUE.                 
017300   03  W-WDGXKEY-4437-X.                                                  
017400     05  W-IDHTYP-4437       PIC X(4)    VALUE '4437'.                    
017500     05  W-IDDC-4437         PIC X(2)    VALUE SPACE.                     
017600     05  W-IDPRC-4437        PIC X(4)    VALUE '0000'.                    
017700     05  W-FILLER-4437       PIC X(20)   VALUE LOW-VALUE.                 
017800   03  W-WDGXKEY-4438-X.                                                  
017900     05  W-DADATUM           PIC 9(8)    VALUE ZERO.                      
018100     EJECT                                                                
018200*01  -COPY W0003                                                          
018300     EJECT                                                                
018400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA1'.              
018500     SKIP2                                                                
018600 01  DLI-IO-AREA.                                                         
018700   03  IO-AREA-KC            PIC X(32)    VALUE SPACE.                    
018800     SKIP2                                                                
018900*03  WLXXKC01 -COPY WDGX4435        -RED IO-AREA-KC.                      
019000     EJECT                                                                
019100*03  WLXXKC11 -COPY WDGX4436        -RED IO-AREA-KC.                      
019200     EJECT                                                                
019300   03  IO-AREA-KD            PIC X(32)    VALUE SPACE.                    
019400     SKIP2                                                                
019500*03  WL443701 -COPY WDGX4437        -RED IO-AREA-KD.                      
019600     EJECT                                                                
019700*03  WL443711 -COPY WDGX4438        -RED IO-AREA-KD.                      
019800     EJECT                                                                
019810 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
019900 01  DLI-IO-AREA2.                                                        
020000   03  IO-AREA-KD2           PIC X(32)    VALUE SPACE.                    
020100     SKIP2                                                                
020200*03  WL443701 -COPY WDGX4437  -PRE IOAREA2-   -RED IO-AREA-KD2.           
020300     EJECT                                                                
020400*03  WL443711 -COPY WDGX4438  -PRE IOAREA2-   -RED IO-AREA-KD2.           
020500     EJECT                                                                
020600 LINKAGE   SECTION.                                                       
020700     SKIP2                                                                
020800*01  -COPY W0008      -PRE   MSG-.                                        
020900       05  FILLER                PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008      -PRE  USEA-.                                        
021200       05  FILLER                PIC X.                                   
021300     EJECT                                                                
021310*01  -COPY W0008      -PRE  XXKC-.                                        
021320       05  FILLER                PIC X.                                   
021330     EJECT                                                                
021400*01  -COPY W0008      -PRE  4437-.                                        
021500       05  FILLER                PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING   MSG-PCB USEA-PCB XXKC-PCB 4437-PCB.          
021800     ENTRY 'DLITCBL' USING   MSG-PCB USEA-PCB XXKC-PCB 4437-PCB.          
021900 STYR SECTION.                                                            
022000                                                                          
022100     PERFORM A-INIT                                                       
022200                                                                          
022300     IF WS-KDCALL              = 1                                        
022400         PERFORM B-AENDRA-TILLAEGG                                        
022500     ELSE                                                                 
022600         PERFORM C-BORTTAG                                                
022700     END-IF                                                               
022800                                                                          
022900     MOVE ZERO TO RETURN-CODE                                             
023000     GOBACK                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 A-INIT SECTION.                                                          
023400                                                                          
023500     OPEN INPUT INDATA                                                    
023600     READ INDATA NEXT RECORD INTO INAREA                                  
023700       AT END MOVE JA               TO EOF                                
023800     END-READ                                                             
023900     CLOSE INDATA                                                         
024000                                                                          
024100     UNSTRING INAREA DELIMITED BY SPACE INTO IN-IDPRC                     
024200                                             IN-IDDC                      
024300                                             IN-START-DATUM               
024400                                             IN-KDCALL                    
024500     MOVE IN-IDPRC                  TO WS-IDPRC                           
024600     MOVE IN-IDDC                   TO WS-IDDC                            
024700     MOVE IN-START-DATUM            TO WS-START-DATUM                     
024800     MOVE IN-KDCALL                 TO WS-KDCALL                          
024900     DISPLAY 'IN-IDPRC******=' IN-IDPRC                                   
025000     DISPLAY 'IN-IDDC*******=' IN-IDDC                                    
025100     DISPLAY 'IN-START-DATUM=' IN-START-DATUM                             
025200     DISPLAY 'IN-KDCALL*****=' IN-KDCALL                                  
025300                                                                          
025400                                                                          
025500     MOVE '4437'                      TO W-IDHTYP-4437                    
025600     MOVE WS-IDDC                     TO W-IDDC-4437                      
025700     MOVE WS-IDPRC                    TO W-IDPRC-4437                     
025800     MOVE LOW-VALUE                   TO W-FILLER-4435                    
025900                                         W-FILLER-4436                    
026000                                         W-FILLER-4437                    
026200     IF WS-START-DATUM = ZERO                                             
026300       ACCEPT WS-START-DATUM        FROM DATE                             
026310                                                                          
026320       IF SDC OR NDC OR LDC-CN                                            
026330         MOVE ALL '+'                 TO MSGI-WMSGINIT                    
026340         MOVE '011'                   TO MSGI-KDCALL                      
026350         MOVE WS-IDDC                 TO IDDC-XX                          
026360         MOVE IDDC-USER               TO MSGI-IDUSER                      
026371         MOVE 'W413'                  TO MSGI-IDTRANS                     
026372         MOVE WS-START-DATUM          TO MSGI-TILOKDAT                    
026380                                                                          
026390         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
026392         MOVE MSGI-TILOKDAT           TO WS-START-DATUM                   
026393         MOVE WS-START-DATUM          TO START-YYMMDD                     
026394                                         END-YYMMDD                       
026395       ELSE                                                               
026396         MOVE WS-START-DATUM          TO START-YYMMDD                     
026397                                         END-YYMMDD                       
026398       END-IF                                                             
026399                                                                          
026600       PERFORM AA-COMPUTE-START-OF-UPDATE                                 
026700     ELSE                                                                 
026800       MOVE WS-START-DATUM            TO START-YYMMDD                     
026900                                         END-YYMMDD                       
027000     END-IF                                                               
027100                                                                          
027200                                                                          
027300     PERFORM AB-COMPUTE-END-OF-UPDATE                                     
027400                                                                          
027500     PERFORM AC-CONTROL-IF-LEAPYEAR                                       
027600                                                                          
027700     MOVE START-YYMMDD              TO W-DADATUM                          
027710     IF START-YYMMDD < 500000                                             
027720       MOVE 20                      TO W-DADATUM (1:2)                    
027730     ELSE                                                                 
027740       MOVE 19                      TO W-DADATUM (1:2)                    
027750     END-IF                                                               
027800                                                                          
027900***START-YYMMDD OCH END-YYMMDD ÄR NU START- RESPEKTIVE SLUT-DATUM         
028000***FÖR UPPDATERING.                                                       
028100     .                                                                    
028200     EJECT                                                                
028300 AA-COMPUTE-START-OF-UPDATE SECTION.                                      
028400                                                                          
028500***BERÄKNING AV START-DATUM FÖR UPPDATERING.                              
028600     COMPUTE WS-START-MONTH = START-MONTH - 3                             
028700                                                                          
028800     EVALUATE WS-START-MONTH                                              
028900       WHEN ZERO                                                          
029000         MOVE +12                   TO START-MONTH                        
029010         IF START-YEAR = 00                                               
029020           MOVE 99                  TO START-YEAR                         
029030         ELSE                                                             
029100           COMPUTE START-YEAR = START-YEAR - 1                            
029110         END-IF                                                           
029200       WHEN -1                                                            
029300         MOVE +11                   TO START-MONTH                        
029410         IF START-YEAR = 00                                               
029420           MOVE 99                  TO START-YEAR                         
029430         ELSE                                                             
029440           COMPUTE START-YEAR = START-YEAR - 1                            
029450         END-IF                                                           
029500       WHEN -2                                                            
029600         MOVE +10                   TO START-MONTH                        
029710         IF START-YEAR = 00                                               
029720           MOVE 99                  TO START-YEAR                         
029730         ELSE                                                             
029740           COMPUTE START-YEAR = START-YEAR - 1                            
029750         END-IF                                                           
029800     END-EVALUATE                                                         
029900     MOVE 01                        TO START-DAY                          
030000     .                                                                    
030100     EJECT                                                                
030200 AB-COMPUTE-END-OF-UPDATE SECTION.                                        
030300                                                                          
030400***BERÄKNING AV SLUT-DATUM FÖR UPPDATERING.                               
030500     IF END-MONTH > +8                                                    
030600       COMPUTE END-YEAR = END-YEAR + 1                                    
030700     END-IF                                                               
030800                                                                          
030900     COMPUTE END-MONTH = END-MONTH + 4                                    
031000     EVALUATE END-MONTH                                                   
031100       WHEN +13                                                           
031200         MOVE +01                   TO END-MONTH                          
031300       WHEN +14                                                           
031400         MOVE +02                   TO END-MONTH                          
031500       WHEN +15                                                           
031600         MOVE +03                   TO END-MONTH                          
031700       WHEN +16                                                           
031800         MOVE +04                   TO END-MONTH                          
031900       WHEN +17                                                           
032000         MOVE +05                   TO END-MONTH                          
032100     END-EVALUATE                                                         
032200                                                                          
032300     MOVE END-MONTH                 TO INDX                               
032400     MOVE WS-MAANAD-TAB(INDX)       TO END-DAY                            
032500     .                                                                    
032600     EJECT                                                                
032700 AC-CONTROL-IF-LEAPYEAR SECTION.                                          
032800                                                                          
032900***KONTROLL OM ANGIVET ÅR ÄR SKOTTÅR.                                     
033000     IF START-YEAR = 00 AND START-MONTH = 02                              
033100       MOVE +29                     TO END-DAY                            
033200     ELSE                                                                 
033300       DIVIDE START-YEAR BY 4 GIVING KVOT REMAINDER REST                  
033400       IF REST = ZERO                                                     
033500         MOVE +29                   TO END-DAY                            
033600       ELSE                                                               
033700         CONTINUE                                                         
033800       END-IF                                                             
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300 B-AENDRA-TILLAEGG      SECTION.                                          
034400                                                                          
034500     PERFORM BA-CREATE-DATE-TABEL                                         
034600                                                                          
034700     PERFORM BB-CREATE-TIME-TABEL                                         
034800                                                                          
034900     PERFORM BC-UPDATE-WL443701                                           
035000                                                                          
035100     PERFORM BD-UPDATE-WL443711                                           
035200     .                                                                    
035300     EJECT                                                                
035400                                                                          
035500 BA-CREATE-DATE-TABEL SECTION.                                            
035600                                                                          
035700***TABELL SKAPAS, BESTÅENDE AV KVKALTIM OCH TIDATUM.                      
035800     MOVE  +1                       TO INDX DATE-INDX                     
035900     MOVE START-YYMMDD              TO WORK-TIAAMMDD-FOM                  
035910                                       WORK-TIAAMMDD-TOM                  
036000                                       WS-YYMMDD                          
036100                                                                          
036200     PERFORM UNTIL INDX > MAX-MONTH                                       
036800         MOVE 001                    TO WORK-KDCALL                       
036810         MOVE WS-IDDC                TO WORK-IDDC                         
036900         CALL WORKDAY  USING            WORK-KDCALL                       
037000                                        WORK-DATE-AREA                    
037100                                        WORK-KDSVAR                       
037200                                                                          
037300       IF WORK-KDSVAR-OK                                                  
037310         IF WORK-KVWORKD = 1                                              
037320           MOVE 8                 TO MONTH-KVKALTIM(DATE-INDX)            
037330         ELSE                                                             
037400           MOVE 0                 TO MONTH-KVKALTIM(DATE-INDX)            
037410         END-IF                                                           
037500         MOVE WS-YYMMDD           TO MONTH-DATE(DATE-INDX)                
037600         ADD  +1                  TO WS-DAY                               
037700         PERFORM BAA-KONTROL-AV-DATUM-OVER-27                             
037800         ADD  +1                  TO DATE-INDX                            
037900         MOVE WS-YYMMDD           TO WORK-TIAAMMDD-FOM                    
037910                                     WORK-TIAAMMDD-TOM                    
038000       ELSE                                                               
038100         ADD  +1                  TO INDX WS-MONTH                        
038200         MOVE +01                 TO WS-DAY                               
038300         MOVE WS-YYMMDD           TO WORK-TIAAMMDD-FOM                    
038310                                     WORK-TIAAMMDD-TOM                    
038400       END-IF                                                             
038500     END-PERFORM                                                          
038600                                                                          
038700     COMPUTE DATE-MAXINDX = DATE-INDX - 1                                 
038800     .                                                                    
038900     EJECT                                                                
039000 BAA-KONTROL-AV-DATUM-OVER-27 SECTION.                                    
039100                                                                          
039111     IF WS-DAY > 27                                                       
039300       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
039400       MOVE WS-YYMMDD         TO DAT-I-TIDATUM                            
039500       CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                  
039600                           DAT-O-TIDATUM, DAT-KDSVAR                      
039700                                                                          
039800       IF DAT-KDSVAR-OK                                                   
039900         CONTINUE                                                         
040000       ELSE                                                               
040100         MOVE +01             TO WS-DAY                                   
040200         ADD  +1              TO INDX WS-MONTH                            
040300***KONTROL-AV-MANADNR-OVER-12                                             
040400         IF WS-MONTH > +12                                                
040500           MOVE +01           TO WS-MONTH                                 
040600           ADD  +1            TO WS-YEAR                                  
040700         END-IF                                                           
040800       END-IF                                                             
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200 BB-CREATE-TIME-TABEL SECTION.                                            
041300                                                                          
041400     MOVE WS-IDDC                   TO W-IDDC-4435                        
041500     PERFORM IMS-GU-WLXXKC01                                              
041600     MOVE +00                       TO W-KVKALTIM                         
041700     MOVE WS-IDPRC                  TO AKTUELL-IDPRC                      
041800                                       W-IDPRC-4436                       
041900                                                                          
042000     PERFORM IMS-GNP-WLXXKC11                                             
042100     IF SEGMENT-FINNS                                                     
042200       MOVE WS-IDPRC              TO W-IDPRC-4437 WS-ISRT-IDPRC           
042300     ELSE                                                                 
042400       MOVE WS-IDPRC              TO WS-ISRT-IDPRC                        
042500       MOVE +9999                 TO WS-IDPRC W-IDPRC-4436                
042600                                     W-IDPRC-4437                         
042700                                     AKTUELL-IDPRC                        
042800                                                                          
042900       PERFORM IMS-GNP2-WLXXKC11                                          
043000       IF SEGMENT-SAKNAS                                                  
043100         MOVE ABENDMED1           TO FELTEXT                              
043200         CALL ABEND USING RKOD-ABEND                                      
043300       END-IF                                                             
043400     END-IF                                                               
043500                                                                          
043600**TABELL MED ARB.TID FÖR OLIKA TYPER AV DAGAR(=KVKALTIM).                 
043700     MOVE +1                        TO PRC-INDX                           
043800     PERFORM UNTIL IDPRC-FINISH                                           
043900       PERFORM BBA-MOVE-4436IOAREA-TO-PRCTAB                              
044000       PERFORM IMS-GNP2-WLXXKC11                                          
044100       MOVE 4436-IDPRC              TO WS-IDPRC                           
044200       ADD  +1                      TO PRC-INDX                           
044300                                                                          
044400       IF WS-IDPRC = AKTUELL-IDPRC                                        
044500        CONTINUE                                                          
044600       ELSE                                                               
044700         MOVE JA                    TO IDPRC-FINISH-SW                    
044800       END-IF                                                             
044900                                                                          
045000       IF SEGMENT-SAKNAS                                                  
045100         MOVE JA                    TO IDPRC-FINISH-SW                    
045200       END-IF                                                             
045300     END-PERFORM                                                          
045400     .                                                                    
045500     EJECT                                                                
045600 BBA-MOVE-4436IOAREA-TO-PRCTAB SECTION.                                   
045700                                                                          
045800     MOVE 4436-KVKALTIM               TO PRC-KVKALTIM(PRC-INDX)           
045900     MOVE 4436-TISTAMIN-PAC           TO PRC-STAPAC(PRC-INDX)             
046000     MOVE 4436-TISTOMIN-PAC           TO PRC-STOPAC(PRC-INDX)             
046100     MOVE 4436-TISTAMIN-ADM           TO PRC-STAADM(PRC-INDX)             
046200     MOVE 4436-TISTOMIN-ADM           TO PRC-STOADM(PRC-INDX)             
046300     MOVE 4436-TISTAMIN-LAST          TO PRC-STALAST(PRC-INDX)            
046400     MOVE 4436-TISTOMIN-LAST          TO PRC-STOLAST(PRC-INDX)            
046500     .                                                                    
046600     EJECT                                                                
046700 BC-UPDATE-WL443701 SECTION.                                              
046800                                                                          
046900     MOVE '4437'                      TO W-IDHTYP-4437                    
047000                                         4437-IDHTYP                      
047100     MOVE WS-IDDC                     TO W-IDDC-4437                      
047200                                         4437-IDDC                        
047300     MOVE WS-ISRT-IDPRC               TO W-IDPRC-4437                     
047400                                         4437-IDPRC                       
047500     MOVE LOW-VALUE                   TO 4437-LOW-VALUE                   
047600     PERFORM IMS-ISRT-WL443701                                            
047700     .                                                                    
047800     EJECT                                                                
047900 BD-UPDATE-WL443711 SECTION.                                              
048000                                                                          
048100     MOVE +1                          TO DATE-INDX PRC-INDX               
048111     PERFORM UNTIL DATE-INDX > DATE-MAXINDX                               
048300       PERFORM BDA-READ-DATE-TABEL                                        
048400       PERFORM BDB-READ-TIME-TABEL                                        
048500       PERFORM BDC-UPDATE-WL443711                                        
048600       ADD +1                         TO DATE-INDX                        
048700     END-PERFORM                                                          
048800     .                                                                    
048900     EJECT                                                                
049000 BDA-READ-DATE-TABEL SECTION.                                             
049100                                                                          
049200     IF MONTH-KVKALTIM(DATE-INDX) = +00                                   
049300       MOVE +1                        TO PRC-INDX                         
049400     END-IF                                                               
049500                                                                          
050000     IF MONTH-KVKALTIM(DATE-INDX) = +08                                   
050100       MOVE +3                        TO PRC-INDX                         
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 BDB-READ-TIME-TABEL SECTION.                                             
050600                                                                          
050700     MOVE MONTH-DATE(DATE-INDX)       TO 4438-DADATUM                     
050701                                         W-DADATUM                        
050710     IF MONTH-DATE(DATE-INDX) NOT = ZERO                                  
050720       IF MONTH-DATE(DATE-INDX) < 500000                                  
050730         MOVE 20                      TO 4438-DADATUM (1:2)               
050740                                         W-DADATUM (1:2)                  
050750       ELSE                                                               
050760         IF MONTH-DATE(DATE-INDX) < 999999                                
050770           MOVE 19                    TO 4438-DADATUM (1:2)               
050780                                         W-DADATUM (1:2)                  
050790         ELSE                                                             
050791           MOVE 99999999              TO 4438-DADATUM                     
050792                                         W-DADATUM                        
050793         END-IF                                                           
050794       END-IF                                                             
050795     END-IF                                                               
050900     MOVE PRC-STAPAC(PRC-INDX)        TO 4438-TISTAMIN-PAC                
051000     MOVE PRC-STOPAC(PRC-INDX)        TO 4438-TISTOMIN-PAC                
051100     MOVE PRC-STAADM(PRC-INDX)        TO 4438-TISTAMIN-ADM                
051200     MOVE PRC-STOADM(PRC-INDX)        TO 4438-TISTOMIN-ADM                
051300     MOVE PRC-STALAST(PRC-INDX)       TO 4438-TISTAMIN-LAST               
051400     MOVE PRC-STOLAST(PRC-INDX)       TO 4438-TISTOMIN-LAST               
051600     .                                                                    
051700     EJECT                                                                
051800 BDC-UPDATE-WL443711 SECTION.                                             
051900                                                                          
052000     IF DATE-INDX = 1                                                     
052100       DISPLAY 'START-4438-DADATUM=' 4438-DADATUM                         
052200     END-IF                                                               
052300     IF DATE-INDX = DATE-MAXINDX                                          
052400       DISPLAY 'END-4438-DADATUM=' 4438-DADATUM                           
052500     END-IF                                                               
052600     PERFORM IMS-ISRT-WL443711                                            
052700     IF SEGMENT-FINNS-REDAN                                               
052800       PERFORM BDCA-MOVE-TO-IOAREA2                                       
052900       PERFORM IMS-GHU-WL443711                                           
053000       PERFORM IMS-REPL-WL443711                                          
053100     END-IF                                                               
053200     .                                                                    
053300     EJECT                                                                
053400 BDCA-MOVE-TO-IOAREA2 SECTION.                                            
053500                                                                          
053600     MOVE MONTH-DATE(DATE-INDX)    TO IOAREA2-4438-DADATUM                
053610     IF MONTH-DATE(DATE-INDX) NOT = ZERO                                  
053620       IF MONTH-DATE(DATE-INDX) < 500000                                  
053630         MOVE 20                   TO IOAREA2-4438-DADATUM (1:2)          
053640       ELSE                                                               
053650         IF MONTH-DATE(DATE-INDX) < 999999                                
053660           MOVE 19                 TO IOAREA2-4438-DADATUM (1:2)          
053670         ELSE                                                             
053680           MOVE 99999999           TO IOAREA2-4438-DADATUM                
053690         END-IF                                                           
053691       END-IF                                                             
053692     END-IF                                                               
053800     MOVE PRC-STAPAC(PRC-INDX)     TO IOAREA2-4438-TISTAMIN-PAC           
053900     MOVE PRC-STOPAC(PRC-INDX)     TO IOAREA2-4438-TISTOMIN-PAC           
054000     MOVE PRC-STAADM(PRC-INDX)     TO IOAREA2-4438-TISTAMIN-ADM           
054100     MOVE PRC-STOADM(PRC-INDX)     TO IOAREA2-4438-TISTOMIN-ADM           
054200     MOVE PRC-STALAST(PRC-INDX)    TO IOAREA2-4438-TISTAMIN-LAST          
054300     MOVE PRC-STOLAST(PRC-INDX)    TO IOAREA2-4438-TISTOMIN-LAST          
054500     .                                                                    
054600     EJECT                                                                
054700 C-BORTTAG            SECTION.                                            
054800                                                                          
054900     PERFORM IMS-GHU-WL443701                                             
055000     IF SEGMENT-FINNS                                                     
055100       PERFORM IMS-DLET-WL443701                                          
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500****----- IMS SEKTIONER ----                                              
055600                                                                          
055700 IMS-GHU-WL443701 SECTION.                                                
055800                                                                          
055900     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
056000            DELIMITED BY SIZE INTO SSA1                                   
056100     MOVE '  GBGE'              TO GODK-STATUSKODER                       
056200     CALL CBLTDLI USING GHU 4437-PCB IO-AREA-KD SSA1                      
056300     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     SKIP2                                                                
056700 IMS-DLET-WL443701 SECTION.                                               
056800                                                                          
056900     MOVE '  '                  TO GODK-STATUSKODER                       
057000     CALL CBLTDLI USING DLET 4437-PCB IO-AREA-KD                          
057100     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     SKIP2                                                                
057500 IMS-GHU-WL443711 SECTION.                                                
057600                                                                          
057700     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
057800            DELIMITED BY SIZE INTO SSA1                                   
057900     STRING 'WL443711(DADATUM  =' W-WDGXKEY-4438-X ')'                    
058000            DELIMITED BY SIZE INTO SSA2                                   
058100     MOVE '  '                  TO GODK-STATUSKODER                       
058200     CALL CBLTDLI USING GHU 4437-PCB IO-AREA-KD SSA1 SSA2                 
058300     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
058400     PERFORM IMS-STATUSKONTROLL                                           
058500     .                                                                    
058600     SKIP2                                                                
058700 IMS-GU-WLXXKC01 SECTION.                                                 
058800                                                                          
058900     STRING 'WLXXKC01(WDGXKEY  =' W-WDGXKEY-4435-X ')'                    
059000            DELIMITED BY SIZE INTO SSA1                                   
059100     MOVE '  '                  TO GODK-STATUSKODER                       
059200     CALL CBLTDLI USING GU  XXKC-PCB IO-AREA-KC SSA1                      
059300     MOVE XXKC-STATUS-CODE      TO STATUS-WS                              
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     SKIP2                                                                
059700 IMS-GNP-WLXXKC11 SECTION.                                                
059800                                                                          
059900     STRING 'WLXXKC11(WDGXKEY  =' W-WDGXKEY-4436-X ')'                    
060000            DELIMITED BY SIZE INTO SSA1                                   
060100     MOVE '  GBGE'              TO GODK-STATUSKODER                       
060200     CALL CBLTDLI USING GNP XXKC-PCB IO-AREA-KC SSA1                      
060300     MOVE XXKC-STATUS-CODE      TO STATUS-WS                              
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     SKIP2                                                                
060700 IMS-GNP2-WLXXKC11 SECTION.                                               
060800                                                                          
060900     STRING 'WLXXKC11(WDGXKEY =>' W-WDGXKEY-4436-X ')'                    
061000            DELIMITED BY SIZE INTO SSA1                                   
061100     MOVE '  GBGE'              TO GODK-STATUSKODER                       
061200     CALL CBLTDLI USING GNP XXKC-PCB IO-AREA-KC SSA1                      
061300     MOVE XXKC-STATUS-CODE      TO STATUS-WS                              
061400     PERFORM IMS-STATUSKONTROLL                                           
061500     .                                                                    
061600     SKIP2                                                                
061700 IMS-ISRT-WL443701 SECTION.                                               
061800                                                                          
061900     MOVE 'WL443701'            TO SSA1                                   
062000     MOVE '  II'                TO GODK-STATUSKODER                       
062100     CALL CBLTDLI USING ISRT  4437-PCB IO-AREA-KD SSA1                    
062200     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500     SKIP2                                                                
062600 IMS-ISRT-WL443711 SECTION.                                               
062700                                                                          
062800     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
062900                    '&IDPRC    =W-IDPRC-4437)'                            
063000            DELIMITED BY SIZE INTO SSA1                                   
063100     MOVE 'WL443711'            TO SSA2                                   
063200     MOVE '  II'                TO GODK-STATUSKODER                       
063300     CALL CBLTDLI USING ISRT  4437-PCB IO-AREA-KD SSA1 SSA2               
063400     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
063500     PERFORM IMS-STATUSKONTROLL                                           
063600     .                                                                    
063700 IMS-REPL-WL443711 SECTION.                                               
063800                                                                          
063900     MOVE '  '                  TO GODK-STATUSKODER                       
064000     CALL CBLTDLI USING REPL  4437-PCB DLI-IO-AREA2                       
064100     MOVE 4437-STATUS-CODE      TO STATUS-WS                              
064200     PERFORM IMS-STATUSKONTROLL                                           
064300     .                                                                    
064400 IMS-STATUSKONTROLL SECTION.                                              
064500                                                                          
064600     SET STATUS-IX TO 1                                                   
064700     SEARCH GODK-STATUS                                                   
064800       AT END                                                             
064900         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
065000           DELIMITED BY SIZE INTO FELTEXT                                 
065100         CALL FELLOG                                                      
065200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
065300     END-SEARCH                                                           
065400     .                                                                    
065410     EJECT                                                                
