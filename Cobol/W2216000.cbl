000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W2216000.                                        
000400 AUTHOR.                 ANN JORDEBO.                                     
000500 DATE-WRITTEN.           MARS 1990.                                       
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            INNAN DETTA PROGRAM LIGGER DET EN STD.SORT DÄR               
001100*            MAN SORTERAR W22181 PÅ LEVNR, ARTNR.                         
001200*            PROGRAMMET LÄSER EN FIL (W22181) MED ARTIKLAR SOM            
001300*            SKALL SÄNDAS TILL LEVERANTÖREN VIA ODETTE.                   
001400*            REGISTER (W22161) LÄSES OCH UPPDATERAS. WLXXBK               
001500*            LÄSES OCH UPPDATERAS.                                        
001600*            INFORMATION HÄMTAS ÄVEN FRÅN WDK6 (KDGK, IDBEST              
001700*            OCH IDKAT)                                                   
001800*                                                                         
001900*    SUBPROGRAM:                                                          
002000*                                                                         
002100*                                                                         
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*- - - - - - - - - - - - - - INFILER:                                     
003000                                                                          
003100     SELECT  W22181                   ASSIGN TO W22160D1.                 
003200     SELECT  W22161-IN                ASSIGN TO W22160D2.                 
003300     SKIP2                                                                
003400*- - - - - - - - - - - - - - UTFILER:                                     
003500                                                                          
003600     SELECT  W22161-UT                ASSIGN TO W22160D3.                 
003700     SELECT  W22160                   ASSIGN TO W22160D4.                 
003710     SELECT  W22160-TMS               ASSIGN TO W22160D5.                 
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300*************** INFIL                                                     
004400 FD  W22181                                                               
004500     LABEL RECORD STANDARD                                                
004600     RECORDING      F                                                     
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004900*01  -COPY W22181                           -L.                           
005000     SKIP3                                                                
005100 FD  W22161-IN                                                            
005200     LABEL RECORD STANDARD                                                
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*01  -COPY W22161                           -L.                           
005700     SKIP3                                                                
005800 FD  W22161-UT                                                            
005900     LABEL RECORD STANDARD                                                
006000     RECORDING   F                                                        
006100     BLOCK CONTAINS 0.                                                    
006200     SKIP2                                                                
006300 01  REG-W22161.                                                          
006400*    03     -COPY W22161                    -L.                           
006500     SKIP3                                                                
006600*************** UTFIL                                                     
006700 FD  W22160                                                               
006800     LABEL RECORD STANDARD                                                
006900     RECORDING   F                                                        
007000     BLOCK CONTAINS 0.                                                    
007100     SKIP2                                                                
007200 01  W22160-POST.                                                         
007300*    03     -COPY W22160                    -L.                           
007400     EJECT                                                                
007410 FD  W22160-TMS                                                           
007420     LABEL RECORD STANDARD                                                
007430     RECORDING   F                                                        
007440     BLOCK CONTAINS 0.                                                    
007450     SKIP2                                                                
007460 01  TMS-W22160-POST.                                                     
007470*    03 TMS -COPY W22160                    -L.                           
007480     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600     SKIP2                                                                
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
008000 77   PROGRAM-NAMN           VALUE 'W2216000'                             
008100                                 PIC X(8).                                
008200     SKIP2                                                                
008300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
008400                                                                          
008500 77  JA                          PIC X       VALUE 'J'.                   
008600 77  NEJ                         PIC X       VALUE 'N'.                   
008700                                                                          
008800 77  NYUPPLAGG                   PIC X       VALUE 'N'.                   
008900     SKIP2                                                                
009000*- - - - - - - - - - - - - -  END-OF-FILE SWITCHAR                        
009100 77  INFIL-EOF                   PIC X       VALUE 'N'.                   
009200 77  REG-EOF                     PIC X       VALUE 'N'.                   
009300 77  SW-WDK611                   PIC X       VALUE 'N'.                   
009400     SKIP2                                                                
009500*- - - - - - - - - - - - - -  INDEX                                       
009600                                                                          
009700     SKIP2                                                                
009800*- - - - - - - - - - - - - -  ARBETSFÄLT                                  
009900 01  ARBETSFAELT.                                                         
010000   03  WS-IDLPLAN.                                                        
010100     05  WS-IDAAVV               PIC S9(4)   VALUE ZERO.                  
010200     05  WS-LLL                  PIC S9(3)   VALUE ZERO.                  
010300   03  WS-IDBEST                 PIC S9(13)  VALUE ZERO.                  
010400   03  WS-IDLPLAN-X              PIC 9(7)    VALUE ZERO.                  
010500   03  TRAFF                     PIC X       VALUE SPACE.                 
010600   03  WS-PREFIX                 PIC 9(3)    VALUE ZERO.                  
010700   03  WS-SUFFIX                 PIC 9(3)    VALUE ZERO.                  
010800     88  MALNINGS-SUFFIX         VALUE 112 113.                           
010900     SKIP3                                                                
011000   03  DAGENS-TIAAVV.                                                     
011100     05  WS-TIAA                 PIC 9(2)    VALUE ZERO.                  
011200     05  WS-TIVV                 PIC 9(2)    VALUE ZERO.                  
011300     SKIP3                                                                
011400   03  DAGENS-DATUM.                                                      
011500     05  WS-TIAA-DAT             PIC 9(2)    VALUE ZERO.                  
011600     05  WS-TIMM-DAT             PIC 9(2)    VALUE ZERO.                  
011700     05  WS-TIDD-DAT             PIC 9(2)    VALUE ZERO.                  
011800     SKIP3                                                                
011900   03  WS-DAGENS-DATUM           PIC S9(6)   VALUE ZERO.                  
012000   03  WS-OLD-IDARTNR            PIC S9(9)   VALUE ZERO.                  
012100   03  WS-IDLEV-SUFF.                                                     
012200     05  WS-IDLEVNR              PIC X(5)   VALUE SPACE.                  
012300     05  WS-IDFTG                PIC 9(2)   VALUE ZERO.                   
012400   03  SPAR-IDLEV-SUFF           PIC 9(7)   VALUE ZERO.                   
012500   03  SPAR-IDOVERFNR-LOP        PIC S9(5)  VALUE ZERO.                   
012600   03  SPAR-IDLEVKND             PIC X(17)  VALUE SPACE.                  
012700   03  ALT-LEVNR                 PIC X(5)   VALUE SPACE.                  
012800   03  ALT-LEVNR2                PIC X(5)   VALUE SPACE.                  
012900     EJECT                                                                
013000*    -COPY W200EMAB                                                       
013100     EJECT                                                                
013200*- - - - - - - - - - - - - -  GENERELLA SUBRUTINER                        
013300 01  SUBPROGRAM.                                                          
013400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
013600   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
013700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013800   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
013900   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
014000     SKIP3                                                                
014100                                                                          
014200*- - - - - - - - - - - - - -  NYCKLAR TILL DLI                            
014300 01  NYCKLAR-TILL-DLI.                                                    
014400   03  W-IDARTNR-X.                                                       
014500     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
014600     SKIP3                                                                
014700   03  W-IDLEVNR-X.                                                       
014800     05  W-IDLEVNR               PIC X(5).                                
014900     SKIP3                                                                
015000   03  W-WDGXKEY-2215-X.                                                  
015100     05  FILLER                  PIC X(4)    VALUE '2215'.                
015200     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
015300     SKIP3                                                                
015400*- - - - - - - - - - - - - -  ARBETSAREOR TILL IMS-SECTIONERNA            
015500 01  IMS-WS.                                                              
015600   03  FILLER                    PIC X(8)   VALUE 'IMS-WS  '.             
015700                                                                          
015800   03  STATUS-WS                 PIC X(2).                                
015900     88  SEGMENT-FINNS                      VALUE '  '.                   
016000     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
016100                                                                          
016200   03  GODK-STATUSKODER.                                                  
016300     05  GODK-STATUS   OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
016400                                                                          
016500   03  SSA1                      PIC X(64).                               
016600   03  SSA2                      PIC X(64).                               
016700     EJECT                                                                
016800*01  -COPY W0003.                                                         
016900     EJECT                                                                
017000 01  DLI-IO-AREA-11.                                                      
017100   03  IO-AREA-11                PIC X(900) VALUE SPACE.                  
017200                                                                          
017300*  03 WLARTC11  -COPY WDK611 -RED IO-AREA-11.                             
017400     EJECT                                                                
017500 01  DLI-IO-AREA.                                                         
017600   03  IO-AREA                   PIC X(200) VALUE SPACE.                  
017700                                                                          
017800*  03 WLARTC22  -COPY WDK622 -RED IO-AREA.                                
017900     EJECT                                                                
018000*  03 WLARTC23  -COPY WDK623 -RED IO-AREA.                                
018100     EJECT                                                                
018200*  03 WLXXBK01  -COPY WDGX01 -PRE XXBK-   -RED IO-AREA.                   
018300     SKIP3                                                                
018400*  03 WLXXBK11  -COPY WDGX2216 -PRE XXBK-   -RED IO-AREA.                 
018500     EJECT                                                                
018600                                                                          
018700*- - - - - - - - - - - - - -  TABELLER                                    
018800 01  FILLER                      PIC X(16)   VALUE ' DAT TABELL'.         
018900 01  FILLER.                                                              
019000   03  TABELL                 OCCURS 50.                                  
019100     05  TAB-DATUM               PIC 9(6).                                
019200     05  TAB-KVANT               PIC 9(9).                                
019300     SKIP3                                                                
019400*- - - - - - - - - - - - - -  SKRIV AREA                                  
019500 01  FILLER                      PIC X(16)   VALUE ' UTPOST '.            
019600 01  UTPOST                      PIC X(128)  VALUE SPACE.                 
019700                                                                          
019800     EJECT                                                                
019900*- - - - - - - - - - - - - -  HJÄLPFÄLT VID FILLÄSNINGARNA                
020000 01  FILLER.                                                              
020100   03  INFIL-ID.                                                          
020200      05  IN-IDLEVNR-ID          PIC X(5)    VALUE SPACE.                 
020300      05  IN-IDFTG-ID            PIC 9(2)    VALUE ZERO COMP-3.           
020400      05  IN-IDARTNR-ID          PIC 9(9)    VALUE ZERO COMP-3.           
020500     SKIP2                                                                
020600   03  REG-ID.                                                            
020700      05  REG-IDLEVNR-ID         PIC X(5)    VALUE SPACE.                 
020800      05  REG-IDFTG-ID           PIC 9(2)    VALUE ZERO COMP-3.           
020900      05  REG-IDARTNR-ID         PIC 9(9)    VALUE ZERO COMP-3.           
021000     SKIP2                                                                
021100 01  MAX-VARDE                   PIC S9(14)  VALUE                        
021200                                             +99999999999999.             
021300     SKIP3                                                                
021400 01  INFIL-TRANSID.                                                       
021500     03  INFIL-FDNAMN            PIC X(6)    VALUE 'W22181'.              
021600     03  INFIL-DDNAMN            PIC X(8)    VALUE 'W22160D1'.            
021700     03  INFIL-TRANSTYP          PIC X(4)    VALUE SPACE.                 
021800     SKIP3                                                                
021900 01  REG-TRANSID-IN.                                                      
022000     03  IN-REG-FDNAMN           PIC X(6)    VALUE 'W22161'.              
022100     03  IN-REG-DDNAMN           PIC X(8)    VALUE 'W22160D2'.            
022200     03  IN-REG-TRANSTYP         PIC X(4)    VALUE SPACE.                 
022300     SKIP3                                                                
022400 01  REG-TRANSID-UT.                                                      
022500     03  UT-REG-FDNAMN           PIC X(6)    VALUE 'W22161'.              
022600     03  UT-REG-DDNAMN           PIC X(8)    VALUE 'W22160D3'.            
022700     03  UT-REG-TRANSTYP         PIC X(4)    VALUE SPACE.                 
022800     SKIP3                                                                
022900 01  W22160-TRANSID.                                                      
023000     03  60-FDNAMN               PIC X(6)    VALUE 'W22160'.              
023100     03  60-DDNAMN               PIC X(8)    VALUE 'W22160D4'.            
023200     03  60-TRANSTYP             PIC X(4)    VALUE SPACE.                 
023201                                                                          
023210 01  W22160B-TRANSID.                                                     
023220     03  60B-FDNAMN              PIC X(7)    VALUE 'W22160B'.             
023230     03  60B-DDNAMN              PIC X(8)    VALUE 'W22160D5'.            
023240     03  60B-TRANSTYP            PIC X(4)    VALUE SPACE.                 
023300     EJECT                                                                
023400*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
023500                                                                          
023600 01  RETURKODER.                                                          
023700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
023800   03  RKOD                      PIC S9(4)   VALUE +0   COMP SYNC.        
023900     SKIP2                                                                
024000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
024100                                                                          
024200*01  -COPY W0005       -PRE POSTSUM-.                                     
024300     EJECT                                                                
024400*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
024500                                                                          
024600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
024700     SKIP2                                                                
024800*01  -COPY WDATKORT                                                       
024900     EJECT                                                                
025000*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
025100                                                                          
025200 01  WDATAREA                    PIC X(8)    VALUE 'WDATAREA'.            
025300     SKIP2                                                                
025400*01  -COPY WDATAREA.                                                      
025500     EJECT                                                                
025600*- - - - - - - - - - - - - -  INPOSTERNA                                  
025700                                                                          
025800 01  FILLER                      PIC X(16)   VALUE 'INFIL'.               
025900     SKIP2                                                                
026000*01  -COPY W22181        -PRE IN-.                                        
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER                      PIC X(16)   VALUE 'INREG'.               
026400     SKIP2                                                                
026500*01  -COPY W22161        -PRE INREG-.                                     
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16)   VALUE 'UTREG'.               
026800     SKIP2                                                                
026900*01  -COPY W22161        -PRE UTREG-.                                     
027000     EJECT                                                                
027100*- - - - - - - - - - - - - -  UTPOSTERNA                                  
027200                                                                          
027300 01  FILLER                      PIC X(16)   VALUE 'UTFIL'.               
027400     SKIP2                                                                
027500*01  -COPY W22160        -PRE UT-.                                        
027600     EJECT                                                                
027700 LINKAGE SECTION.                                                         
027800     SKIP3                                                                
027900*01   -COPY W0009  -PRE MSG-.                                             
028000     SKIP3                                                                
028100*01   -COPY W0008  -PRE WLARTC-.                                          
028200      05  FILLER        PIC X(5).                                         
028300*01   -COPY W0008  -PRE WLXXBK-.                                          
028400      05  FILLER        PIC X(5).                                         
028500                                                                          
028600     EJECT                                                                
028700 PROCEDURE DIVISION USING  MSG-PCB WLARTC-PCB WLXXBK-PCB.                 
028800     ENTRY 'DLITCBL' USING MSG-PCB WLARTC-PCB WLXXBK-PCB.                 
028900     SKIP2                                                                
029000     PERFORM A-INIT                                                       
029100     PERFORM S01-LAS-INFIL                                                
029200     PERFORM S02-LAS-REG                                                  
029300                                                                          
029400     PERFORM UNTIL INFIL-EOF = JA AND REG-EOF = JA                        
029500        IF (WS-IDLEV-SUFF = SPAR-IDLEV-SUFF)                              
029600        OR (INFIL-ID      > REG-ID)                                       
029700           CONTINUE                                                       
029800        ELSE                                                              
029900           PERFORM B-UPPDATERA-IDLPLAN                                    
030000        END-IF                                                            
030100                                                                          
030200        IF INFIL-ID = REG-ID                                              
030300           PERFORM C-BEHANDLA                                             
030400           IF INFIL-EOF = NEJ                                             
030500              PERFORM D-UPPDATERA-REG                                     
030600              PERFORM S03A-SKRIV-UTFIL                                    
030610              PERFORM S03B-SKRIV-UTFIL-TMS                                
030700              PERFORM S01-LAS-INFIL                                       
030800              PERFORM S02-LAS-REG                                         
031000           END-IF                                                         
031100        ELSE                                                              
031200                                                                          
031300           IF INFIL-ID > REG-ID                                           
031400              PERFORM D-UPPDATERA-REG                                     
031500              PERFORM S02-LAS-REG                                         
031600           ELSE                                                           
031700              MOVE JA TO NYUPPLAGG                                        
031800              PERFORM C-BEHANDLA                                          
031900              PERFORM D-UPPDATERA-REG                                     
032000              PERFORM S03A-SKRIV-UTFIL                                    
032010              PERFORM S03B-SKRIV-UTFIL-TMS                                
032100              PERFORM S01-LAS-INFIL                                       
032200           END-IF                                                         
032300        END-IF                                                            
032400     END-PERFORM                                                          
032500                                                                          
032600     PERFORM Z-FINIT                                                      
032700     MOVE ZERO TO RETURN-CODE                                             
032800     GOBACK                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 A-INIT SECTION.                                                          
033200     SKIP2                                                                
033300     OPEN INPUT  W22181                                                   
033400                 W22161-IN                                                
033500     OPEN OUTPUT W22160                                                   
033510                 W22160-TMS                                               
033600                 W22161-UT                                                
033700                                                                          
033800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
033900                                                                          
034000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
034100     MOVE D-AAR    TO WS-TIAA                                             
034200                      WS-TIAA-DAT                                         
034300     MOVE D-VECKA  TO WS-TIVV                                             
034400     MOVE D-MAANAD TO WS-TIMM-DAT                                         
034500     MOVE D-DAG    TO WS-TIDD-DAT                                         
034600                                                                          
034700     MOVE SPACE TO UTREG-IDLEVNR                                          
034800     MOVE ZERO  TO UTREG-IDFTG                                            
034900                   UTREG-IDARTNR                                          
035000                   UTREG-IDLPLAN-ART                                      
035100                   INREG-IDFTG                                            
035200                   INREG-IDARTNR                                          
035300                   INREG-IDLPLAN-ART                                      
035400     MOVE SPACE TO INREG-IDLEVNR                                          
035500     .                                                                    
035600     EJECT                                                                
035700 B-UPPDATERA-IDLPLAN SECTION.                                             
035800     SKIP2                                                                
035900     MOVE IN-IDLEVNR TO W-IDLEVNR                                         
036000     PERFORM IMS-GET-WLXXBK01                                             
036100     PERFORM IMS-GET-WLXXBK11                                             
036200     IF SEGMENT-FINNS                                                     
036300        MOVE XXBK-2216-IDLEVKND  TO SPAR-IDLEVKND                         
036400        MOVE XXBK-2216-IDOVERFNR TO SPAR-IDOVERFNR-LOP                    
036500        MOVE XXBK-2216-TISEND-SEN TO DAT-I-TIDATUM                        
036600        MOVE 'AAMMDD' TO DAT-KDDATFORM                                    
036700        CALL WDATKONV USING DAT-KDDATFORM                                 
036800                            DAT-I-TIDATUM                                 
036900                            DAT-O-TIDATUM                                 
037000                            DAT-KDSVAR                                    
037100     ELSE                                                                 
037200        MOVE ZERO TO DAT-TIAAVV-GRP                                       
037300        MOVE +1   TO SPAR-IDOVERFNR-LOP                                   
037400        MOVE SPACE   TO SPAR-IDLEVKND                                     
037500     END-IF                                                               
037600                                                                          
037700     IF DAT-TIAAVV-GRP = DAGENS-TIAAVV                                    
037800        ADD +1 TO XXBK-2216-IDOVERFNR-VV                                  
037900        MOVE XXBK-2216-IDOVERFNR-VV TO WS-LLL                             
038000     ELSE                                                                 
038100        MOVE +1 TO XXBK-2216-IDOVERFNR-VV WS-LLL                          
038200     END-IF                                                               
038300     MOVE DAGENS-DATUM TO WS-DAGENS-DATUM                                 
038400     MOVE WS-DAGENS-DATUM TO XXBK-2216-TISEND-SEN                         
038500     MOVE DAGENS-TIAAVV TO WS-IDAAVV                                      
038600     MOVE WS-IDLEV-SUFF TO SPAR-IDLEV-SUFF                                
038700     IF SEGMENT-FINNS                                                     
038800        PERFORM IMS-REPL-WLXXBK11                                         
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300 C-BEHANDLA SECTION.                                                      
039400     SKIP2                                                                
039500     IF NYUPPLAGG = NEJ                                                   
039600        MOVE INREG-IDLPLAN-ART TO UT-IDLPLAN-ART                          
039700     ELSE                                                                 
039800        MOVE ZERO              TO UT-IDLPLAN-ART                          
039900     END-IF                                                               
040000                                                                          
040100     MOVE IN-IDARTNR TO W-IDARTNR                                         
040200                                                                          
040300     PERFORM IMS-GET-WDK611                                               
040400                                                                          
040500     IF SEGMENT-FINNS                                                     
040600       MOVE JA              TO SW-WDK611                                  
040700       IF CLAG-IDKAT(3) = 'GEM'                                           
040800         MOVE CLAG-IDKAT(3) TO UT-IDKAT                                   
040900       ELSE                                                               
041000         MOVE SPACE         TO UT-IDKAT                                   
041100       END-IF                                                             
041200       MOVE IN-IDLEVNR      TO WS-IDLEVNR-EMIL                            
041300*      IF EJ-GODK-EMIL-LEVNR                                              
041400*        MOVE SPACE         TO UT-ADINPORT                                
041500*      ELSE                                                               
041600         MOVE CLAG-ADINPORT TO UT-ADINPORT                                
041700*      END-IF                                                             
041800     ELSE                                                                 
041900       MOVE NEJ             TO SW-WDK611                                  
042000       MOVE SPACE           TO UT-IDKAT                                   
042100       MOVE SPACE           TO UT-ADINPORT                                
042200     END-IF                                                               
042300                                                                          
042400     IF SEGMENT-FINNS                                                     
042500* EFTERSOM BOSCH HAR BÅDE 6679 OCH 6680 SOM LEVERANTÖRSNUMMER             
042600* I BESTÄLLNINGSNR MÅSTE MAN SÖKA PÅ BÅDA TVÅ                             
042700       IF IN-IDLEVNR = '6679 ' OR 'T5BVB'                                 
042800          MOVE '6680 ' TO ALT-LEVNR                                       
042900          MOVE 'BRW8A' TO ALT-LEVNR2                                      
043000       ELSE                                                               
043100          MOVE IN-IDLEVNR TO ALT-LEVNR                                    
043200          MOVE IN-IDLEVNR TO ALT-LEVNR2                                   
043300       END-IF                                                             
043400       MOVE ZERO TO UT-IDBEST WS-IDBEST WS-SUFFIX WS-PREFIX               
043500       IF CLAG-KDAVT = 0                                                  
043600         PERFORM IMS-GNP-WDK622                                           
043700         MOVE NEJ TO TRAFF                                                
043800         PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                       
043900           IF (BEST-IDLEVNR-BEST = IN-IDLEVNR OR ALT-LEVNR                
044000               OR ALT-LEVNR2)                                             
044100           AND BEST-KDBEH-BEST   = 1                                      
044200             MOVE BEST-IDBEST     TO WS-IDBEST                            
044300                                     UT-IDBEST                            
044400             MOVE WS-IDBEST(11:3) TO WS-SUFFIX                            
044500             MOVE WS-IDBEST(02:3) TO WS-PREFIX                            
044600             MOVE JA TO TRAFF                                             
044700           END-IF                                                         
044800           PERFORM IMS-GNP-WDK622                                         
044900         END-PERFORM                                                      
045000       ELSE                                                               
045100         PERFORM IMS-GNP-WDK623                                           
045200         MOVE NEJ TO TRAFF                                                
045300         PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                       
045400           IF  AVT-IDLEVNR-AVT   = IN-IDLEVNR OR ALT-LEVNR                
045500               OR ALT-LEVNR2                                              
045600             MOVE AVT-IDAVTAL     TO WS-IDBEST                            
045700                                     UT-IDBEST                            
045800             MOVE WS-IDBEST(11:3) TO WS-SUFFIX                            
045900             MOVE WS-IDBEST(02:3) TO WS-PREFIX                            
046000             MOVE JA TO TRAFF                                             
046100           END-IF                                                         
046200           PERFORM IMS-GNP-WDK623                                         
046300         END-PERFORM                                                      
046400       END-IF                                                             
046500     ELSE                                                                 
046600       MOVE ZERO TO UT-IDBEST                                             
046700                    WS-IDBEST WS-SUFFIX WS-PREFIX                         
046800     END-IF                                                               
046900                                                                          
047000     IF SW-WDK611 = JA                                                    
047100       MOVE CLAG-KDGK TO UT-KDGK-URS                                      
047200       IF MALNINGS-SUFFIX AND WS-PREFIX NOT = 004                         
047300         MOVE +9            TO UT-KDGK-SORT                               
047400       ELSE                                                               
047500           IF CLAG-KDGK = +2                                              
047600             MOVE CLAG-KDGK TO UT-KDGK-SORT                               
047700           ELSE                                                           
047800             MOVE +1        TO UT-KDGK-SORT                               
047900           END-IF                                                         
048000       END-IF                                                             
048100     ELSE                                                                 
048200       IF MALNINGS-SUFFIX AND WS-PREFIX NOT = 004                         
048300         MOVE +9          TO UT-KDGK-SORT                                 
048400       ELSE                                                               
048500         MOVE +1          TO UT-KDGK-SORT                                 
048600       END-IF                                                             
048700       MOVE +0            TO UT-KDGK-URS                                  
048800     END-IF                                                               
048900                                                                          
049000     MOVE IN-IDLEVNR         TO UT-IDLEVNR                                
049100     MOVE IN-IDFTG           TO UT-IDFTG                                  
049200     MOVE IN-IDARTNR         TO UT-IDARTNR                                
049300     MOVE IN-KDPRODSL        TO UT-KDPRODSL                               
049400     MOVE SPAR-IDOVERFNR-LOP TO UT-IDOVERFNR-LOP                          
049500     MOVE SPAR-IDLEVKND      TO UT-IDLEVKND                               
049600     MOVE WS-IDLPLAN         TO WS-IDLPLAN-X                              
049700     MOVE WS-IDLPLAN-X       TO UT-IDLPLAN-LEV                            
049800                                                                          
049900     .                                                                    
050000     EJECT                                                                
050100 D-UPPDATERA-REG SECTION.                                                 
050200     SKIP2                                                                
050300     IF NYUPPLAGG = JA                                                    
050400        MOVE IN-IDLEVNR      TO UTREG-IDLEVNR                             
050500        MOVE IN-IDFTG        TO UTREG-IDFTG                               
050600        MOVE IN-IDARTNR      TO UTREG-IDARTNR                             
050700        MOVE WS-IDLPLAN-X    TO UTREG-IDLPLAN-ART                         
050800        MOVE NEJ TO NYUPPLAGG                                             
050900     ELSE                                                                 
051000        MOVE INREG-IDARTNR      TO UTREG-IDARTNR                          
051100        MOVE INREG-IDFTG        TO UTREG-IDFTG                            
051200        MOVE INREG-IDLEVNR      TO UTREG-IDLEVNR                          
051300        IF INFIL-ID > REG-ID                                              
051400           MOVE INREG-IDLPLAN-ART TO UTREG-IDLPLAN-ART                    
051500        ELSE                                                              
051600           MOVE WS-IDLPLAN-X      TO UTREG-IDLPLAN-ART                    
051700        END-IF                                                            
051800     END-IF                                                               
051900                                                                          
052000     PERFORM S04-SKRIV-REG                                                
052100     EJECT                                                                
052200     .                                                                    
052300 S01-LAS-INFIL SECTION.                                                   
052400     SKIP3                                                                
052500     READ W22181 INTO IN-W22181 AT END MOVE JA TO INFIL-EOF.              
052600                                                                          
052700     IF INFIL-EOF = NEJ                                                   
052800        IF IN-IDARTNR = WS-OLD-IDARTNR                                    
052900           PERFORM UNTIL IN-IDARTNR = WS-OLD-IDARTNR OR                   
053000                                INFIL-EOF = JA                            
053100              READ W22181 INTO IN-W22181 AT END MOVE JA TO                
053200                                INFIL-EOF                                 
053300              END-READ                                                    
053400           END-PERFORM                                                    
053500        END-IF                                                            
053600        MOVE IN-IDARTNR TO WS-OLD-IDARTNR                                 
053700     END-IF                                                               
053800                                                                          
053900     IF INFIL-EOF = NEJ                                                   
054000        MOVE IN-IDLEVNR TO IN-IDLEVNR-ID                                  
054100                           WS-IDLEVNR                                     
054200        MOVE IN-IDFTG   TO IN-IDFTG-ID                                    
054300                           WS-IDFTG                                       
054400        MOVE IN-IDARTNR TO IN-IDARTNR-ID                                  
054500        MOVE INFIL-TRANSID TO POSTSUM-TRANSID                             
054600        CALL POSTSUM USING POSTSUM-PARM                                   
054700     ELSE                                                                 
054800        MOVE MAX-VARDE TO INFIL-ID                                        
054900     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 S02-LAS-REG SECTION.                                                     
055300     SKIP3                                                                
055400     READ W22161-IN INTO INREG-W22161                                     
055500                              AT END MOVE JA TO REG-EOF.                  
055600                                                                          
055700     IF REG-EOF = NEJ                                                     
055800        MOVE INREG-IDLEVNR      TO REG-IDLEVNR-ID                         
055900        MOVE INREG-IDFTG        TO REG-IDFTG-ID                           
056000        MOVE INREG-IDARTNR      TO REG-IDARTNR-ID                         
056100        MOVE REG-TRANSID-IN TO POSTSUM-TRANSID                            
056200        CALL POSTSUM USING POSTSUM-PARM                                   
056300     ELSE                                                                 
056400        MOVE MAX-VARDE TO REG-ID                                          
056500        MOVE SPACE TO INREG-IDLEVNR                                       
056600        MOVE ZERO  TO INREG-IDARTNR                                       
056700                      INREG-IDLPLAN-ART                                   
056800     END-IF                                                               
056900     .                                                                    
057000     EJECT                                                                
057100 S03A-SKRIV-UTFIL SECTION.                                                
057200     SKIP3                                                                
057300     WRITE W22160-POST FROM UT-W22160                                     
057400     MOVE W22160-TRANSID TO POSTSUM-TRANSID                               
057500     CALL POSTSUM USING POSTSUM-PARM                                      
057600     .                                                                    
057700     EJECT                                                                
057710 S03B-SKRIV-UTFIL-TMS SECTION.                                            
057720     SKIP3                                                                
057730     WRITE TMS-W22160-POST FROM UT-W22160                                 
057740     MOVE W22160B-TRANSID TO POSTSUM-TRANSID                              
057750     CALL POSTSUM USING POSTSUM-PARM                                      
057760     .                                                                    
057770     EJECT                                                                
057800 S04-SKRIV-REG SECTION.                                                   
057900     SKIP3                                                                
058000     WRITE REG-W22161 FROM UTREG-W22161                                   
058100     MOVE REG-TRANSID-UT TO POSTSUM-TRANSID                               
058200     CALL POSTSUM USING POSTSUM-PARM                                      
058300                                                                          
058400     .                                                                    
058500     EJECT                                                                
058600 IMS-GET-WDK611 SECTION.                                                  
058700     SKIP3                                                                
058800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
058900     DELIMITED BY SIZE INTO SSA1                                          
059000     MOVE 'WLARTC11' TO SSA2                                              
059100     MOVE '  GE' TO GODK-STATUSKODER                                      
059200     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA-11 SSA1 SSA2            
059300     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     SKIP2                                                                
059700 IMS-GNP-WDK622 SECTION.                                                  
059800     SKIP3                                                                
059900     MOVE 'WLARTC22' TO SSA1                                              
060000     MOVE '  GE' TO GODK-STATUSKODER                                      
060100     CALL CBLTDLI USING GNP WLARTC-PCB DLI-IO-AREA SSA1                   
060200     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     SKIP2                                                                
060600 IMS-GNP-WDK623 SECTION.                                                  
060700     SKIP3                                                                
060800     MOVE 'WLARTC23' TO SSA1                                              
060900     MOVE '  GE' TO GODK-STATUSKODER                                      
061000     CALL CBLTDLI USING GNP WLARTC-PCB DLI-IO-AREA SSA1                   
061100     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
061200     PERFORM IMS-STATUSKONTROLL                                           
061300     .                                                                    
061400     EJECT                                                                
061500 IMS-GET-WLXXBK01 SECTION.                                                
061600     SKIP3                                                                
061700     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-2215-X ')'                    
061800     DELIMITED BY SIZE INTO SSA1                                          
061900     MOVE '  ' TO GODK-STATUSKODER                                        
062000     CALL CBLTDLI USING GU WLXXBK-PCB DLI-IO-AREA SSA1                    
062100     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     SKIP3                                                                
062500 IMS-GET-WLXXBK11 SECTION.                                                
062600     SKIP3                                                                
062700     STRING 'WLXXBK11(IDLEVNR  =' W-IDLEVNR-X ')'                         
062800     DELIMITED BY SIZE INTO SSA1                                          
062900     MOVE '  GE' TO GODK-STATUSKODER                                      
063000     CALL CBLTDLI USING GHNP WLXXBK-PCB DLI-IO-AREA SSA1                  
063100     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400     SKIP3                                                                
063500 IMS-REPL-WLXXBK11 SECTION.                                               
063600     SKIP3                                                                
063700     MOVE '  ' TO GODK-STATUSKODER                                        
063800     CALL CBLTDLI USING REPL WLXXBK-PCB DLI-IO-AREA                       
063900     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     EJECT                                                                
064300 IMS-STATUSKONTROLL SECTION.                                              
064400     SKIP3                                                                
064500     SET STATUS-IX TO 1                                                   
064600     SEARCH GODK-STATUS AT END CALL FELLOG                                
064700     WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                              
064800     CONTINUE                                                             
064900     END-SEARCH                                                           
065000                                                                          
065100     .                                                                    
065200 Z-FINIT   SECTION.                                                       
065300     SKIP3                                                                
065400     CLOSE  W22181                                                        
065500            W22161-IN                                                     
065600            W22161-UT                                                     
065700            W22160                                                        
065710            W22160-TMS                                                    
065800*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
065900*                               SKRIVNA POSTER                            
066000     MOVE 'S' TO POSTSUM-OPKOD                                            
066100     CALL POSTSUM USING POSTSUM-PARM                                      
066200     .                                                                    
