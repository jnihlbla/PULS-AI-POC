000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W2216C00.                                        
000400 AUTHOR.                 JOHAN NIHLBLAD.                                  
000500 DATE-WRITTEN.           14/11/2011.                                      
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            INNAN DETTA PROGRAM LIGGER DET EN STD.SORT DÄR               
001100*            MAN SORTERAR W2216A PÅ LEVNR, ARTNR.                         
001200*            PROGRAMMET LÄSER EN FIL (W2216A) MED ARTIKLAR SOM            
001300*            SKALL SÄNDAS TILL LEVERANTÖREN VIA ODETTE/EDI.               
001400*            REGISTER (W2216A) LÄSES OCH UPPDATERAS. WDGX2206             
001500*            LÄSES OCH UPPDATERAS.                                        
001600*            INFORMATION HÄMTAS ÄVEN FRÅN WDK6 (KDGK, IDBEST              
001700*            OCH IDKAT) - UTGÅR 2012 KINA LOCAL SOURCING.                 
001710*                                                                         
001711*            INFORMATION HÄMTAS ÄVEN FRÅN WDK723 (IDBEST)                 
001720*                                                                         
001730*    CHANGES:                                                             
001800*    2012-08-29 E-TRACKER 10143273 LOCAL SOURCING CHINA                   
001810*                                                                         
001813*    2013-11-11 E-TRACKER 10205391 TESTAR RTN W224V2/W221D3,RÄTTA         
001814*                                  FEL I PROD.                            
001820*                                                                         
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
003100     SELECT  W2216A                   ASSIGN TO W2216CD1.                 
003200     SELECT  W2216E-IN                ASSIGN TO W2216CD2.                 
003300     SKIP2                                                                
003400*- - - - - - - - - - - - - - UTFILER:                                     
003500                                                                          
003600     SELECT  W2216E-UT                ASSIGN TO W2216CD3.                 
003700     SELECT  W2216C                   ASSIGN TO W2216CD4.                 
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300*************** INFIL                                                     
004400 FD  W2216A                                                               
004500     LABEL RECORD STANDARD                                                
004600     RECORDING      F                                                     
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004900*01  -COPY W2216A                           -L.                           
005000     SKIP3                                                                
005100 FD  W2216E-IN                                                            
005200     LABEL RECORD STANDARD                                                
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*01  -COPY W2216E                           -L.                           
005700     SKIP3                                                                
005800 FD  W2216E-UT                                                            
005900     LABEL RECORD STANDARD                                                
006000     RECORDING   F                                                        
006100     BLOCK CONTAINS 0.                                                    
006200     SKIP2                                                                
006300 01  REG-W2216E.                                                          
006400*    03     -COPY W2216E                    -L.                           
006500     SKIP3                                                                
006600*************** UTFIL                                                     
006700 FD  W2216C                                                               
006800     LABEL RECORD STANDARD                                                
006900     RECORDING   F                                                        
007000     BLOCK CONTAINS 0.                                                    
007100     SKIP2                                                                
007200 01  W2216C-POST.                                                         
007300*    03     -COPY W2216C                    -L.                           
007400     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600     SKIP2                                                                
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
008000 77   PROGRAM-NAMN           VALUE 'W2216C00'                             
008100                                 PIC X(8).                                
008200     SKIP2                                                                
008300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
008400                                                                          
008500 77  JA                          PIC X       VALUE 'J'.                   
008600 77  NEJ                         PIC X       VALUE 'N'.                   
008700 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
008710 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
008720                                                                          
008730 77  NYUPPLAGG                   PIC X       VALUE 'N'.                   
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
009920   03  WS-ADINPORT.                                                       
009930     05  FILLER                  PIC X(02)   VALUE 'DC'.                  
009940     05  WS-ADINPORT-DC          PIC X(02).                               
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
012300     05  WS-IDDC                 PIC X(2)   VALUE SPACE.                  
012401   03  SPAR-IDLEV-SUFF           PIC X(7)   VALUE SPACE.                  
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
014700   03  W-IDLEVNR-X.                                                       
014800     05  W-IDLEVNR               PIC X(5).                                
014900     SKIP3                                                                
015300   03  W-WDGXKEY-2205-X.                                                  
015400     05  FILLER                  PIC X(4)    VALUE '2205'.                
015500     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
015600     SKIP3                                                                
015700     03  W-KY2206-X.                                                      
015710         05  W-IDLEVNR-2206      PIC X(5)     VALUE SPACE.                
015800         05  W-IDDC-2206         PIC X(2)     VALUE SPACE.                
016000                                                                          
016010   03  W-IDARTNR-X.                                                       
016020       05  W-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.           
016030                                                                          
016031   03  W-IDDC-X.                                                          
016032       05  W-IDDC                PIC X(2)     VALUE SPACE.                
016033                                                                          
016040                                                                          
016100*- - - - - - - - - - - - - -  ARBETSAREOR TILL IMS-SECTIONERNA            
016200 01  IMS-WS.                                                              
016300   03  FILLER                    PIC X(8)   VALUE 'IMS-WS  '.             
016400                                                                          
016500   03  STATUS-WS                 PIC X(2).                                
016600     88  SEGMENT-FINNS                      VALUE '  '.                   
016700     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
016800                                                                          
016900   03  GODK-STATUSKODER.                                                  
017000     05  GODK-STATUS   OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
017100                                                                          
017200   03  SSA1                      PIC X(64).                               
017300   03  SSA2                      PIC X(64).                               
017400     EJECT                                                                
017500*01  -COPY W0003.                                                         
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800                                                                          
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR2'.                        
018800 01  DLI-IO-WDR201.                                                       
018900*    03  -COPY WDGX01                                                     
019000     EJECT                                                                
019100                                                                          
019200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
019300 01  DLI-IO-WDGX2206.                                                     
019400*    03  -COPY  WDGX2206                                                  
019500     EJECT                                                                
019510                                                                          
019511 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
019512 01  DLI-IO-WDK711.                                                       
019513*    03  -COPY  WDK711                                                    
019514     EJECT                                                                
019520                                                                          
019521 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
019522 01  DLI-IO-WDK723.                                                       
019523*    03  -COPY  WDK723                                                    
019524     EJECT                                                                
019540                                                                          
019600*- - - - - - - - - - - - - -  TABELLER                                    
019700 01  FILLER                      PIC X(16)   VALUE ' DAT TABELL'.         
019800 01  FILLER.                                                              
019900   03  TABELL                 OCCURS 50.                                  
020000     05  TAB-DATUM               PIC 9(6).                                
020100     05  TAB-KVANT               PIC 9(9).                                
020200     SKIP3                                                                
020300*- - - - - - - - - - - - - -  SKRIV AREA                                  
020400 01  FILLER                      PIC X(16)   VALUE ' UTPOST '.            
020500 01  UTPOST                      PIC X(128)  VALUE SPACE.                 
020600                                                                          
020700     EJECT                                                                
020800*- - - - - - - - - - - - - -  HJÄLPFÄLT VID FILLÄSNINGARNA                
020900 01  FILLER.                                                              
021000   03  INFIL-ID.                                                          
021100      05  IN-IDLEVNR-ID          PIC X(5)    VALUE SPACE.                 
021200      05  IN-IDDC-ID             PIC X(2)    VALUE SPACE.                 
021300      05  IN-IDARTNR-ID          PIC 9(9)    VALUE ZERO COMP-3.           
021400     SKIP2                                                                
021500   03  REG-ID.                                                            
021600      05  REG-IDLEVNR-ID         PIC X(5)    VALUE SPACE.                 
021700      05  REG-IDDC-ID            PIC X(2)    VALUE SPACE.                 
021800      05  REG-IDARTNR-ID         PIC 9(9)    VALUE ZERO COMP-3.           
021900     SKIP2                                                                
022000 01  MAX-VARDE                   PIC S9(14)  VALUE                        
022100                                             +99999999999999.             
022200     SKIP3                                                                
022300 01  INFIL-TRANSID.                                                       
022400     03  INFIL-FDNAMN            PIC X(6)    VALUE 'W2216A'.              
022500     03  INFIL-DDNAMN            PIC X(8)    VALUE 'W2216CD1'.            
022600     03  INFIL-TRANSTYP          PIC X(4)    VALUE SPACE.                 
022700     SKIP3                                                                
022800 01  REG-TRANSID-IN.                                                      
022900     03  IN-REG-FDNAMN           PIC X(6)    VALUE 'W2216E'.              
023000     03  IN-REG-DDNAMN           PIC X(8)    VALUE 'W2216CD2'.            
023100     03  IN-REG-TRANSTYP         PIC X(4)    VALUE SPACE.                 
023200     SKIP3                                                                
023300 01  REG-TRANSID-UT.                                                      
023400     03  UT-REG-FDNAMN           PIC X(6)    VALUE 'W2216E'.              
023500     03  UT-REG-DDNAMN           PIC X(8)    VALUE 'W2216CD3'.            
023600     03  UT-REG-TRANSTYP         PIC X(4)    VALUE SPACE.                 
023700     SKIP3                                                                
023800 01  W2216C-TRANSID.                                                      
023900     03  6C-FDNAMN               PIC X(6)    VALUE 'W2216C'.              
024000     03  6C-DDNAMN               PIC X(8)    VALUE 'W2216CD4'.            
024100     03  6C-TRANSTYP             PIC X(4)    VALUE SPACE.                 
024200     EJECT                                                                
024300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
024400                                                                          
024500 01  RETURKODER.                                                          
024600   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
024700   03  RKOD                      PIC S9(4)   VALUE +0   COMP SYNC.        
024800     SKIP2                                                                
024900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
025000                                                                          
025100*01  -COPY W0005       -PRE POSTSUM-.                                     
025200     EJECT                                                                
025300*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
025400                                                                          
025500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
025600     SKIP2                                                                
025700*01  -COPY WDATKORT                                                       
025800     EJECT                                                                
025900*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
026000                                                                          
026100 01  WDATAREA                    PIC X(8)    VALUE 'WDATAREA'.            
026200     SKIP2                                                                
026300*01  -COPY WDATAREA.                                                      
026400     EJECT                                                                
026500*- - - - - - - - - - - - - -  INPOSTERNA                                  
026600                                                                          
026700 01  FILLER                      PIC X(16)   VALUE 'INFIL'.               
026800     SKIP2                                                                
026900*01  -COPY W2216A        -PRE IN-.                                        
027000     EJECT                                                                
027100                                                                          
027200 01  FILLER                      PIC X(16)   VALUE 'INREG'.               
027300     SKIP2                                                                
027400*01  -COPY W2216E        -PRE INREG-.                                     
027500     EJECT                                                                
027600 01  FILLER                      PIC X(16)   VALUE 'UTREG'.               
027700     SKIP2                                                                
027800*01  -COPY W2216E        -PRE UTREG-.                                     
027900     EJECT                                                                
028000*- - - - - - - - - - - - - -  UTPOSTERNA                                  
028100                                                                          
028200 01  FILLER                      PIC X(16)   VALUE 'UTFIL'.               
028300     SKIP2                                                                
028400*01  -COPY W2216C        -PRE UT-.                                        
028500     EJECT                                                                
028600 LINKAGE SECTION.                                                         
028700     SKIP3                                                                
028800*01   -COPY W0009  -PRE MSG-.                                             
028900     SKIP3                                                                
029200*01   -COPY W0008  -PRE WDR2-.                                            
029300      05  FILLER        PIC X(5).                                         
029310*01   -COPY W0008  -PRE WDK7-                                             
029320      05  FILLER        PIC X.                                            
029400                                                                          
029500     EJECT                                                                
029600 PROCEDURE DIVISION USING  MSG-PCB WDR2-PCB WDK7-PCB.                     
029700     ENTRY 'DLITCBL' USING MSG-PCB WDR2-PCB WDK7-PCB.                     
029800     SKIP2                                                                
029900     PERFORM A-INIT                                                       
030000     PERFORM S01-LAS-INFIL                                                
030100     PERFORM S02-LAS-REG                                                  
030200                                                                          
030300     PERFORM UNTIL INFIL-EOF = JA AND REG-EOF = JA                        
030400        IF (WS-IDLEV-SUFF = SPAR-IDLEV-SUFF)                              
030500        OR (INFIL-ID      > REG-ID)                                       
030600           CONTINUE                                                       
030700        ELSE                                                              
030800           PERFORM B-UPPDATERA-IDLPLAN                                    
030900        END-IF                                                            
031000                                                                          
031100        IF INFIL-ID = REG-ID                                              
031200           PERFORM C-BEHANDLA                                             
031300           IF INFIL-EOF = NEJ                                             
031400              PERFORM D-UPPDATERA-REG                                     
031500              PERFORM S03-SKRIV-UTFIL                                     
031600              PERFORM S01-LAS-INFIL                                       
031700              PERFORM S02-LAS-REG                                         
031800           END-IF                                                         
031900        ELSE                                                              
032000                                                                          
032100           IF INFIL-ID > REG-ID                                           
032200              PERFORM D-UPPDATERA-REG                                     
032300              PERFORM S02-LAS-REG                                         
032400           ELSE                                                           
032500              MOVE JA TO NYUPPLAGG                                        
032600              PERFORM C-BEHANDLA                                          
032700              PERFORM D-UPPDATERA-REG                                     
032800              PERFORM S03-SKRIV-UTFIL                                     
032900              PERFORM S01-LAS-INFIL                                       
033000           END-IF                                                         
033100        END-IF                                                            
033200     END-PERFORM                                                          
033300                                                                          
033400     PERFORM Z-FINIT                                                      
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 A-INIT SECTION.                                                          
034000     SKIP2                                                                
034100     OPEN INPUT  W2216A                                                   
034200                 W2216E-IN                                                
034300     OPEN OUTPUT W2216C                                                   
034400                 W2216E-UT                                                
034500                                                                          
034600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
034700                                                                          
034800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
034900     MOVE D-AAR    TO WS-TIAA                                             
035000                      WS-TIAA-DAT                                         
035100     MOVE D-VECKA  TO WS-TIVV                                             
035200     MOVE D-MAANAD TO WS-TIMM-DAT                                         
035300     MOVE D-DAG    TO WS-TIDD-DAT                                         
035400                                                                          
035500     MOVE SPACE TO UTREG-IDLEVNR                                          
035600                   UTREG-IDDC                                             
035700     MOVE ZERO  TO UTREG-IDARTNR                                          
035800                   UTREG-IDLPLAN-ART                                      
036000                   INREG-IDARTNR                                          
036100                   INREG-IDLPLAN-ART                                      
036200     MOVE SPACE TO INREG-IDLEVNR                                          
036210                   INREG-IDDC                                             
036300     .                                                                    
036400     EJECT                                                                
036500 B-UPPDATERA-IDLPLAN SECTION.                                             
036510     MOVE 'B-UPPDATERA-IDLPLAN'    TO CURRENT-SECTION                     
036600     SKIP2                                                                
036700     MOVE IN-IDDC    TO W-IDDC-2206                                       
036800     MOVE IN-IDLEVNR TO W-IDLEVNR-2206                                    
036900     PERFORM IMS-GHU-WDGX2206                                             
037000     IF SEGMENT-FINNS                                                     
037100        MOVE 2206-IDLEVKND  TO SPAR-IDLEVKND                              
037200        MOVE 2206-IDOVERFNR TO SPAR-IDOVERFNR-LOP                         
037300        MOVE 2206-TISEND-SEN TO DAT-I-TIDATUM                             
037400        MOVE 'AAMMDD' TO DAT-KDDATFORM                                    
037500        CALL WDATKONV USING DAT-KDDATFORM                                 
037600                            DAT-I-TIDATUM                                 
037700                            DAT-O-TIDATUM                                 
037800                            DAT-KDSVAR                                    
037900     ELSE                                                                 
038000        MOVE ZERO TO DAT-TIAAVV-GRP                                       
038100        MOVE +1   TO SPAR-IDOVERFNR-LOP                                   
038200        MOVE SPACE   TO SPAR-IDLEVKND                                     
038300     END-IF                                                               
038400                                                                          
038500     IF DAT-TIAAVV-GRP = DAGENS-TIAAVV                                    
038600        ADD +1 TO 2206-IDOVERFNR-VV                                       
038700        MOVE 2206-IDOVERFNR-VV TO WS-LLL                                  
038800     ELSE                                                                 
038900        MOVE +1 TO 2206-IDOVERFNR-VV WS-LLL                               
039000     END-IF                                                               
039100     MOVE DAGENS-DATUM TO WS-DAGENS-DATUM                                 
039200     MOVE WS-DAGENS-DATUM TO 2206-TISEND-SEN                              
039300     MOVE DAGENS-TIAAVV TO WS-IDAAVV                                      
039400     MOVE WS-IDLEV-SUFF TO SPAR-IDLEV-SUFF                                
039500     IF SEGMENT-FINNS                                                     
039600        PERFORM IMS-REPL-WDGX2206                                         
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000                                                                          
040100 C-BEHANDLA SECTION.                                                      
040110     MOVE 'C-BEHANDLA '     TO CURRENT-SECTION                            
040200     SKIP2                                                                
040300     IF NYUPPLAGG = NEJ                                                   
040400        MOVE INREG-IDLPLAN-ART TO UT-IDLPLAN-ART                          
040500     ELSE                                                                 
040600        MOVE ZERO              TO UT-IDLPLAN-ART                          
040700     END-IF                                                               
040800                                                                          
042800*****SÄTT IDBEST + ADINPORT                                               
042810     MOVE IN-IDDC          TO WS-ADINPORT-DC                              
042820     MOVE WS-ADINPORT      TO UT-ADINPORT                                 
044510                                                                          
044520     MOVE IN-IDARTNR      TO W-IDARTNR                                    
044521     MOVE IN-IDDC         TO W-IDDC                                       
044530                                                                          
044540     PERFORM IMS-GU-WDK711                                                
044550     IF SEGMENT-FINNS                                                     
044560       MOVE ZERO TO UT-IDBEST                                             
044570       PERFORM IMS-GNP-WDK723                                             
044571       MOVE NEJ TO TRAFF                                                  
044572       PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                         
044573         IF SAVT-IDLEVNR-AVT   = IN-IDLEVNR                               
044575           MOVE SAVT-IDAVTAL   TO UT-IDBEST                               
044580           MOVE JA TO TRAFF                                               
044590         END-IF                                                           
044591         PERFORM IMS-GNP-WDK723                                           
044592       END-PERFORM                                                        
044593     ELSE                                                                 
044594       MOVE ZERO TO UT-IDBEST                                             
044596     END-IF                                                               
044597                                                                          
044598                                                                          
044600**** SÄTT KDGK                                                            
044700**** KDGK-URS ANVÄNDS EJ I W2216D00                                       
044800     MOVE +1                 TO UT-KDGK-URS                               
044900                                UT-KDGK-SORT                              
044901**** VI SÄTTER SPACE TILL UT-IDKAT FÖR DETTA BEHÖVS EJ LÄNGRE             
044910     MOVE SPACE              TO UT-IDKAT                                  
045000****                                                                      
045010**** KDPRODSL ANVÄND EJ I W2216D00                                        
045100     MOVE ZERO               TO UT-KDPRODSL                               
045200     MOVE IN-IDLEVNR         TO UT-IDLEVNR                                
045300     MOVE IN-IDDC            TO UT-IDDC                                   
045400     MOVE IN-IDARTNR         TO UT-IDARTNR                                
045500     MOVE IN-KVBEART         TO UT-KVBEART                                
045600     MOVE IN-KVDAGAR         TO UT-KVDAGAR                                
045700     MOVE SPAR-IDOVERFNR-LOP TO UT-IDOVERFNR-LOP                          
045800     MOVE SPAR-IDLEVKND      TO UT-IDLEVKND                               
045900     MOVE WS-IDLPLAN         TO WS-IDLPLAN-X                              
046000     MOVE WS-IDLPLAN-X       TO UT-IDLPLAN-LEV                            
046100                                                                          
046200     .                                                                    
046300     EJECT                                                                
046400 D-UPPDATERA-REG SECTION.                                                 
046410     MOVE 'D-UPPDATERA-REG '    TO CURRENT-SECTION                        
046500     SKIP2                                                                
046600     IF NYUPPLAGG = JA                                                    
046700        MOVE IN-IDLEVNR      TO UTREG-IDLEVNR                             
046800        MOVE IN-IDDC         TO UTREG-IDDC                                
046900        MOVE IN-IDARTNR      TO UTREG-IDARTNR                             
047000        MOVE WS-IDLPLAN-X    TO UTREG-IDLPLAN-ART                         
047100        MOVE NEJ TO NYUPPLAGG                                             
047200     ELSE                                                                 
047300        MOVE INREG-IDARTNR      TO UTREG-IDARTNR                          
047400        MOVE INREG-IDDC         TO UTREG-IDDC                             
047500        MOVE INREG-IDLEVNR      TO UTREG-IDLEVNR                          
047600        IF INFIL-ID > REG-ID                                              
047700           MOVE INREG-IDLPLAN-ART TO UTREG-IDLPLAN-ART                    
047800        ELSE                                                              
047900           MOVE WS-IDLPLAN-X      TO UTREG-IDLPLAN-ART                    
048000        END-IF                                                            
048100     END-IF                                                               
048200                                                                          
048300     PERFORM S04-SKRIV-REG                                                
048500     .                                                                    
048510     EJECT                                                                
048600 S01-LAS-INFIL SECTION.                                                   
048700     SKIP3                                                                
048801     READ W2216A INTO IN-W2216A                                           
048802                      AT END MOVE JA TO INFIL-EOF                         
048810     END-READ                                                             
048900                                                                          
049000     IF INFIL-EOF = NEJ                                                   
049100        IF IN-IDARTNR = WS-OLD-IDARTNR                                    
049200           PERFORM UNTIL IN-IDARTNR = WS-OLD-IDARTNR OR                   
049300                                INFIL-EOF = JA                            
049400              READ W2216A INTO IN-W2216A AT END MOVE JA TO                
049500                                INFIL-EOF                                 
049600              END-READ                                                    
049700           END-PERFORM                                                    
049800        END-IF                                                            
049900        MOVE IN-IDARTNR TO WS-OLD-IDARTNR                                 
050000     END-IF                                                               
050100                                                                          
050200     IF INFIL-EOF = NEJ                                                   
050300        MOVE IN-IDLEVNR TO IN-IDLEVNR-ID                                  
050400                           WS-IDLEVNR                                     
050500        MOVE IN-IDDC    TO IN-IDDC-ID                                     
050600                           WS-IDDC                                        
050700        MOVE IN-IDARTNR TO IN-IDARTNR-ID                                  
050800        MOVE INFIL-TRANSID TO POSTSUM-TRANSID                             
050900        CALL POSTSUM USING POSTSUM-PARM                                   
051000     ELSE                                                                 
051100        MOVE MAX-VARDE TO INFIL-ID                                        
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500 S02-LAS-REG SECTION.                                                     
051600     SKIP3                                                                
051700     READ W2216E-IN INTO INREG-W2216E                                     
051800                              AT END MOVE JA TO REG-EOF                   
051810     END-READ                                                             
051900                                                                          
052000     IF REG-EOF = NEJ                                                     
052100        MOVE INREG-IDLEVNR      TO REG-IDLEVNR-ID                         
052200        MOVE INREG-IDDC         TO REG-IDDC-ID                            
052300        MOVE INREG-IDARTNR      TO REG-IDARTNR-ID                         
052400        MOVE REG-TRANSID-IN TO POSTSUM-TRANSID                            
052500        CALL POSTSUM USING POSTSUM-PARM                                   
052600     ELSE                                                                 
052700        MOVE MAX-VARDE TO REG-ID                                          
052800        MOVE SPACE TO INREG-IDLEVNR                                       
052900        MOVE ZERO  TO INREG-IDARTNR                                       
053000                      INREG-IDLPLAN-ART                                   
053100     END-IF                                                               
053200     .                                                                    
053300     EJECT                                                                
053400 S03-SKRIV-UTFIL SECTION.                                                 
053500     SKIP3                                                                
053600     WRITE W2216C-POST FROM UT-W2216C                                     
053700     MOVE W2216C-TRANSID TO POSTSUM-TRANSID                               
053800     CALL POSTSUM USING POSTSUM-PARM                                      
053900     .                                                                    
054000     EJECT                                                                
054100 S04-SKRIV-REG SECTION.                                                   
054200     SKIP3                                                                
054300     WRITE REG-W2216E FROM UTREG-W2216E                                   
054400     MOVE REG-TRANSID-UT TO POSTSUM-TRANSID                               
054500     CALL POSTSUM USING POSTSUM-PARM                                      
054600                                                                          
054700     .                                                                    
054800     EJECT                                                                
056100 IMS-GHU-WDGX2206 SECTION.                                                
056200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
056300             DELIMITED BY SIZE INTO SSA1                                  
056400     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
056500          DELIMITED BY SIZE INTO SSA2                                     
056600     MOVE '  GE' TO GODK-STATUSKODER                                      
056700     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX2206 SSA1 SSA2            
056800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
056900     PERFORM IMS-STATUSKONTROLL                                           
057000     SKIP3                                                                
057100     .                                                                    
057110     EJECT                                                                
057200 IMS-REPL-WDGX2206 SECTION.                                               
057300     SKIP3                                                                
057400     MOVE '  ' TO GODK-STATUSKODER                                        
057500     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-WDGX2206                     
057600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     EJECT                                                                
057910 IMS-GU-WDK711 SECTION.                                                   
057920     MOVE 'IMS-GU-WDK711 '   TO DBS-SECTION                               
057921                                                                          
057930     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
057940            DELIMITED BY SIZE INTO SSA1                                   
057950     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
057960            DELIMITED BY SIZE INTO SSA2                                   
057980     MOVE '  GE' TO GODK-STATUSKODER                                      
057990     CALL CBLTDLI USING                                                   
057991           GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2                            
057992     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
057993     PERFORM IMS-STATUSKONTROLL                                           
057994     .                                                                    
057995     EJECT                                                                
057996 IMS-GNP-WDK723 SECTION.                                                  
057997     MOVE 'IMS-GNP-WDK723 '   TO DBS-SECTION                              
057998                                                                          
057999     MOVE 'WDK723   ' TO SSA1                                             
058000     MOVE '  GE' TO GODK-STATUSKODER                                      
058001     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
058002     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
058003     PERFORM IMS-STATUSKONTROLL                                           
058004     .                                                                    
058005     EJECT                                                                
058010 IMS-STATUSKONTROLL SECTION.                                              
058100     SKIP3                                                                
058200     SET STATUS-IX TO 1                                                   
058300     SEARCH GODK-STATUS AT END CALL FELLOG                                
058400     WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                              
058500     CONTINUE                                                             
058600     END-SEARCH                                                           
058700                                                                          
058800     .                                                                    
058900 Z-FINIT   SECTION.                                                       
059000     SKIP3                                                                
059100     CLOSE  W2216A                                                        
059200            W2216E-IN                                                     
059300            W2216E-UT                                                     
059400            W2216C                                                        
059500*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
059600*                               SKRIVNA POSTER                            
059700     MOVE 'S' TO POSTSUM-OPKOD                                            
059800     CALL POSTSUM USING POSTSUM-PARM                                      
059900     .                                                                    
