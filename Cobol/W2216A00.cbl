000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W2216A00.                                        
000400 AUTHOR.                 JOHAN NIHLBLAD.                                  
000500 DATE-WRITTEN.           NOVEMBER 2011.                                   
000600                                                                          
000700*    DETTA PROGRAM ÄR DELVIS EN KOPIA PÅ W2218000.                        
000800*    JAG HAR FLYTTAT IN IMS LÄSNINGARNA I PGM, FÖR PGM W2218000           
000900*    LIGGER LÄSNINGARNA I W2218010                                        
001000*                                                                         
001100*  FUNKTION: PROGRAMMET LÄSER EN LEVERANTÖR PÅ WDGX2206. MED DENNA        
001200*        SOM NYCKEL +DC HÄMTAS SAMTLIGA ARTIKLAR MED DENNA LEV-           
001300*            RANTÖR PÅ WDGX2248.FÖR VARJE ARTIKEL SKAPAS EN UTPOST        
001400*            OCH SEGMENTET WDGX2248 DELETAS I PGM W2216B00.               
001500*            LEVERANTÖRENS TISEND-SEN OCH IDOVERFNR UPPDATERAS            
001600*            OCH REPLACE SKER AV SEGMENTET.(WDGX2206).                    
001700*                                                                         
001800*            2 UTFILER SKAPAS EN FÖR ARTIKLAR SOM SKALL ÖVERFÖRAS         
001900*            VIA EDI OCH EN SOM SKAPAR DELETE POSTER FÖR W2216A00         
002000*            SOM DELETAR WDGX2248 SEGMENT.                                
002100*                                                                         
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SELECT  PARMIN                   ASSIGN TO W2216AD1.                 
002900*- - - - - - - - - - - - - - UTFILER:                                     
003000                                                                          
003100     SELECT  W2216A                   ASSIGN TO W2216AD2.                 
003200     SELECT  W2216B                   ASSIGN TO W2216AD3.                 
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800                                                                          
003900 FD  PARMIN                                                               
004000     LABEL RECORD STANDARD                                                
004100     RECORDING F                                                          
004200     BLOCK CONTAINS 0.                                                    
004300 01  FILLER                    PIC X(80).                                 
004400     SKIP3                                                                
004500 FD  W2216A                                                               
004600     LABEL RECORD STANDARD                                                
004700     RECORDING F                                                          
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000*01  UTPOST -COPY W2216A       -L.                                        
005100     SKIP3                                                                
005200 FD  W2216B                                                               
005300     LABEL RECORD STANDARD                                                
005400     RECORDING F                                                          
005500     BLOCK CONTAINS 0.                                                    
005600     SKIP2                                                                
005700*01  UTPOST2 -COPY W2216B      -L.                                        
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000     SKIP2                                                                
006100*    -COPY WY2000W1                                                       
006200     SKIP3                                                                
006300*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
006400 77   PROGRAM-NAMN           VALUE 'W2216A00'                             
006500                                 PIC X(8).                                
006600     SKIP2                                                                
006700*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006800                                                                          
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
007200 77  SW-SKRIV-UT-W2216A          PIC X       VALUE 'J'.                   
007300                                                                          
007400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007500                                                                          
007600 01  ARBETS-FALT.                                                         
007700   03  WS-IDLPLAN                PIC 9(7)    VALUE ZERO.                  
007800   03  WS-IDAAVVLLL.                                                      
007900     05 WS-IDAAVV                PIC 9(4).                                
008000     05 WS-LLL                   PIC 9(3).                                
008100     EJECT                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008400   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
008500   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008600   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008700   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008800     SKIP3                                                                
008900 01  WS-DAGENS-DATUM.                                                     
009000     03  DAGENS-AA               PIC 9(02).                               
009100     03  DAGENS-MM               PIC 9(02).                               
009200     03  DAGENS-DD               PIC 9(02).                               
009300                                                                          
009400 01  DAGENS-DATUM                PIC 9(06).                               
009500                                                                          
009600 01  DAGENS-DAGNR                PIC 9(1) VALUE ZERO.                     
009700                                                                          
009800 01  DAGENS-AAR-VV.                                                       
009900     05  DAGENS-AAR              PIC 9(02).                               
010000     05  DAGENS-VV               PIC 9(02).                               
010100     EJECT                                                                
010200 01  PARM-AREA.                                                           
010300     03  PARM-IDDC-FOM           PIC X(02) VALUE SPACE.                   
010400     03  PARM-IDDC-TOM           PIC X(02) VALUE SPACE.                   
010500     03  FILLER                  PIC X(76) VALUE SPACE.                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'WWDC99 '.             
010800*01  -COPY WWDC99                                                         
010900     EJECT                                                                
011000*01  WS           -COPY W2216A       -PRE UTPOST-.                        
011100     EJECT                                                                
011200*01  WS           -COPY W2216B       -PRE UTPOST2-.                       
011300     EJECT                                                                
011400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
011500                                                                          
011600*01  -COPY W0005       -PRE POSTSUM-.                                     
011700     EJECT                                                                
011800*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
011900                                                                          
012000*01  -COPY WDATKORTC0  -PRE DATKORT-.                                     
012100     EJECT                                                                
012200*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
012300                                                                          
012400*01  -COPY WDATAREA.                                                      
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL ABEND                                            
012700                                                                          
012800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013000     SKIP2                                                                
013100 01  FELTEXT.                                                             
013200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013400     EJECT                                                                
013500*- - - - - - - - - - - - - - PARAMETER-AREOR TILL IMS-SUBPGM              
013600*                                                                         
013700 01  NYCKLAR-TILL-DLI.                                                    
013800     03  W-WDGXKEY-2205-X.                                                
013900         05 FILLER                 PIC  X(4)  VALUE '2205'.               
014000         05 FILLER                 PIC  X(26) VALUE LOW-VALUE.            
014100                                                                          
014200     03  W-IDDC-MIN-X.                                                    
014300         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
014400                                                                          
014500     03  W-IDDC-MAX-X.                                                    
014600         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
014700                                                                          
014800     03  W-WDGXKEY-2247-X.                                                
014900         05  W-IDHTYP            PIC X(04)    VALUE '2247'.               
015000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
015100                                                                          
015200     03  W-KY2248-X.                                                      
015300         05  W-IDDC-2248         PIC X(2)     VALUE SPACE.                
015400         05  W-IDLEVNR-2248      PIC X(5)     VALUE SPACE.                
015500         05  W-IDARTNR-2248      PIC S9(9)    VALUE ZERO COMP-3.          
015600                                                                          
015700     03  W-KY2248-MIN-X.                                                  
015800         05  W-IDDC-2248-MIN     PIC X(2)     VALUE SPACE.                
015900         05  W-IDLEVNR-2248-MIN  PIC X(5)     VALUE SPACE.                
016000         05  W-IDARTNR-2248-MIN  PIC S9(9)    VALUE ZERO COMP-3.          
016100                                                                          
016200     03  W-KY2248-MAX-X.                                                  
016300         05  W-IDDC-2248-MAX     PIC X(2)     VALUE SPACE.                
016400         05  W-IDLEVNR-2248-MAX  PIC X(5)     VALUE SPACE.                
016500         05  W-IDARTNR-2248-MAX  PIC S9(9)                                
016600                                         VALUE +999999999 COMP-3.         
016700     EJECT                                                                
016800     SKIP2                                                                
016900*    --- STATUS-KOD FRÅN IMS                                              
017000 01  STATUS-WS                   PIC XX.                                  
017100     88  SEGMENT-FINNS                       VALUE '  '.                  
017200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
017400                                                   'GB'.                  
017500     SKIP2                                                                
017600 01  GODK-STATUSKODER.                                                    
017700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017800     SKIP3                                                                
017900 01  SSA1                        PIC X(128).                              
018000 01  SSA2                        PIC X(128).                              
018100     EJECT                                                                
018200*    --- IMS FUNKTIONSKODER                                               
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500******************************************************************        
018600*          DLI INPUT - OUTPUT AREA                                        
018700******************************************************************        
018800*                                                                         
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR2'.                        
019000 01  DLI-IO-WDR201.                                                       
019100*    03  -COPY WDGX01                                                     
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
019500 01  DLI-IO-WDGX2206.                                                     
019600*    03  -COPY  WDGX2206                                                  
019700                                                                          
019800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501'.                      
019900 01  DLI-IO-WDR501.                                                       
020000*    03  -COPY WDGX01                                                     
020100                                                                          
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
020300 01  DLI-IO-WDGX2248.                                                     
020400*    03  -COPY  WDGX2248                                                  
020500                                                                          
020600     EJECT                                                                
020700 LINKAGE SECTION.                                                         
020800     SKIP3                                                                
020900*01  -COPY  W0009  -PRE MSG-                                              
021000     SKIP3                                                                
021100*01  -COPY W0008  -PRE  WDR2-                                             
021200     05  FILLER                  PIC X.                                   
021300*01  -COPY W0008  -PRE  WDR5-                                             
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION USING  MSG-PCB WDR2-PCB WDR5-PCB.                     
021700     ENTRY 'DLITCBL' USING MSG-PCB WDR2-PCB WDR5-PCB.                     
021800                                                                          
021900 STYR SECTION.                                                            
022000     SKIP3                                                                
022100     PERFORM A-INIT                                                       
022200                                                                          
022300     PERFORM S01-LAS-PARMIN                                               
022400                                                                          
022500     PERFORM IMS-GU-WDR201                                                
022600                                                                          
022700     MOVE PARM-IDDC-FOM TO W-IDDC-MIN                                     
022800     MOVE PARM-IDDC-TOM TO W-IDDC-MAX                                     
022900     PERFORM IMS-GHNP-WDGX2206                                            
023000                                                                          
023100     PERFORM UNTIL SEGMENT-SAKNAS                                         
023900                                                                          
024000        MOVE JA          TO SW-SKRIV-UT-W2216A                            
024100        MOVE 2206-IDDC   TO WS-IDDC                                       
024200        IF 2206-KDVECKOSL = 'V'                                           
024210       AND 2206-FLLEVVB   = 'J'                                           
024300*         OM DET ÄR TORSDAG OCH LEVERANTÖREN HAR FAST TORSDAGS-           
024400*         SÄNDNING (KDVECKOSL).                                           
024503           IF DATKORT-D-DAGNR = +4                                        
024510             IF (NDC-CN AND DAGENS-DAGNR = 4)                             
024520             OR (NDC-US AND DAGENS-DAGNR = 5)                             
024900                MOVE NEJ TO SW-SKRIV-UT-W2216A                            
025202             END-IF                                                       
025203           END-IF                                                         
025204           PERFORM D-LAES-WDGX2248-SKRIV-UTPOST                           
025300        ELSE                                                              
025400          IF 2206-KDVECKOSL = 'D'                                         
025500*            LEVERANTÖREN HAR FAST DAGLIG SÄNDNING (KDVECKOSL)            
025510*            CN - VECKOBATCH MÅNDAG 16:00 - SKAPAS EJ DAGLIG FIL          
025520*            US - VECKOBATCH TISDAG 05:00 - SKAPAS EJ DAGLIG FIL          
025600                                                                          
025640            IF 2206-FLLEVVB   = 'J'                                       
025704              IF (NDC-CN AND DAGENS-DAGNR = 1)                            
025705              OR (NDC-US AND DAGENS-DAGNR = 2)                            
026000                MOVE NEJ TO SW-SKRIV-UT-W2216A                            
026110              END-IF                                                      
026120            END-IF                                                        
026300            PERFORM D-LAES-WDGX2248-SKRIV-UTPOST                          
026400          END-IF                                                          
026500        END-IF                                                            
026600                                                                          
026700        PERFORM IMS-GHNP-WDGX2206                                         
026800     END-PERFORM                                                          
026900                                                                          
027000     PERFORM Z-FINIT                                                      
027100     MOVE ZERO TO RETURN-CODE                                             
027200     GOBACK                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 A-INIT SECTION.                                                          
027600     SKIP3                                                                
027700     OPEN  INPUT PARMIN                                                   
027800     OPEN OUTPUT W2216A                                                   
027900                 W2216B                                                   
028000                                                                          
028100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
028200                                                                          
028300     ACCEPT DAGENS-DATUM FROM DATE                                        
028400                                                                          
028500*--  TAG REDA PÅ DAGENS DAGNUMMER, EJ FRÅN DATKORTET.                     
028600*--       W221D3 KÖRS PÅ MÅNDAG MED FREDAGENS DATUM I DATKORT.            
028700*--       W221D3 KÖRS PÅ TORSDAG MED ONSDAGENS DATUM I DATKORT.           
028800                                                                          
028900     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
029000     CALL WDATKONV USING DAT-KDDATFORM                                    
029100                         DAT-I-TIDATUM                                    
029200                         DAT-O-TIDATUM                                    
029300                         DAT-KDSVAR                                       
029400                                                                          
029500     MOVE DAT-TID     TO DAGENS-DAGNR                                     
029600                                                                          
029700                                                                          
029800*--- HÄMTA DAGENS-DATUM FRÅN DATKORT                                      
029900     CALL DATKORT USING PROGRAM-NAMN                                      
030000                        DATUMKORT-ID                                      
030100                        DATKORT-DATUMKORT                                 
030200                                                                          
030300     MOVE DATKORT-D-AAR       TO DAGENS-AAR                               
030400                                 DAGENS-AA                                
030500     MOVE DATKORT-D-VECKA     TO DAGENS-VV                                
030600     MOVE DATKORT-D-MAANAD    TO DAGENS-MM                                
030700     MOVE DATKORT-D-DAG       TO DAGENS-DD                                
030800     MOVE WS-DAGENS-DATUM     TO DAGENS-DATUM                             
030900                                                                          
031000     .                                                                    
031100     EJECT                                                                
031200 D-LAES-WDGX2248-SKRIV-UTPOST SECTION.                                    
031300     SKIP3                                                                
031400     MOVE 2206-IDDC             TO W-IDDC-2248                            
031500                                   W-IDDC-2248-MIN                        
031600                                   W-IDDC-2248-MAX                        
031700     MOVE 2206-IDLEVNR          TO W-IDLEVNR-2248                         
031800                                   W-IDLEVNR-2248-MIN                     
031900                                   W-IDLEVNR-2248-MAX                     
032000     PERFORM IMS-GU-WDR501                                                
032100     IF SEGMENT-FINNS                                                     
032200       MOVE NEJ TO SW-TRAEFF                                              
032300       PERFORM IMS-GNP-WDGX2248                                           
032400       PERFORM UNTIL SEGMENT-SAKNAS                                       
032900          IF 2206-KDEDI = 'O' OR 'F' OR 'E'                               
033000             IF SW-SKRIV-UT-W2216A = JA                                   
033100                MOVE 2248-IDARTNR TO UTPOST-IDARTNR                       
033200                MOVE 2248-IDLEVNR TO UTPOST-IDLEVNR                       
033300                MOVE 2248-IDDC    TO UTPOST-IDDC                          
033400                MOVE 2248-KVDAGAR TO UTPOST-KVDAGAR                       
033500                MOVE 2248-KVBEART TO UTPOST-KVBEART                       
033600                PERFORM DD-SKRIV-W2216A-UTPOST                            
033700             END-IF                                                       
033800          END-IF                                                          
033900          PERFORM DE-DELETE-WDGX2248                                      
034000          PERFORM IMS-GNP-WDGX2248                                        
034100          MOVE JA TO SW-TRAEFF                                            
034200       END-PERFORM                                                        
034300                                                                          
034400       IF SW-TRAEFF = JA                                                  
034500         ADD +1 TO 2206-IDOVERFNR                                         
034600         PERFORM IMS-REPL-WDGX2206                                        
034700       END-IF                                                             
034800     END-IF                                                               
034900                                                                          
035000     .                                                                    
035100     EJECT                                                                
035200 DE-DELETE-WDGX2248 SECTION.                                              
035300     SKIP3                                                                
035400     MOVE 2248-IDDC     TO UTPOST2-IDDC                                   
035500     MOVE 2248-IDLEVNR  TO UTPOST2-IDLEVNR                                
035600     MOVE 2248-IDARTNR  TO UTPOST2-IDARTNR                                
035700                                                                          
035800     PERFORM DEA-SKRIV-W2216B-UTPOST                                      
035900                                                                          
036000*    DELETE SKER AV KÖRNINGSTEKNISKA SKÄL I EFTERFÖLJANDE                 
036100*    PROGRAM W2216B                                                       
036200     .                                                                    
036300     SKIP3                                                                
036400 DEA-SKRIV-W2216B-UTPOST SECTION.                                         
036500     SKIP3                                                                
036600     WRITE UTPOST2 FROM UTPOST2-WS                                        
036700                                                                          
036800     MOVE 'W2216B'   TO POSTSUM-FDNAMN                                    
036900     MOVE 'W2216AD2' TO POSTSUM-DDNAMN2                                   
037000     CALL POSTSUM USING POSTSUM-PARM                                      
037100     .                                                                    
037200     EJECT                                                                
037300 DD-SKRIV-W2216A-UTPOST SECTION.                                          
037400     SKIP3                                                                
037500     WRITE UTPOST FROM UTPOST-WS                                          
037600                                                                          
037700     MOVE 'W2216A'   TO POSTSUM-FDNAMN                                    
037800     MOVE 'W2216AD1' TO POSTSUM-DDNAMN2                                   
037900     CALL POSTSUM USING POSTSUM-PARM                                      
038000     .                                                                    
038100     EJECT                                                                
038200 S01-LAS-PARMIN   SECTION.                                                
038300                                                                          
038400     READ PARMIN INTO PARM-AREA                                           
038500     .                                                                    
038600     EJECT                                                                
038700 Z-FINIT   SECTION.                                                       
038800     SKIP3                                                                
038900     CLOSE  PARMIN                                                        
039000            W2216A                                                        
039100            W2216B                                                        
039200*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
039300*                               SKRIVNA POSTER                            
039400     MOVE 'S' TO POSTSUM-OPKOD                                            
039500     CALL POSTSUM USING POSTSUM-PARM                                      
039600     .                                                                    
039700     EJECT                                                                
039800 IMS-GU-WDR201   SECTION.                                                 
039900                                                                          
040000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
040100             DELIMITED BY SIZE INTO SSA1                                  
040200     MOVE '  ' TO GODK-STATUSKODER                                        
040300     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDR201 SSA1                    
040400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
040500     PERFORM IMS-STATUSKONTROLL                                           
040600     SKIP3                                                                
040700     .                                                                    
040800 IMS-GHNP-WDGX2206 SECTION.                                               
041000                                                                          
041100     STRING 'WDGX2206(IDDC    >=' W-IDDC-MIN-X                            
041200                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
041300          DELIMITED BY SIZE INTO SSA1                                     
041400     MOVE '  GE' TO GODK-STATUSKODER                                      
041500     CALL CBLTDLI USING GHNP WDR2-PCB DLI-IO-WDGX2206 SSA1                
041600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     SKIP3                                                                
041900     .                                                                    
042000 IMS-REPL-WDGX2206 SECTION.                                               
042100                                                                          
042200     MOVE '  ' TO GODK-STATUSKODER                                        
042300     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-WDGX2206                     
042400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     EJECT                                                                
042800 IMS-GU-WDR501 SECTION.                                                   
043000                                                                          
043100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X    ')'                 
043200          DELIMITED BY SIZE INTO SSA1                                     
043300     MOVE '  GE' TO GODK-STATUSKODER                                      
043400     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501 SSA1                    
043500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP2                                                                
043900 IMS-GNP-WDGX2248 SECTION.                                                
044100                                                                          
044200     STRING 'WDGX2248(KY2248  =>' W-KY2248-MIN-X                          
044300                    '&KY2248  <=' W-KY2248-MAX-X')'                       
044400          DELIMITED BY SIZE INTO SSA1                                     
044500     MOVE '  GE' TO GODK-STATUSKODER                                      
044600     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2248 SSA1                 
044700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
044800     PERFORM IMS-STATUSKONTROLL                                           
044900     .                                                                    
045000     SKIP2                                                                
045100 IMS-STATUSKONTROLL SECTION.                                              
045200                                                                          
045300     SET STATUS-IX TO 1                                                   
045400     SEARCH GODK-STATUS                                                   
045500       AT END                                                             
045600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
045700         DISPLAY FELTEXT                                                  
045800         CALL FELLOG                                                      
045900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046000         CONTINUE                                                         
046100     END-SEARCH                                                           
046200     .                                                                    
046300*    -COPY WY2000Q1                                                       
