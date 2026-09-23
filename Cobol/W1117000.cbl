000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1117000.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   JUNI 1992.                                               
000600*                                                                         
000700*        REMARKS.                                                         
000800*                                                                         
000900*        FUNKTION:                                                        
001000*            -  LÄSER FIL W11123 MED HTYP 9101 SOM                        
001100*               LÄGGS UPP AV ERSÄTTNINGSSYSTEMET.                         
001200*            -  SKAPAR FIL W111   MED HÄNDELSETRANSAR 9101                
001300*               SOM SKA TAS BORT I EN SENARE BMP                          
001400*            -  SKAPAR FIL W11170.                                        
001500*                                                                         
001600*            PROGRAMMET LÄSER       WLERSA (WDD7)                         
001700*                                   WLARTC (WDK6)                         
001800*                                   WLBENA (WDD3)                         
001900*                                   WLXXID (WDG3)                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400                                                                          
002500     SELECT  W11123   ASSIGN TO W11170D1.                                 
002600     SELECT  W11170   ASSIGN TO W11170D2.                                 
002700     SELECT  W11174   ASSIGN TO W11170D3.                                 
002800                                                                          
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100                                                                          
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W11123                                                               
003500     RECORDING F                                                          
003600     BLOCK CONTAINS 0.                                                    
003700                                                                          
003800*01  POST   -COPY W11123     -PRE IN-     -L.                             
003900     EJECT                                                                
004000 FD  W11170                                                               
004100     RECORDING V                                                          
004200     BLOCK CONTAINS 0 RECORDS.                                            
004300     SKIP3                                                                
004400*01  POST   -COPY W111701A   -PRE 701-    -L                              
004500     SKIP3                                                                
004600*01  POST   -COPY W111702A   -PRE 702-    -L                              
004700     EJECT                                                                
004800 FD  W11174                                                               
004900     RECORDING F                                                          
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005200*01  POST   -COPY W11123     -PRE W11174- -L.                             
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500*    -- CHECKED BY WY2000                                                 
005600     SKIP3                                                                
005700*                                                                         
005800 77  IDPGM                  PIC X(8)   VALUE 'W1112000'.                  
005900 77  JA                     PIC X      VALUE 'J'.                         
006000 77  NEJ                    PIC X      VALUE 'N'.                         
006100 77  W-IDLOPNR              PIC 9(5)   VALUE  ZERO   COMP-3.              
006200 77  IX                     PIC S9(9)  VALUE  +1     COMP SYNC.           
006300                                                                          
006400 01  W009KSIF-FALT.                                                       
006500     03  RESK-IDARTNR      PIC 9(9).                                      
006600     03  RESK-9-POS        PIC 9       VALUE 9.                           
006700     03  RESK-REKSIFFR     PIC 9.                                         
006800                                                                          
006900 01  KORNINGSDATUM           PIC 9(6).                                    
007000                                                                          
007100 01  FILLER     REDEFINES KORNINGSDATUM.                                  
007200     03  AA                  PIC 99.                                      
007300     03  MM                  PIC 99.                                      
007400     03  DD                  PIC 99.                                      
007500                                                                          
007600 01  SPRAK-KODER             PIC X(15)                                    
007700     VALUE  'D  E  F  GB S  '.                                            
007800                                                                          
007900 01  FILLER REDEFINES SPRAK-KODER.                                        
008000   03  IDSKYLT               PIC X(3)  OCCURS 5.                          
008100                                                                          
008200*                                                                         
008300*01  -COPY WWPRODSL                                                       
008400*                                                                         
008500 01  FILLER                PIC X(8)    VALUE 'SPARAREA'.                  
008600 01  W-SP-9101.                                                           
008700     03 W-SP-IDARTNR       PIC S9(9)           COMP-3.                    
008800     03 W-SP-KDERS-OLD     PIC S9(3)           COMP-3.                    
008900     03 W-SP-KDERS-NEW     PIC S9(3)           COMP-3.                    
009000     SKIP2                                                                
009100 01  ERS-SW                    PIC X(1).                                  
009200     88  ART-FINNS-ERSREG      VALUE 'J'.                                 
009300     88  ART-SAKNAS-ERSREG     VALUE 'N'.                                 
009400     SKIP2                                                                
009500 77  W11123-EOF-SW              PIC X       VALUE 'N'.                    
009600     88  END-OF-W11123                      VALUE 'J'.                    
009700     EJECT                                                                
009800 01  SUBPROGRAM.                                                          
009900     03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                  
010000     03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                  
010100     03    DATKORT         PIC X(8)    VALUE 'DATKORT '.                  
010200     03    W009KSIF        PIC X(8)    VALUE 'W009KSIF'.                  
010300     03    POSTSUM         PIC X(8)    VALUE 'POSTSUM '.                  
010400     EJECT                                                                
010500*        PARAMETRAR TILL POSTSUM                                          
010600                                                                          
010700*01         -COPY W0005     -PRE POSTSUM-                                 
010800     EJECT                                                                
010900 01  PARAM-TILL-DATUMKORT.                                                
011000     03  PROG-ID             PIC X(8)  VALUE 'W1117000'.                  
011100     03  KORT-ID             PIC X(6)  VALUE 'WDATUM'.                    
011200     SKIP3                                                                
011300*01         -COPY WDATKORT.                                               
011400     EJECT                                                                
011500 01  IN-AREA-START           PIC X(24)   VALUE 'IN-AREA START'.           
011600                                                                          
011700     SKIP2                                                                
011800*01  AREA   -COPY W11123        -PRE IN-                                  
011900     EJECT                                                                
012000 01  FILLER                  PIC X(24)   VALUE 'UT-AREA START'.           
012100 01  UT-AREA                 PIC X(155).                                  
012200     SKIP3                                                                
012300*01  AREA   -COPY W111701A      -PRE UT701- -RED UT-AREA                  
012400     EJECT                                                                
012500*01  AREA   -COPY W111702A      -PRE UT702- -RED UT-AREA                  
012600     EJECT                                                                
012700*01  AREA   -COPY W11123        -PRE W11174-                              
012920     EJECT                                                                
012930                                                                          
013000*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
013100*                                                                         
013200 01  IMS-WS.                                                              
013300     03     FILLER         PIC X(8)    VALUE 'IMS-WS  '.                  
013400*                                                                         
013500 01  NYCKLAR.                                                             
013600     03  W-IDARTNR-X.                                                     
013700       05  W-IDARTNR         PIC S9(9)     COMP-3.                        
013800     03  W-WDG3KEY-X.                                                     
013900       05  FILLER            PIC X(4)      VALUE '9101'.                  
014000       05  FILLER            PIC X(26)     VALUE LOW-VALUE.               
014100     03  W-IDSKYLT-X.                                                     
014200       05  W-IDSKYLT         PIC X(3)      VALUE  SPACE.                  
014300     EJECT                                                                
014400*                            *** STATUSKOD FRÅN IMS                       
014500     03  STATUS-WS         PIC XX.                                        
014600         88  SEGMENT-FINNS             VALUE '  '.                        
014700         88  INSERTEN-OK               VALUE '  '.                        
014800         88  SEGMENT-SAKNAS            VALUE 'GE'.                        
014900*                                                                         
015000*                            *** SEGMENTNIVÅ FRÅN IMS                     
015100     03  LEVEL-WS          PIC XX.                                        
015200         88  ROTEN-SAKNAS              VALUE '00'.                        
015300     SKIP3                                                                
015400     03    SSA1            PIC X(64).                                     
015500     03    SSA2            PIC X(64).                                     
015600     SKIP3                                                                
015700     03    GODK-STATUSKODER.                                              
015800         05    GODK-STATUS OCCURS 3  INDEXED BY STATUS-IX PIC XX.         
015900     EJECT                                                                
016000*01      -COPY W0003                                                      
016100     EJECT                                                                
016200 01  DLI-IO-AREA.                                                         
016300     03  IO-AREA     PIC X(200)  VALUE SPACE.                             
016400     SKIP3                                                                
016500    03  WLERSA01  REDEFINES IO-AREA.                                      
016600*       05        -COPY WDD701 -PRE ERSA01-                               
016700        EJECT                                                             
016800    03  WLERSA11  REDEFINES IO-AREA.                                      
016900*       05        -COPY WDD702 -PRE ERSA11-                               
017000        EJECT                                                             
017100    03  WLARTC01  REDEFINES IO-AREA.                                      
017200*       05        -COPY WDK601                                            
017300        EJECT                                                             
017400    03  WLBENA01  REDEFINES IO-AREA.                                      
017500*       05        -COPY WDD301      -PRE BENA01-                          
017600        EJECT                                                             
017700    03  WLBENA11  REDEFINES IO-AREA.                                      
017800*       05        -COPY WDD311      -PRE BENA11-                          
017900 01  DLI-IO-AREA-2.                                                       
018000     03  IO-AREA-2   PIC X(100)  VALUE SPACE.                             
018100     EJECT                                                                
018200    03  WLXXID11  REDEFINES IO-AREA-2.                                    
018300*       05        -COPY WDGX9102    -PRE 9101-                            
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600     SKIP3                                                                
018700*01      -COPY W0008     -PRE ERSA-                                       
018800         05  FILLER       PIC X.                                          
018900     EJECT                                                                
019000*01      -COPY W0008     -PRE ARTC-                                       
019100         05  FILLER       PIC X.                                          
019200     EJECT                                                                
019300*01      -COPY W0008     -PRE BENA-                                       
019400         05  FILLER       PIC X.                                          
019500     EJECT                                                                
019600*01      -COPY W0008     -PRE XXID-                                       
019700         05  FILLER       PIC X.                                          
019800     EJECT                                                                
019900 PROCEDURE DIVISION  USING  ERSA-PCB ARTC-PCB BENA-PCB XXID-PCB.          
020000     ENTRY 'DLITCBL' USING  ERSA-PCB ARTC-PCB BENA-PCB XXID-PCB.          
020100                                                                          
020200     PERFORM A-INIT                                                       
020300*                                                                         
020400*--- LÄSER IGENOM SAMTLIGA 9101-HÄNDELSER OCH SKRIVER UT                  
020500*    DEM PÅ FIL W11174 FÖR SENARE BORTTAG SAMT SKAPAR 701-                
020600*    OCH 702-POSTER                                                       
020710*                                                                         
020800     PERFORM IMS-GET-XXID01                                               
020900     PERFORM IMS-GET-XXID11                                               
021000                                                                          
021100     PERFORM UNTIL SEGMENT-SAKNAS                                         
021200                                                                          
021300        MOVE 9101-IDARTNR   TO W-SP-IDARTNR                               
021400        MOVE 9101-KDERS-OLD TO W-SP-KDERS-OLD                             
021500        MOVE 9101-KDERS-NEW TO W-SP-KDERS-NEW                             
021600        PERFORM S03-SKRIV-W11174                                          
021700                                                                          
021800        PERFORM B-SKAPA-701-POST                                          
021900        IF (W-SP-KDERS-OLD) < (W-SP-KDERS-NEW) OR                         
022000          (W-SP-KDERS-OLD > 20 AND W-SP-KDERS-NEW > 20)                   
022100          IF ART-FINNS-ERSREG                                             
022200            PERFORM C-SKAPA-702-POST                                      
022300          END-IF                                                          
022400        END-IF                                                            
022500        PERFORM IMS-GET-XXID11                                            
022600                                                                          
022700     END-PERFORM                                                          
022800*                                                                         
022900*--- LÄSER IGENOM FILEN W11123 OCH SKAPAR 701-                            
023000*    OCH 702-POSTER                                                       
023100*                                                                         
023200     PERFORM S02-LAES-W11123                                              
023300                                                                          
023400     PERFORM UNTIL END-OF-W11123                                          
023500                                                                          
023600        MOVE IN-IDARTNR TO W-SP-IDARTNR                                   
023700        MOVE IN-KDERS-OLD TO W-SP-KDERS-OLD                               
023800        MOVE IN-KDERS-NEW TO W-SP-KDERS-NEW                               
023900        PERFORM B-SKAPA-701-POST                                          
024000                                                                          
024100        IF (W-SP-KDERS-OLD) < (W-SP-KDERS-NEW) OR                         
024200          (W-SP-KDERS-OLD > 20 AND W-SP-KDERS-NEW > 20)                   
024300          IF ART-FINNS-ERSREG                                             
024400            PERFORM C-SKAPA-702-POST                                      
024500          END-IF                                                          
024600        END-IF                                                            
024700        PERFORM S02-LAES-W11123                                           
024800                                                                          
024900     END-PERFORM                                                          
025000                                                                          
025100     PERFORM Z-FINIT                                                      
025200     MOVE ZERO TO RETURN-CODE                                             
025300     GOBACK                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 A-INIT SECTION.                                                          
025700                                                                          
025800     OPEN INPUT  W11123                                                   
025900     OPEN OUTPUT W11170                                                   
026000                 W11174                                                   
026100                                                                          
026200     MOVE SPACE TO UT-AREA                                                
026300     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
026400                                                                          
026500     MOVE D-AAR         TO AA                                             
026600     MOVE D-MAANAD      TO MM                                             
026700     MOVE D-DAG         TO DD                                             
026800                                                                          
026900     MOVE ZERO TO W-IDLOPNR                                               
027324     .                                                                    
027325     EJECT                                                                
027200 B-SKAPA-701-POST SECTION.                                                
027340                                                                          
027400*****************************************************************         
027500*  9101-TRANS SKAPAS VID *                                      *         
027600*  - ERSÄTTNING UPPÅT TILL NY EK >10                            *         
027700*  - ÄT FEBR 93  VR VILL HA SAMTLIGA RIVNINGAR TILL 00          *         
027800*                OBEROENDE AV TIDIGARE EK                       *         
027900*                VIPS VILL HA RIVNINGAR > 20 TILL < 10          *         
028000*                BEHANDLAS I VIPS SOM RIVNING TILL 00           *         
028100*  - BYTE AV EK FRÅN EK > 20 TILL EK > 20                       *         
028200*  - UPPDATERING AV TILLK ARTIKLAR VID EK > 20                  *         
028300*   (UPPDAT AV TILLK ART /BYTE EK EJ TILLÅTEN VID PREL EK       *         
028400*****************************************************************         
028500                                                                          
028600     MOVE '701'             TO UT701-IDPTYP                               
028700     MOVE KORNINGSDATUM     TO UT701-TIAAMMDD                             
028800     MOVE W-SP-IDARTNR      TO UT701-IDARTNR-ERS                          
028900                               W-IDARTNR                                  
029000     MOVE W-SP-KDERS-OLD    TO UT701-KDERS-OLD                            
029100     MOVE W-SP-KDERS-NEW    TO UT701-KDERS-NEW                            
029200                                                                          
029300     PERFORM IMS-GET-ARTC01                                               
029400                                                                          
029500     IF SEGMENT-FINNS                                                     
029600        MOVE ART-REKSIFFR TO UT701-REKSIFFR-ERS                           
029700        MOVE ART-TIERSDAT TO UT701-TIERSDAT                               
029800        MOVE ART-KDPRODSL TO TEST-KDPRODSL                                
029900     ELSE                                                                 
030000        MOVE ZERO            TO UT701-REKSIFFR-ERS                        
030100                                UT701-TIERSDAT                            
030200        MOVE ZERO           TO TEST-KDPRODSL                              
030300     END-IF                                                               
030400                                                                          
030500     IF KDPRODSL-VOLVO-UTAN-EMB OR KDPRODSL-BIMA                          
030600       PERFORM IMS-GET-BENA01                                             
030700       IF SEGMENT-FINNS                                                   
030800          MOVE +1 TO IX                                                   
030900          PERFORM UNTIL IX > 5                                            
031000             MOVE IDSKYLT (IX) TO W-IDSKYLT                               
031100                                                                          
031200             PERFORM IMS-GET-BENA11                                       
031300                                                                          
031400             EVALUATE TRUE                                                
031500                WHEN IDSKYLT (IX) = 'D  '                                 
031600                   MOVE BENA11-TEXT-BEART TO UT701-BEART-TYS              
031700                WHEN IDSKYLT (IX) = 'E  '                                 
031800                   MOVE BENA11-TEXT-BEART TO UT701-BEART-SPA              
031900                WHEN IDSKYLT (IX) = 'F  '                                 
032000                   MOVE BENA11-TEXT-BEART TO UT701-BEART-FRA              
032100                WHEN IDSKYLT (IX) = 'GB '                                 
032200                   MOVE BENA11-TEXT-BEART TO UT701-BEART-ENG              
032300                WHEN IDSKYLT (IX) = 'S  '                                 
032400                   MOVE BENA11-TEXT-BEART TO UT701-BEART-SVE              
032500             END-EVALUATE                                                 
032600                                                                          
032700             ADD +1 TO IX                                                 
032800                                                                          
032900          END-PERFORM                                                     
033000       END-IF                                                             
033100                                                                          
033200       PERFORM IMS-GET-ERSA01                                             
033300       IF SEGMENT-FINNS                                                   
033400          MOVE JA TO ERS-SW                                               
033500          MOVE ERSA01-DIERS-ERS TO UT701-DIERS-ERS                        
033600       ELSE                                                               
033700          MOVE NEJ TO ERS-SW                                              
033800          MOVE ZERO        TO UT701-DIERS-ERS                             
033900       END-IF                                                             
034000                                                                          
034100       ADD +1 TO W-IDLOPNR                                                
034200       MOVE W-IDLOPNR      TO UT701-IDLOPNR                               
034300                                                                          
034400       PERFORM S01-SKRIV-UTPOST                                           
034500     ELSE                                                                 
034600        MOVE NEJ TO ERS-SW                                                
034700        MOVE ZERO          TO UT701-DIERS-ERS                             
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 C-SKAPA-702-POST SECTION.                                                
035200                                                                          
035300*****************************************************************         
035400*  POSTTYP 702 SKAPAS FÖR TILLKOMMANDE ARTIKEL VID              *         
035500*  - ERSÄTTNING UPPÅT TILL NY EK >10                            *         
035600*  - BYTE AV EK FRÅN EK > 20 TILL EK > 20                       *         
035700*  - UPPDATERING AV TILLK ARTIKLAR VID EK > 20                  *         
035800*****************************************************************         
035900                                                                          
036000     PERFORM IMS-GET-ERSA11                                               
036100     PERFORM UNTIL SEGMENT-SAKNAS                                         
036200        MOVE '702'           TO UT702-IDPTYP                              
036300        MOVE KORNINGSDATUM   TO UT702-TIAAMMDD                            
036400        MOVE W-SP-IDARTNR    TO UT702-IDARTNR-ERS                         
036500        MOVE ERSA11-IDKORTNR TO UT702-IDKORTNR                            
036600        MOVE ERSA11-FLTEXT   TO UT702-FLTEXT                              
036700        IF ERSA11-FLTEXT = 'J'                                            
036800           MOVE ERSA11-BEERS TO UT702-BEERS                               
036900        ELSE                                                              
037000           MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR                         
037100                                        RESK-IDARTNR                      
037200                                        UT702-IDARTNR-TILLK               
037300           MOVE ERSA11-DIERS-TILLK   TO UT702-DIERS-TILLK                 
037400                                                                          
037500           CALL W009KSIF USING RESK-IDARTNR                               
037600                               RESK-9-POS RESK-REKSIFFR                   
037700           MOVE RESK-REKSIFFR        TO UT702-REKSIFFR-TILLK              
037800                                                                          
037900           PERFORM IMS-GET-BENA01                                         
038000           IF SEGMENT-FINNS                                               
038100              MOVE +1 TO IX                                               
038200                                                                          
038300              PERFORM UNTIL IX > 5                                        
038400                 MOVE IDSKYLT (IX) TO W-IDSKYLT                           
038500                                                                          
038600                 PERFORM IMS-GET-BENA11                                   
038700                                                                          
038800                 EVALUATE TRUE                                            
038900                    WHEN IDSKYLT (IX) = 'D  '                             
039000                       MOVE BENA11-TEXT-BEART                             
039100                                        TO UT702-BEART-TYS-TILLK          
039200                    WHEN IDSKYLT (IX) = 'E  '                             
039300                       MOVE BENA11-TEXT-BEART                             
039400                                        TO UT702-BEART-SPA-TILLK          
039500                    WHEN IDSKYLT (IX) = 'F  '                             
039600                       MOVE BENA11-TEXT-BEART                             
039700                                        TO UT702-BEART-FRA-TILLK          
039800                    WHEN IDSKYLT (IX) = 'GB '                             
039900                       MOVE BENA11-TEXT-BEART                             
040000                                        TO UT702-BEART-ENG-TILLK          
040100                    WHEN IDSKYLT (IX) = 'S  '                             
040200                       MOVE BENA11-TEXT-BEART                             
040300                                        TO UT702-BEART-SVE-TILLK          
040400                 END-EVALUATE                                             
040500                                                                          
040600                 ADD +1 TO IX                                             
040700              END-PERFORM                                                 
040800           END-IF                                                         
040900        END-IF                                                            
041000                                                                          
041100        ADD +1 TO W-IDLOPNR                                               
041200        MOVE W-IDLOPNR                  TO UT702-IDLOPNR                  
041300                                                                          
041400        PERFORM S01-SKRIV-UTPOST                                          
041500                                                                          
041600        PERFORM IMS-GET-ERSA11                                            
041700     END-PERFORM                                                          
041800     .                                                                    
041900     EJECT                                                                
042000 Z-FINIT SECTION.                                                         
042100                                                                          
042200     CLOSE W11123                                                         
042300           W11170                                                         
042400           W11174                                                         
042500                                                                          
042600     MOVE 'S' TO POSTSUM-OPKOD                                            
042700     CALL POSTSUM USING POSTSUM-PARM                                      
042800     .                                                                    
042900     EJECT                                                                
043000 S01-SKRIV-UTPOST SECTION.                                                
043100                                                                          
043200     IF UT701-IDPTYP = '701'                                              
043300        WRITE 701-POST FROM UT701-AREA                                    
043400        MOVE '701'      TO POSTSUM-TRANSTYP                               
043500     ELSE                                                                 
043600        WRITE 702-POST FROM UT702-AREA                                    
043700        MOVE '702'      TO POSTSUM-TRANSTYP                               
043800     END-IF                                                               
043900     MOVE SPACE TO UT-AREA                                                
044000                                                                          
044100     MOVE 'W11170'      TO POSTSUM-FDNAMN                                 
044200     MOVE 'W11170D2'    TO POSTSUM-DDNAMN2                                
044300     CALL POSTSUM USING POSTSUM-PARM                                      
044400     .                                                                    
044500     EJECT                                                                
044600 S02-LAES-W11123  SECTION.                                                
044700     SKIP2                                                                
044800     READ W11123 INTO IN-AREA                                             
044900     AT END                                                               
045000        SET END-OF-W11123 TO TRUE                                         
045100                                                                          
045200     NOT AT END                                                           
045300        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
045400        MOVE 'W11123'   TO POSTSUM-FDNAMN                                 
045500        MOVE 'W11170D1' TO POSTSUM-DDNAMN2                                
045600        CALL POSTSUM USING POSTSUM-PARM                                   
045700     END-READ                                                             
045800     .                                                                    
045900     EJECT                                                                
046000 S03-SKRIV-W11174 SECTION.                                                
046100                                                                          
046200     MOVE '100'          TO W11174-IDPTYP  POSTSUM-TRANSTYP               
046300     MOVE W-SP-IDARTNR   TO W11174-IDARTNR                                
046400     MOVE W-SP-KDERS-OLD TO W11174-KDERS-OLD                              
046500     MOVE W-SP-KDERS-NEW TO W11174-KDERS-NEW                              
046600     WRITE W11174-POST   FROM W11174-AREA                                 
046700                                                                          
046800     MOVE 'W11174'      TO POSTSUM-FDNAMN                                 
046900     MOVE 'W11170D3'    TO POSTSUM-DDNAMN2                                
047000     CALL POSTSUM USING POSTSUM-PARM                                      
047100     .                                                                    
047200     EJECT                                                                
047300* IMS SECTIONER                                                           
047400     SKIP3                                                                
047500 IMS-GET-ERSA01 SECTION.                                                  
047600                                                                          
047700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
047800            DELIMITED BY SIZE INTO SSA1                                   
047900     MOVE '  GE' TO GODK-STATUSKODER                                      
048000     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
048100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSKONTROLL                                           
048300     .                                                                    
048400     SKIP3                                                                
048500 IMS-GET-ERSA11 SECTION.                                                  
048600                                                                          
048700     MOVE 'WLERSA11 '  TO SSA1                                            
048800     MOVE '  GE' TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
049000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     EJECT                                                                
049400 IMS-GET-ARTC01 SECTION.                                                  
049500                                                                          
049600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
049700            DELIMITED BY SIZE INTO SSA1                                   
049800     MOVE '  GE' TO GODK-STATUSKODER                                      
049900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
050000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     EJECT                                                                
050400 IMS-GET-BENA01 SECTION.                                                  
050500                                                                          
050600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
050700            DELIMITED BY SIZE INTO SSA1                                   
050800     MOVE '  GE' TO GODK-STATUSKODER                                      
050900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
051000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     SKIP3                                                                
051400 IMS-GET-BENA11 SECTION.                                                  
051500                                                                          
051600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
051700            DELIMITED BY SIZE INTO SSA1                                   
051800     MOVE '  ' TO GODK-STATUSKODER                                        
051900     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
052000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300     EJECT                                                                
052400 IMS-GET-XXID01 SECTION.                                                  
052500                                                                          
052600     STRING 'WLXXID01(WDG3KEY  =' W-WDG3KEY-X ')'                         
052700            DELIMITED BY SIZE INTO SSA1                                   
052800     MOVE '  ' TO GODK-STATUSKODER                                        
052900     CALL CBLTDLI USING GU XXID-PCB DLI-IO-AREA-2 SSA1                    
053000     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     SKIP3                                                                
053400 IMS-GET-XXID11 SECTION.                                                  
053500                                                                          
053600     MOVE  'WLXXID11 '  TO SSA1                                           
053700     MOVE '  GE' TO GODK-STATUSKODER                                      
053800     CALL CBLTDLI USING GHNP XXID-PCB DLI-IO-AREA-2 SSA1                  
053900     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
054000     PERFORM IMS-STATUSKONTROLL                                           
054100     .                                                                    
054200     SKIP3                                                                
054300 IMS-STATUSKONTROLL SECTION.                                              
054400                                                                          
054500     SET STATUS-IX TO 1                                                   
054600     SEARCH GODK-STATUS                                                   
054700        AT END                                                            
054800           CALL FELLOG                                                    
054900        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
055000           CONTINUE                                                       
055100     END-SEARCH                                                           
055200     .                                                                    
