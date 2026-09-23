000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W3714Q00.                                               
000400 AUTHOR.          INGVAR SKJELBRED                                        
000500 DATE-WRITTEN.    MAJ      1999.                                          
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*       PROGRAMET LÄSER IGENOM WDM6 (HISTORIKBAS FÖR S-LAGER) OCH         
001100*       SKRIVER EN FIL MED DATA TILL LIFO SYSTEMET.                       
001200*       DETTA GÄLLER ENDAST FÖR JAPAN OCH AUSTRALIEN                      
001300*    CHANGE LOG:                                                          
001400*                                                                         
001500*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001600*      ----------------------------------------------------------         
001700*      15/04/17 - REDDY RAHUL     - CHINA EXCHANGE PHASE 2.               
001800*                                   E'TRACKER 10252358                    
001900*                                   ADD DISPLAY FOR CHINA DC'S.           
002000*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800     SELECT W3714A                     ASSIGN TO W3714QD1.                
002900     SELECT W3714B                     ASSIGN TO W3714QD2.                
003000     SELECT W3714D                     ASSIGN TO W3714QD4.                
003100     SELECT W3714E                     ASSIGN TO W3714QD5.                
003200     SELECT W3714F                     ASSIGN TO W3714QD6.                
003300     SELECT W3714G                     ASSIGN TO W3714QD7.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W3714A                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W3714A -PRE UT1-  -L.                                     
004400                                                                          
004500     EJECT                                                                
004600 FD  W3714B                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W3714B -PRE UT2-  -L.                                     
005100                                                                          
005200     EJECT                                                                
005300 FD  W3714D                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W3714D -PRE UT4-  -L.                                     
005800                                                                          
005900     EJECT                                                                
006000 FD  W3714E                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  POST -COPY W3714E -PRE UT5-  -L.                                     
006500                                                                          
006600     EJECT                                                                
006700 FD  W3714F                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  POST -COPY W3714F -PRE UT6-  -L.                                     
007200                                                                          
007300 FD  W3714G                                                               
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS  0.                                                   
007600                                                                          
007700*01  POST -COPY W3714G -PRE UT7-  -L.                                     
007800                                                                          
007900     EJECT                                                                
008000 WORKING-STORAGE SECTION.                                                 
008100                                                                          
008200                                                                          
008300*    -- CHECKED BY WY2000                                                 
008400 77   IDPGM                      PIC X(8)    VALUE 'W3714Q00'.            
008500                                                                          
008600                                                                          
008700 77  KLART-SW                    PIC X       VALUE 'N'.                   
008800     88  KLART-SLUT                          VALUE 'J'.                   
008900                                                                          
009000                                                                          
009100 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
009200 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
009300 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION2'.             
009400 01  WS-SEKTION2             PIC X(30)   VALUE SPACE.                     
009500 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
009600 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
009700 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
009800 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
009900                                                                          
010000 01  JA                          PIC X       VALUE 'J'.                   
010100 01  NEJ                         PIC X       VALUE 'N'.                   
010200 01  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
010300 01  SPAR-IDARTNR                PIC S9(9) COMP-3 VALUE +0.               
010400 01  SPAR-IDDISTR                PIC S9(5) COMP-3 VALUE +0.               
010500 01  SPAR-IDBYTRAP               PIC S9(7) COMP-3 VALUE +0.               
010600 01  SPAR-IDBYTRAP2              PIC S9(7) COMP-3 VALUE +0.               
011700 01  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
011800 01  WS-IDDC-WORKDAY             PIC X(2)    VALUE SPACE.                 
011900 01  SPAR-IDKUNDNR               PIC S9(7) COMP-3 VALUE +0.               
012000 01  SPAR-TIANKDAG               PIC 9(8)    VALUE ZERO.                  
012100 01  WS-TIANKDAG                 PIC 9(8)    VALUE ZERO.                  
012200 01  SPAR-TIREGDAT               PIC 9(8)    VALUE ZERO.                  
012300 01  SPAR-TIREGDAT-GODK          PIC 9(8)    VALUE ZERO.                  
012400 01  WS-TIREGDAT-GODK            PIC 9(8)    VALUE ZERO.                  
012500 01  SPAR-KDBYTSTA               PIC X       VALUE SPACE.                 
012600 01  SPAR-KVRETUR                PIC S9(7) COMP-3 VALUE +0.               
012700 01  SPAR-FLBYTGAR               PIC X.                                   
012800 01  SPAR-FLBYGODK               PIC X       VALUE SPACE.                 
012900 01  POST-SKRIVEN                PIC X       VALUE 'J'.                   
013000 01  WDATUM                      PIC X(6)    VALUE 'WDATUM'.              
013100 01  WS-DAGENS-DATUM             PIC 9(8)   VALUE ZERO.                   
013200 01  DAGENS-DATUM                PIC 9(8).                                
013300 01  RED-DATUM                   REDEFINES   DAGENS-DATUM.                
013400     03   DAGENS-DATUM-SEKEL     PIC 9(2).                                
013500     03   DAGENS-DATUM-AR        PIC 9(2).                                
013600     03   DAGENS-DATUM-MANAD     PIC 9(2).                                
013700     03   DAGENS-DATUM-DAG       PIC 9(2).                                
013800 01  WS-DATUM                    PIC 9(8).                                
013900 01  WS-DATUM-RED       REDEFINES WS-DATUM.                               
014000     03   DATUM-SEKEL            PIC 9(2).                                
014100     03   DATUM-AR               PIC 9(2).                                
014200     03   DATUM-MANAD            PIC 9(2).                                
014300     03   DATUM-DAG              PIC 9(2).                                
014400                                                                          
014500 01  DYNAMISKA-SUBPROGRAM.                                                
014600   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY'.             
014700   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
014800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
014900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
015000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
015100     EJECT                                                                
015200*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
015300                                                                          
015400 01  FILLER                   PIC X(16) VALUE 'DATKORT'.                  
015500 01  DATUMKORT-ID             PIC X(6)  VALUE '000001'.                   
015600*01  -COPY WDATKORT                                                       
015700     EJECT                                                                
015800*    ----  PARAMETRAR TILL WORKDAY                                        
015900                                                                          
016000*01  -COPY WORKAREA.                                                      
016100     EJECT                                                                
016200 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
016300                                                                          
016400 01  NYCKLAR-TILL-DLI.                                                    
016500     03  W-IDDC-B6-X.                                                     
016600         05 W-IDDC-B6            PIC X(2).                                
016700                                                                          
016800 01  IMS-WS.                                                              
016900                                                                          
017000   03  STATUS-WS                 PIC X(2).                                
017100      88  SEGMENT-FINNS                      VALUE '  ' 'GA'              
017200                                                   'GK'.                  
017300      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
017400      88  SEGMENT-SLUT                       VALUE 'GB'.                  
017500                                                                          
017600   03 GODK-STATUSKODER.                                                   
017700      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
017800                                                                          
017900   03 SSA1                       PIC X(64)  VALUE SPACE.                  
018000     EJECT                                                                
018100*01        -COPY W0003                                                    
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16) VALUE 'POSTSUM'.               
018400                                                                          
018500*    -COPY W0005       -PRE POSTSUM-                                      
018600     EJECT                                                                
018700 01  FILLER                    PIC X(16)  VALUE 'UT1-AREA-START'.         
018800                                                                          
018900*01  AREA  -COPY W3714A   -PRE UT1-                                       
019000     EJECT                                                                
019100                                                                          
019200 01  FILLER                    PIC X(16)  VALUE 'UT2-AREA-START'.         
019300                                                                          
019400*01  AREA  -COPY W3714B   -PRE UT2-                                       
019500     EJECT                                                                
019600                                                                          
019700 01  FILLER                    PIC X(16)  VALUE 'UT4-AREA-START'.         
019800                                                                          
019900*01  AREA  -COPY W3714D   -PRE UT4-                                       
020000     EJECT                                                                
020100                                                                          
020200 01  FILLER                    PIC X(16)  VALUE 'UT5-AREA-START'.         
020300                                                                          
020400*01  AREA  -COPY W3714E   -PRE UT5-                                       
020500     EJECT                                                                
020600                                                                          
020700                                                                          
020800 01  FILLER                    PIC X(16)  VALUE 'UT6-AREA-START'.         
020900                                                                          
021000*01  AREA  -COPY W3714F   -PRE UT6-                                       
021100     EJECT                                                                
021200                                                                          
021300 01  FILLER                    PIC X(16)  VALUE 'UT7-AREA-START'.         
021400                                                                          
021500*01  AREA  -COPY W3714G   -PRE UT7-                                       
021600     EJECT                                                                
021700                                                                          
021800 01  FILLER                      PIC X(16)  VALUE 'IO-AREA'.              
021900 01  IO-AREA.                                                             
022000   03  IO-AREA1                  PIC X(150).                              
022100                                                                          
022200*  03  ART-AREA   -COPY WDM601     -RED IO-AREA1                          
022300     EJECT                                                                
022400*  03  INL-AREA   -COPY WDM611     -RED IO-AREA1                          
022500     EJECT                                                                
022600                                                                          
022700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022800 01   DLI-IO-AREA-B601.                                                   
022900*     03  -COPY WDB601                                                    
023000                                                                          
023100 LINKAGE SECTION.                                                         
023200                                                                          
023300*01  -COPY W0008       -PRE BYTF-                                         
023400       05 FILLER                 PIC X(1).                                
023500                                                                          
023600*01  -COPY W0008       -PRE WDB6-                                         
023700       05 FILLER                 PIC X(1).                                
023800     EJECT                                                                
023900 PROCEDURE DIVISION USING BYTF-PCB WDB6-PCB.                              
024000     ENTRY 'DLITCBL' USING BYTF-PCB WDB6-PCB.                             
024100                                                                          
024200     PERFORM A-INIT                                                       
024300     PERFORM IMS-GET-WDM6                                                 
024400     IF BYTF-SEG-NAME-FB = 'WDM601'                                       
024500        MOVE ZERO             TO SPAR-IDBYTRAP2                           
024600        MOVE RAPP-IDDC        TO SPAR-IDDC                                
024700                                 WS-IDDC                                  
024800     END-IF                                                               
024900                                                                          
025000     PERFORM UNTIL SEGMENT-SLUT                                           
025100                                                                          
025200      EVALUATE BYTF-SEG-NAME-FB                                           
025300        WHEN  'WDM601'                                                    
025400            PERFORM BG-KOLLA-DC                                           
025500            MOVE RAPP-IDDC        TO SPAR-IDDC                            
025600                                     WS-IDDC                              
025700            IF SEGMENT-FINNS                                              
025800               IF WS-IDDC NOT = W-IDDC-B6                                 
025900                  MOVE WS-IDDC TO W-IDDC-B6                               
026000                  PERFORM IMS-GU-WDB601                                   
026100               END-IF                                                     
026200               IF DCS-NDC-PF                                              
026300                                                                          
026400               MOVE SPACE               TO  SPAR-FLBYGODK                 
026500                IF RAPP-FLBYGODK = 'N'                                    
026600                  MOVE 'N'              TO  SPAR-FLBYGODK                 
026700                  MOVE RAPP-FLBYTGAR    TO SPAR-FLBYTGAR                  
026800                  MOVE RAPP-IDDISTR     TO SPAR-IDDISTR                   
026900                  MOVE RAPP-IDBYTRAP    TO SPAR-IDBYTRAP                  
027000                  MOVE RAPP-IDDC        TO SPAR-IDDC                      
027100                                           WS-IDDC                        
027200                  MOVE RAPP-IDKUNDNR    TO SPAR-IDKUNDNR                  
027300                  MOVE RAPP-DAANKDAG    TO SPAR-TIANKDAG                  
027400                  MOVE RAPP-DAREGDAT    TO SPAR-TIREGDAT                  
027500                  MOVE RAPP-DAREGDAT-GODK  TO SPAR-TIREGDAT-GODK          
027600                  MOVE RAPP-KDBYTSTA-RAPP TO SPAR-KDBYTSTA                
027700                  MOVE RAPP-KVRETUR-TOT  TO SPAR-KVRETUR                  
027800                  PERFORM B-BEARBETA                                      
027900                END-IF                                                    
028000               END-IF                                                     
028100            END-IF                                                        
028200                                                                          
028300        WHEN  'WDM611'                                                    
028400                                                                          
028500               IF SPAR-FLBYGODK = 'N'                                     
028600                  PERFORM BA-BEARBETA                                     
028700               END-IF                                                     
028800                                                                          
028900       END-EVALUATE                                                       
029000                                                                          
029100       IF NOT SEGMENT-SLUT                                                
029200          PERFORM IMS-GET-WDM6                                            
029300       END-IF                                                             
029400                                                                          
029500     END-PERFORM                                                          
029600                                                                          
029700     PERFORM C-AVSLUTA                                                    
029800                                                                          
029900     PERFORM Z-FINIT                                                      
030000                                                                          
030100     MOVE ZERO TO RETURN-CODE                                             
030200                                                                          
030300     GOBACK                                                               
030400     .                                                                    
030500     SKIP3                                                                
030600 A-INIT SECTION.                                                          
030700                                                                          
030800     MOVE 'A-INIT'        TO WS-SEKTION                                   
030900     OPEN OUTPUT W3714A                                                   
031000                 W3714B                                                   
031100                 W3714D                                                   
031200                 W3714E                                                   
031300                 W3714F                                                   
031400                 W3714G                                                   
031500                                                                          
031600     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
031700                                                                          
031800     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
031900     MOVE D-AAR      TO DAGENS-DATUM-AR                                   
032000     MOVE D-MAANAD   TO DAGENS-DATUM-MANAD                                
032100     MOVE D-DAG      TO DAGENS-DATUM-DAG                                  
032200                                                                          
032300     IF DAGENS-DATUM-AR  > 60                                             
032400        MOVE 19      TO DATUM-SEKEL                                       
032500                        DAGENS-DATUM-SEKEL                                
032600     ELSE                                                                 
032700        MOVE 20      TO DATUM-SEKEL                                       
032800                        DAGENS-DATUM-SEKEL                                
032900     END-IF                                                               
033000     MOVE DAGENS-DATUM-AR     TO DATUM-AR                                 
033100     MOVE DAGENS-DATUM-MANAD  TO DATUM-MANAD                              
033200     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
033300     MOVE DAGENS-DATUM        TO WS-DAGENS-DATUM                          
033400                                                                          
033500     DISPLAY 'DAGENS-DATUM ' WS-DAGENS-DATUM                              
033600                                                                          
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 B-BEARBETA SECTION.                                                      
034100     MOVE 'B-BEARBETA'        TO WS-SEKTION                               
034200                                                                          
034300     IF SPAR-KDBYTSTA = '2'                                               
034400******************************************************************        
034500*** NÄR STATUS = 2 DÅ ÄR BYTESRAPPORTERNA PÅ VÄG               ***        
034600******************************************************************        
034700        MOVE SPAR-IDDISTR     TO UT1-IDDISTR                              
034800        MOVE SPAR-IDBYTRAP    TO UT1-IDBYTRAP                             
034900        MOVE SPAR-IDDC        TO UT1-IDDC                                 
035000        MOVE SPAR-IDKUNDNR    TO UT1-IDKUNDNR                             
035100        MOVE SPAR-KVRETUR     TO UT1-KVRETUR                              
035200        MOVE SPAR-KDBYTSTA    TO UT1-KDBYTSTA                             
035300        MOVE SPAR-TIREGDAT    TO UT1-TIREGDAT                             
035400        PERFORM S01-SKRIV-W3714A                                          
035500     ELSE                                                                 
035600        IF SPAR-KDBYTSTA = '3'                                            
035700        OR SPAR-KDBYTSTA = '4'                                            
035800        OR SPAR-KDBYTSTA = '9'                                            
035900           IF SPAR-TIANKDAG = WS-DAGENS-DATUM                             
036000******************************************************************        
036100*** UNDERLAG TILL PERIODLISTAN SOM MÄTER TIDEN DET TAR FÖR EN  ***        
036200*** BYTESRAPPORT ATT GÅ FRÅN STATUS 2 TILL STATUS 3            ***        
036300******************************************************************        
036400              MOVE SPAR-IDDISTR     TO UT5-IDDISTR                        
036500              MOVE SPAR-IDDC        TO UT5-IDDC                           
036600                                       WS-IDDC-WORKDAY                    
036700              MOVE SPAR-IDKUNDNR    TO UT5-IDKUNDNR                       
036800              MOVE SPAR-TIREGDAT    TO WORK-TIAAMMDD-FOM                  
036900              MOVE SPAR-TIANKDAG    TO WORK-TIAAMMDD-TOM                  
037000              PERFORM BD-BERAKNA-ANTAL-ARBDAGAR                           
037100              MOVE WORK-KVWORKD      TO UT5-KVARBDAG                      
037200              PERFORM S05-SKRIV-W3714E                                    
037300           END-IF                                                         
037400        END-IF                                                            
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 BA-BEARBETA SECTION.                                                     
038000     MOVE 'BA-BEARBETA'        TO WS-SEKTION                              
038100                                                                          
038200     IF SPAR-KDBYTSTA = '3'                                               
038300******************************************************************        
038400*** NÄR STATUS = 3 DÅ ÄR BYTESRAPPORTERNA MOTTAGNA             ***        
038500******************************************************************        
038600        PERFORM BB-SKAPA-FIL-STATUS-3                                     
038700     ELSE                                                                 
038800        IF SPAR-KDBYTSTA = '4'                                            
038900        OR SPAR-KDBYTSTA = '9'                                            
039000******************************************************************        
039100*** NÄR STATUS = 4 ELLER 9 DÅ ÄR BYTESRAPPORTERNA KLARA(GODKÄND)**        
039200******************************************************************        
039300           PERFORM BC-SKAPA-FIL-STATUS-4                                  
039400           IF SPAR-FLBYTGAR = 'J'                                         
039500              IF SPAR-TIREGDAT-GODK = WS-DAGENS-DATUM                     
039600                 IF OBJ-KDBYTREF = '210'                                  
039700                    PERFORM BE-SKAPA-GARANTI-RAPPORT                      
039800                 ELSE                                                     
039900                    PERFORM BF-SKAPA-GARANTI-RAPPORT                      
040000                 END-IF                                                   
040100              END-IF                                                      
040200           END-IF                                                         
040300        END-IF                                                            
040400     END-IF                                                               
040500                                                                          
040600     .                                                                    
040700     EJECT                                                                
040800                                                                          
040900 BB-SKAPA-FIL-STATUS-3 SECTION.                                           
041000     MOVE 'BB-SKAPA-FIL-STATUS-3' TO WS-SEKTION                           
041100                                                                          
041200****************************************************************          
041300*** MAASTRICHT ÄR INTE INTRESERAD AV 019 (OBJEKT SAKNAS)     ***          
041400*** ELLER 220 ( GARANTI UTAN SALDO PÅVERKAN)                 ***          
041500****************************************************************          
041600     IF OBJ-KDBYTSTA-OBJ = ' '                                            
041700        MOVE SPAR-IDDC        TO UT2-IDDC                                 
041800        MOVE SPAR-IDDISTR     TO UT2-IDDISTR                              
041900        MOVE SPAR-KDBYTSTA    TO UT2-KDBYTSTA                             
042000        MOVE SPAR-FLBYTGAR    TO UT2-FLBYTGAR                             
042100        MOVE OBJ-KVRETUR-URSP TO UT2-KVRETUR                              
042200        IF DCS-SDC AND DCS-IDLANDX2 = 'NL'                                
042300        AND (OBJ-KDBYTREF  = '220' OR                                     
042400             OBJ-KDBYTREF  = '019')                                       
042500           CONTINUE                                                       
042600        ELSE                                                              
042700           PERFORM S02-SKRIV-W3714B                                       
042800        END-IF                                                            
042900     END-IF                                                               
043000                                                                          
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400 BC-SKAPA-FIL-STATUS-4 SECTION.                                           
043500     MOVE 'BC-SKAPA-FIL-STATUS-4' TO WS-SEKTION                           
043600     MOVE 'BC-SKAPA-FIL-STATUS-4' TO WS-SEKTION2                          
043700                                                                          
043800     IF SPAR-TIREGDAT-GODK = WS-DAGENS-DATUM                              
043900******************************************************************        
044000**** BERÄKNAR ANTAL DAGAR MELLAN ANKOMSTDAGEN OCH            *****        
044100**** SLUTREGISTRERINGSDAGEN                                  *****        
044200**** DVS TIDEN DET TAR FRÅN STATUS 3 TILL STATUS 4           *****        
044300******************************************************************        
044400        IF SPAR-IDBYTRAP2 = ZERO                                          
044500******************************************************************        
044600***** SPAR-IDBYTRAPP2 ÄR LIKA MED NOLL FÖRSTA GÅNGEN *************        
044700******************************************************************        
044800           MOVE ZERO             TO UT4-KVRETUR                           
044900           MOVE SPAR-IDBYTRAP    TO SPAR-IDBYTRAP2                        
045000           MOVE SPAR-IDDC        TO UT4-IDDC                              
045100                                    WS-IDDC-WORKDAY                       
045200           MOVE SPAR-KDBYTSTA    TO UT4-KDBYTSTA                          
045300           MOVE SPAR-FLBYTGAR    TO UT4-FLBYTGAR                          
045400           MOVE SPAR-TIANKDAG    TO WS-TIANKDAG                           
045500           MOVE SPAR-TIREGDAT-GODK TO WS-TIREGDAT-GODK                    
045600           MOVE 'N'              TO POST-SKRIVEN                          
045700        END-IF                                                            
045800                                                                          
045900        IF SPAR-IDBYTRAP NOT = SPAR-IDBYTRAP2                             
046000           MOVE WS-TIANKDAG      TO WORK-TIAAMMDD-FOM                     
046100           MOVE WS-TIREGDAT-GODK TO WORK-TIAAMMDD-TOM                     
046200           PERFORM BD-BERAKNA-ANTAL-ARBDAGAR                              
046300           MOVE WORK-KVWORKD      TO UT4-KVARBDAG                         
046400           PERFORM S04-SKRIV-W3714D                                       
046500           MOVE ZERO             TO UT4-KVRETUR                           
046600           MOVE 'N'              TO POST-SKRIVEN                          
046700           MOVE  SPAR-IDBYTRAP   TO SPAR-IDBYTRAP2                        
046800           ADD  OBJ-KVRETUR-GODK TO UT4-KVRETUR                           
046900           MOVE SPAR-IDDC        TO UT4-IDDC                              
047000                                    WS-IDDC-WORKDAY                       
047100           MOVE SPAR-KDBYTSTA    TO UT4-KDBYTSTA                          
047200           MOVE SPAR-TIANKDAG    TO WS-TIANKDAG                           
047300           MOVE SPAR-TIREGDAT-GODK TO WS-TIREGDAT-GODK                    
047400        ELSE                                                              
047500           ADD  OBJ-KVRETUR-GODK TO UT4-KVRETUR                           
047600        END-IF                                                            
047700                                                                          
047800     END-IF                                                               
047900                                                                          
048000     .                                                                    
048100     EJECT                                                                
048200                                                                          
048300 BD-BERAKNA-ANTAL-ARBDAGAR SECTION.                                       
048400     MOVE 'BD-BERAKNA-ANTAL-ARBDAGAR' TO WS-SEKTION                       
048500                                                                          
048600     MOVE WS-IDDC-WORKDAY    TO WORK-IDDC                                 
048700     MOVE 001                TO WORK-KDCALL                               
048800     CALL WORKDAY  USING WORK-KDCALL                                      
048900                         WORK-DATE-AREA                                   
049000                         WORK-KDSVAR                                      
049100                                                                          
049200     IF WORK-KDSVAR-OK                                                    
049300        CONTINUE                                                          
049400     ELSE                                                                 
049500        MOVE ZERO            TO WORK-KVWORKD                              
049600        DISPLAY '*** FEL I WORKDAY, PGM W3714Q'                           
049700        DISPLAY 'DAT FOM ' WORK-TIAAMMDD-FOM                              
049800        DISPLAY 'DAT TOM ' WORK-TIAAMMDD-TOM                              
049900        DISPLAY 'DC     ' WS-IDDC-WORKDAY                                 
050000        CALL FELLOG                                                       
050100     END-IF                                                               
050200                                                                          
050300     .                                                                    
050400     EJECT                                                                
050500                                                                          
050600 BE-SKAPA-GARANTI-RAPPORT SECTION.                                        
050700     MOVE 'BE-SKAPA-GARANTI-RAPPORT' TO WS-SEKTION                        
050800                                                                          
050900     MOVE SPAR-IDDC        TO UT6-IDDC                                    
051000     MOVE SPAR-IDDISTR     TO UT6-IDDISTR                                 
051100     MOVE SPAR-IDKUNDNR    TO UT6-IDKUNDNR                                
051200     MOVE SPAR-IDBYTRAP    TO UT6-IDBYTRAP                                
051300     MOVE SPAR-KDBYTSTA    TO UT6-KDBYTSTA                                
051400     MOVE OBJ-KVRETUR-GODK TO UT6-KVRETUR                                 
051500     MOVE OBJ-IDARTNR-OBJ  TO UT6-IDARTNR-OBJ                             
051600     PERFORM S06-SKRIV-W3714F                                             
051700                                                                          
051800                                                                          
051900     .                                                                    
052000     EJECT                                                                
052100                                                                          
052200 BF-SKAPA-GARANTI-RAPPORT SECTION.                                        
052300     MOVE 'BF-SKAPA-GARANTI-RAPPORT' TO WS-SEKTION                        
052400                                                                          
052500     IF OBJ-KDBYTREF = '220'                                              
052600        MOVE SPAR-IDDC        TO UT7-IDDC                                 
052700        MOVE SPAR-IDDISTR     TO UT7-IDDISTR                              
052800        MOVE SPAR-IDKUNDNR    TO UT7-IDKUNDNR                             
052900        MOVE SPAR-IDBYTRAP    TO UT7-IDBYTRAP                             
053000        MOVE SPAR-KDBYTSTA    TO UT7-KDBYTSTA                             
053100        MOVE OBJ-KVRETUR-GODK TO UT7-KVRETUR                              
053200        PERFORM S07-SKRIV-W3714G                                          
053300     END-IF                                                               
053400                                                                          
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 BG-KOLLA-DC SECTION.                                                     
053900     MOVE 'BG-KOLLA-DC' TO WS-SEKTION                                     
054000                                                                          
054100     MOVE NEJ     TO KLART-SW                                             
054200                                                                          
054300     IF RAPP-IDDC = '61'                                                  
054400     OR RAPP-IDDC = '62'                                                  
054500        CONTINUE                                                          
054600     ELSE                                                                 
054700        PERFORM IMS-GET-WDM6                                              
054800        PERFORM UNTIL KLART-SLUT                                          
054900                   OR SEGMENT-SLUT                                        
055000                   OR SEGMENT-SAKNAS                                      
055100                   IF BYTF-SEG-NAME-FB = 'WDM601'                         
055200                   AND  (RAPP-IDDC = '61' OR RAPP-IDDC = '62')            
055300                      MOVE JA       TO KLART-SW                           
055400                   END-IF                                                 
055500                   IF KLART-SLUT                                          
055600                      CONTINUE                                            
055700                   ELSE                                                   
055800                      PERFORM IMS-GET-WDM6                                
055900                   END-IF                                                 
056000        END-PERFORM                                                       
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400                                                                          
056500 C-AVSLUTA SECTION.                                                       
056600     MOVE 'C-AVSLUTA' TO WS-SEKTION                                       
056700     MOVE 'C-AVSLUTA' TO WS-SEKTION2                                      
056800                                                                          
056900     IF POST-SKRIVEN = 'N'                                                
057000        MOVE WS-TIANKDAG      TO WORK-TIAAMMDD-FOM                        
057100        MOVE WS-TIREGDAT-GODK TO WORK-TIAAMMDD-TOM                        
057200        PERFORM BD-BERAKNA-ANTAL-ARBDAGAR                                 
057300        MOVE WORK-KVWORKD      TO UT4-KVARBDAG                            
057400        PERFORM S04-SKRIV-W3714D                                          
057500     END-IF                                                               
057600                                                                          
057700     .                                                                    
057800     EJECT                                                                
057900                                                                          
058000 Z-FINIT SECTION.                                                         
058100     MOVE 'Z-FINIT' TO WS-SEKTION                                         
058200                                                                          
058300     CLOSE W3714A                                                         
058400           W3714B                                                         
058500           W3714D                                                         
058600           W3714E                                                         
058700           W3714F                                                         
058800           W3714G                                                         
058900                                                                          
060200     MOVE 'S' TO POSTSUM-OPKOD                                            
060300     CALL POSTSUM USING POSTSUM-PARM                                      
060400     .                                                                    
060500     SKIP2                                                                
060600 S01-SKRIV-W3714A SECTION.                                                
060700     MOVE 'S01-SKRIV-W3714A' TO WS-FIL-SEKTION                            
060800     WRITE UT1-POST FROM UT1-AREA                                         
060900                                                                          
061000     MOVE 'W3714A' TO POSTSUM-FDNAMN                                      
061100     MOVE 'W3714QD1' TO POSTSUM-DDNAMN2                                   
061200     MOVE 'UT1 '       TO POSTSUM-TRANSTYP                                
061300     CALL POSTSUM USING POSTSUM-PARM                                      
061400     .                                                                    
061500     EJECT                                                                
061600 S02-SKRIV-W3714B SECTION.                                                
061700     MOVE 'S02-SKRIV-W3714B' TO WS-FIL-SEKTION                            
061800     WRITE UT2-POST FROM UT2-AREA                                         
061900                                                                          
062000     MOVE 'W3714B' TO POSTSUM-FDNAMN                                      
062100     MOVE 'W3714QD2' TO POSTSUM-DDNAMN2                                   
062200     MOVE 'UT2 '       TO POSTSUM-TRANSTYP                                
062300     CALL POSTSUM USING POSTSUM-PARM                                      
062400     .                                                                    
062500     EJECT                                                                
062600 S04-SKRIV-W3714D SECTION.                                                
062700     MOVE 'S04-SKRIV-W3714D' TO WS-FIL-SEKTION                            
062800     WRITE UT4-POST FROM UT4-AREA                                         
062900                                                                          
063000     MOVE 'W3714D' TO POSTSUM-FDNAMN                                      
063100     MOVE 'W3714QD4' TO POSTSUM-DDNAMN2                                   
063200     MOVE 'UT4 '       TO POSTSUM-TRANSTYP                                
063300     CALL POSTSUM USING POSTSUM-PARM                                      
063400     .                                                                    
063500     EJECT                                                                
063600 S05-SKRIV-W3714E SECTION.                                                
063700     MOVE 'S05-SKRIV-W3714E' TO WS-FIL-SEKTION                            
063800     WRITE UT5-POST FROM UT5-AREA                                         
063900                                                                          
064000     MOVE 'W3714E' TO POSTSUM-FDNAMN                                      
064100     MOVE 'W3714QD5' TO POSTSUM-DDNAMN2                                   
064200     MOVE 'UT5 '       TO POSTSUM-TRANSTYP                                
064300     CALL POSTSUM USING POSTSUM-PARM                                      
064400     .                                                                    
064500     EJECT                                                                
064600 S06-SKRIV-W3714F SECTION.                                                
064700     MOVE 'S06-SKRIV-W3714F' TO WS-FIL-SEKTION                            
064800     WRITE UT6-POST FROM UT6-AREA                                         
064900                                                                          
065000     MOVE 'W3714F' TO POSTSUM-FDNAMN                                      
065100     MOVE 'W3714QD6' TO POSTSUM-DDNAMN2                                   
065200     MOVE 'UT6 '       TO POSTSUM-TRANSTYP                                
065300     CALL POSTSUM USING POSTSUM-PARM                                      
065400     .                                                                    
065500     EJECT                                                                
065600 S07-SKRIV-W3714G SECTION.                                                
065700     MOVE 'S07-SKRIV-W3714G' TO WS-FIL-SEKTION                            
065800     WRITE UT7-POST FROM UT7-AREA                                         
065900                                                                          
066000     MOVE 'W3714G' TO POSTSUM-FDNAMN                                      
066100     MOVE 'W3714QD7' TO POSTSUM-DDNAMN2                                   
066200     MOVE 'UT7 '       TO POSTSUM-TRANSTYP                                
066300     CALL POSTSUM USING POSTSUM-PARM                                      
066400     .                                                                    
066500     EJECT                                                                
066600 IMS-GET-WDM6 SECTION.                                                    
066700     MOVE 'IMS-GET-WDM6' TO WS-IMS-SEKTION                                
066800                                                                          
066900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
067000     CALL CBLTDLI USING GN BYTF-PCB IO-AREA                               
067100     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
067200     PERFORM IMS-STATUSKONTROLL                                           
070700     .                                                                    
070800     SKIP2                                                                
070900 IMS-GU-WDB601    SECTION.                                                
071000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
071100          DELIMITED BY SIZE INTO SSA1                                     
071200     MOVE '  GE' TO GODK-STATUSKODER                                      
071300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
071400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     IF SEGMENT-SAKNAS                                                    
071700         MOVE SPACE TO DCS-KDDC                                           
071800     END-IF                                                               
071900     .                                                                    
072000                                                                          
072100 IMS-STATUSKONTROLL SECTION.                                              
072200                                                                          
072300     SET STATUS-IX TO 1                                                   
072400     SEARCH GODK-STATUS AT END CALL FELLOG                                
072500     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
072600     END-SEARCH                                                           
072700     .                                                                    
