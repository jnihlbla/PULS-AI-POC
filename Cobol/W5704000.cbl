000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5704000.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   20120111.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKAPAR TOMFIL ENDERA OM DENNA KÖRNING SKER            
000900*        INNAN MÅNADSSKIFTE ELLER OM DEN SKER PÅ ÅRSSKIFTE.               
001000*        ANNARS LÄSER PROGRAMMET IGENOM HELA                              
001100*        BASEN WDL9 (SALDOUPPDATERINGSLOGGBASEN)                          
001200*        OCH SKAPAR EN UTFIL W57040 PÅ ALLA TRANSAR SOM                   
001300*        UPPDATERAT SALDO PÅ WDK6/K7 MELLAN MIDNATT OCH TILLS             
001400*        ALLA W5105C-SALDOPOSTER SKAPATS VID 1 - 2 TIDEN.                 
001500*        (SPARAR KLOCKSLAG FÖR DEN NYASTE POSTEN).                        
001600*                                                                         
001700*        PROGRAMMET LÄSER      WDL9                                       
001800*                                                                         
001900*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- ARTIKELSALDOFIL FRÅN WDK7                                  
002800     SELECT W5105C                     ASSIGN TO W57040D1.                
002900                                                                          
003000*          --- ALLA SALDOPOSTER FRÅN MIDNATT TILL W51054-TID.             
003100     SELECT W57040                     ASSIGN TO W57040D2.                
003200                                                                          
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600                                                                          
003700 FD  W5105C                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W5705C -PRE  IN-   -L.                                    
004200                                                                          
004300 FD  W57040                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W5705C -PRE  UT-   -L.                                    
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W5704000'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  W5105C-EOF                  PIC X       VALUE 'N'.                   
005500 77  NYMAANAD                    PIC X       VALUE 'N'.                   
005600 01  W-DAGENS-DAT                PIC 9(8).                                
005700 01  W-TOM-TIKLOCK               PIC 9(9).                                
005800 01  W-LOGG-DAREGDAT             PIC 9(8).                                
005900 01  W-LOGG-TIKLOCK              PIC 9(9).                                
005910 01  WS-DC                       PIC X(2)    VALUE SPACE.                 
005920 01  WS-KDTRADP                  PIC X(4)    VALUE SPACE.                 
006000                                                                          
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600                                                                          
006700*    --- PARAMETRAR TILL ABEND                                            
006800                                                                          
006900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007200     SKIP2                                                                
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008010*01  -COPY WWDC99                                                         
008100 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
008200                                                                          
008300*01  AREA -COPY W5705C     -PRE IN-                                       
008400     EJECT                                                                
008500 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
008600                                                                          
008700*01  AREA -COPY W5705C     -PRE UT-                                       
008800     EJECT                                                                
008900                                                                          
009000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009310 01  KEYS-FOR-DLI.                                                        
009320     03  W-IDDC-X.                                                        
009330         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
009800                                                                          
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100                                                                          
010200 01  SSA1                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL9'.                        
010900 01  DLI-IO-WDL9.                                                         
011000                                                                          
011100*      05  -COPY WDL901                                                   
011110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
011120 01  DLI-IO-WDB601.                                                       
011130*    03  -COPY WDB601                                                     
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011400                                                                          
011500     EJECT                                                                
011600*01  -COPY W0008  -PRE WDL9-                                              
011700     05  FILLER                  PIC X.                                   
011800     EJECT                                                                
011810*01  -COPY W0008  -PRE WDB6-                                              
011820     05  FILLER                  PIC X.                                   
011830     EJECT                                                                
011900 PROCEDURE DIVISION  USING WDL9-PCB WDB6-PCB.                             
012000 MAIN SECTION.                                                            
012100     ENTRY 'DLITCBL' USING WDL9-PCB WDB6-PCB.                             
012200                                                                          
012300     PERFORM A-INIT                                                       
012400     PERFORM B-LAS-IN-SENASTE-LOGGDAT                                     
012500                                                                          
012600     IF NYMAANAD = JA                                                     
012700**** DÅ HAR MÅDSSKIFTE SKETT, LÄS O SKAPA POSTER PÅ FIL W57040            
012800       PERFORM IMS-GET-WDL9                                               
012900       PERFORM UNTIL SEGMENT-SAKNAS                                       
013000         EVALUATE WDL9-SEG-NAME-FB                                        
013100           WHEN 'WDL901'                                                  
013200             PERFORM C-TESTA-SKRIV                                        
013300         END-EVALUATE                                                     
013400         PERFORM IMS-GET-WDL9                                             
013500       END-PERFORM                                                        
013600     END-IF                                                               
013700                                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     OPEN INPUT  W5105C                                                   
014700          OUTPUT W57040                                                   
014800                                                                          
014900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015000     MOVE ZERO                  TO UT-PRAVCOST                            
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 B-LAS-IN-SENASTE-LOGGDAT SECTION.                                        
015500     PERFORM S01-LAS-W5105C                                               
015600     PERFORM UNTIL W5105C-EOF = JA                                        
015700       IF IN-DAREGDAT(7:2) = 01 AND IN-DAREGDAT(5:2) > 01                 
015800** DAG = 1, DÅ HAR MÅNADSSKIFTE SKETT, DOCK EJ ÅRSSKIFTE(MÅN > 1)         
015900         MOVE IN-DAREGDAT    TO W-DAGENS-DAT                              
016000         IF IN-TIKLOCK > W-TOM-TIKLOCK                                    
016100*** LÄGG IN FILENS SENASTE TID !!!                                        
016200           MOVE IN-TIKLOCK  TO W-TOM-TIKLOCK                              
016300           MOVE JA          TO NYMAANAD                                   
016400         END-IF                                                           
016500         PERFORM S01-LAS-W5105C                                           
016600       ELSE                                                               
016700         MOVE JA            TO W5105C-EOF                                 
016800         MOVE NEJ           TO NYMAANAD                                   
016900         DISPLAY 'INGEN POST SKAPAS PÅ BACKNINGSFIL W57040 '              
017000       END-IF                                                             
017100     END-PERFORM                                                          
017200                                                                          
017300     DISPLAY 'KÖRNINGENS DATUM ' W-DAGENS-DAT                             
017400     DISPLAY 'KÖRNINGENS TID   ' W-TOM-TIKLOCK                            
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 C-TESTA-SKRIV SECTION.                                                   
017900     COMPUTE W-LOGG-DAREGDAT = 99999999  - LOGG-DAREGDAT-9KOMPL           
018000     COMPUTE W-LOGG-TIKLOCK  = 999999999 - LOGG-TIKLOCK-9KOMPL            
018100                                                                          
018110     MOVE LOGG-IDDC             TO WS-IDDC                                
018200     IF XDC-NON-VCC-OWNED  AND                                            
018210        (W-LOGG-DAREGDAT  = W-DAGENS-DAT) AND                             
018300        (W-LOGG-TIKLOCK <  W-TOM-TIKLOCK) AND                             
018400        LOGG-KVART-SALDO NOT = 0 AND                                      
018500        LOGG-DAREGDAT-LADD = 0 AND                                        
018600        LOGG-IDPGM NOT = 'W4752100'                                       
018601                                                                          
018602       PERFORM CA-GET-KDTRADP                                             
018700                                                                          
018900       MOVE LOGG-IDARTNR        TO UT-IDARTNR                             
019000       MOVE LOGG-IDDC           TO UT-IDDC                                
019100       MOVE W-LOGG-DAREGDAT     TO UT-DAREGDAT                            
019200       MOVE W-LOGG-TIKLOCK      TO UT-TIKLOCK                             
019300       MOVE ZERO                TO UT-KVAKS                               
019400                                   UT-KVAKS-PAV                           
019500                                   UT-KVEFRS                              
019600                                   UT-KVLS                                
019700       IF LOGG-IDTECKEN-KVAKS      = '+'                                  
019800         COMPUTE UT-KVAKS     = LOGG-KVART-SALDO * -1                     
019900       END-IF                                                             
020000       IF LOGG-IDTECKEN-KVAKS      = '-'                                  
020100         COMPUTE UT-KVAKS     = LOGG-KVART-SALDO                          
020200       END-IF                                                             
020300       IF LOGG-IDTECKEN-KVAKS-PAV  = '+'                                  
020400         COMPUTE UT-KVAKS-PAV = LOGG-KVART-SALDO * -1                     
020500       END-IF                                                             
020600       IF LOGG-IDTECKEN-KVAKS-PAV  = '-'                                  
020700         COMPUTE UT-KVAKS-PAV = LOGG-KVART-SALDO                          
020800       END-IF                                                             
020900       IF LOGG-IDTECKEN-KVLS       = '+'                                  
021000         COMPUTE UT-KVLS      = LOGG-KVART-SALDO * -1                     
021100       END-IF                                                             
021200       IF LOGG-IDTECKEN-KVLS       = '-'                                  
021300         COMPUTE UT-KVLS      = LOGG-KVART-SALDO                          
021400       END-IF                                                             
021500       IF LOGG-IDTECKEN-KVEFRS     = '+'                                  
021600         COMPUTE UT-KVEFRS    = LOGG-KVART-SALDO * -1                     
021700       END-IF                                                             
021800       IF LOGG-IDTECKEN-KVEFRS     = '-'                                  
021900         COMPUTE UT-KVEFRS    = LOGG-KVART-SALDO                          
022000       END-IF                                                             
022100       PERFORM S02-SKRIV-W57040                                           
022200     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 CA-GET-KDTRADP SECTION.                                                  
022502     MOVE LOGG-IDDC             TO W-IDDC                                 
022504     IF W-IDDC = WS-DC                                                    
022506       MOVE WS-KDTRADP          TO UT-KDTRADP                             
022507     ELSE                                                                 
022509       PERFORM IMS-GU-WDB601                                              
022510       IF SEGMENT-FINNS                                                   
022511         MOVE DCS-KDTRADP       TO UT-KDTRADP                             
022512                                   WS-KDTRADP                             
022514         MOVE W-IDDC            TO WS-DC                                  
022515       ELSE                                                               
022516         MOVE SPACES            TO UT-KDTRADP                             
022517       END-IF                                                             
022518     END-IF                                                               
022519     .                                                                    
022520     EJECT                                                                
022530 Z-FINIT SECTION.                                                         
022600                                                                          
022700     CLOSE W5105C                                                         
022800           W57040                                                         
022900                                                                          
023000     MOVE 'S' TO POSTSUM-OPKOD                                            
023100     CALL POSTSUM USING POSTSUM-PARM                                      
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 S01-LAS-W5105C SECTION.                                                  
023600     READ W5105C INTO IN-AREA                                             
023700     AT END                                                               
023800       MOVE JA TO W5105C-EOF                                              
023900     NOT AT END                                                           
024000       MOVE ' 11 '     TO POSTSUM-TRANSTYP                                
024100       MOVE 'W5105C'   TO POSTSUM-FDNAMN                                  
024200       MOVE 'W57040D1' TO POSTSUM-DDNAMN2                                 
024300       CALL POSTSUM USING POSTSUM-PARM                                    
024400     END-READ                                                             
024500     .                                                                    
024600     EJECT                                                                
024700                                                                          
024800 S02-SKRIV-W57040 SECTION.                                                
024900     WRITE UT-POST FROM UT-AREA                                           
025000                                                                          
025100     MOVE 'SOL '     TO POSTSUM-TRANSTYP                                  
025200     MOVE 'W57040'   TO POSTSUM-FDNAMN                                    
025300     MOVE 'W57040D2' TO POSTSUM-DDNAMN2                                   
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     .                                                                    
025600     EJECT                                                                
025700* --- IMS SEKTIONER ---                                                   
025800                                                                          
025900 IMS-GET-WDL9   SECTION.                                                  
026000                                                                          
026100     CALL CBLTDLI USING GN WDL9-PCB DLI-IO-WDL9                           
026200     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
026300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026400     PERFORM IMS-STATUSKONTROLL                                           
026500     .                                                                    
026501     EJECT                                                                
026510 IMS-GU-WDB601 SECTION.                                                   
026520                                                                          
026530     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
026540          DELIMITED BY SIZE INTO SSA1                                     
026550     MOVE '  GE' TO GODK-STATUSKODER                                      
026560     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
026570     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
026580     PERFORM IMS-STATUSKONTROLL                                           
026590     .                                                                    
026591     EJECT                                                                
026700 IMS-STATUSKONTROLL SECTION.                                              
026800     SET STATUS-IX TO 1                                                   
026900     SEARCH GODK-STATUS                                                   
027000       AT END                                                             
027100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027200           DELIMITED BY SIZE INTO FELTEXT                                 
027300         DISPLAY FELTEXT                                                  
027400         CALL FELLOG                                                      
027500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027600         CONTINUE                                                         
027700     END-SEARCH                                                           
027800     .                                                                    
