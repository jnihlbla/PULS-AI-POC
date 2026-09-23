000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5551400.                                                
000300 AUTHOR.         KARL JOHAN HANSSON                                       
000400 DATE-WRITTEN.   04/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKAPAR TOMFIL ENDERA OM DENNA KÖRNING SKER            
000900*        INNAN MÅNADSSKIFTE ELLER OM DEN SKER PÅ ÅRSSKIFTE.               
001000*        ANNARS LÄSER PROGRAMMET IGENOM HELA                              
001100*        BASEN WDL9/WLLOGA (SALDOUPPDATERINGSLOGGBASEN)                   
001200*        OCH SKAPAR EN UTFIL W55514 PÅ ALLA TRANSAR SOM                   
001300*        UPPDATERAT SALDO PÅ WDK6/K7 MELLAN MIDNATT OCH TILLS             
001400*        ALLA W51054-SALDOPOSTER SKAPATS VID 1 - 2 TIDEN.                 
001500*        (SPARAR KLOCKSLAG FÖR DEN NYASTE POSTEN).                        
001600*                                                                         
001700*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
001800*                                                                         
001801*     SKAPAD FÖR ETRACKER NO 1496327, INSTALLERAD 2004-11-25              
001810*                                                                         
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ARTIKELSALDOFIL FRÅN WDK6                                  
002700     SELECT W51054                     ASSIGN TO W55514D1.                
002800                                                                          
002900*          --- ALLA SALDOPOSTER FRÅN MIDNATT TILL W51054-TID.             
003000     SELECT W55514                     ASSIGN TO W55514D2.                
003100                                                                          
003200 DATA DIVISION.                                                           
003300                                                                          
003400 FILE SECTION.                                                            
003500                                                                          
003600 FD  W51054                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W51054 -PRE  IN-   -L.                                    
004100                                                                          
004200 FD  W55514                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W51054 -PRE  UT-   -L.                                    
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W5551400'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  W51054-EOF                  PIC X       VALUE 'N'.                   
005400 77  NYMAANAD                    PIC X       VALUE 'N'.                   
005500 01  W-DAGENS-DAT                PIC 9(8).                                
005600 01  W-TOM-TIKLOCK               PIC 9(9).                                
005700 01  W-LOGG-DAREGDAT             PIC 9(8).                                
005800 01  W-LOGG-TIKLOCK              PIC 9(9).                                
005900                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300                                                                          
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007900     SKIP2                                                                
008000 01  FELTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
008900                                                                          
009000*01  AREA -COPY W51054     -PRE IN-                                       
009100     EJECT                                                                
009200 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
009300                                                                          
009400*01  AREA -COPY W51054     -PRE UT-                                       
009500     EJECT                                                                
009600                                                                          
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010400     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010500                                                                          
010600 01  GODK-STATUSKODER.                                                    
010700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800                                                                          
010900 01  SSA1                        PIC X(64).                               
011000     EJECT                                                                
011100*    --- IMS FUNKTIONSKODER                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA'.                      
011600 01  DLI-IO-WLLOGA.                                                       
011700                                                                          
011800*      05  -COPY WDL901                                                   
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200     EJECT                                                                
012300*01  -COPY W0008  -PRE LOGA-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING LOGA-PCB.                                      
012700 MAIN SECTION.                                                            
012800     ENTRY 'DLITCBL' USING LOGA-PCB.                                      
012900                                                                          
013000     PERFORM A-INIT                                                       
013100     PERFORM B-LAS-IN-SENASTE-LOGGDAT                                     
013200                                                                          
013300     IF NYMAANAD = JA                                                     
013400**** DÅ HAR MÅDSSKIFTE SKETT, LÄS O SKAPA POSTER PÅ FIL W55514            
013500       PERFORM IMS-GET-LOGA                                               
013600       PERFORM UNTIL SEGMENT-SAKNAS                                       
013700         EVALUATE LOGA-SEG-NAME-FB                                        
013800           WHEN 'WDL901'                                                  
013900             PERFORM C-TESTA-SKRIV                                        
014000         END-EVALUATE                                                     
014100         PERFORM IMS-GET-LOGA                                             
014200       END-PERFORM                                                        
014300     END-IF                                                               
014400                                                                          
014500     PERFORM Z-FINIT                                                      
014600                                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200                                                                          
015300     OPEN INPUT  W51054                                                   
015400          OUTPUT W55514                                                   
015500                                                                          
015600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700                                                                          
016100     MOVE ZERO                  TO UT-PRINK                               
016200                                   UT-PRARTSTD                            
016210     DISPLAY '*****************************************'                  
016300     .                                                                    
016400     EJECT                                                                
016500 B-LAS-IN-SENASTE-LOGGDAT SECTION.                                        
016600                                                                          
016700     PERFORM S01-LAS-W51054                                               
016800     PERFORM UNTIL W51054-EOF = JA                                        
016900       IF IN-DAREGDAT(7:2) = 01 AND IN-DAREGDAT(5:2) > 01                 
017000** DAG = 1, DÅ HAR MÅNADSSKIFTE SKETT, DOCK EJ ÅRSSKIFTE(MÅN > 1)         
017100         MOVE IN-DAREGDAT    TO W-DAGENS-DAT                              
017200         IF IN-TIKLOCK > W-TOM-TIKLOCK                                    
017300*** LÄGG IN FILENS SENASTE TID !!!                                        
017400           MOVE IN-TIKLOCK  TO W-TOM-TIKLOCK                              
017500           MOVE JA          TO NYMAANAD                                   
017600         END-IF                                                           
017700         PERFORM S01-LAS-W51054                                           
017800       ELSE                                                               
017900         MOVE JA            TO W51054-EOF                                 
018000         MOVE NEJ           TO NYMAANAD                                   
018100         DISPLAY 'INGEN POST SKAPAS PÅ BACKNINGSFIL W55514 '              
018200       END-IF                                                             
018300     END-PERFORM                                                          
018400                                                                          
018500     DISPLAY 'KÖRNINGENS DATUM ' W-DAGENS-DAT                             
018600     DISPLAY 'KÖRNINGENS TID   ' W-TOM-TIKLOCK                            
018700     .                                                                    
018800     EJECT                                                                
018900 C-TESTA-SKRIV SECTION.                                                   
019000                                                                          
019100     COMPUTE W-LOGG-DAREGDAT = 99999999  - LOGG-DAREGDAT-9KOMPL           
019200     COMPUTE W-LOGG-TIKLOCK  = 999999999 - LOGG-TIKLOCK-9KOMPL            
019300                                                                          
019400     IF (W-LOGG-DAREGDAT  = W-DAGENS-DAT) AND                             
019500        (W-LOGG-TIKLOCK <  W-TOM-TIKLOCK) AND                             
019600        LOGG-KVART-SALDO NOT = 0 AND                                      
019700        LOGG-DAREGDAT-LADD = 0 AND                                        
019800        LOGG-IDPGM NOT = 'W4752100'                                       
019900                                                                          
020000       MOVE LOGG-IDARTNR        TO UT-IDARTNR                             
020100       MOVE LOGG-IDDC           TO UT-IDDC                                
020200       MOVE W-LOGG-DAREGDAT     TO UT-DAREGDAT                            
020300       MOVE W-LOGG-TIKLOCK      TO UT-TIKLOCK                             
020400       MOVE ZERO                TO UT-KVAKS                               
020500                                   UT-KVAKS-PAV                           
020600                                   UT-KVEFRS                              
020700                                   UT-KVLS                                
020800       IF LOGG-IDTECKEN-KVAKS      = '+'                                  
020900         COMPUTE UT-KVAKS     = LOGG-KVART-SALDO * -1                     
021000       END-IF                                                             
021100       IF LOGG-IDTECKEN-KVAKS      = '-'                                  
021200         COMPUTE UT-KVAKS     = LOGG-KVART-SALDO                          
021300       END-IF                                                             
021400       IF LOGG-IDTECKEN-KVAKS-PAV  = '+'                                  
021500         COMPUTE UT-KVAKS-PAV = LOGG-KVART-SALDO * -1                     
021600       END-IF                                                             
021700       IF LOGG-IDTECKEN-KVAKS-PAV  = '-'                                  
021800         COMPUTE UT-KVAKS-PAV = LOGG-KVART-SALDO                          
021900       END-IF                                                             
022000       IF LOGG-IDTECKEN-KVLS       = '+'                                  
022100         COMPUTE UT-KVLS      = LOGG-KVART-SALDO * -1                     
022200       END-IF                                                             
022300       IF LOGG-IDTECKEN-KVLS       = '-'                                  
022400         COMPUTE UT-KVLS      = LOGG-KVART-SALDO                          
022500       END-IF                                                             
022600       IF LOGG-IDTECKEN-KVEFRS     = '+'                                  
022700         COMPUTE UT-KVEFRS    = LOGG-KVART-SALDO * -1                     
022800       END-IF                                                             
022900       IF LOGG-IDTECKEN-KVEFRS     = '-'                                  
023000         COMPUTE UT-KVEFRS    = LOGG-KVART-SALDO                          
023100       END-IF                                                             
023200       PERFORM S02-SKRIV-W55514                                           
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 Z-FINIT SECTION.                                                         
023700                                                                          
023800     CLOSE W51054                                                         
023900           W55514                                                         
024000                                                                          
024100     MOVE 'S' TO POSTSUM-OPKOD                                            
024200     CALL POSTSUM USING POSTSUM-PARM                                      
024300     .                                                                    
024400     EJECT                                                                
024500 S01-LAS-W51054 SECTION.                                                  
024600                                                                          
024700     READ W51054 INTO IN-AREA                                             
024800     AT END                                                               
024900       MOVE JA TO W51054-EOF                                              
025000     NOT AT END                                                           
025100       MOVE ' 11 '     TO POSTSUM-TRANSTYP                                
025200       MOVE 'W51054'   TO POSTSUM-FDNAMN                                  
025300       MOVE 'W55514D1' TO POSTSUM-DDNAMN2                                 
025400       CALL POSTSUM USING POSTSUM-PARM                                    
025500     END-READ                                                             
025600     .                                                                    
025700     EJECT                                                                
025800 S02-SKRIV-W55514 SECTION.                                                
025900                                                                          
026000     WRITE UT-POST FROM UT-AREA                                           
026100                                                                          
026200     MOVE 'SOL '     TO POSTSUM-TRANSTYP                                  
026300     MOVE 'W55514'   TO POSTSUM-FDNAMN                                    
026400     MOVE 'W55514D2' TO POSTSUM-DDNAMN2                                   
026500     CALL POSTSUM USING POSTSUM-PARM                                      
026600     .                                                                    
026700     EJECT                                                                
026800* --- IMS SEKTIONER ---                                                   
026900                                                                          
027000 IMS-GET-LOGA   SECTION.                                                  
027100                                                                          
027200     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA                         
027300     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
027400     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
027500     PERFORM IMS-STATUSKONTROLL                                           
027600     .                                                                    
027700                                                                          
027800 IMS-STATUSKONTROLL SECTION.                                              
027900                                                                          
028000     SET STATUS-IX TO 1                                                   
028100     SEARCH GODK-STATUS                                                   
028200       AT END                                                             
028300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
028400           DELIMITED BY SIZE INTO FELTEXT                                 
028500         DISPLAY FELTEXT                                                  
028600         CALL FELLOG                                                      
028700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028800         CONTINUE                                                         
028900     END-SEARCH                                                           
029000     .                                                                    
