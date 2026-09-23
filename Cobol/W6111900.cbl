000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6111900.                                                
000400 AUTHOR.         EVA LUNDELL.                                             
000500 DATE-WRITTEN.   96/02/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        EN SB FÖR ATT TÖMMA WDR6 (HÄNDELSEBAS) PÅ LOGGAR PÅ              
001000*        DE SOM GJORT R34-TRANSAR.  PROGRAMMMET LÄSER BASEN OCH           
001100*        SKRIVER TVÅ LIKADANA FILER, EN FÖR LISTA OCH EN FÖR              
001200*        SENARE RENSNING AV BASEN.                                        
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLFILA (WDR6)                              
001500*                              WDB6                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100*  2012-07-10  E-TRACKER 8200058 MANAGEMENT SCRAPPING FOLLOW UP           
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- LOGGPOSTER FÖR RENSNINGAR AV WDR6                          
003200     SELECT W61119                     ASSIGN TO W61119D1.                
003300     SKIP2                                                                
003400*          --- FIL FÖR LISTUTSKRIFT AV LOGGAR                             
003500     SELECT W61121                     ASSIGN TO W61119D2.                
003600     SKIP2                                                                
003700*          --- FIL FÖR FÖR-EKONOMISYSTEMEN W510                           
003800     SELECT W61123                     ASSIGN TO W61119D3.                
003900     SKIP2                                                                
004000*          --- FIL FÖR SCRAPPING FOLLOW-UP W612                           
004100     SELECT W6119P                     ASSIGN TO W61119D4.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W61119                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY WDR601   -PRE  UT1-  -L.                                  
005200     SKIP3                                                                
005300 FD  W61121                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W601R34A -PRE  UT2-  -L.                                  
005800                                                                          
005900 FD  W61123                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W601R34A -PRE  UT3-  -L.                                  
006400                                                                          
006500 FD  W6119P                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  POST -COPY W41403S  -PRE  UT4-  -L.                                  
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200                                                                          
007300                                                                          
007400*    -- CHECKED BY WY2000                                                 
007500 77  IDPGM                       PIC X(8)    VALUE 'W6111900'.            
007600 77  JA                          PIC X       VALUE 'J'.                   
007700 77  NEJ                         PIC X       VALUE 'N'.                   
007800 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
007900                                                                          
008000 77  SKAPA-POST-SW               PIC X.                                   
008100     88  SKAPA-POST                          VALUE 'J'.                   
008200     EJECT                                                                
008300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800                                                                          
008900*01  -COPY W601R34A -PRE LOGG-                                            
009000     EJECT                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009800     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL WL10WBDC                                         
010100*01  -COPY WL10WBDC                                                       
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL ABEND                                            
010400                                                                          
010500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010700     SKIP2                                                                
010800 01  FELTEXT.                                                             
010900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL POSTSUM                                          
011300*                                                                         
011400*01  -COPY W0005   -PRE  POSTSUM-                                         
011500     EJECT                                                                
011600 01  UT1-AREA-START              PIC X(24)   VALUE                        
011700                                 'UT1-AREA-START  '.                      
011800     SKIP2                                                                
011900                                                                          
012000*01  AREA -COPY WDR601       -PRE UT1-                                    
012100     EJECT                                                                
012200 01  UT2-AREA-START              PIC X(24)   VALUE                        
012300                                 'UT2-AREA-START  '.                      
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W601R34A     -PRE UT2-                                    
012700     EJECT                                                                
012800 01  UT3-AREA-START              PIC X(24)   VALUE                        
012900                                 'UT3-AREA-START  '.                      
013000     SKIP2                                                                
013100                                                                          
013200*01  AREA -COPY W601R34A     -PRE UT3-                                    
013300     EJECT                                                                
013400 01  UT4-AREA-START              PIC X(24)   VALUE                        
013500                                 'UT4-AREA-START  '.                      
013600     SKIP2                                                                
013700                                                                          
013800*01  AREA -COPY W41403S      -PRE UT4-                                    
013900     EJECT                                                                
014000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014100*                                                                         
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600     03  W-KDSEGKEY-X.                                                    
014700         05  W-KDSEGKEY          PIC X(27)    VALUE SPACE.                
014800                                                                          
014900     03  W-IDDC-WDB6-X.                                                   
015000        05  W-IDDC-WDB6          PIC X(2)    VALUE SPACE.                 
015100                                                                          
015200     SKIP2                                                                
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FINNS                       VALUE '  '.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015800     SKIP2                                                                
015900 01  GODK-STATUSKODER.                                                    
016000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     SKIP3                                                                
016200 01  SSA1                        PIC X(64).                               
016300 01  SSA2                        PIC X(64).                               
016400     EJECT                                                                
016500*    --- IMS FUNKTIONSKODER                                               
016600*01  -COPY W0003                                                          
016700     EJECT                                                                
016800*    ---  DLI INPUT-OUTPUT AREA                                           
016900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017000     SKIP3                                                                
017100 01  DLI-IO-AREA.                                                         
017200     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
017300     SKIP3                                                                
017400     03  WLFILA01 REDEFINES IO-AREA.                                      
017500*        05  -COPY WDR601                                                 
017600*           07  -COPY W601R34A    -RED FIL-WDR601-DATA                    
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB611'.         
017900 01  DLI-IO-WDB601.                                                       
018000*    03  -COPY WDB601                                                     
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE FILA-                                              
018600     05  FILLER                  PIC X.                                   
018700*01  -COPY W0008  -PRE WDB6-                                              
018800     05  FILLER                  PIC X.                                   
018900     EJECT                                                                
019000 PROCEDURE DIVISION  USING FILA-PCB WDB6-PCB.                             
019100 MAIN SECTION.                                                            
019200     ENTRY 'DLITCBL' USING FILA-PCB WDB6-PCB.                             
019500                                                                          
019600     PERFORM A-INIT                                                       
019700     PERFORM IMS-GET-FILA01                                               
019800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
019900       EVALUATE FIL-CT-IDPTYP                                             
020000         WHEN 'R34'                                                       
020100             MOVE FIL-WDR601      TO UT1-AREA                             
020200             MOVE FIL-WDR601-DATA TO UT2-AREA                             
020300                                     UT3-AREA                             
020400             PERFORM S11-SKRIV-W61119                                     
020500             PERFORM S12-SKRIV-W61121                                     
020600             PERFORM S13-SKRIV-W61123                                     
020700             PERFORM S14-SKRIV-W6119P                                     
020800       END-EVALUATE                                                       
020900       PERFORM IMS-GET-FILA01                                             
021000     END-PERFORM                                                          
021100                                                                          
021200                                                                          
021300     PERFORM Z-FINIT                                                      
021400                                                                          
021500     MOVE ZERO TO RETURN-CODE                                             
021600     GOBACK                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 A-INIT SECTION.                                                          
022000                                                                          
022100     OPEN OUTPUT W61119                                                   
022200                 W61121                                                   
022300                 W61123                                                   
022400                 W6119P                                                   
022500                                                                          
022600     ACCEPT DAGENS-DATUM  FROM DATE                                       
022700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022800                                                                          
022900     MOVE SPACE TO SPAR-IDDC                                              
023000     MOVE NEJ   TO SKAPA-POST-SW                                          
023100     .                                                                    
023200     EJECT                                                                
023300 Z-FINIT SECTION.                                                         
023400     CLOSE W61119                                                         
023500           W61121                                                         
023600           W61123                                                         
023700           W6119P                                                         
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300 S11-SKRIV-W61119 SECTION.                                                
024400                                                                          
024500     WRITE UT1-POST FROM UT1-AREA                                         
024600                                                                          
024700     MOVE UT1-FIL-CT-IDPTYP TO POSTSUM-TRANSTYP                           
024800     MOVE 'W61119' TO POSTSUM-FDNAMN                                      
024900     MOVE 'W61119D1' TO POSTSUM-DDNAMN2                                   
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S12-SKRIV-W61121 SECTION.                                                
025400                                                                          
025500     WRITE UT2-POST FROM UT2-AREA                                         
025600                                                                          
025700     MOVE UT2-IDPTYP TO POSTSUM-TRANSTYP                                  
025800     MOVE 'W61121' TO POSTSUM-FDNAMN                                      
025900     MOVE 'W61119D2' TO POSTSUM-DDNAMN2                                   
026000     CALL POSTSUM USING POSTSUM-PARM                                      
026100     .                                                                    
026200     EJECT                                                                
026300 S13-SKRIV-W61123 SECTION.                                                
026400                                                                          
026500     WRITE UT3-POST FROM UT3-AREA                                         
026600                                                                          
026700     MOVE UT3-IDPTYP TO POSTSUM-TRANSTYP                                  
026800     MOVE 'W61123' TO POSTSUM-FDNAMN                                      
026900     MOVE 'W61119D3' TO POSTSUM-DDNAMN2                                   
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200     EJECT                                                                
027300 S14-SKRIV-W6119P SECTION.                                                
027400                                                                          
027500     IF IDDC NOT = SPAR-IDDC                                              
027600       MOVE IDDC          TO WBDC-IDDC                                    
027700                             W-IDDC-WDB6                                  
027800                             SPAR-IDDC                                    
027900       CALL WL10WBDC USING WBDC-AREA                                      
028000                                                                          
028100       IF WBDC-FLWEBDC = JA                                               
028200                                                                          
028300         PERFORM IMS-GU-WDB601                                            
028400         IF SEGMENT-FINNS                                                 
028500           MOVE JA  TO SKAPA-POST-SW                                      
028600         ELSE                                                             
028700           MOVE NEJ TO SKAPA-POST-SW                                      
028800         END-IF                                                           
028900       ELSE                                                               
029000        MOVE NEJ    TO SKAPA-POST-SW                                      
029100       END-IF                                                             
029200     END-IF                                                               
029300                                                                          
029400     IF SKAPA-POST                                                        
029500       IF IDKONTO = DCS-IDKONTO-MIX                                       
029600                                                                          
029700         MOVE 'SCR'         TO UT4-IDPTYP                                 
029800         MOVE 7             TO UT4-KDSORT1                                
029900         MOVE WBDC-KDMFUP TO UT4-KDMFUP                                   
030000         MOVE IDDC          TO UT4-IDDC                                   
030100         MOVE ZERO          TO UT4-IDDISTR                                
030200                             UT4-IDKUNDNR                                 
030300                             UT4-IDKUNDRF                                 
030400         MOVE IDARTNR       TO UT4-IDARTNR                                
030500         MOVE KVAVIS        TO UT4-KVBEART                                
030600                             UT4-KVBEART-Q                                
030700         MOVE ZERO          TO UT4-PRARTNTO                               
030800                             UT4-SUARTNTO                                 
030900         MOVE 'R34SCRAP' TO UT4-BERADREF                                  
031000                                                                          
031100         WRITE UT4-POST FROM UT4-AREA                                     
031200         MOVE UT4-IDPTYP TO POSTSUM-TRANSTYP                              
031300         MOVE 'W6119P' TO POSTSUM-FDNAMN                                  
031400         MOVE 'W61119D4' TO POSTSUM-DDNAMN2                               
031500         CALL POSTSUM USING POSTSUM-PARM                                  
031600       END-IF                                                             
031700     END-IF                                                               
031800                                                                          
031900     .                                                                    
032000     EJECT                                                                
032100 S99-ABEND SECTION.                                                       
032200                                                                          
032300     SKIP2                                                                
032400     MOVE 'S' TO POSTSUM-OPKOD                                            
032500     CALL POSTSUM USING POSTSUM-PARM                                      
032600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
032700     .                                                                    
032800     EJECT                                                                
032900* --- IMS SEKTIONER ---                                                   
033000     SKIP3                                                                
033100     EJECT                                                                
033200 IMS-GET-FILA01 SECTION.                                                  
033300                                                                          
033400     CALL CBLTDLI USING GN FILA-PCB DLI-IO-AREA                           
033500     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
033600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900     EJECT                                                                
034000 IMS-GU-WDB601 SECTION.                                                   
034100                                                                          
034200     STRING 'WDB601  (IDDC     =' W-IDDC-WDB6-X ')'                       
034300          DELIMITED BY SIZE INTO SSA1                                     
034400     MOVE '  GE'              TO GODK-STATUSKODER                         
034500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
034600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
034700     PERFORM IMS-STATUSKONTROLL                                           
034800     .                                                                    
034900     EJECT                                                                
035000 IMS-STATUSKONTROLL SECTION.                                              
035100                                                                          
035200     SET STATUS-IX TO 1                                                   
035300     SEARCH GODK-STATUS                                                   
035400       AT END                                                             
035500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035600           DELIMITED BY SIZE INTO FELTEXT                                 
035700         DISPLAY FELTEXT                                                  
035800         CALL FELLOG                                                      
035900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036000         CONTINUE                                                         
036100     END-SEARCH                                                           
036200     .                                                                    
