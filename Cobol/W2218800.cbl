000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2218800.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   93/10/14.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER INFIL MED AKTUELLA LEVNR                                   
001010*         SAMT INFIL MED ART MED BEST.REST > 0                            
001100*        UPPDATERAR HÄNDELSEBAS XXBL (FLLEVPLP = J)                       
001200*        MED AKTUELLA ARTNR (MED BEST.REST > 0) / LEVNR                   
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLXXBL (WDR5)                              
001500*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- LEVNR                                                      
003000     SELECT W22186                     ASSIGN TO W22188D1.                
003100     SKIP2                                                                
003200*          --- ARTNR MED BESTREST > 0                                     
003300     SELECT W22187                     ASSIGN TO W22188D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W22186                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  -COPY W22186      -L.                                                
004400     SKIP3                                                                
004500 FD  W22187                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  -COPY W22187      -L.                                                
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200     SKIP2                                                                
005201                                                                          
005210*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W2218800'.            
005400 01  CHKP-VAR.                                                            
005500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006000 03  CHKP-MAX                    PIC S9(3)   VALUE +99.                   
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600                                                                          
006700 01  ARBETSAREOR.                                                         
006800     03  W-ANT-W22186-IN         PIC S9(3)   COMP-3 VALUE ZERO.           
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300                                                                          
007400 77  W22186-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W22186                       VALUE 'J'.                   
007600                                                                          
007700 77  W22187-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W22187                       VALUE 'J'.                   
007900     EJECT                                                                
008000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500     SKIP3                                                                
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL POSTSUM                                          
009400*                                                                         
009500*01  -COPY W0005   -PRE  POSTSUM-                                         
009600     EJECT                                                                
009700 01  INLEV-AREA-START            PIC X(24)   VALUE                        
009800                                             'INLEV-AREA-START'.          
009900     SKIP2                                                                
010000                                                                          
010100*01  AREA -COPY W22186     -PRE INLEV-                                    
010200     EJECT                                                                
010300 01  IN-AREA-START               PIC X(24)   VALUE                        
010400                                             'IN-AREA-START'.             
010500     SKIP2                                                                
010600                                                                          
010700*01  AREA -COPY W22187     -PRE IN-                                       
010800*                                                                         
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300     03  W-WDGXKEY-ROT-X.                                                 
011400         05  FILLER              PIC X(4)     VALUE '2217'.               
011500         05  FILLER              PIC X(1)     VALUE 'J'.                  
011600         05  FILLER              PIC X(25)    VALUE LOW-VALUE.            
011700     03  W-WDGXKEY-X.                                                     
011800         05  W-WDGXKEY-IDLEVNR   PIC X(5)   VALUE SPACE.                  
011900         05  W-WDGXKEY-IDARTNR   PIC S9(9)   VALUE ZERO  COMP-3.          
012000     03  W-IDARTNR-X.                                                     
012100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012200     03  W-KDSEGKEY-X.                                                    
012300         05  W-KDSEGKEY          PIC S9(1)   VALUE +1   COMP-3.           
012400     SKIP2                                                                
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FINNS                       VALUE '  '.                  
012800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013100     88  IMS-EJ-OK                           VALUE 'XD'.                  
013200     SKIP2                                                                
013300 01  GODK-STATUSKODER.                                                    
013400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    --- IMS FUNKTIONSKODER                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    ---  DLI INPUT-OUTPUT AREA                                           
014300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014400     SKIP3                                                                
014500 01  DLI-IO-AREA.                                                         
014600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
014700     SKIP3                                                                
014800     03  WLXXBL01 REDEFINES IO-AREA.                                      
014900*        05  -COPY WDGX2217 -PRE XXBL-                                    
015000     EJECT                                                                
015100     03  WLXXBL11 REDEFINES IO-AREA.                                      
015200*        05  -COPY WDGX2218 -PRE XXBL-                                    
015300     EJECT                                                                
015310 01  DLI-IO-AREA-01.                                                      
015320     03  IO-AREA-01              PIC X(150)  VALUE SPACE.                 
015330     SKIP3                                                                
015400     03  WLARTC01 REDEFINES IO-AREA-01.                                   
015500*        05  -COPY WDK601                                                 
015600     EJECT                                                                
015610 01  DLI-IO-AREA-11.                                                      
015620     03  IO-AREA-11              PIC X(900)  VALUE SPACE.                 
015630     SKIP3                                                                
015700     03  WLARTC11 REDEFINES IO-AREA-11.                                   
015800*        05  -COPY WDK611                                                 
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000 01  FILLER                      PIC X(16)                                
016100                             VALUE 'DLI-IO-AREA-2'.                       
016200     SKIP3                                                                
016300 01  DLI-IO-AREA-2.                                                       
016400     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
016500     EJECT                                                                
016600 LINKAGE SECTION.                                                         
016700                                                                          
016800*01  -COPY W0009   -PRE MSG-                                              
016900     EJECT                                                                
017000*01  -COPY W0008  -PRE XXBL-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300*01  -COPY W0008  -PRE ARTC-                                              
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600 PROCEDURE DIVISION  USING MSG-PCB XXBL-PCB ARTC-PCB.                     
017700     ENTRY 'DLITCBL' USING MSG-PCB XXBL-PCB ARTC-PCB.                     
017800                                                                          
017900     SKIP2                                                                
018000     PERFORM A-INIT                                                       
018100     PERFORM S01-LAES-W22186                                              
018200     PERFORM S02-LAES-W22187                                              
018210                                                                          
018300     PERFORM UNTIL END-OF-W22186 OR END-OF-W22187                         
018400       IF CHKP-ANT > CHKP-MAX                                             
018500         PERFORM X-TAG-CHECKPOINT                                         
018600       END-IF                                                             
018700       IF IN-IDLEVNR < INLEV-IDLEVNR                                      
018800          PERFORM S02-LAES-W22187                                         
018900       ELSE                                                               
019000          IF IN-IDLEVNR > INLEV-IDLEVNR                                   
019100             PERFORM S01-LAES-W22186                                      
019200          ELSE                                                            
019300*            IN-IDLEVNR = INLEV-IDLEVNR                                   
019400             MOVE IN-IDLEVNR  TO XXBL-2218-IDLEVNR                        
019500             MOVE IN-IDARTNR  TO XXBL-2218-IDARTNR                        
019600             MOVE ZERO        TO XXBL-2218-IDANSK                         
019700             PERFORM IMS-ISRT-XXBL-2218                                   
019800*                    DET ÄR OK MED STATUS II                              
019900             ADD +1           TO CHKP-ANT                                 
020000             PERFORM S02-LAES-W22187                                      
020010*************PERFORM S01-LAES-W22186                                      
020100          END-IF                                                          
020200       END-IF                                                             
020300     END-PERFORM                                                          
020400                                                                          
020500                                                                          
020600     PERFORM Z-FINIT                                                      
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300     SKIP2                                                                
021400                                                                          
021500     PERFORM IMS-RESTART                                                  
021600                                                                          
021700     OPEN INPUT W22186                                                    
021800                W22187                                                    
021900                                                                          
022000                                                                          
022100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022200     .                                                                    
022300     EJECT                                                                
022400 Z-FINIT SECTION.                                                         
022500                                                                          
022600                                                                          
022700     CLOSE W22186                                                         
022800           W22187                                                         
022900     SKIP2                                                                
023000     MOVE 'S' TO POSTSUM-OPKOD                                            
023100     CALL POSTSUM USING POSTSUM-PARM                                      
023200     .                                                                    
023300     EJECT                                                                
023400 S01-LAES-W22186  SECTION.                                                
023500     SKIP2                                                                
023510*    LÄS SORTERADE AKTUELLA LEVNR FRÅN DAGL (+ EV PERIOD) KÖRNING         
023520                                                                          
023600     READ W22186 INTO INLEV-AREA                                          
023700     AT END                                                               
023800*       MOVE '99999'      TO INLEV-IDLEVNR                                
023900        SET END-OF-W22186 TO TRUE                                         
024000                                                                          
024100     NOT AT END                                                           
024200        MOVE 'W22186'   TO POSTSUM-FDNAMN                                 
024300        MOVE 'W22188D1' TO POSTSUM-DDNAMN2                                
024400        MOVE 'LEV'      TO POSTSUM-TRANSTYP                               
024500        CALL POSTSUM USING POSTSUM-PARM                                   
024600                                                                          
024700        ADD 1 TO W-ANT-W22186-IN                                          
024800***     IF W-ANT-W22186-IN > 750                                          
024900***        DISPLAY '*** FÖR MÅNGA INPOSTER W22186, VILKET *** '           
025000***        DISPLAY '*** GER FÖR MÅNGA REPL PÅ XXBK I      *** '           
025100***        DISPLAY '*** PGM  W22181, SOM EJ HAR CHKPT     ***'            
025200***        PERFORM S99-ABEND                                              
025300***     END-IF                                                            
025400     END-READ                                                             
025500     .                                                                    
025600     EJECT                                                                
025700 S02-LAES-W22187  SECTION.                                                
025800     SKIP2                                                                
025810*    LÄS SORTERADE (LEVNR) ART MED BESTREST > 0                           
025820                                                                          
025900     READ W22187 INTO IN-AREA                                             
026000     AT END                                                               
026100*       MOVE '99999'    TO IN-IDLEVNR                                     
026200        SET END-OF-W22187 TO TRUE                                         
026300                                                                          
026400     NOT AT END                                                           
026500        MOVE 'W22187'   TO POSTSUM-FDNAMN                                 
026600        MOVE 'W22188D2' TO POSTSUM-DDNAMN2                                
026700        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
026800        CALL POSTSUM USING POSTSUM-PARM                                   
026900                                                                          
027000*       ADD 1 TO W-W22187-KVPOST-IN                                       
027100     END-READ                                                             
027200     .                                                                    
027300     EJECT                                                                
027400 S99-ABEND SECTION.                                                       
027500     SKIP2                                                                
027600     SKIP2                                                                
027700     MOVE 'S' TO POSTSUM-OPKOD                                            
027800     CALL POSTSUM USING POSTSUM-PARM                                      
027900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
028000     .                                                                    
028100     EJECT                                                                
028200 X-TAG-CHECKPOINT   SECTION.                                              
028300                                                                          
028400* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
028500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
028600     PERFORM IMS-CHECKPOINT                                               
028700     MOVE ZERO TO CHKP-ANT                                                
028800* --- LÄS OM DATABAS OM DET BEHÖVS                                        
028900     .                                                                    
029000     EJECT                                                                
029100* --- IMS SEKTIONER ---                                                   
029200                                                                          
029300*IMS-GET-XXBL-ROT SECTION.                                                
029400*    STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
029500*         DELIMITED BY SIZE INTO SSA1                                     
029600*    MOVE '  GE' TO GODK-STATUSKODER                                      
029700*    CALL CBLTDLI USING GU XXBL-PCB DLI-IO-AREA SSA1                      
029800*    MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
029900*    PERFORM IMS-STATUSKONTROLL                                           
030000*    .                                                                    
030100     SKIP3                                                                
030200*IMS-GET-XXBL-2218 SECTION.                                               
030300*    STRING 'WLXXBL11(WDGXKEY  =' W-WDGXKEY-X ')'                         
030400*         DELIMITED BY SIZE INTO SSA1                                     
030500*    MOVE '  GE' TO GODK-STATUSKODER                                      
030600*    CALL CBLTDLI USING GHNP XXBL-PCB DLI-IO-AREA SSA1                    
030700*    MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
030800*    PERFORM IMS-STATUSKONTROLL                                           
030900*    .                                                                    
031000     EJECT                                                                
031010                                                                          
031100 IMS-ISRT-XXBL-2218 SECTION.                                              
031200                                                                          
031210*    HÄNDELSEBAS XXBL (2217J) UPPDATERAS                                  
031220                                                                          
031300     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
031400          DELIMITED BY SIZE INTO SSA1                                     
031500     MOVE 'WLXXBL11 ' TO SSA2                                             
031600     MOVE '  II' TO GODK-STATUSKODER                                      
031700     CALL CBLTDLI USING ISRT XXBL-PCB DLI-IO-AREA SSA1 SSA2               
031800                                                                          
031900     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
032000     PERFORM IMS-STATUSKONTROLL                                           
032100                                                                          
032200     IF SEGMENT-FINNS-REDAN                                               
032300        MOVE 'II'       TO POSTSUM-TRANSTYP                               
032400     ELSE                                                                 
032500        MOVE 'ISRT'     TO POSTSUM-TRANSTYP                               
032600     END-IF                                                               
032700     MOVE '2218'     TO POSTSUM-FDNAMN                                    
032800     MOVE 'WDR5'     TO POSTSUM-DDNAMN2                                   
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200                                                                          
033300*IMS-GET-ARTC-ANSK SECTION.                                               
033400*    STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
033500*         DELIMITED BY SIZE INTO SSA1                                     
033600*    STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
033700*         DELIMITED BY SIZE INTO SSA2                                     
033800*    MOVE '  GE' TO GODK-STATUSKODER                                      
033900*    CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
034000*    MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
034100*    PERFORM IMS-STATUSKONTROLL                                           
034200*    .                                                                    
034300     EJECT                                                                
034400 IMS-RESTART SECTION.                                                     
034500     SKIP2                                                                
034600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034700     MOVE '  ' TO GODK-STATUSKODER                                        
034800     CALL CBLTDLI USING XRST MSG-PCB                                      
034900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035000                        CHKP-AREA-LENGTH CHKP-AREA                        
035100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035200     PERFORM IMS-STATUSKONTROLL                                           
035300     .                                                                    
035400     EJECT                                                                
035500 IMS-CHECKPOINT SECTION.                                                  
035600     SKIP2                                                                
035700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035800     MOVE '  XD' TO GODK-STATUSKODER                                      
035900     CALL CBLTDLI USING CHKP MSG-PCB                                      
036000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036100                        CHKP-AREA-LENGTH CHKP-AREA                        
036200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036300     PERFORM IMS-STATUSKONTROLL                                           
036400                                                                          
036500     IF IMS-EJ-OK                                                         
036600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
036700       DISPLAY FELTEXT                                                    
036800       CALL FELLOG                                                        
036900     END-IF                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 IMS-STATUSKONTROLL SECTION.                                              
037300     SKIP2                                                                
037400     SET STATUS-IX TO 1                                                   
037500     SEARCH GODK-STATUS                                                   
037600       AT END                                                             
037700         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
037800         DISPLAY FELTEXT                                                  
037900         CALL FELLOG                                                      
038000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038100         CONTINUE                                                         
038200     END-SEARCH                                                           
038300     .                                                                    
