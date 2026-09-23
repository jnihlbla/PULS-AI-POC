000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2131400.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   96/10/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        LÄSER FIL MED DB-UPPDATERINGAR FRÅN W21312                       
001100*        WDK6 OCH WDF1 UPPDATERAS MED DESSA                               
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001400*        PROGRAMMET UPPDATERAR WLLEVA (WDF1)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- INFIL MED DB-UPPDATERINGAR FRÅN W213P012                   
002900     SELECT W21314                     ASSIGN TO W21314D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W21314                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W21314      -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2131400'.            
004600 01  CHKP-VAR.                                                            
004700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005200     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900                                                                          
006000 77  W21314-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W21314                       VALUE 'J'.                   
006200     EJECT                                                                
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800     SKIP3                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007900 01  IN-AREA-START               PIC X(24)   VALUE                        
008000                                             'IN-AREA-START'.             
008100     SKIP2                                                                
008200                                                                          
008300*01  AREA -COPY W21314     -PRE IN-                                       
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009100     03  W-KDSEGKEY-X.                                                    
009200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009300     03  W-IDLEVNR-X.                                                     
009400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900 01  SSA3                        PIC X(64).                               
011000     EJECT                                                                
011100*    --- IMS FUNKTIONSKODER                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500                                                                          
011600 01  FILLER         PIC X(24) VALUE 'DLI-IO-AREA    '.                    
011700                                                                          
011800 01  DLI-IO-AREA           PIC X(900).                                    
011900                                                                          
012000 01  DLI-IO-WLARTC01    REDEFINES  DLI-IO-AREA.                           
012100*    03  -COPY WDK601                                                     
012200     EJECT                                                                
012300 01  DLI-IO-WLARTC11    REDEFINES  DLI-IO-AREA.                           
012400*    03  -COPY WDK611                                                     
012500     EJECT                                                                
012600 01  DLI-IO-WLLEVA01    REDEFINES  DLI-IO-AREA.                           
012700*    03  -COPY WDF101                                                     
012800                                                                          
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100                                                                          
013200*01  -COPY W0009   -PRE MSG-                                              
013300     EJECT                                                                
013400*01  -COPY W0008  -PRE LEVA-                                              
013500     05  FILLER                  PIC X.                                   
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE ARTC-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB LEVA-PCB ARTC-PCB.                     
014100 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING MSG-PCB LEVA-PCB ARTC-PCB.                     
014300                                                                          
014400     PERFORM A-INIT                                                       
014500     PERFORM S01-LAES-W21314                                              
014600     PERFORM UNTIL END-OF-W21314                                          
014700       IF CHKP-ANT > CHKP-MAX                                             
014800         PERFORM X-TAG-CHECKPOINT                                         
014900       END-IF                                                             
015000       IF IN-ATGARD = REPL                                                
015100          IF IN-IDSEGM = 'WDF101'                                         
015200             PERFORM B-REPL-WDF101                                        
015300          END-IF                                                          
015400          IF IN-IDSEGM = 'WDK611'                                         
015500             PERFORM C-REPL-WDK611                                        
015600          END-IF                                                          
015700       END-IF                                                             
015800       IF IN-ATGARD = DLET                                                
015900          IF IN-IDSEGM = 'WDF101'                                         
016000             PERFORM D-DLET-WDF101                                        
016100          END-IF                                                          
016200       END-IF                                                             
016300       IF IN-ATGARD = ISRT                                                
016400          IF IN-IDSEGM = 'WDF101'                                         
016500             PERFORM E-ISRT-WDF101                                        
016600          END-IF                                                          
016700       END-IF                                                             
016800       PERFORM S01-LAES-W21314                                            
016900     END-PERFORM                                                          
017000                                                                          
017100     PERFORM Z-FINIT                                                      
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 A-INIT SECTION.                                                          
017800     SKIP2                                                                
017900                                                                          
018000     PERFORM IMS-RESTART                                                  
018100                                                                          
018200     OPEN INPUT W21314                                                    
018300                                                                          
018400                                                                          
018500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018600     .                                                                    
018700     EJECT                                                                
018800 B-REPL-WDF101  SECTION.                                                  
018900     SKIP2                                                                
019000     MOVE IN-IDLEVNR        TO W-IDLEVNR                                  
019100     PERFORM IMS-GET-LEVA-WDF101                                          
019200                                                                          
019300     IF IN-KDCALL = 204                                                   
019400        MOVE IN-PGTABELL    TO LEV-PGTABELL                               
019500     ELSE                                                                 
019600     MOVE IN-KDLEVTYP       TO LEV-KDLEVTYP                               
019700     MOVE IN-KDSPRAK        TO LEV-KDSPRAK                                
019800     MOVE IN-FLRSADR        TO LEV-FLRSADR                                
019900     MOVE IN-KDGK           TO LEV-KDGK                                   
020000     MOVE IN-KVDAGAR-TTC1   TO LEV-KVDAGAR-TTC1                           
020100     MOVE IN-KVDAGAR-TTC2   TO LEV-KVDAGAR-TTC2                           
020200     MOVE IN-KVVECKOR-LT    TO LEV-KVVECKOR-LT                            
020300     MOVE IN-KVVECKOR-AT    TO LEV-KVVECKOR-AT                            
020400     MOVE IN-IDLPKOLL       TO LEV-IDLPKOLL                               
020500     MOVE IN-LEVDAGTAB      TO LEV-LEVDAGTAB                              
020600     END-IF                                                               
020700                                                                          
020800     PERFORM IMS-REPL-LEVA                                                
020900                                                                          
021000     ADD +1 TO CHKP-ANT                                                   
021100     .                                                                    
021200     EJECT                                                                
021300 C-REPL-WDK611  SECTION.                                                  
021400     SKIP2                                                                
021500     MOVE IN-IDARTNR     TO W-IDARTNR                                     
021600     PERFORM IMS-GET-ARTC-CLAG                                            
021700                                                                          
021800     IF IN-KDCALL = 206                                                   
021900        MOVE IN-IDANSK   TO CLAG-IDANSK                                   
022000     ELSE                                                                 
022100       IF IN-KDCALL = 110                                                 
022600          MOVE IN-KDAVT       TO CLAG-KDAVT                               
022700          MOVE IN-KDKSP       TO CLAG-KDKSP                               
022800          MOVE IN-IDPLANGR-AG TO CLAG-IDPLANGR-AG                         
022900          MOVE IN-FLMANAT     TO CLAG-FLMANAT                             
023000          MOVE IN-FLMANLT     TO CLAG-FLMANLT                             
023100          MOVE IN-FLMANGK     TO CLAG-FLMANGK                             
023110       ELSE                                                               
023120         IF IN-KDCALL = 111                                               
023130           MOVE IN-KDGK        TO CLAG-KDGK                               
023140           MOVE IN-KVDAGAR-TT  TO CLAG-KVDAGAR-TT                         
023150           MOVE IN-KVVECKOR-LT TO CLAG-KVVECKOR-LT                        
023160           MOVE IN-KVVECKOR-AT TO CLAG-KVVECKOR-AT                        
023200         END-IF                                                           
023210       END-IF                                                             
023220     END-IF                                                               
023300                                                                          
023400     PERFORM IMS-REPL-ARTC                                                
023500                                                                          
023600     ADD +1 TO CHKP-ANT                                                   
023700     .                                                                    
023800     EJECT                                                                
023900 D-DLET-WDF101  SECTION.                                                  
024000     SKIP2                                                                
024100     MOVE IN-IDLEVNR     TO W-IDLEVNR                                     
024200     PERFORM IMS-GET-LEVA-WDF101                                          
024300                                                                          
024400     IF SEGMENT-FINNS                                                     
024500        PERFORM IMS-DLET-LEVA                                             
024600                                                                          
024700        ADD +1 TO CHKP-ANT                                                
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 E-ISRT-WDF101  SECTION.                                                  
025200     SKIP2                                                                
025300     MOVE IN-IDLEVNR        TO LEV-IDLEVNR                                
025400     MOVE IN-KDLEVTYP       TO LEV-KDLEVTYP                               
025500     MOVE IN-KDSPRAK        TO LEV-KDSPRAK                                
025600     MOVE IN-FLRSADR        TO LEV-FLRSADR                                
025700     MOVE IN-KDGK           TO LEV-KDGK                                   
025800     MOVE IN-KVDAGAR-TTC1   TO LEV-KVDAGAR-TTC1                           
025900     MOVE IN-KVDAGAR-TTC2   TO LEV-KVDAGAR-TTC2                           
026000     MOVE ZERO              TO LEV-KVDAGAR-AVIAVV                         
026100     MOVE ZERO              TO LEV-KVDAGAR-INLAVV                         
026200     MOVE ZERO              TO LEV-KVVECKOR-LVAR                          
026300     MOVE IN-KVVECKOR-LT    TO LEV-KVVECKOR-LT                            
026400     MOVE IN-KVVECKOR-AT    TO LEV-KVVECKOR-AT                            
026500     MOVE IN-IDLPKOLL       TO LEV-IDLPKOLL                               
026600     MOVE IN-PGTABELL       TO LEV-PGTABELL                               
026700     MOVE IN-LEVDAGTAB      TO LEV-LEVDAGTAB                              
026800     MOVE ZERO              TO LEV-DATUM-BORT                             
026900     MOVE SPACE             TO LEV-IDLEVNR-MOTSV                          
027000                                                                          
027100     PERFORM IMS-ISRT-LEVA-WDF101                                         
027200                                                                          
027300     ADD +1 TO CHKP-ANT                                                   
027400     .                                                                    
027500     EJECT                                                                
027600 Z-FINIT SECTION.                                                         
027700                                                                          
027800                                                                          
027900     CLOSE W21314                                                         
028000     SKIP2                                                                
028100     MOVE 'S' TO POSTSUM-OPKOD                                            
028200     CALL POSTSUM USING POSTSUM-PARM                                      
028300     .                                                                    
028400     EJECT                                                                
028500 S01-LAES-W21314  SECTION.                                                
028600     SKIP2                                                                
028700     READ W21314 INTO IN-AREA                                             
028800     AT END                                                               
028900        SET END-OF-W21314 TO TRUE                                         
029000                                                                          
029100     NOT AT END                                                           
029200        MOVE 'W21314' TO POSTSUM-FDNAMN                                   
029300        MOVE 'W21314D1' TO POSTSUM-DDNAMN2                                
029400        MOVE IN-ATGARD TO POSTSUM-TRANSTYP                                
029500        CALL POSTSUM USING POSTSUM-PARM                                   
029600                                                                          
029700***     ADD 1 TO W-W21314-KVPOST-IN                                       
029800     END-READ                                                             
029900     .                                                                    
030000     EJECT                                                                
030100 X-TAG-CHECKPOINT   SECTION.                                              
030200                                                                          
030300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
030400* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
030500     PERFORM IMS-CHECKPOINT                                               
030600     MOVE ZERO TO CHKP-ANT                                                
030700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
030800     .                                                                    
030900     EJECT                                                                
031000* --- IMS SEKTIONER ---                                                   
031100     SKIP3                                                                
031200 IMS-GET-ARTC-CLAG SECTION.                                               
031300                                                                          
031400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
031700          DELIMITED BY SIZE INTO SSA2                                     
031800     MOVE '    ' TO GODK-STATUSKODER                                      
031900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
032000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032300     SKIP3                                                                
032400 IMS-REPL-ARTC SECTION.                                                   
032500                                                                          
032600     MOVE '  ' TO GODK-STATUSKODER                                        
032700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
032800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032900     PERFORM IMS-STATUSKONTROLL                                           
033000     .                                                                    
033100     EJECT                                                                
033200 IMS-GET-LEVA-WDF101 SECTION.                                             
033300                                                                          
033400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
033500          DELIMITED BY SIZE INTO SSA1                                     
033600     MOVE '  GE' TO GODK-STATUSKODER                                      
033700     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-AREA SSA1                     
033800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     EJECT                                                                
034200 IMS-ISRT-LEVA-WDF101 SECTION.                                            
034300                                                                          
034400     STRING 'WLLEVA01   '                                                 
034500          DELIMITED BY SIZE INTO SSA1                                     
034600     MOVE '  II' TO GODK-STATUSKODER                                      
034700     CALL CBLTDLI USING ISRT LEVA-PCB DLI-IO-AREA SSA1                    
034800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
034900     PERFORM IMS-STATUSKONTROLL                                           
035000     .                                                                    
035100     SKIP3                                                                
035200 IMS-REPL-LEVA SECTION.                                                   
035300                                                                          
035400     MOVE '  ' TO GODK-STATUSKODER                                        
035500     CALL CBLTDLI USING REPL LEVA-PCB DLI-IO-AREA                         
035600     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     SKIP3                                                                
036000 IMS-DLET-LEVA SECTION.                                                   
036100                                                                          
036200     MOVE '  ' TO GODK-STATUSKODER                                        
036300     CALL CBLTDLI USING DLET LEVA-PCB DLI-IO-AREA                         
036400     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
036500     PERFORM IMS-STATUSKONTROLL                                           
036600     .                                                                    
036700     EJECT                                                                
036800 IMS-RESTART SECTION.                                                     
036900     SKIP2                                                                
037000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037100     MOVE '  ' TO GODK-STATUSKODER                                        
037200     CALL CBLTDLI USING XRST MSG-PCB                                      
037300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037400                        CHKP-AREA-LENGTH CHKP-AREA                        
037500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037600     PERFORM IMS-STATUSKONTROLL                                           
037700     .                                                                    
037800     EJECT                                                                
037900 IMS-CHECKPOINT SECTION.                                                  
038000     SKIP2                                                                
038100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038200     MOVE '  XD' TO GODK-STATUSKODER                                      
038300     CALL CBLTDLI USING CHKP MSG-PCB                                      
038400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038500                        CHKP-AREA-LENGTH CHKP-AREA                        
038600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038700     PERFORM IMS-STATUSKONTROLL                                           
038800                                                                          
038900     IF IMS-EJ-OK                                                         
039000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
039100       DISPLAY FELTEXT                                                    
039200       CALL FELLOG                                                        
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 IMS-STATUSKONTROLL SECTION.                                              
039700     SKIP2                                                                
039800     SET STATUS-IX TO 1                                                   
039900     SEARCH GODK-STATUS                                                   
040000       AT END                                                             
040100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040200           DELIMITED BY SIZE INTO FELTEXT                                 
040300         DISPLAY FELTEXT                                                  
040400         CALL FELLOG                                                      
040500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040600         CONTINUE                                                         
040700     END-SEARCH                                                           
040800     .                                                                    
