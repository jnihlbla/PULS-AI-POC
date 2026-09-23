000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2120600.                                                
000400 AUTHOR.         PA HELGEGREN.                                            
000500 DATE-WRITTEN.   11/08/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        BEARBETNING AV UPPDATERINGSTRANSAR FRÅN W21202                   
001100*        KONTROLL AV KDAVT                                                
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*        (DELVIS KOPIA AV W21204)                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- TRANSAKTIONSFIL                                            
002300     SELECT W21203                     ASSIGN TO W21206D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W21203                                                               
003000     RECORDING       V                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W2120301      -L.                                              
003400                                                                          
003500*01  -COPY W2120302      -L.                                              
003600                                                                          
003700*01  -COPY W2120303      -L.                                              
003800                                                                          
003900*01  -COPY W2120304      -L.                                              
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2120600'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 01  BEHANDLINGSKODER-R22.                                                
005000     03  NYTT-BEST           PIC S9(1)   VALUE +1    COMP-3.              
005100     03  BEKR-BEST           PIC S9(1)   VALUE +2    COMP-3.              
005200     03  JUSTE-UPP           PIC S9(1)   VALUE +3    COMP-3.              
005300     03  JUSTE-NED           PIC S9(1)   VALUE +4    COMP-3.              
005400     03  NYTT-ANNU           PIC S9(1)   VALUE +5    COMP-3.              
005500     03  BEKR-ANNU           PIC S9(1)   VALUE +6    COMP-3.              
005600                                                                          
005700                                                                          
005800 01  CHKP-VAR.                                                            
005900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006400     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006500     SKIP2                                                                
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900                                                                          
007000 77  W21203-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W21203                       VALUE 'J'.                   
007200                                                                          
007300 77  SEGM-SKA-FINNAS-SW          PIC X       VALUE 'J'.                   
007400     88  SEGM-SKA-FINNAS                     VALUE 'J'.                   
007500     88  SEGM-KAN-SAKNAS                     VALUE 'N'.                   
007600                                                                          
007700 77  RAETT-SEGM-SW               PIC X       VALUE 'N'.                   
007800     88  RAETT-SEGM                          VALUE 'J'.                   
007900     88  FEL-SEGM                            VALUE 'N'.                   
008000                                                                          
008100 77  RAETT-AVT-SW                PIC X       VALUE 'N'.                   
008200     88  RAETT-AVT                           VALUE 'J'.                   
008300     88  FEL-AVT                             VALUE 'N'.                   
008400                                                                          
008500 77  AVTAL-FINNS-SW              PIC X(1)    VALUE 'N'.                   
008600     88  AVTAL-FINNS                         VALUE 'J'.                   
008700     88  AVTAL-SAKNAS                        VALUE 'N'.                   
008800     EJECT                                                                
008900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 01  FILLER REDEFINES DAGENS-DATUM.                                       
009100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009400     EJECT                                                                
009500                                                                          
009600*01  -COPY WWPRODSL                                                       
009700                                                                          
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL ABEND                                            
011000*                                                                         
011100 01  RKOD                        PIC S9(4)   VALUE ZERO BINARY.           
011200     EJECT                                                                
011300 01  IN-AREA-START               PIC X(24)   VALUE                        
011400                                             'IN-AREA-START'.             
011500     SKIP2                                                                
011600 01  IN-AREA.                                                             
011700     03  IN-IDPTYP           PIC X(3).                                    
011800     03  IN-IDARTNR          PIC S9(9)  COMP-3.                           
011900     03  FILLER              PIC X(50).                                   
012000                                                                          
012100*01  FILLER -COPY W2120301  -PRE IN-  -RED  IN-AREA                       
012200                                                                          
012300*01  FILLER -COPY W2120302  -PRE IN-  -RED  IN-AREA                       
012400                                                                          
012500*01  FILLER -COPY W2120303  -PRE IN-  -RED  IN-AREA                       
012600                                                                          
012700*01  FILLER -COPY W2120304  -PRE IN-  -RED  IN-AREA                       
012800                                                                          
012900     EJECT                                                                
013000*    -- ARBETSFÄLT                                                        
013100 01  OLD-IDARTNR             PIC S9(9)  COMP-3.                           
013200 01  ANT-AVT                 PIC S9(3)  COMP-3.                           
013300 01  SPAR-KDAVT              PIC S9(1)  COMP-3.                           
013400 01  TEST-KDAVT              PIC S9(1)  COMP-3.                           
013500 01  TEST-IDAVTAL            PIC 9(13).                                   
013600 01  FILLER REDEFINES TEST-IDAVTAL.                                       
013700     03  FILLER              PIC 9.                                       
013800     03  TEST-PREFIX         PIC 9(3).                                    
013900     03  TEST-ORDERNR        PIC 9(6).                                    
014000     03  FILLER REDEFINES TEST-ORDERNR.                                   
014100         05  ORDERNR-POS1    PIC 9.                                       
014200         05  FILLER          PIC 9(5).                                    
014300     03  TEST-SUFFIX         PIC 9(3).                                    
014400                                                                          
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-IDARTNR-X.                                                     
015000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015100     03  W-IDLEVNR-X.                                                     
015200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
015300     03  W-IDBEST-X.                                                      
015400         05  W-IDBEST            PIC S9(13)  VALUE ZERO COMP-3.           
015500     03  W-IDAVTAL-X.                                                     
015600         05  W-IDAVTAL           PIC S9(13)  VALUE ZERO COMP-3.           
015700     03  W-KDERS-0-X.                                                     
015800         05    FILLER            PIC S9(3)   VALUE ZERO COMP-3.           
015900     SKIP2                                                                
016000*    --- STATUS-KOD FRÅN IMS                                              
016100 01  STATUS-WS                   PIC XX.                                  
016200     88  SEGMENT-FINNS                       VALUE '  '.                  
016300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016600     88  IMS-EJ-OK                           VALUE 'XD'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300 01  SSA3                        PIC X(64).                               
017400     EJECT                                                                
017500*    --- IMS FUNKTIONSKODER                                               
017600*01  -COPY W0003                                                          
017700     EJECT                                                                
017800*    ---  DLI INPUT-OUTPUT AREA                                           
017900                                                                          
018000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
018100 01  DLI-IO-WLARTC01.                                                     
018200*    03  -COPY WDK601  -PRE ARTC-                                         
018300     EJECT                                                                
018400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
018500 01  DLI-IO-WLARTC11.                                                     
018600*    03  -COPY WDK611  -PRE ARTC-                                         
018700     EJECT                                                                
018800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC22'.                    
018900 01  DLI-IO-WLARTC22.                                                     
019000*    03  -COPY WDK622  -PRE ARTC-                                         
019100     EJECT                                                                
019200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC23'.                    
019300 01  DLI-IO-WLARTC23.                                                     
019400*    03  -COPY WDK623  -PRE ARTC-                                         
019500                                                                          
019600     EJECT                                                                
019700 LINKAGE SECTION.                                                         
019800                                                                          
019900*01  -COPY W0009  -PRE MSG-                                               
020000     EJECT                                                                
020100*01  -COPY W0008  -PRE ARTC-                                              
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB.                              
020500 MAIN SECTION.                                                            
020600     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB.                              
020700                                                                          
020800     PERFORM A-INIT                                                       
020900     PERFORM S01-LAES-W21203                                              
021000     PERFORM UNTIL END-OF-W21203                                          
021100       IF IN-IDARTNR NOT = ARTC-ART-IDARTNR                               
021200         IF CHKP-ANT > CHKP-MAX                                           
021300           PERFORM X-TAG-CHECKPOINT                                       
021400         END-IF                                                           
021500         MOVE IN-IDARTNR TO W-IDARTNR OLD-IDARTNR                         
021600         PERFORM IMS-GET-ARTC-ART                                         
021700         MOVE ARTC-ART-KDPRODSL TO TEST-KDPRODSL                          
021800         IF ARTC-ART-IDARTNR NOT = ZERO                                   
021900            PERFORM S09-KOLL-KDAVT                                        
022000         END-IF                                                           
022100       END-IF                                                             
022200                                                                          
022300       PERFORM S01-LAES-W21203                                            
022400     END-PERFORM                                                          
022500                                                                          
022600***  IF ARTC-ART-IDARTNR NOT = ZERO                                       
022700***     PERFORM S09-KOLL-KDAVT                                            
022800***  END-IF                                                               
022900                                                                          
023000     PERFORM Z-FINIT                                                      
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 A-INIT SECTION.                                                          
023700     SKIP2                                                                
023800     PERFORM IMS-RESTART                                                  
023900                                                                          
024000     OPEN INPUT W21203                                                    
024100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024200                                                                          
024300     MOVE ZERO TO ARTC-ART-IDARTNR                                        
024400     .                                                                    
024500     EJECT                                                                
024600 Z-FINIT SECTION.                                                         
024700     SKIP2                                                                
024800     CLOSE W21203                                                         
024900                                                                          
025000     MOVE 'S' TO POSTSUM-OPKOD                                            
025100     CALL POSTSUM USING POSTSUM-PARM                                      
025200     .                                                                    
025300     EJECT                                                                
025400 S01-LAES-W21203  SECTION.                                                
025500     SKIP2                                                                
025600     READ W21203 INTO IN-AREA                                             
025700     AT END                                                               
025800        SET END-OF-W21203 TO TRUE                                         
025900                                                                          
026000     NOT AT END                                                           
026100        MOVE 'W21203' TO POSTSUM-FDNAMN                                   
026200        MOVE 'W21206D1' TO POSTSUM-DDNAMN2                                
026300        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
026400        CALL POSTSUM USING POSTSUM-PARM                                   
026500                                                                          
026600     END-READ                                                             
026700     .                                                                    
026800     EJECT                                                                
026900 S09-KOLL-KDAVT   SECTION.                                                
027000     SKIP2                                                                
027100     IF KDPRODSL-TOOLS OR KDPRODSL-EMB                                    
027200        CONTINUE                                                          
027300     ELSE                                                                 
027400        PERFORM IMS-GET-ARTC-CLAG                                         
027500        IF SEGMENT-FINNS AND ARTC-CLAG-KDAVT = ZERO                       
027600          MOVE ARTC-CLAG-KDAVT     TO SPAR-KDAVT                          
027700          MOVE ZERO                TO TEST-KDAVT                          
027800          PERFORM IMS-GNP-ARTC-AVT                                        
027900          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                    
028000             MOVE ARTC-AVT-IDAVTAL TO TEST-IDAVTAL                        
028100             IF (TEST-SUFFIX = 100 OR 094 OR 490)                         
028200                MOVE 1          TO TEST-KDAVT                             
028300             END-IF                                                       
028400             PERFORM IMS-GNP-ARTC-AVT                                     
028500          END-PERFORM                                                     
028600                                                                          
028700          IF TEST-KDAVT = 1 AND SPAR-KDAVT = ZERO                         
028800             PERFORM IMS-GET-ARTC-CLAG                                    
028900             MOVE TEST-KDAVT    TO ARTC-CLAG-KDAVT                        
029000             PERFORM IMS-REPL-ARTC-CLAG                                   
029100*TEST                                                                     
029200             DISPLAY IN-IDPTYP ' ' W-IDARTNR ' ' TEST-IDAVTAL             
029300                     ' KDAVT ' TEST-KDAVT ' 1 '                           
029400*TEST                                                                     
029500          END-IF                                                          
029600        END-IF                                                            
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 X-TAG-CHECKPOINT   SECTION.                                              
030100                                                                          
030200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
030300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
030400                                                                          
030500     PERFORM IMS-CHECKPOINT                                               
030600     MOVE ZERO TO CHKP-ANT                                                
030700                                                                          
030800* --- TRIGGA OMLÄSNING AV ARTIKEL-ROT                                     
030900     MOVE ZERO TO ARTC-ART-IDARTNR                                        
031000     .                                                                    
031100     EJECT                                                                
031200* --- IMS SEKTIONER ---                                                   
031300                                                                          
031400                                                                          
031500 IMS-GET-ARTC-ART SECTION.                                                
031600                                                                          
031700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031800             DELIMITED BY SIZE INTO SSA1                                  
031900     MOVE '  '   TO GODK-STATUSKODER                                      
032000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
032100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032400     EJECT                                                                
032500 IMS-GET-ARTC-CLAG  SECTION.                                              
032600     SKIP2                                                                
032700     MOVE   'WLARTC11*F(KDSEGKEY =1)' TO SSA1                             
032800     MOVE '  ' TO GODK-STATUSKODER                                        
032900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1                
033000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300     SKIP3                                                                
033400 IMS-REPL-ARTC-CLAG SECTION.                                              
033500                                                                          
033600     MOVE '  ' TO GODK-STATUSKODER                                        
033700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
033800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     ADD 1 TO CHKP-ANT                                                    
034100     .                                                                    
034200     EJECT                                                                
034300 IMS-GNP-ARTC-AVT    SECTION.                                             
034400     SKIP2                                                                
034500     MOVE   'WLARTC11(KDSEGKEY =1)' TO SSA1                               
034600     STRING 'WLARTC23  '                                                  
034700             DELIMITED BY SIZE    INTO SSA2                               
034800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
034900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC23 SSA1 SSA2            
035000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     .                                                                    
035300     SKIP3                                                                
035400 IMS-RESTART SECTION.                                                     
035500     SKIP2                                                                
035600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035700     MOVE '  ' TO GODK-STATUSKODER                                        
035800     CALL CBLTDLI USING XRST MSG-PCB                                      
035900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036000                        CHKP-AREA-LENGTH CHKP-AREA                        
036100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036200     PERFORM IMS-STATUSKONTROLL                                           
036300     .                                                                    
036400     EJECT                                                                
036500 IMS-CHECKPOINT SECTION.                                                  
036600     SKIP2                                                                
036700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036800     MOVE '  XD' TO GODK-STATUSKODER                                      
036900     CALL CBLTDLI USING CHKP MSG-PCB                                      
037000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037100                        CHKP-AREA-LENGTH CHKP-AREA                        
037200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037300     PERFORM IMS-STATUSKONTROLL                                           
037400                                                                          
037500     IF IMS-EJ-OK                                                         
037600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
037700       DISPLAY FELTEXT                                                    
037800       CALL FELLOG                                                        
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 IMS-STATUSKONTROLL SECTION.                                              
038300     SKIP2                                                                
038400     SET STATUS-IX TO 1                                                   
038500     SEARCH GODK-STATUS                                                   
038600       AT END                                                             
038700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038800           DELIMITED BY SIZE INTO FELTEXT                                 
038900         DISPLAY FELTEXT                                                  
039000         CALL FELLOG                                                      
039100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039200         CONTINUE                                                         
039300     END-SEARCH                                                           
039400     .                                                                    
