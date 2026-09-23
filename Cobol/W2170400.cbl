000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2170400.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   96/10/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        LÄSER FIL MED DB-UPPDATERINGAR FRÅN W21702                       
001100*        WDK6  UPPDATERAS MED DESSA                                       
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INFIL MED DB-UPPDATERINGAR FRÅN W217P002                   
002800     SELECT W21705                     ASSIGN TO W21704D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W21705                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W21705      -L.                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W2170400'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W21705-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W21705                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     SKIP3                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600 01  IN-AREA-START               PIC X(24)   VALUE                        
007700                                             'IN-AREA-START'.             
007800     SKIP2                                                                
007900                                                                          
008000*01  AREA -COPY W21705     -PRE IN-                                       
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  NYCKLAR-TILL-DLI.                                                    
008600     03  W-IDARTNR-X.                                                     
008700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008800     03  W-KDSEGKEY-X.                                                    
008900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009000     03  W-KDNOTTYP-X.                                                    
009100         05  W-KDNOTTYP          PIC S9      VALUE ZERO COMP-3.           
009200     SKIP2                                                                
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009900     88  IMS-EJ-OK                           VALUE 'XD'.                  
010000     SKIP2                                                                
010100 01  GODK-STATUSKODER.                                                    
010200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300     SKIP3                                                                
010400 01  SSA1                        PIC X(64).                               
010500 01  SSA2                        PIC X(64).                               
010600 01  SSA3                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200                                                                          
011300 01  FILLER         PIC X(24) VALUE 'DLI-IO-AREA    '.                    
011400                                                                          
011500 01  DLI-IO-AREA           PIC X(900).                                    
011600                                                                          
011700 01  DLI-IO-WLARTC01    REDEFINES  DLI-IO-AREA.                           
011800*    03  -COPY WDK601                                                     
011900     EJECT                                                                
012000 01  DLI-IO-WLARTC11    REDEFINES  DLI-IO-AREA.                           
012100*    03  -COPY WDK611                                                     
012200     EJECT                                                                
012300 01  DLI-IO-WLARTC25    REDEFINES  DLI-IO-AREA.                           
012400*    03  -COPY WDK625                                                     
012500                                                                          
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900*01  -COPY W0009   -PRE MSG-                                              
013000     EJECT                                                                
013100*01  -COPY W0008  -PRE ARTC-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB.                              
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB.                              
013700                                                                          
013800     PERFORM A-INIT                                                       
013900     PERFORM S01-LAES-W21705                                              
014000     PERFORM UNTIL END-OF-W21705                                          
014100       IF CHKP-ANT > CHKP-MAX                                             
014200         PERFORM X-TAG-CHECKPOINT                                         
014300       END-IF                                                             
014400       IF IN-ATGARD = REPL                                                
014500          IF IN-IDSEGM = 'WDK611'                                         
014600             PERFORM B-REPL-WDK611                                        
014700          END-IF                                                          
014800          IF IN-IDSEGM = 'WDK625'                                         
014900             PERFORM C-REPL-WDK625                                        
015000          END-IF                                                          
015100       END-IF                                                             
015200       IF IN-ATGARD = DLET                                                
015300          IF IN-IDSEGM = 'WDK625'                                         
015400             PERFORM D-DLET-WDK625                                        
015500          END-IF                                                          
015600       END-IF                                                             
015700       IF IN-ATGARD = ISRT                                                
015800          IF IN-IDSEGM = 'WDK625'                                         
015900             PERFORM E-ISRT-WDK625                                        
016000          END-IF                                                          
016100       END-IF                                                             
016200       PERFORM S01-LAES-W21705                                            
016300     END-PERFORM                                                          
016400                                                                          
016500     PERFORM Z-FINIT                                                      
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200     SKIP2                                                                
017300                                                                          
017400     PERFORM IMS-RESTART                                                  
017500                                                                          
017600     OPEN INPUT W21705                                                    
017700                                                                          
017800                                                                          
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000     .                                                                    
018100     EJECT                                                                
018200 C-REPL-WDK625  SECTION.                                                  
018300     SKIP2                                                                
018400     MOVE IN-IDARTNR        TO W-IDARTNR                                  
018500     MOVE IN-KDNOTTYP       TO W-KDNOTTYP                                 
018600     PERFORM IMS-GET-ARTC-NOT                                             
018700                                                                          
018800     MOVE IN-TEARTNOT       TO NOT-TEARTNOT                               
018900                                                                          
019100     PERFORM IMS-REPL-ARTC                                                
019210                                                                          
019220     ADD +1 TO CHKP-ANT                                                   
019300     .                                                                    
019400     EJECT                                                                
019500 B-REPL-WDK611  SECTION.                                                  
019600     SKIP2                                                                
019700     MOVE IN-IDARTNR     TO W-IDARTNR                                     
019800     PERFORM IMS-GET-ARTC-CLAG                                            
019900                                                                          
020000     MOVE IN-FLMANAT     TO CLAG-FLMANAT                                  
020100     MOVE IN-FLMANBK     TO CLAG-FLMANBK                                  
020200     MOVE IN-FLMANGK     TO CLAG-FLMANGK                                  
020300     MOVE IN-FLMANKP     TO CLAG-FLMANKP                                  
020400     MOVE IN-FLMANLT     TO CLAG-FLMANLT                                  
020500     MOVE IN-FLMANQ      TO CLAG-FLMANQ                                   
020600     MOVE IN-IDANSK      TO CLAG-IDANSK                                   
020700     MOVE IN-IDINK       TO CLAG-IDINK                                    
020800     MOVE IN-IDPLANGR-AG TO CLAG-IDPLANGR-AG                              
020900     MOVE IN-KDAVT       TO CLAG-KDAVT                                    
021000     MOVE IN-KDGK        TO CLAG-KDGK                                     
021100     MOVE IN-KDHF        TO CLAG-KDHF                                     
021200     MOVE IN-KDKSP       TO CLAG-KDKSP                                    
021300     MOVE IN-KVBK        TO CLAG-KVBK                                     
021400     MOVE IN-KVKP        TO CLAG-KVKP                                     
021500     MOVE IN-KVQ         TO CLAG-KVQ                                      
021600     MOVE IN-KVQ-JUST    TO CLAG-KVQ-JUST                                 
021700     MOVE IN-KVVECKOR-AT TO CLAG-KVVECKOR-AT                              
021800     MOVE IN-KVVECKOR-LT TO CLAG-KVVECKOR-LT                              
021900     MOVE IN-REDIRLEV    TO CLAG-REDIRLEV                                 
021910     MOVE IN-KDLEVPLF    TO CLAG-KDLEVPLF                                 
022000                                                                          
022200     PERFORM IMS-REPL-ARTC                                                
022310                                                                          
022320     ADD +1 TO CHKP-ANT                                                   
022400     .                                                                    
022500     EJECT                                                                
022600 D-DLET-WDK625  SECTION.                                                  
022700     SKIP2                                                                
022800     MOVE IN-IDARTNR     TO W-IDARTNR                                     
022900     MOVE IN-KDNOTTYP    TO W-KDNOTTYP                                    
023000     PERFORM IMS-GET-ARTC-NOT                                             
023100                                                                          
023200     IF SEGMENT-FINNS                                                     
023400        PERFORM IMS-DLET-ARTC                                             
023510                                                                          
023520        ADD +1 TO CHKP-ANT                                                
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 E-ISRT-WDK625  SECTION.                                                  
024000     SKIP2                                                                
024100     MOVE IN-IDARTNR        TO W-IDARTNR                                  
024200     MOVE IN-KDNOTTYP       TO NOT-KDNOTTYP                               
024300     MOVE IN-TEARTNOT       TO NOT-TEARTNOT                               
024400                                                                          
024600     PERFORM IMS-ISRT-ARTC-NOT                                            
024710                                                                          
024720     ADD +1 TO CHKP-ANT                                                   
024800     .                                                                    
024900     EJECT                                                                
025000 Z-FINIT SECTION.                                                         
025100                                                                          
025200                                                                          
025300     CLOSE W21705                                                         
025400     SKIP2                                                                
025500     MOVE 'S' TO POSTSUM-OPKOD                                            
025600     CALL POSTSUM USING POSTSUM-PARM                                      
025700     .                                                                    
025800     EJECT                                                                
025900 S01-LAES-W21705  SECTION.                                                
026000     SKIP2                                                                
026100     READ W21705 INTO IN-AREA                                             
026200     AT END                                                               
026300        SET END-OF-W21705 TO TRUE                                         
026400                                                                          
026500     NOT AT END                                                           
026600        MOVE 'W21705'  TO POSTSUM-FDNAMN                                  
026700        MOVE IN-IDSEGM TO POSTSUM-DDNAMN2                                 
026800        MOVE IN-ATGARD TO POSTSUM-TRANSTYP                                
026900        CALL POSTSUM USING POSTSUM-PARM                                   
027000                                                                          
027100***     ADD 1 TO W-W21705-KVPOST-IN                                       
027200     END-READ                                                             
027300     .                                                                    
027400     EJECT                                                                
027500 X-TAG-CHECKPOINT   SECTION.                                              
027600                                                                          
027700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
027800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
027900     PERFORM IMS-CHECKPOINT                                               
028000     MOVE ZERO TO CHKP-ANT                                                
028100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
028200     .                                                                    
028300     EJECT                                                                
028400* --- IMS SEKTIONER ---                                                   
028500     SKIP3                                                                
028600 IMS-GET-ARTC-CLAG SECTION.                                               
028700                                                                          
028800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
028900          DELIMITED BY SIZE INTO SSA1                                     
029000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
029100          DELIMITED BY SIZE INTO SSA2                                     
029200     MOVE '  GE' TO GODK-STATUSKODER                                      
029300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
029400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700     SKIP3                                                                
029800 IMS-GET-ARTC-NOT  SECTION.                                               
029900                                                                          
030000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
030100          DELIMITED BY SIZE INTO SSA1                                     
030200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
030300          DELIMITED BY SIZE INTO SSA2                                     
030400     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
030500          DELIMITED BY SIZE INTO SSA3                                     
030600     MOVE '  GE' TO GODK-STATUSKODER                                      
030700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
030800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
030900     PERFORM IMS-STATUSKONTROLL                                           
031000     .                                                                    
031100     EJECT                                                                
031200 IMS-ISRT-ARTC-NOT SECTION.                                               
031300                                                                          
031400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
031700          DELIMITED BY SIZE INTO SSA2                                     
031800     STRING 'WLARTC25   '                                                 
031900          DELIMITED BY SIZE INTO SSA3                                     
032000     MOVE '  GEII' TO GODK-STATUSKODER                                    
032100     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
032200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032500     SKIP3                                                                
032600 IMS-REPL-ARTC SECTION.                                                   
032700                                                                          
032800     MOVE '  ' TO GODK-STATUSKODER                                        
032900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
033000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033400     .                                                                    
033500     EJECT                                                                
033600 IMS-DLET-ARTC   SECTION.                                                 
033700                                                                          
033800     MOVE '  ' TO GODK-STATUSKODER                                        
033900     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA                         
034000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034400     .                                                                    
034500     EJECT                                                                
034600 IMS-RESTART SECTION.                                                     
034700     SKIP2                                                                
034800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034900     MOVE '  ' TO GODK-STATUSKODER                                        
035000     CALL CBLTDLI USING XRST MSG-PCB                                      
035100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035200                        CHKP-AREA-LENGTH CHKP-AREA                        
035300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035400     PERFORM IMS-STATUSKONTROLL                                           
035500     .                                                                    
035600     EJECT                                                                
035700 IMS-CHECKPOINT SECTION.                                                  
035800     SKIP2                                                                
035900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036000     MOVE '  XD' TO GODK-STATUSKODER                                      
036100     CALL CBLTDLI USING CHKP MSG-PCB                                      
036200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036300                        CHKP-AREA-LENGTH CHKP-AREA                        
036400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036500     PERFORM IMS-STATUSKONTROLL                                           
036600                                                                          
036700     IF IMS-EJ-OK                                                         
036800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
036900       DISPLAY FELTEXT                                                    
037000       CALL FELLOG                                                        
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 IMS-STATUSKONTROLL SECTION.                                              
037500     SKIP2                                                                
037600     SET STATUS-IX TO 1                                                   
037700     SEARCH GODK-STATUS                                                   
037800       AT END                                                             
037900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038000           DELIMITED BY SIZE INTO FELTEXT                                 
038100         DISPLAY FELTEXT                                                  
038200         CALL FELLOG                                                      
038300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038400         CONTINUE                                                         
038500     END-SEARCH                                                           
038600     .                                                                    
