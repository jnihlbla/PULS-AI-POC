000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6155300.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/04/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR WDR2 MED LAGEROMRÅDEN SOM HAR FÖR MÅNGA               
001000*        ARTIKLAR.                                                        
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDR2                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INFIL MED  LAGERPLATS/KVPLATS/KVANTART                     
002300     SELECT W61552                     ASSIGN TO W61553D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W61552                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W61552      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W6155300'.            
003800 77  CURR-IMS-SECTION            PIC X(50).                               
003900 77  W-W61552-KVPOST-IN          PIC 9(9).                                
004000 01  WS-ADPLATS.                                                          
004100     03 WS-ADPLDEL               PIC 99.                                  
004200     03 WS-OVRIGT-ADPLDEL        PIC 999.                                 
004300                                                                          
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  W61552-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W61552                       VALUE 'J'.                   
006000     EJECT                                                                
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  IN-AREA-START               PIC X(24)   VALUE                        
007800                                             'IN-AREA-START'.             
007900     SKIP2                                                                
008000                                                                          
008100*01  AREA -COPY W61552     -PRE IN-                                       
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-IDDC-X.                                                        
008800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
009100     03  W-WDGXKEY-6341-X.                                                
009200         05  W-IDHTYP            PIC X(4)    VALUE '6341'.                
009300         05  W-6341-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
009400                                                                          
009500     03  W-WDGXKEY-6342-X.                                                
009600         05  W-IDDC-6342         PIC X(2)    VALUE SPACE.                 
009700                                                                          
009800     03  W-WDGXKEY-6344-X.                                                
009900         05  W-ADLAGOMR-6344      PIC S9(3)   VALUE ZERO.                 
010000         05  W-ADGANG-6344        PIC S9(3)   VALUE ZERO.                 
010100         05  W-ADPLDEL-6344       PIC 9(2)    VALUE ZERO.                 
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010900     88  IMS-EJ-OK                           VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100                                                                          
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
012300     SKIP3                                                                
012400 01  DLI-IO-WDGX01  .                                                     
012500*        05  -COPY WDGX01                                                 
012600                                                                          
012700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6342'.                    
012800     SKIP3                                                                
012900 01  DLI-IO-WDGX6342.                                                     
013000*        05  -COPY WDGX6342                                               
013100     EJECT                                                                
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6344'.                    
013300     SKIP3                                                                
013400 01  DLI-IO-WDGX6344.                                                     
013500*        05  -COPY WDGX6344                                               
013600                                                                          
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000*01  -COPY W0009   -PRE MSG-                                              
014100                                                                          
014200*01  -COPY W0008  -PRE WDR2-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500 PROCEDURE DIVISION  USING MSG-PCB WDR2-PCB.                              
014600 MAIN SECTION.                                                            
014700     ENTRY 'DLITCBL' USING MSG-PCB WDR2-PCB.                              
014800                                                                          
014900     SKIP2                                                                
015000     PERFORM A-INIT                                                       
015100     PERFORM S01-LAES-W61552                                              
015200     IF NOT END-OF-W61552                                                 
015300** TA BORT GAMMAL DATA FÖRST                                              
015400       PERFORM IMS-GET-WDGX6341                                           
015500       PERFORM IMS-GHNP-WDGX6342                                          
015600       PERFORM UNTIL SEGMENT-SAKNAS                                       
015700         PERFORM IMS-DLET-WDGX6342                                        
015800         PERFORM IMS-GHNP-WDGX6342                                        
015900       END-PERFORM                                                        
016000     END-IF                                                               
016100                                                                          
016200     PERFORM UNTIL END-OF-W61552                                          
016300       IF CHKP-ANT > CHKP-MAX                                             
016400         PERFORM X-TAG-CHECKPOINT                                         
016500       END-IF                                                             
016600** LÄGG UPP NY DATA                                                       
016700       MOVE IN-IDDC TO W-IDDC-6342                                        
016800       PERFORM IMS-GET-WDGX6342                                           
016900       IF SEGMENT-FINNS                                                   
017000         MOVE IN-ADLAGOMR        TO 6344-ADLAGOMR                         
017100         MOVE IN-ADGANG          TO 6344-ADGANG                           
017200         MOVE IN-ADPLATS         TO WS-ADPLATS                            
017300         MOVE WS-ADPLDEL         TO 6344-ADPLDEL                          
017400         MOVE IN-KVPLATS         TO 6344-KVPLATS                          
017500         MOVE IN-KVANTART        TO 6344-KVANTART                         
017600         PERFORM IMS-ISRT-WDGX6344                                        
017700         ADD +1 TO CHKP-ANT                                               
017800       ELSE                                                               
017900         PERFORM IMS-GET-WDGX6341                                         
018000         IF SEGMENT-FINNS                                                 
018100           MOVE IN-IDDC TO 6342-IDDC                                      
018200           PERFORM IMS-ISRT-WDGX6342                                      
018300           IF SEGMENT-FINNS                                               
018400             MOVE IN-ADLAGOMR        TO 6344-ADLAGOMR                     
018500             MOVE IN-ADGANG          TO 6344-ADGANG                       
018600             MOVE IN-ADPLATS         TO WS-ADPLATS                        
018700             MOVE WS-ADPLDEL         TO 6344-ADPLDEL                      
018800             MOVE IN-KVPLATS         TO 6344-KVPLATS                      
018900             MOVE IN-KVANTART        TO 6344-KVANTART                     
019000             PERFORM IMS-ISRT-WDGX6344                                    
019100             ADD +1 TO CHKP-ANT                                           
019200           END-IF                                                         
019300         END-IF                                                           
019400       END-IF                                                             
019500                                                                          
019600       PERFORM S01-LAES-W61552                                            
019700     END-PERFORM                                                          
019800                                                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700     SKIP2                                                                
020800                                                                          
020900     PERFORM IMS-RESTART                                                  
021000                                                                          
021100     OPEN INPUT W61552                                                    
021200                                                                          
021300                                                                          
021400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500     .                                                                    
021600     EJECT                                                                
021700 Z-FINIT SECTION.                                                         
021800                                                                          
021900                                                                          
022000     CLOSE W61552                                                         
022100     SKIP2                                                                
022200     MOVE 'S' TO POSTSUM-OPKOD                                            
022300     CALL POSTSUM USING POSTSUM-PARM                                      
022400     .                                                                    
022500     EJECT                                                                
022600 S01-LAES-W61552  SECTION.                                                
022700     SKIP2                                                                
022800     READ W61552 INTO IN-AREA                                             
022900     AT END                                                               
023000*       MOVE HIGH-VALUE TO IN-ID                                          
023100        SET END-OF-W61552 TO TRUE                                         
023200                                                                          
023300     NOT AT END                                                           
023400        MOVE 'W61552' TO POSTSUM-FDNAMN                                   
023500        MOVE 'W61553D1' TO POSTSUM-DDNAMN2                                
023600        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
023700        CALL POSTSUM USING POSTSUM-PARM                                   
023800                                                                          
023900        ADD 1 TO W-W61552-KVPOST-IN                                       
024000     END-READ                                                             
024100     .                                                                    
024200     EJECT                                                                
024300 X-TAG-CHECKPOINT   SECTION.                                              
024400                                                                          
024500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
024600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
024700     PERFORM IMS-CHECKPOINT                                               
024800     MOVE ZERO TO CHKP-ANT                                                
024900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
025000     .                                                                    
025100     EJECT                                                                
025200* --- IMS SEKTIONER ---                                                   
025300                                                                          
025400     EJECT                                                                
025500*                                                                         
025600 IMS-GET-WDGX6341 SECTION.                                                
025700     MOVE 'IMS-GET-WDGX6341        ' TO CURR-IMS-SECTION                  
025800                                                                          
025900     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6341-X ')'                    
026000          DELIMITED BY SIZE INTO SSA1                                     
026100     MOVE '  ' TO GODK-STATUSKODER                                        
026200     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX01   SSA1                  
026300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
026400     PERFORM IMS-STATUSKONTROLL                                           
026500     .                                                                    
026600     EJECT                                                                
026700 IMS-GET-WDGX6342      SECTION.                                           
026800     MOVE 'IMS-GET-WDGX6342         ' TO  CURR-IMS-SECTION                
026900                                                                          
027000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6341-X ')'                    
027100          DELIMITED BY SIZE INTO SSA1                                     
027200     STRING 'WDGX6342(IDDC     =' W-WDGXKEY-6342-X ')'                    
027300          DELIMITED BY SIZE INTO SSA2                                     
027400     MOVE '  GE' TO GODK-STATUSKODER                                      
027500     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6342 SSA1 SSA2            
027600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
027700     PERFORM IMS-STATUSKONTROLL                                           
027800     .                                                                    
027900     EJECT                                                                
028000 IMS-ISRT-WDGX6342 SECTION.                                               
028100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6341-X ')'                    
028200          DELIMITED BY SIZE INTO SSA1                                     
028300     MOVE 'WDGX6342 ' TO SSA2                                             
028400     MOVE '  ' TO GODK-STATUSKODER                                        
028500     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX6342 SSA1 SSA2           
028600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
028700     PERFORM IMS-STATUSKONTROLL                                           
028800     .                                                                    
028900     EJECT                                                                
029000 IMS-GHNP-WDGX6342 SECTION.                                               
029100                                                                          
029200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6341-X ')'                    
029300          DELIMITED BY SIZE INTO SSA1                                     
029400                                                                          
029500     MOVE 'WDGX6342 ' TO SSA2                                             
029600     MOVE '  GE' TO GODK-STATUSKODER                                      
029700     CALL CBLTDLI USING GHNP  WDR2-PCB DLI-IO-WDGX6342 SSA1 SSA2          
029800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
029900     PERFORM IMS-STATUSKONTROLL                                           
030000     .                                                                    
030100     EJECT                                                                
030200 IMS-DLET-WDGX6342 SECTION.                                               
030300                                                                          
030400     MOVE '  ' TO GODK-STATUSKODER                                        
030500     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX6342                     
030600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
030700     PERFORM IMS-STATUSKONTROLL                                           
030800     .                                                                    
030900     EJECT                                                                
031000 IMS-ISRT-WDGX6344 SECTION.                                               
031100     MOVE 'WDGX6344 ' TO SSA1                                             
031200     MOVE '  ' TO GODK-STATUSKODER                                        
031300     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX6344 SSA1                
031400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSKONTROLL                                           
031600        MOVE 'W61553'     TO POSTSUM-FDNAMN                               
031700        MOVE W-IDDC-6342  TO POSTSUM-DDNAMN2                              
031800        MOVE 'ISRT'       TO POSTSUM-TRANSTYP                             
031900        CALL POSTSUM USING POSTSUM-PARM                                   
032000                                                                          
032100     .                                                                    
032200     EJECT                                                                
032300 IMS-DLET-WDGX6344 SECTION.                                               
032400                                                                          
032500     MOVE '  ' TO GODK-STATUSKODER                                        
032600     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX6344                     
032700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000     EJECT                                                                
033100 IMS-RESTART SECTION.                                                     
033200     SKIP2                                                                
033300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
033400     MOVE '  ' TO GODK-STATUSKODER                                        
033500     CALL CBLTDLI USING XRST MSG-PCB                                      
033600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
033700                        CHKP-AREA-LENGTH CHKP-AREA                        
033800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     SKIP3                                                                
034200 IMS-CHECKPOINT SECTION.                                                  
034300     SKIP2                                                                
034400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034500     MOVE '  XD' TO GODK-STATUSKODER                                      
034600     CALL CBLTDLI USING CHKP MSG-PCB                                      
034700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
034800                        CHKP-AREA-LENGTH CHKP-AREA                        
034900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035000     PERFORM IMS-STATUSKONTROLL                                           
035100                                                                          
035200     IF IMS-EJ-OK                                                         
035300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
035400       DISPLAY FELTEXT                                                    
035500       CALL FELLOG                                                        
035600     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 IMS-STATUSKONTROLL SECTION.                                              
036000     SKIP2                                                                
036100     SET STATUS-IX TO 1                                                   
036200     SEARCH GODK-STATUS                                                   
036300       AT END                                                             
036400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036500           DELIMITED BY SIZE INTO FELTEXT                                 
036600         DISPLAY FELTEXT                                                  
036700         CALL FELLOG                                                      
036800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036900         CONTINUE                                                         
037000     END-SEARCH                                                           
037100     .                                                                    
