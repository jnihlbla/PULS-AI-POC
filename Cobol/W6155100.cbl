000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6155100.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/04/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR WDR2 MED ARTIKLAR SOM FINNS PÅ                        
001000*        ICKE DEFINIERADE PLATSER                                         
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
002200*          --- INFIL MED ARTIKLAR SOM HAR FELAKTIG LAGERPLATS             
002300     SELECT W61550                     ASSIGN TO W61551D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W61550                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W61550      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W6155100'.            
003800 77  CURR-IMS-SECTION            PIC X(50).                               
003900 77  W-W61550-KVPOST-IN          PIC 9(9).                                
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
005800 77  W61550-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W61550                       VALUE 'J'.                   
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
008100*01  AREA -COPY W61550     -PRE IN-                                       
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-IDDC-X.                                                        
008800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
009100     03  W-WDGXKEY-6335-X.                                                
009200         05  W-IDHTYP            PIC X(4)    VALUE '6335'.                
009300         05  W-6335-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
009400                                                                          
009500     03  W-WDGXKEY-6336-X.                                                
009600         05  W-IDDC-6336         PIC X(2)    VALUE SPACE.                 
009700                                                                          
009800     03  W-WDGXKEY-6338-X.                                                
009900         05  W-IDARTNR-6338    PIC S9(9) COMP-3 VALUE ZERO.               
010000         05  W-ADLAGOMR-6338   PIC S9(3) COMP-3 VALUE ZERO.               
010100         05  W-ADGANG-6338     PIC S9(3) COMP-3 VALUE ZERO.               
010200         05  W-ADPLATS-6338    PIC S9(5) COMP-3 VALUE ZERO.               
010300     SKIP2                                                                
010400*    --- STATUS-KOD FRÅN IMS                                              
010500 01  STATUS-WS                   PIC XX.                                  
010600     88  SEGMENT-FINNS                       VALUE '  '.                  
010700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011000     88  IMS-EJ-OK                           VALUE 'XD'.                  
011100     SKIP2                                                                
011200 01  GODK-STATUSKODER.                                                    
011300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011400     SKIP3                                                                
011500 01  SSA1                        PIC X(64).                               
011600 01  SSA2                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200                                                                          
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
012400     SKIP3                                                                
012500 01  DLI-IO-WDGX01  .                                                     
012600*        05  -COPY WDGX01                                                 
012700                                                                          
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6336'.                    
012900     SKIP3                                                                
013000 01  DLI-IO-WDGX6336.                                                     
013100*        05  -COPY WDGX6336                                               
013200     EJECT                                                                
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6338'.                    
013400     SKIP3                                                                
013500 01  DLI-IO-WDGX6338.                                                     
013600*        05  -COPY WDGX6338                                               
013700                                                                          
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0009   -PRE MSG-                                              
014200                                                                          
014300*01  -COPY W0008  -PRE WDR2-                                              
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600 PROCEDURE DIVISION  USING MSG-PCB WDR2-PCB.                              
014700 MAIN SECTION.                                                            
014800     ENTRY 'DLITCBL' USING MSG-PCB WDR2-PCB.                              
014900                                                                          
015000     SKIP2                                                                
015100     PERFORM A-INIT                                                       
015200     PERFORM S01-LAES-W61550                                              
015300     IF NOT END-OF-W61550                                                 
015400** TA BORT GAMMAL DATA FÖRST                                              
015500       PERFORM IMS-GET-WDGX6335                                           
015600       PERFORM IMS-GHNP-WDGX6336                                          
015700       PERFORM UNTIL SEGMENT-SAKNAS                                       
015800         PERFORM IMS-DLET-WDGX6336                                        
015900         PERFORM IMS-GHNP-WDGX6336                                        
016000       END-PERFORM                                                        
016100     END-IF                                                               
016200                                                                          
016300     PERFORM UNTIL END-OF-W61550                                          
016400       IF CHKP-ANT > CHKP-MAX                                             
016500         PERFORM X-TAG-CHECKPOINT                                         
016600       END-IF                                                             
016700** LÄGG UPP NY DATA                                                       
016800       MOVE IN-IDDC TO W-IDDC-6336                                        
016900       PERFORM IMS-GET-WDGX6336                                           
017000       IF SEGMENT-FINNS                                                   
017100         MOVE IN-IDARTNR         TO 6338-IDARTNR                          
017200         MOVE IN-ADLAGOMR        TO 6338-ADLAGOMR                         
017300         MOVE IN-ADGANG          TO 6338-ADGANG                           
017400         MOVE IN-ADPLATS         TO 6338-ADPLATS                          
017500*        MOVE IN-ADPLATS         TO WS-ADPLATS                            
017600*        MOVE WS-ADPLDEL         TO 6338-ADPLDEL                          
017700         PERFORM IMS-ISRT-WDGX6338                                        
017800         ADD +1 TO CHKP-ANT                                               
017900       ELSE                                                               
018000         PERFORM IMS-GET-WDGX6335                                         
018100         IF SEGMENT-FINNS                                                 
018200           MOVE IN-IDDC TO 6336-IDDC                                      
018300           PERFORM IMS-ISRT-WDGX6336                                      
018400           IF SEGMENT-FINNS                                               
018500             MOVE IN-IDARTNR         TO 6338-IDARTNR                      
018600             MOVE IN-ADLAGOMR        TO 6338-ADLAGOMR                     
018700             MOVE IN-ADGANG          TO 6338-ADGANG                       
018800             MOVE IN-ADPLATS         TO 6338-ADPLATS                      
018900*            MOVE IN-ADPLATS         TO WS-ADPLATS                        
019000*            MOVE WS-ADPLDEL         TO 6338-ADPLDEL                      
019100             PERFORM IMS-ISRT-WDGX6338                                    
019200             ADD +1 TO CHKP-ANT                                           
019300           END-IF                                                         
019400         END-IF                                                           
019500       END-IF                                                             
019600                                                                          
019700       PERFORM S01-LAES-W61550                                            
019800     END-PERFORM                                                          
019900                                                                          
020000                                                                          
020100     PERFORM Z-FINIT                                                      
020200                                                                          
020300     MOVE ZERO TO RETURN-CODE                                             
020400     GOBACK                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 A-INIT SECTION.                                                          
020800     SKIP2                                                                
020900                                                                          
021000     PERFORM IMS-RESTART                                                  
021100                                                                          
021200     OPEN INPUT W61550                                                    
021300                                                                          
021400                                                                          
021500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021600     .                                                                    
021700     EJECT                                                                
021800 Z-FINIT SECTION.                                                         
021900                                                                          
022000                                                                          
022100     CLOSE W61550                                                         
022200     SKIP2                                                                
022300     MOVE 'S' TO POSTSUM-OPKOD                                            
022400     CALL POSTSUM USING POSTSUM-PARM                                      
022500     .                                                                    
022600     EJECT                                                                
022700 S01-LAES-W61550  SECTION.                                                
022800     SKIP2                                                                
022900     READ W61550 INTO IN-AREA                                             
023000     AT END                                                               
023100*       MOVE HIGH-VALUE TO IN-ID                                          
023200        SET END-OF-W61550 TO TRUE                                         
023300                                                                          
023400     NOT AT END                                                           
023500        MOVE 'W61550' TO POSTSUM-FDNAMN                                   
023600        MOVE 'W61551D1' TO POSTSUM-DDNAMN2                                
023700        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
023800        CALL POSTSUM USING POSTSUM-PARM                                   
023900                                                                          
024000        ADD 1 TO W-W61550-KVPOST-IN                                       
024100     END-READ                                                             
024200     .                                                                    
024300     EJECT                                                                
024400 X-TAG-CHECKPOINT   SECTION.                                              
024500                                                                          
024600* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
024700* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
024800     PERFORM IMS-CHECKPOINT                                               
024900     MOVE ZERO TO CHKP-ANT                                                
025000* --- LÄS OM DATABAS OM DET BEHÖVS                                        
025100     .                                                                    
025200     EJECT                                                                
025300* --- IMS SEKTIONER ---                                                   
025400                                                                          
025500     EJECT                                                                
025600*                                                                         
025700 IMS-GET-WDGX6335 SECTION.                                                
025800     MOVE 'IMS-GET-WDGX6335        ' TO CURR-IMS-SECTION                  
025900                                                                          
026000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6335-X ')'                    
026100          DELIMITED BY SIZE INTO SSA1                                     
026200     MOVE '  ' TO GODK-STATUSKODER                                        
026300     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX01   SSA1                  
026400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
026500     PERFORM IMS-STATUSKONTROLL                                           
026600     .                                                                    
026700     EJECT                                                                
026800*IMS-GNP-WDGX6332 SECTION.                                                
026900*    MOVE 'IMS-GNP-WDGX6332        ' TO CURR-IMS-SECTION                  
027000*                                                                         
027100*    STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
027200*         DELIMITED BY SIZE INTO SSA1                                     
027300*                                                                         
027400*    MOVE 'WDGX6332 ' TO SSA2                                             
027500*                                                                         
027600*    MOVE '  GE' TO GODK-STATUSKODER                                      
027700*    CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
027800*    MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
027900*    PERFORM IMS-STATUSKONTROLL                                           
028000*    .                                                                    
028100*    EJECT                                                                
028200 IMS-GET-WDGX6336      SECTION.                                           
028300     MOVE 'IMS-GET-WDGX6336         ' TO  CURR-IMS-SECTION                
028400                                                                          
028500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6335-X ')'                    
028600          DELIMITED BY SIZE INTO SSA1                                     
028700     STRING 'WDGX6336(IDDC     =' W-WDGXKEY-6336-X ')'                    
028800          DELIMITED BY SIZE INTO SSA2                                     
028900     MOVE '  GE' TO GODK-STATUSKODER                                      
029000     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6336 SSA1 SSA2            
029100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     .                                                                    
029400     EJECT                                                                
029500 IMS-ISRT-WDGX6336 SECTION.                                               
029600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6335-X ')'                    
029700          DELIMITED BY SIZE INTO SSA1                                     
029800     MOVE 'WDGX6336 ' TO SSA2                                             
029900     MOVE '  ' TO GODK-STATUSKODER                                        
030000     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX6336 SSA1 SSA2           
030100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSKONTROLL                                           
030300        MOVE 'W61551'   TO POSTSUM-FDNAMN                                 
030400        MOVE 'WDGX6336' TO POSTSUM-DDNAMN2                                
030500        MOVE 'ISRT'     TO POSTSUM-TRANSTYP                               
030600        CALL POSTSUM USING POSTSUM-PARM                                   
030700     .                                                                    
030800     EJECT                                                                
030900 IMS-GHNP-WDGX6336 SECTION.                                               
031000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6335-X ')'                    
031100          DELIMITED BY SIZE INTO SSA1                                     
031200                                                                          
031300     MOVE 'WDGX6336 ' TO SSA2                                             
031400     MOVE '  GE' TO GODK-STATUSKODER                                      
031500     CALL CBLTDLI USING GHNP  WDR2-PCB DLI-IO-WDGX6336 SSA1 SSA2          
031600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-DLET-WDGX6336 SECTION.                                               
032100     MOVE '  ' TO GODK-STATUSKODER                                        
032200     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX6336                     
032300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032600     EJECT                                                                
032700 IMS-ISRT-WDGX6338 SECTION.                                               
032800     MOVE 'WDGX6338 ' TO SSA1                                             
032900     MOVE '  II' TO GODK-STATUSKODER                                      
033000     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX6338 SSA1                
033100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
033200     PERFORM IMS-STATUSKONTROLL                                           
033300        MOVE 'W61551'   TO POSTSUM-FDNAMN                                 
033400        MOVE 'WDGX6338' TO POSTSUM-DDNAMN2                                
033500        MOVE 'ISRT'     TO POSTSUM-TRANSTYP                               
033600        CALL POSTSUM USING POSTSUM-PARM                                   
033700     .                                                                    
033800     EJECT                                                                
033900 IMS-DLET-WDGX6338 SECTION.                                               
034000                                                                          
034100     MOVE '  ' TO GODK-STATUSKODER                                        
034200     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX6338                     
034300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
034400     PERFORM IMS-STATUSKONTROLL                                           
034500     .                                                                    
034600     EJECT                                                                
034700 IMS-RESTART SECTION.                                                     
034800     SKIP2                                                                
034900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035000     MOVE '  ' TO GODK-STATUSKODER                                        
035100     CALL CBLTDLI USING XRST MSG-PCB                                      
035200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035300                        CHKP-AREA-LENGTH CHKP-AREA                        
035400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035500     PERFORM IMS-STATUSKONTROLL                                           
035600     .                                                                    
035700     SKIP3                                                                
035800 IMS-CHECKPOINT SECTION.                                                  
035900     SKIP2                                                                
036000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036100     MOVE '  XD' TO GODK-STATUSKODER                                      
036200     CALL CBLTDLI USING CHKP MSG-PCB                                      
036300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036400                        CHKP-AREA-LENGTH CHKP-AREA                        
036500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036600     PERFORM IMS-STATUSKONTROLL                                           
036700                                                                          
036800     IF IMS-EJ-OK                                                         
036900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
037000       DISPLAY FELTEXT                                                    
037100       CALL FELLOG                                                        
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 IMS-STATUSKONTROLL SECTION.                                              
037600     SKIP2                                                                
037700     SET STATUS-IX TO 1                                                   
037800     SEARCH GODK-STATUS                                                   
037900       AT END                                                             
038000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038100           DELIMITED BY SIZE INTO FELTEXT                                 
038200         DISPLAY FELTEXT                                                  
038300         CALL FELLOG                                                      
038400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038500         CONTINUE                                                         
038600     END-SEARCH                                                           
038700     .                                                                    
