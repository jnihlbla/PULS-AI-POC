000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6155500.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/04/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR WDR2 MED POSTER OM BELÄGGNINGEN PÅ                    
001000*        LAGERPLATSER                                                     
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
002300     SELECT W61554                     ASSIGN TO W61555D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W61554                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W61554      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W6155500'.            
003800 77  CURR-IMS-SECTION            PIC X(50).                               
003900 77  W-W61554-KVPOST-IN          PIC 9(9).                                
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
005800 77  W61554-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W61554                       VALUE 'J'.                   
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
008100*01  AREA -COPY W61554     -PRE IN-                                       
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-IDDC-X.                                                        
008800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
009100     03  W-WDGXKEY-6345-X.                                                
009200         05  W-IDHTYP            PIC X(4)    VALUE '6345'.                
009300         05  W-6345-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
009400                                                                          
009500     03  W-WDGXKEY-6346-X.                                                
009600         05  W-IDDC-6346         PIC X(2)    VALUE SPACE.                 
009700                                                                          
009800     03  W-WDGXKEY-6348-X.                                                
009900         05  W-ADLAGOMR-6348      PIC S9(3)   VALUE ZERO.                 
010000         05  W-ADGANG-6348        PIC S9(3)   VALUE ZERO.                 
010100         05  W-ADPLDEL-6348       PIC 9(2)    VALUE ZERO.                 
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
012700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6346'.                    
012800     SKIP3                                                                
012900 01  DLI-IO-WDGX6346.                                                     
013000*        05  -COPY WDGX6346                                               
013100     EJECT                                                                
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6348'.                    
013300     SKIP3                                                                
013400 01  DLI-IO-WDGX6348.                                                     
013500*        05  -COPY WDGX6348                                               
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
015100     PERFORM S01-LAES-W61554                                              
015200     IF NOT END-OF-W61554                                                 
015300** TA BORT GAMMAL DATA FÖRST                                              
015400       PERFORM IMS-GET-WDGX6345                                           
015500       PERFORM IMS-GHNP-WDGX6346                                          
015600       PERFORM UNTIL SEGMENT-SAKNAS                                       
015700         PERFORM IMS-DLET-WDGX6346                                        
015800         PERFORM IMS-GHNP-WDGX6346                                        
015900       END-PERFORM                                                        
016000     END-IF                                                               
016100                                                                          
016200     PERFORM UNTIL END-OF-W61554                                          
016300       IF CHKP-ANT > CHKP-MAX                                             
016400         PERFORM X-TAG-CHECKPOINT                                         
016500       END-IF                                                             
016600** LÄGG UPP NY DATA                                                       
016700       MOVE IN-IDDC TO W-IDDC-6346                                        
016800       PERFORM IMS-GET-WDGX6346                                           
016900       IF SEGMENT-FINNS                                                   
017000        IF IN-POST-TEXT = 'TOT'                                           
017100         MOVE IN-REDCBEL         TO 6346-REDCBEL                          
017200         PERFORM IMS-REPL-WDGX6346                                        
017300        ELSE                                                              
017400         MOVE IN-ADLAGOMR        TO 6348-ADLAGOMR                         
017500         MOVE IN-RELOBEL         TO 6348-RELOBEL                          
017600         PERFORM IMS-ISRT-WDGX6348                                        
017700        END-IF                                                            
017800         ADD +1 TO CHKP-ANT                                               
017900       ELSE                                                               
018000         PERFORM IMS-GET-WDGX6345                                         
018100         IF SEGMENT-FINNS                                                 
018200           MOVE IN-IDDC TO 6346-IDDC                                      
018300           MOVE 0       TO 6346-REDCBEL                                   
018400                                                                          
018500           PERFORM IMS-ISRT-WDGX6346                                      
018600           IF SEGMENT-FINNS                                               
018700             MOVE IN-ADLAGOMR        TO 6348-ADLAGOMR                     
018800             MOVE IN-RELOBEL         TO 6348-RELOBEL                      
018900             PERFORM IMS-ISRT-WDGX6348                                    
019000             ADD +1 TO CHKP-ANT                                           
019100           END-IF                                                         
019200         END-IF                                                           
019300       END-IF                                                             
019400                                                                          
019500       PERFORM S01-LAES-W61554                                            
019600     END-PERFORM                                                          
019700                                                                          
019800                                                                          
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600     SKIP2                                                                
020700                                                                          
020800     PERFORM IMS-RESTART                                                  
020900                                                                          
021000     OPEN INPUT W61554                                                    
021100                                                                          
021200                                                                          
021300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021400     .                                                                    
021500     EJECT                                                                
021600 Z-FINIT SECTION.                                                         
021700                                                                          
021800                                                                          
021900     CLOSE W61554                                                         
022000     SKIP2                                                                
022100     MOVE 'S' TO POSTSUM-OPKOD                                            
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
022400     EJECT                                                                
022500 S01-LAES-W61554  SECTION.                                                
022600     SKIP2                                                                
022700     READ W61554 INTO IN-AREA                                             
022800     AT END                                                               
022900*       MOVE HIGH-VALUE TO IN-ID                                          
023000        SET END-OF-W61554 TO TRUE                                         
023100                                                                          
023200     NOT AT END                                                           
023300        MOVE 'W61554' TO POSTSUM-FDNAMN                                   
023400        MOVE 'W61555D1' TO POSTSUM-DDNAMN2                                
023500        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
023600        CALL POSTSUM USING POSTSUM-PARM                                   
023700                                                                          
023800        ADD 1 TO W-W61554-KVPOST-IN                                       
023900     END-READ                                                             
024000     .                                                                    
024100     EJECT                                                                
024200 X-TAG-CHECKPOINT   SECTION.                                              
024300                                                                          
024400* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
024500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
024600     PERFORM IMS-CHECKPOINT                                               
024700     MOVE ZERO TO CHKP-ANT                                                
024800* --- LÄS OM DATABAS OM DET BEHÖVS                                        
024900     .                                                                    
025000     EJECT                                                                
025100* --- IMS SEKTIONER ---                                                   
025200                                                                          
025300     EJECT                                                                
025400*                                                                         
025500 IMS-GET-WDGX6345 SECTION.                                                
025600                                                                          
025700     MOVE 'IMS-GET-WDGX6345        ' TO CURR-IMS-SECTION                  
025800                                                                          
025900     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6345-X ')'                    
026000          DELIMITED BY SIZE INTO SSA1                                     
026100     MOVE '  ' TO GODK-STATUSKODER                                        
026200     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX01   SSA1                  
026300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
026400     PERFORM IMS-STATUSKONTROLL                                           
026500     .                                                                    
026600     EJECT                                                                
026700 IMS-GET-WDGX6346      SECTION.                                           
026800                                                                          
026900     MOVE 'IMS-GET-WDGX6346         ' TO  CURR-IMS-SECTION                
027000                                                                          
027100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6345-X ')'                    
027200          DELIMITED BY SIZE INTO SSA1                                     
027300     STRING 'WDGX6346(IDDC     =' W-WDGXKEY-6346-X ')'                    
027400          DELIMITED BY SIZE INTO SSA2                                     
027500     MOVE '  GE' TO GODK-STATUSKODER                                      
027600     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6346 SSA1 SSA2            
027700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     EJECT                                                                
028100 IMS-ISRT-WDGX6346 SECTION.                                               
028200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6345-X ')'                    
028300          DELIMITED BY SIZE INTO SSA1                                     
028400     MOVE 'WDGX6346 ' TO SSA2                                             
028500     MOVE '  ' TO GODK-STATUSKODER                                        
028600     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX6346 SSA1 SSA2           
028700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     .                                                                    
029000     EJECT                                                                
029100 IMS-REPL-WDGX6346 SECTION.                                               
029200     MOVE '  ' TO GODK-STATUSKODER                                        
029300     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-WDGX6346                     
029400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700     EJECT                                                                
029800 IMS-GHNP-WDGX6346 SECTION.                                               
029900                                                                          
030000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6345-X ')'                    
030100          DELIMITED BY SIZE INTO SSA1                                     
030200                                                                          
030300     MOVE 'WDGX6346 ' TO SSA2                                             
030400     MOVE '  GE' TO GODK-STATUSKODER                                      
030500     CALL CBLTDLI USING GHNP  WDR2-PCB DLI-IO-WDGX6346 SSA1 SSA2          
030600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
030700     PERFORM IMS-STATUSKONTROLL                                           
030800     .                                                                    
030900     EJECT                                                                
031000 IMS-DLET-WDGX6346 SECTION.                                               
031100                                                                          
031200     MOVE '  ' TO GODK-STATUSKODER                                        
031300     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX6346                     
031400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031700     EJECT                                                                
031800 IMS-ISRT-WDGX6348 SECTION.                                               
031900     MOVE 'WDGX6348 ' TO SSA1                                             
032000     MOVE '  ' TO GODK-STATUSKODER                                        
032100     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX6348 SSA1                
032200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032500     EJECT                                                                
032600 IMS-DLET-WDGX6348 SECTION.                                               
032700                                                                          
032800     MOVE '  ' TO GODK-STATUSKODER                                        
032900     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX6348                     
033000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300     EJECT                                                                
033400 IMS-RESTART SECTION.                                                     
033500     SKIP2                                                                
033600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
033700     MOVE '  ' TO GODK-STATUSKODER                                        
033800     CALL CBLTDLI USING XRST MSG-PCB                                      
033900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
034000                        CHKP-AREA-LENGTH CHKP-AREA                        
034100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034200     PERFORM IMS-STATUSKONTROLL                                           
034300     .                                                                    
034400     SKIP3                                                                
034500 IMS-CHECKPOINT SECTION.                                                  
034600     SKIP2                                                                
034700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034800     MOVE '  XD' TO GODK-STATUSKODER                                      
034900     CALL CBLTDLI USING CHKP MSG-PCB                                      
035000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035100                        CHKP-AREA-LENGTH CHKP-AREA                        
035200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035300     PERFORM IMS-STATUSKONTROLL                                           
035400                                                                          
035500     IF IMS-EJ-OK                                                         
035600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
035700       DISPLAY FELTEXT                                                    
035800       CALL FELLOG                                                        
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 IMS-STATUSKONTROLL SECTION.                                              
036300     SKIP2                                                                
036400     SET STATUS-IX TO 1                                                   
036500     SEARCH GODK-STATUS                                                   
036600       AT END                                                             
036700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036800           DELIMITED BY SIZE INTO FELTEXT                                 
036900         DISPLAY FELTEXT                                                  
037000         CALL FELLOG                                                      
037100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037200         CONTINUE                                                         
037300     END-SEARCH                                                           
037400     .                                                                    
