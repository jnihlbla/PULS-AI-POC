000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL1001      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W6123200.                                                
000800 AUTHOR.         BODIL LINDAHL.                                           
000900 DATE-WRITTEN.   97/01/27.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*    FUNKTION:                                                            
001300*        LÄSER FIL W612PP MED URVAL FRÅN BILD 6301                        
001400*        SKAPAR UTFIL TILL LISTA                                          
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001700*                              WLINLD (WDL6A)                             
001800*                              WLARTS (WDK7)                              
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- URVAL FRÅN 6301                                            
002900     SELECT W612PP                     ASSIGN TO W61232D1.                
003000     SKIP2                                                                
003100*          --- UTFIL FÖR LISTA                                            
003200     SELECT UTFIL                      ASSIGN TO W61232D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W612PP                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  PARM             PIC X(80).                                          
004300     EJECT                                                                
004400 FD  UTFIL                                                                
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  UT-POST      -COPY W61233     -L.                                    
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(8)    VALUE 'W6123200'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  W-KVBO                      PIC S9(7)   VALUE ZERO.                  
005800 77  W-KVRADER                   PIC S9(7)   VALUE ZERO.                  
005900 77  W-KVNYART                   PIC S9(7)   VALUE ZERO.                  
006000 77  W-KVPRIOART                 PIC S9(7)   VALUE ZERO.                  
006100 77  W-ANTAL-LASN                PIC S9(7)   VALUE ZERO.                  
006200 77  W-SPAR-IDKUNDRF             PIC X(10)   VALUE SPACE.                 
006300 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
006400 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
006500 77  WS-KVROS                    PIC S9(7)            VALUE ZERO.         
006600                                                                          
006700 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006800     88  END-OF-W612PP                       VALUE 'J'.                   
006900                                                                          
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500     EJECT                                                                
007600*    --- VALID DC CODES                                                   
007700*01  -COPY WWDC99                                                         
007800     EJECT                                                                
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  PARM-AREA-START             PIC X(24)   VALUE                        
009300                                 'PARM-AREA-START '.                      
009400 01  PARM-AREA                   PIC X(7).                                
009500 01  FILLER REDEFINES PARM-AREA.                                          
009600     03 PARM-IDFAKT              PIC 9(7).                                
009700     EJECT                                                                
009800 01  UT-AREA-START               PIC X(24)   VALUE                        
009900                                 'UT-AREA-START '.                        
010000*01  AREA -COPY W61233 -PRE UT-                                           
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-IDARTNR-X.                                                     
010900         05  W-IDARTNR           PIC S9(9)              COMP-3.           
011000                                                                          
011100     03  W-KDSEGKEY-X.                                                    
011200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011300                                                                          
011400     03  W-DAINLEV-X.                                                     
011500         05  W-DAINLEV           PIC 9(16).                               
011600                                                                          
011700     03  W-IDFAKT-X.                                                      
011800         05  W-IDFAKT            PIC S9(7)              COMP-3.           
011900                                                                          
012000     03  W-IDDC-X.                                                        
012100         05  W-IDDC              PIC X(2)          VALUE SPACE.           
012200                                                                          
012300     03  W-6301KEY-X.                                                     
012400         05  W-6301-IDHTYP       PIC X(4)      VALUE '6301'.              
012500         05  W-6301-IDDC         PIC X(2)      VALUE SPACE.               
012600         05  FILLER              PIC X(24)     VALUE LOW-VALUE.           
012700                                                                          
012800     03  W-WDL6ASEQ-MIN.                                                  
012900         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
013000         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
013100         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
013200         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
013300                                                                          
013400     03  W-WDL6ASEQ-MAX.                                                  
013500         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
013600         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
013700         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
013800         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
013900                                                                          
014000     03  W-WDL6A1KY-MIN.                                                  
014100         05  W-IDFAKT-MIN         PIC S9(7)             COMP-3.           
014200         05  W-IDKUNDRF-MIN       PIC X(10).                              
014300         05  W-IDKUNDNR-MIN       PIC S9(7)             COMP-3.           
014400         05  W-IDKOLLI-MIN        PIC S9(5)             COMP-3.           
014500         05  FILLER               PIC X(21).                              
014600                                                                          
014700     03  W-WDL6A1KY-MAX.                                                  
014800         05  W-IDFAKT-MAX         PIC S9(7)             COMP-3.           
014900         05  W-IDKUNDRF-MAX       PIC X(10).                              
015000         05  W-IDKUNDNR-MAX       PIC S9(7)             COMP-3.           
015100         05  W-IDKOLLI-MAX        PIC S9(5)             COMP-3.           
015200         05  FILLER               PIC X(21).                              
015300                                                                          
015400     EJECT                                                                
015500*    --- STATUS-KOD FRÅN IMS                                              
015600 01  STATUS-WS                   PIC XX.                                  
015700     88  SEGMENT-FINNS                       VALUE '  '.                  
015800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016000     SKIP2                                                                
016100 01  GODK-STATUSKODER.                                                    
016200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016300     SKIP3                                                                
016400 01  SSA1                        PIC X(128).                              
016500 01  SSA2                        PIC X(64).                               
016600     EJECT                                                                
016700*    --- IMS FUNKTIONSKODER                                               
016800*01  -COPY W0003                                                          
016900     EJECT                                                                
017000*    ---  DLI INPUT-OUTPUT AREA                                           
017100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC'.                      
017200 01  DLI-IO-INLC.                                                         
017300*    03  -COPY WDL611                                                     
017400     EJECT                                                                
017500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS'.                      
017600 01  DLI-IO-ARTS.                                                         
017700*    03  -COPY WDK711                                                     
017800     EJECT                                                                
017900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLD'.                      
018000 01  DLI-IO-INLD.                                                         
018100*    03  -COPY WDL6A1                                                     
018200     EJECT                                                                
018300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGX63'.                      
018400 01  DLI-IO-GX63.                                                         
018500*    03  -COPY WDGX6302                                                   
018600     EJECT                                                                
018700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
018800 01  DLI-IO-WDK611.                                                       
018900*    03  -COPY WDK611                                                     
019000     EJECT                                                                
019100 LINKAGE SECTION.                                                         
019200                                                                          
019300*01  -COPY W0008  -PRE INLC-                                              
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008  -PRE ARTS-                                              
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900*01  -COPY W0008  -PRE INLD-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200*01  -COPY W0008  -PRE GX63-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008  -PRE WDK6-                                              
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800 PROCEDURE DIVISION  USING INLC-PCB ARTS-PCB                              
020900     INLD-PCB GX63-PCB WDK6-PCB.                                          
021000 MAIN SECTION.                                                            
021100     ENTRY 'DLITCBL' USING INLC-PCB ARTS-PCB                              
021200     INLD-PCB GX63-PCB WDK6-PCB.                                          
021300                                                                          
021400     PERFORM A-INIT                                                       
021500                                                                          
021600     PERFORM S11-LAES-W612PP                                              
021700     IF PARM-IDFAKT NOT = ZERO                                            
021800        PERFORM B-SKAPA-UTFIL                                             
021900     END-IF                                                               
022000                                                                          
022100     PERFORM Z-FINIT                                                      
022200                                                                          
022300     MOVE ZERO TO RETURN-CODE                                             
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800                                                                          
022900     OPEN INPUT  W612PP                                                   
023000     OPEN OUTPUT UTFIL                                                    
023100                                                                          
023200     ACCEPT DAGENS-DATUM    FROM DATE                                     
023300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023400                                                                          
023500     MOVE LOW-VALUE  TO W-WDL6ASEQ-MIN                                    
023600                        W-WDL6A1KY-MIN                                    
023700     MOVE HIGH-VALUE TO W-WDL6ASEQ-MAX                                    
023800                        W-WDL6A1KY-MAX                                    
023900     PERFORM S01-NOLLSTALL                                                
024000     .                                                                    
024100     EJECT                                                                
024200 B-SKAPA-UTFIL SECTION.                                                   
024300                                                                          
024400     MOVE PARM-IDFAKT TO W-SEQA-IDFAKT-MIN                                
024500                         W-SEQA-IDFAKT-MAX                                
024600                         W-IDFAKT                                         
024700                                                                          
024800     PERFORM IMS-GU-INLC-WLINLC11-F                                       
024900                                                                          
025000     IF SEGMENT-FINNS                                                     
025100                                                                          
025200        PERFORM BB-SKAPA-UTPOST                                           
025300                                                                          
025400        PERFORM UNTIL SEGMENT-SAKNAS                                      
025500                                                                          
025600           IF  W-SPAR-IDKUNDRF = INL-IDKUNDRF                             
025700           AND W-SPAR-IDKUNDNR = INL-IDKUNDNR                             
025800           AND W-SPAR-IDKOLLI  = INL-IDKOLLI                              
025900                                                                          
026000             PERFORM BA-KOLLA-ARTIKEL                                     
026100                                                                          
026200           ELSE                                                           
026300                                                                          
026400              MOVE W-KVRADER    TO UT-IDARTNR-KOLLI                       
026500              MOVE W-KVNYART    TO UT-IDARTNR-NEW                         
026600              MOVE W-KVPRIOART  TO UT-IDARTNR-PRIO                        
026700              MOVE W-KVBO       TO UT-IDARTNR-BO                          
026800                                                                          
026900              PERFORM IMS-GHU-WL630111                                    
027000              MOVE 6302-IDLBBET  TO UT-IDLBBET                            
027100              MOVE 6302-DABERANK TO UT-DABERANK                           
027200              PERFORM S12-SKRIV-UTFIL                                     
027300              PERFORM S01-NOLLSTALL                                       
027400                                                                          
027500              PERFORM BB-SKAPA-UTPOST                                     
027600              PERFORM BA-KOLLA-ARTIKEL                                    
027700           END-IF                                                         
027800                                                                          
027900           PERFORM IMS-GN-INLC-WLINLC11                                   
028000        END-PERFORM                                                       
028100                                                                          
028200        IF UT-IDKUNDRF NOT = SPACE                                        
028300           MOVE W-KVRADER    TO UT-IDARTNR-KOLLI                          
028400           MOVE W-KVNYART    TO UT-IDARTNR-NEW                            
028500           MOVE W-KVPRIOART  TO UT-IDARTNR-PRIO                           
028600           MOVE W-KVBO       TO UT-IDARTNR-BO                             
028700                                                                          
028800           PERFORM IMS-GHU-WL630111                                       
028900           MOVE 6302-IDLBBET  TO UT-IDLBBET                               
029000           MOVE 6302-DABERANK TO UT-DABERANK                              
029100           PERFORM S12-SKRIV-UTFIL                                        
029200        END-IF                                                            
029300                                                                          
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 BA-KOLLA-ARTIKEL SECTION.                                                
029800                                                                          
029900     ADD 1      TO W-KVRADER                                              
030000     PERFORM BAA-LAS-FRAM-ARTIKEL                                         
030100     ADD +1     TO W-ANTAL-LASN                                           
030200                                                                          
030300     IF CDC-SE                                                            
030400        PERFORM IMS-GU-WDK611                                             
030500        IF CLAG-ADLAGOMR = ZERO                                           
030600           ADD 1 TO W-KVNYART                                             
030700        END-IF                                                            
030800        MOVE CLAG-KVROS TO WS-KVROS                                       
030900     ELSE                                                                 
031000        PERFORM IMS-GU-ARTS11                                             
031100        IF SLAG-ADLAGOMR = ZERO                                           
031200           ADD 1 TO W-KVNYART                                             
031300        END-IF                                                            
031400        COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK               
031500     END-IF                                                               
031600                                                                          
031700     IF WS-KVROS >= INL-KVAVIS                                            
031800        ADD +1 TO W-KVBO                                                  
031900     END-IF                                                               
032000                                                                          
032100     IF INL-FLPRIO = 'J' OR 'Y'                                           
032200        ADD +1 TO W-KVPRIOART                                             
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 BAA-LAS-FRAM-ARTIKEL SECTION.                                            
032700                                                                          
032800     MOVE LOW-VALUE    TO W-WDL6A1KY-MIN                                  
032900     MOVE HIGH-VALUE   TO W-WDL6A1KY-MAX                                  
033000     MOVE INL-IDFAKT   TO W-IDFAKT-MIN                                    
033100                          W-IDFAKT-MAX                                    
033200     MOVE INL-IDKUNDRF TO W-IDKUNDRF-MIN                                  
033300                          W-IDKUNDRF-MAX                                  
033400     MOVE INL-IDKUNDNR TO W-IDKUNDNR-MIN                                  
033500                          W-IDKUNDNR-MAX                                  
033600     MOVE INL-IDKOLLI  TO W-IDKOLLI-MIN                                   
033700                          W-IDKOLLI-MAX                                   
033800                                                                          
033900     IF W-ANTAL-LASN = ZERO                                               
034000        PERFORM IMS-GU-WLINLD01                                           
034100     ELSE                                                                 
034200        PERFORM IMS-GN-WLINLD01                                           
034300     END-IF                                                               
034400                                                                          
034500     MOVE SEQA-IDARTNR           TO W-IDARTNR                             
034600     MOVE INL-IDDC               TO W-IDDC                                
034700                                    WS-IDDC                               
034800     .                                                                    
034900     EJECT                                                                
035000 BB-SKAPA-UTPOST SECTION.                                                 
035100                                                                          
035200     MOVE INL-IDFAKT   TO UT-IDFAKT                                       
035300     MOVE INL-IDDC     TO UT-IDDC                                         
035400                          W-6301-IDDC                                     
035500     MOVE INL-IDKUNDRF TO UT-IDKUNDRF                                     
035600                          W-SPAR-IDKUNDRF                                 
035700     MOVE INL-IDKUNDNR TO UT-IDKUNDNR                                     
035800                          W-SPAR-IDKUNDNR                                 
035900     MOVE INL-IDKOLLI  TO UT-IDKOLLI                                      
036000                          W-SPAR-IDKOLLI                                  
036100                                                                          
036200     IF INL-IDPTYP      =  'R30'                                          
036300     AND INL-TIINLMOT   >   ZERO                                          
036400         MOVE 'MISSING' TO UT-TEINFO                                      
036500     END-IF                                                               
036600                                                                          
036700     MOVE ZERO TO  W-KVRADER                                              
036800                   W-KVNYART                                              
036900                   W-KVPRIOART                                            
037000                   W-ANTAL-LASN                                           
037100                   W-KVBO                                                 
037200     EJECT                                                                
037300     .                                                                    
037400 Z-FINIT SECTION.                                                         
037500                                                                          
037600     CLOSE W612PP                                                         
037700           UTFIL                                                          
037800                                                                          
037900     MOVE 'S' TO POSTSUM-OPKOD                                            
038000     CALL POSTSUM USING POSTSUM-PARM                                      
038100     .                                                                    
038200     EJECT                                                                
038300 S01-NOLLSTALL SECTION.                                                   
038400                                                                          
038500     MOVE SPACE TO UT-IDDC                                                
038600                   UT-IDKUNDRF                                            
038700                   UT-IDLBBET                                             
038800                   UT-TEINFO                                              
038900     MOVE ZERO  TO UT-IDKOLLI                                             
039000                   UT-IDKUNDNR                                            
039100                   UT-IDFAKT                                              
039200                   UT-IDARTNR-KOLLI                                       
039300                   UT-IDARTNR-NEW                                         
039400                   UT-IDARTNR-PRIO                                        
039500                   UT-IDARTNR-BO                                          
039600                   UT-DABERANK                                            
039700     .                                                                    
039800     EJECT                                                                
039900 S11-LAES-W612PP SECTION.                                                 
040000                                                                          
040100     READ W612PP INTO PARM-AREA                                           
040200     AT END                                                               
040300        SET END-OF-W612PP TO TRUE                                         
040400                                                                          
040500     NOT AT END                                                           
040600        MOVE 'INFIL'    TO POSTSUM-FDNAMN                                 
040700        MOVE 'W61232D1' TO POSTSUM-DDNAMN2                                
040800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
040900        CALL POSTSUM USING POSTSUM-PARM                                   
041000     END-READ                                                             
041100     .                                                                    
041200     EJECT                                                                
041300 S12-SKRIV-UTFIL SECTION.                                                 
041400                                                                          
041500     WRITE UT-POST FROM UT-AREA                                           
041600                                                                          
041700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
041800     MOVE 'W61232 '  TO POSTSUM-FDNAMN                                    
041900     MOVE 'W61232D2' TO POSTSUM-DDNAMN2                                   
042000     CALL POSTSUM USING POSTSUM-PARM                                      
042100     .                                                                    
042200     EJECT                                                                
042300* --- IMS SEKTIONER ---                                                   
042400     SKIP3                                                                
042500 IMS-GU-INLC-WLINLC11-F SECTION.                                          
042600                                                                          
042700     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
042800                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
042900          DELIMITED BY SIZE INTO SSA1                                     
043000     MOVE '  GE' TO GODK-STATUSKODER                                      
043100     CALL CBLTDLI USING GU INLC-PCB DLI-IO-INLC SSA1                      
043200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500     SKIP3                                                                
043600 IMS-GN-INLC-WLINLC11 SECTION.                                            
043700                                                                          
043800     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
043900                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
044000          DELIMITED BY SIZE INTO SSA1                                     
044100     MOVE '  GE' TO GODK-STATUSKODER                                      
044200     CALL CBLTDLI USING GN INLC-PCB DLI-IO-INLC SSA1                      
044300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
044400     PERFORM IMS-STATUSKONTROLL                                           
044500     .                                                                    
044600     EJECT                                                                
044700 IMS-GU-WLINLD01 SECTION.                                                 
044800                                                                          
044900     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
045000                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
045100          DELIMITED BY SIZE INTO SSA1                                     
045200     MOVE '  GE' TO GODK-STATUSKODER                                      
045300     CALL CBLTDLI USING GU INLD-PCB DLI-IO-INLD SSA1                      
045400                                                                          
045500     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     SKIP3                                                                
045900 IMS-GN-WLINLD01 SECTION.                                                 
046000                                                                          
046100     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
046200                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
046300          DELIMITED BY SIZE INTO SSA1                                     
046400     MOVE '  GE' TO GODK-STATUSKODER                                      
046500     CALL CBLTDLI USING GN INLD-PCB DLI-IO-INLD SSA1                      
046600     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     .                                                                    
046900     EJECT                                                                
047000 IMS-GU-ARTS11 SECTION.                                                   
047100                                                                          
047200     STRING 'WLARTS01(IDARTNR = ' W-IDARTNR-X ')'                         
047300          DELIMITED BY SIZE INTO SSA1                                     
047400     STRING 'WLARTS11(IDDC    = ' W-IDDC  ')'                             
047500          DELIMITED BY SIZE INTO SSA2                                     
047600     MOVE SPACE  TO GODK-STATUSKODER                                      
047700     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-ARTS SSA1 SSA2                
047800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
047900     PERFORM IMS-STATUSKONTROLL                                           
048000     .                                                                    
048100     SKIP3                                                                
048200 IMS-GU-WDK611 SECTION.                                                   
048300                                                                          
048400     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
048500          DELIMITED BY SIZE INTO SSA1                                     
048600     STRING 'WDK611  (KDSEGKEY= ' W-KDSEGKEY-X ')'                        
048700          DELIMITED BY SIZE INTO SSA2                                     
048800     MOVE SPACE  TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
049000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     SKIP3                                                                
049400 IMS-GHU-WL630111 SECTION.                                                
049500                                                                          
049600     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
049700          DELIMITED BY SIZE INTO SSA1                                     
049800     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
049900          DELIMITED BY SIZE INTO SSA2                                     
050000     MOVE SPACE TO GODK-STATUSKODER                                       
050100     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-GX63 SSA1 SSA2                
050200     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSKONTROLL                                           
050400     EJECT                                                                
050500     .                                                                    
050600 IMS-STATUSKONTROLL SECTION.                                              
050700                                                                          
050800     SET STATUS-IX TO 1                                                   
050900     SEARCH GODK-STATUS                                                   
051000       AT END                                                             
051100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051200           DELIMITED BY SIZE INTO FELTEXT                                 
051300         DISPLAY FELTEXT                                                  
051400         CALL FELLOG                                                      
051500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051600         CONTINUE                                                         
051700     END-SEARCH                                                           
051800     .                                                                    
