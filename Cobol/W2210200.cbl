000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2210200.                                                
000400*AUTHOR.         HENRIK ARONSSON.                                         
000500*DATE-WRITTEN.   JAN 1993.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER INFIL MED ÄNDRINGAR I                           
001100*        ÖVERSÄTTNINGSTABELLEN OCH ÄNDRAR DENNA                           
001200*        SAMT FLYTTA LARMEN TILL RÄTT 'LARMANSKAFFARE'.                   
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLXXBU (WDR2) (2 ST PCB:N)                 
001500*        PROGRAMMET UPPDATERAR WLXXBX (WDR2)                              
001600*        PROGRAMMET LÄSER      WLORDP (WDA5)                              
001700*                                                                         
001800*        VARJE PCB HAR SIN EGEN IO-AREA.                                  
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- FIL MED ÄNDRINGAR I 'ÖVERSÄTTNINGSTABLLEN'                 
003300*          --- DVS ANSKAFFARE SOM SKALL BYTA LARMKÖ                       
003400     SELECT W22102                     ASSIGN TO W22102D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W22102                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300     SKIP2                                                                
004400*01  -COPY W22102      -L.                                                
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W2210200'.            
005100 01  WS-IDORDNR5                 PIC 9(5)    VALUE ZERO.                  
005200 01  CHKP-VAR.                                                            
005300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005700 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005800 03  CHKP-MAX                    PIC S9(3)   VALUE +50.                   
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500                                                                          
006600 77  FLYTT-SW                    PIC X       VALUE 'N'.                   
006700     88  LARM-SKALL-FLYTTAS                  VALUE 'J'.                   
006800                                                                          
006900 77  W22102-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-W22102                       VALUE 'J'.                   
007100                                                                          
007200     EJECT                                                                
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800     EJECT                                                                
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  IN-AREA-START               PIC X(24)   VALUE                        
009000                                             'IN-AREA-START'.             
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W22102     -PRE IN-                                       
009400*                                                                         
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900                                                                          
010000     03  W-WDGXKEY-2223-X.                                                
010100         05  FILLER              PIC X(4)    VALUE '2223'.                
010200         05  W-IDANSK-2223       PIC S9(3)   VALUE ZERO COMP-3.           
010300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
010400                                                                          
010500     03  W-WDGXKEY-2224-X.                                                
010600         05  W-TISENBEK.                                                  
010700             07  W-TISENBEK-DAG  PIC S9(7)   VALUE ZERO COMP-3.           
010800             07  W-TISENBEK-KL   PIC S9(7)   VALUE ZERO COMP-3.           
010900         05  W-KDLARM            PIC S9(3)   VALUE ZERO COMP-3.           
011000                                                                          
011100                                                                          
011200     03  W-WDGXKEY-2231-X.                                                
011300         05  FILLER              PIC X(4)    VALUE '2231'.                
011400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011500                                                                          
011600     03  W-WDGXKEY-2232-X.                                                
011700         05  W-IDANSK-2232       PIC S9(3)   VALUE ZERO COMP-3.           
011800         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
011900                                                                          
012000                                                                          
012100     03  W-WDA501KY-X.                                                    
012200         05  W-IDGMTREF.                                                  
012300             07  W-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.           
012400             07  W-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.           
012500             07  W-IDKUNDRF      PIC X(10).                               
012600             07  FILLER REDEFINES W-IDKUNDRF.                             
012700                 09  W-IDORDNR5  PIC 9(5).                                
012800                 09  FILLER      PIC X(5).                                
012900             07  FILLER REDEFINES W-IDKUNDRF.                             
013000                 09  W-IDORDNR7  PIC 9(7).                                
013100                 09  FILLER      PIC X(3).                                
013200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013300         05  W-IDLOPNR           PIC S9(3)   VALUE ZERO COMP-3.           
013400                                                                          
013500     SKIP2                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014200     88  IMS-EJ-OK                           VALUE 'XD'.                  
014300     SKIP2                                                                
014400 01  GODK-STATUSKODER.                                                    
014500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014600     SKIP3                                                                
014700 01  SSA1                        PIC X(64).                               
014800 01  SSA2                        PIC X(64).                               
014900     EJECT                                                                
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300*    ---  DLI INPUT-OUTPUT AREA                                           
015400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-1'.        
015500     SKIP3                                                                
015600 01  DLI-IO-AREA-1.                                                       
015700     03  IO-AREA-1               PIC X(150) VALUE SPACE.                  
015800     SKIP3                                                                
015900     03  WLXXBU01 REDEFINES IO-AREA-1.                                    
016000*        05  -COPY WDGX2223  -PRE XXBU-1-                                 
016100     SKIP3                                                                
016200     03  WLXXBU11 REDEFINES IO-AREA-1.                                    
016300*        05  -COPY WDGX2224  -PRE XXBU-1-                                 
016400     EJECT                                                                
016500                                                                          
016600*    ---  DLI INPUT-OUTPUT AREA                                           
016700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-2'.        
016800     SKIP3                                                                
016900 01  DLI-IO-AREA-2.                                                       
017000     03  IO-AREA-2               PIC X(150) VALUE SPACE.                  
017100     SKIP3                                                                
017200     03  WLXXBU01 REDEFINES IO-AREA-2.                                    
017300*        05  -COPY WDGX2223  -PRE XXBU-2-                                 
017400     SKIP3                                                                
017500     03  WLXXBU11 REDEFINES IO-AREA-2.                                    
017600*        05  -COPY WDGX2224  -PRE XXBU-2-                                 
017700     EJECT                                                                
017800                                                                          
017900*    ---  DLI INPUT-OUTPUT AREA                                           
018000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-3'.        
018100     SKIP3                                                                
018200 01  DLI-IO-AREA-3.                                                       
018300     03  IO-AREA-3               PIC X(150) VALUE SPACE.                  
018400     SKIP3                                                                
018500     03  WLXXBX01 REDEFINES IO-AREA-3.                                    
018600*        05  -COPY WDGX01    -PRE XXBX-                                   
018700     SKIP3                                                                
018800     03  WLXXBX11 REDEFINES IO-AREA-3.                                    
018900*        05  -COPY WDGX2232  -PRE XXBX-                                   
019000     EJECT                                                                
019100                                                                          
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-4'.        
019400     SKIP3                                                                
019500 01  DLI-IO-AREA-4.                                                       
019600     03  IO-AREA-4               PIC X(300) VALUE SPACE.                  
019700     SKIP3                                                                
019800     03  WLORDP01 REDEFINES IO-AREA-4.                                    
019900*        05  -COPY WDA501  -PRE ORDP-                                     
020000     EJECT                                                                
020100 LINKAGE SECTION.                                                         
020200                                                                          
020300*01  -COPY W0009  -PRE MSG-                                               
020400     EJECT                                                                
020500*01  -COPY W0008  -PRE XXBU-1-                                            
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008  -PRE XXBU-2-                                            
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE XXBX-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE ORDP-                                              
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING MSG-PCB XXBU-1-PCB XXBU-2-PCB XXBX-PCB         
021800                           ORDP-PCB.                                      
021900     ENTRY 'DLITCBL' USING MSG-PCB XXBU-1-PCB XXBU-2-PCB XXBX-PCB         
022000                           ORDP-PCB.                                      
022100                                                                          
022200     SKIP2                                                                
022300     PERFORM A-INIT                                                       
022400     PERFORM IMS-GU-XXBX01                                                
022500     PERFORM IMS-GHNP-XXBX11                                              
022600     PERFORM S01-LAES-W22102                                              
022700     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-W22102                        
022800                                                                          
022900       IF XXBX-2232-IDANSK = IN-IDANSK                                    
023000         IF XXBX-2232-IDANSK-LARM = IN-IDANSK-LARM                        
023100*--------  'ÖVERSÄTTNING' SAMMA SOM INNAN                                 
023200           CONTINUE                                                       
023300         ELSE                                                             
023400           PERFORM B-ANDRA-OVERSATTN-FLYTTA-LARM                          
023500         END-IF                                                           
023600         PERFORM IMS-GHNP-XXBX11                                          
023700         PERFORM S01-LAES-W22102                                          
023800       ELSE                                                               
023900         IF XXBX-2232-IDANSK > IN-IDANSK                                  
024000           PERFORM C-LAEGG-UPP-OVERSAETTNING                              
024100           PERFORM S01-LAES-W22102                                        
024200         ELSE                                                             
024300           PERFORM IMS-GHNP-XXBX11                                        
024400         END-IF                                                           
024500       END-IF                                                             
024600                                                                          
024700       IF CHKP-ANT > CHKP-MAX                                             
024800         PERFORM X-TAG-CHECKPOINT                                         
024900       END-IF                                                             
025000                                                                          
025100     END-PERFORM                                                          
025200                                                                          
025300     PERFORM Z-FINIT                                                      
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     PERFORM IMS-RESTART                                                  
026200                                                                          
026300     OPEN INPUT W22102                                                    
026400                                                                          
026500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026600     .                                                                    
026700     EJECT                                                                
026800 B-ANDRA-OVERSATTN-FLYTTA-LARM SECTION.                                   
026900                                                                          
027000     MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK-2223                          
027100     PERFORM IMS-GU-XXBU01-1                                              
027200     IF SEGMENT-FINNS                                                     
027300*----- KONTROLLERA OM DET FINNS NÅGRA LARM                                
027400       PERFORM IMS-GHNP-XXBU11-1                                          
027500       PERFORM UNTIL SEGMENT-SAKNAS                                       
027600         PERFORM BA-KOLLA-OM-LARM-SKA-FLYTTAS                             
027700         IF LARM-SKALL-FLYTTAS                                            
027800           PERFORM BB-FLYTTA-LARM                                         
027900         END-IF                                                           
028000         PERFORM IMS-GHNP-XXBU11-1                                        
028100       END-PERFORM                                                        
028400     END-IF                                                               
028410     PERFORM BC-UPPD-OVERSATTN-ANSKAFFARE                                 
028500     .                                                                    
028600     EJECT                                                                
028700 BA-KOLLA-OM-LARM-SKA-FLYTTAS SECTION.                                    
028800                                                                          
028900     MOVE XXBU-1-2224-IDDISTR  TO W-IDDISTR                               
029000     MOVE XXBU-1-2224-IDKUNDNR TO W-IDKUNDNR                              
029100     MOVE SPACE                TO W-IDKUNDRF                              
029200     MOVE XXBU-1-2224-IDORDNR7 TO WS-IDORDNR5                             
029300     MOVE WS-IDORDNR5          TO W-IDORDNR5                              
029400     MOVE XXBU-1-2224-IDARTNR  TO W-IDARTNR                               
029500     MOVE XXBU-1-2224-IDLOPNR  TO W-IDLOPNR                               
029600                                                                          
029700     PERFORM IMS-GU-ORDP01                                                
029800     IF SEGMENT-FINNS                                                     
029900       IF IN-IDANSK = ORDP-RAD-IDANSK                                     
030000         MOVE JA  TO FLYTT-SW                                             
030100       ELSE                                                               
030200         MOVE NEJ TO FLYTT-SW                                             
030300       END-IF                                                             
030400     ELSE                                                                 
030500       MOVE JA  TO FLYTT-SW                                               
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 BB-FLYTTA-LARM SECTION.                                                  
031000                                                                          
031100*--- KOLLA OM 'NYA' LARMKÖN REDAN FINNS                                   
031200                                                                          
031300     MOVE IN-IDANSK-LARM TO W-IDANSK-2223                                 
031400     PERFORM IMS-GU-XXBU01-2                                              
031500     IF SEGMENT-FINNS                                                     
031600       CONTINUE                                                           
031700     ELSE                                                                 
031800       MOVE LOW-VALUE      TO IO-AREA-2                                   
031900       MOVE '2223'         TO XXBU-2-2223-IDHTYP                          
032000       MOVE IN-IDANSK-LARM TO XXBU-2-2223-IDANSK                          
032100       PERFORM IMS-ISRT-XXBU01-2                                          
032200       ADD 1 TO CHKP-ANT                                                  
032300     END-IF                                                               
032400                                                                          
032500*--- FLYTTA-LARM                                                          
032600                                                                          
032700     MOVE IO-AREA-1 TO IO-AREA-2                                          
032800     PERFORM IMS-ISRT-XXBU11-2                                            
032900     PERFORM IMS-DLET-XXBU-1                                              
033000     ADD 2 TO CHKP-ANT                                                    
033100     .                                                                    
033200     EJECT                                                                
033300 BC-UPPD-OVERSATTN-ANSKAFFARE SECTION.                                    
033400                                                                          
033500*--- ÄNDRA ÖVERSÄTTNING AV ANSKAFFARE                                     
033600                                                                          
033700     MOVE IN-IDANSK-LARM TO XXBX-2232-IDANSK-LARM                         
033800                                                                          
033900     PERFORM IMS-REPL-XXBX                                                
034000     ADD 1 TO CHKP-ANT                                                    
034100     .                                                                    
034200     EJECT                                                                
034300 C-LAEGG-UPP-OVERSAETTNING SECTION.                                       
034400                                                                          
034500     MOVE SPACE          TO IO-AREA-3                                     
034600     MOVE IN-IDANSK      TO XXBX-2232-IDANSK                              
034700     MOVE LOW-VALUE      TO XXBX-2232-LOW-VALUE                           
034800     MOVE IN-IDANSK-LARM TO XXBX-2232-IDANSK-LARM                         
034900                                                                          
035000     PERFORM IMS-ISRT-XXBX11                                              
035100     ADD 1 TO CHKP-ANT                                                    
035200     .                                                                    
035300     EJECT                                                                
035400 Z-FINIT SECTION.                                                         
035500                                                                          
035600     CLOSE W22102                                                         
035700                                                                          
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     .                                                                    
036100     EJECT                                                                
036200 S01-LAES-W22102  SECTION.                                                
036300                                                                          
036400     READ W22102 INTO IN-AREA                                             
036500     AT END                                                               
036600        MOVE 999          TO IN-IDANSK                                    
036700        SET END-OF-W22102 TO TRUE                                         
036800                                                                          
036900     NOT AT END                                                           
037000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
037100        MOVE 'W22102'   TO POSTSUM-FDNAMN                                 
037200        MOVE 'W22102D1' TO POSTSUM-DDNAMN2                                
037300        CALL POSTSUM USING POSTSUM-PARM                                   
037400     END-READ                                                             
037500     .                                                                    
037600     EJECT                                                                
037700 X-TAG-CHECKPOINT   SECTION.                                              
037800                                                                          
037900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
038000* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
038100     MOVE XXBX-2232-IDANSK TO W-IDANSK-2232                               
038200                                                                          
038300     PERFORM IMS-CHECKPOINT                                               
038400     MOVE ZERO TO CHKP-ANT                                                
038500                                                                          
038600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
038700     PERFORM IMS-GU-XXBX01                                                
038800     PERFORM IMS-GHNP-XXBX11-KVAL                                         
038900     .                                                                    
039000     EJECT                                                                
039100* --- IMS SEKTIONER ---                                                   
039200     EJECT                                                                
039300                                                                          
039400* --- IMS-ANROP MOT XXBU MED PCB 1                                        
039500                                                                          
039600 IMS-GU-XXBU01-1 SECTION.                                                 
039700                                                                          
039800     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
039900          DELIMITED BY SIZE INTO SSA1                                     
040000     MOVE '  GE' TO GODK-STATUSKODER                                      
040100     CALL CBLTDLI USING GU XXBU-1-PCB DLI-IO-AREA-1 SSA1                  
040200     MOVE XXBU-1-STATUS-CODE TO STATUS-WS                                 
040300     PERFORM IMS-STATUSKONTROLL                                           
040400     .                                                                    
040500     SKIP3                                                                
040600 IMS-GHNP-XXBU11-1 SECTION.                                               
040700                                                                          
040800     MOVE 'WLXXBU11' TO SSA1                                              
040900     MOVE '  GE' TO GODK-STATUSKODER                                      
041000     CALL CBLTDLI USING GHNP XXBU-1-PCB DLI-IO-AREA-1 SSA1                
041100     MOVE XXBU-1-STATUS-CODE TO STATUS-WS                                 
041200     PERFORM IMS-STATUSKONTROLL                                           
041300     .                                                                    
041400     SKIP3                                                                
041500 IMS-DLET-XXBU-1 SECTION.                                                 
041600                                                                          
041700     MOVE '  ' TO GODK-STATUSKODER                                        
041800     CALL CBLTDLI USING DLET XXBU-1-PCB DLI-IO-AREA-1                     
041900     MOVE XXBU-1-STATUS-CODE TO STATUS-WS                                 
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     EJECT                                                                
042300* --- IMS-ANROP MOT XXBU MED PCB 2                                        
042400                                                                          
042500 IMS-GU-XXBU01-2 SECTION.                                                 
042600                                                                          
042700     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
042800          DELIMITED BY SIZE INTO SSA1                                     
042900     MOVE '  GE' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GU XXBU-2-PCB DLI-IO-AREA-2 SSA1                  
043100     MOVE XXBU-2-STATUS-CODE TO STATUS-WS                                 
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400     SKIP3                                                                
043500 IMS-ISRT-XXBU01-2 SECTION.                                               
043600                                                                          
043700     MOVE 'WLXXBU01 ' TO SSA1                                             
043800     MOVE '  II' TO GODK-STATUSKODER                                      
043900     CALL CBLTDLI USING ISRT XXBU-2-PCB DLI-IO-AREA-2 SSA1                
044000     MOVE XXBU-2-STATUS-CODE TO STATUS-WS                                 
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     EJECT                                                                
044400 IMS-ISRT-XXBU11-2 SECTION.                                               
044500                                                                          
044600     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
044700          DELIMITED BY SIZE INTO SSA1                                     
044800     MOVE 'WLXXBU11 ' TO SSA2                                             
044900     MOVE '  II' TO GODK-STATUSKODER                                      
045000     CALL CBLTDLI USING ISRT XXBU-2-PCB DLI-IO-AREA-2 SSA1 SSA2           
045100     MOVE XXBU-2-STATUS-CODE TO STATUS-WS                                 
045200     PERFORM IMS-STATUSKONTROLL                                           
045300     .                                                                    
045400     EJECT                                                                
045500 IMS-GU-XXBX01 SECTION.                                                   
045600                                                                          
045700     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     MOVE '  ' TO GODK-STATUSKODER                                        
046000     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA-3 SSA1                    
046100     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-GHNP-XXBX11 SECTION.                                                 
046600                                                                          
046700     MOVE 'WLXXBX11 ' TO SSA1                                             
046800     MOVE '  GE' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GHNP XXBX-PCB DLI-IO-AREA-3 SSA1                  
047000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     SKIP3                                                                
047400 IMS-GHNP-XXBX11-KVAL SECTION.                                            
047500                                                                          
047600     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
047700          DELIMITED BY SIZE INTO SSA1                                     
047800     MOVE '  ' TO GODK-STATUSKODER                                        
047900     CALL CBLTDLI USING GHNP XXBX-PCB DLI-IO-AREA-3 SSA1                  
048000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSKONTROLL                                           
048200     .                                                                    
048300     SKIP3                                                                
048400 IMS-ISRT-XXBX11 SECTION.                                                 
048500                                                                          
048600     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
048700          DELIMITED BY SIZE INTO SSA1                                     
048800     MOVE 'WLXXBX11 ' TO SSA2                                             
048900     MOVE '  II' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING ISRT XXBX-PCB DLI-IO-AREA-3 SSA1 SSA2             
049100     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     SKIP3                                                                
049500 IMS-REPL-XXBX SECTION.                                                   
049600                                                                          
049700     MOVE '  ' TO GODK-STATUSKODER                                        
049800     CALL CBLTDLI USING REPL XXBX-PCB DLI-IO-AREA-3                       
049900     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
050100     .                                                                    
050200     EJECT                                                                
050300 IMS-GU-ORDP01 SECTION.                                                   
050400                                                                          
050500     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
050600          DELIMITED BY SIZE INTO SSA1                                     
050700     MOVE '  GE' TO GODK-STATUSKODER                                      
050800     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA-4 SSA1                    
050900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     .                                                                    
051200     EJECT                                                                
051300 IMS-RESTART SECTION.                                                     
051400                                                                          
051500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
051600     MOVE '  ' TO GODK-STATUSKODER                                        
051700     CALL CBLTDLI USING XRST MSG-PCB                                      
051800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
051900                        CHKP-AREA-LENGTH CHKP-AREA                        
052000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300     EJECT                                                                
052400 IMS-CHECKPOINT SECTION.                                                  
052500                                                                          
052600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
052700     MOVE '  XD' TO GODK-STATUSKODER                                      
052800     CALL CBLTDLI USING CHKP MSG-PCB                                      
052900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
053000                        CHKP-AREA-LENGTH CHKP-AREA                        
053100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053200     PERFORM IMS-STATUSKONTROLL                                           
053300                                                                          
053400     IF IMS-EJ-OK                                                         
053500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
053600       DISPLAY FELTEXT                                                    
053700       CALL FELLOG                                                        
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 IMS-STATUSKONTROLL SECTION.                                              
054200                                                                          
054300     SET STATUS-IX TO 1                                                   
054400     SEARCH GODK-STATUS                                                   
054500       AT END                                                             
054600         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
054700           DELIMITED BY SIZE INTO FELTEXT-STR                             
054800         DISPLAY FELTEXT                                                  
054900         CALL FELLOG                                                      
055000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055100         CONTINUE                                                         
055200     END-SEARCH                                                           
055300     .                                                                    
