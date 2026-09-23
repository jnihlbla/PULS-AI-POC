000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2210300.                                                
000400 AUTHOR.         G KJELLSON.                                              
000500 DATE-WRITTEN.   AUG 2012.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSAR GAMLA LARM 200/210/222                                    
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLXXBU (WDR5 HTYP 2223)                    
001300*        PROGRAMMET LÄSER              WDK7                               
001320*                                      WDB6                               
001400*                                                                         
001500*    KOPIERAT FRÅN W2210400 OCH ANPASSAT FÖR KINA                         
001600*                                                                         
001700*    ÄNDRINGAR:                                                           
001800*                                                                         
001900*                                                                         
002000*                                                                         
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
004000*    -COPY WY2000W1                                                       
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W2210300'.            
004210 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
004220 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
004300 01  CHKP-VAR.                                                            
004400 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004500 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004600 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004700 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004800 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004900 03  CHKP-MAX                    PIC S9(3)   VALUE +5.                    
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 01  RKOD                        PIC S9(4)        COMP SYNC.              
005300 01  WS-IDANSK                   PIC S9(3)   COMP-3 VALUE ZERO.           
005400 01  WS-TISENBEK                 PIC X(7)    VALUE SPACE.                 
005500 01  WS-KDLARM                   PIC 9(3)    VALUE ZERO.                  
005600 01  WS-LAGERTILLG               PIC S9(9)   COMP-3.                      
005700 01  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005800                                                                          
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300     SKIP3                                                                
006400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500 01  FILLER REDEFINES DAGENS-DATUM.                                       
006600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006900 01  DAGENS-DATUM-MINUS-2VV      PIC 9(6)    VALUE ZERO.                  
007000                                                                          
007100*    -COPY WWDCKONS                                                       
007200                                                                          
007300     SKIP3                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008100*                                                                         
008200     EJECT                                                                
008300*--------------------------------------- PARAMETRAR TILL POSTSUM          
008400                                                                          
008500*01  -COPY W0005      -PRE POSTSUM-                                       
008600     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000     SKIP3                                                                
009100***************************************************************           
009200*            N Y C K L A R   T I L L   D L I                              
009300***************************************************************           
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-IDARTNR-X.                                                     
009600         05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.              
009700     03  W-IDDC-X.                                                        
009800         05  W-IDDC          PIC X(2)    VALUE SPACE.                     
009900     03  W-IDDC-B6-X.                                                     
010000         05  W-IDDC-B6       PIC X(2)    VALUE SPACE.                     
010100     03  W-IDLEVNR-X.                                                     
010200         05  W-IDLEVNR       PIC X(5)    VALUE SPACE.                     
010300     03  W-DALEVBSK-X.                                                    
010400         05  W-DALEVBSK      PIC  9(8)   VALUE ZERO.                      
010500     03  W-WDGXKEY-ROT-X.                                                 
010600         05  W-WDGXKEY-IDHTYP      PIC X(04)    VALUE '2223'.             
010700         05  W-WDGXKEY-IDANSK      PIC S9(3)    COMP-3 VALUE ZERO.        
010800         05  W-WDGXKEY-LW          PIC X(24)    VALUE LOW-VALUE.          
010900     03  W-WDGXKEY-X.                                                     
011000         05  W-WDGXKEY-TISENBEK  PIC X(7)    VALUE SPACE.                 
011100         05  W-WDGXKEY-KDLARM    PIC S9(3)   COMP-3 VALUE ZERO.           
011200     03  W-KDLARM-MAX.                                                    
011300         05  W-WDGXKEY-KDLARM-MAX  PIC S9(3)   COMP-3 VALUE 222.          
011400     03  W-KDLARM-MIN.                                                    
011500         05  W-WDGXKEY-KDLARM-MIN  PIC S9(3)   COMP-3 VALUE 200.          
011600     03  W-IDHTYP-X.                                                      
011700         05  W-IDHTYP            PIC X(4)    VALUE '2223'.                
011800     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FINNS                       VALUE '  '.                  
012200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012500     88  IMS-EJ-OK                           VALUE 'XD'.                  
012600     SKIP2                                                                
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     SKIP3                                                                
013000 01  SSA1                        PIC X(128).                              
013100 01  SSA2                        PIC X(64).                               
013200 01  SSA3                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNKTIONSKODER                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013900     SKIP3                                                                
014000 01  DLI-IO-AREA.                                                         
014100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
014200     SKIP3                                                                
014300     03  WLXXBU01 REDEFINES IO-AREA.                                      
014400*        05  -COPY WDGX2223  -PRE XXBU-                                   
014500     EJECT                                                                
014600     03  WLXXBU11 REDEFINES IO-AREA.                                      
014700*        05  -COPY WDGX2224  -PRE XXBU-                                   
014800     EJECT                                                                
014900*    ---  DLI INPUT-OUTPUT AREA                                           
015000 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDK711   '.            
015100 01  DLI-IO-WDK711.                                                       
015200*    03  -COPY WDK711.                                                    
015300     SKIP3                                                                
015400 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDK722   '.            
015500 01  DLI-IO-WDK722.                                                       
015600*    03  -COPY WDK722.                                                    
015700     SKIP3                                                                
015800 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDB601   '.            
015900 01  DLI-IO-WDB601.                                                       
016000*    03  -COPY WDB601.                                                    
016100     EJECT                                                                
016200 LINKAGE SECTION.                                                         
016300                                                                          
016400*01  -COPY W0009  -PRE MSG-                                               
016500     EJECT                                                                
016600*01  -COPY W0008  -PRE XXBU-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900*01  -COPY W0008  -PRE WDK7-                                              
017000     05  FILLER                  PIC X.                                   
017100     EJECT                                                                
017200*01  -COPY W0008  -PRE WDB6-                                              
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500 PROCEDURE DIVISION  USING MSG-PCB XXBU-PCB WDK7-PCB WDB6-PCB.            
017600 MAIN SECTION.                                                            
017700     ENTRY 'DLITCBL' USING MSG-PCB XXBU-PCB WDK7-PCB WDB6-PCB.            
017800                                                                          
017900     SKIP2                                                                
018000     PERFORM A-INIT                                                       
018100     PERFORM IMS-GET-XXBU-ROT                                             
018200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
018300        IF CHKP-ANT > CHKP-MAX                                            
018400          PERFORM X-TAG-CHECKPOINT                                        
018500        END-IF                                                            
018600                                                                          
018700        PERFORM IMS-GET-XXBU-LARM                                         
018800        PERFORM UNTIL SEGMENT-SAKNAS                                      
018900           MOVE XXBU-2224-IDARTNR    TO W-IDARTNR                         
019000           IF XXBU-2224-IDDC NOT = DCS-IDDC                               
019100              MOVE XXBU-2224-IDDC    TO W-IDDC-B6                         
019200              PERFORM IMS-GU-WDB601                                       
019300              IF SEGMENT-SAKNAS                                           
019400                 MOVE SPACE TO DCS-IDDC                                   
019500                               DCS-KDDC                                   
019600              END-IF                                                      
019700           END-IF                                                         
019710                                                                          
019800           IF DCS-NDC-CN                                                  
019801           OR (DCS-NDC-NA AND DCS-USA)                                    
019810             MOVE XXBU-2224-IDDC    TO W-IDDC                             
019900             PERFORM IMS-GU-WDK711                                        
020000             IF SEGMENT-FINNS                                             
020200                PERFORM IMS-GNP-WDK722                                    
020300             END-IF                                                       
020400             IF SEGMENT-FINNS                                             
020500                IF XXBU-2224-KDLARM = 200                                 
020600                   IF SLAG-KVUTRS = ZERO                                  
020700                      PERFORM IMS-DLET-XXBU                               
020800                   END-IF                                                 
020900                END-IF                                                    
021000                IF XXBU-2224-KDLARM = 210                                 
021100                   IF  SLAG-KVROS-BULK  = ZERO                            
021200                   AND SLAG-KVROS-DAG   = ZERO                            
021300                      PERFORM IMS-DLET-XXBU                               
021400                   END-IF                                                 
021500                END-IF                                                    
021600                IF XXBU-2224-KDLARM = 222                                 
021700*                -- LARM = "SÄK.LAGER UNDERSKRIDET"                       
021800*                -- BERÄKNA LAGERTILLGÅNG                                 
021900                   COMPUTE WS-LAGERTILLG = SLAG-KVLS                      
022000                       + SLAG-KVAKS-SDC + SLAG-KVAKS-PAV                  
022100                       - SLAG-KVRESS                                      
022200                       - SLAG-KVROS-BULK                                  
022300                       - SLAG-KVROS-DAG                                   
022400                   IF XLAG-KVSLAGER <= WS-LAGERTILLG                      
022500                     PERFORM IMS-DLET-XXBU                                
022600                   END-IF                                                 
022700                END-IF                                                    
022800             ELSE                                                         
022900                PERFORM IMS-DLET-XXBU                                     
023000             END-IF                                                       
023010           END-IF                                                         
023100           PERFORM IMS-GET-XXBU-LARM                                      
023200        END-PERFORM                                                       
023300        PERFORM IMS-GET-XXBU-ROT                                          
023400     END-PERFORM                                                          
023500                                                                          
023600                                                                          
023700     PERFORM Z-FINIT                                                      
023800                                                                          
023900     MOVE ZERO TO RETURN-CODE                                             
024000     GOBACK                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 A-INIT SECTION.                                                          
024310     MOVE 'A-INIT '      TO CURRENT-SECTION                               
024400     SKIP2                                                                
024500     ACCEPT DAGENS-DATUM FROM DATE                                        
024600                                                                          
026600     PERFORM IMS-RESTART                                                  
026700                                                                          
026800     MOVE 'W22103'           TO POSTSUM-PROGNAMN                          
026900                                                                          
027000     .                                                                    
027100     EJECT                                                                
027200 Z-FINIT SECTION.                                                         
027300                                                                          
027400     MOVE 'S'                TO POSTSUM-OPKOD                             
027500     CALL POSTSUM USING POSTSUM-PARM                                      
027600     .                                                                    
027700     EJECT                                                                
027800 X-TAG-CHECKPOINT   SECTION.                                              
027810     MOVE 'X-TAG-CHECKPOINT   '   TO CURRENT-SECTION                      
027900                                                                          
028000* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
028100                                                                          
028200     PERFORM IMS-CHECKPOINT                                               
028300     MOVE ZERO TO CHKP-ANT                                                
028400* --- LÄS OM DATABAS                                                      
028500     MOVE WS-IDANSK TO W-WDGXKEY-IDANSK                                   
028600     PERFORM IMS-GU-XXBU-ROT                                              
028700     .                                                                    
028800     EJECT                                                                
028900* --- IMS SEKTIONER ---                                                   
029000     SKIP3                                                                
029100 IMS-GET-XXBU-ROT SECTION.                                                
029110     MOVE 'IMS-GET-XXBU-ROT '    TO DBS-SECTION                           
029200                                                                          
029300     STRING 'WLXXBU01(IDHTYP   =' W-IDHTYP-X ')  '                        
029400          DELIMITED BY SIZE INTO SSA1                                     
029500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
029600     CALL CBLTDLI USING GN XXBU-PCB DLI-IO-AREA SSA1                      
029700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSKONTROLL                                           
029900                                                                          
030000     IF SEGMENT-FINNS                                                     
030100        MOVE XXBU-2223-IDANSK TO WS-IDANSK                                
030200        MOVE 'WLXXBU01'     TO POSTSUM-FDNAMN                             
030300        MOVE 'GN  '         TO POSTSUM-DDNAMN2                            
030400        MOVE ' '            TO POSTSUM-TRANSTYP                           
030500        CALL POSTSUM USING POSTSUM-PARM                                   
030600     END-IF                                                               
030700     .                                                                    
030800     SKIP3                                                                
030900 IMS-GU-XXBU-ROT SECTION.                                                 
030910     MOVE 'IMS-GU-XXBU-ROT '  TO DBS-SECTION                              
031000                                                                          
031100     STRING 'WLXXBU01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
031200          DELIMITED BY SIZE INTO SSA1                                     
031300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031400     CALL CBLTDLI USING GU XXBU-PCB DLI-IO-AREA SSA1                      
031500     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
031600     PERFORM IMS-STATUSKONTROLL                                           
031700                                                                          
031800     IF SEGMENT-FINNS                                                     
031900        MOVE 'WLXXBU01'     TO POSTSUM-FDNAMN                             
032000        MOVE 'GU  '         TO POSTSUM-DDNAMN2                            
032100        MOVE ' '            TO POSTSUM-TRANSTYP                           
032200        CALL POSTSUM USING POSTSUM-PARM                                   
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 IMS-GET-XXBU-LARM SECTION.                                               
032610     MOVE 'IMS-GET-XXBU-LARM  '  TO DBS-SECTION                           
032700                                                                          
032800     STRING 'WLXXBU11(KDLARM  >=' W-KDLARM-MIN                            
032900                    '&KDLARM  <=' W-KDLARM-MAX ')'                        
033000          DELIMITED BY SIZE INTO SSA1                                     
033100     MOVE '  GE' TO GODK-STATUSKODER                                      
033200     CALL CBLTDLI USING GHNP XXBU-PCB DLI-IO-AREA SSA1                    
033300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
033400     PERFORM IMS-STATUSKONTROLL                                           
033500                                                                          
033600     IF SEGMENT-FINNS                                                     
033700        MOVE XXBU-2224-KDLARM   TO WS-KDLARM                              
033800        MOVE XXBU-2224-TISENBEK TO WS-TISENBEK                            
033900        MOVE 'WLXXBU11'     TO POSTSUM-FDNAMN                             
034000        MOVE 'GHNP'         TO POSTSUM-DDNAMN2                            
034100        MOVE ' '            TO POSTSUM-TRANSTYP                           
034200        CALL POSTSUM USING POSTSUM-PARM                                   
034300     END-IF                                                               
034400     .                                                                    
034500     SKIP3                                                                
034600 IMS-DLET-XXBU SECTION.                                                   
034610     MOVE 'IMS-DLET-XXBU  '    TO DBS-SECTION                             
034700                                                                          
035200     MOVE '  ' TO GODK-STATUSKODER                                        
035300     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-AREA                         
035400     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
035500     PERFORM IMS-STATUSKONTROLL                                           
035600     ADD +1    TO CHKP-ANT                                                
035700                                                                          
035800     MOVE 'WLXXBU11'     TO POSTSUM-FDNAMN                                
035900     MOVE 'DLET'         TO POSTSUM-DDNAMN2                               
036000     MOVE WS-KDLARM      TO POSTSUM-TRANSTYP                              
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     .                                                                    
036300     EJECT                                                                
036400 IMS-GU-WDK711    SECTION.                                                
036410     MOVE 'IMS-GU-WDK711  '  TO DBS-SECTION                               
036500                                                                          
036600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
036700          DELIMITED BY SIZE INTO SSA1                                     
036800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
036900          DELIMITED BY SIZE INTO SSA2                                     
037000     MOVE '  GE' TO GODK-STATUSKODER                                      
037100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
037200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
037300     PERFORM IMS-STATUSKONTROLL                                           
037400     .                                                                    
037500     EJECT                                                                
037600 IMS-GNP-WDK722    SECTION.                                               
037610     MOVE 'IMS-GNP-WDK722  '  TO DBS-SECTION                              
037700                                                                          
037800     MOVE 'WDK722 ' TO SSA1                                               
037900     MOVE '  GE'    TO GODK-STATUSKODER                                   
038000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
038100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     EJECT                                                                
038500 IMS-GU-WDB601    SECTION.                                                
038510     MOVE 'IMS-GU-WDB601  '  TO DBS-SECTION                               
038600                                                                          
038700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
038800          DELIMITED BY SIZE INTO SSA1                                     
038900     MOVE '  GE' TO GODK-STATUSKODER                                      
039000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
039100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     EJECT                                                                
039500******************************************************'                   
039600 IMS-RESTART SECTION.                                                     
039610     MOVE 'IMS-RESTART  '  TO DBS-SECTION                                 
039700     SKIP2                                                                
039800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039900     MOVE '  ' TO GODK-STATUSKODER                                        
040000     CALL CBLTDLI USING XRST MSG-PCB                                      
040100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
040200                        CHKP-AREA-LENGTH CHKP-AREA                        
040300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040400     PERFORM IMS-STATUSKONTROLL                                           
040500     .                                                                    
040600     EJECT                                                                
040700 IMS-CHECKPOINT SECTION.                                                  
040710     MOVE 'IMS-CHECKPOINT '   TO DBS-SECTION                              
040800     SKIP2                                                                
040900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
041000     MOVE '  XD' TO GODK-STATUSKODER                                      
041100     CALL CBLTDLI USING CHKP MSG-PCB                                      
041200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
041300                        CHKP-AREA-LENGTH CHKP-AREA                        
041400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041500     PERFORM IMS-STATUSKONTROLL                                           
041600                                                                          
041700     IF IMS-EJ-OK                                                         
041800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
041900       DISPLAY FELTEXT                                                    
042000       CALL FELLOG                                                        
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 IMS-STATUSKONTROLL SECTION.                                              
042500     SKIP2                                                                
042600     SET STATUS-IX TO 1                                                   
042700     SEARCH GODK-STATUS                                                   
042800       AT END                                                             
042900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
043000           DELIMITED BY SIZE INTO FELTEXT                                 
043100         DISPLAY FELTEXT                                                  
043200         CALL FELLOG                                                      
043300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043400         CONTINUE                                                         
043500     END-SEARCH                                                           
043600     .                                                                    
043700     EJECT                                                                
043800*    -COPY WY2000P1                                                       
