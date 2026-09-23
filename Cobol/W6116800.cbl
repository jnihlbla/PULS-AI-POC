000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6116800.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   98/09/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOLLAR MOT WDF1 ATT IDLANDX2 NOT = NL                            
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- LISTPOSTER INLEVERANSER C2                                 
002500     SELECT W61168                     ASSIGN TO W61168D1.                
002600     SKIP2                                                                
002700*          --- LISTPOSTER INLEVERANS C2                                   
002800     SELECT W61170                     ASSIGN TO W61168D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61168                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W61168      -L.                                                
003900     SKIP3                                                                
004000 FD  W61170                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W61170 -PRE  UT-  -L.                                     
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W6116800'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005210 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005220 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005300                                                                          
005400 77  W61168-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W61168                       VALUE 'J'.                   
005600                                                                          
005700 01  ARBAREOR.                                                            
005800     03  OLD-IDLEVNR             PIC  X(5)   VALUE SPACE.                 
005900     EJECT                                                                
006000 01  WS-DATUM.                                                            
006100     03  WS-DATUM-SEKEL          PIC 9(2)    VALUE 20.                    
006200     03  WS-DATUM-AAMMDD         PIC 9(6).                                
006300                                                                          
006400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500 01  FILLER REDEFINES DAGENS-DATUM.                                       
006600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006900     EJECT                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007700     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
007800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007810     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
007900     SKIP2                                                                
008000*    --- PARAMETRAR TILL ABEND                                            
008100                                                                          
008200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009100*01  -COPY WZ01SEND                                                       
009200     EJECT                                                                
009300                                                                          
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800                                                                          
009900*    --- PARAMETRAR TILL DATKORT                                          
010000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010100                                                                          
010200*01  -COPY WDATKORT                                                       
010300     EJECT                                                                
010400                                                                          
010500 01  INTUT-AREA-START            PIC X(24)   VALUE                        
010600                                 'INTUT-AREA-START'.                      
010700 01  INTUT-AREA.                                                          
010800*    03  FILLER -COPY WZ01REQU  -PRE INTUT-                               
010900*    03  FILLER -COPY WF2102I1  -PRE INTUT-                               
011000     EJECT                                                                
011100                                                                          
011200 01  IN-AREA-START               PIC X(24)   VALUE                        
011300                                 'IN-AREA-START  '.                       
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL W009CIA                                          
011600*01  -COPY W009CIA                                                        
011610*    --- PARAMETRAR TILL W510CURR                                         
011620*01  -COPY W510CURR                                                       
011700                                                                          
011800     EJECT                                                                
011900*    --- AREOR FÖR KOMMUNIKATION                                          
012000 01  WS-ADRESS-INTRA             PIC X(50)                                
012100                                 VALUE 'CARPARTS.PULS.RECEIVEI'.          
012200 01  ERRTEXT.                                                             
012300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
012400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
012500 01  KDRC-DISPLAY                PIC Z(5).                                
012600     SKIP2                                                                
012700                                                                          
012800*01  AREA -COPY W61168     -PRE IN-                                       
012900     EJECT                                                                
013000 01  UT-AREA-START               PIC X(24)   VALUE                        
013100                                 'UT-AREA-START  '.                       
013200     SKIP2                                                                
013300                                                                          
013400*01  AREA -COPY W61170     -PRE UT-                                       
013500     EJECT                                                                
013600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013700*                                                                         
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014000     SKIP3                                                                
014100 01  NYCKLAR-TILL-DLI.                                                    
014200     03  W-IDLEVNR-X.                                                     
014300         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
014400     03  W-IDLEVSUF-X.                                                    
014500         05  W-IDLEVSUF          PIC S9(1)   VALUE 1    COMP-3.           
014600     03  W-WDGXKEY-X.                                                     
014700         05  W-WDGXKEY           PIC X(30)   VALUE SPACE.                 
014800     03  W-KDSEGKEY-X.                                                    
014900         05  W-KDSEGKEY          PIC X(30)   VALUE SPACE.                 
015000     03  W-IDARTNR-X.                                                     
015100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015200     03  W-IDSKYLT-X.                                                     
015300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
015400     03  W-IDGMT-X.                                                       
015500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015700     SKIP3                                                                
015800     SKIP2                                                                
015900*    --- STATUS-KOD FRÅN IMS                                              
016000 01  STATUS-WS                   PIC XX.                                  
016100     88  SEGMENT-FINNS                       VALUE '  '.                  
016200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016400     SKIP2                                                                
016500 01  GODK-STATUSKODER.                                                    
016600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700     SKIP3                                                                
016800 01  SSA1                        PIC X(64).                               
016900 01  SSA2                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNKTIONSKODER                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA01'.                    
017600 01  DLI-IO-WLLEVA01.                                                     
017700*    03  -COPY WDF101                                                     
017800     EJECT                                                                
017900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA14'.                    
018000 01  DLI-IO-WLLEVA14.                                                     
018100*    03  -COPY WDF106                                                     
018200     EJECT                                                                
018300                                                                          
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
018500 01  DLI-IO-WDD311.                                                       
018600*    03 WDD311   -COPY WDD311                                             
018700     EJECT                                                                
018800 LINKAGE SECTION.                                                         
018900                                                                          
019000*01  -COPY W0009  -PRE MSG-                                               
019100                                                                          
019200*01  -COPY W0009  -PRE RECEIVEI-                                          
019300                                                                          
019400*01  -COPY W0008  -PRE LEVA-                                              
019500     05  FILLER                  PIC X.                                   
019600                                                                          
019700*01  -COPY W0008  -PRE WDG2-                                              
019800     05  FILLER                  PIC X.                                   
019900                                                                          
020000*01  -COPY W0008  -PRE WDD3-                                              
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300                                                                          
020400 PROCEDURE DIVISION  USING MSG-PCB                                        
020500                           RECEIVEI-PCB                                   
020600                           LEVA-PCB                                       
020700                           WDG2-PCB                                       
020800                           WDD3-PCB.                                      
020900 MAIN SECTION.                                                            
021000     ENTRY 'DLITCBL' USING MSG-PCB                                        
021100                           RECEIVEI-PCB                                   
021200                           LEVA-PCB                                       
021300                           WDG2-PCB                                       
021400                           WDD3-PCB.                                      
021500                                                                          
021600     PERFORM A-INIT                                                       
021700                                                                          
021800     PERFORM S01-LAES-W61168                                              
021900     PERFORM UNTIL END-OF-W61168                                          
022000                                                                          
022100          IF IN-IDLEVNR NOT = OLD-IDLEVNR                                 
022200             MOVE IN-IDLEVNR TO W-IDLEVNR                                 
022300             PERFORM IMS-GET-WDF106                                       
022400             IF SEGMENT-FINNS                                             
022500               IF ADR-IDLANDX2 NOT = 'NL'                                 
022600                  PERFORM C-SKRIV-UTAREA                                  
022700                  PERFORM D-NY-INTRASTAT                                  
022800                  MOVE IN-IDLEVNR TO OLD-IDLEVNR                          
022900               END-IF                                                     
023000             ELSE                                                         
023100               DISPLAY 'IDLEVNR SAKNAS '  IN-IDLEVNR                      
023200             END-IF                                                       
023300          ELSE                                                            
023400             IF ADR-IDLANDX2 NOT = 'NL'                                   
023500                PERFORM C-SKRIV-UTAREA                                    
023600                PERFORM D-NY-INTRASTAT                                    
023700             END-IF                                                       
023800          END-IF                                                          
023900                                                                          
024000       PERFORM S01-LAES-W61168                                            
024100     END-PERFORM                                                          
024200                                                                          
024300                                                                          
024400     PERFORM Z-FINIT                                                      
024500                                                                          
024600     MOVE ZERO TO RETURN-CODE                                             
024700     GOBACK                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 A-INIT SECTION.                                                          
025100                                                                          
025200     OPEN INPUT  W61168                                                   
025300                                                                          
025400     OPEN OUTPUT W61170                                                   
025500     PERFORM S80-OPEN-INTRA                                               
025600                                                                          
025700     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
025800     MOVE D-AAR              TO DAGENS-DATUM-AAR                          
025810                                W-DATE-AAMM(1:2)                          
025900     MOVE D-MAANAD           TO DAGENS-DATUM-MAANAD                       
026000                                W-DATE-AAMM(3:2)                          
026100     MOVE D-DAG              TO DAGENS-DATUM-DAG                          
026200                                                                          
026300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026400     .                                                                    
026500     EJECT                                                                
026600 C-SKRIV-UTAREA SECTION.                                                  
026700                                                                          
026800     MOVE IN-AREA TO UT-AREA                                              
026900     PERFORM S11-SKRIV-W61170                                             
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300 D-NY-INTRASTAT SECTION.                                                  
027400                                                                          
027500     MOVE DAGENS-DATUM        TO WS-DATUM-AAMMDD                          
027600     MOVE WS-DATUM            TO INTUT-DAEXDAT                            
027700                                                                          
027800     MOVE 878787              TO INTUT-TIEXTID                            
027900     MOVE 'INT'               TO INTUT-IDPTYP                             
028000     MOVE SPACE               TO INTUT-IDLANDX3-BET                       
028100     MOVE ADR-IDLANDX2        TO INTUT-IDLANDX3-SEND                      
028200     MOVE 'NL'                TO INTUT-IDLANDX3-REC                       
028210     MOVE 'NL800213609B07'    TO INTUT-IDVAT-BET                          
028300                                                                          
028400     MOVE 'EUR'               TO INTUT-KDVALISO                           
028500                                 INTUT-KDVALISO-SEND                      
028600                                 CURR-KDVALISO-ROW                        
028700     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
028710     MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUV                        
028720     MOVE 'M'                 TO CURR-KDVALTYP                            
028730                                                                          
028740     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
028750     IF CURR-KDSVAR = ' '                                                 
028760        CONTINUE                                                          
028770     ELSE                                                                 
028771        MOVE 1                TO CURR-PRKURS-NEW                          
028780     END-IF                                                               
028900     MOVE  CURR-PRKURS-NEW    TO INTUT-PRKURS                             
029000                                 INTUT-PRKURS-SEND                        
029100                                                                          
029200     MOVE 'INL'               TO INTUT-KDFINDOC                           
029300     MOVE WS-DATUM            TO INTUT-DAFINDOC                           
029400     MOVE ZERO                TO INTUT-IDFINDOC                           
029500     MOVE SPACE               TO INTUT-IDEXCUST-1                         
029600                                 INTUT-IDEXCUST-2                         
029700                                                                          
029800     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
029900     MOVE IN-IDARTNR          TO CIA-IDARTBET-IN                          
030000                                 W-IDARTNR                                
030100     CALL W009CIA USING          CIA-W009CIA                              
030200     MOVE CIA-IDARTBET-UT     TO INTUT-IDARTNR-FINANCE                    
030300                                                                          
030400     MOVE 'NL'                TO W-IDSKYLT                                
030500     PERFORM IMS-GET-WDD311-BSEQ                                          
030600     IF SEGMENT-FINNS                                                     
030700       MOVE TEXT-BEART        TO INTUT-BEART                              
030800     ELSE                                                                 
030900       MOVE SPACE             TO INTUT-BEART                              
031000     END-IF                                                               
031100                                                                          
031200     MOVE IN-IDSTATNR         TO INTUT-IDSTATNR                           
031300     MOVE IN-VKARTTOT         TO INTUT-VKORDNTO                           
031300                                 INTUT-VKORDNTO-3DEC                      
031400     MOVE IN-KVANTMOT         TO INTUT-KVLEVART                           
031500     MOVE IN-KDARTURS         TO INTUT-KDARTURS                           
031600     MOVE ZERO                TO INTUT-KDFRAKT                            
031700     MOVE 'NOT USED'          TO INTUT-BELEVVIL                           
031800     MOVE IN-SUARTBES         TO INTUT-SUNTO                              
031900                                                                          
032000     PERFORM S81-PUT-INTRA                                                
032100     .                                                                    
032200     EJECT                                                                
032300                                                                          
032400 Z-FINIT SECTION.                                                         
032500     CLOSE W61168                                                         
032600           W61170                                                         
032700     SKIP2                                                                
032800     MOVE 'S' TO POSTSUM-OPKOD                                            
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000                                                                          
033100     PERFORM S82-CLOSE-INTRA                                              
033200     .                                                                    
033300     EJECT                                                                
033400 S01-LAES-W61168  SECTION.                                                
033500     READ W61168 INTO IN-AREA                                             
033600     AT END                                                               
033700        SET END-OF-W61168 TO TRUE                                         
033800                                                                          
033900     NOT AT END                                                           
034000        MOVE 'W61168'   TO POSTSUM-FDNAMN                                 
034100        MOVE 'W61168D1' TO POSTSUM-DDNAMN2                                
034200        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
034300        CALL POSTSUM USING POSTSUM-PARM                                   
034400     END-READ                                                             
034500     .                                                                    
034600     EJECT                                                                
034700 S11-SKRIV-W61170 SECTION.                                                
034800                                                                          
034900     WRITE UT-POST FROM UT-AREA                                           
035000                                                                          
035100     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
035200     MOVE 'W61170'   TO POSTSUM-FDNAMN                                    
035300     MOVE 'W61168D2' TO POSTSUM-DDNAMN2                                   
035400     CALL POSTSUM USING POSTSUM-PARM                                      
035500     .                                                                    
035600     EJECT                                                                
035700                                                                          
035800 S80-OPEN-INTRA SECTION.                                                  
035900     MOVE WS-ADRESS-INTRA                 TO SEND-ADDISPABS               
036000     MOVE 'OPEN'                          TO SEND-KDFUNC                  
036100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036200                         SEND-OPEN-AREA                                   
036300     IF SEND-KDRC > ZERO                                                  
036400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
036500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
036600       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
036700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
036800     END-IF                                                               
036900     .                                                                    
037000                                                                          
037100 S81-PUT-INTRA SECTION.                                                   
037200     MOVE 'PUT'                           TO SEND-KDFUNC                  
037300     MOVE LENGTH OF INTUT-AREA            TO SEND-KVDLEN                  
037400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
037500                         SEND-KVDLEN                                      
037600                         INTUT-AREA                                       
037700     IF SEND-KDRC > ZERO                                                  
037800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
038000       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
038100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
038200     END-IF                                                               
038300     .                                                                    
038400                                                                          
038500 S82-CLOSE-INTRA SECTION.                                                 
038600     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
038700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038800     .                                                                    
038900     EJECT                                                                
039000* --- IMS SEKTIONER ---                                                   
039100                                                                          
039200 IMS-GET-WDF106 SECTION.                                                  
039300                                                                          
039400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
039500          DELIMITED BY SIZE INTO SSA1                                     
039600     STRING 'WLLEVA14(IDLEVSUF =' W-IDLEVSUF-X ')'                        
039700          DELIMITED BY SIZE INTO SSA2                                     
039800     MOVE '  GE' TO GODK-STATUSKODER                                      
039900     CALL CBLTDLI USING GU  LEVA-PCB DLI-IO-WLLEVA14 SSA1 SSA2            
040000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
040100     PERFORM IMS-STATUSKONTROLL                                           
040200     .                                                                    
040300     SKIP3                                                                
040400                                                                          
040500 IMS-GET-WDD311-BSEQ SECTION.                                             
040600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
040700          DELIMITED BY SIZE INTO SSA1                                     
040800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
040900          DELIMITED BY SIZE INTO SSA2                                     
041000     MOVE '  GE'           TO GODK-STATUSKODER                            
041100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
041200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
041300     PERFORM IMS-STATUSKONTROLL                                           
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 IMS-STATUSKONTROLL SECTION.                                              
041800                                                                          
041900     SET STATUS-IX TO 1                                                   
042000     SEARCH GODK-STATUS                                                   
042100       AT END                                                             
042200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042300           DELIMITED BY SIZE INTO FELTEXT                                 
042400         DISPLAY FELTEXT                                                  
042500         CALL FELLOG                                                      
042600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042700         CONTINUE                                                         
042800     END-SEARCH                                                           
042900     .                                                                    
