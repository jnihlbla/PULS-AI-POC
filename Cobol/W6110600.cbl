000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6110600.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   94/11/29.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄGGER UPP GODS FRÅN SUPPLIER TERMINAL PÅ INLEVERANS             
001100*        REGISTRET OCH UPPDATERAR KVAKS PÅ WDK6.                          
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001400*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001500*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
001600*        PROGRAMMET UPPDATERAR W6CKPH (W6G2)                              
001700*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
001800*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  FEL FRÅN WORKDAY                                        
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- INLEVERANSER FRÅN SUPPLIER TERMINAL                        
003400     SELECT W61106                     ASSIGN TO W61106D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W61106                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W6110501      -L.                                              
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W6110600'.            
005100 01  CHKP-VAR.                                                            
005200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005700 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 01  W-IDLOPNRM                  PIC 9(9).                                
006600 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM.                                   
006700  03 FILLER                      PIC 9(1).                                
006800  03 W-VVD                       PIC 9(3).                                
006900  03 W-LLLL                      PIC 9(4).                                
007000  03 W-K                         PIC 9(1).                                
007100                                                                          
007200 77  W61106-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W61106                       VALUE 'J'.                   
007400     EJECT                                                                
007500 01  WS-DAGENS-DATUM             PIC 9(9)    VALUE ZERO.                  
007600 01  WS-TID                      PIC 9(9)    VALUE ZERO.                  
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL ABEND                                            
008400                                                                          
008500 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   COMP VALUE +16.               
008600 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   COMP VALUE +1000.             
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'INDEX '.              
008900 01  EMB-IX                      PIC S9(4)   VALUE +0 COMP SYNC.          
009000 01  ANT-IX                      PIC S9(4)   VALUE +0 COMP SYNC.          
009100                                                                          
009200 01  FILLER                      PIC X(16)                                
009300                                   VALUE 'ARBETSFAELT '.                  
009400 77  W-W61106-KVPOST-IN         PIC S9(5)  VALUE ZERO   COMP-3.           
009500 77  W-ART-EMBQ3                PIC 9(9).                                 
009600 77  W-SPAR-IDARTNR             PIC S9(9)  VALUE ZERO   COMP-3.           
009700 77  W-SPAR-IDFS                PIC  X(8)  VALUE SPACE.                   
009800 77  W-SPAR-RAD-IDLEVNR-KOLLI   PIC  X(5)  VALUE SPACE.                   
009900 77  W-SPAR-RAD-IDOKOLLI        PIC  9(9)  VALUE ZERO.                    
010000 77  W-SPAR-IDSHIPM             PIC  9(7)  VALUE ZERO.                    
010100 77  W-SPAR-RAD-FLSATS          PIC  X(1)  VALUE SPACE.                   
010200 77  W-KVLEVART-TOT-ART         PIC S9(7)  VALUE ZERO   COMP-3.           
010300 77  W-KVAVIS-KIT               PIC S9(7)  VALUE ZERO   COMP-3.           
010400 77  W-BEART                    PIC X(25)  VALUE SPACE.                   
010500 77  W-ANT                      PIC 9(1)   VALUE ZERO.                    
010600 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
010700     88  OMSTART                             VALUE 'J'.                   
010800                                                                          
010900*     -- INLEVERANS-ID (9-KOMPLEMENT TILL DATE+TIME)                      
011000 77      WS-DAINLEV              PIC 9(16)  VALUE ZERO.                   
011100*                                                                         
011200 77      WS-LOGG-IDARTNR         PIC S9(9)              COMP-3.           
011300 77      WS-LOGG-IDLEVNR         PIC  X(5) VALUE SPACE.                   
011400 77      WS-LOGG-IDFS            PIC X(8).                                
011500*     -- DATE + TIME                                                      
011600 01      WS-TIAAAAMMDDTTMMSSTH    PIC 9(16)   VALUE ZERO.                 
011700 01      FILLER                  REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
011800   03    WS-TISEKEL               PIC 9(2).                               
011900   03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                               
012000   03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                               
012100                                                                          
012200 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
012300     03  FLT-LGD                 PIC S9  COMP SYNC   VALUE +7.            
012400     03  VAEGNINGSTAL            PIC 9(7)        VALUE 2121212.           
012500     03  VAEGNTAL-LGD            PIC S9  COMP SYNC   VALUE +7.            
012600     03  MODUL-10-11             PIC 9(2)            VALUE 10.            
012700     03  ALT-A-B                 PIC X(1)            VALUE 'B'.           
012800                                                                          
012900*      --- VALID IDDC CODES                                               
013000*                                                                         
013100*01    -COPY WWDCKONS                                                     
013200       EJECT                                                              
013300*      --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                             
013400*01    -COPY W611EMB3                                                     
013500       EJECT                                                              
013600 01  DYNAMISKA-SUBPROGRAM.                                                
013700*                                                                         
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014300     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
014400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
014500     EJECT                                                                
014600*01  -COPY WORKAREA                                                       
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
014900*01 -COPY WDATAREA                                                        
015000     EJECT                                                                
015100*    --- PARAMETRAR TILL POSTSUM                                          
015200*                                                                         
015300*01  -COPY W0005   -PRE  POSTSUM-                                         
015400     EJECT                                                                
015500 01  IN-AREA-START           PIC X(24)   VALUE                            
015600                                             'IN-AREA-START'.             
015700*01  AREA -COPY W6110501     -PRE IN-                                     
015800*                                                                         
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200 01  NYCKLAR-TILL-DLI.                                                    
016300     03  W-IDRADNR-INL-X.                                                 
016400         05 W-IDRADNR-INL        PIC S9(5)   VALUE ZERO COMP-3.           
016500     03  W-IDRADNR-X.                                                     
016600         05 W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.           
016700     03  W-IDARTNR-X.                                                     
016800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016900     03  W-KDSEGKEY-X.                                                    
017000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
017100     03  W-KDCLAGER-X.                                                    
017200         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
017300     03  W-W6D101KY-X.                                                    
017400         05  W-D101KY-IDDC       PIC  X(2)   VALUE SPACE.                 
017500         05  W-D101KY-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
017600         05  W-D101KY-IDFS       PIC  X(8)   VALUE SPACE.                 
017700         05  W-D101KY-TIAVIDAT   PIC S9(7)   VALUE ZERO COMP-3.           
017800     03  W-IDOKOLLI-X.                                                    
017900         05  W-IDOKOLLI          PIC  9(9)   VALUE ZERO.                  
018000     03  W-IDSKYLT-X.                                                     
018100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
018200     03  W-W6GXKEY-6017-X.                                                
018300         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
018400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
018500     03  W-W6GXKEY-6018-X.                                                
018600         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
018700     03  W-W6GX-6029-KEY-X.                                               
018800         05  W-IDHTYP-6029       PIC X(04)   VALUE '6029'.                
018900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
019000     03  W-W6GX-6030-KEY-X.                                               
019100         05  FILLER              PIC X       VALUE '1'.                   
019110     03  W-IDDC-B6-LEV-X.                                                 
019120         05 W-IDDC-B6-LEV        PIC X(5)    VALUE SPACE.                 
019200*    --- STATUS-KOD FRÅN IMS                                              
019300 01  STATUS-WS                   PIC XX.                                  
019400     88  SEGMENT-FINNS                       VALUE '  '.                  
019500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019800     88  IMS-EJ-OK                           VALUE 'XD'.                  
019900     SKIP2                                                                
020000 01  GODK-STATUSKODER.                                                    
020100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020200     SKIP3                                                                
020300 01  SSA1                        PIC X(64).                               
020400 01  SSA2                        PIC X(64).                               
020500 01  SSA3                        PIC X(64).                               
020600     EJECT                                                                
020700*    --- IMS FUNKTIONSKODER                                               
020800*01  -COPY W0003                                                          
020900     EJECT                                                                
021000*    ---  DLI INPUT-OUTPUT AREA                                           
021100 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
021200*01  WLLOGA01  -COPY WDL901                                               
021300     SKIP3                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021500     SKIP3                                                                
021600 01  DLI-IO-AREA.                                                         
021700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
021800                                                                          
021900     03  W6INLA01 REDEFINES IO-AREA.                                      
022000*        05  -COPY W6D101                                                 
022100     EJECT                                                                
022200     03  W6INLA11 REDEFINES IO-AREA.                                      
022300*        05  -COPY W6D111                                                 
022400     EJECT                                                                
022500     03  W6INLA21 REDEFINES IO-AREA.                                      
022600*        05  -COPY W6D121                                                 
022700     EJECT                                                                
022800     03  WLBENA11 REDEFINES IO-AREA.                                      
022900*        05  -COPY WDD311    -PRE BENA11-                                 
023000     EJECT                                                                
023100     03  W6CKPH11 REDEFINES IO-AREA.                                      
023200*        05  -COPY W6GX6030  -PRE CKPH-                                   
023300     EJECT                                                                
023400 01  FILLER                      PIC X(16)                                
023500                             VALUE 'DLI-IO-AREA1'.                        
023600     SKIP3                                                                
023700 01  DLI-IO-AREA1.                                                        
023800     03  IO-AREA1               PIC X(150)  VALUE SPACE.                  
023900     03  WLARTC01 REDEFINES IO-AREA1.                                     
024000*        05  -COPY WDK601  -PRE ARTC-                                     
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)                                
024300                             VALUE 'DLI-IO-AREA2'.                        
024400     SKIP3                                                                
024500 01  DLI-IO-AREA2.                                                        
024600     03  IO-AREA2               PIC X(900)  VALUE SPACE.                  
024700     03  WLARTC11 REDEFINES IO-AREA2.                                     
024800*        05  -COPY WDK611  -PRE ARTC-                                     
024900     EJECT                                                                
025000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA6'.          
025100     SKIP3                                                                
025200 01  DLI-IO-AREA6.                                                        
025300     03  IO-AREA6              PIC X(150)  VALUE SPACE.                   
025400     03  W6LOPA11 REDEFINES IO-AREA6.                                     
025500*        05  -COPY W6GX6018 -PRE LOPA-                                    
025600     EJECT                                                                
025700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA7'.          
025800     SKIP3                                                                
025900 01  DLI-IO-AREA7.                                                        
026000     03  IO-AREA7              PIC X(150)  VALUE SPACE.                   
026100     03  WLINLE01 REDEFINES IO-AREA7.                                     
026200*        05  -COPY WDL201  -PRE INLE-                                     
026300     EJECT                                                                
026400     03  WLINLE11 REDEFINES IO-AREA7.                                     
026500*        05  -COPY WDL211  -PRE INLE-                                     
026600     EJECT                                                                
026700     03  WLINLE21 REDEFINES IO-AREA7.                                     
026800*        05  -COPY WDL221  -PRE INLE-                                     
026900     EJECT                                                                
026910 01  FILLER                    PIC X(16)   VALUE 'WDB601 LEV'.            
026920 01   DLI-IO-AREA-B601-LEV.                                               
026930*     03  -COPY WDB601   -PRE LEV-                                        
026940                                                                          
027000 LINKAGE SECTION.                                                         
027100                                                                          
027200*01  -COPY W0009   -PRE MSG-                                              
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE WLLOGA-                                            
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE ARTC-                                              
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008  -PRE INLA-                                              
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008  -PRE BENA-                                              
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008  -PRE LOPA-                                              
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900*01  -COPY W0008  -PRE CKPH-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200*01  -COPY W0008  -PRE INLE-                                              
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029410*01  -COPY W0008  -PRE WDB6-LEV-                                          
029420     05  FILLER                  PIC X.                                   
029430     EJECT                                                                
029500 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB INLA-PCB                      
029600                          BENA-PCB LOPA-PCB CKPH-PCB                      
029700                          INLE-PCB WLLOGA-PCB WDB6-LEV-PCB.               
029800     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB INLA-PCB                      
029900                          BENA-PCB LOPA-PCB CKPH-PCB                      
030000                          INLE-PCB WLLOGA-PCB WDB6-LEV-PCB.               
030100                                                                          
030200                                                                          
030300     PERFORM A-INIT                                                       
030400     PERFORM IMS-GHU-LOPA11                                               
030500     MOVE LOPA-6018-IDLOPNRM   TO W-IDLOPNRM                              
030600     IF NOT OMSTART                                                       
030700       PERFORM S01-LAES-W61106                                            
030800     END-IF                                                               
030900     PERFORM UNTIL END-OF-W61106                                          
031000       MOVE IN-IDFS                TO W-SPAR-IDFS                         
031100       MOVE IN-IDSHIPM             TO W-SPAR-IDSHIPM                      
031200       IF NOT OMSTART                                                     
031300         MOVE +1                     TO W-IDRADNR-INL                     
031400       END-IF                                                             
031500       PERFORM B-SKAPA-INLA01                                             
031600       PERFORM UNTIL END-OF-W61106 OR                                     
031700                     IN-IDFS       NOT = W-SPAR-IDFS                      
031800          IF CHKP-ANT              > CHKP-MAX                             
031900            MOVE W-IDLOPNRM        TO LOPA-6018-IDLOPNRM                  
032000            PERFORM IMS-REPL-LOPA11                                       
032100            PERFORM X-TAG-CHECKPOINT                                      
032200            PERFORM IMS-GHU-LOPA11                                        
032300            MOVE LOPA-6018-IDLOPNRM TO W-IDLOPNRM                         
032400          END-IF                                                          
032500          PERFORM C-LAES-ART-INFO                                         
032600          PERFORM D-SKAPA-INLA11                                          
032700          MOVE IN-IDARTNR      TO W-SPAR-IDARTNR                          
032800          MOVE +2              TO W-IDRADNR                               
032900          MOVE ZERO            TO W-KVLEVART-TOT-ART                      
033000                                  W-KVAVIS-KIT                            
033100          PERFORM UNTIL END-OF-W61106                   OR                
033200                        IN-IDFS    NOT = W-SPAR-IDFS    OR                
033300                        IN-IDARTNR NOT = W-SPAR-IDARTNR                   
033400             MOVE IN-IDARTNR   TO W-SPAR-IDARTNR                          
033500             PERFORM E-SKAPA-INLA21                                       
033600             IF IN-FLSATS      =  JA                                      
033700                ADD IN-KVLEVART TO W-KVAVIS-KIT                           
033800             END-IF                                                       
033900             ADD IN-KVLEVART   TO W-KVLEVART-TOT-ART                      
034000             ADD +1            TO W-IDRADNR                               
034100             PERFORM S01-LAES-W61106                                      
034200          END-PERFORM                                                     
034300          MOVE SPACE           TO    W-SPAR-RAD-IDLEVNR-KOLLI             
034400          MOVE +0              TO    W-SPAR-RAD-IDOKOLLI                  
034500                                     W-SPAR-RAD-FLSATS                    
034600          PERFORM F-UPPDATERA-INLA11                                      
034700          PERFORM G-UPPDATERA-INLEV-HIST                                  
034800          PERFORM H-UPPDATERA-ARTC                                        
034900          ADD +1               TO W-IDRADNR-INL                           
035000        END-PERFORM                                                       
035100     END-PERFORM                                                          
035200                                                                          
035300     MOVE W-IDLOPNRM     TO LOPA-6018-IDLOPNRM                            
035400     PERFORM IMS-REPL-LOPA11                                              
035500                                                                          
035600     PERFORM Z-FINIT                                                      
035700                                                                          
035800     MOVE ZERO TO RETURN-CODE                                             
035900     GOBACK                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 A-INIT SECTION.                                                          
036300     SKIP2                                                                
036400                                                                          
036500     OPEN INPUT W61106                                                    
036600                                                                          
036700     ACCEPT  DAGENS-DATUM     FROM DATE                                   
036800                                                                          
036900     PERFORM IMS-RESTART                                                  
037000                                                                          
037100     PERFORM IMS-LAS-ATERSTART                                            
037200                                                                          
037300     IF SEGMENT-FINNS                                                     
037400       IF CKPH-6030-KVPOST     > ZERO                                     
037500         PERFORM AA-ATERSTART-EFTER-ABEND                                 
037600         MOVE JA               TO OMSTART-SW                              
037700        ELSE                                                              
037800         MOVE NEJ              TO OMSTART-SW                              
037900       END-IF                                                             
038000     END-IF                                                               
038100     MOVE ZERO                 TO CHKP-ANT                                
038200                                                                          
038300     MOVE SPACE                TO W-SPAR-IDFS                             
038400     MOVE ZERO                 TO W-SPAR-IDARTNR                          
038500     MOVE ZERO                 TO W-SPAR-IDSHIPM                          
038600                                                                          
038700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
038800                                                                          
038900     MOVE 'IDAG'               TO DAT-KDDATFORM                           
039000     CALL WDATKONV USING          DAT-KDDATFORM                           
039100                                  DAT-I-TIDATUM                           
039200                                  DAT-O-TIDATUM                           
039300                                  DAT-KDSVAR                              
039400     .                                                                    
039500     EJECT                                                                
039600 AA-ATERSTART-EFTER-ABEND SECTION.                                        
039700                                                                          
039800     MOVE WC-CDC-SE         TO W-D101KY-IDDC                              
039900     PERFORM UNTIL W-W61106-KVPOST-IN    = CKPH-6030-KVPOST OR            
040000                                           END-OF-W61106                  
040100       PERFORM S01-LAES-W61106                                            
040200       MOVE IN-IDLEVNR      TO W-D101KY-IDLEVNR                           
040300       MOVE IN-IDFS         TO W-D101KY-IDFS                              
040400       MOVE IN-TIAVIDAT     TO W-D101KY-TIAVIDAT                          
040500     END-PERFORM                                                          
040600                                                                          
040700     PERFORM IMS-GU-INLA11-LAST                                           
040800     IF SEGMENT-FINNS                                                     
040900        COMPUTE W-IDRADNR-INL = ART-IDRADNR-INL + 1                       
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 B-SKAPA-INLA01   SECTION.                                                
041400                                                                          
041500     MOVE WC-CDC-SE         TO INL-IDDC                                   
041600                               W-D101KY-IDDC                              
041700     MOVE IN-IDLEVNR        TO INL-IDLEVNR                                
041800                               W-D101KY-IDLEVNR                           
041900     MOVE IN-IDFS           TO INL-IDFS                                   
042000                               W-D101KY-IDFS                              
042100     MOVE IN-TIAVIDAT       TO INL-TIAVIDAT                               
042200                               W-D101KY-TIAVIDAT                          
042300     MOVE NEJ               TO INL-FLFEL                                  
042400     MOVE SPACE             TO INL-IDANALYS                               
042410                               INL-IDKST                                  
042500     MOVE ZERO              TO INL-IDARTNR                                
042600     MOVE 57                TO INL-IDFTG                                  
042700     MOVE ZERO              TO INL-IDKONTO                                
042900     MOVE IN-IDLBBET        TO INL-IDLBBET                                
043000     MOVE '310'             TO INL-KDINL                                  
043100     MOVE ZERO              TO INL-TIINLMOT                               
043200     MOVE IN-IDSHIPM        TO INL-IDSHIPM                                
043300                                                                          
043400     PERFORM BA-BERAEKNA-TIANKDAG                                         
043500                                                                          
043600     ADD +1                 TO CHKP-ANT                                   
043700     PERFORM IMS-ISRT-INLA01                                              
043800                                                                          
043900     .                                                                    
044000     EJECT                                                                
044100 BA-BERAEKNA-TIANKDAG SECTION.                                            
044200                                                                          
044300     MOVE IN-TIAVIDAT     TO WORK-TIAAMMDD-FOM                            
044400     MOVE +002            TO WORK-KDCALL                                  
044500     MOVE WC-CDC-SE       TO WORK-IDDC                                    
044501                                                                          
044510* READ WDB6 USING IDLEVNR. USE KDDC FROM WDB6 TO DETERMINE THE            
044520* NUMBER OF WORK DAYS TO BE CONSIDERED FOR A SHIPMENT TO SHOW UP          
044530* ON A LIST(W61121-002) CREATED IN W61121.                                
044600     IF IN-IDLEVNR NOT = SPACE                                            
044620       MOVE IN-IDLEVNR    TO W-IDDC-B6-LEV                                
044630       PERFORM IMS-GU-WDB601-LEV                                          
044640       IF SEGMENT-FINNS                                                   
044641          EVALUATE TRUE                                                   
044650            WHEN LEV-DCS-KDDC = 'S'                                       
044660             MOVE +10     TO WORK-KVWORKD                                 
044661            WHEN LEV-DCS-KDDC = 'NP'                                      
044662             MOVE +40     TO WORK-KVWORKD                                 
044663            WHEN OTHER                                                    
044664             MOVE +20     TO WORK-KVWORKD                                 
044665          END-EVALUATE                                                    
044666       ELSE                                                               
044667          MOVE +20        TO WORK-KVWORKD                                 
044670       END-IF                                                             
044671     ELSE                                                                 
044672       MOVE +20           TO WORK-KVWORKD                                 
044680     END-IF                                                               
044700                                                                          
044800     CALL WORKDAY USING WORK-KDCALL                                       
044900                   WORK-DATE-AREA WORK-KDSVAR                             
045000     IF WORK-KDSVAR-OK                                                    
045100        MOVE WORK-TIAAMMDD-NEXT-WORKDAY  TO INL-TIANKDAG                  
045200     ELSE                                                                 
045300        MOVE 'FEL UR WORKDAY'           TO FELTEXT-STR                    
045400        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 C-LAES-ART-INFO  SECTION.                                                
045900                                                                          
046000     MOVE IN-IDARTNR       TO W-IDARTNR                                   
046100                              WS-LOGG-IDARTNR                             
046200     MOVE IN-IDLEVNR       TO WS-LOGG-IDLEVNR                             
046300     MOVE IN-IDFS          TO WS-LOGG-IDFS                                
046400     MOVE '1'              TO W-KDCLAGER                                  
046500     PERFORM IMS-GU-ARTC01                                                
046600     PERFORM IMS-GHNP-ARTC11                                              
046700                                                                          
046800     MOVE 'S  '            TO W-IDSKYLT                                   
046900     PERFORM IMS-GU-BENA11                                                
047000     MOVE BENA11-TEXT-BEART  TO W-BEART                                   
047100     .                                                                    
047200     EJECT                                                                
047300 D-SKAPA-INLA11   SECTION.                                                
047400                                                                          
047500     MOVE W-IDRADNR-INL            TO ART-IDRADNR-INL                     
047600     MOVE IN-IDARTNR               TO ART-IDARTNR                         
047700     MOVE ZERO                     TO ART-TIUPPDAT                        
047800     MOVE ARTC-CLAG-ADGANG            TO ART-ADGANG                       
047900     MOVE ARTC-CLAG-ADLAGOMR          TO ART-ADLAGOMR                     
048000     MOVE ARTC-CLAG-ADPLATS           TO ART-ADPLATS                      
048100     MOVE W-BEART                  TO ART-BEART                           
048200     MOVE ARTC-CLAG-BEFT              TO ART-BEFT                         
048300     MOVE SPACE                    TO ART-ADTRDEST-KIT                    
048400     MOVE NEJ                      TO ART-FLETIKETT                       
048500                                      ART-FLFEL                           
048600                                      ART-FLKLAR                          
048700                                      ART-FLKVAKAR                        
048800                                      ART-FLANNULL                        
048900     MOVE IN-FLKVAFEL              TO ART-FLKVAFEL                        
049000     MOVE ARTC-ART-IDFKNGRP          TO ART-IDFKNGRP                      
049100     MOVE WC-CDC-SE                TO ART-IDDC                            
049200     MOVE ARTC-CLAG-KDFARLIG       TO ART-KDFARLIG                        
049300     MOVE ZERO                     TO ART-KDINLPRIO                       
049400                                      ART-KDKVAANT                        
049500     MOVE 8                        TO ART-KDRT                            
049600     MOVE ARTC-ART-KDSORT          TO ART-KDSORT                          
049700     MOVE ZERO                     TO ART-KVAVIS                          
049800                                      ART-KVAVIS-KIT                      
049900                                      ART-KVAVIS-PRIO                     
050000                                      ART-KVKVAPRIM-BER                   
050100                                      ART-KVKVAPRIM-VER                   
050200                                      ART-KVKVASEK-BER                    
050300                                      ART-KVKVASEK-VER                    
050400     MOVE ARTC-CLAG-KVMP           TO ART-KVMP                            
050500     MOVE ARTC-CLAG-PRARTSTD       TO ART-PRARTSTD                        
050600     MOVE ARTC-CLAG-VKART          TO ART-VKART                           
050700     MOVE ARTC-CLAG-VLARTNTO       TO ART-VLARTNTO                        
050800     MOVE ARTC-CLAG-KDARTURS       TO ART-KDARTURS                        
050900     MOVE NEJ                      TO ART-FLSPLPART                       
051000     MOVE SPACE                    TO ART-ADTRDEST                        
051100                                                                          
051200     PERFORM DA-KDLAGEMB                                                  
051300                                                                          
051400     PERFORM DB-TA-UT-IDLOPNRM                                            
051500     MOVE W-IDLOPNRM               TO ART-IDLOPNRM                        
051501                                                                          
051510     IF IN-IDLBBET(1:7) = 'QUALITY'                                       
051520        MOVE 'QC'                  TO ART-KDKVAINL                        
051530     ELSE                                                                 
051540        MOVE SPACE                 TO ART-KDKVAINL                        
051550     END-IF                                                               
051600                                                                          
051700     PERFORM IMS-ISRT-INLA11                                              
051800     ADD +1                 TO CHKP-ANT                                   
051900     .                                                                    
052000     EJECT                                                                
052100 DA-KDLAGEMB      SECTION.                                                
052200                                                                          
052300     MOVE ARTC-CLAG-IDARTNR-EMBQ3  TO W-ART-EMBQ3                         
052400                                                                          
052500     MOVE SPACE                 TO ART-KDLAGEMB                           
052600                                                                          
052700     MOVE 1                     TO EMB-IX                                 
052800     PERFORM UNTIL EMB-IX       >  TAB-EMBQ3-MAX OR                       
052900          TAB-KOD (EMB-IX)      = W-ART-EMBQ3                             
053000       ADD 1                    TO EMB-IX                                 
053100     END-PERFORM                                                          
053200                                                                          
053300     IF EMB-IX                  > TAB-EMBQ3-MAX                           
053400         CONTINUE                                                         
053500      ELSE                                                                
053600         IF TAB-KOD (EMB-IX)        =  W-ART-EMBQ3                        
053700             MOVE TAB-TEXT (EMB-IX) TO ART-KDLAGEMB                       
053800         END-IF                                                           
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200 DB-TA-UT-IDLOPNRM      SECTION.                                          
054300                                                                          
054400     IF DAT-TIAAVVD-GRP (3:3)  =  W-VVD                                   
054500         ADD +1                TO W-LLLL                                  
054600      ELSE                                                                
054700         MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                              
054800         MOVE +1                    TO W-LLLL                             
054900     END-IF                                                               
055000     CALL CHECK USING W-IDLOPNRM (2:7) FLT-LGD                            
055100          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
055200     .                                                                    
055300     EJECT                                                                
055400 E-SKAPA-INLA21   SECTION.                                                
055500                                                                          
055600     IF IN-IDLEVNR  = W-SPAR-RAD-IDLEVNR-KOLLI AND                        
055700        IN-IDOKOLLI = W-SPAR-RAD-IDOKOLLI      AND                        
055800        IN-FLSATS   = W-SPAR-RAD-FLSATS                                   
055900       SUBTRACT 1 FROM W-IDRADNR                                          
056000       PERFORM IMS-GHU-INLA21                                             
056100       ADD IN-KVLEVART TO RAD-KVINLART                                    
056200       PERFORM IMS-REPL-INLA                                              
056300     ELSE                                                                 
056400       MOVE W-IDRADNR                TO RAD-IDRADNR                       
056500       MOVE SPACE                    TO RAD-ADINLOMR                      
056600                                        RAD-ADINLOMR-NXT                  
056700       MOVE IN-FLDIVKLI              TO RAD-FLDIVKLI                      
056800       MOVE IN-FLSATS                TO RAD-FLSATS                        
056900                                        W-SPAR-RAD-FLSATS                 
057000       MOVE NEJ                      TO RAD-FLINLFP                       
057100                                        RAD-FLKVAANT                      
057200                                        RAD-FLPRIO                        
057300                                        RAD-FLINLFB                       
057400                                        RAD-FLSVSLS                       
057500       MOVE ZERO                     TO RAD-IDANSTNR                      
057600                                        RAD-IDILIRAD                      
057700                                        RAD-IDILIST                       
057800                                        RAD-IDINLVGN                      
057900       MOVE IN-IDLEVNR               TO RAD-IDLEVNR-KOLLI                 
058000                                        W-SPAR-RAD-IDLEVNR-KOLLI          
058100       MOVE IN-IDOKOLLI              TO RAD-IDOKOLLI                      
058200                                        W-SPAR-RAD-IDOKOLLI               
058300       MOVE ZERO                     TO RAD-KDINLPRIO                     
058400       MOVE 'AVI'                    TO RAD-KDINLSTA                      
058500       MOVE IN-KVLEVART              TO RAD-KVINLART                      
058600       MOVE ZERO                     TO RAD-TIUPPDAT                      
058700                                                                          
058800       PERFORM IMS-ISRT-INLA21                                            
058900     END-IF                                                               
059000     ADD +1                 TO CHKP-ANT                                   
059100     .                                                                    
059200     EJECT                                                                
059300 F-UPPDATERA-INLA11   SECTION.                                            
059400                                                                          
059500     PERFORM IMS-GHU-INLA11                                               
059600                                                                          
059700     MOVE W-KVLEVART-TOT-ART    TO ART-KVAVIS                             
059800     MOVE W-KVAVIS-KIT          TO ART-KVAVIS-KIT                         
059900     PERFORM IMS-REPL-INLA                                                
060000     .                                                                    
060100     EJECT                                                                
060200 G-UPPDATERA-INLEV-HIST SECTION.                                          
060300                                                                          
060400*    -- SKAPA IDINLEV                                                     
060500     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
060600     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
060700     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
060800     COMPUTE WS-DAINLEV          = 9999999999999999                       
060900                                 - WS-TIAAAAMMDDTTMMSSTH                  
061000     END-COMPUTE                                                          
061100     PERFORM IMS-GU-INLE01                                                
061200                                                                          
061300     IF  SEGMENT-SAKNAS                                                   
061400       MOVE W-SPAR-IDARTNR     TO INLE-ART-IDARTNR                        
061500       PERFORM IMS-ISRT-INLE01                                            
061600     END-IF                                                               
061700                                                                          
061800     MOVE WS-DAINLEV             TO INLE-INL-DAINLEV                      
061900     PERFORM IMS-ISRT-INLE11                                              
062000     PERFORM UNTIL SEGMENT-FINNS                                          
062010       SUBTRACT +1 FROM WS-DAINLEV                                        
062020       MOVE WS-DAINLEV           TO INLE-INL-DAINLEV                      
062030       PERFORM IMS-ISRT-INLE11                                            
062040     END-PERFORM                                                          
062100                                                                          
062200     MOVE '310'                  TO INLE-MOT-IDPTYP                       
062300     MOVE W-IDLOPNRM             TO INLE-MOT-IDLOPNRM                     
062400     MOVE W-D101KY-IDLEVNR       TO INLE-MOT-IDLEVNR                      
062500     PERFORM GA-KOLLA-ANTAL-SIFFROR                                       
062600     MOVE W-D101KY-IDFS(1:W-ANT) TO INLE-MOT-IDAVINR                      
062700     MOVE ZERO                   TO INLE-MOT-IDKONTO                      
062800     MOVE ARTC-CLAG-ADLAGOMR     TO INLE-MOT-ADLAGOMR                     
062900     MOVE ARTC-CLAG-ADGANG       TO INLE-MOT-ADGANG                       
063000     MOVE ARTC-CLAG-ADPLATS      TO INLE-MOT-ADPLATS                      
063100     MOVE WC-CDC-SE              TO INLE-MOT-IDDC                         
063200     MOVE 8                      TO INLE-MOT-KDRT                         
063300     MOVE SPACE                  TO INLE-MOT-IDFS                         
063400     MOVE ZERO                   TO INLE-MOT-KDAVVANT                     
063500     MOVE ZERO                   TO INLE-MOT-KDAVVKV                      
063600     MOVE ZERO                   TO INLE-MOT-KVANTMOT                     
063700     MOVE W-KVLEVART-TOT-ART     TO INLE-MOT-KVAVIS                       
063800     MOVE ZERO                   TO INLE-MOT-KVFORDEL                     
063900     MOVE ZERO                   TO INLE-MOT-KVRETUR                      
064000     MOVE ZERO                   TO INLE-MOT-KVFORV                       
064100     MOVE W-D101KY-TIAVIDAT      TO INLE-MOT-TIAVIDAT                     
064200     MOVE ZERO                   TO INLE-MOT-TIUPPDAT                     
064300     MOVE W-SPAR-IDSHIPM         TO INLE-MOT-IDSHIPM                      
064400                                                                          
064500     PERFORM IMS-ISRT-INLE21                                              
064600     .                                                                    
064700     EJECT                                                                
064800 GA-KOLLA-ANTAL-SIFFROR   SECTION.                                        
064900                                                                          
065000     MOVE 1               TO ANT-IX                                       
065100     PERFORM UNTIL ANT-IX > 7 OR W-D101KY-IDFS(ANT-IX:1)  = SPACE         
065200        ADD +1            TO ANT-IX                                       
065300     END-PERFORM                                                          
065400                                                                          
065500     COMPUTE W-ANT        = ANT-IX - 1                                    
065600     .                                                                    
065700     EJECT                                                                
065800 H-UPPDATERA-ARTC     SECTION.                                            
065900                                                                          
066000** ARTIKELSEG ÄR LÄST I C-  SEKTIONEN                                     
066100                                                                          
066200     ADD  W-KVLEVART-TOT-ART TO ARTC-CLAG-KVAKS-PAV                       
066300     PERFORM IMS-REPL-ARTC                                                
066400     PERFORM HA-FLYTTA-SALDOLOGG-DATA                                     
066500     .                                                                    
066600     EJECT                                                                
066700 HA-FLYTTA-SALDOLOGG-DATA SECTION.                                        
066800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
066900     ACCEPT WS-TID                   FROM TIME                            
067000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
067100     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
067200     MOVE 9                   TO LOGG-IDSEKVNR                            
067300     MOVE 'INBO'              TO LOGG-IDHUVTYP                            
067400     MOVE '310'               TO LOGG-IDSUBTYP                            
067500     MOVE 'W6110600'          TO LOGG-IDPGM                               
067600     MOVE SPACE               TO LOGG-IDTRANS                             
067700     MOVE 'W6110600'          TO LOGG-IDUSER                              
067800     MOVE SPACE               TO LOGG-REF                                 
067900     MOVE WS-LOGG-IDLEVNR     TO LOGG-IDLEVNR                             
068000     MOVE WS-LOGG-IDFS        TO LOGG-IDFS                                
068100     MOVE WS-LOGG-IDARTNR     TO LOGG-IDARTNR                             
068200     MOVE WC-CDC-SE           TO LOGG-IDDC                                
068300*   ---SALDOFÖRÄNDRINGAR PÅ WDK611                                        
068400*   ---LOGGAS PÅ WDL9                                                     
068500     MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                           
068600     MOVE '+'                 TO LOGG-IDTECKEN-KVAKS-PAV                  
068700     MOVE ARTC-CLAG-KVLS      TO LOGG-KVLS                                
068800     MOVE ARTC-CLAG-KVEFRS    TO LOGG-KVEFRS                              
068900     MOVE SPACE               TO LOGG-IDTECKEN-KVLS                       
069000     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
069100     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
069200     MOVE W-KVLEVART-TOT-ART  TO LOGG-KVART-SALDO                         
069300     COMPUTE LOGG-KVAKS       =  ARTC-CLAG-KVAKS-CDC                      
069400                              +  ARTC-CLAG-KVAKS-T                        
069500     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
069600     PERFORM S02-ISRT-SALDOLOGG                                           
069700     .                                                                    
069800     EJECT                                                                
069900 Z-FINIT SECTION.                                                         
070000                                                                          
070100     PERFORM IMS-LAS-ATERSTART                                            
070200     MOVE ZERO                 TO CKPH-6030-KVPOST                        
070300     ACCEPT CKPH-6030-TIUPPDAT FROM DATE                                  
070400     ACCEPT CKPH-6030-TIUPPTID FROM TIME                                  
070500     IF SEGMENT-SAKNAS                                                    
070600       MOVE '1'                TO CKPH-6030-KDSEGKEY                      
070700       PERFORM IMS-ISRT-ATERSTART                                         
070800     ELSE                                                                 
070900       PERFORM IMS-REPL-ATERSTART                                         
071000     END-IF                                                               
071100                                                                          
071200     CLOSE W61106                                                         
071300                                                                          
071400     MOVE 'S' TO POSTSUM-OPKOD                                            
071500     CALL POSTSUM USING POSTSUM-PARM                                      
071600     .                                                                    
071700     EJECT                                                                
071800 S01-LAES-W61106  SECTION.                                                
071900     SKIP2                                                                
072000     READ W61106 INTO IN-AREA                                             
072100     AT END                                                               
072200        SET END-OF-W61106 TO TRUE                                         
072300                                                                          
072400     NOT AT END                                                           
072500        MOVE 'W61106'      TO POSTSUM-FDNAMN                              
072600        MOVE 'W61106D1'    TO POSTSUM-DDNAMN2                             
072700        MOVE 'INL'         TO POSTSUM-TRANSTYP                            
072800        CALL POSTSUM USING POSTSUM-PARM                                   
072900                                                                          
073000        ADD 1 TO W-W61106-KVPOST-IN                                       
073100     END-READ                                                             
073200     .                                                                    
073300     EJECT                                                                
073400 S02-ISRT-SALDOLOGG SECTION.                                              
073500     PERFORM IMS-ISRT-WDL901                                              
073600     IF SEGMENT-FINNS-REDAN                                               
073700       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
073800         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
073900         PERFORM IMS-ISRT-WDL901                                          
074000       END-PERFORM                                                        
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 X-TAG-CHECKPOINT   SECTION.                                              
074500                                                                          
074600     PERFORM IMS-LAS-ATERSTART                                            
074700     MOVE W-W61106-KVPOST-IN   TO CKPH-6030-KVPOST                        
074800     ACCEPT CKPH-6030-TIUPPDAT FROM DATE                                  
074900     ACCEPT CKPH-6030-TIUPPTID FROM TIME                                  
075000     IF SEGMENT-SAKNAS                                                    
075100       MOVE '1'                TO CKPH-6030-KDSEGKEY                      
075200       PERFORM IMS-ISRT-ATERSTART                                         
075300     ELSE                                                                 
075400       PERFORM IMS-REPL-ATERSTART                                         
075500     END-IF                                                               
075600                                                                          
075700     PERFORM IMS-CHECKPOINT                                               
075800     MOVE ZERO                 TO CHKP-ANT                                
075900     .                                                                    
076000     EJECT                                                                
076100* --- IMS SEKTIONER ---                                                   
076200                                                                          
076300 IMS-GU-ARTC01 SECTION.                                                   
076400                                                                          
076500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
076600          DELIMITED BY SIZE INTO SSA1                                     
076700     MOVE '  GE' TO GODK-STATUSKODER                                      
076800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
076900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
077000     PERFORM IMS-STATUSKONTROLL                                           
077100     .                                                                    
077200     EJECT                                                                
077300 IMS-GHNP-ARTC11 SECTION.                                                 
077400                                                                          
077500     MOVE 'WLARTC11 ' TO SSA1                                             
077600     MOVE '  GE' TO GODK-STATUSKODER                                      
077700     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA2 SSA1                   
077800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     EJECT                                                                
078200 IMS-REPL-ARTC   SECTION.                                                 
078300                                                                          
078400     MOVE '  ' TO GODK-STATUSKODER                                        
078500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA2                        
078600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900     EJECT                                                                
079000 IMS-ISRT-INLA01 SECTION.                                                 
079100                                                                          
079200     MOVE 'W6INLA01 ' TO SSA1                                             
079300     MOVE '  II' TO GODK-STATUSKODER                                      
079400     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1                    
079500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
079600     PERFORM IMS-STATUSKONTROLL                                           
079700     .                                                                    
079800     EJECT                                                                
079900 IMS-GHU-INLA11 SECTION.                                                  
080000                                                                          
080100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
080200          DELIMITED BY SIZE INTO SSA1                                     
080300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
080400          DELIMITED BY SIZE INTO SSA2                                     
080500     MOVE '  ' TO GODK-STATUSKODER                                        
080600     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1 SSA2                
080700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSKONTROLL                                           
080900     .                                                                    
081000     EJECT                                                                
081100 IMS-GHU-INLA21 SECTION.                                                  
081200                                                                          
081300     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
081400          DELIMITED BY SIZE INTO SSA1                                     
081500     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
081600          DELIMITED BY SIZE INTO SSA2                                     
081700     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
081800          DELIMITED BY SIZE INTO SSA3                                     
081900     MOVE '  ' TO GODK-STATUSKODER                                        
082000     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
082100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
082200     PERFORM IMS-STATUSKONTROLL                                           
082300     .                                                                    
082400     EJECT                                                                
082500 IMS-GU-INLA11-LAST SECTION.                                              
082600                                                                          
082700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
082800          DELIMITED BY SIZE INTO SSA1                                     
082900     MOVE 'W6INLA11*L'        TO SSA2                                     
083000     MOVE '  GE' TO GODK-STATUSKODER                                      
083100     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1 SSA2                 
083200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     EJECT                                                                
083600 IMS-REPL-INLA   SECTION.                                                 
083700                                                                          
083800     MOVE '  ' TO GODK-STATUSKODER                                        
083900     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA                         
084000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
084100     PERFORM IMS-STATUSKONTROLL                                           
084200     .                                                                    
084300     EJECT                                                                
084400 IMS-ISRT-INLA11 SECTION.                                                 
084500                                                                          
084600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
084700          DELIMITED BY SIZE INTO SSA1                                     
084800     MOVE 'W6INLA11 ' TO SSA2                                             
084900     MOVE '  ' TO GODK-STATUSKODER                                        
085000     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1 SSA2               
085100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
085200     PERFORM IMS-STATUSKONTROLL                                           
085300     .                                                                    
085400     EJECT                                                                
085500 IMS-ISRT-INLA21 SECTION.                                                 
085600                                                                          
085700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
085800          DELIMITED BY SIZE INTO SSA1                                     
085900     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
086000          DELIMITED BY SIZE INTO SSA2                                     
086100     MOVE 'W6INLA21 ' TO SSA3                                             
086200     MOVE '  ' TO GODK-STATUSKODER                                        
086300     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
086400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     EJECT                                                                
086800 IMS-GU-BENA11    SECTION.                                                
086900                                                                          
087000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
087100          DELIMITED BY SIZE INTO SSA1                                     
087200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
087300          DELIMITED BY SIZE INTO SSA2                                     
087400     MOVE '  GE' TO GODK-STATUSKODER                                      
087500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
087600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     .                                                                    
087900     EJECT                                                                
088000 IMS-GHU-LOPA11 SECTION.                                                  
088100     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
088200          DELIMITED BY SIZE INTO SSA1                                     
088300     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
088400          DELIMITED BY SIZE INTO SSA2                                     
088500     MOVE '    ' TO GODK-STATUSKODER                                      
088600     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA6 SSA1 SSA2               
088700     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
088800     PERFORM IMS-STATUSKONTROLL                                           
088900     .                                                                    
089000     SKIP3                                                                
089100 IMS-REPL-LOPA11 SECTION.                                                 
089200     MOVE '    ' TO GODK-STATUSKODER                                      
089300     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA6                        
089400     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
089500     PERFORM IMS-STATUSKONTROLL                                           
089600     .                                                                    
089700     EJECT                                                                
089800 IMS-GU-INLE01   SECTION.                                                 
089900     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
090000          DELIMITED BY SIZE INTO SSA1                                     
090100     MOVE '  GE' TO GODK-STATUSKODER                                      
090200     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA7 SSA1                     
090300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600     SKIP3                                                                
090700 IMS-ISRT-INLE01   SECTION.                                               
090800     MOVE 'WLINLE01 ' TO SSA1                                             
090900     MOVE '  ' TO GODK-STATUSKODER                                        
091000     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA7 SSA1                   
091100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
091200     PERFORM IMS-STATUSKONTROLL                                           
091300     .                                                                    
091400     SKIP3                                                                
091500 IMS-ISRT-INLE11   SECTION.                                               
091600     MOVE 'WLINLE11 ' TO SSA1                                             
091700     MOVE '  II' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA7 SSA1                   
091900     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     SKIP3                                                                
092300 IMS-ISRT-INLE21   SECTION.                                               
092400     MOVE 'WLINLE21 ' TO SSA1                                             
092500     MOVE '  ' TO GODK-STATUSKODER                                        
092600     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA7 SSA1                   
092700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
092800     PERFORM IMS-STATUSKONTROLL                                           
092900     .                                                                    
093000     EJECT                                                                
093100 IMS-LAS-ATERSTART SECTION.                                               
093200     SKIP2                                                                
093300     STRING 'W6CKPH01(W6GXKEY  =' W-W6GX-6029-KEY-X ')'                   
093400            DELIMITED BY SIZE INTO SSA1                                   
093500     STRING 'W6CKPH11(KDSEGKEY =' W-W6GX-6030-KEY-X ')'                   
093600            DELIMITED BY SIZE INTO SSA2                                   
093700     MOVE '  GE' TO GODK-STATUSKODER                                      
093800     CALL CBLTDLI USING GHU CKPH-PCB DLI-IO-AREA SSA1 SSA2                
093900     MOVE CKPH-STATUS-CODE TO STATUS-WS                                   
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     EJECT                                                                
094300 IMS-ISRT-ATERSTART SECTION.                                              
094400     SKIP2                                                                
094500     STRING 'W6CKPH01(W6GXKEY  =' W-W6GX-6029-KEY-X ')'                   
094600          DELIMITED BY SIZE INTO SSA1                                     
094700     MOVE 'W6CKPH11' TO SSA2                                              
094800     MOVE '  ' TO GODK-STATUSKODER                                        
094900     CALL CBLTDLI USING ISRT CKPH-PCB DLI-IO-AREA SSA1 SSA2               
095000     MOVE CKPH-STATUS-CODE TO STATUS-WS                                   
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     .                                                                    
095300     EJECT                                                                
095400 IMS-REPL-ATERSTART SECTION.                                              
095500     SKIP2                                                                
095600     MOVE SPACE TO GODK-STATUSKODER                                       
095700     CALL CBLTDLI USING REPL CKPH-PCB DLI-IO-AREA                         
095800     MOVE CKPH-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     EJECT                                                                
096200 IMS-RESTART SECTION.                                                     
096300     SKIP2                                                                
096400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
096500     MOVE '  ' TO GODK-STATUSKODER                                        
096600     CALL CBLTDLI USING XRST MSG-PCB                                      
096700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
096800                        CHKP-AREA-LENGTH CHKP-AREA                        
096900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     EJECT                                                                
097300 IMS-CHECKPOINT SECTION.                                                  
097400     SKIP2                                                                
097500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
097600     MOVE '  XD' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING CHKP MSG-PCB                                      
097800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
097900                        CHKP-AREA-LENGTH CHKP-AREA                        
098000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098100     PERFORM IMS-STATUSKONTROLL                                           
098200                                                                          
098300     IF IMS-EJ-OK                                                         
098400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
098500       CALL FELLOG                                                        
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 IMS-ISRT-WDL901 SECTION.                                                 
099000                                                                          
099100     MOVE 'WLLOGA01 ' TO SSA1                                             
099200     MOVE '  II' TO GODK-STATUSKODER                                      
099300     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
099400     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
099500     PERFORM IMS-STATUSKONTROLL                                           
099600     .                                                                    
099700     EJECT                                                                
099710 IMS-GU-WDB601-LEV SECTION.                                               
099720     STRING 'WDB601  (IDLEVNDC =' W-IDDC-B6-LEV-X ')'                     
099730          DELIMITED BY SIZE INTO SSA1                                     
099740     MOVE '  GE' TO GODK-STATUSKODER                                      
099750     CALL CBLTDLI USING GU WDB6-LEV-PCB DLI-IO-AREA-B601-LEV SSA1         
099760     MOVE WDB6-LEV-STATUS-CODE    TO STATUS-WS                            
099770     PERFORM IMS-STATUSKONTROLL                                           
099780     .                                                                    
099790     EJECT                                                                
099800 IMS-STATUSKONTROLL SECTION.                                              
099900     SKIP2                                                                
100000     SET STATUS-IX TO 1                                                   
100100     SEARCH GODK-STATUS                                                   
100200       AT END                                                             
100300         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
100400         DISPLAY FELTEXT                                                  
100500         CALL FELLOG                                                      
100600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100700         CONTINUE                                                         
100800     END-SEARCH                                                           
100900     .                                                                    
