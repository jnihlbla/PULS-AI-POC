000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2241300.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/02/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERING NY LEVERANSPLAN                                      
001000*        FIL W22413 FRÅN W2241200                                         
001100*        POSTTYP 001 UPPDATERING WDK711                                   
001200*        POSTTYP 002 UPPDATERING WDK722                                   
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WDK7                                       
001500*        PROGRAMMET UPPDATERAR WDD9                                       
001600*        PROGRAMMET UPPDATERAR WDD6                                       
001610*        PROGRAMMET UPPDATERAR WDR3                                       
001700*                                                                         
001701*-------------------------------------------------------------            
001710* CHANGE LOG:                                                             
001720* 2015-09-10   ETRACKER 10209749    (WDD903)                              
001730*              ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.            
001740*                                                                         
001750*                                                                         
001760*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*          --- UPPDATERINGSPOSTER                                         
002600     SELECT W22413                     ASSIGN TO W22413D1.                
002700                                                                          
002800                                                                          
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100                                                                          
003200 FD  W22413                                                               
003300     RECORDING       V                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W22413      -L.                                                
003700                                                                          
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W2241300'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004600 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004700                                                                          
004800 01  WC-IDPTYP.                                                           
004900     03  UPDATE-WDK711           PIC X(3)    VALUE '001'.                 
005000     03  UPDATE-WDK722           PIC X(3)    VALUE '002'.                 
005100     03  INSERT-WDD901           PIC X(3)    VALUE '003'.                 
005200     03  INSERT-WDD902           PIC X(3)    VALUE '004'.                 
005600     03  DELETE-WDD901           PIC X(3)    VALUE '008'.                 
005700     03  DELETE-WDD904           PIC X(3)    VALUE '009'.                 
005800     03  DELETE-WDD601           PIC X(3)    VALUE '010'.                 
005900     03  DELETE-WDD905           PIC X(3)    VALUE '011'.                 
006000     03  REPLACE-WDD905          PIC X(3)    VALUE '012'.                 
006100     03  INSERT-WDD905           PIC X(3)    VALUE '013'.                 
006200                                                                          
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600                                                                          
006610 77  W-ANTAL-POSTER              PIC 9(7)    VALUE ZERO.                  
006620                                                                          
006700 77  W22413-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W22413                       VALUE 'J'.                   
006900                                                                          
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500                                                                          
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700*                                                                         
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008100                                                                          
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500                                                                          
008600 01  UPD-AREA-START              PIC X(24)   VALUE                        
008700                                             'UPD-AREA-START'.            
008800                                                                          
008900*01  UPD-AREA -COPY W22413                                                
009000*                                                                         
009100                                                                          
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009310                                                                          
009320 01  CHKP-VAR.                                                            
009330 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
009340 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
009350 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
009360 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
009370 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
009380 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
009400                                                                          
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009800     03  W-IDDC-X.                                                        
009900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010000                                                                          
010100     03  W-WDD901KY-X.                                                    
010200         05 W-IDARTNR-D9         PIC S9(9)   VALUE ZERO COMP-3.           
010300         05 W-IDDC-D9            PIC X(2)    VALUE SPACE.                 
010400                                                                          
010500     03  W-IDLEVNR-X.                                                     
010600         05 W-IDLEVNR            PIC  X(5)   VALUE SPACE.                 
010700                                                                          
011100     03  W-DASPECST-X.                                                    
011200         05 W-DASPECST           PIC  9(6)   VALUE ZERO.                  
011300                                                                          
011400     03  W-WDD905KY-X.                                                    
011500         05 W-DAAVROP-AVS        PIC  9(6)   VALUE ZERO.                  
011600         05 W-TILEVDAG           PIC S9      VALUE ZERO COMP-3.           
011610                                                                          
011620     03  W-KDAVROP-X.                                                     
011630         05 W-KDAVROP            PIC  S9     VALUE ZERO COMP-3.           
011700                                                                          
011800     03  W-WDD601KY-MIN-X.                                                
011900         05 W-IDDC-D6-MIN     PIC  X(2)   VALUE SPACE.                    
012000         05 FILLER            PIC  X(12)  VALUE LOW-VALUE.                
012201     03  W-WDD601KY-MAX-X.                                                
012202         05 W-IDDC-D6-MAX     PIC  X(2)   VALUE SPACE.                    
012203         05 FILLER            PIC  X(12)  VALUE HIGH-VALUE.               
012204                                                                          
012206     03  W-IDARTNR-D6-X.                                                  
012207         05 W-IDARTNR-D6      PIC S9(9)   VALUE ZERO COMP-3.              
012208                                                                          
012210     03  W-WDGXKEY-4579-X.                                                
012220          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
012230          05 W-IDPGM             PIC X(8)    VALUE 'W2241300'.            
012240          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
012300                                                                          
012400                                                                          
012500                                                                          
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FINNS                       VALUE '  '.                  
012900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013200     88  IMS-EJ-OK                           VALUE 'XD'.                  
013300                                                                          
013400 01  GODK-STATUSKODER.                                                    
013500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013600                                                                          
013700 01  ALL-SSA.                                                             
013800     03 SSA1                     PIC X(200).                              
013900     03 SSA2                     PIC X(200).                              
014000     03 SSA3                     PIC X(200).                              
014100                                                                          
014200                                                                          
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500                                                                          
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700                                                                          
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
014900 01  DLI-IO-WDK711.                                                       
015000*    03  -COPY WDK711                                                     
015100                                                                          
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
015300 01  DLI-IO-WDK722.                                                       
015400*    03  -COPY WDK722                                                     
015500                                                                          
015600                                                                          
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
015800 01  DLI-IO-WDD901.                                                       
015900*    03  -COPY WDD901  -PRE D901-                                         
016000                                                                          
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
016200 01  DLI-IO-WDD902.                                                       
016300*    03  -COPY WDD902  -PRE D902-                                         
016800                                                                          
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
017000 01  DLI-IO-WDD904.                                                       
017100*    03  -COPY WDD904  -PRE D904-                                         
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
017400 01  DLI-IO-WDD905.                                                       
017500*    03  -COPY WDD905  -PRE D905-                                         
017600                                                                          
017700                                                                          
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
017900 01  DLI-IO-WDD601.                                                       
018000*    03  -COPY WDD601  -PRE D601-                                         
018010                                                                          
018020                                                                          
018030 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
018040 01  DLI-IO-WDGX4580.                                                     
018050*    03  -COPY WDGX4580                                                   
018100                                                                          
018200                                                                          
018300 LINKAGE SECTION.                                                         
018400                                                                          
018500*01  -COPY W0009   -PRE MSG-                                              
018600                                                                          
018700*01  -COPY W0008  -PRE WDK7-                                              
018800     05  FILLER                  PIC X.                                   
018900                                                                          
019000*01  -COPY W0008  -PRE WDD9-                                              
019100     05  FILLER                  PIC X.                                   
019200                                                                          
019300*01  -COPY W0008  -PRE WDD6-                                              
019400     05  FILLER                  PIC X.                                   
019410                                                                          
019420*01  -COPY W0008  -PRE 4579-                                              
019430     05  FILLER                  PIC X.                                   
019500                                                                          
019600                                                                          
019700 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDD9-PCB WDD6-PCB             
019710                                   4579-PCB.                              
019800 MAIN SECTION.                                                            
019900     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDD9-PCB WDD6-PCB             
020000                                   4579-PCB.                              
020100                                                                          
020200     PERFORM A-INIT                                                       
020300     PERFORM S01-LAES-W22413                                              
020400     PERFORM UNTIL END-OF-W22413                                          
020500       EVALUATE UPD-IDPTYP                                                
020600          WHEN UPDATE-WDK711                                              
020700               PERFORM B-UPDATE-WDK711                                    
020800                                                                          
020900          WHEN UPDATE-WDK722                                              
021000               PERFORM C-UPDATE-WDK722                                    
021100                                                                          
021200          WHEN INSERT-WDD901                                              
021300               PERFORM D-INSERT-WDD901                                    
021400                                                                          
021500          WHEN INSERT-WDD902                                              
021600               PERFORM E-INSERT-WDD902                                    
021700                                                                          
022700          WHEN DELETE-WDD901                                              
022800               PERFORM I-DELETE-WDD901                                    
022900                                                                          
023000          WHEN DELETE-WDD904                                              
023100               PERFORM J-DELETE-WDD904                                    
023200                                                                          
023300          WHEN DELETE-WDD601                                              
023400               PERFORM K-DELETE-WDD601                                    
023500                                                                          
023600          WHEN DELETE-WDD905                                              
023700               PERFORM L-DELETE-WDD905                                    
023800                                                                          
023900          WHEN REPLACE-WDD905                                             
024000               PERFORM M-REPLACE-WDD905                                   
024100                                                                          
024200          WHEN INSERT-WDD905                                              
024300               PERFORM N-INSERT-WDD905                                    
024400                                                                          
024500       END-EVALUATE                                                       
024600                                                                          
024610       IF CHKP-ANT > CHKP-MAX                                             
024620          PERFORM X-TAG-CHECKPOINT                                        
024630       END-IF                                                             
024700       PERFORM S01-LAES-W22413                                            
024800     END-PERFORM                                                          
024900                                                                          
025000                                                                          
025100     PERFORM Z-FINIT                                                      
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600                                                                          
025700                                                                          
025800 A-INIT SECTION.                                                          
025900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
026000                                                                          
026100     OPEN INPUT W22413                                                    
026200                                                                          
026300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026310                                                                          
026320     PERFORM IMS-RESTART                                                  
026330                                                                          
026340     PERFORM IMS-GHU-RESTART                                              
026350     IF 4580-KVPOST > ZERO                                                
026360        MOVE ZERO TO W-ANTAL-POSTER                                       
026370        PERFORM UNTIL W-ANTAL-POSTER = 4580-KVPOST                        
026380           PERFORM S01-LAES-W22413                                        
026390           ADD 1  TO W-ANTAL-POSTER                                       
026391        END-PERFORM                                                       
026392     END-IF                                                               
026400     .                                                                    
026500                                                                          
026600                                                                          
026700 B-UPDATE-WDK711 SECTION.                                                 
026800     MOVE 'B-UPD-WDK711    ' TO CURRENT-SECTION                           
026900                                                                          
027000     MOVE UPD-IDARTNR        TO W-IDARTNR                                 
027100     MOVE UPD-IDDC           TO W-IDDC                                    
027200     PERFORM IMS-GHU-WDK711                                               
027300                                                                          
027400     MOVE UPD-KVREFBER       TO SLAG-KVREFBER                             
027500     MOVE UPD-KVREFPKT       TO SLAG-KVREFPKT                             
027600     MOVE UPD-KVREFOVL       TO SLAG-KVREFOVL                             
027700     MOVE UPD-TIREFPAF       TO SLAG-TIREFPAF                             
027800     MOVE UPD-TIREFPKT       TO SLAG-TIREFPKT                             
027900                                                                          
028000     PERFORM IMS-REPL-WDK711                                              
028100     .                                                                    
028200                                                                          
028300                                                                          
028400 C-UPDATE-WDK722 SECTION.                                                 
028500     MOVE 'B-UPD-WDK722    ' TO CURRENT-SECTION                           
028600                                                                          
028700     MOVE UPD-IDARTNR        TO W-IDARTNR                                 
028800     MOVE UPD-IDDC           TO W-IDDC                                    
028900     PERFORM IMS-GHU-WDK722                                               
029000                                                                          
029010     IF SEGMENT-FINNS                                                     
029020* --- TESTEN BORDE NOG INTE BEHÖVAS, MEN I ALLA FALL DEVE                 
029030* --- ÄR LITE KNACKIG...                                                  
029100     MOVE UPD-KVSLAGER          TO XLAG-KVSLAGER                          
029200     MOVE UPD-KVEOQ             TO XLAG-KVEOQ                             
029300     MOVE UPD-TIMANSEC          TO XLAG-TIMANSEC                          
029400     MOVE UPD-KDLPSP            TO XLAG-KDLPSP                            
029500     MOVE UPD-KVSLUTKP          TO XLAG-KVSLUTKP                          
029600     MOVE UPD-TILPSP            TO XLAG-TILPSP                            
029700     MOVE UPD-KDLEVPLF          TO XLAG-KDLEVPLF                          
029800     MOVE UPD-DAPBPLAN          TO XLAG-DAPBPLAN                          
029900     MOVE UPD-KVPB-PLAN         TO XLAG-KVPB-PLAN                         
030000     MOVE UPD-KVSLAGER          TO XLAG-KVSLAGER                          
030100     MOVE UPD-DASEASON          TO XLAG-DASEASON                          
030200     MOVE UPD-RESEASON-PLAN(01) TO XLAG-RESEASON-PLAN(01)                 
030300     MOVE UPD-RESEASON-PLAN(02) TO XLAG-RESEASON-PLAN(02)                 
030400     MOVE UPD-RESEASON-PLAN(03) TO XLAG-RESEASON-PLAN(03)                 
030500     MOVE UPD-RESEASON-PLAN(04) TO XLAG-RESEASON-PLAN(04)                 
030600     MOVE UPD-RESEASON-PLAN(05) TO XLAG-RESEASON-PLAN(05)                 
030700     MOVE UPD-RESEASON-PLAN(06) TO XLAG-RESEASON-PLAN(06)                 
030800     MOVE UPD-RESEASON-PLAN(07) TO XLAG-RESEASON-PLAN(07)                 
030900     MOVE UPD-RESEASON-PLAN(08) TO XLAG-RESEASON-PLAN(08)                 
031000     MOVE UPD-RESEASON-PLAN(09) TO XLAG-RESEASON-PLAN(09)                 
031100     MOVE UPD-RESEASON-PLAN(10) TO XLAG-RESEASON-PLAN(10)                 
031200     MOVE UPD-RESEASON-PLAN(11) TO XLAG-RESEASON-PLAN(11)                 
031300     MOVE UPD-RESEASON-PLAN(12) TO XLAG-RESEASON-PLAN(12)                 
031400                                                                          
031500     PERFORM IMS-REPL-WDK722                                              
031510     END-IF                                                               
031600     .                                                                    
031700                                                                          
031800                                                                          
031900 D-INSERT-WDD901 SECTION.                                                 
032000     MOVE 'D-INSERT-WDD901 ' TO CURRENT-SECTION                           
032100                                                                          
032200     MOVE UPD-IDARTNR        TO D901-IDARTNR                              
032300     MOVE UPD-IDDC           TO D901-IDDC                                 
032400                                                                          
032500     PERFORM IMS-ISRT-WDD901                                              
032600     .                                                                    
032700                                                                          
032800                                                                          
032900 E-INSERT-WDD902 SECTION.                                                 
033000     MOVE 'E-INSERT-WDD902 ' TO CURRENT-SECTION                           
033100                                                                          
033200     MOVE UPD-IDARTNR        TO W-IDARTNR-D9                              
033300     MOVE UPD-IDDC           TO W-IDDC-D9                                 
033400     MOVE UPD-IDLEVNR        TO D902-IDLEVNR                              
033500     MOVE ZERO               TO D902-KVBR                                 
033510                                D902-TILEVPL                              
033600                                                                          
033700     PERFORM IMS-ISRT-WDD902                                              
033800     .                                                                    
033900                                                                          
034000                                                                          
038300 I-DELETE-WDD901 SECTION.                                                 
038400     MOVE 'I-DELETE-WDD901 ' TO CURRENT-SECTION                           
038500                                                                          
038600     MOVE UPD-IDARTNR        TO W-IDARTNR-D9                              
038700     MOVE UPD-IDDC           TO W-IDDC-D9                                 
038800                                                                          
038900     PERFORM IMS-GHU-WDD901                                               
039000     PERFORM IMS-DLET-WDD901                                              
039100     .                                                                    
039200                                                                          
039300                                                                          
039400 J-DELETE-WDD904 SECTION.                                                 
039500     MOVE 'J-DELETE-WDD904 ' TO CURRENT-SECTION                           
039600                                                                          
039700     MOVE UPD-IDARTNR        TO W-IDARTNR-D9                              
039800     MOVE UPD-IDDC           TO W-IDDC-D9                                 
039900     MOVE UPD-IDLEVNR        TO W-IDLEVNR                                 
040100                                                                          
040200     PERFORM IMS-GHU-WDD904                                               
040300     PERFORM IMS-DLET-WDD904                                              
040400     .                                                                    
040500                                                                          
040600                                                                          
040700 K-DELETE-WDD601 SECTION.                                                 
040800     MOVE 'K-DELETE-WDD601 ' TO CURRENT-SECTION                           
040900                                                                          
041100     MOVE UPD-IDDC           TO W-IDDC-D6-MIN                             
041110                                W-IDDC-D6-MAX                             
041200     MOVE UPD-IDARTNR        TO W-IDARTNR-D6                              
041400                                                                          
041500     PERFORM IMS-GHU-WDD601                                               
041510     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
041600        PERFORM IMS-DLET-WDD601                                           
041610        PERFORM IMS-GHN-WDD601                                            
041620     END-PERFORM                                                          
041700     .                                                                    
041800                                                                          
041900                                                                          
042000 L-DELETE-WDD905 SECTION.                                                 
042100     MOVE 'L-DELETE-WDD905 ' TO CURRENT-SECTION                           
042200                                                                          
042300     MOVE UPD-IDARTNR        TO W-IDARTNR-D9                              
042400     MOVE UPD-IDDC           TO W-IDDC-D9                                 
042500     MOVE UPD-IDLEVNR        TO W-IDLEVNR                                 
042600     MOVE UPD-DAAVROP-AVS    TO W-DAAVROP-AVS                             
042700     MOVE UPD-TILEVDAG       TO W-TILEVDAG                                
042710     MOVE UPD-KDAVROP        TO W-KDAVROP                                 
042800                                                                          
042900     PERFORM IMS-GHU-WDD905                                               
042910     IF  SEGMENT-FINNS                                                    
043000        PERFORM IMS-DLET-WDD905                                           
043010     END-IF                                                               
043100     .                                                                    
043200                                                                          
043300                                                                          
043400 M-REPLACE-WDD905 SECTION.                                                
043500     MOVE 'M-REPLACE-WDD905' TO CURRENT-SECTION                           
043600                                                                          
043700     MOVE UPD-IDARTNR        TO W-IDARTNR-D9                              
043800     MOVE UPD-IDDC           TO W-IDDC-D9                                 
043900     MOVE UPD-IDLEVNR        TO W-IDLEVNR                                 
044000     MOVE UPD-DAAVROP-AVS    TO W-DAAVROP-AVS                             
044100     MOVE UPD-TILEVDAG       TO W-TILEVDAG                                
044110     MOVE UPD-KDAVROP        TO W-KDAVROP                                 
044200                                                                          
044300     PERFORM IMS-GHU-WDD905                                               
044310     IF SEGMENT-FINNS                                                     
044400        MOVE UPD-KVAVROP     TO D905-KVAVROP                              
044500        PERFORM IMS-REPL-WDD905                                           
044510     END-IF                                                               
044600     .                                                                    
044700                                                                          
044800                                                                          
044900 N-INSERT-WDD905 SECTION.                                                 
045000     MOVE 'N-INSERT-WDD905 ' TO CURRENT-SECTION                           
045100                                                                          
045200     MOVE UPD-IDARTNR        TO W-IDARTNR-D9                              
045300     MOVE UPD-IDDC           TO W-IDDC-D9                                 
045400     MOVE UPD-IDLEVNR        TO W-IDLEVNR                                 
045500                                                                          
045600     MOVE UPD-KDAVROP        TO D905-KDAVROP                              
045700     MOVE UPD-DAAVROP-AVS    TO D905-DAAVROP-AVS                          
045800     MOVE UPD-TILEVDAG       TO D905-TILEVDAG                             
045900     MOVE UPD-TIAVRDAT-INL   TO D905-TIAVRDAT-INL                         
046000     MOVE UPD-TIAVRDAT-DISP  TO D905-TIAVRDAT-DISP                        
046100     MOVE UPD-KVAVROP        TO D905-KVAVROP                              
046200                                                                          
046300     PERFORM IMS-ISRT-WDD905                                              
046400     .                                                                    
046500                                                                          
046600                                                                          
046700 Z-FINIT SECTION.                                                         
046800     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
046900                                                                          
047000     CLOSE W22413                                                         
047100                                                                          
047200     MOVE 'S' TO POSTSUM-OPKOD                                            
047300     CALL POSTSUM USING POSTSUM-PARM                                      
047310                                                                          
047320     PERFORM IMS-GHU-RESTART                                              
047330     MOVE ZERO       TO 4580-KVPOST                                       
047340     ACCEPT 4580-TIUPPDAT FROM DATE                                       
047350     ACCEPT 4580-TIUPPTID FROM TIME                                       
047360     PERFORM IMS-REPL-RESTART                                             
047400     .                                                                    
047500                                                                          
047600                                                                          
047700 S01-LAES-W22413  SECTION.                                                
047800                                                                          
047900     READ W22413 INTO UPD-AREA                                            
048000     AT END                                                               
048100        SET END-OF-W22413 TO TRUE                                         
048200                                                                          
048300     NOT AT END                                                           
048400        MOVE 'W22413' TO POSTSUM-FDNAMN                                   
048500        MOVE 'W22413D1' TO POSTSUM-DDNAMN2                                
048600        MOVE UPD-IDPTYP TO POSTSUM-TRANSTYP                               
048700        CALL POSTSUM USING POSTSUM-PARM                                   
048710                                                                          
048720        ADD 1 TO W-ANTAL-POSTER                                           
048800     END-READ                                                             
048900     .                                                                    
048910                                                                          
048920                                                                          
048930 X-TAG-CHECKPOINT   SECTION.                                              
048931                                                                          
048932     PERFORM IMS-GHU-RESTART                                              
048933     MOVE W-ANTAL-POSTER TO 4580-KVPOST                                   
048934     ACCEPT 4580-TIUPPDAT FROM DATE                                       
048935     ACCEPT 4580-TIUPPTID FROM TIME                                       
048936     PERFORM IMS-REPL-RESTART                                             
048940                                                                          
048950     PERFORM IMS-CHECKPOINT                                               
048960     MOVE ZERO TO CHKP-ANT                                                
048970     .                                                                    
049000                                                                          
049100                                                                          
049200* --- IMS SEKTIONER ---                                                   
049210                                                                          
049220                                                                          
049230 IMS-RESTART SECTION.                                                     
049240     MOVE 'IMS-RESTART     ' TO CURRENT-IMS-SECTION                       
049250                                                                          
049260     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
049270     MOVE '  ' TO GODK-STATUSKODER                                        
049280     CALL CBLTDLI USING XRST MSG-PCB                                      
049290                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
049291                        CHKP-AREA-LENGTH CHKP-AREA                        
049292     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049293     PERFORM IMS-STATUSKONTROLL                                           
049294     .                                                                    
049295                                                                          
049296                                                                          
049297 IMS-CHECKPOINT SECTION.                                                  
049298     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
049299                                                                          
049300     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
049301     MOVE '  XD' TO GODK-STATUSKODER                                      
049302     CALL CBLTDLI USING CHKP MSG-PCB                                      
049303                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
049304                        CHKP-AREA-LENGTH CHKP-AREA                        
049305     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049306     PERFORM IMS-STATUSKONTROLL                                           
049307                                                                          
049308     IF IMS-EJ-OK                                                         
049309       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
049310       DISPLAY FELTEXT                                                    
049311       CALL FELLOG                                                        
049312     END-IF                                                               
049313     .                                                                    
049314                                                                          
049320                                                                          
049400 IMS-GHU-WDK711 SECTION.                                                  
049500     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
049600                                                                          
049700     MOVE SPACE               TO ALL-SSA                                  
049800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
049900          DELIMITED BY SIZE INTO SSA1                                     
050000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
050100          DELIMITED BY SIZE INTO SSA2                                     
050200     MOVE '  '                TO GODK-STATUSKODER                         
050300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
050400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700                                                                          
050800                                                                          
050900 IMS-REPL-WDK711 SECTION.                                                 
051000     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
051100                                                                          
051200     MOVE SPACE               TO ALL-SSA                                  
051300     MOVE '  '                TO GODK-STATUSKODER                         
051400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
051500     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
051600     PERFORM IMS-STATUSKONTROLL                                           
051610     ADD +2 TO CHKP-ANT                                                   
051700     .                                                                    
051800                                                                          
051900                                                                          
052000 IMS-GHU-WDK722 SECTION.                                                  
052100     MOVE 'IMS-GHU-WDK722  ' TO CURRENT-IMS-SECTION                       
052200                                                                          
052300     MOVE SPACE               TO ALL-SSA                                  
052400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
052500          DELIMITED BY SIZE INTO SSA1                                     
052600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
052700          DELIMITED BY SIZE INTO SSA2                                     
052800     MOVE 'WDK722 '           TO SSA3                                     
052900     MOVE '  GE'              TO GODK-STATUSKODER                         
053000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
053100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
053200     PERFORM IMS-STATUSKONTROLL                                           
053300     .                                                                    
053400                                                                          
053500                                                                          
053600 IMS-REPL-WDK722 SECTION.                                                 
053700     MOVE 'IMS-REPL-WDK722 ' TO CURRENT-IMS-SECTION                       
053800                                                                          
053900     MOVE SPACE               TO ALL-SSA                                  
054000     MOVE '  '                TO GODK-STATUSKODER                         
054100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
054200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
054300     PERFORM IMS-STATUSKONTROLL                                           
054310     ADD +1 TO CHKP-ANT                                                   
054400     .                                                                    
054500                                                                          
054600                                                                          
054700 IMS-GHU-WDD901 SECTION.                                                  
054800     MOVE 'IMS-GHU-WDD901  ' TO CURRENT-IMS-SECTION                       
054900                                                                          
055000     MOVE SPACE               TO ALL-SSA                                  
055100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
055200          DELIMITED BY SIZE INTO SSA1                                     
055300     MOVE '  '                TO GODK-STATUSKODER                         
055400     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD901 SSA1                  
055500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
055600     PERFORM IMS-STATUSKONTROLL                                           
055700     .                                                                    
055800                                                                          
055900                                                                          
056000 IMS-ISRT-WDD901 SECTION.                                                 
056100     MOVE 'IMS-ISRT-WDD901 ' TO CURRENT-IMS-SECTION                       
056200                                                                          
056300     MOVE SPACE               TO ALL-SSA                                  
056400     MOVE 'WDD901 '           TO SSA1                                     
056500     MOVE '  '                TO GODK-STATUSKODER                         
056600     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD901 SSA1                  
056700     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
056800     PERFORM IMS-STATUSKONTROLL                                           
056810     ADD +1 TO CHKP-ANT                                                   
056900     .                                                                    
057000                                                                          
057100                                                                          
057200 IMS-DLET-WDD901 SECTION.                                                 
057300     MOVE 'IMS-DLET-WDD901 ' TO CURRENT-IMS-SECTION                       
057400                                                                          
057500     MOVE SPACE               TO ALL-SSA                                  
057600     MOVE '  '                TO GODK-STATUSKODER                         
057700     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD901                       
057800     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
057900     PERFORM IMS-STATUSKONTROLL                                           
057910     ADD +1 TO CHKP-ANT                                                   
058000     .                                                                    
058100                                                                          
058200                                                                          
058300 IMS-ISRT-WDD902 SECTION.                                                 
058400     MOVE 'IMS-ISRT-WDD902 ' TO CURRENT-IMS-SECTION                       
058500                                                                          
058600     MOVE SPACE               TO ALL-SSA                                  
058700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
058800          DELIMITED BY SIZE INTO SSA1                                     
058900     MOVE 'WDD902 '           TO SSA2                                     
059000     MOVE '  II'              TO GODK-STATUSKODER                         
059100     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
059200     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
059300     PERFORM IMS-STATUSKONTROLL                                           
059310     ADD +2 TO CHKP-ANT                                                   
059400     .                                                                    
059500                                                                          
059600                                                                          
059700 IMS-DLET-WDD902 SECTION.                                                 
059800     MOVE 'IMS-DLET-WDD902 ' TO CURRENT-IMS-SECTION                       
059900                                                                          
060000     MOVE SPACE               TO ALL-SSA                                  
060100     MOVE '  '                TO GODK-STATUSKODER                         
060200     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD902                       
060300     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
060400     PERFORM IMS-STATUSKONTROLL                                           
060410     ADD +2 TO CHKP-ANT                                                   
060500     .                                                                    
060600                                                                          
060700                                                                          
066200 IMS-GHU-WDD904 SECTION.                                                  
066300     MOVE 'IMS-GHU-WDD904  ' TO CURRENT-IMS-SECTION                       
066400                                                                          
066500     MOVE SPACE               TO ALL-SSA                                  
066600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
066700          DELIMITED BY SIZE INTO SSA1                                     
066800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
066900          DELIMITED BY SIZE INTO SSA2                                     
067000     MOVE   'WDD904 '         TO SSA3                                     
067200     MOVE '  '                TO GODK-STATUSKODER                         
067300     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3        
067400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     .                                                                    
067700                                                                          
067800                                                                          
067900 IMS-DLET-WDD904 SECTION.                                                 
068000     MOVE 'IMS-DLET-WDD904 ' TO CURRENT-IMS-SECTION                       
068100                                                                          
068200     MOVE SPACE               TO ALL-SSA                                  
068300     MOVE '  '                TO GODK-STATUSKODER                         
068400     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
068500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
068600     PERFORM IMS-STATUSKONTROLL                                           
068610     ADD +1 TO CHKP-ANT                                                   
068700     .                                                                    
068800                                                                          
068900                                                                          
069000 IMS-GHU-WDD905 SECTION.                                                  
069100     MOVE 'IMS-GHU-WDD905  ' TO CURRENT-IMS-SECTION                       
069200                                                                          
069300     MOVE SPACE               TO ALL-SSA                                  
069400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
069500          DELIMITED BY SIZE INTO SSA1                                     
069600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
069700          DELIMITED BY SIZE INTO SSA2                                     
069800     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
069810                    '&KDAVROP  =' W-KDAVROP-X ')'                         
069900          DELIMITED BY SIZE INTO SSA3                                     
070000     MOVE '  GE'              TO GODK-STATUSKODER                         
070100     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
070200     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     .                                                                    
070500                                                                          
070600                                                                          
070700 IMS-DLET-WDD905 SECTION.                                                 
070800     MOVE 'IMS-DLET-WDD905 ' TO CURRENT-IMS-SECTION                       
070900                                                                          
071000     MOVE SPACE               TO ALL-SSA                                  
071100     MOVE '  '                TO GODK-STATUSKODER                         
071200     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
071300     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
071400     PERFORM IMS-STATUSKONTROLL                                           
071410     ADD +1 TO CHKP-ANT                                                   
071500     .                                                                    
071600                                                                          
071700                                                                          
071800 IMS-REPL-WDD905 SECTION.                                                 
071900     MOVE 'IMS-REPL-WDD905 ' TO CURRENT-IMS-SECTION                       
072000                                                                          
072100     MOVE SPACE               TO ALL-SSA                                  
072200     MOVE '  '                TO GODK-STATUSKODER                         
072300     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
072400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
072500     PERFORM IMS-STATUSKONTROLL                                           
072510     ADD +1 TO CHKP-ANT                                                   
072600     .                                                                    
072700                                                                          
072800                                                                          
072900 IMS-ISRT-WDD905 SECTION.                                                 
073000     MOVE 'IMS-ISRT-WDD905 ' TO CURRENT-IMS-SECTION                       
073100                                                                          
073200     MOVE SPACE               TO ALL-SSA                                  
073300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
073400          DELIMITED BY SIZE INTO SSA1                                     
073500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
073600          DELIMITED BY SIZE INTO SSA2                                     
073700     MOVE 'WDD905 '           TO SSA3                                     
073800     MOVE '  '                TO GODK-STATUSKODER                         
073900     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905                       
074000     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
074100     PERFORM IMS-STATUSKONTROLL                                           
074110     ADD +1 TO CHKP-ANT                                                   
074200     .                                                                    
074300                                                                          
074400                                                                          
074500 IMS-GHU-WDD601 SECTION.                                                  
074600     MOVE 'IMS-GHU-WDD601  ' TO CURRENT-IMS-SECTION                       
074700                                                                          
074800     MOVE SPACE               TO ALL-SSA                                  
074900     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
074910                    '&WDD601KY<=' W-WDD601KY-MAX-X                        
074920                    '&IDARTNR  =' W-IDARTNR-D6-X')'                       
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '  GEGB'              TO GODK-STATUSKODER                       
075200     CALL CBLTDLI USING GHU  WDD6-PCB DLI-IO-WDD601 SSA1                  
075300     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600                                                                          
075700                                                                          
075710 IMS-GHN-WDD601 SECTION.                                                  
075720     MOVE 'IMS-GHN-WDD601  ' TO CURRENT-IMS-SECTION                       
075730                                                                          
075740     MOVE SPACE               TO ALL-SSA                                  
075750     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
075760                    '&WDD601KY<=' W-WDD601KY-MAX-X                        
075770                    '&IDARTNR  =' W-IDARTNR-D6-X')'                       
075780          DELIMITED BY SIZE INTO SSA1                                     
075790     MOVE '  GEGB'              TO GODK-STATUSKODER                       
075791     CALL CBLTDLI USING GHN  WDD6-PCB DLI-IO-WDD601 SSA1                  
075792     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
075793     PERFORM IMS-STATUSKONTROLL                                           
075794     .                                                                    
075795                                                                          
075796                                                                          
075800 IMS-DLET-WDD601 SECTION.                                                 
075900     MOVE 'IMS-DLET-WDD601 ' TO CURRENT-IMS-SECTION                       
076000                                                                          
076100     MOVE SPACE               TO ALL-SSA                                  
076200     MOVE '  '                TO GODK-STATUSKODER                         
076300     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
076400     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
076500     PERFORM IMS-STATUSKONTROLL                                           
076510     ADD +1 TO CHKP-ANT                                                   
076600     .                                                                    
076610                                                                          
076620                                                                          
076630 IMS-GHU-RESTART  SECTION.                                                
076640     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
076650                                                                          
076660     MOVE SPACE          TO ALL-SSA                                       
076670     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
076680          DELIMITED BY SIZE INTO SSA1                                     
076690     MOVE 'WDR470   '    TO SSA2                                          
076691     MOVE '    '         TO GODK-STATUSKODER                              
076692     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
076693     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
076694     PERFORM IMS-STATUSKONTROLL                                           
076695     .                                                                    
076696                                                                          
076697                                                                          
076698 IMS-REPL-RESTART SECTION.                                                
076699     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
076700                                                                          
076701     MOVE '  '             TO GODK-STATUSKODER                            
076702     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
076703     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
076704     PERFORM IMS-STATUSKONTROLL                                           
076705     .                                                                    
076710                                                                          
076800                                                                          
076900 IMS-STATUSKONTROLL SECTION.                                              
077000                                                                          
077100     SET STATUS-IX TO 1                                                   
077200     SEARCH GODK-STATUS                                                   
077300       AT END                                                             
077400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077500           DELIMITED BY SIZE INTO FELTEXT                                 
077600         DISPLAY FELTEXT                                                  
077700         CALL FELLOG                                                      
077800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
077900         CONTINUE                                                         
078000     END-SEARCH                                                           
078100     .                                                                    
