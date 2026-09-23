000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6113600.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   FEB 2002.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BERÄKNA OM DET BEHÖVER FLYTTAS GODS FRÅN SVS TILL CDC            
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6   MHA SB                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
001600*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900*    E-TRACKER: 7956816                                                   
002000*        EXCLUDE VOHF-QUANTITY FROM THE CDC SUPPLY QUANTITY WHEN          
002100*        CALCULATING TRANSPORT SUGGESTIOBS FROM SVS TO RA                 
002200*                                                                         
002300*    E-TRACKER: 10254592   2015                                           
002400*        DECOMISSION VOHF                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400     SELECT W611UT                     ASSIGN TO W61136D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W611UT                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W61136  -PRE UT-    -L.                                   
004500                                                                          
004600                                                                          
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W6113600'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  AKTIV                       PIC X       VALUE 'A'.                   
005600 01  CHKP-VAR.                                                            
005700   03 CHKP-MSG-IO-AREA-LENGTH    PIC S9(9)   VALUE +32 COMP SYNC.         
005800   03 CHKP-MSG-IO-AREA           PIC X(32)   VALUE SPACE.                 
005900   03 CHKP-AREA-LENGTH           PIC S9(9)   VALUE +32 COMP SYNC.         
006000   03 CHKP-AREA                  PIC X(32)   VALUE SPACE.                 
006100   03 CHKP-ANT                   PIC S9(3)   VALUE +0.                    
006200   03 CHKP-MAX                   PIC S9(3)   VALUE +500.                  
006300                                                                          
006400*    --- INDEX SAMT MAX-INDEX                                             
006500 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006600 77  INDX                        PIC 9(2)    VALUE ZERO.                  
006700 77  IX                          PIC 9(3)    VALUE ZERO.                  
006800 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
006900 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 10.                    
007000                                                                          
007100 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
007200                                                                          
007300 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
007400 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
007500                                                                          
007600 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
007700 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
007800                                                                          
007900*    --- SWITCHAR                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008100                                                                          
008200                                                                          
008300*    --- ARBETSFÄLT                                                       
008400 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
008500 01  ARBETSFAELT.                                                         
008600     03  PERIODTABELL            OCCURS 12.                               
008700         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
008800         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
008900         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
009000         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
009100                                                                          
009200     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
009300     03  START-VV                PIC 9(2)    VALUE ZERO.                  
009400                                                                          
009500     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
009600     03  WS-IDDC                 PIC X(2)    VALUE SPACE.                 
009700     03  WS-ART-KDERS-UTG        PIC 9(3)    VALUE ZERO.                  
009800     03  WS-KDPRODSL             PIC 9(3)    VALUE ZERO.                  
009900     03  WS-ANTAL-TRAEFF         PIC 9(9)   VALUE ZERO.                   
010000     03  WS-ANTAL-WDK601         PIC 9(9)   VALUE ZERO.                   
010100     03  WS-ANTAL-WDK611         PIC 9(9)   VALUE ZERO.                   
010200     03  WS-ANTAL-WDK626         PIC 9(9)   VALUE ZERO.                   
010300     03  WS-SATSBEHOV            PIC S9(9)    VALUE ZERO.                 
010400     03  WS-SALDO                PIC S9(9)    VALUE ZERO.                 
010500     03  WS-KVRESS               PIC S9(9)    VALUE ZERO.                 
010600     03  WS-KVANTAL              PIC S9(9)    VALUE ZERO.                 
010700     03  WS-KVRETUR              PIC S9(9)    VALUE ZERO.                 
010800     03  WS-DAG-I-VECKA          PIC 9(1)   VALUE ZERO.                   
010900     03  WS-ANTAL-VECKOR         PIC 9(3)   VALUE ZERO COMP-3.            
011000     03  WS-TIAAVV-NUM           PIC 9(4)  VALUE ZERO.                    
011100     03  WS-VECKO-SEP-BEHOV      PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011200     03  WS-FAKTOR               PIC S9(1)V9(3)          COMP-3.          
011300     03  WS-ZERO                 PIC 9(4)  VALUE ZERO.                    
011400     03  WS-KVLS-SVS             PIC S9(9)    VALUE ZERO.                 
011500                                                                          
011600     03  WS-CURRENT-DATE.                                                 
011700         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
011800         05  FILLER              PIC 9(4)   VALUE ZERO.                   
011900         05  FILLER              PIC 9(6)   VALUE ZERO.                   
012000                                                                          
012100     03  FILLER REDEFINES WS-CURRENT-DATE.                                
012200*-----   INKLUSIVE SEKEL                                                  
012300         05  WS-DAGENS-DATUM     PIC 9(8).                                
012400         05  WS-DAGENS-TID.                                               
012500             07 WS-DAGENS-TIMME  PIC 9(2).                                
012600             07 WS-DAGENS-MINUT  PIC 9(2).                                
012700             07 WS-DAGENS-SEKUND PIC 9(2).                                
012800                                                                          
012900     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
013000     03  FILLER REDEFINES WS-TIAAVV.                                      
013100         05 WS-TIAA              PIC 9(2).                                
013200         05 WS-TIVV              PIC 9(2).                                
013300     03  WS-DADATTID             PIC 9(14)   VALUE ZERO.                  
013400                                                                          
013500     03  SEP-TPO-SDC-NDC         PIC  X(2)   VALUE '21'.                  
013600                                                                          
013700                                                                          
013800*      --- VALID IDDC CODES                                               
013900*                                                                         
014000*01    -COPY WWDCKONS                                                     
014100                                                                          
014200     EJECT                                                                
014300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014400 01  FILLER REDEFINES DAGENS-DATUM.                                       
014500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014800     EJECT                                                                
014900 01  DYNAMISKA-SUBPROGRAM.                                                
015000*                                                                         
015100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015700     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015800     03  W271SEAS                PIC X(8)    VALUE 'W271SEAS'.            
015900     03  W22222                  PIC X(8)    VALUE 'W22222'.              
016000     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
016100     SKIP2                                                                
016200     EJECT                                                                
016300*    *************************************                                
016400*    **  LINK-AREA                      **                                
016500*    **  BEHOVSTABELL                   **                                
016600*    *************************************                                
016700*01  AREA  -COPY W222L222   -PRE LINK-.                                   
016800     EJECT                                                                
016900 01  UT-AREA-START              PIC X(24)   VALUE                         
017000                                 'UT-AREA-START  '.                       
017100     SKIP2                                                                
017200                                                                          
017300*01  AREA -COPY W61136     -PRE UT-                                       
017400     EJECT                                                                
017500*    --- PARAMETRAR TILL ABEND                                            
017600                                                                          
017700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017900     EJECT                                                                
018000                                                                          
018100 01  FELTEXT.                                                             
018200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL DATKORT                                          
018600*                                                                         
018700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61136'.              
018800     SKIP2                                                                
018900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
019000     SKIP2                                                                
019100*01  -COPY WDATKORT                                                       
019200     EJECT                                                                
019300*    --- PARAMETRAR TILL POSTSUM                                          
019400*                                                                         
019500*01  -COPY W0005   -PRE  POSTSUM-                                         
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL WDATKONV                                         
019800*                                                                         
019900*01  -COPY WDATAREA                                                       
020000     EJECT                                                                
020100 01  FILLER             PIC X(16) VALUE 'WORKAREA     '.                  
020200*01   -COPY WORKAREA.                                                     
020300     EJECT                                                                
020400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020500                                                                          
020600     SKIP3                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020800     SKIP3                                                                
020900 01  NYCKLAR-TILL-DLI.                                                    
021000     03  W-IDARTNR-X.                                                     
021100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021200                                                                          
021300     03  W-WDJ2ESEQ-X.                                                    
021400         05  W-KDCLAGER          PIC S9(1)   VALUE 1    COMP-3.           
021500         05  W-IDARTNR-ESEQ      PIC S9(9)   VALUE ZERO COMP-3.           
021600                                                                          
021700     03  W-IDDC-X.                                                        
021800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021900                                                                          
022000     03  W-KDSEGKEY-X.                                                    
022100         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
022200                                                                          
022300     03  W-IDSKYLT-X.                                                     
022400         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
022500                                                                          
022600     03  W-KDNOTTYP-X.                                                    
022700         05  W-KDNOTTYP          PIC  S9(01)  COMP-3 VALUE ZERO.          
022800                                                                          
022900     03  W-ADTRDEST-X.                                                    
023000         05  W-ADTRDEST          PIC X(3)    VALUE SPACE.                 
023100                                                                          
023200     03  W-IDTRPTNR-X.                                                    
023300         05  W-IDTRPTNR          PIC S9(5)   VALUE ZERO COMP-3.           
023400                                                                          
023500     03  W-WDD901KY-X.                                                    
023600         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
023700         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
023800                                                                          
023900     03  W-KDAVROP-D9-X.                                                  
024000         05  W-KDAVROP-D9        PIC S9(1)   VALUE ZERO COMP-3.           
024100                                                                          
024200*                                                                         
024300     SKIP2                                                                
024400*    --- STATUS-KOD FRÅN IMS                                              
024500 01  STATUS-WS                   PIC XX.                                  
024600     88  SEGMENT-FINNS                       VALUE '  '.                  
024700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024800     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
024900                                                   'GB'.                  
025000     88  IMS-EJ-OK                           VALUE 'XD'.                  
025100     SKIP2                                                                
025200 01  GODK-STATUSKODER.                                                    
025300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025400     SKIP3                                                                
025500 01  SSA1                        PIC X(128).                              
025600 01  SSA2                        PIC X(128).                              
025700 01  SSA3                        PIC X(128).                              
025800     EJECT                                                                
025900*    --- IMS FUNKTIONSKODER                                               
026000*01  -COPY W0003                                                          
026100     EJECT                                                                
026200*    ---  DLI INPUT-OUTPUT AREA                                           
026300 01  FILLER                    PIC X(16)  VALUE 'DLI-WDK601'.             
026400     SKIP3                                                                
026500 01  DLI-IO-AREA-K6.                                                      
026600     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
026700     SKIP3                                                                
026800     03  WLARTS01 REDEFINES IO-AREA-K6.                                   
026900*        05  -COPY WDK601                                                 
027000     SKIP3                                                                
027100     03  WLARTS11 REDEFINES IO-AREA-K6.                                   
027200*        05  -COPY WDK611                                                 
027300     EJECT                                                                
027400                                                                          
027500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK901'.                      
027600 01  DLI-IO-WDK901.                                                       
027700*    03  -COPY WDK901                                                     
027800     EJECT                                                                
027900                                                                          
028000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ211'.                      
028100 01  DLI-IO-WDJ211.                                                       
028200*    03  -COPY WDJ211                                                     
028300     EJECT                                                                
028400                                                                          
028500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM501'.                      
028600 01  DLI-IO-WDM501.                                                       
028700*    03  -COPY WDM501                                                     
028800     EJECT                                                                
028900                                                                          
029000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM511'.                      
029100 01  DLI-IO-WDM511.                                                       
029200*    03  -COPY WDM511                                                     
029300     EJECT                                                                
029400                                                                          
029500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM521'.                      
029600 01  DLI-IO-WDM521.                                                       
029700*    03  -COPY WDM521                                                     
029800     EJECT                                                                
029900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL201'.                      
030000 01  DLI-IO-WDL201.                                                       
030100*    03  -COPY WDL201                                                     
030200     EJECT                                                                
030300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL221'.                      
030400 01  DLI-IO-WDL221.                                                       
030500*    03  -COPY WDL221                                                     
030600     EJECT                                                                
030700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD905'.                      
030800 01  DLI-IO-WDD905.                                                       
030900*    03  -COPY WDD905                                                     
031000     EJECT                                                                
031100 LINKAGE SECTION.                                                         
031200                                                                          
031300*01  -COPY W0009   -PRE MSG-                                              
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE WDK6-                                              
031600     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
031700     EJECT                                                                
031800*01  -COPY W0008  -PRE WDJ2-                                              
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008  -PRE WDM5-                                              
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008  -PRE WDL2-                                              
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700 01  W222-WDK6-PCB               PIC X.                                   
032800 01  W222-WDK7-PCB               PIC X.                                   
032900     EJECT                                                                
033000*01  -COPY W0008  -PRE ARTM-                                              
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300 01  W222-2501-PCB               PIC X.                                   
033400 01  W222-WDB6R-PCB              PIC X.                                   
033500 01  W222-WDK7R-PCB              PIC X.                                   
033600 01  W222-WDB6-PCB               PIC X.                                   
033700 01  W222-WDD7-PCB               PIC X.                                   
033800 01  W222-WDK7E-PCB              PIC X.                                   
033900 01  W222-UTIL-WDK6-PCB          PIC X.                                   
034000 01  W222-UTIL-WDK7-PCB          PIC X.                                   
034100 01  W222-UTIL-WDB6-PCB          PIC X.                                   
034200 01  W222-UTUP-WDK7-PCB          PIC X.                                   
034300 01  W222-UTUP-WDB6-PCB          PIC X.                                   
034400 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
034500 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
034600 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008      -PRE WDD9-                                          
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100 PROCEDURE DIVISION  USING WDK6-PCB  WDJ2-PCB  WDM5-PCB  WDL2-PCB         
035200                           W222-WDK6-PCB  W222-WDK7-PCB                   
035300                           ARTM-PCB  W222-2501-PCB                        
035400                           W222-WDB6R-PCB W222-WDK7R-PCB                  
035500                           W222-WDB6-PCB  W222-WDD7-PCB                   
035600                           W222-WDK7E-PCB                                 
035700                           W222-UTIL-WDK6-PCB                             
035800                           W222-UTIL-WDK7-PCB                             
035900                           W222-UTIL-WDB6-PCB                             
036000                           W222-UTUP-WDK7-PCB                             
036100                           W222-UTUP-WDB6-PCB                             
036200                           W222-UTUP-UTIL-WDK6-PCB                        
036300                           W222-UTUP-UTIL-WDK7-PCB                        
036400                           W222-UTUP-UTIL-WDB6-PCB                        
036500                           WDD9-PCB.                                      
036600     ENTRY 'DLITCBL' USING WDK6-PCB  WDJ2-PCB  WDM5-PCB  WDL2-PCB         
036700                           W222-WDK6-PCB  W222-WDK7-PCB                   
036800                           ARTM-PCB  W222-2501-PCB                        
036900                           W222-WDB6R-PCB W222-WDK7R-PCB                  
037000                           W222-WDB6-PCB  W222-WDD7-PCB                   
037100                           W222-WDK7E-PCB                                 
037200                           W222-UTIL-WDK6-PCB                             
037300                           W222-UTIL-WDK7-PCB                             
037400                           W222-UTIL-WDB6-PCB                             
037500                           W222-UTUP-WDK7-PCB                             
037600                           W222-UTUP-WDB6-PCB                             
037700                           W222-UTUP-UTIL-WDK6-PCB                        
037800                           W222-UTUP-UTIL-WDK7-PCB                        
037900                           W222-UTUP-UTIL-WDB6-PCB                        
038000                           WDD9-PCB.                                      
038100                                                                          
038200     PERFORM A-INIT                                                       
038300     PERFORM IMS-GN-WDK6                                                  
038400     PERFORM UNTIL SEGMENT-SAKNAS                                         
038500       EVALUATE WDK6-SEG-NAME-FB                                          
038600         WHEN 'WDK601  '                                                  
038700             ADD 1           TO WS-ANTAL-WDK601                           
038800             MOVE WDK6-KEY-FB-AREA-IDARTNR                                
038900                             TO WS-IDARTNR                                
039000                                W-IDARTNR                                 
039100                                LINK-IDARTNR                              
039200                                W-IDARTNR-ESEQ                            
039300*                                                                         
039400         WHEN 'WDK611  '                                                  
039500            ADD 1            TO WS-ANTAL-WDK611                           
039600            IF  CLAG-ADPLATS-SVS > ZERO                                   
039700            AND CLAG-KVLS-SVS > ZERO                                      
039800            AND CLAG-BEFT NOT = 95                                        
039900            AND CLAG-BEFT NOT = 94                                        
040000            AND CLAG-BEFT NOT = 98                                        
040100            AND CLAG-BEFT NOT = 93                                        
040200                                                                          
040300                PERFORM C-BEHANDLA                                        
040400                                                                          
040500            END-IF                                                        
040600       END-EVALUATE                                                       
040700       PERFORM IMS-GN-WDK6                                                
040800     END-PERFORM                                                          
040900                                                                          
041000     PERFORM Z-FINIT                                                      
041100                                                                          
041200     MOVE ZERO TO RETURN-CODE                                             
041300     GOBACK                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 A-INIT SECTION.                                                          
041700                                                                          
041800     OPEN OUTPUT W611UT                                                   
041900                                                                          
042000     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
042100*                                                                         
042200     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
042300     CALL WDATKONV USING DAT-KDDATFORM                                    
042400                         DAT-I-TIDATUM                                    
042500                         DAT-O-TIDATUM                                    
042600                         DAT-KDSVAR                                       
042700                                                                          
042800     IF DAT-KDSVAR-OK                                                     
042900        MOVE 1                   TO WS-ANTAL-VECKOR                       
043000                                                                          
043100        MOVE DAT-TIAAVV-GRP      TO WS-TIAAVV-NUM                         
043200        MOVE WS-TIAAVV-NUM       TO LINK-TIAAVV-AKTUELL                   
043300        DISPLAY 'LINK-TIAAVV-AKTUELL : '                                  
043400                                    LINK-TIAAVV-AKTUELL                   
043500        CALL W009VADD USING    LINK-TIAAVV-AKTUELL WS-ANTAL-VECKOR        
043600        MOVE LINK-TIAAVV-AKTUELL TO LINK-TIBEHOV-START                    
043700        MOVE DAT-TID             TO LINK-TID-AKTUELL                      
043800                                    WS-DAG-I-VECKA                        
043900        MOVE SPACE               TO LINK-IDDC                             
044000        DISPLAY 'WS-DAG-I-VECKA      : ' WS-DAG-I-VECKA                   
044100     ELSE                                                                 
044200        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W61136' TO          
044300                                    FELTEXT-STR                           
044400        DISPLAY FELTEXT                                                   
044500        PERFORM S99-ABEND                                                 
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900 C-BEHANDLA SECTION.                                                      
045000                                                                          
045100                                                                          
045200     MOVE ZERO               TO WS-SATSBEHOV                              
045300     PERFORM IMS-GET-WDJ211-ESEQ-FIRST                                    
045400     PERFORM UNTIL SEGMENT-SAKNAS                                         
045500       ADD SRAD-KVSATRES                                                  
045600                             TO WS-SATSBEHOV                              
045700       PERFORM IMS-GET-WDJ211-ESEQ-NEXT                                   
045800     END-PERFORM                                                          
045900                                                                          
046000     COMPUTE WS-KVRESS = CLAG-KVRESS                                      
046100                       - WS-SATSBEHOV                                     
046200                                                                          
046300     COMPUTE WS-SALDO = CLAG-KVLS                                         
046400                      + CLAG-KVAKS-CDC                                    
046500                      - CLAG-KVLS-SVS                                     
046600                      - CLAG-KVSPARR-KVAL                                 
046700                      - CLAG-KVUTRS                                       
046800                      - WS-KVRESS                                         
046900                                                                          
047000     PERFORM IMS-GU-K901                                                  
047100     IF SEGMENT-FINNS                                                     
047200       COMPUTE WS-SALDO = WS-SALDO                                        
047300                        - ART-KVOKS-BULK                                  
047400                        - ART-KVOKS-DAG                                   
047500                        - ART-KVOKS-VOR                                   
047600     END-IF                                                               
047700                                                                          
047800     MOVE ZERO               TO WS-KVANTAL                                
047900     MOVE 'CDC'              TO W-ADTRDEST                                
048000     PERFORM IMS-GU-M501                                                  
048100     PERFORM IMS-GNP-M521                                                 
048200                                                                          
048300     PERFORM UNTIL SEGMENT-SAKNAS                                         
048400                                                                          
048500        IF W-IDARTNR = AVG-IDARTNR                                        
048600           ADD AVG-KVANTAL   TO WS-KVANTAL                                
048700        END-IF                                                            
048800                                                                          
048900        PERFORM IMS-GNP-M521                                              
049000                                                                          
049100     END-PERFORM                                                          
049200                                                                          
049300     COMPUTE WS-SALDO = WS-SALDO                                          
049400                      + WS-KVANTAL                                        
049500                                                                          
049600     MOVE ZERO               TO WS-KVRETUR                                
049700     PERFORM IMS-GU-L201                                                  
049800     IF SEGMENT-FINNS                                                     
049900        PERFORM IMS-GNP-L221                                              
050000        PERFORM UNTIL SEGMENT-SAKNAS                                      
050100           IF MOT-KDRT = 7                                                
050200              COMPUTE WS-KVRETUR = WS-KVRETUR +                           
050300                      MOT-KVAVIS - MOT-KVANTMOT                           
050400           END-IF                                                         
050500           PERFORM IMS-GNP-L221                                           
050600        END-PERFORM                                                       
050700     END-IF                                                               
050800                                                                          
050900     COMPUTE WS-SALDO = WS-SALDO                                          
051000                      - WS-KVRETUR                                        
051100                                                                          
051200     COMPUTE WS-VECKO-SEP-BEHOV ROUNDED =                                 
051300                             CLAG-KVPB-SEP / 4.33                         
051400     MOVE +1                 TO WS-FAKTOR                                 
051500     SUBTRACT CLAG-REDIRLEV                                               
051600                             FROM WS-FAKTOR                               
051700                                                                          
051800     MOVE W-IDARTNR          TO W-IDARTNR-D9                              
051900     MOVE WC-CDC-SE          TO W-IDDC-D9                                 
052000                                                                          
052100     MOVE 2                  TO W-KDAVROP-D9                              
052200     PERFORM IMS-GU-WDD905                                                
052300     IF SEGMENT-FINNS                                                     
052400        MOVE 001            TO WORK-KDCALL                                
052500        MOVE WC-CDC-SE      TO WORK-IDDC                                  
052600        MOVE DAGENS-DATUM   TO WORK-TIAAMMDD-FOM                          
052700        MOVE TIAVRDAT-DISP  TO WORK-TIAAMMDD-TOM                          
052800        CALL WORKDAY USING WORK-KDCALL,                                   
052900                              WORK-DATE-AREA,                             
053000                              WORK-KDSVAR                                 
053100        IF WORK-KDSVAR-OK AND                                             
053200           WORK-KVWORKD < 5                                               
053300                                                                          
053400           MOVE 1            TO LINK-KVVECKOR-BEHOV                       
053500        ELSE                                                              
053600           MOVE 2            TO LINK-KVVECKOR-BEHOV                       
053700        END-IF                                                            
053800     ELSE                                                                 
053900        MOVE 2               TO LINK-KVVECKOR-BEHOV                       
054000     END-IF                                                               
054100                                                                          
054200     MOVE SEP-TPO-SDC-NDC    TO LINK-KDBEHOV                              
054300     MOVE NEJ                TO LINK-FLINKLDIRLEV                         
054400     CALL W22222 USING LINK-AREA W222-WDK6-PCB                            
054500                                 W222-WDK7-PCB                            
054600                                 ARTM-PCB                                 
054700                                 W222-2501-PCB                            
054800                                 W222-WDB6R-PCB                           
054900                                 W222-WDK7R-PCB                           
055000                                 W222-WDB6-PCB                            
055100                                 W222-WDD7-PCB                            
055200                                 W222-WDK7E-PCB                           
055300                                 W222-UTIL-WDK6-PCB                       
055400                                 W222-UTIL-WDK7-PCB                       
055500                                 W222-UTIL-WDB6-PCB                       
055600                                 W222-UTUP-WDK7-PCB                       
055700                                 W222-UTUP-WDB6-PCB                       
055800                                 W222-UTUP-UTIL-WDK6-PCB                  
055900                                 W222-UTUP-UTIL-WDK7-PCB                  
056000                                 W222-UTUP-UTIL-WDB6-PCB                  
056100                                                                          
056200     IF WS-SALDO < LINK-KVBEHOV-SUMMA                                     
056300        ADD 1                TO WS-ANTAL-TRAEFF                           
056400        MOVE WS-IDARTNR      TO UT-IDARTNR                                
056500        COMPUTE UT-KVANTAL   = LINK-KVBEHOV-SUMMA - WS-SALDO              
056600        COMPUTE WS-KVLS-SVS  = CLAG-KVLS-SVS                              
056700                             - WS-KVANTAL                                 
056800        IF UT-KVANTAL > WS-KVLS-SVS                                       
056900          MOVE WS-KVLS-SVS   TO UT-KVANTAL                                
057000        END-IF                                                            
057100                                                                          
057200        IF UT-KVANTAL > ZERO                                              
057300          PERFORM S01-SKRIV-W611UT                                        
057400        END-IF                                                            
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 Z-FINIT SECTION.                                                         
058000                                                                          
058100     CLOSE W611UT                                                         
058200     .                                                                    
058300     EJECT                                                                
058400 S01-SKRIV-W611UT SECTION.                                                
058500                                                                          
058600     WRITE UT-POST FROM UT-AREA                                           
058700     .                                                                    
058800     EJECT                                                                
058900                                                                          
059000 S99-ABEND SECTION.                                                       
059100                                                                          
059200     SKIP2                                                                
059300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700* --- IMS SEKTIONER ---                                                   
059800     SKIP3                                                                
059900 IMS-GN-WDK6 SECTION.                                                     
060000                                                                          
060100     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA-K6                        
060200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
060300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     EJECT                                                                
060700 IMS-GU-K901           SECTION.                                           
060800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
060900            DELIMITED BY SIZE INTO SSA1                                   
061000     MOVE '  GE' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-WDK901 SSA1                    
061200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     EJECT                                                                
061600*                                                                         
061700*IMS-GET-WDJ211-ESEQ-FIRST SECTION.                                       
061800*    STRING 'WDJ211  *F(WDJ2ESEQ =' W-WDJ2ESEQ-X ')'                      
061900*         DELIMITED BY SIZE INTO SSA1                                     
062000*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
062100*    CALL CBLTDLI USING GN WDJ2-PCB DLI-IO-WDJ211 SSA1                    
062200*    MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
062300*    PERFORM IMS-STATUSKONTROLL                                           
062400*    .                                                                    
062500*    SKIP3                                                                
062600*                                                                         
062700*IMS-GET-WDJ211-ESEQ-FIRST SECTION.                                       
062800*    STRING 'WDJ211  (WDJ2E1KY>=' W-WDJ2ESEQ-MIN-X                        
062900*                   '&WDJ2E1KY<=' W-WDJ2ESEQ-MAX-X ')'                    
063000*         DELIMITED BY SIZE INTO SSA1                                     
063100*    MOVE '  GE' TO GODK-STATUSKODER                                      
063200*    CALL CBLTDLI USING GU WDJ2-PCB DLI-IO-WDJ211 SSA1                    
063300*    MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
063400*    PERFORM IMS-STATUSKONTROLL                                           
063500*    .                                                                    
063600*    EJECT                                                                
063700 IMS-GET-WDJ211-ESEQ-FIRST SECTION.                                       
063800     STRING 'WDJ211  (WDJ2ESEQ =' W-WDJ2ESEQ-X ')'                        
063900          DELIMITED BY SIZE INTO SSA1                                     
064000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
064100     CALL CBLTDLI USING GU WDJ2-PCB DLI-IO-WDJ211 SSA1                    
064200     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500     EJECT                                                                
064600 IMS-GET-WDJ211-ESEQ-NEXT SECTION.                                        
064700     STRING 'WDJ211  (WDJ2ESEQ =' W-WDJ2ESEQ-X ')'                        
064800          DELIMITED BY SIZE INTO SSA1                                     
064900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
065000     CALL CBLTDLI USING GN WDJ2-PCB DLI-IO-WDJ211 SSA1                    
065100     MOVE WDJ2-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400     EJECT                                                                
065500 IMS-GU-M501   SECTION.                                                   
065600                                                                          
065700     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
065800          DELIMITED BY SIZE INTO SSA1                                     
065900     MOVE SPACE  TO GODK-STATUSKODER                                      
066000     CALL CBLTDLI USING GU  WDM5-PCB DLI-IO-WDM501 SSA1                   
066100     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
066200     PERFORM IMS-STATUSKONTROLL                                           
066300     .                                                                    
066400     EJECT                                                                
066500 IMS-GNP-M521 SECTION.                                                    
066600                                                                          
066700     STRING 'WDM511    '                                                  
066800          DELIMITED BY SIZE INTO SSA1                                     
066900     STRING 'WDM521    '                                                  
067000          DELIMITED BY SIZE INTO SSA2                                     
067100     MOVE '  GE' TO GODK-STATUSKODER                                      
067200     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2              
067300     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
067400     PERFORM IMS-STATUSKONTROLL                                           
067500     .                                                                    
067600     EJECT                                                                
067700 IMS-GU-L201 SECTION.                                                     
067800                                                                          
067900     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
068000          DELIMITED BY SIZE INTO SSA1                                     
068100     MOVE '  GE' TO GODK-STATUSKODER                                      
068200     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
068300     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     EJECT                                                                
068700 IMS-GNP-L221 SECTION.                                                    
068800                                                                          
068900     STRING 'WDL211     '                                                 
069000          DELIMITED BY SIZE INTO SSA1                                     
069100     STRING 'WDL221     '                                                 
069200          DELIMITED BY SIZE INTO SSA2                                     
069300     MOVE '  GE' TO GODK-STATUSKODER                                      
069400     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
069500     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
069600     PERFORM IMS-STATUSKONTROLL                                           
069700     .                                                                    
069800     EJECT                                                                
069900 IMS-GU-WDD905 SECTION.                                                   
070000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
070100          DELIMITED BY SIZE INTO SSA1                                     
070200     MOVE   'WDD902 '         TO SSA2                                     
070300     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-D9-X ')'                      
070400          DELIMITED BY SIZE INTO SSA3                                     
070500     MOVE '  GE' TO GODK-STATUSKODER                                      
070600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
070700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
070800     PERFORM IMS-STATUSKONTROLL                                           
070900     .                                                                    
071000     EJECT                                                                
071100 IMS-STATUSKONTROLL SECTION.                                              
071200                                                                          
071300     SET STATUS-IX TO 1                                                   
071400     SEARCH GODK-STATUS                                                   
071500       AT END                                                             
071600         MOVE 'OTILLÅTEN RETURKOD FRÅN IMS: '                             
071700                             TO FELTEXT-STR                               
071800         DISPLAY FELTEXT                                                  
071900         CALL FELLOG                                                      
072000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
072100         CONTINUE                                                         
072200     END-SEARCH                                                           
072300     .                                                                    
