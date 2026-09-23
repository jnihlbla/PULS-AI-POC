000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4405D00.                                                
000300 AUTHOR.         BO SVENSSON.                                             
000400 DATE-WRITTEN.   03/03/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        H S S R                                                          
000900*        LÄSER VORKÖ NY (WDA6)                                            
001000*        SKAPAR FIL TILL VOR-UPPF 24TIM    , W4405D                       
001100*        SKAPAR FIL TILL VOR-UPPF DAGAR    , W4405E                       
001200*        SKAPAR FIL TILL OLÖSTA NU         , W4405F                       
001210*        SKAPAR FIL TILL 24TIM/DAG OBKR > 0, W4405V                       
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FIL TILL VOR-UPPF 24 TIM                                   
002800     SELECT W4405D                     ASSIGN TO W4405DD1.                
002900     SKIP2                                                                
003000*          --- FIL TILL VOR-UPPF DAGAR                                    
003100     SELECT W4405E                     ASSIGN TO W4405DD2.                
003200     SKIP2                                                                
003300*          --- FIL TILL UPPF OLÖSTA NU                                    
003400     SELECT W4405F                     ASSIGN TO W4405DD3.                
003410     SKIP2                                                                
003420*          --- FIL TILL UPPF 24 TIM OCH DAGAR OBKR > 0                    
003430     SELECT W4405V                     ASSIGN TO W4405DD4.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W4405D                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W4405D  -PRE  VOR24- -L.                                  
004500     SKIP3                                                                
004600 FD  W4405E                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W4405D -PRE  VORDAG- -L.                                  
005100     SKIP3                                                                
005200 FD  W4405F                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY W4405E -PRE  VORNU-  -L.                                  
005700 01  VORNU-HEAD-POST PIC X(100).                                          
005710     SKIP3                                                                
005720 FD  W4405V                                                               
005730     RECORDING       F                                                    
005740     BLOCK CONTAINS  0.                                                   
005750                                                                          
005760*01  POST -COPY W4405V  -PRE  VOR24DAG- -L.                               
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100 77  IDPGM                       PIC X(8)    VALUE 'W4405D00'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  FEL                         PIC X       VALUE 'F'.                   
006500 77  HORIZTAB                    PIC X       VALUE X'05'.                 
006600 77  ABENDTEXT                   PIC X(30)   VALUE ' '.                   
006700     EJECT                                                                
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007400 01  WS-VOR-DATUM.                                                        
007500     03  WS-VOR24-FOM            PIC S9(7)  COMP-3 VALUE 0.               
007600     03  WS-VOR24-TOM            PIC S9(7)  COMP-3 VALUE 0.               
007700     03  WS-VORDAG-FOM           PIC S9(7)  COMP-3 VALUE 0.               
007800     03  WS-VORDAG-TOM           PIC S9(7)  COMP-3 VALUE 0.               
007900     EJECT                                                                
008000 01  WS-TIKLATID                 PIC S9(9)  COMP-3 VALUE 0.               
008100 01  WS-TIREGTID                 PIC S9(9)  COMP-3 VALUE 0.               
008200 01  WS-KVWORKD                  PIC S9(3)  COMP-3 VALUE 0.               
008300                                                                          
008400 01  WS-TIAAVVD-X.                                                        
008500     03  WS-TIAAVVD              PIC 9(5).                                
008600 01  WS-AAVVD REDEFINES WS-TIAAVVD-X.                                     
008700     03  FILLER                  PIC X(4).                                
008800     03  WS-DAG                  PIC 9.                                   
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
009700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009800     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
009900     SKIP2                                                                
010000*    --- PARAMETRAR TILL ABEND                                            
010100                                                                          
010200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010500     SKIP2                                                                
010600 01  FELTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL POSTSUM                                          
011100*                                                                         
011200*01  -COPY W0005   -PRE  POSTSUM-                                         
011300     EJECT                                                                
011400*01  -COPY WORKAREA                                                       
011500     EJECT                                                                
011600*01  -COPY WDAGAREA                                                       
011700     EJECT                                                                
011800*01  -COPY WDATAREA                                                       
011900     EJECT                                                                
012000 01  VOR24-AREA-START             PIC X(24)   VALUE                       
012100                                 'VOR24-AREA-START '.                     
012200     SKIP2                                                                
012300*01  AREA -COPY W4405D     -PRE VOR24-                                    
012400     EJECT                                                                
012500 01  VORDAG-AREA-START            PIC X(24)   VALUE                       
012600                                 'VORDAG-AREA-START'.                     
012700     SKIP2                                                                
012800*01  AREA -COPY W4405D     -PRE VORDAG-                                   
012900     EJECT                                                                
013000 01  VORNU-AREA-START             PIC X(24)   VALUE                       
013100                                 'VORNU-AREA-START '.                     
013200     SKIP2                                                                
013300*01  AREA -COPY W4405E     -PRE VORNU-                                    
013400     EJECT                                                                
013410 01  VOR24DAG-AREA-START          PIC X(24)   VALUE                       
013420                                 'VOR24DAG-AREA-START '.                  
013430     SKIP2                                                                
013440*01  AREA -COPY W4405V     -PRE VOR24DAG-                                 
013450     EJECT                                                                
013500*                                                                         
013600 01  VORNU-HEAD-AREA                   PIC X(100).                        
013700     EJECT                                                                
013800                                                                          
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     SKIP2                                                                
014400*    --- STATUS-KOD FRÅN IMS                                              
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FINNS                       VALUE '  '.                  
014700     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA6'.                        
016000 01  DLI-IO-WDA6.                                                         
016100*    03  -COPY WDA601                                                     
016200     EJECT                                                                
016300 LINKAGE SECTION.                                                         
016400                                                                          
016500                                                                          
016600*01  -COPY W0008  -PRE WDA6-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900 PROCEDURE DIVISION  USING WDA6-PCB.                                      
017000 MAIN SECTION.                                                            
017100     ENTRY 'DLITCBL' USING WDA6-PCB.                                      
017200                                                                          
017300     PERFORM A-INIT                                                       
017400                                                                          
017500     PERFORM E-BERAKNA-DATUM                                              
017600                                                                          
017700     PERFORM IMS-GET-WDA6                                                 
017800     PERFORM UNTIL SEGMENT-SAKNAS                                         
018100                                                                          
018800        PERFORM B-BERAKNA-TID                                             
018900                                                                          
019000        PERFORM C-TILL-VOR24                                              
019100                                                                          
019200        PERFORM D-TILL-VORDAG                                             
019300                                                                          
019400        PERFORM E-TILL-VORNU                                              
019500                                                                          
019700       PERFORM IMS-GET-WDA6                                               
019800     END-PERFORM                                                          
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600                                                                          
020700     OPEN OUTPUT W4405D                                                   
020800                 W4405E                                                   
020900                 W4405F                                                   
020910                 W4405V                                                   
021000                                                                          
021100     ACCEPT DAGENS-DATUM  FROM DATE                                       
021200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021300     PERFORM S01-TILL-VORNU-HEAD                                          
021400                                                                          
021500     .                                                                    
021600     EJECT                                                                
021700 E-BERAKNA-DATUM SECTION.                                                 
021800                                                                          
021900     MOVE 20 TO DAT-TISEKEL                                               
022000                                                                          
022100     MOVE DAGENS-DATUM    TO DAT-I-TIDATUM                                
022200     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
022300                                                                          
022400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
022500                         DAT-O-TIDATUM DAT-KDSVAR                         
022600     IF DAT-KDSVAR-FEL                                                    
022700       MOVE 'FELSVAR FRÅN WDATKONV 1' TO ABENDTEXT                        
022800       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
022900       PERFORM S99-ABEND                                                  
023000     ELSE                                                                 
023100       MOVE DAT-TIAAVVD     TO WS-TIAAVVD                                 
023200       MOVE 1               TO WS-DAG                                     
023300       MOVE WS-TIAAVVD      TO DAT-I-TIDATUM                              
023400       MOVE 'AAVVD'         TO DAT-KDDATFORM                              
023500                                                                          
023600       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
023700                           DAT-O-TIDATUM DAT-KDSVAR                       
023800       IF DAT-KDSVAR-FEL                                                  
023900         MOVE 'FELSVAR FRÅN WDATKONV 2' TO ABENDTEXT                      
024000         MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                           
024100         PERFORM S99-ABEND                                                
024200       END-IF                                                             
024300     END-IF                                                               
024400                                                                          
024500     MOVE DAT-TIAAMMDD    TO DAG-TIAAMMDD-TOM                             
024600     MOVE 15              TO DAG-KVKALDAG                                 
024700*    ---    2 VECKOR BAKÅT - COMPUTE FOM = TOM - DAGAR                    
024800     MOVE 003 TO DAG-KDCALL                                               
024900                                                                          
025000     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
025100     IF DAG-KDSVAR = FEL                                                  
025200       MOVE 'FELSVAR FRÅN WDAGKONV 3' TO ABENDTEXT                        
025300       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
025400       PERFORM S99-ABEND                                                  
025500     ELSE                                                                 
025600       MOVE DAG-TIAAMMDD-FOM TO WS-VORDAG-FOM                             
025700     END-IF                                                               
025800                                                                          
025900     MOVE DAT-TIAAMMDD    TO DAG-TIAAMMDD-TOM                             
026000     MOVE 9               TO DAG-KVKALDAG                                 
026100*    ---    8 DAGAR  BAKÅT - COMPUTE FOM = TOM - DAGAR                    
026200     MOVE 003 TO DAG-KDCALL                                               
026300                                                                          
026400     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
026500     IF DAG-KDSVAR = FEL                                                  
026600       MOVE 'FELSVAR FRÅN WDAGKONV 4' TO ABENDTEXT                        
026700       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
026800       PERFORM S99-ABEND                                                  
026900     ELSE                                                                 
027000       MOVE DAG-TIAAMMDD-FOM TO WS-VORDAG-TOM                             
027100     END-IF                                                               
027200                                                                          
027300     MOVE DAT-TIAAMMDD    TO DAG-TIAAMMDD-TOM                             
027400     MOVE 8               TO DAG-KVKALDAG                                 
027500*    ---    1 VECKA  BAKÅT - COMPUTE FOM = TOM - DAGAR                    
027600     MOVE 003 TO DAG-KDCALL                                               
027700                                                                          
027800     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
027900     IF DAG-KDSVAR = FEL                                                  
028000       MOVE 'FELSVAR FRÅN WDAGKONV 5' TO ABENDTEXT                        
028100       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
028200       PERFORM S99-ABEND                                                  
028300     ELSE                                                                 
028400       MOVE DAG-TIAAMMDD-FOM TO WS-VOR24-FOM                              
028500     END-IF                                                               
028600                                                                          
028700     MOVE DAT-TIAAMMDD    TO DAG-TIAAMMDD-TOM                             
028800     MOVE 2               TO DAG-KVKALDAG                                 
028900*    ---    1 DAG    BAKÅT - COMPUTE FOM = TOM - DAGAR                    
029000     MOVE 003 TO DAG-KDCALL                                               
029100                                                                          
029200     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
029300     IF DAG-KDSVAR = FEL                                                  
029400       MOVE 'FELSVAR FRÅN WDAGKONV 6' TO ABENDTEXT                        
029500       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
029600       PERFORM S99-ABEND                                                  
029700     ELSE                                                                 
029800       MOVE DAG-TIAAMMDD-FOM TO WS-VOR24-TOM                              
029900     END-IF                                                               
030000                                                                          
030100     .                                                                    
030200                                                                          
030300     EJECT                                                                
030400 B-BERAKNA-TID SECTION.                                                   
030500                                                                          
030600     IF    VOR-TIKLAR > 0 AND VOR-TIKLAR > 100101                         
030601                          AND VOR-TIREGDAT-URSP > 100101                  
030610*     IF  (VOR-TIKLAR - VOR-TIREGDAT-URSP) < 200000                       
030620*        MOVE 998 TO WS-KVWORKD                                           
030630*     ELSE                                                                
030700                                                                          
030800       MOVE 001               TO WORK-KDCALL                              
030900       MOVE '11'              TO WORK-IDDC                                
031000       MOVE VOR-TIREGDAT-URSP TO WORK-TIAAMMDD-FOM                        
031100       MOVE VOR-TIKLAR        TO WORK-TIAAMMDD-TOM                        
031200                                                                          
031300       CALL WORKDAY USING                                                 
031400            WORK-KDCALL                                                   
031500            WORK-DATE-AREA                                                
031600            WORK-KDSVAR                                                   
031700                                                                          
031800       IF    WORK-KDSVAR = SPACE                                          
031900         MOVE WORK-KVWORKD    TO WS-KVWORKD                               
032000         IF  WS-KVWORKD = 0                                               
032100           MOVE 1             TO WS-KVWORKD                               
032200         END-IF                                                           
032300       ELSE                                                               
032400         MOVE 'FELSVAR FRÅN WORKDAY 7' TO ABENDTEXT                       
032500         MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                           
032600         PERFORM S99-ABEND                                                
032700       END-IF                                                             
032800                                                                          
032900       MOVE VOR-TIREGDAT-URSP TO WORK-TIAAMMDD-FOM                        
033000       MOVE VOR-TIREGDAT-URSP TO WORK-TIAAMMDD-TOM                        
033100                                                                          
033200       CALL WORKDAY USING                                                 
033300            WORK-KDCALL                                                   
033400            WORK-DATE-AREA                                                
033500            WORK-KDSVAR                                                   
033600                                                                          
033700       IF    WORK-KDSVAR = SPACE                                          
033800         IF  WORK-KVWORKD = 0                                             
033900             MOVE 0                 TO WS-TIREGTID                        
034000         ELSE                                                             
034100             MOVE VOR-TIREGTID-URSP TO WS-TIREGTID                        
034200         END-IF                                                           
034300       ELSE                                                               
034400         MOVE 'FELSVAR FRÅN WORKDAY 8' TO ABENDTEXT                       
034500         MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                           
034600         PERFORM S99-ABEND                                                
034700       END-IF                                                             
034800                                                                          
034900       MOVE VOR-TIKLAR        TO WORK-TIAAMMDD-FOM                        
035000       MOVE VOR-TIKLAR        TO WORK-TIAAMMDD-TOM                        
035100                                                                          
035200       CALL WORKDAY USING                                                 
035300            WORK-KDCALL                                                   
035400            WORK-DATE-AREA                                                
035500            WORK-KDSVAR                                                   
035600                                                                          
035700       IF    WORK-KDSVAR = SPACE                                          
035800         IF  WORK-KVWORKD = 0                                             
035900             MOVE 24000000          TO WS-TIKLATID                        
036000         ELSE                                                             
036100             MOVE VOR-TIKLATID      TO WS-TIKLATID                        
036200             COMPUTE WS-TIKLATID = WS-TIKLATID                            
036300                                 * 100                                    
036400             END-COMPUTE                                                  
036500         END-IF                                                           
036600       ELSE                                                               
036700         MOVE 'FELSVAR FRÅN WORKDAY 9' TO ABENDTEXT                       
036800         MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                           
036900         PERFORM S99-ABEND                                                
037000       END-IF                                                             
037100                                                                          
037200       IF  WS-TIKLATID <= WS-TIREGTID                                     
037300           SUBTRACT 1  FROM WS-KVWORKD                                    
037400       END-IF                                                             
037410*     END-IF                                                              
037500     ELSE                                                                 
037600       MOVE 999               TO WS-KVWORKD                               
037700     END-IF                                                               
037800     .                                                                    
037900                                                                          
038000     EJECT                                                                
038100 C-TILL-VOR24 SECTION.                                                    
038200                                                                          
038300     IF  VOR-TIREGDAT-URSP >= WS-VOR24-FOM                                
038400     AND VOR-TIREGDAT-URSP <= WS-VOR24-TOM                                
038500       MOVE VOR-IDDISTR       TO VOR24-IDDISTR                            
038510                                 VOR24DAG-IDDISTR                         
038600       MOVE VOR-IDKUNDNR      TO VOR24-IDKUNDNR                           
038610                                 VOR24DAG-IDKUNDNR                        
038700       MOVE VOR-IDKUNDRF      TO VOR24-IDKUNDRF                           
038710                                 VOR24DAG-IDKUNDRF                        
038800       MOVE VOR-TIREGDAT-URSP TO VOR24-TIREGDAT-URSP                      
038810                                 VOR24DAG-TIREGDAT-URSP                   
038902       MOVE VOR-TIREGTID-AVV  TO VOR24DAG-TIREGTID-AVV                    
038903       MOVE VOR-IDARTNR       TO VOR24-IDARTNR                            
038910                                 VOR24DAG-IDARTNR                         
039000       MOVE VOR-KDVORATG      TO VOR24-KDVORATG                           
039010                                 VOR24DAG-KDVORATG                        
039100       MOVE WS-KVWORKD        TO VOR24-KVWORKD                            
039200       MOVE 1                 TO VOR24-KVRADER                            
039300                                                                          
039400       PERFORM S11-SKRIV-W4405D                                           
039410       IF VOR-KDORDBEK > ZERO                                             
039411          MOVE '24 '             TO VOR24DAG-IDPTYP                       
039412          MOVE VOR-TIREGTID-URSP TO VOR24DAG-TIREGTID-URSP                
039413          MOVE VOR-TIREGDAT-AVV  TO VOR24DAG-TIREGDAT-AVV                 
039414          MOVE VOR-TIKLAR        TO VOR24DAG-TIKLAR                       
039415          MOVE VOR-TIKLATID      TO VOR24DAG-TIKLATID                     
039416          PERFORM S14-SKRIV-W4405V                                        
039420       END-IF                                                             
039500     END-IF                                                               
039600     .                                                                    
039700                                                                          
039800 D-TILL-VORDAG SECTION.                                                   
039900                                                                          
040000     IF  VOR-TIREGDAT-URSP >= WS-VORDAG-FOM                               
040100     AND VOR-TIREGDAT-URSP <= WS-VORDAG-TOM                               
040200       MOVE VOR-IDDISTR       TO VORDAG-IDDISTR                           
040210                                 VOR24DAG-IDDISTR                         
040300       MOVE VOR-IDKUNDNR      TO VORDAG-IDKUNDNR                          
040310                                 VOR24DAG-IDKUNDNR                        
040400       MOVE VOR-IDKUNDRF      TO VORDAG-IDKUNDRF                          
040410                                 VOR24DAG-IDKUNDRF                        
040500       MOVE VOR-TIREGDAT-URSP TO VORDAG-TIREGDAT-URSP                     
040510                                 VOR24DAG-TIREGDAT-URSP                   
040520       MOVE VOR-TIREGTID-URSP TO VOR24DAG-TIREGTID-URSP                   
040530       MOVE VOR-TIREGDAT-AVV  TO VOR24DAG-TIREGDAT-AVV                    
040540       MOVE VOR-TIREGTID-AVV  TO VOR24DAG-TIREGTID-AVV                    
040600       MOVE VOR-IDARTNR       TO VORDAG-IDARTNR                           
040610                                 VOR24DAG-IDARTNR                         
040700       MOVE VOR-KDVORATG      TO VORDAG-KDVORATG                          
040710                                 VOR24DAG-KDVORATG                        
040800       MOVE WS-KVWORKD        TO VORDAG-KVWORKD                           
040900       MOVE 1                 TO VORDAG-KVRADER                           
041000                                                                          
041100       PERFORM S12-SKRIV-W4405E                                           
041110       IF VOR-KDORDBEK > ZERO                                             
041120          MOVE 'DAG'             TO VOR24DAG-IDPTYP                       
041122          MOVE VOR-TIREGTID-URSP TO VOR24DAG-TIREGTID-URSP                
041123          MOVE VOR-TIREGDAT-AVV  TO VOR24DAG-TIREGDAT-AVV                 
041124          MOVE VOR-TIKLAR        TO VOR24DAG-TIKLAR                       
041125          MOVE VOR-TIKLATID      TO VOR24DAG-TIKLATID                     
041130          PERFORM S14-SKRIV-W4405V                                        
041140       END-IF                                                             
041200     END-IF                                                               
041300     .                                                                    
041400                                                                          
041500 E-TILL-VORNU  SECTION.                                                   
041600                                                                          
041700     IF VOR-KDVORATG = '0'                                                
041800     OR VOR-KDVORATG = '1'                                                
041900*------------ OBS ' ' SKALL INNEHÅLLA TAB (HEX-05)                        
042000       MOVE ALL '	'              TO VORNU-AREA                            
042100       MOVE VOR-IDDISTR          TO VORNU-IDDISTR                         
042200       MOVE VOR-IDKUNDNR         TO VORNU-IDKUNDNR                        
042300       MOVE VOR-IDKUNDRF         TO VORNU-IDKUNDRF                        
042400       MOVE VOR-TIREGDAT-URSP    TO VORNU-TIREGDAT-URSP                   
042500       MOVE VOR-IDARTNR          TO VORNU-IDARTNR                         
042600       MOVE WS-KVWORKD           TO VORNU-KVWORKD                         
042700       MOVE 1                    TO VORNU-KVRADER                         
042800       MOVE 'P'                  TO VORNU-KDVORSTA                        
042900                                                                          
043000       PERFORM S13-SKRIV-W4405F                                           
043100     END-IF                                                               
043200     .                                                                    
043300                                                                          
043400     EJECT                                                                
043500 Z-FINIT SECTION.                                                         
043600     CLOSE W4405D                                                         
043700           W4405E                                                         
043800           W4405F                                                         
043810           W4405V                                                         
043900     SKIP2                                                                
044000     MOVE 'S' TO POSTSUM-OPKOD                                            
044100     CALL POSTSUM USING POSTSUM-PARM                                      
044200     .                                                                    
044300     EJECT                                                                
044400                                                                          
044500**** RUBRIK TILL EXCEL-FIL ****                                           
044600 S01-TILL-VORNU-HEAD  SECTION.                                            
044700                                                                          
044800     STRING 'DISTRICT', HORIZTAB,                                         
044900            'CUST NO', HORIZTAB,                                          
045000            'ORDER NO', HORIZTAB,                                         
045100            'REG DATE', HORIZTAB,                                         
045200            'PART NO', HORIZTAB,                                          
045300            'STATUS', HORIZTAB,                                           
045400            'WORKDAY', HORIZTAB,                                          
045500            'LINES'                                                       
045600     DELIMITED BY SIZE INTO VORNU-HEAD-AREA                               
045700                                                                          
045800     WRITE VORNU-HEAD-POST FROM VORNU-HEAD-AREA                           
045900*    MOVE SPACE    TO POSTSUM-TRANSTYP                                    
046000*    MOVE 'W4405F' TO POSTSUM-FDNAMN                                      
046100*    MOVE 'W4405DD3' TO POSTSUM-DDNAMN2                                   
046200*    CALL POSTSUM USING POSTSUM-PARM                                      
046300     .                                                                    
046400                                                                          
046500     EJECT                                                                
046600 S11-SKRIV-W4405D SECTION.                                                
046700                                                                          
046800     WRITE VOR24-POST FROM VOR24-AREA                                     
046900                                                                          
047000     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
047100     MOVE 'W4405D' TO POSTSUM-FDNAMN                                      
047200     MOVE 'W4405DD1' TO POSTSUM-DDNAMN2                                   
047300     CALL POSTSUM USING POSTSUM-PARM                                      
047400     .                                                                    
047500     EJECT                                                                
047600 S12-SKRIV-W4405E SECTION.                                                
047700                                                                          
047800     WRITE VORDAG-POST FROM VORDAG-AREA                                   
047900                                                                          
048000     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
048100     MOVE 'W4405E' TO POSTSUM-FDNAMN                                      
048200     MOVE 'W4405DD2' TO POSTSUM-DDNAMN2                                   
048300     CALL POSTSUM USING POSTSUM-PARM                                      
048400     .                                                                    
048500     EJECT                                                                
048600 S13-SKRIV-W4405F SECTION.                                                
048700                                                                          
048800     WRITE VORNU-POST FROM VORNU-AREA                                     
048900                                                                          
049000     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
049100     MOVE 'W4405F' TO POSTSUM-FDNAMN                                      
049200     MOVE 'W4405DD3' TO POSTSUM-DDNAMN2                                   
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400     .                                                                    
049500     EJECT                                                                
049510 S14-SKRIV-W4405V SECTION.                                                
049520                                                                          
049530     WRITE VOR24DAG-POST FROM VOR24DAG-AREA                               
049540                                                                          
049550     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
049560     MOVE 'W4405V' TO POSTSUM-FDNAMN                                      
049570     MOVE 'W4405DD4' TO POSTSUM-DDNAMN2                                   
049580     CALL POSTSUM USING POSTSUM-PARM                                      
049590     .                                                                    
049591     EJECT                                                                
049600 S99-ABEND SECTION.                                                       
049700                                                                          
049800     SKIP2                                                                
049900     MOVE 'S' TO POSTSUM-OPKOD                                            
050000     CALL POSTSUM USING POSTSUM-PARM                                      
050100     CALL ABEND USING RKOD-ABEND                                          
050200     .                                                                    
050300     EJECT                                                                
050400* --- IMS SEKTIONER ---                                                   
050500                                                                          
050600                                                                          
050700 IMS-GET-WDA6   SECTION.                                                  
050800                                                                          
050900     CALL CBLTDLI USING GN WDA6-PCB DLI-IO-WDA6                           
051000     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
051100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-STATUSKONTROLL SECTION.                                              
051600                                                                          
051700     SET STATUS-IX TO 1                                                   
051800     SEARCH GODK-STATUS                                                   
051900       AT END                                                             
052000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
052100           DELIMITED BY SIZE INTO FELTEXT                                 
052200         DISPLAY FELTEXT                                                  
052300         CALL FELLOG                                                      
052400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
052500         CONTINUE                                                         
052600     END-SEARCH                                                           
052700     .                                                                    
