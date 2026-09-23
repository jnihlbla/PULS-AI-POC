000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4027500.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/03/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET UPPDATERAR VOR MESSAGES.                              
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDA6                                       
001100*                              WDP5                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T275                                              
001500*        MID:         W4I27501                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O27501                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  IDPGM                       PIC X(08)   VALUE 'W4027500'.            
002800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003010 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003020 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003100 77  WS-IDARTNR                  PIC X(8)    VALUE SPACE.                 
003200 77  SW-INFO-FINNS               PIC X       VALUE 'N'.                   
003300 77  SW-TEVOREXT                 PIC X       VALUE 'N'.                   
003400 77  SW-FLLAEST                  PIC X       VALUE 'N'.                   
003500 77  SW-HOPP                     PIC X       VALUE 'N'.                   
003600 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO COMP-3.           
003700 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
003800 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
003900 77  WS-TIREGDAT-URSP            PIC 9(6)    VALUE ZERO.                  
004000 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
004100 77  WS-TIREGTID-URSP            PIC 9(8)    VALUE ZERO.                  
004200 77  WS-TIREGDAT-AVV             PIC 9(6)    VALUE ZERO.                  
004300 77  WS-TIREGTID-AVV             PIC 9(8)    VALUE ZERO.                  
004400                                                                          
004410 01  -COPY WWDCKONS                                                       
004500 01  W-CURRENT-DATE              PIC 9(12).                               
004600 01  FILLER REDEFINES W-CURRENT-DATE.                                     
004700     03  W-DATE                  PIC 9(6).                                
004800     03  W-TIME                  PIC 9(6).                                
004900                                                                          
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005100     88  INDATA-OK                           VALUE 'J'.                   
005200     88  INDATA-FEL                          VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '4275'.                
005600     88  GODK-MID                            VALUE '4225'.                
005700                                                                          
005800 01  TEXT-VORNOT.                                                         
005900     03 FILLER                   PIC X(8).                                
006000     03 VORNOT-TEXT              PIC X(302).                              
006100                                                                          
006200 01  TEXT-LOSNOT.                                                         
006300     03 LOSNOT-RUBRIK            PIC X(8).                                
006400     03 LOSNOT-TEXT              PIC X(142).                              
006500     EJECT                                                                
006600 01  W-PROG-TO-PROG-SW.                                                   
006700     03  M-SW-LL                 PIC S9(4)   VALUE +80 COMP SYNC.         
006800     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
006900     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W0T501  '.            
007000     03  M-SW-IDTRANS            PIC X(4)    VALUE '4275'.                
007100     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
007200                                                                          
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007900*01 -COPY WMEDAREA                                                        
008000                                                                          
008100 01  MESSAGE-CODES.                                                       
008200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008500     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800                                                                          
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W4I27501                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009610*    --- FÖR HOPP TILL 2106-DELIVERY NOTE                                 
009620       05  2106-MID REDEFINES MSG-MID-OUT.                                
009630*          07  -COPY W2I10601 -PRE 2106-                                  
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O27501                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700                                                                          
010800 01  NYCKLAR-TILL-DLI.                                                    
010900     03  W-WDA601KY-X.                                                    
011000         05  W-IDDISTR           PIC S9(5)  COMP-3 VALUE ZERO.            
011100         05  W-IDKUNDNR          PIC S9(7)  COMP-3 VALUE ZERO.            
011200         05  W-IDKUNDRF          PIC X(10)  VALUE SPACE.                  
011300         05  W-TIREGDAT-URSP     PIC S9(7)  COMP-3 VALUE ZERO.            
011400         05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.            
011500         05  W-TIREGTID-URSP     PIC S9(9)  COMP-3 VALUE ZERO.            
011600         05  W-TIREGDAT-AVV      PIC S9(7)  COMP-3 VALUE ZERO.            
011700         05  W-TIREGTID-AVV      PIC S9(9)  COMP-3 VALUE ZERO.            
011800                                                                          
011900      03 W-WDP501KY-X.                                                    
012000         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
012100         05  W-IDDOKTYP          PIC X(8)   VALUE SPACE.                  
012200         05  W-IDDOK             PIC X(8)   VALUE SPACE.                  
012300                                                                          
012400      03  W-IDSID-X.                                                      
012500         05  W-IDSID             PIC S9(3)  COMP-3 VALUE ZERO.            
012510                                                                          
012520     03  W-WDD901KY-X.                                                    
012530         05  W-IDARTNR-D9        PIC S9(9)  VALUE ZERO COMP-3.            
012540         05  W-IDDC-D9           PIC X(2)   VALUE SPACE.                  
012550     03  W-IDLEVNR-X.                                                     
012560         05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                  
012570     03  W-IDLEVBSK-X.                                                    
012580         05 W-IDLEVBSK           PIC S9(1)  VALUE ZERO COMP-3.            
012600     EJECT                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013100     SKIP3                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400*                                                                         
013500 01  ALL-SSA.                                                             
013510     03 SSA1                     PIC X(64).                               
013600     03 SSA2                     PIC X(64).                               
013610     03 SSA3                     PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200                                                                          
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
014400 01  DLI-IO-WDA601.                                                       
014500*    03  -COPY WDA601                                                     
014600     EJECT                                                                
014700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA611'.                      
014800 01  DLI-IO-WDA611.                                                       
014900*    03  -COPY WDA611                                                     
015000     EJECT                                                                
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
015200 01  DLI-IO-WDA612.                                                       
015300*    03  -COPY WDA612                                                     
015400     EJECT                                                                
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
015600 01  DLI-IO-WDA613.                                                       
015700*    03  -COPY WDA613                                                     
015800     EJECT                                                                
015900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP501'.                      
016000 01  DLI-IO-WDP501.                                                       
016100*    03  -COPY WDP501                                                     
016200     EJECT                                                                
016300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP512'.                      
016400 01  DLI-IO-WDP512.                                                       
016500*    03  -COPY WDP512                                                     
016600     EJECT                                                                
016610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
016620 01  DLI-IO-WDD925.                                                       
016630*    03  -COPY WDD925  -PRE D9-                                           
016640     EJECT                                                                
016700 LINKAGE SECTION.                                                         
016800*01  -COPY W0009   -PRE MSG-                                              
016900     EJECT                                                                
017000*01  -COPY W0009   -PRE ALT-                                              
017100     EJECT                                                                
017110*01  -COPY W0009   -PRE 2106-                                             
017120     EJECT                                                                
017200*01  -COPY W0008    -PRE WDA6-                                            
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008    -PRE WDP5-                                            
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017710*01  -COPY W0008    -PRE WDD9-                                            
017720     05  FILLER                  PIC X.                                   
017730     EJECT                                                                
017800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB 2106-PCB                       
017810                           WDA6-PCB WDP5-PCB WDD9-PCB.                    
017900 MAIN SECTION.                                                            
018000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB 2106-PCB                       
018010                           WDA6-PCB WDP5-PCB WDD9-PCB.                    
018100                                                                          
018200     PERFORM IMS-GET-MSG                                                  
018300     IF SEGMENT-FINNS                                                     
018400        PERFORM A-INIT                                                    
018500        IF EGEN-MID OR GODK-MID                                           
018600           PERFORM B-KOLLA-NYCKLAR                                        
018710           IF MFS-UPDATE                                                  
018800              PERFORM G-KOLLA-INPUT                                       
018900              IF INDATA-OK                                                
019000                 PERFORM H-UPPDATERA                                      
019100                 PERFORM F-LAES-VISA-INFO                                 
019200              END-IF                                                      
019300           ELSE                                                           
019400              IF (MFS-FIRST AND GODK-MID)                                 
019410              OR  MFS-PREVIOUS                                            
019411*             MOVE 'TILL F-LAS' TO CURRENT-SECTION                        
019420*       CALL FELLOG                                                       
019500                 PERFORM F-LAES-VISA-INFO                                 
019501                 IF MFS-PREVIOUS                                          
019510                    PERFORM C-HOPP-TILL-2106                              
019511*                   CALL FELLOG                                           
019512                    MOVE JA TO SW-HOPP                                    
019513                    PERFORM IMS-INSERT-2106-MSG                           
019520                 END-IF                                                   
019600              ELSE                                                        
019700                 IF EGEN-MID                                              
019800                    PERFORM E-SAMMA-SIDA                                  
019900                 ELSE                                                     
020000                    IF GODK-MID                                           
020100                       PERFORM F-LAES-VISA-INFO                           
020200                    END-IF                                                
020300                 END-IF                                                   
020400              END-IF                                                      
020500           END-IF                                                         
020600           IF SW-HOPP = NEJ                                               
020700             COMPUTE MSG-KVLL = LENGTH OF MOD-W4O27501-CTX + 4            
020800             PERFORM IMS-INSERT-MSG                                       
020900           END-IF                                                         
021000        ELSE                                                              
021100           PERFORM IMS-INSERT-ALT-MSG                                     
021200        END-IF                                                            
021300     END-IF                                                               
021400                                                                          
021500     MOVE ZERO TO RETURN-CODE                                             
021600     GOBACK                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 A-INIT SECTION.                                                          
021910     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
022000                                                                          
022100     IF MSG-DUBBLA-TRANSKODER                                             
022200        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I27501-CTX            
022300        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
022400        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
022500     ELSE                                                                 
022600        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I27501-CTX             
022700        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
022800        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
022900     END-IF                                                               
023000                                                                          
023100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
023200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
023300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023310*             MOVE 'I A-INIT  ' TO CURRENT-SECTION                        
023320*       CALL FELLOG                                                       
023400                                                                          
023500     MOVE LOW-VALUE TO MSG-AREA                                           
023600     MOVE 'W4O275N1' TO MFS-IDMOD                                         
023700     MOVE '4275' TO MOD-IDTRANS                                           
023800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023900                                                                          
024000     IF EGEN-MID                                                          
024100        CONTINUE                                                          
024200     ELSE                                                                 
024300        MOVE SPACE TO MFS-KDTRTYP                                         
024400        MOVE '7' TO MFS-IDPFK                                             
024500     END-IF                                                               
024600                                                                          
024700     MOVE FUNCTION CURRENT-DATE (3:12) TO W-CURRENT-DATE                  
024800     MOVE NEJ TO SW-HOPP                                                  
024900     .                                                                    
025000     EJECT                                                                
025100 B-KOLLA-NYCKLAR SECTION.                                                 
025110     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
025200                                                                          
025300     MOVE 'SE ' TO MED-IDSKYLT                                            
025400                                                                          
025500     INSPECT MID-IDDISTR REPLACING LEADING SPACE BY ZERO                  
025600     IF MID-IDDISTR NUMERIC                                               
025700        MOVE MID-IDDISTR TO WS-IDDISTR                                    
025800        MOVE WS-IDDISTR TO W-IDDISTR                                      
025900     ELSE                                                                 
026000        MOVE ZERO TO W-IDDISTR                                            
026100                     WS-IDDISTR                                           
026200     END-IF                                                               
026300                                                                          
026400     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
026500     IF MID-IDKUNDNR NUMERIC                                              
026600        MOVE MID-IDKUNDNR TO WS-IDKUNDNR                                  
026700        MOVE WS-IDKUNDNR TO W-IDKUNDNR                                    
026800     ELSE                                                                 
026900        MOVE ZERO TO W-IDKUNDNR                                           
027000                     WS-IDKUNDNR                                          
027100     END-IF                                                               
027200                                                                          
027300     MOVE MID-IDORDNR7 TO W-IDKUNDRF                                      
027400                                                                          
027500     IF MID-TIREGDAT-URSP NUMERIC                                         
027600        MOVE MID-TIREGDAT-URSP TO W-TIREGDAT-URSP                         
027700                                  WS-TIREGDAT-URSP                        
027800     ELSE                                                                 
027900        MOVE ZERO TO W-TIREGDAT-URSP                                      
028000                     WS-TIREGDAT-URSP                                     
028100     END-IF                                                               
028200                                                                          
028300     INSPECT MID-IDARTNR REPLACING LEADING SPACE BY ZERO                  
028400     IF MID-IDARTNR NUMERIC                                               
028500        MOVE MID-IDARTNR TO WS-IDARTNR-NUM                                
028600        MOVE WS-IDARTNR-NUM TO W-IDARTNR                                  
028700     ELSE                                                                 
028800        MOVE ZERO TO W-IDARTNR                                            
028900                     WS-IDARTNR-NUM                                       
029000     END-IF                                                               
029100                                                                          
029200     IF MID-TIREGTID-URSP NUMERIC                                         
029300        MOVE MID-TIREGTID-URSP TO W-TIREGTID-URSP                         
029400                                  WS-TIREGTID-URSP                        
029500     ELSE                                                                 
029600        MOVE ZERO TO W-TIREGTID-URSP                                      
029700                     WS-TIREGTID-URSP                                     
029800     END-IF                                                               
029900                                                                          
030000     IF MID-TIREGDAT-AVV NUMERIC                                          
030100        MOVE MID-TIREGDAT-AVV TO W-TIREGDAT-AVV                           
030200                                 WS-TIREGDAT-AVV                          
030300     ELSE                                                                 
030400        MOVE ZERO TO W-TIREGDAT-AVV                                       
030500                     WS-TIREGDAT-AVV                                      
030600     END-IF                                                               
030700                                                                          
030800     IF MID-TIREGTID-AVV NUMERIC                                          
030900        MOVE MID-TIREGTID-AVV TO W-TIREGTID-AVV                           
031000                                 WS-TIREGTID-AVV                          
031100     ELSE                                                                 
031200        MOVE ZERO TO W-TIREGTID-AVV                                       
031300                     WS-TIREGTID-AVV                                      
031400     END-IF                                                               
031500                                                                          
031600     IF EGEN-MID OR GODK-MID                                              
031700        MOVE WS-IDDISTR       TO MOD-IDDISTR                              
031800        MOVE WS-IDKUNDNR      TO MOD-IDKUNDNR                             
031900        MOVE W-IDKUNDRF       TO MOD-IDORDNR7                             
032000        MOVE WS-TIREGDAT-URSP TO MOD-TIREGDAT-URSP                        
032100        MOVE WS-IDARTNR-NUM   TO MOD-IDARTNR                              
032200        MOVE WS-TIREGTID-URSP TO MOD-TIREGTID-URSP                        
032300        MOVE WS-TIREGDAT-AVV  TO MOD-TIREGDAT-AVV                         
032400        MOVE WS-TIREGTID-AVV  TO MOD-TIREGTID-AVV                         
032500     ELSE                                                                 
032600        PERFORM MFS-RENSA-DOLDA-NYCKLAR                                   
032700     END-IF                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 C-HOPP-TILL-2106 SECTION.                                                
033010     MOVE 'C-HOPP-TILL-2106' TO CURRENT-SECTION                           
033100                                                                          
033101     MOVE ALL '+'        TO 2106-MID                                      
033102     MOVE VOR-IDARTNR    TO WS-IDARTNR-NUM                                
033103     MOVE WS-IDARTNR-NUM TO 2106-MID-IDARTNR-IN                           
033104     MOVE VOR-IDLEVNR    TO 2106-MID-IDLEVNR-IN                           
033105                                                                          
033106     COMPUTE MSG-KVLL = LENGTH OF 2106-MID-W2I10601 + 17                  
033107     MOVE LOW-VALUE      TO MSG-KDZ1                                      
033108                            MSG-KDZ2                                      
033109     MOVE 'W2T106'       TO MSG-KDTRANS-1                                 
033110     MOVE '4275'         TO MSG-IDTRANS-1                                 
033111*    MOVE '2'            TO MSG-KDMFSFOR-1                                
033112     .                                                                    
033120     EJECT                                                                
033130 E-SAMMA-SIDA SECTION.                                                    
033140     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
033150                                                                          
033200     MOVE NEJ TO SW-FLLAEST                                               
033300     IF MID-FLLAEST = 'J' OR 'Y' OR 'N'                                   
033400        MOVE JA TO SW-FLLAEST                                             
033500        MOVE MFS-ROER-EJ-FAELT TO MOD-FLLAEST                             
033600     ELSE                                                                 
033700        MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                               
033800     END-IF                                                               
033900                                                                          
034000     MOVE NEJ TO SW-TEVOREXT                                              
034100     PERFORM IMS-GHU-WDA612                                               
034200     IF SEGMENT-FINNS                                                     
034300        IF VTE-TEVOREXT NOT = MID-TEVOREXT                                
034400           MOVE JA TO SW-TEVOREXT                                         
034500        END-IF                                                            
034600     ELSE                                                                 
034700        IF MID-TEVOREXT = SPACE OR LOW-VALUE                              
034800           CONTINUE                                                       
034900        ELSE                                                              
035000           MOVE JA TO SW-TEVOREXT                                         
035100        END-IF                                                            
035200     END-IF                                                               
035300                                                                          
035400     IF (MID-TEVORINT = ALL '+') AND (SW-FLLAEST = NEJ)                   
035500     AND (SW-TEVOREXT = NEJ) AND (MID-TELOSNOT = ALL '+')                 
035600        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
035700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
035800     ELSE                                                                 
035900        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
036000        CALL WMEDKONV USING MED-WMEDAREA                                  
036100        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
036200        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
036300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
036400        PERFORM EA-MID-INDATA-TILL-MOD                                    
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 EA-MID-INDATA-TILL-MOD SECTION.                                          
036810     MOVE 'EA-MID-TILL-MOD ' TO CURRENT-SECTION                           
036900                                                                          
037000     IF SW-FLLAEST = JA                                                   
037100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLAEST-ATTR                    
037200     ELSE                                                                 
037300        MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                               
037400     END-IF                                                               
037500                                                                          
037600     IF MID-TEVORINT = ALL '+'                                            
037700        CONTINUE                                                          
037800     ELSE                                                                 
037900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEVORINT-ATTR                   
038000     END-IF                                                               
038100                                                                          
038200     IF MID-TELOSNOT = ALL '+'                                            
038300        CONTINUE                                                          
038400     ELSE                                                                 
038500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELOSNOT-ATTR                   
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 F-LAES-VISA-INFO SECTION.                                                
038910     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
039000                                                                          
039100     MOVE NEJ TO SW-INFO-FINNS                                            
039200                                                                          
039300     PERFORM IMS-GET-WDA601                                               
039400     IF SEGMENT-FINNS                                                     
039500        MOVE VOR-IDDISTR  TO MOD-IDDISTR-UT                               
039600        MOVE VOR-IDKUNDNR TO MOD-IDKUNDNR-UT                              
039700        MOVE VOR-IDKUNDRF TO MOD-IDKUNDRF-UT                              
039800        INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE           
039900        MOVE VOR-IDARTNR  TO MOD-IDARTNR-UT                               
040000        COMPUTE WS-KVANTAL = VOR-KVBEART-Q - VOR-KVPREAVB                 
040100        END-COMPUTE                                                       
040200        MOVE WS-KVANTAL   TO MOD-KVANTAL-UT                               
040300                                                                          
040400        PERFORM FA-LAES-WDA6                                              
040500        PERFORM FB-LAES-WDP5                                              
040510        PERFORM FC-LAES-WDD9                                              
040600                                                                          
040700        IF SW-INFO-FINNS = NEJ                                            
040800           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
040900           CALL WMEDKONV USING MED-WMEDAREA                               
041000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
041100        END-IF                                                            
041200     ELSE                                                                 
041300        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
041400        CALL WMEDKONV USING MED-WMEDAREA                                  
041500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 FA-LAES-WDA6 SECTION.                                                    
041910     MOVE 'FA-LAES-WDA6    ' TO CURRENT-SECTION                           
042000                                                                          
042100     PERFORM IMS-GET-WDA611                                               
042200     IF SEGMENT-FINNS                                                     
042300        MOVE VTI-TEVORINT TO MOD-TEVORINT                                 
042400        MOVE JA TO SW-INFO-FINNS                                          
042500     ELSE                                                                 
042600        MOVE MFS-RENSA-FAELT TO MOD-TEVORINT                              
042700     END-IF                                                               
042800                                                                          
042900     PERFORM IMS-GET-WDA612                                               
043000     IF SEGMENT-FINNS                                                     
043100        MOVE VTE-TEVOREXT TO MOD-TEVOREXT                                 
043200        MOVE JA TO SW-INFO-FINNS                                          
043300     END-IF                                                               
043400                                                                          
043500     PERFORM IMS-GET-WDA613                                               
043600     IF SEGMENT-FINNS                                                     
043700        MOVE VTS-TEVORSC TO MOD-TEVORSC                                   
043800        MOVE JA TO SW-INFO-FINNS                                          
043900     ELSE                                                                 
044000        MOVE MFS-RENSA-FAELT TO MOD-TEVORSC                               
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 FB-LAES-WDP5 SECTION.                                                    
044410     MOVE 'FB-LAES-WDP5    ' TO CURRENT-SECTION                           
044500                                                                          
044600     MOVE 'S  '       TO W-IDSKYLT                                        
044700     MOVE +1          TO W-IDSID                                          
044800     MOVE MID-IDARTNR(2:8) TO WS-IDARTNR                                  
044900     INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                   
045000     MOVE WS-IDARTNR  TO W-IDDOK                                          
045100                                                                          
046300     MOVE 'LOSNOT' TO W-IDDOKTYP                                          
046400     PERFORM IMS-GHU-WDP512                                               
046500     IF SEGMENT-FINNS                                                     
046600        MOVE TEXT-TEINFO TO TEXT-LOSNOT                                   
046700        MOVE LOSNOT-TEXT TO MOD-TELOSNOT                                  
046800        MOVE JA TO SW-INFO-FINNS                                          
046900     ELSE                                                                 
047000        MOVE MFS-RENSA-FAELT TO MOD-TELOSNOT                              
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047310 FC-LAES-WDD9 SECTION.                                                    
047320     MOVE 'FC-LAES-WDD9    ' TO CURRENT-SECTION                           
047330                                                                          
047340     MOVE VOR-IDARTNR    TO W-IDARTNR-D9                                  
047350     MOVE WC-CDC-SE      TO W-IDDC-D9                                     
047360     MOVE VOR-IDLEVNR    TO W-IDLEVNR                                     
047390                                                                          
047391     MOVE +2             TO W-IDLEVBSK                                    
047392     PERFORM IMS-GU-WDD925                                                
047393     IF SEGMENT-FINNS                                                     
047394        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(1)                          
047396        MOVE JA TO SW-INFO-FINNS                                          
047397     ELSE                                                                 
047398        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(1)                           
047402     END-IF                                                               
047403                                                                          
047404     MOVE +4             TO W-IDLEVBSK                                    
047405     PERFORM IMS-GU-WDD925                                                
047406     IF SEGMENT-FINNS                                                     
047407        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(2)                          
047408        MOVE JA TO SW-INFO-FINNS                                          
047409     ELSE                                                                 
047410        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(2)                           
047411     END-IF                                                               
047412                                                                          
047413     MOVE +5             TO W-IDLEVBSK                                    
047414     PERFORM IMS-GU-WDD925                                                
047415     IF SEGMENT-FINNS                                                     
047416        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(3)                          
047417        MOVE JA TO SW-INFO-FINNS                                          
047418     ELSE                                                                 
047419        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(3)                           
047420     END-IF                                                               
047421                                                                          
047422     MOVE +6             TO W-IDLEVBSK                                    
047423     PERFORM IMS-GU-WDD925                                                
047424     IF SEGMENT-FINNS                                                     
047425        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(4)                          
047426        MOVE JA TO SW-INFO-FINNS                                          
047427     ELSE                                                                 
047428        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(4)                           
047429     END-IF                                                               
047606     .                                                                    
047607     EJECT                                                                
047608 G-KOLLA-INPUT SECTION.                                                   
047609     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
047610                                                                          
047620     MOVE NEJ TO SW-FLLAEST                                               
047700     IF MID-FLLAEST = 'J' OR 'Y' OR 'N'                                   
047800        MOVE JA TO SW-FLLAEST                                             
047900        MOVE MFS-ROER-EJ-FAELT TO MOD-FLLAEST                             
048000     ELSE                                                                 
048100        MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                               
048200     END-IF                                                               
048300                                                                          
048400     MOVE NEJ TO SW-TEVOREXT                                              
048500     PERFORM IMS-GHU-WDA612                                               
048600     IF SEGMENT-FINNS                                                     
048700        IF VTE-TEVOREXT NOT = MID-TEVOREXT                                
048800           MOVE JA TO SW-TEVOREXT                                         
048900        END-IF                                                            
049000     ELSE                                                                 
049100        IF MID-TEVOREXT = SPACE OR LOW-VALUE                              
049200           CONTINUE                                                       
049300        ELSE                                                              
049400           MOVE JA TO SW-TEVOREXT                                         
049500        END-IF                                                            
049600     END-IF                                                               
049700                                                                          
049800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
049900     MOVE JA TO INDATA-SW                                                 
050000     IF (MID-TEVORINT = ALL '+') AND (SW-FLLAEST = NEJ)                   
050100     AND (SW-TEVOREXT = NEJ) AND (MID-TELOSNOT = ALL '+')                 
050200        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
050300        CALL WMEDKONV USING MED-WMEDAREA                                  
050400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
050500        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
050600        MOVE NEJ TO INDATA-SW                                             
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 H-UPPDATERA SECTION.                                                     
051010     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
051100                                                                          
051200     IF MID-FLLAEST = 'J' OR 'Y' OR 'N'                                   
051300        PERFORM IMS-GHU-WDA613                                            
051400        IF SEGMENT-FINNS                                                  
051500           IF MID-FLLAEST = 'J' OR 'Y'                                    
051600              MOVE 'J' TO VTS-FLLAEST                                     
051700           ELSE                                                           
051800              MOVE 'N' TO VTS-FLLAEST                                     
051900           END-IF                                                         
052000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLLAEST-ATTR                 
052100           PERFORM IMS-REPL-WDA613                                        
052200        ELSE                                                              
052300           MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                            
052400        END-IF                                                            
052500     END-IF                                                               
052600                                                                          
052700     IF MID-TEVORINT NOT = ALL '+'                                        
052800        PERFORM IMS-GHU-WDA611                                            
052900        IF SEGMENT-FINNS                                                  
053000           IF MID-TEVORINT = SPACE                                        
053100              PERFORM IMS-DLET-WDA611                                     
053200           ELSE                                                           
053300              MOVE MID-TEVORINT TO VTI-TEVORINT                           
053400              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORINT-ATTR             
053500              PERFORM IMS-REPL-WDA611                                     
053600           END-IF                                                         
053700        ELSE                                                              
053800           MOVE '1'          TO VTI-KDSEGKEY                              
053900           MOVE MID-TEVORINT TO VTI-TEVORINT                              
054000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORINT-ATTR                
054100           PERFORM IMS-ISRT-WDA611                                        
054200        END-IF                                                            
054300     ELSE                                                                 
054400        MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORINT                            
054500     END-IF                                                               
054600                                                                          
054700     IF SW-TEVOREXT = JA                                                  
054800        PERFORM IMS-GHU-WDA612                                            
054900        IF SEGMENT-FINNS                                                  
055000           IF MID-TEVOREXT = SPACE                                        
055100              PERFORM IMS-DLET-WDA612                                     
055200           ELSE                                                           
055300              MOVE MID-TEVOREXT TO VTE-TEVOREXT                           
055400              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVOREXT-ATTR             
055500              MOVE 'N'          TO VTE-FLLAEST                            
055600              PERFORM IMS-REPL-WDA612                                     
055700           END-IF                                                         
055800        ELSE                                                              
055900           MOVE '1'          TO VTE-KDSEGKEY                              
056000           MOVE 'N'          TO VTE-FLLAEST                               
056100           MOVE MID-TEVOREXT TO VTE-TEVOREXT                              
056200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVOREXT-ATTR                
056300           PERFORM IMS-ISRT-WDA612                                        
056400        END-IF                                                            
056500     ELSE                                                                 
056600        MOVE MFS-ROER-EJ-FAELT TO MOD-TEVOREXT                            
056700     END-IF                                                               
056800                                                                          
056900                                                                          
057000     IF MID-TELOSNOT NOT = ALL '+'                                        
057100        MOVE 'LOSNOT'    TO W-IDDOKTYP                                    
057200        MOVE 'S  '       TO W-IDSKYLT                                     
057300        MOVE +1          TO W-IDSID                                       
057400        MOVE MID-IDARTNR(2:8) TO WS-IDARTNR                               
057500        INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                
057600        MOVE WS-IDARTNR  TO W-IDDOK                                       
057700                                                                          
057800        PERFORM IMS-GET-WDP501                                            
057900        IF SEGMENT-FINNS                                                  
058000           PERFORM IMS-GHU-WDP512                                         
058100           IF SEGMENT-FINNS                                               
058200              IF MID-TELOSNOT = SPACE                                     
058300                 PERFORM IMS-GET-WDP501                                   
058400                 PERFORM IMS-DLET-WDP501                                  
058500              ELSE                                                        
058600                 MOVE MID-TELOSNOT TO LOSNOT-TEXT                         
058700                 MOVE 'LÖSN   :'   TO LOSNOT-RUBRIK                       
058800                 MOVE TEXT-LOSNOT  TO TEXT-TEINFO                         
058900                 MOVE W-DATE       TO TEXT-TIREGDAT                       
059000                 MOVE W-TIME       TO TEXT-TIREGTID                       
059100                 PERFORM IMS-REPL-WDP512                                  
059200                 MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELOSNOT-ATTR          
059300                 PERFORM IMS-GET-WDP501                                   
059400                 MOVE W-DATE       TO INFO-TIREGDAT                       
059500                 MOVE W-TIME       TO INFO-TIREGTID                       
059600                 PERFORM IMS-REPL-WDP501                                  
059700              END-IF                                                      
059800           ELSE                                                           
059900              IF MID-TELOSNOT NOT = SPACE                                 
060000                 PERFORM HB-FIXA-WDP512                                   
060100                 MOVE MID-TELOSNOT TO LOSNOT-TEXT                         
060200                 MOVE TEXT-LOSNOT  TO TEXT-TEINFO                         
060300                 PERFORM IMS-ISRT-WDP512                                  
060400                 MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELOSNOT-ATTR          
060500              END-IF                                                      
060600           END-IF                                                         
060700        ELSE                                                              
060800           IF MID-TELOSNOT NOT = SPACE                                    
060900              PERFORM HA-FIXA-WDP501                                      
061000              PERFORM IMS-ISRT-WDP501                                     
061100              PERFORM HB-FIXA-WDP512                                      
061200              MOVE MID-TELOSNOT TO LOSNOT-TEXT                            
061300              MOVE 'LÖSN   :'   TO LOSNOT-RUBRIK                          
061400              MOVE TEXT-LOSNOT  TO TEXT-TEINFO                            
061500              PERFORM IMS-ISRT-WDP512                                     
061600              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELOSNOT-ATTR             
061700           END-IF                                                         
061800        END-IF                                                            
061900     ELSE                                                                 
062000        MOVE MFS-ROER-EJ-FAELT TO MOD-TELOSNOT                            
062100     END-IF                                                               
062200                                                                          
062300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
062400     CALL WMEDKONV USING MED-WMEDAREA                                     
062500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
062600     .                                                                    
062700     EJECT                                                                
062800 HA-FIXA-WDP501 SECTION.                                                  
062810     MOVE 'HA-FIXA-WDP501  ' TO CURRENT-SECTION                           
062900                                                                          
063000     MOVE 'S  '             TO INFO-IDSKYLT                               
063100     MOVE 'LOSNOT'          TO INFO-IDDOKTYP                              
063200     MOVE MID-IDARTNR(2:8)  TO WS-IDARTNR                                 
063300     INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                   
063400     MOVE WS-IDARTNR        TO INFO-IDDOK                                 
063500     MOVE W-DATE            TO INFO-TIREGDAT                              
063600     MOVE W-TIME            TO INFO-TIREGTID                              
063700     MOVE MSG-SIGNON-USERID TO INFO-IDUSER                                
063800     MOVE SPACE             TO INFO-BEDOK                                 
063900     MOVE LOW-VALUE         TO INFO-FILLER                                
064000     .                                                                    
064100     EJECT                                                                
064200 HB-FIXA-WDP512 SECTION.                                                  
064210     MOVE 'HB-FIXA-WDP512  ' TO CURRENT-SECTION                           
064300                                                                          
064400     MOVE +1                TO TEXT-IDSID                                 
064500     MOVE MSG-SIGNON-USERID TO TEXT-IDUSER                                
064600     MOVE W-DATE            TO TEXT-TIREGDAT                              
064700     MOVE W-TIME            TO TEXT-TIREGTID                              
064800     MOVE 'LÖSN   :'        TO LOSNOT-RUBRIK                              
064900     MOVE MID-TELOSNOT      TO LOSNOT-TEXT                                
065000     MOVE TEXT-LOSNOT       TO TEXT-TEINFO                                
065100     .                                                                    
065200     EJECT                                                                
065300 MFS-RENSA-DOLDA-NYCKLAR SECTION.                                         
065400                                                                          
065500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR                                  
065600                             MOD-IDKUNDNR                                 
065700                             MOD-IDORDNR7                                 
065800                             MOD-TIREGDAT-URSP                            
065900                             MOD-IDARTNR                                  
066000                             MOD-TIREGTID-URSP                            
066100                             MOD-TIREGDAT-AVV                             
066200                             MOD-TIREGTID-AVV                             
066300     .                                                                    
066400     SKIP3                                                                
066500 MFS-ROER-EJ-FAELT-IN-UT SECTION.                                         
066600                                                                          
066700     MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORINT                               
066800                               MOD-TEVOREXT                               
066900                               MOD-TEVORSC                                
067000                               MOD-TEVORNOT(1)                            
067010                               MOD-TEVORNOT(2)                            
067020                               MOD-TEVORNOT(3)                            
067030                               MOD-TEVORNOT(4)                            
067100                               MOD-TELOSNOT                               
067200     .                                                                    
067300     EJECT                                                                
067400 MFS-ROER-EJ-FAELT-UT SECTION.                                            
067500                                                                          
067600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                             
067700                               MOD-IDKUNDNR-UT                            
067800                               MOD-IDKUNDRF-UT                            
067900                               MOD-IDARTNR-UT                             
068000                               MOD-KVANTAL-UT                             
068100     .                                                                    
068200     EJECT                                                                
068300* --- IMS SEKTIONER ---                                                   
068400     SKIP3                                                                
068500 IMS-GET-MSG SECTION.                                                     
068600     MOVE '  QC' TO GODK-STATUSKODER                                      
068700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
068800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100     SKIP3                                                                
069200 IMS-INSERT-MSG SECTION.                                                  
069300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
069400     MOVE SPACE TO GODK-STATUSKODER                                       
069500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
069600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900     SKIP3                                                                
070000 IMS-INSERT-ALT-MSG SECTION.                                              
070100     MOVE SPACE TO GODK-STATUSKODER                                       
070200     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
070300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
070400     PERFORM IMS-STATUSKONTROLL                                           
070500     .                                                                    
070600     EJECT                                                                
070610 IMS-INSERT-2106-MSG SECTION.                                             
070620     MOVE SPACE TO GODK-STATUSKODER                                       
070630     CALL CBLTDLI USING ISRT 2106-PCB MSG-IO-AREA                         
070640     MOVE 2106-STATUS-CODE TO STATUS-WS                                   
070650     PERFORM IMS-STATUSKONTROLL                                           
070660     .                                                                    
070670     EJECT                                                                
070700 IMS-GET-WDA601 SECTION.                                                  
070710     MOVE 'GET-WDA601      ' TO CURRENT-IMS-SECTION                       
070720                                                                          
070730     MOVE SPACE               TO ALL-SSA                                  
070800     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
070900          DELIMITED BY SIZE INTO SSA1                                     
071000     MOVE '  GE' TO GODK-STATUSKODER                                      
071100     CALL CBLTDLI USING GU WDA6-PCB DLI-IO-WDA601 SSA1                    
071200     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
071300     PERFORM IMS-STATUSKONTROLL                                           
071400     .                                                                    
071500     SKIP3                                                                
071600 IMS-GHU-WDA611 SECTION.                                                  
071610     MOVE 'GHU-WDA611      ' TO CURRENT-IMS-SECTION                       
071620                                                                          
071630     MOVE SPACE               TO ALL-SSA                                  
071700     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
071800          DELIMITED BY SIZE INTO SSA1                                     
071900     MOVE 'WDA611  ' TO SSA2                                              
072000     MOVE '  GE' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA611 SSA1 SSA2              
072200     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     SKIP3                                                                
072600 IMS-GET-WDA611 SECTION.                                                  
072610     MOVE 'GET-WDA611      ' TO CURRENT-IMS-SECTION                       
072620                                                                          
072630     MOVE SPACE               TO ALL-SSA                                  
072700     MOVE 'WDA611  ' TO SSA1                                              
072800     MOVE '  GE' TO GODK-STATUSKODER                                      
072900     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA611 SSA1                   
073000     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     EJECT                                                                
073400 IMS-ISRT-WDA611 SECTION.                                                 
073410     MOVE 'ISRT-WDA611     ' TO CURRENT-IMS-SECTION                       
073420                                                                          
073430     MOVE SPACE               TO ALL-SSA                                  
073500     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
073600          DELIMITED BY SIZE INTO SSA1                                     
073700     MOVE 'WDA611 ' TO SSA2                                               
073800     MOVE '  ' TO GODK-STATUSKODER                                        
073900     CALL CBLTDLI USING ISRT WDA6-PCB DLI-IO-WDA611 SSA1 SSA2             
074000     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
074100     PERFORM IMS-STATUSKONTROLL                                           
074200     .                                                                    
074300     SKIP3                                                                
074400 IMS-REPL-WDA611 SECTION.                                                 
074410     MOVE 'REPL-WDA611     ' TO CURRENT-IMS-SECTION                       
074420                                                                          
074430     MOVE SPACE               TO ALL-SSA                                  
074500     MOVE '  ' TO GODK-STATUSKODER                                        
074600     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA611                       
074700     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
074800     PERFORM IMS-STATUSKONTROLL                                           
074900     .                                                                    
075000     SKIP3                                                                
075100 IMS-DLET-WDA611 SECTION.                                                 
075110     MOVE 'DLET-WDA611     ' TO CURRENT-IMS-SECTION                       
075120                                                                          
075130     MOVE SPACE               TO ALL-SSA                                  
075200     MOVE '  ' TO GODK-STATUSKODER                                        
075300     CALL CBLTDLI USING DLET WDA6-PCB DLI-IO-WDA611                       
075400     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     .                                                                    
075700     EJECT                                                                
075800 IMS-GHU-WDA612 SECTION.                                                  
075810     MOVE 'GHU-WDA612      ' TO CURRENT-IMS-SECTION                       
075820                                                                          
075830     MOVE SPACE               TO ALL-SSA                                  
075900     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
076000          DELIMITED BY SIZE INTO SSA1                                     
076100     MOVE 'WDA612  ' TO SSA2                                              
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA612 SSA1 SSA2              
076400     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     SKIP3                                                                
076800 IMS-GET-WDA612 SECTION.                                                  
076810     MOVE 'GET-WDA612      ' TO CURRENT-IMS-SECTION                       
076820                                                                          
076830     MOVE SPACE               TO ALL-SSA                                  
076900     MOVE 'WDA612  ' TO SSA1                                              
077000     MOVE '  GE' TO GODK-STATUSKODER                                      
077100     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA612 SSA1                   
077200     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     SKIP3                                                                
077600 IMS-ISRT-WDA612 SECTION.                                                 
077610     MOVE 'ISRT-WDA612     ' TO CURRENT-IMS-SECTION                       
077620                                                                          
077630     MOVE SPACE               TO ALL-SSA                                  
077700     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
077800          DELIMITED BY SIZE INTO SSA1                                     
077900     MOVE 'WDA612 ' TO SSA2                                               
078000     MOVE '  ' TO GODK-STATUSKODER                                        
078100     CALL CBLTDLI USING ISRT WDA6-PCB DLI-IO-WDA612 SSA1 SSA2             
078200     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
078300     PERFORM IMS-STATUSKONTROLL                                           
078400     .                                                                    
078500     EJECT                                                                
078600 IMS-REPL-WDA612 SECTION.                                                 
078610     MOVE 'REPL-WDA612     ' TO CURRENT-IMS-SECTION                       
078620                                                                          
078630     MOVE SPACE               TO ALL-SSA                                  
078700     MOVE '  ' TO GODK-STATUSKODER                                        
078800     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA612                       
078900     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
079000     PERFORM IMS-STATUSKONTROLL                                           
079100     .                                                                    
079200     SKIP3                                                                
079300 IMS-DLET-WDA612 SECTION.                                                 
079310     MOVE 'DLET-WDA612     ' TO CURRENT-IMS-SECTION                       
079320                                                                          
079330     MOVE SPACE               TO ALL-SSA                                  
079400     MOVE '  ' TO GODK-STATUSKODER                                        
079500     CALL CBLTDLI USING DLET WDA6-PCB DLI-IO-WDA612                       
079600     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
079700     PERFORM IMS-STATUSKONTROLL                                           
079800     .                                                                    
079900     EJECT                                                                
080000 IMS-GHU-WDA613 SECTION.                                                  
080010     MOVE 'GHU-WDA613      ' TO CURRENT-IMS-SECTION                       
080020                                                                          
080030     MOVE SPACE               TO ALL-SSA                                  
080100     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
080200          DELIMITED BY SIZE INTO SSA1                                     
080300     MOVE 'WDA613  ' TO SSA2                                              
080400     MOVE '  GE' TO GODK-STATUSKODER                                      
080500     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA613 SSA1 SSA2              
080600     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
080700     PERFORM IMS-STATUSKONTROLL                                           
080800     .                                                                    
080900     SKIP3                                                                
081000 IMS-GET-WDA613 SECTION.                                                  
081010     MOVE 'GET-WDA613      ' TO CURRENT-IMS-SECTION                       
081020                                                                          
081030     MOVE SPACE               TO ALL-SSA                                  
081100     MOVE 'WDA613  ' TO SSA1                                              
081200     MOVE '  GE' TO GODK-STATUSKODER                                      
081300     CALL CBLTDLI USING GHNP WDA6-PCB DLI-IO-WDA613 SSA1                  
081400     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     SKIP3                                                                
081800 IMS-REPL-WDA613 SECTION.                                                 
081810     MOVE 'REPL-WDA613     ' TO CURRENT-IMS-SECTION                       
081820                                                                          
081830     MOVE SPACE               TO ALL-SSA                                  
081900     MOVE '  ' TO GODK-STATUSKODER                                        
082000     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA613                       
082100     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
082200     PERFORM IMS-STATUSKONTROLL                                           
082300     .                                                                    
082400     EJECT                                                                
082500 IMS-GET-WDP501 SECTION.                                                  
082510     MOVE 'GET-WDP501      ' TO CURRENT-IMS-SECTION                       
082520                                                                          
082530     MOVE SPACE               TO ALL-SSA                                  
082600     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
082700          DELIMITED BY SIZE INTO SSA1                                     
082800     MOVE '  GE' TO GODK-STATUSKODER                                      
082900     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-WDP501 SSA1                   
083000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
083100     PERFORM IMS-STATUSKONTROLL                                           
083200     .                                                                    
083300     SKIP3                                                                
083400 IMS-ISRT-WDP501 SECTION.                                                 
083410     MOVE 'ISRT-WDP501     ' TO CURRENT-IMS-SECTION                       
083420                                                                          
083430     MOVE SPACE               TO ALL-SSA                                  
083500     MOVE 'WDP501 ' TO SSA1                                               
083600     MOVE '  ' TO GODK-STATUSKODER                                        
083700     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-WDP501 SSA1                  
083800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
083900     PERFORM IMS-STATUSKONTROLL                                           
084000     .                                                                    
084100     EJECT                                                                
084200 IMS-REPL-WDP501 SECTION.                                                 
084210     MOVE 'REPL-WDP501     ' TO CURRENT-IMS-SECTION                       
084220                                                                          
084230     MOVE SPACE               TO ALL-SSA                                  
084300     MOVE '  ' TO GODK-STATUSKODER                                        
084400     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-WDP501                       
084500     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800     SKIP3                                                                
084900 IMS-GHU-WDP512 SECTION.                                                  
084910     MOVE 'GHU-WDP512      ' TO CURRENT-IMS-SECTION                       
084920                                                                          
084930     MOVE SPACE               TO ALL-SSA                                  
085000     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
085100          DELIMITED BY SIZE INTO SSA1                                     
085200     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
085300          DELIMITED BY SIZE INTO SSA2                                     
085400     MOVE '  GE' TO GODK-STATUSKODER                                      
085500     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-WDP512 SSA1 SSA2              
085600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     .                                                                    
085900     SKIP3                                                                
086000 IMS-ISRT-WDP512 SECTION.                                                 
086010     MOVE 'ISRT-WDP512     ' TO CURRENT-IMS-SECTION                       
086020                                                                          
086030     MOVE SPACE               TO ALL-SSA                                  
086100     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
086200          DELIMITED BY SIZE INTO SSA1                                     
086300     MOVE 'WDP512 ' TO SSA2                                               
086400     MOVE '  ' TO GODK-STATUSKODER                                        
086500     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-WDP512 SSA1 SSA2             
086600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     EJECT                                                                
087000 IMS-REPL-WDP512 SECTION.                                                 
087010     MOVE 'REPL-WDP512     ' TO CURRENT-IMS-SECTION                       
087020                                                                          
087030     MOVE SPACE               TO ALL-SSA                                  
087100     MOVE '  ' TO GODK-STATUSKODER                                        
087200     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-WDP512                       
087300     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
087400     PERFORM IMS-STATUSKONTROLL                                           
087500     .                                                                    
087600     SKIP3                                                                
087700 IMS-DLET-WDP501 SECTION.                                                 
087710     MOVE 'DLET-WDP501     ' TO CURRENT-IMS-SECTION                       
087720                                                                          
087730     MOVE SPACE               TO ALL-SSA                                  
087800     MOVE '  ' TO GODK-STATUSKODER                                        
087900     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-WDP501                       
088000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
088100     PERFORM IMS-STATUSKONTROLL                                           
088200     .                                                                    
088300     SKIP3                                                                
088310 IMS-GU-WDD925  SECTION.                                                  
088311     MOVE 'GU-WDD925       ' TO CURRENT-IMS-SECTION                       
088312                                                                          
088313     MOVE SPACE               TO ALL-SSA                                  
088320     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
088330            DELIMITED BY SIZE INTO SSA1                                   
088340     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
088350            DELIMITED BY SIZE INTO SSA2                                   
088360     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
088370            DELIMITED BY SIZE INTO SSA3                                   
088380     MOVE '  GE' TO GODK-STATUSKODER                                      
088390     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3          
088391     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
088392     PERFORM IMS-STATUSKONTROLL                                           
088393     .                                                                    
088400 IMS-STATUSKONTROLL SECTION.                                              
088500     SET STATUS-IX TO 1                                                   
088600     SEARCH GODK-STATUS                                                   
088700       AT END                                                             
088800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
088900         DELIMITED BY SIZE INTO FELTEXT                                   
089000         CALL FELLOG                                                      
089100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
089200         CONTINUE                                                         
089300     END-SEARCH                                                           
089400     .                                                                    
