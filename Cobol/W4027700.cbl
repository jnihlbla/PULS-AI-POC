000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4027700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   MARS 2003.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET UPPDATERAR VOR MESSAGES.                              
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDA6                                       
001100*                              WDP5                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T277                                              
001500*        MID:         W4I27701                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O27701                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  IDPGM                       PIC X(08)   VALUE 'W4027700'.            
002800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003010 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003020 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003100 77  WS-IDARTNR                  PIC X(8)    VALUE SPACE.                 
003200 77  SW-INFO-FINNS               PIC X       VALUE 'N'.                   
003400 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO COMP-3.           
003500 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
003600 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
003700 77  WS-TIREGDAT-URSP            PIC 9(6)    VALUE ZERO.                  
003800 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
003900 77  WS-TIREGTID-URSP            PIC 9(8)    VALUE ZERO.                  
004000 77  WS-TIREGDAT-AVV             PIC 9(6)    VALUE ZERO.                  
004100 77  WS-TIREGTID-AVV             PIC 9(8)    VALUE ZERO.                  
004200 77  SW-HOPP                     PIC X       VALUE 'N'.                   
004300                                                                          
004310 01  -COPY WWDCKONS                                                       
004400 01  W-CURRENT-DATE              PIC 9(12).                               
004500 01  FILLER REDEFINES W-CURRENT-DATE.                                     
004600     03  W-DATE                  PIC 9(6).                                
004700     03  W-TIME                  PIC 9(6).                                
004800                                                                          
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200     EJECT                                                                
005300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005400     88  EGEN-MID                            VALUE '4277'.                
005500     88  GODK-MID                            VALUE '4227'.                
005600                                                                          
005700 01  TEXT-VORNOT.                                                         
005800     03 VORNOT-RUBRIK            PIC X(8).                                
005900     03 VORNOT-TEXT              PIC X(302).                              
006000                                                                          
006100 01  TEXT-LOSNOT.                                                         
006200     03 LOSNOT-RUBRIK            PIC X(8).                                
006300     03 LOSNOT-TEXT              PIC X(142).                              
006400     EJECT                                                                
006500 01  W-PROG-TO-PROG-SW.                                                   
006600     03  M-SW-LL                 PIC S9(4)   VALUE +80 COMP SYNC.         
006700     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
006800     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W0T501  '.            
006900     03  M-SW-IDTRANS            PIC X(4)    VALUE '4277'.                
007000     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
007100                                                                          
007200 01  GENERELLA-SUBPROGRAM.                                                
007300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007800*01 -COPY WMEDAREA                                                        
007900                                                                          
008000 01  MESSAGE-CODES.                                                       
008100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008400     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008500     EJECT                                                                
008600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W4I27701                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009510*    --- FÖR HOPP TILL 2106-DELIVERY NOTE                                 
009520       05  2106-MID REDEFINES MSG-MID-OUT.                                
009530*          07  -COPY W2I10601 -PRE 2106-                                  
009600     03  MOD REDEFINES MSG-AREA.                                          
009700*      05  -COPY W4O27701                                                 
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010000     SKIP3                                                                
010100*01  -COPY WMFSAREA                                                       
010200     EJECT                                                                
010300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600                                                                          
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-WDA601KY-X.                                                    
010900         05  W-IDDISTR           PIC S9(5)  COMP-3 VALUE ZERO.            
011000         05  W-IDKUNDNR          PIC S9(7)  COMP-3 VALUE ZERO.            
011100         05  W-IDKUNDRF          PIC X(10)  VALUE SPACE.                  
011200         05  W-TIREGDAT-URSP     PIC S9(7)  COMP-3 VALUE ZERO.            
011300         05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.            
011400         05  W-TIREGTID-URSP     PIC S9(9)  COMP-3 VALUE ZERO.            
011500         05  W-TIREGDAT-AVV      PIC S9(7)  COMP-3 VALUE ZERO.            
011600         05  W-TIREGTID-AVV      PIC S9(9)  COMP-3 VALUE ZERO.            
011700                                                                          
011800      03 W-WDP501KY-X.                                                    
011900         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
012000         05  W-IDDOKTYP          PIC X(8)   VALUE SPACE.                  
012100         05  W-IDDOK             PIC X(8)   VALUE SPACE.                  
012200                                                                          
012300      03  W-IDSID-X.                                                      
012400         05  W-IDSID             PIC S9(3)  COMP-3 VALUE ZERO.            
012410                                                                          
012420     03  W-WDD901KY-X.                                                    
012430         05  W-IDARTNR-D9        PIC S9(9)  VALUE ZERO COMP-3.            
012440         05  W-IDDC-D9           PIC X(2)   VALUE SPACE.                  
012450     03  W-IDLEVNR-X.                                                     
012460         05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                  
012470     03  W-IDLEVBSK-X.                                                    
012480         05 W-IDLEVBSK           PIC S9(1)  VALUE ZERO COMP-3.            
012500     EJECT                                                                
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FINNS                       VALUE '  '.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     SKIP3                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300*                                                                         
013400 01  ALL-SSA.                                                             
013410     03 SSA1                     PIC X(64).                               
013500     03 SSA2                     PIC X(64).                               
013510     03 SSA3                     PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNKTIONSKODER                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100                                                                          
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
014300 01  DLI-IO-WDA601.                                                       
014400*    03  -COPY WDA601                                                     
014500     EJECT                                                                
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA611'.                      
014700 01  DLI-IO-WDA611.                                                       
014800*    03  -COPY WDA611                                                     
014900     EJECT                                                                
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
015100 01  DLI-IO-WDA612.                                                       
015200*    03  -COPY WDA612                                                     
015300     EJECT                                                                
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
015500 01  DLI-IO-WDA613.                                                       
015600*    03  -COPY WDA613                                                     
015700     EJECT                                                                
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP501'.                      
015900 01  DLI-IO-WDP501.                                                       
016000*    03  -COPY WDP501                                                     
016100     EJECT                                                                
016200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP512'.                      
016300 01  DLI-IO-WDP512.                                                       
016400*    03  -COPY WDP512                                                     
016410     EJECT                                                                
016420 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
016430 01  DLI-IO-WDD925.                                                       
016440*    03  -COPY WDD925  -PRE D9-                                           
016500     EJECT                                                                
016600 LINKAGE SECTION.                                                         
016700*01  -COPY W0009   -PRE MSG-                                              
016800     EJECT                                                                
016900*01  -COPY W0009   -PRE ALT-                                              
017000     EJECT                                                                
017010*01  -COPY W0009   -PRE 2106-                                             
017020     EJECT                                                                
017100*01  -COPY W0008   -PRE WDA6-                                             
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400*01  -COPY W0008   -PRE WDP5-                                             
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017610*01  -COPY W0008   -PRE WDD9-                                             
017620     05  FILLER                  PIC X.                                   
017630     EJECT                                                                
017700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB 2106-PCB                       
017710                           WDA6-PCB WDP5-PCB WDD9-PCB.                    
017800 MAIN SECTION.                                                            
017900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB 2106-PCB                       
017910                           WDA6-PCB WDP5-PCB WDD9-PCB.                    
018000                                                                          
018100     PERFORM IMS-GET-MSG                                                  
018200     IF SEGMENT-FINNS                                                     
018300        PERFORM A-INIT                                                    
018400        IF EGEN-MID OR GODK-MID                                           
018500           PERFORM B-KOLLA-NYCKLAR                                        
018600           IF MFS-UPDATE                                                  
018700              PERFORM G-KOLLA-INPUT                                       
018800              IF INDATA-OK                                                
018900                 PERFORM H-UPPDATERA                                      
019000                 PERFORM F-LAES-VISA-INFO                                 
019100              END-IF                                                      
019200           ELSE                                                           
019300              IF (MFS-FIRST AND GODK-MID)                                 
019310              OR  MFS-PREVIOUS                                            
019400                 PERFORM F-LAES-VISA-INFO                                 
019410                 IF MFS-PREVIOUS                                          
019420                    PERFORM C-HOPP-TILL-2106                              
019430                    MOVE JA TO SW-HOPP                                    
019440                    PERFORM IMS-INSERT-2106-MSG                           
019450                 END-IF                                                   
019500              ELSE                                                        
019600                 IF EGEN-MID                                              
019700                    PERFORM E-SAMMA-SIDA                                  
019800                 ELSE                                                     
019900                    IF GODK-MID                                           
020000                       PERFORM F-LAES-VISA-INFO                           
020100                    END-IF                                                
020200                 END-IF                                                   
020300              END-IF                                                      
020400           END-IF                                                         
020410           IF SW-HOPP = NEJ                                               
020500              COMPUTE MSG-KVLL = LENGTH OF MOD-W4O27701-CTX + 4           
020600              PERFORM IMS-INSERT-MSG                                      
020610           END-IF                                                         
020700        ELSE                                                              
020800           PERFORM IMS-INSERT-ALT-MSG                                     
020900        END-IF                                                            
021000     END-IF                                                               
021100                                                                          
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021610     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
021700                                                                          
021800     IF MSG-DUBBLA-TRANSKODER                                             
021900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I27701-CTX            
022000        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
022100        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
022200     ELSE                                                                 
022300        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I27701-CTX             
022400        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
022500        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
022600     END-IF                                                               
022700                                                                          
022800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
023000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023100                                                                          
023200     MOVE LOW-VALUE TO MSG-AREA                                           
023300     MOVE 'W4O277N1' TO MFS-IDMOD                                         
023400     MOVE '4277' TO MOD-IDTRANS                                           
023500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023600                                                                          
023700     IF EGEN-MID                                                          
023800        CONTINUE                                                          
023900     ELSE                                                                 
024000        MOVE SPACE TO MFS-KDTRTYP                                         
024100        MOVE '7' TO MFS-IDPFK                                             
024200     END-IF                                                               
024300                                                                          
024400     MOVE FUNCTION CURRENT-DATE (3:12) TO W-CURRENT-DATE                  
024500     .                                                                    
024600     EJECT                                                                
024700 B-KOLLA-NYCKLAR SECTION.                                                 
024710     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
024800                                                                          
024900     MOVE 'SE ' TO MED-IDSKYLT                                            
025000                                                                          
025100     INSPECT MID-IDDISTR REPLACING LEADING SPACE BY ZERO                  
025200     IF MID-IDDISTR NUMERIC                                               
025300        MOVE MID-IDDISTR TO WS-IDDISTR                                    
025400        MOVE WS-IDDISTR TO W-IDDISTR                                      
025500     ELSE                                                                 
025600        MOVE ZERO TO W-IDDISTR                                            
025700                     WS-IDDISTR                                           
025800     END-IF                                                               
025900                                                                          
026000     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
026100     IF MID-IDKUNDNR NUMERIC                                              
026200        MOVE MID-IDKUNDNR TO WS-IDKUNDNR                                  
026300        MOVE WS-IDKUNDNR TO W-IDKUNDNR                                    
026400     ELSE                                                                 
026500        MOVE ZERO TO W-IDKUNDNR                                           
026600                     WS-IDKUNDNR                                          
026700     END-IF                                                               
026800                                                                          
026900     MOVE MID-IDORDNR7 TO W-IDKUNDRF                                      
027000                                                                          
027100     IF MID-TIREGDAT-URSP NUMERIC                                         
027200        MOVE MID-TIREGDAT-URSP TO W-TIREGDAT-URSP                         
027300                                  WS-TIREGDAT-URSP                        
027400     ELSE                                                                 
027500        MOVE ZERO TO W-TIREGDAT-URSP                                      
027600                     WS-TIREGDAT-URSP                                     
027700     END-IF                                                               
027800                                                                          
027900     INSPECT MID-IDARTNR REPLACING LEADING SPACE BY ZERO                  
028000     IF MID-IDARTNR NUMERIC                                               
028100        MOVE MID-IDARTNR TO WS-IDARTNR-NUM                                
028200        MOVE WS-IDARTNR-NUM TO W-IDARTNR                                  
028300     ELSE                                                                 
028400        MOVE ZERO TO W-IDARTNR                                            
028500                     WS-IDARTNR-NUM                                       
028600     END-IF                                                               
028700                                                                          
028800     IF MID-TIREGTID-URSP NUMERIC                                         
028900        MOVE MID-TIREGTID-URSP TO W-TIREGTID-URSP                         
029000                                  WS-TIREGTID-URSP                        
029100     ELSE                                                                 
029200        MOVE ZERO TO W-TIREGTID-URSP                                      
029300                     WS-TIREGTID-URSP                                     
029400     END-IF                                                               
029500                                                                          
029600     IF MID-TIREGDAT-AVV NUMERIC                                          
029700        MOVE MID-TIREGDAT-AVV TO W-TIREGDAT-AVV                           
029800                                 WS-TIREGDAT-AVV                          
029900     ELSE                                                                 
030000        MOVE ZERO TO W-TIREGDAT-AVV                                       
030100                     WS-TIREGDAT-AVV                                      
030200     END-IF                                                               
030300                                                                          
030400     IF MID-TIREGTID-AVV NUMERIC                                          
030500        MOVE MID-TIREGTID-AVV TO W-TIREGTID-AVV                           
030600                                 WS-TIREGTID-AVV                          
030700     ELSE                                                                 
030800        MOVE ZERO TO W-TIREGTID-AVV                                       
030900                     WS-TIREGTID-AVV                                      
031000     END-IF                                                               
031100                                                                          
031200     IF EGEN-MID OR GODK-MID                                              
031300        MOVE WS-IDDISTR       TO MOD-IDDISTR                              
031400        MOVE WS-IDKUNDNR      TO MOD-IDKUNDNR                             
031500        MOVE W-IDKUNDRF       TO MOD-IDORDNR7                             
031600        MOVE WS-TIREGDAT-URSP TO MOD-TIREGDAT-URSP                        
031700        MOVE WS-IDARTNR-NUM   TO MOD-IDARTNR                              
031800        MOVE WS-TIREGTID-URSP TO MOD-TIREGTID-URSP                        
031900        MOVE WS-TIREGDAT-AVV  TO MOD-TIREGDAT-AVV                         
032000        MOVE WS-TIREGTID-AVV  TO MOD-TIREGTID-AVV                         
032100     ELSE                                                                 
032200        PERFORM MFS-RENSA-DOLDA-NYCKLAR                                   
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032510 C-HOPP-TILL-2106 SECTION.                                                
032520     MOVE 'C-HOPP-TILL-2106' TO CURRENT-SECTION                           
032530                                                                          
032540     MOVE ALL '+'        TO 2106-MID                                      
032550     MOVE VOR-IDARTNR    TO WS-IDARTNR-NUM                                
032560     MOVE WS-IDARTNR-NUM TO 2106-MID-IDARTNR-IN                           
032570     MOVE VOR-IDLEVNR    TO 2106-MID-IDLEVNR-IN                           
032580                                                                          
032590     COMPUTE MSG-KVLL = LENGTH OF 2106-MID-W2I10601 + 17                  
032591     MOVE LOW-VALUE      TO MSG-KDZ1                                      
032592                            MSG-KDZ2                                      
032593     MOVE 'W2T106'       TO MSG-KDTRANS-1                                 
032594     MOVE '4275'         TO MSG-IDTRANS-1                                 
032595*    MOVE '2'            TO MSG-KDMFSFOR-1                                
032596     .                                                                    
032597     EJECT                                                                
032600 E-SAMMA-SIDA SECTION.                                                    
032610     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
032700                                                                          
035100     IF MID-TELOSNOT = ALL '+'                                            
035200        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
035300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
035400     ELSE                                                                 
035500        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
035600        CALL WMEDKONV USING MED-WMEDAREA                                  
035700        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
035800        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
035900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
036000        PERFORM EA-MID-INDATA-TILL-MOD                                    
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 EA-MID-INDATA-TILL-MOD SECTION.                                          
036410     MOVE 'EA-MID-TILL-MOD ' TO CURRENT-SECTION                           
036500                                                                          
036600     IF MID-TELOSNOT = ALL '+'                                            
036700        CONTINUE                                                          
036800     ELSE                                                                 
036900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELOSNOT-ATTR                   
037000     END-IF                                                               
037100     .                                                                    
037200     EJECT                                                                
037300 F-LAES-VISA-INFO SECTION.                                                
037310     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
037400                                                                          
037500     MOVE NEJ TO SW-INFO-FINNS                                            
037600                                                                          
037700     PERFORM IMS-GET-WDA601                                               
037800     IF SEGMENT-FINNS                                                     
037900        MOVE VOR-IDDISTR  TO MOD-IDDISTR-UT                               
038000        MOVE VOR-IDKUNDNR TO MOD-IDKUNDNR-UT                              
038100        MOVE VOR-IDKUNDRF TO MOD-IDKUNDRF-UT                              
038200        INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE           
038300        MOVE VOR-IDARTNR  TO MOD-IDARTNR-UT                               
038400        COMPUTE WS-KVANTAL = VOR-KVBEART-Q - VOR-KVPREAVB                 
038500        END-COMPUTE                                                       
038600        MOVE WS-KVANTAL   TO MOD-KVANTAL-UT                               
038700                                                                          
038800        PERFORM FA-LAES-WDA6                                              
038900        PERFORM FB-LAES-WDP5                                              
038910        PERFORM FC-LAES-WDD9                                              
039000                                                                          
039100        IF SW-INFO-FINNS = NEJ                                            
039200           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
039300           CALL WMEDKONV USING MED-WMEDAREA                               
039400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
039500        END-IF                                                            
039600     ELSE                                                                 
039700        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
039800        CALL WMEDKONV USING MED-WMEDAREA                                  
039900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 FA-LAES-WDA6 SECTION.                                                    
040310     MOVE 'FA-LAES-WDA6    ' TO CURRENT-SECTION                           
040400                                                                          
040500     PERFORM IMS-GET-WDA611                                               
040600     IF SEGMENT-FINNS                                                     
040700        MOVE VTI-TEVORINT TO MOD-TEVORINT                                 
040800        MOVE JA TO SW-INFO-FINNS                                          
040900     ELSE                                                                 
041000        MOVE MFS-RENSA-FAELT TO MOD-TEVORINT                              
041100     END-IF                                                               
041200                                                                          
041300     PERFORM IMS-GET-WDA612                                               
041400     IF SEGMENT-FINNS                                                     
041500        MOVE VTE-TEVOREXT TO MOD-TEVOREXT                                 
041600        MOVE JA TO SW-INFO-FINNS                                          
041700     ELSE                                                                 
041800        MOVE MFS-RENSA-FAELT TO MOD-TEVOREXT                              
042000     END-IF                                                               
042100                                                                          
042200     PERFORM IMS-GET-WDA613                                               
042300     IF SEGMENT-FINNS                                                     
042400        MOVE VTS-TEVORSC TO MOD-TEVORSC                                   
042500        MOVE JA TO SW-INFO-FINNS                                          
042600     ELSE                                                                 
042700        MOVE MFS-RENSA-FAELT TO MOD-TEVORSC                               
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100 FB-LAES-WDP5 SECTION.                                                    
043110     MOVE 'FB-LAES-WDP5    ' TO CURRENT-SECTION                           
043200                                                                          
043300     MOVE 'S  '       TO W-IDSKYLT                                        
043400     MOVE +1          TO W-IDSID                                          
043500     MOVE MID-IDARTNR(2:8) TO WS-IDARTNR                                  
043600     INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                   
043700     MOVE WS-IDARTNR  TO W-IDDOK                                          
043800                                                                          
044900     MOVE 'LOSNOT' TO W-IDDOKTYP                                          
045000     PERFORM IMS-GHU-WDP512                                               
045100     IF SEGMENT-FINNS                                                     
045200        MOVE TEXT-TEINFO TO TEXT-LOSNOT                                   
045300        MOVE LOSNOT-TEXT TO MOD-TELOSNOT                                  
045400        MOVE JA TO SW-INFO-FINNS                                          
045500     ELSE                                                                 
045600        MOVE MFS-RENSA-FAELT TO MOD-TELOSNOT                              
045700     END-IF                                                               
045800     .                                                                    
045900     EJECT                                                                
045910 FC-LAES-WDD9 SECTION.                                                    
045920     MOVE 'FC-LAES-WDD9    ' TO CURRENT-SECTION                           
045930                                                                          
046006     MOVE VOR-IDARTNR    TO W-IDARTNR-D9                                  
046007     MOVE WC-CDC-SE      TO W-IDDC-D9                                     
046008     MOVE VOR-IDLEVNR    TO W-IDLEVNR                                     
046009                                                                          
046010     MOVE +2             TO W-IDLEVBSK                                    
046011     PERFORM IMS-GU-WDD925                                                
046012     IF SEGMENT-FINNS                                                     
046013        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(1)                          
046014        MOVE JA TO SW-INFO-FINNS                                          
046015     ELSE                                                                 
046016        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(1)                           
046017     END-IF                                                               
046018                                                                          
046019     MOVE +4             TO W-IDLEVBSK                                    
046020     PERFORM IMS-GU-WDD925                                                
046021     IF SEGMENT-FINNS                                                     
046022        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(2)                          
046023        MOVE JA TO SW-INFO-FINNS                                          
046024     ELSE                                                                 
046025        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(2)                           
046026     END-IF                                                               
046027                                                                          
046028     MOVE +5             TO W-IDLEVBSK                                    
046029     PERFORM IMS-GU-WDD925                                                
046030     IF SEGMENT-FINNS                                                     
046031        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(3)                          
046032        MOVE JA TO SW-INFO-FINNS                                          
046033     ELSE                                                                 
046034        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(3)                           
046035     END-IF                                                               
046036                                                                          
046037     MOVE +6             TO W-IDLEVBSK                                    
046038     PERFORM IMS-GU-WDD925                                                
046039     IF SEGMENT-FINNS                                                     
046040        MOVE D9-INFO-TELEVBSK TO MOD-TEVORNOT(4)                          
046041        MOVE JA TO SW-INFO-FINNS                                          
046042     ELSE                                                                 
046043        MOVE MFS-RENSA-FAELT TO MOD-TEVORNOT(4)                           
046044     END-IF                                                               
046045     .                                                                    
046046     EJECT                                                                
046047 G-KOLLA-INPUT SECTION.                                                   
046050     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
046100                                                                          
048400     PERFORM MFS-ROER-EJ-FAELT-UT                                         
048500     MOVE JA TO INDATA-SW                                                 
048600     IF MID-TELOSNOT = ALL '+'                                            
048700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
048800        CALL WMEDKONV USING MED-WMEDAREA                                  
048900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
049000        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
049100        MOVE NEJ TO INDATA-SW                                             
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 H-UPPDATERA SECTION.                                                     
049510     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
049600                                                                          
055100     IF MID-TELOSNOT NOT = ALL '+'                                        
055200        MOVE 'LOSNOT'    TO W-IDDOKTYP                                    
055300        MOVE 'S  '       TO W-IDSKYLT                                     
055400        MOVE +1          TO W-IDSID                                       
055500        MOVE MID-IDARTNR(2:8) TO WS-IDARTNR                               
055600        INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                
055700        MOVE WS-IDARTNR  TO W-IDDOK                                       
055800                                                                          
055900        PERFORM IMS-GET-WDP501                                            
056000        IF SEGMENT-FINNS                                                  
056100           PERFORM IMS-GHU-WDP512                                         
056200           IF SEGMENT-FINNS                                               
056300              IF MID-TELOSNOT = SPACE                                     
056400                 PERFORM IMS-GET-WDP501                                   
056500                 PERFORM IMS-DLET-WDP501                                  
056600              ELSE                                                        
056700                 MOVE MID-TELOSNOT TO LOSNOT-TEXT                         
056800                 MOVE 'LÖSN   :'   TO LOSNOT-RUBRIK                       
056900                 MOVE TEXT-LOSNOT  TO TEXT-TEINFO                         
057000                 MOVE W-DATE       TO TEXT-TIREGDAT                       
057100                 MOVE W-TIME       TO TEXT-TIREGTID                       
057200                 PERFORM IMS-REPL-WDP512                                  
057300                 MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELOSNOT-ATTR          
057400                 PERFORM IMS-GET-WDP501                                   
057500                 MOVE W-DATE       TO TEXT-TIREGDAT                       
057600                 MOVE W-TIME       TO TEXT-TIREGTID                       
057700                 PERFORM IMS-REPL-WDP501                                  
057800              END-IF                                                      
057900           ELSE                                                           
058000              IF MID-TELOSNOT NOT = SPACE                                 
058100                 PERFORM HB-FIXA-WDP512                                   
058200                 MOVE MID-TELOSNOT TO LOSNOT-TEXT                         
058300                 MOVE TEXT-LOSNOT  TO TEXT-TEINFO                         
058400                 PERFORM IMS-ISRT-WDP512                                  
058500                 MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELOSNOT-ATTR          
058600              END-IF                                                      
058700           END-IF                                                         
058800        ELSE                                                              
058900           IF MID-TELOSNOT NOT = SPACE                                    
059000              PERFORM HA-FIXA-WDP501                                      
059100              MOVE 'LOSNOT' TO INFO-IDDOKTYP                              
059200              PERFORM IMS-ISRT-WDP501                                     
059300              PERFORM HB-FIXA-WDP512                                      
059400              MOVE MID-TELOSNOT TO LOSNOT-TEXT                            
059500              MOVE 'LÖSN   :'   TO LOSNOT-RUBRIK                          
059600              MOVE TEXT-LOSNOT  TO TEXT-TEINFO                            
059700              PERFORM IMS-ISRT-WDP512                                     
059800              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELOSNOT-ATTR             
059900           END-IF                                                         
060000        END-IF                                                            
060100     ELSE                                                                 
060200        MOVE MFS-ROER-EJ-FAELT TO MOD-TELOSNOT                            
060300     END-IF                                                               
060400                                                                          
060500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
060600     CALL WMEDKONV USING MED-WMEDAREA                                     
060700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
060800     .                                                                    
060900     EJECT                                                                
061000 HA-FIXA-WDP501 SECTION.                                                  
061010     MOVE 'HA-FIXA-WDP501  ' TO CURRENT-SECTION                           
061100                                                                          
061200     MOVE 'S  '             TO INFO-IDSKYLT                               
061300     MOVE MID-IDARTNR(2:8)  TO WS-IDARTNR                                 
061400     INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                   
061500     MOVE WS-IDARTNR        TO INFO-IDDOK                                 
061600     MOVE W-DATE            TO INFO-TIREGDAT                              
061700     MOVE W-TIME            TO INFO-TIREGTID                              
061800     MOVE MSG-SIGNON-USERID TO INFO-IDUSER                                
061900     MOVE SPACE             TO INFO-BEDOK                                 
062000     MOVE LOW-VALUE         TO INFO-FILLER                                
062100     .                                                                    
062200     EJECT                                                                
062300 HB-FIXA-WDP512 SECTION.                                                  
062310     MOVE 'HB-FIXA-WDP512  ' TO CURRENT-SECTION                           
062400                                                                          
062500     MOVE +1                TO TEXT-IDSID                                 
062600     MOVE MSG-SIGNON-USERID TO TEXT-IDUSER                                
062700     MOVE W-DATE            TO TEXT-TIREGDAT                              
062800     MOVE W-TIME            TO TEXT-TIREGTID                              
062900     MOVE 'LÖSN   :'        TO LOSNOT-RUBRIK                              
063000     MOVE MID-TELOSNOT      TO LOSNOT-TEXT                                
063100     MOVE TEXT-LOSNOT       TO TEXT-TEINFO                                
063200     .                                                                    
063300     EJECT                                                                
064500 MFS-RENSA-DOLDA-NYCKLAR SECTION.                                         
064600                                                                          
064700     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR                                  
064800                             MOD-IDKUNDNR                                 
064900                             MOD-IDORDNR7                                 
065000                             MOD-TIREGDAT-URSP                            
065100                             MOD-IDARTNR                                  
065200                             MOD-TIREGTID-URSP                            
065300                             MOD-TIREGDAT-AVV                             
065400                             MOD-TIREGTID-AVV                             
065500     .                                                                    
065600     SKIP3                                                                
065700 MFS-ROER-EJ-FAELT-IN-UT SECTION.                                         
065800                                                                          
065900     MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORINT                               
066000                               MOD-TEVOREXT                               
066100                               MOD-TEVORSC                                
066200                               MOD-TEVORNOT(1)                            
066210                               MOD-TEVORNOT(2)                            
066220                               MOD-TEVORNOT(3)                            
066230                               MOD-TEVORNOT(4)                            
066300                               MOD-TELOSNOT                               
066400     .                                                                    
066500     EJECT                                                                
066600 MFS-ROER-EJ-FAELT-UT SECTION.                                            
066700                                                                          
066800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                             
066900                               MOD-IDKUNDNR-UT                            
067000                               MOD-IDKUNDRF-UT                            
067100                               MOD-IDARTNR-UT                             
067200                               MOD-KVANTAL-UT                             
067300     .                                                                    
067400     EJECT                                                                
067500* --- IMS SEKTIONER ---                                                   
067600     SKIP3                                                                
067700 IMS-GET-MSG SECTION.                                                     
067800     MOVE '  QC' TO GODK-STATUSKODER                                      
067900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
068000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068100     PERFORM IMS-STATUSKONTROLL                                           
068200     .                                                                    
068300     SKIP3                                                                
068400 IMS-INSERT-MSG SECTION.                                                  
068500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
068600     MOVE SPACE TO GODK-STATUSKODER                                       
068700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
068800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100     SKIP3                                                                
069200 IMS-INSERT-ALT-MSG SECTION.                                              
069300     MOVE SPACE TO GODK-STATUSKODER                                       
069400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
069500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
069600     PERFORM IMS-STATUSKONTROLL                                           
069700     .                                                                    
069800     EJECT                                                                
069810 IMS-INSERT-2106-MSG SECTION.                                             
069820     MOVE SPACE TO GODK-STATUSKODER                                       
069830     CALL CBLTDLI USING ISRT 2106-PCB MSG-IO-AREA                         
069840     MOVE 2106-STATUS-CODE TO STATUS-WS                                   
069850     PERFORM IMS-STATUSKONTROLL                                           
069860     .                                                                    
069870     EJECT                                                                
069900 IMS-GET-WDA601 SECTION.                                                  
069910     MOVE 'GET-WDA601      ' TO CURRENT-IMS-SECTION                       
069920                                                                          
069930     MOVE SPACE               TO ALL-SSA                                  
070000     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
070100          DELIMITED BY SIZE INTO SSA1                                     
070200     MOVE '  GE' TO GODK-STATUSKODER                                      
070300     CALL CBLTDLI USING GU WDA6-PCB DLI-IO-WDA601 SSA1                    
070400     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     .                                                                    
070700     SKIP3                                                                
070800 IMS-GET-WDA611 SECTION.                                                  
070810     MOVE 'GET-WDA611      ' TO CURRENT-IMS-SECTION                       
070820                                                                          
070830     MOVE SPACE               TO ALL-SSA                                  
070900     MOVE 'WDA611  ' TO SSA1                                              
071000     MOVE '  GE' TO GODK-STATUSKODER                                      
071100     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA611 SSA1                   
071200     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
071300     PERFORM IMS-STATUSKONTROLL                                           
071400     .                                                                    
071500     SKIP3                                                                
071600 IMS-GET-WDA612 SECTION.                                                  
071610     MOVE 'GET-WDA612      ' TO CURRENT-IMS-SECTION                       
071620                                                                          
071630     MOVE SPACE               TO ALL-SSA                                  
071700     MOVE 'WDA612  ' TO SSA1                                              
071800     MOVE '  GE' TO GODK-STATUSKODER                                      
071900     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA612 SSA1                   
072000     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
072100     PERFORM IMS-STATUSKONTROLL                                           
072200     .                                                                    
072300     EJECT                                                                
072400 IMS-GET-WDA613 SECTION.                                                  
072410     MOVE 'GET-WDA613      ' TO CURRENT-IMS-SECTION                       
072420                                                                          
072430     MOVE SPACE               TO ALL-SSA                                  
072500     MOVE 'WDA613  ' TO SSA1                                              
072600     MOVE '  GE' TO GODK-STATUSKODER                                      
072700     CALL CBLTDLI USING GHNP WDA6-PCB DLI-IO-WDA613 SSA1                  
072800     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
072900     PERFORM IMS-STATUSKONTROLL                                           
073000     .                                                                    
073100     SKIP3                                                                
073200 IMS-GET-WDP501 SECTION.                                                  
073210     MOVE 'GET-WDP501      ' TO CURRENT-IMS-SECTION                       
073220                                                                          
073230     MOVE SPACE               TO ALL-SSA                                  
073300     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
073400          DELIMITED BY SIZE INTO SSA1                                     
073500     MOVE '  GE' TO GODK-STATUSKODER                                      
073600     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-WDP501 SSA1                   
073700     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
073800     PERFORM IMS-STATUSKONTROLL                                           
073900     .                                                                    
074000     SKIP3                                                                
074100 IMS-ISRT-WDP501 SECTION.                                                 
074110     MOVE 'ISRT-WDP501     ' TO CURRENT-IMS-SECTION                       
074120                                                                          
074130     MOVE SPACE               TO ALL-SSA                                  
074200     MOVE 'WDP501 ' TO SSA1                                               
074300     MOVE '  ' TO GODK-STATUSKODER                                        
074400     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-WDP501 SSA1                  
074500     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     EJECT                                                                
074900 IMS-GHU-WDP512 SECTION.                                                  
074910     MOVE 'GHU-WDP512      ' TO CURRENT-IMS-SECTION                       
074920                                                                          
074930     MOVE SPACE               TO ALL-SSA                                  
075000     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
075100          DELIMITED BY SIZE INTO SSA1                                     
075200     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
075300          DELIMITED BY SIZE INTO SSA2                                     
075400     MOVE '  GE' TO GODK-STATUSKODER                                      
075500     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-WDP512 SSA1 SSA2              
075600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     SKIP3                                                                
076000 IMS-ISRT-WDP512 SECTION.                                                 
076010     MOVE 'ISRT-WDP512     ' TO CURRENT-IMS-SECTION                       
076020                                                                          
076030     MOVE SPACE               TO ALL-SSA                                  
076100     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
076200          DELIMITED BY SIZE INTO SSA1                                     
076300     MOVE 'WDP512 ' TO SSA2                                               
076400     MOVE '  ' TO GODK-STATUSKODER                                        
076500     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-WDP512 SSA1 SSA2             
076600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
076700     PERFORM IMS-STATUSKONTROLL                                           
076800     .                                                                    
076900     SKIP3                                                                
077000 IMS-REPL-WDP512 SECTION.                                                 
077010     MOVE 'REPL-WDP512     ' TO CURRENT-IMS-SECTION                       
077020                                                                          
077030     MOVE SPACE               TO ALL-SSA                                  
077100     MOVE '  ' TO GODK-STATUSKODER                                        
077200     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-WDP512                       
077300     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
077400     PERFORM IMS-STATUSKONTROLL                                           
077500     .                                                                    
077600     EJECT                                                                
077700 IMS-REPL-WDP501 SECTION.                                                 
077710     MOVE 'REPL-WDP501     ' TO CURRENT-IMS-SECTION                       
077720                                                                          
077730     MOVE SPACE               TO ALL-SSA                                  
077800     MOVE '  ' TO GODK-STATUSKODER                                        
077900     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-WDP501                       
078000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
078100     PERFORM IMS-STATUSKONTROLL                                           
078200     .                                                                    
078300     SKIP3                                                                
078400 IMS-DLET-WDP501 SECTION.                                                 
078410     MOVE 'REPL-WDP501     ' TO CURRENT-IMS-SECTION                       
078420                                                                          
078430     MOVE SPACE               TO ALL-SSA                                  
078500     MOVE '  ' TO GODK-STATUSKODER                                        
078600     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-WDP501                       
078700     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000     SKIP3                                                                
079010 IMS-GU-WDD925  SECTION.                                                  
079020     MOVE 'GU-WDD925       ' TO CURRENT-IMS-SECTION                       
079030                                                                          
079040     MOVE SPACE               TO ALL-SSA                                  
079050     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
079060            DELIMITED BY SIZE INTO SSA1                                   
079070     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
079080            DELIMITED BY SIZE INTO SSA2                                   
079090     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
079091            DELIMITED BY SIZE INTO SSA3                                   
079092     MOVE '  GE' TO GODK-STATUSKODER                                      
079093     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3          
079094     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
079095     PERFORM IMS-STATUSKONTROLL                                           
079096     .                                                                    
079097     SKIP3                                                                
079100 IMS-STATUSKONTROLL SECTION.                                              
079200     SET STATUS-IX TO 1                                                   
079300     SEARCH GODK-STATUS                                                   
079400       AT END                                                             
079500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
079600         DELIMITED BY SIZE INTO FELTEXT                                   
079700         CALL FELLOG                                                      
079800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079900         CONTINUE                                                         
080000     END-SEARCH                                                           
080100     .                                                                    
