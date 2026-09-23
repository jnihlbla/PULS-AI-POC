000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4027600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   MARS 2003                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET UPPDATERAR VOR MESSAGES.                              
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDA6                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T276                                              
001500*        MID:         W4I27601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O27601                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  IDPGM                       PIC X(08)   VALUE 'W4027600'.            
002800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  SW-INFO-FINNS               PIC X       VALUE 'N'.                   
003300 77  SW-TESC                     PIC X       VALUE 'N'.                   
003310 77  SW-TEDEL                    PIC X       VALUE 'N'.                   
003400 77  SW-FLLAEST                  PIC X       VALUE 'N'.                   
003600 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO COMP-3.           
003700 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
003800 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
003900 77  WS-TIREGDAT-URSP            PIC 9(6)    VALUE ZERO.                  
004000 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
004100 77  WS-TIREGTID-URSP            PIC 9(8)    VALUE ZERO.                  
004200 77  WS-TIREGDAT-AVV             PIC 9(6)    VALUE ZERO.                  
004300 77  WS-TIREGTID-AVV             PIC 9(8)    VALUE ZERO.                  
004900                                                                          
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005001     88  INDATA-OK                           VALUE 'J'.                   
005002     88  INDATA-FEL                          VALUE 'N'.                   
005003                                                                          
005004 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005005     88  EGEN-MID                            VALUE '4276'.                
005006     88  GODK-MID                            VALUE '4226'.                
005008     EJECT                                                                
005500 01  GENERELLA-SUBPROGRAM.                                                
005600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     SKIP3                                                                
005910 01  W-PROG-TO-PROG-SW.                                                   
005920     03  M-SW-LL                 PIC S9(4)   VALUE +80 COMP SYNC.         
005930     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
005940     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W0T501  '.            
005950     03  M-SW-IDTRANS            PIC X(4)    VALUE '4276'.                
005960     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
005970     EJECT                                                                
006000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006100*01 -COPY WMEDAREA                                                        
006200                                                                          
006300 01  MESSAGE-CODES.                                                       
006400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006700     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
006800     EJECT                                                                
006900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007000                                                                          
007100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007200     SKIP3                                                                
007300*01  MID -COPY W4I27601                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
007600     SKIP3                                                                
007700*01  -COPY WMSGAREA                                                       
007800     EJECT                                                                
007900     03  MOD REDEFINES MSG-AREA.                                          
008000*      05  -COPY W4O27601                                                 
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008300     SKIP3                                                                
008400*01  -COPY WMFSAREA                                                       
008500     EJECT                                                                
008600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900                                                                          
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-WDA601KY-X.                                                    
009200         05  W-IDDISTR           PIC S9(5)  COMP-3 VALUE ZERO.            
009300         05  W-IDKUNDNR          PIC S9(7)  COMP-3 VALUE ZERO.            
009400         05  W-IDKUNDRF          PIC X(10)  VALUE SPACE.                  
009500         05  W-TIREGDAT-URSP     PIC S9(7)  COMP-3 VALUE ZERO.            
009600         05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.            
009700         05  W-TIREGTID-URSP     PIC S9(9)  COMP-3 VALUE ZERO.            
009800         05  W-TIREGDAT-AVV      PIC S9(7)  COMP-3 VALUE ZERO.            
009900         05  W-TIREGTID-AVV      PIC S9(9)  COMP-3 VALUE ZERO.            
010000                                                                          
010800     EJECT                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP3                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600*                                                                         
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400                                                                          
012500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
012600 01  DLI-IO-WDA601.                                                       
012700*    03  -COPY WDA601                                                     
012800     EJECT                                                                
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
013400 01  DLI-IO-WDA612.                                                       
013500*    03  -COPY WDA612                                                     
013600     EJECT                                                                
013610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
013611 01  DLI-IO-WDA613.                                                       
013612*    03  -COPY WDA613                                                     
013613     EJECT                                                                
013614 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA614'.                      
013615 01  DLI-IO-WDA614.                                                       
013616*    03  -COPY WDA614                                                     
013617     EJECT                                                                
013630     EJECT                                                                
013640 LINKAGE SECTION.                                                         
013650*01  -COPY W0009   -PRE MSG-                                              
013660     EJECT                                                                
013670*01  -COPY W0009   -PRE ALT-                                              
013680     EJECT                                                                
013690*01  -COPY W0008   -PRE WDA6-                                             
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
014200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDA6-PCB.                      
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDA6-PCB.                      
014500                                                                          
014600     PERFORM IMS-GET-MSG                                                  
014700     IF SEGMENT-FINNS                                                     
014800        PERFORM A-INIT                                                    
014900        IF EGEN-MID OR GODK-MID                                           
015000           PERFORM B-KOLLA-NYCKLAR                                        
015100           IF MFS-UPDATE                                                  
015200              PERFORM G-KOLLA-INPUT                                       
015300              IF INDATA-OK                                                
015400                 PERFORM H-UPPDATERA                                      
015401                 PERFORM F-LAES-VISA-INFO                                 
015402              END-IF                                                      
015403           ELSE                                                           
015408              IF MFS-FIRST AND GODK-MID                                   
015409                 PERFORM F-LAES-VISA-INFO                                 
015410              ELSE                                                        
015411                 IF EGEN-MID                                              
015412                    PERFORM E-SAMMA-SIDA                                  
015413                 ELSE                                                     
015414                    IF GODK-MID                                           
015415                       PERFORM F-LAES-VISA-INFO                           
015416                    END-IF                                                
015418                 END-IF                                                   
015419              END-IF                                                      
015420           END-IF                                                         
015440           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O27601-CTX + 4              
015450           PERFORM IMS-INSERT-MSG                                         
015470        ELSE                                                              
015471           PERFORM IMS-INSERT-ALT-MSG                                     
015472        END-IF                                                            
015485     END-IF                                                               
015490                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000                                                                          
016100     IF MSG-DUBBLA-TRANSKODER                                             
016200        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I27601-CTX            
016300        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
016400        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
016500     ELSE                                                                 
016600        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I27601-CTX             
016700        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
016800        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
016900     END-IF                                                               
017000                                                                          
017100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017400                                                                          
017500     MOVE LOW-VALUE TO MSG-AREA                                           
017600     MOVE 'W4O276N1' TO MFS-IDMOD                                         
017700     MOVE '4276' TO MOD-IDTRANS                                           
017800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
017900                                                                          
018000     IF EGEN-MID                                                          
018100        CONTINUE                                                          
018200     ELSE                                                                 
018300        MOVE SPACE TO MFS-KDTRTYP                                         
018400        MOVE '7' TO MFS-IDPFK                                             
018500     END-IF                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 B-KOLLA-NYCKLAR SECTION.                                                 
019200                                                                          
019300     MOVE 'SE ' TO MED-IDSKYLT                                            
019400                                                                          
019500     INSPECT MID-IDDISTR REPLACING LEADING SPACE BY ZERO                  
019600     IF MID-IDDISTR NUMERIC                                               
019700        MOVE MID-IDDISTR TO WS-IDDISTR                                    
019800        MOVE WS-IDDISTR TO W-IDDISTR                                      
019900     ELSE                                                                 
020000        MOVE ZERO TO W-IDDISTR                                            
020100                     WS-IDDISTR                                           
020200     END-IF                                                               
020300                                                                          
020400     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
020500     IF MID-IDKUNDNR NUMERIC                                              
020600        MOVE MID-IDKUNDNR TO WS-IDKUNDNR                                  
020700        MOVE WS-IDKUNDNR TO W-IDKUNDNR                                    
020800     ELSE                                                                 
020900        MOVE ZERO TO W-IDKUNDNR                                           
021000                     WS-IDKUNDNR                                          
021100     END-IF                                                               
021200                                                                          
021300     MOVE MID-IDORDNR7 TO W-IDKUNDRF                                      
021400                                                                          
021500     IF MID-TIREGDAT-URSP NUMERIC                                         
021600        MOVE MID-TIREGDAT-URSP TO W-TIREGDAT-URSP                         
021700                                  WS-TIREGDAT-URSP                        
021800     ELSE                                                                 
021900        MOVE ZERO TO W-TIREGDAT-URSP                                      
022000                     WS-TIREGDAT-URSP                                     
022010     END-IF                                                               
022020                                                                          
022030     INSPECT MID-IDARTNR REPLACING LEADING SPACE BY ZERO                  
022031     IF MID-IDARTNR NUMERIC                                               
022032        MOVE MID-IDARTNR TO WS-IDARTNR-NUM                                
022033        MOVE WS-IDARTNR-NUM TO W-IDARTNR                                  
022034     ELSE                                                                 
022035        MOVE ZERO TO W-IDARTNR                                            
022036                     WS-IDARTNR-NUM                                       
022037     END-IF                                                               
022038                                                                          
022039     IF MID-TIREGTID-URSP NUMERIC                                         
022040        MOVE MID-TIREGTID-URSP TO W-TIREGTID-URSP                         
022041                                  WS-TIREGTID-URSP                        
022042     ELSE                                                                 
022043        MOVE ZERO TO W-TIREGTID-URSP                                      
022044                     WS-TIREGTID-URSP                                     
022045     END-IF                                                               
022046                                                                          
022047     IF MID-TIREGDAT-AVV NUMERIC                                          
022048        MOVE MID-TIREGDAT-AVV TO W-TIREGDAT-AVV                           
022049                                 WS-TIREGDAT-AVV                          
022050     ELSE                                                                 
022051        MOVE ZERO TO W-TIREGDAT-AVV                                       
022052                     WS-TIREGDAT-AVV                                      
022053     END-IF                                                               
022054                                                                          
022055     IF MID-TIREGTID-AVV NUMERIC                                          
022056        MOVE MID-TIREGTID-AVV TO W-TIREGTID-AVV                           
022057                                 WS-TIREGTID-AVV                          
022058     ELSE                                                                 
022059        MOVE ZERO TO W-TIREGTID-AVV                                       
022060                     WS-TIREGTID-AVV                                      
022070     END-IF                                                               
022080                                                                          
022090     IF EGEN-MID OR GODK-MID                                              
022100        MOVE WS-IDDISTR       TO MOD-IDDISTR                              
022101        MOVE WS-IDKUNDNR      TO MOD-IDKUNDNR                             
022102        MOVE W-IDKUNDRF       TO MOD-IDORDNR7                             
022103        MOVE WS-TIREGDAT-URSP TO MOD-TIREGDAT-URSP                        
022104        MOVE WS-IDARTNR-NUM   TO MOD-IDARTNR                              
022105        MOVE WS-TIREGTID-URSP TO MOD-TIREGTID-URSP                        
022106        MOVE WS-TIREGDAT-AVV  TO MOD-TIREGDAT-AVV                         
022107        MOVE WS-TIREGTID-AVV  TO MOD-TIREGTID-AVV                         
022108     ELSE                                                                 
022109        PERFORM MFS-RENSA-DOLDA-NYCKLAR                                   
022110     END-IF                                                               
022120     .                                                                    
022130     EJECT                                                                
022140 E-SAMMA-SIDA SECTION.                                                    
022150                                                                          
022160     MOVE NEJ TO SW-FLLAEST                                               
022170     IF MID-FLLAEST = 'J' OR 'Y' OR 'N'                                   
022180        MOVE JA TO SW-FLLAEST                                             
022190        MOVE MFS-ROER-EJ-FAELT TO MOD-FLLAEST                             
022200     ELSE                                                                 
022300        MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                               
022400     END-IF                                                               
022500                                                                          
022600     MOVE NEJ TO SW-TESC                                                  
022700     PERFORM IMS-GHU-WDA613                                               
022800     IF SEGMENT-FINNS                                                     
022900        IF VTS-TEVORSC NOT = MID-TEVORSC                                  
023000           MOVE JA TO SW-TESC                                             
023100        END-IF                                                            
023200     ELSE                                                                 
023300        IF MID-TEVORSC = SPACE OR LOW-VALUE                               
023310           CONTINUE                                                       
023320        ELSE                                                              
023321           MOVE JA TO SW-TESC                                             
023322        END-IF                                                            
023323     END-IF                                                               
023324                                                                          
023325     MOVE NEJ TO SW-TEDEL                                                 
023326     PERFORM IMS-GHU-WDA614                                               
023327     IF SEGMENT-FINNS                                                     
023328        IF VTD-TEVORDEL NOT = MID-TEVORDEL                                
023329           MOVE JA TO SW-TEDEL                                            
023330        END-IF                                                            
023331     ELSE                                                                 
023332        IF MID-TEVORDEL = SPACE OR LOW-VALUE                              
023333           CONTINUE                                                       
023334        ELSE                                                              
023335           MOVE JA TO SW-TEDEL                                            
023336        END-IF                                                            
023337     END-IF                                                               
023338                                                                          
023339     IF (SW-FLLAEST = NEJ) AND (SW-TESC = NEJ)                            
023340     AND (SW-TEDEL = NEJ)                                                 
023341        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
023342        PERFORM MFS-ROER-EJ-FAELT-UT                                      
023343     ELSE                                                                 
023344        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
023345        CALL WMEDKONV USING MED-WMEDAREA                                  
023346        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
023347        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
023348        PERFORM MFS-ROER-EJ-FAELT-UT                                      
023349        PERFORM EA-MID-INDATA-TILL-MOD                                    
023350     END-IF                                                               
023351     .                                                                    
023352     EJECT                                                                
023353 EA-MID-INDATA-TILL-MOD SECTION.                                          
023354                                                                          
023355     IF SW-FLLAEST = JA                                                   
023356        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLAEST-ATTR                    
023357     ELSE                                                                 
023358        MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                               
023359     END-IF                                                               
023370     .                                                                    
023371     EJECT                                                                
023372 F-LAES-VISA-INFO SECTION.                                                
023373                                                                          
023374     MOVE NEJ TO SW-INFO-FINNS                                            
023375                                                                          
023380     PERFORM IMS-GET-WDA601                                               
023381     IF SEGMENT-SAKNAS                                                    
023382        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
023383        CALL WMEDKONV USING MED-WMEDAREA                                  
023384        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023385     ELSE                                                                 
023390        MOVE VOR-IDDISTR  TO MOD-IDDISTR-UT                               
023400        MOVE VOR-IDKUNDNR TO MOD-IDKUNDNR-UT                              
023500        MOVE VOR-IDKUNDRF TO MOD-IDKUNDRF-UT                              
023510        INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE           
023520        MOVE VOR-IDARTNR  TO MOD-IDARTNR-UT                               
023530        COMPUTE WS-KVANTAL = VOR-KVBEART-Q - VOR-KVPREAVB                 
023540        END-COMPUTE                                                       
023550        MOVE WS-KVANTAL   TO MOD-KVANTAL-UT                               
023560                                                                          
023580        PERFORM IMS-GET-WDA612                                            
023581        IF SEGMENT-FINNS                                                  
023582           MOVE VTE-TEVOREXT TO MOD-TEVOREXT                              
023583           MOVE JA TO SW-INFO-FINNS                                       
023584        ELSE                                                              
023585           MOVE MFS-RENSA-FAELT TO MOD-TEVOREXT                           
023586        END-IF                                                            
023587                                                                          
023588        PERFORM IMS-GET-WDA613                                            
023589        IF SEGMENT-FINNS                                                  
023590           MOVE VTS-TEVORSC TO MOD-TEVORSC                                
023591           MOVE JA TO SW-INFO-FINNS                                       
023592        ELSE                                                              
023593           MOVE MFS-RENSA-FAELT TO MOD-TEVORSC                            
023594        END-IF                                                            
023596                                                                          
023597        PERFORM IMS-GET-WDA614                                            
023598        IF SEGMENT-FINNS                                                  
023599           MOVE VTD-TEVORDEL TO MOD-TEVORDEL                              
023600           MOVE JA TO SW-INFO-FINNS                                       
023601        ELSE                                                              
023602           MOVE MFS-RENSA-FAELT TO MOD-TEVORDEL                           
023603        END-IF                                                            
023604                                                                          
023605        IF SW-INFO-FINNS = NEJ                                            
023610           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
023620           CALL WMEDKONV USING MED-WMEDAREA                               
023621           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
023622        END-IF                                                            
023623     END-IF                                                               
023624     .                                                                    
023625     EJECT                                                                
024616 G-KOLLA-INPUT SECTION.                                                   
024625                                                                          
024626     MOVE NEJ TO SW-FLLAEST                                               
024627     IF MID-FLLAEST = 'J' OR 'Y' OR 'N'                                   
024628        MOVE 'J' TO SW-FLLAEST                                            
024630        MOVE MFS-ROER-EJ-FAELT TO MOD-FLLAEST                             
024640     ELSE                                                                 
024650        MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                               
024660     END-IF                                                               
024670                                                                          
024680     MOVE NEJ TO SW-TESC                                                  
024690     PERFORM IMS-GHU-WDA613                                               
024700     IF SEGMENT-FINNS                                                     
024800        IF VTS-TEVORSC NOT = MID-TEVORSC                                  
024900           MOVE JA TO SW-TESC                                             
025000        END-IF                                                            
025100     ELSE                                                                 
025200        IF MID-TEVORSC = SPACE OR LOW-VALUE                               
025300           CONTINUE                                                       
025400        ELSE                                                              
025500           MOVE JA TO SW-TESC                                             
025600        END-IF                                                            
025610     END-IF                                                               
025620                                                                          
025621     MOVE NEJ TO SW-TEDEL                                                 
025622     PERFORM IMS-GHU-WDA614                                               
025623     IF SEGMENT-FINNS                                                     
025624        IF VTD-TEVORDEL NOT = MID-TEVORDEL                                
025625           MOVE JA TO SW-TEDEL                                            
025626        END-IF                                                            
025627     ELSE                                                                 
025628        IF MID-TEVORDEL = SPACE OR LOW-VALUE                              
025629           CONTINUE                                                       
025630        ELSE                                                              
025631           MOVE JA TO SW-TEDEL                                            
025632        END-IF                                                            
025633     END-IF                                                               
025634                                                                          
025636     PERFORM MFS-ROER-EJ-FAELT-UT                                         
025637     MOVE JA TO INDATA-SW                                                 
025638     IF (SW-FLLAEST = NEJ) AND (SW-TESC = NEJ)                            
025639     AND (SW-TEDEL = NEJ)                                                 
025640        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
025641        CALL WMEDKONV USING MED-WMEDAREA                                  
025642        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
025643        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
025644        MOVE NEJ TO INDATA-SW                                             
025645     END-IF                                                               
025646                                                                          
025647     PERFORM IMS-GET-WDA601                                               
025648     IF SEGMENT-SAKNAS                                                    
025649        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
025650        CALL WMEDKONV USING MED-WMEDAREA                                  
025651        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
025652        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
025653        MOVE NEJ TO INDATA-SW                                             
025654     END-IF                                                               
025655     .                                                                    
025656     EJECT                                                                
025657 H-UPPDATERA SECTION.                                                     
025660                                                                          
025670     IF MID-FLLAEST = 'J' OR 'Y' OR 'N'                                   
025680        PERFORM IMS-GHU-WDA612                                            
025681        IF SEGMENT-FINNS                                                  
025682           IF MID-FLLAEST = 'J' OR 'Y'                                    
025683              MOVE 'J' TO VTE-FLLAEST                                     
025684           ELSE                                                           
025685              MOVE 'N' TO VTE-FLLAEST                                     
025686           END-IF                                                         
025688           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLLAEST-ATTR                 
025689           PERFORM IMS-REPL-WDA612                                        
025690        ELSE                                                              
025691           MOVE MFS-RENSA-FAELT TO MOD-FLLAEST                            
025692        END-IF                                                            
025693     END-IF                                                               
025700                                                                          
025720     IF SW-TESC = JA                                                      
025721        PERFORM IMS-GHU-WDA613                                            
025722        IF SEGMENT-FINNS                                                  
025723           IF MID-TEVORSC = SPACE                                         
025724              PERFORM IMS-DLET-WDA613                                     
025725           ELSE                                                           
025726              MOVE MID-TEVORSC TO VTS-TEVORSC                             
025727              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORSC-ATTR              
025728              MOVE 'N'         TO VTS-FLLAEST                             
025729              PERFORM IMS-REPL-WDA613                                     
025730           END-IF                                                         
025731        ELSE                                                              
025732           MOVE '1'          TO VTS-KDSEGKEY                              
025733           MOVE 'N'          TO VTS-FLLAEST                               
025734           MOVE MID-TEVORSC  TO VTS-TEVORSC                               
025735           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORSC-ATTR                 
025736           PERFORM IMS-ISRT-WDA613                                        
025737        END-IF                                                            
025738     ELSE                                                                 
025739        MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORSC                             
025740     END-IF                                                               
025741                                                                          
025750     IF SW-TEDEL = JA                                                     
025760        PERFORM IMS-GHU-WDA614                                            
025770        IF SEGMENT-FINNS                                                  
025780           IF MID-TEVORDEL = SPACE                                        
025790              PERFORM IMS-DLET-WDA614                                     
025791           ELSE                                                           
025792              MOVE MID-TEVORDEL TO VTD-TEVORDEL                           
025793              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORDEL-ATTR             
025794              PERFORM IMS-REPL-WDA614                                     
025795           END-IF                                                         
025796        ELSE                                                              
025797           MOVE '1'          TO VTD-KDSEGKEY                              
025799           MOVE MID-TEVORDEL TO VTD-TEVORDEL                              
025800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORDEL-ATTR                
025801           PERFORM IMS-ISRT-WDA614                                        
025802        END-IF                                                            
025803     ELSE                                                                 
025804        MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORDEL                            
025805     END-IF                                                               
025807                                                                          
025808     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025809     CALL WMEDKONV USING MED-WMEDAREA                                     
025810     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
025811     .                                                                    
025812     EJECT                                                                
025844 MFS-RENSA-DOLDA-NYCKLAR SECTION.                                         
025845                                                                          
025846     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR                                  
025847                             MOD-IDKUNDNR                                 
025848                             MOD-IDORDNR7                                 
025849                             MOD-TIREGDAT-URSP                            
025850                             MOD-IDARTNR                                  
025860                             MOD-TIREGTID-URSP                            
025870                             MOD-TIREGDAT-AVV                             
025880                             MOD-TIREGTID-AVV                             
025890     .                                                                    
025900     SKIP3                                                                
026000 MFS-ROER-EJ-FAELT-IN-UT SECTION.                                         
026100                                                                          
026200     MOVE MFS-ROER-EJ-FAELT TO MOD-TEVOREXT                               
026400                               MOD-TEVORSC                                
026600                               MOD-TEVORDEL                               
026700     .                                                                    
026800     EJECT                                                                
026810 MFS-ROER-EJ-FAELT-UT SECTION.                                            
026820                                                                          
026830     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                             
026851                               MOD-IDKUNDNR-UT                            
026852                               MOD-IDKUNDRF-UT                            
026853                               MOD-IDARTNR-UT                             
026854                               MOD-KVANTAL-UT                             
026860     .                                                                    
026870     EJECT                                                                
026900* --- IMS SEKTIONER ---                                                   
027000     SKIP3                                                                
027100 IMS-GET-MSG SECTION.                                                     
027200     MOVE '  QC' TO GODK-STATUSKODER                                      
027300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
027400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027500     PERFORM IMS-STATUSKONTROLL                                           
027600     .                                                                    
027700     SKIP3                                                                
027800 IMS-INSERT-MSG SECTION.                                                  
028000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
028100     MOVE SPACE TO GODK-STATUSKODER                                       
028200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
028300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028400     PERFORM IMS-STATUSKONTROLL                                           
028500     .                                                                    
028600     SKIP3                                                                
028700 IMS-INSERT-ALT-MSG SECTION.                                              
028800     MOVE SPACE TO GODK-STATUSKODER                                       
028900     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
029000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
029100     PERFORM IMS-STATUSKONTROLL                                           
029200     .                                                                    
029300     EJECT                                                                
029400 IMS-GET-WDA601 SECTION.                                                  
029500     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
029600          DELIMITED BY SIZE INTO SSA1                                     
029700     MOVE '  GE' TO GODK-STATUSKODER                                      
029800     CALL CBLTDLI USING GU WDA6-PCB DLI-IO-WDA601 SSA1                    
029900     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     .                                                                    
030200     SKIP3                                                                
030300 IMS-GET-WDA612 SECTION.                                                  
030400     MOVE 'WDA612  ' TO SSA1                                              
030500     MOVE '  GE' TO GODK-STATUSKODER                                      
030600     CALL CBLTDLI USING GHNP WDA6-PCB DLI-IO-WDA612 SSA1                  
030700     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     SKIP3                                                                
032565 IMS-GHU-WDA612 SECTION.                                                  
032566     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
032567          DELIMITED BY SIZE INTO SSA1                                     
032568     MOVE 'WDA612  ' TO SSA2                                              
032569     MOVE '  GE' TO GODK-STATUSKODER                                      
032570     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA612 SSA1 SSA2              
032571     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032572     PERFORM IMS-STATUSKONTROLL                                           
032573     .                                                                    
032574     EJECT                                                                
032593 IMS-REPL-WDA612 SECTION.                                                 
032594     MOVE '  ' TO GODK-STATUSKODER                                        
032595     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA612                       
032596     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032597     PERFORM IMS-STATUSKONTROLL                                           
032598     .                                                                    
032599     SKIP3                                                                
032607 IMS-GHU-WDA613 SECTION.                                                  
032608     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
032609          DELIMITED BY SIZE INTO SSA1                                     
032610     MOVE 'WDA613  ' TO SSA2                                              
032611     MOVE '  GE' TO GODK-STATUSKODER                                      
032612     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA613 SSA1 SSA2              
032613     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032614     PERFORM IMS-STATUSKONTROLL                                           
032615     .                                                                    
032616     SKIP3                                                                
032617 IMS-GET-WDA613 SECTION.                                                  
032618     MOVE 'WDA613  ' TO SSA1                                              
032619     MOVE '  GE' TO GODK-STATUSKODER                                      
032620     CALL CBLTDLI USING GHNP WDA6-PCB DLI-IO-WDA613 SSA1                  
032621     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032622     PERFORM IMS-STATUSKONTROLL                                           
032623     .                                                                    
032624     EJECT                                                                
032625 IMS-REPL-WDA613 SECTION.                                                 
032626     MOVE '  ' TO GODK-STATUSKODER                                        
032627     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA613                       
032628     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032629     PERFORM IMS-STATUSKONTROLL                                           
032630     .                                                                    
032640     SKIP3                                                                
032650 IMS-ISRT-WDA613 SECTION.                                                 
032660     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
032670          DELIMITED BY SIZE INTO SSA1                                     
032680     MOVE 'WDA613 ' TO SSA2                                               
032690     MOVE '  ' TO GODK-STATUSKODER                                        
032700     CALL CBLTDLI USING ISRT WDA6-PCB DLI-IO-WDA613 SSA1 SSA2             
032710     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032720     PERFORM IMS-STATUSKONTROLL                                           
032721     .                                                                    
032722     SKIP3                                                                
032730 IMS-DLET-WDA613 SECTION.                                                 
032731     MOVE '  ' TO GODK-STATUSKODER                                        
032732     CALL CBLTDLI USING DLET WDA6-PCB DLI-IO-WDA613                       
032733     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032734     PERFORM IMS-STATUSKONTROLL                                           
032735     .                                                                    
032736     EJECT                                                                
032737 IMS-GHU-WDA614 SECTION.                                                  
032738     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
032739          DELIMITED BY SIZE INTO SSA1                                     
032740     MOVE 'WDA614  ' TO SSA2                                              
032741     MOVE '  GE' TO GODK-STATUSKODER                                      
032742     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA614 SSA1 SSA2              
032743     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032744     PERFORM IMS-STATUSKONTROLL                                           
032745     .                                                                    
032746     SKIP3                                                                
032747 IMS-GET-WDA614 SECTION.                                                  
032748     MOVE 'WDA614  ' TO SSA1                                              
032749     MOVE '  GE' TO GODK-STATUSKODER                                      
032750     CALL CBLTDLI USING GHNP WDA6-PCB DLI-IO-WDA614 SSA1                  
032751     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032752     PERFORM IMS-STATUSKONTROLL                                           
032753     .                                                                    
032754     SKIP3                                                                
032755 IMS-REPL-WDA614 SECTION.                                                 
032756     MOVE '  ' TO GODK-STATUSKODER                                        
032757     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA614                       
032758     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032759     PERFORM IMS-STATUSKONTROLL                                           
032760     .                                                                    
032761     EJECT                                                                
032762 IMS-ISRT-WDA614 SECTION.                                                 
032763     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
032764          DELIMITED BY SIZE INTO SSA1                                     
032765     MOVE 'WDA614 ' TO SSA2                                               
032766     MOVE '  ' TO GODK-STATUSKODER                                        
032767     CALL CBLTDLI USING ISRT WDA6-PCB DLI-IO-WDA614 SSA1 SSA2             
032768     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032769     PERFORM IMS-STATUSKONTROLL                                           
032770     .                                                                    
032771     SKIP3                                                                
032772 IMS-DLET-WDA614 SECTION.                                                 
032773     MOVE '  ' TO GODK-STATUSKODER                                        
032774     CALL CBLTDLI USING DLET WDA6-PCB DLI-IO-WDA614                       
032775     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
032776     PERFORM IMS-STATUSKONTROLL                                           
032777     .                                                                    
032778     SKIP3                                                                
032779 IMS-STATUSKONTROLL SECTION.                                              
032780     SET STATUS-IX TO 1                                                   
032781     SEARCH GODK-STATUS                                                   
032782       AT END                                                             
032783         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032784         DELIMITED BY SIZE INTO FELTEXT                                   
032785         CALL FELLOG                                                      
032790       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    
