001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W6015400.                                                
001600 AUTHOR.         NIHLBLAD JOHAN.                                          
001700 DATE-WRITTEN.   02/02/11.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        PGM ANVÄNDS FÖR MANUELL PÅFYLLNING AV CDC/SVS                    
002200*        PGM ANVÄNDER WDK6 OCH WDM5                                       
002300*                                                                         
002401*        PROGRAMMET LÄSER      WDK6                                       
002410*        PROGRAMMET UPPDATERAR WDM5                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W6T154                                              
002800*        MID:         W6I15401                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W6O15401                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W6015400'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004510 77  FLBEST                      PIC X       VALUE 'N'.                   
004520 77  FLSALDO                     PIC X       VALUE 'N'.                   
004530 77  CDC-SALDO                   PIC S9(9)   VALUE ZERO.                  
004600                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005101 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005102     88  INDATA-OK                           VALUE 'J'.                   
005110     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  EGEN-MID                            VALUE '6154'.                
005900     88  GODK-MID                            VALUE '6154'.                
006400     88  HELP-MID                            VALUE '0551'.                
006500     EJECT                                                                
006510 01  DAGENS-DATUM                PIC 9(8).                                
006520 01  DADATTID                    PIC 9(14).                               
006530 01  WS-ADTRDEST                 PIC X(3).                                
006540 01  WS-KVBEST                   PIC S9(9)    VALUE ZERO.                 
006630*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006700 01  GENERELLA-SUBPROGRAM.                                                
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007210     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007801     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007802     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007803     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007810     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
008101     EJECT                                                                
008102 01  FELTEXTER.                                                           
008103     03  FEL0  PIC X(32) VALUE 'DEST,ARTNR,ANTAL MÅSTE FYLLAS I '.        
008104     03  FEL1  PIC X(32) VALUE 'ENDAST SVS ELLER CDC I DEST-FÄLT'.        
008105     03  FEL2  PIC X(32) VALUE 'ARTNR SAKNAR LAGERPLATS FÖR DEST'.        
008106     03  FEL3  PIC X(32) VALUE 'ANTAL FELAKTIGT                 '.        
008107     03  FEL4  PIC X(32) VALUE 'BESTÄLLNING FINNS REDAN PÅ 6152 '.        
008108     03  FEL5  PIC X(32) VALUE 'ANTAL FINNS PÅ CDC              '.        
008109     03  FEL6  PIC X(32) VALUE 'BEST FINNS PÅ 6152/ SALDO PÅ CDC'.        
008120*01  -COPY WDATAREA                                                       
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009000*                                                                         
009100 01  SPAR-AREA.                                                           
009200     03  SPAR-IDTRANS           PIC X(4)    VALUE '6154'.                 
009400     EJECT                                                                
009500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W6I15401                                                   
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500     03  MOD REDEFINES MSG-AREA.                                          
010600*      05  -COPY W6O15401                                                 
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900     SKIP3                                                                
011000*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011701     03  W-IDARTNR-X.                                                     
011702         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011703     03  W-KDSEGKEY-X.                                                    
011704         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011705     03  W-ADTRDEST-X.                                                    
011706         05  W-ADTRDEST          PIC X(3)    VALUE SPACE.                 
011707     03  W-IDTRPTNR-X.                                                    
011708         05  W-IDTRPTNR          PIC S9(5)   VALUE ZERO COMP-3.           
011709     03  W-DADATTID-9KOMPL-X.                                             
011710         05  W-DADATTID-9KOMPL   PIC S9(14)   VALUE ZERO.                 
011800     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FINNS                       VALUE '  '.                  
012200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GODK-STATUSKODER.                                                    
012600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(64).                               
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013702 01  DLI-IO-WDK601.                                                       
013703*    03  -COPY WDK601                                                     
013704     EJECT                                                                
013705 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013706 01  DLI-IO-WDK611.                                                       
013707*    03  -COPY WDK611                                                     
013708 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM501'.                      
013709 01  DLI-IO-WDM501.                                                       
013710*    03  -COPY WDM501                                                     
013711     EJECT                                                                
013712 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM511'.                      
013713 01  DLI-IO-WDM511.                                                       
013714*    03  -COPY WDM511                                                     
013715     EJECT                                                                
013716 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM521'.                      
013717 01  DLI-IO-WDM521.                                                       
013720*    03  -COPY WDM521                                                     
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0009   -PRE MSG-                                              
014300*01  -COPY W0008   -PRE WDP7-                                             
014400     05  FILLER                  PIC X.                                   
014501                                                                          
014505*01  -COPY W0008  -PRE WDK6-                                              
014506     05  FILLER                  PIC X.                                   
014507                                                                          
014508*01  -COPY W0008  -PRE WDM5-                                              
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
014702                           WDK6-PCB WDM5-PCB.                             
014703 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
014720                           WDK6-PCB WDM5-PCB.                             
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200       PERFORM A-INIT                                                     
015501       IF MFS-UPDATE OR MFS-UPD-V                                         
015502         PERFORM G-KOLLA-INPUT                                            
015503         IF INDATA-OK                                                     
015504           PERFORM H-UPPDATERA                                            
015505         END-IF                                                           
015510       ELSE                                                               
015701         IF MFS-FIRST                                                     
015702           PERFORM C-FOERSTA-SIDA                                         
015703         ELSE                                                             
015704           PERFORM E-SAMMA-SIDA                                           
015710         END-IF                                                           
016000       END-IF                                                             
016100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016300       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15401 + 4                      
016400       PERFORM IMS-INSERT-MSG                                             
016500     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     IF MSG-DUBBLA-TRANSKODER                                             
017500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I15401                 
017600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017800     ELSE                                                                 
017900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I15401                  
018000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018200     END-IF                                                               
018300                                                                          
018400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018700                                                                          
018800     MOVE LOW-VALUE TO MSG-AREA                                           
018900     MOVE 'W6O15401' TO MFS-IDMOD                                         
019000     MOVE '6154' TO MOD-IDTRANS                                           
019100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019200                                                                          
019300     IF EGEN-MID OR HELP-MID                                              
019400       CONTINUE                                                           
019500     ELSE                                                                 
019600       MOVE SPACE TO MFS-KDTRTYP                                          
019700       MOVE '7' TO MFS-IDPFK                                              
019800     END-IF                                                               
019801     MOVE ALL '+'            TO MSGI-WMSGINIT                             
019802     MOVE '001'              TO MSGI-KDCALL                               
019803     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
019804     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
019805     MOVE '6154'             TO MSGI-IDTRANS                              
019810     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
019900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
020000     MOVE FUNCTION  CURRENT-DATE(1:8)  TO DAGENS-DATUM                    
020100     .                                                                    
020200     EJECT                                                                
023401 C-FOERSTA-SIDA SECTION.                                                  
023402                                                                          
023403     PERFORM MFS-RENSA-FAELT-IN                                           
023404     .                                                                    
023405     EJECT                                                                
023406 E-SAMMA-SIDA SECTION.                                                    
023407                                                                          
023408     IF EGEN-MID OR HELP-MID                                              
023409       IF MID-INPUT = ALL '+'                                             
023410         PERFORM MFS-RENSA-FAELT-IN                                       
023411       ELSE                                                               
023412         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
023413         CALL WMEDKONV USING MED-WMEDAREA                                 
023414         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023416       END-IF                                                             
023417     ELSE                                                                 
023418       PERFORM MFS-RENSA-FAELT-IN                                         
023419     END-IF                                                               
023420     .                                                                    
023421     EJECT                                                                
025702 G-KOLLA-INPUT SECTION.                                                   
025703                                                                          
025705     MOVE JA  TO INDATA-SW                                                
025706     IF MID-INPUT = ALL '+'                                               
025708       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025709       CALL WMEDKONV USING MED-WMEDAREA                                   
025710       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
025711       PERFORM MFS-ROER-EJ-FAELT-UT                                       
025712       MOVE NEJ TO INDATA-SW                                              
025713     ELSE                                                                 
025715       IF MID-ADLASTPL = ALL '+'                                          
025716       OR MID-IDARTNR  = ALL '+'                                          
025717       OR MID-KVANTAL  = ALL '+'                                          
025722          MOVE FEL0               TO MOD-TEMFSFEL                         
025723          MOVE NEJ TO INDATA-SW                                           
025724       ELSE                                                               
025726         IF MID-ADLASTPL = 'SVS' OR MID-ADLASTPL = 'CDC'                  
025728            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLASTPL-ATTR                
025729            IF MID-ADLASTPL = 'SVS'                                       
025730              MOVE 'CDC' TO WS-ADTRDEST                                   
025731            ELSE                                                          
025732              MOVE 'SVS' TO WS-ADTRDEST                                   
025733            END-IF                                                        
025745                                                                          
025746            IF MID-IDARTNR NOT NUMERIC OR (MID-FLEJBOK NOT =              
025747               '+' AND 'J' AND 'N') OR MID-KVANTAL NOT NUMERIC            
025748              MOVE NEJ TO INDATA-SW                                       
025749              IF MID-KVANTAL NOT NUMERIC                                  
025751                MOVE MFS-ADD-LAES-IN-FAELT-HI                             
025752                                      TO MOD-KVANTAL-ATTR                 
025753                MOVE MFS-ADD-LAES-IN-FAELT                                
025754                                      TO MOD-ADLASTPL-ATTR                
025755                                         MOD-IDARTNR-ATTR                 
025756                                         MOD-FLEJBOK-ATTR                 
025757                                         MOD-TETRPMED-ATTR                
025758              ELSE                                                        
025759                MOVE MFS-ADD-LAES-IN-FAELT-HI                             
025760                                      TO MOD-IDARTNR-ATTR                 
025761                MOVE MFS-ADD-LAES-IN-FAELT                                
025762                                      TO MOD-ADLASTPL-ATTR                
025763                                         MOD-KVANTAL-ATTR                 
025764                                         MOD-FLEJBOK-ATTR                 
025765                                         MOD-TETRPMED-ATTR                
025766              END-IF                                                      
025768            ELSE                                                          
025769              MOVE MID-IDARTNR TO W-IDARTNR                               
025770              PERFORM IMS-GU-K611                                         
025771                IF SEGMENT-FINNS                                          
025772                                                                          
025773                  MOVE ZERO  TO WS-KVBEST                                 
025774                  MOVE WS-ADTRDEST                                        
025775                                TO W-ADTRDEST                             
025776                  MOVE NEJ      TO FLBEST                                 
025777                  PERFORM IMS-GU-M501                                     
025778                  PERFORM IMS-GNP-M521                                    
025779                                                                          
025780                  PERFORM UNTIL SEGMENT-SAKNAS                            
025781                                                                          
025782                     IF W-IDARTNR = AVG-IDARTNR                           
025783                        IF MFS-UPDATE                                     
025784                          MOVE JA  TO FLBEST                              
025785                          MOVE NEJ TO INDATA-SW                           
025786                          MOVE MFS-ADD-LAES-IN-FAELT                      
025787                                          TO MOD-ADLASTPL-ATTR            
025788                                             MOD-IDARTNR-ATTR             
025789                                             MOD-KVANTAL-ATTR             
025790                                             MOD-FLEJBOK-ATTR             
025791                                             MOD-TETRPMED-ATTR            
025792                                                                          
025793                          ADD AVG-KVANTAL TO WS-KVBEST                    
025794                        END-IF                                            
025795                     END-IF                                               
025796                                                                          
025797                     PERFORM IMS-GNP-M521                                 
025798                                                                          
025799                  END-PERFORM                                             
025800                                                                          
025801                  IF MFS-UPDATE AND WS-ADTRDEST = 'CDC'                   
025802                    MOVE NEJ TO FLSALDO                                   
025803                    COMPUTE CDC-SALDO = CLAG-KVLS - CLAG-KVLS-SVS         
025804                    IF CDC-SALDO NOT > 0                                  
025805                      CONTINUE                                            
025806                    ELSE                                                  
025807** SALDO FINNS REDAN PÅ CDC                                               
025808                      MOVE JA  TO FLSALDO                                 
025809                      MOVE NEJ TO INDATA-SW                               
025810                      MOVE MFS-ADD-LAES-IN-FAELT                          
025811                                      TO MOD-ADLASTPL-ATTR                
025812                                         MOD-IDARTNR-ATTR                 
025813                                         MOD-KVANTAL-ATTR                 
025814                                         MOD-FLEJBOK-ATTR                 
025815                                         MOD-TETRPMED-ATTR                
025816                    END-IF                                                
025817                  END-IF                                                  
025818                                                                          
025819                  IF FLBEST = JA OR FLSALDO = JA                          
025820                    IF FLBEST = JA AND FLSALDO = JA                       
025821                       MOVE  FEL6   TO MOD-TEMFSFEL                       
025822                    ELSE                                                  
025823                      IF FLBEST = JA                                      
025824                        MOVE FEL4   TO MOD-TEMFSFEL                       
025825                      ELSE                                                
025826                        MOVE FEL5   TO MOD-TEMFSFEL                       
025827                      END-IF                                              
025828                    END-IF                                                
025829                  END-IF                                                  
025830                                                                          
025831                  IF (WS-ADTRDEST = 'SVS'                                 
025832                     AND CLAG-ADLAGOMR-SVS > 0)                           
025833                  OR (WS-ADTRDEST = 'CDC'                                 
025834                     AND CLAG-ADPLATS > 0)                                
025835                     MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR         
025836                  ELSE                                                    
025837                     MOVE FEL2    TO MOD-TEMFSFEL                         
025838                     MOVE NEJ TO INDATA-SW                                
025839                     MOVE MFS-ADD-LAES-IN-FAELT-HI                        
025840                                      TO MOD-ADLASTPL-ATTR                
025841                     MOVE MFS-ADD-LAES-IN-FAELT                           
025842                                      TO MOD-IDARTNR-ATTR                 
025843                                         MOD-KVANTAL-ATTR                 
025844                                         MOD-FLEJBOK-ATTR                 
025845                                         MOD-TETRPMED-ATTR                
025846                  END-IF                                                  
025847                                                                          
025848                  IF MID-FLEJBOK = JA                                     
025849                    MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-ATTR          
025850                  ELSE                                                    
025851                    IF (WS-ADTRDEST = 'SVS'                               
025852                     AND (MID-KVANTAL > (CLAG-KVLS - CLAG-KVLS-SVS        
025853                                       - WS-KVBEST)))                     
025854                    OR (WS-ADTRDEST = 'CDC'                               
025855                    AND (MID-KVANTAL >                                    
025856                                     (CLAG-KVLS-SVS - WS-KVBEST)))        
025857                       MOVE FEL3    TO MOD-TEMFSFEL                       
025858                       MOVE NEJ TO INDATA-SW                              
025859                       MOVE MFS-ADD-LAES-IN-FAELT-HI                      
025860                                        TO MOD-KVANTAL-ATTR               
025861                       MOVE MFS-ADD-LAES-IN-FAELT                         
025862                                        TO MOD-ADLASTPL-ATTR              
025863                                           MOD-IDARTNR-ATTR               
025864                                           MOD-FLEJBOK-ATTR               
025865                                           MOD-TETRPMED-ATTR              
025866                    ELSE                                                  
025867                      MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-ATTR        
025868                    END-IF                                                
025869                  END-IF                                                  
025870                ELSE                                                      
025871                  MOVE PART-MISSING TO MED-IDMFSFEL                       
025872                  CALL WMEDKONV USING MED-WMEDAREA                        
025873                  MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                       
025874                  MOVE NEJ TO INDATA-SW                                   
025875                  PERFORM MFS-ROER-EJ-FAELT-UT                            
025876                  MOVE MFS-ADD-LAES-IN-FAELT-HI                           
025877                                      TO MOD-IDARTNR-ATTR                 
025878                  MOVE MFS-ADD-LAES-IN-FAELT                              
025879                                      TO MOD-ADLASTPL-ATTR                
025880                                         MOD-KVANTAL-ATTR                 
025881                                         MOD-FLEJBOK-ATTR                 
025882                                         MOD-TETRPMED-ATTR                
025883                END-IF                                                    
025884            END-IF                                                        
025885         ELSE                                                             
025886            MOVE FEL1             TO MOD-TEMFSFEL                         
025887            MOVE NEJ TO INDATA-SW                                         
025888            MOVE MFS-ADD-LAES-IN-FAELT-HI                                 
025889                                   TO MOD-ADLASTPL-ATTR                   
025890            MOVE MFS-ADD-LAES-IN-FAELT                                    
025891                                   TO MOD-IDARTNR-ATTR                    
025892                                      MOD-KVANTAL-ATTR                    
025893                                      MOD-FLEJBOK-ATTR                    
025894                                      MOD-TETRPMED-ATTR                   
025895         END-IF                                                           
025896         IF INDATA-FEL                                                    
025897            PERFORM MFS-ROER-EJ-FAELT-UT                                  
025898         END-IF                                                           
025899       END-IF                                                             
025900                                                                          
025901     END-IF                                                               
025902     .                                                                    
025903     EJECT                                                                
025904 H-UPPDATERA SECTION.                                                     
025905                                                                          
025906     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
025907     MOVE 99999              TO W-IDTRPTNR                                
025908     PERFORM IMS-GU-M511                                                  
025909     IF SEGMENT-SAKNAS                                                    
025910        MOVE 99999           TO TRAN-IDTRPTNR                             
025911        PERFORM IMS-ISRT-M511                                             
025912     END-IF                                                               
025913     MOVE SPACE                       TO AVG-WDM521                       
025914     MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                         
025915     MOVE DADATTID                   TO AVG-DADATTID                      
025916     MOVE MID-IDARTNR                TO AVG-IDARTNR                       
025917     MOVE SPACE                      TO AVG-KDTRPSTA                      
025918     MOVE MID-KVANTAL                TO AVG-KVANTAL                       
025919     MOVE DAGENS-DATUM               TO AVG-TIUPPDAT                      
025920     MOVE MSGI-IDUSER                TO AVG-IDUSER-TRP                    
025921     IF MID-FLEJBOK = ALL '+'                                             
025922        MOVE NEJ                     TO AVG-FLEJBOK                       
025923     ELSE                                                                 
025924        MOVE MID-FLEJBOK             TO AVG-FLEJBOK                       
025925     END-IF                                                               
025926     IF MID-TETRPMED = ALL '+'                                            
025927        MOVE SPACES                  TO AVG-TETRPMED                      
025928     ELSE                                                                 
025929        MOVE MID-TETRPMED            TO AVG-TETRPMED                      
025930     END-IF                                                               
025931     PERFORM IMS-ISRT-M521                                                
025932     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025933     CALL WMEDKONV USING MED-WMEDAREA                                     
025934     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
025935     PERFORM MFS-FORM-ATTR                                                
025936     PERFORM MFS-RENSA-FAELT-IN                                           
025937                                                                          
025940     .                                                                    
026000     EJECT                                                                
026800 MFS-RENSA-FAELT-IN SECTION.                                              
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027100     MOVE MFS-RENSA-FAELT TO MOD-ADLASTPL                                 
027200                             MOD-IDARTNR                                  
027210                             MOD-KVANTAL                                  
027220                             MOD-FLEJBOK                                  
027230                             MOD-TETRPMED                                 
027300     .                                                                    
027400     EJECT                                                                
027500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027600                                                                          
027700*    --- ALLA UTDATA-FÄLT                                                 
027900     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLASTPL                               
028000                               MOD-IDARTNR                                
028100                               MOD-KVANTAL                                
028110                               MOD-FLEJBOK                                
028120                               MOD-TETRPMED                               
028200     .                                                                    
028300     SKIP3                                                                
029100 MFS-FORM-ATTR SECTION.                                                   
029200                                                                          
029300*    --- ALLA INDATA-FÄLT                                                 
029400     MOVE MFS-FORMATETS-ATTR TO MOD-ADLASTPL-ATTR                         
029500                                MOD-IDARTNR-ATTR                          
029510                                MOD-KVANTAL-ATTR                          
029600     .                                                                    
029700     SKIP2                                                                
030700 IMS-GET-MSG SECTION.                                                     
030800                                                                          
030900     MOVE '  QC' TO GODK-STATUSKODER                                      
031000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031400     SKIP3                                                                
031500 IMS-INSERT-MSG SECTION.                                                  
031600                                                                          
031700     IF MSGI-IDLAND-SPR = 'SE'                                            
031800       MOVE '0' TO MFS-KDHUVOMR                                           
031900     END-IF                                                               
032000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032100     MOVE SPACE TO GODK-STATUSKODER                                       
032200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032601     EJECT                                                                
032612 IMS-GU-K611 SECTION.                                                     
032613                                                                          
032614     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032615          DELIMITED BY SIZE INTO SSA1                                     
032616     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032617          DELIMITED BY SIZE INTO SSA2                                     
032618     MOVE '  GE' TO GODK-STATUSKODER                                      
032619     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
032620     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032621     PERFORM IMS-STATUSKONTROLL                                           
032622     .                                                                    
032623     EJECT                                                                
032624 IMS-GU-M501   SECTION.                                                   
032625                                                                          
032626     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
032627          DELIMITED BY SIZE INTO SSA1                                     
032628     MOVE SPACE  TO GODK-STATUSKODER                                      
032629     CALL CBLTDLI USING GU  WDM5-PCB DLI-IO-WDM501 SSA1                   
032630     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
032631     PERFORM IMS-STATUSKONTROLL                                           
032632     .                                                                    
032633     EJECT                                                                
032660 IMS-GU-M511 SECTION.                                                     
032661                                                                          
032662     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST-X ')'                        
032663          DELIMITED BY SIZE INTO SSA1                                     
032664     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
032665          DELIMITED BY SIZE INTO SSA2                                     
032666     MOVE '  GE' TO GODK-STATUSKODER                                      
032667     CALL CBLTDLI USING GU WDM5-PCB DLI-IO-WDM511 SSA1 SSA2               
032668     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
032669     PERFORM IMS-STATUSKONTROLL                                           
032670     .                                                                    
032671     EJECT                                                                
032672 IMS-ISRT-M511 SECTION.                                                   
032673                                                                          
032674     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST-X ')'                        
032675          DELIMITED BY SIZE INTO SSA1                                     
032676     MOVE 'WDM511 ' TO SSA2                                               
032677     MOVE '  II' TO GODK-STATUSKODER                                      
032678     CALL CBLTDLI USING ISRT WDM5-PCB DLI-IO-WDM511 SSA1 SSA2             
032679     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
032680     PERFORM IMS-STATUSKONTROLL                                           
032681     .                                                                    
032682     SKIP3                                                                
032692 IMS-ISRT-M521 SECTION.                                                   
032693                                                                          
032696     MOVE 'WDM521 ' TO SSA1                                               
032697     MOVE '  II' TO GODK-STATUSKODER                                      
032698     CALL CBLTDLI USING ISRT WDM5-PCB DLI-IO-WDM521 SSA1                  
032699     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSKONTROLL                                           
032701     .                                                                    
032702     SKIP3                                                                
032703 IMS-GNP-M521 SECTION.                                                    
032704                                                                          
032705     STRING 'WDM511    '                                                  
032706          DELIMITED BY SIZE INTO SSA1                                     
032707     STRING 'WDM521    '                                                  
032708          DELIMITED BY SIZE INTO SSA2                                     
032709     MOVE '  GE' TO GODK-STATUSKODER                                      
032710     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2              
032720     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
032730     PERFORM IMS-STATUSKONTROLL                                           
032740     .                                                                    
032750     EJECT                                                                
032800 IMS-STATUSKONTROLL SECTION.                                              
032900                                                                          
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GODK-STATUS                                                   
033200       AT END                                                             
033300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033400         DELIMITED BY SIZE INTO FELTEXT                                   
033500         CALL FELLOG                                                      
033600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033700         CONTINUE                                                         
033800     END-SEARCH                                                           
033900     .                                                                    
