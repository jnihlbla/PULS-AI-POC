001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W2224800.                                                
001300 AUTHOR.         ANDREASSON STEFAN.                                       
001400 DATE-WRITTEN.   00/04/14.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        ÄRVA SÄSONG                                                      
002000*                                                                         
002101*        PROGRAMMET LÄSER      WDD7                                       
002110*        PROGRAMMET UPPDATERAR WDK6                                       
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003001     SKIP2                                                                
003002*          --- ARTIKLAR MED FÖRÄNDRAD ERS. KOD                            
003010     SELECT W22247                     ASSIGN TO W22248D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003601     SKIP3                                                                
003602 FD  W22247                                                               
003603     RECORDING       F                                                    
003604     BLOCK CONTAINS  0.                                                   
003605                                                                          
003610*01  -COPY W22247      -L.                                                
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W2224800'.            
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  WS.                                                                  
005101     03 WS-RESEASON              OCCURS 12 TIMES                          
005102                                 PIC S9V9(2) VALUE ZERO.                  
005103                                                                          
005104 01  DD-PLUS-30-MAN-TISSAAMMDD.                                           
005105     03  DD-PLUS-30-MAN-TISS     PIC 9(2)    VALUE ZERO.                  
005106     03  DD-PLUS-30-MAN-TIAAMMDD PIC 9(6)    VALUE ZERO.                  
005107*                                                                         
005110 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005501                                                                          
005502 77  W22247-EOF-SW               PIC X       VALUE 'N'.                   
005510     88  END-OF-W22247                       VALUE 'J'.                   
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006920     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006930     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007020     EJECT                                                                
007030*    --- PARAMETRAR TILL DAGKONV                                          
007040*                                                                         
007050*01  -COPY WDAGAREA                                                       
007060     SKIP2                                                                
007070*    --- PARAMETRAR TILL ABEND                                            
007080                                                                          
007090 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007301     EJECT                                                                
007302 01  IN-AREA-START               PIC X(24)   VALUE                        
007303                                             'IN-AREA-START'.             
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W22247     -PRE IN-                                       
007400*                                                                         
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007901     03  W-IDARTNR-X.                                                     
007902         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007905     03  W-IDDC-X.                                                        
007910         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008000     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     88  IMS-EJ-OK                           VALUE 'XD'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009310 01  SSA3                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000                                                                          
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD701'.                      
010102 01  DLI-IO-WDD701.                                                       
010103*    03  -COPY WDD701                                                     
010104     EJECT                                                                
010105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD702'.                      
010106 01  DLI-IO-WDD702.                                                       
010107*    03  -COPY WDD702                                                     
010120     EJECT                                                                
010130 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK626'.                      
010140     SKIP3                                                                
010150 01  DLI-IO-WDK626.                                                       
010160*    03  -COPY WDK626                                                     
010200                                                                          
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010900*01  -COPY W0009   -PRE MSG-                                              
011001                                                                          
011002*01  -COPY W0008  -PRE WDD7-                                              
011003     05  FILLER                  PIC X.                                   
011004                                                                          
011005*01  -COPY W0008  -PRE WDK6-                                              
011010     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011401 PROCEDURE DIVISION  USING MSG-PCB WDD7-PCB WDK6-PCB.                     
011402 MAIN SECTION.                                                            
011410     ENTRY 'DLITCBL' USING MSG-PCB WDD7-PCB WDK6-PCB.                     
011500                                                                          
011700     SKIP2                                                                
011800     PERFORM A-INIT                                                       
011910     PERFORM S01-LAES-W22247                                              
012001     PERFORM UNTIL END-OF-W22247                                          
012002                                                                          
012003       PERFORM B-BEHANDLA-POSTER                                          
013010       PERFORM S01-LAES-W22247                                            
013100     END-PERFORM                                                          
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100     SKIP2                                                                
014200                                                                          
014300     PERFORM IMS-RESTART                                                  
014501                                                                          
014510     OPEN INPUT W22247                                                    
014800                                                                          
014900     MOVE FUNCTION CURRENT-DATE (1:8)                                     
014910                            TO DAGENS-DATUM                               
015000     MOVE FUNCTION CURRENT-DATE (1:2)                                     
015010                            TO DAG-TISEKEL-FOM                            
015011     MOVE FUNCTION CURRENT-DATE (3:6)                                     
015020                            TO DAG-TIAAMMDD-FOM                           
015030     MOVE 002               TO DAG-KDCALL                                 
015040     MOVE 913               TO DAG-KVKALDAG                               
015050                                                                          
015060     CALL WDAGKONV USING DAG-KDCALL                                       
015070                         DAG-DATUM-AREA                                   
015080                         DAG-KDSVAR                                       
015090     IF DAG-KDSVAR = SPACE                                                
015091        MOVE DAG-TISEKEL-TOM  TO DD-PLUS-30-MAN-TISS                      
015092        MOVE DAG-TIAAMMDD-TOM TO DD-PLUS-30-MAN-TIAAMMDD                  
015093     ELSE                                                                 
015094        MOVE 'FEL FRÅN WDAGKONV 1  I A-INIT SECTION I W27136' TO          
015095                                    FELTEXT-STR                           
015096        DISPLAY FELTEXT                                                   
015097        PERFORM S99-ABEND                                                 
015098     END-IF                                                               
015100                                                                          
015210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500     .                                                                    
015700     EJECT                                                                
015710 B-BEHANDLA-POSTER SECTION.                                               
015720                                                                          
015721     MOVE IN-IDARTNR         TO W-IDARTNR                                 
015730     PERFORM IMS-GU-K626                                                  
015740     IF SEGMENT-FINNS                                                     
015741     AND NOT (JUST-RESEASON (1) = 1.00                                    
015750     AND       JUST-RESEASON (2) = 1.00                                   
015760     AND       JUST-RESEASON (3) = 1.00                                   
015770     AND       JUST-RESEASON (4) = 1.00                                   
015780     AND       JUST-RESEASON (5) = 1.00                                   
015790     AND       JUST-RESEASON (6) = 1.00                                   
015800     AND       JUST-RESEASON (7) = 1.00                                   
015900     AND       JUST-RESEASON (8) = 1.00                                   
015910     AND       JUST-RESEASON (9) = 1.00                                   
015920     AND       JUST-RESEASON (10) = 1.00                                  
015921     AND       JUST-RESEASON (11) = 1.00                                  
015922     AND       JUST-RESEASON (12) = 1.00)                                 
015924       MOVE JUST-RESEASON (1)                                             
015925                           TO WS-RESEASON (1)                             
015926       MOVE JUST-RESEASON (2)                                             
015927                           TO WS-RESEASON (2)                             
015928       MOVE JUST-RESEASON (3)                                             
015929                           TO WS-RESEASON (3)                             
015930       MOVE JUST-RESEASON (4)                                             
015931                           TO WS-RESEASON (4)                             
015932       MOVE JUST-RESEASON (5)                                             
015933                           TO WS-RESEASON (5)                             
015934       MOVE JUST-RESEASON (6)                                             
015935                           TO WS-RESEASON (6)                             
015936       MOVE JUST-RESEASON (7)                                             
015937                           TO WS-RESEASON (7)                             
015938       MOVE JUST-RESEASON (8)                                             
015939                           TO WS-RESEASON (8)                             
015940       MOVE JUST-RESEASON (9)                                             
015941                           TO WS-RESEASON (9)                             
015942       MOVE JUST-RESEASON (10)                                            
015943                           TO WS-RESEASON (10)                            
015944       MOVE JUST-RESEASON (11)                                            
015945                           TO WS-RESEASON (11)                            
015946       MOVE JUST-RESEASON (12)                                            
015947                           TO WS-RESEASON (12)                            
015948       PERFORM IMS-GU-D701                                                
015949       IF SEGMENT-FINNS                                                   
015951          PERFORM IMS-GNP-D702                                            
015952          IF SEGMENT-FINNS                                                
015953             MOVE IDARTNR-TILLK                                           
015954                           TO W-IDARTNR                                   
015956             PERFORM IMS-GNP-D702                                         
015957             IF SEGMENT-SAKNAS                                            
015958*                                                                         
015959*  RAK ERSÄTTNING, ÄRVA SÄSONG                                            
015960*                                                                         
015961               PERFORM IMS-GHU-K626                                       
015962               IF SEGMENT-FINNS                                           
015963               AND JUST-DASPSEA < DAGENS-DATUM                            
015965                  MOVE DD-PLUS-30-MAN-TISSAAMMDD                          
015966                             TO JUST-DASPSEA                              
015967                  MOVE WS-RESEASON (1)                                    
015968                             TO JUST-RESEASON (1)                         
015969                  MOVE WS-RESEASON (2)                                    
015970                             TO JUST-RESEASON (2)                         
015971                  MOVE WS-RESEASON (3)                                    
015972                             TO JUST-RESEASON (3)                         
015973                  MOVE WS-RESEASON (4)                                    
015974                             TO JUST-RESEASON (4)                         
015975                  MOVE WS-RESEASON (5)                                    
015976                             TO JUST-RESEASON (5)                         
015977                  MOVE WS-RESEASON (6)                                    
015978                             TO JUST-RESEASON (6)                         
015979                  MOVE WS-RESEASON (7)                                    
015980                             TO JUST-RESEASON (7)                         
015981                  MOVE WS-RESEASON (8)                                    
015982                             TO JUST-RESEASON (8)                         
015983                  MOVE WS-RESEASON (9)                                    
015984                             TO JUST-RESEASON (9)                         
015985                  MOVE WS-RESEASON (10)                                   
015986                             TO JUST-RESEASON (10)                        
015987                  MOVE WS-RESEASON (11)                                   
015988                             TO JUST-RESEASON (11)                        
015989                  MOVE WS-RESEASON (12)                                   
015990                             TO JUST-RESEASON (12)                        
016007                  PERFORM IMS-REPL-K626                                   
016008                                                                          
016009                  IF CHKP-ANT > CHKP-MAX                                  
016010                    PERFORM X-TAG-CHECKPOINT                              
016011                  END-IF                                                  
016012               ELSE                                                       
016013                 IF SEGMENT-SAKNAS                                        
016014                    MOVE ZERO                                             
016015                             TO JUST-REPBJUST                             
016016                                JUST-TIPBJUST-CENTR                       
016017                                JUST-KVPB-JUST (1)                        
016018                                JUST-TIPBJUST (1)                         
016019                                JUST-KVPB-JUST (2)                        
016020                                JUST-TIPBJUST (2)                         
016021                    MOVE DD-PLUS-30-MAN-TISSAAMMDD                        
016022                             TO JUST-DASPSEA                              
016023                    MOVE WS-RESEASON (1)                                  
016024                             TO JUST-RESEASON (1)                         
016025                    MOVE WS-RESEASON (2)                                  
016026                             TO JUST-RESEASON (2)                         
016027                    MOVE WS-RESEASON (3)                                  
016028                             TO JUST-RESEASON (3)                         
016029                    MOVE WS-RESEASON (4)                                  
016030                             TO JUST-RESEASON (4)                         
016031                    MOVE WS-RESEASON (5)                                  
016032                             TO JUST-RESEASON (5)                         
016033                    MOVE WS-RESEASON (6)                                  
016034                             TO JUST-RESEASON (6)                         
016035                    MOVE WS-RESEASON (7)                                  
016036                             TO JUST-RESEASON (7)                         
016037                    MOVE WS-RESEASON (8)                                  
016038                             TO JUST-RESEASON (8)                         
016039                    MOVE WS-RESEASON (9)                                  
016040                             TO JUST-RESEASON (9)                         
016041                    MOVE WS-RESEASON (10)                                 
016042                             TO JUST-RESEASON (10)                        
016043                    MOVE WS-RESEASON (11)                                 
016044                             TO JUST-RESEASON (11)                        
016045                    MOVE WS-RESEASON (12)                                 
016046                             TO JUST-RESEASON (12)                        
016063                    PERFORM IMS-ISRT-K626                                 
016064                                                                          
016065                    IF CHKP-ANT > CHKP-MAX                                
016066                      PERFORM X-TAG-CHECKPOINT                            
016067                    END-IF                                                
016068                 END-IF                                                   
016069               END-IF                                                     
016070             END-IF                                                       
016071          END-IF                                                          
016072       END-IF                                                             
016073     END-IF                                                               
016074     .                                                                    
016075     EJECT                                                                
016080 Z-FINIT SECTION.                                                         
016100                                                                          
016301                                                                          
016310     CLOSE W22247                                                         
016501     SKIP2                                                                
016502     MOVE 'S' TO POSTSUM-OPKOD                                            
016510     CALL POSTSUM USING POSTSUM-PARM                                      
016700     .                                                                    
016801     EJECT                                                                
016802 S01-LAES-W22247  SECTION.                                                
016803     SKIP2                                                                
016804     READ W22247 INTO IN-AREA                                             
016805     AT END                                                               
016807        SET END-OF-W22247 TO TRUE                                         
016808                                                                          
016809     NOT AT END                                                           
016810        MOVE 'W22247'        TO POSTSUM-FDNAMN                            
016811        MOVE 'W22248D1'      TO POSTSUM-DDNAMN2                           
016812        MOVE SPACE           TO POSTSUM-TRANSTYP                          
016813        CALL POSTSUM USING POSTSUM-PARM                                   
016814                                                                          
016816     END-READ                                                             
016820     .                                                                    
017100     EJECT                                                                
017200 X-TAG-CHECKPOINT   SECTION.                                              
017300                                                                          
017400* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
017500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
017900     PERFORM IMS-CHECKPOINT                                               
018000     MOVE ZERO TO CHKP-ANT                                                
018100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
018200     .                                                                    
018300     EJECT                                                                
018330 S99-ABEND SECTION.                                                       
018340                                                                          
018350     MOVE 'S' TO POSTSUM-OPKOD                                            
018360     CALL POSTSUM USING POSTSUM-PARM                                      
018370     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
018380     .                                                                    
018390     EJECT                                                                
018400* --- IMS SEKTIONER ---                                                   
018500                                                                          
018601     EJECT                                                                
018622 IMS-GU-K626 SECTION.                                                     
018623     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018624          DELIMITED BY SIZE INTO SSA1                                     
018625     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
018626     MOVE 'WDK626   ' TO SSA3                                             
018627     MOVE '  GE' TO GODK-STATUSKODER                                      
018628     CALL CBLTDLI USING GU                                                
018629                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
018630     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018640     PERFORM IMS-STATUSKONTROLL                                           
018650     .                                                                    
018651     EJECT                                                                
018652 IMS-GHU-K626 SECTION.                                                    
018653     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018654          DELIMITED BY SIZE INTO SSA1                                     
018655     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
018656     MOVE 'WDK626   ' TO SSA3                                             
018657     MOVE '  GE' TO GODK-STATUSKODER                                      
018658     CALL CBLTDLI USING GHU                                               
018659                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
018660     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018661     PERFORM IMS-STATUSKONTROLL                                           
018662     .                                                                    
018670     EJECT                                                                
018710 IMS-ISRT-K626 SECTION.                                                   
018711                                                                          
018712     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018713          DELIMITED BY SIZE INTO SSA1                                     
018714     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
018715     MOVE 'WDK626   ' TO SSA3                                             
018716     MOVE '  ' TO GODK-STATUSKODER                                        
018717     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK626                       
018718                             SSA1 SSA2 SSA3                               
018719     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018720     PERFORM IMS-STATUSKONTROLL                                           
018721     .                                                                    
018722     EJECT                                                                
018723 IMS-REPL-K626 SECTION.                                                   
018724                                                                          
018725     MOVE '  ' TO GODK-STATUSKODER                                        
018726     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK626                       
018727     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018728     PERFORM IMS-STATUSKONTROLL                                           
018729     .                                                                    
018730     EJECT                                                                
018731 IMS-GU-D701 SECTION.                                                     
018732     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
018733            DELIMITED BY SIZE INTO SSA1                                   
018740     MOVE '  GE' TO GODK-STATUSKODER                                      
018750     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
018760     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
018770     PERFORM IMS-STATUSKONTROLL                                           
018780     .                                                                    
018790     SKIP2                                                                
018791 IMS-GNP-D702 SECTION.                                                    
018792     STRING 'WDD702  (FLTEXT   =N)'                                       
018793            DELIMITED BY SIZE INTO SSA1                                   
018794     MOVE '  GE' TO GODK-STATUSKODER                                      
018795     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
018796     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
018797     PERFORM IMS-STATUSKONTROLL                                           
018798     .                                                                    
018799     SKIP2                                                                
018800 IMS-RESTART SECTION.                                                     
018900     SKIP2                                                                
019000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019100     MOVE '  ' TO GODK-STATUSKODER                                        
019200     CALL CBLTDLI USING XRST MSG-PCB                                      
019300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019400                        CHKP-AREA-LENGTH CHKP-AREA                        
019500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019600     PERFORM IMS-STATUSKONTROLL                                           
019700     .                                                                    
019800     SKIP3                                                                
019900 IMS-CHECKPOINT SECTION.                                                  
020000     SKIP2                                                                
020100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020200     MOVE '  XD' TO GODK-STATUSKODER                                      
020300     CALL CBLTDLI USING CHKP MSG-PCB                                      
020400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020500                        CHKP-AREA-LENGTH CHKP-AREA                        
020600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020700     PERFORM IMS-STATUSKONTROLL                                           
020800                                                                          
020900     IF IMS-EJ-OK                                                         
021000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021100       DISPLAY FELTEXT                                                    
021200       CALL FELLOG                                                        
021300     END-IF                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 IMS-STATUSKONTROLL SECTION.                                              
021700     SKIP2                                                                
021800     SET STATUS-IX TO 1                                                   
021900     SEARCH GODK-STATUS                                                   
022000       AT END                                                             
022100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022200           DELIMITED BY SIZE INTO FELTEXT                                 
022300         DISPLAY FELTEXT                                                  
022400         CALL FELLOG                                                      
022500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022600         CONTINUE                                                         
022700     END-SEARCH                                                           
022800     .                                                                    
