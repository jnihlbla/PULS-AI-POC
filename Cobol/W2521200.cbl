000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2521200.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   19/04/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PS&L NEW PARTS INFORMATION                                       
000900*                                                                         
001000*        THE PROGRAM READS     WDC1                                       
001100*                              WDC9                                       
001200*                              WDD2                                       
001300*                              WDD7A                                      
001400*                              WDD9                                       
001500*                              WDK6                                       
001501*                              WDK7                                       
001510*                              WDK9                                       
001600*                              WDP3                                       
001610*                              WDP3A/B/C                                  
001700*                                                                         
001800*    ABENDCODES:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          ---                                                            
003100     SELECT W25211                     ASSIGN TO W25212D1.                
003200*          ---                                                            
003300     SELECT W25212                     ASSIGN TO W25212D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W25211                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W25209      -L.                                                
004400     EJECT                                                                
004500 FD  W25212                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004810*01  RECORD -COPY W25209 -PRE  OUT-  -L.                                  
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W2521200'.            
005310 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005320 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005400 77  YES                         PIC X       VALUE 'J'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
005510 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
005600 77  IX1                         PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77  MAX-IX1-DC                  PIC S9(9)   VALUE +6   COMP SYNC.        
005701 77  WS-IDPERSON                 PIC S9(3)   VALUE +0 COMP-3.             
005702 77  W-IDINK-ALPHA               PIC X(04)   VALUE SPACE.                 
005703 77  W-IDINK-NUM                 PIC 9(03)   VALUE ZERO.                  
005704 77  WS-IDLAND                   PIC X(2)    VALUE SPACE.                 
005705 01  WORK-AREA.                                                           
005706     03 W-TIAAAAVV.                                                       
005707       05 W-TISEKEL              PIC  9(2)   VALUE ZERO.                  
005708       05 W-TIAA                 PIC  9(2)   VALUE ZERO.                  
005709       05 W-TIVV                 PIC  9(2)   VALUE ZERO.                  
005710     03  W-DADISPIN              PIC  9(6)   VALUE ZERO.                  
005720     03  WS-TPO-NAESTA-INLEV     PIC S9(9)   VALUE ZERO COMP-3.           
005800                                                                          
005900 77  W25211-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W25211                       VALUE 'J'.                   
006100     EJECT                                                                
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007420     EJECT                                                                
007511*      --- VALID IDDC CODES                                               
007512*01    -COPY WWLNDKON                                                     
007513*01    -COPY WWDC99                                                       
007520     EJECT                                                                
007600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008100     SKIP2                                                                
008200 01  ERROR-TEXT.                                                          
008300     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     EJECT                                                                
008910*    --- PARAMETRAR TILL WDATKONV                                         
009000*01  -COPY WDATAREA                                                       
009100     EJECT                                                                
009110*01    -COPY WWDCKONS                                                     
009120     EJECT                                                                
009200 01  IN-AREA-START               PIC X(24)   VALUE                        
009300                                 'IN-AREA-START  '.                       
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W25209     -PRE IN-                                       
009700     EJECT                                                                
009710 01  OUT-AREA-START              PIC X(24)   VALUE                        
009720                                 'OUT-AREA-START  '.                      
009730     SKIP2                                                                
009740                                                                          
009750*01  AREA -COPY W25209     -PRE OUT-                                      
009760     EJECT                                                                
009800*    --- AREAS FOR IMS-SECTIONS                                           
009900*                                                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  KEYS-FOR-DLI.                                                        
010310   03  W-WDD7A1KY-MIN.                                                    
010320     05  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
010330     05  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
010340     05  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
010350                                                                          
010360   03  W-WDD7A1KY-MAX.                                                    
010370     05  W-IDARTNR-MAX7           PIC S9(9)  COMP-3 VALUE ZERO.           
010380     05  FILLER                 PIC S9(9) COMP-3 VALUE +999999999.        
010390     05  FILLER                   PIC S9(3)  COMP-3 VALUE +999.           
010391                                                                          
010400     03  W-IDARTNR-X.                                                     
010500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010600                                                                          
010700     03  W-IDDC-X.                                                        
010710         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
010720                                                                          
011800     03  W-KDARBTYP-X.                                                    
011900         05  W-KDARBTYP          PIC X(08)    VALUE SPACE.                
012000     03  W-IDPERSON-X.                                                    
012100         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
012200                                                                          
012210     03  W-WDC101KY-X.                                                    
012220         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
012230         05  W-IDMARKBO-111      PIC X       VALUE 'B'.                   
012240                                                                          
012250     03  W-WDD901KY-X.                                                    
012260         05 W-IDARTNR-901        PIC S9(9)   VALUE ZERO COMP-3.           
012270         05 W-IDDC-901           PIC X(2)    VALUE SPACE.                 
012271                                                                          
012280     03  W-IDLEVNR-X.                                                     
012290         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012291                                                                          
012292     03  W-IDLEVBSK-X.                                                    
012294         05  W-IDLEVBSK          PIC S9      VALUE +2   COMP-3.           
012296                                                                          
012297     03  W-DABEHOV-MIN-X.                                                 
012298         05  W-DABEHOV-MIN       PIC   9(6)  VALUE ZERO.                  
012299     03  W-DABEHOV-MAX-X.                                                 
012300         05  W-DABEHOV-MAX       PIC   9(6)  VALUE ZERO.                  
012301                                                                          
012302     03  W-WDP3A1-MIN.                                                    
012303       05 W-IDLAND-A-MIN         PIC X(2)    VALUE SPACE.                 
012304       05  W-IDARTNRF-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
012305       05  W-IDARTNRT-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
012306       05  W-KDARBTYP-A-MIN      PIC X(8)    VALUE SPACE.                 
012307     03  W-WDP3A1-MAX.                                                    
012308       05  W-IDLAND-A-MAX        PIC X(2)    VALUE SPACE.                 
012309       05  W-IDARTNRF-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
012310       05  W-IDARTNRT-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
012311       05  W-KDARBTYP-A-MAX      PIC X(8)    VALUE SPACE.                 
012312     03  W-WDP3B1-X.                                                      
012313         05  W-IDLAND-B          PIC X(2)    VALUE SPACE.                 
012314         05  W-IDLEVNR-B         PIC X(5)    VALUE SPACE.                 
012315         05  W-KDARBTYP-B        PIC X(8)    VALUE SPACE.                 
012316     03  W-WDP3C1-MIN.                                                    
012317       05  W-IDLAND-C-MIN        PIC X(2)    VALUE SPACE.                 
012318       05  W-IDFKNGRPF-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
012319       05  W-IDFKNGRPT-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
012320       05  W-KDARBTYP-C-MIN      PIC X(8)    VALUE SPACE.                 
012321     03  W-WDP3C1-MAX.                                                    
012322       05  W-IDLAND-C-MAX        PIC X(2)    VALUE SPACE.                 
012323       05  W-IDFKNGRPF-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
012324       05  W-IDFKNGRPT-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
012325       05  W-KDARBTYP-C-MAX      PIC X(8)    VALUE SPACE.                 
012330                                                                          
012331     03  W-W6GXKEY-X.                                                     
012332         05  FILLER              PIC X(4)    VALUE '6005'.                
012333         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
012334         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
012335                                                                          
012336     03  W-ADINLOMR-X.                                                    
012337         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
012338                                                                          
012339     03  W-WDGX2263-X.                                                    
012340         05  W-IDHTYP-2263       PIC X(4)    VALUE '2263'.                
012341         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
012342     03  W-TISOP-X.                                                       
012343         05  W-TISOP-2264        PIC S9(5)           COMP-3.              
012344     03  W-W2266KY-X.                                                     
012345         05  W-IDARTNR-2266      PIC S9(9)           COMP-3.              
012346         05  W-IDDC-2266         PIC X(2).                                
012348     EJECT                                                                
012350*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FOUND                       VALUE '  '.                  
012600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012800     SKIP2                                                                
012900 01  GOOD-STATUSCODES.                                                    
013000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(128).                              
013300 01  SSA2                        PIC X(128).                              
013310 01  SSA3                        PIC X(128).                              
013400     EJECT                                                                
013500*    --- IMS FUNCTION CODES                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7A1'.                      
014000 01  DLI-IO-WDD7A1.                                                       
014100*    03  -COPY WDD7A1 -PRE WDD7A1-                                        
014200     EJECT                                                                
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
014400 01  DLI-IO-WDD201.                                                       
014500*    03  -COPY WDD201                                                     
014600     EJECT                                                                
014602 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
014603 01  DLI-IO-WDK611.                                                       
014604*    03  -COPY WDK611                                                     
014605     EJECT                                                                
014606 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
014607 01  DLI-IO-WDK711.                                                       
014608*    03  -COPY WDK711                                                     
014609     EJECT                                                                
015810 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC101'.                      
015820 01  DLI-IO-WDC101.                                                       
015830*    03  -COPY WDC101 -PRE WDC1-                                          
015831     EJECT                                                                
015832 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
015833 01  DLI-IO-WDD901.                                                       
015834*    03  -COPY WDD901    -PRE LEV-                                        
015835     EJECT                                                                
015836 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
015837 01  DLI-IO-WDD902.                                                       
015838*    03  -COPY WDD902    -PRE LEV-                                        
015839     EJECT                                                                
015840 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
015841 01  DLI-IO-WDD924.                                                       
015842*    03  -COPY WDD924                                                     
015843     EJECT                                                                
015844 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
015845 01  DLI-IO-WDD925.                                                       
015846*    03  -COPY WDD925                                                     
015847     EJECT                                                                
015848 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
015849 01  DLI-IO-WDK901.                                                       
015850*    03  -COPY WDK901                                                     
015851     EJECT                                                                
015852 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK911'.                      
015853 01  DLI-IO-WDK911.                                                       
015854*    03  -COPY WDK911                                                     
015855     EJECT                                                                
015856 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3A'.         
015857 01  DLI-IO-WDP3A.                                                        
015858*    03  -COPY WDP3A1                                                     
015859     EJECT                                                                
015864 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP3B1'.                      
015865 01  DLI-IO-WDP3B.                                                        
015866*    03  -COPY WDP3B1.                                                    
015867     EJECT                                                                
015868 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3C'.         
015869 01  DLI-IO-WDP3C.                                                        
015870*    03  -COPY WDP3C1                                                     
015871     EJECT                                                                
015872 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
015873 01  DLI-IO-WDP311.                                                       
015874*    03  -COPY WDP311                                                     
015875     EJECT                                                                
015876 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2266'.                    
015877 01  DLI-IO-WDGX2266.                                                     
015878*    03  -COPY WDGX2266                                                   
015879     EJECT                                                                
015890                                                                          
015900 LINKAGE SECTION.                                                         
016100                                                                          
016200*01  -COPY W0008  -PRE WDD7A-                                             
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE WDD2-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016710*01  -COPY W0008  -PRE WDD9-                                              
016720     05  FILLER                  PIC X.                                   
016730     EJECT                                                                
016740*01  -COPY W0008  -PRE WDK6-                                              
016750     05  FILLER                  PIC X.                                   
016760     EJECT                                                                
016770*01  -COPY W0008  -PRE WDK7-                                              
016780     05  FILLER                  PIC X.                                   
016790     EJECT                                                                
017010*01  -COPY W0008  -PRE WDP3A-                                             
017020     05  FILLER                  PIC X.                                   
017030     EJECT                                                                
017040*01  -COPY W0008  -PRE WDP3B-                                             
017050     05  FILLER                  PIC X.                                   
017060     EJECT                                                                
017070*01  -COPY W0008  -PRE WDP3C-                                             
017080     05  FILLER                  PIC X.                                   
017090     EJECT                                                                
017091*01  -COPY W0008  -PRE WDP3-                                              
017092     05  FILLER                  PIC X.                                   
017093     EJECT                                                                
017100*01  -COPY W0008  -PRE WDC1-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017340*01  -COPY W0008  -PRE WDK9-                                              
017350     05  FILLER                  PIC X.                                   
017360     EJECT                                                                
017370*01  -COPY W0008  -PRE WDR2-                                              
017380     05  FILLER                  PIC X.                                   
017390     EJECT                                                                
017400 PROCEDURE DIVISION  USING WDD7A-PCB WDD2-PCB WDD9-PCB                    
017401                           WDK6-PCB  WDK7-PCB                             
017402                           WDP3A-PCB WDP3B-PCB WDP3C-PCB WDP3-PCB         
017411                           WDC1-PCB  WDK9-PCB WDR2-PCB.                   
017500 MAIN SECTION.                                                            
017600     ENTRY 'CBLTDLI' USING WDD7A-PCB WDD2-PCB WDD9-PCB                    
017601                           WDK6-PCB  WDK7-PCB                             
017603                           WDP3A-PCB WDP3B-PCB WDP3C-PCB WDP3-PCB         
017607                           WDC1-PCB  WDK9-PCB WDR2-PCB.                   
017700                                                                          
017800     PERFORM A-INIT                                                       
017900                                                                          
018000     PERFORM S01-READ-W25211                                              
018100     PERFORM UNTIL END-OF-W25211                                          
018200                                                                          
018300       PERFORM C-UPDATE-W25212                                            
018400                                                                          
018500       PERFORM S01-READ-W25211                                            
018600     END-PERFORM                                                          
018700                                                                          
018800                                                                          
018900     PERFORM Z-FINIT                                                      
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     OPEN INPUT  W25211                                                   
019710         OUTPUT  W25212                                                   
019800                                                                          
019900     ACCEPT TODAYS-DATE  FROM DATE                                        
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500     .                                                                    
021600     EJECT                                                                
021700 C-UPDATE-W25212 SECTION.                                                 
021800                                                                          
021900     MOVE IN-AREA                TO OUT-AREA                              
022000                                                                          
022200*    WDC1                                                                 
022201     MOVE IN-IDARTNR             TO W-IDARTNR-111                         
022210     PERFORM IMS-GU-WDC101                                                
022220     IF SEGMENT-FOUND                                                     
022230       MOVE WDC1-ART-TIUPPDAT    TO OUT-TIUPPDAT-BTO                      
022240     ELSE                                                                 
022241       MOVE +0                   TO OUT-TIUPPDAT-BTO                      
022250     END-IF                                                               
022400***  WDR2 - WDGX2266                                                      
022410     MOVE IN-TISOP               TO W-TISOP-2264                          
022420     MOVE IN-IDARTNR             TO W-IDARTNR-2266                        
022430     MOVE WC-CDC-SE              TO W-IDDC-2266                           
022440     PERFORM IMS-GU-WDGX2266                                              
022450     IF SEGMENT-FOUND                                                     
022460        MOVE 2266-TIMOTSI        TO OUT-TIMOTSI                           
022470     END-IF                                                               
022480                                                                          
022500     MOVE +1                     TO IX1                                   
022600     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
022601       MOVE +0                   TO OUT-TIMOTSI-SLAG(IX1)                 
022602       IF IN-IDDC-SLAG(IX1) NOT = SPACE                                   
022603          MOVE IN-TISOP          TO W-TISOP-2264                          
022610          MOVE IN-IDARTNR        TO W-IDARTNR-2266                        
022700          MOVE IN-IDDC-SLAG(IX1) TO W-IDDC-2266                           
022800          PERFORM IMS-GU-WDGX2266                                         
022900          IF SEGMENT-FOUND                                                
023000             MOVE 2266-TIMOTSI   TO OUT-TIMOTSI-SLAG(IX1)                 
023100          END-IF                                                          
023101       END-IF                                                             
023110       ADD +1                    TO IX1                                   
023200     END-PERFORM                                                          
023300                                                                          
023400***  WDD2                                                                 
023410     MOVE IN-IDARTNR             TO W-IDARTNR                             
023500     PERFORM IMS-GU-WDD201                                                
023600     IF SEGMENT-FOUND                                                     
023700        MOVE ART-FLPISK          TO OUT-FLPISK                            
023800        MOVE ART-FLUPG           TO OUT-FLUPG                             
023900        MOVE ART-KDTPD           TO OUT-KDTPD                             
024100     ELSE                                                                 
024101        MOVE SPACE               TO OUT-FLPISK                            
024102        MOVE SPACE               TO OUT-FLUPG                             
024103        MOVE SPACE               TO OUT-KDTPD                             
024110     END-IF                                                               
024200                                                                          
024300***  WDD7                                                                 
024310     MOVE +0                     TO OUT-IDARTNR-TILLK                     
024320     MOVE NOO                    TO OUT-FLERS                             
024400     MOVE IN-IDARTNR             TO W-IDARTNR-MIN7                        
024500                                    W-IDARTNR-MAX7                        
024600     PERFORM IMS-GU-WDD7-WDD7A-MINMAX                                     
024700     IF SEGMENT-FOUND                                                     
024800        IF WDD7A1-ERS-IDARTNR NOT = ZERO                                  
024810           MOVE WDD7A1-ERS-IDARTNR  TO OUT-IDARTNR-TILLK                  
024820           PERFORM IMS-GN-WDD7-WDD7A-MINMAX                               
024830           IF SEGMENT-FOUND                                               
024835              MOVE +0               TO OUT-IDARTNR-TILLK                  
024840              MOVE YES              TO OUT-FLERS                          
024860           END-IF                                                         
024870        END-IF                                                            
024910     END-IF                                                               
024912                                                                          
024920*** SS AVAIL IN STOCK CDC                                                 
024930     MOVE +0                    TO OUT-KVLS-SS                            
024940     MOVE +0                    TO OUT-KVRESS-SS                          
024950     MOVE +0                    TO OUT-KVROS-SS                           
024951                                                                          
024960     IF  OUT-IDARTNR-TILLK > +0                                           
024961     AND OUT-FLERS     = NOO                                              
024970       MOVE OUT-IDARTNR-TILLK   TO W-IDARTNR                              
024980       PERFORM IMS-GU-WDK611                                              
024990       IF SEGMENT-FOUND                                                   
024991         MOVE CLAG-KVLS         TO OUT-KVLS-SS                            
024992         MOVE CLAG-KVRESS       TO OUT-KVRESS-SS                          
024993         MOVE CLAG-KVROS        TO OUT-KVROS-SS                           
024994       END-IF                                                             
024995     END-IF                                                               
025000                                                                          
025010*** SS AVAIL IN STOCK SLAG                                                
025020     MOVE +1                    TO IX1                                    
025030     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
025040       MOVE +0                  TO OUT-KVLS-SS-SLAG      (IX1)            
025050       MOVE +0                  TO OUT-KVRESS-SS-SLAG    (IX1)            
025060       MOVE +0                  TO OUT-KVOKS-BULK-SS-SLAG(IX1)            
025070       MOVE +0                  TO OUT-KVOKS-DAG-SS-SLAG (IX1)            
025080       MOVE +0                  TO OUT-KVROS-BULK-SS-SLAG(IX1)            
025090       MOVE +0                  TO OUT-KVROS-DAG-SS-SLAG (IX1)            
025091       MOVE +0                  TO OUT-KVAKS-SDC-SS-SLAG (IX1)            
025092       IF OUT-IDARTNR-TILLK > +0                                          
025093         MOVE OUT-IDARTNR-TILLK TO W-IDARTNR                              
025094         MOVE IN-IDDC-SLAG(IX1) TO W-IDDC                                 
025097         PERFORM IMS-GU-WDK711                                            
025098         IF SEGMENT-FOUND                                                 
025099           MOVE SLAG-KVLS       TO OUT-KVLS-SS-SLAG      (IX1)            
025100           MOVE SLAG-KVRESS     TO OUT-KVRESS-SS-SLAG    (IX1)            
025101           MOVE SLAG-KVOKS-BULK TO OUT-KVOKS-BULK-SS-SLAG(IX1)            
025102           MOVE SLAG-KVOKS-DAG  TO OUT-KVOKS-DAG-SS-SLAG (IX1)            
025103           MOVE SLAG-KVROS-BULK TO OUT-KVROS-BULK-SS-SLAG(IX1)            
025104           MOVE SLAG-KVROS-DAG  TO OUT-KVROS-DAG-SS-SLAG (IX1)            
025105           MOVE SLAG-KVAKS-SDC  TO OUT-KVAKS-SDC-SS-SLAG (IX1)            
025113         END-IF                                                           
025114       END-IF                                                             
025115       ADD +1                   TO IX1                                    
025116     END-PERFORM                                                          
025117                                                                          
025118***  WDD9                                                                 
025131     MOVE ZERO                     TO OUT-TILEVBSK-DISP                   
025132     MOVE IN-IDARTNR               TO W-IDARTNR-901                       
025135     MOVE WC-CDC-SE                TO W-IDDC-901                          
025136     MOVE IN-IDLEVNR               TO W-IDLEVNR                           
025137     PERFORM IMS-GU-WDD924                                                
025138     IF SEGMENT-FOUND                                                     
025139       MOVE LEV-TILEVBSK-DISP      TO OUT-TILEVBSK-DISP                   
025140     END-IF                                                               
025141                                                                          
025142     MOVE SPACE                    TO OUT-TELEVBSK                        
025143     IF IN-IDLEVNR NOT = SPACE                                            
025144       MOVE IN-IDARTNR             TO W-IDARTNR-901                       
025145       MOVE WC-CDC-SE              TO W-IDDC-901                          
025146       MOVE IN-IDLEVNR             TO W-IDLEVNR                           
025150       PERFORM IMS-GU-WDD925                                              
025151       IF SEGMENT-FOUND                                                   
025152         MOVE INFO-TELEVBSK        TO OUT-TELEVBSK                        
025154       END-IF                                                             
025155     END-IF                                                               
025156                                                                          
025157     MOVE +1                       TO IX1                                 
025158     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
025159       MOVE +0                     TO OUT-TILEVBSK-DISP-SLAG(IX1)         
025160       MOVE SPACE                  TO OUT-TELEVBSK-SLAG(IX1)              
025161       IF IN-IDDC-SLAG(IX1) NOT = SPACE                                   
025162         MOVE IN-IDARTNR           TO W-IDARTNR-901                       
025163         MOVE IN-IDDC-SLAG(IX1)    TO W-IDDC-901                          
025164         MOVE IN-IDLEVNR-SLAG(IX1) TO W-IDLEVNR                           
025170         PERFORM IMS-GU-WDD924                                            
025180         IF SEGMENT-FOUND                                                 
025191           MOVE LEV-TILEVBSK-DISP  TO OUT-TILEVBSK-DISP-SLAG(IX1)         
025197         END-IF                                                           
025198                                                                          
025199         IF IN-IDLEVNR-SLAG(IX1) NOT = SPACE                              
025200           MOVE IN-IDARTNR           TO W-IDARTNR-901                     
025201           MOVE IN-IDDC-SLAG(IX1)    TO W-IDDC-901                        
025202           MOVE IN-IDLEVNR-SLAG(IX1) TO W-IDLEVNR                         
025206           PERFORM IMS-GU-WDD925                                          
025207           IF SEGMENT-FOUND                                               
025208             MOVE INFO-TELEVBSK    TO OUT-TELEVBSK-SLAG(IX1)              
025210           END-IF                                                         
025211         END-IF                                                           
025212       END-IF                                                             
025213                                                                          
025214       ADD +1                      TO IX1                                 
025215     END-PERFORM                                                          
025220                                                                          
025300***  WDK9                                                                 
025301     MOVE +0                       TO OUT-KVOKS-BULK                      
025302     MOVE +0                       TO OUT-KVOKS-DAG                       
025303     MOVE +0                       TO OUT-KVOKS-VOR                       
025304     MOVE +0                       TO OUT-SUTPO-TOT                       
025305     MOVE +0                       TO WS-TPO-NAESTA-INLEV                 
025306     MOVE IN-IDARTNR               TO W-IDARTNR                           
025307     PERFORM IMS-GU-WDK901                                                
025308     IF SEGMENT-FOUND                                                     
025309       MOVE ART-KVOKS-BULK         TO OUT-KVOKS-BULK                      
025310       MOVE ART-KVOKS-DAG          TO OUT-KVOKS-DAG                       
025311       MOVE ART-KVOKS-VOR          TO OUT-KVOKS-VOR                       
025320                                                                          
025321       IF IN-TIDISPIN = ZERO                                              
025322         MOVE ART-SUTPO-TOT        TO WS-TPO-NAESTA-INLEV                 
025324       ELSE                                                               
025326         MOVE ZERO                 TO W-DABEHOV-MIN                       
025327         PERFORM CA-KONV-TIDISPIN                                         
025328         MOVE W-DADISPIN           TO W-DABEHOV-MAX                       
025329         PERFORM IMS-GNP-WDK911                                           
025330         PERFORM UNTIL SEGMENT-MISSING                                    
025331           COMPUTE WS-TPO-NAESTA-INLEV = WS-TPO-NAESTA-INLEV              
025332                                       + ANT-SUTPO-PB                     
025333                                       + ANT-SUTPO-EJPB                   
025334           PERFORM IMS-GNP-WDK911                                         
025335         END-PERFORM                                                      
025337       END-IF                                                             
025338       MOVE WS-TPO-NAESTA-INLEV    TO OUT-SUTPO-TOT                       
025339     END-IF                                                               
025400                                                                          
026301***  WDP3 - NAME IDANSK - CDC                                             
026302     MOVE SPACE                 TO OUT-IDNAMN-IDANSK                      
026303     IF IN-IDANSK NOT = +0                                                
026304        MOVE 'ANSK'             TO W-KDARBTYP                             
026305        MOVE IN-IDANSK          TO W-IDPERSON                             
026306        PERFORM IMS-GET-WDP3-NAME                                         
026307        IF SEGMENT-FOUND                                                  
026308        AND PERS-IDNAMN NOT = SPACE                                       
026309            MOVE PERS-IDNAMN    TO OUT-IDNAMN-IDANSK                      
026310        END-IF                                                            
026311     END-IF                                                               
026312                                                                          
026313***  WDP3 - NAME IDANSK - FORP                                            
026315     MOVE SPACE                 TO OUT-IDNAMN-FORP                        
026317                                                                          
026318     MOVE NOO                    TO SW-TRAEFF                             
026319                                                                          
026320     MOVE LOW-VALUE              TO W-WDP3A1-MIN                          
026321                                    W-WDP3C1-MIN                          
026322     MOVE HIGH-VALUE             TO W-WDP3A1-MAX                          
026323                                    W-WDP3C1-MAX                          
026324     IF NDC-CN                                                            
026325        MOVE WC-LAND-CN          TO WS-IDLAND                             
026326     ELSE                                                                 
026327        IF NDC-US                                                         
026328           MOVE WC-LAND-US       TO WS-IDLAND                             
026329        ELSE                                                              
026330           MOVE WC-LAND-SE       TO WS-IDLAND                             
026331        END-IF                                                            
026332     END-IF                                                               
026333                                                                          
026334     MOVE WS-IDLAND              TO W-IDLAND-A-MIN                        
026335                                    W-IDLAND-A-MAX                        
026336                                    W-IDLAND-B                            
026337                                    W-IDLAND-C-MIN                        
026338                                    W-IDLAND-C-MAX                        
026339                                                                          
026340     MOVE 'CDC'                  TO W-KDARBTYP                            
026341                                    W-KDARBTYP-B                          
026342                                                                          
026343     PERFORM IMS-GU-WDP3A                                                 
026345     PERFORM UNTIL SEGMENT-MISSING OR SW-TRAEFF = YES                     
026348       IF SEQA-IDARTNR-TOM < IN-IDARTNR                                   
026349         PERFORM IMS-GN-WDP3A                                             
026350       ELSE                                                               
026354         IF SEQA-IDARTNR-FOM <= IN-IDARTNR AND                            
026355            SEQA-IDARTNR-TOM >= IN-IDARTNR                                
026356           MOVE YES              TO SW-TRAEFF                             
026357         ELSE                                                             
026358           MOVE 'GE'             TO STATUS-WS                             
026359         END-IF                                                           
026360       END-IF                                                             
026361     END-PERFORM                                                          
026362                                                                          
026364     IF SW-TRAEFF = YES                                                   
026365       MOVE SEQA-IDPERSON        TO WS-IDPERSON                           
026366     ELSE                                                                 
026367       MOVE IN-IDLEVNR           TO W-IDLEVNR-B                           
026368       PERFORM IMS-GU-WDP3B                                               
026369       IF SEGMENT-FOUND                                                   
026370         MOVE YES                TO SW-TRAEFF                             
026371         MOVE SEQB-IDPERSON      TO WS-IDPERSON                           
026372       ELSE                                                               
026373         PERFORM IMS-GU-WDP3C                                             
026374         PERFORM UNTIL SEGMENT-MISSING OR SW-TRAEFF = YES                 
026375           IF SEQC-IDFKNGRP-TOM < IN-IDFKNGRP                             
026376             PERFORM IMS-GN-WDP3C                                         
026377           ELSE                                                           
026378             IF SEQC-IDFKNGRP-FOM <= IN-IDFKNGRP AND                      
026379                SEQC-IDFKNGRP-TOM >= IN-IDFKNGRP                          
026380               MOVE YES          TO SW-TRAEFF                             
026381             ELSE                                                         
026382               MOVE 'GE'         TO STATUS-WS                             
026383             END-IF                                                       
026384           END-IF                                                         
026385         END-PERFORM                                                      
026386         IF SW-TRAEFF = YES                                               
026387           MOVE SEQC-IDPERSON    TO WS-IDPERSON                           
026388         END-IF                                                           
026389       END-IF                                                             
026390     END-IF                                                               
026391                                                                          
026393     IF SW-TRAEFF = YES                                                   
026394       MOVE WS-IDPERSON          TO W-IDPERSON                            
026395       PERFORM IMS-GET-WDP3-NAME                                          
026396       IF SEGMENT-FOUND                                                   
026397         MOVE PERS-IDNAMN        TO OUT-IDNAMN-FORP                       
026398       END-IF                                                             
026399     END-IF                                                               
026400                                                                          
026441***  WDP3 - NAME IDINK - CDC                                              
026442     MOVE SPACE                 TO OUT-IDNAMN-IDINK                       
026443     IF IN-IDINK NOT = SPACE                                              
026444        MOVE 'INK'              TO W-KDARBTYP                             
026445        MOVE ZERO               TO W-IDINK-NUM                            
026446        IF IN-IDINK(3:1) = SPACE                                          
026447       AND IN-IDINK(2:1) = SPACE                                          
026450           MOVE IN-IDINK(3:1)   TO W-IDINK-NUM(3:1)                       
026460        ELSE                                                              
026470          IF IN-IDINK(3:1) = SPACE                                        
026480             MOVE IN-IDINK(2:1) TO W-IDINK-NUM(3:1)                       
026490             MOVE IN-IDINK(1:1) TO W-IDINK-NUM(2:1)                       
026491          ELSE                                                            
026492             MOVE IN-IDINK(1:3) TO W-IDINK-NUM                            
026493          END-IF                                                          
026494        END-IF                                                            
026495        MOVE W-IDINK-NUM        TO W-IDPERSON                             
026496        PERFORM IMS-GET-WDP3-NAME                                         
026497        IF SEGMENT-FOUND                                                  
026498        AND PERS-IDNAMN NOT = SPACE                                       
026499            MOVE PERS-IDNAMN    TO OUT-IDNAMN-IDINK                       
026500        END-IF                                                            
026501     END-IF                                                               
026502                                                                          
026510*** WDP3 - NAME IDANSK - SLAG                                             
026600     MOVE +1                    TO IX1                                    
026700     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
026701       MOVE SPACE               TO OUT-IDNAMN-IDANSK-SLAG(IX1)            
026710       IF IN-IDANSK-SLAG(IX1) NOT = +0                                    
026720         MOVE 'ANSK'            TO W-KDARBTYP                             
026800         MOVE IN-IDANSK-SLAG(IX1) TO W-IDPERSON                           
026900         PERFORM IMS-GET-WDP3-NAME                                        
027000         IF SEGMENT-FOUND                                                 
027100        AND PERS-IDNAMN NOT = SPACE                                       
027200           MOVE PERS-IDNAMN     TO OUT-IDNAMN-IDANSK-SLAG(IX1)            
027300         END-IF                                                           
027310       END-IF                                                             
027400       ADD +1                   TO IX1                                    
027500     END-PERFORM                                                          
027600                                                                          
027700***  WDP3 - NAME IDINK - SLAG                                             
027900     MOVE +1                         TO IX1                               
028000     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
028001       MOVE SPACE                    TO OUT-IDNAMN-IDINK-SLAG(IX1)        
028017       IF IN-IDINK-SLAG(IX1) NOT = SPACE                                  
028020         MOVE 'INK'                  TO W-KDARBTYP                        
028040         MOVE ZERO                   TO W-IDINK-NUM                       
028041         MOVE IN-IDINK-SLAG(IX1)     TO W-IDINK-ALPHA                     
028050         IF W-IDINK-ALPHA(3:1) = SPACE                                    
028060        AND W-IDINK-ALPHA(2:1) = SPACE                                    
028070           MOVE W-IDINK-ALPHA(3:1)   TO W-IDINK-NUM(3:1)                  
028080         ELSE                                                             
028090           IF W-IDINK-ALPHA(3:1) = SPACE                                  
028091             MOVE W-IDINK-ALPHA(2:1) TO W-IDINK-NUM(3:1)                  
028092             MOVE W-IDINK-ALPHA(1:1) TO W-IDINK-NUM(2:1)                  
028093           ELSE                                                           
028094             MOVE W-IDINK-ALPHA(1:3) TO W-IDINK-NUM                       
028095           END-IF                                                         
028096         END-IF                                                           
028097         MOVE W-IDINK-NUM            TO W-IDPERSON                        
028200         PERFORM IMS-GET-WDP3-NAME                                        
028300         IF SEGMENT-FOUND                                                 
028400        AND PERS-IDNAMN NOT = SPACE                                       
028500           MOVE PERS-IDNAMN          TO OUT-IDNAMN-IDINK-SLAG(IX1)        
028600         END-IF                                                           
028610       END-IF                                                             
028700       ADD +1                        TO IX1                               
028800     END-PERFORM                                                          
028900                                                                          
028946*** PACK AVAIL IN STOCK CDC                                               
028947     MOVE +0                    TO OUT-KVLS-EMBQ0                         
028948     MOVE +0                    TO OUT-KVRESS-EMBQ0                       
028949     MOVE +0                    TO OUT-KVROS-EMBQ0                        
028950     IF IN-IDARTNR-EMBQ0 > +0                                             
028951        MOVE IN-IDARTNR-EMBQ0   TO W-IDARTNR                              
028952        PERFORM IMS-GU-WDK611                                             
028953        IF SEGMENT-FOUND                                                  
028954           MOVE CLAG-KVLS       TO OUT-KVLS-EMBQ0                         
028955           MOVE CLAG-KVRESS     TO OUT-KVRESS-EMBQ0                       
028956           MOVE CLAG-KVROS      TO OUT-KVROS-EMBQ0                        
028957        END-IF                                                            
028958     END-IF                                                               
028959                                                                          
028960*** PACK AVAIL IN STOCK SLAG                                              
028961     MOVE +1                    TO IX1                                    
028962     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
028963       MOVE +0                  TO OUT-KVLS-EMBQ0-SLAG      (IX1)         
028964       MOVE +0                  TO OUT-KVRESS-EMBQ0-SLAG    (IX1)         
028965       MOVE +0                  TO OUT-KVOKS-BULK-EMBQ0-SLAG(IX1)         
028966       MOVE +0                  TO OUT-KVOKS-DAG-EMBQ0-SLAG (IX1)         
028967       MOVE +0                  TO OUT-KVROS-BULK-EMBQ0-SLAG(IX1)         
028968       MOVE +0                  TO OUT-KVROS-DAG-EMBQ0-SLAG (IX1)         
028969       MOVE +0                  TO OUT-KVAKS-SDC-EMBQ0-SLAG (IX1)         
028970       IF IN-IDARTNR-EMBQ0-SLAG(IX1) > +0                                 
028971         MOVE IN-IDARTNR-EMBQ0-SLAG(IX1) TO W-IDARTNR                     
028972         MOVE IN-IDDC-SLAG(IX1)  TO W-IDDC                                
028976         PERFORM IMS-GU-WDK711                                            
028977         IF SEGMENT-FOUND                                                 
028990           MOVE SLAG-KVLS       TO OUT-KVLS-EMBQ0-SLAG      (IX1)         
028991           MOVE SLAG-KVRESS     TO OUT-KVRESS-EMBQ0-SLAG    (IX1)         
028992           MOVE SLAG-KVOKS-BULK TO OUT-KVOKS-BULK-EMBQ0-SLAG(IX1)         
028993           MOVE SLAG-KVOKS-DAG  TO OUT-KVOKS-DAG-EMBQ0-SLAG (IX1)         
028994           MOVE SLAG-KVROS-BULK TO OUT-KVROS-BULK-EMBQ0-SLAG(IX1)         
028995           MOVE SLAG-KVROS-DAG  TO OUT-KVROS-DAG-EMBQ0-SLAG (IX1)         
028996           MOVE SLAG-KVAKS-SDC  TO OUT-KVAKS-SDC-EMBQ0-SLAG (IX1)         
028997         END-IF                                                           
028998       END-IF                                                             
028999       ADD +1               TO IX1                                        
029000     END-PERFORM                                                          
029001                                                                          
029002     PERFORM S11-WRITE-W25212                                             
029010     .                                                                    
029100     EJECT                                                                
029110 CA-KONV-TIDISPIN SECTION.                                                
029120                                                                          
029130     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
029140     MOVE IN-TIDISPIN      TO DAT-I-TIDATUM                               
029150     CALL WDATKONV      USING DAT-KDDATFORM                               
029160                              DAT-I-TIDATUM                               
029170                              DAT-O-TIDATUM                               
029180                              DAT-KDSVAR                                  
029190     IF DAT-KDSVAR-OK                                                     
029191       MOVE DAT-TIAA-VECKA TO W-TIAA                                      
029192       MOVE DAT-TIVV       TO W-TIVV                                      
029193       MOVE DAT-TISEKEL    TO W-TISEKEL                                   
029194       MOVE W-TIAAAAVV     TO W-DADISPIN                                  
029195     ELSE                                                                 
029196       MOVE ZERO           TO W-DADISPIN                                  
029197     END-IF                                                               
029198     .                                                                    
029199     EJECT                                                                
029218                                                                          
029220 Z-FINIT SECTION.                                                         
029300     CLOSE W25211                                                         
029310           W25212                                                         
029320                                                                          
029500     MOVE 'S' TO POSTSUM-OPKOD                                            
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     .                                                                    
029800     EJECT                                                                
029900 S01-READ-W25211  SECTION.                                                
030000     READ W25211 INTO IN-AREA                                             
030100     AT END                                                               
030200        MOVE HIGH-VALUE TO IN-AREA                                        
030300        SET END-OF-W25211 TO TRUE                                         
030400                                                                          
030500     NOT AT END                                                           
030600        MOVE 'W25211'   TO POSTSUM-FDNAMN                                 
030700        MOVE 'W25212D1' TO POSTSUM-DDNAMN2                                
030800*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
030900        MOVE SPACE      TO POSTSUM-TRANSTYP                               
031000        CALL POSTSUM USING POSTSUM-PARM                                   
031100     END-READ                                                             
031200     .                                                                    
031300     EJECT                                                                
031310 S11-WRITE-W25212 SECTION.                                                
031320                                                                          
031330     WRITE OUT-RECORD FROM OUT-AREA                                       
031340                                                                          
031350     MOVE 'IN'       TO POSTSUM-TRANSTYP                                  
031360     MOVE 'W25212'   TO POSTSUM-FDNAMN                                    
031370     MOVE 'W25212D2' TO POSTSUM-DDNAMN2                                   
031380     CALL POSTSUM USING POSTSUM-PARM                                      
031390     .                                                                    
031391     EJECT                                                                
031400 S99-ABEND SECTION.                                                       
031500                                                                          
031600     SKIP2                                                                
031700     MOVE 'S' TO POSTSUM-OPKOD                                            
031800     CALL POSTSUM USING POSTSUM-PARM                                      
031900     CALL ABEND USING RKOD-ABEND                                          
032000     .                                                                    
032100     EJECT                                                                
032200* --- IMS SECTIONS  ---                                                   
032300                                                                          
032400     EJECT                                                                
032500 IMS-GU-WDD201     SECTION.                                               
032510     MOVE 'IMS-GU-WDD201        ' TO DBS-SECTION                          
032520                                                                          
032600     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
032700          DELIMITED BY SIZE INTO SSA1                                     
032800     MOVE '  GE' TO GOOD-STATUSCODES                                      
032900     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
033000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSCHECK                                              
033200     .                                                                    
033300                                                                          
033310 IMS-GU-WDD7-WDD7A-MINMAX SECTION.                                        
033311     MOVE 'IMS-GU-WDD7-WDD7A-MINMAX' TO DBS-SECTION                       
033320                                                                          
033330     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
033340                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
033350            DELIMITED BY SIZE INTO SSA1                                   
033360     MOVE '  GE'            TO GOOD-STATUSCODES                           
033370     CALL CBLTDLI USING GU WDD7A-PCB DLI-IO-WDD7A1 SSA1                   
033380     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
033391     PERFORM IMS-STATUSCHECK                                              
033392     .                                                                    
033393                                                                          
033394 IMS-GN-WDD7-WDD7A-MINMAX SECTION.                                        
033395     MOVE 'IMS-GN-WDD7-WDD7A-MINMAX' TO DBS-SECTION                       
033396                                                                          
033397     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
033398                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
033399            DELIMITED BY SIZE INTO SSA1                                   
033400     MOVE '  GE'            TO GOOD-STATUSCODES                           
033401     CALL CBLTDLI USING GN WDD7A-PCB DLI-IO-WDD7A1 SSA1                   
033402     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
033403     PERFORM IMS-STATUSCHECK                                              
033405     .                                                                    
033406                                                                          
034535 IMS-GU-WDD901 SECTION.                                                   
034536     MOVE 'IMS-GU-WDD901 '  TO DBS-SECTION                                
034537                                                                          
034538     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
034539             DELIMITED BY SIZE INTO SSA1                                  
034540     MOVE '  GE'                 TO GOOD-STATUSCODES                      
034541     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
034542     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
034543     PERFORM IMS-STATUSCHECK                                              
034544     .                                                                    
034545                                                                          
034546 IMS-GNP-WDD902 SECTION.                                                  
034547     MOVE 'IMS-GNP-WDD902'  TO DBS-SECTION                                
034549                                                                          
034550     MOVE 'WDD902   '            TO SSA1                                  
034551     MOVE '  GE'                 TO GOOD-STATUSCODES                      
034552     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
034553     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
034554     PERFORM IMS-STATUSCHECK                                              
034555     .                                                                    
034556                                                                          
034557 IMS-GU-WDD924 SECTION.                                                   
034558     MOVE 'IMS-GU-WDD924 '  TO DBS-SECTION                                
034559                                                                          
034560     MOVE SPACE  TO SSA1 SSA2 SSA3                                        
034561     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
034562          DELIMITED BY SIZE INTO SSA1                                     
034563     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
034564          DELIMITED BY SIZE INTO SSA2                                     
034565     MOVE 'WDD924'            TO SSA3                                     
034566     MOVE '  GE' TO GOOD-STATUSCODES                                      
034567     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD924 SSA1 SSA2 SSA3          
034568     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
034569     PERFORM IMS-STATUSCHECK                                              
034570     .                                                                    
034571                                                                          
034572 IMS-GU-WDD925 SECTION.                                                   
034573     MOVE 'IMS-GU-WDD925 '  TO DBS-SECTION                                
034574                                                                          
034575     MOVE SPACE  TO SSA1 SSA2 SSA3                                        
034576     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
034577          DELIMITED BY SIZE INTO SSA1                                     
034578     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
034579          DELIMITED BY SIZE INTO SSA2                                     
034580     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
034581          DELIMITED BY SIZE INTO SSA3                                     
034582     MOVE '  GE' TO GOOD-STATUSCODES                                      
034583     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3          
034584     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
034585     PERFORM IMS-STATUSCHECK                                              
034586     .                                                                    
034587                                                                          
034588 IMS-GU-WDC101 SECTION.                                                   
034589     MOVE 'IMS-GU-WDC101        ' TO DBS-SECTION                          
034590     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
034591          DELIMITED BY SIZE INTO SSA1                                     
034592     MOVE '  GE' TO GOOD-STATUSCODES                                      
034593     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
034594     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
034595     PERFORM IMS-STATUSCHECK                                              
034596     .                                                                    
034597                                                                          
034598 IMS-GU-WDK611     SECTION.                                               
034599     MOVE 'IMS-GU-WDK611  '  TO DBS-SECTION                               
034600                                                                          
034601     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
034602       DELIMITED BY SIZE INTO SSA1                                        
034603     MOVE 'WDK611   '      TO SSA2                                        
034604     MOVE '  GE'           TO GOOD-STATUSCODES                            
034605     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
034606     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
034607     PERFORM IMS-STATUSCHECK                                              
034608     .                                                                    
034609                                                                          
034610 IMS-GU-WDK711    SECTION.                                                
034611     MOVE 'IMS-GU-WDK711  '  TO DBS-SECTION                               
034612                                                                          
034613     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
034614          DELIMITED BY SIZE INTO SSA1                                     
034615     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
034616          DELIMITED BY SIZE INTO SSA2                                     
034617     MOVE '  GE' TO GOOD-STATUSCODES                                      
034618     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
034619     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
034620     PERFORM IMS-STATUSCHECK                                              
034621     .                                                                    
034622                                                                          
034624 IMS-GU-WDGX2266 SECTION.                                                 
034625     MOVE 'IMS-GU-WDGX2266      ' TO DBS-SECTION                          
034626                                                                          
034627     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
034628          DELIMITED BY SIZE INTO SSA1                                     
034629     STRING 'WDGX2264(TISOP    =' W-TISOP-X ')'                           
034630          DELIMITED BY SIZE INTO SSA2                                     
034631     STRING 'WDGX2266(KY2266   =' W-W2266KY-X ')'                         
034632          DELIMITED BY SIZE INTO SSA3                                     
034635     MOVE '  GE'              TO GOOD-STATUSCODES                         
034636     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2266 SSA1                  
034637                                                    SSA2                  
034638                                                    SSA3                  
034639     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
034640     PERFORM IMS-STATUSCHECK                                              
034641     .                                                                    
034650                                                                          
036900                                                                          
036910 IMS-GU-WDK901          SECTION.                                          
036911     MOVE 'IMS-GU-WDK901        ' TO DBS-SECTION                          
036912                                                                          
036913     MOVE SPACES    TO SSA1 SSA2 SSA3                                     
036920     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
036940            DELIMITED BY SIZE INTO SSA1                                   
036950     MOVE '  GE' TO GOOD-STATUSCODES                                      
036960     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
036970     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
036971     PERFORM IMS-STATUSCHECK                                              
036990     .                                                                    
036991                                                                          
036992 IMS-GNP-WDK911         SECTION.                                          
036993     MOVE 'IMS-GNP-WDK911       ' TO DBS-SECTION                          
036994                                                                          
036995     MOVE SPACES    TO SSA1 SSA2 SSA3                                     
036996     STRING 'WDK911  (DABEHOV >=' W-DABEHOV-MIN-X                         
036997                    '&DABEHOV <=' W-DABEHOV-MAX-X ')'                     
036998            DELIMITED BY SIZE INTO SSA1                                   
036999     MOVE '  GE' TO GOOD-STATUSCODES                                      
037000     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-WDK911 SSA1                   
037001     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
037002     PERFORM IMS-STATUSCHECK                                              
037003     .                                                                    
037004                                                                          
037005 IMS-GET-WDP3-NAME SECTION.                                               
037006     MOVE 'IMS-GET-WDP3-NAME' TO DBS-SECTION                              
037007                                                                          
037008     MOVE SPACE               TO SSA1                                     
037009                                 SSA2                                     
037010     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
037011          DELIMITED BY SIZE INTO SSA1                                     
037012     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
037013          DELIMITED BY SIZE INTO SSA2                                     
037014     MOVE '  GE' TO GOOD-STATUSCODES                                      
037015     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
037016     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
037017     PERFORM IMS-STATUSCHECK                                              
037018     .                                                                    
037019                                                                          
037020 IMS-GU-WDP3A SECTION.                                                    
037021     MOVE 'IMS-GU-WDP3A         ' TO DBS-SECTION                          
037022     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
037023                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
037024                    '&KDARBTYP =' W-KDARBTYP ')'                          
037025       DELIMITED BY SIZE INTO SSA1                                        
037026     MOVE '  GE'                 TO GOOD-STATUSCODES                      
037027     CALL CBLTDLI             USING GU WDP3A-PCB                          
037028                                    DLI-IO-WDP3A SSA1                     
037029     MOVE WDP3A-STATUS-CODE      TO STATUS-WS                             
037030     PERFORM IMS-STATUSCHECK                                              
037031     .                                                                    
037032                                                                          
037033 IMS-GN-WDP3A SECTION.                                                    
037034     MOVE 'IMS-GN-WDP3A         ' TO DBS-SECTION                          
037035     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
037036                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
037037                    '&KDARBTYP =' W-KDARBTYP ')'                          
037038       DELIMITED BY SIZE INTO SSA1                                        
037039     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
037040     CALL CBLTDLI             USING GN WDP3A-PCB                          
037041                                    DLI-IO-WDP3A SSA1                     
037042     MOVE WDP3A-STATUS-CODE      TO STATUS-WS                             
037043     PERFORM IMS-STATUSCHECK                                              
037044     .                                                                    
037045     SKIP2                                                                
037046 IMS-GU-WDP3B SECTION.                                                    
037047     MOVE 'IMS-GU-WDP3B         ' TO DBS-SECTION                          
037048     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X                              
037049                    '&KDARBTYP =' W-KDARBTYP ')'                          
037050       DELIMITED BY SIZE INTO SSA1                                        
037051     MOVE '  GE'                 TO GOOD-STATUSCODES                      
037052     CALL CBLTDLI             USING GU WDP3B-PCB                          
037053                                    DLI-IO-WDP3B SSA1                     
037054     MOVE WDP3B-STATUS-CODE      TO STATUS-WS                             
037055     PERFORM IMS-STATUSCHECK                                              
037056     .                                                                    
037057                                                                          
037058 IMS-GU-WDP3C SECTION.                                                    
037059     MOVE 'IMS-GU-WDP3C         ' TO DBS-SECTION                          
037060     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
037061                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
037062                    '&KDARBTYP =' W-KDARBTYP ')'                          
037063       DELIMITED BY SIZE INTO SSA1                                        
037064     MOVE '  GE'                 TO GOOD-STATUSCODES                      
037065     CALL CBLTDLI             USING GU WDP3C-PCB                          
037066                                    DLI-IO-WDP3C SSA1                     
037067     MOVE WDP3C-STATUS-CODE      TO STATUS-WS                             
037068     PERFORM IMS-STATUSCHECK                                              
037069     .                                                                    
037070     SKIP2                                                                
037071 IMS-GN-WDP3C SECTION.                                                    
037072     MOVE 'IMS-GN-WDP3C         ' TO DBS-SECTION                          
037073     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
037074                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
037075                    '&KDARBTYP =' W-KDARBTYP ')'                          
037076       DELIMITED BY SIZE INTO SSA1                                        
037077     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
037078     CALL CBLTDLI             USING GN WDP3C-PCB                          
037079                                    DLI-IO-WDP3C SSA1                     
037080     MOVE WDP3C-STATUS-CODE      TO STATUS-WS                             
037081     PERFORM IMS-STATUSCHECK                                              
037082     .                                                                    
037090                                                                          
037091 IMS-STATUSCHECK SECTION.                                                 
037100                                                                          
037200     SET STATUS-IX TO 1                                                   
037300     SEARCH GOOD-STATUS                                                   
037400       AT END                                                             
037500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037600           DELIMITED BY SIZE INTO ERROR-TEXT                              
037700         DISPLAY ERROR-TEXT                                               
037800         CALL FELLOG                                                      
037900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
038000         CONTINUE                                                         
038100     END-SEARCH                                                           
038200     .                                                                    
