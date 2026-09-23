001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W6034600.                                                
001500 AUTHOR.         MARTIEN HOMPES.                                          
001600 DATE-WRITTEN.   97/04/03.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        ADD LOCATIONS                                                    
002100*                                                                         
002210*        THE PROGRAM READS     WLLOCA (WDJ8)                              
002220*        THE PROGRAM READS     WDK6E1 (WDK6E)                             
002230*        THE PROGRAM READS     WLARTD (WDD8A)                             
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: W6T346                                              
002600*        MID:         W6I34601                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        MOD:         W6O34601                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6034600'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  IO-COUNT                   PIC S9(4)  VALUE +0    COMP SYNC.         
004401 77  MAX-IO-COUNT               PIC S9(4)  VALUE +3    COMP SYNC.         
004420 77  LNG-P-TO-P-PREFIX          PIC S9(4)  VALUE +17   COMP SYNC.         
004430*    --- INDEX FOR SCROLL LINES                                           
004440 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004450 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004510                                                                          
004610 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
004620 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
004630 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
004631 77  WS-ADGANG                   PIC 9(2)   VALUE ZERO.                   
004651 77  WS-ADSEC11-FOM              PIC 9(3)   VALUE ZERO.                   
004652 77  WS-ADSEC11-TOM              PIC 9(3)   VALUE ZERO.                   
004653 77  WS-ADSEC11                  PIC 9(3)   VALUE ZERO.                   
004660 77  WS-ADLEVEL11-FOM            PIC 9(1)   VALUE ZERO.                   
004670 77  WS-ADLEVEL11-TOM            PIC 9(1)   VALUE ZERO.                   
004671 77  WS-ADLEVEL11                PIC 9(1)   VALUE ZERO.                   
004691 77  WS-ADSEQ                    PIC 9(2)   VALUE ZERO.                   
004692 77  WS-KDLOC                    PIC X(1)   VALUE SPACE.                  
004693 77  WS-KDFREQ                   PIC 9(2)   VALUE ZERO.                   
004694 77  WS-TELOC                    PIC X(1)   VALUE SPACE.                  
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004802     88  INDATA-OK                           VALUE 'Y'.                   
004810     88  INDATA-WRONG                        VALUE 'N'.                   
004900                                                                          
005000 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
005100     88  KEYS-OK                             VALUE 'Y'.                   
005200     88  KEYS-WRONG                          VALUE 'N'.                   
005201                                                                          
005202 77  PRINT-SW                    PIC X       VALUE 'Y'.                   
005203     88  PRINT-OK                            VALUE 'Y'.                   
005204     88  PRINT-WRONG                         VALUE 'N'.                   
005206                                                                          
005207 77  OCCU-SW                     PIC X       VALUE 'Y'.                   
005208     88  OCCUPIED                            VALUE 'Y'.                   
005209     88  NOT-OCCUPIED                        VALUE 'N'.                   
005210                                                                          
005211 77  RESTART-SW                  PIC X       VALUE 'N'.                   
005212     88  RESTART                             VALUE 'Y'.                   
005220                                                                          
005250 77  PRIME                       PIC X        VALUE 'P'.                  
005260 77  BUFFER                      PIC X        VALUE 'R'.                  
005270 77  MIXED                       PIC X        VALUE 'M'.                  
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  OWN-MID                             VALUE '6346'.                
005600     88  GOOD-MID                            VALUE '6346'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*      --- VALID IDDC CODES                                               
006301*                                                                         
006302*01    -COPY WWDC99                                                       
006303       EJECT                                                              
006310*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007000     EJECT                                                                
007100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007310*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
007320*   -COPY W006PRT                                                         
007330     EJECT                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007504     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007520     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007530     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
007900     EJECT                                                                
008000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I34601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W6O34601                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010301 01  W-PROG-TO-PROG-SW.                                                   
010302*03 -COPY WMSGSOP                                                         
010303  SKIP3                                                                   
010304 01 PARM-TESYMBV.                                                         
010305     03 FILLER                   PIC X(2)    VALUE 'A('.                  
010306     03 PARM-IDDC                PIC X(2)    VALUE SPACE.                 
010307     03 FILLER                   PIC X(1)    VALUE ')'.                   
010308     03 FILLER                   PIC X(2)    VALUE 'B('.                  
010309     03 PARM-ADLAGOMR            PIC 9(2)    VALUE ZERO.                  
010310     03 FILLER                   PIC X(1)    VALUE ')'.                   
010311     03 FILLER                   PIC X(2)    VALUE 'C('.                  
010312     03 PARM-ADGANG-FOM          PIC 9(2)    VALUE ZERO.                  
010313     03 FILLER                   PIC X(1)    VALUE ')'.                   
010314     03 FILLER                   PIC X(2)    VALUE 'D('.                  
010315     03 PARM-ADGANG-TOM          PIC 9(2)    VALUE ZERO.                  
010316     03 FILLER                   PIC X(1)    VALUE ')'.                   
010317     03 FILLER                   PIC X(2)    VALUE 'E('.                  
010318     03 PARM-ADSEC11-FOM         PIC 9(3)    VALUE ZERO.                  
010319     03 FILLER                   PIC X(1)    VALUE ')'.                   
010320     03 FILLER                   PIC X(2)    VALUE 'F('.                  
010321     03 PARM-ADSEC11-TOM         PIC 9(3)    VALUE ZERO.                  
010322     03 FILLER                   PIC X(1)    VALUE ')'.                   
010323     03 FILLER                   PIC X(2)    VALUE 'G('.                  
010324     03 PARM-ADLEVEL11-FOM       PIC 9(1)    VALUE ZERO.                  
010325     03 FILLER                   PIC X(1)    VALUE ')'.                   
010326     03 FILLER                   PIC X(2)    VALUE 'H('.                  
010327     03 PARM-ADLEVEL11-TOM       PIC 9(1)    VALUE ZERO.                  
010328     03 FILLER                   PIC X(1)    VALUE ')'.                   
010329     03 FILLER                   PIC X(2)    VALUE 'I('.                  
010330     03 PARM-KDLOC               PIC X       VALUE SPACE.                 
010331     03 FILLER                   PIC X(1)    VALUE ')'.                   
010332     03 FILLER                   PIC X(2)    VALUE 'J('.                  
010333     03 PARM-KDFREQ              PIC 9(2)    VALUE ZERO.                  
010334     03 FILLER                   PIC X(1)    VALUE ')'.                   
010335     03 FILLER                   PIC X(2)    VALUE 'K('.                  
010336     03 PARM-TELOC               PIC X       VALUE SPACE.                 
010337     03 FILLER                   PIC X(1)    VALUE ')'.                   
010338     03 FILLER                   PIC X(2)    VALUE 'L('.                  
010339     03 PARM-KDPRT               PIC X(3)    VALUE SPACE.                 
010340     03 FILLER                   PIC X(1)    VALUE ')'.                   
010400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010500*                                                                         
010600     EJECT                                                                
010601 01  WORK-AREA.                                                           
010603     03 WORK-ADLAGOMR            PIC 9(2).                                
010604     03 WORK-ADGANG              PIC 9(2).                                
010605     03 WORK-ADPLATS.                                                     
010606        05 WORK-ADSEC11          PIC 9(3).                                
010607        05 WORK-ADLEVEL11        PIC 9(1).                                
010608        05 WORK-ADSEQ            PIC 9(1).                                
010609     03 WORK-TELOC               PIC X(1).                                
010645                                                                          
010650 01  SAVE-AREA.                                                           
010660     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
010670     03 SAVE-ADLAGOMR-ENTER      PIC 9(2).                                
010680     03 SAVE-ADGANG-ENTER        PIC 9(2).                                
010681     03 SAVE-ADPLATS-ENTER.                                               
010682        05 SAVE-ADSEC11-ENTER    PIC 9(3).                                
010683        05 SAVE-ADLEVEL11-ENTER  PIC 9(1).                                
010684        05 SAVE-ADSEQ-ENTER      PIC 9(1).                                
010685     03 SAVE-ADLAGOMR-NEXT       PIC 9(2).                                
010686     03 SAVE-ADGANG-NEXT         PIC 9(2).                                
010687     03 SAVE-ADPLATS-NEXT.                                                
010688        05 SAVE-ADSEC11-NEXT     PIC 9(3).                                
010689        05 SAVE-ADLEVEL11-NEXT   PIC 9(1).                                
010690        05 SAVE-ADSEQ-NEXT       PIC 9(1).                                
010691                                                                          
010692                                                                          
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  KEYS-TO-DLI.                                                         
011016                                                                          
011017     03  W-WDJ8KY-MIN-X.                                                  
011018         05  W-LOC-IDDC-MIN       PIC X(2).                               
011019         05  W-LOC-ADLAGOMR-MIN   PIC 9(2).                               
011020         05  W-LOC-ADGANG-MIN     PIC 9(2).                               
011030         05  W-LOC-ADPLATS-MIN.                                           
011040             07 W-LOC-ADSEC11-MIN PIC 9(3).                               
011050             07 W-LOC-ADLEVEL11-MIN PIC 9(1).                             
011060             07 W-LOC-ADSEQ-MIN   PIC 9(1).                               
011061                                                                          
011062   03  W-WDJ8KY-MAX-X.                                                    
011063     05  W-LOC-IDDC-MAX      PIC X(2).                                    
011064     05  W-LOC-ADLAGOMR-MAX  PIC 9(2).                                    
011065     05  W-LOC-ADGANG-MAX    PIC 9(2).                                    
011066     05  W-LOC-ADPLATS-MAX.                                               
011067       07  W-LOC-ADSEC11-MAX PIC 9(3).                                    
011068       07  W-LOC-ADLEVEL11-MAX PIC 9(1).                                  
011069       07  W-LOC-ADSEQ-MAX   PIC 9(1).                                    
011070                                                                          
011097   03  WDK6E1KY-MIN-X.                                                    
011099     05  W-ADART-MIN.                                                     
011100       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.               
011101       07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.               
011102       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.               
011103     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3  VALUE ZERO.               
011104                                                                          
011106   03  WDK6E1KY-MAX-X.                                                    
011108     05  W-ADART-MAX.                                                     
011109       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.               
011110       07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.               
011111       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.               
011112     05 W-IDARTNR-MAX     PIC S9(9)  COMP-3  VALUE 99999999.              
011113                                                                          
011118   03  W-WDD8ASEQ-X.                                                      
011119     05  W-IDDC-ASEQ         PIC X(2)           VALUE SPACE.              
011120     05  W-ADBUFFOMR-ASEQ    PIC S9(3)  COMP-3  VALUE ZERO.               
011121     05  W-ADBUFFGANG-ASEQ   PIC S9(3)  COMP-3  VALUE ZERO.               
011122     05  W-ADBUFFPL-ASEQ     PIC S9(5)  COMP-3  VALUE ZERO.               
011125                                                                          
011150     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FOUND                       VALUE '  '.                  
011500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011610     88  END-OF-DATABASE                     VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GOOD-STATUSCODES.                                                    
011900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNCTION CODES                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013021 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
013022 01  DLI-IO-WLLOCA01.                                                     
013030*    03  -COPY WDJ801                                                     
013031                                                                          
013032 01  FILLER         PIC X(20) VALUE 'WLARTA01-AREA'.                      
013033 01  DLI-IO-WLARTA01.                                                     
013050*    03  -COPY WDK6E1                                                     
013060                                                                          
013071 01  FILLER         PIC X(20) VALUE 'WLARTD11-AREA'.                      
013072 01  DLI-IO-WLARTD11.                                                     
013100*    03  -COPY WDD811                                                     
013200                                                                          
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009  -PRE MSG-                                               
013501*01  -COPY W0009  -PRE ALT-                                               
013502     EJECT                                                                
013600*01  -COPY W0008  -PRE USEA-                                              
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013808*01  -COPY W0008  -PRE LOCA-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000*01  -COPY W0008  -PRE ARTR-                                              
014001     05  FILLER                  PIC X.                                   
014002     EJECT                                                                
014003*01  -COPY W0008  -PRE ARTD-                                              
014004     05  FILLER                  PIC X.                                   
014005     EJECT                                                                
014006 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
014007           USEA-PCB                                                       
014008           LOCA-PCB ARTR-PCB ARTD-PCB.                                    
014009 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
014011            USEA-PCB                                                      
014012            LOCA-PCB ARTR-PCB ARTD-PCB.                                   
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FOUND                                                     
014500       PERFORM A-INIT                                                     
014510       IF GOOD-MID OR HELP-MID                                            
014600          PERFORM B-CHECK-KEYS                                            
014700          IF KEYS-OK                                                      
014710             IF MFS-PRINT                                                 
014720                PERFORM G-CHECK-PRINT                                     
014730                IF PRINT-OK                                               
014740                   PERFORM GA-STARTA-JOB                                  
014750                END-IF                                                    
014760             END-IF                                                       
014800             IF MFS-FIRST                                                 
014900                PERFORM C-FIRST-PAGE                                      
015000              ELSE                                                        
015100                IF MFS-NEXT                                               
015101                   PERFORM D-NEXT-PAGE                                    
015102                 ELSE                                                     
015103                   PERFORM E-SAME-PAGE                                    
015104                END-IF                                                    
015105             END-IF                                                       
015106             PERFORM F-READ-SHOW-INFO                                     
015110          END-IF                                                          
015310       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34601 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015900     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DOUBLE-TRANSACTIONS                                           
016800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34601                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I34601                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W6O346N1' TO MFS-IDMOD                                         
018300     MOVE '6346' TO MOD-IDTRANS                                           
018400     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
018401                                                                          
018411                                                                          
018420     MOVE ALL '+'           TO MSGI-WMSGINIT                              
018430     MOVE '001'             TO MSGI-KDCALL                                
018440     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
018450     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
018460     MOVE '6346'            TO MSGI-IDTRANS                               
018470     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
018480                                                                          
018490     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
018491     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
018492     MOVE MSGI-IDDC         TO WS-IDDC                                    
018500                                                                          
018600     IF GOOD-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019010       PERFORM MFS-INIT-KEY-FIELD-IN                                      
019020       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
019021       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
019100     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-CHECK-KEYS SECTION.                                                    
019700                                                                          
020720                                                                          
020800     MOVE YES               TO KEYS-SW                                    
020830                                                                          
020840     PERFORM MFS-INIT-KEY-FIELD-IN                                        
021017                                                                          
021018*                                                                         
021019*    -- CONTROL  ON WAREHOUSE                                             
021020*                                                                         
021021     IF CDC                                                               
021022       CONTINUE                                                           
021023     ELSE                                                                 
021024       MOVE NOO                 TO KEYS-SW                                
021025     END-IF                                                               
021026*                                                                         
021027*    -- CONTROL  AREA                                                     
021028*                                                                         
021029     IF MID-ADLAGOMR-IN = ALL '+'                                         
021030       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
021040       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
021050     ELSE                                                                 
021060       IF MID-ADLAGOMR-IN NUMERIC                                         
021061          MOVE '7' TO MFS-IDPFK                                           
021070          MOVE    SPACE        TO MFS-KDTRTYP                             
021080          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
021081       ELSE                                                               
021082          MOVE NOO             TO KEYS-SW                                 
021083       END-IF                                                             
021084     END-IF                                                               
021085*                                                                         
021086*    -- CONTROL  AISLE FROM                                               
021087*                                                                         
021088     IF MID-ADGANG-FOM-IN = ALL '+'                                       
021089       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
021090       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
021091     ELSE                                                                 
021092       IF MID-ADGANG-FOM-IN NUMERIC                                       
021093          MOVE '7' TO MFS-IDPFK                                           
021094          MOVE    SPACE          TO MFS-KDTRTYP                           
021095          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
021096       ELSE                                                               
021097          MOVE NOO               TO KEYS-SW                               
021098       END-IF                                                             
021099     END-IF                                                               
021100*                                                                         
021101*    -- CONTROL  AISLE THRU                                               
021102*                                                                         
021103     IF MID-ADGANG-TOM-IN = ALL '+'                                       
021104       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
021105       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
021106     ELSE                                                                 
021107       IF MID-ADGANG-TOM-IN NUMERIC                                       
021108          MOVE '7' TO MFS-IDPFK                                           
021109          MOVE    SPACE          TO MFS-KDTRTYP                           
021110          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
021111       ELSE                                                               
021112          MOVE NOO               TO KEYS-SW                               
021113       END-IF                                                             
021114     END-IF                                                               
021115*                                                                         
021116*    -- CONTROL  SECTION FROM                                             
021117*                                                                         
021118     IF MID-ADSEC11-FOM-IN = ALL '+'                                      
021119       INSPECT MID-ADSEC11-FOM-UT REPLACING LEADING SPACE BY ZERO         
021120       MOVE MID-ADSEC11-FOM-UT  TO WS-ADSEC11-FOM                         
021130     ELSE                                                                 
021140       IF MID-ADSEC11-FOM-IN NUMERIC                                      
021141          MOVE '7' TO MFS-IDPFK                                           
021142          MOVE    SPACE          TO MFS-KDTRTYP                           
021143          MOVE MID-ADSEC11-FOM-IN TO WS-ADSEC11-FOM                       
021144       ELSE                                                               
021145          MOVE NOO               TO KEYS-SW                               
021146       END-IF                                                             
021147     END-IF                                                               
021148*                                                                         
021149*    -- CONTROL  SECTION THRU                                             
021150*                                                                         
021151     IF MID-ADSEC11-TOM-IN = ALL '+'                                      
021152       INSPECT MID-ADSEC11-TOM-UT REPLACING LEADING SPACE BY ZERO         
021153       MOVE MID-ADSEC11-TOM-UT  TO WS-ADSEC11-TOM                         
021154     ELSE                                                                 
021155       IF MID-ADSEC11-TOM-IN NUMERIC                                      
021156          MOVE '7' TO MFS-IDPFK                                           
021157          MOVE    SPACE          TO MFS-KDTRTYP                           
021158          MOVE MID-ADSEC11-TOM-IN TO WS-ADSEC11-TOM                       
021159       ELSE                                                               
021160          MOVE NOO               TO KEYS-SW                               
021161       END-IF                                                             
021162     END-IF                                                               
021163*                                                                         
021164*    -- CONTROL  LEVEL FROM                                               
021165*                                                                         
021166     IF MID-ADLEVEL11-FOM-IN = ALL '+'                                    
021167      INSPECT MID-ADLEVEL11-FOM-UT REPLACING LEADING SPACE BY ZERO        
021168       MOVE MID-ADLEVEL11-FOM-UT  TO WS-ADLEVEL11-FOM                     
021169     ELSE                                                                 
021170       IF MID-ADLEVEL11-FOM-IN NUMERIC                                    
021171          MOVE '7' TO MFS-IDPFK                                           
021172          MOVE    SPACE           TO MFS-KDTRTYP                          
021173          MOVE MID-ADLEVEL11-FOM-IN TO WS-ADLEVEL11-FOM                   
021174       ELSE                                                               
021175          MOVE NOO                TO KEYS-SW                              
021176       END-IF                                                             
021177     END-IF                                                               
021178                                                                          
021179*                                                                         
021180*    -- CONTROL  LEVEL THRU                                               
021181*                                                                         
021182     IF MID-ADLEVEL11-TOM-IN = ALL '+'                                    
021183      INSPECT MID-ADLEVEL11-TOM-UT REPLACING LEADING SPACE BY ZERO        
021184       MOVE MID-ADLEVEL11-TOM-UT  TO WS-ADLEVEL11-TOM                     
021185     ELSE                                                                 
021186       IF MID-ADLEVEL11-TOM-IN NUMERIC                                    
021187          MOVE '7' TO MFS-IDPFK                                           
021188          MOVE    SPACE           TO MFS-KDTRTYP                          
021189          MOVE MID-ADLEVEL11-TOM-IN TO WS-ADLEVEL11-TOM                   
021190       ELSE                                                               
021191          MOVE NOO                TO KEYS-SW                              
021192       END-IF                                                             
021193     END-IF                                                               
021194*                                                                         
021195*    -- CONTROL  TYPE OF LOCATION                                         
021196*                                                                         
021197     IF MID-KDLOC-IN  = ALL '+'                                           
021200        MOVE MID-KDLOC-UT          TO WS-KDLOC                            
021211      ELSE                                                                
021212        IF MID-KDLOC-IN  =  PRIME OR BUFFER OR MIXED                      
021214           MOVE '7'                TO MFS-IDPFK                           
021215           MOVE SPACE              TO MFS-KDTRTYP                         
021216           MOVE MID-KDLOC-IN       TO WS-KDLOC                            
021217         ELSE                                                             
021218           MOVE NOO                 TO KEYS-SW                            
021219        END-IF                                                            
021220     END-IF                                                               
021221*                                                                         
021222*    -- CONTROL  FREQUENCY TYPE                                           
021223*                                                                         
021224     IF MID-KDFREQ-IN = ALL '+'                                           
021225        MOVE MID-KDFREQ-UT          TO WS-KDFREQ                          
021228     ELSE                                                                 
021229       IF MID-KDFREQ-IN NUMERIC                                           
021230          MOVE '7'                  TO MFS-IDPFK                          
021231          MOVE SPACE                TO MFS-KDTRTYP                        
021232          MOVE MID-KDFREQ-IN        TO WS-KDFREQ                          
021233       ELSE                                                               
021234          MOVE NOO                  TO KEYS-SW                            
021235       END-IF                                                             
021236     END-IF                                                               
021237*                                                                         
021238*    -- FILL IN REMARK FIELD INPUT                                        
021239*                                                                         
021240     IF MID-TELOC-IN  = ALL '+'                                           
021241        MOVE MID-TELOC-UT         TO WS-TELOC                             
021242      ELSE                                                                
021244        MOVE '7'                  TO MFS-IDPFK                            
021245        MOVE SPACE                TO MFS-KDTRTYP                          
021246        MOVE MID-TELOC-IN         TO WS-TELOC                             
021250     END-IF                                                               
021251*                                                                         
021252*    -- FILL MOD KEY-OUTPUT FIELDS                                        
021253*                                                                         
021254     MOVE WS-IDDC        TO  MOD-IDDC-UT                                  
021255     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
021256     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
021257     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
021258     MOVE WS-ADSEC11-FOM TO  MOD-ADSEC11-FOM-UT                           
021259     MOVE WS-ADSEC11-TOM TO  MOD-ADSEC11-TOM-UT                           
021260     MOVE WS-ADLEVEL11-FOM TO MOD-ADLEVEL11-FOM-UT                        
021261     MOVE WS-ADLEVEL11-TOM TO MOD-ADLEVEL11-TOM-UT                        
021262     MOVE WS-KDLOC       TO  MOD-KDLOC-UT                                 
021263     MOVE WS-KDFREQ      TO  MOD-KDFREQ-UT                                
021264     MOVE WS-TELOC       TO  MOD-TELOC-UT                                 
021265                                                                          
021270                                                                          
021300     IF KEYS-WRONG                                                        
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021610      ELSE                                                                
021620       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
021621          WS-ADSEC11-FOM  >  WS-ADSEC11-TOM OR                            
021622          WS-ADLEVEL11-FOM > WS-ADLEVEL11-TOM                             
021624                                                                          
021625          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
021626          CALL WMEDKONV USING MED-WMEDAREA                                
021627          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
021630          MOVE NOO                 TO KEYS-SW                             
021640       END-IF                                                             
021900     END-IF                                                               
021901                                                                          
022000     .                                                                    
022200     EJECT                                                                
022301 C-FIRST-PAGE SECTION.                                                    
022302                                                                          
022304                                                                          
022308*                                                                         
022309*    -- FILL START KEY FIELDS                                             
022310*                                                                         
022312     MOVE WS-IDDC               TO W-LOC-IDDC-MIN                         
022313     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MIN                     
022314     MOVE WS-ADGANG-FOM         TO W-LOC-ADGANG-MIN                       
022316     MOVE WS-ADSEC11-FOM        TO W-LOC-ADSEC11-MIN                      
022317     MOVE WS-ADLEVEL11-FOM      TO W-LOC-ADLEVEL11-MIN                    
022318     MOVE ZEROES                TO W-LOC-ADSEQ-MIN                        
022319                                                                          
022343     .                                                                    
022344     EJECT                                                                
022345 E-SAME-PAGE SECTION.                                                     
022346                                                                          
022347*                                                                         
022348*    -- FILL START KEY FIELDS                                             
022349*                                                                         
022350     MOVE WS-IDDC               TO W-LOC-IDDC-MIN                         
022351     MOVE SAVE-ADLAGOMR-ENTER   TO W-LOC-ADLAGOMR-MIN                     
022352     MOVE SAVE-ADGANG-ENTER     TO W-LOC-ADGANG-MIN                       
022353     MOVE SAVE-ADSEC11-ENTER    TO W-LOC-ADSEC11-MIN                      
022354     MOVE SAVE-ADLEVEL11-ENTER  TO W-LOC-ADLEVEL11-MIN                    
022355     MOVE SAVE-ADSEQ-ENTER      TO W-LOC-ADSEQ-MIN                        
022361     .                                                                    
022362     EJECT                                                                
022363                                                                          
022370 D-NEXT-PAGE SECTION.                                                     
022380                                                                          
022381*                                                                         
022382*    -- FILL START KEY FIELDS                                             
022383*                                                                         
022384     MOVE WS-IDDC               TO W-LOC-IDDC-MIN                         
022385     MOVE SAVE-ADLAGOMR-NEXT    TO W-LOC-ADLAGOMR-MIN                     
022386     MOVE SAVE-ADGANG-NEXT      TO W-LOC-ADGANG-MIN                       
022387     MOVE SAVE-ADSEC11-NEXT     TO W-LOC-ADSEC11-MIN                      
022388     MOVE SAVE-ADLEVEL11-NEXT   TO W-LOC-ADLEVEL11-MIN                    
022389     MOVE SAVE-ADSEQ-NEXT       TO W-LOC-ADSEQ-MIN                        
022390     .                                                                    
022391     EJECT                                                                
022392                                                                          
024898 F-READ-SHOW-INFO SECTION.                                                
024899                                                                          
024900*                                                                         
024901*    -- FILL FINISH (MAX) KEY FIELDS                                      
024902*                                                                         
024903     MOVE WS-IDDC               TO W-LOC-IDDC-MAX                         
024904     MOVE WS-ADLAGOMR           TO W-LOC-ADLAGOMR-MAX                     
024905     MOVE WS-ADGANG-TOM         TO W-LOC-ADGANG-MAX                       
024906     MOVE WS-ADSEC11-TOM        TO W-LOC-ADSEC11-MAX                      
024907     MOVE WS-ADLEVEL11-TOM      TO W-LOC-ADLEVEL11-MAX                    
024908     MOVE 9                     TO W-LOC-ADSEQ-MAX                        
024909                                                                          
024910     MOVE 99999999              TO W-IDARTNR-MAX                          
024911                                                                          
024912     PERFORM  IMS-GU-LOCA                                                 
024920                                                                          
024921     IF SEGMENT-MISSING                                                   
024923        IF MFS-NEXT                                                       
024924           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
024925         ELSE                                                             
024926           MOVE ITEMS-MISSING       TO    MED-IDMFSFEL                    
024927        END-IF                                                            
024928        CALL    WMEDKONV    USING MED-WMEDAREA                            
024929        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
024930                                                                          
024934        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
024935                                                                          
024936        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-ENTER                        
024937        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-ENTER                          
024938        MOVE WS-ADSEC11-FOM TO SAVE-ADSEC11-ENTER                         
024939        MOVE WS-ADLEVEL11-FOM TO SAVE-ADLEVEL11-ENTER                     
024940        MOVE ZEROES         TO SAVE-ADSEQ-ENTER                           
024941                                                                          
024942        MOVE WS-ADLAGOMR    TO SAVE-ADLAGOMR-NEXT                         
024943        MOVE WS-ADGANG-FOM  TO SAVE-ADGANG-NEXT                           
024944        MOVE WS-ADSEC11-FOM TO SAVE-ADSEC11-NEXT                          
024945        MOVE WS-ADLEVEL11-FOM TO SAVE-ADLEVEL11-NEXT                      
024946        MOVE ZEROES         TO SAVE-ADSEQ-NEXT                            
024947                                                                          
024948      ELSE                                                                
024949        MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-ENTER                        
024950        MOVE LOC-ADGANG     TO SAVE-ADGANG-ENTER                          
024951        MOVE LOC-ADPLATS    TO SAVE-ADPLATS-ENTER                         
024955        MOVE +1             TO INDX                                       
024956        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
024957                      INDX > MAX-INDX                                     
024958         MOVE NOO                 TO OCCU-SW                              
024961         MOVE LOC-ADPLATS         TO WORK-ADPLATS                         
024962         MOVE LOC-TELOC           TO WORK-TELOC                           
024964         IF WS-ADLEVEL11-FOM <= WORK-ADLEVEL11          AND               
024965            WS-ADLEVEL11-TOM >= WORK-ADLEVEL11          AND               
024966            WS-ADSEC11-FOM <= WORK-ADSEC11              AND               
024967            WS-ADSEC11-TOM >= WORK-ADSEC11              AND               
024968            (WS-KDLOC  = " " OR  WS-KDLOC = LOC-KDLOC)  AND               
024969            (WS-KDFREQ = 0   OR WS-KDFREQ = LOC-KDFREQ) AND               
024970            (WS-TELOC  = " " OR WS-TELOC  = WORK-TELOC)                   
024971            IF LOC-KDLOC     =  PRIME OR MIXED                            
024972*              MOVE LOC-IDDC            TO W-IDDC-MIN                     
024973*                                          W-IDDC-MAX                     
024974               MOVE LOC-ADLAGOMR        TO W-ADLAGOMR-MIN                 
024975                                           W-ADLAGOMR-MAX                 
024976               MOVE LOC-ADGANG          TO W-ADGANG-MIN                   
024977                                           W-ADGANG-MAX                   
024978               MOVE LOC-ADPLATS         TO W-ADPLATS-MIN                  
024979                                        W-ADPLATS-MAX                     
024980               PERFORM  IMS-GU-WDK6E1                                     
024981                                                                          
024982               IF SEGMENT-FOUND                                           
024983                  MOVE  YES              TO OCCU-SW                       
024984               END-IF                                                     
024985            END-IF                                                        
024986            IF LOC-KDLOC  =  (BUFFER OR MIXED) AND NOT-OCCUPIED           
024987               MOVE LOC-IDDC            TO W-IDDC-ASEQ                    
024988               MOVE LOC-ADLAGOMR        TO W-ADBUFFOMR-ASEQ               
024989               MOVE LOC-ADGANG          TO W-ADBUFFGANG-ASEQ              
024990               MOVE LOC-ADPLATS         TO W-ADBUFFPL-ASEQ                
024991                                                                          
025000               PERFORM  IMS-GU-ARTD-ASEQ                                  
025001                                                                          
025002               IF SEGMENT-FOUND                                           
025003                  MOVE  YES              TO OCCU-SW                       
025004               END-IF                                                     
025005            END-IF                                                        
025006            IF NOT-OCCUPIED                                               
025007               MOVE LOC-ADLAGOMR    TO MOD-ADLAGOMR-LINE (INDX)           
025008               MOVE LOC-ADGANG      TO MOD-ADGANG-LINE   (INDX)           
025009               MOVE LOC-ADPLATS     TO MOD-ADPLATS-LINE  (INDX)           
025010               MOVE LOC-KDLOC       TO MOD-KDLOC-LINE    (INDX)           
025011               MOVE LOC-KDFREQ      TO MOD-KDFREQ-LINE   (INDX)           
025012               MOVE LOC-KDSTOR      TO MOD-KDSTOR-LINE   (INDX)           
025013               MOVE LOC-TELOC       TO MOD-TELOC-LINE    (INDX)           
025015               ADD 1 TO INDX                                              
025016            END-IF                                                        
025017         END-IF                                                           
025018         PERFORM IMS-GN-LOCA                                              
025019        END-PERFORM                                                       
025020        IF SEGMENT-FOUND                                                  
025021           MOVE LOC-ADLAGOMR   TO SAVE-ADLAGOMR-NEXT                      
025022           MOVE LOC-ADGANG     TO SAVE-ADGANG-NEXT                        
025023           MOVE LOC-ADPLATS    TO SAVE-ADPLATS-NEXT                       
025024           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
025025           CALL WMEDKONV USING MED-WMEDAREA                               
025026           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
025027         ELSE                                                             
025028           MOVE LOC-ADLAGOMR        TO    SAVE-ADLAGOMR-NEXT              
025029           MOVE LOC-ADGANG          TO    SAVE-ADGANG-NEXT                
025030           MOVE LOC-ADPLATS         TO    SAVE-ADPLATS-NEXT               
025031           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
025032           CALL WMEDKONV            USING MED-WMEDAREA                    
025033           MOVE MED-MFSFEL          TO    MOD-TEMFSFEL                    
025034           PERFORM UNTIL INDX > MAX-INDX                                  
025035             MOVE MFS-ERASE-FIELD   TO  MOD-ADLAGOMR-LINE (INDX)          
025036                                        MOD-ADGANG-LINE   (INDX)          
025037                                        MOD-ADPLATS-LINE  (INDX)          
025038                                        MOD-KDLOC-LINE    (INDX)          
025039                                        MOD-KDFREQ-LINE   (INDX)          
025040                                        MOD-KDSTOR-LINE   (INDX)          
025041                                        MOD-TELOC-LINE    (INDX)          
025043             ADD 1 TO INDX                                                
025044           END-PERFORM                                                    
025045         END-IF                                                           
025046                                                                          
025047       MOVE '002'      TO MSGI-KDCALL                                     
025048       MOVE '6346'     TO SAVE-IDTRANS                                    
025049       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
025050       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
025051     END-IF                                                               
025052     .                                                                    
025060     EJECT                                                                
025061 G-CHECK-PRINT SECTION.                                                   
025062     IF MID-KDPRT = ALL '+'                                               
025063        MOVE NOO TO PRINT-SW                                              
025064        MOVE 'PRINT CODE WRONG'  TO MOD-TEMFSFEL                          
025065     ELSE                                                                 
025066        MOVE '6L'      TO PRT-IDPRTLST(1:2)                               
025067        MOVE MID-KDPRT TO PRT-IDPRTLST(3:3)                               
025068        MOVE SPACE     TO PRT-IDPRTLST(6:3)                               
025069        MOVE 1                 TO PRT-KDCALL                              
025070        CALL W006PRT USING PRT-W006PRT                                    
025071        IF PRT-IDLTERM = 'SAKNAS  '                                       
025072          MOVE NOO TO PRINT-SW                                            
025073          MOVE 'PRINTER MISSING IN W006PRT' TO MOD-TEMFSFEL               
025074        ELSE                                                              
025075          MOVE WS-IDDC           TO PARM-IDDC                             
025076          MOVE WS-ADLAGOMR       TO PARM-ADLAGOMR                         
025077          MOVE WS-ADGANG-FOM     TO PARM-ADGANG-FOM                       
025078          MOVE WS-ADGANG-TOM     TO PARM-ADGANG-TOM                       
025079          MOVE WS-ADSEC11-FOM    TO PARM-ADSEC11-FOM                      
025080          MOVE WS-ADSEC11-TOM    TO PARM-ADSEC11-TOM                      
025081          MOVE WS-ADLEVEL11-FOM  TO PARM-ADLEVEL11-FOM                    
025082          MOVE WS-ADLEVEL11-TOM  TO PARM-ADLEVEL11-TOM                    
025083          MOVE WS-KDLOC          TO PARM-KDLOC                            
025084          MOVE WS-KDFREQ         TO PARM-KDFREQ                           
025085          MOVE WS-TELOC          TO PARM-TELOC                            
025086          MOVE MID-KDPRT         TO PARM-KDPRT                            
025087          MOVE 'PRINT STARTED'   TO MOD-TEMFSFEL                          
025088        END-IF                                                            
025089     END-IF                                                               
025090     MOVE MFS-ERASE-FIELD        TO MOD-KDPRT                             
025091                                                                          
025092     EJECT                                                                
025093     .                                                                    
025094                                                                          
025095*    FLYTTAR PARAMETRAR TILL SOPRUTIN OCH STARTAR UPP                     
025096 GA-STARTA-JOB SECTION.                                                   
025097     MOVE '6346'       TO MSGSOP-IDTRANS                                  
025098     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
025099     MOVE 'W615S1    ' TO MSGSOP-IDPROCESS                                
025100     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
025101     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
025102     PERFORM IMS-INSERT-ALT-MSG                                           
025103                                                                          
025104     SKIP3                                                                
025105     .                                                                    
025106 MFS-INIT-KEY-FIELD-IN SECTION.                                           
025107                                                                          
025108*    --- ALL INPUT KEY FIELDS                                             
025109                                                                          
025110     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
025111                               MOD-ADLAGOMR-IN                            
025112                               MOD-ADGANG-FOM-IN                          
025113                               MOD-ADGANG-TOM-IN                          
025114                               MOD-ADSEC11-FOM-IN                         
025115                               MOD-ADSEC11-TOM-IN                         
025116                               MOD-ADLEVEL11-FOM-IN                       
025117                               MOD-ADLEVEL11-TOM-IN                       
025118                               MOD-KDLOC-IN                               
025119                               MOD-KDFREQ-IN                              
025120                               MOD-TELOC-IN                               
025121     .                                                                    
025122     EJECT                                                                
025123 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
025124                                                                          
025125*    --- ALL OUTPUT KEY FIELDS                                            
025126                                                                          
025127     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
025128     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
025129                               MOD-ADGANG-FOM-UT                          
025130                               MOD-ADGANG-TOM-UT                          
025131                               MOD-ADSEC11-FOM-UT                         
025132                               MOD-ADSEC11-TOM-UT                         
025133                               MOD-ADLEVEL11-FOM-UT                       
025134                               MOD-ADLEVEL11-TOM-UT                       
025135                               MOD-KDLOC-UT                               
025136                               MOD-KDFREQ-UT                              
025137                               MOD-TELOC-UT                               
025138     .                                                                    
025140     EJECT                                                                
025200 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
025300                                                                          
025400*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
025500                                                                          
025600     MOVE +1 TO INDX                                                      
025610     PERFORM UNTIL INDX > MAX-INDX                                        
025620      MOVE MFS-ERASE-FIELD     TO MOD-ADLAGOMR-LINE   (INDX)              
025630                                  MOD-ADGANG-LINE     (INDX)              
025640                                  MOD-ADPLATS-LINE    (INDX)              
025650                                  MOD-KDLOC-LINE      (INDX)              
025660                                  MOD-KDFREQ-LINE     (INDX)              
025661                                  MOD-KDSTOR-LINE     (INDX)              
025662                                  MOD-TELOC-LINE      (INDX)              
025670      ADD +1 TO INDX                                                      
025680     END-PERFORM                                                          
025690     .                                                                    
025691     SKIP3                                                                
029400* --- IMS SECTIONS ---                                                    
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GOOD-STATUSCODES                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSCHECK                                              
030200     .                                                                    
030300     SKIP3                                                                
030310 IMS-INSERT-ALT-MSG SECTION.                                              
030320                                                                          
030330     MOVE SPACE TO GOOD-STATUSCODES                                       
030340     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
030350     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
030360     PERFORM IMS-STATUSCHECK                                              
030370     .                                                                    
030380     EJECT                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GOOD-STATUSCODES                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSCHECK                                              
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GU-LOCA SECTION.                                                     
031503                                                                          
031504     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
031505                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
031506          DELIMITED BY SIZE INTO SSA1                                     
031507     MOVE '  GE' TO GOOD-STATUSCODES                                      
031508     CALL CBLTDLI USING GU LOCA-PCB LOC-WDJ801 SSA1                       
031509     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
031510     PERFORM IMS-STATUSCHECK                                              
031511     .                                                                    
031512     SKIP3                                                                
031513                                                                          
031514 IMS-GN-LOCA SECTION.                                                     
031515                                                                          
031516     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ8KY-MIN-X                          
031517                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
031518          DELIMITED BY SIZE INTO SSA1                                     
031519     MOVE '  GE' TO GOOD-STATUSCODES                                      
031520     CALL CBLTDLI USING GN LOCA-PCB LOC-WDJ801 SSA1                       
031521     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
031522     PERFORM IMS-STATUSCHECK                                              
031523     .                                                                    
031524     SKIP3                                                                
031525                                                                          
031526 IMS-GU-WDK6E1 SECTION.                                                   
031527                                                                          
031528     STRING 'WDK6E1  (WDK6E1KY=>' WDK6E1KY-MIN-X                          
031529                    '&WDK6E1KY=<' WDK6E1KY-MAX-X ')'                      
031530          DELIMITED BY SIZE INTO SSA1                                     
031531     MOVE '  GE' TO GOOD-STATUSCODES                                      
031532     CALL CBLTDLI USING GU ARTR-PCB SEQE-WDK6E1 SSA1                      
031533     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
031534     PERFORM IMS-STATUSCHECK                                              
031536     .                                                                    
031537     SKIP3                                                                
031550                                                                          
031560 IMS-GU-ARTD-ASEQ SECTION.                                                
031570                                                                          
031580     STRING 'WLARTD11(WDD8ASEQ =' W-WDD8ASEQ-X ')'                        
031590          DELIMITED BY SIZE INTO SSA1                                     
031600     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
031610     CALL CBLTDLI USING GU  ARTD-PCB SALDO-WDD811 SSA1                    
031620     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
031621     PERFORM IMS-STATUSCHECK                                              
031640     .                                                                    
031650     EJECT                                                                
031710 IMS-STATUSCHECK SECTION.                                                 
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GOOD-STATUS                                                   
032100       AT END                                                             
032200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
032300         DELIMITED BY SIZE INTO ERROR-TEXT                                
032400         CALL FELLOG                                                      
032500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
