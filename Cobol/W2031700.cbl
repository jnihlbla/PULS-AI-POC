000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2031700.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   16/05/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CAMPAIGN INFORMATION PART                                        
000900*                                                                         
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSACTION: W2T317                                              
001300*        MID:         W2I31701                                            
001400*                                                                         
001500*    OUTDATA.                                                             
001600*        MOD:         W2O31701                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W2031700'.            
004882 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004883 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004884 77  W-DATUM                     PIC 9(06)   VALUE ZERO.                  
004886 77  W-JUMP                      PIC X(01)   VALUE 'N'.                   
004887 01  W-IDKAMPRF-1.                                                        
004888     03 W-IDKAMPRF    OCCURS 7   PIC 9(01).                               
004889 01  W-IDKAMP.                                                            
004890     03 W-IDKAMP-ALFA OCCURS 7   PIC X(01).                               
004891                                                                          
004892*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004893 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004894                                                                          
004895 77  YES                         PIC X       VALUE 'J'.                   
004896 77  NOO                         PIC X       VALUE 'N'.                   
004897                                                                          
004898*    --- INDEX FOR SCROLL LINES                                           
004899 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  IX2                         PIC S9(4)  VALUE +0    COMP SYNC.        
004901 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004902 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004903*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004904                                                                          
004905                                                                          
004906 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004907     88  KEYS-OK                             VALUE 'J'.                   
004908     88  KEYS-WRONG                          VALUE 'N'.                   
004909                                                                          
004910 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004911     88  OWN-MID                             VALUE '2317'.                
004912     88  GOOD-MID                            VALUE '2311' '2312'          
004913                                                   '2313' '2314'          
004914                                                   '2315' '2316'          
004920                                                   '2317' '2318'          
005000                                                   '2319'.                
005100     88  HELP-MID                            VALUE '0551'.                
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     EJECT                                                                
006000*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006100*01 -COPY WMEDAREA                                                        
006200     SKIP3                                                                
006300 01  MESSAGE-CODES.                                                       
006400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006600     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
006700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006710     03  ERR-WRONG-SEL-CODE      PIC X(3)    VALUE '416'.                 
006720     03  ERR-NO-LINE-SELECTED    PIC X(3)    VALUE '362'.                 
006800     EJECT                                                                
006900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
007000*                                                                         
007100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007200     SKIP3                                                                
007300*01 -COPY WMSGINIT                                                        
007400     EJECT                                                                
007500*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
007600*                                                                         
007700 01  SAVE-AREA.                                                           
007800     03  SAVE-IDTRANS             PIC X(4)   VALUE '2317'.                
007900     03  SAVE-IDARTNR-ENTER       PIC X(09)  VALUE SPACE.                 
008000     03  SAVE-IDDC-ENTER          PIC X(02)  VALUE SPACE.                 
008010     03  SAVE-IDKAMPRF-ENTER      PIC X(07)  VALUE SPACE.                 
008100     03  SAVE-IDARTNR-NEXT        PIC X(09)  VALUE SPACE.                 
008200     03  SAVE-IDDC-NEXT           PIC X(02)  VALUE SPACE.                 
008210     03  SAVE-IDKAMPRF-NEXT       PIC X(07)  VALUE SPACE.                 
008300     EJECT                                                                
008400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008700     SKIP3                                                                
008800*01  MID -COPY W2I31701                                                   
008900     EJECT                                                                
008910 01  FILLER                PIC X(16)   VALUE 'MID TILL 2311 '.            
008920 01  ALT1-MSG-AREA.                                                       
008940     03  ALT1-LL           PIC S9(4)   VALUE +0 COMP SYNC.                
008950     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
008960     03  ALT1-KDTRANS      PIC X(8)    VALUE 'W2T311  '.                  
008970     03  ALT1-IDTRANS      PIC X(4)    VALUE '2317'.                      
008980     03  ALT1-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
008990     03  MID -COPY W2I31101    -PRE ALT1-                                 
008991     EJECT                                                                
008992 01  ALT2-MSG-AREA.                                                       
008993     03  ALT2-LL           PIC S9(4)   VALUE +0 COMP SYNC.                
008994     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
008995     03  ALT2-KDTRANS      PIC X(8)    VALUE 'W2T312  '.                  
008996     03  ALT2-IDTRANS      PIC X(4)    VALUE '2317'.                      
008997     03  ALT2-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
008998     03  MID -COPY W2I31201    -PRE ALT2-                                 
008999     EJECT                                                                
009000 01  ALT3-MSG-AREA.                                                       
009001     03  ALT3-LL           PIC S9(4)   VALUE +0 COMP SYNC.                
009002     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
009003     03  ALT3-KDTRANS      PIC X(8)    VALUE 'W2T313  '.                  
009004     03  ALT3-IDTRANS      PIC X(4)    VALUE '2317'.                      
009005     03  ALT3-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
009006     03  MID -COPY W2I31301    -PRE ALT3-                                 
009007     EJECT                                                                
009008 01  ALT4-MSG-AREA.                                                       
009009     03  ALT4-LL           PIC S9(4)   VALUE +0 COMP SYNC.                
009010     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
009011     03  ALT4-KDTRANS      PIC X(8)    VALUE 'W2T314  '.                  
009012     03  ALT4-IDTRANS      PIC X(4)    VALUE '2317'.                      
009013     03  ALT4-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
009014     03  MID -COPY W2I31401    -PRE ALT4-                                 
009015     EJECT                                                                
009016 01  ALT5-MSG-AREA.                                                       
009017     03  ALT5-LL           PIC S9(4)   VALUE +0 COMP SYNC.                
009018     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
009019     03  ALT5-KDTRANS      PIC X(8)    VALUE 'W2T315  '.                  
009020     03  ALT5-IDTRANS      PIC X(4)    VALUE '2317'.                      
009021     03  ALT5-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
009022     03  MID -COPY W2I31501    -PRE ALT5-                                 
009023     EJECT                                                                
009024 01  ALT6-MSG-AREA.                                                       
009025     03  ALT6-LL           PIC S9(4)   VALUE +0 COMP SYNC.                
009026     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
009027     03  ALT6-KDTRANS      PIC X(8)    VALUE 'W2T316  '.                  
009028     03  ALT6-IDTRANS      PIC X(4)    VALUE '2317'.                      
009029     03  ALT6-KDMFSFOR     PIC X(1)    VALUE SPACE.                       
009030     03  MID -COPY W2I31601    -PRE ALT6-                                 
009031     EJECT                                                                
009032                                                                          
009040 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009100     SKIP3                                                                
009200*01  -COPY WMSGAREA                                                       
009300     EJECT                                                                
009400     03  MOD REDEFINES MSG-AREA.                                          
009500*      05  -COPY W2O31701                                                 
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009800     SKIP3                                                                
009900*01  -COPY WMFSAREA                                                       
010000     EJECT                                                                
010100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  KEYS-FOR-DLI.                                                        
010600*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
010700                                                                          
011300     03  W-WDM2A1KY-MIN-X.                                                
011310         05  W-IDARTNR-MIN      PIC S9(9)           COMP-3.               
011320         05  W-IDKAMPRF-MIN     PIC S9(7)           COMP-3.               
011330         05  W-IDDC-MIN         PIC X(02).                                
011340                                                                          
011341     03  W-WDM2A1KY-MAX-X.                                                
011342         05  W-IDARTNR-MAX      PIC S9(9)           COMP-3.               
011343         05  W-IDKAMPRF-MAX     PIC S9(7)           COMP-3.               
011344         05  W-IDDC-MAX         PIC X(02).                                
011345                                                                          
011346     03  W-IDDC-X.                                                        
011347         05  W-IDDC             PIC X(02).                                
011348                                                                          
011349     03  W-WDM201-X.                                                      
011350         05  W-KAMP-IDKAMPRF    PIC S9(7)           COMP-3.               
011351         05  W-KAMP-IDDC        PIC X(02).                                
011352                                                                          
011353     03  W-WDM211-X.                                                      
011360         05  W-KART-IDARTNR     PIC S9(9)           COMP-3.               
011361                                                                          
012200     SKIP2                                                                
012300*    --- STATUS CODES FROM IMS                                            
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FOUND                       VALUE '  '.                  
012600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012710     88  SEGMENT-END                         VALUE 'GB'.                  
012800     SKIP2                                                                
012900 01  GOOD-STATUSCODES.                                                    
013000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(128).                              
013300 01  SSA2                        PIC X(128).                              
013400     EJECT                                                                
013500*    --- IMS FUNCTION CODES                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900                                                                          
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM2A1'.         
014200 01  DLI-IO-WDM2A1.                                                       
014300*    03 -COPY WDM2A1                                                      
014400     EJECT                                                                
014410 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
014420 01  DLI-IO-WDM201.                                                       
014430*    03 -COPY WDM201                                                      
014440     EJECT                                                                
014500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
014600 01  DLI-IO-WDM211.                                                       
014700*    03 -COPY WDM211                                                      
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000*01  -COPY W0009   -PRE MSG-                                              
015001                                                                          
015010*    PSB FÖR HOPP TILL 2311                                               
015100*01  -COPY W0009   -PRE ALT1-                                             
015300                                                                          
015301*    PSB FÖR HOPP TILL 2312                                               
015302*01  -COPY W0009   -PRE ALT2-                                             
015304                                                                          
015305*    PSB FÖR HOPP TILL 2313                                               
015306*01  -COPY W0009   -PRE ALT3-                                             
015307                                                                          
015308*    PSB FÖR HOPP TILL 2314                                               
015309*01  -COPY W0009   -PRE ALT4-                                             
015310                                                                          
015311*    PSB FÖR HOPP TILL 2315                                               
015312*01  -COPY W0009   -PRE ALT5-                                             
015313                                                                          
015314*    PSB FÖR HOPP TILL 2316                                               
015315*01  -COPY W0009   -PRE ALT6-                                             
015316                                                                          
015317*01  -COPY W0008   -PRE WDP7-                                             
015320     05  FILLER                  PIC X.                                   
015330                                                                          
015400*01  -COPY W0008   -PRE WDM2A-                                            
015500     05  FILLER                  PIC X.                                   
015510                                                                          
015610*01  -COPY W0008   -PRE WDM2-                                             
015620     05  FILLER                  PIC X.                                   
015630     EJECT                                                                
015700                                                                          
015800 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
015801                                   ALT4-PCB ALT5-PCB ALT6-PCB             
015810                                   WDP7-PCB WDM2A-PCB WDM2-PCB.           
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
016001                                   ALT4-PCB ALT5-PCB ALT6-PCB             
016010                                   WDP7-PCB WDM2A-PCB WDM2-PCB.           
016100                                                                          
016200     PERFORM IMS-GET-MSG                                                  
016300     IF SEGMENT-FOUND                                                     
016400       PERFORM A-INIT                                                     
016500       PERFORM B-CHECK-KEYS                                               
016600       IF KEYS-OK                                                         
016700          IF MFS-FIRST                                                    
016800            PERFORM C-FIRST-PAGE                                          
016900          ELSE                                                            
017000            IF MFS-NEXT                                                   
017100              PERFORM D-NEXT-PAGE                                         
017200            ELSE                                                          
017300              PERFORM E-SAME-PAGE                                         
017400            END-IF                                                        
017500          END-IF                                                          
017601                                                                          
017602          PERFORM F-READ-SHOW-INFO                                        
017603                                                                          
017700       END-IF                                                             
017710                                                                          
017720       IF MFS-SPLIT                                                       
017730          PERFORM I-JUMP-TO-OTHER-SCREEN                                  
017740       ELSE                                                               
018000         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31701 + 4                    
018100         PERFORM IMS-INSERT-MSG                                           
018110       END-IF                                                             
018200     END-IF                                                               
018300                                                                          
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900     MOVE 'A-INIT                  '  TO CURRENT-SECTION                  
019000                                                                          
019100     IF MSG-DOUBLE-TRANSACTIONS                                           
019200       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I31701                 
019300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019500     ELSE                                                                 
019600       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W2I31701                 
019700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019900     END-IF                                                               
020000                                                                          
020100     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
020200     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
020300     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
020400                                                                          
020500     MOVE LOW-VALUE        TO MSG-AREA                                    
020600     MOVE 'W2O317N1'       TO MFS-IDMOD                                   
020700     MOVE '2317'           TO MOD-IDTRANS                                 
020800     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL                                
020900                              MOD-TEMFSINF                                
021000                                                                          
021100     IF OWN-MID OR HELP-MID                                               
021200       CONTINUE                                                           
021300     ELSE                                                                 
021400       MOVE SPACE          TO MFS-KDTRTYP                                 
021500       MOVE '7'            TO MFS-IDPFK                                   
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 B-CHECK-KEYS SECTION.                                                    
022000     MOVE 'B-CHECK-KEYS            '  TO CURRENT-SECTION                  
022100                                                                          
022200     MOVE ALL '+'            TO MSGI-WMSGINIT                             
022300     MOVE '001'              TO MSGI-KDCALL                               
022400     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
022500     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
022600     MOVE '2317'             TO MSGI-IDTRANS                              
022700     IF OWN-MID                                                           
022900        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
023200        MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                              
023400     END-IF                                                               
023500                                                                          
023600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023710     IF MSGI-SPAR-AREA (1:4) = '2317'                                     
023720        MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                 
023730     END-IF                                                               
023800                                                                          
023900*    - LANGUAGE TO BE USED BY MEDKONV                                     
024000     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
024100                                                                          
024200     MOVE YES                TO KEYS-SW                                   
024300                                                                          
024303*    -- KONTROLL AV IDARTNR                                               
024310     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
024400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
024500        MOVE '7'             TO MFS-IDPFK                                 
024600        MOVE SPACE           TO MFS-KDTRTYP                               
024700     END-IF                                                               
024710                                                                          
024711     MOVE LOW-VALUE          TO W-WDM2A1KY-MIN-X                          
024712     MOVE HIGH-VALUE         TO W-WDM2A1KY-MAX-X                          
024720     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
024730     IF MSGI-IDARTNR NUMERIC                                              
024800        MOVE MSGI-IDARTNR    TO W-IDARTNR-MIN                             
024900                                W-IDARTNR-MAX                             
024901     ELSE                                                                 
024902        MOVE NOO             TO KEYS-SW                                   
024903     END-IF                                                               
024910                                                                          
024911*    -- KONTROLL AV IDDC                                                  
024920     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
024930     IF MID-IDDC-IN NOT = ALL '+'                                         
024940        MOVE '7'             TO MFS-IDPFK                                 
024950        MOVE SPACE           TO MFS-KDTRTYP                               
024960     END-IF                                                               
025000     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
025100                                                                          
025200     IF GOOD-MID OR KEYS-OK                                               
025300       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
025310       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZEROES BY SPACE           
025400       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
025500     ELSE                                                                 
025600       MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR-UT                            
025700                                MOD-IDDC-UT                               
025800     END-IF                                                               
025900                                                                          
026000     IF KEYS-WRONG                                                        
026100       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
026200       CALL WMEDKONV USING MED-WMEDAREA                                   
026300       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
026400       PERFORM MFS-ERASE-FIELD-IN                                         
026500       PERFORM MFS-ERASE-FIELD-OUT                                        
026600     END-IF                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 C-FIRST-PAGE SECTION.                                                    
027000     MOVE 'C-FIRST-PAGE            '  TO CURRENT-SECTION                  
027100                                                                          
027200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
027300     CALL WMEDKONV USING MED-WMEDAREA                                     
027400     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
027500                                                                          
027600     PERFORM MFS-ERASE-FIELD-IN                                           
027700     .                                                                    
027800     EJECT                                                                
027900 D-NEXT-PAGE SECTION.                                                     
028000     MOVE 'D-NEXT-PAGE             '  TO CURRENT-SECTION                  
028100                                                                          
028200     IF SAVE-IDTRANS = '2317'                                             
028210       INSPECT SAVE-IDARTNR-NEXT  REPLACING LEADING SPACE BY ZERO         
028300       MOVE SAVE-IDARTNR-NEXT    TO W-IDARTNR-MIN                         
028401       INSPECT SAVE-IDKAMPRF-NEXT  REPLACING LEADING SPACE BY ZERO        
028410       MOVE SAVE-IDKAMPRF-NEXT   TO W-IDKAMPRF-MIN                        
028420       MOVE SAVE-IDDC-NEXT       TO W-IDDC-MIN                            
028500     ELSE                                                                 
028600       PERFORM MFS-ERASE-FIELD-IN                                         
028700     END-IF                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 E-SAME-PAGE SECTION.                                                     
029100     MOVE 'E-SAME-PAGE             '  TO CURRENT-SECTION                  
029200                                                                          
029300     IF SAVE-IDTRANS = '2317' OR '0551'                                   
029400       MOVE SAVE-IDARTNR-ENTER   TO W-IDARTNR-MIN                         
029500       MOVE SAVE-IDDC-ENTER      TO W-IDDC-MIN                            
029510       MOVE SAVE-IDKAMPRF-ENTER  TO W-IDKAMPRF-MIN                        
029600       IF MID-INPUT = ALL '+'                                             
029700         PERFORM MFS-ERASE-FIELD-IN                                       
029800       ELSE                                                               
029810         IF NOT MFS-SPLIT                                                 
029900            MOVE INF-PRESS-PF9    TO MED-IDMFSINF                         
030000            CALL WMEDKONV      USING MED-WMEDAREA                         
030100            MOVE MED-MFSINF       TO MOD-TEMFSFEL                         
030200            PERFORM EA-MID-INDATA-FOR-MOD                                 
030210         END-IF                                                           
030300       END-IF                                                             
030400     ELSE                                                                 
030500       PERFORM MFS-ERASE-FIELD-IN                                         
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 EA-MID-INDATA-FOR-MOD SECTION.                                           
031000     MOVE 'EA-MID-INDATA-FOR-MOD   '  TO CURRENT-SECTION                  
031100                                                                          
031200     MOVE +1                      TO INDX                                 
031300     PERFORM UNTIL INDX > MAX-INDX                                        
031400       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
031500          MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD         (INDX)             
031600       ELSE                                                               
031700          MOVE MFS-RENSA-FAELT    TO MOD-KDCMD         (INDX)             
031800       END-IF                                                             
031900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR    (INDX)             
032000                                     MOD-IDKAMPRF-ATTR (INDX)             
032100       ADD +1                     TO INDX                                 
032200     END-PERFORM                                                          
032300     .                                                                    
032400     EJECT                                                                
032500 F-READ-SHOW-INFO SECTION.                                                
032600     MOVE 'F-READ-SHOW-INFO        '  TO CURRENT-SECTION                  
033200                                                                          
033210     IF MSGI-IDDC-KEY = SPACE                                             
033222        PERFORM IMS-GU-WDM2A1                                             
033224     ELSE                                                                 
033225        PERFORM IMS-GU-WDM2A1-DC                                          
033227     END-IF                                                               
033730                                                                          
033800     MOVE +0                           TO INDX                            
033900     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
034000                OR INDX = MAX-INDX                                        
034100                                                                          
034321          MOVE SEQA-IDKAMPRF           TO W-KAMP-IDKAMPRF                 
034322          MOVE SEQA-IDDC               TO W-KAMP-IDDC                     
034400          PERFORM IMS-GU-WDM201                                           
034401          IF SEGMENT-FOUND                                                
034403            MOVE SEQA-IDARTNR          TO W-KART-IDARTNR                  
034404            PERFORM IMS-GNP-WDM211                                        
034405            IF SEGMENT-FOUND                                              
034407              ADD +1                   TO INDX                            
034500              MOVE KAMP-IDDC           TO MOD-IDDC        (INDX)          
034600              MOVE KAMP-IDKAMPRF       TO MOD-IDKAMPRF    (INDX)          
034700              MOVE KART-KVBEART-KAMP   TO MOD-KVBEART-KAMP(INDX)          
034800              MOVE KART-TIRES          TO W-DATUM                         
034810              MOVE W-DATUM             TO MOD-TIRES       (INDX)          
034811              IF MOD-TIRES (INDX) = ZERO                                  
034820                 INSPECT MOD-TIRES(INDX)                                  
034830                         REPLACING LEADING ZERO BY SPACE                  
034840              END-IF                                                      
034900              MOVE KART-KVRESS-KAMP    TO MOD-KVRESS-KAMP (INDX)          
035000              COMPUTE MOD-KVRESS-REMAIN (INDX) =                          
035100                      KART-KVRESS-KAMP - KART-KVBEART-KUND                
035200              MOVE KART-KVBEART-KUND   TO MOD-KVBEART-KUND(INDX)          
035300                                                                          
035400              MOVE KAMP-TISTADAT       TO W-DATUM                         
035410              MOVE W-DATUM             TO MOD-TISTADAT    (INDX)          
035411              IF MOD-TISTADAT (INDX) = ZERO                               
035420                 INSPECT MOD-TISTADAT(INDX)                               
035430                         REPLACING LEADING ZERO BY SPACE                  
035440              END-IF                                                      
035500              MOVE KAMP-TISTODAT       TO W-DATUM                         
035501              MOVE W-DATUM             TO MOD-TISTODAT    (INDX)          
035502              IF MOD-TISTODAT(INDX) = ZERO                                
035503                 INSPECT MOD-TISTODAT(INDX)                               
035504                         REPLACING LEADING ZERO BY SPACE                  
035505              END-IF                                                      
035506              IF INDX = +1                                                
035507                 MOVE SEQA-IDARTNR     TO SAVE-IDARTNR-ENTER              
035508                 MOVE SEQA-IDKAMPRF    TO SAVE-IDKAMPRF-ENTER             
035509                 MOVE SEQA-IDDC        TO SAVE-IDDC-ENTER                 
035510              END-IF                                                      
035511            END-IF                                                        
035520          END-IF                                                          
035600                                                                          
035610          IF MSGI-IDDC-KEY = SPACE                                        
035620             PERFORM IMS-GN-WDM2A1                                        
035630          ELSE                                                            
035640             PERFORM IMS-GN-WDM2A1-DC                                     
035650          END-IF                                                          
036000                                                                          
036100     END-PERFORM                                                          
036200                                                                          
036300     PERFORM UNTIL INDX = MAX-INDX                                        
036310       ADD 1                 TO INDX                                      
036400       MOVE MFS-ERASE-FIELD  TO MOD-IDKAMPRF          (INDX)              
036500                                MOD-IDDC              (INDX)              
036600                                MOD-KVBEART-KAMP      (INDX)              
036700                                MOD-TIRES             (INDX)              
036800                                MOD-KVRESS-KAMP       (INDX)              
036900                                MOD-KVRESS-REMAIN     (INDX)              
037000                                MOD-KVBEART-KUND      (INDX)              
037100                                MOD-TISTADAT          (INDX)              
037200                                MOD-TISTODAT          (INDX)              
037300       MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR        (INDX)              
037400     END-PERFORM                                                          
037500                                                                          
037600     IF SEGMENT-FOUND                                                     
037700        MOVE SEQA-IDARTNR   TO SAVE-IDARTNR-NEXT                          
037810        MOVE SEQA-IDKAMPRF  TO SAVE-IDKAMPRF-NEXT                         
037820        MOVE SEQA-IDDC      TO SAVE-IDDC-NEXT                             
037900        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
038000        CALL WMEDKONV    USING MED-WMEDAREA                               
038100        MOVE MED-TEMFSINF   TO MOD-TEMFSINF                               
038200     ELSE                                                                 
038210        MOVE SAVE-IDARTNR-ENTER  TO SAVE-IDARTNR-NEXT                     
038220        MOVE SAVE-IDKAMPRF-ENTER TO SAVE-IDKAMPRF-NEXT                    
038230        MOVE SAVE-IDDC-ENTER     TO SAVE-IDDC-NEXT                        
038500     END-IF                                                               
038600                                                                          
038700     MOVE '002'             TO MSGI-KDCALL                                
038800     MOVE '2317'            TO SAVE-IDTRANS                               
038900     MOVE SAVE-AREA         TO MSGI-SPAR-AREA                             
039000     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
039100     .                                                                    
039200     EJECT                                                                
039210 I-JUMP-TO-OTHER-SCREEN SECTION.                                          
039211     MOVE 'I-JUMP-TO-OTHER-SCREEN' TO CURRENT-SECTION                     
039212                                                                          
039218     MOVE YES                        TO W-JUMP                            
039219                                                                          
039220     IF MID-INPUT = ALL '+'                                               
039223        MOVE ERR-NO-LINE-SELECTED    TO MED-IDMFSFEL                      
039224        CALL WMEDKONV             USING MED-WMEDAREA                      
039225        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
039227        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31701 + 4                     
039228        PERFORM IMS-INSERT-MSG                                            
039229        MOVE NOO                     TO W-JUMP                            
039230     END-IF                                                               
039231                                                                          
039232     MOVE +1                         TO INDX                              
039233     PERFORM UNTIL INDX > MAX-INDX OR W-JUMP = NOO                        
039234       IF MID-KDCMD (INDX) NOT = '+'                                      
039235          IF MID-KDCMD (INDX) < '1'                                       
039236          OR MID-KDCMD (INDX) > '6'                                       
039237             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
039238             MOVE ERR-WRONG-SEL-CODE TO MED-IDMFSFEL                      
039239             CALL WMEDKONV        USING MED-WMEDAREA                      
039240             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
039241             COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31701 + 4                
039242             PERFORM IMS-INSERT-MSG                                       
039243             MOVE NOO                TO W-JUMP                            
039247           END-IF                                                         
039248       END-IF                                                             
039249                                                                          
039252       IF W-JUMP = YES                                                    
039253         IF MID-KDCMD (INDX) = '1'                                        
039254           COMPUTE ALT1-LL = LENGTH OF ALT1-MID-W2I31101-CTX + 17         
039255           MOVE ALL '+'               TO ALT1-MID                         
039256           MOVE MOD-IDKAMPRF   (INDX) TO ALT1-MID-IDKAMPRF-IN             
039257           MOVE MOD-IDDC       (INDX) TO ALT1-MID-IDDC-IN                 
039258           MOVE MSGI-IDARTNR          TO ALT1-MID-IDARTNR-IN              
039259           MOVE MFS-KDMFSFOR          TO ALT1-KDMFSFOR                    
039260                                                                          
039261           PERFORM IMS-INSERT-ALT1-2311                                   
039262           MOVE MAX-INDX              TO INDX                             
039263         END-IF                                                           
039264                                                                          
039265         IF MID-KDCMD (INDX) = '2'                                        
039266           COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I31201-CTX + 1          
039267           MOVE ALL '+'               TO ALT2-MID                         
039268           MOVE MOD-IDKAMPRF   (INDX) TO ALT2-MID-IDKAMPRF-IN             
039269           MOVE MOD-IDDC       (INDX) TO ALT2-MID-IDDC-IN                 
039270           MOVE MSGI-IDARTNR          TO ALT2-MID-IDARTNR-IN              
039271           MOVE MFS-KDMFSFOR          TO ALT2-KDMFSFOR                    
039272           PERFORM IMS-INSERT-ALT2-2312                                   
039273           MOVE MAX-INDX              TO INDX                             
039274         END-IF                                                           
039275                                                                          
039279         IF MID-KDCMD (INDX) = '3' OR '4' OR '5' OR '6'                   
039280            MOVE MOD-IDKAMPRF (INDX)  TO W-IDKAMPRF-1                     
039281            MOVE SPACE                TO W-IDKAMP                         
039282            MOVE +1                   TO IX1                              
039283            MOVE +0                   TO IX2                              
039284            PERFORM UNTIL IX1 > +7                                        
039285              IF W-IDKAMPRF (IX1) > ZERO                                  
039286                 ADD +1               TO IX2                              
039289                 MOVE W-IDKAMPRF(IX1) TO W-IDKAMP-ALFA (IX2)              
039292                 ADD +1               TO IX1                              
039293                 PERFORM UNTIL IX1 > +7                                   
039294                   ADD +1             TO IX2                              
039295                   MOVE W-IDKAMPRF(IX1) TO W-IDKAMP-ALFA (IX2)            
039298                   ADD +1             TO IX1                              
039299                 END-PERFORM                                              
039300              ELSE                                                        
039301                 ADD +1               TO IX1                              
039302              END-IF                                                      
039303            END-PERFORM                                                   
039304         END-IF                                                           
039305                                                                          
039306         IF MID-KDCMD (INDX) = '3'                                        
039307           COMPUTE ALT3-LL = LENGTH OF ALT3-MID-W2I31301 + 1              
039308           MOVE ALL '+'               TO ALT3-MID                         
039309           MOVE W-IDKAMP              TO ALT3-MID-IDKAMP-IN               
039310           MOVE MFS-KDMFSFOR          TO ALT3-KDMFSFOR                    
039311           PERFORM IMS-INSERT-ALT3-2313                                   
039312           MOVE MAX-INDX              TO INDX                             
039313         END-IF                                                           
039314                                                                          
039315         IF MID-KDCMD (INDX) = '4'                                        
039316           COMPUTE ALT4-LL = LENGTH OF ALT4-MID-W2I31401 + 1              
039317           MOVE ALL '+'               TO ALT4-MID                         
039318           MOVE W-IDKAMP              TO ALT4-MID-IDKAMP-IN               
039319           MOVE MFS-KDMFSFOR          TO ALT4-KDMFSFOR                    
039320           PERFORM IMS-INSERT-ALT4-2314                                   
039321           MOVE MAX-INDX              TO INDX                             
039322         END-IF                                                           
039323                                                                          
039324         IF MID-KDCMD (INDX) = '5'                                        
039325           COMPUTE ALT5-LL = LENGTH OF ALT5-MID-W2I31501-CTX + 1          
039326           MOVE ALL '+'               TO ALT5-MID                         
039327           MOVE MSGI-IDARTNR          TO ALT5-MID-IDARTNR                 
039328           MOVE MFS-KDMFSFOR          TO ALT5-KDMFSFOR                    
039329           PERFORM IMS-INSERT-ALT5-2315                                   
039330           MOVE MAX-INDX              TO INDX                             
039331         END-IF                                                           
039332                                                                          
039333         IF MID-KDCMD (INDX) = '6'                                        
039334           COMPUTE ALT6-LL = LENGTH OF ALT6-MID-W2I31601 + 1              
039335           MOVE ALL '+'               TO ALT6-MID                         
039336           MOVE W-IDKAMP              TO ALT6-MID-IDKAMP-IN               
039337           MOVE MFS-KDMFSFOR          TO ALT6-KDMFSFOR                    
039338           PERFORM IMS-INSERT-ALT6-2316                                   
039339           MOVE MAX-INDX              TO INDX                             
039340         END-IF                                                           
039341       END-IF                                                             
039342                                                                          
039343       ADD +1                       TO INDX                               
039344                                                                          
039345     END-PERFORM                                                          
039346     .                                                                    
039347     EJECT                                                                
039350 MFS-ERASE-FIELD-OUT SECTION.                                             
039400                                                                          
039500*    --- ALLA UTDATA-FÄLT                                                 
039600*    --- INCL. SCROLL KEYS                                                
039700     MOVE +1                      TO INDX                                 
039800     PERFORM UNTIL INDX > MAX-INDX                                        
039900       MOVE MFS-ERASE-FIELD       TO MOD-KDCMD (INDX)                     
040000                                                                          
040100       ADD +1                     TO INDX                                 
040200     END-PERFORM                                                          
040300     .                                                                    
040400     SKIP3                                                                
040500 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
040600                                                                          
040700*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
040800                                                                          
040900     MOVE +1                      TO INDX                                 
041000     PERFORM UNTIL INDX > MAX-INDX                                        
041100       MOVE MFS-ERASE-FIELD       TO MOD-KDCMD (INDX)                     
041200       ADD +1                     TO INDX                                 
041300     END-PERFORM                                                          
041400     .                                                                    
041500     SKIP3                                                                
041600 MFS-ERASE-FIELD-IN SECTION.                                              
041700                                                                          
041800*    --- ALLA INDATA-FÄLT                                                 
041900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
042000                             MOD-IDDC-IN                                  
042001                                                                          
042002     MOVE +1                      TO INDX                                 
042003     PERFORM UNTIL INDX > MAX-INDX                                        
042030       MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                           
042050       ADD +1               TO INDX                                       
042060     END-PERFORM                                                          
042100     .                                                                    
042200     EJECT                                                                
042300 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
042400                                                                          
042500*    --- ALLA UTDATA-FÄLT                                                 
042600*    --- INCL SCROLL KEYS AND LINEDATA                                    
042700                                                                          
042800     MOVE +1                      TO INDX                                 
042900     PERFORM UNTIL INDX > MAX-INDX                                        
043000       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
043100       ADD +1                     TO INDX                                 
043200     END-PERFORM                                                          
043300     .                                                                    
043400     SKIP2                                                                
043500 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
043600                                                                          
043700*    --- OUTDATA FIELD ON SCROLL KEYS                                     
043800     MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDCMD         (INDX)             
043900     .                                                                    
044000     SKIP3                                                                
044100 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
044200                                                                          
044300*    --- ALLA INDATA-FÄLT                                                 
044400     MOVE +1                       TO INDX                                
044500     PERFORM UNTIL INDX > MAX-INDX                                        
044600       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD (INDX)                    
044700       ADD +1                      TO INDX                                
044800     END-PERFORM                                                          
044900     .                                                                    
045000     EJECT                                                                
045100 MFS-FORM-ATTR SECTION.                                                   
045200                                                                          
045300*    --- ALL INDATA-FIELDS                                                
045400     MOVE +1                        TO INDX                               
045500     PERFORM UNTIL INDX > MAX-INDX                                        
045600       MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDCMD-ATTR   (INDX)            
045700                                       MOD-IDKAMPRF-ATTR(INDX)            
045800       ADD +1                       TO INDX                               
045900     END-PERFORM                                                          
046000     .                                                                    
046100     SKIP2                                                                
046200 MFS-READ-IN-AGAIN SECTION.                                               
046300                                                                          
046400*    --- ALL INDATA-FIELDS                                                
046500     MOVE +1                        TO INDX                               
046600     PERFORM UNTIL INDX > MAX-INDX                                        
046700       MOVE MFS-ADD-READ-FIELD      TO MOD-KDCMD-ATTR (INDX)              
046800                                       MOD-IDKAMPRF-ATTR(INDX)            
046900       ADD +1                       TO INDX                               
047000     END-PERFORM                                                          
047100     .                                                                    
047200     EJECT                                                                
047300* --- IMS SECTIONS ---                                                    
047400     SKIP3                                                                
047500 IMS-GET-MSG SECTION.                                                     
047600                                                                          
047700     MOVE '  QC' TO GOOD-STATUSCODES                                      
047800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
047900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048000     PERFORM IMS-STATUSCHECK                                              
048100     .                                                                    
048200     SKIP3                                                                
048300 IMS-INSERT-MSG SECTION.                                                  
048400                                                                          
048800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
048900     MOVE SPACE TO GOOD-STATUSCODES                                       
049000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
049100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049200     PERFORM IMS-STATUSCHECK                                              
049300     .                                                                    
049400     EJECT                                                                
050717 IMS-GU-WDM2A1 SECTION.                                                   
050718     MOVE 'IMS-GN-WDM2A1            ' TO DBS-SECTION                      
050720                                                                          
050722     STRING 'WDM2A1  (WDM2A1KY>=' W-WDM2A1KY-MIN-X                        
050723                    '&WDM2A1KY<=' W-WDM2A1KY-MAX-X ')'                    
050726            DELIMITED BY SIZE INTO SSA1                                   
050727     MOVE '  GE'              TO GOOD-STATUSCODES                         
050728     CALL CBLTDLI USING GU WDM2A-PCB DLI-IO-WDM2A1 SSA1                   
050729     MOVE WDM2A-STATUS-CODE   TO STATUS-WS                                
050730     PERFORM IMS-STATUSCHECK                                              
050731     .                                                                    
050732                                                                          
050733 IMS-GN-WDM2A1 SECTION.                                                   
050734     MOVE 'IMS-GN-WDM2A1            ' TO DBS-SECTION                      
050737                                                                          
050738     STRING 'WDM2A1  (WDM2A1KY>=' W-WDM2A1KY-MIN-X                        
050739                    '&WDM2A1KY<=' W-WDM2A1KY-MAX-X ')'                    
050740            DELIMITED BY SIZE INTO SSA1                                   
050741     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
050742     CALL CBLTDLI USING GN WDM2A-PCB DLI-IO-WDM2A1 SSA1                   
050743     MOVE WDM2A-STATUS-CODE   TO STATUS-WS                                
050744     PERFORM IMS-STATUSCHECK                                              
050745     .                                                                    
050746                                                                          
050747 IMS-GU-WDM2A1-DC SECTION.                                                
050748     MOVE 'IMS-GN-WDM2A1            ' TO DBS-SECTION                      
050749                                                                          
050750     STRING 'WDM2A1  (WDM2A1KY>=' W-WDM2A1KY-MIN-X                        
050751                    '&WDM2A1KY<=' W-WDM2A1KY-MAX-X                        
050752                    '&IDDC     =' W-IDDC-X ')'                            
050753            DELIMITED BY SIZE INTO SSA1                                   
050754     MOVE '  GE'              TO GOOD-STATUSCODES                         
050755     CALL CBLTDLI USING GU WDM2A-PCB DLI-IO-WDM2A1 SSA1                   
050756     MOVE WDM2A-STATUS-CODE   TO STATUS-WS                                
050757     PERFORM IMS-STATUSCHECK                                              
050758     .                                                                    
050759                                                                          
050760 IMS-GN-WDM2A1-DC SECTION.                                                
050761     MOVE 'IMS-GN-WDM2A1            ' TO DBS-SECTION                      
050762                                                                          
050763     STRING 'WDM2A1  (WDM2A1KY>=' W-WDM2A1KY-MIN-X                        
050764                    '&WDM2A1KY<=' W-WDM2A1KY-MAX-X                        
050765                    '&IDDC     =' W-IDDC-X ')'                            
050766            DELIMITED BY SIZE INTO SSA1                                   
050767     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
050768     CALL CBLTDLI USING GN WDM2A-PCB DLI-IO-WDM2A1 SSA1                   
050769     MOVE WDM2A-STATUS-CODE   TO STATUS-WS                                
050770     PERFORM IMS-STATUSCHECK                                              
050771     .                                                                    
050772                                                                          
050773 IMS-GU-WDM201 SECTION.                                                   
050774     MOVE 'IMS-GU-WDM201            ' TO DBS-SECTION                      
050775                                                                          
050776     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
050777          DELIMITED BY SIZE INTO SSA1                                     
050778     MOVE '  GE'              TO GOOD-STATUSCODES                         
050779     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM201 SSA1                    
050780     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
050781     PERFORM IMS-STATUSCHECK                                              
050782     .                                                                    
050783                                                                          
050784 IMS-GNP-WDM211 SECTION.                                                  
050785     MOVE 'IMS-GNP-WDM211           ' TO DBS-SECTION                      
050786                                                                          
050790     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
050800          DELIMITED BY SIZE INTO SSA1                                     
050900     MOVE '  GE'              TO GOOD-STATUSCODES                         
051000     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM211 SSA1                   
051100     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
051110     PERFORM IMS-STATUSCHECK                                              
051300     .                                                                    
051400                                                                          
051500 IMS-INSERT-ALT1-2311 SECTION.                                            
051600     MOVE 'IMS-INSERT-ALT1-2311      ' TO DBS-SECTION                     
051700                                                                          
051800     MOVE SPACE           TO GOOD-STATUSCODES                             
051900     CALL CBLTDLI      USING ISRT ALT1-PCB ALT1-MSG-AREA                  
052000     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
052001     PERFORM IMS-STATUSCHECK                                              
052200     .                                                                    
052300                                                                          
052400 IMS-INSERT-ALT2-2312 SECTION.                                            
052500     MOVE 'IMS-INSERT-ALT2-2312      ' TO DBS-SECTION                     
052600                                                                          
052700     MOVE SPACE           TO GOOD-STATUSCODES                             
052800     CALL CBLTDLI      USING ISRT ALT2-PCB ALT2-MSG-AREA                  
052900     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
052910     PERFORM IMS-STATUSCHECK                                              
053100     .                                                                    
053200                                                                          
053300 IMS-INSERT-ALT3-2313 SECTION.                                            
053400     MOVE 'IMS-INSERT-ALT3-2313      ' TO DBS-SECTION                     
053500                                                                          
053600     MOVE SPACE           TO GOOD-STATUSCODES                             
053700     CALL CBLTDLI      USING ISRT ALT3-PCB ALT3-MSG-AREA                  
053800     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
053900     PERFORM IMS-STATUSCHECK                                              
054000     .                                                                    
054100                                                                          
054200 IMS-INSERT-ALT4-2314 SECTION.                                            
054300     MOVE 'IMS-INSERT-ALT4-2314      ' TO DBS-SECTION                     
054400                                                                          
054500     MOVE SPACE           TO GOOD-STATUSCODES                             
054600     CALL CBLTDLI      USING ISRT ALT4-PCB ALT4-MSG-AREA                  
054700     MOVE ALT4-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSCHECK                                              
054900     .                                                                    
055000                                                                          
055100 IMS-INSERT-ALT5-2315 SECTION.                                            
055200     MOVE 'IMS-INSERT-ALT5-2315      ' TO DBS-SECTION                     
055300                                                                          
055400     MOVE SPACE           TO GOOD-STATUSCODES                             
055500     CALL CBLTDLI      USING ISRT ALT5-PCB ALT5-MSG-AREA                  
055600     MOVE ALT5-STATUS-CODE TO STATUS-WS                                   
055700     PERFORM IMS-STATUSCHECK                                              
055800     .                                                                    
055900                                                                          
056000 IMS-INSERT-ALT6-2316 SECTION.                                            
056100     MOVE 'IMS-INSERT-ALT6-2316      ' TO DBS-SECTION                     
056200                                                                          
056300     MOVE SPACE           TO GOOD-STATUSCODES                             
056400     CALL CBLTDLI      USING ISRT ALT6-PCB ALT6-MSG-AREA                  
056500     MOVE ALT6-STATUS-CODE TO STATUS-WS                                   
056600     PERFORM IMS-STATUSCHECK                                              
056700     .                                                                    
056800                                                                          
060085 IMS-STATUSCHECK SECTION.                                                 
060086                                                                          
060087     SET STATUS-IX TO 1                                                   
060088     SEARCH GOOD-STATUS                                                   
060089       AT END                                                             
060090         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
060091         DELIMITED BY SIZE INTO ERROR-TEXT                                
060092         CALL FELLOG                                                      
060093       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
060094         CONTINUE                                                         
060095     END-SEARCH                                                           
060100     .                                                                    
