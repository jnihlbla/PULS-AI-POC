001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W6034200.                                                
001500 AUTHOR.         MARTIEN HOMPES.                                          
001600 DATE-WRITTEN.   97/02/06.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        MAINTANCE STORAGE CODE TABEL                                     
002100*                                                                         
002210*        THE PROGRAM UPDATES   WL6315 (WDR2)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: W6T342                                              
002600*        MID:         W6I34201                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        MOD:         W6O34201                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6034200'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  NEW                         PIC X        VALUE 'N'.                  
004401 77  DEL                         PIC X        VALUE 'D'.                  
004402 77  CHG                         PIC X        VALUE 'C'.                  
004403                                                                          
004404*    --- INDEX FOR SCROLL LINES                                           
004405 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004802     88  INDATA-OK                           VALUE 'Y'.                   
004810     88  INDATA-WRONG                        VALUE 'N'.                   
004900                                                                          
005000 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
005100     88  KEYS-OK                             VALUE 'Y'.                   
005200     88  KEYS-WRONG                          VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  OWN-MID                             VALUE '6342'.                
005600     88  GOOD-MID                            VALUE '6342'.                
006100     88  HELP-MID                            VALUE '0551'.                
006101                                                                          
006102 01  WS-KDMATT                   PIC X.                                   
006103     88 US-MEASUREMENT           VALUE 'U'.                               
006104     88 SIS-MEASUREMENT          VALUE 'S'.                               
006105                                                                          
006160 77  WS-KDSTOR-IN                PIC X(3)   VALUE SPACE.                  
006170 77  WS-KDSTOR                   PIC X(3)   VALUE SPACE.                  
006180 77  WS-TESTORAGE                PIC X(18)  VALUE SPACE.                  
006190 77  WS-DISTORD                  PIC 9(3)V9(1).                           
006191 77  WS-DISTORB                  PIC 9(3)V9(1).                           
006192 77  WS-DISTORH                  PIC 9(3)V9(1).                           
006193 77  WS-KDVSOP1                  PIC 9(3)   VALUE ZERO.                   
006194 77  WS-KDVSOP2                  PIC 9(3)   VALUE ZERO.                   
006195 77  WS-KDVSOP3                  PIC 9(3)   VALUE ZERO.                   
006196 77  WS-KDVSOP4                  PIC 9(3)   VALUE ZERO.                   
006197 77  WS-KDVSOP5                  PIC 9(3)   VALUE ZERO.                   
006198*      --- VALID IDDC CODES                                               
006199*                                                                         
006200*01    -COPY WWDC99                                                       
006210       EJECT                                                              
006300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006910     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
007000     EJECT                                                                
007100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007504     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007505     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007620     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007900     EJECT                                                                
008000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008410*    --- PARAMETERS FOR SUB PROGRAM WDECAREA                              
008420*                                                                         
008500 01  FILLER                      PIC X(16)  VALUE 'WDECAREA '.            
008503*01    -COPY WDECAREA                                                     
008504     SKIP3                                                                
008505*                                                                         
008506 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
008507*    -COPY WWOMVAND                                                       
008508     EJECT                                                                
008509*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008510*                                                                         
008511 01  SAVE-AREA.                                                           
008512     03  SAVE-IDTRANS        PIC X(4)    VALUE  SPACE.                    
008513     03  SAVE-KDSTOR-ENTER   PIC X(3).                                    
008520     03  SAVE-KDSTOR-NEXT    PIC X(3).                                    
008600     EJECT                                                                
008700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I34201                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W6O34201                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  KEYS-TO-DLI.                                                         
011001*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011002     03  W-KDSTOR-MIN-X.                                                  
011003          05  W-KDSTOR-MIN       PIC X(3).                                
011004                                                                          
011005     03  W-KDSTOR-MAX-X.                                                  
011006          05  W-KDSTOR-MAX       PIC X(3)    VALUE HIGH-VALUE.            
011007                                                                          
011008     03  W-WDGXKEY-6315-X.                                                
011009          05 W-6315-IDHTYP       PIC X(4)    VALUE '6315'.                
011010          05 W-6315-IDDC         PIC X(2)    VALUE SPACE.                 
011011          05 W-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
011012                                                                          
011013     03  W-WDGXKEY-6316-X.                                                
011020         05  W-KDSTOR            PIC X(3)    VALUE SPACE.                 
011100     SKIP2                                                                
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
013002 01  FILLER         PIC X(30) VALUE 'WL631501-AREA'.                      
013005 01  WL631501-AREA.                                                       
013006*    03  -COPY WDGX6315                                                   
013007     EJECT                                                                
013009 01  FILLER         PIC X(23) VALUE 'WL631511-AREA'.                      
013011 01  WL631511-AREA.                                                       
013020*    03  -COPY WDGX6316                                                   
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE 6315-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6315-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6315-PCB.                     
014100                                                                          
014200     PERFORM IMS-GET-MSG                                                  
014210     IF SEGMENT-FOUND                                                     
014220       PERFORM A-INIT                                                     
014240          PERFORM B-CHECK-KEYS                                            
014250          IF KEYS-OK                                                      
014260             IF MFS-UPDATE                                                
014270                PERFORM G-CHECK-INPUT                                     
014280                IF INDATA-OK                                              
014290                   PERFORM H-UPDATE                                       
014291                   PERFORM F-READ-SHOW-INFO                               
014292                 END-IF                                                   
014293              ELSE                                                        
014294               IF MFS-FIRST                                               
014295                  PERFORM C-FIRST-PAGE                                    
014296                ELSE                                                      
014297                  IF MFS-NEXT                                             
014298                     PERFORM D-NEXT-PAGE                                  
014299                   ELSE                                                   
014300                     PERFORM E-SAME-PAGE                                  
014301                  END-IF                                                  
014302               END-IF                                                     
014303               PERFORM F-READ-SHOW-INFO                                   
014304             END-IF                                                       
014305          END-IF                                                          
014307       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34201 + 4                      
014308       PERFORM IMS-INSERT-MSG                                             
014309     END-IF                                                               
014310                                                                          
014311     MOVE ZERO TO RETURN-CODE                                             
014312     GOBACK                                                               
014320                                                                          
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DOUBLE-TRANSACTIONS                                           
016800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34201                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W6I34201                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
017800     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
017900     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
018000                                                                          
018100     MOVE LOW-VALUE        TO MSG-AREA                                    
018200     MOVE 'W6O342N1'       TO MFS-IDMOD                                   
018300     MOVE '6342'           TO MOD-IDTRANS                                 
018400     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
018401                                                                          
018600     IF GOOD-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE                         TO MFS-KDTRTYP                  
019000       MOVE '7'                           TO MFS-IDPFK                    
019010       PERFORM MFS-INIT-KEY-FIELD-IN                                      
019020       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
019030       PERFORM MFS-ERASE-FIELD-IN                                         
019040       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
019100     END-IF                                                               
019200                                                                          
019300     IF MID-KDSTOR-IN NOT = ALL '+'                                       
019310       MOVE '7'         TO MFS-IDPFK                                      
019320       MOVE SPACE       TO MFS-KDTRTYP                                    
019330     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-CHECK-KEYS SECTION.                                                    
019700                                                                          
021001                                                                          
021002     MOVE YES               TO KEYS-SW                                    
021003                                                                          
021004     MOVE MFS-ERASE-FIELD   TO MOD-KDSTOR-IN                              
021005                                                                          
021006     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021007     MOVE '001'             TO MSGI-KDCALL                                
021008     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021009     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021010     MOVE '6342'            TO MSGI-IDTRANS                               
021011                                                                          
021012     IF OWN-MID                                                           
021013        MOVE MID-KDSTOR-IN TO MSGI-KDSTOR                                 
021014     END-IF                                                               
021015                                                                          
021016     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021017*                                                                         
021018*    -- CONTROL  ON STORAGE CODE                                          
021023     IF MSGI-KDSTOR     NUMERIC                                           
021024        MOVE 'A00' TO MSGI-KDSTOR                                         
021025     ELSE                                                                 
021026        IF MSGI-KDSTOR = SPACE                                            
021027          MOVE 'A00' TO MSGI-KDSTOR                                       
021030        END-IF                                                            
021031     END-IF                                                               
021035*    -- END OF CONTROL                                                    
021037*                                                                         
021039                                                                          
021040     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
021041     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
021042     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
021043     MOVE MSGI-IDDC         TO WS-IDDC                                    
021044     MOVE MSGI-KDSTOR       TO WS-KDSTOR                                  
021045                                                                          
021046     IF CDC                                                               
021047       CONTINUE                                                           
021048     ELSE                                                                 
021049       MOVE NOO                 TO KEYS-SW                                
021050     END-IF                                                               
021051                                                                          
021052     MOVE WS-KDSTOR             TO MOD-KDSTOR-UT                          
021053     MOVE WS-IDDC               TO MOD-IDDC                               
021060                                                                          
021300     IF KEYS-WRONG                                                        
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-ERASE-FIELD-IN                                         
021710       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FIRST-PAGE SECTION.                                                    
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
022107                                                                          
022108     PERFORM MFS-ERASE-FIELD-IN                                           
022110     MOVE WS-KDSTOR      TO W-KDSTOR                                      
022111     .                                                                    
022112     EJECT                                                                
022113 D-NEXT-PAGE SECTION.                                                     
022114                                                                          
022115     IF SAVE-IDTRANS = '6342'                                             
022116       MOVE SAVE-KDSTOR-NEXT TO W-KDSTOR                                  
022117     ELSE                                                                 
022118       MOVE SPACES           TO W-KDSTOR                                  
022120     END-IF                                                               
022121     .                                                                    
022122     EJECT                                                                
022123 E-SAME-PAGE SECTION.                                                     
022124                                                                          
022125     IF OWN-MID OR HELP-MID                                               
022126        MOVE SAVE-KDSTOR-ENTER TO W-KDSTOR                                
022127                                  MOD-KDSTOR-UT                           
022128        IF MID-KDSTOR-MAIN      = ALL '+' AND                             
022129           MID-TESTORAGE-MAIN   = ALL '+' AND                             
022130           MID-DISTORD-MAIN     = ALL '+' AND                             
022131           MID-DISTORB-MAIN     = ALL '+' AND                             
022132           MID-DISTORH-MAIN     = ALL '+' AND                             
022133           MID-KDVSOP-MAIN(1)   = ALL '+' AND                             
022134           MID-KDVSOP-MAIN(2)   = ALL '+' AND                             
022135           MID-KDVSOP-MAIN(3)   = ALL '+' AND                             
022136           MID-KDVSOP-MAIN(4)   = ALL '+' AND                             
022137           MID-KDVSOP-MAIN(5)   = ALL '+' AND                             
022138           MID-KDANDR-MAIN      = ALL '+'                                 
022149           PERFORM MFS-ERASE-FIELD-IN                                     
022150         ELSE                                                             
022151           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
022152           CALL WMEDKONV USING MED-WMEDAREA                               
022153           MOVE MED-MFSINF TO MOD-TEMFSINF                                
022154           PERFORM EA-MID-INDATA-TO-MOD                                   
022155         END-IF                                                           
022156      ELSE                                                                
022157        PERFORM MFS-ERASE-FIELD-IN                                        
022158      END-IF                                                              
022159     .                                                                    
022160     EJECT                                                                
022161 EA-MID-INDATA-TO-MOD SECTION.                                            
022162                                                                          
022163     IF MID-KDVSOP-MAIN (1)  = ALL '+'                                    
022164        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP1-ATTR                   
022166      ELSE                                                                
022167        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP1-MAIN                   
022168        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP1-ATTR                   
022170     END-IF                                                               
022171                                                                          
022172     IF MID-KDVSOP-MAIN (2)  = ALL '+'                                    
022173        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP2-ATTR                   
022175      ELSE                                                                
022176        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP2-MAIN                   
022177        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP2-ATTR                   
022179     END-IF                                                               
022180                                                                          
022181     IF MID-KDVSOP-MAIN (3) = ALL '+'                                     
022182        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP3-ATTR                   
022184      ELSE                                                                
022185        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP3-MAIN                   
022186        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP3-ATTR                   
022188     END-IF                                                               
022189                                                                          
022190     IF MID-KDVSOP-MAIN (4)  = ALL '+'                                    
022191        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP4-ATTR                   
022193      ELSE                                                                
022194        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP4-MAIN                   
022195        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP4-ATTR                   
022197     END-IF                                                               
022198                                                                          
022199     IF MID-KDVSOP-MAIN (5)  = ALL '+'                                    
022200        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP5-ATTR                   
022202      ELSE                                                                
022203        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP5-MAIN                   
022204        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP5-ATTR                   
022206     END-IF                                                               
022207                                                                          
022217     IF MID-DISTORH-MAIN = ALL '+'                                        
022218        MOVE MFS-ERASE-FIELD        TO MOD-DISTORH-ATTR                   
022220      ELSE                                                                
022221        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DISTORH-MAIN                   
022222        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORH-ATTR                   
022224     END-IF                                                               
022225                                                                          
022226     IF MID-DISTORB-MAIN = ALL '+'                                        
022227        MOVE MFS-ERASE-FIELD        TO MOD-DISTORB-ATTR                   
022229      ELSE                                                                
022230        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DISTORB-MAIN                   
022231        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORB-ATTR                   
022233     END-IF                                                               
022234                                                                          
022235     IF MID-DISTORD-MAIN = ALL '+'                                        
022236        MOVE MFS-ERASE-FIELD        TO MOD-DISTORD-ATTR                   
022238      ELSE                                                                
022239        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DISTORD-MAIN                   
022240        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORD-ATTR                   
022242     END-IF                                                               
022243                                                                          
022244     IF MID-TESTORAGE-MAIN = ALL '+'                                      
022245        MOVE MFS-ERASE-FIELD        TO MOD-TESTORAGE-ATTR                 
022247      ELSE                                                                
022248        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TESTORAGE-MAIN                 
022249        MOVE MFS-ADD-READ-FIELD     TO MOD-TESTORAGE-ATTR                 
022251     END-IF                                                               
022252                                                                          
022253     IF MID-KDANDR-MAIN = ALL '+'                                         
022254        MOVE MFS-ERASE-FIELD        TO MOD-KDANDR-ATTR                    
022256      ELSE                                                                
022257        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDANDR-MAIN                    
022258        MOVE MFS-ADD-READ-FIELD     TO MOD-KDANDR-ATTR                    
022260     END-IF                                                               
022261                                                                          
022262     IF MID-KDSTOR-MAIN = ALL '+'                                         
022263        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR-ATTR                    
022265      ELSE                                                                
022266        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSTOR-MAIN                    
022267        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTOR-ATTR                    
022269     END-IF                                                               
022270     .                                                                    
022300     EJECT                                                                
022400 F-READ-SHOW-INFO SECTION.                                                
022500                                                                          
022610     MOVE WS-IDDC      TO  W-6315-IDDC                                    
022611                                                                          
022620     PERFORM  IMS-GU-6315-6316                                            
022700                                                                          
022800     IF SEGMENT-MISSING                                                   
022920        MOVE ITEMS-MISSING TO MED-IDMFSFEL                                
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
023210        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
023220        MOVE WS-KDSTOR     TO SAVE-KDSTOR-ENTER                           
023230        MOVE WS-KDSTOR     TO SAVE-KDSTOR-NEXT                            
023300     ELSE                                                                 
023400        MOVE 6316-KDSTOR   TO SAVE-KDSTOR-ENTER                           
023405        MOVE +1            TO INDX                                        
023414        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
023415                      INDX > MAX-INDX                                     
023418         MOVE 6316-KDSTOR         TO MOD-KDSTOR-LINE   (INDX)             
023419         MOVE 6316-DISTORD        TO WS-DISTORD                           
023420         MOVE 6316-DISTORB        TO WS-DISTORB                           
023421         MOVE 6316-DISTORH        TO WS-DISTORH                           
023430         PERFORM S01-EV-CONVERT-TO-US-MEASURE                             
023431         MOVE WS-DISTORD          TO MOD-DISTORD-LINE   (INDX)            
023432         MOVE WS-DISTORB          TO MOD-DISTORB-LINE   (INDX)            
023433         MOVE WS-DISTORH          TO MOD-DISTORH-LINE   (INDX)            
023434         MOVE 6316-TESTORAGE      TO MOD-TESTORAGE-LINE (INDX)            
023435         IF 6316-KDVSOP(1) = 0                                            
023437            MOVE SPACE               TO MOD-KDVSOP1-LINE   (INDX)         
023438          ELSE                                                            
023439            MOVE 6316-KDVSOP(1)      TO MOD-KDVSOP1-LINE   (INDX)         
023440         END-IF                                                           
023441         IF 6316-KDVSOP(2) = 0                                            
023442            MOVE SPACE               TO MOD-KDVSOP2-LINE   (INDX)         
023443          ELSE                                                            
023444            MOVE 6316-KDVSOP(2)      TO MOD-KDVSOP2-LINE   (INDX)         
023445         END-IF                                                           
023446         IF 6316-KDVSOP(3) = 0                                            
023447            MOVE SPACE               TO MOD-KDVSOP3-LINE   (INDX)         
023448          ELSE                                                            
023449            MOVE 6316-KDVSOP(3)      TO MOD-KDVSOP3-LINE   (INDX)         
023450         END-IF                                                           
023451         IF 6316-KDVSOP(4) = 0                                            
023452            MOVE SPACE               TO MOD-KDVSOP4-LINE   (INDX)         
023453          ELSE                                                            
023454             MOVE 6316-KDVSOP(4)     TO MOD-KDVSOP4-LINE   (INDX)         
023455         END-IF                                                           
023456         IF 6316-KDVSOP(5) = 0                                            
023457            MOVE SPACE               TO MOD-KDVSOP5-LINE   (INDX)         
023458          ELSE                                                            
023459             MOVE 6316-KDVSOP(5)     TO MOD-KDVSOP5-LINE   (INDX)         
023460         END-IF                                                           
023462         PERFORM  IMS-GN-6315-6316                                        
023463         ADD 1 TO INDX                                                    
023464        END-PERFORM                                                       
023465        IF SEGMENT-FOUND                                                  
023466           MOVE 6316-KDSTOR  TO SAVE-KDSTOR-NEXT                          
023467           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
023468           CALL WMEDKONV USING MED-WMEDAREA                               
023469           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
023470         ELSE                                                             
023471           MOVE 6316-KDSTOR         TO SAVE-KDSTOR-NEXT                   
023472           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
023473           CALL WMEDKONV            USING MED-WMEDAREA                    
023474           MOVE MED-MFSFEL          TO    MOD-TEMFSFEL                    
023475           PERFORM UNTIL INDX > MAX-INDX                                  
023476            MOVE MFS-ERASE-FIELD     TO MOD-KDSTOR-LINE    (INDX)         
023477                                        MOD-TESTORAGE-LINE (INDX)         
023478                                        MOD-DISTORD-LINE   (INDX)         
023479                                        MOD-DISTORB-LINE   (INDX)         
023480                                        MOD-DISTORH-LINE   (INDX)         
023481                                        MOD-KDVSOP1-LINE   (INDX)         
023482                                        MOD-KDVSOP2-LINE   (INDX)         
023483                                        MOD-KDVSOP3-LINE   (INDX)         
023484                                        MOD-KDVSOP4-LINE   (INDX)         
023485                                        MOD-KDVSOP5-LINE   (INDX)         
023486            ADD 1 TO INDX                                                 
023487           END-PERFORM                                                    
023488         END-IF                                                           
023489                                                                          
023490       MOVE '002'      TO MSGI-KDCALL                                     
023491       MOVE '6342'     TO SAVE-IDTRANS                                    
023492       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
023493       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024602 G-CHECK-INPUT SECTION.                                                   
024603                                                                          
024604     MOVE YES  TO INDATA-SW                                               
024605                                                                          
024606     IF MID-KDSTOR-MAIN      = ALL '+' AND                                
024607        MID-TESTORAGE-MAIN   = ALL '+' AND                                
024608        MID-DISTORD-MAIN     = ALL '+' AND                                
024609        MID-DISTORB-MAIN     = ALL '+' AND                                
024610        MID-DISTORH-MAIN     = ALL '+' AND                                
024611        MID-KDVSOP-MAIN(1)   = ALL '+' AND                                
024612        MID-KDVSOP-MAIN(2)   = ALL '+' AND                                
024613        MID-KDVSOP-MAIN(3)   = ALL '+' AND                                
024614        MID-KDVSOP-MAIN(4)   = ALL '+' AND                                
024615        MID-KDVSOP-MAIN(5)   = ALL '+' AND                                
024616        MID-KDANDR-MAIN      = ALL '+'                                    
024630          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
024640          CALL WMEDKONV USING MED-WMEDAREA                                
024650          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
024662          MOVE NOO TO INDATA-SW                                           
024663     END-IF                                                               
024664     IF INDATA-OK                                                         
024665        IF MID-KDSTOR-MAIN = ALL '+'                                      
024666           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDSTOR-ATTR                 
024667           MOVE NOO                    TO INDATA-SW                       
024668         ELSE                                                             
024670           MOVE MFS-ALPHA-FIELD-OK     TO MOD-KDSTOR-ATTR                 
024671           MOVE MID-KDSTOR-MAIN        TO WS-KDSTOR                       
024672                                          W-KDSTOR                        
024677        END-IF                                                            
024678        IF MID-KDANDR-MAIN = ALL '+'                                      
024679           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDANDR-ATTR                 
024680           MOVE NOO                    TO INDATA-SW                       
024681         ELSE                                                             
024682           IF MID-KDANDR-MAIN = NEW OR DEL OR CHG                         
024683              MOVE MFS-ALPHA-FIELD-OK        TO MOD-KDANDR-ATTR           
024694           ELSE                                                           
024695              MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-KDANDR-ATTR           
024696              MOVE NOO                       TO INDATA-SW                 
024697           END-IF                                                         
024698        END-IF                                                            
024699        IF INDATA-WRONG                                                   
024700           PERFORM GD-MID-INDATA-TO-MOD                                   
024701           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
024702           CALL WMEDKONV USING MED-WMEDAREA                               
024703           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
024704        END-IF                                                            
024705     END-IF                                                               
024706                                                                          
024707     IF INDATA-OK                                                         
024708        IF MID-KDANDR-MAIN = NEW                                          
024709           PERFORM GA-CHECK-INPUT-NEW                                     
024710        END-IF                                                            
024711        IF MID-KDANDR-MAIN = CHG                                          
024712           PERFORM GB-CHECK-INPUT-CHANGE                                  
024713        END-IF                                                            
024714        IF MID-KDANDR-MAIN = DEL                                          
024715           PERFORM GC-CHECK-INPUT-DELETE                                  
024716        END-IF                                                            
024717        IF INDATA-WRONG                                                   
024718           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
024719           CALL WMEDKONV USING MED-WMEDAREA                               
024720           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
024721        END-IF                                                            
024722     END-IF                                                               
024723                                                                          
024724     IF INDATA-WRONG                                                      
024725        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
024760     END-IF                                                               
024769                                                                          
024770     .                                                                    
024771     EJECT                                                                
024772 GA-CHECK-INPUT-NEW SECTION.                                              
024773                                                                          
024774*                                                                         
024775*   --- CHECK VSOP CODE                                                   
024776*                                                                         
024777     IF MID-KDVSOP-MAIN (1) = ALL '+'                                     
024778        MOVE ZEROES                 TO WS-KDVSOP1                         
024779        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP1-ATTR                   
024780       ELSE                                                               
024781        IF MID-KDVSOP-MAIN (1) NUMERIC                                    
024782          MOVE MID-KDVSOP-MAIN (1)  TO WS-KDVSOP1                         
024783          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP1-ATTR                   
024784         ELSE                                                             
024785          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP1-ATTR                   
024786          MOVE NOO                  TO INDATA-SW                          
024787        END-IF                                                            
024788     END-IF                                                               
024789                                                                          
024790     IF MID-KDVSOP-MAIN (2) = ALL '+'                                     
024791        MOVE ZEROES                 TO WS-KDVSOP2                         
024792        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP2-ATTR                   
024793       ELSE                                                               
024794        IF MID-KDVSOP-MAIN (2) NUMERIC                                    
024795          MOVE MID-KDVSOP-MAIN (2)  TO WS-KDVSOP2                         
024796          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP2-ATTR                   
024797         ELSE                                                             
024798          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP2-ATTR                   
024799          MOVE NOO                  TO INDATA-SW                          
024800        END-IF                                                            
024801     END-IF                                                               
024802                                                                          
024803     IF MID-KDVSOP-MAIN (3) = ALL '+'                                     
024804        MOVE ZEROES                 TO WS-KDVSOP3                         
024805        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP3-ATTR                   
024806       ELSE                                                               
024807        IF MID-KDVSOP-MAIN (3) NUMERIC                                    
024808          MOVE MID-KDVSOP-MAIN (3)  TO WS-KDVSOP3                         
024809          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP3-ATTR                   
024810         ELSE                                                             
024811          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP3-ATTR                   
024812          MOVE NOO                  TO INDATA-SW                          
024813        END-IF                                                            
024814     END-IF                                                               
024815                                                                          
024816     IF MID-KDVSOP-MAIN (4) = ALL '+'                                     
024817        MOVE ZEROES                 TO WS-KDVSOP4                         
024818        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP4-ATTR                   
024819       ELSE                                                               
024820        IF MID-KDVSOP-MAIN (4) NUMERIC                                    
024821          MOVE MID-KDVSOP-MAIN (4)  TO WS-KDVSOP4                         
024822          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP4-ATTR                   
024823         ELSE                                                             
024824          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP4-ATTR                   
024825          MOVE NOO                  TO INDATA-SW                          
024826        END-IF                                                            
024827     END-IF                                                               
024828                                                                          
024829     IF MID-KDVSOP-MAIN (5) = ALL '+'                                     
024830        MOVE ZEROES                 TO WS-KDVSOP5                         
024831        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP5-ATTR                   
024832       ELSE                                                               
024833        IF MID-KDVSOP-MAIN (5) NUMERIC                                    
024834          MOVE MID-KDVSOP-MAIN (5)  TO WS-KDVSOP5                         
024835          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP5-ATTR                   
024836         ELSE                                                             
024837          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP5-ATTR                   
024838          MOVE NOO                  TO INDATA-SW                          
024839        END-IF                                                            
024840     END-IF                                                               
024841*                                                                         
024842*   --- CHECK STORAGE  DESCRIPTION                                        
024843*                                                                         
024844     IF MID-TESTORAGE-MAIN = ALL '+'                                      
024845        MOVE SPACE                  TO WS-TESTORAGE                       
024846        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
024847       ELSE                                                               
024848        MOVE MID-TESTORAGE-MAIN     TO WS-TESTORAGE                       
024849        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
024850     END-IF                                                               
024851*                                                                         
024852*   --- CHECK DEPTH OF STORAGE                                            
024853*                                                                         
024854     IF MID-DISTORH-MAIN = ALL '+'                                        
024855        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-DISTORH-ATTR                   
024856        MOVE NOO                    TO INDATA-SW                          
024857       ELSE                                                               
024858        MOVE MID-DISTORH-MAIN       TO DEC-IDFRIDATA                      
024859        MOVE 3                      TO DEC-KVHELTAL                       
024860        MOVE 1                      TO DEC-KVDECIMAL                      
024861        CALL WDECEDIT USING DEC-WDECAREA                                  
024862        END-CALL                                                          
024863        IF DEC-KDSVAR-OK                                                  
024864          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORH-ATTR                   
024865          MOVE DEC-IDEDITDATA        TO WS-DISTORH                        
024866         ELSE                                                             
024867          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORH-ATTR                  
024868          MOVE NOO                   TO INDATA-SW                         
024869        END-IF                                                            
024870     END-IF                                                               
024871*                                                                         
024872*   --- CHECK WIDTH OF STORAGE                                            
024873*                                                                         
024874     IF MID-DISTORB-MAIN = ALL '+'                                        
024875        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-DISTORB-ATTR                   
024876        MOVE NOO                    TO INDATA-SW                          
024877       ELSE                                                               
024878        MOVE MID-DISTORB-MAIN       TO DEC-IDFRIDATA                      
024879        MOVE 3                      TO DEC-KVHELTAL                       
024880        MOVE 1                      TO DEC-KVDECIMAL                      
024881        CALL WDECEDIT USING DEC-WDECAREA                                  
024882        END-CALL                                                          
024883        IF DEC-KDSVAR-OK                                                  
024884          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORB-ATTR                   
024885          MOVE DEC-IDEDITDATA        TO WS-DISTORB                        
024886         ELSE                                                             
024887          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORB-ATTR                  
024888          MOVE NOO                   TO INDATA-SW                         
024889        END-IF                                                            
024890     END-IF                                                               
024891*                                                                         
024892*   --- CHECK LENGTH OF STORAGE                                           
024893*                                                                         
024894     IF MID-DISTORD-MAIN = ALL '+'                                        
024895        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-DISTORD-ATTR                   
024896        MOVE NOO                    TO INDATA-SW                          
024897       ELSE                                                               
024898        MOVE MID-DISTORD-MAIN       TO DEC-IDFRIDATA                      
024899        MOVE 3                      TO DEC-KVHELTAL                       
024900        MOVE 1                      TO DEC-KVDECIMAL                      
024901        CALL WDECEDIT USING DEC-WDECAREA                                  
024902        END-CALL                                                          
024903        IF DEC-KDSVAR-OK                                                  
024904          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORD-ATTR                   
024905          MOVE DEC-IDEDITDATA        TO WS-DISTORD                        
024906         ELSE                                                             
024907          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORD-ATTR                  
024908          MOVE NOO                   TO INDATA-SW                         
024909        END-IF                                                            
024910     END-IF                                                               
024911     .                                                                    
024912                                                                          
024913*                                                                         
024914*   --- CHECK STORAGE ALREADY EXIST                                       
024915*                                                                         
024917     MOVE WS-IDDC                    TO W-6315-IDDC                       
024918     MOVE MID-KDSTOR-MAIN            TO W-KDSTOR                          
024920                                                                          
024921     PERFORM  IMS-GU-6316                                                 
024922                                                                          
024923     IF SEGMENT-FOUND                                                     
024924        MOVE NOO                     TO INDATA-SW                         
024925        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                   
024926       ELSE                                                               
024927        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                   
024928        MOVE MID-KDSTOR-MAIN         TO WS-KDSTOR                         
024929     END-IF                                                               
024930                                                                          
024931     .                                                                    
024932     EJECT                                                                
024933 GB-CHECK-INPUT-CHANGE SECTION.                                           
024934                                                                          
024935*                                                                         
024940*   --- CHECK VSOP CODE                                                   
024950*                                                                         
024960     IF MID-KDVSOP-MAIN (1) =  ALL '+'                                    
024970        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP1-ATTR                   
024980       ELSE                                                               
024990        IF MID-KDVSOP-MAIN (1) NUMERIC                                    
024991          MOVE MID-KDVSOP-MAIN (1)  TO WS-KDVSOP1                         
024992          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP1-ATTR                   
024993         ELSE                                                             
024994          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP1-ATTR                   
024995          MOVE NOO                  TO INDATA-SW                          
024996        END-IF                                                            
024997     END-IF                                                               
024998                                                                          
024999     IF MID-KDVSOP-MAIN (2) =  ALL '+'                                    
025000        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP2-ATTR                   
025001       ELSE                                                               
025002        IF MID-KDVSOP-MAIN (2) NUMERIC                                    
025004          MOVE MID-KDVSOP-MAIN (2)  TO WS-KDVSOP2                         
025005          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP2-ATTR                   
025006         ELSE                                                             
025007          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP2-ATTR                   
025008          MOVE NOO                  TO INDATA-SW                          
025009        END-IF                                                            
025010     END-IF                                                               
025011                                                                          
025012     IF MID-KDVSOP-MAIN (3) =  ALL '+'                                    
025013        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP3-ATTR                   
025014       ELSE                                                               
025015        IF MID-KDVSOP-MAIN (3) NUMERIC                                    
025016          MOVE MID-KDVSOP-MAIN (3)  TO WS-KDVSOP3                         
025017          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP3-ATTR                   
025018         ELSE                                                             
025019          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP3-ATTR                   
025020          MOVE NOO                  TO INDATA-SW                          
025021        END-IF                                                            
025022     END-IF                                                               
025023                                                                          
025024     IF MID-KDVSOP-MAIN (4) =  ALL '+'                                    
025025        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP4-ATTR                   
025026       ELSE                                                               
025027        IF MID-KDVSOP-MAIN (4) NUMERIC                                    
025029          MOVE MID-KDVSOP-MAIN (4)  TO WS-KDVSOP4                         
025030          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP4-ATTR                   
025031         ELSE                                                             
025032          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP4-ATTR                   
025033          MOVE NOO                  TO INDATA-SW                          
025034        END-IF                                                            
025035     END-IF                                                               
025036                                                                          
025037     IF MID-KDVSOP-MAIN (5) =  ALL '+'                                    
025038        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP5-ATTR                   
025039       ELSE                                                               
025041        IF MID-KDVSOP-MAIN (5) NUMERIC                                    
025042          MOVE MID-KDVSOP-MAIN (5)  TO WS-KDVSOP5                         
025043          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP5-ATTR                   
025044         ELSE                                                             
025045          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP5-ATTR                   
025046          MOVE NOO                  TO INDATA-SW                          
025047        END-IF                                                            
025048     END-IF                                                               
025049                                                                          
025084*                                                                         
025085*   --- CHECK STORAGE  DESCRIPTION                                        
025086*                                                                         
025087     IF MID-TESTORAGE-MAIN = ALL '+'                                      
025088        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
025089       ELSE                                                               
025090        MOVE MID-TESTORAGE-MAIN     TO WS-TESTORAGE                       
025091        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
025092     END-IF                                                               
025093*                                                                         
025094*   --- CHECK STORAGE DEPTH                                               
025095*                                                                         
025096     IF MID-DISTORH-MAIN NOT = ALL '+'                                    
025097        MOVE MID-DISTORH-MAIN       TO DEC-IDFRIDATA                      
025098        MOVE 3                      TO DEC-KVHELTAL                       
025099        MOVE 1                      TO DEC-KVDECIMAL                      
025100        CALL WDECEDIT USING DEC-WDECAREA                                  
025101        END-CALL                                                          
025102        IF DEC-KDSVAR-OK                                                  
025103          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORH-ATTR                   
025104          MOVE DEC-IDEDITDATA        TO WS-DISTORH                        
025105         ELSE                                                             
025106          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORH-ATTR                  
025107          MOVE NOO                   TO INDATA-SW                         
025108        END-IF                                                            
025109     END-IF                                                               
025110*                                                                         
025111*   --- CHECK STORAGE WIDTH                                               
025112*                                                                         
025113     IF MID-DISTORB-MAIN NOT = ALL '+'                                    
025114        MOVE MID-DISTORB-MAIN       TO DEC-IDFRIDATA                      
025115        MOVE 3                      TO DEC-KVHELTAL                       
025116        MOVE 1                      TO DEC-KVDECIMAL                      
025117        CALL WDECEDIT USING DEC-WDECAREA                                  
025118        END-CALL                                                          
025119        IF DEC-KDSVAR-OK                                                  
025120          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORB-ATTR                   
025121          MOVE DEC-IDEDITDATA        TO WS-DISTORB                        
025122         ELSE                                                             
025123          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORB-ATTR                  
025124          MOVE NOO                   TO INDATA-SW                         
025125        END-IF                                                            
025126     END-IF                                                               
025127*                                                                         
025128*   --- CHECK STORAGE LENGTH                                              
025129*                                                                         
025130     IF MID-DISTORD-MAIN NOT = ALL '+'                                    
025131        MOVE MID-DISTORD-MAIN       TO DEC-IDFRIDATA                      
025132        MOVE 3                      TO DEC-KVHELTAL                       
025133        MOVE 1                      TO DEC-KVDECIMAL                      
025134        CALL WDECEDIT USING DEC-WDECAREA                                  
025135        END-CALL                                                          
025136        IF DEC-KDSVAR-OK                                                  
025137          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORD-ATTR                   
025138          MOVE DEC-IDEDITDATA        TO WS-DISTORD                        
025139         ELSE                                                             
025140          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORD-ATTR                  
025141          MOVE NOO                   TO INDATA-SW                         
025142        END-IF                                                            
025143     END-IF                                                               
025144                                                                          
025145*                                                                         
025146*   --- CHECK IF STORAGE KODE EXIST                                       
025147*                                                                         
025148     MOVE WS-IDDC                    TO W-6315-IDDC                       
025149     MOVE MID-KDSTOR-MAIN            TO W-KDSTOR                          
025150                                                                          
025151     PERFORM  IMS-GHU-6316                                                
025152                                                                          
025153     IF SEGMENT-MISSING                                                   
025154        MOVE NOO                     TO INDATA-SW                         
025155        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                   
025156       ELSE                                                               
025157        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                   
025158        MOVE MID-KDSTOR-MAIN         TO WS-KDSTOR                         
025159     END-IF                                                               
025160     .                                                                    
025161     EJECT                                                                
025162                                                                          
025163 GC-CHECK-INPUT-DELETE SECTION.                                           
025164                                                                          
025165     MOVE WS-IDDC                    TO W-6315-IDDC                       
025166     MOVE MID-KDSTOR-MAIN            TO W-KDSTOR                          
025167                                                                          
025168     PERFORM  IMS-GHU-6316                                                
025169*                                                                         
025170*   --- CHECK IF STORAGE KODE EXIST                                       
025171*                                                                         
025172     IF SEGMENT-MISSING                                                   
025173        MOVE NOO                     TO INDATA-SW                         
025174        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                   
025175       ELSE                                                               
025176        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                   
025177     END-IF                                                               
025178     .                                                                    
025179     EJECT                                                                
025180 GD-MID-INDATA-TO-MOD SECTION.                                            
025181                                                                          
025182     IF MID-KDVSOP-MAIN (1)  = ALL '+'                                    
025183        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP1-ATTR                   
025184      ELSE                                                                
025186        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP1-ATTR                   
025187     END-IF                                                               
025188                                                                          
025189     IF MID-KDVSOP-MAIN (2)  = ALL '+'                                    
025190        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP2-ATTR                   
025191      ELSE                                                                
025193        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP2-ATTR                   
025194     END-IF                                                               
025195                                                                          
025196     IF MID-KDVSOP-MAIN (3) = ALL '+'                                     
025197        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP3-ATTR                   
025198      ELSE                                                                
025200        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP3-ATTR                   
025201     END-IF                                                               
025202                                                                          
025203     IF MID-KDVSOP-MAIN (4)  = ALL '+'                                    
025204        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP4-ATTR                   
025205      ELSE                                                                
025207        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP4-ATTR                   
025208     END-IF                                                               
025209                                                                          
025210     IF MID-KDVSOP-MAIN (5)  = ALL '+'                                    
025211        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP5-ATTR                   
025212      ELSE                                                                
025214        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP5-ATTR                   
025215     END-IF                                                               
025216                                                                          
025217     IF MID-TESTORAGE-MAIN = ALL '+'                                      
025218        MOVE MFS-ERASE-FIELD        TO MOD-TESTORAGE-ATTR                 
025219      ELSE                                                                
025221        MOVE MFS-ADD-READ-FIELD     TO MOD-TESTORAGE-ATTR                 
025222     END-IF                                                               
025223                                                                          
025224     IF MID-DISTORB-MAIN = ALL '+'                                        
025225        MOVE MFS-ERASE-FIELD        TO MOD-DISTORB-ATTR                   
025226      ELSE                                                                
025228        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORB-ATTR                   
025229     END-IF                                                               
025230                                                                          
025231     IF MID-DISTORD-MAIN = ALL '+'                                        
025232        MOVE MFS-ERASE-FIELD        TO MOD-DISTORD-ATTR                   
025233      ELSE                                                                
025235        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORD-ATTR                   
025236     END-IF                                                               
025237                                                                          
025238     IF MID-DISTORH-MAIN = ALL '+'                                        
025239        MOVE MFS-ERASE-FIELD        TO MOD-DISTORH-ATTR                   
025240      ELSE                                                                
025242        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORH-ATTR                   
025243     END-IF                                                               
025244                                                                          
025245     IF MID-TESTORAGE-MAIN = ALL '+'                                      
025246        MOVE MFS-ERASE-FIELD        TO MOD-TESTORAGE-ATTR                 
025247      ELSE                                                                
025249        MOVE MFS-ADD-READ-FIELD     TO MOD-TESTORAGE-ATTR                 
025250     END-IF                                                               
025251                                                                          
025265     .                                                                    
025266     EJECT                                                                
025267 H-UPDATE SECTION.                                                        
025268                                                                          
025269     IF MID-KDANDR-MAIN = NEW                                             
025270        PERFORM HA-NEW                                                    
025271     END-IF                                                               
025272     IF MID-KDANDR-MAIN = CHG                                             
025273        PERFORM HB-CHANGE                                                 
025274     END-IF                                                               
025275     IF MID-KDANDR-MAIN = DEL                                             
025276        PERFORM HC-DELETE                                                 
025277     END-IF                                                               
025278                                                                          
025279     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025280     CALL WMEDKONV USING MED-WMEDAREA                                     
025281     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
025282     PERFORM MFS-FORM-ATTR                                                
025283     PERFORM MFS-ERASE-FIELD-IN                                           
025284     .                                                                    
025285     EJECT                                                                
025286 HA-NEW SECTION.                                                          
025287                                                                          
025290     MOVE WS-IDDC         TO  W-6315-IDDC                                 
025292                                                                          
025293     MOVE MID-KDSTOR-MAIN TO  W-KDSTOR                                    
025294                              6316-KDSTOR                                 
025295                              MOD-KDSTOR-UT                               
025296                                                                          
025297     MOVE WS-TESTORAGE    TO  6316-TESTORAGE                              
025298     IF US-MEASUREMENT                                                    
025299        COMPUTE 6316-DISTORD  ROUNDED =                                   
025300                   WS-DISTORD * CONV-IN-TO-CM  END-COMPUTE                
025302        COMPUTE 6316-DISTORB  ROUNDED =                                   
025303                   WS-DISTORB * CONV-IN-TO-CM  END-COMPUTE                
025305        COMPUTE 6316-DISTORH  ROUNDED =                                   
025306                   WS-DISTORH * CONV-IN-TO-CM  END-COMPUTE                
025308      ELSE                                                                
025309        MOVE WS-DISTORD      TO  6316-DISTORD                             
025310        MOVE WS-DISTORB      TO  6316-DISTORB                             
025311        MOVE WS-DISTORH      TO  6316-DISTORH                             
025312     END-IF                                                               
025313     MOVE WS-KDVSOP1      TO  6316-KDVSOP (1)                             
025314     MOVE WS-KDVSOP2      TO  6316-KDVSOP (2)                             
025315     MOVE WS-KDVSOP3      TO  6316-KDVSOP (3)                             
025316     MOVE WS-KDVSOP4      TO  6316-KDVSOP (4)                             
025317     MOVE WS-KDVSOP5      TO  6316-KDVSOP (5)                             
025318                                                                          
025319     PERFORM IMS-ISRT-6315-6316                                           
025320                                                                          
025321     MOVE WS-KDSTOR       TO  MOD-KDSTOR-UT                               
025322                              W-KDSTOR                                    
025323     .                                                                    
025324     EJECT                                                                
025325 HB-CHANGE SECTION.                                                       
025326                                                                          
025327     IF MID-KDVSOP-MAIN (1) NOT = ALL '+'                                 
025328        MOVE WS-KDVSOP1       TO  6316-KDVSOP (1)                         
025329     END-IF                                                               
025330                                                                          
025331     IF MID-KDVSOP-MAIN (2) NOT = ALL '+'                                 
025332        MOVE WS-KDVSOP2       TO  6316-KDVSOP (2)                         
025333     END-IF                                                               
025334                                                                          
025335     IF MID-KDVSOP-MAIN (3) NOT = ALL '+'                                 
025336        MOVE WS-KDVSOP3       TO  6316-KDVSOP (3)                         
025337     END-IF                                                               
025338                                                                          
025339     IF MID-KDVSOP-MAIN (4) NOT = ALL '+'                                 
025340        MOVE WS-KDVSOP4       TO  6316-KDVSOP (4)                         
025341     END-IF                                                               
025342                                                                          
025343     IF MID-KDVSOP-MAIN (5) NOT = ALL '+'                                 
025344        MOVE WS-KDVSOP5       TO  6316-KDVSOP (5)                         
025345     END-IF                                                               
025346                                                                          
025347     IF MID-TESTORAGE-MAIN NOT = ALL '+'                                  
025348        MOVE WS-TESTORAGE     TO 6316-TESTORAGE                           
025349     END-IF                                                               
025350                                                                          
025351     IF MID-DISTORD-MAIN NOT = ALL '+'                                    
025352        IF US-MEASUREMENT                                                 
025353           COMPUTE 6316-DISTORD =                                         
025354                   (WS-DISTORD * CONV-IN-TO-CM)                           
025355                   END-COMPUTE                                            
025356        ELSE                                                              
025357          MOVE WS-DISTORD      TO  6316-DISTORD                           
025358        END-IF                                                            
025359     END-IF                                                               
025360                                                                          
025361     IF MID-DISTORB-MAIN NOT = ALL '+'                                    
025362        IF US-MEASUREMENT                                                 
025363           COMPUTE 6316-DISTORB =                                         
025364                   WS-DISTORB * CONV-IN-TO-CM  END-COMPUTE                
025365        ELSE                                                              
025366          MOVE WS-DISTORB      TO  6316-DISTORB                           
025367        END-IF                                                            
025368     END-IF                                                               
025369                                                                          
025370     IF MID-DISTORH-MAIN NOT = ALL '+'                                    
025371        IF US-MEASUREMENT                                                 
025372           COMPUTE 6316-DISTORH ROUNDED =                                 
025373                   WS-DISTORH * CONV-IN-TO-CM  END-COMPUTE                
025374        ELSE                                                              
025375          MOVE WS-DISTORH      TO  6316-DISTORH                           
025376        END-IF                                                            
025377     END-IF                                                               
025378                                                                          
025379     PERFORM IMS-REPL-6315-6316                                           
025380                                                                          
025381     MOVE WS-KDSTOR            TO W-KDSTOR                                
025382                                  MOD-KDSTOR-UT                           
025383     .                                                                    
025384     EJECT                                                                
025385 HC-DELETE SECTION.                                                       
025386                                                                          
025387     PERFORM IMS-DLET-6315-6316                                           
025388     .                                                                    
025389     EJECT                                                                
025390 S01-EV-CONVERT-TO-US-MEASURE        SECTION.                             
025391                                                                          
025392                                                                          
025393     IF US-MEASUREMENT                                                    
025394       COMPUTE WS-DISTORD ROUNDED = WS-DISTORD  * CONV-CM-TO-IN           
025395       END-COMPUTE                                                        
025396                                                                          
025397       COMPUTE WS-DISTORB ROUNDED = WS-DISTORB * CONV-CM-TO-IN            
025398       END-COMPUTE                                                        
025399                                                                          
025400       COMPUTE WS-DISTORH ROUNDED = WS-DISTORH * CONV-CM-TO-IN            
025401       END-COMPUTE                                                        
025402                                                                          
025403     END-IF                                                               
025404     .                                                                    
025405     SKIP2                                                                
025406*                                                                         
025407 MFS-INIT-KEY-FIELD-IN SECTION.                                           
025408                                                                          
025409*    --- ALL INPUT KEY FIELDS                                             
025410                                                                          
025411     MOVE MFS-ERASE-FIELD          TO MOD-KDSTOR-IN                       
025413     .                                                                    
025414     EJECT                                                                
025415 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
025416                                                                          
025417*    --- ALL OUTPUT KEY FIELDS                                            
025418                                                                          
025419     MOVE MSGI-IDDC                TO MOD-IDDC                            
025420     MOVE MFS-ERASE-FIELD          TO MOD-KDSTOR-UT                       
025421     .                                                                    
025422     EJECT                                                                
025423 MFS-ERASE-FIELD-OUT SECTION.                                             
025424                                                                          
025425*    --- ALL  OUTPUT-FIELDS                                               
025426*    --- INCL. SCROLL KEYS                                                
025427     MOVE MFS-ERASE-FIELD          TO MOD-KDSTOR-MAIN                     
025428                                      MOD-TESTORAGE-MAIN                  
025429                                      MOD-DISTORD-MAIN                    
025430                                      MOD-DISTORB-MAIN                    
025431                                      MOD-DISTORH-MAIN                    
025432                                      MOD-KDVSOP1-MAIN                    
025433                                      MOD-KDVSOP2-MAIN                    
025434                                      MOD-KDVSOP3-MAIN                    
025435                                      MOD-KDVSOP4-MAIN                    
025436                                      MOD-KDVSOP5-MAIN                    
025440                                      MOD-KDANDR-MAIN                     
025500     .                                                                    
025501     SKIP3                                                                
025502 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
025503                                                                          
025504*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
025505                                                                          
025510     MOVE +1 TO INDX                                                      
025511     PERFORM UNTIL INDX > MAX-INDX                                        
025517     MOVE MFS-ERASE-FIELD     TO MOD-KDSTOR-LINE    (INDX)                
025518                                 MOD-TESTORAGE-LINE (INDX)                
025519                                 MOD-DISTORD-LINE   (INDX)                
025520                                 MOD-DISTORB-LINE   (INDX)                
025521                                 MOD-DISTORH-LINE   (INDX)                
025522                                 MOD-KDVSOP1-LINE   (INDX)                
025523                                 MOD-KDVSOP2-LINE   (INDX)                
025524                                 MOD-KDVSOP3-LINE   (INDX)                
025525                                 MOD-KDVSOP4-LINE   (INDX)                
025526                                 MOD-KDVSOP5-LINE   (INDX)                
025527      ADD +1 TO INDX                                                      
025528     END-PERFORM                                                          
025530     .                                                                    
025600     SKIP3                                                                
025700 MFS-ERASE-FIELD-IN SECTION.                                              
025800                                                                          
025900*    --- ALL INPUT DATA FIELDS                                            
025901                                                                          
025990     MOVE MFS-ERASE-FIELD TO MOD-KDSTOR-MAIN                              
026000                             MOD-TESTORAGE-MAIN                           
026100                             MOD-DISTORD-MAIN                             
026110                             MOD-DISTORB-MAIN                             
026120                             MOD-DISTORH-MAIN                             
026130                             MOD-KDVSOP1-MAIN                             
026140                             MOD-KDVSOP2-MAIN                             
026150                             MOD-KDVSOP3-MAIN                             
026160                             MOD-KDVSOP4-MAIN                             
026170                             MOD-KDVSOP5-MAIN                             
026180                             MOD-KDANDR-MAIN                              
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
026500                                                                          
026600*    --- ALL OUTDATA FIELDS                                               
026710*    --- INCL SCROLL KEYS AND LINEDATA                                    
026721     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSTOR-MAIN                       
026722                                    MOD-TESTORAGE-MAIN                    
026723                                    MOD-DISTORD-MAIN                      
026724                                    MOD-DISTORB-MAIN                      
026725                                    MOD-DISTORH-MAIN                      
026726                                    MOD-KDVSOP1-MAIN                      
026727                                    MOD-KDVSOP2-MAIN                      
026728                                    MOD-KDVSOP3-MAIN                      
026729                                    MOD-KDVSOP4-MAIN                      
026730                                    MOD-KDVSOP5-MAIN                      
026731                                    MOD-KDANDR-MAIN                       
027001     MOVE +1 TO INDX                                                      
027002     PERFORM UNTIL INDX > MAX-INDX                                        
027003       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
027004       ADD +1 TO INDX                                                     
027005     END-PERFORM                                                          
027006     .                                                                    
027007     EJECT                                                                
027009 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
027010                                                                          
027011*    --- OUTDATA FIELD ON SCROLL KEYS                                     
027013     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSTOR-LINE    (INDX)             
027014                                    MOD-TESTORAGE-LINE (INDX)             
027015                                    MOD-DISTORD-LINE   (INDX)             
027016                                    MOD-DISTORB-LINE   (INDX)             
027017                                    MOD-DISTORH-LINE   (INDX)             
027018                                    MOD-KDVSOP1-LINE   (INDX)             
027019                                    MOD-KDVSOP2-LINE   (INDX)             
027020                                    MOD-KDVSOP3-LINE   (INDX)             
027021                                    MOD-KDVSOP4-LINE   (INDX)             
027022                                    MOD-KDVSOP5-LINE   (INDX)             
027800     .                                                                    
027900     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALL INDATA-FIELDS                                                
028300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDSTOR-ATTR                      
028440                                     MOD-TESTORAGE-ATTR                   
028441                                     MOD-DISTORD-ATTR                     
028442                                     MOD-DISTORB-ATTR                     
028443                                     MOD-DISTORH-ATTR                     
028444                                     MOD-KDVSOP1-ATTR                     
028445                                     MOD-KDVSOP2-ATTR                     
028446                                     MOD-KDVSOP3-ATTR                     
028447                                     MOD-KDVSOP4-ATTR                     
028448                                     MOD-KDVSOP5-ATTR                     
028450                                     MOD-KDANDR-ATTR                      
028500     .                                                                    
028600     SKIP2                                                                
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
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GOOD-STATUSCODES                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSCHECK                                              
031400     .                                                                    
031501     EJECT                                                                
031538 IMS-GU-6316 SECTION.                                                     
031539                                                                          
031545     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
031546          DELIMITED BY SIZE INTO SSA1                                     
031547     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
031549          DELIMITED BY SIZE INTO SSA2                                     
031553     MOVE '  GE' TO GOOD-STATUSCODES                                      
031554     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
031556     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031557     PERFORM IMS-STATUSCHECK                                              
031558     .                                                                    
031559     SKIP3                                                                
031561 IMS-GHU-6316 SECTION.                                                    
031562                                                                          
031563     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
031564          DELIMITED BY SIZE INTO SSA1                                     
031565     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
031566          DELIMITED BY SIZE INTO SSA2                                     
031567     MOVE '  GE' TO GOOD-STATUSCODES                                      
031568     CALL CBLTDLI USING GHU 6315-PCB 6316-WDGX6316 SSA1 SSA2              
031569     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031570     PERFORM IMS-STATUSCHECK                                              
031571     .                                                                    
031572     SKIP3                                                                
031574 IMS-GU-6315-6316 SECTION.                                                
031575                                                                          
031576     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
031577          DELIMITED BY SIZE INTO SSA1                                     
031578     STRING 'WL631511(KDSTOR  >=' W-WDGXKEY-6316-X                        
031579                    '&KDSTOR  =<' W-KDSTOR-MAX-X ')'                      
031580          DELIMITED BY SIZE INTO SSA2                                     
031581     MOVE '  GE' TO GOOD-STATUSCODES                                      
031582     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
031583     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031584     PERFORM IMS-STATUSCHECK                                              
031585     .                                                                    
031586     SKIP3                                                                
031588 IMS-GN-6315-6316 SECTION.                                                
031589                                                                          
031590     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
031591          DELIMITED BY SIZE INTO SSA1                                     
031592     STRING 'WL631511(KDSTOR  >=' W-WDGXKEY-6316-X                        
031593                    '&KDSTOR  =<' W-KDSTOR-MAX-X ')'                      
031594          DELIMITED BY SIZE INTO SSA2                                     
031595     MOVE '  GE' TO GOOD-STATUSCODES                                      
031596     CALL CBLTDLI USING GN 6315-PCB 6316-WDGX6316 SSA1 SSA2               
031597     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031598     PERFORM IMS-STATUSCHECK                                              
031599     .                                                                    
031600     SKIP3                                                                
031601 IMS-ISRT-6315-6316 SECTION.                                              
031602                                                                          
031609                                                                          
031610     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
031611          DELIMITED BY SIZE INTO SSA1                                     
031614     MOVE 'WL631511 ' TO SSA2                                             
031615     MOVE '  II' TO GOOD-STATUSCODES                                      
031616     CALL CBLTDLI USING ISRT 6315-PCB 6316-WDGX6316 SSA1 SSA2             
031617     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031618     PERFORM IMS-STATUSCHECK                                              
031619     .                                                                    
031620     SKIP3                                                                
031621 IMS-REPL-6315-6316 SECTION.                                              
031622                                                                          
031623     MOVE '  ' TO GOOD-STATUSCODES                                        
031624     CALL CBLTDLI USING REPL 6315-PCB 6316-WDGX6316                       
031625     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031626     PERFORM IMS-STATUSCHECK                                              
031627     .                                                                    
031628     SKIP3                                                                
031629 IMS-DLET-6315-6316 SECTION.                                              
031630                                                                          
031631     MOVE '  ' TO GOOD-STATUSCODES                                        
031632     CALL CBLTDLI USING DLET 6315-PCB 6316-WDGX6316                       
031633     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
031634     PERFORM IMS-STATUSCHECK                                              
031635     .                                                                    
031640     EJECT                                                                
031700 IMS-STATUSCHECK SECTION.                                                 
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
