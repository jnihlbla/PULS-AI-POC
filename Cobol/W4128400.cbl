000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4128400.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   09/09/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        SEND ORDERS TO ORDER-ENTRY MPP:S (W4025X, W4026X) VIA            
001000*        DISPATCHER.                                                      
001010*        A CONFIRMATION MAIL WITH IS RETURNED                             
001100*        TO THE SENDER SHOWING GENERATED ORDER NUMBERS.                   
001200*        THIS PROGRAM ONLY RUNS IF THE INPUT DATA HAS                     
001300*        PASSED A PREVIOUS VALIDATION PROGRAM.                            
001400*                                                                         
001500*        THE PROGRAM UPDATES   WDR4                                       
001600*                              WDP8 (VIA W006KOM)                         
001610*                              WDK7                                       
001620*               LÄSER          WDB2  LDC KUNDREGISTER                     
001700*                                                                         
001800*    ABEND CODES:                                                         
001900*        U0016 -  IF ERRORS FROM W006KOM                                  
002000*        U0999 -  IF ERRORS FROM CBLTDLI                                  
002100*                                                                         
002110*                                                                         
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900                                                                          
003000*          --- DATE AND TIME INFO FROM WEB                                
003100     SELECT INPARM                     ASSIGN TO W41284D0.                
003200                                                                          
003300*          --- ORDER TRANSACTIONS                                         
003400     SELECT W41283                     ASSIGN TO W41284D1.                
003500                                                                          
003600*          --- ACCEPTANCE MAIL IN (AFTER RESTART)                         
003700     SELECT W4128A                     ASSIGN TO W41284D2.                
003800                                                                          
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100                                                                          
004200 FILE SECTION.                                                            
004300                                                                          
004400 FD  INPARM                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  FILLER          PIC X(80).                                           
004900                                                                          
005000                                                                          
005100 FD  W41283                                                               
005200     RECORDING       V                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  -COPY W41283      -L.                                                
005600                                                                          
005700                                                                          
005800 FD  W4128A                                                               
005900     RECORDING       V                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200 01  FILLER           PIC X(998).                                         
006300 01  AMAIL-RECORD-IN  PIC X(80).                                          
006400                                                                          
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800 77  IDPGM                       PIC X(8)    VALUE 'W4128400'.            
006900 77  YES                         PIC X       VALUE 'J'.                   
007000 77  NOO                         PIC X       VALUE 'N'.                   
007010*77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007020 77  WS-TILOKDAT                 PIC 9(6)    VALUE ZERO.                  
007030 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
007040 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
007050 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
007100                                                                          
007200 01  ERROR-TEXT.                                                          
007300     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT '.         
007400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007500                                                                          
007600 01  W41283-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W41283                       VALUE 'Y'.                   
007800                                                                          
007900 77  W4128A-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W4128A                       VALUE 'J'.                   
008100                                                                          
008200 77  AMAIL-HEADER-WRITTEN-SW     PIC X       VALUE 'N'.                   
008300     88  AMAIL-HEADER-WRITTEN                VALUE 'J'.                   
008400                                                                          
008500*    -- USED IN CALLS TO W411ORDN INSTEAD OF REAL                         
008600*    -- PCB FOR UNUSED DATABASES.                                         
008700 01  DUMMY-PCB                   PIC X       VALUE LOW-VALUE.             
008800                                                                          
008900*    -- TEMPORARY FIELD FOR ORDER NBR IN DISPLAY FMT                      
009000 01  W-IDORDNR                   PIC 9(7).                                
009100                                                                          
009200 01  CURRENT-IDDISTR             PIC 9(4)    VALUE ZERO.                  
009300 01  CURRENT-IDKUNDNR            PIC 9(6)    VALUE ZERO.                  
009400 01  CURRENT-KDFRAKT             PIC 9(6)    VALUE ZERO.                  
009410 01  CURRENT-KDORDKL             PIC 9(6)    VALUE ZERO.                  
009420 01  CURRENT-IDSYSTEM            PIC X(4)    VALUE SPACE.                 
009500                                                                          
009600*    -- DATA FROM WEB VIA SYMBOLIC PARAMETERS (W41284D0)                  
009700 01  CURRENT-DATE-AND-TIME.                                               
009800     03  CURRENT-CENTURY         PIC X(2).                                
009900     03  CURRENT-YEAR            PIC X(2).                                
010000     03  CURRENT-MONTH           PIC X(2).                                
010100     03  CURRENT-DAY             PIC X(2).                                
010200     03  CURRENT-TIMESTAMP.                                               
010300       05 CURRENT-HOUR           PIC X(2).                                
010400       05 CURRENT-MINUTE         PIC X(2).                                
010500       05 CURRENT-SECOND         PIC X(2).                                
010600                                                                          
010700 01  CHKP-VAR.                                                            
010800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
010900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
011000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
011100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
011200     03 CHKP-CNTR                PIC S9(3)   VALUE +0   COMP-3.           
011300                                                                          
011400*    -- COUNTERS FOR INPUT DATA FILE AND OUTPUT MAIL GSAM FILE            
011500 01  W-KVRESTART-W41283          PIC S9(9)   BINARY VALUE ZERO.           
011600 01  W-KVRESTART-W4128A          PIC S9(9)   BINARY VALUE ZERO.           
011700                                                                          
011800     EJECT                                                                
011900 01  GENERAL-SUBPROGRAMS.                                                 
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
012400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
012500     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
012510     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
012600                                                                          
012700*    --- PARAMETRAR TILL ABEND                                            
012800                                                                          
012900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013100                                                                          
013200                                                                          
013300*    --- PARAMETRAR TILL POSTSUM                                          
013400                                                                          
013500*01  -COPY W0005   -PRE  POSTSUM-                                         
013510 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
013520*   -COPY WORKAREA                                                        
013600                                                                          
013700     EJECT                                                                
013800 01  ORDN-AREA-START             PIC X(16)   VALUE                        
013900                                             'ORDN-AREA-START '.          
014000*    -COPY W411ORDN                                                       
014100     EJECT                                                                
014200 01  IN-AREA-START               PIC X(16)   VALUE                        
014300                                             'IN-AREA-START'.             
014400 01  IN-AREA.                                                             
014500*    03 FILLER -COPY W41283  -PRE IN-                                     
014600*                                                                         
014700     EJECT                                                                
014800 01  AMAIL-AREA-START            PIC X(16)   VALUE                        
014900                                             'AMAIL-AREA-START'.          
015000 01  AMAIL-HDR-1.                                                         
015100     03  FILLER                  PIC X(80)                                
015200     VALUE 'Volvo Car Customer Service'.                                  
015300                                                                          
015400 01  AMAIL-HDR-2.                                                         
015500     03  FILLER                  PIC X(80)                                
015600     VALUE 'The following uploaded order lines have been accepted         
015700-          'by the PULS system.'.                                         
015800                                                                          
016300 01  AMAIL-HDR-3A.                                                        
016400     03  FILLER                  PIC X(8)    VALUE 'Market: '.            
016500     03  AMAIL-BEMARKN           PIC X(24).                               
016600                                                                          
016700 01  AMAIL-HDR-3B.                                                        
016800     03  FILLER                  PIC X(11)   VALUE 'Order ref: '.         
016900     03  AMAIL-BEKUNDRF          PIC X(15).                               
017000                                                                          
017100 01  AMAIL-HDR-4.                                                         
017200     03  FILLER                  PIC X(6)    VALUE 'Date: '.              
017300     03  AMAIL-YEAR              PIC XX.                                  
017400     03  FILLER                  PIC X       VALUE '-'.                   
017500     03  AMAIL-MONTH             PIC XX.                                  
017600     03  FILLER                  PIC X       VALUE '-'.                   
017700     03  AMAIL-DAY               PIC XX.                                  
017800     03  FILLER                  PIC X(7)    VALUE ' Time: '.             
017900     03  AMAIL-HOUR              PIC XX.                                  
018000     03  FILLER                  PIC X       VALUE ':'.                   
018100     03  AMAIL-MINUTE            PIC XX.                                  
018200     03  FILLER                  PIC X       VALUE ':'.                   
018300     03  AMAIL-SECOND            PIC XX.                                  
018400                                                                          
018500 01  AMAIL-HDR-5.                                                         
018600     03  FILLER                  PIC X(10)   VALUE '  District'.          
018700     03  FILLER                  PIC X(10)   VALUE '    Dealer'.          
018800     03  FILLER                  PIC X(10)   VALUE '  Order no'.          
018900     03  FILLER                  PIC X(10)   VALUE '.  Part no'.          
019000     03  FILLER                  PIC X(10)   VALUE '. Quantity'.          
019100                                                                          
019200 01  AMAIL-DATA.                                                          
019300     03  FILLER                  PIC X(6)    VALUE SPACE.                 
019400     03  AMAIL-IDDISTR           PIC ZZZ9.                                
019500     03  FILLER                  PIC X(4)    VALUE SPACE.                 
019600     03  AMAIL-IDKUNDNR          PIC Z(5)9.                               
019700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
019800     03  AMAIL-IDORDNR           PIC Z(6)9.                               
019900     03  FILLER                  PIC X(2)    VALUE SPACE.                 
020000     03  AMAIL-IDARTNR           PIC Z(7)9.                               
020100     03  FILLER                  PIC X(4)    VALUE SPACE.                 
020200     03  AMAIL-KVBEART           PIC Z(5)9.                               
020300                                                                          
020400 01  AMAIL-SPACE                 PIC X       VALUE SPACE.                 
020500                                                                          
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020800                                                                          
020900 01  KEYS-TILL-DLI.                                                       
021000     03  W-WDGXKEY-X.                                                     
021100         05  W-4201-IDHTYP       PIC X(4).                                
021200         05  W-4201-FILLER       PIC X(26).                               
021300     03  W-KDSEGKEY-X.                                                    
021400         05  W-4202-KDSEGKEY     PIC X(1).                                
021410     03  W-IDARTNR-X.                                                     
021420         05  W-IDARTNR           PIC S9(9) COMP-3.                        
021430     03 W-IDDC-X.                                                         
021440         05 W-IDDC               PIC X(2).                                
021450     03 W-IDGMT-X.                                                        
021460         05 W-WDB2-IDDISTR       PIC S9(5) VALUE ZERO COMP-3.             
021470         05 W-WDB2-IDKUNDNR      PIC S9(7) VALUE ZERO COMP-3.             
021500                                                                          
021600*    --- STATUS-KOD FRÅN IMS                                              
021700 01  STATUS-WS                   PIC XX.                                  
021800     88  SEGMENT-FOUND                       VALUE '  '.                  
021900     88  SEGMENT-ALREADY-EXISTS              VALUE 'II'.                  
022000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
022100     88  IMS-NOT-OK                          VALUE 'XD'.                  
022200                                                                          
022300 01  GOOD-STATUSCODES.                                                    
022400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022500                                                                          
022600 01  SSA1                        PIC X(200).                              
022700 01  SSA2                        PIC X(200).                              
022800                                                                          
022900*    --- IMS FUNCTION CODES                                               
023000*01  -COPY W0003                                                          
023100                                                                          
023200     EJECT                                                                
023300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX4202 '.        
023400 01  DLI-IO-WDGX4202.                                                     
023500*    03  -COPY WDGX4202                                                   
023600                                                                          
023610 01  FILLER                  PIC X(16) VALUE 'IO-AREA-K711'.              
023620 01  IO-AREA-K711.                                                        
023630*    03  -COPY WDK711                                                     
023640     EJECT                                                                
023641                                                                          
023642 01  FILLER                      PIC X(16)  VALUE 'AREA FOR WDB2'.        
023643 01  DLI-IO-AREA-WDB2.                                                    
023644     03  DLI-IO-WDB201.                                                   
023645*        05  -COPY WDB201                                                 
023650                                                                          
023700     EJECT                                                                
023800*    --- AREAS FOR SUB MODULES W006KOM                                    
023900                                                                          
024000 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
024100*01  -COPY WMSGKOM                                                        
024200     EJECT                                                                
024300 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
024400*01  -COPY WMSGAREA                                                       
024500     EJECT                                                                
024600 01  FILLER                    PIC X(16) VALUE '4251-MID-AREA   '.        
024700*01  -COPY W4I25101 -PRE 4251-                                            
024800     EJECT                                                                
024900 01  FILLER                    PIC X(16) VALUE '4252-MID-AREA   '.        
025000 01  LIX                       PIC S9(4)  BINARY.                         
025100*01  -COPY W4I25201 -PRE 4252-                                            
025200     EJECT                                                                
025210 01  FILLER                    PIC X(16) VALUE '4261-MID-AREA   '.        
025220*01  -COPY W4I26101 -PRE 4261-                                            
025230     EJECT                                                                
025240 01  FILLER                    PIC X(16) VALUE '4262-MID-AREA   '.        
025250                                                                          
025260*01  -COPY W4I26201 -PRE 4262-                                            
025270     EJECT                                                                
025300 01  FILLER                    PIC X(16) VALUE 'AMAIL-IO-AREA'.           
025400                                                                          
025500 01  MAX-GSAM                  PIC S9(4) BINARY VALUE +82.                
025600                                                                          
025700 01  AMAIL-GSAM-IO-AREA.                                                  
025800     03  AMAIL-GSAM-LRECL      PIC S9(4) COMP.                            
025900     03  AMAIL-GSAM-AREA       PIC X(80).                                 
026000                                                                          
026100     EJECT                                                                
026200*01  -COPY WWDIST20                                                       
026300     EJECT                                                                
026400 LINKAGE SECTION.                                                         
026500                                                                          
026600*01  -COPY W0009   -PRE MSG-                                              
026700                                                                          
026800*01  -COPY W0009   -PRE 0693X-                                            
026900                                                                          
027000*01  -COPY W0008   -PRE WDR4-                                             
027100     05 FILLER                   PIC X.                                   
027200                                                                          
027300*01  -COPY W0008   -PRE WDP8-                                             
027400     05  FILLER                  PIC X.                                   
027500                                                                          
027600*01  -COPY W0008  -PRE XXKP-                                              
027700     05  FILLER                  PIC X.                                   
027800                                                                          
027900*01  -COPY W0008  -PRE ORQL-                                              
028000     05  FILLER                  PIC X.                                   
028100                                                                          
028200*01  -COPY W0008  -PRE PROC-                                              
028300     05  FILLER                  PIC X.                                   
028400                                                                          
028500*01  -COPY W0008  -PRE ORQI-                                              
028600     05  FILLER                  PIC X.                                   
028610                                                                          
028620*01  -COPY W0008   -PRE WDK7-                                             
028630     05 FILLER                   PIC X.                                   
028700                                                                          
028710*01  -COPY W0008   -PRE WDB2-                                             
028720     05 FILLER                   PIC X.                                   
028730                                                                          
028800*01  -COPY W0008  -PRE AMAIL-GSAM-                                        
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100 PROCEDURE DIVISION  USING MSG-PCB 0693X-PCB WDR4-PCB WDP8-PCB            
029200                           XXKP-PCB ORQL-PCB PROC-PCB ORQI-PCB            
029300                           WDK7-PCB WDB2-PCB AMAIL-GSAM-PCB.              
029400 MAIN SECTION.                                                            
029500     ENTRY 'DLITCBL' USING MSG-PCB 0693X-PCB WDR4-PCB WDP8-PCB            
029600                           XXKP-PCB ORQL-PCB PROC-PCB ORQI-PCB            
029610                           WDK7-PCB WDB2-PCB AMAIL-GSAM-PCB.              
029800                                                                          
029900     PERFORM A-INIT                                                       
030000                                                                          
030100*    -- READ RESTART SEGMENT (4202) AND SKIP ALREADY PROCESSED            
030200*    -- INPUT RECORDS AND REWRITE WRITTEN GSAM FILE RECORDS               
030300*    -- NORMALLY KVPOST IN 4202 IS ZERO AND PROCESSING STARTS             
030400*    -- FROM THE BEGINNING                                                
030500     PERFORM IMS-GHU-WDGX4202                                             
030600     PERFORM E-RESTART-W41283-INPUT                                       
030700     PERFORM F-RESTART-W4128A-GSAM                                        
030800                                                                          
030900     PERFORM UNTIL END-OF-W41283                                          
031000                                                                          
031100       IF IN-OLIN-IDDISTR  NOT = CURRENT-IDDISTR                          
031200       OR IN-OLIN-IDKUNDNR NOT = CURRENT-IDKUNDNR                         
031300       OR IN-OLIN-KDFRAKT  NOT = CURRENT-KDFRAKT                          
031310       OR IN-OLIN-KDORDKL  NOT = CURRENT-KDORDKL                          
031400                                                                          
031500*        -- DO NOT TAKE CHECK-POINT BEFORE FIRST ORDER HEAD               
031600         IF CHKP-CNTR > 0                                                 
031700           PERFORM X-TAKE-CHECKPOINT                                      
031800         END-IF                                                           
031900         IF LIX > 0                                                       
032000*          -- ORDER LINES FROM PREVIOUS ORDER EXIST.                      
032100*          -- DISPATCH THEM AND MARK ORDER AS COMPLETE                    
032110            IF CURRENT-IDSYSTEM NOT = 'PXCL'                              
032200               MOVE YES TO 4252-MID-FLSLUT                                
032300               PERFORM C-DISPATCH-ORDER-LINES                             
032310            ELSE                                                          
032311               MOVE YES TO 4262-MID-FLSLUT                                
032312               PERFORM I-DISPATCH-PROFORMA-LINES                          
032320            END-IF                                                        
032400         END-IF                                                           
032500         MOVE IN-OLIN-IDDISTR  TO CURRENT-IDDISTR                         
032600         MOVE IN-OLIN-IDKUNDNR TO CURRENT-IDKUNDNR                        
032700         MOVE IN-OLIN-KDFRAKT  TO CURRENT-KDFRAKT                         
032710         MOVE IN-OLIN-KDORDKL  TO CURRENT-KDORDKL                         
032720         MOVE IN-OLIN-IDSYSTEM TO CURRENT-IDSYSTEM                        
032810         IF CURRENT-IDSYSTEM NOT = 'PXCL'                                 
032820            PERFORM B-CREATE-DISPATCH-ORDER-HEAD                          
032830         ELSE                                                             
032831            PERFORM H-CREATE-DISPATCH-PROF-HEAD                           
032840         END-IF                                                           
032900         MOVE ZERO TO LIX                                                 
033000       END-IF                                                             
033100                                                                          
033200       ADD 1 TO LIX                                                       
033300       IF LIX > 5                                                         
033310          IF CURRENT-IDSYSTEM NOT = 'PXCL'                                
033311*      -- THE 4252 TRANSACTION IS FULL -                                  
033312*      -- DISPATCH IT AND CONTINUE WITH ANOTHER                           
033313            MOVE NOO TO 4252-MID-FLSLUT                                   
033314            PERFORM C-DISPATCH-ORDER-LINES                                
033315            MOVE 1 TO LIX                                                 
033320          ELSE                                                            
033321*      -- THE 4262 TRANSACTION IS FULL -                                  
033322*      -- DISPATCH IT AND CONTINUE WITH ANOTHER                           
033323            MOVE NOO TO 4262-MID-FLSLUT                                   
033324            PERFORM I-DISPATCH-PROFORMA-LINES                             
033325            MOVE 1 TO LIX                                                 
033330          END-IF                                                          
033900       END-IF                                                             
033910       IF CURRENT-IDSYSTEM NOT = 'PXCL'                                   
034000          PERFORM D-CREATE-ORDER-LINE                                     
034010       ELSE                                                               
034011          PERFORM J-CREATE-PROFORMA-LINE                                  
034020       END-IF                                                             
034100       PERFORM G-WRITE-ACCEPTANCE-MAIL-DATA                               
034200                                                                          
034300       PERFORM S01-READ-W41283                                            
034400     END-PERFORM                                                          
034500                                                                          
034600     IF LIX > 0                                                           
034610       IF CURRENT-IDSYSTEM NOT = 'PXCL'                                   
034700*      -- ORDER LINES FROM LAST ORDER EXIST.                              
034800*      -- DISPATCH THEM AND MARK ORDER AS COMPLETE                        
034900          MOVE YES TO 4252-MID-FLSLUT                                     
035000          PERFORM C-DISPATCH-ORDER-LINES                                  
035010       ELSE                                                               
035011*      -- ORDER LINES FROM LAST ORDER EXIST.                              
035012*      -- DISPATCH THEM AND MARK ORDER AS COMPLETE                        
035013          MOVE YES TO 4262-MID-FLSLUT                                     
035014          PERFORM I-DISPATCH-PROFORMA-LINES                               
035020       END-IF                                                             
035100     END-IF                                                               
035200                                                                          
035300     PERFORM Z-FINIT                                                      
035400                                                                          
035500     MOVE ZERO TO RETURN-CODE                                             
035600     GOBACK                                                               
035700     .                                                                    
035800                                                                          
035900     EJECT                                                                
036000 A-INIT SECTION.                                                          
036100                                                                          
036200*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
036300*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
036400     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
036500     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
036600     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
036700     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
036800     MOVE 'W4128400'                 TO MSG-KOM-IDSNDJOB                  
036900     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
037000*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH ORDER                     
037100*    -- THIS IS THE START VALUE                                           
037200     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
037300                                                                          
037400*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
037500     MOVE LOW-VALUE                  TO MSG-KDZ1                          
037600     MOVE LOW-VALUE                  TO MSG-KDZ2                          
037700                                                                          
037800*    -- INITIALIZE KEYS FOR RESTART DB                                    
037900     MOVE '4201'     TO W-4201-IDHTYP                                     
038000     MOVE LOW-VALUE  TO W-4201-FILLER                                     
038100     MOVE '1'        TO W-4202-KDSEGKEY                                   
038200                                                                          
038300     PERFORM IMS-RESTART                                                  
038400                                                                          
038500     OPEN INPUT INPARM                                                    
038600                W41283                                                    
038700                                                                          
038800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
038900                                                                          
039000*    -- FETCH TIME INFO FROM SYMBOLIC PARAMETERS IN JCL                   
039100     READ INPARM INTO CURRENT-DATE-AND-TIME                               
039200     AT END                                                               
039300*      -- WHY NOT?                                                        
039400       MOVE FUNCTION CURRENT-DATE(1:14) TO CURRENT-DATE-AND-TIME          
039500     END-READ                                                             
039600     MOVE CURRENT-YEAR      TO AMAIL-YEAR                                 
039700     MOVE CURRENT-MONTH     TO AMAIL-MONTH                                
039800     MOVE CURRENT-DAY       TO AMAIL-DAY                                  
039900     MOVE CURRENT-HOUR      TO AMAIL-HOUR                                 
040000     MOVE CURRENT-MINUTE    TO AMAIL-MINUTE                               
040100     MOVE CURRENT-SECOND    TO AMAIL-SECOND                               
040200                                                                          
040300     MOVE MAX-GSAM          TO AMAIL-GSAM-LRECL                           
040400                                                                          
040500     MOVE ZERO TO LIX                                                     
040600     MOVE ZERO TO CHKP-CNTR                                               
040700     .                                                                    
040800                                                                          
040900     EJECT                                                                
041000 B-CREATE-DISPATCH-ORDER-HEAD SECTION.                                    
041100                                                                          
041200*    -- INITIALIZE MID DATA TO W40251                                     
041300     MOVE SPACE               TO 4251-MID-W4I25101                        
041400     MOVE IN-OLIN-IDSYSTEM    TO 4251-MID-IDSYSTEM                        
041500     MOVE IN-OLIN-IDDISTR     TO 4251-MID-IDDISTR                         
041600     MOVE IN-OLIN-IDKUNDNR    TO 4251-MID-IDKUNDNR                        
041700     MOVE IN-OLIN-KDORDKL     TO 4251-MID-KDORDKL                         
041800     MOVE IN-OLIN-BEKUNDRF    TO 4251-MID-BEKUNDRF                        
041810     MOVE IN-OLIN-BEVARREF    TO 4251-MID-BEVARREF                        
041900                                                                          
042000     IF IN-OLIN-KDFRAKT NOT = ZERO                                        
042100       MOVE IN-OLIN-KDFRAKT   TO 4251-MID-KDFRAKT                         
042200     END-IF                                                               
042310     MOVE IN-OLIN-FLFORBI     TO 4251-MID-FLFORBI                         
042400     MOVE IN-OLIN-KDTPOTYP    TO 4251-MID-KDTPOTYP                        
042500     MOVE IN-OLIN-TITPO       TO 4251-MID-TITPO                           
042510     MOVE IN-OLIN-TIREPDAT    TO 4251-MID-TIREPDAT                        
042520     IF CURRENT-IDSYSTEM = 'XCEL'                                         
042521        IF 4251-MID-TIREPDAT = SPACE OR ZERO                              
042530           CONTINUE                                                       
042531        ELSE                                                              
042540           PERFORM BB-SKAPA-RFSDATUM                                      
042541        END-IF                                                            
042550     END-IF                                                               
042552     IF CURRENT-IDSYSTEM NOT = 'REFB'                                     
042802        MOVE IN-OLIN-IDDC        TO 4251-MID-IDDC                         
042803        MOVE IN-OLIN-BEGMT-RAD1  TO 4251-MID-BEGMT-RAD1                   
042804        MOVE IN-OLIN-BEGMT-RAD2  TO 4251-MID-BEGMT-RAD2                   
042805        MOVE IN-OLIN-ADGMT-GATA  TO 4251-MID-ADGMT-GATA                   
042810     END-IF                                                               
042900                                                                          
043000*    -- W4T251 HAS NO "LAND" ADDRESS FIELD.                               
043100*    -- APPEND ANY COUNTRY NAME AFTER THE POSTAL ADDRESS                  
043200     MOVE SPACE               TO 4251-MID-ADGMT-PADR                      
043300     STRING                                                               
043400          IN-OLIN-ADGMT-PADR DELIMITED BY '  '                            
043500          ' '                DELIMITED BY SIZE                            
043600          IN-OLIN-ADGMT-LAND DELIMITED BY SIZE                            
043700        INTO 4251-MID-ADGMT-PADR                                          
043800                                                                          
043900     PERFORM BA-GENERATE-ORDER-NUMBER                                     
044000*    -- ORDER NBR FROM W411ORDN                                           
044100     MOVE ORDN-IDORDNR-UT     TO W-IDORDNR                                
044200     MOVE W-IDORDNR           TO 4251-MID-IDORDNR                         
044300                                                                          
044400*    -- INITALIZE SOME FIELDS THAT MUST NOT BE BLANK                      
044500     MOVE IN-OLIN-IDDISTR TO DIST20-IDDISTR                               
044600     IF DIST20-EMBALLAGE                                                  
044700       MOVE YES               TO 4251-MID-FLEMBORD                        
044800     ELSE                                                                 
044900       MOVE NOO               TO 4251-MID-FLEMBORD                        
045000     END-IF                                                               
045100                                                                          
045200     MOVE NOO                 TO 4251-MID-FLAUTPAC                        
045400                                 4251-MID-FLOVRLEV                        
045500                                                                          
045600     MOVE ZERO                TO 4251-MID-IDDEPT                          
045700                                 4251-MID-IDGROSS                         
045800                                                                          
045900*    -- MOVE TO MSG-IO-AREA AND ADD TRANSACTION PREFIX                    
046000*    -- OUTSIDE THE MID COPYTEXT                                          
046100     COMPUTE MSG-KVLL = LENGTH OF 4251-MID-W4I25101 + 17                  
046200     MOVE 4251-MID-W4I25101   TO MSG-INDATA-MINUS-1-TRANSKOD              
046300     MOVE 'W4T251X '          TO MSG-KDTRANS-1                            
046400     MOVE '4251'              TO MSG-IDTRANS-1                            
046500     MOVE '1'                 TO MSG-KDMFSFOR-1                           
046600                                                                          
046700*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
046800*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
046900     MOVE 'W4I25101'          TO MSG-KOM-IDCPYTXT                         
046910     IF CURRENT-IDSYSTEM = 'REFB'                                         
047000       MOVE IN-OLIN-IDSYSTEM  TO MSG-KOM-IDSNDNOD(1:4)                    
047100       MOVE IN-OLIN-IDDISTR   TO MSG-KOM-IDSNDNOD(5:4)                    
047110     ELSE                                                                 
047111       MOVE 'BYPASS  '        TO MSG-KOM-IDSNDNOD                         
047120     END-IF                                                               
047200     ADD  1                   TO MSG-KOM-TIKLOCK                          
047310                                                                          
047400     CALL W006KOM USING MSG-PCB                                           
047500                        0693X-PCB                                         
047600                        WDP8-PCB                                          
047700                        MSG-KOM-WMSGKOM                                   
047800                        MSG-IO-AREA                                       
047900                                                                          
048000     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
048100        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
048200          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
048300        DISPLAY  ERROR-TEXT-STR                                           
048400        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
048500     END-IF                                                               
048600                                                                          
048700*    -- COUNT NUMBER OF ORDER HEADS FOR CHECKPOINT LOGIC                  
048800     ADD 1 TO CHKP-CNTR                                                   
048900                                                                          
049000*    -- CLEAR ORDER-LINE AREA BEFORE FIRST LINE IS ADDED                  
049100     MOVE SPACE               TO 4252-MID-W4I25201                        
049200     .                                                                    
049300     EJECT                                                                
049400 BA-GENERATE-ORDER-NUMBER SECTION.                                        
049500                                                                          
049600     MOVE IN-OLIN-IDSYSTEM   TO ORDN-IDSYSTEM                             
049700     MOVE IN-OLIN-IDDISTR    TO ORDN-IDDISTR                              
049800     MOVE IN-OLIN-IDKUNDNR   TO ORDN-IDKUNDNR                             
049900     MOVE ZERO               TO ORDN-IDORDNR-IN                           
050000                                                                          
050100     CALL W411ORDN USING  ORDN-W411ORDN XXKP-PCB ORQL-PCB                 
050200                          PROC-PCB ORQI-PCB DUMMY-PCB                     
050300     .                                                                    
050400                                                                          
050500     EJECT                                                                
050510 BB-SKAPA-RFSDATUM SECTION.                                               
050520     SKIP2                                                                
050531                                                                          
050532     MOVE IN-Olin-IDDISTR        TO W-WDB2-IDDISTR                        
050533     MOVE IN-Olin-IDKUNDNR       TO W-WDB2-IDKUNDNR                       
050534     PERFORM IMS-GU-WDB201                                                
050535     IF SEGMENT-Found                                                     
050540        MOVE GMT-IDDC-BULK(1)      TO WORK-IDDC                           
050550        MOVE +002                  TO WORK-KDCALL                         
050560        MOVE +001                  TO WORK-KVWORKD                        
050570*    IF  IN-OLIN-TIREPDAT = ZERO                                          
050580*     OR IN-OLIN-TIREPDAT < WS-TILOKDAT                                   
050590*       MOVE WS-TILOKDAT           TO WORK-TIAAMMDD-FOM                   
050591*    ELSE                                                                 
050592        MOVE IN-OLIN-TIREPDAT      TO WORK-TIAAMMDD-FOM                   
050593*    END-IF                                                               
050594        CALL WORKDAY               USING WORK-KDCALL                      
050595                                            WORK-DATE-AREA                
050596                                            WORK-KDSVAR                   
050597        IF WORK-KDSVAR-FEL                                                
050598           MOVE 'SECT CBA-1, DATUM SAKNAS I WORKDAY'                      
050599                                      TO FELTEXT                          
050600           CALL ABEND              USING RKOD-ABEND-NO-DUMP               
050601        ELSE                                                              
050602          MOVE +003                TO WORK-KDCALL                         
050603                                                                          
050604          MOVE GMT-KVDAGAR-RFS-DEF TO WORK-KVWORKD                        
050605          PERFORM                                                         
050606          VARYING RFS-IX FROM 1 BY 1                                      
050607            UNTIL RFS-IX > MAX-RFS-IX                                     
050608            IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                          
050609              MOVE GMT-KVDAGAR-RFS (RFS-IX)                               
050610                                      TO WORK-KVWORKD                     
050611            END-IF                                                        
050612          END-PERFORM                                                     
050613          ADD +1 TO WORK-KVWORKD                                          
050614*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
050615*      ANTAL DAGAR FÖRE RFS.                                              
050616*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
050617*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
050618*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
050619*                                                                         
050620          CALL WORKDAY             USING WORK-KDCALL                      
050621                                            WORK-DATE-AREA                
050622                                            WORK-KDSVAR                   
050623          IF WORK-KDSVAR-FEL                                              
050624             MOVE 'SECT CBA-2, DATUM SAKNAS I WORKDAY'                    
050625                                      TO FELTEXT                          
050626             CALL ABEND            USING RKOD-ABEND-NO-DUMP               
050627          ELSE                                                            
050628            IF WORK-TIAAMMDD-FOM < WS-TILOKDAT                            
050629              MOVE GMT-IDDC-BULK(1) TO WORK-IDDC                          
050630              MOVE +002             TO WORK-KDCALL                        
050631              MOVE +001             TO WORK-KVWORKD                       
050632              MOVE WS-TILOKDAT      TO WORK-TIAAMMDD-FOM                  
050633              CALL WORKDAY         USING WORK-KDCALL                      
050634                                            WORK-DATE-AREA                
050635                                            WORK-KDSVAR                   
050636              IF WORK-KDSVAR-FEL                                          
050637                 MOVE 'SECT CBA-3, DATUM SAKNAS I WORKDAY'                
050638                                      TO FELTEXT                          
050639                 CALL ABEND        USING RKOD-ABEND-NO-DUMP               
050640              ELSE                                                        
050641                MOVE WORK-TIAAMMDD-TOM TO 4251-MID-TIRFS                  
050642             END-IF                                                       
050643           ELSE                                                           
050644             MOVE WORK-TIAAMMDD-FOM TO 4251-MID-TIRFS                     
050645           END-IF                                                         
050646         END-IF                                                           
050647       END-IF                                                             
050648     END-IF                                                               
050649     .                                                                    
050650     EJECT                                                                
050651                                                                          
050670 C-DISPATCH-ORDER-LINES SECTION.                                          
050700                                                                          
050800     COMPUTE MSG-KVLL = LENGTH OF 4252-MID-W4I25201 + 17                  
050900     MOVE 4252-MID-W4I25201   TO MSG-INDATA-MINUS-1-TRANSKOD              
051000     MOVE 'W4T252X '          TO MSG-KDTRANS-1                            
051100     MOVE '4252'              TO MSG-IDTRANS-1                            
051200     MOVE '1'                 TO MSG-KDMFSFOR-1                           
051300                                                                          
051400     CALL W006KOM USING MSG-PCB                                           
051500                        0693X-PCB                                         
051600                        WDP8-PCB                                          
051700                        MSG-KOM-WMSGKOM                                   
051800                        MSG-IO-AREA                                       
051900                                                                          
052000     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
052100        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
052200          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
052300        DISPLAY  ERROR-TEXT-STR                                           
052400        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
052500     END-IF                                                               
052600                                                                          
052700*    -- CLEAR ORDER-LINE AREA BEFORE NEXT LINE IS ADDED                   
052800     MOVE SPACE               TO 4252-MID-W4I25201                        
052900     .                                                                    
053000                                                                          
053100 D-CREATE-ORDER-LINE SECTION.                                             
053200                                                                          
053300     MOVE 'W4I25201'          TO MSG-KOM-IDCPYTXT                         
053400                                                                          
053500     MOVE IN-OLIN-IDSYSTEM    TO 4252-MID-IDSYSTEM                        
053600     MOVE IN-OLIN-IDDISTR     TO 4252-MID-IDDISTR                         
053700     MOVE IN-OLIN-IDKUNDNR    TO 4252-MID-IDKUNDNR                        
053800     MOVE IN-OLIN-IDARTNR     TO 4252-MID-IDARTNR(LIX)                    
053900     MOVE IN-OLIN-REKSIFFR    TO 4252-MID-REKSIFFR(LIX)                   
054000     MOVE IN-OLIN-KVBEART     TO 4252-MID-KVBEART(LIX)                    
054100     MOVE IN-OLIN-BERADREF    TO 4252-MID-BERADREF(LIX)                   
054200                                                                          
054210***IF BYPASS REFILL ORDER THEN UPDATE ON ORDER(KVBEART)                   
054220     IF CURRENT-IDSYSTEM = 'REFB'                                         
054230       MOVE IN-OLIN-IDARTNR TO W-IDARTNR                                  
054240       MOVE IN-OLIN-IDDC       TO W-IDDC                                  
054250       PERFORM IMS-GHU-WDK711                                             
054260       IF SEGMENT-FOUND                                                   
054270         ADD IN-OLIN-KVBEART TO SLAG-KVBEART                              
054280         PERFORM IMS-REPL-WDK711                                          
054290       ELSE                                                               
054291         CALL FELLOG                                                      
054292       END-IF                                                             
054293     END-IF                                                               
054300*    -- ORDER NBR FROM W411ORDN                                           
054400     MOVE W-IDORDNR           TO 4252-MID-IDORDNR                         
054500     .                                                                    
054600                                                                          
054700     EJECT                                                                
054800 E-RESTART-W41283-INPUT       SECTION.                                    
054900                                                                          
055000     PERFORM S01-READ-W41283                                              
055100     PERFORM UNTIL END-OF-W41283                                          
055200     OR W-KVRESTART-W41283 >= 4202-KVRESTART-W41283                       
055300        PERFORM S01-READ-W41283                                           
055400     END-PERFORM                                                          
055500     IF W-KVRESTART-W41283 < 4202-KVRESTART-W41283                        
055600        MOVE 'EOF on W41283 during restart before enough skipped'         
055700        TO ERROR-TEXT-STR                                                 
055800        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
055900     END-IF                                                               
056000     .                                                                    
056100                                                                          
056200     EJECT                                                                
056300 F-RESTART-W4128A-GSAM SECTION.                                           
056400                                                                          
056500*    -- DO NOT OPEN THE RESTART FILE UNLESS RESTART                       
056600*    -- REALLY SHOULD BE DONE. THIS AVOIDS PROBLEMS                       
056700*    -- WITH AN UNREADABLE GSAM FILE AFTER ABEND                          
056800     IF 4202-KVRESTART-W4128A > 0                                         
056900       OPEN INPUT W4128A                                                  
057000                                                                          
057100       PERFORM S02-READ-W4128A                                            
057200       PERFORM UNTIL END-OF-W4128A                                        
057300       OR W-KVRESTART-W4128A >= 4202-KVRESTART-W4128A                     
057400          PERFORM S03-WRITE-AMAIL-GSAM                                    
057500          PERFORM S02-READ-W4128A                                         
057600       END-PERFORM                                                        
057700       IF W-KVRESTART-W4128A < 4202-KVRESTART-W4128A                      
057800          MOVE                                                            
057900          'EOF on W4128A during restart before enough skipped'            
058000          TO ERROR-TEXT-STR                                               
058100          CALL ABEND USING RKOD-ABEND-NO-DUMP                             
058200       END-IF                                                             
058300                                                                          
058400       CLOSE W4128A                                                       
058500                                                                          
058600*      -- HEADER ALREADY WRITTEN IN PREVIOUS RUN                          
058700       SET AMAIL-HEADER-WRITTEN TO TRUE                                   
058800     END-IF                                                               
058900     .                                                                    
059000                                                                          
059100     EJECT                                                                
059200 G-WRITE-ACCEPTANCE-MAIL-DATA SECTION.                                    
059300                                                                          
059400     IF NOT AMAIL-HEADER-WRITTEN                                          
059500        SET AMAIL-HEADER-WRITTEN TO TRUE                                  
059600                                                                          
059700        MOVE AMAIL-HDR-1    TO AMAIL-GSAM-AREA                            
059800        PERFORM S03-WRITE-AMAIL-GSAM                                      
059900        MOVE AMAIL-SPACE    TO AMAIL-GSAM-AREA                            
060000        PERFORM S03-WRITE-AMAIL-GSAM                                      
060100        MOVE AMAIL-HDR-2    TO AMAIL-GSAM-AREA                            
060200        PERFORM S03-WRITE-AMAIL-GSAM                                      
060500        MOVE AMAIL-SPACE    TO AMAIL-GSAM-AREA                            
060600        PERFORM S03-WRITE-AMAIL-GSAM                                      
060700                                                                          
060800        IF IN-OLIN-IDSYSTEM   = 'SPX '                                    
060900          MOVE IN-OLIN-BEMARKN  TO AMAIL-BEMARKN                          
061000          MOVE AMAIL-HDR-3A   TO AMAIL-GSAM-AREA                          
061100          PERFORM S03-WRITE-AMAIL-GSAM                                    
061200        ELSE                                                              
061210          IF IN-OLIN-IDSYSTEM NOT = 'REFB'                                
061300            MOVE IN-OLIN-BEKUNDRF TO AMAIL-BEKUNDRF                       
061400            MOVE AMAIL-HDR-3B TO AMAIL-GSAM-AREA                          
061500            PERFORM S03-WRITE-AMAIL-GSAM                                  
061510          END-IF                                                          
061600        END-IF                                                            
061700        MOVE AMAIL-HDR-4    TO AMAIL-GSAM-AREA                            
061800        PERFORM S03-WRITE-AMAIL-GSAM                                      
061900        MOVE AMAIL-SPACE    TO AMAIL-GSAM-AREA                            
062000        PERFORM S03-WRITE-AMAIL-GSAM                                      
062100        MOVE AMAIL-HDR-5    TO AMAIL-GSAM-AREA                            
062200        PERFORM S03-WRITE-AMAIL-GSAM                                      
062300     END-IF                                                               
062400                                                                          
062500     MOVE AMAIL-SPACE    TO AMAIL-GSAM-AREA                               
062600     PERFORM S03-WRITE-AMAIL-GSAM                                         
062700     MOVE IN-OLIN-IDDISTR  TO AMAIL-IDDISTR                               
062800     MOVE IN-OLIN-IDKUNDNR TO AMAIL-IDKUNDNR                              
062900     MOVE W-IDORDNR        TO AMAIL-IDORDNR                               
063000     MOVE IN-OLIN-IDARTNR  TO AMAIL-IDARTNR                               
063100     MOVE IN-OLIN-KVBEART  TO AMAIL-KVBEART                               
063200     MOVE AMAIL-DATA       TO AMAIL-GSAM-AREA                             
063300     PERFORM S03-WRITE-AMAIL-GSAM                                         
063400     .                                                                    
063500                                                                          
063600     EJECT                                                                
063610 H-CREATE-DISPATCH-PROF-HEAD SECTION.                                     
063620                                                                          
063630*    -- INITIALIZE MID DATA TO W40251                                     
063640     MOVE SPACE               TO 4261-MID-W4I26101                        
063650*    MOVE IN-OLIN-IDSYSTEM    TO 4261-MID-IDSYSTEM                        
063660     MOVE IN-OLIN-IDDISTR     TO 4261-MID-IDDISTR                         
063670     MOVE IN-OLIN-IDKUNDNR    TO 4261-MID-IDKUNDNR                        
063680     MOVE IN-OLIN-KDORDKL     TO 4261-MID-KDORDKL                         
063690     MOVE IN-OLIN-BEKUNDRF    TO 4261-MID-BEKUNDRF                        
063691     MOVE IN-OLIN-BEVARREF    TO 4261-MID-BEVARREF                        
063692                                                                          
063693     IF IN-OLIN-KDFRAKT NOT = ZERO                                        
063694       MOVE IN-OLIN-KDFRAKT   TO 4261-MID-KDFRAKT                         
063695     END-IF                                                               
063696     MOVE IN-OLIN-IDKONTO     TO 4261-MID-IDKONTO                         
063697     MOVE IN-OLIN-IDSKYLT     TO 4261-MID-IDSKYLT                         
063698     MOVE IN-OLIN-FORFDAT     TO 4261-MID-FORFDAT                         
063699     MOVE IN-OLIN-KDPROTYP    TO 4261-MID-KDPROTYP                        
063700     MOVE IN-OLIN-KDFAKTYP    TO 4261-MID-KDFAKTYP                        
063703*    MOVE IN-OLIN-IDDC        TO 4261-MID-IDDC                            
063704*    MOVE IN-OLIN-FLFORBI     TO 4261-MID-FLFORBI                         
063705*    MOVE IN-OLIN-KDTPOTYP    TO 4261-MID-KDTPOTYP                        
063706*    MOVE IN-OLIN-TITPO       TO 4261-MID-TITPO                           
063707     MOVE IN-OLIN-BEGMT-RAD1  TO 4261-MID-BEGMT-RAD1                      
063708     MOVE IN-OLIN-BEGMT-RAD2  TO 4261-MID-BEGMT-RAD2                      
063709     MOVE IN-OLIN-ADGMT-GATA  TO 4261-MID-ADGMT-GATA                      
063710                                                                          
063711*    -- W4T261 HAS NO "LAND" ADDRESS FIELD.                               
063712*    -- APPEND ANY COUNTRY NAME AFTER THE POSTAL ADDRESS                  
063713     MOVE SPACE               TO 4261-MID-ADGMT-PADR                      
063714     STRING                                                               
063715          IN-OLIN-ADGMT-PADR DELIMITED BY '  '                            
063716          ' '                DELIMITED BY SIZE                            
063717          IN-OLIN-ADGMT-LAND DELIMITED BY SIZE                            
063718        INTO 4261-MID-ADGMT-PADR                                          
063719     MOVE IN-OLIN-BEBETRAD-1  TO 4261-MID-BEBETRAD-1                      
063720     MOVE IN-OLIN-BEBETRAD-2  TO 4261-MID-BEBETRAD-2                      
063721     MOVE IN-OLIN-ADBETRAD-1  TO 4261-MID-ADBETRAD-1                      
063722     MOVE IN-OLIN-ADBETRAD-2  TO 4261-MID-ADBETRAD-2                      
063723                                                                          
063724     PERFORM HA-GENERATE-ORDER-NUMBER                                     
063725*    -- ORDER NBR FROM W411ORDN                                           
063726     MOVE ORDN-IDORDNR-UT     TO W-IDORDNR                                
063727     MOVE W-IDORDNR           TO 4261-MID-IDORDNR                         
063728                                                                          
063729*    -- INITALIZE SOME FIELDS THAT MUST NOT BE BLANK                      
063730*    MOVE IN-OLIN-IDDISTR TO DIST20-IDDISTR                               
063731*    IF DIST20-EMBALLAGE                                                  
063732*      MOVE YES               TO 4261-MID-FLEMBORD                        
063733*    ELSE                                                                 
063734*      MOVE NOO               TO 4261-MID-FLEMBORD                        
063735*    END-IF                                                               
063736                                                                          
063737*    MOVE NOO                 TO 4261-MID-FLAUTPAC                        
063738*                                4261-MID-FLOVRLEV                        
063739                                                                          
063740*    MOVE ZERO                TO 4261-MID-IDDEPT                          
063741*                                4261-MID-IDGROSS                         
063742                                                                          
063743*    -- MOVE TO MSG-IO-AREA AND ADD TRANSACTION PREFIX                    
063744*    -- OUTSIDE THE MID COPYTEXT                                          
063745     COMPUTE MSG-KVLL = LENGTH OF 4261-MID-W4I26101 + 17                  
063746     MOVE 4261-MID-W4I26101   TO MSG-INDATA-MINUS-1-TRANSKOD              
063747     MOVE 'W4T261X '          TO MSG-KDTRANS-1                            
063748     MOVE '4261'              TO MSG-IDTRANS-1                            
063749     MOVE '1'                 TO MSG-KDMFSFOR-1                           
063750                                                                          
063751*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
063752*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
063753     MOVE 'W4I26101'          TO MSG-KOM-IDCPYTXT                         
063754     MOVE IN-OLIN-IDSYSTEM    TO MSG-KOM-IDSNDNOD(1:4)                    
063755     MOVE IN-OLIN-IDDISTR     TO MSG-KOM-IDSNDNOD(5:4)                    
063756     ADD  1                   TO MSG-KOM-TIKLOCK                          
063757                                                                          
063758     CALL W006KOM USING MSG-PCB                                           
063759                        0693X-PCB                                         
063760                        WDP8-PCB                                          
063761                        MSG-KOM-WMSGKOM                                   
063762                        MSG-IO-AREA                                       
063763                                                                          
063764     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
063765        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
063766          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
063767        DISPLAY  ERROR-TEXT-STR                                           
063768        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
063769     END-IF                                                               
063770                                                                          
063771*    -- COUNT NUMBER OF ORDER HEADS FOR CHECKPOINT LOGIC                  
063772     ADD 1 TO CHKP-CNTR                                                   
063773                                                                          
063774*    -- CLEAR ORDER-LINE AREA BEFORE FIRST LINE IS ADDED                  
063775     MOVE SPACE               TO 4262-MID-W4I26201-CTX                    
063776     .                                                                    
063777     EJECT                                                                
063778 HA-GENERATE-ORDER-NUMBER SECTION.                                        
063779                                                                          
063780     MOVE 'PROF'             TO ORDN-IDSYSTEM                             
063781     MOVE IN-OLIN-IDDISTR    TO ORDN-IDDISTR                              
063782     MOVE IN-OLIN-IDKUNDNR   TO ORDN-IDKUNDNR                             
063783     MOVE ZERO               TO ORDN-IDORDNR-IN                           
063784                                                                          
063785     CALL W411ORDN USING  ORDN-W411ORDN XXKP-PCB ORQL-PCB                 
063786                          PROC-PCB ORQI-PCB DUMMY-PCB                     
063787     .                                                                    
063788                                                                          
063789     EJECT                                                                
063790 I-DISPATCH-PROFORMA-LINES SECTION.                                       
063793                                                                          
063794     COMPUTE MSG-KVLL = LENGTH OF 4262-MID-W4I26201-CTX + 17              
063795     MOVE 4262-MID-W4I26201-CTX TO MSG-INDATA-MINUS-1-TRANSKOD            
063796     MOVE 'W4T262X '          TO MSG-KDTRANS-1                            
063797     MOVE '4262'              TO MSG-IDTRANS-1                            
063798     MOVE '1'                 TO MSG-KDMFSFOR-1                           
063799                                                                          
063800     CALL W006KOM USING MSG-PCB                                           
063801                        0693X-PCB                                         
063802                        WDP8-PCB                                          
063803                        MSG-KOM-WMSGKOM                                   
063804                        MSG-IO-AREA                                       
063805                                                                          
063806     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
063807        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
063808          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
063809        DISPLAY  ERROR-TEXT-STR                                           
063810        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
063811     END-IF                                                               
063812                                                                          
063813*    -- CLEAR ORDER-LINE AREA BEFORE NEXT LINE IS ADDED                   
063814     MOVE SPACE               TO 4262-MID-W4I26201-CTX                    
063815     .                                                                    
063816                                                                          
063817 J-CREATE-PROFORMA-LINE SECTION.                                          
063818                                                                          
063820     MOVE 'W4I26201'          TO MSG-KOM-IDCPYTXT                         
063821                                                                          
063822*    MOVE IN-OLIN-IDSYSTEM    TO 4262-MID-IDSYSTEM                        
063823     MOVE IN-OLIN-IDDISTR     TO 4262-MID-IDDISTR-IN                      
063824     MOVE IN-OLIN-IDKUNDNR    TO 4262-MID-IDKUNDNR-IN                     
063825     MOVE IN-OLIN-IDARTNR     TO 4262-MID-IDARTNR-006(LIX)                
063827     MOVE '-'                 TO 4262-MID-IDARTNR-006(LIX) (10:1)         
063828     MOVE IN-OLIN-REKSIFFR    TO 4262-MID-IDARTNR-006(LIX) (11:1)         
063829*    MOVE IN-OLIN-REKSIFFR    TO 4262-MID-REKSIFFR(LIX)                   
063831     MOVE IN-OLIN-KVBEART     TO 4262-MID-KVBEART(LIX)                    
063832     MOVE IN-OLIN-BERADREF    TO 4262-MID-BERADREF(LIX)                   
063833                                                                          
063834*    -- ORDER NBR FROM W411ORDN                                           
063835     MOVE W-IDORDNR           TO 4262-MID-IDORDNR-IN                      
063836     .                                                                    
063837                                                                          
063838     EJECT                                                                
063839 Z-FINIT SECTION.                                                         
063840                                                                          
063900     CLOSE INPARM W41283                                                  
064000                                                                          
064100     MOVE 'S' TO POSTSUM-OPKOD                                            
064200     CALL POSTSUM USING POSTSUM-PARM                                      
064300                                                                          
064400*--- CLEAR RESTART SEGMENT                                                
064500     PERFORM IMS-GHU-WDGX4202                                             
064600     MOVE ZERO           TO 4202-KVRESTART-W41283                         
064700     MOVE ZERO           TO 4202-KVRESTART-W4128A                         
064800     MOVE ZERO           TO 4202-TIUPPDAT                                 
064900     MOVE ZERO           TO 4202-TIUPPTID                                 
065000     PERFORM IMS-REPL-WDGX4202                                            
065100     .                                                                    
065200                                                                          
065300     EJECT                                                                
065400 S01-READ-W41283  SECTION.                                                
065500                                                                          
065600     READ W41283 INTO IN-AREA                                             
065700     AT END                                                               
065800        MOVE HIGH-VALUE TO IN-AREA                                        
065900        SET END-OF-W41283 TO TRUE                                         
066000                                                                          
066100     NOT AT END                                                           
066200        MOVE 'W41283'   TO POSTSUM-FDNAMN                                 
066300        MOVE 'W41284D1' TO POSTSUM-DDNAMN2                                
066400        MOVE SPACE      TO POSTSUM-TRANSTYP                               
066500        CALL POSTSUM USING POSTSUM-PARM                                   
066600                                                                          
066700        ADD 1 TO W-KVRESTART-W41283                                       
066800     END-READ                                                             
066900     .                                                                    
067000                                                                          
067100 S02-READ-W4128A  SECTION.                                                
067200                                                                          
067300     READ W4128A INTO AMAIL-GSAM-AREA                                     
067400     AT END                                                               
067500        MOVE HIGH-VALUE TO AMAIL-GSAM-AREA                                
067600        SET END-OF-W4128A TO TRUE                                         
067700                                                                          
067800     NOT AT END                                                           
067900        MOVE 'W4128A'   TO POSTSUM-FDNAMN                                 
068000        MOVE 'W41284D2' TO POSTSUM-DDNAMN2                                
068100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
068200        CALL POSTSUM USING POSTSUM-PARM                                   
068300     END-READ                                                             
068400     .                                                                    
068500                                                                          
068600 S03-WRITE-AMAIL-GSAM SECTION.                                            
068700                                                                          
068800     PERFORM IMS-ISRT-AMAIL-GSAM                                          
068900     ADD +1  TO W-KVRESTART-W4128A                                        
069000     .                                                                    
069100                                                                          
069200     EJECT                                                                
069300 X-TAKE-CHECKPOINT   SECTION.                                             
069400                                                                          
069500*--- UPDATE RESTART DB WITH NUMBER OF CURRENTLY READ RECORDS              
069600     PERFORM IMS-GHU-WDGX4202                                             
069700     MOVE W-KVRESTART-W41283         TO 4202-KVRESTART-W41283             
069800     MOVE W-KVRESTART-W4128A         TO 4202-KVRESTART-W4128A             
069900     MOVE FUNCTION CURRENT-DATE(3:6) TO 4202-TIUPPDAT                     
070000     MOVE FUNCTION CURRENT-DATE(9:8) TO 4202-TIUPPTID                     
070100     PERFORM IMS-REPL-WDGX4202                                            
070200                                                                          
070300     PERFORM IMS-CHECKPOINT                                               
070400                                                                          
070500*    -- CLEAR COUNTER FOR CHECK-POINTING                                  
070600     MOVE ZERO TO CHKP-CNTR                                               
070700     .                                                                    
070800                                                                          
070900     EJECT                                                                
071000* --- IMS SECTIONS  -----------------------------------------             
071100                                                                          
071200 IMS-GHU-WDGX4202 SECTION.                                                
071300                                                                          
071400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
071500          DELIMITED BY SIZE INTO SSA1                                     
071600     STRING 'WDGX4202(KDSEGKEY =' W-KDSEGKEY-X ')'                        
071700          DELIMITED BY SIZE INTO SSA2                                     
071800     MOVE '  '   TO GOOD-STATUSCODES                                      
071900     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-WDGX4202                      
072000                        SSA1 SSA2                                         
072100     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
072200     PERFORM IMS-STATUSCHECK                                              
072300     .                                                                    
072400                                                                          
072500 IMS-REPL-WDGX4202 SECTION.                                               
072600                                                                          
072700     MOVE '  ' TO GOOD-STATUSCODES                                        
072800     CALL CBLTDLI USING REPL WDR4-PCB DLI-IO-WDGX4202                     
072900     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
073000     PERFORM IMS-STATUSCHECK                                              
073100     .                                                                    
073200                                                                          
073300     EJECT                                                                
073400 IMS-RESTART SECTION.                                                     
073500                                                                          
073600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
073700     MOVE '  '  TO GOOD-STATUSCODES                                       
073800     CALL CBLTDLI USING XRST MSG-PCB                                      
073900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
074000                        CHKP-AREA-LENGTH CHKP-AREA                        
074100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074200     PERFORM IMS-STATUSCHECK                                              
074300     .                                                                    
074400                                                                          
074500 IMS-CHECKPOINT SECTION.                                                  
074600                                                                          
074700     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
074800     MOVE '  XD' TO GOOD-STATUSCODES                                      
074900     CALL CBLTDLI USING CHKP MSG-PCB                                      
075000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
075100                        CHKP-AREA-LENGTH CHKP-AREA                        
075200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075300     PERFORM IMS-STATUSCHECK                                              
075400                                                                          
075500     IF IMS-NOT-OK                                                        
075600       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
075700       TO ERROR-TEXT-STR                                                  
075800       DISPLAY ERROR-TEXT                                                 
075900       CALL FELLOG                                                        
076000     END-IF                                                               
076100     .                                                                    
076200                                                                          
076300     EJECT                                                                
076400 IMS-ISRT-AMAIL-GSAM SECTION.                                             
076500                                                                          
076600     MOVE 'IMS-ISRT-GSAM' TO SSA1                                         
076700     MOVE '  '  TO GOOD-STATUSCODES                                       
076800     CALL CBLTDLI USING ISRT AMAIL-GSAM-PCB                               
076900                        AMAIL-GSAM-IO-AREA                                
077000     MOVE AMAIL-GSAM-STATUS-CODE TO STATUS-WS                             
077100     PERFORM IMS-STATUSCHECK                                              
077200     .                                                                    
077300     EJECT                                                                
077310 IMS-GHU-WDK711         SECTION.                                          
077320                                                                          
077330     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
077340          DELIMITED BY SIZE INTO SSA1                                     
077350     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
077360          DELIMITED BY SIZE INTO SSA2                                     
077370     MOVE '  GE'                TO GOOD-STATUSCODES                       
077380     CALL CBLTDLI USING GHU                                               
077390                        WDK7-PCB                                          
077391                        IO-AREA-K711                                      
077392                        SSA1                                              
077393                        SSA2                                              
077394     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
077395     PERFORM IMS-STATUSCHECK                                              
077396     .                                                                    
077397 IMS-REPL-WDK711 SECTION.                                                 
077398                                                                          
077399     MOVE '  ' TO GOOD-STATUSCODES                                        
077400     CALL CBLTDLI USING REPL WDK7-PCB IO-AREA-K711                        
077401     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
077402     PERFORM IMS-STATUSCHECK                                              
077403     .                                                                    
077404     EJECT                                                                
077405                                                                          
077406 IMS-GU-WDB201      SECTION.                                              
077407                                                                          
077408     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
077409          DELIMITED BY SIZE INTO SSA1                                     
077410     MOVE '  GE' TO GOOD-STATUSCODES                                      
077411     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
077412     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
077413     PERFORM IMS-STATUSCHECK                                              
077414     .                                                                    
077415     EJECT                                                                
077420 IMS-STATUSCHECK SECTION.                                                 
077500                                                                          
077600     SET STATUS-IX TO 1                                                   
077700     SEARCH GOOD-STATUS                                                   
077800       AT END                                                             
077900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
078000           DELIMITED BY SIZE INTO ERROR-TEXT-STR                          
078100         DISPLAY ERROR-TEXT                                               
078200         CALL FELLOG                                                      
078300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
078400         CONTINUE                                                         
078500     END-SEARCH                                                           
078600     .                                                                    
