000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017500.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   NOVEMBER 2004                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.SHOWINVENTORYQUEUE                          
000800*    WEB-LDC: WL017500 PROGRAM IS A REPLICA OF W5030200 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        INVENTERING                                                      
001300*        PROGRAMMET HAR TRE FUNKTIONER                                    
001400*        - TITTA PÅ INVENTERINGSKÖN                                       
001500*        - VÄLJA VILKA ARTIKLAR MAN VILL HA UT PÅ INVENTERINGS-           
001600*          UNDERLAG                                                       
001700*        - VISAR INTE ARTIKLAR SOM HAR 1,2 ELLER 3 I IDPRTOMG             
001800*          OCH FLINVSKR = N, DESSA VISAS PÅ BILD 5308                     
001900*                                                                         
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION: WL0175T                                             
002400*        REQUEST:     WL0175I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:    WL0175O1                                            
002800*                                                                         
002900*    CHANGE LOG:                                                          
003000*                                                                         
003100*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
003200*      ----------------------------------------------------------         
003300*      15/06/26 - REDDY RAHUL     - PREVENTATIVE CHANGE.                  
003400*                                   E'TRACKER 10260777.                   
003500*                                   FIX THE FOLLOWING:                    
003600*                                   SHOW PARTS IN QUEUE WHEN              
003700*                                   SEARCHED WITH PART NUMBER.            
003800*                                   CORRECT THE KVRADER WHEN THERE        
003900*                                   IS AN ERROR.                          
004000*                                                                         
004100                                                                          
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400 INPUT-OUTPUT SECTION.                                                    
004500 FILE-CONTROL.                                                            
004600                                                                          
004700 DATA DIVISION.                                                           
004800 FILE SECTION.                                                            
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -COPY WY2000W1                                                       
005200 77  IDPGM                       PIC X(08)   VALUE 'WL017500'.            
005300                                                                          
005400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005600 77  KDRC-DISPLAY                PIC Z(5).                                
005700 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
005800 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
005900 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
006000                                                                          
006100 77  YES                         PIC X       VALUE 'J'.                   
006200 77  NOO                         PIC X       VALUE 'N'.                   
006300 77  WS-KVRADER                  PIC S9(9)   VALUE +0.                    
006400 77  IX                        PIC S9(9)  VALUE +0   COMP SYNC.           
006500 77  IND                       PIC S9(9)  VALUE +0   COMP SYNC.           
006600 77  INDX                      PIC S9(9)  VALUE +0   COMP SYNC.           
006700 77  MAX-IX                    PIC S9(9)  VALUE +500 COMP SYNC.           
006800 77  MSG-IX                    PIC S9(9)  VALUE +0   COMP SYNC.           
006900 77  MAX-IX-10                 PIC  9(2)  VALUE 10.                       
007000                                                                          
007100 77  SW-URVAL-OK               PIC  X(1)  VALUE SPACE.                    
007200 77  WS-IDARTNR-SPAR           PIC 9(9).                                  
007300 77  WS-ADLAGOMR-NUM           PIC 9(3).                                  
007400 77  WS-ADGANG-NUM             PIC 9(3).                                  
007500 77  WS-KDINVKAT-NUM           PIC 9(3).                                  
007600                                                                          
007700 77  KEYS-SW                   PIC X      VALUE 'J'.                      
007800     88  KEYS-OK                          VALUE 'J'.                      
007900     88  KEYS-WRONG                       VALUE 'N'.                      
008000                                                                          
008100 77  INDATA-SW                 PIC X      VALUE 'J'.                      
008200      88  INDATA-OK                       VALUE 'J'.                      
008300      88  INDATA-FEL                      VALUE 'N'.                      
008400                                                                          
008500 77  ALLT-OK-SW                PIC X      VALUE 'J'.                      
008600      88  ALLT-OK                         VALUE 'J'.                      
008700      88  ALLT-FEL                        VALUE 'N'.                      
008800                                                                          
008900 01  WS-FIX-DATUM.                                                        
009000     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
009100     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
009200         05 WS-FILLER1-1-2   PIC 9(2).                                    
009300         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
009400         05 WS-FILLER1-9     PIC 9(1).                                    
009500     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
009600         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
009700         05 WS-FILLER2-9     PIC 9(1).                                    
009800                                                                          
009900     EJECT                                                                
010000 01  WS-LISTNR-UT.                                                        
010100     03 WS-LISTNR          PIC 9(6).                                      
010200     03 WS-FILLER REDEFINES WS-LISTNR.                                    
010300       05 WS-IDPRTOMG      PIC 9.                                         
010400       05 WS-IDLOPNR       PIC 9(5).                                      
010500                                                                          
010600 01  WS-DAREGDAT                 PIC X(6).                                
010700                                                                          
010800 01  WS-API-DATE.                                                         
010900     03  FILLER                  PIC X(2)    VALUE '20'.                  
011000     03  WS-API-YEAR             PIC 9(2).                                
011100     03  FILLER                  PIC X(1)    VALUE '-'.                   
011200     03  WS-API-MONTH            PIC 9(2).                                
011300     03  FILLER                  PIC X(1)    VALUE '-'.                   
011400     03  WS-API-DAY              PIC 9(2).                                
011500                                                                          
011600 01  CURRENT-INPUT.                                                       
011700     03   CURRI-DAREGDAT-KEY-X.                                           
011800       05 CURRI-DAREGDAT-KEY      PIC 9(6).                               
011900                                                                          
012000 01  CURRENT-OUTPUT.                                                      
012100     03  CURRO-DAREGDAT-KEY      PIC X(6).                                
012200     03  CURRO-RAD-INFO OCCURS 500 TIMES.                                 
012300        05 CURRO-DAREGDAT-UTSKR  PIC X(6).                                
012400     EJECT                                                                
012500                                                                          
012600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012700 01  GENERAL-SUBPROGRAMS.                                                 
012800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
013200     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
013300     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
013400                                                                          
013500*    --- PARAMETERS TO ABEND                                              
013600                                                                          
013700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014000                                                                          
014100 01  MESSAGE-CODES.                                                       
014200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
014300     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
014400                                                                          
014500*                                                                         
014501*01  -COPY WWDC99                                                         
014510*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014700*01  -COPY WZ01SUB                                                        
014800                                                                          
014900 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
015000*01  -COPY WMSGCONV                                                       
015100                                                                          
015200 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
015300*01  -COPY WZ01AUTH                                                       
015400                                                                          
015500******************************************************************        
015600*****                                                                     
015700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015800*****                                                                     
015900 01  IMS-KEYS.                                                            
016000   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
016100                                                                          
016200 01    NYCKLAR-TILL-DLI.                                                  
016300   03    W-IDARTNR-X.                                                     
016400     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
016500                                                                          
016600   03    W-ARTC-IDARTNR-X.                                                
016700     05    W-IDARTNR-ARTC        PIC S9(9)   VALUE ZERO  COMP-3.          
016800                                                                          
016900   03    W-KDINVPRIO-X.                                                   
017000     05    W-KDINVPRIO           PIC S9      VALUE ZERO  COMP-3.          
017100                                                                          
017200   03    W-WDH111KY-X.                                                    
017300     05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.                 
017400     05    W-KDINVKAT-UNIK       PIC S9(3)   VALUE ZERO COMP-3.           
017500     05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.           
017600     05    W-DAREGDAT-9KOMPL-UNIK PIC 9(8) VALUE ZERO.                    
017700                                                                          
017800   03    W-WDH11-KEY-MIN-X.                                               
017900     05    W-IDDC-MIN        PIC X(2)  VALUE SPACE.                       
018000     05    W-KDINVKAT-MIN    PIC S9(3) VALUE ZERO COMP-3.                 
018100     05    W-TISEGKEY-MIN    PIC S9(9) VALUE ZERO COMP-3.                 
018200     05    W-DAREGDAT-9KOMPL-MIN PIC 9(8) VALUE ZERO.                     
018300                                                                          
018400   03    W-WDH11-KEY-MAX-X.                                               
018500     05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                       
018600     05    W-KDINVKAT-MAX    PIC S9(3) VALUE +049       COMP-3.           
018700     05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.           
018800     05    W-DAREGDAT-9KOMPL-MAX PIC 9(8) VALUE 99999999.                 
018900                                                                          
019000   03    W-WDH111KY-MIN.                                                  
019100     05    W-IDDC-MIN-KY         PIC X(2)    VALUE SPACE.                 
019200     05    W-KDINVKAT-MIN-KY     PIC S9(3)   VALUE ZERO COMP-3.           
019300     05    W-TISEGKEY-MIN-KY     PIC S9(9)   VALUE ZERO COMP-3.           
019400     05    W-DAREGDAT-9KOMPL-MIN-KY PIC 9(8) VALUE ZERO.                  
019500                                                                          
019600   03    W-WDH111KY-MAX.                                                  
019700     05    W-IDDC-MAX-KY         PIC X(2)    VALUE SPACE.                 
019800     05    W-KDINVKAT-MAX-KY     PIC S9(3)   VALUE +999 COMP-3.           
019900     05    W-TISEGKEY-MAX-KY  PIC S9(9) VALUE +999999999 COMP-3.          
020000     05    W-DAREGDAT-9KOMPL-MAX-KY PIC 9(8) VALUE 99999999.              
020100                                                                          
020200   03    W-WDH1A1KY-MIN-X.                                                
020300     05    W-SEQA-IDDC-MIN       PIC X(2)    VALUE SPACE.                 
020400     05    W-SEQA-ADLAGOMR-MIN   PIC S9(3)   VALUE ZERO  COMP-3.          
020500     05    W-SEQA-ADGANG-MIN     PIC S9(3)   VALUE ZERO  COMP-3.          
020600     05    W-SEQA-ADPLATS-MIN    PIC S9(5)   VALUE ZERO  COMP-3.          
020700     05    W-SEQA-KDINVPRIO-MIN  PIC S9      VALUE ZERO  COMP-3.          
020800     05    W-SEQA-KDVVKL-MIN     PIC S9      VALUE ZERO  COMP-3.          
020900     05    W-SEQA-IDARTNR-MIN    PIC S9(9)   VALUE ZERO  COMP-3.          
021000     05    W-SEQA-KDINVKAT-MIN   PIC S9(3)   VALUE ZERO  COMP-3.          
021100     05    W-SEQA-TISEGKEY-MIN   PIC S9(9)   VALUE ZERO  COMP-3.          
021200                                                                          
021300   03    W-WDH1A1KY-MAX-X.                                                
021400     05    W-SEQA-IDDC-MAX      PIC X(2)  VALUE SPACE.                    
021500     05    W-SEQA-ADLAGOMR-MAX  PIC S9(3) VALUE +999       COMP-3.        
021600     05    W-SEQA-ADGANG-MAX    PIC S9(3) VALUE +999       COMP-3.        
021700     05    W-SEQA-ADPLATS-MAX   PIC S9(5) VALUE +99999     COMP-3.        
021800     05    W-SEQA-KDINVPRIO-MAX PIC S9    VALUE +9         COMP-3.        
021900     05    W-SEQA-KDVVKL-MAX    PIC S9    VALUE +9         COMP-3.        
022000     05    W-SEQA-IDARTNR-MAX   PIC S9(9) VALUE +999999999 COMP-3.        
022100     05    W-SEQA-KDINVKAT-MAX  PIC S9(3) VALUE +999       COMP-3.        
022200     05    W-SEQA-TISEGKEY-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
022300                                                                          
022400                                                                          
022500 01  IMS-WS.                                                              
022600   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
022700*****                    **** STATUS-KOD FRÅN IMS                         
022800   03    STATUS-WS       PIC XX.                                          
022900         88  SEGMENT-FOUND       VALUE '  '.                              
023000         88  SEGMENT-MISSING     VALUE 'GE'.                              
023100         88  SEGMENT-EXISTS      VALUE 'II'.                              
023200                                                                          
023300   03    GOOD-STATUSCODES.                                                
023400     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023500                                                                          
023600 01      SSA1            PIC X(256) VALUE SPACE.                          
023700 01      SSA2            PIC X(256) VALUE SPACE.                          
023800                                                                          
023900*                            IMS FUNKTIONSKODER                           
024000*01      -COPY W0003                                                      
024100                                                                          
024200 01    FILLER            PIC X(16)   VALUE 'IO-INVA01'.                   
024300 01    DLI-IO-AREA-INVA01.                                                
024400*  03    WDH101 -COPY WDH101.                                             
024500                                                                          
024600 01    FILLER            PIC X(16)   VALUE 'IO-INVA11'.                   
024700 01    DLI-IO-AREA-INVA11.                                                
024800*  03    WDH111 -COPY WDH111.                                             
024900                                                                          
025000 01    FILLER            PIC X(16)   VALUE 'IO-INVA21'.                   
025100 01    DLI-IO-AREA-INVA21.                                                
025200*  03    WDH121 -COPY WDH121.                                             
025300                                                                          
025400 01    FILLER            PIC X(16)   VALUE 'IO-WDH1A1'.                   
025500 01    DLI-IO-AREA-WDH1A1.                                                
025600*  03    WLINVB01 -COPY WDH1A1.                                           
025700                                                                          
025800 01  FILLER              PIC X(16)   VALUE 'IO-WDH111'.                   
025900 01  DLI-IO-WDH111.                                                       
026000*    03 -COPY WDH111 -PRE INVA-                                           
026100                                                                          
026200                                                                          
026300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
026400 01  REQU-AREA.                                                           
026500*    03  -COPY WZ01REQ2                                                   
026600*    03  -COPY WL0175I1                                                   
026700                                                                          
026800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
026900                                                                          
027000 01  RESP-AREA.                                                           
027100*    03  -COPY WZ01RES2                                                   
027200*    03  -COPY WL0175O1                                                   
027300                                                                          
027400 LINKAGE SECTION.                                                         
027500*01    -COPY W0009     -PRE MSG-                                          
027600 01  ATAB-PCB                    PIC X.                                   
027700                                                                          
027800*01    -COPY W0008     -PRE INVB-                                         
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01    -COPY W0008     -PRE INVA-                                         
028200     05  FILLER                  PIC X.                                   
028300                                                                          
028400 PROCEDURE DIVISION USING MSG-PCB ATAB-PCB INVB-PCB INVA-PCB.             
028500                                                                          
028600 MAIN SECTION.                                                            
028700                                                                          
028800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
028900     IF SUB-KDRC = 0                                                      
029000       PERFORM A-INIT                                                     
029100       IF KEYS-OK                                                         
029200         PERFORM B-CHECK-KEYS                                             
029300         IF KEYS-OK                                                       
029400           IF REQU-UPDATE                                                 
029500             PERFORM C-KOLLA-INDATA-UPPDATERING                           
029600             IF INDATA-OK                                                 
029700               PERFORM D-SKICKA-ART-TILL-UTSKRIFT                         
029800             END-IF                                                       
029900           END-IF                                                         
030000           IF REQU-IDARTNR-KEY NUMERIC AND                                
030100              (REQU-IDARTNR-KEY > ZERO)                                   
030200              PERFORM G-LAES-ARTIKEL                                      
030300              PERFORM F-LAES-VISA-INFO-ART                                
030400           ELSE                                                           
030500              PERFORM E-LAES-VISA-INFO                                    
030600           END-IF                                                         
030700           PERFORM H-DATUM-TILL-RESP                                      
030800         END-IF                                                           
030900       END-IF                                                             
031000                                                                          
031100       IF SUB-KDTRANS(1:6) = 'WLA175'                                     
031200         PERFORM S11-MSG-CONV                                             
031300       END-IF                                                             
031400       PERFORM S02-RETURN-RESPONSE                                        
031500     END-IF                                                               
031600                                                                          
031700     MOVE ZERO TO RETURN-CODE                                             
031800     GOBACK                                                               
031900     .                                                                    
032000     EJECT                                                                
032100 A-INIT SECTION.                                                          
032200     MOVE 'A-INIT' TO CURR-SECTION                                        
032300                                                                          
032400     MOVE YES                    TO KEYS-SW                               
032500                                                                          
032600     MOVE ALL '+'   TO RESP-AREA                                          
032700     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
032800                       RESP-IDMSG-INFO                                    
032900                       RESP-IDELMT-ERROR                                  
033000     MOVE ZERO      TO RESP-KVRADER-MAX                                   
033100     MOVE 001       TO RESP-IDRESVER                                      
033200                                                                          
033300     MOVE ALL '+'   TO CURRENT-OUTPUT                                     
033400                                                                          
033500     IF SUB-KDTRANS(1:7) = 'WL0175T' OR 'WL0175U'                         
033600       MOVE REQU-DAREGDAT-KEY      TO CURRI-DAREGDAT-KEY-X                
033700     ELSE                                                                 
033800       MOVE 001                  TO AUTH-KDCALL                           
033900       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
034000                                    REQU-WZ01REQ2                         
034100       IF AUTH-KDRC > 0                                                   
034200         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
034300         MOVE NOO                TO KEYS-SW                               
034400       END-IF                                                             
034500                                                                          
034600       MOVE REQU-DAREGDAT-KEY(3:2) TO CURRI-DAREGDAT-KEY(1:2)             
034700       MOVE REQU-DAREGDAT-KEY(6:2) TO CURRI-DAREGDAT-KEY(3:2)             
034800       MOVE REQU-DAREGDAT-KEY(9:2) TO CURRI-DAREGDAT-KEY(5:2)             
034900                                                                          
035000       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)     TO                    
035100                                 REQU-KDPGMACT                            
035200       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)     TO                    
035300                                 REQU-IDDC-KEY                            
035400       MOVE FUNCTION UPPER-CASE (REQU-FLINVSKR-KEY) TO                    
035500                                 REQU-FLINVSKR-KEY                        
035600       MOVE FUNCTION UPPER-CASE (REQU-FLBUFDOC)     TO                    
035700                                 REQU-FLBUFDOC                            
035800       MOVE FUNCTION UPPER-CASE (REQU-FLURVAL-IN)   TO                    
035900                                 REQU-FLURVAL-IN                          
036000     END-IF                                                               
036100     .                                                                    
036200                                                                          
036300 B-CHECK-KEYS SECTION.                                                    
036400     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
036500                                                                          
036600     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
036700                           W-SEQA-IDDC-MIN                                
036800                           W-SEQA-IDDC-MAX                                
036900                           W-IDDC-MIN                                     
037000                           W-IDDC-MAX                                     
037010                           WS-IDDC                                        
037100                                                                          
037200     IF REQU-QUERY OR REQU-UPDATE                                         
037300        CONTINUE                                                          
037400     ELSE                                                                 
037500        MOVE '023'              TO RESP-IDMSG-ERROR                       
037600*       WRONG ACTION KEY ***                                              
037700        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
037800        MOVE NOO                TO KEYS-SW                                
037900     END-IF                                                               
038000                                                                          
038100     IF KEYS-OK                                                           
038200        IF REQU-ADLAGOMR-KEY = ALL '+'                                    
038300           MOVE ZERO            TO REQU-ADLAGOMR-KEY                      
038400        ELSE                                                              
038500           IF REQU-ADLAGOMR-KEY NOT NUMERIC                               
038600              MOVE '024'        TO RESP-IDMSG-ERROR                       
038700*             NOT NUMERIC ***                                             
038800              MOVE 'ADLAGOMR'   TO RESP-IDELMT-ERROR                      
038900              MOVE NOO          TO KEYS-SW                                
039000           ELSE                                                           
039100              MOVE REQU-ADLAGOMR-KEY                                      
039200                                TO RESP-ADLAGOMR-KEY                      
039300           END-IF                                                         
039400        END-IF                                                            
039500     END-IF                                                               
039600                                                                          
039700     IF KEYS-OK                                                           
039800        IF REQU-ADGANG-KEY = ALL '+'                                      
039900           MOVE ZERO            TO REQU-ADGANG-KEY                        
040000        ELSE                                                              
040100           IF REQU-ADGANG-KEY NOT NUMERIC                                 
040200              MOVE '024'        TO RESP-IDMSG-ERROR                       
040300*             NOT NUMERIC ***                                             
040400              MOVE 'ADGANG'     TO RESP-IDELMT-ERROR                      
040500              MOVE NOO          TO KEYS-SW                                
040600           ELSE                                                           
040700              MOVE REQU-ADGANG-KEY                                        
040800                                TO RESP-ADGANG-KEY                        
040900           END-IF                                                         
041000        END-IF                                                            
041100     END-IF                                                               
041200                                                                          
041300     IF KEYS-OK                                                           
041400        IF REQU-ADPLATS-KEY = ALL '+'                                     
041500           MOVE ZERO            TO REQU-ADPLATS-KEY                       
041600        ELSE                                                              
041700           IF REQU-ADPLATS-KEY NOT NUMERIC                                
041800              MOVE '024'        TO RESP-IDMSG-ERROR                       
041900*             NOT NUMERIC ***                                             
042000              MOVE 'ADPLATS'    TO RESP-IDELMT-ERROR                      
042100              MOVE NOO          TO KEYS-SW                                
042200           ELSE                                                           
042300              MOVE REQU-ADPLATS-KEY                                       
042400                                TO RESP-ADPLATS-KEY                       
042500           END-IF                                                         
042600        END-IF                                                            
042700     END-IF                                                               
042800                                                                          
042900     IF KEYS-OK                                                           
043000        IF REQU-KDINVPRIO-KEY = ALL '+'                                   
043100           MOVE ZERO            TO REQU-KDINVPRIO-KEY                     
043200        ELSE                                                              
043300           IF REQU-KDINVPRIO-KEY NOT NUMERIC                              
043400              MOVE '024'        TO RESP-IDMSG-ERROR                       
043500*             NOT NUMERIC ***                                             
043600              MOVE 'KDINVPRIO'  TO RESP-IDELMT-ERROR                      
043700              MOVE NOO          TO KEYS-SW                                
043800           ELSE                                                           
043900              IF REQU-KDINVPRIO-KEY = 0 OR 1 OR 2                         
044000                 MOVE REQU-KDINVPRIO-KEY                                  
044100                                TO RESP-KDINVPRIO-KEY                     
044200              ELSE                                                        
044300                 MOVE '023'       TO RESP-IDMSG-ERROR                     
044400*                IS INVALID  ***                                          
044500                 MOVE 'KDINVPRIO' TO RESP-IDELMT-ERROR                    
044600                 MOVE NOO         TO KEYS-SW                              
044700              END-IF                                                      
044800           END-IF                                                         
044900        END-IF                                                            
045000     END-IF                                                               
045100                                                                          
045200     IF KEYS-OK                                                           
045300        IF REQU-KDVVKL-KEY = ALL '+'                                      
045400           MOVE ZERO            TO REQU-KDVVKL-KEY                        
045500        ELSE                                                              
045600           IF REQU-KDVVKL-KEY  NOT NUMERIC                                
045700              MOVE '024'        TO RESP-IDMSG-ERROR                       
045800*             NOT NUMERIC ***                                             
045900              MOVE 'KDVVKL'     TO RESP-IDELMT-ERROR                      
046000              MOVE NOO          TO KEYS-SW                                
046100           ELSE                                                           
046200              MOVE REQU-KDVVKL-KEY                                        
046300                                TO RESP-KDVVKL-KEY                        
046400           END-IF                                                         
046500        END-IF                                                            
046600     END-IF                                                               
046700                                                                          
046800     IF KEYS-OK                                                           
046900        IF REQU-KDINVKAT-KEY = ALL '+'                                    
047000           MOVE ZERO            TO REQU-KDINVKAT-KEY                      
047100        ELSE                                                              
047200           IF REQU-KDINVKAT-KEY  NOT NUMERIC                              
047300              MOVE '024'        TO RESP-IDMSG-ERROR                       
047400*             NOT NUMERIC ***                                             
047500              MOVE 'KDINVKAT'   TO RESP-IDELMT-ERROR                      
047600              MOVE NOO          TO KEYS-SW                                
047700           ELSE                                                           
047800              MOVE REQU-KDINVKAT-KEY                                      
047900                                TO RESP-KDINVKAT-KEY                      
048000           END-IF                                                         
048100        END-IF                                                            
048200     END-IF                                                               
048300                                                                          
048400     IF KEYS-OK                                                           
048500        IF REQU-FLINVSKR-KEY = YES OR NOO                                 
048600           MOVE REQU-FLINVSKR-KEY TO RESP-FLINVSKR-KEY                    
048700        ELSE                                                              
048800           MOVE '185'       TO RESP-IDMSG-ERROR                           
048900*          IS INVALID  ***                                                
049000           MOVE NOO         TO KEYS-SW                                    
049100        END-IF                                                            
049200     END-IF                                                               
049300                                                                          
049400     IF KEYS-OK                                                           
049500        IF REQU-KDPRODSL-KEY = ALL '+'                                    
049600           MOVE ZERO            TO REQU-KDPRODSL-KEY                      
049700        ELSE                                                              
049800           IF REQU-KDPRODSL-KEY  NOT NUMERIC                              
049900              MOVE '024'        TO RESP-IDMSG-ERROR                       
050000*             NOT NUMERIC ***                                             
050100              MOVE 'KDPRODSL'   TO RESP-IDELMT-ERROR                      
050200              MOVE NOO          TO KEYS-SW                                
050300           ELSE                                                           
050400              MOVE REQU-KDPRODSL-KEY                                      
050500                                TO RESP-KDPRODSL-KEY                      
050600           END-IF                                                         
050700        END-IF                                                            
050800     END-IF                                                               
050900                                                                          
051000     IF KEYS-OK                                                           
051100        IF REQU-IDFKNGRP-KEY = ALL '+'                                    
051200           MOVE ZERO            TO REQU-IDFKNGRP-KEY                      
051300        ELSE                                                              
051400           IF REQU-IDFKNGRP-KEY  NOT NUMERIC                              
051500              MOVE '024'        TO RESP-IDMSG-ERROR                       
051600*             NOT NUMERIC ***                                             
051700              MOVE 'IDFKNGRP'   TO RESP-IDELMT-ERROR                      
051800              MOVE NOO          TO KEYS-SW                                
051900           ELSE                                                           
052000              MOVE REQU-IDFKNGRP-KEY                                      
052100                                TO RESP-IDFKNGRP-KEY                      
052200           END-IF                                                         
052300        END-IF                                                            
052400     END-IF                                                               
052500                                                                          
052600     IF KEYS-OK                                                           
052700        IF CURRI-DAREGDAT-KEY = ALL '+'                                   
052800           MOVE ZERO            TO CURRI-DAREGDAT-KEY                     
052900           MOVE SPACE           TO CURRO-DAREGDAT-KEY                     
053000        ELSE                                                              
053100           IF CURRI-DAREGDAT-KEY NOT NUMERIC                              
053200              MOVE '024'        TO RESP-IDMSG-ERROR                       
053300*             NOT NUMERIC ***                                             
053400              MOVE 'DAREGDAT'   TO RESP-IDELMT-ERROR                      
053500              MOVE NOO          TO KEYS-SW                                
053600           ELSE                                                           
053700              MOVE CURRI-DAREGDAT-KEY                                     
053800                                TO CURRO-DAREGDAT-KEY                     
053900           END-IF                                                         
054000        END-IF                                                            
054100     END-IF                                                               
054200                                                                          
054300     IF KEYS-OK                                                           
054400        IF REQU-IDARTNR-KEY  = ALL '+'                                    
054500           MOVE ZERO            TO REQU-IDARTNR-KEY                       
054600        ELSE                                                              
054700           IF REQU-IDARTNR-KEY   NOT NUMERIC                              
054800              MOVE '024'        TO RESP-IDMSG-ERROR                       
054900*             NOT NUMERIC ***                                             
055000              MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                      
055100              MOVE NOO          TO KEYS-SW                                
055200           ELSE                                                           
055300              MOVE REQU-IDARTNR-KEY                                       
055400                                TO RESP-IDARTNR-KEY                       
055500           END-IF                                                         
055600        END-IF                                                            
055700     END-IF                                                               
055800                                                                          
055900     IF REQU-UPDATE                                                       
056000        IF KEYS-OK                                                        
056100           IF REQU-FLBUFDOC   = YES OR NOO                                
056200              MOVE REQU-FLBUFDOC     TO RESP-FLBUFDOC                     
056300           ELSE                                                           
056400              MOVE '282'       TO RESP-IDMSG-ERROR                        
056500*             IS INVALID  ***                                             
056600              MOVE NOO         TO KEYS-SW                                 
056700           END-IF                                                         
056800        END-IF                                                            
056900                                                                          
057000        IF KEYS-OK                                                        
057100           IF REQU-FLURVAL-IN   = YES OR NOO                              
057200              MOVE REQU-FLURVAL-IN   TO RESP-FLURVAL-IN                   
057300           ELSE                                                           
057400              MOVE '186'       TO RESP-IDMSG-ERROR                        
057500*             IS INVALID  ***                                             
057600              MOVE NOO         TO KEYS-SW                                 
057700           END-IF                                                         
057800        END-IF                                                            
057900                                                                          
058000        IF REQU-KVRADER-MAX = ALL '+'                                     
058100           MOVE '026'           TO RESP-IDMSG-ERROR                       
058200*          NO INPUT DATA IS ENTERED ***                                   
058300           MOVE 'KVRADER'       TO RESP-IDELMT-ERROR                      
058400           MOVE NOO             TO KEYS-SW                                
058500        ELSE                                                              
058600           IF REQU-KVRADER-MAX NOT NUMERIC                                
058700              MOVE '024'        TO RESP-IDMSG-ERROR                       
058800*             NOT NUMERIC ***                                             
058900              MOVE 'KVRADER'    TO RESP-IDELMT-ERROR                      
059000              MOVE NOO          TO KEYS-SW                                
059100           ELSE                                                           
059200              IF REQU-KVRADER-MAX > 0                                     
059300                 CONTINUE                                                 
059400              ELSE                                                        
059500                 MOVE '023'       TO RESP-IDMSG-ERROR                     
059600*                IS INVALID  ***                                          
059700                 MOVE 'KVRADER'   TO RESP-IDELMT-ERROR                    
059800                 MOVE NOO         TO KEYS-SW                              
059900              END-IF                                                      
060000           END-IF                                                         
060100        END-IF                                                            
060200     END-IF                                                               
060300                                                                          
060400     .                                                                    
060500 C-KOLLA-INDATA-UPPDATERING SECTION.                                      
060600     MOVE 'C-KOLLA-INDATA' TO CURR-SECTION                                
060700                                                                          
060800     MOVE YES TO INDATA-SW                                                
060900*    IF REQU-KVINVSKR-5302 = ALL '+' OR SPACE                             
061000*      MOVE REQU-KVRADER-MAX TO REQU-KVINVSKR-5302                        
061100*    END-IF                                                               
061200     IF REQU-KVINVSKR-5302 NOT NUMERIC                                    
061300        MOVE '024'        TO RESP-IDMSG-ERROR                             
061400*       NOT NUMERIC ***                                                   
061500        MOVE 'KVINVSKR'   TO RESP-IDELMT-ERROR                            
061600        MOVE NOO          TO INDATA-SW                                    
061700     ELSE                                                                 
061800        IF REQU-KVINVSKR-5302 > ZERO AND < 11                             
061900           CONTINUE                                                       
062000        ELSE                                                              
062100           MOVE '023'       TO RESP-IDMSG-ERROR                           
062200*          IS INVALID  ***                                                
062300           MOVE 'KVINVSKR'  TO RESP-IDELMT-ERROR                          
062400           MOVE NOO         TO INDATA-SW                                  
062500        END-IF                                                            
062600     END-IF                                                               
062700                                                                          
062800     IF INDATA-OK                                                         
062900        IF REQU-KVINVSKR-5302 > REQU-KVRADER-MAX                          
063000           MOVE '023'       TO RESP-IDMSG-ERROR                           
063100*          IS INVALID  ***                                                
063200           MOVE NOO         TO INDATA-SW                                  
063300           MOVE 'KVINVSKR'  TO RESP-IDELMT-ERROR                          
063400        END-IF                                                            
063500     END-IF                                                               
063600     .                                                                    
063700                                                                          
063800 D-SKICKA-ART-TILL-UTSKRIFT SECTION.                                      
063900     MOVE 'D-SKICKA-ART  ' TO CURR-SECTION                                
064000                                                                          
064100     PERFORM DA-INITIERA-ALT-RESP-AREA                                    
064200     MOVE +1 TO IX                                                        
064300     MOVE +0 TO IND                                                       
064400                                                                          
064500     IF REQU-IDARTNR-KEY > ZERO                                           
064600        MOVE +1        TO IND                                             
064700        PERFORM DB-REDIGERA-FLYTTA-ART                                    
064800*       *** PRINT REQUESTED ***                                           
064900                                                                          
065000     ELSE                                                                 
065100        PERFORM UNTIL (IND = REQU-KVINVSKR-5302) OR                       
065200                      (IX > MAX-IX)              OR                       
065300                      (REQU-IDARTNR-UTSKR(IX) NOT NUMERIC)                
065400           PERFORM DC-KOLLA-PRINTKOD-WDH1                                 
065500                                                                          
065600           IF INV-FLINVSKR = 'N'                                          
065700              ADD +1 TO IND                                               
065800              PERFORM DB-REDIGERA-FLYTTA-ART                              
065900           END-IF                                                         
066000           ADD +1 TO IX                                                   
066100        END-PERFORM                                                       
066200                                                                          
066300        IF IND = ZERO                                                     
066400           MOVE '250'     TO RESP-IDMSG-ERROR                             
066500                                                                          
066600*          *** NO PRINTING     ***                                        
066700        END-IF                                                            
066800     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100 DA-INITIERA-ALT-RESP-AREA SECTION.                                       
067200     MOVE 'DA-INITIERA-ALT' TO CURR-SECTION                               
067300                                                                          
067400     MOVE +1              TO IX                                           
067500     PERFORM 10 TIMES                                                     
067600        MOVE ALL '+'      TO RESP-WL0173I1(IX)                            
067700                             RESP-WL0174I1(IX)                            
067800        ADD +1            TO IX                                           
067900     END-PERFORM                                                          
068000     .                                                                    
068100                                                                          
068200     EJECT                                                                
068300 DB-REDIGERA-FLYTTA-ART SECTION.                                          
068400     MOVE 'DB-REDIGERA-FLYTTA' TO CURR-SECTION                            
068500                                                                          
068600     MOVE REQU-IDARTNR-UTSKR(IX)     TO W-IDARTNR                         
068700                                        RESP-IDARTNR-L173(IND)            
068800                                        RESP-IDARTNR-L174(IND)            
068900                                                                          
069000     IF REQU-KDINVPRIO-UTSKR(IX) = SPACE                                  
069100        MOVE '2'                      TO RESP-KDINVPRIO-L173(IND)         
069200                                         RESP-KDINVPRIO-L174(IND)         
069300     ELSE                                                                 
069400        MOVE REQU-KDINVPRIO-UTSKR(IX) TO RESP-KDINVPRIO-L173(IND)         
069500                                         RESP-KDINVPRIO-L174(IND)         
069600     END-IF                                                               
069700     MOVE REQU-KDINVKAT-UTSKR(IX)     TO RESP-KDINVKAT-L173(IND)          
069800                                         RESP-KDINVKAT-L174(IND)          
069900     .                                                                    
070000                                                                          
070100 DC-KOLLA-PRINTKOD-WDH1 SECTION.                                          
070200     MOVE 'DC-KOLLA-PRINTKOD ' TO CURR-SECTION                            
070300                                                                          
070400     MOVE REQU-IDARTNR-UTSKR(IX)     TO W-IDARTNR                         
070500                                                                          
070600     MOVE REQU-KDINVKAT-UTSKR(IX)     TO W-KDINVKAT-MIN                   
070700                                         W-KDINVKAT-MAX                   
070800     PERFORM IMS-01-GET-INVENTERINGS-ROT                                  
070900     IF SEGMENT-FOUND                                                     
071000       PERFORM IMS-02-GNP-INVENTERING                                     
071100     END-IF                                                               
071200     .                                                                    
071300                                                                          
071400 E-LAES-VISA-INFO SECTION.                                                
071500     MOVE 'E-LAES-VISA-INFO  ' TO CURR-SECTION                            
071600*                                                                         
071700     MOVE +1 TO INDX                                                      
071800                                                                          
071900     PERFORM IMS-03-GET-WDH1-WITH-SEC-FIRST                               
072000     IF SEGMENT-MISSING                                                   
072100        MOVE '221'              TO RESP-IDMSG-ERROR                       
072200*       NOT FOUND        ***                                              
072300        MOVE NOO                TO KEYS-SW                                
072400     ELSE                                                                 
072500        PERFORM UNTIL INDX > MAX-IX OR SEGMENT-MISSING                    
072600           IF  (REQU-ADLAGOMR-KEY = ZERO OR                               
072700                REQU-ADLAGOMR-KEY = SEQA-ADLAGOMR)                        
072800           AND (REQU-ADGANG-KEY = SEQA-ADGANG OR                          
072900                REQU-ADGANG-KEY  = ZERO)                                  
073000           AND (REQU-ADPLATS-KEY = SEQA-ADPLATS OR                        
073100                REQU-ADPLATS-KEY  = ZERO)                                 
073200           AND (REQU-KDINVPRIO-KEY = SEQA-KDINVPRIO OR                    
073300                REQU-KDINVPRIO-KEY = ZERO)                                
073400           AND (REQU-KDVVKL-KEY = SEQA-KDVVKL OR                          
073500                REQU-KDVVKL-KEY = ZERO)                                   
073600           AND (REQU-KDINVKAT-KEY = SEQA-KDINVKAT OR                      
073700                REQU-KDINVKAT-KEY = ZERO AND                              
073800                SEQA-KDINVKAT NOT = 6)                                    
073900           AND (REQU-FLINVSKR-KEY = SEQA-FLINVSKR OR                      
074000                REQU-FLINVSKR-KEY = '0')                                  
074100           AND (REQU-KDPRODSL-KEY = SEQA-KDPRODSL OR                      
074200                REQU-KDPRODSL-KEY = ZERO)                                 
074300           AND (REQU-IDFKNGRP-KEY = SEQA-IDFKNGRP OR                      
074400                REQU-IDFKNGRP-KEY = ZERO)                                 
074500                                                                          
074600              MOVE SEQA-TISEGKEY     TO WS-FIX-TISEGKEY                   
074700              MOVE CURRI-DAREGDAT-KEY TO TMP1-YYMMDD                      
074800              MOVE WS-TISEGKEY-3-8   TO TMP2-YYMMDD                       
074900                                                                          
075000              PERFORM WY2000P1                                            
075100              IF TMP1-YYMMDD >= TMP2-YYMMDD                               
075200              OR CURRI-DAREGDAT-KEY = ZERO                                
075300                 IF SEQA-FLINVBEH = NOO AND                               
075400                    SEQA-KDINVKAT NOT = +8                                
075500                    PERFORM EA-KONTROLLERA-IDPRTINV                       
075600                    MOVE SEQA-IDARTNR TO W-IDARTNR-ARTC                   
075700                    IF ALLT-OK                                            
075800                        PERFORM EB-SKRIV-RAD                              
075900                        ADD +1 TO WS-KVRADER                              
076000                        ADD +1 TO INDX                                    
076100                    END-IF                                                
076200                 END-IF                                                   
076300              END-IF                                                      
076400           END-IF                                                         
076500           PERFORM IMS-05-GET-WDH1-WITH-SEC-INDEX                         
076600        END-PERFORM                                                       
076700     END-IF                                                               
076800                                                                          
076900     MOVE ZERO       TO RESP-KVINVSKR-5302                                
077000     MOVE 'N'        TO RESP-FLURVAL-IN                                   
077100****                    RESP-FLBUFDOC                                     
077200     IF REQU-QUERY                                                        
077300       MOVE 'N'      TO RESP-FLBUFDOC                                     
077400     END-IF                                                               
077500     MOVE WS-KVRADER TO RESP-KVRADER-MAX                                  
077600                        RESP-KVANT-ART                                    
077700                                                                          
077800     IF SEGMENT-FOUND AND INDX > MAX-IX                                   
077900        MOVE '028'   TO RESP-IDMSG-INFO                                   
078000     END-IF                                                               
078100     IF WS-KVRADER = ZERO                                                 
078200        MOVE '027'   TO RESP-IDMSG-INFO                                   
078300     END-IF                                                               
078400     .                                                                    
078500 EA-KONTROLLERA-IDPRTINV SECTION.                                         
078600     MOVE 'EA-KONTROLLERA    ' TO CURR-SECTION                            
078700                                                                          
078800     MOVE YES TO ALLT-OK-SW                                               
078900     MOVE SEQA-IDARTNR    TO W-IDARTNR                                    
079000     MOVE SEQA-IDDC       TO W-IDDC-MIN-KY                                
079100                             W-IDDC-MAX-KY                                
079200     MOVE SEQA-KDINVKAT   TO W-KDINVKAT-MIN-KY                            
079300                             W-KDINVKAT-MAX-KY                            
079400     MOVE SEQA-TISEGKEY   TO W-TISEGKEY-MIN-KY                            
079500                             W-TISEGKEY-MAX-KY                            
079600     PERFORM IMS-01-GET-INVENTERINGS-ROT                                  
079700     IF SEGMENT-FOUND                                                     
079800       PERFORM IMS-GN-WDH111                                              
079900       IF SEGMENT-FOUND                                                   
080000         IF SUB-KDTRANS(1:6) = 'WLA175' AND REQU-QUERY                    
080100          IF INVA-INV-IDPRTOMG > 0 AND INVA-INV-FLINVSKR = NOO            
080200            MOVE NOO TO ALLT-OK-SW                                        
080300          ELSE                                                            
080400            IF REQU-FLINVSKR-KEY = NOO                                    
080500              IF INVA-INV-IDPRTOMG = 0                                    
080600                CONTINUE                                                  
080700              ELSE                                                        
080800                MOVE NOO TO ALLT-OK-SW                                    
080900              END-IF                                                      
081000            ELSE                                                          
081100              IF INVA-INV-IDPRTOMG = 1                                    
081200                CONTINUE                                                  
081300              ELSE                                                        
081400                MOVE NOO TO ALLT-OK-SW                                    
081500              END-IF                                                      
081600            END-IF                                                        
081700          END-IF                                                          
081800         ELSE                                                             
081900          IF INVA-INV-IDPRTOMG > 0 AND INVA-INV-FLINVSKR = NOO            
082000             MOVE NOO TO ALLT-OK-SW                                       
082100          END-IF                                                          
082200         END-IF                                                           
082300       ELSE                                                               
082400          MOVE NOO TO ALLT-OK-SW                                          
082500       END-IF                                                             
082600     ELSE                                                                 
082700        MOVE NOO TO ALLT-OK-SW                                            
082800     END-IF                                                               
082900     .                                                                    
083000                                                                          
083100 EB-SKRIV-RAD SECTION.                                                    
083200     MOVE 'EB-SKRIV-RAD      ' TO CURR-SECTION                            
083300                                                                          
083400     MOVE SEQA-ADLAGOMR        TO WS-ADLAGOMR-NUM                         
083500     MOVE WS-ADLAGOMR-NUM(2:2) TO RESP-ADLAGOMR-UTSKR  (INDX)             
083600     MOVE SEQA-ADGANG          TO WS-ADGANG-NUM                           
083700     MOVE WS-ADGANG-NUM(2:2)   TO RESP-ADGANG-UTSKR    (INDX)             
083800     MOVE SEQA-ADPLATS    TO RESP-ADPLATS-UTSKR   (INDX)                  
083810     IF CDC-SE                                                            
083820       IF SEQA-KDINVPRIO = +2                                             
083830         MOVE +0             TO RESP-KDINVPRIO-UTSKR (INDX)               
083840       ELSE                                                               
083850         MOVE SEQA-KDINVPRIO TO RESP-KDINVPRIO-UTSKR (INDX)               
083860       END-IF                                                             
083870     ELSE                                                                 
083880        MOVE SEQA-KDINVPRIO  TO RESP-KDINVPRIO-UTSKR (INDX)               
083890     END-IF                                                               
084000     MOVE SEQA-KDVVKL     TO RESP-KDVVKL-UTSKR    (INDX)                  
084100     MOVE SEQA-KDINVKAT   TO RESP-KDINVKAT-UTSKR  (INDX)                  
084200                                                                          
084300     IF SEQA-FLINVSKR = 'J'                                               
084400       MOVE 'Y'           TO RESP-FLINVSKR-UTSKR  (INDX)                  
084500     ELSE                                                                 
084600       MOVE SEQA-FLINVSKR TO RESP-FLINVSKR-UTSKR  (INDX)                  
084700     END-IF                                                               
084800                                                                          
084900     MOVE SEQA-KDPRODSL   TO RESP-KDPRODSL-UTSKR  (INDX)                  
085000     MOVE SEQA-IDFKNGRP   TO RESP-IDFKNGRP-UTSKR  (INDX)                  
085100     MOVE SEQA-TISEGKEY   TO WS-FIX-TISEGKEY                              
085200     MOVE WS-TISEGKEY-3-8 TO CURRO-DAREGDAT-UTSKR (INDX)                  
085300                                                                          
085400     IF CURRO-DAREGDAT-UTSKR (INDX) = '000000'                            
085500       MOVE SPACE         TO CURRO-DAREGDAT-UTSKR (INDX)                  
085600     END-IF                                                               
085700     MOVE SEQA-IDARTNR    TO RESP-IDARTNR-UTSKR   (INDX)                  
085800                                                                          
085900     MOVE INVA-INV-IDPRTOMG TO WS-IDPRTOMG                                
086000     MOVE INVA-INV-IDLOPNR  TO WS-IDLOPNR                                 
086100     MOVE WS-LISTNR         TO RESP-LISTNR-UTSKR (INDX)                   
086200                                                                          
086300     .                                                                    
086400 F-LAES-VISA-INFO-ART SECTION.                                            
086500*                                                                         
086600     MOVE YES                 TO SW-URVAL-OK                              
086700     IF SEGMENT-FOUND                                                     
086800       MOVE WS-IDARTNR-SPAR   TO W-IDARTNR-ARTC                           
086900     END-IF                                                               
087000                                                                          
087100     MOVE +1 TO INDX                                                      
087200                                                                          
087300     IF SEGMENT-FOUND AND (INV-FLINVBEH = NOO)                            
087400     AND (SW-URVAL-OK = YES)                                              
087500       MOVE 'GE'              TO STATUS-WS                                
087600       PERFORM FA-SKRIV-RAD                                               
087700     ELSE                                                                 
087800       MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                        
087900       MOVE '041'             TO RESP-IDMSG-ERROR                         
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300                                                                          
088400 FA-SKRIV-RAD SECTION.                                                    
088500     MOVE INV-ADLAGOMR         TO WS-ADLAGOMR-NUM                         
088600     MOVE WS-ADLAGOMR-NUM(2:2) TO RESP-ADLAGOMR-UTSKR  (INDX)             
088700     MOVE INV-ADGANG           TO WS-ADGANG-NUM                           
088800     MOVE WS-ADGANG-NUM(2:2)   TO RESP-ADGANG-UTSKR   (INDX)              
088900     MOVE INV-ADPLATS     TO RESP-ADPLATS-UTSKR  (INDX)                   
088910     IF CDC-SE                                                            
088920       IF INV-KDINVPRIO = +2                                              
088930         MOVE +0            TO RESP-KDINVPRIO-UTSKR (INDX)                
088940       ELSE                                                               
088950         MOVE INV-KDINVPRIO TO RESP-KDINVPRIO-UTSKR (INDX)                
088960       END-IF                                                             
088970     ELSE                                                                 
088980       MOVE INV-KDINVPRIO   TO RESP-KDINVPRIO-UTSKR (INDX)                
088990     END-IF                                                               
089100     MOVE INV-KDVVKL      TO RESP-KDVVKL-UTSKR   (INDX)                   
089200     MOVE INV-KDINVKAT    TO WS-KDINVKAT-NUM                              
089300     MOVE WS-KDINVKAT-NUM TO RESP-KDINVKAT-UTSKR  (INDX)                  
089400     IF INV-FLINVSKR = 'J'                                                
089500       MOVE 'Y'           TO RESP-FLINVSKR-UTSKR  (INDX)                  
089600     ELSE                                                                 
089700       MOVE INV-FLINVSKR  TO RESP-FLINVSKR-UTSKR  (INDX)                  
089800     END-IF                                                               
089900     MOVE INV-KDPRODSL    TO RESP-KDPRODSL-UTSKR   (INDX)                 
090000     MOVE INV-IDFKNGRP    TO RESP-IDFKNGRP-UTSKR   (INDX)                 
090100     MOVE INV-TISEGKEY    TO WS-FIX-TISEGKEY                              
090200     MOVE WS-TISEGKEY-3-8 TO CURRO-DAREGDAT-UTSKR (INDX)                  
090300     IF CURRO-DAREGDAT-UTSKR (INDX) = '000000'                            
090400       MOVE SPACE         TO CURRO-DAREGDAT-UTSKR (INDX)                  
090500     END-IF                                                               
090600     MOVE WS-IDARTNR-SPAR TO RESP-IDARTNR-UTSKR  (INDX)                   
090700                                                                          
090800     MOVE INV-IDPRTOMG    TO WS-IDPRTOMG                                  
090900     MOVE INV-IDLOPNR     TO WS-IDLOPNR                                   
091000     MOVE WS-LISTNR         TO RESP-LISTNR-UTSKR (INDX)                   
091100                                                                          
091200     MOVE 1               TO RESP-KVRADER-MAX                             
091300                             RESP-KVANT-ART                               
091400                                                                          
091500     MOVE ZERO            TO RESP-KVINVSKR-5302                           
091600     MOVE 'N'             TO RESP-FLURVAL-IN                              
091700                                                                          
091800     IF REQU-QUERY                                                        
091900       MOVE 'N'           TO RESP-FLBUFDOC                                
092000     END-IF                                                               
092100                                                                          
092200     .                                                                    
092300     EJECT                                                                
092400                                                                          
092500 G-LAES-ARTIKEL SECTION.                                                  
092600     MOVE REQU-IDARTNR-KEY      TO W-IDARTNR                              
092700     PERFORM IMS-01-GET-INVENTERINGS-ROT                                  
092800     IF SEGMENT-FOUND                                                     
092900       MOVE ART-IDARTNR          TO WS-IDARTNR-SPAR                       
093000       IF REQU-KDINVPRIO-KEY = ZERO                                       
093100          PERFORM IMS-02-GNP-INVENTERING                                  
093200          IF SEGMENT-FOUND                                                
093300            IF SUB-KDTRANS(1:6) = 'WLA175' AND REQU-QUERY                 
093400             IF REQU-FLINVSKR-KEY = NOO                                   
093500               IF (INV-KDINVPRIO = +1 OR +2) AND                          
093600                  (INV-KDINVKAT NOT = 6) AND                              
093700                  INV-IDPRTOMG = ZERO                                     
093800                  CONTINUE                                                
093900               ELSE                                                       
094000                  MOVE 'GE'        TO STATUS-WS                           
094100               END-IF                                                     
094200             ELSE                                                         
094300               IF (INV-KDINVPRIO = +1 OR +2) AND                          
094400                  (INV-KDINVKAT NOT = 6) AND                              
094500                  INV-IDPRTOMG = 1                                        
094600                  CONTINUE                                                
094700               ELSE                                                       
094800                  MOVE 'GE'        TO STATUS-WS                           
094900               END-IF                                                     
095000             END-IF                                                       
095100            ELSE                                                          
095200             IF (INV-KDINVPRIO = +1 OR +2) AND                            
095300                (INV-KDINVKAT NOT = 6)                                    
095400                CONTINUE                                                  
095500             ELSE                                                         
095600                MOVE 'GE'        TO STATUS-WS                             
095700             END-IF                                                       
095800            END-IF                                                        
095900          END-IF                                                          
096000       ELSE                                                               
096100         MOVE REQU-KDINVPRIO-KEY TO W-KDINVPRIO                           
096200         PERFORM IMS-06-GNP-INVENTERINGS-PRIO                             
096300         IF SUB-KDTRANS(1:6) = 'WLA175' AND REQU-QUERY                    
096400           IF REQU-FLINVSKR-KEY = NOO                                     
096500             IF INV-IDPRTOMG = ZERO                                       
096600               CONTINUE                                                   
096700             ELSE                                                         
096800               MOVE 'GE'        TO STATUS-WS                              
096900             END-IF                                                       
097000           ELSE                                                           
097100             IF INV-IDPRTOMG = 1                                          
097200               CONTINUE                                                   
097300             ELSE                                                         
097400               MOVE 'GE'        TO STATUS-WS                              
097500             END-IF                                                       
097600           END-IF                                                         
097700         END-IF                                                           
097800       END-IF                                                             
097900     ELSE                                                                 
098000       MOVE '223'                TO RESP-IDMSG-ERROR                      
098100     END-IF                                                               
098200                                                                          
098300     IF SEGMENT-FOUND                                                     
098400       MOVE YES                  TO SW-URVAL-OK                           
098500       MOVE W-IDARTNR            TO W-IDARTNR-ARTC                        
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900                                                                          
099000 H-DATUM-TILL-RESP SECTION.                                               
099100                                                                          
099200     IF SUB-KDTRANS(1:7) = 'WL0175T' OR 'WL0175U'                         
099300       MOVE CURRO-DAREGDAT-KEY        TO RESP-DAREGDAT-KEY                
099400                                                                          
099500       MOVE +1 TO INDX                                                    
099600       PERFORM UNTIL INDX > MAX-IX                                        
099700         MOVE CURRO-DAREGDAT-UTSKR (INDX) TO                              
099800              RESP-DAREGDAT-UTSKR  (INDX)                                 
099900         ADD +1 TO INDX                                                   
100000       END-PERFORM                                                        
100100     ELSE                                                                 
100200       IF CURRO-DAREGDAT-KEY NOT NUMERIC                                  
100300         MOVE CURRO-DAREGDAT-KEY      TO RESP-DAREGDAT-KEY                
100400       ELSE                                                               
100500         MOVE CURRO-DAREGDAT-KEY(1:2) TO WS-API-YEAR                      
100600         MOVE CURRO-DAREGDAT-KEY(3:2) TO WS-API-MONTH                     
100700         MOVE CURRO-DAREGDAT-KEY(5:2) TO WS-API-DAY                       
100800         MOVE WS-API-DATE             TO RESP-DAREGDAT-KEY                
100900       END-IF                                                             
101000                                                                          
101100       MOVE +1 TO INDX                                                    
101200       PERFORM UNTIL INDX > MAX-IX                                        
101300         IF CURRO-DAREGDAT-UTSKR(INDX) NOT NUMERIC                        
101400           MOVE CURRO-DAREGDAT-UTSKR(INDX) TO                             
101500                RESP-DAREGDAT-UTSKR (INDX)                                
101600         ELSE                                                             
101700           MOVE CURRO-DAREGDAT-UTSKR(INDX) TO WS-DAREGDAT                 
101800           MOVE WS-DAREGDAT(1:2) TO WS-API-YEAR                           
101900           MOVE WS-DAREGDAT(3:2) TO WS-API-MONTH                          
102000           MOVE WS-DAREGDAT(5:2) TO WS-API-DAY                            
102100           MOVE WS-API-DATE      TO RESP-DAREGDAT-UTSKR (INDX)            
102200         END-IF                                                           
102300         ADD +1 TO INDX                                                   
102400       END-PERFORM                                                        
102500     END-IF                                                               
102600     .                                                                    
102700     EJECT                                                                
102800*    --- DISPATCHER SECTIONS                                              
102900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
103000                                                                          
103100     MOVE 'GETARG'               TO SUB-KDFUNC                            
103200     MOVE 'CARPARTS.LDC.SHOWINVENTORYQUEUE' TO SUB-ADDISPABS              
103300                                                                          
103400*    MOVE MAX-IX (500) TO REQU-KVRADER-MAX SO THAT THE                    
103500*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
103600*    POSSIBLE INPUT                                                       
103700     MOVE MAX-IX                 TO REQU-KVRADER-MAX                      
103800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
103900                                                                          
104000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
104100                                                                          
104200     IF SUB-KDRC > 0                                                      
104300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
104400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
104500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
104600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
104700     END-IF                                                               
104800     .                                                                    
104900                                                                          
105000 S02-RETURN-RESPONSE SECTION.                                             
105100                                                                          
105200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
105300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
105400                                                                          
105500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
105600                                                                          
105700     IF SUB-KDRC > 0                                                      
105800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
105900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
106000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
106100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
106200     END-IF                                                               
106300     .                                                                    
106400                                                                          
106500 S11-MSG-CONV SECTION.                                                    
106600     MOVE SPACES                  TO RESP-MESSAGES (1)                    
106700                                     RESP-MESSAGES (2)                    
106800     MOVE 1                       TO MSG-IX                               
106900*    REQUEST OK                                                           
107000     MOVE 200                     TO RESP-KDSTATUS-API                    
107100     IF RESP-IDMSG-INFO > SPACE                                           
107200       MOVE SPACES                TO MSG-CONV-AREA                        
107300       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
107400       CALL WMSGCONV           USING MSG-CONV-AREA                        
107500       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
107600       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
107700       ADD 1                      TO MSG-IX                               
107800     END-IF                                                               
107900     IF RESP-IDMSG-ERROR > SPACE                                          
108000*      BAD REQUEST                                                        
108100       MOVE 400                   TO RESP-KDSTATUS-API                    
108200       MOVE SPACES                TO MSG-CONV-AREA                        
108300       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
108400       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
108500       CALL WMSGCONV           USING MSG-CONV-AREA                        
108600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
108700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
108800     END-IF                                                               
108900     .                                                                    
109000                                                                          
109100 IMS-01-GET-INVENTERINGS-ROT  SECTION.                                    
109200     MOVE 'IMS-01'  TO CURR-IMS-SECTION                                   
109300                                                                          
109400     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
109500              DELIMITED BY SIZE INTO SSA1                                 
109600     MOVE '  GE'                  TO GOOD-STATUSCODES                     
109700     CALL CBLTDLI USING GU INVA-PCB DLI-IO-AREA-INVA01 SSA1               
109800     MOVE INVA-STATUS-CODE        TO STATUS-WS                            
109900     PERFORM IMS-STATUS-CHECK                                             
110000     .                                                                    
110100 IMS-02-GNP-INVENTERING  SECTION.                                         
110200     MOVE 'IMS-02'  TO CURR-IMS-SECTION                                   
110300                                                                          
110400     STRING 'WDH111  (WDH111KY>=' W-WDH11-KEY-MIN-X                       
110500                    '&WDH111KY<=' W-WDH11-KEY-MAX-X                       
110600                    '&FLINVBEH =' NOO ')'                                 
110700              DELIMITED BY SIZE INTO SSA1                                 
110800     MOVE '  GE'                  TO GOOD-STATUSCODES                     
110900     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA-INVA11 SSA1              
111000     MOVE INVA-STATUS-CODE        TO STATUS-WS                            
111100     PERFORM IMS-STATUS-CHECK                                             
111200     .                                                                    
111300 IMS-06-GNP-INVENTERINGS-PRIO SECTION.                                    
111400     STRING 'WDH111  (WDH111KY>=' W-WDH11-KEY-MIN-X                       
111500                    '&WDH111KY<=' W-WDH11-KEY-MAX-X                       
111600                    '&KDINVPRI =' W-KDINVPRIO-X                           
111700                    '&FLINVBEH =' NOO ')'                                 
111800              DELIMITED BY SIZE INTO SSA1                                 
111900     MOVE '  GE' TO GOOD-STATUSCODES                                      
112000     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA-INVA11 SSA1              
112100     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
112200     PERFORM IMS-STATUS-CHECK                                             
112300     .                                                                    
112400     EJECT                                                                
112500 IMS-03-GET-WDH1-WITH-SEC-FIRST  SECTION.                                 
112600     MOVE 'IMS-03'  TO CURR-IMS-SECTION                                   
112700                                                                          
112800     STRING 'WLINVB01*F(WDH1A1KY>=' W-WDH1A1KY-MIN-X                      
112900                      '&WDH1A1KY<=' W-WDH1A1KY-MAX-X ')'                  
113000              DELIMITED BY SIZE INTO SSA1                                 
113100     MOVE '  GEGB'                TO GOOD-STATUSCODES                     
113200     CALL CBLTDLI USING GU INVB-PCB DLI-IO-AREA-WDH1A1 SSA1               
113300     MOVE INVB-STATUS-CODE        TO STATUS-WS                            
113400     PERFORM IMS-STATUS-CHECK                                             
113500     .                                                                    
113600*                                                                         
113700*IMS-04-GU-WDH111  SECTION.                                               
113800*    MOVE 'IMS-04'  TO CURR-IMS-SECTION                                   
113900*                                                                         
114000*    STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
114100*             DELIMITED BY SIZE INTO SSA1                                 
114200*    STRING 'WDH111(WDH111KY =' W-WDH111KY-X ')'                          
114300*             DELIMITED BY SIZE INTO SSA2                                 
114400*    MOVE '  GE'                  TO GOOD-STATUSCODES                     
114500*    CALL CBLTDLI USING GU INVA-PCB DLI-IO-WDH111 SSA1 SSA2               
114600*    MOVE INVA-STATUS-CODE        TO STATUS-WS                            
114700*    PERFORM IMS-STATUS-CHECK                                             
114800*    .                                                                    
114900 IMS-GN-WDH111  SECTION.                                                  
115000     MOVE 'IMS-GN-WDH11'  TO CURR-IMS-SECTION                             
115100                                                                          
115200     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
115300              DELIMITED BY SIZE INTO SSA1                                 
115400     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN                          
115500                    '&WDH111KY<=' W-WDH111KY-MAX ')'                      
115600              DELIMITED BY SIZE INTO SSA2                                 
115700     MOVE '  GE'                  TO GOOD-STATUSCODES                     
115800     CALL CBLTDLI USING GN INVA-PCB DLI-IO-WDH111 SSA1 SSA2               
115900     MOVE INVA-STATUS-CODE        TO STATUS-WS                            
116000     PERFORM IMS-STATUS-CHECK                                             
116100     .                                                                    
116200 IMS-GU-WDH111-MIN-MAX  SECTION.                                          
116300     MOVE 'IMS-GU-WDH11'  TO CURR-IMS-SECTION                             
116400                                                                          
116500     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
116600              DELIMITED BY SIZE INTO SSA1                                 
116700     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN                          
116800                    '&WDH111KY<=' W-WDH111KY-MAX ')'                      
116900              DELIMITED BY SIZE INTO SSA2                                 
117000     MOVE '  GE'                  TO GOOD-STATUSCODES                     
117100     CALL CBLTDLI USING GU INVA-PCB DLI-IO-WDH111 SSA1 SSA2               
117200     MOVE INVA-STATUS-CODE        TO STATUS-WS                            
117300     PERFORM IMS-STATUS-CHECK                                             
117400     .                                                                    
117500 IMS-05-GET-WDH1-WITH-SEC-INDEX  SECTION.                                 
117600     MOVE 'IMS-05'  TO CURR-IMS-SECTION                                   
117700                                                                          
117800     STRING 'WLINVB01(WDH1A1KY>=' W-WDH1A1KY-MIN-X                        
117900                    '&WDH1A1KY<=' W-WDH1A1KY-MAX-X ')'                    
118000              DELIMITED BY SIZE INTO SSA1                                 
118100     MOVE '  GEGB'                TO GOOD-STATUSCODES                     
118200     CALL CBLTDLI USING GN INVB-PCB DLI-IO-AREA-WDH1A1 SSA1               
118300     MOVE INVB-STATUS-CODE        TO STATUS-WS                            
118400     PERFORM IMS-STATUS-CHECK                                             
118500     .                                                                    
118600 IMS-STATUS-CHECK   SECTION.                                              
118700                                                                          
118800     SET STATUS-IX TO 1                                                   
118900     SEARCH GOOD-STATUS                                                   
119000       AT END                                                             
119100         CALL FELLOG                                                      
119200     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
119300       CONTINUE                                                           
119400     END-SEARCH                                                           
119500     .                                                                    
119600*    -COPY WY2000P1                                                       
