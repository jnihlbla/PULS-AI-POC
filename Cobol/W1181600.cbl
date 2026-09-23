000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1181600.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   2025/07/25.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        READS INP FILE W1181101 WITH PARTS INFO FROM PRINS/TCPLM         
001000*        TRIGGERS TRANS 1113 AND 3151 BASED ON IDPTYP                     
001100*                                                                         
001200*        IDPTYP                                                           
001300*        SSI,SSU,SWS,REJ  REGISTERING SUPERSESSION                        
001400*                         TRIGGER 1113 FOR INS OR UPD RESPECTIVELY        
001500*             EXI OR EXU  REGISTERING EXCHANGE AND CORE PARTS             
001600*                         TRIGGER 3151 FOR INS OR UPD RESPECTIVELY        
001700*             EXS         UPDATE EXCHANE AND CORE PARTS                   
001800*                         UPDATE SUPERSESSION CODE                        
001900*                         TRIGGER 3151 FOR INS OR UPD RESPECTIVELY        
002000*                         TRIGGER 1113 FOR SUPERSESSION UPDATE            
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- FILE WITH PART INFO                                        
003100     SELECT W11811                     ASSIGN TO W11816D1.                
003200     EJECT                                                                
003210***  ERROR FILE FOR PARTS PLANNER                                         
003220     SELECT W11814                     ASSIGN TO W11816D2.                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W11811                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W1181101  PRE IN- -L.                                          
004200     EJECT                                                                
004210 FD  W11814                                                               
004220     RECORDING   F                                                        
004230     BLOCK CONTAINS 0.                                                    
004240*01  POST -COPY W1181102 -PRE  UT1-  -L.                                  
004250                                                                          
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W1181600'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004710 77  FEL                         PIC X       VALUE 'F'.                   
004800 77  NEW                         PIC X       VALUE 'N'.                   
004810 77  INPUT-RETT                  PIC X       VALUE 'J'.                   
004900 01  CHKP-VAR.                                                            
005000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005800 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
005900 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
006000 77  RAD-IX                      PIC 9(3)    VALUE ZERO.                  
006001 77  REQU-IX                     PIC 9(3)    VALUE ZERO.                  
006002 77  BEERS-IX                    PIC 9(3)    VALUE ZERO.                  
006003 77  MAX-IX                      PIC 9(3)    VALUE 99.                    
006004 77  BEERS-MAX-IX                PIC 9(3)    VALUE 10.                    
006010 77  WS-IDKORTNR                 PIC 9(3)    VALUE ZERO.                  
006100     SKIP2                                                                
006200                                                                          
006300 77  W11811-EMPTY-SW             PIC X       VALUE 'Y'.                   
006400 77  W11811-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W11811                       VALUE 'Y'.                   
006600     EJECT                                                                
006700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES TODAYS-DATE.                                        
006900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007100     03  TODAYS-DATE-DAY         PIC 9(2).                                
007200     EJECT                                                                
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007910     03  W111ERSA                PIC X(8)    VALUE 'W111ERSA'.            
008000     SKIP2                                                                
008100*    --- PARAMETERS TO ABEND                                              
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600*                                                                         
008700 01  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
008800 01  IDARTNR-WS REDEFINES WS-IDARTNR                                      
008900                                 PIC 9(9).                                
008930                                                                          
009000 01  ERROR-TEXT.                                                          
009100     03  FILLER                  PIC X(8)    VALUE 'ERRTXT'.              
009200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*-----------------------------------------------------------              
009500***                    MESSAGE AREA                                       
009600*-----------------------------------------------------------              
009700*                                                                         
009800*01  -COPY WMSGAREA                                                       
010100       05  FILLER REDEFINES MSG-MID-OUT.                                  
010200          07  -COPY W3I15101 -PRE 3151-                                   
010300       05  FILLER REDEFINES MSG-MID-OUT.                                  
010400          07  -COPY W2I13301 -PRE 2133-                                   
010500     EJECT                                                                
010600******************************************************************        
010700*                AREA TO COMMUNICATE WITH W111ERSA                        
010800*                                                                         
010900 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
011000 01  REQU-AREA.                                                           
011100     03 -COPY WZ01REQU                                                    
011200     03 -COPY W111ERIN                                                    
011300                                                                          
011400 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
011500 01  RESP-AREA.                                                           
011600     03 -COPY WZ01RESP                                                    
011700     03 -COPY W111ERUT                                                    
011800*****************************************************************         
011900*                                                                         
012000*    --- PARAMETRAR TILL POSTSUM                                          
012100*                                                                         
012200*01  -COPY W0005   -PRE  POSTSUM-                                         
012300     EJECT                                                                
012400 01  FILLER                      PIC X(8)   VALUE                         
012500                                             'IN-AREA'.                   
012700*01  AREA -COPY W1181101   -PRE IN-                                       
012800     EJECT                                                                
012810 01  FILLER                      PIC X(8)   VALUE                         
012820                                             'UT1-AREA'.                  
012830*01  AREA -COPY W1181102   -PRE UT1-                                      
012840                                                                          
012900*    ---  AREA FÖR W006KOM SUBMODUL                                       
013000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
013100                                                                          
013200 01  KOM-IO-AREA.                                                         
013300*    03  -COPY WMSGKOM                                                    
013400     EJECT                                                                
013500*                                                                         
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700                                                                          
013800 01  NYCKLAR-TILL-DLI.                                                    
013900     03  W-IDARTNR-X.                                                     
014000         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
014100                                                                          
014120     03  W-KDSEGKEY-X.                                                    
014130         05  W-KDSEGKEY          PIC  X      VALUE '1'.                   
014200     SKIP3                                                                
014300*    --- STATUS-CODE FROM IMS                                             
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FOUND                       VALUE '  '.                  
014600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
014800     88  IMS-NOT-OK                          VALUE 'XD'.                  
014900                                                                          
015000 01  GOOD-STATUSCODES.                                                    
015100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015200                                                                          
015210 01  ALL-SSA.                                                             
015300     03  SSA1                        PIC X(64).                           
015400     03  SSA2                        PIC X(64).                           
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900                                                                          
016000*    ---  DLI INPUT-OUTPUT AREA                                           
016150 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016160 01  DLI-IO-WDK611.                                                       
016170*    03  -COPY WDK611                                                     
016200                                                                          
016300 LINKAGE SECTION.                                                         
016400                                                                          
016500*01  -COPY W0009 -PRE MSG-                                                
016600     EJECT                                                                
016700*01  -COPY W0009 -PRE 0693X-                                              
016800     EJECT                                                                
016900*01  -COPY W0009 -PRE WDP8-                                               
017000     EJECT                                                                
017010*01  -COPY W0008 -PRE WDK6-                                               
017020     05  FILLER                  PIC X.                                   
017100*01  -COPY W0008 -PRE ARTC-                                               
017200         05  FILLER              PIC X.                                   
017300     EJECT                                                                
017400*01  -COPY W0008 -PRE ERSA-                                               
017500         05  FILLER              PIC X.                                   
017600     EJECT                                                                
017700*01  -COPY W0008 -PRE ERSB-                                               
017800         05  FILLER              PIC X.                                   
017900     EJECT                                                                
018000*01  -COPY W0008 -PRE INLB-                                               
018100         05  FILLER              PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008 -PRE BENA-                                               
018400         05  FILLER              PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008 -PRE XXAN-                                               
018700         05  FILLER              PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008 -PRE 2303-                                               
019000         05  FILLER              PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008 -PRE XXBJ-                                               
019300         05  FILLER              PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008 -PRE XXID-                                               
019600         05  FILLER              PIC X.                                   
019700     EJECT                                                                
019800*01  -COPY W0008 -PRE ZZAC-                                               
019900         05  FILLER              PIC X.                                   
020000     EJECT                                                                
020100*01  -COPY W0008 -PRE ART2-                                               
020200         05  FILLER              PIC X.                                   
020300     EJECT                                                                
020400*01  -COPY W0008 -PRE XXAV-                                               
020500         05  FILLER              PIC X.                                   
020600     EJECT                                                                
020700*01  -COPY W0008 -PRE ARTG-                                               
020800         05  FILLER              PIC X.                                   
020900     EJECT                                                                
021000*01  -COPY W0008 -PRE XXAW-                                               
021100         05  FILLER              PIC X.                                   
021200     EJECT                                                                
021300*01  -COPY W0008 -PRE SATE-                                               
021400         05  FILLER              PIC X.                                   
021500     EJECT                                                                
021600*01  -COPY W0008 -PRE SATB-                                               
021700         05  FILLER              PIC X.                                   
021800     EJECT                                                                
021900*01  -COPY W0008 -PRE XXBW-                                               
022000         05  FILLER              PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008 -PRE XXCW-                                               
022300         05  FILLER              PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008 -PRE FILC-                                               
022600         05  FILLER              PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008 -PRE WDK7-                                               
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008 -PRE WDB6-                                               
023200     05  FILLER                  PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008 -PRE WDR2-                                               
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700*01  -COPY W0008 -PRE WDR5-                                               
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000                                                                          
024100 PROCEDURE DIVISION  USING MSG-PCB 0693X-PCB WDP8-PCB WDK6-PCB            
024200                           ARTC-PCB ERSA-PCB ERSB-PCB                     
024300                           INLB-PCB BENA-PCB XXAN-PCB                     
024400                           2303-PCB XXBJ-PCB                              
024500                           XXID-PCB ZZAC-PCB ART2-PCB                     
024600                           XXAV-PCB ARTG-PCB XXAW-PCB                     
024700                           SATE-PCB SATB-PCB                              
024800                           XXBW-PCB XXCW-PCB                              
024900                           FILC-PCB WDK7-PCB WDB6-PCB                     
025000                           WDR2-PCB WDR5-PCB.                             
025100                                                                          
025200     ENTRY 'DLITCBL' USING MSG-PCB 0693X-PCB WDP8-PCB WDK6-PCB            
025300                           ARTC-PCB ERSA-PCB ERSB-PCB                     
025400                           INLB-PCB BENA-PCB XXAN-PCB                     
025500                           2303-PCB XXBJ-PCB                              
025600                           XXID-PCB ZZAC-PCB ART2-PCB                     
025700                           XXAV-PCB ARTG-PCB XXAW-PCB                     
025800                           SATE-PCB SATB-PCB                              
025900                           XXBW-PCB XXCW-PCB                              
026000                           FILC-PCB WDK7-PCB WDB6-PCB                     
026100                           WDR2-PCB WDR5-PCB.                             
026200                                                                          
026300     SKIP2                                                                
026400     PERFORM A-INIT                                                       
026500     PERFORM S01-READ-W11811                                              
026600     PERFORM UNTIL END-OF-W11811                                          
026700                                                                          
026701       IF IN-IDPTYP NOT = 'NSP'                                           
026710          PERFORM IMS-GU-WDK611                                           
026720       END-IF                                                             
026730                                                                          
026800       EVALUATE IN-IDPTYP                                                 
026900                                                                          
027000          WHEN 'SSI'                                                      
027100          WHEN 'SSU'                                                      
027200          WHEN 'SWS'                                                      
027300          WHEN 'REJ'                                                      
027400            PERFORM B-SS-UPD                                              
027500                                                                          
027600          WHEN 'EXI'                                                      
027700          WHEN 'EXU'                                                      
027800            PERFORM C-EXCH-TRANS                                          
027900            PERFORM E-SEND-TRANSACTION                                    
028000                                                                          
028100          WHEN 'EXS'                                                      
028200            PERFORM C-EXCH-TRANS                                          
028300            PERFORM E-SEND-TRANSACTION                                    
028400                                                                          
028500            PERFORM B-SS-UPD                                              
028600                                                                          
028700          WHEN 'SWI'                                                      
028800            PERFORM D-PRICE-TRANS                                         
028900            PERFORM E-SEND-TRANSACTION                                    
029000                                                                          
029100       END-EVALUATE                                                       
029200                                                                          
029300       PERFORM S01-READ-W11811                                            
029400                                                                          
029500     END-PERFORM                                                          
029600                                                                          
029700     IF W11811-EMPTY-SW = 'Y'                                             
029800        DISPLAY 'INPUT FILE IS EMPTY FROM TCPLM/PRINS'                    
029900     END-IF                                                               
030000                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800     SKIP2                                                                
030900                                                                          
031000                                                                          
031100     OPEN INPUT  W11811                                                   
031110         OUTPUT  W11814                                                   
031200     MOVE ZERO TO CHKP-ANT                                                
031300     PERFORM IMS-RESTART                                                  
031400     PERFORM AA-WRITE-HEADER                                              
031500     .                                                                    
031600     EJECT                                                                
031700 AA-WRITE-HEADER SECTION.                                                 
031800*                                                                         
031900     MOVE +54                   TO MSG-KOM-KVLL                           
032000     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
032100                                   MSG-KOM-KDZ2                           
032200     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
032300     MOVE 'TC PLM'              TO MSG-KOM-IDSNDNOD                       
032400     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
032500                                                                          
032600     ACCEPT WS-TIUPPDAT FROM DATE                                         
032700     ACCEPT WS-TIUPPTID FROM TIME                                         
032800     MOVE WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                       
032900     MOVE WS-TIUPPTID           TO MSG-KOM-TIKLOCK                        
033000     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
033100                                   MSG-KOM-KDSVAR                         
033200     MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
033300     .                                                                    
033400     EJECT                                                                
033500 B-SS-UPD SECTION.                                                        
033600                                                                          
033700***  CALLS W111ERSA SUBPROGRAM TO UPDATE SUPERSESSION DETAILS             
033710                                                                          
033810     IF CLAG-KDERS NOT = ZERO AND                                         
033820        IN-KDERS   NOT = ZERO                                             
033822                                                                          
033823*       RESET KDERS TO ZERO IF THE PART IS ALREADY SUPERCEDED WITH        
033824*       DIFFERENT SUPERSESSION CODE OR SAME SUPERSESSION CODE.            
033825*       ALL SS ATTRIBUTES WILL BE UPDATED AGAIN.                          
033826                                                                          
033827        MOVE ALL ZEROES               TO REQU-KDERS                       
033828        MOVE '102'                    TO REQU-IDMSGVER                    
033829        SET  REQU-UPDATE              TO TRUE                             
033830        MOVE IN-IDCDS                 TO REQU-IDUSER                      
033831                                                                          
033832        MOVE IN-IDARTNR               TO WS-IDARTNR                       
033833        MOVE IDARTNR-WS               TO REQU-IDARTNR-KEY                 
033834        MOVE '11'                     TO REQU-IDDC                        
033835        MOVE ALL '+'                  TO REQU-IDKORTNR-SPAR1              
033836                                         REQU-IDKORTNR-SPAR2              
033837                                         REQU-IDKORTNR-SPAR3              
033838                                         REQU-DIERS-ERS                   
033839                                         REQU-IDAO                        
033840                                         REQU-TIERSDAT-PREL               
033841                                         REQU-TEARTNOT                    
033842        MOVE SPACE                    TO REQU-KDARBTYP-SEC-IDLEV          
033843        MOVE IN-KDARTSYS              TO REQU-KDARTSYS                    
033844        MOVE JA                       TO REQU-FLKLAR                      
033845        MOVE 'EN'                     TO REQU-IDSPRAK                     
033846        MOVE ZERO                     TO REQU-KVRADER-MAX9                
033891*                                                                         
033892        PERFORM BB-CALL-BIZ-LOGIC-W111ERSA                                
033894        IF RESP-KDSVAR = FEL                                              
033895           PERFORM BC-WRITE-ERROR-FILE                                    
033896        ELSE                                                              
033898           PERFORM BD-UPD-ACTUAL-SS                                       
033907        END-IF                                                            
033908     ELSE                                                                 
033909        PERFORM BD-UPD-ACTUAL-SS                                          
034000     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 BA-INIT-REQU SECTION.                                                    
034800                                                                          
034900     MOVE JA                       TO INPUT-RETT                          
034910                                                                          
035000/* TO INDICATE THAT THE MSG IS FROM TCPLM                                 
035100     MOVE '102'                    TO REQU-IDMSGVER                       
035200/*                                                                        
035300     SET  REQU-UPDATE              TO TRUE                                
035400     MOVE IN-IDCDS                 TO REQU-IDUSER                         
035500                                                                          
035600     MOVE IN-IDARTNR               TO WS-IDARTNR                          
035700     MOVE IDARTNR-WS               TO REQU-IDARTNR-KEY                    
035800     MOVE '11'                     TO REQU-IDDC                           
035900     MOVE ZERO                     TO REQU-IDKORTNR-SPAR1                 
036000     MOVE ZERO                     TO REQU-IDKORTNR-SPAR2                 
036100     MOVE ZERO                     TO REQU-IDKORTNR-SPAR3                 
036101                                                                          
036110     IF IN-DIERS-ERS  = SPACES                                            
036120        MOVE ALL '+'               TO REQU-DIERS-ERS                      
036130     ELSE                                                                 
036200        MOVE IN-DIERS-ERS          TO REQU-DIERS-ERS                      
036210     END-IF                                                               
036220                                                                          
036300     MOVE IN-KDERS                 TO REQU-KDERS                          
036310                                                                          
036400     IF IN-IDAO       = SPACES                                            
036401        MOVE ALL '+'               TO REQU-IDAO                           
036402     ELSE                                                                 
036403        MOVE IN-IDAO               TO REQU-IDAO                           
036404     END-IF                                                               
036410                                                                          
036420     IF IN-TIERSDAT-PREL = SPACES                                         
036500        MOVE ALL '+'               TO REQU-TIERSDAT-PREL                  
036510     ELSE                                                                 
036511        MOVE IN-TIERSDAT-PREL      TO REQU-TIERSDAT-PREL                  
036520     END-IF                                                               
036530                                                                          
036550     MOVE ALL '+'                  TO REQU-TEARTNOT                       
036590                                                                          
036600     MOVE IN-KDARTSYS              TO REQU-KDARTSYS                       
036700                                                                          
036900     MOVE +1                       TO RAD-IX                              
036910                                      REQU-IX                             
036920     MOVE ZERO                     TO WS-IDKORTNR                         
036921                                      REQU-KVRADER-MAX9                   
036930                                                                          
037300     PERFORM UNTIL RAD-IX   > IN-KVRADER-MAX9 OR                          
037400                   REQU-IX  > MAX-IX                                      
037500                                                                          
037700*                                                                         
038510*       IF IN-KDERS = 4 OR 5 OR 6 OR 8                                    
038512***        TO POPULATE TEXT.                                              
038513***        NEED TO INCREASE BEERS-MAX-IX IF WE HAVE TO INCREASE           
038514***        MAX LIMIT OF 200 BYTES FOR TEXT.                               
038520           MOVE +1                 TO BEERS-IX                            
038521           PERFORM UNTIL ( IN-BEERS (RAD-IX,BEERS-IX) = SPACES )          
038522                      OR  BEERS-IX > BEERS-MAX-IX                         
038523*                         TO AVOID TEXT AT THE LAST LINE                  
038524                      OR  REQU-IX = MAX-IX                                
038525                                                                          
038526            MOVE ALL '+'           TO REQU-RAD (REQU-IX)                  
038527                                                                          
038528            COMPUTE WS-IDKORTNR     =  REQU-IX * 10                       
038529            MOVE WS-IDKORTNR       TO REQU-IDKORTNR (REQU-IX)             
038530                                                                          
038531            MOVE IN-BEERS (RAD-IX,BEERS-IX)                               
038532                                   TO REQU-BEERS (REQU-IX)                
038533            ADD +1                 TO REQU-IX                             
038534                                      BEERS-IX                            
038537           END-PERFORM                                                    
038538                                                                          
038539*       END-IF                                                            
038540***                                                                       
038541        MOVE ALL '+'               TO REQU-RAD (REQU-IX)                  
038542        COMPUTE WS-IDKORTNR =  REQU-IX * 10                               
038543        MOVE WS-IDKORTNR           TO REQU-IDKORTNR (REQU-IX)             
038544                                                                          
038545        MOVE IN-IDARTNR-TILLK (RAD-IX)                                    
038546                                   TO REQU-IDARTNR-TILLK (REQU-IX)        
038547                                                                          
038548        MOVE IN-DIERS-TILLK (RAD-IX)                                      
038550                                   TO REQU-DIERS-TILLK (REQU-IX)          
038560*                                                                         
038561        MOVE REQU-IX               TO REQU-KVRADER-MAX9                   
038570*                                                                         
038600        ADD +1                     TO RAD-IX                              
038610                                      REQU-IX                             
038700     END-PERFORM                                                          
038800                                                                          
038810*    IF ( IN-KDERS = 4 OR 5 OR 6 OR 8 )    AND                            
038811     IF RAD-IX  <= MAX-IX                                                 
038812        IF IN-IDARTNR-TILLK (RAD-IX) > ZERO   AND                         
038813           IN-IDARTNR-TILLK (RAD-IX) NUMERIC                              
038815           MOVE 'NEW PARTS EXCEED MAX LIMIT'                              
038816                                   TO RESP-FELTEXT                        
038817           MOVE NEJ                TO INPUT-RETT                          
038818        END-IF                                                            
038820     END-IF                                                               
038900                                                                          
039100     MOVE SPACE                    TO REQU-KDARBTYP-SEC-IDLEV             
039300     MOVE JA                       TO REQU-FLKLAR                         
039400     MOVE 'EN'                     TO REQU-IDSPRAK                        
039500     .                                                                    
039600     EJECT                                                                
039760 BB-CALL-BIZ-LOGIC-W111ERSA    SECTION.                                   
039800                                                                          
039900     CALL W111ERSA USING                                                  
040000          REQU-AREA RESP-AREA                                             
040100          ARTC-PCB ERSA-PCB ERSB-PCB                                      
040200          INLB-PCB BENA-PCB XXAN-PCB                                      
040300          2303-PCB XXBJ-PCB                                               
040400          XXID-PCB ZZAC-PCB ART2-PCB                                      
040500          XXAV-PCB ARTG-PCB XXAW-PCB                                      
040600          SATE-PCB SATB-PCB                                               
040700          XXBW-PCB XXCW-PCB                                               
040800          FILC-PCB WDK7-PCB WDB6-PCB                                      
040900          WDR2-PCB WDR5-PCB                                               
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041210 BC-WRITE-ERROR-FILE SECTION.                                             
041220                                                                          
041250     INITIALIZE UT1-AREA                                                  
041260     MOVE IN-IDARTNR            TO UT1-IDARTNR                            
041270     MOVE IN-IDBERED            TO UT1-IDBERED                            
041280     MOVE IN-IDCDS              TO UT1-IDCDS                              
041290                                                                          
041291     IF IN-KDSORT = 'SW'                                                  
041292          MOVE 'SP'             TO UT1-SPAREPARTUNIT                      
041293     ELSE                                                                 
041294          MOVE 'HW'             TO UT1-SPAREPARTUNIT                      
041295     END-IF                                                               
041296                                                                          
041301     MOVE RESP-FELTEXT          TO UT1-FELTEXT(1)                         
041308                                                                          
041309     PERFORM S02-WRITE-W11814                                             
041310     .                                                                    
041311     EJECT                                                                
041312 BD-UPD-ACTUAL-SS SECTION.                                                
041313*  TO UPDATE ACTUAL SUPERSESSION DATA FROM TCPLM                          
041314                                                                          
041315     PERFORM BA-INIT-REQU                                                 
041316     IF INPUT-RETT = JA                                                   
041317        PERFORM BB-CALL-BIZ-LOGIC-W111ERSA                                
041318     END-IF                                                               
041319                                                                          
041322     IF RESP-KDSVAR = FEL OR                                              
041323        INPUT-RETT  = NEJ                                                 
041324        PERFORM BC-WRITE-ERROR-FILE                                       
041325     END-IF                                                               
041326     .                                                                    
041330 C-EXCH-TRANS SECTION.                                                    
041400                                                                          
041500***  CALLS PGM W30151 THROUGH DISPACTHER                                  
041600***  WE FIRST LOAD MID-COPYBOOK WITH ALL + VALUES                         
041700                                                                          
041800     INITIALIZE MSG-IO-AREA                                               
041900     MOVE ALL '+'               TO 3151-MID-W3I15101                      
042000     PERFORM CA-CREATE-HEADER-3151                                        
042100*                                                                         
042200     MOVE IN-IDARTNR            TO 3151-MID-IDARTNR-IN                    
042300                                   3151-MID-IDARTNR-UT                    
042400     MOVE IN-IDPRODNR           TO 3151-MID-IDPRODNR-IN                   
042500     MOVE NEW                   TO 3151-MID-UPPDATERINGSSORT              
042600*                                                                         
042700     IF IN-IDPTYP = 'EXI'                                                 
042800        MOVE  100               TO 3151-MID-IDDISTR-RENOV-IN              
042900        MOVE  9927              TO 3151-MID-IDDISTR-NDC-IN                
043000                                   3151-MID-IDDISTR-CAN-IN                
043100                                   3151-MID-IDDISTR-PAC-IN                
043200                                   3151-MID-IDDISTR-AUS-IN                
043300                                   3151-MID-IDDISTR-KOR-IN                
043400        MOVE  8480              TO 3151-MID-IDDISTR-CHN-IN                
043500     END-IF                                                               
043600*                                                                         
043700     .                                                                    
043800     EJECT                                                                
043900                                                                          
044000 CA-CREATE-HEADER-3151  SECTION.                                          
044100                                                                          
044200***  FOR MSG-KON-AREA                                                     
044300*                                                                         
044400     MOVE 'W3I15101'            TO MSG-KOM-IDCPYTXT                       
044500*                                                                         
044600***  FOR MSG-IO-AREA                                                      
044700*                                                                         
044800     COMPUTE MSG-KVLL  = LENGTH OF 3151-MID-W3I15101 + 17                 
044900     MOVE LOW-VALUE             TO MSG-KDZ1                               
045000                                   MSG-KDZ2                               
045100     MOVE 'W3T151X'             TO MSG-KDTRANS-1                          
045200     MOVE '3151'                TO MSG-IDTRANS-1                          
045300     MOVE '1'                   TO MSG-KDMFSFOR-1                         
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 D-PRICE-TRANS SECTION.                                                   
045800                                                                          
045900***  CALLS PGM W20133 THROUGH DISPATCHER TO UPDATE PRICE,                 
046000***  WE FIRST LOAD MID-COPYBOOK WITH ALL + VALUES                         
046100                                                                          
046200     INITIALIZE MSG-IO-AREA                                               
046300     MOVE ALL '+'               TO 2133-MID-W2I13301                      
046400     PERFORM DA-CREATE-HEADER-2133                                        
046500*                                                                         
046600     MOVE IN-IDARTNR            TO 2133-MID-IDARTNR-UT                    
046700     MOVE IN-IDLEVNR            TO 2133-MID-IDLEVNR                       
046800     MOVE IN-IDPLANGR-AG        TO 2133-MID-IDPLANGR-AG                   
046900     MOVE IN-IDANSK             TO 2133-MID-IDANSK                        
047000     MOVE IN-PRARTSTD           TO 2133-MID-PRARTSTD                      
047100     MOVE IN-KVPB-C1            TO 2133-MID-KVPB-C1                       
047200     .                                                                    
047300     EJECT                                                                
047400 DA-CREATE-HEADER-2133  SECTION.                                          
047500                                                                          
047600***  FOR MSG-KON-AREA                                                     
047700*                                                                         
047800     MOVE 'W2I13301'            TO MSG-KOM-IDCPYTXT                       
047900*                                                                         
048000***  FOR MSG-IO-AREA                                                      
048100*                                                                         
048200     COMPUTE MSG-KVLL  = LENGTH OF 2133-MID-W2I13301 + 17                 
048300     MOVE LOW-VALUE             TO MSG-KDZ1                               
048400                                   MSG-KDZ2                               
048500     MOVE 'W2T133X'             TO MSG-KDTRANS-1                          
048600     MOVE '2133'                TO MSG-IDTRANS-1                          
048700     MOVE '2'                   TO MSG-KDMFSFOR-1                         
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100 E-SEND-TRANSACTION SECTION.                                              
049200*    -- INITIALIZE W006KOM FIELDS                                         
049300      ADD +1                TO MSG-KOM-TIKLOCK                            
049400      CALL W006KOM USING MSG-PCB                                          
049500                         0693X-PCB                                        
049600                         WDP8-PCB                                         
049700                         MSG-KOM-WMSGKOM                                  
049800                         MSG-IO-AREA                                      
049900                                                                          
050000                                                                          
050100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
050200        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
050300          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
050400        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
050500     END-IF                                                               
050600     ADD +1                 TO CHKP-ANT                                   
050700                                                                          
050800     IF CHKP-ANT > CHKP-MAX                                               
050900        PERFORM IMS-CHECKPOINT                                            
051000        MOVE +0             TO CHKP-ANT                                   
051100        ADD  +1             TO MSG-KOM-TIKLOCK                            
051200     END-IF                                                               
051300                                                                          
051400      .                                                                   
051500      EJECT                                                               
051600 Z-FINIT SECTION.                                                         
051700                                                                          
051800                                                                          
051900     CLOSE W11811                                                         
051910           W11814                                                         
052000     SKIP2                                                                
052100     MOVE 'S' TO POSTSUM-OPKOD                                            
052200     CALL POSTSUM USING POSTSUM-PARM                                      
052300     .                                                                    
052400     EJECT                                                                
052500 S01-READ-W11811  SECTION.                                                
052600     SKIP2                                                                
052700     READ W11811 INTO IN-AREA                                             
052800     AT END                                                               
052900        MOVE HIGH-VALUE TO IN-AREA                                        
053000        SET END-OF-W11811 TO TRUE                                         
053100                                                                          
053200     NOT AT END                                                           
053300        MOVE NOO      TO W11811-EMPTY-SW                                  
053400        MOVE 'W11811' TO POSTSUM-FDNAMN                                   
053500        MOVE 'W11816D1' TO POSTSUM-DDNAMN2                                
053600        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
053700        CALL POSTSUM USING POSTSUM-PARM                                   
053800                                                                          
053900     END-READ                                                             
054000     .                                                                    
054100     EJECT                                                                
054110 S02-WRITE-W11814 SECTION.                                                
054120                                                                          
054130     WRITE UT1-POST            FROM UT1-AREA                              
054140                                                                          
054150     MOVE 'UT1'                  TO POSTSUM-TRANSTYP                      
054160     MOVE 'W11814'               TO POSTSUM-FDNAMN                        
054170     MOVE 'W11816D2'             TO POSTSUM-DDNAMN2                       
054180     CALL POSTSUM             USING POSTSUM-PARM                          
054190     .                                                                    
054191     EJECT                                                                
054200****************************************************************          
054300***                    IMS SECTION                                        
054400****************************************************************          
054500*                                                                         
054510 IMS-GU-WDK611 SECTION.                                                   
054520                                                                          
054550     MOVE SPACES              TO ALL-SSA                                  
054551     MOVE IN-IDARTNR          TO WS-IDARTNR                               
054552     MOVE IDARTNR-WS          TO W-IDARTNR                                
054553                                                                          
054560     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
054570          DELIMITED BY SIZE INTO SSA1                                     
054580     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
054590          DELIMITED BY SIZE INTO SSA2                                     
054591     MOVE ' '                 TO GOOD-STATUSCODES                         
054592     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
054593     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
054594     PERFORM IMS-STATUSCHECK                                              
054595     .                                                                    
054596     EJECT                                                                
054600 IMS-RESTART SECTION.                                                     
054700     SKIP2                                                                
054800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
054900     MOVE '  ' TO GOOD-STATUSCODES                                        
055000     CALL CBLTDLI USING XRST MSG-PCB                                      
055100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
055200                        CHKP-AREA-LENGTH CHKP-AREA                        
055300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055400     PERFORM IMS-STATUSCHECK                                              
055500     .                                                                    
055600     SKIP3                                                                
055700 IMS-CHECKPOINT SECTION.                                                  
055800     SKIP2                                                                
055900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
056000     MOVE '  XD' TO GOOD-STATUSCODES                                      
056100     CALL CBLTDLI USING CHKP MSG-PCB                                      
056200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
056300                        CHKP-AREA-LENGTH CHKP-AREA                        
056400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056500     PERFORM IMS-STATUSCHECK                                              
056600                                                                          
056700     IF IMS-NOT-OK                                                        
056800       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
056900                                           TO ERROR-TEXT-STR              
057000       DISPLAY ERROR-TEXT                                                 
057100       CALL FELLOG                                                        
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 IMS-STATUSCHECK SECTION.                                                 
057600     SKIP2                                                                
057700     SET STATUS-IX TO 1                                                   
057800     SEARCH GOOD-STATUS                                                   
057900       AT END                                                             
058000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
058100           DELIMITED BY SIZE INTO ERROR-TEXT                              
058200         DISPLAY ERROR-TEXT                                               
058300         CALL FELLOG                                                      
058400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
058500         CONTINUE                                                         
058600     END-SEARCH                                                           
058700     .                                                                    
