000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4032000.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   23/01/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.PICKINGLIST                                
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAM TO HANDLE PICKING LISTS                                  
001100*                                                                         
001200*        THE PROGRAM READS     WDQ3J                                      
001300*        THE PROGRAM UPDATES   WDQ3 WDR4 WDE4                             
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W40320T                                             
001700*        REQUEST:     W40320I1                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        RESPONSE:    WL0134O2                                            
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4032000'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300 77  KDRC-NUM                    PIC S9(9).                               
003400                                                                          
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  IX1                         PIC S9(9) COMP SYNC.                     
003900 77  IX2                         PIC S9(9) COMP SYNC.                     
004000 77  MSG-IX                      PIC S9(9) COMP SYNC.                     
004100                                                                          
004200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004300     88  KEYS-OK                             VALUE 'J'.                   
004400     88  KEYS-WRONG                          VALUE 'N'.                   
004500                                                                          
004600 77  CONTINUE-SW                 PIC X       VALUE 'J'.                   
004700     88  CONTINUE-YES                        VALUE 'J'.                   
004800     88  CONTINUE-NO                         VALUE 'N'.                   
004900                                                                          
005000 77  REKY-SW                     PIC X       VALUE 'N'.                   
005100     88  REKY-FOUND                          VALUE 'J'.                   
005200     88  REKY-MISSING                        VALUE 'N'.                   
005300                                                                          
005400 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
005500     88  UPDATE-OK                           VALUE 'J'.                   
005600     88  UPDATE-NOT-OK                       VALUE 'N'.                   
005700                                                                          
005800 77  REPORTED-SW                 PIC X       VALUE 'N'.                   
005900     88  NOT-REPORTED-LINE                   VALUE 'N'.                   
006000     88  REPORTED-LINE                       VALUE 'J'.                   
006100                                                                          
006200 01  SMALL-LETTERS               PIC X(31)   VALUE                        
006300     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖüÉ'.                                   
006400 01  CAPS-LETTERS                PIC X(31)   VALUE                        
006500     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
006600                                                                          
006700 01  WS-IDANSTNR-X.                                                       
006800     03  WS-IDANSTNR             PIC 9(8).                                
006900                                                                          
007000 01  WS-ADPLATS                  PIC 9(5).                                
007100 01  FILLER REDEFINES WS-ADPLATS.                                         
007200     03  WS-ADPLATSNR            PIC 9(3).                                
007300     03  WS-ADPLNIV-LEFT         PIC 9.                                   
007400     03  WS-ADPLNIV-RIGHT        PIC 9.                                   
007500                                                                          
007600 01  WS-SAVE-FIELDS.                                                      
007700     03  WS-SAVE-DAUTSKR         PIC X(8)    VALUE SPACES.                
007800     03  WS-SAVE-TIUTSTID        PIC S9(7)   COMP-3 VALUE 0.              
007900     03  WS-SAVE-IDPRCPLK        PIC X(4)    VALUE SPACES.                
008000     03  WS-SAVE-IDLOTNR-PLK     PIC S9(3)   COMP-3 VALUE 0.              
008100     03  WS-PREV-DAUTSKR         PIC X(8)    VALUE SPACES.                
008200     03  WS-PREV-TIUTSTID        PIC S9(7)   COMP-3 VALUE 0.              
008300     03  WS-PREV-IDPRCPLK        PIC X(4)    VALUE SPACES.                
008400     03  WS-PREV-IDLOTNR-PLK     PIC S9(3)   COMP-3 VALUE 0.              
008500     03  WS-IDLIST OCCURS 100 INDEXED BY IDLIST-IX.                       
008600         05  WS-IDPRODNR         PIC 9(7)    VALUE ZERO.                  
008700         05  WS-IDPLKLST         PIC 9(3)    VALUE ZERO.                  
008800     03  WS-IDLIST-TAB-MAX       PIC 9(3)    VALUE ZERO.                  
008900                                                                          
009000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009100 01  GENERAL-SUBPROGRAMS.                                                 
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009600     03  WZ04REKY                PIC X(8)    VALUE 'WZ04REKY'.            
009700     03  WZ04REDA                PIC X(8)    VALUE 'WZ04REDA'.            
009800     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
009900                                                                          
010000*    --- PARAMETERS TO ABEND                                              
010100                                                                          
010200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010500                                                                          
010600 01  MESSAGE-CODES.                                                       
010700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010800     03  INF-NO-LINES            PIC X(3)    VALUE '027'.                 
010900     03  INF-MORE-LINES-EXIST    PIC X(3)    VALUE '011'.                 
011000                                                                          
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011300                                                                          
011400*01  -COPY WZ01SUB                                                        
011500                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
011700                                                                          
011800*01  -COPY WMSGCONV                                                       
011900                                                                          
012000 01  FILLER                      PIC X(16)   VALUE 'WZ04REKY'.            
012100                                                                          
012200*01  -COPY WZ04REKY                                                       
012300                                                                          
012400 01  FILLER                      PIC X(16)   VALUE 'WZ04REDA'.            
012500                                                                          
012600*01  -COPY WZ04REDA                                                       
012700                                                                          
012800 01  FILLER                      PIC X(16)   VALUE 'WL01321'.             
012900                                                                          
013000*01  -COPY WL01321                                                        
013100                                                                          
013200 01  FILLER                      PIC X(16)   VALUE 'WL01322'.             
013300                                                                          
013400*01  -COPY WL01322                                                        
013500                                                                          
013600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013700                                                                          
013800                                                                          
013900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014000                                                                          
014100 01  REQU-AREA.                                                           
014200*    03  -COPY WZ01REQ2.                                                  
014300*    03  -COPY W40320I1.                                                  
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014600                                                                          
014700 01  RESP-AREA.                                                           
014800*    03  -COPY WZ01RES2                                                   
014900*    03  -COPY WL0134O2                                                   
015000                                                                          
015100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015400                                                                          
015500 01  KEYS-FOR-DLI.                                                        
015600*WDQ3H1                                                                   
015700                                                                          
015800     03  W-WDQ3H1KY-MIN-X.                                                
015900         05  W-Q3H1KY-MIN-IDDC        PIC X(2).                           
016000         05  W-Q3H1KY-MIN-IDPRCPLK    PIC X(4).                           
016100         05  W-Q3H1KY-MIN-IDLOTNR-PLK PIC S9(3) COMP-3.                   
016200         05  FILLER-MIN               PIC X(6) VALUE LOW-VALUE.           
016300                                                                          
016400     03  W-WDQ3H1KY-MAX-X.                                                
016500         05  W-Q3H1KY-MAX-IDDC        PIC X(2).                           
016600         05  W-Q3H1KY-MAX-IDPRCPLK    PIC X(4).                           
016700         05  W-Q3H1KY-MAX-IDLOTNR-PLK PIC S9(3) COMP-3.                   
016800         05  FILLER-MAX               PIC X(6) VALUE HIGH-VALUE.          
016900                                                                          
017000*END WDQ3H1                                                               
017100                                                                          
017200     03  W-WDQ3J1KY-MIN-X.                                                
017300         05  W-IDDC-WDQ3J-MIN    PIC X(2).                                
017400         05  W-DAUTSKR-WDQ3J-MIN PIC X(8).                                
017500         05  W-TIUTSTID-WDQ3J-MIN                                         
017600                                 PIC S9(7) COMP-3.                        
017700         05  W-IDPRCPLK-WDQ3J-MIN                                         
017800                                 PIC X(4).                                
017900         05  W-IDLOTNRP-WDQ3J-MIN                                         
018000                                 PIC S9(3) COMP-3.                        
018100         05  FILLER              PIC X(6).                                
018200     03  W-WDQ3J1KY-MAX-X.                                                
018300         05  W-IDDC-WDQ3J-MAX    PIC X(2).                                
018400         05  W-DAUTSKR-WDQ3J-MAX PIC X(8).                                
018500         05  W-TIUTSTID-WDQ3J-MAX                                         
018600                                 PIC S9(7) COMP-3.                        
018700         05  W-IDPRCPLK-WDQ3J-MAX                                         
018800                                 PIC X(4).                                
018900         05  W-IDLOTNRP-WDQ3J-MAX                                         
019000                                 PIC S9(3) COMP-3.                        
019100         05  FILLER              PIC X(6).                                
019200     03  W-IDUSER-MIN-X.                                                  
019300         05  W-IDUSER-MIN        PIC X(8).                                
019400     03  W-IDUSER-MAX-X.                                                  
019500         05  W-IDUSER-MAX        PIC X(8).                                
019600     03  W-WDQ3J1KY-X.                                                    
019700         05  W-IDDC-WDQ3J        PIC X(2)     VALUE SPACE.                
019800         05  FILLER              PIC X(24)    VALUE HIGH-VALUES.          
019900     03  W-WDQ301KY-X.                                                    
020000         05  W-WDQ301KY          PIC X(12)    VALUE SPACE.                
020100     03  W-4447-X.                                                        
020200         05  W-4447-IDHTYP       PIC X(4)     VALUE '4447'.               
020300         05  W-4447-IDDC         PIC X(2).                                
020400         05  FILLER              PIC X(24)    VALUE LOW-VALUES.           
020500     03  W-4448-X.                                                        
020600         05  W-4448-IDPRC        PIC X(4).                                
020700         05  FILLER              PIC X(1)     VALUE LOW-VALUES.           
020800     03  W-4487-X.                                                        
020900         05  W-4487-IDHTYP       PIC X(4)     VALUE '4487'.               
021000         05  W-4487-IDDC         PIC X(2).                                
021100         05  FILLER              PIC X(24)    VALUE LOW-VALUES.           
021200     03  W-4488-X.                                                        
021300         05  W-4488-KDPRCGRP     PIC X(5).                                
021400     03  W-4490-X.                                                        
021500         05  W-4490-DARFS        PIC 9(12).                               
021600         05  W-4490-IDPRODNR     PIC S9(7)  COMP-3.                       
021700         05  W-4490-IDPLKLST     PIC S9(3)  COMP-3.                       
021800     03  W-WDE401KY-X.                                                    
021900         05  W-IDDISTR-E4        PIC S9(5)  COMP-3.                       
022000         05  W-IDKUNDNR-E4       PIC S9(7)  COMP-3.                       
022100         05  W-IDORDNR5-E4       PIC 9(5).                                
022200         05  FILLER              PIC X(5).                                
022300         05  W-IDPRODNR-E4       PIC S9(7)  COMP-3.                       
022400         05  W-IDPLKLST-E4       PIC S9(3)  COMP-3.                       
022500     03  W-IDPURAD-X.                                                     
022600         05  W-IDPURAD-E4        PIC S9(5)  COMP-3.                       
022700                                                                          
022800                                                                          
022900*    --- STATUS-KOD FRÅN IMS                                              
023000 01  STATUS-WS                   PIC XX.                                  
023100     88  SEGMENT-FOUND                       VALUE '  '.                  
023200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
023300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
023400                                                                          
023500 01  GOOD-STATUSCODES.                                                    
023600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700                                                                          
023800 01  SSA1                        PIC X(128).                              
023900 01  SSA2                        PIC X(128).                              
024000                                                                          
024100*    --- IMS FUNCTION CODES                                               
024200*01  -COPY W0003                                                          
024300                                                                          
024400                                                                          
024500 01  FILLER               PIC X(16)   VALUE 'WDQ3H1-AREA'.                
024600 01  DLI-IO-WDQ3H1.                                                       
024700*    03  -COPY WDQ3H1                                                     
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3J1'.                      
024900 01  DLI-IO-WDQ3J1.                                                       
025000*    03  -COPY WDQ3J1                                                     
025100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
025200 01  DLI-IO-WDQ301.                                                       
025300*    03  -COPY WDQ301                                                     
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4448'.                    
025500 01  DLI-IO-WDGX4448.                                                     
025600*    03  -COPY WDGX4448                                                   
025700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4488'.                    
025800 01  DLI-IO-WDGX4488.                                                     
025900*    03  -COPY WDGX4488                                                   
026000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4490'.                    
026100 01  DLI-IO-WDGX4490.                                                     
026200*    03  -COPY WDGX4490                                                   
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
026400 01  DLI-IO-WDE401.                                                       
026500*    03  -COPY WDE401                                                     
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
026700 01  DLI-IO-WDE411.                                                       
026800*    03  -COPY WDE411                                                     
026900                                                                          
027000                                                                          
027100 LINKAGE SECTION.                                                         
027200*01  -COPY W0009  -PRE MSG-                                               
027300                                                                          
027400*01  -COPY W0008  -PRE WDQ3H-                                             
027500     05  FILLER                  PIC X.                                   
027600                                                                          
027700*01  -COPY W0008  -PRE WDQ3J-                                             
027800     05  FILLER                  PIC X.                                   
027900                                                                          
028000*01  -COPY W0008  -PRE WDQ3-                                              
028100     05  FILLER                  PIC X.                                   
028200                                                                          
028300*01  -COPY W0008  -PRE 4447-                                              
028400     05  FILLER                  PIC X.                                   
028500                                                                          
028600*01  -COPY W0008  -PRE 4487-                                              
028700     05  FILLER                  PIC X.                                   
028800                                                                          
028900*01  -COPY W0008  -PRE WDE4-                                              
029000     05  FILLER                  PIC X.                                   
029100                                                                          
029200*01  -COPY W0008  -PRE WDE4-R-                                            
029300     05  FILLER                  PIC X.                                   
029400                                                                          
029500 PROCEDURE DIVISION  USING MSG-PCB   WDQ3H-PCB  WDQ3J-PCB WDQ3-PCB        
029600                           4447-PCB  4487-PCB   WDE4-PCB                  
029700                           WDE4-R-PCB.                                    
029800 MAIN SECTION.                                                            
029900                                                                          
030000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
030100                                                                          
030200     IF SUB-KDRC = 0                                                      
030300       PERFORM A-INIT                                                     
030400       PERFORM B-CHECK-KEYS                                               
030500       IF KEYS-OK                                                         
030600         IF REQU-UPDATE                                                   
030700           PERFORM H-UPDATE                                               
030800         END-IF                                                           
030900         IF UPDATE-OK                                                     
031000           PERFORM F-READ-SHOW-INFO                                       
031100         END-IF                                                           
031200       END-IF                                                             
031300       PERFORM S11-MSG-CONV                                               
031400       PERFORM S02-RETURN-RESPONSE                                        
031500     END-IF                                                               
031600                                                                          
031700     PERFORM Z-FINIT                                                      
031800     MOVE ZERO                   TO RETURN-CODE                           
031900     GOBACK                                                               
032000     .                                                                    
032100                                                                          
032200 A-INIT SECTION.                                                          
032300                                                                          
032400     MOVE SPACES                 TO RESP-IDMSG-INFO                       
032500                                    RESP-IDMSG-ERROR                      
032600                                    RESP-IDELMT-ERROR                     
032700     MOVE ZERO                   TO RESP-PL-KVRADER-MAX1                  
032800                                                                          
032900     MOVE FUNCTION UPPER-CASE (REQU-IDDC)                                 
033000                                 TO REQU-IDDC                             
033100     INSPECT REQU-IDPRCPLK                                                
033200                         CONVERTING SMALL-LETTERS                         
033300                                 TO CAPS-LETTERS                          
033400     .                                                                    
033500                                                                          
033600 B-CHECK-KEYS SECTION.                                                    
033700                                                                          
033800     SET KEYS-OK                 TO TRUE                                  
033900                                                                          
034000     IF REQU-IDDC = SPACES OR LOW-VALUES                                  
034100       SET KEYS-WRONG            TO TRUE                                  
034200       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
034300       MOVE 'IDDC'               TO RESP-IDELMT-ERROR                     
034400     END-IF                                                               
034500                                                                          
034600     IF KEYS-OK                                                           
034700       IF REQU-IDANSTNR IS NUMERIC                                        
034800         MOVE REQU-IDANSTNR      TO WS-IDANSTNR                           
034900       ELSE                                                               
035000         SET KEYS-WRONG          TO TRUE                                  
035100         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
035200         MOVE 'IDANSTNR'         TO RESP-IDELMT-ERROR                     
035300       END-IF                                                             
035400     END-IF                                                               
035500                                                                          
035600     IF KEYS-OK                                                           
035700       IF REQU-IDQUEUENR IS NUMERIC                                       
035800         IF REQU-IDQUEUENR > 0 AND                                        
035900            REQU-IDQUEUENR <= 999                                         
036000           STRING '00099' REQU-IDQUEUENR                                  
036100                       DELIMITED BY SIZE                                  
036200                               INTO W-IDUSER-MIN                          
036300           MOVE W-IDUSER-MIN     TO W-IDUSER-MAX                          
036400         ELSE                                                             
036500           MOVE '00099000'       TO W-IDUSER-MIN                          
036600           MOVE '00099999'       TO W-IDUSER-MAX                          
036700         END-IF                                                           
036800       ELSE                                                               
036900         MOVE '00099000'         TO W-IDUSER-MIN                          
037000         MOVE '00099999'         TO W-IDUSER-MAX                          
037100       END-IF                                                             
037200     END-IF                                                               
037300                                                                          
037400     IF KEYS-OK                                                           
037500       IF REQU-KDCALL = 2                                                 
037600         IF REQU-IDPRCPLK = SPACES OR LOW-VALUES                          
037700           SET KEYS-WRONG        TO TRUE                                  
037800           MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                      
037900           MOVE 'IDPRCPLK'       TO RESP-IDELMT-ERROR                     
038000         END-IF                                                           
038100                                                                          
038200         IF REQU-IDLOTNR-PLK IS NUMERIC                                   
038300           CONTINUE                                                       
038400         ELSE                                                             
038500           SET KEYS-WRONG        TO TRUE                                  
038600           MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                      
038700           MOVE 'IDLOTNRPLK'     TO RESP-IDELMT-ERROR                     
038800         END-IF                                                           
038900       END-IF                                                             
039000     END-IF                                                               
039100     .                                                                    
039200                                                                          
039300 H-UPDATE SECTION.                                                        
039400                                                                          
039500     SET CONTINUE-YES            TO TRUE                                  
039600     SET UPDATE-OK               TO TRUE                                  
039700     SET IDLIST-IX               TO 1                                     
039800                                                                          
039900     MOVE LOW-VALUES             TO W-WDQ3J1KY-MIN-X                      
040000     MOVE HIGH-VALUES            TO W-WDQ3J1KY-MAX-X                      
040100     MOVE REQU-IDDC              TO W-IDDC-WDQ3J-MIN                      
040200                                    W-IDDC-WDQ3J-MAX                      
040300                                                                          
040400     IF REQU-KDCALL = 002                                                 
040500       MOVE REQU-IDDC            TO W-Q3H1KY-MIN-IDDC                     
040600                                    W-Q3H1KY-MAX-IDDC                     
040700       MOVE REQU-IDPRCPLK        TO W-Q3H1KY-MIN-IDPRCPLK                 
040800                                    W-Q3H1KY-MAX-IDPRCPLK                 
040900       MOVE REQU-IDLOTNR-PLK     TO W-Q3H1KY-MIN-IDLOTNR-PLK              
041000                                    W-Q3H1KY-MAX-IDLOTNR-PLK              
041100       PERFORM IMS-GU-WDQ3H                                               
041200     ELSE                                                                 
041300       PERFORM IMS-GU-WDQ3J                                               
041400     END-IF                                                               
041500                                                                          
041600     IF SEGMENT-FOUND                                                     
041700       CONTINUE                                                           
041800     ELSE                                                                 
041900       SET CONTINUE-NO           TO TRUE                                  
042000       SET UPDATE-NOT-OK         TO TRUE                                  
042100       MOVE INF-NO-LINES         TO RESP-IDMSG-INFO                       
042200     END-IF                                                               
042300                                                                          
042400     PERFORM UNTIL CONTINUE-NO                                            
042500       PERFORM HAA-UPDATE-WDQ3                                            
042600       PERFORM HAB-UPDATE-PRODTAB                                         
042700       PERFORM HAC-UPDATE-WDE4                                            
042800       IF REQU-KDCALL = 002                                               
042900         MOVE SEQH-IDPRODNR      TO WS-IDPRODNR (IDLIST-IX)               
043000         MOVE SEQH-IDPLKLST      TO WS-IDPLKLST (IDLIST-IX)               
043100       ELSE                                                               
043200         MOVE SEQJ-DAUTSKR       TO WS-PREV-DAUTSKR                       
043300         MOVE SEQJ-TIUTSTID      TO WS-PREV-TIUTSTID                      
043400         MOVE SEQJ-IDPRCPLK      TO WS-PREV-IDPRCPLK                      
043500         MOVE SEQJ-IDLOTNR-PLK   TO WS-PREV-IDLOTNR-PLK                   
043600         MOVE SEQJ-IDPRODNR      TO WS-IDPRODNR (IDLIST-IX)               
043700         MOVE SEQJ-IDPLKLST      TO WS-IDPLKLST (IDLIST-IX)               
043800       END-IF                                                             
043900       SET IDLIST-IX          UP BY 1                                     
044000       IF REQU-KDCALL = 002                                               
044100         PERFORM IMS-GN-WDQ3H                                             
044200       ELSE                                                               
044300         PERFORM IMS-GN-WDQ3J                                             
044400       END-IF                                                             
044500       IF SEGMENT-FOUND                                                   
044600         IF REQU-KDCALL = 002                                             
044700           CONTINUE                                                       
044800         ELSE                                                             
044900           IF SEQJ-DAUTSKR   = WS-PREV-DAUTSKR     AND                    
045000              SEQJ-TIUTSTID  = WS-PREV-TIUTSTID    AND                    
045100              SEQJ-IDPRCPLK  = WS-PREV-IDPRCPLK    AND                    
045200              SEQJ-IDLOTNR-PLK = WS-PREV-IDLOTNR-PLK                      
045300             CONTINUE                                                     
045400           ELSE                                                           
045500             SET CONTINUE-NO     TO TRUE                                  
045600           END-IF                                                         
045700         END-IF                                                           
045800       ELSE                                                               
045900         SET CONTINUE-NO         TO TRUE                                  
046000       END-IF                                                             
046100     END-PERFORM                                                          
046200     SET WS-IDLIST-TAB-MAX       TO IDLIST-IX                             
046300     COMPUTE WS-IDLIST-TAB-MAX    = WS-IDLIST-TAB-MAX - 1                 
046400     .                                                                    
046500                                                                          
046600 HAA-UPDATE-WDQ3 SECTION.                                                 
046700                                                                          
046800     IF REQU-KDCALL = 002                                                 
046900       MOVE SEQH-IDWDQ301        TO W-WDQ301KY                            
047000     ELSE                                                                 
047100       MOVE SEQJ-IDWDQ301        TO W-WDQ301KY                            
047200     END-IF                                                               
047300     PERFORM IMS-GHU-WDQ301                                               
047400     MOVE WS-IDANSTNR            TO ODEL-IDUSER                           
047500     PERFORM IMS-REPL-WDQ301                                              
047600     .                                                                    
047700                                                                          
047800 HAB-UPDATE-PRODTAB SECTION.                                              
047900                                                                          
048000     MOVE REQU-IDDC              TO W-4447-IDDC                           
048100     MOVE ODEL-IDPRC             TO W-4448-IDPRC                          
048200     PERFORM IMS-GU-WDGX4448                                              
048300                                                                          
048400     MOVE REQU-IDDC              TO W-4487-IDDC                           
048500     MOVE 4448-KDPRCGRP          TO W-4488-KDPRCGRP                       
048600     PERFORM IMS-GU-WDGX4488                                              
048700                                                                          
048800     IF SEGMENT-FOUND                                                     
048900       MOVE ODEL-DARFS           TO W-4490-DARFS                          
049000       MOVE ODEL-IDPRODNR        TO W-4490-IDPRODNR                       
049100       MOVE ODEL-IDPLKLST        TO W-4490-IDPLKLST                       
049200       PERFORM IMS-GHNP-WDGX4490                                          
049300                                                                          
049400       IF SEGMENT-FOUND                                                   
049500         MOVE WS-IDANSTNR        TO 4490-IDUSER                           
049600         PERFORM IMS-REPL-WDGX4490                                        
049700       END-IF                                                             
049800     END-IF                                                               
049900     .                                                                    
050000                                                                          
050100 HAC-UPDATE-WDE4 SECTION.                                                 
050200                                                                          
050300     MOVE SPACES                 TO W-WDE401KY-X                          
050400     MOVE ODEL-IDDISTR           TO W-IDDISTR-E4                          
050500     MOVE ODEL-IDKUNDNR          TO W-IDKUNDNR-E4                         
050600     MOVE ODEL-IDORDNR7          TO W-IDORDNR5-E4                         
050700     MOVE ODEL-IDPRODNR          TO W-IDPRODNR-E4                         
050800     MOVE ODEL-IDPLKLST          TO W-IDPLKLST-E4                         
050900     PERFORM IMS-GHU-WDE401                                               
051000     MOVE WS-IDANSTNR            TO KORD-IDUSER                           
051100     PERFORM IMS-REPL-WDE401                                              
051200     .                                                                    
051300                                                                          
051400 F-READ-SHOW-INFO SECTION.                                                
051500                                                                          
051600     SET REKY-MISSING            TO TRUE                                  
051700     MOVE 'PICKING-LABEL2'       TO REKY-IDOUTTYPE-KEY                    
051800     MOVE ALL '%'                TO REKY-IDOUTREC-KEY                     
051900     MOVE REQU-IDDC              TO REKY-IDOUTREC-KEY (1:2)               
052000     MOVE ZEROES                 TO REKY-TIREGDAT-MIN-KEY                 
052100     MOVE 999999                 TO REKY-TIREGDAT-MAX-KEY                 
052200     MOVE ZEROES                 TO REKY-TIKLOCK-MIN-KEY                  
052300     MOVE 99999999               TO REKY-TIKLOCK-MAX-KEY                  
052400     MOVE ZEROES                 TO REKY-IDLOPNR-MIN-KEY                  
052500     MOVE 999                    TO REKY-IDLOPNR-MAX-KEY                  
052600                                                                          
052700     PERFORM                                                              
052800     VARYING IDLIST-IX FROM 1 BY 1                                        
052900       UNTIL REKY-FOUND OR                                                
053000             IDLIST-IX > WS-IDLIST-TAB-MAX                                
053100       MOVE WS-IDLIST (IDLIST-IX)                                         
053200                                 TO REKY-IDLIST-KEY                       
053300                                                                          
053400       CALL WZ04REKY            USING REKY-WZ04REKY                       
053500                                                                          
053600       IF REKY-KVRADER > 0                                                
053700         SET REKY-FOUND          TO TRUE                                  
053800         PERFORM FA-RETRIEVE-DATA-FROM-DAP                                
053900       END-IF                                                             
054000     END-PERFORM                                                          
054100                                                                          
054200     IF REKY-MISSING                                                      
054300       SET CONTINUE-NO           TO TRUE                                  
054400       SET UPDATE-NOT-OK         TO TRUE                                  
054500       MOVE INF-NO-LINES         TO RESP-IDMSG-INFO                       
054600       PERFORM IMS-ROLLBACK                                               
054700     END-IF                                                               
054800     .                                                                    
054900                                                                          
055000 FA-RETRIEVE-DATA-FROM-DAP SECTION.                                       
055100                                                                          
055200     SET CONTINUE-YES            TO TRUE                                  
055300     MOVE REKY-IDOUTTYPE (1)     TO REDA-IDOUTTYPE-KEY                    
055400     MOVE REKY-IDOUTREC  (1)     TO REDA-IDOUTREC-KEY                     
055500     MOVE REKY-IDLIST    (1)     TO REDA-IDLIST-KEY                       
055600     MOVE REKY-TIREGDAT  (1)     TO REDA-TIREGDAT-KEY                     
055700     MOVE REKY-TIKLOCK   (1)     TO REDA-TIKLOCK-KEY                      
055800     MOVE REKY-IDLOPNR   (1)     TO REDA-IDLOPNR-KEY                      
055900     MOVE ZEROES                 TO REDA-KVPOST-LAST                      
056000     PERFORM UNTIL CONTINUE-NO                                            
056100       CALL WZ04REDA          USING REDA-WZ04REDA                         
056200       IF REDA-IDMSG = SPACES                                             
056300         SET CONTINUE-NO         TO TRUE                                  
056400         PERFORM FAA-MOVE-DATA                                            
056500       ELSE                                                               
056600         IF REDA-IDMSG = INF-MORE-LINES-EXIST                             
056700           PERFORM FAA-MOVE-DATA                                          
056800         ELSE                                                             
056900           MOVE REDA-IDMSG       TO RESP-IDMSG-ERROR                      
057000           MOVE REDA-IDELMT-ERROR                                         
057100                                 TO RESP-IDELMT-ERROR                     
057200           SET CONTINUE-NO       TO TRUE                                  
057300         END-IF                                                           
057400       END-IF                                                             
057500     END-PERFORM                                                          
057600     .                                                                    
057700                                                                          
057800 FAA-MOVE-DATA SECTION.                                                   
057900                                                                          
058000     PERFORM                                                              
058100     VARYING IX1 FROM 1 BY 1                                              
058200       UNTIL IX1 > REDA-KVRADER                                           
058300       IF  REDA-TEOUTDATA-DATA (IX1) (1:1) = '1'                          
058400         MOVE REDA-TEOUTDATA-DATA (IX1)                                   
058500                (1:REDA-TEOUTDATA-LEN (IX1) )                             
058600                                 TO LINE-WL01321                          
058700         PERFORM FAAA-CHECK-REPORTED-STATUS                               
058800         IF NOT-REPORTED-LINE                                             
058900           ADD +1                TO IX2                                   
059000           MOVE LINE-IDAFPRCD    TO RESP-PL-IDAFPRCD    (IX2)             
059100           MOVE LINE-ADLAGOMR    TO RESP-PL-ADLAGOMR    (IX2)             
059200           MOVE LINE-ADGANG      TO RESP-PL-ADGANG      (IX2)             
059300                                                                          
059400           MOVE LINE-ADPLATSNR   TO WS-ADPLATSNR                          
059500           MOVE LINE-ADPLNIV-LEFT TO WS-ADPLNIV-LEFT                      
059600           MOVE LINE-ADPLNIV-RIGHT TO WS-ADPLNIV-RIGHT                    
059700           MOVE WS-ADPLATS       TO RESP-PL-ADPLATS     (IX2)             
059800                                                                          
059900           MOVE LINE-BERADREF    TO RESP-PL-BERADREF    (IX2)             
060000           MOVE LINE-BEART       TO RESP-PL-BEART       (IX2)             
060100           MOVE LINE-FLAKPLOC    TO RESP-PL-FLAKPLOC    (IX2)             
060200           MOVE FUNCTION TRIM (LINE-IDARTNR)                              
060300                                 TO RESP-PL-IDARTNR     (IX2)             
060400           MOVE LINE-IDBORD      TO RESP-PL-IDBORD      (IX2)             
060500           MOVE LINE-IDDC        TO RESP-PL-IDDC        (IX2)             
060600           MOVE FUNCTION TRIM (LINE-IDDISTR)                              
060700                                 TO RESP-PL-IDDISTR     (IX2)             
060800           MOVE FUNCTION TRIM (LINE-IDKUNDNR)                             
060900                                 TO RESP-PL-IDKUNDNR    (IX2)             
061000           MOVE LINE-IDKUNDRF(3:5)                                        
061100                                 TO RESP-PL-IDORDNR7  (IX2)               
061200           MOVE LINE-IDLOPNR-ORD TO RESP-PL-IDLOPNR-ORD (IX2)             
061300           MOVE LINE-IDLOPNR-PL  TO RESP-PL-IDLOPNR-PL  (IX2)             
061400           MOVE FUNCTION TRIM (LINE-IDPLKLST)                             
061500                                 TO RESP-PL-IDPLKLST    (IX2)             
061600           MOVE LINE-IDPRC       TO RESP-PL-IDPRC       (IX2)             
061700           MOVE FUNCTION TRIM (LINE-IDPRODNR)                             
061800                                 TO RESP-PL-IDPRODNR    (IX2)             
061900           MOVE FUNCTION TRIM (LINE-IDRADNR)                              
062000                                 TO RESP-PL-IDRADNR     (IX2)             
062100           MOVE LINE-IDPSN       TO RESP-PL-IDPSN       (IX2)             
062200           MOVE FUNCTION TRIM (LINE-IDSPECEMB)                            
062300                                 TO RESP-PL-IDSPECEMB   (IX2)             
062400           MOVE LINE-IDZON       TO RESP-PL-IDZON       (IX2)             
062500           MOVE FUNCTION TRIM (LINE-KDARTHNT)                             
062600                                 TO RESP-PL-KDARTHNT    (IX2)             
062700           MOVE LINE-KDARTURS    TO RESP-PL-KDARTURS    (IX2)             
062800           MOVE LINE-KDEMBAL     TO RESP-PL-KDEMBAL     (IX2)             
062900           MOVE LINE-KDFARLIG    TO RESP-PL-KDFARLIG    (IX2)             
063000           MOVE LINE-KDORDKL     TO RESP-PL-KDORDKL     (IX2)             
063100           MOVE LINE-KDSORT      TO RESP-PL-KDSORT      (IX2)             
063200           MOVE FUNCTION TRIM (LINE-KVAVBART)                             
063300                                 TO RESP-PL-KVAVBART    (IX2)             
063400           MOVE LINE-IDLEVART    TO RESP-PL-IDLEVART    (IX2)             
063500           MOVE LINE-IDKOLLI     TO RESP-PL-IDKOLLI     (IX2)             
063600           MOVE LINE-KDKOLLI     TO RESP-PL-KDKOLLI     (IX2)             
063700           MOVE LINE-BELAGINS-GRP TO RESP-PL-BELAGINS-GRP(IX2)            
063800           MOVE LINE-BEFDKRAV    TO RESP-PL-BEFDKRAV    (IX2)             
063900         END-IF                                                           
064000       ELSE                                                               
064100         IF REDA-TEOUTDATA-DATA (IX1) (1:1) = '2'                         
064200           MOVE REDA-TEOUTDATA-DATA (IX1)                                 
064300                (1:REDA-TEOUTDATA-LEN (IX1) )                             
064400                                 TO TOTAL-WL01322                         
064500           MOVE TOTAL-IDAFPRCD   TO RESP-PL-IDAFPRCD-TOT                  
064600           MOVE TOTAL-IDLOPNR-ORD                                         
064700                                 TO RESP-PL-IDLOPNR-ORD-TOT               
064800           MOVE TOTAL-IDLOPNR-PL TO RESP-PL-IDLOPNR-PL-TOT                
064900           MOVE TOTAL-IDPRC      TO RESP-PL-IDPRC-TOT                     
065000*          MOVE TOTAL-KVRADER    TO RESP-PL-KVRADER                       
065100           MOVE IX2              TO RESP-PL-KVRADER-MAX1                  
065200           MOVE TOTAL-IDTRPTNR   TO RESP-PL-IDTRPTNR                      
065300           MOVE TOTAL-TIRFSDAT   TO RESP-PL-TIRFSDAT                      
065400           MOVE TOTAL-TIRFSTID   TO RESP-PL-TIRFSTID                      
065500         END-IF                                                           
065600       END-IF                                                             
065700     END-PERFORM                                                          
065800     .                                                                    
065900                                                                          
066000 FAAA-CHECK-REPORTED-STATUS SECTION.                                      
066100                                                                          
066200     SET NOT-REPORTED-LINE       TO TRUE                                  
066300     MOVE SPACES                 TO W-WDE401KY-X                          
066400                                    W-IDPURAD-X                           
066500     MOVE FUNCTION TRIM (LINE-IDDISTR)                                    
066600                                 TO W-IDDISTR-E4                          
066700     MOVE FUNCTION TRIM (LINE-IDKUNDNR)                                   
066800                                 TO W-IDKUNDNR-E4                         
066900     MOVE LINE-IDKUNDRF(3:5)                                              
067000                                 TO W-IDORDNR5-E4                         
067100     MOVE FUNCTION TRIM (LINE-IDPRODNR)                                   
067200                                 TO W-IDPRODNR-E4                         
067300     MOVE FUNCTION TRIM (LINE-IDPLKLST)                                   
067400                                 TO W-IDPLKLST-E4                         
067500     MOVE FUNCTION TRIM (LINE-IDRADNR)                                    
067600                                 TO W-IDPURAD-E4                          
067700     PERFORM IMS-GU-WDE411                                                
067800     IF SEGMENT-FOUND                                                     
067900       IF ORAD-KDRADSTA > 3                                               
068000         SET REPORTED-LINE       TO TRUE                                  
068100       END-IF                                                             
068200     END-IF                                                               
068300     .                                                                    
068400                                                                          
068500 Z-FINIT SECTION.                                                         
068600                                                                          
068700     CONTINUE                                                             
068800     .                                                                    
068900                                                                          
069000*    --- DISPATCHER SECTIONS                                              
069100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
069200                                                                          
069300     MOVE 'GETARG'               TO SUB-KDFUNC                            
069400     MOVE 'CARPARTS.PULS.PICKINGLIST'        TO SUB-ADDISPABS             
069500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
069600                                                                          
069700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
069800                                                                          
069900     COMPUTE KDRC-NUM             = SUB-KDRC                              
070000                                                                          
070100     IF SUB-KDRC > 0                                                      
070200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
070300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
070400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
070500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
070600     END-IF                                                               
070700     .                                                                    
070800                                                                          
070900 S02-RETURN-RESPONSE SECTION.                                             
071000                                                                          
071100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
071200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
071300                                                                          
071400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
071500                                                                          
071600     COMPUTE KDRC-NUM             = SUB-KDRC                              
071700                                                                          
071800     IF SUB-KDRC > 0                                                      
071900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
072000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
072100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
072200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
072300     END-IF                                                               
072400     .                                                                    
072500                                                                          
072600 S11-MSG-CONV SECTION.                                                    
072700     MOVE SPACES                  TO RESP-MESSAGES (1)                    
072800                                     RESP-MESSAGES (2)                    
072900     MOVE 1                       TO MSG-IX                               
073000*    REQUEST OK                                                           
073100     MOVE 200                     TO RESP-KDSTATUS-API                    
073200     IF RESP-IDMSG-INFO > SPACE                                           
073300       MOVE SPACES                TO MSG-CONV-AREA                        
073400       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
073500       CALL WMSGCONV           USING MSG-CONV-AREA                        
073600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
073700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
073800       ADD 1                      TO MSG-IX                               
073900     END-IF                                                               
074000     IF RESP-IDMSG-ERROR > SPACE                                          
074100*      BAD REQUEST                                                        
074200       MOVE 400                   TO RESP-KDSTATUS-API                    
074300       MOVE SPACES                TO MSG-CONV-AREA                        
074400       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
074500       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
074600       CALL WMSGCONV           USING MSG-CONV-AREA                        
074700       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
074800       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
074900     END-IF                                                               
075000     .                                                                    
075100                                                                          
075200 IMS-GU-WDQ3J SECTION.                                                    
075300                                                                          
075400     STRING 'WDQ3J1  (WDQ3J1KY>=' W-WDQ3J1KY-MIN-X                        
075500                    '&WDQ3J1KY<=' W-WDQ3J1KY-MAX-X                        
075600                    '&IDUSER  >=' W-IDUSER-MIN-X                          
075700                    '&IDUSER  <=' W-IDUSER-MAX-X   ')'                    
075800             DELIMITED BY SIZE INTO SSA1                                  
075900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
076000     CALL CBLTDLI             USING GU                                    
076100                                    WDQ3J-PCB                             
076200                                    DLI-IO-WDQ3J1                         
076300                                    SSA1                                  
076400     MOVE WDQ3J-STATUS-CODE      TO STATUS-WS                             
076500     PERFORM IMS-STATUSCHECK                                              
076600     .                                                                    
076700                                                                          
076800 IMS-GN-WDQ3J SECTION.                                                    
076900                                                                          
077000     STRING 'WDQ3J1  (WDQ3J1KY>=' W-WDQ3J1KY-MIN-X                        
077100                    '&WDQ3J1KY<=' W-WDQ3J1KY-MAX-X                        
077200                    '&IDUSER  >=' W-IDUSER-MIN-X                          
077300                    '&IDUSER  <=' W-IDUSER-MAX-X   ')'                    
077400             DELIMITED BY SIZE INTO SSA1                                  
077500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
077600     CALL CBLTDLI             USING GN                                    
077700                                    WDQ3J-PCB                             
077800                                    DLI-IO-WDQ3J1                         
077900                                    SSA1                                  
078000     MOVE WDQ3J-STATUS-CODE      TO STATUS-WS                             
078100     PERFORM IMS-STATUSCHECK                                              
078200     .                                                                    
078300                                                                          
078400 IMS-GU-WDQ3H SECTION.                                                    
078500                                                                          
078600     STRING 'WDQ3H1  (WDQ3H1KY>=' W-WDQ3H1KY-MIN-X                        
078700                    '&WDQ3H1KY<=' W-WDQ3H1KY-MAX-X ')'                    
078800             DELIMITED BY SIZE INTO SSA1                                  
078900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
079000     CALL CBLTDLI USING GU WDQ3H-PCB DLI-IO-WDQ3H1 SSA1                   
079100     MOVE WDQ3H-STATUS-CODE      TO STATUS-WS                             
079200     PERFORM IMS-STATUSCHECK                                              
079300     .                                                                    
079400                                                                          
079500 IMS-GN-WDQ3H SECTION.                                                    
079600                                                                          
079700     STRING 'WDQ3H1  (WDQ3H1KY>=' W-WDQ3H1KY-MIN-X                        
079800                    '&WDQ3H1KY<=' W-WDQ3H1KY-MAX-X ')'                    
079900             DELIMITED BY SIZE INTO SSA1                                  
080000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
080100     CALL CBLTDLI USING GN WDQ3H-PCB DLI-IO-WDQ3H1 SSA1                   
080200     MOVE WDQ3H-STATUS-CODE      TO STATUS-WS                             
080300     PERFORM IMS-STATUSCHECK                                              
080400     .                                                                    
080500                                                                          
080600 IMS-GHU-WDQ301 SECTION.                                                  
080700                                                                          
080800     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
080900             DELIMITED BY SIZE INTO SSA1                                  
081000     MOVE '  '                   TO GOOD-STATUSCODES                      
081100     CALL CBLTDLI             USING GHU                                   
081200                                    WDQ3-PCB                              
081300                                    DLI-IO-WDQ301                         
081400                                    SSA1                                  
081500     MOVE WDQ3-STATUS-CODE       TO STATUS-WS                             
081600     PERFORM IMS-STATUSCHECK                                              
081700     .                                                                    
081800                                                                          
081900 IMS-REPL-WDQ301 SECTION.                                                 
082000                                                                          
082100     MOVE '  '                   TO GOOD-STATUSCODES                      
082200     CALL CBLTDLI             USING REPL                                  
082300                                    WDQ3-PCB                              
082400                                    DLI-IO-WDQ301                         
082500     MOVE WDQ3-STATUS-CODE       TO STATUS-WS                             
082600     PERFORM IMS-STATUSCHECK                                              
082700     .                                                                    
082800                                                                          
082900 IMS-GU-WDGX4448 SECTION.                                                 
083000                                                                          
083100     MOVE SPACES                 TO GOOD-STATUSCODES                      
083200     STRING 'WDR101  (WDGXKEY  =' W-4447-X ')'                            
083300             DELIMITED BY SIZE INTO SSA1                                  
083400     STRING 'WDR160  (WDGXKEY  =' W-4448-X ')'                            
083500             DELIMITED BY SIZE INTO SSA2                                  
083600     CALL CBLTDLI             USING GU                                    
083700                                    4447-PCB                              
083800                                    DLI-IO-WDGX4448                       
083900                                    SSA1                                  
084000                                    SSA2                                  
084100     MOVE 4447-STATUS-CODE       TO STATUS-WS                             
084200     PERFORM IMS-STATUSCHECK                                              
084300     .                                                                    
084400                                                                          
084500 IMS-GU-WDGX4488 SECTION.                                                 
084600                                                                          
084700     MOVE SPACES                 TO GOOD-STATUSCODES                      
084800     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
084900             DELIMITED BY SIZE INTO SSA1                                  
085000     STRING 'WDGX4488(KDPRCGRP =' W-4488-X ')'                            
085100             DELIMITED BY SIZE INTO SSA2                                  
085200     CALL CBLTDLI             USING GU                                    
085300                                    4487-PCB                              
085400                                    DLI-IO-WDGX4488                       
085500                                    SSA1                                  
085600                                    SSA2                                  
085700     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
085800     PERFORM IMS-STATUSCHECK                                              
085900     .                                                                    
086000                                                                          
086100 IMS-GHNP-WDGX4490 SECTION.                                               
086200                                                                          
086300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
086400     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
086500             DELIMITED BY SIZE INTO SSA1                                  
086600     CALL CBLTDLI             USING GHNP                                  
086700                                    4487-PCB                              
086800                                    DLI-IO-WDGX4490                       
086900                                    SSA1                                  
087000     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
087100     PERFORM IMS-STATUSCHECK                                              
087200     .                                                                    
087300                                                                          
087400 IMS-REPL-WDGX4490 SECTION.                                               
087500                                                                          
087600     MOVE SPACES                 TO GOOD-STATUSCODES                      
087700     CALL CBLTDLI             USING REPL                                  
087800                                    4487-PCB                              
087900                                    DLI-IO-WDGX4490                       
088000     MOVE 4487-STATUS-CODE       TO STATUS-WS                             
088100     PERFORM IMS-STATUSCHECK                                              
088200     .                                                                    
088300                                                                          
088400 IMS-GU-WDE411 SECTION.                                                   
088500                                                                          
088600     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
088700             DELIMITED BY SIZE INTO SSA1                                  
088800     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
088900             DELIMITED BY SIZE INTO SSA2                                  
089000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
089100     CALL CBLTDLI             USING GU                                    
089200                                    WDE4-R-PCB                            
089300                                    DLI-IO-WDE411                         
089400                                    SSA1 SSA2                             
089500     MOVE WDE4-R-STATUS-CODE       TO STATUS-WS                           
089600     PERFORM IMS-STATUSCHECK                                              
089700     .                                                                    
089800                                                                          
089900 IMS-GHU-WDE401 SECTION.                                                  
090000                                                                          
090100     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
090200             DELIMITED BY SIZE INTO SSA1                                  
090300     MOVE SPACES                 TO GOOD-STATUSCODES                      
090400     CALL CBLTDLI             USING GHU                                   
090500                                    WDE4-PCB                              
090600                                    DLI-IO-WDE401                         
090700                                    SSA1                                  
090800     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
090900     PERFORM IMS-STATUSCHECK                                              
091000     .                                                                    
091100                                                                          
091200 IMS-REPL-WDE401 SECTION.                                                 
091300                                                                          
091400     MOVE '  '                   TO GOOD-STATUSCODES                      
091500     CALL CBLTDLI             USING REPL                                  
091600                                    WDE4-PCB                              
091700                                    DLI-IO-WDE401                         
091800     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
091900     PERFORM IMS-STATUSCHECK                                              
092000     .                                                                    
092100                                                                          
092200 IMS-ROLLBACK    SECTION.                                                 
092300     MOVE '  '                   TO GOOD-STATUSCODES                      
092400     CALL CBLTDLI             USING ROLB    MSG-PCB                       
092500     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
092600     PERFORM IMS-STATUSCHECK                                              
092700     .                                                                    
092800                                                                          
092900 IMS-STATUSCHECK SECTION.                                                 
093000                                                                          
093100     SET STATUS-IX               TO 1                                     
093200     SEARCH GOOD-STATUS                                                   
093300       AT END                                                             
093400         CALL FELLOG                                                      
093500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
093600         CONTINUE                                                         
093700     END-SEARCH                                                           
093800     .                                                                    
093900                                                                          
094000                                                                          
