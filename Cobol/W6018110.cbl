000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6018110.                                                
000300 AUTHOR.         JOHAN LINDKVIST / ARCHANA BHAT.                          
000400 DATE-WRITTEN.   99/07/13 / MAY 2012.                                     
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VISAR OCH/ELLER UPPDATERAR FÖRPACKNINGSTYP                       
000900*                                                                         
001000*        THE PROGRAM UPDATES   WLARTC (WDK6)                              
001100*        THE PROGRAM READS     WLBENA (WDD3)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        REQU:   W60181I1                                                 
001500*                                                                         
001600* UTDATA.                                                                 
001700*        RESP:   W60181O1                                                 
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W6018110'.            
002700                                                                          
002800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
002900 77  DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
003000 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
003100 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
003200 77  P-TO-P-KVLL-CPYTXT          PIC S9(4)           COMP SYNC.           
003300 77  W-TEBEFT-79                 PIC X(79)   VALUE SPACE.                 
003400 77  W-TEBEFT02-79               PIC X(79)   VALUE SPACE.                 
003500 77  W-IMS-SECTION               PIC X(79)   VALUE SPACE.                 
003600 77  WS-CP-UNICODE               PIC X(4)    VALUE 'UTF8'.                
003700 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
003800 77  WS-EXTERNAL                 PIC X.                                   
003900 77  WS-CHINA-UPD                PIC X.                                   
004000 77  WS-US-UPD                   PIC X.                                   
004100 77  WS-IDLAND-SAVE              PIC X(2).                                
004110 77  WS-FLIDDC-KEY               PIC X       VALUE 'J'.                   
004200                                                                          
004300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005000                                                                          
005100 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
005200     88  INDATA-OK                           VALUE 'Y'.                   
005300     88  INDATA-WRONG                        VALUE 'N'.                   
005400                                                                          
005500 77  BEFT-SW                     PIC X       VALUE 'N'.                   
005600     88  BEFT-RETT                           VALUE 'Y'.                   
005700                                                                          
005800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005900     88  KEYS-OK                             VALUE 'Y'.                   
006000     88  KEYS-WRONG                          VALUE 'N'.                   
006100                                                                          
006200*    --- WORKING STORAGE FIELDS                                           
006300                                                                          
006400 01  HIST-IX                     PIC 99      VALUE ZERO.                  
006500 01  ALL-SPACE.                                                           
006600     03 FILLER                   PIC X(80)   VALUE SPACE.                 
006700 01  ALL-PLUS.                                                            
006800     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
006900 01  ALL-UTF8-SPACE.                                                      
007000     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
007100 01  ALL-UTF8-PLUS.                                                       
007200     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
007300                                                                          
007400                                                                          
007500 01  WS-TID                      PIC S9(9).                               
007600 01  WS-DAREGDAT                 PIC 9(8).                                
007700 01  WS-DAREGDAT-2 REDEFINES WS-DAREGDAT.                                 
007800     03 FILLER                   PIC 9(2).                                
007900     03 WS-TIREGDAT              PIC 9(6).                                
008000                                                                          
008100     EJECT                                                                
008200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008300 01  GENERAL-SUBPROGRAMS.                                                 
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
008700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
008800     EJECT                                                                
008900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009000*01 -COPY WMEDAREA                                                        
009100     SKIP3                                                                
009200 01  MESSAGE-CODES.                                                       
009300   02 ERROR-CODES.                                                        
009400     03  ERR-UPDATE-NOT-DONE        PIC X(3)    VALUE '004'.              
009500     03  ERR-PRESS-PF23             PIC X(3)    VALUE '013'.              
009600     03  ERR-PF11-AND-NO-DATA       PIC X(3)    VALUE '014'.              
009700     03  ERR-CORR-HILITE-FLDS       PIC X(3)    VALUE '020'.              
009800     03  ERR-WRONG-KEY              PIC X(3)    VALUE '022'.              
009900     03  ERR-PART-MISSING           PIC X(3)    VALUE '025'.              
010000     03  ERR-REFILL-PART            PIC X(3)    VALUE '386'.              
010100   02 INFO-CODES.                                                         
010200     03  INF-UPDATE-DONE            PIC X(3)    VALUE '001'.              
010300     03  INF-FOR-MORE-INFORMATION   PIC X(3)    VALUE '011'.              
010400     03  INF-LAST-PAGE              PIC X(3)    VALUE '012'.              
010500     03  INF-PRESS-PF11             PIC X(3)    VALUE '013'.              
010600                                                                          
010700*                                                                         
010800*-- SÄTT 2213-IDDC = 71 OM KINA UPPDATERAR BEFT, ANNARS DC=11.            
010900   03    W-WDGX2213-X.                                                    
011000     05  FILLER          PIC X(4)    VALUE '2213'.                        
011100     05  W-IDDC-2213     PIC X(2)    VALUE '  '.                          
011200     05  FILLER          PIC X(24)   VALUE LOW-VALUE.                     
011300*                                                                         
011400     03  W-WDGXKEY-6317-X.                                                
011500         05  W-IDHTYP            PIC X(4)    VALUE '6317'.                
011600         05  W-6317-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
011700     03  W-BEFT-X.                                                        
011800         05  W-BEFT              PIC S9(3)   VALUE ZERO COMP-3.           
011900     EJECT                                                                
012000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012100 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
012200     SKIP3                                                                
012300*01  -COPY WMSGAREA                                                       
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012600     SKIP3                                                                
012700*01  -COPY WMFSAREA                                                       
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
013000     SKIP3                                                                
013100 01  KOM-MSG-IO-AREA.                                                     
013200*03  -COPY WMSGKOM                                                        
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'DC CODES   '.         
013500     SKIP3                                                                
013600*   -COPY WWDC99                                                          
013700*   -COPY WWLNDKON                                                        
013800*   -COPY WWDCKONS                                                        
013900 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8-AREA'.        
014000*01  -COPY WTRAUTF8                                                       
014100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014200*                                                                         
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600 01  KEYS-TO-DLI.                                                         
014700     03  W-IDARTNR-X.                                                     
014800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014900     03  W-IDSKYLT-X.                                                     
015000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
015100     03  W-IDLAND-X.                                                      
015200         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
015300     03  W-IDDC-X.                                                        
015400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015500     03  W-WDT311KY-X.                                                    
015600         05  W-DAREGDAT-9KOMPL   PIC 9(8)    VALUE ZERO.                  
015700         05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO COMP-3.           
015800         05  W-IDLAND-KY         PIC X(2)    VALUE SPACE.                 
015900     03  W-IDDC-B6-X.                                                     
016000         05 W-IDDC-B6            PIC X(2).                                
016100     SKIP2                                                                
016200 01      P-TO-P-SW.                                                       
016300                                                                          
016400  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
016500  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
016600  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
016700  02     P-TO-P-KDTRANS          PIC X(8).                                
016800  02     P-TO-P-IDTRANS          PIC X(4).                                
016900  02     P-TO-P-KDMFSFOR         PIC X(1).                                
017000  02     P-TO-P-DATA             PIC X(1000).                             
017100     SKIP2                                                                
017200 01      FILLER                  PIC X(24)   VALUE                        
017300                                 'MOD619B-MID-W6I19B01'.                  
017400     SKIP2                                                                
017500     -COPY W6I19B01 -PRE MOD619B-                                         
017600     EJECT                                                                
017700*    --- STATUS-KOD FRÅN IMS                                              
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FOUND                       VALUE '  '.                  
018000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018200     SKIP2                                                                
018300 01  GOOD-STATUSCODES.                                                    
018400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600 01  SSA1                        PIC X(64).                               
018700 01  SSA2                        PIC X(64).                               
018800     EJECT                                                                
018900*    --- IMS FUNCTION CODES                                               
019000*01  -COPY W0003                                                          
019100     EJECT                                                                
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300                                                                          
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
019500 01  DLI-IO-WLARTC01.                                                     
019600*    03  -COPY WDK601  -PRE ARTC-                                         
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
019800 01  DLI-IO-WLARTC11.                                                     
019900*    03  -COPY WDK611  -PRE ARTC-                                         
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
020100 01  DLI-IO-WLBENA11.                                                     
020200*    03  -COPY WDD311  -PRE BENA-                                         
020300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
020400 01  DLI-IO-WDGX2214.                                                     
020500*    03  -COPY WDGX2214                                                   
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301  '.                    
020700 01  DLI-IO-WDT301.                                                       
020800*    03  -COPY WDT301                                                     
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311  '.                    
021000 01  DLI-IO-WDT311.                                                       
021100*    03  -COPY WDT311                                                     
021200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
021300 01  DLI-IO-WDK711.                                                       
021400*    03  -COPY WDK711                                                     
021500     EJECT                                                                
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
021700 01  DLI-IO-WDK712.                                                       
021800*    03  -COPY WDK712                                                     
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
022100 01   DLI-IO-AREA-B601.                                                   
022200*     03  -COPY WDB601                                                    
022300     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500 01  REQU-AREA.                                                           
022600*    03 -COPY WZ01REQU                                                    
022700*    03 -COPY W60181I1                                                    
022800     EJECT                                                                
022900 01  RESP-AREA.                                                           
023000*    03 -COPY WZ01RESP                                                    
023100*    03 -COPY W60181O1                                                    
023200     EJECT                                                                
023300 01  MAX-KVRADER                 PIC S9(4) COMP.                          
023400*                                                                         
023500*01  -COPY W0009  -PRE MSG-                                               
023600     EJECT                                                                
023700*01  -COPY W0009  -PRE DISP-                                              
023800     EJECT                                                                
023900*01  -COPY W0008  -PRE ARTC-                                              
024000     05  FILLER                  PIC X.                                   
024100                                                                          
024200*01  -COPY W0008  -PRE BENA-                                              
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE XXBI-                                              
024600     05 FILLER                   PIC X.                                   
024700*01  -COPY W0008  -PRE WDT3-                                              
024800     05 FILLER                   PIC X.                                   
024900*01  -COPY W0008  -PRE WDK7-                                              
025000     05 FILLER                   PIC X.                                   
025100*01  -COPY W0008  -PRE WDB6-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*    PCB'ER FÖR SUBPGM                                                    
025500                                                                          
025600 01 KOM-KOMA-PCB                 PIC X.                                   
025700     EJECT                                                                
025800 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
025900                           MSG-PCB DISP-PCB ARTC-PCB BENA-PCB             
026000                           XXBI-PCB WDT3-PCB WDK7-PCB WDB6-PCB            
026100                           KOM-KOMA-PCB.                                  
026200 MAIN SECTION.                                                            
026300                                                                          
026400     PERFORM A-INIT                                                       
026500     PERFORM B-CHECK-KEYS                                                 
026600                                                                          
026700     IF KEYS-OK                                                           
026800       IF REQU-UPDATE OR REQU-UPD-V                                       
026900         PERFORM G-CHECK-INPUT                                            
027000         IF INDATA-OK                                                     
027100           PERFORM H-UPDATE                                               
027200         END-IF                                                           
027300       ELSE                                                               
027400         IF REQU-FIRST                                                    
027500           PERFORM C-FIRST-PAGE                                           
027600         ELSE                                                             
027700           IF REQU-QUERY                                                  
027800              PERFORM E-SAME-PAGE                                         
027900           END-IF                                                         
028000           IF REQU-NEXT                                                   
028100              PERFORM D-NEXT-PAGE                                         
028200           END-IF                                                         
028300         END-IF                                                           
028400       END-IF                                                             
028500       PERFORM F-READ-SHOW-INFO                                           
028600     END-IF                                                               
028700     GOBACK                                                               
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100 A-INIT SECTION.                                                          
029200                                                                          
029300     MOVE ALL '+'       TO RESP-W60181O1                                  
029400     PERFORM MFS-FORM-ATTR                                                
029500     MOVE 001          TO RESP-IDMSGVER                                   
029600     MOVE SPACE        TO RESP-IDMSG-ERROR                                
029700                          RESP-IDMSG-INFO                                 
029800                          RESP-IDELMT-ERROR                               
029900                                                                          
030000     MOVE REQU-KVRADER TO RESP-KVRADER                                    
030100                                                                          
030200     ACCEPT DAGENS-DATUM FROM DATE                                        
030300     ACCEPT DAGENS-TID   FROM TIME                                        
030400     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
030500                                                                          
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 B-CHECK-KEYS SECTION.                                                    
031000     MOVE YES TO KEYS-SW                                                  
031100                                                                          
031200     EVALUATE REQU-IDSPRAK                                                
031300       WHEN 'ZH'                                                          
031400        MOVE 'RCN'           TO W-IDSKYLT                                 
031500        MOVE WS-CP-UNICODE   TO TRAUTF8-KDCP                              
031600       WHEN 'SV'                                                          
031700        MOVE 'S  '           TO W-IDSKYLT                                 
031800        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
031900       WHEN OTHER                                                         
032000        MOVE 'GB '           TO W-IDSKYLT                                 
032100        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
032200     END-EVALUATE                                                         
032300*                                                                         
032402     IF REQU-IDDC-KEY NUMERIC                                             
032502        MOVE REQU-IDDC-KEY      TO WS-IDDC                                
032602                                   W-IDDC                                 
032702                                   W-IDDC-B6                              
032802     ELSE                                                                 
032902        MOVE REQU-IDDC          TO WS-IDDC                                
033002                                   W-IDDC                                 
033102                                   W-IDDC-B6                              
033103     END-IF                                                               
033400     IF NDC-CN OR LDC-CN                                                  
033500       MOVE WC-LAND-CN     TO W-IDLAND                                    
033600     ELSE                                                                 
033700       IF NDC-US                                                          
033800         MOVE WC-LAND-US   TO W-IDLAND                                    
033900       ELSE                                                               
034000         MOVE WC-LAND-SE   TO W-IDLAND                                    
034100       END-IF                                                             
034200     END-IF                                                               
034300     MOVE W-IDLAND         TO WS-IDLAND-SAVE                              
034400*                                                                         
034500     INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO             
034600     IF REQU-IDARTNR-KEY NUMERIC                                          
034700       MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                 
034800     ELSE                                                                 
034900       MOVE NEJ TO KEYS-SW                                                
035000     END-IF                                                               
035100*                                                                         
035200     IF KEYS-WRONG                                                        
035300       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
035400       MOVE ZERO          TO RESP-KVRADER                                 
035500       PERFORM MFS-ERASE-FIELD-IN                                         
035600       PERFORM MFS-ERASE-FIELD-OUT                                        
035700     ELSE                                                                 
035800       PERFORM IMS-GU-WDB601                                              
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 C-FIRST-PAGE SECTION.                                                    
036300                                                                          
036400     PERFORM MFS-ERASE-FIELD-IN                                           
036500     .                                                                    
036600     EJECT                                                                
036700 D-NEXT-PAGE SECTION.                                                     
036800                                                                          
036900     PERFORM MFS-ERASE-FIELD-IN                                           
037000     MOVE REQU-DAREGDAT-START  TO W-DAREGDAT-9KOMPL                       
037100     MOVE REQU-TIKLOCK-START   TO W-TIKLOCK-9KOMPL                        
037200     MOVE SPACE                TO W-IDLAND-KY                             
037300     .                                                                    
037400     EJECT                                                                
037500 E-SAME-PAGE SECTION.                                                     
037600                                                                          
037700     IF  REQU-BEFT          = ALL '+'                                     
037800     AND REQU-KDFORP        = ALL '+'                                     
037900     AND REQU-TEBEFT        = ALL '+'                                     
038000     AND REQU-TEBEFT-79     = ALL '+'                                     
038100     AND REQU-TEBEFT02-79   = ALL '+'                                     
038200       PERFORM MFS-ERASE-FIELD-IN                                         
038300     ELSE                                                                 
038400       MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                             
038500       PERFORM EA-REQU-INDATA-TO-RESP                                     
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900                                                                          
039000 EA-REQU-INDATA-TO-RESP SECTION.                                          
039100                                                                          
039200     IF REQU-BEFT          NOT = ALL '+'                                  
039300        MOVE MFS-ADD-READ-FIELD     TO RESP-BEFT-ATTR                     
039400        MOVE ALL-PLUS               TO RESP-BEFT                          
039500     ELSE                                                                 
039600        MOVE ALL-SPACE              TO RESP-BEFT                          
039700     END-IF                                                               
039800                                                                          
039900                                                                          
040000     IF REQU-KDFORP     NOT = ALL '+'                                     
040100        MOVE MFS-ADD-READ-FIELD     TO RESP-KDFORP-ATTR                   
040200        MOVE ALL-PLUS               TO RESP-KDFORP                        
040300     ELSE                                                                 
040400        MOVE ALL-SPACE              TO RESP-KDFORP                        
040500     END-IF                                                               
040600                                                                          
040700                                                                          
040800     IF REQU-TEBEFT     NOT = ALL '+'                                     
040900        MOVE MFS-ADD-READ-FIELD     TO RESP-TEBEFT-ATTR                   
041000        MOVE ALL-PLUS               TO RESP-TEBEFT                        
041100     ELSE                                                                 
041200        MOVE ALL-SPACE              TO RESP-TEBEFT                        
041300     END-IF                                                               
041400                                                                          
041500     IF REQU-TEBEFT-79  NOT = ALL '+'                                     
041600        MOVE MFS-ADD-READ-FIELD     TO RESP-TEBEFT-79-ATTR                
041700        MOVE ALL-PLUS               TO RESP-TEBEFT-79                     
041800     ELSE                                                                 
041900        MOVE ALL-SPACE              TO RESP-TEBEFT-79                     
042000     END-IF                                                               
042100                                                                          
042200     IF REQU-TEBEFT02-79  NOT = ALL '+'                                   
042300        MOVE MFS-ADD-READ-FIELD     TO RESP-TEBEFT02-79-ATTR              
042400        MOVE ALL-PLUS               TO RESP-TEBEFT02-79                   
042500     ELSE                                                                 
042600        MOVE ALL-SPACE              TO RESP-TEBEFT02-79                   
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 F-READ-SHOW-INFO SECTION.                                                
043100                                                                          
043200     MOVE 0                   TO RESP-KVRADER                             
043300     PERFORM FA-READ-BASICDATA                                            
043400                                                                          
043500     IF SEGMENT-MISSING                                                   
043600        MOVE ERR-PART-MISSING TO RESP-IDMSG-ERROR                         
043700        MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                        
043800        PERFORM MFS-ERASE-FIELD-OUT                                       
043900     ELSE                                                                 
044000        PERFORM FB-SHOW-PART-INFO                                         
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 FA-READ-BASICDATA SECTION.                                               
044500                                                                          
044600     PERFORM IMS-GET-ARTC-ARTC                                            
044700     .                                                                    
044800     EJECT                                                                
044900 FB-SHOW-PART-INFO SECTION.                                               
045000                                                                          
045001     IF REQU-IDDC-KEY NUMERIC                                             
045010       MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                
045011     ELSE                                                                 
045012       MOVE SPACE         TO RESP-IDDC-KEY                                
045020     END-IF                                                               
045100     PERFORM FC-GET-BEART-INFO                                            
045200     PERFORM FD-GET-HIST-LINE-INFO                                        
045300     .                                                                    
045400     EJECT                                                                
045500 FC-GET-BEART-INFO SECTION.                                               
045600                                                                          
045700     PERFORM MFS-ERASE-FIELD-OUT                                          
045800     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
045900     IF DCS-UNICODE-IDSKYLT                                               
046000        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
046100     ELSE                                                                 
046200        MOVE '278 '             TO TRAUTF8-KDCP                           
046300     END-IF                                                               
046400     PERFORM IMS-GET-BENA-TEXT                                            
046500     IF SEGMENT-FOUND                                                     
046600        MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                    
046700     ELSE                                                                 
046800        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
046900                                   BENA-TEXT-BEART                        
047000        MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                           
047100     END-IF                                                               
047200     IF TRAUTF8-TECONV-FROM = SPACES                                      
047300      MOVE 'GB'  TO W-IDSKYLT                                             
047400      MOVE '278' TO TRAUTF8-KDCP                                          
047500      PERFORM IMS-GET-BENA-TEXT                                           
047600      MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                      
047700     END-IF                                                               
047800                                                                          
047900     IF REQU-IDMSGVER = '001'                                             
048000*    CALL FROM WEB AND CHINA                                              
048100*    CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE           
048200        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
048300        MOVE TRAUTF8-TECONV-TO  TO RESP-BEART-UT                          
048400     ELSE                                                                 
048500*    CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                         
048600        MOVE BENA-TEXT-BEART    TO RESP-BEART-UT                          
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 FD-GET-HIST-LINE-INFO SECTION.                                           
049100                                                                          
049200     PERFORM IMS-GU-WDT301                                                
049300     IF SEGMENT-FOUND                                                     
049400        MOVE ZERO TO HIST-IX                                              
049500        MOVE WS-IDLAND-SAVE TO W-IDLAND                                   
049600        PERFORM IMS-GNP-WDT311-FIRST                                      
049700        PERFORM UNTIL SEGMENT-MISSING OR HIST-IX > MAX-KVRADER            
049800           IF HIST-IX = 0                                                 
049900             MOVE FPCK-BEFT      TO RESP-BEFT-UT                          
050000             MOVE FPCK-KDFORP    TO RESP-KDFORP-UT                        
050100             MOVE FPCK-IDUSER    TO RESP-IDUSER-UT                        
050200             MOVE FPCK-TEBEFT(1) TO RESP-TEBEFT-UT                        
050300             MOVE FPCK-TEBEFT(2) TO W-TEBEFT-79(1:40)                     
050400             MOVE FPCK-TEBEFT(3) TO W-TEBEFT-79(41:39)                    
050500             MOVE FPCK-TEBEFT(4) TO W-TEBEFT02-79(1:40)                   
050600             MOVE FPCK-TEBEFT(5) TO W-TEBEFT02-79(41:39)                  
050700             MOVE W-TEBEFT-79    TO RESP-TEBEFT-79-UT                     
050800             MOVE W-TEBEFT02-79  TO RESP-TEBEFT02-79-UT                   
050900             PERFORM FD-CONVERT-FROM-9KOMPL                               
051000             MOVE WS-TIREGDAT    TO RESP-TIREGDAT-UT                      
051100                                                                          
051200             MOVE FPCK-DAREGDAT-9KOMPL TO RESP-DAREGDAT-NEXT              
051300             MOVE FPCK-TIKLOCK-9KOMPL  TO RESP-TIKLOCK-NEXT               
051400                                                                          
051500           ELSE                                                           
051600             MOVE FPCK-BEFT      TO RESP-BEFT-HIST-LINE(HIST-IX)          
051700             MOVE FPCK-KDFORP    TO RESP-KDFORP-HIST-LINE(HIST-IX)        
051800             MOVE FPCK-IDUSER    TO RESP-IDUSER-HIST-LINE(HIST-IX)        
051900             MOVE FPCK-TEBEFT(1) TO RESP-TEBEFT-HIST-LINE(HIST-IX)        
052000             MOVE FPCK-TEBEFT(2) TO W-TEBEFT-79(1:40)                     
052100             MOVE FPCK-TEBEFT(3) TO W-TEBEFT-79(41:39)                    
052200             MOVE FPCK-TEBEFT(4) TO W-TEBEFT02-79(1:40)                   
052300             MOVE FPCK-TEBEFT(5) TO W-TEBEFT02-79(41:39)                  
052400             MOVE W-TEBEFT-79    TO                                       
052500                              RESP-TEBEFT-79-HIST-LINE(HIST-IX)           
052600             MOVE W-TEBEFT02-79  TO                                       
052700                              RESP-TEBEFT02-79-HIST-LINE(HIST-IX)         
052800                                                                          
052900             PERFORM FD-CONVERT-FROM-9KOMPL                               
053000             MOVE WS-TIREGDAT TO RESP-TIREGDAT-HIST-LINE(HIST-IX)         
053100           END-IF                                                         
053200                                                                          
053300           ADD +1 TO HIST-IX                                              
053400                                                                          
053500           PERFORM IMS-GNP-WDT311                                         
053600        END-PERFORM                                                       
053700     END-IF                                                               
053800                                                                          
053900     IF HIST-IX > 0                                                       
054000        COMPUTE RESP-KVRADER = HIST-IX - 1                                
054100     END-IF                                                               
054200                                                                          
054300     IF SEGMENT-FOUND                                                     
054400       MOVE INF-FOR-MORE-INFORMATION   TO RESP-IDMSG-INFO                 
054500       MOVE '*'                        TO RESP-IDELMT-ERROR               
054600       MOVE FPCK-DAREGDAT-9KOMPL       TO RESP-DAREGDAT-NEXT              
054700       MOVE FPCK-TIKLOCK-9KOMPL        TO RESP-TIKLOCK-NEXT               
054800     ELSE                                                                 
054900       MOVE INF-LAST-PAGE              TO RESP-IDMSG-INFO                 
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300 FD-CONVERT-FROM-9KOMPL SECTION.                                          
055400                                                                          
055500     COMPUTE WS-DAREGDAT = 999999999 - FPCK-DAREGDAT-9KOMPL               
055600     .                                                                    
055700     EJECT                                                                
055800 G-CHECK-INPUT SECTION.                                                   
055900                                                                          
056000     MOVE YES                        TO INDATA-SW                         
056100     IF REQU-BEFT    = ALL '+'                                            
056200     AND REQU-KDFORP = ALL '+'                                            
056300     AND REQU-TEBEFT = ALL '+'                                            
056400     AND REQU-TEBEFT-79   = ALL '+'                                       
056500     AND REQU-TEBEFT02-79 = ALL '+'                                       
056600       MOVE ERR-PF11-AND-NO-DATA     TO RESP-IDMSG-ERROR                  
056700       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
056800       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
056900       MOVE NEJ                      TO INDATA-SW                         
057000     ELSE                                                                 
057100       IF REQU-BEFT  NOT = ALL '+'                                        
057200         IF REQU-BEFT NOT NUMERIC                                         
057300           MOVE MFS-NUM-FIELD-WRONG TO RESP-BEFT-ATTR                     
057400           MOVE NEJ                 TO INDATA-SW                          
057500         ELSE                                                             
057600           IF NOT CDC                                                     
057700             PERFORM IMS-GU-WDK711                                        
057800             IF SEGMENT-FOUND                                             
057900               IF SLAG-IDDC-REF NOT = SPACE                               
058000                 MOVE NEJ                 TO INDATA-SW                    
058100               END-IF                                                     
058200             ELSE                                                         
058300               MOVE NEJ                 TO INDATA-SW                      
058400             END-IF                                                       
058500           END-IF                                                         
058600         END-IF                                                           
058700       END-IF                                                             
058800                                                                          
058900       IF REQU-KDFORP NOT = ALL '+'                                       
059000          IF REQU-KDFORP NOT NUMERIC                                      
059100            MOVE MFS-NUM-FIELD-WRONG TO RESP-KDFORP-ATTR                  
059200            MOVE NEJ                 TO INDATA-SW                         
059300          ELSE                                                            
059400            MOVE MFS-NUM-FIELD-OK    TO RESP-KDFORP-ATTR                  
059500          END-IF                                                          
059600       END-IF                                                             
059700                                                                          
059800       IF REQU-TEBEFT  NOT = ALL '+'                                      
059900         MOVE MFS-ALPHA-FIELD-OK     TO RESP-TEBEFT-ATTR                  
060000       END-IF                                                             
060100                                                                          
060200       IF REQU-TEBEFT-79 NOT = ALL '+'                                    
060300         MOVE MFS-ALPHA-FIELD-OK     TO RESP-TEBEFT-79-ATTR               
060400       END-IF                                                             
060500                                                                          
060600       IF REQU-TEBEFT02-79 NOT = ALL '+'                                  
060700         MOVE MFS-ALPHA-FIELD-OK     TO RESP-TEBEFT02-79-ATTR             
060800       END-IF                                                             
060900                                                                          
061000       MOVE NEJ TO WS-FLIDDC-KEY                                          
061003       IF REQU-IDDC-KEY NUMERIC AND REQU-IDDC NOT = REQU-IDDC-KEY         
061303         MOVE NEJ TO INDATA-SW                                            
061304         MOVE NEJ TO WS-FLIDDC-KEY                                        
061403       END-IF                                                             
061503                                                                          
061603       IF INDATA-WRONG                                                    
061703         MOVE ERR-CORR-HILITE-FLDS   TO RESP-IDMSG-ERROR                  
061803         IF REQU-BEFT NUMERIC                                             
061903           MOVE MFS-NUM-FIELD-WRONG TO RESP-BEFT-ATTR                     
062003           MOVE ERR-REFILL-PART     TO RESP-IDMSG-ERROR                   
062103         END-IF                                                           
062104         IF WS-FLIDDC-KEY = NEJ                                           
062106           MOVE ERR-WRONG-KEY       TO RESP-IDMSG-ERROR                   
062107         END-IF                                                           
062203         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
062303         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
062403       ELSE                                                               
062503         PERFORM IMS-GET-ARTC-ARTC                                        
062603         IF SEGMENT-MISSING                                               
062703           MOVE ERR-PART-MISSING     TO RESP-IDMSG-ERROR                  
062803           MOVE 'IDARTNR'            TO RESP-IDELMT-ERROR                 
062903           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
063003           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
063103           IF NOT CDC                                                     
063203             PERFORM IMS-GU-WDK711                                        
063303             IF SEGMENT-MISSING                                           
063403               MOVE ERR-PART-MISSING TO RESP-IDMSG-ERROR                  
063503               MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                 
063603               MOVE NEJ              TO INDATA-SW                         
063703             END-IF                                                       
063803           END-IF                                                         
063903         ELSE                                                             
064003           PERFORM IMS-GU-WDT301                                          
064103           IF SEGMENT-FOUND                                               
064203              PERFORM IMS-GNP-WDT311                                      
064303           END-IF                                                         
064403           PERFORM GA-CHECK-BEFT-UPDATING-RULES                           
064503           IF INDATA-WRONG                                                
064603             MOVE ERR-PRESS-PF23     TO RESP-IDMSG-ERROR                  
064703             MOVE 'PF23'             TO RESP-IDELMT-ERROR                 
064803             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
064903             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
065003           END-IF                                                         
065103         END-IF                                                           
065203       END-IF                                                             
065303     END-IF                                                               
065403     .                                                                    
065503     EJECT                                                                
065603                                                                          
065703 GA-CHECK-BEFT-UPDATING-RULES SECTION.                                    
065803     CONTINUE                                                             
065903*    IF SEGMENT-FOUND                                                     
066003*      IF FPCK-BEFT = 75                                                  
066103*        IF NOT REQU-UPD-V                                                
066203*          MOVE NEJ TO INDATA-SW                                          
066303*        END-IF                                                           
066403*      END-IF                                                             
066503*    END-IF                                                               
066603*                                                                         
066703*    IF REQU-BEFT        = 79                                             
066803*      IF NOT REQU-UPD-V                                                  
066903*        MOVE NEJ    TO INDATA-SW                                         
067003*      END-IF                                                             
067103*     END-IF                                                              
067203     .                                                                    
067303     EJECT                                                                
067403 H-UPDATE SECTION.                                                        
067503                                                                          
067603     PERFORM IMS-GET-ARTC-ARTC                                            
067703     IF SEGMENT-FOUND                                                     
067803        MOVE ZEROS TO FPCK-BEFT                                           
067903        MOVE ZEROS TO FPCK-KDFORP                                         
068003        PERFORM IMS-GU-WDT301                                             
068103        IF SEGMENT-MISSING                                                
068203           MOVE W-IDARTNR TO FART-IDARTNR                                 
068303           PERFORM IMS-ISRT-WDT301                                        
068403        END-IF                                                            
068503        PERFORM IMS-GU-WDT301                                             
068603        PERFORM IMS-GNP-WDT311                                            
068703*                                                                         
068803        MOVE SPACE        TO FPCK-TEBEFT(1)                               
068903        MOVE SPACE        TO FPCK-TEBEFT(2)                               
069003        MOVE SPACE        TO FPCK-TEBEFT(3)                               
069103        MOVE SPACE        TO FPCK-TEBEFT(4)                               
069203        MOVE SPACE        TO FPCK-TEBEFT(5)                               
069303*                                                                         
069403        IF NDC-CN OR LDC-CN OR NDC-US                                     
069503           PERFORM IMS-GHU-WDK712                                         
069603        ELSE                                                              
069703           PERFORM IMS-GET-ARTC-ARTC                                      
069803           PERFORM IMS-GHNP-ARTC-CLAG                                     
069903        END-IF                                                            
070003*                                                                         
070103        IF REQU-BEFT NOT = ALL '+'                                        
070203          MOVE REQU-BEFT TO FPCK-BEFT  RESP-BEFT-UT                       
070303          IF NDC-CN OR LDC-CN OR NDC-US                                   
070403            MOVE REQU-BEFT             TO LART-BEFT                       
070503          ELSE                                                            
070603            MOVE REQU-BEFT             TO ARTC-CLAG-BEFT                  
070703          END-IF                                                          
070803                                                                          
070903          PERFORM HB-STARTA-DISPATCHEN                                    
071003          MOVE MFS-ADD-HILIGHT-FIELD TO RESP-BEFT-ATTR                    
071103          MOVE YES TO BEFT-SW                                             
071203        ELSE                                                              
071303          MOVE MFS-DO-NOT-TOUCH-FIELD TO RESP-BEFT-ATTR                   
071403          MOVE NEJ TO BEFT-SW                                             
071503        END-IF                                                            
071603                                                                          
071703        IF REQU-KDFORP NOT = ALL '+'                                      
071803          MOVE REQU-KDFORP TO FPCK-KDFORP RESP-KDFORP-UT                  
071903*       --------------------------                 <-- DUBBELLAGRA        
072003                               ARTC-CLAG-KDFORP                           
072103*       --------------------------                 <-- DUBBELLAGRA        
072203          MOVE MFS-ADD-HILIGHT-FIELD  TO RESP-KDFORP-ATTR                 
072303        ELSE                                                              
072403          MOVE MFS-DO-NOT-TOUCH-FIELD TO RESP-KDFORP-ATTR                 
072503        END-IF                                                            
072603                                                                          
072703        IF REQU-TEBEFT   NOT = ALL '+'                                    
072803          MOVE REQU-TEBEFT TO FPCK-TEBEFT(1) RESP-TEBEFT-UT               
072903          MOVE MFS-ADD-HILIGHT-FIELD  TO RESP-TEBEFT-ATTR                 
073003        ELSE                                                              
073103          MOVE MFS-DO-NOT-TOUCH-FIELD TO RESP-TEBEFT-ATTR                 
073203        END-IF                                                            
073303        IF REQU-TEBEFT-79 NOT = ALL '+'                                   
073403          MOVE REQU-TEBEFT-79(1:40)   TO FPCK-TEBEFT(2)                   
073503          MOVE REQU-TEBEFT-79(41:39)  TO FPCK-TEBEFT(3)                   
073603          MOVE REQU-TEBEFT-79         TO RESP-TEBEFT-79-UT                
073703          MOVE MFS-ADD-HILIGHT-FIELD  TO RESP-TEBEFT-79-ATTR              
073803        ELSE                                                              
073903          MOVE MFS-DO-NOT-TOUCH-FIELD TO RESP-TEBEFT-79-ATTR              
074003        END-IF                                                            
074103        IF REQU-TEBEFT02-79 NOT = ALL '+'                                 
074203          MOVE REQU-TEBEFT02-79(1:40)   TO FPCK-TEBEFT(4)                 
074303          MOVE REQU-TEBEFT02-79(41:39)  TO FPCK-TEBEFT(5)                 
074403          MOVE REQU-TEBEFT02-79         TO RESP-TEBEFT02-79-UT            
074503          MOVE MFS-ADD-HILIGHT-FIELD    TO RESP-TEBEFT02-79-ATTR          
074603        ELSE                                                              
074703          MOVE MFS-DO-NOT-TOUCH-FIELD   TO RESP-TEBEFT02-79-ATTR          
074803        END-IF                                                            
074903                                                                          
075003        MOVE REQU-IDUSER-IN           TO FPCK-IDUSER                      
075103        IF NDC-CN OR LDC-CN                                               
075203          MOVE WC-LAND-CN             TO FPCK-IDLANDX2                    
075303        ELSE                                                              
075403          IF NDC-US                                                       
075503            MOVE WC-LAND-US           TO FPCK-IDLANDX2                    
075603          ELSE                                                            
075703            MOVE WC-LAND-SE           TO FPCK-IDLANDX2                    
075803          END-IF                                                          
075903        END-IF                                                            
076003        PERFORM HA-CONVERT-TO-9KOMPL                                      
076103                                                                          
076203*       --------------------------                 <-- DUBBELLAGRA        
076303        IF SEGMENT-FOUND                                                  
076403           IF NDC-CN OR LDC-CN OR NDC-US                                  
076503             PERFORM IMS-REPL-WDK712                                      
076603           ELSE                                                           
076703             PERFORM IMS-REPL-ARTC-CLAG                                   
076803           END-IF                                                         
076903*         --------------------------               <-- DUBBELLAGRA        
077003           PERFORM IMS-ISRT-WDT311                                        
077103                                                                          
077203           MOVE INF-UPDATE-DONE       TO RESP-IDMSG-INFO                  
077303           PERFORM MFS-FORM-ATTR                                          
077403           PERFORM MFS-ERASE-FIELD-IN                                     
077503        ELSE                                                              
077603           IF NDC-CN OR LDC-CN OR NDC-US                                  
077703              PERFORM IMS-ISRT-WDT311                                     
077803              PERFORM MFS-FORM-ATTR                                       
077903              PERFORM MFS-ERASE-FIELD-IN                                  
078003           ELSE                                                           
078103              MOVE ERR-UPDATE-NOT-DONE  TO RESP-IDMSG-ERROR               
078203              PERFORM MFS-ERASE-FIELD-OUT                                 
078303           END-IF                                                         
078403        END-IF                                                            
078503*    * * MFS-DO-NOT-TOUCH-FIELD TO LOCKED VALUES                          
078603                                                                          
078703        IF BEFT-RETT                                                      
078803           MOVE   W-IDARTNR           TO 2214-IDARTNR                     
078903           MOVE   NEJ                 TO WS-EXTERNAL                      
079003                                         WS-CHINA-UPD                     
079103           IF NDC-CN OR LDC-CN OR NDC-US                                  
079203             IF NDC-US                                                    
079303               MOVE WS-IDDC           TO W-IDDC-2213                      
079403             ELSE                                                         
079503               MOVE WC-NDC-CN-71      TO W-IDDC-2213                      
079603             END-IF                                                       
079703           ELSE                                                           
079803             MOVE '11'                TO W-IDDC-2213                      
079903             IF NDC-US                                                    
080003               PERFORM HC-UPDATE-USA                                      
080103             ELSE                                                         
080203               PERFORM HD-UPDATE-CHINA                                    
080303             END-IF                                                       
080403           END-IF                                                         
080503           PERFORM IMS-ISRT-2214                                          
080603           IF WS-US-UPD = YES                                             
080703             MOVE WS-IDDC             TO W-IDDC-2213                      
080803             PERFORM IMS-ISRT-2214                                        
080903           END-IF                                                         
081003           IF WS-CHINA-UPD = YES                                          
081103             MOVE WC-NDC-CN-71        TO W-IDDC-2213                      
081203             PERFORM IMS-ISRT-2214                                        
081303           END-IF                                                         
081403        END-IF                                                            
081503                                                                          
081603     ELSE                                                                 
081703        MOVE ERR-PART-MISSING         TO RESP-IDMSG-ERROR                 
081803        MOVE 'IDARTNR'                TO RESP-IDELMT-ERROR                
081903        PERFORM MFS-ERASE-FIELD-OUT                                       
082003     END-IF                                                               
082103     .                                                                    
082203     EJECT                                                                
082303 HA-CONVERT-TO-9KOMPL SECTION.                                            
082403     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAREGDAT                      
082503     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT               
082603     ACCEPT WS-TID FROM TIME                                              
082703     COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                     
082803                                                                          
082903     MOVE FPCK-DAREGDAT-9KOMPL   TO W-DAREGDAT-9KOMPL                     
083003     MOVE FPCK-TIKLOCK-9KOMPL    TO W-TIKLOCK-9KOMPL                      
083103                                                                          
083203     .                                                                    
083303     EJECT                                                                
083403 HB-STARTA-DISPATCHEN       SECTION.                                      
083503                                                                          
083603     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
083703     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
083803     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
083903     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
084003     MOVE SPACE                TO MSG-KOM-KDTRANS                         
084103     MOVE 'W6I19B01'           TO MSG-KOM-IDCPYTXT                        
084203     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
084303     MOVE 'W6018100'           TO MSG-KOM-IDSNDJOB                        
084403     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
084503     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
084603     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
084703                                                                          
084803     MOVE ALL '+'              TO MOD619B-MID-W6I19B01                    
084903     MOVE REQU-IDARTNR-KEY     TO MOD619B-MID-IDARTNR                     
085003     MOVE REQU-IDDC            TO MOD619B-MID-IDDC                        
085103     MOVE REQU-BEFT            TO MOD619B-MID-BEFT                        
085203     COMPUTE P-TO-P-KVLL-CPYTXT = LENGTH OF MOD619B-MID-W6I19B01          
085303     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
085403                                  P-TO-P-KVLL-CPYTXT                      
085503     MOVE 'W6T19BX '           TO P-TO-P-KDTRANS                          
085603     MOVE '6181'               TO P-TO-P-IDTRANS                          
085703     IF REQU-IDSPRAK = 'SV'                                               
085803        MOVE '1'               TO P-TO-P-KDMFSFOR                         
085903     ELSE                                                                 
086003        MOVE '2'               TO P-TO-P-KDMFSFOR                         
086103     END-IF                                                               
086203     MOVE MOD619B-MID-W6I19B01 TO P-TO-P-DATA                             
086303                                                                          
086403     CALL W006KOM USING MSG-PCB                                           
086503                        DISP-PCB                                          
086603                        KOM-KOMA-PCB                                      
086703                        MSG-KOM-WMSGKOM                                   
086803                        P-TO-P-SW                                         
086903     .                                                                    
087003     EJECT                                                                
087103 HC-UPDATE-USA  SECTION.                                                  
087203     MOVE WC-NDC-US-RU        TO W-IDDC                                   
087303     PERFORM IMS-GU-WDK711                                                
087403     IF SEGMENT-FOUND                                                     
087503       IF SLAG-IDDC-REF = SPACE                                           
087603         MOVE YES   TO WS-EXTERNAL                                        
087703       END-IF                                                             
087803     END-IF                                                               
087903     IF WS-EXTERNAL = NEJ                                                 
088003       MOVE WC-NDC-US-LA      TO W-IDDC                                   
088103       PERFORM IMS-GU-WDK711                                              
088203       IF SEGMENT-FOUND                                                   
088303         IF SLAG-IDDC-REF = SPACE                                         
088403           MOVE YES   TO WS-EXTERNAL                                      
088503         END-IF                                                           
088603       END-IF                                                             
088703     END-IF                                                               
088803     IF WS-EXTERNAL = NEJ                                                 
088903       MOVE WC-NDC-US-SE      TO W-IDDC                                   
089003       PERFORM IMS-GU-WDK711                                              
089103       IF SEGMENT-FOUND                                                   
089203         IF SLAG-IDDC-REF = SPACE                                         
089303           MOVE YES   TO WS-EXTERNAL                                      
089403         END-IF                                                           
089503       END-IF                                                             
089603     END-IF                                                               
089703     IF WS-EXTERNAL = NEJ                                                 
089803       MOVE WC-NDC-US-CH      TO W-IDDC                                   
089903       PERFORM IMS-GU-WDK711                                              
090003       IF SEGMENT-FOUND                                                   
090103         IF SLAG-IDDC-REF = SPACE                                         
090203           MOVE YES   TO WS-EXTERNAL                                      
090303         END-IF                                                           
090403       END-IF                                                             
090503     END-IF                                                               
090603     IF WS-EXTERNAL = NEJ                                                 
090703       MOVE WC-NDC-US-JA      TO W-IDDC                                   
090803       PERFORM IMS-GU-WDK711                                              
090903       IF SEGMENT-FOUND                                                   
091003         IF SLAG-IDDC-REF = SPACE                                         
091103           MOVE YES   TO WS-EXTERNAL                                      
091203         END-IF                                                           
091303       END-IF                                                             
091403     END-IF                                                               
091503     IF WS-EXTERNAL = YES                                                 
091603       MOVE WC-LAND-US        TO W-IDLAND                                 
091703       PERFORM IMS-GHU-WDK712                                             
091803       IF SEGMENT-FOUND                                                   
091903         IF LART-BEFT = ZERO                                              
092003           MOVE YES TO WS-US-UPD                                          
092103         END-IF                                                           
092203       ELSE                                                               
092303         MOVE YES TO WS-US-UPD                                            
092403       END-IF                                                             
092503     END-IF                                                               
092603     .                                                                    
092703     EJECT                                                                
092803 HD-UPDATE-CHINA SECTION.                                                 
092903     MOVE WC-NDC-CN-71        TO W-IDDC                                   
093003     PERFORM IMS-GU-WDK711                                                
093103     IF SEGMENT-FOUND                                                     
093203       IF SLAG-IDDC-REF = SPACE                                           
093303         MOVE YES   TO WS-EXTERNAL                                        
093403       END-IF                                                             
093503     END-IF                                                               
093603     IF WS-EXTERNAL = NEJ                                                 
093703       MOVE WC-NDC-CN-72      TO W-IDDC                                   
093803       PERFORM IMS-GU-WDK711                                              
093903       IF SEGMENT-FOUND                                                   
094003         IF SLAG-IDDC-REF = SPACE                                         
094103           MOVE YES   TO WS-EXTERNAL                                      
094203         END-IF                                                           
094303       END-IF                                                             
094403     END-IF                                                               
094503     IF WS-EXTERNAL = NEJ                                                 
094603       MOVE WC-NDC-CN-73      TO W-IDDC                                   
094703       PERFORM IMS-GU-WDK711                                              
094803       IF SEGMENT-FOUND                                                   
094903         IF SLAG-IDDC-REF = SPACE                                         
095003           MOVE YES   TO WS-EXTERNAL                                      
095103         END-IF                                                           
095203       END-IF                                                             
095303     END-IF                                                               
095403     IF WS-EXTERNAL = NEJ                                                 
095503       MOVE WC-NDC-CN-74      TO W-IDDC                                   
095603       PERFORM IMS-GU-WDK711                                              
095703       IF SEGMENT-FOUND                                                   
095803         IF SLAG-IDDC-REF = SPACE                                         
095903           MOVE YES   TO WS-EXTERNAL                                      
096003         END-IF                                                           
096103       END-IF                                                             
096203     END-IF                                                               
096303     IF WS-EXTERNAL = YES                                                 
096403       MOVE WC-LAND-CN        TO W-IDLAND                                 
096503       PERFORM IMS-GHU-WDK712                                             
096603       IF SEGMENT-FOUND                                                   
096703         IF LART-BEFT = ZERO                                              
096803           MOVE YES TO WS-CHINA-UPD                                       
096903         END-IF                                                           
097003       ELSE                                                               
097103         MOVE YES TO WS-CHINA-UPD                                         
097203       END-IF                                                             
097303     END-IF                                                               
097403     .                                                                    
097503     EJECT                                                                
097603 MFS-ERASE-FIELD-OUT SECTION.                                             
097703*    --- ALLA UTDATA-FÄLT                                                 
097803     IF REQU-IDMSGVER = '001'                                             
097903       MOVE ALL-UTF8-SPACE  TO RESP-BEART-UT                              
098003     ELSE                                                                 
098103       MOVE ALL-SPACE       TO RESP-BEART-UT                              
098203     END-IF                                                               
098303                                                                          
098403     MOVE ALL-SPACE       TO RESP-BEFT-UT                                 
098503                             RESP-KDFORP-UT                               
098603                             RESP-IDUSER-UT                               
098703                             RESP-TIREGDAT-UT                             
098803                             RESP-TEBEFT-UT                               
098903                             RESP-TEBEFT-79-UT                            
099003                             RESP-TEBEFT02-79-UT                          
099103                             RESP-DAREGDAT-NEXT                           
099203                             RESP-TIKLOCK-NEXT                            
099303                             RESP-BEFT-HIST-LINE(1)                       
099403                             RESP-KDFORP-HIST-LINE(1)                     
099503                             RESP-IDUSER-HIST-LINE(1)                     
099603                             RESP-TIREGDAT-HIST-LINE(1)                   
099703                             RESP-TEBEFT-HIST-LINE(1)                     
099803                             RESP-TEBEFT-79-HIST-LINE(1)                  
099903                             RESP-TEBEFT02-79-HIST-LINE(1)                
100003                             RESP-BEFT-HIST-LINE(2)                       
100103                             RESP-KDFORP-HIST-LINE(2)                     
100203                             RESP-IDUSER-HIST-LINE(2)                     
100303                             RESP-TIREGDAT-HIST-LINE(2)                   
100403                             RESP-TEBEFT-HIST-LINE(2)                     
100503                             RESP-TEBEFT-79-HIST-LINE(2)                  
100603                             RESP-TEBEFT02-79-HIST-LINE(2)                
100703     .                                                                    
100803     SKIP3                                                                
100903 MFS-ERASE-FIELD-IN SECTION.                                              
101003*    --- ALLA INDATA-FÄLT                                                 
101103     MOVE ALL-SPACE       TO RESP-BEFT                                    
101203                             RESP-KDFORP                                  
101303                             RESP-TEBEFT                                  
101403                             RESP-TEBEFT-79                               
101503                             RESP-TEBEFT02-79                             
101603     .                                                                    
101703     EJECT                                                                
101803 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
101903*    --- ALLA UTDATA-FÄLT                                                 
102003     IF REQU-IDMSGVER = '001'                                             
102103       MOVE ALL-UTF8-PLUS        TO RESP-BEART-UT                         
102203     ELSE                                                                 
102303       MOVE ALL-PLUS             TO RESP-BEART-UT                         
102403     END-IF                                                               
102503                                                                          
102603     MOVE ALL-PLUS               TO RESP-BEFT-UT                          
102703                                    RESP-KDFORP-UT                        
102803                                    RESP-IDUSER-UT                        
102903                                    RESP-TIREGDAT-UT                      
103003                                    RESP-TEBEFT-UT                        
103103                                    RESP-TEBEFT-79-UT                     
103203                                    RESP-TEBEFT02-79-UT                   
103303                                    RESP-DAREGDAT-NEXT                    
103403                                    RESP-TIKLOCK-NEXT                     
103503                                    RESP-BEFT-HIST-LINE(1)                
103603                                    RESP-KDFORP-HIST-LINE(1)              
103703                                    RESP-IDUSER-HIST-LINE(1)              
103803                                    RESP-TIREGDAT-HIST-LINE(1)            
103903                                    RESP-TEBEFT-HIST-LINE(1)              
104003                                    RESP-TEBEFT-79-HIST-LINE(1)           
104103                                    RESP-TEBEFT02-79-HIST-LINE(1)         
104203                                    RESP-BEFT-HIST-LINE(2)                
104303                                    RESP-KDFORP-HIST-LINE(2)              
104403                                    RESP-IDUSER-HIST-LINE(2)              
104503                                    RESP-TIREGDAT-HIST-LINE(2)            
104603                                    RESP-TEBEFT-HIST-LINE(2)              
104703                                    RESP-TEBEFT-79-HIST-LINE(2)           
104803                                    RESP-TEBEFT02-79-HIST-LINE(2)         
104903     .                                                                    
105003     SKIP3                                                                
105103 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
105203*    --- ALLA INDATA-FÄLT                                                 
105303     MOVE ALL-PLUS               TO RESP-BEFT                             
105403                                    RESP-KDFORP                           
105503                                    RESP-TEBEFT                           
105603                                    RESP-TEBEFT-79                        
105703                                    RESP-TEBEFT02-79                      
105803                                                                          
105903     .                                                                    
106003     EJECT                                                                
106103 MFS-FORM-ATTR SECTION.                                                   
106203*    --- ALL INDATA-FIELDS                                                
106303     MOVE MFS-FORMAT-DEFAULT-ATTR TO RESP-BEFT-ATTR                       
106403                                     RESP-KDFORP-ATTR                     
106503                                     RESP-TEBEFT-ATTR                     
106603                                     RESP-TEBEFT-79-ATTR                  
106703                                     RESP-TEBEFT02-79-ATTR                
106803                                                                          
106903     .                                                                    
107003     SKIP2                                                                
107103* --- IMS SECTIONS ---                                                    
107203                                                                          
107303 IMS-GET-ARTC-ARTC SECTION.                                               
107403     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
107503          DELIMITED BY SIZE INTO SSA1                                     
107603     MOVE '  GE'           TO GOOD-STATUSCODES                            
107703     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
107803     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
107903     PERFORM IMS-STATUSCHECK                                              
108003     .                                                                    
108103     EJECT                                                                
108203                                                                          
108303*                                                                         
108403* DUBBELLAGRA                                                             
108503*                                                                         
108603 IMS-GHNP-ARTC-CLAG      SECTION.                                         
108703     MOVE 'WDK611  '       TO SSA1                                        
108803     MOVE '  GE'           TO GOOD-STATUSCODES                            
108903     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1                
109003     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
109103     PERFORM IMS-STATUSCHECK                                              
109203     SKIP3                                                                
109303     .                                                                    
109403 IMS-REPL-ARTC-CLAG      SECTION.                                         
109503     MOVE '  '             TO GOOD-STATUSCODES                            
109603     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
109703     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
109803     PERFORM IMS-STATUSCHECK                                              
109903     .                                                                    
110003     EJECT                                                                
110103*                                                                         
110203* DUBBELLAGRA                                                             
110303*                                                                         
110403                                                                          
110503 IMS-GU-WDT301           SECTION.                                         
110603     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
110703          DELIMITED BY SIZE INTO SSA1                                     
110803     MOVE '  GE'           TO GOOD-STATUSCODES                            
110903     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT301 SSA1                    
111003     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
111103     PERFORM IMS-STATUSCHECK                                              
111203     .                                                                    
111303     SKIP3                                                                
111403 IMS-ISRT-WDT301    SECTION.                                              
111503                                                                          
111603     MOVE 'WDT301 '        TO SSA1                                        
111703     MOVE '    ' TO GOOD-STATUSCODES                                      
111803     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
111903     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
112003     PERFORM IMS-STATUSCHECK                                              
112103     .                                                                    
112203 IMS-GNP-WDT311         SECTION.                                          
112303                                                                          
112403     STRING 'WDT311  (WDT311KY=>' W-WDT311KY-X                            
112503                    '&IDLAND   =' W-IDLAND-X ')'                          
112603          DELIMITED BY SIZE INTO SSA1                                     
112703     MOVE '  GE'           TO GOOD-STATUSCODES                            
112803     CALL CBLTDLI USING GNP WDT3-PCB DLI-IO-WDT311 SSA1                   
112903     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
113003     PERFORM IMS-STATUSCHECK                                              
113103     .                                                                    
113203     SKIP3                                                                
113303                                                                          
113403 IMS-GNP-WDT311-FIRST   SECTION.                                          
113503                                                                          
113603     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
113703          DELIMITED BY SIZE INTO SSA1                                     
113803     MOVE '  GE' TO GOOD-STATUSCODES                                      
113903     CALL CBLTDLI USING GNP WDT3-PCB DLI-IO-WDT311 SSA1                   
114003     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
114103     PERFORM IMS-STATUSCHECK                                              
114203     .                                                                    
114303     EJECT                                                                
114403 IMS-ISRT-WDT311    SECTION.                                              
114503                                                                          
114603     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
114703          DELIMITED BY SIZE INTO SSA1                                     
114803     MOVE 'WDT311 '        TO SSA2                                        
114903     MOVE '  II' TO GOOD-STATUSCODES                                      
115003     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
115103     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
115203     PERFORM IMS-STATUSCHECK                                              
115303     .                                                                    
115403     EJECT                                                                
115503 IMS-GET-BENA-TEXT SECTION.                                               
115603     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
115703             DELIMITED BY SIZE INTO SSA1                                  
115803     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
115903              DELIMITED BY SIZE INTO SSA2                                 
116003     MOVE '  GE' TO GOOD-STATUSCODES                                      
116103     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
116203     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
116303     PERFORM IMS-STATUSCHECK                                              
116403     .                                                                    
116503     EJECT                                                                
116603 IMS-ISRT-2214 SECTION.                                                   
116703     STRING 'WLXXBI01(WDG3KEY  =' W-WDGX2213-X ')'                        
116803             DELIMITED BY SIZE INTO SSA1                                  
116903     MOVE 'WLXXBI11 '      TO SSA2                                        
117003     MOVE '  IIGE'         TO GOOD-STATUSCODES                            
117103     CALL CBLTDLI USING ISRT XXBI-PCB DLI-IO-WDGX2214 SSA1 SSA2           
117203     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
117303     PERFORM IMS-STATUSCHECK                                              
117403     .                                                                    
117503     EJECT                                                                
117603 IMS-GU-WDK711 SECTION.                                                   
117703     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
117803          DELIMITED BY SIZE INTO SSA1                                     
117903     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
118003          DELIMITED BY SIZE INTO SSA2                                     
118103     MOVE '  GE' TO GOOD-STATUSCODES                                      
118203     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
118303     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
118403     PERFORM IMS-STATUSCHECK                                              
118503     .                                                                    
118603     SKIP3                                                                
118703 IMS-GHU-WDK712 SECTION.                                                  
118803     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
118903          DELIMITED BY SIZE INTO SSA1                                     
119003     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
119103          DELIMITED BY SIZE INTO SSA2                                     
119203     MOVE '  ' TO GOOD-STATUSCODES                                        
119303     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
119403     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
119503     PERFORM IMS-STATUSCHECK                                              
119603     .                                                                    
119703     EJECT                                                                
119803 IMS-REPL-WDK712 SECTION.                                                 
119903     MOVE '  ' TO GOOD-STATUSCODES                                        
120003     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
120103     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120203     PERFORM IMS-STATUSCHECK                                              
120303     .                                                                    
120403     EJECT                                                                
120503 IMS-GU-WDB601    SECTION.                                                
120603     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
120703          DELIMITED BY SIZE INTO SSA1                                     
120803     MOVE '  GE' TO GOOD-STATUSCODES                                      
120903     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
121003     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
121103     PERFORM IMS-STATUSCHECK                                              
121203     .                                                                    
121303     EJECT                                                                
121403 IMS-STATUSCHECK SECTION.                                                 
121503                                                                          
121603     SET STATUS-IX           TO 1                                         
121703     SEARCH GOOD-STATUS                                                   
121803       AT END                                                             
121903         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
122003         DELIMITED BY SIZE INTO ERROR-TEXT                                
122103         CALL FELLOG                                                      
122203       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
122303         CONTINUE                                                         
122403     END-SEARCH                                                           
123001     .                                                                    
