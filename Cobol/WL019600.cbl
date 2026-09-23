000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL019600.                                                
000400 AUTHOR.         ASPFJÄLL MARKUS.                                         
000500 DATE-WRITTEN.   07/11/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       REQUESTEDPARTINFO                                        
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        REQUESTED PART INFORFMATION                                      
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDK6 CDC                                   
001400*        PROGRAMMET LÄSER      WDK7 LDC                                   
001500*        PROGRAMMET LÄSER      WDD8 BUFFER OMR                            
001600*        PROGRAMMET LÄSER      WDD3 BENÄMNING                             
001700*        PROGRAMMET LÄSER      TP6ARTP                                    
001800*        PROGRAMMET LÄSER      TP6PART                                    
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0196T                                             
002200*        REQUEST:     WL0196I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        RESPONSE:    WL0196O1                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'WL019600'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  LATIN1-LOWERCASE            PIC X(31)                                
005200     VALUE 'àáãçñêëèíîïìßöéðýþæüäôòóõåûùúÿ'.                              
005300 77  LATIN1-UPPERCASE            PIC X(31)                                
005400     VALUE 'ÀÁÃÇÑÊËÈÍÎÏÌßÖÉÐÝÞÆÜÄÔÒÓÕÅÛÙÚÿ'.                              
005500                                                                          
005600 77  WS-SOKNING                  PIC 9       VALUE ZERO.                  
005700 77  WS-FORSTA-POST              PIC X       VALUE SPACE.                 
005800 77  WS-DAP-OPEN                 PIC X       VALUE SPACE.                 
005900 77  INDX                        PIC S9(9)   VALUE ZERO.                  
006000 77  MAX-INDX                    PIC S9(9)   VALUE 500.                   
006100 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
006200 77  WS-NUMBER                   PIC S9(9)   VALUE ZERO COMP-3.           
006300 77  WS-KVRADER-PACK             PIC S9(9)   VALUE ZERO COMP-3.           
006400 77  WS-KVRADER                  PIC S9(9)   VALUE ZERO COMP-3.           
006500 77  WS-ANTAL-DAGAR              PIC S9(9)   VALUE ZERO COMP-3.           
006600 77  WS-6-MANADER                PIC S9(9)   VALUE ZERO COMP-3.           
006700 77  WS-DATUM                    PIC X(8)    VALUE ZERO.                  
006800 77  WS-DATUM-NYTT               PIC X(8)    VALUE ZERO.                  
006900 77  WS-DATUM-6-MAN              PIC X(8)    VALUE ZERO.                  
007000 77  WS-DATUM-6                  PIC S9(7)   VALUE ZERO COMP-3.           
007100 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
007200 77  WS-ADLAGOMR-ZERO            PIC S9(3)   VALUE ZERO COMP-3.           
007300 77  WS-ADGANG-FROM              PIC S9(3)   VALUE ZERO COMP-3.           
007400 77  WS-ADGANG-TOM               PIC S9(3)   VALUE ZERO COMP-3.           
007500 01  WS-BEART                    PIC X(25)   VALUE SPACE.                 
007510 01  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
007600 01  WS-REQU-BEART-KEY           PIC X(25)   VALUE SPACE.                 
007700                                                                          
007800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007900     88  NYCKLAR-OK                          VALUE 'J'.                   
008000     88  NYCKLAR-FEL                         VALUE 'N'.                   
008100 77  AISLE-SW                    PIC X       VALUE 'J'.                   
008200     88  AISLE-YES                           VALUE 'J'.                   
008300     88  AISLE-NOO                           VALUE 'N'.                   
008400     EJECT                                                                
008500                                                                          
008600 01  WS-YYMMDDHHMM.                                                       
008700     03 WS-YYMMDD                PIC  9(6).                               
008800     03 WS-TIME                  PIC  9(4).                               
008900                                                                          
009000 01  WS-HHMMSSTH                 PIC  9(8).                               
009100 01  FILLER REDEFINES WS-HHMMSSTH.                                        
009200       03  WS-HHMM               PIC 9(4).                                
009300       03  WS-SSTH               PIC 9(4).                                
009400     EJECT                                                                
009500                                                                          
009600 01  -COPY WWPRODSL                                                       
009700     EJECT                                                                
009800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009900 01  GENERELLA-SUBPROGRAM.                                                
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
010500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
010600     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
010700     03  WTRAEBCD                PIC X(8)    VALUE 'WTRAEBCD'.            
010800     SKIP3                                                                
010900*    --- PARAMETRAR TILL ABEND                                            
011000                                                                          
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
011500     SKIP3                                                                
011600 01  WZ20DAYS                    PIC X(8) VALUE 'WZ20DAYS'.               
011700     SKIP3                                                                
011800*    -COPY WZ20DAYS                                                       
011900     EJECT                                                                
012000                                                                          
012100 01  WWSKYLT                     PIC X(8) VALUE 'WWSKYLT '.               
012200     SKIP3                                                                
012300*    -COPY WWSKYLT                                                        
012400     EJECT                                                                
012500                                                                          
012600 01  WTRAUTF8-AREA               PIC X(8) VALUE 'WTRAUTF8'.               
012700     SKIP3                                                                
012800*    -COPY WTRAUTF8                                                       
012900     EJECT                                                                
013000                                                                          
013100 01  WTRAEBCD-AREA               PIC X(8) VALUE 'WTRAEBCD'.               
013200     SKIP3                                                                
013300*    -COPY WTRAEBCD                                                       
013400     EJECT                                                                
013500                                                                          
013600 01  MESSAGE-CODES.                                                       
013700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
013800     EJECT                                                                
013900*                                                                         
014000 01  FILLER                      PIC X(16) VALUE 'TAB VARDE'.             
014100 01  TABENTRY-PARM.                                                       
014200     03  STEGLAANGD              PIC S9(9) COMP.                          
014300     03  ANTAL                   PIC S9(9) COMP.                          
014400     03  NYCKELLAANGD            PIC S9(9) COMP.                          
014500 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
014600 01  SORT-TABELL.                                                         
014700     03  TAB-RAD OCCURS 500.                                              
014800        05  TAB-SORT-BEGREPP1.                                            
014900            07 TAB-IDARTNR       PIC S9(9) COMP-3.                        
015000        05  TAB-SORT-BEGREPP2.                                            
015100            07 TAB-ADLAGOMR      PIC S9(3) COMP-3.                        
015200            07 TAB-ADGANG        PIC S9(3) COMP-3.                        
015300            07 TAB-ADPLATS       PIC S9(5) COMP-3.                        
015400        05  TAB-BEART            PIC X(25).                               
015500        05  TAB-KDSORT           PIC X(2).                                
015600        05  TAB-KVLS             PIC S9(7) COMP-3.                        
015700        05  TAB-KVPB-REF         PIC S9(6)V9(1) COMP-3.                   
015800        05  TAB-ADBUFFOMR        PIC 9(2).                                
015900        05  TAB-ADBUFFGANG       PIC 9(2).                                
016000        05  TAB-ADBUFFPL         PIC 9(4).                                
016199                                                                          
016200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016300     SKIP3                                                                
016400*01  -COPY WZ01SUB                                                        
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
016700     SKIP3                                                                
016800 01  REQU-AREA.                                                           
016900*    03  -COPY WZ01REQU                                                   
017000*    03  -COPY WL0196I1                                                   
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017300     SKIP3                                                                
017400 01  RESP-AREA.                                                           
017500*    03  -COPY WZ01RESP                                                   
017600*    03  -COPY WL0196O1                                                   
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
017900 01  HDR-AREA.                                                            
018000*    03  -COPY WZ01REQU  -PRE HDR-                                        
018100*    03  -COPY WZ04HDR                                                    
018200*                                                                         
018300 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
018400 01  DOC-AREA.                                                            
018500*    03  -COPY WL01961                                                    
018600*                                                                         
018700 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
018800     SKIP3                                                                
018900*01  -COPY WZ01SEND                                                       
019000     EJECT                                                                
019100                                                                          
019200*        PROGRAMMET LÄSER      TABELL TP6ARTP                             
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
019500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
019600                                                                          
019700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
019800 01  DB2-WS.                                                              
019900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
020000         88  CURSOR-OK                       VALUE 000.                   
020100         88  RADER-FINNS                     VALUE 000.                   
020200         88  RADER-SAKNAS                    VALUE 100.                   
020300         88  ATKOMST-FEL                     VALUE 904.                   
020400     03  GODK-SQLCODEKODER.                                               
020500         05  GODK-SQLCODE OCCURS 5                                        
020600             INDEXED BY SQLCODE-IX PIC 9(3).                              
020700                                                                          
020800     EJECT                                                                
020900 01  FILLER                      PIC X(16)  VALUE 'TP6ARTP-AREA'.         
021000                                                                          
021100*01  -COPY TP6ARTP -PRE TP6ARTP-                                          
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)  VALUE 'TP6PART-AREA'.         
021400                                                                          
021500*01  -COPY TP6PART -PRE TP6PART-                                          
021600     EJECT                                                                
021700     EXEC SQL INCLUDE TP6ARTP END-EXEC.                                   
021800     EJECT                                                                
021900     EXEC SQL INCLUDE TP6PART END-EXEC.                                   
022000     EJECT                                                                
022100 01  NYCKLAR-TILL-DLI.                                                    
022200     03  W-IDARTNR-X.                                                     
022300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022400     03  W-IDDC-X.                                                        
022500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
022600     03  W-IDSKYLT-X.                                                     
022700         05  W-IDSKYLT           PIC X(3)    VALUE '   '.                 
022800     03  W-IDBENNR-X.                                                     
022900         05  W-IDBENNR           PIC S9(7) COMP-3 VALUE ZERO.             
023000     03  W-BEART-MIN-X.                                                   
023100         05  WS-BEART-MIN        PIC X(25)   VALUE ZERO.                  
023200     03  W-BEART-MAX-X.                                                   
023300         05  WS-BEART-MAX        PIC X(25)   VALUE ZERO.                  
023400     03  W-WDD811KY-MIN-X.                                                
023500         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
023600         05  W-ADBUFFOMR-MIN     PIC S9(3)   VALUE ZERO COMP-3.           
023700         05  W-DABUFPAF-MIN      PIC  9(8)   VALUE ZERO.                  
023800         05  W-ADBUFFGANG-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
023900         05  W-ADBUFFPL-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
024000     03  W-WDD811KY-MAX-X.                                                
024100         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
024200         05  W-ADBUFFOMR-MAX     PIC S9(3)   VALUE +999 COMP-3.           
024300         05  W-ADBUFPAF-MAX      PIC  9(8)   VALUE  99999999.             
024400         05  W-ADBUFFGANG-MAX    PIC S9(3)   VALUE +999 COMP-3.           
024500         05  W-ADBUFFPL-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
024600     03  WDK7A1KY-MIN-X.                                                  
024700         05 W-IDDC-K7-MIN          PIC X(2)           VALUE SPACE.        
024800         05 W-ADART-K7-MIN.                                               
024900           07  W-ADLAGOMR-K7-MIN    PIC S9(3)  COMP-3  VALUE ZERO.        
025000           07  W-ADGANG-K7-MIN      PIC S9(3)  COMP-3  VALUE ZERO.        
025100           07  W-ADPLATS-K7-MIN     PIC S9(5)  COMP-3  VALUE ZERO.        
025200         05 W-IDARTNR-K7-MIN        PIC S9(9)  COMP-3  VALUE ZERO.        
025300     03  WDK7A1KY-MAX-X.                                                  
025400         05  W-IDDC-K7-MAX        PIC X(2)           VALUE SPACE.         
025500         05  W-ADART-K7-MAX.                                              
025600            07  W-ADLAGOMR-K7-MAX PIC S9(3)  COMP-3  VALUE ZERO.          
025700            07  W-ADGANG-K7-MAX   PIC S9(3)  COMP-3  VALUE ZERO.          
025800            07  W-ADPLATS-K7-MAX  PIC S9(5)  COMP-3  VALUE ZERO.          
025900         05 W-IDARTNR-K7-MAX      PIC S9(9)  COMP-3  VALUE ZERO.          
026000                                                                          
026100*                                                                         
026200 01  LAST-IMS-CALL               PIC  X(100) VALUE SPACE.                 
026300 01    FILLER          PIC X(16)   VALUE '     IMS-WS     '.              
026400 01    STATUS-WS       PIC XX.                                            
026500         88  SEGMENT-FINNS       VALUE '  '.                              
026600         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
026700         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
026800         88  END-OF-DB           VALUE 'GB'.                              
026900         88  PARENT-MISSING      VALUE 'GP'.                              
027000                                                                          
027100 01    GODK-STATUSKODER.                                                  
027200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027300                                                                          
027400 01      SSA1            PIC X(128) VALUE SPACE.                          
027500 01      SSA2            PIC X(128) VALUE SPACE.                          
027600 01      SSA3            PIC X(128) VALUE SPACE.                          
027700                                                                          
027800*                            IMS FUNKTIONSKODER                           
027900*01      -COPY W0003                                                      
028000                                                                          
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
028200 01  DLI-IO-WDK701.                                                       
028300*    03  -COPY WDK701                                                     
028400     EJECT                                                                
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028600 01  DLI-IO-WDK711.                                                       
028700*    03  -COPY WDK711                                                     
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
028900 01  DLI-IO-WDK7A1.                                                       
029000*    03  -COPY WDK7A1                                                     
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD801'.                      
029200 01  DLI-IO-WDD801.                                                       
029300*    03  -COPY WDD801 -PRE ARTD-                                          
029400     EJECT                                                                
029500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
029600 01  DLI-IO-WDD811.                                                       
029700*    03  -COPY WDD811 -PRE ARTD-                                          
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD3A1'.                      
029900 01  DLI-IO-WDD3A1.                                                       
030000*    03  -COPY WDD3A1 -PRE  BENA-                                         
030100     EJECT                                                                
030200 01  DLI-IO-WDD301.                                                       
030300*    03  -COPY WDD301 -PRE  BENA-                                         
030400     EJECT                                                                
030500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
030600 01  DLI-IO-WDD311.                                                       
030700*    03  -COPY WDD311 -PRE  BENA-                                         
030800     EJECT                                                                
030900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD312'.                      
031000 01  DLI-IO-WDD312.                                                       
031100*    03  -COPY WDD312 -PRE  BENA-                                         
031200     EJECT                                                                
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
031400 01  DLI-IO-WDK601.                                                       
031500*    03  -COPY WDK601 -PRE ARTC-                                          
031600     EJECT                                                                
031700 LINKAGE SECTION.                                                         
031800*01  -COPY W0009   -PRE MSG-    PIC X.                                    
031900                                                                          
032000 01  DAP-PCB                    PIC X.                                    
032100     EJECT                                                                
032200                                                                          
032300*01  -COPY W0008  -PRE WDK7-                                              
032400     05  FILLER                  PIC X.                                   
032500                                                                          
032600*01  -COPY W0008  -PRE WDK7A-                                             
032700     05  FILLER                  PIC X.                                   
032800                                                                          
032900*01  -COPY W0008  -PRE WDD8-                                              
033000     05  FILLER                  PIC X.                                   
033100                                                                          
033200*01  -COPY W0008  -PRE WDK6-                                              
033300     05  FILLER                  PIC X.                                   
033400                                                                          
033500*01  -COPY W0008  -PRE WDD3-                                              
033600     05  FILLER                  PIC X.                                   
033700                                                                          
033800*01  -COPY W0008  -PRE WDD3A-                                             
033900     05  FILLER                  PIC X.                                   
034000                                                                          
034100*01  -COPY W0008  -PRE WDD3B-                                             
034200     05  FILLER                  PIC X.                                   
034300                                                                          
034400     EJECT                                                                
034500 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB                                
034600                           WDK7-PCB WDK7A-PCB WDD8-PCB                    
034700                           WDK6-PCB WDD3-PCB WDD3A-PCB WDD3B-PCB.         
034800 MAIN SECTION.                                                            
034900     ENTRY 'DLITCBL' USING  MSG-PCB  DAP-PCB                              
035000                            WDK7-PCB WDK7A-PCB WDD8-PCB                   
035100                            WDK6-PCB WDD3-PCB WDD3A-PCB WDD3B-PCB.        
035200                                                                          
035300     PERFORM S01-HAEMTA-ANROPSDATA                                        
035400                                                                          
035500     IF SUB-KDRC = 0                                                      
035600       PERFORM A-INIT                                                     
035700       PERFORM B-KOLLA-NYCKLAR                                            
035800       IF NYCKLAR-OK                                                      
035900         IF REQU-KDPGMACT = 'S'                                           
036000           PERFORM F-LAES-VISA-INFO                                       
036100         END-IF                                                           
036200         IF REQU-KDPGMACT = 'P'                                           
036300           PERFORM G-PRINT-LISTA                                          
036400         END-IF                                                           
036500       END-IF                                                             
036600       PERFORM S02-RETURNERA-SVAR                                         
036700     END-IF                                                               
036800                                                                          
036900     MOVE ZERO TO RETURN-CODE                                             
037000     GOBACK                                                               
037100     .                                                                    
037200     EJECT                                                                
037300                                                                          
037400 A-INIT SECTION.                                                          
037410                                                                          
037500     MOVE ALL '+'   TO RESP-AREA                                          
037600     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
037700                       RESP-IDMSG-INFO                                    
037800                       RESP-IDELMT-ERROR                                  
037900     MOVE 001       TO RESP-IDMSGVER                                      
038000     MOVE ZERO      TO RESP-KVRADER                                       
038100                                                                          
038200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
038300     MOVE LOW-VALUE  TO WDK7A1KY-MIN-X                                    
038400     MOVE HIGH-VALUE TO WDK7A1KY-MAX-X                                    
038500     MOVE REQU-IDDC-KEY TO W-IDDC                                         
038600                           W-IDDC-MIN                                     
038700                           W-IDDC-MAX                                     
038800                           W-IDDC-K7-MAX                                  
038900                           W-IDDC-K7-MIN                                  
039000                           RESP-IDDC-KEY                                  
039100     MOVE 001           TO RESP-IDMSGVER                                  
039200     MOVE ZERO          TO RESP-KVRADER                                   
039300     MOVE SPACE         TO WS-BEART                                       
039310     MOVE SPACE         TO WS-KDSORT                                      
039400                                                                          
039500     INITIALIZE SORT-TABELL                                               
039600     ACCEPT WS-YYMMDD      FROM DATE                                      
039700     ACCEPT WS-HHMMSSTH    FROM TIME                                      
039800     MOVE WS-HHMM          TO WS-TIME                                     
039900                                                                          
040000     .                                                                    
040100     EJECT                                                                
040200 B-KOLLA-NYCKLAR SECTION.                                                 
040300                                                                          
040310* KOLLA IDSPRAK                                                           
040320     IF REQU-IDSPRAK-KEY = ALL '+'                                        
040330       MOVE 'GB '              TO W-IDSKYLT                               
040340                                  RESP-IDSPRAK-KEY                        
040350     ELSE                                                                 
040360       MOVE REQU-IDSPRAK-KEY   TO W-IDSKYLT                               
040370                                  RESP-IDSPRAK-KEY                        
040398     END-IF                                                               
040399                                                                          
040400*    -- MOVE UNMODIFIED BEART-KEY VALUE TO RESP                           
040500     MOVE REQU-BEART-KEY        TO RESP-BEART-KEY                         
040600                                                                          
040700*    -- INPUT IS A HEX STRING REPRESENTING UNICODE UTF-8 VALUE            
040800*    -- CHANGE IT TO SAME FORMAT AS IN DATABASE                           
040900*    -- (UNICODE+SPACE OR EBCDIC CP278)                                   
041000     IF REQU-BEART-KEY = ALL '+'                                          
041100       CONTINUE                                                           
041200     ELSE                                                                 
041300       MOVE REQU-BEART-KEY      TO TRAEBCD-TECONV-FROM                    
041400       PERFORM S05-CALL-WTRAEBCD                                          
041500       MOVE TRAEBCD-TECONV-TO   TO REQU-BEART-KEY                         
041600     END-IF                                                               
041700                                                                          
041900     MOVE JA TO NYCKLAR-SW                                                
042000     IF REQU-KDSORT-KEY         = ALL '+' AND                             
042100        REQU-FREQPACK-KEY       = ALL '+' AND                             
042200        REQU-BEART-KEY          = ALL '+'                                 
042300        MOVE NEJ TO NYCKLAR-SW                                            
042400        MOVE '027'              TO RESP-IDMSG-ERROR                       
042500     END-IF                                                               
042600     IF REQU-KDSORT-KEY         = ALL '+' AND                             
042700        REQU-FREQPACK-KEY       = ALL '+' AND                             
042800        REQU-ADLAGOMR-KEY       = ALL '+' AND                             
042900        REQU-KVRADER-PERIOD-KEY = ALL '+' AND                             
043000        REQU-KVRADER-PACK-KEY   = ALL '+' AND                             
043100        REQU-KDSORT1-KEY        = ALL '+' AND                             
043200        REQU-BEART-KEY          = ALL '+'                                 
043300        MOVE NEJ TO NYCKLAR-SW                                            
043400        MOVE '027'              TO RESP-IDMSG-ERROR                       
043500     END-IF                                                               
043600     IF NYCKLAR-OK                                                        
043700       IF REQU-FREQPACK-KEY NOT = ALL '+' OR                              
043701          REQU-KDSORT-KEY NOT = ALL '+' OR                                
043702          REQU-BEART-KEY NOT = ALL '+'                                    
045601         IF REQU-ADGANG-FROM-KEY = ALL '+'                                
045602           MOVE NOO TO AISLE-SW                                           
045603           MOVE ZERO TO WS-ADGANG-FROM                                    
045604           MOVE ZERO TO WS-ADGANG-TOM                                     
045605         ELSE                                                             
045606           MOVE YES TO AISLE-SW                                           
045607           IF  REQU-ADGANG-FROM-KEY NUMERIC                               
045608           AND REQU-ADGANG-FROM-KEY > ZERO                                
045609             MOVE REQU-ADGANG-FROM-KEY TO WS-ADGANG-FROM                  
045610             COMPUTE WS-ADGANG-FROM = WS-ADGANG-FROM - 1                  
045611           ELSE                                                           
045612             MOVE ZERO TO WS-ADGANG-FROM                                  
045613           END-IF                                                         
045614           IF  REQU-ADGANG-TOM-KEY = ALL '+'                              
045615             MOVE ZERO  TO WS-ADGANG-TOM                                  
045616                           REQU-ADGANG-TOM-KEY                            
045617           END-IF                                                         
045618                                                                          
045619           IF  REQU-ADGANG-TOM-KEY NUMERIC                                
045620           AND REQU-ADGANG-TOM-KEY > ZERO                                 
045621             MOVE REQU-ADGANG-TOM-KEY TO WS-ADGANG-TOM                    
045622             COMPUTE WS-ADGANG-TOM = WS-ADGANG-TOM + 1                    
045623           ELSE                                                           
045624             IF REQU-ADGANG-FROM-KEY > ZERO                               
045625               COMPUTE WS-ADGANG-TOM = WS-ADGANG-FROM + 2                 
045626             ELSE                                                         
045627               MOVE ZERO TO WS-ADGANG-TOM                                 
045628             END-IF                                                       
045629           END-IF                                                         
045630                                                                          
045631           IF  REQU-ADGANG-TOM-KEY < REQU-ADGANG-FROM-KEY                 
045632             COMPUTE WS-ADGANG-TOM = WS-ADGANG-FROM + 2                   
045633           END-IF                                                         
045634         END-IF                                                           
045635         IF REQU-KVRADER-PACK-KEY = ALL '+'                               
045636            IF REQU-FREQPACK-KEY NOT = ALL '+'                            
045637              MOVE 100              TO RESP-KVRADER-PACK-KEY              
045638              MOVE 100              TO WS-KVRADER-PACK                    
045639            ELSE                                                          
045640              MOVE 500              TO WS-KVRADER-PACK                    
045650            END-IF                                                        
049301         ELSE                                                             
049302           IF REQU-KVRADER-PACK-KEY NUMERIC                               
049303             IF REQU-KVRADER-PACK-KEY > 500                               
049304               MOVE '023'        TO RESP-IDMSG-ERROR                      
049305               MOVE 'KVRADER'    TO RESP-IDELMT-ERROR                     
049306               MOVE NEJ          TO NYCKLAR-SW                            
049307             ELSE                                                         
049308               MOVE REQU-KVRADER-PACK-KEY TO RESP-KVRADER-PACK-KEY        
049309               MOVE REQU-KVRADER-PACK-KEY TO WS-KVRADER-PACK              
049310             END-IF                                                       
049311           ELSE                                                           
049312             MOVE '023'          TO RESP-IDMSG-ERROR                      
049313             MOVE 'KVRADER'      TO RESP-IDELMT-ERROR                     
049314             MOVE NEJ            TO NYCKLAR-SW                            
049315           END-IF                                                         
049316         END-IF                                                           
049317         IF REQU-ADLAGOMR-KEY = ALL '+'                                   
049318           MOVE ZERO             TO WS-ADLAGOMR                           
051401         ELSE                                                             
051402           IF REQU-ADLAGOMR-KEY NUMERIC                                   
051403             MOVE REQU-ADLAGOMR-KEY TO RESP-ADLAGOMR-KEY                  
051404                                       WS-ADLAGOMR                        
051405           ELSE                                                           
051406             MOVE '023'          TO RESP-IDMSG-ERROR                      
051407             MOVE 'ADLAGOMR'     TO RESP-IDELMT-ERROR                     
051408             MOVE NEJ            TO NYCKLAR-SW                            
051453           END-IF                                                         
051454         END-IF                                                           
051455       END-IF                                                             
051456     END-IF                                                               
051457     IF NYCKLAR-OK                                                        
051458       IF REQU-FREQPACK-KEY NOT = ALL '+'                                 
051459         MOVE REQU-FREQPACK-KEY TO RESP-FREQPACK-KEY                      
051460                                                                          
051461         IF REQU-FREQPACK-KEY = 'N'                                       
051462           MOVE 4 TO WS-SOKNING                                           
051463           MOVE 4 TO RESP-WEB-LAYOUT                                      
051464         ELSE                                                             
051465           MOVE 1 TO WS-SOKNING                                           
051466           MOVE 1 TO RESP-WEB-LAYOUT                                      
051467         END-IF                                                           
051468         IF REQU-ADLAGOMR-KEY = ALL '+'                                   
051469           IF REQU-FREQPACK-KEY = 'N'                                     
051470              CONTINUE                                                    
051471           ELSE                                                           
051472              MOVE '023'            TO RESP-IDMSG-ERROR                   
051473              MOVE 'ADLAGOMR'       TO RESP-IDELMT-ERROR                  
051474              MOVE NEJ              TO NYCKLAR-SW                         
051475           END-IF                                                         
051476         END-IF                                                           
052500         IF REQU-KVRADER-PERIOD-KEY = ALL '+'                             
052600           IF WS-SOKNING = 4                                              
052700             MOVE 26           TO RESP-KVRADER-PERIOD-KEY                 
052800                                  REQU-KVRADER-PERIOD-KEY                 
052900           ELSE                                                           
053000             MOVE 1            TO RESP-KVRADER-PERIOD-KEY                 
053100                                  REQU-KVRADER-PERIOD-KEY                 
053200           END-IF                                                         
053300           COMPUTE WS-ANTAL-DAGAR = REQU-KVRADER-PERIOD-KEY * 7           
053400                                                                          
053500***  RÄKNA UT DATUM                                                       
053600           MOVE 'YYYYMMDD'     TO DAYS-KDDATFMT1                          
053700           MOVE SPACE          TO DAYS-TIDATE1                            
053800           MOVE 'YYYYMMDD'     TO DAYS-KDDATFMT2                          
053900           MOVE WS-DATUM (1:8) TO DAYS-TIDATE2                            
054000           MOVE WS-ANTAL-DAGAR TO DAYS-KVDAYS                             
054100           MOVE SPACE          TO DAYS-IDCALEND                           
054200                                                                          
054300           CALL WZ20DAYS USING                                            
054400                DAYS-WZ20DAYS                                             
054500           IF DAYS-KDRC = ZERO                                            
054600             MOVE DAYS-TIDATE1 (1:8)  TO WS-DATUM-NYTT                    
054700             MOVE WS-DATUM-NYTT (3:6) TO WS-DATUM-6                       
054800           END-IF                                                         
054900                                                                          
055000         ELSE                                                             
055100           IF REQU-KVRADER-PERIOD-KEY NUMERIC                             
055200             IF WS-SOKNING = 4                                            
055300               IF  REQU-KVRADER-PERIOD-KEY <= 99                          
055400                 MOVE REQU-KVRADER-PERIOD-KEY TO                          
055500                                      RESP-KVRADER-PERIOD-KEY             
055600               ELSE                                                       
055700                 MOVE '023'        TO RESP-IDMSG-ERROR                    
055800                 MOVE 'PERIOD'     TO RESP-IDELMT-ERROR                   
055900                 MOVE NEJ          TO NYCKLAR-SW                          
056000               END-IF                                                     
056100             ELSE                                                         
056200               IF REQU-KVRADER-PERIOD-KEY <= 26                           
056300                 MOVE REQU-KVRADER-PERIOD-KEY TO                          
056400                                      RESP-KVRADER-PERIOD-KEY             
056500               ELSE                                                       
056600                 MOVE '023'        TO RESP-IDMSG-ERROR                    
056700                 MOVE 'PERIOD'     TO RESP-IDELMT-ERROR                   
056800                 MOVE NEJ          TO NYCKLAR-SW                          
056900               END-IF                                                     
057000             END-IF                                                       
057100             COMPUTE WS-ANTAL-DAGAR =                                     
057200                     REQU-KVRADER-PERIOD-KEY * 7                          
057300***    RÄKNA UT DATUM                                                     
057400             MOVE 'YYYYMMDD'   TO DAYS-KDDATFMT1                          
057500             MOVE SPACE        TO DAYS-TIDATE1                            
057600             MOVE 'YYYYMMDD'   TO DAYS-KDDATFMT2                          
057700             MOVE WS-DATUM (1:8) TO DAYS-TIDATE2                          
057800             MOVE WS-ANTAL-DAGAR TO DAYS-KVDAYS                           
057900             MOVE SPACE        TO DAYS-IDCALEND                           
058000                                                                          
058100             CALL WZ20DAYS USING                                          
058200                  DAYS-WZ20DAYS                                           
058300             IF DAYS-KDRC = ZERO                                          
058400               MOVE DAYS-TIDATE1 (1:8) TO WS-DATUM-NYTT                   
058500               MOVE WS-DATUM-NYTT (3:6) TO WS-DATUM-6                     
058600             END-IF                                                       
058700           ELSE                                                           
058800             MOVE '023'        TO RESP-IDMSG-ERROR                        
058900             MOVE 'PERIOD'     TO RESP-IDELMT-ERROR                       
059000             MOVE NEJ          TO NYCKLAR-SW                              
059100           END-IF                                                         
059200         END-IF                                                           
059210       END-IF                                                             
059211     END-IF                                                               
059212* KOLLA KDSORT                                                            
059213     IF NYCKLAR-OK                                                        
059220       IF REQU-KDSORT-KEY NOT = ALL '+'                                   
059900         IF REQU-FREQPACK-KEY NOT = ALL '+'                               
059901           CONTINUE                                                       
059902         ELSE                                                             
059903           IF REQU-BEART-KEY NOT = ALL '+'                                
059904               MOVE '023'        TO RESP-IDMSG-ERROR                      
059905               MOVE 'BEART'      TO RESP-IDELMT-ERROR                     
059906               MOVE NEJ          TO NYCKLAR-SW                            
059907           END-IF                                                         
059908           MOVE 2 TO WS-SOKNING                                           
059909           MOVE 2 TO RESP-WEB-LAYOUT                                      
060800         END-IF                                                           
061001         MOVE REQU-KDSORT-KEY   TO RESP-KDSORT-KEY                        
061002                                   WS-KDSORT                              
061003         IF REQU-KDSORT1-KEY NOT = ALL '+'                                
061004           MOVE REQU-KDSORT1-KEY TO RESP-KDSORT1-KEY                      
061005         ELSE                                                             
061006           MOVE 'P'              TO RESP-KDSORT1-KEY                      
061007         END-IF                                                           
061008       ELSE                                                               
061009         MOVE '%' TO   WS-KDSORT(1:1)                                     
061010         MOVE '%' TO   WS-KDSORT(2:1)                                     
061011       END-IF                                                             
061012     END-IF                                                               
061013* KOLLA BEART                                                             
061014     IF NYCKLAR-OK                                                        
061015       IF REQU-BEART-KEY NOT = ALL '+'                                    
061016         IF REQU-BEART-KEY (1:1) = SPACE OR                               
061017            REQU-BEART-KEY (1:1) = '+'                                    
061018           MOVE '023'        TO RESP-IDMSG-ERROR                          
061019           MOVE 'BEART'      TO RESP-IDELMT-ERROR                         
061020           MOVE NEJ          TO NYCKLAR-SW                                
061021         END-IF                                                           
061022         IF REQU-BEART-KEY (2:1) = SPACE OR                               
061023            REQU-BEART-KEY (2:1) = '+'                                    
061024           MOVE '023'        TO RESP-IDMSG-ERROR                          
061025           MOVE 'BEART'      TO RESP-IDELMT-ERROR                         
061026           MOVE NEJ          TO NYCKLAR-SW                                
061027         END-IF                                                           
061028         IF REQU-BEART-KEY (3:1) = SPACE OR                               
061029            REQU-BEART-KEY (3:1) = '+'                                    
061030           MOVE '023'        TO RESP-IDMSG-ERROR                          
061031           MOVE 'BEART'      TO RESP-IDELMT-ERROR                         
061032           MOVE NEJ          TO NYCKLAR-SW                                
061033         END-IF                                                           
061034                                                                          
064100         IF REQU-FREQPACK-KEY NOT = ALL '+'                               
064101           CONTINUE                                                       
064102         ELSE                                                             
064103           MOVE 3 TO RESP-WEB-LAYOUT                                      
064104           MOVE 3 TO WS-SOKNING                                           
065000         END-IF                                                           
065201         MOVE REQU-BEART-KEY   TO WS-BEART-MIN                            
065202         MOVE REQU-BEART-KEY   TO WS-BEART-MAX                            
065203         MOVE REQU-BEART-KEY   TO WS-BEART                                
065204         INSPECT WS-BEART-MIN REPLACING ALL SPACE BY LOW-VALUE            
065205         INSPECT WS-BEART-MAX REPLACING ALL SPACE BY HIGH-VALUE           
067956       END-IF                                                             
067957     END-IF                                                               
067958                                                                          
067959     IF WS-SOKNING NOT = 2                                                
067960       IF WS-BEART(1:1) = X'25'                                           
067961         MOVE '%' TO WS-BEART(1:1)                                        
067962       END-IF                                                             
067963                                                                          
067964       IF WS-BEART(2:1) = X'25'                                           
067965         MOVE '%' TO WS-BEART(2:1)                                        
067966       END-IF                                                             
067967                                                                          
067968       IF WS-BEART(3:1) = X'25'                                           
067969         MOVE '%' TO WS-BEART(3:1)                                        
067970       END-IF                                                             
067971                                                                          
067972       IF WS-BEART(4:1) = X'25'                                           
067973         MOVE '%' TO WS-BEART(4:1)                                        
067974       END-IF                                                             
067975                                                                          
067976       IF WS-BEART(5:1) = X'25'                                           
067977         MOVE '%' TO WS-BEART(5:1)                                        
067978       END-IF                                                             
067979                                                                          
067980       IF WS-BEART(6:1) = X'25'                                           
067981         MOVE '%' TO WS-BEART(6:1)                                        
067982       END-IF                                                             
067983                                                                          
067984       IF WS-BEART(7:1) = X'25'                                           
067985         MOVE '%' TO WS-BEART(7:1)                                        
067986       END-IF                                                             
067987                                                                          
067988       IF WS-BEART(8:1) = X'25'                                           
067989         MOVE '%' TO WS-BEART(8:1)                                        
067990       END-IF                                                             
067991                                                                          
067992       IF WS-BEART(9:1) = X'25'                                           
067993         MOVE '%' TO WS-BEART(9:1)                                        
067994       END-IF                                                             
067995                                                                          
067996       IF WS-BEART(10:1) = X'25'                                          
067997         MOVE '%' TO WS-BEART(10:1)                                       
067998       END-IF                                                             
067999                                                                          
068000       IF WS-BEART(11:1) = X'25'                                          
068001         MOVE '%' TO WS-BEART(11:1)                                       
068002       END-IF                                                             
068003                                                                          
068004       IF WS-BEART(12:1) = X'25'                                          
068005         MOVE '%' TO WS-BEART(12:1)                                       
068006       END-IF                                                             
068007                                                                          
068008       IF WS-BEART(13:1) = X'25'                                          
068009         MOVE '%' TO WS-BEART(13:1)                                       
068010       END-IF                                                             
068011                                                                          
068012       IF WS-BEART(14:1) = X'25'                                          
068013         MOVE '%' TO WS-BEART(14:1)                                       
068014       END-IF                                                             
068015                                                                          
068016       IF WS-BEART(15:1) = X'25'                                          
068017         MOVE '%' TO WS-BEART(15:1)                                       
068018       END-IF                                                             
068019                                                                          
068020       IF WS-BEART(16:1) = X'25'                                          
068021         MOVE '%' TO WS-BEART(16:1)                                       
068022       END-IF                                                             
068023                                                                          
068024       IF WS-BEART(17:1) = X'25'                                          
068025         MOVE '%' TO WS-BEART(17:1)                                       
068026       END-IF                                                             
068027                                                                          
068028       IF WS-BEART(18:1) = X'25'                                          
068029         MOVE '%' TO WS-BEART(18:1)                                       
068030       END-IF                                                             
068031                                                                          
068032       IF WS-BEART(19:1) = X'25'                                          
068033         MOVE '%' TO WS-BEART(19:1)                                       
068034       END-IF                                                             
068035                                                                          
068036       IF WS-BEART(20:1) = X'25'                                          
068037         MOVE '%' TO WS-BEART(20:1)                                       
068038       END-IF                                                             
068039                                                                          
068040       IF WS-BEART(21:1) = X'25'                                          
068041         MOVE '%' TO WS-BEART(21:1)                                       
068042       END-IF                                                             
068043                                                                          
068044       IF WS-BEART(22:1) = X'25'                                          
068045         MOVE '%' TO WS-BEART(22:1)                                       
068046       END-IF                                                             
068047                                                                          
068048       IF WS-BEART(23:1) = X'25'                                          
068049         MOVE '%' TO WS-BEART(23:1)                                       
068050       END-IF                                                             
068051                                                                          
068052       IF WS-BEART(24:1) = X'25'                                          
068053         MOVE '%' TO WS-BEART(24:1)                                       
068054       END-IF                                                             
068055                                                                          
068056       IF WS-BEART(25:1) = X'25'                                          
068057         MOVE '%' TO WS-BEART(25:1)                                       
068058       END-IF                                                             
068059                                                                          
068060       IF WS-BEART(25:1) = SPACE                                          
068061         MOVE '%' TO WS-BEART(25:1)                                       
068062       END-IF                                                             
068063       IF WS-BEART(24:1) = SPACE                                          
068064         MOVE '%' TO WS-BEART(24:1)                                       
068065       END-IF                                                             
068066       IF WS-BEART(23:1) = SPACE                                          
068067         MOVE '%' TO WS-BEART(23:1)                                       
068068       END-IF                                                             
068069       IF WS-BEART(22:1) = SPACE                                          
068070         MOVE '%' TO WS-BEART(22:1)                                       
068071       END-IF                                                             
068072       IF WS-BEART(21:1) = SPACE                                          
068073         MOVE '%' TO WS-BEART(21:1)                                       
068074       END-IF                                                             
068075       IF WS-BEART(20:1) = SPACE                                          
068076         MOVE '%' TO WS-BEART(20:1)                                       
068077       END-IF                                                             
068078       IF WS-BEART(19:1) = SPACE                                          
068079         MOVE '%' TO WS-BEART(19:1)                                       
068080       END-IF                                                             
068081       IF WS-BEART(18:1) = SPACE                                          
068082         MOVE '%' TO WS-BEART(18:1)                                       
068083       END-IF                                                             
068084       IF WS-BEART(17:1) = SPACE                                          
068085         MOVE '%' TO WS-BEART(17:1)                                       
068086       END-IF                                                             
068087       IF WS-BEART(16:1) = SPACE                                          
068088         MOVE '%' TO WS-BEART(16:1)                                       
068089       END-IF                                                             
068090       IF WS-BEART(15:1) = SPACE                                          
068091         MOVE '%' TO WS-BEART(15:1)                                       
068092       END-IF                                                             
068093       IF WS-BEART(14:1) = SPACE                                          
068094         MOVE '%' TO WS-BEART(14:1)                                       
068095       END-IF                                                             
068096       IF WS-BEART(13:1) = SPACE                                          
068097         MOVE '%' TO WS-BEART(13:1)                                       
068098       END-IF                                                             
068099       IF WS-BEART(12:1) = SPACE                                          
068100         MOVE '%' TO WS-BEART(12:1)                                       
068101       END-IF                                                             
068102       IF WS-BEART(11:1) = SPACE                                          
068103         MOVE '%' TO WS-BEART(11:1)                                       
068104       END-IF                                                             
068105       IF WS-BEART(10:1) = SPACE                                          
068106         MOVE '%' TO WS-BEART(10:1)                                       
068107       END-IF                                                             
068108       IF WS-BEART(9:1) = SPACE                                           
068109         MOVE '%' TO WS-BEART(9:1)                                        
068110       END-IF                                                             
068111       IF WS-BEART(8:1) = SPACE                                           
068112         MOVE '%' TO WS-BEART(8:1)                                        
068113       END-IF                                                             
068114       IF WS-BEART(7:1) = SPACE                                           
068115         MOVE '%' TO WS-BEART(7:1)                                        
068116       END-IF                                                             
068117       IF WS-BEART(6:1) = SPACE                                           
068118         MOVE '%' TO WS-BEART(6:1)                                        
068119       END-IF                                                             
068120       IF WS-BEART(5:1) = SPACE                                           
068121         MOVE '%' TO WS-BEART(5:1)                                        
068122       END-IF                                                             
068123       IF WS-BEART(4:1) = SPACE                                           
068124         MOVE '%' TO WS-BEART(4:1)                                        
068125       END-IF                                                             
068126       IF WS-BEART(3:1) = SPACE                                           
068127         MOVE '%' TO WS-BEART(3:1)                                        
068128       END-IF                                                             
068129       IF WS-BEART(2:1) = SPACE                                           
068130         MOVE '%' TO WS-BEART(2:1)                                        
068131       END-IF                                                             
068132       IF WS-BEART(1:1) = SPACE                                           
068133         MOVE '%' TO WS-BEART(1:1)                                        
068134       END-IF                                                             
068135     END-IF                                                               
068136                                                                          
068137     IF NYCKLAR-FEL                                                       
068138       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
068139     END-IF                                                               
068140     .                                                                    
068200     EJECT                                                                
068300 F-LAES-VISA-INFO SECTION.                                                
068310                                                                          
068400     IF WS-SOKNING = 1                                                    
068500       PERFORM FA-LAES-GRUNDDATA                                          
068600     END-IF                                                               
068700                                                                          
068800     IF WS-SOKNING = 2                                                    
068900       PERFORM FB-LAES-GRUNDDATA                                          
069000     END-IF                                                               
069100                                                                          
069200     IF WS-SOKNING = 3                                                    
069300       PERFORM FC-LAES-GRUNDDATA                                          
069400     END-IF                                                               
069500                                                                          
069600     IF WS-SOKNING = 4                                                    
069700       PERFORM FD-LAES-GRUNDDATA                                          
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 FA-LAES-GRUNDDATA SECTION.                                               
070110                                                                          
070200* SÖKNING 1 LÄS TP6ARTP                                                   
070300     MOVE +0 TO INDX                                                      
070400     MOVE +0 TO WS-KVRADER                                                
070410                                                                          
070600     IF REQU-FREQPACK-KEY = 'M'                                           
073020       PERFORM DB2-DCL-OPN-TP6ARTP-CRS-M                                  
073021       PERFORM DB2-FETCH-TP6ARTP-CRS-M                                    
076701     ELSE                                                                 
076702       PERFORM DB2-DCL-OPN-TP6ARTP-CRS-L                                  
076703       PERFORM DB2-FETCH-TP6ARTP-CRS-L                                    
079730     END-IF                                                               
079731                                                                          
079740     IF RADER-SAKNAS                                                      
079780       MOVE '027' TO RESP-IDMSG-ERROR                                     
079791     END-IF                                                               
079792                                                                          
079793     PERFORM UNTIL RADER-SAKNAS OR INDX >= MAX-INDX OR                    
079794                   INDX >= WS-KVRADER-PACK                                
079795       MOVE WS-IDARTNR             TO W-IDARTNR                           
079796                                                                          
079797       PERFORM IMS-GET-WDK701                                             
079798       IF SEGMENT-FINNS                                                   
079799         PERFORM IMS-GNP-WDK711                                           
079800         IF SEGMENT-FINNS                                                 
079803           IF  SLAG-ADLAGOMR  = WS-ADLAGOMR                               
079805             IF AISLE-NOO                                                 
079807               PERFORM FAA-MOVE-DATA                                      
081300             ELSE                                                         
081302               IF SLAG-ADGANG > WS-ADGANG-FROM                            
081303               AND SLAG-ADGANG < WS-ADGANG-TOM                            
081304                 PERFORM FAA-MOVE-DATA                                    
082702               END-IF                                                     
082802             END-IF                                                       
082803           END-IF                                                         
082804         END-IF                                                           
082805       END-IF                                                             
082917                                                                          
082932       IF REQU-FREQPACK-KEY = 'M'                                         
082933         PERFORM DB2-FETCH-TP6ARTP-CRS-M                                  
082934       ELSE                                                               
082935         PERFORM DB2-FETCH-TP6ARTP-CRS-L                                  
082940       END-IF                                                             
082950     END-PERFORM                                                          
082960                                                                          
083000     MOVE WS-KVRADER TO RESP-KVRADER                                      
083001                                                                          
083002     IF REQU-FREQPACK-KEY = 'M'                                           
083003       PERFORM DB2-CLOSE-TP6ARTP-CRS-M                                    
083007     ELSE                                                                 
083008       PERFORM DB2-CLOSE-TP6ARTP-CRS-L                                    
083040     END-IF                                                               
083050     .                                                                    
083051     EJECT                                                                
083052 FAA-MOVE-DATA SECTION.                                                   
083053                                                                          
083054     ADD +1 TO WS-KVRADER                                                 
083055     ADD +1 TO INDX                                                       
083056     MOVE WS-IDARTNR                TO RESP-IDARTNR-UT(INDX)              
083057     MOVE WS-NUMBER                 TO RESP-PACK-ANTAL-UT(INDX)           
083058                                                                          
083059     MOVE SLAG-ADLAGOMR             TO RESP-ADLAGOMR-UT(INDX)             
083060     MOVE SLAG-ADGANG               TO RESP-ADGANG-UT  (INDX)             
083061     MOVE SLAG-ADPLATS              TO RESP-ADPLATS-UT (INDX)             
083062     MOVE SLAG-KVLS                 TO RESP-KVLS-UT    (INDX)             
083063     MOVE SLAG-KVPB-REF             TO RESP-KVPB-REF-UT (INDX)            
083064                                                                          
083065     PERFORM IMS-GET-WDD3B1                                               
083066     IF SEGMENT-FINNS                                                     
083067       MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM                        
083068     ELSE                                                                 
083069       MOVE SPACE                   TO TRAUTF8-TECONV-FROM                
083070     END-IF                                                               
083071     PERFORM S06-CALL-WTRAUTF8                                            
083072     MOVE TRAUTF8-TECONV-TO TO RESP-BEART-UT (INDX)                       
083073                               DOC-BEART                                  
083074                                                                          
083075     PERFORM IMS-GET-WDD801                                               
083076     IF SEGMENT-FINNS                                                     
083077       PERFORM IMS-GNP-WDD811                                             
083078       IF SEGMENT-FINNS                                                   
083079         MOVE ARTD-SALDO-ADBUFFOMR          TO                            
083080                                 RESP-ADBUFFOMR-UT(INDX)                  
083081         MOVE ARTD-SALDO-ADBUFFGANG TO                                    
083082                                 RESP-ADBUFFGANG-UT(INDX)                 
083083         MOVE ARTD-SALDO-ADBUFFPL           TO                            
083084                                 RESP-ADBUFFPL-UT(INDX)                   
083085       END-IF                                                             
083086     END-IF                                                               
083088     .                                                                    
083089     EJECT                                                                
083090 FB-LAES-GRUNDDATA SECTION.                                               
083093                                                                          
083094     MOVE +0 TO INDX                                                      
083095     PERFORM DB2-DCL-OPN-TP6PART-SORT                                     
083096     PERFORM DB2-FETCH-TP6PART-SORT                                       
083097                                                                          
083098     IF RADER-SAKNAS                                                      
083099       MOVE '027' TO RESP-IDMSG-ERROR                                     
083100     END-IF                                                               
083101                                                                          
083102     PERFORM UNTIL INDX >= MAX-INDX OR RADER-SAKNAS OR                    
083103                   INDX >= WS-KVRADER-PACK                                
083104       MOVE TP6PART-IDARTNR    TO W-IDARTNR                               
083105                                                                          
083106       PERFORM IMS-GHU-WDK711                                             
083107       IF SEGMENT-FINNS                                                   
083108         IF WS-ADLAGOMR = ZERO                                            
083109            PERFORM FBB-MOVE-DATA                                         
083110         ELSE                                                             
083111           IF  SLAG-ADLAGOMR  = WS-ADLAGOMR                               
083112             IF AISLE-NOO                                                 
083113               PERFORM FBB-MOVE-DATA                                      
083114             ELSE                                                         
083115               IF SLAG-ADGANG > WS-ADGANG-FROM                            
083116               AND SLAG-ADGANG < WS-ADGANG-TOM                            
083117                 PERFORM FBB-MOVE-DATA                                    
083118               END-IF                                                     
083200             END-IF                                                       
083301           END-IF                                                         
083302         END-IF                                                           
083303       END-IF                                                             
083304                                                                          
083700       PERFORM DB2-FETCH-TP6PART-SORT                                     
083800     END-PERFORM                                                          
083810                                                                          
083900     PERFORM DB2-CLOSE-TP6PART-SORT                                       
083910                                                                          
084000     IF INDX = 501                                                        
084100       MOVE 500 TO INDX                                                   
084200     END-IF                                                               
084210                                                                          
084300     MOVE INDX TO RESP-KVRADER                                            
084400                  ANTAL                                                   
084500                  WS-KVRADER                                              
084510                                                                          
084600     IF WS-KVRADER = +0                                                   
084700       MOVE '027' TO RESP-IDMSG-ERROR                                     
084800     END-IF                                                               
084810                                                                          
084820     IF ANTAL >= 1                                                        
085000**** SORTERA TABELLEN  ****                                               
085100       MOVE +55 TO STEGLAANGD                                             
085200       IF REQU-KDSORT1-KEY   = 'P'                                        
085300         MOVE +5    TO NYCKELLAANGD                                       
085901                                                                          
085902         CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                  
085903         TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                
085904       ELSE                                                               
085905         IF REQU-KDSORT1-KEY   = 'L'                                      
085906           MOVE +7    TO NYCKELLAANGD                                     
086000                                                                          
086100           CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                
086200           TAB-SORT-BEGREPP2(1) NYCKELLAANGD                              
086300         END-IF                                                           
086400       END-IF                                                             
086500                                                                          
086600**** FLYTTA TABELL TILL RESP-AREA                                         
086700                                                                          
086800       MOVE +1 TO INDX                                                    
086900       PERFORM UNTIL INDX > WS-KVRADER                                    
087000         MOVE TAB-IDARTNR     (INDX) TO RESP-IDARTNR-UT   (INDX)          
087100         MOVE TAB-BEART       (INDX) TO RESP-BEART-UT     (INDX)          
087200         MOVE TAB-KDSORT      (INDX) TO RESP-KDSORT-UT    (INDX)          
087300         MOVE TAB-KVLS        (INDX) TO RESP-KVLS-UT      (INDX)          
087400         MOVE TAB-KVPB-REF    (INDX) TO RESP-KVPB-REF-UT  (INDX)          
087500         MOVE TAB-ADLAGOMR    (INDX) TO RESP-ADLAGOMR-UT  (INDX)          
087600         MOVE TAB-ADGANG      (INDX) TO RESP-ADGANG-UT    (INDX)          
087700         MOVE TAB-ADPLATS     (INDX) TO RESP-ADPLATS-UT   (INDX)          
087800         IF TAB-ADBUFFOMR(INDX) > 0                                       
087900           MOVE TAB-ADBUFFOMR   (INDX) TO RESP-ADBUFFOMR-UT (INDX)        
088000           MOVE TAB-ADBUFFGANG  (INDX) TO RESP-ADBUFFGANG-UT(INDX)        
088100           MOVE TAB-ADBUFFPL    (INDX) TO RESP-ADBUFFPL-UT  (INDX)        
088200         END-IF                                                           
088300         ADD +1 TO INDX                                                   
088400       END-PERFORM                                                        
088410     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088610 FBB-MOVE-DATA SECTION.                                                   
088611                                                                          
088620     ADD +1 TO INDX                                                       
088630     MOVE TP6PART-KDSORT          TO TAB-KDSORT      (INDX)               
088640     MOVE W-IDARTNR               TO TAB-IDARTNR     (INDX)               
088650                                                                          
088660     MOVE SLAG-KVLS               TO TAB-KVLS        (INDX)               
088670     MOVE SLAG-KVPB-REF           TO TAB-KVPB-REF    (INDX)               
088680     MOVE SLAG-ADLAGOMR           TO TAB-ADLAGOMR    (INDX)               
088690     MOVE SLAG-ADGANG             TO TAB-ADGANG      (INDX)               
088691     MOVE SLAG-ADPLATS            TO TAB-ADPLATS     (INDX)               
088692                                                                          
088693     MOVE TP6PART-BEART       TO TRAUTF8-TECONV-FROM                      
088694     PERFORM S06-CALL-WTRAUTF8                                            
088695     MOVE TRAUTF8-TECONV-TO TO TAB-BEART(INDX)                            
088696                                                                          
088697     PERFORM IMS-GET-WDD801                                               
088698     IF SEGMENT-FINNS                                                     
088699       PERFORM IMS-GNP-WDD811                                             
088700       IF SEGMENT-FINNS                                                   
088701         MOVE ARTD-SALDO-ADBUFFOMR TO TAB-ADBUFFOMR    (INDX)             
088702         MOVE ARTD-SALDO-ADBUFFGANG TO TAB-ADBUFFGANG (INDX)              
088703         MOVE ARTD-SALDO-ADBUFFPL    TO TAB-ADBUFFPL   (INDX)             
088704       END-IF                                                             
088705     END-IF                                                               
088707     .                                                                    
088708     EJECT                                                                
088710 FC-LAES-GRUNDDATA SECTION.                                               
088800***  SÖKNING MED BENÄMING *****                                           
088900                                                                          
089000*    -- CHANGE UNICODE % TO EBCDIC %                                      
089100     MOVE ZERO TO INDX                                                    
106600                                                                          
106700     PERFORM DB2-DCL-OPN-TP6PART-LANG                                     
106800     PERFORM DB2-FETCH-TP6PART-LANG                                       
106900                                                                          
107000     IF RADER-SAKNAS                                                      
107100       MOVE '027' TO RESP-IDMSG-ERROR                                     
107200     END-IF                                                               
107300                                                                          
107500     PERFORM UNTIL INDX >= MAX-INDX  OR RADER-SAKNAS OR                   
107510                   INDX >= WS-KVRADER-PACK                                
113081       IF WS-ADLAGOMR = ZERO                                              
113082          PERFORM FCC-MOVE-DATA                                           
113083       ELSE                                                               
113084         IF  TP6PART-ADLAGOMR  = WS-ADLAGOMR                              
113085           IF AISLE-NOO                                                   
113086             PERFORM FCC-MOVE-DATA                                        
113087           ELSE                                                           
113088             IF TP6PART-ADGANG  > WS-ADGANG-FROM                          
113089             AND TP6PART-ADGANG < WS-ADGANG-TOM                           
113090               PERFORM FCC-MOVE-DATA                                      
113091             END-IF                                                       
113092           END-IF                                                         
113113         END-IF                                                           
113114       END-IF                                                             
113115       PERFORM DB2-FETCH-TP6PART-LANG                                     
113121     END-PERFORM                                                          
113122     PERFORM DB2-CLOSE-TP6PART-LANG                                       
113128     IF INDX >= 1                                                         
113129       IF INDX = 501                                                      
113130         MOVE 500 TO INDX                                                 
113131       END-IF                                                             
113132       MOVE INDX TO RESP-KVRADER                                          
113133     ELSE                                                                 
113134       MOVE '027' TO RESP-IDMSG-ERROR                                     
113135     END-IF                                                               
113136     .                                                                    
113137     EJECT                                                                
113139 FCC-MOVE-DATA SECTION.                                                   
113140                                                                          
113141     ADD +1 TO INDX                                                       
113142     MOVE TP6PART-ADLAGOMR         TO RESP-ADLAGOMR-UT  (INDX)            
113143     MOVE TP6PART-ADGANG           TO RESP-ADGANG-UT    (INDX)            
113144     MOVE TP6PART-ADPLATS          TO RESP-ADPLATS-UT   (INDX)            
113145     MOVE TP6PART-KVLS             TO RESP-KVLS-UT      (INDX)            
113146                                                                          
113147     MOVE TP6PART-BEART            TO TRAUTF8-TECONV-FROM                 
113148     PERFORM S06-CALL-WTRAUTF8                                            
113149     MOVE TRAUTF8-TECONV-TO        TO RESP-BEART-UT     (INDX)            
113150                                                                          
113151     MOVE TP6PART-IDARTNR         TO W-IDARTNR                            
113152     MOVE W-IDARTNR               TO RESP-IDARTNR-UT   (INDX)             
113153                                                                          
113154     PERFORM IMS-GHU-WDK711                                               
113155     IF SEGMENT-FINNS                                                     
113156       MOVE SLAG-ADLAGOMR          TO RESP-ADLAGOMR-UT  (INDX)            
113157       MOVE SLAG-ADGANG            TO RESP-ADGANG-UT    (INDX)            
113158       MOVE SLAG-ADPLATS           TO RESP-ADPLATS-UT   (INDX)            
113159       MOVE SLAG-KVLS              TO RESP-KVLS-UT      (INDX)            
113160     END-IF                                                               
113161                                                                          
113162     PERFORM IMS-GET-WDD801                                               
113163     IF SEGMENT-FINNS                                                     
113164       PERFORM IMS-GNP-WDD811                                             
113165       IF SEGMENT-FINNS                                                   
113166         MOVE ARTD-SALDO-ADBUFFOMR TO RESP-ADBUFFOMR-UT (INDX)            
113167         MOVE ARTD-SALDO-ADBUFFGANG TO                                    
113168                         RESP-ADBUFFGANG-UT(INDX)                         
113169         MOVE ARTD-SALDO-ADBUFFPL  TO RESP-ADBUFFPL-UT  (INDX)            
113170       END-IF                                                             
113171     END-IF                                                               
113172     .                                                                    
113173     EJECT                                                                
113174 FD-LAES-GRUNDDATA SECTION.                                               
113200* SÖKNING 1 LÄS TP6PART                                                   
113300     MOVE +0 TO INDX                                                      
113400     MOVE +0 TO WS-KVRADER                                                
113500**** SÖKNING UTAN AISLE                                                   
113600     IF AISLE-NOO                                                         
113700       PERFORM DB2-DCL-OPN-TP6PART                                        
113800       PERFORM DB2-FETCH-TP6PART                                          
113900     ELSE                                                                 
114000       PERFORM DB2-DCL-OPN-TP6PART-AISLE                                  
114100       PERFORM DB2-FETCH-TP6PART-AISLE                                    
114200     END-IF                                                               
114210                                                                          
114300     IF RADER-SAKNAS                                                      
114400       MOVE '027' TO RESP-IDMSG-ERROR                                     
114500     END-IF                                                               
114510                                                                          
114600     PERFORM UNTIL RADER-SAKNAS OR INDX >= MAX-INDX OR                    
114700                   INDX >= WS-KVRADER-PACK                                
114710       IF WS-ADLAGOMR = ZERO                                              
114730          PERFORM FDD-MOVE-DATA                                           
115200       ELSE                                                               
115201         IF  TP6PART-ADLAGOMR  = WS-ADLAGOMR                              
115202           PERFORM FDD-MOVE-DATA                                          
115203         END-IF                                                           
116500       END-IF                                                             
116510**** PRODUKTSLAG 19 SKALL EJ VISAS PÅ LISTAN                              
116700       IF AISLE-NOO                                                       
116800         PERFORM DB2-FETCH-TP6PART                                        
116900       ELSE                                                               
117000         PERFORM DB2-FETCH-TP6PART-AISLE                                  
117100       END-IF                                                             
117200     END-PERFORM                                                          
117210                                                                          
117300     MOVE WS-KVRADER TO RESP-KVRADER                                      
117400     IF AISLE-NOO                                                         
117500       PERFORM DB2-CLOSE-TP6PART                                          
117600     ELSE                                                                 
117700       PERFORM DB2-CLOSE-TP6PART-AISLE                                    
117800     END-IF                                                               
117900     .                                                                    
118000     EJECT                                                                
118001 FDD-MOVE-DATA SECTION.                                                   
118002                                                                          
118003     MOVE TP6PART-KDPRODSL        TO TEST-KDPRODSL                        
118004     IF KDPRODSL-EMB                                                      
118005       CONTINUE                                                           
118007     ELSE                                                                 
118009       ADD +1 TO WS-KVRADER                                               
118010       ADD +1 TO INDX                                                     
118011       MOVE TP6PART-IDARTNR       TO RESP-IDARTNR-UT (INDX)               
118012       MOVE TP6PART-ADLAGOMR      TO RESP-ADLAGOMR-UT(INDX)               
118013       MOVE TP6PART-ADGANG        TO RESP-ADGANG-UT  (INDX)               
118014       MOVE TP6PART-ADPLATS       TO RESP-ADPLATS-UT (INDX)               
118015       MOVE TP6PART-KVLS          TO RESP-KVLS-UT    (INDX)               
118016       MOVE TP6PART-KVPB-REF      TO RESP-KVPB-REF-UT(INDX)               
118017       MOVE TP6PART-TIREFEFT      TO RESP-TIREFEFT-UT(INDX)               
118018       MOVE TP6PART-BEART         TO TRAUTF8-TECONV-FROM                  
118019       PERFORM S06-CALL-WTRAUTF8                                          
118020       MOVE TRAUTF8-TECONV-TO     TO RESP-BEART-UT(INDX)                  
118021     END-IF                                                               
118022     .                                                                    
118023     EJECT                                                                
118100 G-PRINT-LISTA SECTION.                                                   
118110                                                                          
118200     IF WS-SOKNING = 1                                                    
118300       PERFORM GA-PRINT-FREQ                                              
118400     END-IF                                                               
118500     IF WS-SOKNING = 2                                                    
118600       PERFORM GB-PRINT-KDSORT                                            
118700     END-IF                                                               
118800     IF WS-SOKNING = 3                                                    
118900       PERFORM GC-PRINT-BEART                                             
119000     END-IF                                                               
119100     IF WS-SOKNING = 4                                                    
119200       PERFORM GD-PRINT-NIL                                               
119300     END-IF                                                               
119400     .                                                                    
119500     EJECT                                                                
119600*                                                                         
119700 GA-PRINT-FREQ SECTION.                                                   
119710                                                                          
119800     MOVE +0 TO INDX                                                      
119900     MOVE +0 TO WS-KVRADER                                                
120000     MOVE JA  TO WS-FORSTA-POST                                           
120100     MOVE NEJ TO WS-DAP-OPEN                                              
120200                                                                          
120501     IF REQU-FREQPACK-KEY = 'M'                                           
120503       PERFORM DB2-DCL-OPN-TP6ARTP-CRS-M                                  
120600       PERFORM DB2-FETCH-TP6ARTP-CRS-M                                    
121100     ELSE                                                                 
121101       PERFORM DB2-DCL-OPN-TP6ARTP-CRS-L                                  
121102       PERFORM DB2-FETCH-TP6ARTP-CRS-L                                    
122833     END-IF                                                               
122834                                                                          
122835     IF RADER-FINNS                                                       
122836       IF WS-FORSTA-POST = 'J'                                            
122838          MOVE NEJ TO WS-FORSTA-POST                                      
122839          MOVE JA  TO WS-DAP-OPEN                                         
122840          PERFORM S04-SKICKA-OPEN                                         
122841          PERFORM S04-SKAPA-HEADER-FREQ                                   
122842          PERFORM S04-PUT-DAP-HEADER                                      
122843       END-IF                                                             
122846     END-IF                                                               
122847                                                                          
122900     PERFORM UNTIL RADER-SAKNAS OR INDX >= MAX-INDX OR                    
123000                   INDX >= WS-KVRADER-PACK                                
123100       MOVE WS-IDARTNR             TO W-IDARTNR                           
123300       PERFORM IMS-GET-WDK701                                             
123400       IF SEGMENT-FINNS                                                   
123500         PERFORM IMS-GNP-WDK711                                           
123600         IF SEGMENT-FINNS                                                 
123610           IF  SLAG-ADLAGOMR  = WS-ADLAGOMR                               
123630             IF AISLE-NOO                                                 
123650               PERFORM GAA-MOVE-DATA                                      
123660             ELSE                                                         
123680               IF SLAG-ADGANG > WS-ADGANG-FROM                            
123690               AND SLAG-ADGANG < WS-ADGANG-TOM                            
123691                 PERFORM GAA-MOVE-DATA                                    
123692               END-IF                                                     
128400             END-IF                                                       
128500           END-IF                                                         
128800         END-IF                                                           
128900       END-IF                                                             
128901                                                                          
128902       IF REQU-FREQPACK-KEY = 'M'                                         
128903         PERFORM DB2-FETCH-TP6ARTP-CRS-M                                  
129600       ELSE                                                               
129700         PERFORM DB2-FETCH-TP6ARTP-CRS-L                                  
130200       END-IF                                                             
130300     END-PERFORM                                                          
130310                                                                          
130400     MOVE WS-KVRADER TO RESP-KVRADER                                      
130500                                                                          
131200     IF REQU-FREQPACK-KEY = 'M'                                           
131300       PERFORM DB2-CLOSE-TP6ARTP-CRS-M                                    
131400     ELSE                                                                 
131500       PERFORM DB2-CLOSE-TP6ARTP-CRS-L                                    
131601     END-IF                                                               
131602                                                                          
131603     IF WS-DAP-OPEN = 'J'                                                 
131604       PERFORM S04-SKICKA-CLOSE                                           
131605       MOVE '015'   TO RESP-IDMSG-INFO                                    
131606     END-IF                                                               
132200     .                                                                    
132300     EJECT                                                                
132400*                                                                         
132410 GAA-MOVE-DATA   SECTION.                                                 
132411                                                                          
132420     ADD +1 TO WS-KVRADER                                                 
132430     ADD +1 TO INDX                                                       
132440     MOVE 'LINE'               TO DOC-IDAFPRCD                            
132450     MOVE REQU-IDDC-KEY        TO DOC-IDDC                                
132460     MOVE WS-IDARTNR           TO RESP-IDARTNR-UT(INDX)                   
132470                                    DOC-IDARTNR                           
132480     MOVE WS-NUMBER            TO RESP-PACK-ANTAL-UT(INDX)                
132490                                    DOC-KVRADER-PACK                      
132491                                                                          
132492     MOVE SLAG-ADLAGOMR        TO RESP-ADLAGOMR-UT(INDX)                  
132493                                    DOC-ADLAGOMR                          
132494     MOVE SLAG-ADGANG          TO RESP-ADGANG-UT  (INDX)                  
132495                                    DOC-ADGANG                            
132496     MOVE SLAG-ADPLATS         TO RESP-ADPLATS-UT (INDX)                  
132497                                    DOC-ADPLATS                           
132498     MOVE SLAG-KVLS            TO RESP-KVLS-UT    (INDX)                  
132499                                    DOC-KVLS                              
132500     MOVE SLAG-KVPB-REF        TO RESP-KVPB-REF-UT (INDX)                 
132501                                    DOC-KVPB-REF                          
132502     IF REQU-FREQPACK-KEY = 'M'                                           
132503       MOVE 'MOST'               TO DOC-LIST-TYP                          
132504     ELSE                                                                 
132505       MOVE 'LESS'               TO DOC-LIST-TYP                          
132506     END-IF                                                               
132507                                                                          
132508     PERFORM IMS-GET-WDD3B1                                               
132509     IF SEGMENT-FINNS                                                     
132510       MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                     
132511     ELSE                                                                 
132512       MOVE SPACE              TO TRAUTF8-TECONV-FROM                     
132513     END-IF                                                               
132514     PERFORM S06-CALL-WTRAUTF8                                            
132515     MOVE TRAUTF8-TECONV-TO    TO RESP-BEART-UT (INDX)                    
132516                                   DOC-BEART                              
132517                                                                          
132518     PERFORM IMS-GET-WDD801                                               
132519     IF SEGMENT-FINNS                                                     
132520       PERFORM IMS-GNP-WDD811                                             
132521       IF SEGMENT-FINNS                                                   
132522         MOVE ARTD-SALDO-ADBUFFOMR TO                                     
132523                                    RESP-ADBUFFOMR-UT (INDX)              
132524                                       DOC-ADBUFFOMR                      
132525         MOVE ARTD-SALDO-ADBUFFGANG TO                                    
132526                                   RESP-ADBUFFGANG-UT (INDX)              
132527                                       DOC-ADBUFFGANG                     
132528         MOVE ARTD-SALDO-ADBUFFPL TO RESP-ADBUFFPL-UT (INDX)              
132529                                       DOC-ADBUFFPL                       
132530       END-IF                                                             
132531     END-IF                                                               
132532     PERFORM S04-PUT-DOC                                                  
132533     MOVE ALL '+' TO DOC-AREA                                             
132534     .                                                                    
132535     EJECT                                                                
132536*                                                                         
132537                                                                          
132550 GB-PRINT-KDSORT SECTION.                                                 
132560                                                                          
132600     MOVE +0 TO INDX                                                      
132700                                                                          
132800     PERFORM DB2-DCL-OPN-TP6PART-SORT                                     
132900     PERFORM DB2-FETCH-TP6PART-SORT                                       
133000                                                                          
133010     IF RADER-SAKNAS                                                      
133020       MOVE '027' TO RESP-IDMSG-ERROR                                     
133030     END-IF                                                               
133040                                                                          
133100     PERFORM UNTIL INDX >= MAX-INDX OR RADER-SAKNAS or                    
133110                   INDX >= WS-KVRADER-PACK                                
133200       MOVE TP6PART-IDARTNR    TO W-IDARTNR                               
133300                                                                          
133400       PERFORM IMS-GHU-WDK711                                             
133500       IF SEGMENT-FINNS                                                   
133510         IF WS-ADLAGOMR = ZERO                                            
133520            PERFORM GBB-MOVE-DATA                                         
133530         ELSE                                                             
133540           IF  SLAG-ADLAGOMR  = WS-ADLAGOMR                               
133550             IF AISLE-NOO                                                 
133560               PERFORM GBB-MOVE-DATA                                      
133570             ELSE                                                         
133580               IF SLAG-ADGANG > WS-ADGANG-FROM                            
133590               AND SLAG-ADGANG < WS-ADGANG-TOM                            
133591                 PERFORM GBB-MOVE-DATA                                    
133592               END-IF                                                     
133593             END-IF                                                       
135601           END-IF                                                         
135602         END-IF                                                           
135603       END-IF                                                             
136000       PERFORM DB2-FETCH-TP6PART-SORT                                     
136100     END-PERFORM                                                          
136110                                                                          
136200     PERFORM DB2-CLOSE-TP6PART-SORT                                       
136210                                                                          
136300     IF INDX = 501                                                        
136400       MOVE 500 TO INDX                                                   
136500     END-IF                                                               
136510                                                                          
136600     MOVE INDX TO RESP-KVRADER                                            
136700                  ANTAL                                                   
136800                  WS-KVRADER                                              
136810                                                                          
136900     IF WS-KVRADER = +0                                                   
137000       MOVE '027' TO RESP-IDMSG-ERROR                                     
137100     END-IF                                                               
137200                                                                          
137300**** SORTERA TABELLEN  ****                                               
137310     IF ANTAL >= 1                                                        
137400       MOVE +55 TO STEGLAANGD                                             
137500       IF REQU-KDSORT1-KEY = 'P'                                          
137600         MOVE +5  TO NYCKELLAANGD                                         
138201                                                                          
138202         CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                  
138203         TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                
138204       ELSE                                                               
138205         IF REQU-KDSORT1-KEY = 'L'                                        
138206           MOVE +7  TO NYCKELLAANGD                                       
138300                                                                          
138400           CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                
138500           TAB-SORT-BEGREPP2(1) NYCKELLAANGD                              
138600         END-IF                                                           
138700       END-IF                                                             
138800                                                                          
138900***    FLYTTA TABELL TILL RESP-AREA                                       
139000                                                                          
139100       MOVE +1 TO INDX                                                    
139200       MOVE JA TO WS-FORSTA-POST                                          
139300       MOVE NEJ TO WS-DAP-OPEN                                            
139400       IF WS-KVRADER > 0                                                  
139500         IF WS-FORSTA-POST = 'J'                                          
139600            MOVE NEJ TO WS-FORSTA-POST                                    
139700            MOVE JA TO WS-DAP-OPEN                                        
139800            PERFORM S04-SKICKA-OPEN                                       
139900            PERFORM S04-SKAPA-HEADER-UNIT                                 
140000            PERFORM S04-PUT-DAP-HEADER                                    
140100         END-IF                                                           
140200       END-IF                                                             
140300       PERFORM UNTIL INDX > WS-KVRADER                                    
140400         MOVE 'LINE'               TO DOC-IDAFPRCD                        
140500         MOVE REQU-IDDC-KEY        TO DOC-IDDC                            
140600         MOVE TAB-IDARTNR   (INDX) TO RESP-IDARTNR-UT   (INDX)            
140700                                        DOC-IDARTNR                       
140800         MOVE TAB-BEART     (INDX) TO RESP-BEART-UT     (INDX)            
140900                                        DOC-BEART                         
141000         MOVE TAB-KDSORT    (INDX) TO RESP-KDSORT-UT    (INDX)            
141100                                        DOC-KDSORT                        
141200         MOVE TAB-KVLS      (INDX) TO RESP-KVLS-UT      (INDX)            
141300                                        DOC-KVLS                          
141400         MOVE TAB-KVPB-REF  (INDX) TO RESP-KVPB-REF-UT  (INDX)            
141500                                        DOC-KVPB-REF                      
141600         MOVE TAB-ADLAGOMR  (INDX) TO RESP-ADLAGOMR-UT  (INDX)            
141700                                        DOC-ADLAGOMR                      
141800         MOVE TAB-ADGANG    (INDX) TO RESP-ADGANG-UT    (INDX)            
141900                                        DOC-ADGANG                        
142000         MOVE TAB-ADPLATS   (INDX) TO RESP-ADPLATS-UT   (INDX)            
142100                                        DOC-ADPLATS                       
142200         IF TAB-ADBUFFOMR(INDX) > 0                                       
142300          MOVE TAB-ADBUFFOMR (INDX) TO RESP-ADBUFFOMR-UT (INDX)           
142400                                        DOC-ADBUFFOMR                     
142500          MOVE TAB-ADBUFFGANG (INDX) TO RESP-ADBUFFGANG-UT(INDX)          
142600                                        DOC-ADBUFFGANG                    
142700          MOVE TAB-ADBUFFPL  (INDX) TO RESP-ADBUFFPL-UT  (INDX)           
142800                                        DOC-ADBUFFPL                      
142900         END-IF                                                           
143000                                                                          
143100         PERFORM S04-PUT-DOC                                              
143200         MOVE ALL '+' TO DOC-AREA                                         
143300         ADD +1 TO INDX                                                   
143400       END-PERFORM                                                        
143410     END-IF                                                               
143438                                                                          
143500     IF WS-DAP-OPEN = 'J'                                                 
143600       PERFORM S04-SKICKA-CLOSE                                           
143700       MOVE '015'   TO RESP-IDMSG-INFO                                    
143800     END-IF                                                               
143900     .                                                                    
144000     EJECT                                                                
144100*                                                                         
144110 GBB-MOVE-DATA SECTION.                                                   
144111                                                                          
144120     ADD +1 TO INDX                                                       
144130     MOVE TP6PART-KDSORT          TO TAB-KDSORT      (INDX)               
144140     MOVE W-IDARTNR               TO TAB-IDARTNR     (INDX)               
144150                                                                          
144160     MOVE SLAG-KVLS               TO TAB-KVLS        (INDX)               
144170     MOVE SLAG-KVPB-REF           TO TAB-KVPB-REF    (INDX)               
144180     MOVE SLAG-ADLAGOMR           TO TAB-ADLAGOMR    (INDX)               
144190     MOVE SLAG-ADGANG             TO TAB-ADGANG      (INDX)               
144191     MOVE SLAG-ADPLATS            TO TAB-ADPLATS     (INDX)               
144192                                                                          
144193     MOVE TP6PART-BEART           TO TRAUTF8-TECONV-FROM                  
144194     PERFORM S06-CALL-WTRAUTF8                                            
144195     MOVE TRAUTF8-TECONV-TO       TO TAB-BEART(INDX)                      
144196                                                                          
144197     PERFORM IMS-GET-WDD801                                               
144198     IF SEGMENT-FINNS                                                     
144199       PERFORM IMS-GNP-WDD811                                             
144200       IF SEGMENT-FINNS                                                   
144201         MOVE ARTD-SALDO-ADBUFFOMR  TO TAB-ADBUFFOMR  (INDX)              
144202         MOVE ARTD-SALDO-ADBUFFGANG TO TAB-ADBUFFGANG (INDX)              
144203         MOVE ARTD-SALDO-ADBUFFPL   TO TAB-ADBUFFPL   (INDX)              
144204       END-IF                                                             
144205     END-IF                                                               
144207     .                                                                    
144208     EJECT                                                                
144210 GC-PRINT-BEART  SECTION.                                                 
144300***  SÖKNING MED BENÄMING *****                                           
144310     MOVE +0  TO INDX                                                     
144400     MOVE JA  TO WS-FORSTA-POST                                           
144500     MOVE NEJ TO WS-DAP-OPEN                                              
152100                                                                          
152200     PERFORM DB2-DCL-OPN-TP6PART-LANG                                     
152300     PERFORM DB2-FETCH-TP6PART-LANG                                       
153100                                                                          
153300     PERFORM UNTIL INDX >= MAX-INDX  OR RADER-SAKNAS OR                   
153310                   INDX >= WS-KVRADER-PACK                                
153320                                                                          
153330         IF WS-ADLAGOMR = ZERO                                            
153340            PERFORM GCC-MOVE-DATA                                         
153350         ELSE                                                             
153360           IF  TP6PART-ADLAGOMR  = WS-ADLAGOMR                            
153370             IF AISLE-NOO                                                 
153380               PERFORM GCC-MOVE-DATA                                      
153390             ELSE                                                         
153391               IF TP6PART-ADGANG  > WS-ADGANG-FROM                        
153392               AND TP6PART-ADGANG < WS-ADGANG-TOM                         
153393                 PERFORM GCC-MOVE-DATA                                    
153394               END-IF                                                     
153395             END-IF                                                       
153396           END-IF                                                         
153397         END-IF                                                           
159100                                                                          
159200       PERFORM DB2-FETCH-TP6PART-LANG                                     
159800     END-PERFORM                                                          
159810     PERFORM DB2-CLOSE-TP6PART-LANG                                       
159900     IF WS-DAP-OPEN = 'J'                                                 
160000       PERFORM S04-SKICKA-CLOSE                                           
160100       IF INDX >= 1                                                       
160200         IF INDX = 501                                                    
160300           MOVE 500 TO INDX                                               
160400         END-IF                                                           
160500         MOVE INDX TO RESP-KVRADER                                        
160600         MOVE '015'   TO RESP-IDMSG-INFO                                  
160700       END-IF                                                             
160800     END-IF                                                               
160900     .                                                                    
161000     EJECT                                                                
161100 GCC-MOVE-DATA SECTION.                                                   
161101                                                                          
161110     ADD +1 TO INDX                                                       
161120     MOVE TP6PART-ADLAGOMR         TO RESP-ADLAGOMR-UT  (INDX)            
161130                                      DOC-ADLAGOMR                        
161140     MOVE TP6PART-ADGANG           TO RESP-ADGANG-UT    (INDX)            
161150                                      DOC-ADGANG                          
161160     MOVE TP6PART-ADPLATS          TO RESP-ADPLATS-UT   (INDX)            
161170                                      DOC-ADPLATS                         
161180     MOVE TP6PART-KVLS             TO RESP-KVLS-UT      (INDX)            
161190                                      DOC-KVLS                            
161191     MOVE TP6PART-BEART            TO TRAUTF8-TECONV-FROM                 
161192     PERFORM S06-CALL-WTRAUTF8                                            
161193     MOVE TRAUTF8-TECONV-TO        TO RESP-BEART-UT(INDX)                 
161194                                      DOC-BEART                           
161195                                                                          
161196     MOVE TP6PART-IDARTNR         TO W-IDARTNR                            
161197     MOVE W-IDARTNR               TO RESP-IDARTNR-UT   (INDX)             
161198                                     DOC-IDARTNR                          
161199                                                                          
161200     PERFORM IMS-GHU-WDK711                                               
161201     IF SEGMENT-FINNS                                                     
161202       IF WS-FORSTA-POST = 'J'                                            
161203         MOVE NEJ TO WS-FORSTA-POST                                       
161204         MOVE JA  TO WS-DAP-OPEN                                          
161205         PERFORM S04-SKICKA-OPEN                                          
161206         PERFORM S04-SKAPA-HEADER                                         
161207         PERFORM S04-PUT-DAP-HEADER                                       
161208       END-IF                                                             
161209       MOVE 'LINE'                 TO DOC-IDAFPRCD                        
161210       MOVE REQU-IDDC-KEY          TO DOC-IDDC                            
161211       MOVE W-IDARTNR              TO DOC-IDARTNR                         
161212                                      RESP-IDARTNR-UT  (INDX)             
161213       MOVE SLAG-ADLAGOMR          TO DOC-ADLAGOMR                        
161214                                      RESP-ADLAGOMR-UT (INDX)             
161215       MOVE SLAG-ADGANG            TO DOC-ADGANG                          
161216                                      RESP-ADGANG-UT   (INDX)             
161217       MOVE SLAG-ADPLATS           TO DOC-ADPLATS                         
161218                                      RESP-ADPLATS-UT  (INDX)             
161219       MOVE SLAG-KVLS              TO DOC-KVLS                            
161220                                      RESP-KVLS-UT  (INDX)                
161221     END-IF                                                               
161222                                                                          
161223     PERFORM IMS-GET-WDD801                                               
161224     IF SEGMENT-FINNS                                                     
161225       PERFORM IMS-GNP-WDD811                                             
161226       IF SEGMENT-FINNS                                                   
161227         MOVE ARTD-SALDO-ADBUFFOMR TO DOC-ADBUFFOMR                       
161228                                      RESP-ADBUFFOMR-UT (INDX)            
161229         MOVE ARTD-SALDO-ADBUFFGANG TO                                    
161230                                      DOC-ADBUFFGANG                      
161231                                      RESP-ADBUFFGANG-UT(INDX)            
161232         MOVE ARTD-SALDO-ADBUFFPL  TO DOC-ADBUFFPL                        
161233                                      RESP-ADBUFFPL-UT   (INDX)           
161234       END-IF                                                             
161235     END-IF                                                               
161236                                                                          
161237     PERFORM S04-PUT-DOC                                                  
161238     MOVE ALL '+'                 TO DOC-AREA                             
161239     .                                                                    
161240     EJECT                                                                
161250 GD-PRINT-NIL  SECTION.                                                   
161300     MOVE +0 TO INDX                                                      
161400     MOVE +0 TO WS-KVRADER                                                
161500     MOVE JA  TO WS-FORSTA-POST                                           
161600     MOVE NEJ TO WS-DAP-OPEN                                              
161700                                                                          
161800     IF AISLE-NOO                                                         
161900       PERFORM DB2-DCL-OPN-TP6PART                                        
162000       PERFORM DB2-FETCH-TP6PART                                          
162100     ELSE                                                                 
162200       PERFORM DB2-DCL-OPN-TP6PART-AISLE                                  
162300       PERFORM DB2-FETCH-TP6PART-AISLE                                    
162400     END-IF                                                               
162500     IF RADER-FINNS                                                       
162600       IF WS-FORSTA-POST = 'J'                                            
162700          MOVE NEJ TO WS-FORSTA-POST                                      
162800          MOVE JA  TO WS-DAP-OPEN                                         
162900          PERFORM S04-SKICKA-OPEN                                         
163000          PERFORM S04-SKAPA-HEADER-NIL                                    
163100          PERFORM S04-PUT-DAP-HEADER                                      
163200       END-IF                                                             
163300     END-IF                                                               
163400     PERFORM UNTIL RADER-SAKNAS OR INDX >= MAX-INDX OR                    
163500                   INDX >= WS-KVRADER-PACK                                
163510       IF WS-ADLAGOMR = ZERO                                              
163520          PERFORM GDD-MOVE-DATA                                           
163530       ELSE                                                               
163540         IF  TP6PART-ADLAGOMR  = WS-ADLAGOMR                              
163550           PERFORM GDD-MOVE-DATA                                          
163560         END-IF                                                           
163570       END-IF                                                             
166900       IF AISLE-NOO                                                       
167000         PERFORM DB2-FETCH-TP6PART                                        
167100       ELSE                                                               
167200         PERFORM DB2-FETCH-TP6PART-AISLE                                  
167300       END-IF                                                             
167400     END-PERFORM                                                          
167410                                                                          
167500     MOVE WS-KVRADER TO RESP-KVRADER                                      
167600     IF AISLE-NOO                                                         
167700       PERFORM DB2-CLOSE-TP6PART                                          
167800     ELSE                                                                 
167900       PERFORM DB2-CLOSE-TP6PART-AISLE                                    
168000     END-IF                                                               
168100     IF WS-DAP-OPEN = 'J'                                                 
168200       PERFORM S04-SKICKA-CLOSE                                           
168300       MOVE '015'   TO RESP-IDMSG-INFO                                    
168400     END-IF                                                               
168500     .                                                                    
168600     EJECT                                                                
168610 GDD-MOVE-DATA SECTION.                                                   
168611                                                                          
168620     MOVE TP6PART-KDPRODSL        TO TEST-KDPRODSL                        
168630     IF KDPRODSL-EMB                                                      
168640       CONTINUE                                                           
168650     ELSE                                                                 
168660       ADD +1 TO WS-KVRADER                                               
168670       ADD +1 TO INDX                                                     
168680       MOVE 'LINE'                TO DOC-IDAFPRCD                         
168690       MOVE REQU-IDDC-KEY         TO DOC-IDDC                             
168691       MOVE TP6PART-IDARTNR       TO RESP-IDARTNR-UT (INDX)               
168692                                     DOC-IDARTNR                          
168693                                     W-IDARTNR                            
168694       MOVE TP6PART-ADLAGOMR      TO RESP-ADLAGOMR-UT(INDX)               
168695                                     DOC-ADLAGOMR                         
168696       MOVE TP6PART-ADGANG        TO RESP-ADGANG-UT  (INDX)               
168697                                     DOC-ADGANG                           
168698       MOVE TP6PART-ADPLATS       TO RESP-ADPLATS-UT (INDX)               
168699                                     DOC-ADPLATS                          
168700       MOVE TP6PART-KVLS          TO RESP-KVLS-UT    (INDX)               
168701                                     DOC-KVLS                             
168702       MOVE TP6PART-KVPB-REF      TO RESP-KVPB-REF-UT(INDX)               
168703                                     DOC-KVPB-REF                         
168704       MOVE TP6PART-TIREFEFT      TO RESP-TIREFEFT-UT(INDX)               
168705                                     DOC-TIREFEFT                         
168706                                                                          
168707       MOVE TP6PART-BEART         TO TRAUTF8-TECONV-FROM                  
168708       PERFORM S06-CALL-WTRAUTF8                                          
168709       MOVE TRAUTF8-TECONV-TO     TO RESP-BEART-UT(INDX)                  
168710                                     DOC-BEART                            
168711     END-IF                                                               
168712     MOVE 'NIL'                TO DOC-LIST-TYP                            
168713                                                                          
168714     PERFORM S04-PUT-DOC                                                  
168715     MOVE ALL '+' TO DOC-AREA                                             
168716     .                                                                    
168717     EJECT                                                                
168720*                                                                         
168800*    --- DISPATCHER-SEKTIONER                                             
168900 S01-HAEMTA-ANROPSDATA SECTION.                                           
169000                                                                          
169100     MOVE 'GETARG'               TO SUB-KDFUNC                            
169200     MOVE 'CARPARTS.LDC.REQUESTEDPARTINFO'    TO SUB-ADDISPABS            
169300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
169400*    MOVE LENGTH OF REQU-AREA-UTF   TO SUB-KVDLEN                         
169500                                                                          
169600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
169700                                                                          
169800     IF SUB-KDRC > 0                                                      
169900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
170000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
170100       DELIMITED BY SIZE INTO FELTEXT                                     
170200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
170300     END-IF                                                               
170400                                                                          
170500     .                                                                    
170600     SKIP3                                                                
170700 S02-RETURNERA-SVAR SECTION.                                              
170800                                                                          
170900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
171000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
171100                                                                          
171200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
171300                                                                          
171400     IF SUB-KDRC > 0                                                      
171500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
171600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
171700       DELIMITED BY SIZE INTO FELTEXT                                     
171800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
171900     END-IF                                                               
172000     .                                                                    
172100     EJECT                                                                
172200*                                                                         
172300 S04-SKICKA-OPEN SECTION.                                                 
172400                                                                          
172500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
172600     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
172700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
172800                                                                          
172900     IF SEND-KDRC > 0                                                     
173000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
173100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
173200       DELIMITED BY SIZE INTO FELTEXT                                     
173300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
173400     END-IF                                                               
173500     .                                                                    
173600     SKIP3                                                                
173700                                                                          
173800 S04-SKICKA-CLOSE SECTION.                                                
173900                                                                          
174000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
174100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
174200                                                                          
174300     IF SEND-KDRC > 0                                                     
174400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
174500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
174600       DELIMITED BY SIZE INTO FELTEXT                                     
174700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
174800     END-IF                                                               
174900     .                                                                    
175000     EJECT                                                                
175100 S04-SKAPA-HEADER   SECTION.                                              
175200     MOVE 001             TO HDR-REQU-IDMSGVER                            
175300     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
175400     MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                              
175500                                                                          
175600     MOVE 'PART-DESC'     TO HDR-IDOUTTYPE                                
175700     MOVE SPACE               TO HDR-IDOUTREC                             
175800     MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                        
175900     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
176000     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
176100     .                                                                    
176200 S04-SKAPA-HEADER-UNIT   SECTION.                                         
176300     MOVE 001             TO HDR-REQU-IDMSGVER                            
176400     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
176500     MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                              
176600                                                                          
176700     MOVE 'PART-UNIT'      TO HDR-IDOUTTYPE                               
176800     MOVE SPACE               TO HDR-IDOUTREC                             
176900     MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                        
177000     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
177100     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
177200     .                                                                    
177300 S04-SKAPA-HEADER-FREQ   SECTION.                                         
177400     MOVE 001             TO HDR-REQU-IDMSGVER                            
177500     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
177600     MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                              
177700     MOVE SPACE               TO HDR-IDOUTREC                             
177800     MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                        
177900     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
178000     IF REQU-FREQPACK-KEY = 'M'                                           
178100       MOVE 'PART-FREQUENT-M' TO HDR-IDOUTTYPE                            
178200     ELSE                                                                 
178300       MOVE 'PART-FREQUENT-L' TO HDR-IDOUTTYPE                            
178400     END-IF                                                               
178500     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
178600     .                                                                    
178700 S04-SKAPA-HEADER-NIL    SECTION.                                         
178800     MOVE 001                 TO HDR-REQU-IDMSGVER                        
178900     MOVE SPACE               TO HDR-REQU-KDPGMACT                        
179000     MOVE REQU-IDUSER         TO HDR-REQU-IDUSER                          
179100     MOVE SPACE               TO HDR-IDOUTREC                             
179200     MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                        
179300     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
179400     MOVE 'PART-FREQUENT-N'   TO HDR-IDOUTTYPE                            
179500     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
179600     .                                                                    
179700 S04-PUT-DAP-HEADER SECTION.                                              
179800     MOVE 'PUT'                           TO SEND-KDFUNC                  
179900     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
180000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
180100                         SEND-KVDLEN                                      
180200                         HDR-AREA                                         
180300     IF SEND-KDRC > ZERO                                                  
180400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
180500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
180600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
180700       CALL ABEND USING  RKOD-ABEND-MED-DUMP                              
180800     END-IF                                                               
180900     .                                                                    
181000 S04-PUT-DOC      SECTION.                                                
181010                                                                          
181100     MOVE 'PUT'                           TO SEND-KDFUNC                  
181200     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
181300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
181400                         SEND-KVDLEN                                      
181500                         DOC-AREA                                         
181600     IF SEND-KDRC > ZERO                                                  
181700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
181800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
181900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
182000       CALL ABEND USING  RKOD-ABEND-MED-DUMP                              
182100     END-IF                                                               
182200     .                                                                    
182300 S05-CALL-WTRAEBCD SECTION.                                               
182400                                                                          
182500     MOVE W-IDSKYLT             TO IDSKYLT-DB                             
182600     IF UNICODE-IDSKYLT                                                   
182700       MOVE 'UTF8'              TO TRAEBCD-KDCP                           
182800     ELSE                                                                 
182900       MOVE '278'               TO TRAEBCD-KDCP                           
183000     END-IF                                                               
183100     CALL WTRAEBCD USING TRAEBCD-AREA                                     
183200     IF EBCDIC-IDSKYLT                                                    
183300       MOVE FUNCTION UPPER-CASE(TRAEBCD-TECONV-TO)                        
183400                             TO TRAEBCD-TECONV-TO                         
183500       INSPECT TRAEBCD-TECONV-TO                                          
183600         CONVERTING LATIN1-LOWERCASE TO LATIN1-UPPERCASE                  
183700     END-IF                                                               
183800     .                                                                    
183900     EJECT                                                                
184000 S06-CALL-WTRAUTF8 SECTION.                                               
184100                                                                          
184200     MOVE 25 TO TRAUTF8-KVMAXTL                                           
184300     MOVE REQU-IDSPRAK-KEY      TO IDSKYLT-DB                             
184400     IF UNICODE-IDSKYLT                                                   
184500       MOVE 'UTF8'              TO TRAUTF8-KDCP                           
184600     ELSE                                                                 
184700       MOVE '278'               TO TRAUTF8-KDCP                           
184800     END-IF                                                               
184900     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
185000     .                                                                    
185100     EJECT                                                                
185200 IMS-GET-WDK701 SECTION.                                                  
185300                                                                          
185400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
185500          DELIMITED BY SIZE INTO SSA1                                     
185600     MOVE '  GE' TO GODK-STATUSKODER                                      
185700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
185800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
185900     PERFORM IMS-STATUSKONTROLL                                           
186000     .                                                                    
186100     EJECT                                                                
186200 IMS-GNP-WDK711 SECTION.                                                  
186300                                                                          
186400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
186500          DELIMITED BY SIZE INTO SSA1                                     
186600     MOVE '  GEGP' TO GODK-STATUSKODER                                    
186700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
186800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
186900     PERFORM IMS-STATUSKONTROLL                                           
187000     .                                                                    
187100     EJECT                                                                
187200 IMS-GHU-WDK711 SECTION.                                                  
187300                                                                          
187400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
187500          DELIMITED BY SIZE INTO SSA1                                     
187600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
187700          DELIMITED BY SIZE INTO SSA2                                     
187800     MOVE '  GE' TO GODK-STATUSKODER                                      
187900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
188000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
188100     PERFORM IMS-STATUSKONTROLL                                           
188200     .                                                                    
188300     EJECT                                                                
188400 IMS-GN-WDK7A1 SECTION.                                                   
188500     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
188600                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
188700          DELIMITED BY SIZE INTO SSA1                                     
188800     MOVE '  GE' TO GODK-STATUSKODER                                      
188900     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
189000     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
189100     PERFORM IMS-STATUSKONTROLL                                           
189200     .                                                                    
189300     SKIP3                                                                
189400 IMS-GET-WDD801 SECTION.                                                  
189500                                                                          
189600     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
189700          DELIMITED BY SIZE INTO SSA1                                     
189800     MOVE '  GE' TO GODK-STATUSKODER                                      
189900     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
190000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUSKONTROLL                                           
190200     .                                                                    
190300     EJECT                                                                
190400 IMS-GNP-WDD811     SECTION.                                              
190500     STRING 'WDD811  (WDD811KY=>' W-WDD811KY-MIN-X                        
190600                    '&WDD811KY=<' W-WDD811KY-MAX-X ')'                    
190700          DELIMITED BY SIZE INTO SSA1                                     
190800     MOVE '  GE' TO GODK-STATUSKODER                                      
190900     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
191000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300     SKIP3                                                                
191400 IMS-GET-WDD301 SECTION.                                                  
191500     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
191600          DELIMITED BY SIZE INTO SSA1                                     
191700     MOVE '  GE' TO GODK-STATUSKODER                                      
191800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
191900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUSKONTROLL                                           
192100     .                                                                    
192200     EJECT                                                                
192300 IMS-GNP-WDD311 SECTION.                                                  
192400     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
192500          DELIMITED BY SIZE INTO SSA1                                     
192600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
192700          DELIMITED BY SIZE INTO SSA2                                     
192800     MOVE '  GE' TO GODK-STATUSKODER                                      
192900     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
193000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
193100     PERFORM IMS-STATUSKONTROLL                                           
193200     .                                                                    
193300     EJECT                                                                
193400 IMS-GNP-WDD312 SECTION.                                                  
193500     MOVE 'WDD312   ' TO SSA1                                             
193600     MOVE '  GE' TO GODK-STATUSKODER                                      
193700     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD312 SSA1                   
193800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
193900     PERFORM IMS-STATUSKONTROLL                                           
194000     .                                                                    
194100     EJECT                                                                
194200 IMS-GET-WDD3B1 SECTION.                                                  
194300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
194400          DELIMITED BY SIZE INTO SSA1                                     
194500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
194600          DELIMITED BY SIZE INTO SSA2                                     
194700     MOVE '  GE' TO GODK-STATUSKODER                                      
194800     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311   SSA1 SSA2            
194900     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
195000     PERFORM IMS-STATUSKONTROLL                                           
195100     .                                                                    
195200     EJECT                                                                
195300 IMS-GET-WDD3A1      SECTION.                                             
195400     STRING 'WDD301  (WDD3ASEQ=>' W-IDSKYLT                               
195500                      W-BEART-MIN-X                                       
195600                     '&WDD3ASEQ <' W-IDSKYLT                              
195700                      W-BEART-MAX-X  ')'                                  
195800            DELIMITED BY SIZE INTO SSA1                                   
195900     MOVE '  GE' TO GODK-STATUSKODER                                      
196000     CALL CBLTDLI USING GN WDD3A-PCB DLI-IO-WDD301 SSA1                   
196100     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     SKIP3                                                                
196500 IMS-GET-WDK601 SECTION.                                                  
196600                                                                          
196700     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
196800          DELIMITED BY SIZE INTO SSA1                                     
196900     MOVE SPACE  TO GODK-STATUSKODER                                      
197000     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK601   SSA1                 
197100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
197200     PERFORM IMS-STATUSKONTROLL                                           
197300     .                                                                    
197400     EJECT                                                                
197500 IMS-STATUSKONTROLL SECTION.                                              
197600                                                                          
197700     SET STATUS-IX TO 1                                                   
197800     SEARCH GODK-STATUS                                                   
197900       AT END                                                             
198000       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS '. ' LAST-IMS-CALL          
198100         DELIMITED BY SIZE INTO ERROR-TEXT                                
198200         CALL FELLOG                                                      
198300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
198400     END-SEARCH                                                           
198500     .                                                                    
198600 DB2-DCL-OPN-TP6ARTP-CRS-L SECTION.                                       
198700                                                                          
198800     MOVE 000100  TO GODK-SQLCODEKODER                                    
198900                                                                          
199000     EXEC SQL                                                             
199100         DECLARE TP6ARTPLESS-CRS CURSOR FOR                               
199200                                                                          
199210           SELECT DISTINCT(A.IDARTNR) AS WS-IDARTNR                       
199220                  ,COUNT(A.IDARTNR)   AS WS-NUMBER                        
199230           FROM  TP6ARTP A                                                
199240               , TP6PART B                                                
199250           WHERE  A.IDDC     = :REQU-IDDC-KEY                             
199260           AND    A.DAREGDAT > :WS-DATUM-NYTT                             
199270           AND    A.ADLAGOMR > :WS-ADLAGOMR-ZERO                          
199280           AND    B.IDDC     = A.IDDC                                     
199290           AND    B.IDARTNR  = A.IDARTNR                                  
199291           AND    B.BEART    LIKE :WS-BEART                               
199292           AND    B.KDSORT   LIKE :WS-KDSORT                              
199293           AND    B.IDSKYLT  = :W-IDSKYLT                                 
199294           GROUP BY A.IDARTNR                                             
199295           ORDER BY WS-NUMBER                                             
200200     END-EXEC                                                             
200300                                                                          
200400     MOVE 000100  TO GODK-SQLCODEKODER                                    
200500     EXEC SQL OPEN TP6ARTPLESS-CRS    END-EXEC                            
200600                                                                          
200700     .                                                                    
200800     SKIP3                                                                
200900 DB2-FETCH-TP6ARTP-CRS-L SECTION.                                         
201000     SKIP2                                                                
201100     MOVE 000100  TO GODK-SQLCODEKODER                                    
201200     EXEC SQL                                                             
201300         FETCH TP6ARTPLESS-CRS                                            
201400         INTO:WS-IDARTNR                                                  
201500           , :WS-NUMBER                                                   
201600                                                                          
201700     END-EXEC                                                             
201800                                                                          
201900     MOVE SQLCODE TO SQLCODE-WS                                           
202000     PERFORM DB2-STATUS-KONTROLL                                          
202100     .                                                                    
202200     SKIP3                                                                
202300 DB2-CLOSE-TP6ARTP-CRS-L SECTION.                                         
202400                                                                          
202500     EXEC SQL CLOSE TP6ARTPLESS-CRS    END-EXEC                           
202600     .                                                                    
202610 DB2-DCL-OPN-TP6ARTP-CRS-M SECTION.                                       
202620                                                                          
202630     MOVE 000100  TO GODK-SQLCODEKODER                                    
202640                                                                          
202650     EXEC SQL                                                             
203200         DECLARE TP6ARTPMOST-CRS CURSOR FOR                               
203300                                                                          
204210           SELECT DISTINCT(A.IDARTNR) AS WS-IDARTNR                       
204220                  ,COUNT(A.IDARTNR)   AS WS-NUMBER                        
204230           FROM  TP6ARTP A                                                
204240               , TP6PART B                                                
204250           WHERE  A.IDDC     = :REQU-IDDC-KEY                             
204260           AND    A.DAREGDAT > :WS-DATUM-NYTT                             
204261           AND    A.ADLAGOMR > :WS-ADLAGOMR-ZERO                          
204270           AND    B.IDDC     = A.IDDC                                     
204280           AND    B.IDARTNR  = A.IDARTNR                                  
204292           AND    B.BEART    LIKE :WS-BEART                               
204293           AND    B.KDSORT   LIKE :WS-KDSORT                              
204294           AND    B.IDSKYLT  = :W-IDSKYLT                                 
204295           GROUP BY A.IDARTNR                                             
204296           ORDER BY WS-NUMBER DESC                                        
204300     END-EXEC                                                             
204400                                                                          
204500     MOVE 000100  TO GODK-SQLCODEKODER                                    
204600     EXEC SQL OPEN TP6ARTPMOST-CRS    END-EXEC                            
204700                                                                          
204800     .                                                                    
204900     SKIP3                                                                
205000 DB2-FETCH-TP6ARTP-CRS-M SECTION.                                         
205100     SKIP2                                                                
205200     MOVE 000100  TO GODK-SQLCODEKODER                                    
205300     EXEC SQL                                                             
205400         FETCH TP6ARTPMOST-CRS                                            
205500         INTO:WS-IDARTNR                                                  
205600         , :WS-NUMBER                                                     
205700     END-EXEC                                                             
205800                                                                          
205900     MOVE SQLCODE TO SQLCODE-WS                                           
206000     PERFORM DB2-STATUS-KONTROLL                                          
206100     .                                                                    
206200     SKIP3                                                                
206300 DB2-CLOSE-TP6ARTP-CRS-M SECTION.                                         
206400                                                                          
206500     EXEC SQL CLOSE TP6ARTPMOST-CRS    END-EXEC                           
206600     .                                                                    
206700 DB2-DCL-OPN-TP6PART SECTION.                                             
206800                                                                          
206900     MOVE 000100  TO GODK-SQLCODEKODER                                    
207000                                                                          
207100     EXEC SQL                                                             
207200         DECLARE TP6PART-CRS CURSOR FOR                                   
207300                                                                          
207400           SELECT   IDARTNR                                               
207500                   ,IDDC                                                  
207600                   ,ADLAGOMR                                              
207700                   ,ADGANG                                                
207800                   ,ADPLATS                                               
207900                   ,KVLS                                                  
208000                   ,KVPB_REF                                              
208100                   ,TIREFEFT                                              
208200                   ,KDPRODSL                                              
208300                   ,IDSKYLT                                               
208400                   ,BEART                                                 
208500                                                                          
208600           FROM    TP6PART                                                
208700           WHERE   IDDC     = :REQU-IDDC-KEY                              
208900           AND     ADLAGOMR > :WS-ADLAGOMR-ZERO                           
209000           AND     TIREFEFT < :WS-DATUM-6                                 
209010           AND     BEART    LIKE :WS-BEART                                
209020           AND     KDSORT   LIKE :WS-KDSORT                               
209100           AND     IDSKYLT  = :W-IDSKYLT                                  
209200           ORDER BY TIREFEFT                                              
209300     END-EXEC                                                             
209400                                                                          
209500     MOVE 000100  TO GODK-SQLCODEKODER                                    
209600     EXEC SQL OPEN TP6PART-CRS    END-EXEC                                
209700     .                                                                    
209800     SKIP3                                                                
209900                                                                          
210000 DB2-FETCH-TP6PART SECTION.                                               
210100     SKIP2                                                                
210200     MOVE 000100  TO GODK-SQLCODEKODER                                    
210300     EXEC SQL                                                             
210400         FETCH TP6PART-CRS                                                
210500         INTO :TP6PART-IDARTNR                                            
210600             ,:TP6PART-IDDC                                               
210700             ,:TP6PART-ADLAGOMR                                           
210800             ,:TP6PART-ADGANG                                             
210900             ,:TP6PART-ADPLATS                                            
211000             ,:TP6PART-KVLS                                               
211100             ,:TP6PART-KVPB-REF                                           
211200             ,:TP6PART-TIREFEFT                                           
211300             ,:TP6PART-KDPRODSL                                           
211400             ,:TP6PART-IDSKYLT                                            
211500             ,:TP6PART-BEART                                              
211600     END-EXEC                                                             
211700                                                                          
211800     MOVE SQLCODE TO SQLCODE-WS                                           
211900     PERFORM DB2-STATUS-KONTROLL                                          
212000     .                                                                    
212100     SKIP3                                                                
212200                                                                          
212300 DB2-CLOSE-TP6PART SECTION.                                               
212400                                                                          
212500     EXEC SQL CLOSE TP6PART-CRS    END-EXEC                               
212600     .                                                                    
212700                                                                          
212800 DB2-DCL-OPN-TP6PART-AISLE SECTION.                                       
212900                                                                          
213000     MOVE 000100  TO GODK-SQLCODEKODER                                    
213100                                                                          
213200     EXEC SQL                                                             
213300         DECLARE TP6PART2-CRS CURSOR FOR                                  
213400                                                                          
213500           SELECT   IDARTNR                                               
213600                   ,IDDC                                                  
213700                   ,ADLAGOMR                                              
213800                   ,ADGANG                                                
213900                   ,ADPLATS                                               
214000                   ,KVLS                                                  
214100                   ,KVPB_REF                                              
214200                   ,TIREFEFT                                              
214300                   ,KDPRODSL                                              
214400                   ,IDSKYLT                                               
214500                   ,BEART                                                 
214600                                                                          
214700           FROM    TP6PART                                                
214800           WHERE   IDDC     = :REQU-IDDC-KEY                              
214900           AND     ADLAGOMR = :WS-ADLAGOMR                                
215000           AND     ADLAGOMR > :WS-ADLAGOMR-ZERO                           
215100           AND     ADGANG   > :WS-ADGANG-FROM                             
215200           AND     ADGANG   < :WS-ADGANG-TOM                              
215300           AND     TIREFEFT < :WS-DATUM-6                                 
215310           AND     BEART    LIKE :WS-BEART                                
215320           AND     KDSORT   LIKE :WS-KDSORT                               
215400           AND     IDSKYLT  = :W-IDSKYLT                                  
215500           ORDER BY TIREFEFT                                              
215600     END-EXEC                                                             
215700                                                                          
215800     MOVE 000100  TO GODK-SQLCODEKODER                                    
215900     EXEC SQL OPEN TP6PART2-CRS    END-EXEC                               
216000     .                                                                    
216100     SKIP3                                                                
216200                                                                          
216300 DB2-FETCH-TP6PART-AISLE SECTION.                                         
216400     SKIP2                                                                
216500     MOVE 000100  TO GODK-SQLCODEKODER                                    
216600     EXEC SQL                                                             
216700         FETCH TP6PART2-CRS                                               
216800         INTO :TP6PART-IDARTNR                                            
216900             ,:TP6PART-IDDC                                               
217000             ,:TP6PART-ADLAGOMR                                           
217100             ,:TP6PART-ADGANG                                             
217200             ,:TP6PART-ADPLATS                                            
217300             ,:TP6PART-KVLS                                               
217400             ,:TP6PART-KVPB-REF                                           
217500             ,:TP6PART-TIREFEFT                                           
217600             ,:TP6PART-KDPRODSL                                           
217700             ,:TP6PART-IDSKYLT                                            
217800             ,:TP6PART-BEART                                              
217900     END-EXEC                                                             
218000                                                                          
218100     MOVE SQLCODE TO SQLCODE-WS                                           
218200     PERFORM DB2-STATUS-KONTROLL                                          
218300     .                                                                    
218400     SKIP3                                                                
218500                                                                          
218600 DB2-CLOSE-TP6PART-AISLE SECTION.                                         
218700                                                                          
218800     EXEC SQL CLOSE TP6PART2-CRS    END-EXEC                              
218900     .                                                                    
219000                                                                          
219100 DB2-DCL-OPN-TP6PART-SORT SECTION.                                        
219200                                                                          
219300     MOVE 000100  TO GODK-SQLCODEKODER                                    
219400                                                                          
219500     EXEC SQL                                                             
219600         DECLARE TP6PART3-CRS CURSOR FOR                                  
219700                                                                          
219800           SELECT   IDARTNR                                               
219900                   ,IDDC                                                  
220000                   ,ADLAGOMR                                              
220100                   ,ADGANG                                                
220200                   ,ADPLATS                                               
220300                   ,KVLS                                                  
220400                   ,KVPB_REF                                              
220500                   ,KDSORT                                                
220600                   ,KDPRODSL                                              
220700                   ,IDSKYLT                                               
220800                   ,BEART                                                 
220900                                                                          
221000           FROM    TP6PART                                                
221100           WHERE   IDDC     = :REQU-IDDC-KEY                              
221200           AND     KDSORT   = :REQU-KDSORT-KEY                            
221300           AND     ADLAGOMR > :WS-ADLAGOMR-ZERO                           
221400           AND     IDSKYLT  = :W-IDSKYLT                                  
221500           ORDER BY IDARTNR                                               
221600     END-EXEC                                                             
221700                                                                          
221800     MOVE 000100  TO GODK-SQLCODEKODER                                    
221900     EXEC SQL OPEN TP6PART3-CRS    END-EXEC                               
222000     .                                                                    
222100     SKIP3                                                                
222200                                                                          
222300 DB2-FETCH-TP6PART-SORT  SECTION.                                         
222400     SKIP2                                                                
222500     MOVE 000100  TO GODK-SQLCODEKODER                                    
222600     EXEC SQL                                                             
222700         FETCH TP6PART3-CRS                                               
222800         INTO :TP6PART-IDARTNR                                            
222900             ,:TP6PART-IDDC                                               
223000             ,:TP6PART-ADLAGOMR                                           
223100             ,:TP6PART-ADGANG                                             
223200             ,:TP6PART-ADPLATS                                            
223300             ,:TP6PART-KVLS                                               
223400             ,:TP6PART-KVPB-REF                                           
223500             ,:TP6PART-KDSORT                                             
223600             ,:TP6PART-KDPRODSL                                           
223700             ,:TP6PART-IDSKYLT                                            
223800             ,:TP6PART-BEART                                              
223900     END-EXEC                                                             
224000                                                                          
224100     MOVE SQLCODE TO SQLCODE-WS                                           
224200     PERFORM DB2-STATUS-KONTROLL                                          
224300     .                                                                    
224400     SKIP3                                                                
224500                                                                          
224600 DB2-CLOSE-TP6PART-SORT SECTION.                                          
224700                                                                          
224800     EXEC SQL CLOSE TP6PART3-CRS    END-EXEC                              
224900     .                                                                    
225000                                                                          
225100 DB2-DCL-OPN-TP6PART-LANG SECTION.                                        
225200                                                                          
225300     MOVE 000100  TO GODK-SQLCODEKODER                                    
225400                                                                          
225500     EXEC SQL                                                             
225600         DECLARE TP6PART4-CRS CURSOR FOR                                  
225700                                                                          
225800           SELECT   IDARTNR                                               
225900                   ,IDDC                                                  
226000                   ,ADLAGOMR                                              
226100                   ,ADGANG                                                
226200                   ,ADPLATS                                               
226300                   ,KVLS                                                  
226400                   ,KVPB_REF                                              
226500                   ,KDSORT                                                
226600                   ,KDPRODSL                                              
226700                   ,IDSKYLT                                               
226800                   ,BEART                                                 
226900                                                                          
227000           FROM    TP6PART                                                
227100           WHERE   IDDC         = :REQU-IDDC-KEY                          
227200           AND     ADLAGOMR     > :WS-ADLAGOMR-ZERO                       
227300           AND     BEART     LIKE :WS-BEART                               
227400           AND     IDSKYLT      = :W-IDSKYLT                              
227500           ORDER BY BEART, IDARTNR                                        
227600     END-EXEC                                                             
227700                                                                          
227800     MOVE 000100  TO GODK-SQLCODEKODER                                    
227900     EXEC SQL OPEN TP6PART4-CRS    END-EXEC                               
228000     .                                                                    
228100     SKIP3                                                                
228200                                                                          
228300 DB2-FETCH-TP6PART-LANG  SECTION.                                         
228400     SKIP2                                                                
228500     MOVE 000100  TO GODK-SQLCODEKODER                                    
228600     EXEC SQL                                                             
228700         FETCH TP6PART4-CRS                                               
228800         INTO :TP6PART-IDARTNR                                            
228900             ,:TP6PART-IDDC                                               
229000             ,:TP6PART-ADLAGOMR                                           
229100             ,:TP6PART-ADGANG                                             
229200             ,:TP6PART-ADPLATS                                            
229300             ,:TP6PART-KVLS                                               
229400             ,:TP6PART-KVPB-REF                                           
229500             ,:TP6PART-KDSORT                                             
229600             ,:TP6PART-KDPRODSL                                           
229700             ,:TP6PART-IDSKYLT                                            
229800             ,:TP6PART-BEART                                              
229900     END-EXEC                                                             
230000                                                                          
230100     MOVE SQLCODE TO SQLCODE-WS                                           
230200     PERFORM DB2-STATUS-KONTROLL                                          
230300     .                                                                    
230400     SKIP3                                                                
230500                                                                          
230600 DB2-CLOSE-TP6PART-LANG SECTION.                                          
230700                                                                          
230800     EXEC SQL CLOSE TP6PART4-CRS    END-EXEC                              
230900     .                                                                    
239300                                                                          
245900 DB2-STATUS-KONTROLL  SECTION.                                            
246000     SET SQLCODE-IX TO 1                                                  
246100     SEARCH GODK-SQLCODE                                                  
246200       AT END                                                             
246300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
246400          DELIMITED BY SIZE INTO FELTEXT                                  
246500          CALL ABEND USING RKOD-ABEND-DB2                                 
246600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
246700     END-SEARCH                                                           
246800     .                                                                    
246900     EJECT                                                                
