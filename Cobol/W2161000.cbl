000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2161000.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   94/01/31.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*              BMP  (UTAN CHECKPOINT)                                     
001200*       (PROGRAMMET AVBRYTES EFTER 1000 UPPDATERINGAR,                    
001300*        RESTERANDE '2402':OR VID NÄSTA KÖRNINGSTILLFÄLLE)                
001400*                                                                         
001500*        SKAPAR LISTPOSTER FÖR SKROTBEORDRADE ARTIKLAR                    
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WL2401 (WDR5) (2401)                       
001800*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001900*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002000*        PROGRAMMET LÄSER      WLARTD (WDD8)                              
002100*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
002200*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
002300*        PROGRAMMET LÄSER      WLKATN (WDN6)                              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900*    2011-10-27  E'TRACKER  10143271 CHINA  WAREHOUSE PROJECT-1           
003000*                                                                         
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*          --- LISTPOSTER SKROTORDER                                      
004100     SELECT W21611                     ASSIGN TO W21610D1.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W21611                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100*01  POST -COPY W21611 -PRE  W21611-  -L.                                 
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400     SKIP2                                                                
005500*    -- CHECKED BY WY2000                                                 
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(8)    VALUE 'W2161000'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 01  W-SDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.                  
006200 01  W-SDC-KVAKS         PIC S9(7)    VALUE ZERO COMP-3.                  
006300 01  W-SDC-KVOKS         PIC S9(7)    VALUE ZERO COMP-3.                  
006400 01  W-ANTAL-UPPDAT      PIC S9(5)    VALUE ZERO COMP-3.                  
006500 01  MAX-ANTAL-UPPDAT    PIC S9(5)    VALUE 1000 COMP-3.                  
006600     EJECT                                                                
006700 01      INDEXFALT.                                                       
006800   03    XCL             PIC S9(3)                   COMP-3.              
006900   03    IX              PIC S9(9)                   COMP SYNC.           
007000   03    UT-IX           PIC S9(3)                   COMP-3.              
007100   03    UT-IX-MAX       PIC S9(3)    VALUE 10       COMP-3.              
007200   03    TEXT-IX         PIC S9(9)                   COMP SYNC.           
007300     SKIP3                                                                
007400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500     SKIP2                                                                
008600*    --- PARAMETRAR TILL ABEND                                            
008700                                                                          
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900 01  W21611-AREA-START           PIC X(24)   VALUE                        
010000                                 'W21611-AREA-START  '.                   
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W21611     -PRE W21611-                                   
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700                                                                          
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100    03    W-WDGXKEY-X.                                                    
011200       05 W-IDHTYP               PIC X(4)    VALUE '2401'.                
011300       05 FILLER                 PIC X(26)   VALUE LOW-VALUE.             
011400     03  W-IDARTNR-X.                                                     
011500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011600     03  W-IDDC-X.                                                        
011700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011800     03  W-IDSKYLT-X.                                                     
011900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
012000     03  W-KDSEGKEY-X.                                                    
012100         05  W-KDSEGKEY          PIC S9(1)   VALUE ZERO COMP-3.           
012200     03  W-WDD811KY-X.                                                    
012300         05  W-WDD811KY          PIC S9(12)   VALUE ZERO COMP-3.          
012400     03  W-KDARBTYP-X.                                                    
012500         05  W-KDARBTYP          PIC X(8)    VALUE 'ANSK'.                
012600     03  W-IDPERSON-X.                                                    
012700         05  W-IDPERSON          PIC S9(3)   COMP-3 VALUE +0.             
012800     03  W-IDDC-B6-X.                                                     
012900         05 W-IDDC-B6                  PIC X(2).                          
013000     03  W-WDGXKEY-6321.                                                  
013100         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
013200         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
013300         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
013400     03  W-DASKROT9-X.                                                    
013500         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
013600     03  W-DASKROT9-MIN-X.                                                
013700         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
013800     03  W-DASKROT9-MAX-X.                                                
013900         05  W-DASKROT9-MAX  PIC 9(8)   VALUE 99999999.                   
014000     03  W-KDSTASKR-MIN-X.                                                
014100         05  W-KDSTASKR-MIN  PIC 9      VALUE ZERO.                       
014200     03  W-KDSTASKR-MAX-X.                                                
014300         05  W-KDSTASKR-MAX  PIC 9      VALUE 9.                          
014400     03  W-KY6324-MIN-X.                                                  
014500         05  W-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.                
014600         05  W-IDDC-MIN      PIC X(2)   VALUE SPACE.                      
014700         05  FILLER          PIC X      VALUE LOW-VALUE.                  
014800     03  W-KY6324-MAX-X.                                                  
014900         05  W-IDARTNR-MAX   PIC S9(9)  VALUE ZERO COMP-3.                
015000         05  W-IDDC-MAX      PIC X(2)   VALUE SPACE.                      
015100         05  FILLER          PIC X      VALUE HIGH-VALUE.                 
015200     03  W-KY6324-KVAL-X.                                                 
015300         05  W-IDARTNR-KVAL  PIC S9(9)  VALUE ZERO COMP-3.                
015400         05  W-IDDC-KVAL     PIC X(2)   VALUE SPACE.                      
015500         05  W-KDSTASKR-KVAL PIC S9     VALUE ZERO COMP-3.                
015600     SKIP2                                                                
015700     EJECT                                                                
015800*    --- STATUS-KOD FRÅN IMS                                              
015900 01  STATUS-WS                   PIC XX.                                  
016000     88  SEGMENT-FINNS                       VALUE '  '.                  
016100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016300     SKIP2                                                                
016400 01  GODK-STATUSKODER.                                                    
016500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016600     SKIP3                                                                
016700 01  SSA1                        PIC X(96).                               
016800 01  SSA2                        PIC X(96).                               
016900 01  SSA3                        PIC X(96).                               
017000     EJECT                                                                
017100*    --- IMS FUNKTIONSKODER                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017600     SKIP3                                                                
017700 01  DLI-IO-AREA.                                                         
017800     03  IO-AREA                 PIC X(688)  VALUE SPACE.                 
017900     SKIP3                                                                
018000     03  WDGX2402 REDEFINES IO-AREA.                                      
018100*        05  -COPY WDGX2402                                               
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-2'.         
018400     SKIP3                                                                
018500 01  DLI-IO-AREA-2.                                                       
018600     03  IO-AREA-2               PIC X(150) VALUE SPACE.                  
018700     SKIP3                                                                
018800     03  WLBENA01 REDEFINES IO-AREA-2.                                    
018900*        05  -COPY WDD301  -PRE WDD301-                                   
019000     EJECT                                                                
019100     03  WLBENA11 REDEFINES IO-AREA-2.                                    
019200*        05  -COPY WDD311  -PRE WDD311-                                   
019300     EJECT                                                                
019400     03  WLARTD01 REDEFINES IO-AREA-2.                                    
019500*        05  -COPY WDD801  -PRE WDD801-                                   
019600     EJECT                                                                
019700     03  WLARTD11 REDEFINES IO-AREA-2.                                    
019800*        05  -COPY WDD811  -PRE WDD811-                                   
019900     EJECT                                                                
020000     03  WLARTM01 REDEFINES IO-AREA-2.                                    
020100*        05  -COPY WDK901  -PRE WDK901-                                   
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-3'.         
020400     SKIP3                                                                
020500 01  DLI-IO-AREA-3.                                                       
020600     03  IO-AREA-3               PIC X(300) VALUE SPACE.                  
020700     SKIP3                                                                
020800     03  WLARTS01 REDEFINES IO-AREA-3.                                    
020900*        05  -COPY WDK701                                                 
021000     EJECT                                                                
021100     03  WLARTS11 REDEFINES IO-AREA-3.                                    
021200*        05  -COPY WDK711                                                 
021300     EJECT                                                                
021400     03  WLKATN01 REDEFINES IO-AREA-3.                                    
021500*        05  -COPY WDN601                                                 
021600     EJECT                                                                
021700     03  WLKATN11 REDEFINES IO-AREA-3.                                    
021800*        05  -COPY WDN611                                                 
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-01'.        
022100     SKIP3                                                                
022200 01  DLI-IO-AREA-01.                                                      
022300     03  IO-AREA-01              PIC X(150) VALUE SPACE.                  
022400     SKIP3                                                                
022500     03  WLARTC01 REDEFINES IO-AREA-01.                                   
022600*        05  -COPY WDK601                                                 
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-11'.        
022900     SKIP3                                                                
023000 01  DLI-IO-AREA-11.                                                      
023100     03  IO-AREA-11              PIC X(900) VALUE SPACE.                  
023200     SKIP3                                                                
023300     03  WLARTC11 REDEFINES IO-AREA-11.                                   
023400*        05  -COPY WDK611                                                 
023500     EJECT                                                                
023600*- - - - - - - - - - - - - - -                                            
023700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-P311'.           
023800 01  DLI-IO-P311.                                                         
023900*   03  -COPY WDP311                                                      
024000     EJECT                                                                
024100                                                                          
024200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024300 01   DLI-IO-AREA-B601.                                                   
024400*     03  -COPY WDB601                                                    
024500     EJECT                                                                
024600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
024700 01  DLI-IO-WDR501-6321.                                                  
024800*    03  -COPY WDGX6321                                                   
024900     EJECT                                                                
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
025100 01  DLI-IO-WDGX6322.                                                     
025200*    03  -COPY WDGX6322                                                   
025300     EJECT                                                                
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
025500 01  DLI-IO-WDGX6324.                                                     
025600*    03  -COPY WDGX6324                                                   
025700     EJECT                                                                
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
025900 01  DLI-IO-WDGX6325.                                                     
026000*    03  -COPY WDGX6325                                                   
026100                                                                          
026200 LINKAGE SECTION.                                                         
026300                                                                          
026400                                                                          
026500*01  -COPY W0009  -PRE MSG-                                               
026600     EJECT                                                                
026700*01  -COPY W0008  -PRE 2401-                                              
026800     05  FILLER                  PIC X.                                   
026900     EJECT                                                                
027000*01  -COPY W0008  -PRE BENA-                                              
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027300*01  -COPY W0008  -PRE ARTC-                                              
027400     05  FILLER                  PIC X.                                   
027500     EJECT                                                                
027600*01  -COPY W0008  -PRE ARTD-                                              
027700     05  FILLER                  PIC X.                                   
027800     EJECT                                                                
027900*01  -COPY W0008  -PRE ARTM-                                              
028000     05  FILLER                  PIC X.                                   
028100     EJECT                                                                
028200*01  -COPY W0008  -PRE ARTS-                                              
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028500*01  -COPY W0008  -PRE KATN-                                              
028600     05  FILLER                  PIC X.                                   
028700     EJECT                                                                
028800*01    -COPY W0008     -PRE WDP3-                                         
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01    -COPY W0008     -PRE WDB6-                                         
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008  -PRE 6321-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700                                                                          
029800 PROCEDURE DIVISION    USING    MSG-PCB  2401-PCB                         
029900     BENA-PCB ARTC-PCB ARTD-PCB ARTM-PCB ARTS-PCB KATN-PCB                
030000                                WDP3-PCB WDB6-PCB 6321-PCB.               
030100                                                                          
030200     ENTRY 'DLITCBL'   USING    MSG-PCB  2401-PCB                         
030300     BENA-PCB ARTC-PCB ARTD-PCB ARTM-PCB ARTS-PCB KATN-PCB                
030400                                WDP3-PCB WDB6-PCB 6321-PCB.               
030500     SKIP2                                                                
030600     PERFORM A-INIT                                                       
030700                                                                          
030800     PERFORM IMS-GET-WDR501-2401                                          
030900     PERFORM IMS-GHNP-WDGX2402                                            
031000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
031100        W-ANTAL-UPPDAT > MAX-ANTAL-UPPDAT                                 
031200        PERFORM B-FLYTTA-SKROTSEG-UPPGIFTER                               
031300        PERFORM IMS-DLET-WDGX2402                                         
031400        ADD +1  TO W-ANTAL-UPPDAT                                         
031500        PERFORM C-SAMLA-UPPGIFTER                                         
031600        PERFORM S11-SKRIV-W21611                                          
031700        PERFORM IMS-GHNP-WDGX2402                                         
031800     END-PERFORM                                                          
031900     PERFORM Z-FINIT                                                      
032000                                                                          
032100     MOVE ZERO TO RETURN-CODE                                             
032200     GOBACK                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 A-INIT SECTION.                                                          
032600                                                                          
032700     OPEN OUTPUT W21611                                                   
032800     SKIP2                                                                
032900     ACCEPT DAGENS-DATUM  FROM DATE                                       
033000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033100     MOVE ZERO TO W21611-ADLAGOMR                                         
033200     MOVE ZERO TO W21611-ADGANG                                           
033300     MOVE ZERO TO W21611-ADPLATS                                          
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700                                                                          
033800 B-FLYTTA-SKROTSEG-UPPGIFTER SECTION.                                     
033900                                                                          
034000        MOVE 2402-IDARTNR         TO W-IDARTNR                            
034100                                     W21611-IDARTNR                       
034200                                     W-IDARTNR-KVAL                       
034300        MOVE 2402-IDANALYS        TO W21611-IDANALYS                      
034400        MOVE 2402-IDDC            TO W21611-IDDC                          
034500                                     W-IDDC-KVAL                          
034600        MOVE 2402-IDKUNDNR        TO W21611-IDKUNDNR                      
034700        MOVE 2402-IDDISTR         TO W21611-IDDISTR                       
034800        MOVE 2402-IDKONTO         TO W21611-IDKONTO                       
034900        MOVE 2402-IDKST           TO W21611-IDKST                         
035000        MOVE 2402-IDPERSON        TO W21611-IDPERSON                      
035100        MOVE 2402-KDARBTYP        TO W21611-KDARBTYP                      
035200                                     W-6321-KDARBTYP                      
035300        MOVE 2402-KVSKROT-BEORD   TO W21611-KVSKROT-BEORD                 
035400        MOVE 2402-KVSKROT-KVAR    TO W21611-KVSKROT-KVAR                  
035500        MOVE 2402-IDUSER          TO W21611-IDUSER                        
035600        MOVE 2402-BEANST          TO W21611-BEANST                        
035700        MOVE 2402-DASKROT9-BEORD  TO W21611-DASKROT9-BEORD                
035800                                     W-DASKROT9                           
035900        MOVE 2402-KDERS-UTG       TO W21611-KDERS-UTG                     
036000        MOVE 2402-SUTPO-TOT       TO W21611-SUTPO-TOT                     
036100        MOVE 2402-KVTILLG-CDC     TO W21611-KVTILLG-CDC                   
036200        MOVE 2402-KVTILLG-SDC     TO W21611-KVTILLG-SDC                   
036300        MOVE 2402-KVAKS-CDC       TO W21611-KVAKS-CDC                     
036400        MOVE 2402-KVAKS-SDC       TO W21611-KVAKS-SDC                     
036500        MOVE +1 TO UT-IX                                                  
036600        PERFORM UNTIL UT-IX > UT-IX-MAX                                   
036700           MOVE 2402-BEANST-GODK(UT-IX)                                   
036800                                   TO W21611-BEANST-GODK(UT-IX)           
036900           MOVE 2402-TIDATETIME(UT-IX)                                    
037000                                   TO W21611-TIDATETIME(UT-IX)            
037100           MOVE 2402-IDUSER-GODK(UT-IX)                                   
037200                                   TO W21611-IDUSER-GODK(UT-IX)           
037300           ADD +1 TO UT-IX                                                
037400        END-PERFORM                                                       
037500        MOVE +1 TO UT-IX                                                  
037600        PERFORM UNTIL UT-IX > 20                                          
037700           MOVE 2402-BEEMBLEM(UT-IX)                                      
037800                                   TO W21611-BEEMBLEM(UT-IX)              
037900           ADD +1 TO UT-IX                                                
038000        END-PERFORM                                                       
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400                                                                          
038500 C-SAMLA-UPPGIFTER SECTION.                                               
038600                                                                          
038700     PERFORM CB-LAS-FLYTTA-WDD3                                           
038800     PERFORM CC-LAS-FLYTTA-WDK6                                           
038900     PERFORM CD-LAS-FLYTTA-WDD8                                           
039000*    PERFORM CE-LAS-FLYTTA-WDK9                                           
039100*    PERFORM CF-LAS-FLYTTA-WDN6                                           
039200     PERFORM CG-HAMTA-IDPERSON                                            
039300     PERFORM CH-HAMTA-TEXT-WDR5-6325                                      
039400     .                                                                    
039500     EJECT                                                                
039600                                                                          
039700                                                                          
039800 CB-LAS-FLYTTA-WDD3 SECTION.                                              
039900                                                                          
040000                                                                          
040100     PERFORM IMS-GET-WDD301-BSEQ                                          
040200     IF W21611-IDDC NOT = W-IDDC-B6                                       
040300        MOVE W21611-IDDC   TO W-IDDC-B6                                   
040400        PERFORM IMS-GU-WDB601                                             
040500     END-IF                                                               
041110     IF DCS-SWEDEN                                                        
041140         MOVE 'S ' TO W-IDSKYLT                                           
041150     ELSE                                                                 
041160         MOVE 'GB' TO W-IDSKYLT                                           
041170     END-IF                                                               
041200     PERFORM IMS-GET-WDD311-BSEQ                                          
041300     IF SEGMENT-FINNS                                                     
041400        MOVE WDD311-TEXT-BEART     TO W21611-BEART                        
041500     ELSE                                                                 
041600        MOVE SPACE                 TO W21611-BEART                        
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000                                                                          
042100                                                                          
042200 CC-LAS-FLYTTA-WDK6 SECTION.                                              
042300                                                                          
042400     PERFORM IMS-GET-WDK601                                               
042500     IF SEGMENT-FINNS                                                     
042600        PERFORM IMS-GNP-WDK611                                            
042700*       MOVE CLAG-KDERS        TO W21611-KDERS-UTG                        
042800        MOVE CLAG-IDANSK       TO W21611-IDANSK                           
042900        MOVE CLAG-PRARTSTD     TO W21611-PRARTSTD                         
043000        MOVE +1                TO XCL                                     
043100*       MOVE CLAG-KVLS         TO W21611-KVLS(XCL)                        
043200*       MOVE CLAG-KVRESS       TO W21611-KVRESS                           
043300*       MOVE CLAG-KVAKS-CDC    TO W21611-KVAKS  (XCL)                     
043400*       ADD  CLAG-KVAKS-PAV    TO W21611-KVAKS  (XCL)                     
043500*       ADD  CLAG-KVAKS-T      TO W21611-KVAKS  (XCL)                     
043600*       MOVE CLAG-KVROS        TO W21611-KVROS                            
043700        IF W21611-IDDC NOT = W-IDDC-B6                                    
043800           MOVE W21611-IDDC   TO W-IDDC-B6                                
043900           PERFORM IMS-GU-WDB601                                          
044000        END-IF                                                            
044100        IF DCS-CDC                                                        
044200           MOVE CLAG-ADLAGOMR  TO W21611-ADLAGOMR                         
044300           MOVE CLAG-ADGANG    TO W21611-ADGANG                           
044400           MOVE CLAG-ADPLATS   TO W21611-ADPLATS                          
044500        END-IF                                                            
044600        MOVE +2 TO XCL                                                    
044700*       MOVE ZERO              TO W21611-KVLS    (XCL)                    
044800*                                 W21611-KVAKS   (XCL)                    
044900*                                 W21611-KVOKS   (XCL)                    
045000        PERFORM CCA-LAS-FLYTTA-WDK7                                       
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400                                                                          
045500 CCA-LAS-FLYTTA-WDK7 SECTION.                                             
045600                                                                          
045700     PERFORM IMS-GET-SART-SEG                                             
045800     IF SEGMENT-FINNS                                                     
045900        MOVE W21611-IDDC  TO W-IDDC                                       
046200        PERFORM IMS-GET-SLAG-SEG                                          
046300        IF SEGMENT-FINNS                                                  
047601          IF DCS-NDC-CN OR DCS-NDC-NA                                     
047610            MOVE SLAG-PRAVCOST   TO W21611-PRARTSTD                       
047620          END-IF                                                          
047800          MOVE SLAG-ADLAGOMR     TO W21611-ADLAGOMR                       
047900          MOVE SLAG-ADGANG       TO W21611-ADGANG                         
048000          MOVE SLAG-ADPLATS      TO W21611-ADPLATS                        
048200        ELSE                                                              
048210          MOVE ZERO              TO W21611-ADLAGOMR                       
048220          MOVE ZERO              TO W21611-ADGANG                         
048230          MOVE ZERO              TO W21611-ADPLATS                        
048300        END-IF                                                            
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100                                                                          
049200 CD-LAS-FLYTTA-WDD8 SECTION.                                              
049300                                                                          
049400     PERFORM IMS-GET-WDD801                                               
049500     PERFORM CDA-NOLLSTALL                                                
049600     IF SEGMENT-FINNS                                                     
049700        MOVE W21611-IDDC     TO W-IDDC                                    
049800        PERFORM IMS-GET-WDD811                                            
049900        MOVE +1 TO IX                                                     
050000        PERFORM UNTIL SEGMENT-SAKNAS OR IX > +4                           
050100           IF IX = +1                                                     
050200              MOVE WDD811-SALDO-ADBUFFOMR TO W21611-ADBUFFOMR-1           
050300              MOVE WDD811-SALDO-ADBUFFGANG TO W21611-ADBUFFGANG-1         
050400              MOVE WDD811-SALDO-ADBUFFPL TO W21611-ADBUFFPL-1             
050500           END-IF                                                         
050600           IF IX = +2                                                     
050700              MOVE WDD811-SALDO-ADBUFFOMR TO W21611-ADBUFFOMR-2           
050800              MOVE WDD811-SALDO-ADBUFFGANG TO W21611-ADBUFFGANG-2         
050900              MOVE WDD811-SALDO-ADBUFFPL TO W21611-ADBUFFPL-2             
051000           END-IF                                                         
051100           IF IX = +3                                                     
051200              MOVE WDD811-SALDO-ADBUFFOMR TO W21611-ADBUFFOMR-3           
051300              MOVE WDD811-SALDO-ADBUFFGANG TO W21611-ADBUFFGANG-3         
051400              MOVE WDD811-SALDO-ADBUFFPL TO W21611-ADBUFFPL-3             
051500           END-IF                                                         
051600           IF IX = +4                                                     
051700              MOVE WDD811-SALDO-ADBUFFOMR TO W21611-ADBUFFOMR-4           
051800              MOVE WDD811-SALDO-ADBUFFGANG TO W21611-ADBUFFGANG-4         
051900              MOVE WDD811-SALDO-ADBUFFPL TO W21611-ADBUFFPL-4             
052000           END-IF                                                         
052100           ADD +1 TO IX                                                   
052200           PERFORM IMS-GET-WDD811                                         
052300        END-PERFORM                                                       
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700                                                                          
052800                                                                          
052900 CDA-NOLLSTALL SECTION.                                                   
053000                                                                          
053100     MOVE ZERO    TO W21611-ADBUFFOMR-1                                   
053200                     W21611-ADBUFFGANG-1                                  
053300                     W21611-ADBUFFPL-1                                    
053400                     W21611-ADBUFFOMR-2                                   
053500                     W21611-ADBUFFGANG-2                                  
053600                     W21611-ADBUFFPL-2                                    
053700                     W21611-ADBUFFOMR-3                                   
053800                     W21611-ADBUFFGANG-3                                  
053900                     W21611-ADBUFFPL-3                                    
054000                     W21611-ADBUFFOMR-4                                   
054100                     W21611-ADBUFFGANG-4                                  
054200                     W21611-ADBUFFPL-4                                    
054300      .                                                                   
054400      EJECT                                                               
054500                                                                          
054600                                                                          
054700*CE-LAS-FLYTTA-WDK9 SECTION.                                              
054800*                                                                         
054900*    PERFORM IMS-GET-WDK901                                               
055000*    IF SEGMENT-FINNS                                                     
055100*       MOVE WDK901-ART-SUTPO-TOT   TO W21611-SUTPO-TOT                   
055200*       MOVE WDK901-ART-KVOKS-BULK  TO W21611-KVOKS (1)                   
055300*       ADD  WDK901-ART-KVOKS-DAG   TO W21611-KVOKS (1)                   
055400*       ADD  WDK901-ART-KVOKS-VOR   TO W21611-KVOKS (1)                   
055500*    ELSE                                                                 
055600*       MOVE ZERO                   TO W21611-SUTPO-TOT                   
055700*       MOVE ZERO                   TO W21611-KVOKS (1)                   
055800*    END-IF                                                               
055900*     .                                                                   
056000*     EJECT                                                               
056100*                                                                         
056200*CF-LAS-FLYTTA-WDN6 SECTION.                                              
056300*    MOVE +1 TO IX                                                        
056400*    PERFORM UNTIL IX > 20                                                
056500*       MOVE SPACE        TO W21611-BEEMBLEM (IX)                         
056600*       ADD +1        TO IX                                               
056700*    END-PERFORM                                                          
056800*    PERFORM IMS-GET-MASTER-WDN6                                          
056900*    IF SEGMENT-FINNS                                                     
057000*       PERFORM IMS-GET-KATINFO-MASTER                                    
057100*       MOVE +1 TO IX                                                     
057200*       PERFORM UNTIL IX > 20 OR SEGMENT-SAKNAS                           
057300*          MOVE KAT-BEEMBLEM TO W21611-BEEMBLEM (IX)                      
057400*          ADD +1        TO IX                                            
057500*          PERFORM IMS-GET-KATINFO-MASTER                                 
057600*       END-PERFORM                                                       
057700*       IF SEGMENT-FINNS                                                  
057800*          MOVE 'MORE' TO W21611-BEEMBLEM (20)                            
057900*       END-IF                                                            
058000*    END-IF                                                               
058100*     .                                                                   
058200*     EJECT                                                               
058300                                                                          
058400 CG-HAMTA-IDPERSON  SECTION.                                              
058500                                                                          
058600     MOVE SPACE            TO W21611-IDNAMN                               
058700     MOVE ZERO             TO W21611-IDAVD                                
058800     MOVE 2402-KDARBTYP    TO W-KDARBTYP                                  
058900     MOVE 2402-IDPERSON    TO W-IDPERSON                                  
059000                                                                          
059100     PERFORM IMS-LAS-IDPERSON                                             
059200                                                                          
059300     IF SEGMENT-FINNS                                                     
059400        MOVE PERS-IDNAMN   TO W21611-IDNAMN                               
059500        MOVE PERS-IDAVD    TO W21611-IDAVD                                
059600     ELSE                                                                 
059700        DISPLAY ' NAMN SAKNAS  ' 2402-KDARBTYP '  ' 2402-IDPERSON         
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100                                                                          
060200 CH-HAMTA-TEXT-WDR5-6325 SECTION.                                         
060300     PERFORM CHA-BLANKA-FAELT                                             
060400     MOVE +1      TO TEXT-IX                                              
060500     MOVE 1      TO W-KDSTASKR-KVAL                                       
060600     PERFORM IMS-GU-WDGX6324                                              
060700     IF SEGMENT-SAKNAS                                                    
060800       MOVE 2    TO W-KDSTASKR-KVAL                                       
060900       PERFORM IMS-GU-WDGX6324                                            
061000     END-IF                                                               
061100     IF SEGMENT-FINNS                                                     
061200       PERFORM IMS-GNP-WDGX6325                                           
061300       PERFORM UNTIL TEXT-IX > 11                                         
061400         IF SEGMENT-FINNS                                                 
061500           MOVE 6325-TEMEMO TO W21611-TEMEMO(TEXT-IX)                     
061600           PERFORM IMS-GNP-WDGX6325                                       
061700         ELSE                                                             
061800           MOVE SPACE TO W21611-TEMEMO(TEXT-IX)                           
061900         END-IF                                                           
062000         ADD +1 TO TEXT-IX                                                
062100       END-PERFORM                                                        
062200     END-IF                                                               
062300                                                                          
062400     .                                                                    
062500     EJECT                                                                
062600                                                                          
062700 CHA-BLANKA-FAELT SECTION.                                                
062800     MOVE +1      TO TEXT-IX                                              
062900     PERFORM UNTIL TEXT-IX > 11                                           
063000       MOVE SPACE    TO W21611-TEMEMO(TEXT-IX)                            
063100       ADD +1 TO TEXT-IX                                                  
063200     END-PERFORM                                                          
063300     .                                                                    
063400     EJECT                                                                
063500                                                                          
063600 Z-FINIT SECTION.                                                         
063700     CLOSE W21611                                                         
063800     SKIP2                                                                
063900     MOVE 'S' TO POSTSUM-OPKOD                                            
064000     CALL POSTSUM USING POSTSUM-PARM                                      
064100     .                                                                    
064200     EJECT                                                                
064300 S11-SKRIV-W21611 SECTION.                                                
064400     SKIP2                                                                
064500     WRITE W21611-POST FROM W21611-AREA                                   
064600                                                                          
064700     MOVE 'W21611' TO POSTSUM-FDNAMN                                      
064800     MOVE 'W21610D1' TO POSTSUM-DDNAMN2                                   
064900     CALL POSTSUM USING POSTSUM-PARM                                      
065000     MOVE ZERO TO W21611-ADLAGOMR                                         
065100     MOVE ZERO TO W21611-ADGANG                                           
065200     MOVE ZERO TO W21611-ADPLATS                                          
065300     .                                                                    
065400     EJECT                                                                
065500* --- IMS SEKTIONER ---                                                   
065600                                                                          
065700                                                                          
065800 IMS-GET-WDR501-2401 SECTION.                                             
065900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
066000            DELIMITED BY SIZE INTO SSA1                                   
066100     MOVE '  ' TO GODK-STATUSKODER                                        
066200     CALL CBLTDLI USING GU 2401-PCB DLI-IO-AREA SSA1                      
066300     MOVE 2401-STATUS-CODE TO STATUS-WS                                   
066400     PERFORM IMS-STATUSKONTROLL                                           
066500     .                                                                    
066600                                                                          
066700                                                                          
066800 IMS-GHNP-WDGX2402 SECTION.                                               
066900     MOVE 'WDGX2402 ' TO SSA1                                             
067000     MOVE '  GE' TO GODK-STATUSKODER                                      
067100     CALL CBLTDLI USING GHNP 2401-PCB DLI-IO-AREA SSA1                    
067200     MOVE 2401-STATUS-CODE TO STATUS-WS                                   
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500     EJECT                                                                
067600                                                                          
067700                                                                          
067800 IMS-DLET-WDGX2402 SECTION.                                               
067900     MOVE ' ' TO GODK-STATUSKODER                                         
068000     CALL CBLTDLI USING DLET 2401-PCB DLI-IO-AREA                         
068100     MOVE 2401-STATUS-CODE TO STATUS-WS                                   
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     .                                                                    
068400     EJECT                                                                
068500                                                                          
068600                                                                          
068700 IMS-GET-WDD301-BSEQ SECTION.                                             
068800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
068900     DELIMITED BY SIZE INTO SSA1                                          
069000     MOVE '  ' TO GODK-STATUSKODER                                        
069100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-2 SSA1                    
069200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
069300     PERFORM IMS-STATUSKONTROLL                                           
069400     .                                                                    
069500                                                                          
069600                                                                          
069700 IMS-GET-WDD311-BSEQ SECTION.                                             
069800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
069900     DELIMITED BY SIZE INTO SSA1                                          
070000     MOVE '  ' TO GODK-STATUSKODER                                        
070100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-2 SSA1                   
070200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     .                                                                    
070500     EJECT                                                                
070600                                                                          
070700                                                                          
070800 IMS-GET-WDK601 SECTION.                                                  
070900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
071000            DELIMITED BY SIZE INTO SSA1                                   
071100     MOVE '  GE' TO GODK-STATUSKODER                                      
071200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
071300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
071400     PERFORM IMS-STATUSKONTROLL                                           
071500     .                                                                    
071600                                                                          
071700                                                                          
071800 IMS-GNP-WDK611 SECTION.                                                  
071900                                                                          
072000     MOVE 'WLARTC11(KDSEGKEY =1)'    TO SSA1                              
072100     MOVE '  ' TO GODK-STATUSKODER                                        
072200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
072300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
072400     PERFORM IMS-STATUSKONTROLL                                           
072500     .                                                                    
072600     EJECT                                                                
072700                                                                          
072800 IMS-GET-WDD801 SECTION.                                                  
072900     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
073000     DELIMITED BY SIZE INTO SSA1                                          
073100     MOVE '  GE' TO GODK-STATUSKODER                                      
073200     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA-2 SSA1                    
073300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
073400     PERFORM IMS-STATUSKONTROLL                                           
073500     .                                                                    
073600                                                                          
073700                                                                          
073800 IMS-GET-WDD811 SECTION.                                                  
073900     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
074000     DELIMITED BY SIZE INTO SSA1                                          
074100     MOVE '  GE' TO GODK-STATUSKODER                                      
074200     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA-2 SSA1                   
074300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     .                                                                    
074600     EJECT                                                                
074700                                                                          
074800                                                                          
074900 IMS-GET-WDK901 SECTION.                                                  
075000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
075100     DELIMITED BY SIZE INTO SSA1                                          
075200     MOVE '  GE' TO GODK-STATUSKODER                                      
075300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-2 SSA1                    
075400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     .                                                                    
075700     EJECT                                                                
075800                                                                          
075900 IMS-GET-SART-SEG SECTION.                                                
076000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
076100            DELIMITED BY SIZE INTO SSA1                                   
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-3 SSA1                    
076400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     SKIP3                                                                
076800 IMS-GET-SLAG-SEG SECTION.                                                
076810     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
076820     DELIMITED BY SIZE INTO SSA1                                          
077000     MOVE '  GE' TO GODK-STATUSKODER                                      
077100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA-3 SSA1                   
077200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     EJECT                                                                
077600 IMS-GET-MASTER-WDN6 SECTION.                                             
077700     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
077800             DELIMITED BY SIZE INTO SSA1                                  
077900     MOVE '  GE' TO GODK-STATUSKODER                                      
078000     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA-3 SSA1                    
078100     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400     SKIP3                                                                
078500 IMS-GET-KATINFO-MASTER SECTION.                                          
078600     MOVE 'WLKATN11 ' TO SSA1                                             
078700     MOVE '  GE' TO GODK-STATUSKODER                                      
078800     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA-3 SSA1                   
078900     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
079000     PERFORM IMS-STATUSKONTROLL                                           
079100     .                                                                    
079200     EJECT                                                                
079300 IMS-LAS-IDPERSON SECTION.                                                
079400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
079500            DELIMITED BY SIZE INTO SSA1                                   
079600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
079700            DELIMITED BY SIZE INTO SSA2                                   
079800     MOVE '  GE' TO GODK-STATUSKODER                                      
079900     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
080000     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
080100     PERFORM IMS-STATUSKONTROLL                                           
080200     .                                                                    
080300     EJECT                                                                
080400                                                                          
080500 IMS-GU-WDB601    SECTION.                                                
080600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
080700          DELIMITED BY SIZE INTO SSA1                                     
080800     MOVE '  GE' TO GODK-STATUSKODER                                      
080900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
081000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
081100     PERFORM IMS-STATUSKONTROLL                                           
081200     IF SEGMENT-SAKNAS                                                    
081300         MOVE SPACE TO DCS-KDDC                                           
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700 IMS-GU-WDGX6324 SECTION.                                                 
081800                                                                          
081900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
082000            DELIMITED BY SIZE INTO SSA1                                   
082100     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
082200            DELIMITED BY SIZE INTO SSA2                                   
082300     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
082400          DELIMITED BY SIZE INTO SSA3                                     
082500     MOVE '  GE' TO GODK-STATUSKODER                                      
082600     CALL CBLTDLI USING GU 6321-PCB DLI-IO-WDGX6324                       
082700                                      SSA1 SSA2 SSA3                      
082800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
082900     PERFORM IMS-STATUSKONTROLL                                           
083000     .                                                                    
083100     SKIP3                                                                
083200                                                                          
083300 IMS-GNP-WDGX6325 SECTION.                                                
083400                                                                          
083500     MOVE 'WDGX6325 '  TO SSA1                                            
083600     MOVE '  GE' TO GODK-STATUSKODER                                      
083700     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6325 SSA1                 
083800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
083900     PERFORM IMS-STATUSKONTROLL                                           
084000     .                                                                    
084100     SKIP3                                                                
084200                                                                          
084300 IMS-STATUSKONTROLL SECTION.                                              
084400     SKIP2                                                                
084500     SET STATUS-IX TO 1                                                   
084600     SEARCH GODK-STATUS                                                   
084700       AT END                                                             
084800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
084900         DISPLAY FELTEXT                                                  
085000         CALL FELLOG                                                      
085100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
085200         CONTINUE                                                         
085300     END-SEARCH                                                           
085400     .                                                                    
