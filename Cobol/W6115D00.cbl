000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6115D00.                                                
000400 AUTHOR.         UMESH JAIN.                                              
000500 DATE-WRITTEN.   09/01/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*    CLONE OF W6115500 PROGRAM                                            
001000*                                                                         
001100*    LÄSER FIL MED STATUSPOSTER FRÅN W6G3 OCH SUMMERAR VAD SOM            
001200*    PRODUCERATS IDAG OCH GENOMLOPPSTIDEN.                                
001300*                                                                         
001400*    LÄSER FIL FRÅN W6G1 MED ÖNSKADE GENOMLOPPSTIDER PER DC/AVD.          
001500*                                                                         
001600*    NOTE:                                                                
001700*    FIL FRÅN W6G1 SKALL VARA SORTERAD PÅ IDDC FÖR                        
001800*    RÄTT PROGRAMFUNKTION.                                                
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  TABELL FULL ELLER FEL FRÅN WORKDAY                      
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- STATUSPOSTER FRÅN W6G3                                     
003200     SELECT W61140                     ASSIGN TO W6115DD1.                
003300     SKIP2                                                                
003400*          --- PARAMETRAR   FRÅN W6G1                                     
003500     SELECT W6115A                     ASSIGN TO W6115DD2.                
003600     SKIP2                                                                
003700*          --- SUMMAFIL MED PRODUCERAT IDAG OCH GENOMLOPPSTID             
003800     SELECT W6115D                     ASSIGN TO W6115DD3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W61140                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800*01  -COPY W6114001      -L.                                              
004900     SKIP3                                                                
005000* CCID 6442199 -                                                          
005100 FD  W6115A                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400     SKIP2                                                                
005500*01  -COPY W6115A01      -L.                                              
005600     SKIP3                                                                
005700 FD  W6115D                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100*01  POST -COPY W6115D01 -PRE  UT-  -L.                                   
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500*    -COPY WY2000W1                                                       
006600     SKIP3                                                                
006700 77  IDPGM                       PIC X(8)    VALUE 'W6115D00'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
006910 77  TIGLT-ADDED                 PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  MAX-SUM-IX                  PIC S9(9)   VALUE +500 COMP SYNC.        
007200                                                                          
007300 77  MAX-MAAL-IX                 PIC S9(9)   VALUE +500 COMP SYNC.        
007400                                                                          
007500 77  DCIX                        PIC S9(9)   COMP-3 VALUE ZERO.           
007600                                                                          
007700*FIX                                                                      
007800 01  WS-TIREGDATFIX          PIC 9(6)        VALUE ZERO.                  
007900 01  FILLER REDEFINES WS-TIREGDATFIX.                                     
008000   05 WS-TIREGDATFIX-AA      PIC 9(2).                                    
008100   05 WS-TIREGDATFIX-MM      PIC 9(2).                                    
008200   05 WS-TIREGDATFIX-DD      PIC 9(2).                                    
008300*SLUTFIX                                                                  
008400                                                                          
008500 01  DC-SPARFAELT.                                                        
008600     05 SPAR-KDINLUPF           PIC X(4)     VALUE SPACE.                 
008700     05 W-KDINLUPF              PIC X(4)     VALUE SPACE.                 
008800     05 W-IDLEVNR               PIC X(5)     VALUE SPACE.                 
008900     05 W-OLD-IDLOPNRM          PIC S9(9)    VALUE ZERO COMP-3.           
009000     05 W-TIGLT-MM              PIC S9(9)    VALUE ZERO COMP-3.           
009100     05 W-IDAVD-DAG             PIC X(5)     VALUE SPACE.                 
009200     05 W-IDGRUPP-DAG           PIC X(2)     VALUE SPACE.                 
009300     05 W-IDAVD-NATT            PIC X(5)     VALUE SPACE.                 
009400     05 W-IDGRUPP-NATT          PIC X(2)     VALUE SPACE.                 
009500                                                                          
009800 77  W-TIKLOCK-OLD-MM        PIC 9(8)        VALUE ZERO.                  
009900 77  W-TIKLOCK-NEW-MM        PIC 9(8)        VALUE ZERO.                  
010000                                                                          
010100 01  W-DK-TIKLOCK            PIC 9(8)        VALUE ZERO.                  
010200 01  FILLER REDEFINES W-DK-TIKLOCK.                                       
010300   05 W-DK-HH                PIC 9(2).                                    
010400   05 W-DK-MM                PIC 9(2).                                    
010500   05 FILLER                 PIC 9(4).                                    
010600                                                                          
010700 01  W-NEW-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
010800 01  FILLER REDEFINES W-NEW-TIKLOCK.                                      
010900   05 W-NEW-HH               PIC 9(2).                                    
011000   05 W-NEW-MM               PIC 9(2).                                    
011100   05 FILLER                 PIC 9(4).                                    
011200                                                                          
011300 01  W-OLD-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
011400 01  FILLER REDEFINES W-OLD-TIKLOCK.                                      
011500   05 W-OLD-HH               PIC 9(2).                                    
011600   05 W-OLD-MM               PIC 9(2).                                    
011700   05 FILLER                 PIC 9(4).                                    
011800                                                                          
011900 01  W-GLT                   PIC 9(5)        VALUE ZERO.                  
012000 01  FILLER REDEFINES W-GLT.                                              
012100   05 W-GLT-HH               PIC 9(3).                                    
012200   05 W-GLT-MM               PIC 9(2).                                    
012300                                                                          
012400 77  W-TIKLOCK-MM            PIC S9(5)      COMP-3  VALUE ZERO.           
012500 77  W-TOT-MM                PIC S9(5)      COMP-3  VALUE ZERO.           
012510 77  NEW-TIKLOCK             PIC 9(9)               VALUE ZERO.           
012520 77  OLD-TIKLOCK             PIC 9(9)               VALUE ZERO.           
012600                                                                          
012700 77  D-DISPLAY               PIC -9(8).                                   
012800 77  W-TIVV                  PIC 9(2)               VALUE ZERO.           
012900 77  W-TIAAVVD               PIC 9(5)               VALUE ZERO.           
013000 01  W-TIAAVVD-ALPHA.                                                     
013100     03 W-TI-FILLER          PIC X(4).                                    
013200     03 W-WEEKDAY            PIC 9(1).                                    
013300 77  W-TIAAVVD-OLD           PIC 9(5)               VALUE ZERO.           
013400 01  W-TIAAVVD-OLD-ALPHA.                                                 
013500     03 W-TI-FILLER-OLD      PIC X(4).                                    
013600     03 W-WEEKDAY-OLD        PIC 9(1).                                    
013700                                                                          
013800* CCID 6442199 -                                                          
013900 01  WS-TIGLT.                                                            
014000     03 WS-TIGLT-HH          PIC 9(2).                                    
014100     03 WS-TIGLT-MM          PIC 9(2).                                    
014200                                                                          
014300 77  W61140-EOF-SW               PIC X       VALUE 'N'.                   
014400     88  END-OF-W61140                       VALUE 'J'.                   
014500                                                                          
014600 77  W6115A-EOF-SW               PIC X       VALUE 'N'.                   
014700     88  END-OF-W6115A                       VALUE 'J'.                   
014800                                                                          
014900 77  OLD-SW                      PIC XX      VALUE '  '.                  
015000     88  OLD-FM                              VALUE 'FM'.                  
015100     88  OLD-EM                              VALUE 'EM'.                  
015200                                                                          
015300 77  NEW-SW                      PIC XX      VALUE '  '.                  
015400     88  NEW-FM                              VALUE 'FM'.                  
015500     88  NEW-EM                              VALUE 'EM'.                  
015600                                                                          
015700 77  NEW-DAG-KVALL-SW            PIC X(5)    VALUE '     '.               
015800     88  NEW-DAG                             VALUE 'DAG  '.               
015900     88  NEW-KVALL                           VALUE 'KVALL'.               
016000                                                                          
016100 77  ADD-RADER-SW                PIC X       VALUE 'N'.                   
016200     88  ADD-RADER                           VALUE 'J'.                   
016300     EJECT                                                                
016400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
016500 01  FILLER REDEFINES DAGENS-DATUM.                                       
016600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016900     SKIP2                                                                
017000 01  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
017100     EJECT                                                                
017200*      --- VALID IDDC CODES                                               
017300*                                                                         
017400*01    -COPY WWDC99                                                       
017500       EJECT                                                              
017600 01  DYNAMISKA-SUBPROGRAM.                                                
017700*                                                                         
017800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
018000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
018200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018300     SKIP2                                                                
018400*    --- PARAMETRAR TILL ABEND                                            
018500                                                                          
018600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018800     SKIP2                                                                
018900 01  FELTEXT.                                                             
019000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
019200     EJECT                                                                
019300*    --- PARAMETRAR TILL DATKORT                                          
019400*                                                                         
019500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6115D'.              
019600     SKIP2                                                                
019700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
019800     SKIP2                                                                
019900*01  -COPY WDATKORT                                                       
020000     EJECT                                                                
020100*    --- PARAMETRAR TILL POSTSUM                                          
020200*                                                                         
020300*01  -COPY W0005   -PRE  POSTSUM-                                         
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)  VALUE 'WORKDAY'.              
020600*01  -COPY WORKAREA                                                       
020700     EJECT                                                                
020800*01  -COPY WDATAREA                                                       
020900     EJECT                                                                
021000*    --- AREA FÖR BERÄKNING AV GLT PER PARTI                              
021100 01  DC-W-SPAR1.                                                          
021200*      05 AREA -COPY W6114001    -PRE W-SPAR1-                            
021300     EJECT                                                                
021400                                                                          
021500*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
021600 01  DC-W-SPAR2.                                                          
021700       05 AREA -COPY W6114001    -PRE W-SPAR2-                            
021800     EJECT                                                                
021900                                                                          
021910*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
021920 01  DC-W-SPAR3.                                                          
021930       05 AREA -COPY W6114001    -PRE W-SPAR3-                            
021940     EJECT                                                                
021950                                                                          
021960*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
021970 01  DC-W-SPAR4.                                                          
021980       05 AREA -COPY W6114001    -PRE W-SPAR4-                            
021990     EJECT                                                                
021991                                                                          
022000 01  IN-AREA-START          PIC X(24)   VALUE 'IN-AREA-START  '.          
022100*01  AREA -COPY W6114001     -PRE IN-                                     
022200     EJECT                                                                
022300                                                                          
022400 01  MAAL-AREA-START        PIC X(24)   VALUE 'MAAL-AREA-START'.          
022500* CCID 6442199 -                                                          
022600*01  AREA -COPY W6115A01     -PRE MIN-                                    
022700                                                                          
022800 01  MAAL-AREA.                                                           
022900     03 MAAL-AREA-DATA.                                                   
023000        05 MAAL-IDDC          PIC X(2).                                   
023100        05 MAAL-KDINLUPF      PIC X(4).                                   
023200        05 MAAL-TIGLT         PIC 9(4).                                   
023300        05 MAAL-TIGLT-PRIO    PIC 9(4).                                   
023400        05 MAAL-FLEXCP        PIC X(1).                                   
023500        05 MAAL-IDLEVNR       PIC X(5).                                   
023600        05 MAAL-IDAVD-DAG     PIC X(5)  VALUE SPACE.                      
023700        05 MAAL-IDGRUPP-DAG   PIC X(2)  VALUE SPACE.                      
023800        05 MAAL-IDAVD-NATT    PIC X(5)  VALUE SPACE.                      
023900        05 MAAL-IDGRUPP-NATT  PIC X(2)  VALUE SPACE.                      
024000     EJECT                                                                
024100 01  UT-AREA-START      PIC X(24)   VALUE   'UT-AREA-START  '.            
024200*01  AREA -COPY W6115D01     -PRE UT-                                     
024300     EJECT                                                                
024400*    --- TABELL FÖR SUMMERINGAR PER KDINLUPF                              
024500 01  TABELL-START          PIC X(24)   VALUE                              
024600                                 'TABELL-START  '.                        
024700 01  FILLER.                                                              
024800     05 W-SUM-TAB   OCCURS 500 INDEXED BY SUM-IX.                         
024900        07  W-SUM-KDINLUPF            PIC X(4).                           
025000        07  W-SUM-IDAVD-DAG           PIC X(5).                           
025100        07  W-SUM-IDGRUPP-DAG         PIC X(2).                           
025200        07  W-SUM-IDAVD-NATT          PIC X(5).                           
025300        07  W-SUM-IDGRUPP-NATT        PIC X(2).                           
025400        07  W-SUM-KVRADER             PIC S9(5)      COMP-3.              
025500        07  W-SUM-KVRADER-DAG         PIC S9(5)      COMP-3.              
025600        07  W-SUM-KVRADER-KVALL       PIC S9(5)      COMP-3.              
025700        07  W-SUM-KVRADER-PRIO        PIC S9(5)      COMP-3.              
025800        07  W-SUM-KVRADER-PRIO-DAG    PIC S9(5)      COMP-3.              
025900        07  W-SUM-KVRADER-PRIO-KVALL  PIC S9(5)      COMP-3.              
026000        07  W-SUM-TIGLT-DAG           PIC S9(9)      COMP-3.              
026100        07  W-SUM-TIGLT-KVALL         PIC S9(9)      COMP-3.              
026200        07  W-SUM-TIGLT-PRIO-DAG      PIC S9(9)      COMP-3.              
026300        07  W-SUM-TIGLT-PRIO-KVALL    PIC S9(9)      COMP-3.              
026400        07  W-SUM-KVART-MAAL-DAG      PIC S9(5)      COMP-3.              
026500        07  W-SUM-KVART-MAAL-KVALL    PIC S9(5)      COMP-3.              
026600        07  W-SUM-KVART-MAAL-PRIO-DAG PIC S9(5)      COMP-3.              
026700        07  W-SUM-KVART-MAAL-PRIO-KVALL PIC S9(5)     COMP-3.             
026800        07  W-SUM-VLARTNTO-DAG        PIC S9(13)V9(1) COMP-3.             
026900        07  W-SUM-VLARTNTO-KVALL      PIC S9(13)V9(1) COMP-3.             
026901        07  W-SUM-SUBEL-DAG           PIC S9(9)V9(2)  COMP-3.             
026910        07  W-SUM-SUBEL-KVALL         PIC S9(9)V9(2)  COMP-3.             
027000        07  W-SUM-IDLEVNR             PIC X(5).                           
027100        07  W-LAST-IDLOPNRM           PIC S9(9)       COMP-3.             
027200     EJECT                                                                
027300 01  FILLER                PIC X(24)   VALUE  'MAAL TABELL   '.           
027400 01  MAAL-TABELL.                                                         
027500      04 W-MAAL-TAB  OCCURS 500 INDEXED BY MAAL-IX.                       
027600         05  W-MAAL-TOTAL.                                                
027700           06  W-MAAL-IDDC             PIC XX.                            
027800           06  W-MAAL-KDINLUPF         PIC X(4).                          
027900           06  W-MAAL-TIGLT            PIC 9(4).                          
028000           06  W-MAAL-TIGLT-PRIO       PIC 9(4).                          
028100           06  W-MAAL-FLEXCP           PIC X(1).                          
028200           06  W-MAAL-IDLEVNR          PIC X(5).                          
028300           06  W-MAAL-IDAVD-DAG        PIC X(5).                          
028400           06  W-MAAL-IDGRUPP-DAG      PIC X(2).                          
028500           06  W-MAAL-IDAVD-NATT       PIC X(5).                          
028600           06  W-MAAL-IDGRUPP-NATT     PIC X(2).                          
028700     EJECT                                                                
028800 PROCEDURE DIVISION.                                                      
028900                                                                          
029000     PERFORM A-INIT                                                       
029100     PERFORM S01-LAES-W61140                                              
029200     PERFORM UNTIL END-OF-W61140                                          
029300         PERFORM B-BEHANDLA-STATUS-POST                                   
029400         PERFORM S01-LAES-W61140                                          
029500     END-PERFORM                                                          
029600                                                                          
029700     PERFORM C-TOEM-KDINLUPF-TAB                                          
029800                                                                          
029900     PERFORM Z-FINIT                                                      
030000                                                                          
030100     MOVE ZERO TO RETURN-CODE                                             
030200     GOBACK                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 A-INIT SECTION.                                                          
030600                                                                          
030700     OPEN INPUT  W61140                                                   
030800                 W6115A                                                   
030900                                                                          
031000     OPEN OUTPUT W6115D                                                   
031100     SKIP2                                                                
031200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
031300     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
031400     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
031500     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
031600     MOVE K-VECKA              TO  DAGENS-VECKA                           
032200     MOVE IDPGM                TO  POSTSUM-PROGNAMN                       
032360                                                                          
032400     SET SUM-IX           TO +1                                           
032500     PERFORM UNTIL SUM-IX      >  MAX-SUM-IX                              
032600       MOVE SPACE  TO W-SUM-KDINLUPF            (SUM-IX)                  
032700                      W-SUM-IDGRUPP-DAG         (SUM-IX)                  
032800                      W-SUM-IDGRUPP-NATT        (SUM-IX)                  
032900                      W-SUM-IDAVD-DAG           (SUM-IX)                  
033000                      W-SUM-IDAVD-NATT          (SUM-IX)                  
033100       MOVE ZERO   TO W-SUM-KVRADER             (SUM-IX)                  
033200                      W-SUM-KVRADER-DAG         (SUM-IX)                  
033300                      W-SUM-KVRADER-KVALL       (SUM-IX)                  
033400                      W-SUM-KVRADER-PRIO        (SUM-IX)                  
033500                      W-SUM-KVRADER-PRIO-DAG    (SUM-IX)                  
033600                      W-SUM-KVRADER-PRIO-KVALL  (SUM-IX)                  
033700                      W-SUM-TIGLT-DAG           (SUM-IX)                  
033800                      W-SUM-TIGLT-KVALL         (SUM-IX)                  
033900                      W-SUM-TIGLT-PRIO-DAG      (SUM-IX)                  
034000                      W-SUM-TIGLT-PRIO-KVALL    (SUM-IX)                  
034100                      W-SUM-KVART-MAAL-DAG      (SUM-IX)                  
034200                      W-SUM-KVART-MAAL-KVALL    (SUM-IX)                  
034300                      W-SUM-KVART-MAAL-PRIO-DAG (SUM-IX)                  
034400                      W-SUM-KVART-MAAL-PRIO-KVALL (SUM-IX)                
034500                      W-SUM-VLARTNTO-DAG        (SUM-IX)                  
034600                      W-SUM-VLARTNTO-KVALL      (SUM-IX)                  
034610                      W-SUM-SUBEL-DAG           (SUM-IX)                  
034620                      W-SUM-SUBEL-KVALL         (SUM-IX)                  
034700                      W-LAST-IDLOPNRM           (SUM-IX)                  
034800         MOVE SPACE TO  W-SUM-IDLEVNR           (SUM-IX)                  
034900       SET SUM-IX UP BY +1                                                
035000     END-PERFORM                                                          
035100                                                                          
035200**   MOVE 'R31 '     TO W-SUM-KDINLUPF  ( 1 )                             
035300**   MOVE 'C2  '     TO W-SUM-KDINLUPF  ( 2 )                             
035400     SET SUM-IX  TO   +1                                                  
035500                                                                          
035600     MOVE SPACE TO W-SPAR1-AREA                                           
035700                   W-SPAR2-AREA                                           
035800     MOVE ZERO TO  W-SPAR1-IDLOPNRM                                       
035900                   W-SPAR2-IDLOPNRM                                       
036000                   W-SPAR2-IDRADNR                                        
036100     PERFORM S02-LAES-W6115A                                              
036200     PERFORM S23-CONVERT-HHMM                                             
036300                                                                          
036400     SET MAAL-IX TO +1                                                    
036500*                             --- LADDA MÅL-TABELL FÖR CDC                
036600     MOVE MAAL-IDDC    TO WS-IDDC                                         
036700     PERFORM UNTIL END-OF-W6115A                                          
036800             OR CDC-TR OR SDC OR NDC                                      
036900       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (MAAL-IX)                      
037000       SET MAAL-IX UP BY +1                                               
037100       PERFORM S02-LAES-W6115A                                            
037200       PERFORM S23-CONVERT-HHMM                                           
037300       MOVE MAAL-IDDC     TO WS-IDDC                                      
037400     END-PERFORM                                                          
037500*                                                                         
037510     IF MAAL-IX   > MAX-MAAL-IX                                           
037520       STRING 'MÅL TAB FULL FÖR CDC='                                     
037530       DELIMITED BY SIZE   INTO  FELTEXT-STR                              
037540       DISPLAY FELTEXT                                                    
037550       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
037560     END-IF                                                               
037570*                             --- RENSA RESTEN AV CDC-TABELLEN            
037600     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
037700       MOVE SPACE TO W-MAAL-KDINLUPF   (MAAL-IX)                          
037800                     W-MAAL-IDDC       (MAAL-IX)                          
037900                     W-MAAL-IDLEVNR    (MAAL-IX)                          
038000                     W-MAAL-IDGRUPP-DAG (MAAL-IX)                         
038100                     W-MAAL-IDGRUPP-NATT (MAAL-IX)                        
038200                     W-MAAL-IDAVD-DAG  (MAAL-IX)                          
038300                     W-MAAL-IDAVD-NATT (MAAL-IX)                          
038400       MOVE ZERO  TO W-MAAL-TIGLT      (MAAL-IX)                          
038500                     W-MAAL-TIGLT-PRIO (MAAL-IX)                          
038600       SET MAAL-IX UP BY +1                                               
038700     END-PERFORM                                                          
038800     .                                                                    
038900     EJECT                                                                
039000 B-BEHANDLA-STATUS-POST     SECTION.                                      
039100                                                                          
039110     MOVE NEJ TO TIGLT-ADDED                                              
039200     MOVE IN-IDDC        TO WS-IDDC                                       
039300     MOVE IN-TIKLOCK     TO W-DK-TIKLOCK                                  
039400     MOVE 'DAG' TO NEW-DAG-KVALL-SW                                       
039500     IF W-DK-HH <= 15                                                     
039600       IF W-DK-HH = 15 AND W-DK-MM > 30                                   
039700         MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                 
039800       END-IF                                                             
039900     ELSE                                                                 
040000       MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                   
040100     END-IF                                                               
040200                                                                          
040202     IF IN-IDLOPNRM NOT = W-SPAR1-IDLOPNRM                                
040203       MOVE ZERO TO W-SPAR3-TIREGDAT                                      
040204                    W-SPAR3-TIKLOCK                                       
040205                    W-SPAR4-TIREGDAT                                      
040206                    W-SPAR4-TIKLOCK                                       
040207     END-IF                                                               
040209                                                                          
040210     IF IN-IDRADNR = 1                                                    
040211       IF IN-KVINLART = 0                                                 
040212         MOVE IN-AREA TO W-SPAR4-AREA                                     
040213       ELSE                                                               
040214         MOVE IN-AREA TO W-SPAR3-AREA                                     
040215       END-IF                                                             
040220     END-IF                                                               
040230                                                                          
040300     PERFORM BA-KOLLA-OM-NY-KDINLUPF                                      
040400     IF IN-IDLOPNRM    = W-SPAR1-IDLOPNRM                                 
040500       IF IN-IDRADNR = 2                                                  
040600         IF ((IN-TIREGDAT = W-SPAR1-TIREGDAT) AND                         
040700             (IN-TIKLOCK = W-SPAR1-TIKLOCK))                              
040800                         OR                                               
040900             (IN-TIREGDAT < W-SPAR1-TIREGDAT)                             
041000           MOVE IN-AREA TO W-SPAR1-AREA                                   
041100         END-IF                                                           
041200       END-IF                                                             
041300     ELSE                                                                 
041400       MOVE IN-AREA TO W-SPAR1-AREA                                       
041500     END-IF                                                               
041600                                                                          
041700     PERFORM S21-KONVERTERA-DATUM                                         
041800                                                                          
041900     IF IN-IDLOPNRM        = W-SPAR2-IDLOPNRM  AND                        
042000        IN-IDRADNR         = W-SPAR2-IDRADNR   AND                        
042100        W-TIVV             = DAGENS-VECKA                                 
042200       PERFORM BB-TA-FRAM-KDINLUPF-INDEX                                  
042300                                                                          
042400       IF IN-KDINLUPF-NXT = SPACE                                         
042500         IF IN-KDINLUPF   = W-SPAR2-KDINLUPF AND                          
042600            IN-KDINLSTA   = W-SPAR2-KDINLSTA AND                          
042700            IN-KVINLART   = W-SPAR2-KVINLART                              
042800           CONTINUE                                                       
042900         ELSE                                                             
043000           PERFORM BC-BEHANDLA-PRODUCERAT-IDAG                            
043100           PERFORM BD-BEHANDLA-GENOMLOPPSTID                              
043200         END-IF                                                           
043300       ELSE                                                               
043400         IF IN-KDINLUPF-NXT = W-SPAR2-KDINLUPF AND                        
043500            IN-KDINLSTA     = W-SPAR2-KDINLSTA AND                        
043600            IN-KVINLART     = W-SPAR2-KVINLART                            
043700           CONTINUE                                                       
043800         ELSE                                                             
043900           PERFORM BC-BEHANDLA-PRODUCERAT-IDAG                            
044000           PERFORM BD-BEHANDLA-GENOMLOPPSTID                              
044100         END-IF                                                           
044200       END-IF                                                             
044300     END-IF                                                               
044400                                                                          
044500     IF (IN-KDINLSTA = 'INL' OR 'VOR' OR 'FRD') AND                       
044600         W-TIVV      = DAGENS-VECKA                                       
044700       IF IN-IDRADNR NOT = W-SPAR2-IDRADNR                                
044800         PERFORM BA-KOLLA-OM-NY-KDINLUPF                                  
044801         MOVE IN-TIKLOCK TO NEW-TIKLOCK                                   
044802         MOVE W-SPAR4-TIKLOCK TO OLD-TIKLOCK                              
044810         IF TIGLT-ADDED = JA OR (W-SPAR4-TIREGDAT > 0 AND                 
044820             OLD-TIKLOCK(1:4) = NEW-TIKLOCK(1:4))                         
044900           ADD  +1  TO W-SUM-KVRADER       (SUM-IX)                       
045000           IF NEW-DAG                                                     
045100             ADD +1 TO W-SUM-KVRADER-DAG   (SUM-IX)                       
045200           ELSE                                                           
045300             ADD +1 TO W-SUM-KVRADER-KVALL (SUM-IX)                       
045400           END-IF                                                         
045500                                                                          
045600           IF IN-KDINLPRIO      <  +31                                    
045700             ADD +1    TO W-SUM-KVRADER-PRIO (SUM-IX)                     
045800             IF NEW-DAG                                                   
045900               ADD +1  TO W-SUM-KVRADER-PRIO-DAG (SUM-IX)                 
046000             ELSE                                                         
046100               ADD +1  TO W-SUM-KVRADER-PRIO-KVALL (SUM-IX)               
046200             END-IF                                                       
046300                                                                          
046400           END-IF                                                         
046500                                                                          
046600           IF NEW-DAG                                                     
046700              COMPUTE W-SUM-VLARTNTO-DAG (SUM-IX) =                       
046800                      W-SUM-VLARTNTO-DAG (SUM-IX) +                       
046900                      (IN-VLARTNTO * IN-KVINLART)                         
046910              COMPUTE W-SUM-SUBEL-DAG (SUM-IX) =                          
046920                      W-SUM-SUBEL-DAG (SUM-IX) +                          
046930                      (IN-PRARTSTD * IN-KVINLART)                         
047000           ELSE                                                           
047100              COMPUTE W-SUM-VLARTNTO-KVALL (SUM-IX) =                     
047200                      W-SUM-VLARTNTO-KVALL (SUM-IX) +                     
047300                      (IN-VLARTNTO * IN-KVINLART)                         
047310              COMPUTE W-SUM-SUBEL-KVALL (SUM-IX) =                        
047320                      W-SUM-SUBEL-KVALL (SUM-IX) +                        
047330                      (IN-PRARTSTD * IN-KVINLART)                         
047400           END-IF                                                         
047401                                                                          
047410           IF TIGLT-ADDED = NEJ                                           
047411             IF W-SPAR3-TIREGDAT > 0 AND W-SPAR3-KDINLUPF =               
047412               IN-KDINLUPF AND W-SPAR3-KVINLART = IN-KVINLART             
047420               MOVE W-SPAR3-TIREGDAT TO W-SPAR2-TIREGDAT                  
047421               MOVE W-SPAR3-TIKLOCK  TO W-SPAR2-TIKLOCK                   
047422               PERFORM BD-BEHANDLA-GENOMLOPPSTID                          
047423             END-IF                                                       
047430           END-IF                                                         
047500         END-IF                                                           
047510                                                                          
047600         IF IN-IDLOPNRM NOT = W-LAST-IDLOPNRM (SUM-IX)                    
047700           MOVE IN-IDLOPNRM TO W-LAST-IDLOPNRM (SUM-IX)                   
047800         END-IF                                                           
047900       END-IF                                                             
048000     END-IF                                                               
048100                                                                          
048200     MOVE IN-AREA      TO W-SPAR2-AREA                                    
048300     .                                                                    
048400     EJECT                                                                
048500 BA-KOLLA-OM-NY-KDINLUPF     SECTION.                                     
048600                                                                          
048700     MOVE IN-KDINLUPF      TO W-KDINLUPF                                  
048800                                                                          
048900     IF  W-KDINLUPF      = SPACE                                          
049000     AND IN-KDINLSTA     = SPACE                                          
049100        MOVE 'R31 '        TO W-KDINLUPF                                  
049200     END-IF                                                               
049300                                                                          
049400     IF  W-KDINLUPF      = SPACE                                          
049500     AND IN-KDINLSTA     = 'AVI'                                          
049600        MOVE 'C2  '        TO W-KDINLUPF                                  
049700     END-IF                                                               
049800                                                                          
049900*    HÄMTA LEVERANTÖRSID FRÅN MÅL TABELL                                  
050000     PERFORM S11-KOLLA-MAAL-TABELL                                        
050100     IF MAAL-IX NOT > MAX-MAAL-IX                                         
050200       MOVE W-MAAL-IDLEVNR      (MAAL-IX) TO W-IDLEVNR                    
050300       MOVE W-MAAL-IDAVD-DAG    (MAAL-IX) TO W-IDAVD-DAG                  
050400       MOVE W-MAAL-IDGRUPP-DAG  (MAAL-IX) TO W-IDGRUPP-DAG                
050500       MOVE W-MAAL-IDAVD-NATT   (MAAL-IX) TO W-IDAVD-NATT                 
050600       MOVE W-MAAL-IDGRUPP-NATT (MAAL-IX) TO W-IDGRUPP-NATT               
050700     ELSE                                                                 
050800       MOVE SPACE                    TO W-IDLEVNR                         
050900                                        W-IDAVD-DAG                       
051000                                        W-IDGRUPP-DAG                     
051100                                        W-IDAVD-NATT                      
051200                                        W-IDGRUPP-NATT                    
051300     END-IF                                                               
051400                                                                          
051500     SET SUM-IX    TO +1                                                  
051600     PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                  
051700             OR W-SUM-KDINLUPF (SUM-IX) = SPACE                           
051800             OR W-SUM-KDINLUPF (SUM-IX) = W-KDINLUPF                      
051900                 SET SUM-IX UP BY +1                                      
052000     END-PERFORM                                                          
052100                                                                          
052200     IF SUM-IX   > MAX-SUM-IX                                             
052300         STRING 'SUM TAB FULL FÖR IDDC=' IN-IDDC                          
052400         DELIMITED BY SIZE   INTO  FELTEXT-STR                            
052500         DISPLAY FELTEXT                                                  
052600         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
052700     ELSE                                                                 
052800       IF W-SUM-KDINLUPF (SUM-IX) = SPACE                                 
052900         MOVE W-KDINLUPF     TO W-SUM-KDINLUPF        (SUM-IX)            
053000         MOVE W-IDLEVNR      TO W-SUM-IDLEVNR         (SUM-IX)            
053100         MOVE W-IDAVD-DAG    TO W-SUM-IDAVD-DAG       (SUM-IX)            
053200         MOVE W-IDGRUPP-DAG  TO W-SUM-IDGRUPP-DAG     (SUM-IX)            
053300         MOVE W-IDAVD-NATT   TO W-SUM-IDAVD-NATT      (SUM-IX)            
053400         MOVE W-IDGRUPP-NATT TO W-SUM-IDGRUPP-NATT    (SUM-IX)            
053500                                                                          
053600         MOVE ZERO        TO W-SUM-KVRADER            (SUM-IX)            
053700                             W-SUM-KVRADER-DAG        (SUM-IX)            
053800                             W-SUM-KVRADER-KVALL      (SUM-IX)            
053900                             W-SUM-KVRADER-PRIO       (SUM-IX)            
054000                             W-SUM-KVRADER-PRIO-DAG   (SUM-IX)            
054100                             W-SUM-KVRADER-PRIO-KVALL (SUM-IX)            
054200                             W-SUM-TIGLT-DAG          (SUM-IX)            
054300                             W-SUM-TIGLT-KVALL        (SUM-IX)            
054400                             W-SUM-TIGLT-PRIO-DAG     (SUM-IX)            
054500                             W-SUM-TIGLT-PRIO-KVALL   (SUM-IX)            
054600                             W-SUM-KVART-MAAL-DAG     (SUM-IX)            
054700                             W-SUM-KVART-MAAL-KVALL   (SUM-IX)            
054800                             W-SUM-KVART-MAAL-PRIO-DAG (SUM-IX)           
054900                             W-SUM-KVART-MAAL-PRIO-KVALL (SUM-IX)         
055000                             W-SUM-VLARTNTO-DAG       (SUM-IX)            
055100                             W-SUM-VLARTNTO-KVALL     (SUM-IX)            
055101                             W-SUM-SUBEL-DAG          (SUM-IX)            
055110                             W-SUM-SUBEL-KVALL        (SUM-IX)            
055200       END-IF                                                             
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 BB-TA-FRAM-KDINLUPF-INDEX  SECTION.                                      
055700                                                                          
055800** GÅ IGENOM KDINLUPF TAB FÖR ATT FÅ FRAM RÄTT SUM-IX EFTERSOM            
055900** GENOMLOPPSTIDEN M M SKALL BERÄKNAS PÅ FÖREGÅENDE POST                  
056000** FÖR DETTA IDDC.                                                        
056100                                                                          
056200     MOVE W-SPAR2-KDINLUPF      TO W-KDINLUPF                             
056300                                                                          
056400     SET SUM-IX      TO +1                                                
056500     PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                  
056600             OR W-SUM-KDINLUPF (SUM-IX) = W-KDINLUPF                      
056700                  SET SUM-IX UP BY +1                                     
056800     END-PERFORM                                                          
056900                                                                          
057000     IF SUM-IX                 > MAX-SUM-IX                               
057100         STRING  'KDINLUPF SAKNAS I SUM TAB FÖR IDDC=' IN-IDDC            
057200         DELIMITED BY SIZE   INTO  FELTEXT-STR                            
057300         DISPLAY FELTEXT                                                  
057400         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800 BC-BEHANDLA-PRODUCERAT-IDAG SECTION.                                     
057900                                                                          
058000     IF IN-KDINLSTA = W-SPAR2-KDINLSTA AND                                
058100        IN-KDINLUPF = W-SPAR2-KDINLUPF AND                                
058200       (IN-KVINLART NOT = W-SPAR2-KVINLART )                              
058300       MOVE NEJ TO ADD-RADER-SW                                           
058400     ELSE                                                                 
058500       MOVE JA  TO ADD-RADER-SW                                           
058600       ADD +1   TO W-SUM-KVRADER (SUM-IX)                                 
058700* CCID 6442199 -                                                          
058800       IF NEW-DAG                                                         
058900         ADD +1 TO W-SUM-KVRADER-DAG   (SUM-IX)                           
059000       ELSE                                                               
059100         ADD +1 TO W-SUM-KVRADER-KVALL (SUM-IX)                           
059200       END-IF                                                             
059300       IF IN-KDINLPRIO      <  +31                                        
059400         ADD +1    TO W-SUM-KVRADER-PRIO (SUM-IX)                         
059500* CCID 6442199 -                                                          
059600         IF NEW-DAG                                                       
059700           ADD +1  TO W-SUM-KVRADER-PRIO-DAG (SUM-IX)                     
059800         ELSE                                                             
059900           ADD +1  TO W-SUM-KVRADER-PRIO-KVALL (SUM-IX)                   
060000         END-IF                                                           
060100       END-IF                                                             
060200                                                                          
060300       IF NEW-DAG                                                         
060400         COMPUTE W-SUM-VLARTNTO-DAG (SUM-IX) =                            
060500                 W-SUM-VLARTNTO-DAG (SUM-IX) +                            
060600                (IN-VLARTNTO * IN-KVINLART)                               
060610         COMPUTE W-SUM-SUBEL-DAG (SUM-IX) =                               
060620                 W-SUM-SUBEL-DAG (SUM-IX) +                               
060630                 (IN-PRARTSTD * IN-KVINLART)                              
060700       ELSE                                                               
060800         COMPUTE W-SUM-VLARTNTO-KVALL (SUM-IX) =                          
060900                 W-SUM-VLARTNTO-KVALL (SUM-IX) +                          
061000                (IN-VLARTNTO * IN-KVINLART)                               
061010         COMPUTE W-SUM-SUBEL-KVALL (SUM-IX) =                             
061020                 W-SUM-SUBEL-KVALL (SUM-IX) +                             
061030                 (IN-PRARTSTD * IN-KVINLART)                              
061100       END-IF                                                             
061200                                                                          
061300       IF IN-IDLOPNRM NOT = W-LAST-IDLOPNRM   (SUM-IX)                    
061400         MOVE IN-IDLOPNRM  TO W-LAST-IDLOPNRM (SUM-IX)                    
061500       END-IF                                                             
061600     END-IF                                                               
061700                                                                          
061800     .                                                                    
061900     EJECT                                                                
062000 BD-BEHANDLA-GENOMLOPPSTID   SECTION.                                     
062100                                                                          
062200* CCID 6628002 -                                                          
062300     IF IN-KDINLSTA = W-SPAR2-KDINLSTA AND                                
062400        IN-KDINLUPF = W-SPAR2-KDINLUPF AND                                
062500       (IN-KVINLART NOT = W-SPAR2-KVINLART )                              
062600       CONTINUE                                                           
062700     ELSE                                                                 
062710       MOVE JA TO TIGLT-ADDED                                             
062800       PERFORM BDA-BERAEKNA-ARBETSDAGAR                                   
062900       MOVE IN-TIKLOCK              TO W-NEW-TIKLOCK                      
063000       MOVE W-SPAR2-TIKLOCK         TO W-OLD-TIKLOCK                      
063100                                                                          
063200* CCID 6442199 -                                                          
063300       MOVE W-SPAR2-TIREGDAT TO DAT-I-TIDATUM                             
063400       PERFORM S22-CALC-WEEKDAY                                           
063500                                                                          
063600       PERFORM S20-BERAEKNA-TIGLT                                         
063700                                                                          
063800       IF IN-KVKOLLI = +0                                                 
063900* CCID 6442199 -                                                          
064000         IF NEW-DAG                                                       
064100           ADD W-TOT-MM TO W-SUM-TIGLT-DAG   (SUM-IX)                     
064200         ELSE                                                             
064300           ADD W-TOT-MM TO W-SUM-TIGLT-KVALL (SUM-IX)                     
064400         END-IF                                                           
064500       ELSE                                                               
064600* CCID 6442199 -                                                          
064700         IF NEW-DAG                                                       
064800           COMPUTE W-SUM-TIGLT-DAG (SUM-IX) =                             
064900                   W-SUM-TIGLT-DAG (SUM-IX) +                             
065000                   IN-KVKOLLI * W-TOT-MM                                  
065100         ELSE                                                             
065200           COMPUTE W-SUM-TIGLT-KVALL (SUM-IX) =                           
065300                   W-SUM-TIGLT-KVALL (SUM-IX) +                           
065400                   IN-KVKOLLI * W-TOT-MM                                  
065500         END-IF                                                           
065600       END-IF                                                             
065700                                                                          
065800       PERFORM S11-KOLLA-MAAL-TABELL                                      
065900                                                                          
066000       IF IN-KDINLPRIO      <  +31                                        
066100* CCID 6442199 -                                                          
066200         IF NEW-DAG                                                       
066300           ADD W-TOT-MM TO                                                
066400               W-SUM-TIGLT-PRIO-DAG (SUM-IX)                              
066500         ELSE                                                             
066600           ADD W-TOT-MM TO                                                
066700               W-SUM-TIGLT-PRIO-KVALL(SUM-IX)                             
066800         END-IF                                                           
066900                                                                          
067000         IF ADD-RADER OR IN-FLINLI = JA                                   
067100           IF MAAL-IX NOT > MAX-MAAL-IX                                   
067200             IF W-TOT-MM < W-MAAL-TIGLT-PRIO   (MAAL-IX)                  
067300               IF NEW-DAG                                                 
067400                 ADD +1 TO W-SUM-KVART-MAAL-PRIO-DAG (SUM-IX)             
067500                 ADD +1 TO W-SUM-KVART-MAAL-DAG (SUM-IX)                  
067600               ELSE                                                       
067700                 ADD +1 TO W-SUM-KVART-MAAL-PRIO-KVALL (SUM-IX)           
067800                 ADD +1 TO W-SUM-KVART-MAAL-KVALL (SUM-IX)                
067900               END-IF                                                     
068000             END-IF                                                       
068100           END-IF                                                         
068200         END-IF                                                           
068300       ELSE                                                               
068400                                                                          
068500         IF ADD-RADER OR IN-FLINLI = JA                                   
068600           IF MAAL-IX NOT > MAX-MAAL-IX                                   
068700             IF W-TOT-MM < W-MAAL-TIGLT   (MAAL-IX)                       
068800               IF NEW-DAG                                                 
068900                 ADD +1 TO W-SUM-KVART-MAAL-DAG (SUM-IX)                  
069000               ELSE                                                       
069100                 ADD +1 TO W-SUM-KVART-MAAL-KVALL (SUM-IX)                
069200               END-IF                                                     
069300             END-IF                                                       
069400           END-IF                                                         
069500         END-IF                                                           
069600       END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 BDA-BERAEKNA-ARBETSDAGAR    SECTION.                                     
070100                                                                          
070200     MOVE +001                 TO WORK-KDCALL                             
070300     MOVE WS-IDDC              TO WORK-IDDC                               
070400                                                                          
070500     MOVE W-SPAR2-TIREGDAT     TO WORK-TIAAMMDD-FOM                       
070600     MOVE IN-TIREGDAT          TO WORK-TIAAMMDD-TOM                       
070700*FIX                                                                      
070800     IF W-SPAR2-TIREGDAT       < 900000 AND > 001231                      
070900        MOVE W-SPAR2-TIREGDAT  TO WORK-TIAAMMDD-FOM                       
071000     ELSE                                                                 
071100*       IF W-SPAR2-TIREGDAT       < 980000                                
071200           MOVE W-SPAR2-TIREGDAT       TO WS-TIREGDATFIX                  
071300           MOVE 01 TO WS-TIREGDATFIX-AA                                   
071400           MOVE WS-TIREGDATFIX TO WORK-TIAAMMDD-FOM                       
071500*       ELSE                                                              
071600*          MOVE W-SPAR2-TIREGDAT TO WORK-TIAAMMDD-FOM                     
071700*       END-IF                                                            
071800     END-IF                                                               
071900                                                                          
072000     IF IN-TIREGDAT < 900000 AND > 001231                                 
072100        MOVE IN-TIREGDAT       TO WORK-TIAAMMDD-TOM                       
072200     ELSE                                                                 
072300*       IF IN-TIREGDAT < 980000                                           
072400           MOVE IN-TIREGDAT    TO WS-TIREGDATFIX                          
072500           MOVE 01             TO WS-TIREGDATFIX-AA                       
072600           MOVE WS-TIREGDATFIX TO WORK-TIAAMMDD-TOM                       
072700*       ELSE                                                              
072800*          MOVE IN-TIREGDAT TO WORK-TIAAMMDD-TOM                          
072900*       END-IF                                                            
073000     END-IF                                                               
073100                                                                          
073200*SLUTFIX                                                                  
073300     MOVE WORK-TIAAMMDD-FOM    TO TMP1-YYMMDD                             
073400     MOVE WORK-TIAAMMDD-TOM    TO TMP2-YYMMDD                             
073500     PERFORM WY2000P1                                                     
073600     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
073700       MOVE ZERO TO WORK-KVWORKD                                          
073800       DISPLAY 'NY TIDRÄKNING???? '                                       
073900               ' FOM = ' WORK-TIAAMMDD-FOM                                
074000               ' TOM = ' WORK-TIAAMMDD-TOM                                
074100     ELSE                                                                 
074200       IF WORK-TIAAMMDD-FOM  =  WORK-TIAAMMDD-TOM                         
074300         MOVE ZERO             TO  WORK-KVWORKD                           
074400       ELSE                                                               
074500         CALL WORKDAY USING WORK-KDCALL                                   
074600                   WORK-DATE-AREA WORK-KDSVAR                             
074700                                                                          
074800         IF WORK-KDSVAR-OK                                                
074900** STARTDAG OCH SLUTDAG FÖRUTSÄTTS VARA ICKE-HELA ARBETSDAGAR             
075000             IF WORK-KVWORKD > 2                                          
075100               COMPUTE  WORK-KVWORKD = WORK-KVWORKD - 2                   
075200             ELSE                                                         
075300               MOVE ZERO TO WORK-KVWORKD                                  
075400             END-IF                                                       
075500         ELSE                                                             
075600             MOVE 'FEL UR WORKDAY' TO FELTEXT-STR                         
075700             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
075800         END-IF                                                           
075900       END-IF                                                             
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 C-TOEM-KDINLUPF-TAB        SECTION.                                      
076400                                                                          
076500     SET SUM-IX          TO +1                                            
076600     PERFORM UNTIL SUM-IX > MAX-SUM-IX OR                                 
076700                   W-SUM-KDINLUPF (SUM-IX) = SPACE                        
076800      IF W-SUM-KVRADER (SUM-IX) > 0                                       
076900***** PREPARE AND WRITE OUTPUT RECORD FOR IDAVD-DAG                       
077000       MOVE W-SUM-IDAVD-DAG     (SUM-IX) TO UT-IDAVD                      
077100       IF UT-IDAVD =  SPACE                                               
077200          IF W-SUM-IDLEVNR      (SUM-IX) NOT =  SPACE                     
077300             MOVE W-SUM-IDLEVNR (SUM-IX) TO UT-IDAVD                      
077400             MOVE 57                     TO UT-IDAVD(1:2)                 
077500          END-IF                                                          
077600       END-IF                                                             
077700       MOVE W-SUM-IDGRUPP-DAG     (SUM-IX) TO UT-IDGRUPP                  
077800       MOVE W-SUM-KVRADER-DAG     (SUM-IX) TO UT-KVRADER                  
077900       MOVE W-SUM-KVRADER-PRIO-DAG(SUM-IX) TO UT-KVRADER-PRIO             
078000       MOVE ZERO                           TO UT-KVMIN                    
078100                                              UT-KVMIN-PRIO               
078200       MOVE W-SUM-TIGLT-DAG       (SUM-IX) TO UT-KVMIN                    
078300       MOVE W-SUM-TIGLT-PRIO-DAG  (SUM-IX) TO UT-KVMIN-PRIO               
078400       MOVE W-SUM-KVART-MAAL-DAG  (SUM-IX) TO UT-KVART-MAAL               
078500       MOVE W-SUM-KVART-MAAL-PRIO-DAG (SUM-IX)                            
078600                                           TO UT-KVART-MAAL-PRIO          
078610       MOVE W-SUM-SUBEL-DAG (SUM-IX) TO UT-SUBEL                          
078700                                                                          
078800       COMPUTE UT-VLARTNTO =                                              
078900               W-SUM-VLARTNTO-DAG(SUM-IX) / 1000000                       
079000                                                                          
079010       IF UT-KVRADER > 0                                                  
079100         PERFORM S03-SKRIV-W6115D                                         
079110       END-IF                                                             
079200***** PREPARE AND WRITE OUTPUT RECORD FOR IDAVD-KVALL                     
079300       MOVE W-SUM-IDAVD-NATT    (SUM-IX) TO UT-IDAVD                      
079400       IF UT-IDAVD =  SPACE                                               
079500          IF W-SUM-IDLEVNR      (SUM-IX) NOT =  SPACE                     
079600             MOVE W-SUM-IDLEVNR (SUM-IX)   TO UT-IDAVD                    
079700             MOVE 57                       TO UT-IDAVD(1:2)               
079800          END-IF                                                          
079900       END-IF                                                             
080000       MOVE W-SUM-IDGRUPP-NATT    (SUM-IX) TO UT-IDGRUPP                  
080100       MOVE W-SUM-KVRADER-KVALL   (SUM-IX) TO UT-KVRADER                  
080200       MOVE W-SUM-KVRADER-PRIO-KVALL(SUM-IX)                              
080300                                           TO UT-KVRADER-PRIO             
080400       MOVE ZERO                           TO UT-KVMIN                    
080500                                              UT-KVMIN-PRIO               
080600       MOVE W-SUM-TIGLT-KVALL     (SUM-IX) TO UT-KVMIN                    
080700       MOVE W-SUM-TIGLT-PRIO-KVALL(SUM-IX) TO UT-KVMIN-PRIO               
080800       MOVE W-SUM-KVART-MAAL-KVALL(SUM-IX) TO UT-KVART-MAAL               
080900       MOVE W-SUM-KVART-MAAL-PRIO-KVALL (SUM-IX)                          
081000                                           TO UT-KVART-MAAL-PRIO          
081010       MOVE W-SUM-SUBEL-KVALL (SUM-IX) TO UT-SUBEL                        
081100       COMPUTE UT-VLARTNTO =                                              
081200               W-SUM-VLARTNTO-KVALL(SUM-IX) / 1000000                     
081220       IF UT-KVRADER > 0                                                  
081230         PERFORM S03-SKRIV-W6115D                                         
081240       END-IF                                                             
081400      END-IF                                                              
081500      SET SUM-IX UP BY +1                                                 
081600     END-PERFORM                                                          
081700     .                                                                    
081800     EJECT                                                                
081900 Z-FINIT SECTION.                                                         
082000     CLOSE W61140                                                         
082100           W6115A                                                         
082200           W6115D                                                         
082300     SKIP2                                                                
082400     MOVE 'S' TO POSTSUM-OPKOD                                            
082500     CALL POSTSUM USING POSTSUM-PARM                                      
082600     .                                                                    
082700     EJECT                                                                
082800 S01-LAES-W61140  SECTION.                                                
082900     SKIP2                                                                
083000     READ W61140 INTO IN-AREA                                             
083100     AT END                                                               
083200        SET END-OF-W61140 TO TRUE                                         
083300                                                                          
083400     NOT AT END                                                           
083500        MOVE 'W61140'          TO POSTSUM-FDNAMN                          
083600        MOVE 'W6115DD1'        TO POSTSUM-DDNAMN2                         
083700        CALL POSTSUM USING POSTSUM-PARM                                   
083800     END-READ                                                             
083900     .                                                                    
084000     EJECT                                                                
084100* CCID 6442199 -                                                          
084200 S02-LAES-W6115A   SECTION.                                               
084300     SKIP2                                                                
084400     READ W6115A   INTO MIN-W6115A01                                      
084500     AT END                                                               
084600        SET END-OF-W6115A      TO TRUE                                    
084700                                                                          
084800     NOT AT END                                                           
084900        MOVE 'W6115A  '        TO POSTSUM-FDNAMN                          
085000        MOVE 'W6115DD2'        TO POSTSUM-DDNAMN2                         
085100        CALL POSTSUM USING POSTSUM-PARM                                   
085200     END-READ                                                             
085300                                                                          
085400     MOVE MIN-W6115A01         TO MAAL-AREA                               
085500     .                                                                    
085600     EJECT                                                                
085700 S03-SKRIV-W6115D SECTION.                                                
085800     SKIP2                                                                
085900     WRITE UT-POST FROM UT-AREA                                           
086000                                                                          
086100     MOVE SPACE                TO POSTSUM-TRANSTYP                        
086200     MOVE 'W6115D'             TO POSTSUM-FDNAMN                          
086300     MOVE 'W6115DD3'           TO POSTSUM-DDNAMN2                         
086400     CALL POSTSUM USING POSTSUM-PARM                                      
086500     .                                                                    
086600     EJECT                                                                
086700 S11-KOLLA-MAAL-TABELL SECTION.                                           
086800     SET MAAL-IX               TO +1                                      
086900     PERFORM UNTIL MAAL-IX  > MAX-MAAL-IX                                 
087000             OR    W-MAAL-KDINLUPF  (MAAL-IX)                             
087100                 = W-KDINLUPF                                             
087200       SET MAAL-IX      UP BY       +1                                    
087300     END-PERFORM                                                          
087400     .                                                                    
087500     EJECT                                                                
087600 S20-BERAEKNA-TIGLT   SECTION.                                            
087700                                                                          
087800     PERFORM S20A-JUSTERA-TIDERNA                                         
087900                                                                          
088000     PERFORM S20B-BESTAEM-FM-EM                                           
088100                                                                          
088200     PERFORM S20C-BERAEKNA-OLD                                            
088300                                                                          
088400     PERFORM S20D-BERAEKNA-NEW                                            
088500                                                                          
088600* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
088700     IF W-WEEKDAY = 5                                                     
088800       IF W-SPAR2-TIREGDAT = IN-TIREGDAT                                  
088900         COMPUTE W-TIKLOCK-MM =                                           
089000       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 738)                      
089100       ELSE                                                               
089200        COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM        
089300       END-IF                                                             
089400     END-IF                                                               
089500                                                                          
089600     IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                      
089700       IF W-SPAR2-TIREGDAT = IN-TIREGDAT                                  
089800         COMPUTE W-TIKLOCK-MM =                                           
089900       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 972)                      
090000       ELSE                                                               
090100        COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM        
090200       END-IF                                                             
090300                                                                          
090400     END-IF                                                               
090500                                                                          
090600*** 16,2 = 972 MIN                                                        
090700     IF WORK-KVWORKD > 0                                                  
090800       COMPUTE W-TOT-MM    = (WORK-KVWORKD * 972) +                       
090900                                    W-TIKLOCK-MM                          
091000       IF W-WEEKDAY-OLD > W-WEEKDAY                                       
091100** HAR LEGAT EN FREDAG SOM HAR 234 FÄRRE ARBETSMINUTER                    
091200         COMPUTE W-TOT-MM  = W-TOT-MM - 234                               
091300       END-IF                                                             
091400     ELSE                                                                 
091500       MOVE W-TIKLOCK-MM TO W-TOT-MM                                      
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 S20A-JUSTERA-TIDERNA  SECTION.                                           
092000                                                                          
092100* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
092200                                                                          
092300** TIDER UTANFÖR 06.30 - 11.00 ; 11.42 - 18.00 ; 18.30 - 23.54            
092400** JUSTERAS TILL NÄRMSTA INTERVALL                                        
092500                                                                          
092600** SATURDAY/SUNDAY AND MONDAY BEFORE 6.30 WILL BE SET TO                  
092700** FRIDAY 20.00                                                           
092800     IF W-WEEKDAY = 6 OR 7 OR                                             
092900       (W-WEEKDAY = 1 AND                                                 
093000      ((W-OLD-HH < 6) OR (W-OLD-HH = 6 AND W-OLD-MM < 30)))               
093100                                                                          
093200       MOVE 20     TO W-OLD-HH                                            
093300       MOVE 00     TO W-OLD-MM                                            
093400     ELSE                                                                 
093500                                                                          
093600** MONDAY-FRIDAY BETWEEN 11.00 - 11.21 WILL BE SET TO 11.00               
093700** MONDAY-FRIDAY BETWEEN 11.22 - 11.42 WILL BE SET TO 11.42               
093800     IF W-OLD-HH = 11                                                     
093900       IF W-OLD-MM < 42                                                   
094000         IF W-OLD-MM < 21                                                 
094100           MOVE 00 TO W-OLD-MM                                            
094200         ELSE                                                             
094300           MOVE 42 TO W-OLD-MM                                            
094400         END-IF                                                           
094500       END-IF                                                             
094600     ELSE                                                                 
094700                                                                          
094800** MONDAY-FRIDAY BETWEEN 18.00 - 18.14 WILL BE SET TO 18.00               
094900** MONDAY-FRIDAY BETWEEN 18.15 - 18.30 WILL BE SET TO 18.30               
095000     IF W-OLD-HH = 18                                                     
095100       IF W-OLD-MM < 30                                                   
095200         IF W-OLD-MM < 15                                                 
095300           MOVE 00 TO W-OLD-MM                                            
095400         ELSE                                                             
095500           MOVE 30 TO W-OLD-MM                                            
095600         END-IF                                                           
095700       END-IF                                                             
095800     ELSE                                                                 
095900                                                                          
096000** FRIDAY AFTER 20.00 WILL BE SET TO 20.00                                
096100     IF W-WEEKDAY = 5                                                     
096200       IF (W-OLD-HH > 20) OR (W-OLD-HH = 20 AND W-OLD-MM > 00)            
096300         MOVE 20   TO W-OLD-HH                                            
096400         MOVE 00   TO W-OLD-MM                                            
096500       END-IF                                                             
096600     ELSE                                                                 
096700                                                                          
096800** MONDAY-FRIDAY BEFORE 6.30 WILL BE SET TO 6.30                          
096900     IF (W-OLD-HH < 6) OR (W-OLD-HH = 6 AND W-OLD-MM < 30)                
097000       MOVE  6     TO W-OLD-HH                                            
097100       MOVE 30     TO W-OLD-MM                                            
097200     ELSE                                                                 
097300                                                                          
097400** MONDAY-FRIDAY AFTER 23.54 WILL BE SET TO 23.54                         
097500     IF (W-OLD-HH > 23) OR (W-OLD-HH = 23 AND W-OLD-MM > 54)              
097600       MOVE 23     TO W-OLD-HH                                            
097700       MOVE 54     TO W-OLD-MM                                            
097800     END-IF                                                               
097900     END-IF                                                               
098000     END-IF                                                               
098100     END-IF                                                               
098200     END-IF                                                               
098300     END-IF                                                               
098400                                                                          
098500                                                                          
098600** SATURDAY/SUNDAY AND MONDAY BEFORE 6.30 WILL BE SET TO                  
098700** FRIDAY 20.00                                                           
098800     IF (W-WEEKDAY = 6 OR 7) OR                                           
098900        (W-WEEKDAY = 1 AND                                                
099000        (W-NEW-HH < 6) OR (W-NEW-HH = 6 AND W-NEW-MM < 30))               
099100       MOVE 20     TO W-NEW-HH                                            
099200       MOVE 00     TO W-NEW-MM                                            
099300     ELSE                                                                 
099400                                                                          
099500** MONDAY-FRIDAY BETWEEN 11.00 - 11.21 WILL BE SET TO 11.00               
099600** MONDAY-FRIDAY BETWEEN 11.22 - 11.42 WILL BE SET TO 11.42               
099700     IF W-NEW-HH = 11                                                     
099800       IF W-NEW-MM < 42                                                   
099900         IF W-NEW-MM < 21                                                 
100000           MOVE 00 TO W-NEW-MM                                            
100100         ELSE                                                             
100200           MOVE 42 TO W-NEW-MM                                            
100300         END-IF                                                           
100400       END-IF                                                             
100500     ELSE                                                                 
100600                                                                          
100700** FRIDAY AFTER 20.00 WILL BE SET TO 20.00                                
100800     IF W-WEEKDAY = 5                                                     
100900       IF (W-NEW-HH > 20) OR (W-NEW-HH = 20 AND W-NEW-MM > 00)            
101000         MOVE 20   TO W-NEW-HH                                            
101100         MOVE 00   TO W-NEW-MM                                            
101200       END-IF                                                             
101300     ELSE                                                                 
101400                                                                          
101500** MONDAY-FRIDAY BEFORE 6.30 WILL BE SET TO 6.30                          
101600     IF (W-NEW-HH < 6) OR (W-NEW-HH = 6 AND W-NEW-MM < 30)                
101700       MOVE  6     TO W-NEW-HH                                            
101800       MOVE 30     TO W-NEW-MM                                            
101900     ELSE                                                                 
102000                                                                          
102100** MONDAY-FRIDAY AFTER 23.54 WILL BE SET TO 23.54                         
102200     IF (W-NEW-HH > 23) OR (W-NEW-HH = 23 AND W-NEW-MM > 54)              
102300       MOVE 23     TO W-NEW-HH                                            
102400       MOVE 54     TO W-NEW-MM                                            
102500     END-IF                                                               
102600     END-IF                                                               
102700     END-IF                                                               
102800     END-IF                                                               
102900     END-IF                                                               
103000     .                                                                    
103100     EJECT                                                                
103200 S20B-BESTAEM-FM-EM  SECTION.                                             
103300                                                                          
103400     MOVE 'FM' TO OLD-SW                                                  
103500     IF W-OLD-HH <= 11                                                    
103600       IF W-OLD-HH = 11 AND W-OLD-MM > 41                                 
103700         MOVE 'EM' TO OLD-SW                                              
103800       END-IF                                                             
103900     ELSE                                                                 
104000       MOVE 'EM' TO OLD-SW                                                
104100     END-IF                                                               
104200                                                                          
104300     MOVE 'FM' TO NEW-SW                                                  
104400     IF W-NEW-HH <= 11                                                    
104500       IF W-NEW-HH = 11 AND W-NEW-MM > 41                                 
104600         MOVE 'EM' TO NEW-SW                                              
104700       END-IF                                                             
104800     ELSE                                                                 
104900       MOVE 'EM' TO NEW-SW                                                
105000     END-IF                                                               
105100                                                                          
105200     .                                                                    
105300     EJECT                                                                
105400 S20C-BERAEKNA-OLD SECTION.                                               
105500                                                                          
105600* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
105700                                                                          
105800     IF OLD-FM                                                            
105900       IF W-WEEKDAY = 5                                                   
106000         COMPUTE W-TIKLOCK-OLD-MM =                                       
106100         (((11 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM)) + 468           
106200** 7.8 TIM  = 468 MIN                                                     
106300       ELSE                                                               
106400         IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                  
106500           COMPUTE W-TIKLOCK-OLD-MM =                                     
106600           (((11 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM)) + 702         
106700** 11.7 TIM = 702 MIN                                                     
106800         END-IF                                                           
106900       END-IF                                                             
107000     ELSE                                                                 
107100       IF W-WEEKDAY = 5                                                   
107200         IF W-OLD-HH > 18 OR (W-OLD-HH = 18 AND W-OLD-MM > 15)            
107300           COMPUTE W-TIKLOCK-OLD-MM =                                     
107400           (((20 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM))               
107500         ELSE                                                             
107600           COMPUTE W-TIKLOCK-OLD-MM =                                     
107700           (((20 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM - 30))          
107800         END-IF                                                           
107900       ELSE                                                               
108000       IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                    
108100         IF W-OLD-HH > 18 OR (W-OLD-HH = 18 AND W-OLD-MM > 15)            
108200           COMPUTE W-TIKLOCK-OLD-MM =                                     
108300           (((23 * 60) - (W-OLD-HH * 60)) + (54 - W-OLD-MM))              
108400         ELSE                                                             
108500           COMPUTE W-TIKLOCK-OLD-MM =                                     
108600           (((23 * 60) - (W-OLD-HH * 60)) + (54 - W-OLD-MM - 30))         
108700         END-IF                                                           
108800       END-IF                                                             
108900       END-IF                                                             
109000     END-IF                                                               
109100     .                                                                    
109200     EJECT                                                                
109300 S20D-BERAEKNA-NEW SECTION.                                               
109400                                                                          
109500* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
109600                                                                          
109700     IF NEW-FM                                                            
109800       COMPUTE W-TIKLOCK-NEW-MM =                                         
109900       (((W-NEW-HH * 60) - (6.5 * 60)) + W-NEW-MM)                        
110000     ELSE                                                                 
110100       COMPUTE W-TIKLOCK-NEW-MM =                                         
110200       (((270 + (W-NEW-HH * 60)) - ((11 * 60) + 42))) + W-NEW-MM          
110300** 4.5 TIM = 270                                                          
110400     END-IF                                                               
110500     .                                                                    
110600     EJECT                                                                
110700 S21-KONVERTERA-DATUM SECTION.                                            
110800                                                                          
110900     MOVE IN-TIREGDAT TO DAT-I-TIDATUM                                    
111000     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
111100                                                                          
111200     CALL WDATKONV USING DAT-KDDATFORM                                    
111300                         DAT-I-TIDATUM                                    
111400                         DAT-O-TIDATUM                                    
111500                         DAT-KDSVAR                                       
111600                                                                          
111700     IF DAT-KDSVAR = SPACE                                                
111800       MOVE DAT-TIVV       TO W-TIVV                                      
111900     ELSE                                                                 
112000       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
112100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
112200     END-IF                                                               
112300     .                                                                    
112400     EJECT                                                                
112500 S22-CALC-WEEKDAY SECTION.                                                
112600                                                                          
112700* CCID 6442199 - NEW SECTION                                              
112800                                                                          
112900     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
113000                                                                          
113100     CALL WDATKONV USING DAT-KDDATFORM                                    
113200                         DAT-I-TIDATUM                                    
113300                         DAT-O-TIDATUM                                    
113400                         DAT-KDSVAR                                       
113500                                                                          
113600     IF DAT-KDSVAR = SPACE                                                
113700       MOVE DAT-TIAAVVD    TO W-TIAAVVD                                   
113800       MOVE W-TIAAVVD      TO W-TIAAVVD-OLD-ALPHA                         
113900     ELSE                                                                 
114000       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
114100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
114200     END-IF                                                               
114300                                                                          
114400     MOVE IN-TIREGDAT TO DAT-I-TIDATUM                                    
114500     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
114600                                                                          
114700     CALL WDATKONV USING DAT-KDDATFORM                                    
114800                         DAT-I-TIDATUM                                    
114900                         DAT-O-TIDATUM                                    
115000                         DAT-KDSVAR                                       
115100                                                                          
115200     IF DAT-KDSVAR = SPACE                                                
115300       MOVE DAT-TIAAVVD    TO W-TIAAVVD                                   
115400       MOVE W-TIAAVVD      TO W-TIAAVVD-ALPHA                             
115500     ELSE                                                                 
115600       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
115700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100 S23-CONVERT-HHMM SECTION.                                                
116200                                                                          
116300* CCID 6442199 - NEW SECTION                                              
116400                                                                          
116500     MOVE MAAL-TIGLT      TO WS-TIGLT                                     
116600     COMPUTE MAAL-TIGLT = (WS-TIGLT-HH * 60) + WS-TIGLT-MM                
116700     END-COMPUTE                                                          
116800                                                                          
116900     MOVE MAAL-TIGLT-PRIO TO WS-TIGLT                                     
117000     COMPUTE MAAL-TIGLT-PRIO = (WS-TIGLT-HH * 60) +                       
117100                                WS-TIGLT-MM                               
117200     END-COMPUTE                                                          
117300     .                                                                    
117400     EJECT                                                                
117500*    -COPY WY2000P1                                                       
